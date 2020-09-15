;; Input plugin to get data from VitalNode
;; Matthias Görges, 2014

;; Rupi call with shorter wait times
(define (rupi-cmd-shortwait . x) (apply rupi:cmd (append (list 0.05) x)))

;; Check for empty lists - this one is better than using pair? for it!
(define (list-notempty? lst) (and (list? lst) (not (null? lst))))

(define (store-update-data store lst . category)
  (let ((c (if (fx= (length category) 1) (car category) "remote")))
    (let loop ((i 0))
      (if (< i (length lst))
        (let ((row (list-ref lst i)))
          (store-set! store (car row) (cadr row) c)
          (loop (+ i 1))
        )
      )
    )
  ))

;; And the actual data retrieval code
(define (vitalnode:init store instance)
  (let* ((hostname (instance-refvar store instance "Hostname" "bcch-or.part-dns.org"))
         (port (instance-refvar store instance "Port" 8080))
         (key (instance-refvar store instance "Key" (u8vector 77 71 148 114 103 101 115 31)))
         (addr (with-exception-catcher (lambda (e) #f) (lambda () (car (host-info-addresses (host-info hostname)))))))
    (instance-setvar! store instance "RupiClient" (rupi-client 0 key addr port))
    (instance-setvar! store instance "DispatchStart" (flo (+ ##now 0.1)))
    (instance-setvar! store instance "DispatchCount" 0.)
  )
)

(define (vitalnode:caseinit store instance)
  (instance-setvar! store instance "NextRun" 0.)
  #t
)

(define (vitalnode:run store instance)
  (if (fl>= (fl- ##now (instance-refvar store instance "DispatchStart" 0.)) (instance-refvar store instance "DispatchCount" 0.))
    ;; run every 0.25 seconds
    (let ((dt 0.25))
      (instance-setvar! store instance "DispatchCount" (fl+ (instance-refvar store instance "DispatchCount" 0.) dt))
      (let* ((rc (instance-refvar store instance "RupiClient" #f))
             (pin (instance-refvar store instance "Pin" "9999"))
             (location (instance-refvar store instance "Location" "OR5"))
             (oldts (store-ref store "lastwave-timestamp" 0.))
             (wavets (rupi-cmd-shortwait rc "GETVALUE" pin location "waveform-timestamp"))
             (newts (if wavets wavets 1.)))
        (if (fl> newts oldts) (begin
          (store-set! store "lastwave-timestamp" wavets)
          (vitalnode:getdata store instance)
        ))
        ;; Clear stale waveform data [this doesn't seem right - merge commit]
        (let ((age (fl- ##now (store-timestamp store "lastwave-timestamp"))))
          (if (and (fl> age 1.2) (fl< age 5.0))
            (store-clear! store (map car (store-listcat store "waveform")))
          )
        )
      )
    )
  )
)

(define (vitalnode:getdata store instance)
  (let ((rc (instance-refvar store instance "RupiClient" #f))
        (pin (instance-refvar store instance "Pin" "9999"))
        (location (instance-refvar store instance "Location" "OR5")))
    (let ((data (rupi-cmd-shortwait rc "GETVALUES" pin location)))
      (if (pair? data) (begin
        (store-update-data store data)
        (store-clearexpired! store 5 (map car (store-listcat store "remote")))
      ))
    )
    (let ((data (rupi-cmd-shortwait rc "GETWAVES" pin location)))
      (if (list-notempty? data) (store-update-data store (cdar data) "waveform"))
    )
  )
)

(define (vitalnode:caseend store instance)
  #t
)

(define (vitalnode:end store instance)
  #t
)

;; register the plugin
(plugin-register "vitalnode" vitalnode:init vitalnode:caseinit vitalnode:run
                 vitalnode:caseend vitalnode:end 'input)
;; eof

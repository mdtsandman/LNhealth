;; Philips Waveform and Numerics Logger via VitalNode
;; Matthias Görges 2012-2017
;; Shaylene Beaudry 2017

;; Global variables
(define buf "")
(define delta-update 10) ;;sec
(define trend-time 3600) ;;sec
(define trend-len (fix (/ trend-time delta-update))) ;;sec
(define gui:numtimes 4) ;; Number of time lines
(define screenshot-time #f)
(define quit-armed? #f)

(define or-list (list))      ;; list of all monitoring locations
(define subject-location #f) ;; monitoring location for current subject

(define markers-timeline 
  (list "Bronchoscopy started" "Bronchoscopy ended" "Intubation" "Incision" "Fistula ligation complete" "Surgery ended")
)
(define markers-events
  (list (list "Surgery paused:" #f) "Surgery resumed" (list "HFJV paused:" #f) "HFJV resumed")
)

(define rupi_hostname "bcch-or.part-dns.org") ;; prod
(define rupi_port 8031)                       ;; prod

;(define rupi_hostname "ecem.ece.ubc.ca")      ;; demo
;(define rupi_port 8080)                       ;; demo

;; -----------------------------------------------------------------------------
;;  MAIN GUI
;; -----------------------------------------------------------------------------

;; (init-gui-main)
;;
;; The main gui parts: Logging list, Title row and all buttons are defined here.
(define gui:main #f)
(define (init-gui-main)
  ;; Instantiate the gui
  (set! gui:main (make-glgui))
  ;; Stamp with copyright image
  (glgui-pixmap gui:main 675 2 copyright.img)
  ;; Menubar
  (glgui-menubar gui:main 0 (- (glgui-height-get) 50) (glgui-width-get) 50)
  ;; Label in upper left corner
  (glgui-label gui:main 10 (- (glgui-height-get) 24 3) 350 24 "HFJV Data Logger" ascii_24.fnt White)
  ;; Clock in upper right corner
  (set! clock (glgui-label gui:main (- (glgui-width-get) 70) (- (glgui-height-get) 24) 60 16 "" ascii_16.fnt White))

  ;; Timeline buttons
  (let ([w 200] [x 1150] [y (- (glgui-height-get) 85) ])
    (let loop ([i 0])
      (if (< i (length markers-timeline))
        (let ([marker (list-ref markers-timeline i)])
          (if marker
            (let (
              [str (if (list? marker) (car marker) marker)]
              [cb (if (list? marker) marker-freetext-callback marker-callback)]
              [cl (list? marker)])
              (set! bs (glgui-button-string gui:main x (- y (* 35 i)) w 30 str ascii_16.fnt cb))
              (glgui-widget-set! gui:main bs 'value -1)
              (if cl (glgui-widget-set! gui:main bs 'color Pink))
            )
            ;; else do nothing
          )
          (loop (+ i 1))
        )
        ;; else do nothing
      )
    )
  )

  ;; Event buttons
  (let ([w 200] [x 1150] [y (- (glgui-height-get) 100 (* 35 6)) ])
    (let loop ([i 0])
      (if (< i (length markers-events))
        (let ([marker (list-ref markers-events i)])
          (if marker
            (let ([cl (list? marker)])
              (let 
                ([str (if cl (car marker) marker)] [cb (if cl marker-freetext-callback marker-callback)])
                (set! bs 
                  (
                    glgui-button-string
                    gui:main
                    x
                    (- y (* 35 i))
                    w
                    30
                    str
                    ascii_16.fnt
                    cb
                  )
                )
                (glgui-widget-set! gui:main bs 'value -1)
              )
            )
            ;; else do nothing
          )
          (loop (+ i 1))
        )
        ;; else do nothing
      )
    )
  )

  ;; Logging List
  (let ( [x 600] [y (- (glgui-height-get) 50 )] [w 500] [num_rows 24] [row_height 30] )
    ;;Header row
    (glgui-label gui:main (+ x  5) y 70         row_height "Time"      ascii_16.fnt White)
    (glgui-label gui:main (+ x 75) y (- w 75 5) row_height "Log Entry" ascii_16.fnt White)
    ;;Text Entry String
    (set! text (glgui-label gui:main x (- y row_height) w row_height "" ascii_24.fnt Black White ))
    ;;The actual list 
    (set! log-list
      (glgui-list gui:main x (- y (* (+ num_rows 1) row_height)) w (* num_rows row_height) row_height (build-log-list) #f)
    )
  ) 

) ;; end of init-gui-main definition

;; Other markers
(define (marker-callback g w t x y)
  (let* ([idx (glgui-widget-get g w 'value)] [marker (car (glgui-widget-get g w 'image))])
    (store-event-add store 0 marker)
    (store-set! store "EventMarker" 1.)
    (set! quit-armed? #f)
    (glgui-widget-set! gui:main log-list 'list (build-log-list))
    ;; Arm screenshot
    (if (string=? marker (car (reverse markers-timeline)))
      (set! screenshot-time (fl+ ##now 30.))
    )
  )
  (glgui-widget-set! g w 'color Black)
)

(define (marker-freetext-callback g w t x y)
  (let* ([idx (glgui-widget-get g w 'value)] [marker (car (glgui-widget-get g w 'image))])
    (set! buf (string-append marker " "))
    (glgui-widget-set! g text 'label buf)
                  
    (set! text 
      (
        glgui-label           ;; Create a Label widget from a string 
        gui:main              ;; The Graphical User Interface (GUI) belonging to this widget
        (+ x w 5)             ;; The lower left corner along the x-axis in pixels
        (- y 100 (* 35 6))    ;; The lower left corner along the y-axis in pixels
        250                   ;; The width of the element in pixels
        24                    ;; The height of the element in pixels
        ""                    ;; The label string
        ascii_24.fnt          ;; The font used to render the label string
        White                 ;; The widget color
      )
    )
  )
)

;; (build-log-list)
;;
;; The build-log list creator, which loops through the log entries and makes appropriate gui elements for it
(define (build-log-list)
  (let ((logs (store-event-listnew store)))
    (if logs
      (let loop ((i 0) (result (list)))
        (if (= i (length logs)) result
          (loop (+ i 1)(append result (list (log-list-element (list-ref logs i)))))
        )
      )
      (list)
    )
  )
)

;; (log-list-element entry)
;;
;; Draw a log-list element with data from the entry field
(define (log-list-element entry)
  (lambda (g wgt x y w h s)
    (glgui:draw-text-left (+ x 5) (+ y (/ (- h 16) 2)) 70 16 (seconds->string (car entry) "%T") ascii_16.fnt White)
    (glgui:draw-text-left (+ x 75) (+ y (/ (- h 24) 2)) (- w 90) 24 (cadr entry) ascii_24.fnt White)
  )
)

;; -----------------------------------------------------------------------------
;;  SETUP GUI
;; -----------------------------------------------------------------------------
(define (init-gui-setup)
  (let ([w 360] [h 240]) ;; dimensions of popup dialog for entry of setup info
    (let ([x (- (/ (glgui-width-get) 2) (/ w 2))] [y (- (/ (glgui-height-get) 2) (/ h 2))]) ;; center the popup
      (set! gui:setup (glgui-container gui:main x y w h))
      (glgui-box gui:setup 0 0 w h Navy)
      (set! setup-label (glgui-label gui:setup 20 (- h 30) (- w 40) 25 "Study Setup" ascii_24.fnt White))
      (glgui-widget-set! gui:setup setup-label 'align GUI_ALIGNCENTER)
      (glgui-label gui:setup 5 (- h 40 30) 195 30 "Subject No: BCCH-" ascii_24.fnt White)
      (set! setup-subjno (glgui-inputlabel gui:setup (+ 5 195) (- h 40 25) 60 25
                                         "" ascii_24.fnt White (color-shade White 0.2)))
      (glgui-label gui:setup 5 (- h 40 (* 30 2)) 100 30 "Age:" ascii_24.fnt White)
      (set! setup-age (glgui-inputlabel gui:setup (+ 5 195) (- h 40 (* 30 1) 25) 60 25
                                      "" ascii_24.fnt White (color-shade White 0.2)))
      (glgui-label gui:setup 5 (- h 40 (* 30 3)) 100 30 "Sex:" ascii_24.fnt White)
      (set! setup-gender (glgui-button-string gui:setup (+ 5 195) (- h 40 (* 30 2) 25) 150 25
                                            (list "Male" "Female") ascii_24.fnt #f))
      (glgui-label gui:setup 5 (- h 40 (* 30 4) 5) 100 30 "Location:" ascii_24.fnt White)
      (set! location-label (glgui-dropdownbox gui:setup (+ 5 195) (- h 40 (* 30 3) 25 10) 150 35
        (map (lambda (str) (lambda (lg lw x y w h s)
          (if s (glgui:draw-box x y w h Grey))
            (glgui:draw-text-left (+ x 5) y (- w 10) h str ascii_24.fnt Black)))
          or-list)
        Black DarkGrey Blue))
      (glgui-widget-set! gui:setup location-label 'callback location-callback)
      (glgui-button-string gui:setup 90 5 180 40 "Start Recording" ascii_24.fnt start-callback)
    )
  )
)

;; (location-callback g w t x y)
;;
;; Select the OR room to monitor
(define (location-callback g w t x y)
  ;; Get data from rupi
  (let* ( [cur (glgui-widget-get g w 'current)] [location (list-ref or-list cur)] )
    (if subject-location  ;; subject-location is a global var
      (begin
        (instance-setvar! store "VNmonitor" "Location" location)
        (store-clear! store (map car (store-listcat store "remote")))
      )
      (make-instance store "VNmonitor" "vitalnode" `("Location" ,location) `("Hostname" ,rupi_hostname))
    )
    (set! subject-location location)
  )
)

;; (start-callback g w t x y)
;;
;; Start Recording data
(define (start-callback g w t x y)
  (set! subject-no (string->number (glgui-widget-get gui:setup setup-subjno 'label)))
  (set! subject-age (string->number (glgui-widget-get gui:setup setup-age 'label)))
  (set! subject-gender (car (list-ref (glgui-widget-get gui:setup setup-gender 'image)
                                 (glgui-widget-get gui:setup setup-gender 'value))))
  ;; Clear the comment string
  (set! buf "")
  (glgui-widget-set! gui:main text 'label buf)
  ;; Check if we got data
  (let ((remote-lst (store-listcat "main" "remote")))
    (set! subject-hasdata? (and (list? remote-lst) (not (null? remote-lst))))
  )
  ;; Check if we got everything set
  (if (and subject-no subject-age subject-gender subject-hasdata?)
    (begin
      ;; Record the trend variables
      (make-instance "main" "TRENDOUT" "trendoutput" `("Trends"
        ,(append (list "time_str") (append ivue:physdatavalues_basic ivue:physdatavalues_aisys ivue:physdatavalues_nirs ivue:physdatavalues_tcco2))))
      ;; Record some waveforms
      (let ((waves (append ivue:waveform_basic ivue:waveforms_aisys)))
        (for-each (lambda (l) (make-instance "main" (string-append "WAVEOUT" l) "waveoutput" `("Source" ,l))) waves)
      )
      ;; Start the scheduler
      (scheduler-startcase store
        (string-append "BCCH-" (number->string subject-no) "_" (time->timestamp (current-time))))
      ;; Log the demographics
      (store-event-add store 0 (string-append "SubjectNo: BCCH-" (number->string subject-no)
                                              ",Location: " subject-location))
      (store-event-add store 0 (string-append "Age: " (number->string subject-age)
                                              ",Gender: " subject-gender))
      (glgui-widget-set! gui:main log-list 'list (build-log-list))
      ;; Hide the input box
      (glgui-widget-set! gui:main gui:setup 'hidden #t)
    )
  )
)


;; -----------------------------------------------------------------------------
;;  TREND GUI
;; -----------------------------------------------------------------------------
(define gui:trends #f)
;; vmin and vmax define the scale for each waveform
;; label.img must correspond to an item in the file STRINGS
;; yoffset is the vertical offset of the numeric where 1.0 represents the width of one trend band?
;; traceh is the width of the trend band, in pixels?
;; traceoffset is the ordinal vertical offset of the trace band to use for the waveform?
(define trends 
  (list
;;        name        vmin  vmax  color       label.img           storename   yoffset  trace-h  traceoffset
    (list "hr"        90    175   Green       label_hr.img        "HR"        0.8      100      1)
    (list "pulse"     90    175   DarkGreen   label_pulse.img     "PRSpO2"    1.1      100      1)
    (list "map_art"   35    105   Red         label_map_art.img   "ABPmean"   1.5      100      2)
    (list "map_nibp"  35    105   IndianRed   label_map_nibp.img  "NBPmean"   1.8      100      2)
    (list "spo2"      70    101   Aquamarine  label_spo2.img      "SpO2"      2.2      100      3)
    (list "rso2_1"    50    101   Blue        label_rso2_1.img    "rSO2-1"    2.6      100      4)
    (list "rso2_2"    50    101   LightBlue   label_rso2_2.img    "rSO2-2"    2.9      100      4)
    (list "pco2_tc"    0    121   Orange      label_pco2_tc.img   "tcpCO2"    3.3      100      5)
    (list "fio2_imv"  21    101   Blue        label_fio2_imv.img  "FiO2"      3.7      100      6)
    (list "rr_imv"     0     61   White       label_rr_imv.img    "RR"        4.1      100      7)
    (list "peep_imv"   0     36   White       label_peep_imv.img  "PEEP"      4.5      100      8)
    (list "pip_imv"    0     36   White       label_pip_imv.img   "PIP"       4.8      100      8)
  )
)

;; Plotting functions
(define (make-trends g x y0 s vars)
  (let* ([w trend-len] [h 75] [y (- y0 h)] [ws trend-len] [min_y 0])
    (for-each (lambda (v)
      ;; Make trend plot
      (let* ([name (car v)]                         ;; first element of main list
             [vmin (cadr v)]                        ;; = (car (cdr v)) = second element of main list
             [vmax (caddr v)]                       ;; = (car (cdr (cdr v) ) ) = third element ...
             [color (cadddr v)]                     ;; = (car (cdr (cdr (cdr v) ) ) ) = fourth element ...
             [trace-h (list-ref v 7)]               ;; seventh element of main list
             [traceoffset (list-ref v 8)]           ;; eighth element of main list
             [trace (string-append name "-trace")]
             [wave (string-append name "-wave")])
        ;; Define trace to plot waveform
        (let ([trc (make-gltrace ws trace-h GLTRACE_SHIFT vmin vmax vmin vmax)])
          (store-set! s trace trc)
          ;; Clear the trace
          (gltrace:clear trc)
          ;; Make a box
          (glgui-box g (+ x 5) (- (glgui-height-get) 50 (* 120 traceoffset)) (+ w 40) 110 color)
          ;; Place the trace widget
          (store-set! s wave (
            glgui-trace-slider                       
              g                                     ;; GUI container widget 
              (+ x 5)                               ;; LLC x-coord (pixels)
              (- (glgui-height-get) 50 (* 120 traceoffset)) ;; LLC y-coord (pixels)
              (+ w 40)                              ;; width (pixels)
              110                                   ;; height (pixels)
              trc                                   ;; data
              color                               
              ascii_16.fnt))
        )
        (set! min_y (- (glgui-height-get) 50 (* 120 traceoffset)))
      )
      ;; Trend numbers
      (let ([value (string-append (car v) "-value")]
            [color (cadddr v)]
            [lbl (list-ref v 4)]
            [yoffset (list-ref v 6)])
        (store-set! s value (glgui-valuelabel g (+ x w 100) (- (glgui-height-get) (* 120 yoffset)) lbl num_40.fnt color))
      ))
      vars
    )
    min_y
  )
)

(define (init-gui-trends)
  
  (set! gui:trends (make-glgui))
  ;; Create trend numbers and waveforms
  (set! gui:trends-h
    (make-trends 
      gui:trends 
      0 
      (- (glgui-height-get) 50) 
      store 
      trends
    )
  )

  ;; Marker trace
  (let ([trace (
    make-gltrace
      trend-len           ;; Width
      100                 ;; Height
      GLTRACE_SHIFT       ;; Mode (SHIFT, OVERWRITE, RESET)
      0                   ;; Lowest plotted value
      1                   ;; Highest plotted value
      0                   ;; Lower limit value, used if limit font set in glgui-trace
      1)])                ;; Upper limit value, used if limit font set in glgui-trace
  
    (store-set! 
      store 
      "EventMarker-trace" 
      trace
    )
    (gltrace:clear trace)
    (store-set! 
      store 
      "EventMarker-wave" 
      (glgui-trace 
        gui:trends        ;; GUI container widget                                         
        5                 ;; Lower left corner along the x-axis in pixels
        gui:trends-h      ;; Lower left corner along the x-axis in pixels
        (+ trend-len 40)  ;; Width (pixels)
        (- (glgui-height-get) 50 gui:trends-h ) ;; Height (pixels)
        trace             ;; Data to be traced
        Yellow            ;; Color
      )
    )
  )

  ;; Add a grid
  (let* (
      [y (- (glgui-height-get) 50 (* 120 6))]
      [h (* 120 6)]
      [w (+ trend-len 40)]
      [num (fx- gui:numtimes 1)]
      [bw 2]
    )
    (let loop ([x 5] [i 0])
      (if (fx> i num) #f
        (begin
          (glgui-box gui:trends x y bw (+ h (if (fx= i 0) 120 0)) DimGray)
          (store-set! store (string-append "time-" (number->string i))
            (glgui-label gui:trends (if (fx< x 17) 0 (fx- x 17)) (fx- y 20) 60 16 "" ascii_16.fnt DarkGray))
          (loop (fx+ x (fix (/ w num))) (fx+ i 1))
        )
      )
    )
  )

)

;; (update-trends store)
;;
;; Update the trends every delta-time using data from STORE
(define last-trend-update 0)
(define show-recording-once #f)
(define (update-trends store)
  (if (> (- ##now last-trend-update) delta-update)
    (begin
      ;; Update the Trend Traces including Marker lines
      (for-each (lambda (trend)
                  (let* ((name (car trend))
                         (storename (list-ref trend 5))
                         (val (store-timedref store storename #f))
                         (trace (store-ref store (string-append name "-trace"))))
                    (gltrace-add trace val)
                    (gltrace-update trace)))
                (append trends (list (list "EventMarker" #f #f #f #f "EventMarker"))))
      (set! last-trend-update ##now)
      ;; Mark a new logging entry if found
      (let ((em (store-ref store "EventMarker")))
        (if em (store-set! store "EventMarker" (if (fl> em 0.) 0. #f)))
      )
      ;; Save a screenshot
      (if (and screenshot-time (fl> ##now screenshot-time)) (begin
        (set! screenshot-time #f)
        (screenshot->png (string-append (instance-refvar store "TRENDOUT" "CasePath" "")
          (system-pathseparator) (seconds->string ##now "%Y%m%d_%H%M%S") ".png"))
      ))
    )
  )
)

;; (update-values store)
;;
;; Update the values using data from STORE
(define (update-values store)
  (if (fl>= (fl- ##now (store:instance-ref store "DispatchStart" 0.))
              (store:instance-ref store "DispatchCount" 0.))
    (begin
      ;; Update the Trend Numerics
      (for-each (lambda (trend)
                  (let* ([name (car trend)]
                         [storename (list-ref trend 5)]
                         [val (store-timedref store storename #f)]
                         [label (store-ref store (string-append name "-value"))])
                    (glgui-widget-set! gui:trends label 'label (if val (number->string (fix val)) ""))))
                trends)
      ;; Update times everywhere
      (store-set! "main" "time_str" (seconds->string ##now "%H%M%S"))
      (glgui-widget-set! gui:main clock 'label (seconds->string ##now "%T"))
      ;; Update the other clocks too
      (let loop ((i 0) (t (- ##now trend-time)))
        (if (fx= i gui:numtimes) #f
          (let ((wgt (store-ref store (string-append "time-" (number->string i)))))
            (glgui-widget-set! gui:main wgt 'label (seconds->string t "%H:%M"))
            (loop (fx+ i 1) (fl+ t (flo (/ trend-time (fx- gui:numtimes 1)))))
          )
        )
      )
    )
  )
)

;; -----------------------------------------------------------------------------
;;  MAIN PROGRAM
;; -----------------------------------------------------------------------------
(main
  
;; Initialize
  (lambda (w h)
    
    (if 
      (or 
        (string=? (system-platform) "macosx")
        (string=? (system-platform) "linux")
        (string=? (system-platform) "win32")
      ) 
      ;; (1600 x 850) works for my X1 Carbon laptop
      ;; TODO: Obtain optimal window size from system
      (make-window (* 1600) (* 850))
    )
    
    (glgui-orientation-set! GUI_LANDSCAPE)
    
    ;; See if we can get a better list of OR names from the vitalnode
    (let* (
        [rupi:addr (
            with-exception-catcher
            (lambda (e) #f)
            (lambda () (car (host-info-addresses (host-info rupi_hostname))))
          )
        ]
        [rupi:port rupi_port]
        [rupi:key (u8vector 77 71 148 114 103 101 115 31)]
        [rc (rupi-client 0 rupi:key rupi:addr rupi:port)]
        [rooms (rupi-cmd rc "GETOVERVIEW" "")]
      )
      (if (pair? rooms) (set! or-list (sort (map car rooms) string<?)))
    )

    ;; Initialize the monitor connection
    (set! store (make-store "main"))
    
    ;; Initialize the gui
    (init-gui-main)
    (init-gui-setup)
    (init-gui-trends)

    ;;Make sure that the scheduler actually runs !!!
    (scheduler-init)
  
  )

;; Handle events
  (lambda (t x y)
    (update-trends "main")
    (update-values "main")
    ;; These are button presses
    (if (= t EVENT_KEYPRESS)
      (begin
	      (cond
	        ((= x EVENT_KEYESCAPE) ;; This is ESC
            (if quit-armed?
              (terminate)
              (begin
                (set! quit-armed? #t)
                (store-event-add store 1 "Press ESC again to quit!")
                (glgui-widget-set! gui:main log-list 'list (build-log-list))
              )
            )
          )
	        ((and (>= x 32) (< x 127)) ;; All printable ASCII chars (127 = delete)
	          (set! buf (string-append buf (string (integer->char x))))
	        )
	        ((= x EVENT_KEYBACKSPACE)
	          (if (> (string-length buf) 0) (set! buf (substring buf 0 (- (string-length buf) 1))))
	        )
          ((= x EVENT_KEYTAB) (for-each display (list (store-listcat "main" "remote") "\n")))
	        ((= x EVENT_KEYENTER)
	          (begin
	            (if (> (string-length buf) 0)
		            ;; If there is data in the string log it
		            (begin
                  (store-event-add store 1 buf)
		              (glgui-widget-set! gui:main log-list 'list (build-log-list))
		            )
	            )
	            (set! buf "")
	          )
	        )
	      )
        (glgui-widget-set! gui:main text 'label buf)
      )
    )
    (glgui-event (list gui:main gui:trends) t x y)

    ;; Garbage collect, sleep and iterate over new plugin data
    (##gc)                     ;; This calls the garbage collector
    (thread-sleep! 0.005)      ;; Sleep for 5 microseconds
    (scheduler-iterate)
  
  )

;; Terminate
  (lambda ()
    (scheduler-endcase "main")
    (scheduler-cleanup)
    #t
  )

;; Suspend
  (lambda () (glgui-suspend))

;; Resume
  (lambda () (glgui-resume))
)

;; eof

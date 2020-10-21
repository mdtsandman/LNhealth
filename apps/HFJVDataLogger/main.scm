;; Philips Waveform and Numerics Logger via VitalNode
;; Matthias Görges 2012-2017
;; Shaylene Beaudry 2017
;; Mike Traynor 2020


;; -----------------------------------------------------------------------------
;; Global variables
;; -----------------------------------------------------------------------------

(define debug #t)

(define server "prod")  ;; "demo" or "prod"

;; Dimensions that work with my Lenovo X1 Carbon laptop :)
(define screen_width 1600) (define screen_height 850)

(define rupi:envs
  (list
    (list "bcch-or.part-dns.org" 8031) ;; production environment (ivue)
    (list "ecem.ece.ubc.ca" 8080) ;; demo server (S5)
  )
)

(define rupi:key (u8vector 77 71 148 114 103 101 115 31))

(define trend_update_interval 10) ;;sec
(define trend_duration 7200) ;;sec
(define trend_len (fix (/ trend_duration trend_update_interval))) ;;sec
(define num_trend_ticks 5)
(define num_trend_rows 8)
(define trend_row_height 90)

(define screenshot_time #f)
(define quit_armed? #f)

(define or_list (list))      ;; list of all monitoring locations
(define subject_location #f) ;; monitoring location for current subject

(define markers_timeline 
  (list "Bronchoscopy started" "Bronchoscopy ended" "Intubation" "Incision" "Fistula ligation complete" "Surgery ended")
)
(define markers_events
  (list (list "Surgery paused:" #f) "Surgery resumed" (list "HFJV paused:" #f) "HFJV resumed")
)
(define markers_vent
  (list (list "PIP" #f) (list "PEEP" #f) (list "RR" #f) (list "Ti" #f) (list "FiO2" #f))
)

;; -----------------------------------------------------------------------------
;;  MAIN SCREEN
;; -----------------------------------------------------------------------------

;; (init-gui-main)
;;
;; The main gui parts: Logging list, Title row and all buttons are defined here.
(define gui:main #f)

(define (init-gui-main)
  
  ;(dbln "init-gui-main: begin")
  
  (set! gui:main (make-glgui))
  (glgui-menubar gui:main 0 (- (glgui-height-get) 50) (glgui-width-get) 50)
  (glgui-label gui:main 10 (- (glgui-height-get) 24 3) 350 24 "Caudal Data Logger" ascii_24.fnt White)
  (set! clock (glgui-label gui:main (- (glgui-width-get) 70) (- (glgui-height-get) 24) 60 16 "" ascii_16.fnt White))
  
  ;; Timeline buttons
  (let (
      [n (length markers_timeline)]
      [w 180]
      [x (- (glgui-width-get) 180 20)]
      [y (- (glgui-height-get) 85)]
    )
    (let loop ([i 0])
      (if (< i n)
        (let ([marker (list-ref markers_timeline i)])
          (if marker
            (set! bs (glgui-button-string gui:main x (- y (* 35 i)) w 30 marker ascii_16.fnt marker-callback))
            (glgui-widget-set! gui:main bs 'value -1)
          )
          (loop (+ i 1))
        )
      )
    )
  )
  
  ;; Event buttons
  (let (
      [n (length markers_events)]
      [w 180]
      [x (- (glgui-width-get) 180 20)]
      [y (- (glgui-height-get) 85 (* 35 (length markers_timeline)) 15)]
    )
    (let loop ([i 0])
      (if (< i n)
        (let ([marker (list-ref markers_events i)])
          (if marker
            (let ([cl (list? marker)])
              (let (
                  [str (if cl (car marker) marker)]
                  [cb (if cl marker-freetext-callback marker-callback)]
                )
                (set! bs (glgui-button-string gui:main x (- y (* 35 i)) w 30 str ascii_16.fnt cb))
                (glgui-widget-set! gui:main bs 'value -1)
                (if cl (glgui-widget-set! gui:main bs 'color CornflowerBlue))
              )
            )
          )
          (loop (+ i 1))
        )
      )
    )
  )
  
  ;; LifePulse settings buttons
  (let (
      [n (length markers_vent)]
      [w 120]
      [x (- (glgui-width-get) 180 20)]
      [y (- (glgui-height-get) 85 (* 35 (length markers_timeline)) 15 (* 35 (length markers_events)) 15)]
    )
    (let loop ([i 0])
      (if (< i n)
        (let ([marker (list-ref markers_vent i)])
          (if marker
            (let ([str (car marker)])
              (set! bs (glgui-button-string gui:main x (- y (* 35 i)) w 30 str ascii_16.fnt hfjv-settings-callback))
              (set! label (glgui-label gui:main (+ x w 15) (- y (* 35 i)) 100 30 "" ascii_24.fnt White))
              (glgui-widget-set! gui:main bs 'value -1)
              (glgui-widget-set! gui:main bs 'color Green)
            )
          )
          (loop (+ i 1))
        )
      )
    )
  )
  
  ;; Log widget
  (let (
      [x 1000]
      [y (- (glgui-height-get) 50 )]
      [w 380]
      [num_rows 18]
      [row_height 30]
    )
    (glgui-label gui:main (+ x  5) y 70         row_height "Time"      ascii_16.fnt White)
    (glgui-label gui:main (+ x 75) y (- w 75 5) row_height "Log Entry" ascii_16.fnt White)
    (set! text (glgui-inputlabel gui:main x (- y row_height) w row_height "" ascii_24.fnt White CornflowerBlue))
    (set! log-list (glgui-list gui:main x (- y (* (+ num_rows 1) row_height)) w (* num_rows row_height) row_height (build-log-list) #f))
  )

  ;(dbln "init-gui-main: end")

)

;; Other markers
(define (marker-callback g w t x y)
  (let* (
      [idx (glgui-widget-get g w 'value)] 
      [marker (car (glgui-widget-get g w 'image))]
    )
    (store-event-add store 0 marker)
    (store-set! store "EventMarker" 1.)
    (set! quit-armed? #f)
    (glgui-widget-set! gui:main log-list 'list (build-log-list))
    (if (string=? marker (car (reverse markers_timeline)))
      (set! screenshot-time (fl+ ##now 30.))
    )
    
  )
)

(define (marker-freetext-callback g w t x y)
  (let* (
      [idx (glgui-widget-get g w 'value)] 
      [marker (car (glgui-widget-get g w 'image))]
      [buf (glgui-widget-get gui:main text 'label)]
    )    
    (set! buf (string-append marker " "))
    (glgui-widget-set! g text 'label buf)
  )
)

;; (build-log-list)
;;
;; The build-log list creator, which loops through the log entries and makes appropriate gui elements for it
(define (build-log-list)
  (let ([logs (store-event-listnew store)])
    (if logs
      (let loop ([i 0] [result (list)])
        (if (= i (length logs)) result
          (loop (+ i 1) (append result (list (log-list-element (list-ref logs i)))))
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
    (glgui:draw-text-left (+ x 15) (+ y (/ (- h 16) 2)) 70 16 (seconds->string (car entry) "%T") ascii_16.fnt White)
    (glgui:draw-text-left (+ x 75) (+ y (/ (- h 24) 2)) (- w 90) 24 (cadr entry) ascii_24.fnt White)
  )
)


;; -----------------------------------------------------------------------------
;;  SETUP DIALOG
;; -----------------------------------------------------------------------------
(define (init-gui-setup)
  
  ;(dbln "init-gui-setup: begin")

  (let ([w 300] [h 270]) ;; dimensions of popup dialog for entry of setup info
    
    (let (
        ;; center the popup over the log widget
        [x (+ 1000 (/ (- 380 w) 2))]
        [y (- (/ (glgui-height-get) 2) (/ h 2))]
      ) 
      
      (set! gui:setup (glgui-container gui:main x y w h))
      (glgui-box gui:setup 0 0 w h CornflowerBlue)

      ;; title
      (set! setup_label (glgui-label gui:setup 20 (- h 45) (- w 40) 25 "Study Setup" ascii_24.fnt White))
      (glgui-widget-set! gui:setup setup_label 'align GUI_ALIGNCENTER)
      
      ;; patient
      (set! id_label (glgui-label gui:setup 20 (- h 55 30) 100 30 "Pt #: " ascii_24.fnt White))
      (glgui-widget-set! gui:setup id_label 'align GUI_ALIGNRIGHT)
      (set! setup_id (glgui-inputlabel gui:setup (+ 20 100) (- h 55 (* 30 0) 25) (- w 40 100) 25 "" ascii_24.fnt White (color-shade White 0.2)))
      (glgui-widget-set! gui:setup setup_id 'align GUI_ALIGNRIGHT)
      (glgui-widget-set! gui:setup setup_id 'focus #t)

      ;; age
      (set! age_label (glgui-label gui:setup 20 (- h 55 (* 30 2)) 100 30 "Age: " ascii_24.fnt White))
      (glgui-widget-set! gui:setup age_label 'align GUI_ALIGNRIGHT)
      (set! setup_age (glgui-inputlabel gui:setup (+ 20 100) (- h 55 (* 30 1) 25) (- w 40 100) 25 "" ascii_24.fnt White (color-shade White 0.2)))
      (glgui-widget-set! gui:setup setup_age 'align GUI_ALIGNRIGHT)
      
      ;; sex
      (set! sex_label (glgui-label gui:setup 20 (- h 55 (* 30 3)) 100 30 "Sex: " ascii_24.fnt White))
      (glgui-widget-set! gui:setup sex_label 'align GUI_ALIGNRIGHT)
      (set! setup_sex (glgui-button-string gui:setup (+ 20 100) (- h 55 (* 30 2) 25) (- w 40 100) 25 (list "Male" "Female") ascii_24.fnt #f))

      ;; location list
      (set! location-label (glgui-label gui:setup 20 (- h 55 (* 30 4) 5) 100 30 "Location: " ascii_24.fnt White))
      (glgui-widget-set! gui:setup location-label 'align GUI_ALIGNRIGHT)
      (set! location_dropdown 
        (glgui-dropdownbox 
          gui:setup 
          (+ 20 100) 
          (- h 55 (* 30 3) 25 10) 
          (- w 40 100) 
          35
          (map 
            (lambda (str) 
              (lambda (lg lw x y w h s)
                (if s (glgui:draw-box x y w h Grey))
                (glgui:draw-text-left (+ x 5) y (- w 10) h str ascii_24.fnt Black)
              )
            )
            or_list
          )
          Black 
          DarkGrey 
          Blue
        )
      )
      (glgui-widget-set! gui:setup location_dropdown 'callback location-callback)

      ;; start button
      (set! start_btn (glgui-button-string gui:setup (- (/ w 2) (/ 180 2)) 20 180 40 "Start Recording" ascii_24.fnt start-callback))
      (glgui-widget-set! gui:setup start_btn 'align GUI_ALIGNCENTER)
      
    )
  )
  ;(dbln "init-gui-setup: end")
)

;; (hfjv-settings-callback g w t x y)
;; (list "PIPhfjv" "Tihfjv" "RRhfjv" "FiO2hfjv")
(define (hfjv-settings-callback g w t x y)
  (set! pip  (string->number (glgui-widget-get gui:setup hfjv_pip  'label)))
  (set! fio2 (string->number (glgui-widget-get gui:setup hfjv_fio2 'label)))
  (set! rate (string->number (glgui-widget-get gui:setup hfjv_rate 'label)))
  (set! ti   (string->number (glgui-widget-get gui:setup hfjv_ti   'label)))
  (let* (
      [idx (glgui-widget-get g w 'value)] 
      [buf (glgui-widget-get gui:main text 'label)]
    )    
    (set! buf (string-append marker " "))
    (glgui-widget-set! g text 'label buf)
  )
)

;; (location-callback g w t x y)
;;
;; Select the OR room to monitor
(define (location-callback g w t x y)
  ;; Get data from rupi
  (let* (
      [rupi:env (if (string=? server "prod") (car rupi:envs) (cadr rupi:envs))]
      [cur (glgui-widget-get g w 'current)]
      [location (list-ref or_list cur)]
    )
    (if subject_location  ;; subject_location is a global var
      (begin
        (instance-setvar! store "VNmonitor" "Location" location)
        (store-clear! store (map car (store-listcat store "remote")))
      )
      (make-instance store "VNmonitor" "vitalnode" `("Location" ,location) `("Hostname" ,(car rupi:env)))
    )
    (set! subject_location location)
  )
)


;; (start-callback g w t x y)
;;
;; Start Recording data
(define (start-callback g w t x y)
  
  (set! subject-num (string->number (glgui-widget-get gui:setup setup_id 'label)))
  (set! subject-age (string->number (glgui-widget-get gui:setup setup_age 'label)))
  (set! subject-sex (car (list-ref (glgui-widget-get gui:setup setup_sex 'image)
                                   (glgui-widget-get gui:setup setup_sex 'value))))

  ;; Check that we have data
  (let ((remote-lst (store-listcat "main" "remote")))
    (set! subject-hasdata? (and (list? remote-lst) (not (null? remote-lst))))
  )
  
  ;; Check that we have everything set
  (if (and subject-num subject-age subject-sex subject-hasdata?)
    
    (begin

      ;; Record trend variables (trendoutput plugin)
      (make-instance 
        "main"                                ;; store name
        "ALLTRENDS"                           ;; name of plugin instance
        "trendoutput"                         ;; plugin name
        `("Trends"                            ;; configuration options to be passed to plugin    
          ,(append 
            (list "time_str")
            (append 
              ivue:physdatavalues_basic       ;; includes ivue_timestamp
              ivue:physdatavalues_aisys 
              ivue:physdatavalues_nirs
              ivue:physdatavalues_tcco2
              (list "PIPhfjv" "Tihfjv" "RRhfjv" "FiO2hfjv")
            )
          )
        )
      )
      
      ;; Record ivue waveforms (waveoutput plugin)
      (let ([waves (append ivue:waveform_basic ivue:waveforms_aisys)])
        (for-each 
          (lambda (l)
            (make-instance                    
              "main"                          ;; store name  
              (string-append "WAVEOUT" l)     ;; name of plugin instance
              "waveoutput"                    ;; plugin name
              `("Source" ,l)                  ;; configuration options to be passed to plugin
            )
         ) 
          waves
        )
      )
    
      ;; Start the scheduler
      (scheduler-startcase store
        (string-append "BCCH-" (number->string subject-num) "_" (time->timestamp (current-time))))
      
      ;; Log the demographics
      (store-event-add store 0 (string-append "Patient: BCCH-" (number->string subject-num)))
      (store-event-add store 0 (string-append "Location: " subject_location))
      (store-event-add store 0 (string-append "Age: " (number->string subject-age)))
      (store-event-add store 0 (string-append "Sex: " subject-sex))
      (glgui-widget-set! gui:main log-list 'list (build-log-list))
      
      ;; Hide the input box
      (glgui-widget-set! gui:main gui:setup 'hidden #t)
   
      ;; Focus the text-entry label
      (glgui-widget-set! gui:main text 'focus #t)

    )
  
  )

)


;; -----------------------------------------------------------------------------
;; TRENDS
;; -----------------------------------------------------------------------------

(define gui:trends #f)
;; vmin & vmax: define the scale for each waveform
;; label.img:   must correspond to an item in the file STRINGS
;; yoffset:     vertical offset of the numeric where 1.0 represents the width of one trend band?
;; traceh:      width of the trend band, in pixels?
;; traceoffset: ordinal vertical offset of the trace band to use for the waveform?

(define trends 
  (list
;;        name        vmin  vmax  color       label.img           storename   yoffset  trace-h  traceoffset
    (list "hr"        90    200   Green       label_hr.img        "HR"        1        90       1)
    (list "pulse"     90    200   DarkGreen   label_pulse.img     "PRSpO2"    1.3      90       1)
    (list "map_art"   25    100   Red         label_map_art.img   "ABPmean"   2        90       2)
    (list "map_nibp"  25    100   IndianRed   label_map_nibp.img  "NBPmean"   2.3      90       2)
    (list "spo2"      70    100   Aquamarine  label_spo2.img      "SpO2"      3        90       3)
    (list "rso2_1"    50    100   Blue        label_rso2_1.img    "rSO2-1"    4        90       4)
    (list "rso2_2"    50    100   LightBlue   label_rso2_2.img    "rSO2-2"    4.3      90       4)
    (list "pco2_tc"    0    150   Orange      label_pco2_tc.img   "tcpCO2"    5        90       5)
    (list "fio2_imv"  21    100   Blue        label_fio2_imv.img  "FiO2"      6        90       6)
    (list "rr_imv"     0     60   DimGray     label_rr_imv.img    "RR"        7        90       7)
    (list "pip_imv"    0     35   White       label_pip_imv.img   "PIP"       8        90       8)
    (list "peep_imv"   0     35   White       label_peep_imv.img  "PEEP"      8.3      90       8)
  )
)

(define hfjv_settings 
  (list
;;        name        color   label.img           storename   yoffset  
    (list "rr_hfjv"   White   label_hr.img        "HR"        1)
    (list "ti_hfjv"   White   label_spo2.img      "SpO2"      3)
    (list "pip_hfjv"  White   label_pulse.img     "PRSpO2"    1.3)
    (list "fio2_hfjv" White   label_map_nibp.img  "NBPmean"   2.3)
  )
)


(define (make-trends g x y0 s vars)
  ;(dbln "make-trends: begin")
  (let* ([w trend_len] [h 100] [y (- y0 h)] [ws trend_len] [min_y 0])
    (glgui-box g (+ x 20) (- (glgui-height-get) 50) (+ w 40) 1 DimGray) ; HLine above top trend widget
    (for-each 
      (lambda (v)
        ;; graphical trend
        (let* (
            [name (car v)] [vmin (cadr v)] [vmax (caddr v)] [color (cadddr v)] [trace-h (list-ref v 7)]
            [traceoffset (list-ref v 8)] [trace (string-append name "-trace")] [wave (string-append name "-wave")]
          )
          (let ([trc (make-gltrace ws trace-h GLTRACE_SHIFT vmin vmax vmin vmax)])
            (store-set! s trace trc)
            (gltrace:clear trc)
            (glgui-box g (+ x 20) (- (glgui-height-get) 50 (* trend_row_height traceoffset)) (+ w 40) 1 DimGray) ; HLine below each trend widget
            (store-set! 
              s
              wave
              (glgui-trace-slider g (+ x 20) (- (glgui-height-get) 50 (* 90 traceoffset)) (+ w 40) 90 trc color ascii_16.fnt)
            )
          )
          (set! min_y (- (glgui-height-get) 50 (* 90 traceoffset)))
        )
        ;; label for numeric
        (let ([value (string-append (car v) "-value")] [color (cadddr v)] [lbl (list-ref v 4)] [yoffset (list-ref v 6)])
          (store-set! s value (glgui-valuelabel g (+ x w 175) (- (glgui-height-get) (* 90 yoffset)) lbl num_40.fnt color))
        )
      )
      vars
    )
    ;(dbln "make-trends: end")
    min_y ; return value
  )
)

(define (init-gui-trends)
  ;(dbln "init-gui-trends: begin")
  (set! gui:trends (make-glgui))
  ;iVue trends
  (set! gui:trends-h (make-trends gui:trends 0 (- (glgui-height-get) 50) store trends))
  ;HFJV settings
  (make-hfjv-settings-trends gui:main store trends)
  ;Event markers
  (let ( 
      [trace
        (make-gltrace   ;; This is the trace itself
          trend_len     ;; Width
          100           ;; Height
          GLTRACE_SHIFT ;; Mode (SHIFT, OVERWRITE, RESET)
          0             ;; Lowest plotted value
          1             ;; Highest plotted value
          0             ;; Lower limit (used if limit font set in glgui-trace)
          1             ;; Upper limit (used if limit font set in glgui-trace)
        )
      ]
    ) 
    (store-set! store "EventMarker-trace" trace )
    (gltrace:clear trace)
    (store-set! 
      store 
      "EventMarker-wave" 
      (glgui-trace                             ;; This is the widget in which the trace is displayed
        gui:trends                             ;; Parent widget                                         
        20                                     ;; LLC x-axis (pixels)
        gui:trends-h                           ;; LLC y-axis (pixels)
        (+ trend_len 40)                       ;; Width (pixels)
        (- (glgui-height-get) 50 gui:trends-h) ;; Height (pixels)
        trace                                  ;; Data to be traced
        Yellow                                 ;; Color
      )
    )  
  )
  ;(dbln "Grid with time markers")
  (let* (
      [y (- (glgui-height-get) 50 (* trend_row_height num_trend_rows))]
      [h (* trend_row_height num_trend_rows)]
      [w (+ trend_len 40)]
      [num (fx- num_trend_ticks 1)]
      [bw 1]
    )
    (let loop ([x 20] [i 0])
      (if (fx> i num) #f
        (begin
          (glgui-box gui:trends x y bw h DimGray)
          (store-set! store (string-append "time-trends-" (number->string i))
            (glgui-label gui:trends (if (fx< x 17) 0 (fx- x 17)) (fx- y 20) 60 16 "" ascii_16.fnt White))
          (loop (fx+ x (fix (/ w num))) (fx+ i 1))
        )
      )
    )
  )
  ;(dbln "init-gui-trends: end")
)

;; (update-trends store)
;;
;; Update the trends every interval using data from STORE
(define last_trend_update 0)

(define (update-trends store)
  
  ;(dbln "update-trends : begin")
  
  (if (> (- ##now last_trend_update) trend_update_interval)
    
    (begin
      ;; Update the trend traces, including marker lines
      (for-each 
        (lambda (trend)
          (let* (
              [name (car trend)]
              [storename (list-ref trend 5)]
              [val (store-timedref store storename #f)]
              [trace (store-ref store (string-append name "-trace"))]
            )
            (gltrace-add trace val)
            (gltrace-update trace)
          )
        )
        (append trends (list (list "EventMarker" #f #f #f #f "EventMarker")))
      )
      
      (dbln trends)

      ;; Reset last update time
      (set! last_trend_update ##now)
      ;; Mark a new logging entry, if one is found
      (let ([em (store-ref store "EventMarker")])
        (if em (store-set! store "EventMarker" (if (fl> em 0.) 0. #f)))
      )
    )
  
  )
  
  ;(dbln "update-trends : end")

)

;; (update-values store)
;;
;; Update the values using data from STORE
(define (update-values store)
  
  ;(db "update-values : begin\n")
  
  (if (fl>= (fl- ##now (store:instance-ref store "DispatchStart" 0.)) (store:instance-ref store "DispatchCount" 0.))
    
      ;(dbln "trend numerics ...")
      (for-each 
        (lambda (trend)
          (let* (
              [name (car trend)]
              [storename (list-ref trend 5)]
              [val (store-timedref store storename #f)]
              [label (store-ref store (string-append name "-value"))]
            )
            ;(dbln (list "[" name "|" storename "|" val "|" label "]")) 
            (glgui-widget-set! gui:trends label 'label (if val (number->string (fix val)) ""))
          )
        )
        trends
      )
      
      ;(dbln "trend viewer ticks ...")
      (let loop (
          [i 0]
          [t (- ##now trend_duration)]
        )
        (if (fx= i num_trend_ticks) #f
          (let ([wgt (store-ref store (string-append "time-trends-" (number->string i)))])
            (glgui-widget-set! gui:main wgt 'label (seconds->string t "%H:%M"))
            (loop (fx+ i 1) (fl+ t (flo (/ trend_duration (fx- num_trend_ticks 1)))))
          )
        )
      )
      
      ;(dbln "clock ...")
      (glgui-widget-set! gui:main clock 'label (seconds->string ##now "%T"))
    
    )
  
  )
  
  ;(dbln "update-values : end")

)

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

;; Check for empty lists
(define (list-notempty? lst)
  (and (list? lst) (not (null? lst)))
)

(define (db data . args)
  (let (
      [lst (if (list? data) data (list data))]
      [show (if (list-notempty? args) (car args) #t)]
    )
    (if (and debug show)
      (for-each
        (lambda (item) 
          (display item)
        )
        lst
      )
    )
  )
)

(define (dbln data . args)
  (if (list? data)
    (db (append data "\n") args)
    (db (list data "\n") args)
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
      (make-window (* screen_width) (* screen_height))
    )
    
    (glgui-orientation-set! GUI_LANDSCAPE)
    
    ;; See if we can get a better list of OR names from the vitalnode
    (let* (
        [rupi:env (if (string=? server "prod") (car rupi:envs) (cadr rupi:envs))]
        [rupi:host (car rupi:env)]
        [rupi:port (cadr rupi:env)]
        [rupi:addr (
            with-exception-catcher
            (lambda (e) #f)
            (lambda () (car (host-info-addresses (host-info rupi:host))))
          )
        ]
        [rc (rupi-client 0 rupi:key rupi:addr rupi:port)] ;; rupi:key is global
        [rooms (rupi-cmd rc "GETOVERVIEW" "")]
      )
      
      (if (pair? rooms) 
        (begin
          (set! or_list (sort (map car rooms) string<?))
          (for-each (lambda (room) (dbln room)) rooms)  ;; display data available for online rooms
        )
        (begin
          (display "Unable to get location list ... terminating\n")
          (terminate)
        )
      )
    )

    ;; Initialize the store (connection to monitor)
    (set! store (make-store "main"))
    
    ;; Initialize the gui
    (init-gui-main)
    (init-gui-setup)
    (init-gui-trends)

    ;; Start the scheduler
    (scheduler-init)
  
  )

  ;; Handle events
  (lambda (t x y)

    (update-trends "main")
    (update-values "main")
    
    (if (= t EVENT_KEYPRESS)
      (if (glgui-widget-get gui:main gui:setup 'hidden) ;; Ignore keypresses when setup dialog visible
        (cond 
          [(and (= x EVENT_KEYESCAPE) quit_armed?)
            (terminate) ;; ESC (x2 consecutive) terminates program
          ]
          [(= x EVENT_KEYESCAPE)
            (begin
              (set! quit_armed? #t) ;; ESC (x1) arms quit
              (store-event-add store 1 "Press ESC again to quit!")
    		      (glgui-widget-set! gui:main log-list 'list (build-log-list))
          )
          ]
          [quit_armed?
            (begin
              (set! quit_armed? #f) ;; Any key other than ESC disarms quit.
              (store-event-add store 1 "Quit sequence aborted")
  		        (glgui-widget-set! gui:main log-list 'list (build-log-list))
            )
          ]
          [(= x EVENT_KEYENTER)
	          (begin
              (let ([buf (glgui-widget-get gui:main text 'label)])
	              (if (> (string-length buf) 0)
		              ;; If there is data in the string, log it
		              (begin
                    (store-event-add store 1 buf)
  		              (glgui-widget-set! gui:main log-list 'list (build-log-list))
                    (glgui-widget-set! gui:main text 'label "")
  	  	          )
	              )
	            )
            )
          ]
        )
      )
      (glgui-event (list gui:main gui:trends) t x y )
    )
    
    ;; Garbage collect, sleep and iterate over new plugin data
    (##gc)                    ;; This calls the garbage collector
    (thread-sleep! 0.005)     ;; Sleep for 5 microseconds
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

;; Philips Waveform and Numerics Logger via VitalNode
;; Matthias Görges 2012-2017
;; Shaylene Beaudry 2017
;; Mike Traynor 2020

;; Global variables

;(define rupi_hostname "bcch-or.part-dns.org") ;; prod
;(define rupi_port 8031)                       ;; prod

(define rupi_hostname "ecem.ece.ubc.ca")      ;; demo
(define rupi_port 8080)                       ;; demo

(define screen_width 1600)
(define screen_height 850)

(define trend_update_interval 10) ;;sec
(define trend_duration 7200) ;;sec
(define trend_len (fix (/ trend_duration trend_update_interval))) ;;sec
(define num_trend_ticks 5)

(define wave_update_interval 0.1) ;;sec
(define wave_duration 140) ;;sec
(define wave_len (fix (/ wave_duration wave_update_interval))) ;;sec
(define num_wave_ticks 10)

(define screenshot_time #f)
(define quit_armed? #f)

(define or-list (list))      ;; list of all monitoring locations
(define subject-location #f) ;; monitoring location for current subject

(define markers_timeline 
  (list "Caudal injection start" "Caudal injection end" "Surgery start" "Surgery end")
)
(define markers_events
  (list "Patient movement" (list "Fentanyl bolus:" #f) (list "Propofol bolus:" #f))
)




;; -----------------------------------------------------------------------------
;;  MAIN SCREEN
;; -----------------------------------------------------------------------------

;; (init-gui-main)
;;
;; The main gui parts: Logging list, Title row and all buttons are defined here.
(define gui:main #f)

(define (init-gui-main)
  ;; Instantiate the gui
  (set! gui:main (make-glgui))
  ;; Menubar
  (glgui-menubar gui:main 0 (- (glgui-height-get) 50) (glgui-width-get) 50)
  ;; Label in upper left corner
  (glgui-label gui:main 10 (- (glgui-height-get) 24 3) 350 24 "Caudal Data Logger" ascii_24.fnt White)
  ;; Clock in upper right corner
  (set! clock (glgui-label gui:main (- (glgui-width-get) 70) (- (glgui-height-get) 24) 60 16 "" ascii_16.fnt White))

  ;; Timeline buttons
  (let ([w 180] [x (- (glgui-width-get) 180 20)] [y (- (glgui-height-get) 85) ])
    (let loop ([i 0])
      (if (< i (length markers_timeline))
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
  (let ([w 180] [x (- (glgui-width-get) 180 20)] [y (- (glgui-height-get) 100 (* 35 4)) ])
    (let loop ([i 0])
      (if (< i (length markers_events))
        (let ([marker (list-ref markers_events i)])
          (if marker
            (let ([cl (list? marker)])
              (let 
                ([str (if cl (car marker) marker)] [cb (if cl marker-freetext-callback marker-callback)])
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

  ;; Logging List
  (let ( [x 1000] [y (- (glgui-height-get) 50 )] [w 380] [num_rows 18] [row_height 30] )
    ;;Header row
    (glgui-label gui:main (+ x  5) y 70         row_height "Time"      ascii_16.fnt White)
    (glgui-label gui:main (+ x 75) y (- w 75 5) row_height "Log Entry" ascii_16.fnt White)
    ;;Text Entry String
    (set! text (glgui-inputlabel gui:main x (- y row_height) w row_height "" ascii_24.fnt White CornflowerBlue))
    ;;The actual list 
    (set! log-list (glgui-list gui:main x (- y (* (+ num_rows 1) row_height)) w (* num_rows row_height) row_height (build-log-list) #f))
  ) 

) ;; end of init-gui-main definition

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
    
    ;; Arm screenshot
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
  (let ([w 300] [h 270]) ;; dimensions of popup dialog for entry of setup info
    (let ([x (+ 1000 (/ (- 380 w) 2))] [y (- (/ (glgui-height-get) 2) (/ h 2))]) ;; center the popup over the log widget
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
      (set! location-dropdown (glgui-dropdownbox gui:setup (+ 20 100) (- h 55 (* 30 3) 25 10) (- w 40 100) 35
        (map (lambda (str) (lambda (lg lw x y w h s)
          (if s (glgui:draw-box x y w h Grey))
            (glgui:draw-text-left (+ x 5) y (- w 10) h str ascii_24.fnt Black)))
          or-list)
        Black DarkGrey Blue))
      (glgui-widget-set! gui:setup location-dropdown 'callback location-callback)

      ;; start button
      (set! start_btn (glgui-button-string gui:setup (- (/ w 2) (/ 180 2)) 20 180 40 "Start Recording" ascii_24.fnt start-callback))
      (glgui-widget-set! gui:setup start_btn 'align GUI_ALIGNCENTER)
   
    )
  )
)

;; (location-callback g w t x y)
;;
;; Select the OR room to monitor
(define (location-callback g w t x y)
  ;; Get data from rupi
  (let* (
      [cur (glgui-widget-get g w 'current)]
      [location (list-ref or-list cur)]
    )
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

      ;; Record the trend variables (trendoutput plugin)
      (make-instance 
        "main"                                ;; store name
        "ALLTRENDS"                           ;; name of plugin instance
        "trendoutput"                         ;; plugin name
        `("Trends"                            ;; configuration options to be passed to plugin
          ,(append (list "time_str")
            (append 
              ivue:physdatavalues_basic 
              ivue:physdatavalues_aisys 
              ivue:physdatavalues_nirs
            )
          )
        )
      )
      
      ;; Record the waveforms
      (let ((waves (append ivue:waveform_basic ivue:waveforms_aisys)))
        (for-each 
          (lambda (l) 
            (make-instance
              "main"
              (string-append "WAVEOUT" l)
              "waveoutput" 
              `("Source" ,l)
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
      (store-event-add store 0 (string-append "Location: " subject-location))
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
;;  CONTAINER WIDGET FOR TRENDS
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
    (list "hr"        75    225   Green       label_hr.img        "HR"        1        90       1)
    (list "map_nibp"   0    100   IndianRed   label_map_nibp.img  "NBPmean"   1.4      90       1)
    (list "fio2"       0    100   Blue        label_fio2.img      "FiO2"      2        90       2)
    (list "spo2"       0    100   Aquamarine  label_spo2.img      "SpO2"      2.4      90       2)
    (list "pco2_et"    0    100   Orange      label_pco2_et.img   "ETCO2"     3        90       3)
    (list "rr"         0     60   DimGray     label_rr.img        "RR"        4        90       4)
    (list "pip"        0     40   White       label_pip.img       "PIP"       5        90       5)
    (list "peep"       0     40   White       label_peep.img      "PEEP"      5.4      90       5)
    (list "rso2"      50    100   Blue        label_rso2.img      "rSO2-1"    6        90       6)
  )
)

;; Plotting functions
;; g:     container widget
;; x:     LLC x-coord (pixels)
;; y0:    LLC y-coord (pixels)?
;; s:     store
;; vars:  list of all vars to be trended (name, vmin, vmax, color, label.img, storename, yoffset, trace-h, traceoffset)
(define (make-trends g x y0 s vars)
  (let* ([w trend_len] [h 100] [y (- y0 h)] [ws trend_len] [min_y 0])
    ;; Draw a horizontal line above the top trace widget
    (glgui-box g (+ x 20) (- (glgui-height-get) 50) (+ w 40) 1 DimGray)
    (for-each 
      (lambda (v)
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
            ;; Draw a horizontal line below the trace widget
            (glgui-box g (+ x 20) (- (glgui-height-get) 50 (* 90 traceoffset)) (+ w 40) 1 DimGray)
            ;; Place the trace widget
            (store-set! s wave (
              glgui-trace-slider                       
                g                                     ;; GUI container widget 
                (+ x 20)                              ;; LLC x-coord (pixels)
                (- (glgui-height-get) 50 (* 90 traceoffset)) ;; LLC y-coord (pixels)
                (+ w 40)                              ;; width (pixels)
                90                                    ;; height (pixels)
                trc                                   ;; data
                color                               
                ascii_16.fnt))
          )
          (set! min_y (- (glgui-height-get) 50 (* 90 traceoffset)))
        )
        ;; Show each numeric beside the appropriate trace widgets
        (let ([value (string-append (car v) "-value")]
              [color (cadddr v)]
              [lbl (list-ref v 4)]
              [yoffset (list-ref v 6)])
          (store-set! s value (glgui-valuelabel g (+ x w 150) (- (glgui-height-get) (* 90 yoffset)) lbl num_40.fnt color))
        )
      )
      vars
    )
    min_y
  )
)

(define (init-gui-trends)
  
  (set! gui:trends (make-glgui))

  ;; Create trend numbers and waveforms
  (set! gui:trends-h (make-trends gui:trends 0 (- (glgui-height-get) 50) store trends))

  ;; Marker trace
  (let
    ( [trace
        (make-gltrace    ;; This is the trace itself
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
  
    (store-set! 
      store 
      "EventMarker-trace" 
      trace
    )

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

  ;; Add a grid with time markers at bottom
  (let* (
      [y (- (glgui-height-get) 50 (* 90 6))]
      [h (* 90 6)]
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

)

;; (update-trends store)
;;
;; Update the trends every interval using data from STORE
(define last_trend_update 0)

(define (update-trends store)
  (if (> (- ##now last_trend_update) trend_update_interval)
    (begin
      ;; Update the trend traces, including marker lines
      (for-each (lambda (trend)
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
      ;; Reset last update time
      (set! last_trend_update ##now)
      ;; Mark a new logging entry, if one is found
      (let ((em (store-ref store "EventMarker")))
        (if em (store-set! store "EventMarker" (if (fl> em 0.) 0. #f)))
      )
    )
  )
)

;; (update-values store)
;;
;; Update the values using data from STORE
(define (update-values store)
  (if (fl>= (fl- ##now (store:instance-ref store "DispatchStart" 0.)) (store:instance-ref store "DispatchCount" 0.))
    (begin
      ;; Update numerics (trends)
      (for-each 
        (lambda (trend)
          (let* (
              [name (car trend)]
              [storename (list-ref trend 5)]
              [val (store-timedref store storename #f)]
              [label (store-ref store (string-append name "-value"))]
            )
            (glgui-widget-set! gui:trends label 'label (if val (number->string (fix val)) ""))
          )
        )
        trends
      )
      ;; Update numerics (waves)
      (for-each 
        (lambda (wave)
          (let* (
              [name (car wave)]
              [storename (list-ref wave 5)]
              [val (store-timedref store storename #f)]
              [label (store-ref store (string-append name "-value"))]
            )
            (glgui-widget-set! gui:waves label 'label (if val (number->string (fix val)) ""))
          )
        )
        waves
      )
     ;; Update ticks (trends)
      (display "Updating ticks (trends)\n")
      (let loop ((i 0) (t (- ##now trend_duration)))
        (if (fx= i num_trend_ticks) #f
          (let ([wgt (store-ref store (string-append "time-trends-" (number->string i)))])
            (glgui-widget-set! gui:main wgt 'label (seconds->string t "%H:%M"))
            (loop (fx+ i 1) (fl+ t (flo (/ trend_duration (fx- num_trend_ticks 1)))))
          )
        )
      )
      ;; Update ticks (waves)
      (display "Updating ticks (waves)\n")
      (let loop ((i 0) (t (- ##now wave_duration)))
        (if (fx= i num_wave_ticks) #f
          (let ([wgt (store-ref store (string-append "time-waves-" (number->string i)))])
            (glgui-widget-set! gui:main wgt 'label (seconds->string t "%H:%M:%S"))
            (loop (fx+ i 1) (fl+ t (flo (/ wave_duration (fx- num_wave_ticks 1)))))
          )
        )
      )
      ;; Update clock
      (glgui-widget-set! gui:main clock 'label (seconds->string ##now "%T"))
    )
  )
)

;; -----------------------------------------------------------------------------
;;  CONTAINER WIDGET FOR WAVES
;; -----------------------------------------------------------------------------
(define gui:waves #f)
;; vmin & vmax: define the scale for each waveform
;; label.img:   must correspond to an item in the file STRINGS
;; yoffset:     vertical offset of the numeric where 1.0 represents the width of one trend band?
;; traceh:      width of the trend band, in pixels?
;; traceoffset: ordinal vertical offset of the trace band to use for the waveform?
(define waves
  (list
;;        last_update name        vmin  vmax  color       label.img           storename   yoffset  trace-h  traceoffset   duration  interval
    (list (- ##now 1)      "icp"       0     100   Yellow      label_icp.img       "ICP"       0.5      200      1             140       0.1)
  )
)

(define (make-waves g x y0 s vars)
  (display "make-waves : begin\n")
  (let* ([w wave_len] [h 100] [y (- y0 h)] [ws wave_len] [min_y 0])
    ;; Draw a horizontal line above the top trace widget
    (glgui-box g (+ x 20) (- (glgui-height-get) 650) (+ w 40) 1 DimGray)
    (for-each
      (lambda (v)
        ;; Make trend plot
        (let* ([name (cadr v)]                         
               [vmin (caddr v)]                        
               [vmax (cadddr v)]
               [color (list-ref v 4)]    
               [trace-h (list-ref v 8)]        
               [traceoffset (list-ref v 9)]    
               [trace (string-append name "-trace")]
               [wave (string-append name "-wave")])
          ;; Define trace to plot waveform
          (let ([trc (make-gltrace ws trace-h GLTRACE_SHIFT vmin vmax vmin vmax)])
            (store-set! s trace trc)
            ;; Clear the trace
            (gltrace:clear trc)
            ;; Draw a horizontal line below the trace widget
            (glgui-box g (+ x 20) (- (glgui-height-get) 650 (* 150 traceoffset)) (+ w 40) 1 DimGray)
            ;; Place the trace widget
            (store-set! s wave (
              glgui-trace-slider                       
                g                                     ;; GUI container widget 
                (+ x 20)                              ;; LLC x-coord (pixels)
                (- (glgui-height-get) 650 (* 150 traceoffset)) ;; LLC y-coord (pixels)
                (+ w 40)                              ;; width (pixels)
                150                                   ;; height (pixels)
                trc                                   ;; data
                color                               
                ascii_16.fnt))
          )
          (set! min_y (- (glgui-height-get) 650 (* 150 traceoffset)))
        )
        ;; Show each numeric beside the appropriate trace widgets
        (let ([value (string-append (cadr v) "-value")]
              [color (list-ref v 4)]
              [lbl (list-ref v 5)]
              [yoffset (list-ref v 7)])
          (store-set! s value (glgui-valuelabel g (+ x w 150) (- (glgui-height-get) 650 (* 150 yoffset)) lbl num_40.fnt color))
        )
      )
      vars
    )
    min_y
  )
  (display "make-waves : end\n")
)

(define (init-gui-waves)
  (display "init-waves : begin\n")
  (set! gui:waves (make-glgui))
  ;; Create waveforms and numerics
  (set! gui:waves-h (make-waves gui:waves 0 (- (glgui-height-get) 650) store waves))
  ;; Add a grid with time markers at bottom
  (let* (
      [y (- (glgui-height-get) 650 (* 150 1))]
      [h (* 150 1)]
      [w (+ wave_len 40)]
      [num (fx- num_wave_ticks 1)]
      [bw 1]
    )
    (let loop ([x 20] [i 0])
      (if (fx> i num) #f
        (begin
          (glgui-box gui:waves x y bw h DimGray)
          (store-set!
            store 
            (string-append "time-waves-" (number->string i))
            (glgui-label 
              gui:waves 
              (if (fx< x 17) 0 (fx- x 17))  ;; LLC x
              (fx- y 20)                    ;; LLC y
              60                            ;; width
              16                            ;; height
              ""                            ;; text
              ascii_16.fnt
              Yellow
            )
          )
          (loop (fx+ x (fix (/ w num))) (fx+ i 1))
        )
      )
    )
  )
  (display "init-waves : end\n")
)

;; (update-waves store)
;;
;; Update the waves every interval using data from STORE
(define (update-waves store)
  (display "update-waves : begin\n")
  ;; Update the wave traces
  (for-each 
    (lambda (wave)
      (let* (
          [last_update (car wave)]
          [name (cadr wave)]
          [storename (list-ref wave 6)]
          [interval (list-ref wave 11)]
          [val (store-timedref store storename #f)]
          [trace (store-ref store (string-append name "-trace"))]
        )
        (display wave) (display "\n")
        (if (> (- ##now last_update) interval)
          (begin
            (display "gltrace-add trace val\n")
            (gltrace-add trace val)
            (display "gltrace-update trace\n")
            (gltrace-update trace)
            (display "set-car! wave ##now\n")
            (set-car! wave ##now) ;; Reset last update time
          )
        )
      )
    )
    (display "appending event markers\n")
    (append waves (list (list "EventMarker" #f #f #f #f "EventMarker")))
  )
  (display "update-waves : end\n")
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

    ;; Initialize the store (connection to monitor)
    (set! store (make-store "main"))
    
    ;; Initialize the gui
    (init-gui-main)
    (init-gui-setup)
    (init-gui-trends)
    (init-gui-waves)

    ;; Start the scheduler
    (scheduler-init)
  
  )

  ;; Handle events
  (lambda (t x y)
    (store-set! "main" "time_str" (seconds->string ##now "%H%M%S"))
    (update-trends "main")
    (update-waves  "main")
    (update-values "main")
    (if (= t EVENT_KEYPRESS)
      (if (glgui-widget-get gui:main gui:setup 'hidden) ;; Ignore keypresses when setup dialog visible
        (cond 
          [(and (= x EVENT_KEYESCAPE) quit-armed?)
            (terminate) ;; ESC (x2 consecutive) terminates program
          ]
          [(= x EVENT_KEYESCAPE)
            (begin
              (set! quit-armed? #t) ;; ESC (x1) arms quit
              (store-event-add store 1 "Press ESC again to quit!")
    		      (glgui-widget-set! gui:main log-list 'list (build-log-list))
            )
          ]
          [quit-armed?
            (begin
              (set! quit-armed? #f) ;; Any key other than ESC disarms quit.
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
      (glgui-event 
        (list 
          gui:main
          gui:trends
          gui:waves
        ) 
        t 
        x 
        y
      )
    )
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

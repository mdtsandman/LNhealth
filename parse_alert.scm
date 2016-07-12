;; Philips Intellivue Parser
;; Matthias Görges, 2016

(define (ivueparser:parseDevAlarmList buf palarm?)
  (let* ((count (u8data-u16 (subu8data buf 0 2)))
         (len (u8data-u16 (subu8data buf 2 4)))
         (al_prefix (string-append (if palarm? "p" "t") "_alarm"))
         (al_lst (string-append al_prefix "_lst")))
    (store-clear! ivueparser:store (store-ref ivueparser:store al_lst))
    (store-set! ivueparser:store al_lst '())
    (let loop ((n 0)(p (u8data-skip buf 4)))
      (if (fx= n count)
        p
        (loop (fx+ n 1) (ivueparser:parseDevAlarmEntry p n al_prefix))
      )
    )
  ))

(define (ivueparser:parseDevAlarmEntry buf al_ct prefix)
  (let ((al_source (u8data-u16 (subu8data buf 0 2)))
        (al_code (u8data-u16 (subu8data buf 2 4)))
        (al_type (u8data-u16 (subu8data buf 4 6)))
        (al_state (u8data-u16 (subu8data buf 6 8)))
        (alert_info_id (u8data-u16 (subu8data buf 14 16)))
        (len (u8data-u16 (subu8data buf 16 18))))
    (let* ((name (table-ref ivueparser:phystable1 al_source "???"))
           (priostr (if (= al_type LOW_PRI_P_AL) "*" (if (= al_type MED_PRI_P_AL) "**"
                      (if (= al_type HI_PRI_P_AL) "***" ""))))
           (msg0 (car (table-ref ivueparser:alarmtable al_code '(""))))
           (msg (string-replace-substring msg0 "XXXXXX" name))
           (al_lst_name (string-append prefix "_lst"))
           (al_lst (store-ref ivueparser:store al_lst_name '()))
           (al_ct_name (string-append prefix (number->string al_ct))))
      (store-set! ivueparser:store al_ct_name msg "ivue")
      (store-set! ivueparser:store al_lst_name (append al_lst (list al_ct_name)))
      ;; New alarms
      (if (and (fx= al_state 8) (fx> (string-length msg) 0))
        (store-event-add ivueparser:store 0 (store-ref ivueparser:store "location" ivueparser:store) msg))
      ;; Parse alarm internals
      (cond
        ((fx= alert_info_id STR_ALMON_INFO)
          (ivueparser:parseStrAlMonInfo (subu8data buf 18 (fx+ len 18)) al_ct_name))
        ((fx= alert_info_id GEN_ALMON_INFO)
          (ivueparser:parseAlMonGenInfo (subu8data buf 18 (fx+ len 18)) al_ct_name))
        (else (ivueparser:log 2 "ivueparser: unknown alert_info_id:" alert_info_id))
      )
    )
    (u8data-skip buf (fx+ len 18))
  ))

(define (ivueparser:parseAlMonGenInfo buf al_ct_name)
  (let ((al_inst_no (u8data-u16 (subu8data buf 0 2)))
        (al_text (u8data-u32 (subu8data buf 2 6)))
        (priority (u8data-u16 (subu8data buf 6 8)))
        (flags (u8data-u16 (subu8data buf 8 10))))
    (store-set! ivueparser:store (string-append al_ct_name "_prio") priority)
    (u8data-skip buf 10)
  ))

(define (ivueparser:parseStrAlMonInfo buf al_no)
  (let* ((AlMonGenInfo (ivueparser:parseAlMonGenInfo buf al_no))
         (str (ivueparser:parseString AlMonGenInfo)))
    #f
  ))

(define (ivueparser:parseDeviceAlertCondition buf)
  (let ((device_alert_state (u8data-u16 (subu8data buf 0 2)))
        (al_stat_chg_cnt (u8data-u16 (subu8data buf 2 4)))
        (max_p_alarm (u8data-u16 (subu8data buf 4 6)))
        (max_t_alarm (u8data-u16 (subu8data buf 6 8)))
        (max_aud_alarm (u8data-u16 (subu8data buf 8 10))))
    (store-set! ivueparser:store "max_p_alarm" max_p_alarm "ivue")
    (store-set! ivueparser:store "max_t_alarm" max_t_alarm "ivue")
    (store-set! ivueparser:store "max_aud_alarm" max_aud_alarm "ivue")
  ))

;; eof

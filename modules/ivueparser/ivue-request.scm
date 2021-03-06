#|
lnHealth - Health related apps for the LambdaNative framework
Copyright (c) 2009-2018, University of British Columbia
All rights reserved.

Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the
following conditions are met:

* Redistributions of source code must retain the above
copyright notice, this list of conditions and the following
disclaimer.

* Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following
disclaimer in the documentation and/or other materials
provided with the distribution.

* Neither the name of the University of British Columbia nor
the names of its contributors may be used to endorse or
promote products derived from this software without specific
prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

;;
;; Association Request Message functions
;;

(define AssocReqPresentationHeader (u8vector
  #x31 #x80 #xA0 #x80 #x80 #x01 #x01 #x00 #x00 #xA2 #x80 #xA0 #x03
  #x00 #x00 #x01 #xA4 #x80 #x30 #x80 #x02 #x01 #x01 #x06 #x04 #x52
  #x01 #x00 #x01 #x30 #x80 #x06 #x02 #x51 #x01 #x00 #x00 #x00 #x00
  #x30 #x80 #x02 #x01 #x02 #x06 #x0C #x2A #x86 #x48 #xCE #x14 #x02
  #x01 #x00 #x00 #x00 #x01 #x01 #x30 #x80 #x06 #x0C #x2A #x86 #x48
  #xCE #x14 #x02 #x01 #x00 #x00 #x00 #x02 #x01 #x00 #x00 #x00 #x00
  #x00 #x00 #x61 #x80 #x30 #x80 #x02 #x01 #x01 #xA0 #x80 #x60 #x80
  #xA1 #x80 #x06 #x0C #x2A #x86 #x48 #xCE #x14 #x02 #x01 #x00 #x00
  #x00 #x03 #x01 #x00 #x00 #xBE #x80 #x28 #x80 #x06 #x0C #x2A #x86
  #x48 #xCE #x14 #x02 #x01 #x00 #x00 #x00 #x01 #x01 #x02 #x01 #x02 #x81))
(define AssocReqSessionData (u8vector
  #x05 #x08 #x13 #x01 #x00 #x16 #x01 #x02 #x80 #x00 #x14 #x02 #x00 #x02))
(define AssocReqPresentationTrailer (u8vector
  #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00))

;; build an association message
(define (ivue:arequest)
  (ivue:encodemessage
    (ivue:encodesession #x0D (u8vector-append
       AssocReqSessionData
       (ivue:encodesession #xC1 (u8vector-append
         AssocReqPresentationHeader
         (ivue:encodeassociation (u8vector-append
           (ivue:encodeu32 #x80000000)  ;; MDDL_VERSION
           (ivue:encodeu32 #x40000000)  ;; NOMEN_VERSION
           (u8vector #x00 #x00 #x00 #x00)  ;; functional units
           (ivue:encodeu32 #x80000000)  ;; SYST_CLIENT
           (ivue:encodeu32 #x20000000)  ;; COLD_START
           (u8vector #x00 #x00 #x00 #x00)  ;; option list
           (ivue:encodeattributelist
             (ivue:encodeattribute 1 ;;NOM_POLL_PROFILE_SUPPORT
               (u8vector-append
                 (ivue:encodeu32 #x80000000)  ;; POLL_PROFILE_REV0
                 (ivue:encodeu32 #xBB80)        ;; poll period in 1/8ms [6sec=#xBB80, 1sec=#x1F40]
                 (ivue:encodeu32 1000)        ;; max_mtu_rx
                 (ivue:encodeu32 1000)        ;; max_mtu_tx
                 (ivue:encodeu32 #xffffffff)  ;; max_bw_tx
                 (ivue:encodeu32 #x60000000)  ;; pollprofileoptions
                   (ivue:encodeattributelist
                     (ivue:encodeattribute 61441 ;; NOM_ATTR_POLL_PROFILE_EXT
                       (u8vector-append
                          (ivue:encodeu32 #x88000000)
                          (ivue:encodeattributelist (u8vector))
                       ))))))
        ))
        AssocReqPresentationTrailer))))))

;;
;;  Poll Request functions for Trends, Waveforms, and selected Waveforms
;;

(define (ivue:polltrends)
  (ivue:encodemessage (u8vector-append
    (ivue:encodeSPpdu #xE100 2)
    (ivue:encodeROapdus ROIV_APDU 28)
    (ivue:encodeROIVapdu 0 CMD_CONFIRMED_ACTION 22)
    (ivue:encodeManagedObjectId NOM_MOC_VMS_MDS 0 0)
    (ivue:encodeActionArgument 0 NOM_ACT_POLL_MDIB_DATA 8)
    (ivue:encodePollMdibDataReq 1 NOM_PART_OBJ
         NOM_MOC_VMO_METRIC_NU NOM_ATTR_ALL_GROUPS)
 )))

;; poll for continous wave data
(define (ivue:pollwaves)
  (ivue:encodemessage (u8vector-append
    (ivue:encodeSPpdu #xE100 2)
    (ivue:encodeROapdus ROIV_APDU 40)
    (ivue:encodeROIVapdu 0 CMD_CONFIRMED_ACTION 34)
    (ivue:encodeManagedObjectId NOM_MOC_VMS_MDS 0 0)
    (ivue:encodeActionArgument 0 NOM_ACT_POLL_MDIB_DATA_EXT 20)
    (ivue:encodePollMdibDataReq 1 NOM_PART_OBJ
         NOM_MOC_VMO_METRIC_SA_RT NOM_ATTR_ALL_GROUPS)
    (ivue:encodeattributelist
      (ivue:encodeattribute NOM_ATTR_TIME_PD_POLL (ivue:encodeu32 #xCDFE6000)) ;;5days
    )
 )))

;; poll for continous wave data, but limit the number of waves
(define (ivue:pollwaves2)
  (ivue:encodemessage (u8vector-append
    (ivue:encodeSPpdu #xE100 2)
    (ivue:encodeROapdus ROIV_APDU 48)
    (ivue:encodeROIVapdu 0 CMD_CONFIRMED_ACTION 42)
    (ivue:encodeManagedObjectId NOM_MOC_VMS_MDS 0 0)
    (ivue:encodeActionArgument 0 NOM_ACT_POLL_MDIB_DATA_EXT 28)
    (ivue:encodePollMdibDataReq 1 NOM_PART_OBJ
         NOM_MOC_VMO_METRIC_SA_RT NOM_ATTR_ALL_GROUPS)
    (ivue:encodeattributelist
      (ivue:encodeattribute NOM_ATTR_TIME_PD_POLL
        (u8vector-append
           (ivue:encodeu32 #xffffffff)))
      (ivue:encodeattribute NOM_ATTR_POLL_OBJ_PRIO_NUM
        (u8vector-append
           (ivue:encodeu16 2)
           (ivue:encodeu16 2)  ;; no. of waves in poll??
        ))
 ))))

;; poll for alarms
(define (ivue:pollalarms)
  (ivue:encodemessage (u8vector-append
    (ivue:encodeSPpdu #xE100 2)
    (ivue:encodeROapdus ROIV_APDU 28)
    (ivue:encodeROIVapdu 0 CMD_CONFIRMED_ACTION 22)
    (ivue:encodeManagedObjectId NOM_MOC_VMS_MDS 0 0)
    (ivue:encodeActionArgument 0 NOM_ACT_POLL_MDIB_DATA 8)
    (ivue:encodePollMdibDataReq 1 NOM_PART_OBJ
         NOM_MOC_VMO_AL_MON NOM_ATTR_ALL_GROUPS)
  )))

;;
;; MDS Results message builder
;;

(define (ivue:mresult buf)
  (let ((invoke_id  (u8data-u16 (subu8data buf 8 10)))
        (obj_class  (u8data-u16 (subu8data buf 14 16)))
        (context_id (u8data-u16 (subu8data buf 16 18)))
        (handle     (u8data-u16 (subu8data buf 18 20))))
   (ivue:encodemessage (u8vector-append
      (u8vector #xE1 #x00 #x00 #x02 #x00 #x02 #x00 #x14)
      (ivue:encodeu16 invoke_id)
      (u8vector #x00 #x01 #x00 #x0e)
      (ivue:encodeu16 obj_class)
      (ivue:encodeu16 context_id)
      (ivue:encodeu16 handle)
      (u8vector #x00 #x48 #x47 #x00 #x0d #x06 #x00 #x00)
 ))))

;;
;; Get Priority List
;;

(define (ivue:getprioritylist)
  (ivue:encodemessage (u8vector
    #xE1 #x00 #x00 #x02
    #x00 #x01 #x00 #x16
    #x00 #x00 #x00 #x03 #x00 #x10
    #x00 #x21 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x01 #x00 #x02 #xF2 #x3A)))

;;
;; Release request
;;
(define (ivue:release)
  (ivue:encodemessage (u8vector
  #x09 #x18
  #xC1 #x16 #x61 #x80 #x30 #x80 #x02 #x01
  #x01 #xA0 #x80 #x62 #x80 #x80 #x01 #x00
  #x00 #x00 #x00 #x00
  #x00 #x00 #x00 #x00))
)


;; eof

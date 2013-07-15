;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002 ChangeSafe, LLC
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;;          ChangeSafe, LLC CONFIDENTIAL and PROPRIETARY material.
;;;;
;;;;          ChangeSafe, LLC
;;;;
;;;; This software and information comprise valuable intellectual
;;;; property and trade secrets of ChangeSafe, LLC, developed at
;;;; substantial expense by ChangeSafe, which ChangeSafe intends to
;;;; preserve as trade secrets.  This software is furnished pursuant
;;;; to a written license agreement and may be used, copied,
;;;; transmitted, and stored only in accordance with the terms of such
;;;; license and with the inclusion of the above copyright notice.
;;;; This software and information or any other copies thereof may not
;;;; be provided or otherwise made available to any other person.  NO
;;;; title to or ownership of this software and information is hereby
;;;; transferred.  ChangeSafe, LLC assumes no responsibility for the
;;;; use or reliability of this software.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CSF/UTILITY")

(proclaim (standard-optimizations))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(        ;; timestamps, meant to have some depth of specificity.
            timestamp
            timestamp-allocate         ;allocate a unique timestamp for the current time
            timestamp-create           ;allocate an approximate timestamp for some other time, or
                                        ; a copy of a pre-existing timestamp
            timestamp?                 ;true if object is a timestamp
            timestamp/compare          ;return :less-than :greater-than :equal for T1 vs T2
            timestamp-equal            ;return true if T1 is equal to T2
            timestamp/more-recent?     ;return true if T1 is more recent (is newer) than T2
            timestamp/less-recent?     ;return true if T1 is less recent (is older) than T2
            timestamp/earlier?         ;return true if T1 is more recent (is newer) than T2
            timestamp/later?                   ;return true if T1 is less recent (is older) than T2
            timestamp/time-flows-left-to-right? ; true if T1 happened before T2
            timestamp/time-flows-right-to-left? ; true if T2 happened before T1
            timestamp->=
            timestamp-<=
            timestamp/min
            timestamp/max
            timestamp-in-range-p
            timestamp->string  ;something you can display, please
            timestamp->universal-time ; lose precision, return lisp universal time
            timestamp/encode-as-string    ;machine readable string representation
            timestamp/decode-from-string  ;parser for machine readable string representation

            timestamp/days-and-millis          ;break a timestamp into 2 fixnums
            timestamp/components-from-days-and-millis
            timestamp/from-days-and-millis)))

;;;
;;; TIMESTAMP
;;;
;;; In order to allow persistent and non-persistent use of timestamps with a minimum of fuss,
;;; they're implemented as lists of two integers.  The first integer is UNIVERSAL-TIME
;;; measured in seconds in an ANSI specified canonical form.

;;; Unfortunately, lisp doesn't offer a more granular absolute time mechanism.
;;; GET-INTERNAL-REAL-TIME is good for relative sub-second measures, in increments
;;; of INTERNAL-TIME-UNITS-PER-SECOND (which is 1000 in Franz Allegro 5 on NT).
;;; Still, even then, sufficiently fast execution will yield the same value in two calls
;;; to this measure.
;;;
;;; We therefore use a per-process counter which measures successive calls
;;; uniquely to TIMESTAMP-ALLOCATE so that at least no two sequential calls in a
;;; process will show the same counter.
;;;

;;; Implementation notes:
;;;   A timestamp may have a null sequence number.  This is because
;;;   users are allowed to create timestamps from universal times.
;;;   Timestamps with a null sequence number are considered later than
;;;   any other timestamp in that particular second, but equal to all
;;;   other timestamps in that second with null sequence numbers.
;;;   This has two important advantages:
;;;
;;;   1.  Timestamp equivalence is transitive:  if a = b and b = c,
;;;       then a = c.
;;;
;;;   2.  Timestamps are reasonably ordered: if a < b and b < c, then
;;;       a < c.
;;;
;;;   3.  User generated timestamps being later than the computer
;;;       generated ones, user specified times are inclusive rather
;;;       than exclusive intervals.  (This is probably the desired
;;;       behavior).

(defstruct (timestamp
            (:type list)
            (:named)
            (:copier nil)
            (:conc-name timestamp/)
            (:constructor %timestamp-create (universal-time sequence-number))
            (:predicate timestamp?))
  (universal-time  nil :read-only t)
  (sequence-number nil :read-only t)    ;NIL implies UNSPECIFIC, number is a specific timestamp granularity
  )

(defvar *largest-delay* 0 "The largest `small' delay that we've needed for a timestamp.")

(defun small-delay (n)
  (setq *largest-delay* (max n *largest-delay*))
  (if (< n 2)
      n
      (+ (small-delay (- n 1))
         (small-delay (- n 2)))))

(deftype timestamp () `(satisfies timestamp?))

(defvar *timestamp-last-current* nil
  "The last timestamp that was issued for the then 'current' time.")

(defun timestamp-allocate (&optional (n 0))
  "Allocate a unique TIMESTAMP object to identify the current universal time, even if
   other TIMESTAMP objects have been allocated which would share the same universal time."
  ;; If we are using allegro, we can get finer granularity.  Note that
  ;; we sample the universal time *twice* to ensure that we don't get
  ;; caught by rollover on the internal-real-time.

  ;; On a fast machine, we might end up overflowing the stack
  ;; waiting for the next timestamp!
  ;; This small delay introduces an exponential backoff.
  (small-delay n)
  (#+allegro excl:without-interrupt
   #+lispworks lw:without-interrupts
    #+allegro
    (let ((utime1 (get-universal-time))
          (tick   (nth-value 1 (excl::cl-internal-real-time)))
          (utime2 (get-universal-time)))
      (cond ((> utime2 utime1) (timestamp-allocate)) ; rollover occured
            ((and *timestamp-last-current*
                  (= utime1 (timestamp/universal-time *timestamp-last-current*))
                  (= tick   (timestamp/sequence-number *timestamp-last-current*)))
             (timestamp-allocate (+ n 1)))             ; ensure timestamp is unique
            (t (setq *timestamp-last-current* (%timestamp-create utime1 tick)))))
    #-allegro
    (multiple-value-bind (utime tick) (win32-system-time)
      (cond ((and *timestamp-last-current*
                  (= utime (timestamp/universal-time *timestamp-last-current*))
                  (= tick  (timestamp/sequence-number *timestamp-last-current*)))
             (timestamp-allocate (+ n 1)))             ; ensure timestamp is unique
            (t (setq *timestamp-last-current* (%timestamp-create utime tick)))))))

;; Note that this routine converts a timestamp to a different representation
;; in such a way that the result is two fixnums, and so that the conversion
;; is reversible without loss of information.  When the sequence-number of
;; the timestamp is nil, the encoding is such that the converted representation
;; preserves the ordering on timestamps used by timestamp-compare.
;; At present, we have no need to reverse this representation shift, but we
;; could add it later, if desirable.  The reversibility does make the
;; interpretation of the second number be a little confusing, as it is not
;; merely the milliseconds within the day, but has "holes" for encoding the
;; nil sequence numbers.
(defconstant +seconds-sequence-number-encoding-modulus+ 1001
  "We want space for a second's worth of milliseconds and an extra value to flag
   the case of a null sequence number.")
(defconstant +null-sequence-number-encoding-value+ 1000
  "This is the value that indicates that the sequence number was NIL.")

(defun timestamp/days-and-millis (timestamp)
  "Break timestamp into days and milliseconds in that day"
  ;; 86400 is the number of seconds in a day
  (multiple-value-bind (days secs)
      (floor (timestamp/universal-time timestamp) 86400)
    (values days (+ (* secs +seconds-sequence-number-encoding-modulus+)
                    (or (timestamp/sequence-number timestamp)
                        +null-sequence-number-encoding-value+) ))))

(defun timestamp/components-from-days-and-millis (days millis)
  "The inverse of TIMESTAMP/DAYS-AND-MILLIS.
   Returns two values, the universal time, and the time stamp
   sequence number (or NIL).

   If you want a TIMESTAMP, use TIMESTAMP/FROM-DAYS-AND-MILLIS."
  (multiple-value-bind (seconds remainder)
      (floor millis +seconds-sequence-number-encoding-modulus+)
    (let ((ut (+ (* days 86400) seconds))
          (sequence-number (if (= remainder
                                  +null-sequence-number-encoding-value+)
                               nil
                             remainder)))
      (values ut sequence-number))))

(defun timestamp/from-days-and-millis (days millis)
  "The inverse of TIMESTAMP/DAYS-AND-MILLIS.  Returns a TIMESTAMP."
  (multiple-value-bind (ut sequence-number)
      (timestamp/components-from-days-and-millis days millis)
    (%timestamp-create ut sequence-number)))

(defgeneric timestamp-create (source)
  (:documentation
   "Create and return a TIMESTAMP object reflecting the specified SOURCE.

    If SOURCE is a timestamp, the newly allocated timestamp will be a copy.

    If SOURCE is a universal time, which by necessity lacks the granularity of a timestamp,
    we create a TIMESTAMP which is known to be unspecific in its below-utime-granularity
    specificity for purposes of comparison, etc.")
  (:method ((utime integer))
    "Create a timestamp from a universal time.  It's granularity detail beyond the utime is unspecific."
    (%timestamp-create utime nil))
  (:method ((source list))
    "Create a timestamp from another timestamp"
    (assert (timestamp? source))
    (%timestamp-create (timestamp/universal-time source) (timestamp/sequence-number source))))

;(defun timestamp? (timestamp)
;  "Return true if timestamp is a TIMESTAMP object (as far as we can
; tell, anyway)" (and (listp timestamp) (= (length timestamp) 2)
; (integerp (first timestamp)) (not (negativep (first timestamp)))
; (integerp (second timestamp)) (not (negativep (second timestamp)))
; ))

(defun timestamp/compare (left right)
  "Compare two TIMESTAMP objects and return :EARLIER :EQUAL or :LATER
   as regards the relationship of LEFT and RIGHT.  If LEFT is earlier
   than RIGHT, return :EARLIER, etc."
  (let ((ltime (timestamp/universal-time left))
        (rtime (timestamp/universal-time right))
        (lsn   (timestamp/sequence-number left))
        (rsn   (timestamp/sequence-number right)))
    (cond ((< ltime rtime) :earlier)
          ((> ltime rtime) :later)
          ;; The universal times are the same, so we check the
          ;; sequence numbers.  Null sequence numbers are later than
          ;; any integer sequence number, but equal to other nulls.
          ((null lsn) (if (null rsn)
                          :equal
                        :later))
          ((null rsn) :earlier)
          ((< lsn rsn)       :earlier)
          ((> lsn rsn)       :later)
          (t :equal))))

(defsubst timestamp-equal (left right)
  "Return true if TIMESTAMP LEFT is equal to RIGHT"
  (eq (timestamp/compare left right) :equal))

;;; These predicates are just hard to understand.
;;; I *think* that the following two might be
;;; the right thing:
(defsubst timestamp/time-flows-left-to-right? (left right)
  "This means that left occurred first, then right."
  (eq (timestamp/compare left right) :earlier))

(defsubst timestamp/time-flows-right-to-left? (left right)
  "This means that right occurred first, then left."
  (eq (timestamp/compare left right) :later))

(defsubst timestamp/more-recent? (left right)
  "Return true if TIMESTAMP LEFT is more recent than RIGHT.
   Often useful if you don't want to decipher the meaning of the above TIMESTAMP/COMPARE"
  (eq (timestamp/compare left right) :later))

(defsubst timestamp/less-recent? (left right)
  "Return true if TIMESTAMP LEFT is less recent than RIGHT.
   Often useful if you don't want to decipher the meaning of the above TIMESTAMP/COMPARE"
  (eq (timestamp/compare left right) :earlier))

(defsubst timestamp/earlier? (left right)
  "Return true if TIMESTAMP LEFT is more recent than RIGHT.
   Often useful if you don't want to decipher the meaning of the above TIMESTAMP/COMPARE"
  (eq (timestamp/compare left right) :earlier))

(defsubst timestamp/later? (left right)
  "Return true if TIMESTAMP LEFT is less recent than RIGHT.
   Often useful if you don't want to decipher the meaning of the above TIMESTAMP/COMPARE"
  (eq (timestamp/compare left right) :later))

(defsubst timestamp->= (timestamp-1 timestamp-2)
  (ecase (timestamp/compare timestamp-1 timestamp-2)
    (:earlier nil)
    (:equal   t)
    (:later   t)))

(defsubst timestamp-<= (timestamp-1 timestamp-2)
  (ecase (timestamp/compare timestamp-1 timestamp-2)
    (:earlier t)
    (:equal   t)
    (:later   nil)))

(defun timestamp/min (&rest timestamps)
  "Of the TIMESTAMPs in TIMESTAMPS, return the earliest one.
   For the convenience of the caller, any NILs in TIMESTAMPS are ignored."
  (let ((min nil))
    (dolist (ts timestamps)
      (cond ((null min)
             (setq min ts))
            ((and ts (timestamp/earlier? ts min))
             (setq min ts))
            (t nil)))
    min))

(defun timestamp/max (&rest timestamps)
  "Of the TIMESTAMPs in TIMESTAMPS, return the latest one.
   For the convenience of the caller, any NILs in TIMESTAMPS are ignored."
  (let ((max nil))
    (dolist (ts timestamps)
      (cond ((null max)
             (setq max ts))
            ((and ts (timestamp/later? ts max))
             (setq max ts))
            (t nil)))
    max))

(defun timestamp-in-range-p (timestamp start-time end-time)
  "TIMESTAMP is a TIMESTAMP.
   START-TIME and END-TIME are either TIMESTAMPs or nil.  NIL indicates no
   bound at that end.

   Returns true if TIMESTAMP falls within the specified range: that is
   on or after START-TIME (if specified) and before END-TIME (if specified)."
  (let ((ut-ts (timestamp/universal-time timestamp)))
    (and (if start-time
             (<= (timestamp/universal-time start-time)
                 ut-ts)
           t)
         (if end-time
             (<= ut-ts
                 (timestamp/universal-time end-time))
           t))))

(defun timestamp->string (timestamp &key subseconds no-days brief-days)
  "Return a string similar to TIME-STRING.

   NO-DAYS and BRIEF-DAYS are passed to the TIME-STRING function.

   If SUBSECONDS is true, we'll print something to indicate subsecond granularity,
   though it may not be in any industry standard format, and may not reflect any actual
   absolute subsecond time."
  (let ((result (time-string (timestamp/universal-time timestamp)
                             :no-days no-days :brief-days brief-days)))
    (if (and subseconds (timestamp/sequence-number timestamp))
        (concatenate 'string result "." (princ-to-string (timestamp/sequence-number timestamp)))
      result)))

(defsubst timestamp->universal-time (timestamp)
  "Convert a timestamp to a lisp universal-time.
   **** NOTE **** THIS OPERATION MAY INVOLVE LOSS OF PRECISION."
  (timestamp/universal-time timestamp))

(defun timestamp/encode-as-string (timestamp)
  "Encode the TIMESTAMP as a string.  This format is for machines, not people."
  (format nil "~d~@[.~d~]"
          (timestamp/universal-time timestamp)
          (timestamp/sequence-number timestamp)))

(defun timestamp/decode-from-string (string)
  "Returns the TIMESTAMP represented by STRING.
   STRING is assumed to be the result of TIMESTAMP-ENCODE-AS-STRING."
  (let ((ut (parse-integer string))
        (seq (let ((dot (position #\. string)))
               (when dot
                 (parse-integer string :start (1+ dot))))))
    (%timestamp-create ut seq)))

#||
;;; Naha's proposed new implementation

(defvar *timestamp-most-recent* nil)

(defun timestamp-create (&optional (utime (get-universal-time) utime-p))
  "Create a TIMESTAMP object (a list) reflecting the current (or specified) time."
  ;; NOTE that unless you're getting a timestamp for the current time,
  ;; the sequence counter in the timestamp is liekly to be wrong.
  ;; Consider what happens if within one second one does
  ;;    (TIMESTAMP-CREATE)
  ;;    (TIMESTAMP-CREATE <some-time-in-the-past>)
  ;;    (TIMESTAMP-CREATE)
  ;; The timestamp of the third call will not have a sequence number
  ;; that's one more than the timestamp of the first call.
  (declare (integer utime))
  (if utime-p
      (%timestamp-create utime nil)
    ;; making a timestamp for the current time
    (setq *timestamp-most-recent*
      (if (= utime (timestamp-universal-time *timestamp-most-recent*))
          (%timestamp-create utime (1+ (timestamp-sequence-number *timestamp-most-recent*)))
        (%timestamp-create utime 0)))))

;;; TIMESTAMP COMPARISSON RULE: A TIMESTAMP that was created by
;;; calling TIMESTAMP-CREATE with a UTIME is always between (never
;;; equal to) timestamps which were created based on the current time.

;;; The basis behind this rule and how it affects time stamp
;;; comparisson is as follows:  The typical use of timestamps is to
;;; test to see if the timestamp of some database event (which will
;;; have been created from the then-current universal time) falls
;;; within some time interval that was specified by the user (and
;;; whose timestamp universal times will have been specified upon
;;; creation).

;;; When calling TIMESTAMP-CREATE to stamp the time of some database
;;; event, one should never specify the UTIME argument.  When calling
;;; it to define some time interval for comparisson purposes, one
;;; should always provide it, even if one means to use the current
;;; time.  This is because we need some means of distinguishing
;;; whether the timestamp needs to be a unique monotonically
;;; increasing value (in the case of database events) or whether it is
;;; just being constructed to bound some time interval.

;;; This can lead to confusion if the callers are unaware of the
;;; potential indeterminancy in comparing user supplied timestamps
;;; with database event timestamps.  For that reason, we can define
;;; TIMESTAMP-IN-INTERVAL? so the caller can clearly indicate whether
;;; they are comparing with the start of an interval or the end of
;;; one.

(defun timestamp-in-interval? (start-ts timestamp end-ts)
  "Returns true if TIMESTAMP falls within the time interval bounded by
   START-TS and END-TS.  Either of START-TS or END-TS can be NIL to indicate
   an open-ended interval, otherwise they are TIMESTAMPs."
  (cond ((and start-ts (timestamp-more-recent? start-ts timestamp))
         nil)
        ((and end-ts (timestamp-less-recent? end-ts timestamp))
         nil)
        (t t)))

(defun timestamp-compare (t1 t2)
  "Compare two TIMESTAMP objects and return :LESS-THAN :EQUAL or :GREATER-THAN
   as regards the relationship of T1 to T2.  If T1 is less than T2, return :LESS-THAN, etc.
   The greater the time stamp, the more recent the time."
  (let ((utime1 (timestamp-universal-time t1))
        (utime2 (timestamp-universal-time t2)))
    (cond ((< utime1 utime2) :less-than)
          ((> utime1 utime2) :greater-than)
          (t                            ;universal times are equal
           (let ((rtime1 (timestamp-sequence-number t1))
                 (rtime2 (timestamp-sequence-number t2)))
             (cond ((and (null rtime1) (null rtime2))
                    :equal)
                   ;; In the case where one of the timestamps has a
                   ;; user specified time and the other doesn't, we
                   ;; know they are not equal (see TIMESTAMP
                   ;; COMPARISSON RULE above) but we don't know which
                   ;; is greater.  It's up to the caller to decide how
                   ;; to interpret this.
                   ((or (null rtime1) (null rtime2))
                    :not-equal)
                   ((< rtime1 rtime2) :less-than)
                   ((> rtime1 rtime2) :greater-than)
                   (t :equal)))))))

(defun timestamp-equal (t1 t2)
  "Return true if TIMESTAMP T1 is equal to T2"
  (eq (timestamp-compare t1 t2) :equal))

(defun timestamp-more-recent? (t1 t2)
  "Return true if TIMESTAMP T1 is more recent than T2.
   Often useful if you don't want to decipher the meaning of the above TIMESTAMP-COMPARE"
  ;; The greater the time stamp, the more recent the time.
  (ecase (timestamp-compare t1 t2)
    ((:greater-than) t)
    ((:less-than :equal :not-equal) nil)))

(defun timestamp/less-recent? (t1 t2)
  "Return true if TIMESTAMP T1 is less recent than T2.
   Often useful if you don't want to decipher the meaning of the above TIMESTAMP-COMPARE"
  ;; The greater the time stamp, the more recent the time.
  (ecase (timestamp/compare t1 t2)
    ((:less-than) t)
    ((:greater-than :equal :not-equal) nil)))

||#

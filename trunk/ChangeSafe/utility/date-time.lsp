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
;;;;
;;;; File Name:     date-time.lsp
;;;; Author:        jrm, Mark Nahabedian, Dave Tenny
;;;;                (module represents content merged from utils.lsp as well
;;;;                 as new content)
;;;; Creation Date: October 1999
;;;;
;;;; Module Description:
;;;;
;;;; Utility functions for manipulating date and time
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CSF/UTILITY")

(eval-when (:load-toplevel :execute)
  (export '(
            decode-elapsed-time
            default-time-zone
            iso-date-time
            numeric-date-string
            )))


(proclaim (standard-optimizations))

(defun common-lisp-user::format-elapsed-time (stream elapsed-time &optional (colonp nil) atsignp)
  (check-type stream stream)
  (check-type elapsed-time number)
  (when atsignp (error "FORMAT-ELAPSED-TIME does not take an atsign."))
  (cond ((< elapsed-time 60)         (if colonp
                                         (format stream "~,2f second~:P" elapsed-time)
                                         (format stream "~,2f s" elapsed-time)))
        ((< elapsed-time (* 60 60))  (multiple-value-bind (minutes seconds)
                                         (floor elapsed-time 60)
                                       (if colonp
                                           (format stream "~d minute~:P ~,2f second~:P" minutes seconds)
                                           (format stream "~d m ~,2f s" minutes seconds))))
        ((< elapsed-time (* 24 60 60)) (multiple-value-bind (hours rem)
                                           (floor elapsed-time (* 60 60))
                                         (if colonp
                                             (format stream "~d hour~:P ~d minute~:P" hours (floor rem 60))
                                             (format stream "~d h ~d m" hours (floor rem 60)))))
        ((< elapsed-time (* 90 24 60 60)) (multiple-value-bind (days rem1)
                                              (floor elapsed-time (* 24 60 60))
                                            (if (> days 7)
                                                (if colonp
                                                    (format stream "~d day~:P" days)
                                                    (format stream "~d d" days))
                                                (if colonp
                                                    (format stream "~d day~:P ~d hour~:P" days (floor rem1 (* 60 60)))
                                                    (format stream "~d d ~d h" days (floor rem1 (* 60 60)))))))
        ((< elapsed-time (* 12 30 24 60 60)) (if colonp
                                                 (format stream "~d month~:P" (floor elapsed-time (* 30 24 60 60)))
                                                 ;; m is ambiguous
                                                 (format stream "~d d" (floor elapsed-time (* 24 60 60)))))
        (t (multiple-value-bind (years rem)
               (floor elapsed-time (* 365 24 60 60))
             (if colonp
                 (format stream "~d year~:P ~d month~:P" years (floor rem (* 30 24 60 60)))
                 (format stream "~d y ~d m" years (floor rem (* 30 24 60 60))))))))

(defun elapsed-time-in-seconds (base &optional (now (get-internal-real-time)))
  "Returns the time in seconds that has elapsed between Base and Now.
   BASE and NOW must be the results of calls to GET-INTERNAL-REAL-TIME.
   We convert the result to seconds."
  (coerce (/ (- now base)
             internal-time-units-per-second)
          'float))

(defun time-string (&optional universal-time &key no-days brief-days time-before-date)
  "Return a universal time as a nicely formatted string.
   If NO-DAYS is true, we won't print the day of the week.
   If BRIEF-DAYS is true, we print the day of the week as a 3-character references.
   If TIME-BEFORE-DATE is true, we specify the time first, then the date.  Otherwise we
   print it the other way around.
   "
  (unless universal-time
    (setf universal-time (get-universal-time)))
  ;; DECODE-UNIVERSAL-TIME currently (1999-08-30) doesn't understand
  ;; negative universal times (the CommonLisp Spec doesn't require it
  ;; to).  Since it's better to do something than fail, we work around
  ;; this bug by just formatting timestamps which it can't decode as
  ;; integers, padded to the appropriate width.
  (or (ignore-errors
       (multiple-value-bind (secs min hour date month year dow)
           (decode-universal-time universal-time)
         (let ((day-string              ;includes post-word padding, if necessary
                (cond (no-days "")
                      (brief-days (svref '#("Mon " "Tue " "Wed " "Thu " "Fri " "Sat " "Sun ") dow))
                      ;; Full days are padded to column width == largest string, plus post-field blank pad
                      (t (svref '#("Monday    "
                                   "Tuesday   "
                                   "Wednesday "
                                   "Thursday  "
                                   "Friday    "
                                   "Saturday  "
                                   "Sunday    ")
                                dow))))
               (month-string (svref '#(0 "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                       "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                                    month)))
           ;; ~(str~) is case conversion
           (if time-before-date
               (format nil "~:(~2,'0d:~2,'0d:~2,'0d ~a~2,'0d-~A-~A~)"
                       hour min secs day-string date month-string year)
             (format nil "~:(~a~2,'0d-~A-~A ~2,'0d:~2,'0d:~2,'0d~)"
                     day-string date month-string year hour min secs)))))
      (format nil "[~v,' d]"
              (+ -2                     ; square brackets
                 20                     ; width without day
                 (cond (no-days 0)      ; width of day field
                       (brief-days 4)
                       (t 10)))
              universal-time)))

(defun iso-date-string (year month day &key to-stream)
  (format to-stream "~4,'0d-~2,'0d-~2,'0d" year month day))

(defun default-time-zone (&optional (universal-time (get-universal-time)))
  "Return the time zone adjustment (i.e. N in GMT-N) for the the location of the executing lisp code
   (often thought of as the server time zone).  Positive return values are 'west', so
   Massachusetts would be '5'."
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour date month year day))
    (if daylight-p
        (1- zone)
      zone)))

(defun numeric-date-string (&optional (universal-time (get-universal-time)))
  "Return the date as a string in the form YYYYMMDD, where all values are numeric."
  (multiple-value-bind (secs min hour date month year dow)
      (decode-universal-time universal-time)
    (declare (ignore dow secs min hour))
    (format nil "~4,'0d~2,'0d~2,'0d"    ;yyyymmdd
            year
            month                       ;1-relative
            date)))

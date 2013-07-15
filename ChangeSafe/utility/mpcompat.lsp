;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright (c) 2002 ChangeSafe, LLC
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
  (export '(
            call-with-timeout
            make-process-lock
            with-process-lock
            without-scheduling
            with-timeout
            )))

(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  "Execute BODY; if execution takes more than SECONDS seconds, terminate
   and evaluate TIMEOUT-FORMS."
  `(CALL-WITH-TIMEOUT
    ,seconds
    (LAMBDA () ,@body)
    (LAMBDA () ,@timeout-forms)))

(defmacro with-process-lock ((lock &key norecursive) &body forms)
  (declare (ignore norecursive))
  `(MP:WITH-LOCK (,lock) ,@forms))

(defmacro without-scheduling (&body forms)
  `(MP:WITHOUT-PREEMPTION ,@forms))

(defmacro make-process-lock (&rest args)
  `(MP:MAKE-LOCK ,@args))

(defun process-allow-schedule ()
  (assert (null system::*in-no-interrupts*))
  (assert (null mp:*inhibit-scheduling-flag*))
  (mp:process-allow-scheduling))

(defunimplemented process-add-run-reason (process run-reason))
(defunimplemented process-revoke-run-reason (process run-reason))

(defun call-scheduling-timer (thunk timer timeout)
  (unwind-protect-without-interrupts
     (progn (mp:schedule-timer-relative timer timeout)
            (handler-bind ((cl:error (lambda (condition)
                                       (declare (ignore condition))
                                       (when-debugging 0
                                         (mp:unschedule-timer timer)
                                         (debug-message 0 "Cancelling timeout to enter error handler."))
                                       ;; Decline to handle the condition
                                       ;; by returning normally.
                                       nil)))
              (funcall thunk)))
   (mp:unschedule-timer timer)))

(defun call-with-timeout (time body-thunk timeout-thunk)
  (let* ((expired t)
         return-values)
    (block timeout
      (let* ((process mp:*current-process*)
             (timer (mp:make-timer
                     (lambda ()
                       (let (#+:lispworks 
                             (mp::*interrupted-process-priority*
                              (mp:process-priority process)))
                         (mp:process-interrupt process
                                               (lambda ()
                                                 (debug-message 1 "Timing out.")
                                                 (return-from timeout (values)))))))))
        (setq return-values
              (multiple-value-list
               (call-scheduling-timer body-thunk timer time)))
        (setq expired nil)))
    (if expired
        (funcall timeout-thunk)
        (values-list return-values))))

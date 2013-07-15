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


;;; This file has utility functions that other utility functions use.
;;; As such, it *must* be written using *only* the built-in CommonLisp
;;; features (and native extensions).

(in-package "CSF/UTILITY")

(proclaim (standard-optimizations))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(call-with-deeper-print-level
            debug-note-condition
            *log-stack-backtrace-on-error*)))

(defvar *debug-noise-print-level* 5)
(defvar *debug-noise-print-length* 10)

(defun format-debug-message (noise format-string arglist)
  ;; Do not `optimize' this.  We want to construct the format-string
  ;; and *then* print it.
  (let ((message (let ((*print-length* *debug-noise-print-length*)
                       (*print-level*  *debug-noise-print-level*))
                   ;; Magic format string:  print DEBUG level and indent that many spaces.
                   (format nil "DEBUG ~d>~v@T ~?~&" noise noise format-string arglist))))
    (fresh-line *debug-io*)
    (write-string message *debug-io*)
    (unless system::*in-no-interrupts*
      ;; Send the output, but don't wait.
      (force-output *debug-io*)
      ;; Let other processes (like the GUI that will display this output) run.
      (mp:process-allow-scheduling 0))))

(defun debug-note-condition (where condition)
  (declare (ignore where)) ;; not sure where to print this.
  (cond ((typep condition 'changesafe-condition)
         (debug-message 1 "Condition ~s signalled from subsystem ~s."
                        condition (changesafe-condition/subsystem condition)))
        ((typep condition 'cl:simple-condition)
         (debug-message 1 "Condition ~s signalled." condition))
        (t nil))
  (when (typep condition 'cl:simple-condition)
    (debug-message 2 "~?"
                   (simple-condition-format-control condition)
                   (simple-condition-format-arguments condition)))
  (when-debugging 3
                  (error-log-output condition *debug-io*)))

(defun dump-stack-backtrace (stream)
    "Write a stack backtrace to stream."
  ;; Don't use regular ignore-errors here because it contains
  ;; code to call this backtrace.
  (handler-bind                                 ;prevent recursion
      (#+allegro (excl:interrupt-signal #'SIGNAL)

       (cl:error (lambda (condition)
                    (return-from dump-stack-backtrace
                      (values nil condition)))))
    #+allegro (let ((*terminal-io* stream)
                    (*standard-output* stream))
                (tpl:do-command "zoom"
                                :from-read-eval-print-loop nil
                                :count t :all t))
    #-allegro (format stream "No backtrace available.")))

(defvar *log-stack-backtrace-on-error* nil
  "When T, stack backtraces will sent to the error log output on errors.")

(defun error-log-output (condition stream)
  "Write a log entry describing the CONDITION to STREAM."
  (when (streamp stream)
    (with-standard-io-syntax
     (let ((*print-readably*    nil)
           (*print-miser-width* 40)
           (*print-pretty*      t)
           (*print-circle*      t)
           (*print-level*       nil)
           (*print-length*      nil))
        ;; Don't use regular ignore-errors here because it contains
        ;; code to call this backtrace.
        (handler-bind                           ;prevent recursion
            (#+allegro (excl:interrupt-signal #'SIGNAL)

             (cl:error (lambda (condition)
                        (return-from error-log-output
                          (values nil condition)))))
          (format stream "~&~%~/iso-date-time/:  ~s~&~%" (get-universal-time) condition)
          (finish-output stream)
          (when (or *log-stack-backtrace-on-error*
                    (debug-level-meets-or-exceeds? 3))
            (dump-stack-backtrace stream)))))))

(defun call-with-deeper-print-level (stream thunk)
  "Invoke thunk with *print-level* decremented.  If *print-level* is zero,
   thunk is NOT invoked and a # character is written to stream."
  (let ((*print-level* (and (numberp *print-level*)
                            (1- *print-level*))))
    (if (and (numberp *print-level*) (minusp *print-level*))
        (write-char #\# stream)
        (funcall thunk))))

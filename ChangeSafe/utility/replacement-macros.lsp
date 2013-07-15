;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;           Copyright © 2000 - 2005 Content Integrity, Inc.
;;;           ALL RIGHTS RESERVED.
;;;
;;;           Content Integrity, Inc. CONFIDENTIAL and PROPRIETARY material.
;;;
;;;           Content Integrity, Inc
;;;           Braintree Executive Office Park
;;;           P.O. Box 850942
;;;           Braintree, MA 02185-0942
;;;
;;; This software and information comprise valuable intellectual property
;;; and trade secrets of Content Integrity, Inc., developed at substantial
;;; expense by Content Integrity, which Content Integrity intends to
;;; preserve as trade secrets.  This software is furnished pursuant to a
;;; written license agreement and may be used, copied, transmitted, and
;;; stored only in accordance with the terms of such license and with the
;;; inclusion of the above copyright notice.  This software and
;;; information or any other copies thereof may not be provided or
;;; otherwise made available to any other person.  NO title to or
;;; ownership of this software and information is hereby transferred.
;;; Content Integrity assumes no responsibility for the use or reliability
;;; of this software.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File Name:     replacement-macros.lsp
;;; Author:        jrm
;;; Creation Date: Oct 2000
;;;
;;; Module Description:  Replacements for standard CL macros that are
;;;                      usually incorrectly implemented.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CSF/UTILITY")

;;; NOTE NOTE NOTE NOTE
;;; If you edit this file, you must rebuild the development GUI.

(eval-when (:load-toplevel :execute)
  (export '(
            debug-message
            debug-level-meets-or-exceeds?
            unwind-suggest

            when-debugging
            without-interrupts
            with-deferred-interrupts
            with-deferred-interrupts-restored
            ))
  (export '(csf/config::*disable-debug-messages*
            csf/config::*debug-noise-level*
            csf/config::*debug-noise-print-level*
            csf/config::*debug-noise-print-length*)
          "CSF/CONFIG"))

;; By default, this will be set to nil.

(defparameter *disable-debug-messages* nil
  "Set this to T and then recompile the system to get rid of all the debug messages.")

(defvar *debug-noise-level*
    nil
  "Controls the level of debugging noise output.  Should be NIL or a value
   from 0 through 5.  Each DEBUG-MESSAGE has a value from 0 to 5.  If the
   current value of *DEBUG-NOISE-LEVEL* is equal to or greater than the
   level associated with a particular DEBUG-MESSAGE, the DEBUG-MESSAGE will
   be printed.

   When debugging, use (INCF *DEBUG-NOISE-LEVEL*) to increase the amount
   of debug messages, and (DECF *DEBUG-NOISE-LEVEL*) to decrease it.")

;;; This macro is used in conjunction with Dave's debug flags
;;; in order to couple them.  You might see something like
;;; (when (or *with-version-debug* (debug-level-meets-or-exceeds? 3))
;;;    ....)
(defmacro debug-level-meets-or-exceeds? (number)
  "Return T if *DEBUG-NOISE-LEVEL* is currently equal to or
   greater than NUMBER.  Use this macro instead
   of the obvious => because *DEBUG-NOISE-LEVEL* isn't always a
   number."
  (if (and (boundp '*disable-debug-messages*)
           (eq *disable-debug-messages* t))
      '(PROGN)
    `(AND (NUMBERP *DEBUG-NOISE-LEVEL*)
          (>= *DEBUG-NOISE-LEVEL* ,number))))

(defmacro when-debugging (noise &body body)
  "Execute BODY if *DEBUG-NOISE-LEVEL* is equal to or greater than NOISE.
   Use this macro to call complicated debugging output routines."
  (if (and (boundp '*disable-debug-messages*)
           (eq *disable-debug-messages* t))
      '(PROGN)
    `(WHEN (DEBUG-LEVEL-MEETS-OR-EXCEEDS? ,noise)
       (LOCALLY ,@body))))

(defvar *debug-noise-print-level* 5 "Print level when printing debug noise.")
(defvar *debug-noise-print-length* 10 "Print level when printing debug noise.")

(defmacro debug-message (noise format-string &rest args)
  "Print a message on *DEBUG-IO* using FORMAT-STRING and ARGS
   iff the *DEBUG-NOISE-LEVEL* is equal to or greater than NOISE.

   When writing code, sprinkle calls to DEBUG-MESSAGE at strategic
   points to aid in debugging.  The different noise levels should be
   used at different semantic levels in the code.  Level 0 is
   for only the highest level of functionality, Level 3 is for
   module level, Level 5 is for extreme detail."
  (if (and (boundp '*disable-debug-messages*)
           (eq *disable-debug-messages* t))
      `(PROGN)
      (let ((noise-var (gensym "NOISE-VAR-")))
        `(LET ((,noise-var ,noise))
           #+ALLEGRO (DECLARE (:FBOUND FORMAT-DEBUG-MESSAGE))
           (WHEN-DEBUGGING ,noise-var
             (FORMAT-DEBUG-MESSAGE ,noise-var ,format-string (LIST ,@args)))))))

;;; For some reason, Franz believes control-c to be an error (thus it is ignored
;;; when you have ignore-errors in place).  Fix this.

#+(and allegro (not dont-fix-franz))
(defmacro ignore-errors (&rest forms)
  "Working version of CL:IGNORE-ERRORS"
  (let ((block-name (gensym "IGNORE-ERRORS-BLOCK-")))
    `(BLOCK ,block-name
       (HANDLER-BIND (#+allegro (EXCL:INTERRUPT-SIGNAL #'ERROR)

                      (ERROR (LAMBDA (CONDITION)
                                #+ALLEGRO (DECLARE (:FBOUND DEBUG-NOTE-CONDITION))
                                (WHEN-DEBUGGING 1
                                  (DEBUG-NOTE-CONDITION "caught by an ignore-errors form" CONDITION))
                                (RETURN-FROM ,block-name
                                  (values NIL CONDITION)))))
         ,@forms))))

;;; If you turn this on, don't forget to update the packages file.
;(defmacro the (type form)
;  "Asserts that the result of FORM is of type TYPE.
;   Under paranoid conditions, checks that this is true."
;  ;; Paranoid version
;  (let ((result-name (gensym "RESULT-")))
;    `(LET ((,result-name ,form))
;       (ASSERT (TYPEP ,result-name ',type))
;       ,result-name)))

(defmacro with-deferred-interrupts (reason &body body)
  "Call body in a context where asynchronous interrupts are deferred.

   Within body, the macro WITH-DEFERRED-INTERRUPTS-RESTORED will execute
   *its* body with the asynchronous interrupts re-enabled (if they were
   enabled before, of course)."
  (let ((restore  (gensym "RESTORE-")))
    `(CALL-WITH-DEFERRED-INTERRUPTS ,reason
      (LAMBDA (,restore)
        (DECLARE (IGNORABLE ,restore))
        (MACROLET ((WITH-DEFERRED-INTERRUPTS-RESTORED (&BODY BODY)
                     `(FUNCALL ,',restore (LAMBDA () ,@body))))
          ,@body)))))

#+(or :lispworks
      (and allegro (not dont-fix-franz)))
(defmacro unwind-protect (protected-form &body cleanup-forms)
  (let ((reason (when (stringp (car cleanup-forms))
                  (car cleanup-forms))))
    `(WITH-DEFERRED-INTERRUPTS ,(or reason "performing undocumented cleanup")
       (CL:UNWIND-PROTECT
           (WITH-DEFERRED-INTERRUPTS-RESTORED
               ,protected-form)
         ,@(if reason
               (cdr cleanup-forms)
             cleanup-forms)))))

(defmacro unwind-suggest (protected-form &body cleanup-forms)
  "Like UNWIND-PROTECT, but asynchronous interrupts that occur during
   the cleanup form cause the cleanup to be aborted."
  `(CL:UNWIND-PROTECT ,protected-form ,@cleanup-forms))

#+(or :lispworks
      (and allegro (not dont-fix-franz)))
(defmacro with-open-file ((stream filespec &rest options) &body body)
  "Like with-open-file, but no race conditions."
  (let ((abort-p             (gensym "ABORT-P-"))
        (condition           (gensym "CONDITION-"))
        (file                (gensym "FILE-"))
        (attempt-open        (gensym "ATTEMPT-OPEN-"))
        (interrupts-deferred (gensym "INTERRUPTS-DEFERRED-"))
        (arglist             (mapcar (lambda (option)
                                       (declare (ignore option))
                                       (gensym "ARGUMENT-"))
                                     options)))
    `(LET ((,stream nil)
           (,abort-p t)
           (,condition nil)
           (,file ,filespec)
           ,@(mapcar #'list arglist options)) ; eval arguments normally
      (MULTIPLE-VALUE-PROG1
          (WITH-DEFERRED-INTERRUPTS (FORMAT NIL "WITH-OPEN-FILE ~s" ,file)
            (CL:UNWIND-PROTECT
                 ;; Catch file errors and resignal them once we
                 ;; are out of the deferred context.

                 ;; This isn't quite right, though.  Since this is a
                 ;; synchronous error, I should just raise it.
                 (CATCH ',attempt-open
                   (SETQ ,stream (HANDLER-BIND ((FILE-ERROR
                                                 (LAMBDA (CONDITION)
                                                   (SETQ ,condition CONDITION)
                                                   (THROW ',attempt-open (VALUES)))))
                                   (OPEN ,file ,@arglist)))
                   (MULTIPLE-VALUE-PROG1
                       (WITH-DEFERRED-INTERRUPTS-RESTORED
                           ,@body)
                     (SETQ ,abort-p NIL)))
              (WHEN ,stream
                (CLOSE ,stream :ABORT ,abort-p))))
        (WHEN ,condition
          (CL:ERROR ,condition))))))

#+(or :lispworks
      (and allegro (not dont-fix-franz)))
(defmacro with-open-stream ((var stream) &body body)
  "Like WITH-OPEN-STREAM, but has no race conditions."
  (let ((interrupts-deferred (gensym "INTERRUPTS-DEFERRED-"))
        (arglist nil))
    (when (consp stream)
      (dotimes (i (1- (length stream)))
        (push (gensym "ARG-") arglist)))
    `(LET ((,var NIL)
           ,@(when (consp stream)
               (mapcar #'list arglist (cdr stream))))
       (WITH-DEFERRED-INTERRUPTS "Opening or closing stream"
         (CL:UNWIND-PROTECT
             (PROGN (SETQ ,var ,@(if (consp stream)
                                     (list (cons (car stream) arglist))
                                   (list stream)))
                    (WITH-DEFERRED-INTERRUPTS-RESTORED
                        ,@body))
           (WHEN ,var
             (CLOSE ,var)))))))

#+(and allegro (not dont-fix-franz))
(defmacro without-interrupts (&body body)
  `(EXCL:WITHOUT-INTERRUPTS ,@body))


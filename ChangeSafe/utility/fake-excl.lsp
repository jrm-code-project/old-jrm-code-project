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


;;; Declarations macros.
;;; This file loads first.
;;;
;;; Every sourcefile should have
;;;
;;; (in-package "...")
;;;
;;; (proclaim (standard-optimizations))
;;;
;;; at the top.

(in-package "FAKE-EXCL")

(proclaim (standard-optimizations))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(
            errorset
            fasl-write
            fasl-read
            fast
            filesys-size
            filesys-type
            filesys-write-date
            *initial-terminal-io*
            match-regexp
            compile-regexp
            stream-input-fn
            named-function
            *cl-default-special-bindings*
            stream-property-list
            )))

(defunimplemented fasl-read (stream))
(defunimplemented fasl-write (object stream flag))
(defunimplemented filesys-size (file))

(defun filesys-type (file)
  (if (lw:file-directory-p file)
      :directory
      :file))

(defunimplemented filesys-write-date (file))

(defun match-regexp (pattern string &key return)
  (declare (ignore return))
  (csf/utility:pregexp-match pattern string))

(defunimplemented compile-regexp (pattern))

;; Just return the stream.
(defun stream-input-fn (stream)
  stream)

(defun filesys-size (stream)
  (file-length stream))

(defun filesys-write-date (stream)
  (file-write-date stream))

(defmacro atomically (&body forms)
  `(MP:WITHOUT-PREEMPTION ,@forms))

(defvar *initial-terminal-io* #+lispworks *terminal-io*)

(defmacro errorset (&body forms)
  (with-unique-names (errorp return-values)
    `(LET* ((,errorp T)
            (,return-values
             (CSF/UTILITY:IGNORE-ERRORS-UNLESS-DEBUGGING
               (PROG1
                   (MULTIPLE-VALUE-LIST
                    (PROGN ,@forms))
                 (SETQ ,errorp NIL)))))
       (UNLESS ,errorp
         (APPLY #'VALUES T ,return-values)))))

(defmacro fast (&body forms)
  `(LOCALLY (DECLARE ,(performance-optimizations))
     ,@forms))

(define-condition socket-chunking-end-of-file (csf/utility:changesafe-condition)
  ((format-arguments :initform nil)
   (format-control :initform "~1@<The stream ~s had a chunking end of file~:~@>")))

(defmacro named-function (name lambda)
  (destructure-function-lambda
   nil lambda
   (lambda (bvl docstring declarations body)
     `(NAMED-LAMBDA ,name ,bvl ,@declarations ,docstring ,@body))
   (lambda ()
     (error "NAMED-FUNCTION must have a literal lambda."))))

(defvar *cl-default-special-bindings* '())

(defconstant *stream-property-hash-table*
    (let ((table (if (boundp '*stream-property-hash-table*)
                     (symbol-value '*stream-property-hash-table*)
                     (make-hash-table
                      :test #'equal
                    #+allegro :values #+allegro :weak))))
    #+lispworks (hcl:set-hash-table-weak table t)
    table))

(defun stream-property-list (stream)
  (let ((probe (gethash stream *stream-property-hash-table*)))
    (or probe
        (let ((plist (list :lock nil)))
          (setf (gethash stream *stream-property-hash-table*) plist)
          plist))))

(defun (setf stream-property-list) (new-value stream)
  (setf (gethash stream *stream-property-hash-table*) new-value))



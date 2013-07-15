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
  (export '(
            command-line->argument-list
            required-command-line-argument
            optional-command-line-argument
            keyword-flag-command-line-argument
            keyword-single-command-line-argument
            keyword-multiple-command-line-argument
            )))

(defun command-line-keyword? (platform argument)
  (check-type platform platform)
  (check-type argument string)
  (when (and (> (string-length argument) 1)
             (member (char argument 0) (platform/command-line-switch platform)))
    (find-symbol (string-upcase (subseq argument 1)) (keyword-package))))

(defclass command-line-argument ()
  ((type :initarg :type
         :initform (error "Required initarg :type omitted.")
         :reader command-line-argument/type)
   (description-key :initarg :description-key
                    :initform (error "Required initarg :description-key omitted.")
                    :reader command-line-argument/description-key
                    :type keyword))
  (:documentation "Base type of command-line-argument descriptor."))

(defclass required-command-line-argument (command-line-argument)
  ()
  (:documentation "A positional command line argument that must be supplied."))

(defclass optional-command-line-argument (command-line-argument)
  ((compute-default-value 
    :initarg :compute-default-value
    :initform nil
    :reader command-line-argument/compute-default-value))
  (:documentation "A positional command line argument that may or may not be supplied."))

(defclass keyword-command-line-argument (command-line-argument)
  ((compute-default-value
    :initarg :compute-default-value
    :initform nil
    :reader command-line-argument/compute-default-value)
   (key :initarg :key
        :initform (error "Required initarg :key omitted.")
        :reader command-line-argument/key))
  (:documentation "Base class for key-specified command-line arguments."))

(defun command-line-argument/default-value (command-line-argument)
  (funcall (command-line-argument/compute-default-value command-line-argument)))

(defclass keyword-flag-command-line-argument (keyword-command-line-argument)
  ()
  (:documentation "A keyword argument that is true if specified, false otherwise."))

(defclass keyword-single-command-line-argument (keyword-command-line-argument)
  ()
  (:documentation "A keyword argument that must be followed by a single value when specified."))

(defclass keyword-multiple-command-line-argument (keyword-command-line-argument)
  ()
  (:documentation "A keyword argument that consumes all values up to the next keyword argument."))

(defun command-line->argument-list (platform command-line argument-specs)
  "A command line is a list of the literal strings given when starting
   the process.  We convert these strings to an argument list suitable
   for APPLY.  Argument specs is an alist describing the valid arguments."
  ;; The CAR of the command-line is assumed to be the name of the executable.
  (labels ((parse-required (unprocessed argspecs required)
             (cond ((null unprocessed)
                    (if (or (null argspecs) 
                            (not (typep (car argspecs) 'required-command-line-argument)))
                        (nreconc required
                                 (map 'list #'command-line-argument/default-value argspecs))
                        (error 'changesafe-command-line-error
                               :message :required-argument-omitted)))
                   ((null argspecs)
                    (error 'changesafe-command-line-error
                           :message :too-many-arguments))

                   ((typep (car argspecs) 'required-command-line-argument)
                    (parse-required (cdr unprocessed) (cdr argspecs)
                                    (cons (parse-argument platform (car unprocessed) (car argspecs))
                                          required)))

                   (t (parse-optional unprocessed argspecs required '()))))

           (parse-optional (unprocessed argspecs required optional)
             (cond ((null unprocessed) (dolist (remaining argspecs (nreconc required (nreverse optional)))
                                         (push (command-line-argument/default-value remaining) optional)))

                   ((null argspecs) (error 'changesafe-command-line-error
                                           :message :too-many-arguments))

                   ((typep (car argspecs) 'optional-command-line-argument)
                    (parse-optional (cdr unprocessed) (cdr argspecs)
                                    required
                                    (cons (parse-argument platform (car unprocessed) (car argspecs))
                                          optional)))

                   (t (parse-keyword unprocessed argspecs required optional '()))))

           (parse-keyword (unprocessed argspecs required optional keys)
             (cond ((null unprocessed) (dolist (remaining argspecs (nreconc required
                                                                            (nreconc optional
                                                                                     (nreverse keys))))
                                         (push (command-line-argument/key remaining) keys)
                                         (push (command-line-argument/default-value remaining) keys)))

                   ((null argspecs) (error 'changesafe-command-line-error
                                           :message :too-many-arguments))

                   ((command-line-keyword? platform (car unprocessed))
                    => (lambda (keyword)
                         (let ((argspec (find keyword argspecs :key #'command-line-argument/key)))
                           (cond ((null argspec) (error 'changesafe-command-line-error
                                                        :message :unrecognized-keyword))
                                 ((typep argspec 'keyword-flag-command-line-argument)
                                  (parse-keyword (cdr unprocessed)
                                                 (remove argspec argspecs)
                                                 required
                                                 optional
                                                 (cons t (cons (command-line-argument/key argspec) keys))))
                                 (t (error 'changesafe-command-line-error :message :foo))))))

                   (t
                    (error 'changesafe-command-line-error
                           :message :keyword-expected)))))

    (parse-required (cdr command-line) argument-specs '())))

(defun parse-argument (platform argument-string argument-spec)
  (declare (ignore platform))
  (cond ((eq (command-line-argument/type argument-spec) 'string) argument-string)
        ((eq (command-line-argument/type argument-spec) 'keyword) (find-symbol (string-upcase argument-string)
                                                                               (keyword-package)))
        ((eq (command-line-argument/type argument-spec) 'number) (parse-integer argument-string))
        (t (error 'changesafe-cond-failure))))

(defun testit ()
  (command-line->argument-list 
   (server-platform)
   '(nil "foo" "message")
   (list (make-instance 'required-command-line-argument
                        :type 'string
                        :description-key :name)
         (make-instance 'required-command-line-argument
                        :type 'keyword
                        :description-key :keyword)
         (make-instance 'optional-command-line-argument
                        :type 'number
                        :compute-default-value (lambda () 42)
                        :description-key :port)
         (make-instance 'keyword-flag-command-line-argument
                        :key :flag
                        :type 'boolean
                        :compute-default-value (lambda () nil)
                        :description-key :random-flag))))
                               

#||

(defun command-line-flag (name unprocessed processed argument-specs continue)
  (declare (ignore argument-specs))
  (funcall continue unprocessed (cons t (cons name processed))))

(defun command-line-file (name unprocessed processed argument-specs continue)
  (let ((file (parse-namestring (car unprocessed))))
    (unless (file-pathname? file)
      (error 'changesafe-command-line-usage
             :usage argument-specs))
    (funcall continue (cdr unprocessed) (cons file (cons name processed)))))

(defun command-line-absolute-directory (name unprocessed processed argument-specs continue)
  (let ((dir (parse-namestring (car unprocessed))))
    (unless (typep dir 'absolute-directory-pathname)
      (error 'changesafe-command-line-usage
             :usage argument-specs))
    (funcall continue (cdr unprocessed) (cons dir (cons name processed)))))

(defun command-line-absolute-directory (name unprocessed processed argument-specs continue)
  (let ((dir (parse-namestring (car unprocessed))))
    (unless (typep dir 'absolute-directory-pathname)
      (error 'changesafe-command-line-usage
             :usage argument-specs))
    (funcall continue (cdr unprocessed) (cons dir (cons name processed)))))
||#

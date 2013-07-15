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


;;; Start of condition hierarchy for changesafe

(in-package "CSF/UTILITY")

(proclaim (standard-optimizations))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(
            changesafe-command-line-usage
            changesafe-condition
            changesafe-cond-failure
            changesafe-consistency
            changesafe-database-error
            changesafe-database-recovery-error
            changesafe-end-of-file
            changesafe-error
            changesafe-parse-error
            changesafe-range-error
            changesafe-schema-upgrade
            changesafe-schema-mismatch
            changesafe-serious-condition
            changesafe-simple-condition
            changesafe-simple-error
            changesafe-simple-type-error
            changesafe-simple-warning
            changesafe-stream-closed-error
            changesafe-stream-error
            changesafe-type-error
            changesafe-unimplemented-function
            changesafe-utf-8-format
            changesafe-utf-8-unexpeced-octet
            changesafe-utf-8-illegal-octet
            changesafe-utf-8-sequence-too-short
            changesafe-utf-8-sequence-too-long
            changesafe-utf-8-illegal-code-point
            changesafe-utf-8-utf-16-surrogate
            changesafe-warning
            check-range
            )))

(define-condition changesafe-condition (condition)
  ((subsystem :initarg :subsystem
              :reader changesafe-condition/subsystem
              :type string))
  (:documentation "Root condition for changesafe.")
  (:report (lambda (condition stream)
             (format stream
                     " The Changesafe ~a subsystem raised a ~a. ~
                      ~%This is unusual because raw conditions should not exist."
                     (changesafe-condition/subsystem condition)
                     (type-of condition)))))

(define-condition changesafe-simple-condition (simple-condition changesafe-condition)
  ()
  (:documentation "A condition raised by WARN or ERROR in the changesafe system.")
  (:report (lambda (condition stream)
             (format stream "~?~&[ChangeSafe internal condition, subsystem ~a]"
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)
                     (changesafe-condition/subsystem condition)))))

(define-condition changesafe-warning (warning changesafe-condition)())
(define-condition changesafe-simple-warning (changesafe-simple-condition changesafe-warning)())

(define-condition changesafe-serious-condition (serious-condition changesafe-condition)())
(define-condition changesafe-error (cl:error changesafe-serious-condition) ())

(define-condition changesafe-simple-error (changesafe-simple-condition changesafe-error) ())
(define-condition changesafe-consistency (changesafe-error)
  ()
  (:documentation "A condition raised when an internal sanity check fails."))

(define-condition changesafe-cond-failure (changesafe-consistency)
  ()
  (:documentation "A condition raised when a COND that cannot logically fail does so."))

(define-condition changesafe-stream-error (stream-error changesafe-error)
  ((stream :initarg stream
           :reader changesafe-stream-error/stream
           :type stream)))

(define-condition changesafe-stream-closed-error (changesafe-stream-error) ()
  (:report (lambda (condition stream)
             (format stream "The stream ~s has been closed."
                     (changesafe-stream-error/stream condition)))))

(define-condition changesafe-end-of-file (end-of-file changesafe-stream-error)())
(define-condition changesafe-parse-error (parse-error changesafe-stream-error)())

(define-condition changesafe-type-error (type-error changesafe-error) ())
(define-condition changesafe-simple-type-error (simple-type-error
                                                changesafe-simple-condition
                                                changesafe-type-error)
  ()
  (:report (lambda (condition stream)
             (format stream #.(concatenate 'string
                                           "The value ~s, is not of expected type ~s."
                                           "~&[ChangeSafe internal error, subsystem ~a]")
                     (type-error-datum condition)
                     (type-error-expected-type condition)
                     (changesafe-condition/subsystem condition)))))

(define-condition changesafe-range-error (changesafe-error)
  ((datum :initarg :datum
          :accessor changesafe-range-error/datum)
   (minimum :initarg :minimum
            :accessor changesafe-range-error/minimum
            :type number)
   (maximum :initarg :maximum
            :accessor changesafe-range-error/maximum
            :type number))
  (:documentation "Error raised when an index is out of range.")
  (:report (lambda (condition stream)
             (format stream #.(concatenate 'string
                                         "The value ~s is not in the range [~s, ~s)."
                                         "~&[ChangeSafe internal error, subsystem ~a]")
                     (changesafe-range-error/datum condition)
                     (changesafe-range-error/minimum condition)
                     (changesafe-range-error/maximum condition)
                     (changesafe-condition/subsystem condition)))))

(define-condition changesafe-unimplemented-function (changesafe-error)
  ((name :initarg :name
         :accessor changesafe-unimplemented-function/name
         :type symbol)
   (arguments :initarg :arguments
              :accessor changesafe-unimplemented-function/arguments
              :type list))
  (:documentation "Error raised by stub functions.")
  (:report (lambda (condition stream)
             (format stream #.(concatenate 'string
                                           "The unimplemented function ~s was invoked on arguments ~s."
                                           "~&[ChangeSafe internal error, subsystem ~a]")
                     (changesafe-unimplemented-function/name condition)
                     (changesafe-unimplemented-function/arguments condition)
                     (changesafe-condition/subsystem condition)))))

(defun changesafe-warn (subsystem datum &rest arguments)
  (if (stringp datum)
      (cl:warn 'changesafe-simple-warning
               :subsystem subsystem
               :format-control datum
               :format-arguments arguments)
      (cl:warn (apply #'make-instance datum
                      :subsystem subsystem
                      arguments))))

(defun changesafe-error (subsystem datum &rest arguments)
  (if (stringp datum)
      (cl:error 'changesafe-simple-error
                :subsystem subsystem
                :format-control datum
                :format-arguments arguments)
      (cl:error (apply #'make-instance datum
                       :subsystem subsystem
                       arguments)))
  (changesafe-error "CSF/UTILITY"
                    "Internal error within the error handler.  Continuing is not possible."))

(defmacro error (&rest arguments)
  `(CHANGESAFE-ERROR ,(package-name *package*) ,@arguments))

;;; Have to do this because we are shadowing the error symbol.
;;; Insert jrm's package system rant here.
(define-condition error (changesafe-error) ())

(defmacro warn (&rest arguments)
  `(CHANGESAFE-WARN ,(package-name *package*) ,@arguments))

(defmacro check-type (place typespec)
  `(ASSERT (TYPEP ,PLACE ',typespec) (,place)
           'changesafe-simple-type-error
           :format-string  #.(concatenate 'string
                                           "The value ~s, is not of expected type ~s."
                                           "~&[ChangeSafe internal error, subsystem ~a]")
           :format-arguments (list ,place ',typespec ,(package-name *package*))
           :subsystem ,(package-name *package*)
           :datum ,place
           :expected-type ',typespec))

(defmacro check-range (place min max)
  `(ASSERT (AND (<= ,min ,place)
                (< ,place ,max))
           (,place)
           'changesafe-range-error
           :subsystem ,(package-name *package*)
           :datum ,place
           :minimum ,min
           :maximum ,max))

(define-condition changesafe-utf-8-format (changesafe-simple-error) ()
  (:documentation "Signalled when errors occur during parsing of a UTF-8 sequence.")
  (:report (lambda (condition stream)
             (format stream
                     #.(concatenate 'string
                                    " While decoding a UTF-8 sequence, ~?"
                                    "~&[ChangeSafe internal error, subsystem ~a]")
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)
                     (changesafe-condition/subsystem condition)))))

(define-condition changesafe-utf-8-unexpeced-octet (changesafe-utf-8-format)
  ((octet :initarg :octet
          :reader changesafe-condition/octet
          :type (unsigned-byte 8)))
  (:documentation "Signalled when a UTF-8 continuation octet is encountered without a multibyte octet.")
  (:default-initargs :format-control "an unexpected multibyte continuation octet was encountered."))

(define-condition changesafe-utf-8-illegal-octet (changesafe-utf-8-format)
  ((octet :initarg :octet
          :reader changesafe-condition/octet
          :type (unsigned-byte 8)))
  (:documentation "Signalled when an illegal UTF-8 octet is encountered.")
  (:default-initargs :format-control "an illegal octet was encountered."))

(define-condition changesafe-utf-8-sequence-too-short (changesafe-utf-8-format)
  ((bytes-expected :initarg :bytes-expected
                   :reader changesafe-condition/bytes-expected
                   :type fixnum)
   (bytes-read     :initarg :bytes-read
                   :reader changesafe-condition/bytes-read
                   :type fixnum))
  (:documentation "Signalled when a multibyte UTF-8 sequence has too few continuation bytes.")
  (:default-initargs
   :format-control "a multibyte sequence with too few bytes was encountered."))

(define-condition changesafe-utf-8-sequence-too-long (changesafe-utf-8-format)
  ((bytes-read     :initarg :bytes-read
                   :reader changesafe-condition/bytes-read
                   :type fixnum))
  (:documentation "Signalled when a multibyte UTF-8 sequence is longer than necessary to encode the character.")
  (:default-initargs
   :format-control "an overlong multibyte sequence was encountered."))

(define-condition changesafe-utf-8-illegal-code-point (changesafe-utf-8-format)
  ((code-point :initarg :code-point
               :reader changesafe-condition/code-point
               :type (unsigned-byte 31)))
  (:documentation "Signalled when a UTF-8 sequence decodes into an illegal Unicode code point.")
  (:default-initargs
   :format-control "a sequence decoding to an illegal value was encountered."))

(define-condition changesafe-utf-8-utf-16-surrogate (changesafe-utf-8-format)
  ((code-point :initarg :code-point
               :reader changesafe-condition/code-point
               :type bmp-s-zone-code))
  (:documentation "Signalled when a UTF-8 sequence decodes into a UTF-16 surrogate.")
  (:default-initargs
   :format-control "a sequence decoding to a UTF-16 surrogate was encountered."))

(define-condition changesafe-utf-8-format (changesafe-simple-error) ()
  (:documentation "Signalled when errors occur during parsing of a UTF-8 sequence.")
  (:report (lambda (condition stream)
             (format stream
                     #.(concatenate 'string
                                    " While decoding a UTF-8 sequence, ~?"
                                    "~&[ChangeSafe internal error, subsystem ~a]")
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)
                     (changesafe-condition/subsystem condition)))))

(define-condition changesafe-database-error (changesafe-simple-error) ()
  (:documentation "Signalled when errors concerning the database occur."))

(define-condition changesafe-database-recovery-error (changesafe-database-error) ()
  (:documentation "Signalled when errors are found during database recovery."))

(define-condition changesafe-schema-upgrade (changesafe-warning) ()
  (:documentation "Signalled when image schema exceeds persistent schema."))

(define-condition changesafe-schema-mismatch (changesafe-database-recovery-error) ()
  (:documentation "Signalled when the persistent schema exceeds the image schema."))

(define-condition changesafe-command-line-usage (changesafe-simple-error) ()
  (:documentation "Signalled when illegal command line arguments are given.")
  (:default-initargs
   :format-control "An illegal command line argument was given."))

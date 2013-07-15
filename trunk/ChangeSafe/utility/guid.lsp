;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002,2003,2004 ChangeSafe, LLC
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

(eval-when (:compile-toplevel :load-toplevel)
  (export '(
            guid?
            generate-guid
            guid
            guid<?
            read-guid
            write-guid
            )))

(eval-when (:compile-toplevel :load-toplevel)
 (defconstant *bits-per-guid* 128)
 (defconstant *bits-per-byte* 8)
 (defconstant *bytes-per-guid* (/ *bits-per-guid* *bits-per-byte*))
 )

(defstruct (guid
            (:conc-name %guid/)
            (:constructor %make-guid (hash byte-array))
            (:copier nil)
            (:predicate guid?))
  (hash 0 :read-only t :type fixnum)
  (byte-array (make-array *bytes-per-guid*
                          :element-type `(unsigned-byte 8))
              :read-only t
              :type (simple-vector-8b 16)))

(defun guid->bignum (guid)
  (reduce (lambda (byte result)
            (+ (ash result *bits-per-byte*) byte))
          (%guid/byte-array guid)
          :from-end t))

(defvar *guid-hash-table*
    (let ((table (if (boundp '*guid-hash-table*)
                     *guid-hash-table*
                     (make-hash-table :test #'eql
                                      #+allegro :values #+allegro :weak))))
      #+lispworks (hcl:set-hash-table-weak table t)
      table))

(defun bignum->guid (bignum)
  (check-type bignum (integer 0 (#.(expt 2 128))))
  (let* ((probe (gethash bignum *guid-hash-table*))
         (result (or probe
                     (let ((bytes (make-array *bytes-per-guid* :element-type `(UNSIGNED-BYTE ,*bits-per-byte*))))
                       (declare (type (simple-vector-8b 16) bytes))
                       (dotimes (i 16)
                         (setf (aref bytes i) (ldb (byte *bits-per-byte* (* i *bits-per-byte*)) bignum)))
                       (%make-guid (hash-byte-vector bytes) bytes)))))
    (unless probe
      (setf (gethash bignum *guid-hash-table*) result))
    result))

(defmethod make-load-form ((object guid) &optional environment)
  (declare (ignore environment))
  `(bignum->guid ,(guid->bignum object)))

(defun format-guid-bytes (guid stream)
  (let ((bytes (%guid/byte-array guid)))
    (format stream "~2,'0x~2,'0x~2,'0x~2,'0x-~
                    ~2,'0x~2,'0x-~
                    ~2,'0x~2,'0x-~
                    ~2,'0x~2,'0x-~
                    ~2,'0x~2,'0x~2,'0x~2,'0x~2,'0x~2,'0x"
            (aref bytes 15)
            (aref bytes 14)
            (aref bytes 13)
            (aref bytes 12)
            (aref bytes 11)
            (aref bytes 10)
            (aref bytes  9)
            (aref bytes  8)
            (aref bytes  7)
            (aref bytes  6)
            (aref bytes  5)
            (aref bytes  4)
            (aref bytes  3)
            (aref bytes  2)
            (aref bytes  1)
            (aref bytes  0))))

(defmethod print-object ((guid guid) stream)
  (write-string "#{" stream)
  (format-guid-bytes guid stream)
  (write-char #\} stream))

(defun read-guid (stream)
  "Read a 16 bytes of the GUID in binary form from STREAM."
  (let ((bytes (make-array *bytes-per-guid* :element-type `(unsigned-byte ,*bits-per-byte*))))
    (read-sequence bytes stream)
    ;; do it this way so that GUID's are EQ
    (bignum->guid
     (reduce (lambda (byte result)
               (+ (ash result *bits-per-byte*) byte))
             bytes
             :from-end t))))

(defun write-guid (guid stream)
  "Write a 16-byte GUID in binary form to STREAM."
  (write-sequence (%guid/byte-array guid) stream))

(define-condition guid-syntax (changesafe-simple-condition changesafe-parse-error)
  ()
  (:documentation "Raised when GUID could not be parsed."))

(defun |#{-reader| (stream subchar arg)
  (declare (ignore arg subchar))
  (let* ((buffer (make-string 37))
         (bignum 0)
         (bytes-read (read-sequence buffer stream)))

    ;; check micro-syntax
    (unless (dotimes (i bytes-read t)
              (cond ((member i '(8 13 18 23))
                     (unless (eql (char buffer i) #\-) (return nil)))

                    ((= i 36)
                     (unless (eql (char buffer i) #\}) (return nil)))
                    (t
                     (unless (member (char buffer i)
                                     '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
                                       #\8 #\9 #\a #\b #\c #\d #\e #\f
                                       #\A #\B #\C #\D #\E #\F))
                       (return nil)))))
      (error 'guid-syntax
             :stream stream
             :format-control "The string ~s is not legal syntax for a GUID."
             :format-arguments (list (subseq buffer 0
                                             (or (position #\} buffer)
                                                 bytes-read)))))

    (unless (= bytes-read 37)
      (error 'changesafe-end-of-file :stream stream))

    (flet ((push-byte (byte)
             (setq bignum (+ (ash bignum *bits-per-byte*) byte))))

      (push-byte (parse-integer buffer :start 0  :end 2  :radix 16))
      (push-byte (parse-integer buffer :start 2  :end 4  :radix 16))
      (push-byte (parse-integer buffer :start 4  :end 6  :radix 16))
      (push-byte (parse-integer buffer :start 6  :end 8  :radix 16))

      (push-byte (parse-integer buffer :start 9  :end 11 :radix 16))
      (push-byte (parse-integer buffer :start 11 :end 13 :radix 16))

      (push-byte (parse-integer buffer :start 14 :end 16 :radix 16))
      (push-byte (parse-integer buffer :start 16 :end 18 :radix 16))

      (push-byte (parse-integer buffer :start 19 :end 21 :radix 16))
      (push-byte (parse-integer buffer :start 21 :end 23 :radix 16))

      (push-byte (parse-integer buffer :start 24 :end 26 :radix 16))
      (push-byte (parse-integer buffer :start 26 :end 28 :radix 16))
      (push-byte (parse-integer buffer :start 28 :end 30 :radix 16))
      (push-byte (parse-integer buffer :start 30 :end 32 :radix 16))
      (push-byte (parse-integer buffer :start 32 :end 34 :radix 16))
      (push-byte (parse-integer buffer :start 34 :end 36 :radix 16)))
    (bignum->guid bignum)))

(defvar *beginning-guid* 0)
(defvar *guid-step* 0)

;;; Need to ensure that the GC switches are
;;; autoloaded.  This accomplishes that and adds yet some
;;; more randomness.
(eval-when (:compile-toplevel :load-toplevel)
(defparameter *load-time-entropy*
  #+lispworks
  (with-output-to-string (*trace-output*)
    (hcl:extended-time
     (labels ((waste-time (n)
                (if (< n 2)
                    n
                    (+ (waste-time (- n 1))
                       (waste-time (- n 2))))))
         (waste-time 25))))))

(eval-when (:load-toplevel :execute)
  (let* ((state   (make-random-state t)))
    (setq *beginning-guid* 0)
    (flet ((entropy-step (format-string &rest args)
             (setq *beginning-guid*
                   (+ (ash *beginning-guid* *bits-per-byte*)
                      (ldb (byte *bits-per-byte* 0)
                           (hash-simple-string
                            (format nil "~d~d~?" *beginning-guid* (random 256 state)
                                    format-string args)))))
             (setq *beginning-guid*
                   (logand *beginning-guid* #.(1- (expt 2 128))))))

      ;; Should call entropy-step on as many externally random
      ;; uncorrelated things as possible.  The set below is nowhere
      ;; near good enough, but will do for now.
      (entropy-step "~a" (with-output-to-string (*standard-output*)
                           (room t)))
      (entropy-step "~a" (get-internal-run-time))
      (entropy-step "~a" (get-internal-real-time))
      (entropy-step "~s" (copy-readtable))
      (entropy-step "~a" (machine-instance))
      #+lispworks
      (entropy-step "~a" *load-time-entropy*)
      #+lispworks
      (entropy-step "~a"
                    (with-output-to-string (*trace-output*)
                      (hcl:extended-time
                       (labels ((waste-time (n)
                                  (if (< n 2)
                                      n
                                      (+ (waste-time (- n 1))
                                         (waste-time (- n 2))))))
                         (waste-time 25)))))
      (dotimes (i 10) (entropy-step "~a" nil))))

  ;; select a step size relatively prime to the guid range
  (setq *guid-step* (n-digit-prime 128)))

(defun generate-guid ()
  (incf *beginning-guid* *guid-step*)
  (when (>= *beginning-guid* #.(expt 2 128))
    (decf *beginning-guid* #.(expt 2 128))
    (let ((timestamp (timestamp-allocate)))
      (incf *beginning-guid* (logxor (timestamp/universal-time timestamp)
                                     (timestamp/sequence-number timestamp))))
    (when (>= *beginning-guid* #.(expt 2 128))
      (decf *beginning-guid* #.(expt 2 128))))
  (bignum->guid *beginning-guid*))

(defun guid<? (left right)
  (declare #.(performance-optimizations))
  (and (not (eq left right))
       (let ((left-bytes  (%guid/byte-array left))
             (right-bytes (%guid/byte-array right))
             (left-hash   (%guid/hash left))
             (right-hash  (%guid/hash right)))
         (declare (type (simple-vector-8b 16) left-bytes right-bytes)
                  (type fixnum left-hash right-hash))
         (if (= left-hash right-hash)
             (or (dotimes (i 8 nil)
                   (let ((lv (aref left-bytes (- 15 (* i 2))))
                         (rv (aref right-bytes (- 15 (* i 2)))))
                     (cond ((< lv rv) (return-from guid<? t))
                           ((> lv rv) (return-from guid<? nil))
                           (t nil))))
                 (dotimes (i 8 nil)
                   (let ((lv (aref left-bytes (* i 2)))
                         (rv (aref right-bytes (* i 2))))
                     (cond ((< lv rv) (return-from guid<? t))
                           ((> lv rv) (return-from guid<? nil))
                           (t nil)))))
             (< left-hash right-hash)))))

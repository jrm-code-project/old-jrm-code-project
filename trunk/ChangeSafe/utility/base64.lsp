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
;;;; File Name:     base64.lsp
;;;; Author:        naha
;;;; Creation Date: August 1999
;;;;
;;;; Module Description: base64 binary encodeing
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CSF/UTILITY")

(proclaim (standard-optimizations))

;;; Encoding and decoding of Base64 format.
;;; See RFCs 1113 and 1341.

;;; This currently makes no attempt at line wrapping since we are (for
;;; now) just using it for "Basic" HTTP authentication (RFC2617).

(eval-when (:load-toplevel :execute)
  (export '(base64-encode-string
            base64-decode-string-to-string
            )))


(defconstant +base64-pad-char+ #\=
  "The pad character that is used to pad the encoding to a 4 character multiple.")

(defconstant +base64-encoding-chars+
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-"
  "Indexed by a 6 bit integer, this string provides the base64 encoding
character for that integer.")

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant +base64-decoding-vector+
    ;; Vector indexed by character code.
    (let ((vector (if (boundp '+base64-decoding-vector+)
                      (symbol-value '+base64-decoding-vector+)
                      (make-array 256 :initial-element :error))))
      (flet ((set-char-decoding (char value)
               (setf (svref vector (char-code char)) value)))
        (dotimes (index (string-length +base64-encoding-chars+))
          (set-char-decoding (schar +base64-encoding-chars+ index) index))
        (set-char-decoding +base64-pad-char+ :pad))
      vector)
  "This provides the inverse mapping from +BASE64-ENCODING-CHARS+."))

(defparameter +base64-ignore-chars+ '(#\space #\tab)
  "These are characters which the base64 decoder should inore.")

(eval-when (:compile-toplevel)
  ;; This macro isn't expected to be used except while compiling the base64 utilities.
  ;; It needn't be cluttering up the runtime environment.
  (defmacro with-bitpacking (bitpacker-name (destination-size source-size
                                             source-empty-test
                                             destination-empty-value
                                             &optional (destination-xformer nil
                                                        destination-xformer-p))
                             &body body)
    (let ((unpack-p (< source-size destination-size)))
      `(macrolet ((,bitpacker-name (destination
                                    source1 source1-bits-remaining
                                    &optional (source2 nil source2-p))
                    ;; The value stored in DESTINATION will either be
                    ;; DESTINATION-EMPTY-VALUE or the result of applying
                    ;; DESTINATION-XFORMER (if there is one) to the
                    ;; bitpacking result.
                    (let* ((source1-bits-used (min source1-bits-remaining ,destination-size))
                           (source1-bytespec (byte source1-bits-used
                                                   (- source1-bits-remaining source1-bits-used)))
                           (destination-bytespec (byte source1-bits-used
                                                       (- ,destination-size source1-bits-used)))
                           (source2-bits-used (- ,destination-size source1-bits-used))
                           (source2-bytespec (byte source2-bits-used
                                                   (- ,source-size
                                                      (- ,destination-size source1-bits-used)))))
                      `(let ((source1 ,source1)
                             ,@(when source2-p
                                 `((source2 ,source2))))
                         (setf ,destination
                           (if (or (funcall ,',source-empty-test source1)
                                   ,@(when ,unpack-p
                                       `((funcall ,',source-empty-test source2))))
                               ,',destination-empty-value
                             (let ((v
                                    ;; Don't bother with the LDB if
                                    ;; we're using the whole thing.
                                    ,(if (= ,destination-size source1-bits-used)
                                         `(ldb ',source1-bytespec source1)
                                       `(dpb (ldb ',source1-bytespec source1)
                                             ',destination-bytespec
                                             (if (funcall ,',source-empty-test source2)
                                                 0
                                               ;; Don't bother with
                                               ;; the LDB if we're
                                               ;; using the whole
                                               ;; thing.
                                               ,(if (= ,source-size source2-bits-used)
                                                    `source2
                                                  `(ldb ',source2-bytespec source2)))))))
                               ;; If there's a transforming function for
                               ;; the result value the use it.
                               ,(if ,destination-xformer-p
                                    `(funcall ,',destination-xformer v)
                                  `v))))))))
         ,@body)))
  )

(defun base64-encode-triple (byte1 &optional byte2 byte3)
  "Returns four characters representing the Base64 encoding of the three bytes."
  (declare (values encoded1 encoded2 encoded3 encoded4))
  ;; At least make sure the first byte isn't NIL since the caller
  ;; should have stopped by then.
  (check-type byte1 (unsigned-byte 8))
  (check-type byte2 (optional (unsigned-byte 8)))
  (check-type byte3 (optional (unsigned-byte 8)))
  (let (encoded1 encoded2 encoded3 encoded4)
    (flet ((encoded-char (value)
             (schar +base64-encoding-chars+ value)))
      (with-bitpacking base64-encoding-step (6 8 #'null +base64-pad-char+
                                             #'encoded-char)
        (base64-encoding-step encoded1 byte1 8)
        (base64-encoding-step encoded2 byte1 2 byte2)
        (base64-encoding-step encoded3 byte2 4 byte3)
        (base64-encoding-step encoded4 byte3 6)))
    (values encoded1 encoded2 encoded3 encoded4)))

(defun base64-decode-quad (encoded1 encoded2 encoded3 encoded4)
  "Decode four characters representing the Base64 encoding of three bytes
back into their three bytes."
  (declare (values byte1 byte2 byte3))
  (flet ((decode-char (char)
           (let ((decoded (svref +base64-decoding-vector+ (char-code char))))
             (when (eq decoded :error)
               (error "~s is not a valid base64 encoding character" char))
             decoded)))
    (let ((e1 (decode-char encoded1))
          (e2 (decode-char encoded2))
          (e3 (decode-char encoded3))
          (e4 (decode-char encoded4)))
      (let (byte1 byte2 byte3)
        (flet ((pad-p (c) (eq c :pad)))
          (with-bitpacking base64-decode-step (8 6 #'pad-p nil)
            (base64-decode-step byte1 e1 6 e2)
            (base64-decode-step byte2 e2 4 e3)
            (base64-decode-step byte3 e3 2 e4)))
        (values byte1 byte2 byte3)))))

#||

;;; Tests

(defun base64-triple-quad-test (triple)
  (unless (equal triple
                 (multiple-value-list
                  (apply #'base64-decode-quad
                         (multiple-value-list (apply #'base64-encode-triple triple)))))
    (format *error-output*
            "~&BASE64-TRIPLE-QUAD-TEST failed for triple ~s" triple)))

(defun test-base64-basic-operations ()
  (base64-triple-quad-test '(1 nil nil))
  (base64-triple-quad-test '(128 nil nil))
  (base64-triple-quad-test '(1 2 nil))
  (base64-triple-quad-test '(1 2 3))
  (base64-triple-quad-test '(255 255 255))
  (base64-triple-quad-test '(255 0 0))
  (base64-triple-quad-test '(255 0 nil))
  (base64-triple-quad-test '(255 nil nil))
  (base64-triple-quad-test '(0 0 0))
  (base64-triple-quad-test '(0 0 nil))
  (base64-triple-quad-test '(0 nil nil)))

||#

(defun base64-encode-string (string)
  "Encode the string to a base64 string."
  (check-type string string)
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (flet ((read-char-code-or-nil ()
               (let ((c (read-char in nil nil)))
                 (when c (char-code c)))))
        (loop
          (let ((c1 (read-char-code-or-nil))
                (c2 (read-char-code-or-nil))
                (c3 (read-char-code-or-nil)))
            (unless c1 (return))
            (multiple-value-bind (enc1 enc2 enc3 enc4)
                (base64-encode-triple c1 c2 c3)
              (write-char enc1 out)
              (write-char enc2 out)
              (write-char enc3 out)
              (write-char enc4 out))))))))

(defun base64-decode-string-to-string (string)
  "Decode the base64 encoded string, treating the decoded result as a string."
  ;; The quick and sloppy way.
  ;; More efficient would be to pre-allocate the destination string
  ;; based on the length of the source string and the number of pad
  ;; characters at the end.
  (check-type string string)
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (flet ((get-encoding-char ()
               (loop for c = (read-char in nil nil)
                   while c
                   do (unless (member c +base64-ignore-chars+
                                      :test #'char-equal)
                        (return c)))))
        (loop
          (let ((c1 (get-encoding-char))
                (c2 (get-encoding-char))
                (c3 (get-encoding-char))
                (c4 (get-encoding-char)))
            (unless c1 (return))
            (multiple-value-bind (v1 v2 v3)
                (base64-decode-quad c1 c2 c3 c4)
              (write-char (code-char v1) out)
              (when v2 (write-char (code-char v2) out))
              (when v3 (write-char (code-char v3) out))
              (unless v3 (return)))))))))

#||

(defun test-base64-strings ()
  (loop
      for s in '("xyzzy" "abc" "abcd" "abcde" "abcdef" "abcdefg" "abcdefgh")
      for encoded = (base64-encode-string s)
      for decoded = (base64-decode-string-to-string encoded)
      do (unless (string= s decoded)
           (format *error-output*
                   "~&~s encoded as ~s, decoded as ~s"
                   s encoded decoded))))

||#

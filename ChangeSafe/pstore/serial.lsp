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

(in-package "CSF/PERSISTENT-STORE")

(proclaim (standard-optimizations))

;;;; Serialization
;;;
;;; In order to make objects persistent, there must be a means
;;; to emit them to raw storage.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(serialization-code/dense-cid-set
            serialization-code/distributed-identifier
            serialize
            deserialize-dispatch
            serialization-code/change-context-filechange
            serialization-code/change-context-fileadd
            serialization-code/change-context-filerename
            serialization-code/change-context-fileremove
            serialization-code/change-context-new-filerename
            serialization-code/change-context-new-fileremove
            )))

(eval-when (:compile-toplevel :load-toplevel)

  (defconstant serialization-code/illegal-00 0)
  ;; if we make these mnemonic, then we can view the database
  ;; in emacs and make some (not much) sense of it.  Helps debugging.
  ;; DO NOT CHANGE THIS FIRST ONE!!!  IT MUST BE AN OPEN PAREN.
  (defconstant serialization-code/commit-record (char-code #\())

  (defconstant serialization-code/cons               (char-code #\.))
  (defconstant serialization-code/complex            (char-code #\C))
  (defconstant serialization-code/character          (char-code #\%))
  (defconstant serialization-code/bignum             (char-code #\B))
  (defconstant serialization-code/positive-fixnum    (char-code #\#))
  (defconstant serialization-code/negative-fixnum    (char-code #\-))
  (defconstant serialization-code/short-float        (char-code #\S))
  (defconstant serialization-code/single-float       (char-code #\F))
  (defconstant serialization-code/double-float       (char-code #\D))
  (defconstant serialization-code/long-float         (char-code #\L))
  (defconstant serialization-code/guid               (char-code #\{))
  (defconstant serialization-code/keyword            (char-code #\:))
  (defconstant serialization-code/nil                (char-code #\X))
  (defconstant serialization-code/package            (char-code #\P))
  (defconstant serialization-code/pathname           (char-code #\p))
  (defconstant serialization-code/initializer        (char-code #\I))
  (defconstant serialization-code/logical-pathname   (char-code #\L))
  (defconstant serialization-code/media-type         (char-code #\M))
  (defconstant serialization-code/reference          (char-code #\R))
  (defconstant serialization-code/ratio              (char-code #\/))
  (defconstant serialization-code/simple-vector      (char-code #\V))
  (defconstant serialization-code/simple-bit-vector  (char-code #\b))
  (defconstant serialization-code/string             (char-code #\"))
  (defconstant serialization-code/symbol             (char-code #\$))
  (defconstant serialization-code/t                  (char-code #\t))
  (defconstant serialization-code/dense-cid-set      (char-code #\c))
  (defconstant serialization-code/distributed-identifier (char-code #\Z))
  (defconstant serialization-code/change-context-filechange     (char-code #\0)) ;zero
  (defconstant serialization-code/change-context-fileadd        (char-code #\1))
  (defconstant serialization-code/change-context-filerename     (char-code #\2))
  (defconstant serialization-code/change-context-fileremove     (char-code #\3))
  (defconstant serialization-code/change-context-new-filerename (char-code #\4))
  (defconstant serialization-code/change-context-new-fileremove (char-code #\5))
  (defconstant serialization-code/illegal-ff #xff))

(defvar-unbound *serialization-table*
  "A table of objects that are being serialized, for supporting
   serialization of circular objects.")

(defvar *deserialized-initializers* '())

(defstruct (initializer
            (:conc-name initializer/)
            (:constructor make-initializer (class schema-version init-plist))
            (:copier nil)
            (:predicate initializer?))
  (class        nil :read-only t   :type persistent-standard-class)
  (schema-version 0 :read-only t   :type non-negative-fixnum)
  (init-plist   '() :read-only t   :type list))

(defmethod print-object ((object initializer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "for ~s.~d"
            (class-name (initializer/class object))
            (initializer/schema-version object))))

(defgeneric serialize (object stream symbol-table)
  (:documentation
   "Convert a lisp object into a form suitable for putting
    into a file.")

  (:method ((object null) stream symbol-table)
    (declare (ignore symbol-table))
    (write-byte serialization-code/nil stream))

  (:method ((object (eql t)) stream symbol-table)
    (declare (ignore symbol-table))
    (write-byte serialization-code/t stream))

  (:method ((object bignum) stream symbol-table)
    (write-byte serialization-code/bignum stream)
    (multiple-value-bind (quo rem) (truncate object most-positive-fixnum)
      (serialize rem stream symbol-table)
      (serialize quo stream symbol-table)))

  (:method ((object character) stream symbol-table)
    (declare (ignore symbol-table))
    (write-byte serialization-code/character stream)
    (write-unsigned16 (char-code object) stream))

  (:method ((object complex) stream symbol-table)
    (write-byte serialization-code/complex stream)
    (serialize (realpart object) stream symbol-table)
    (serialize (imagpart object) stream symbol-table))

  (:method ((object fixnum) stream symbol-table)
    (if (minusp object)
        (progn
          (write-byte serialization-code/negative-fixnum stream)
          (write-fixnum (- object) stream))
        (progn
          (write-byte serialization-code/positive-fixnum stream)
          (write-fixnum object stream))))

  (:method ((object single-float) stream symbol-table)
    (write-byte serialization-code/single-float stream)
    (multiple-value-bind (signif expon sign) (integer-decode-float object)
      (serialize signif stream symbol-table)
      (serialize expon  stream symbol-table)
      (serialize sign   stream symbol-table)))

  (:method ((object double-float) stream symbol-table)
    (write-byte serialization-code/double-float stream)
    (multiple-value-bind (signif expon sign) (integer-decode-float object)
      (serialize signif stream symbol-table)
      (serialize expon  stream symbol-table)
      (serialize sign   stream symbol-table)))

#||
  (:method ((object short-float) stream symbol-table)
    (write-byte serialization-code/short-float stream)
    (multiple-value-bind (signif expon sign) (integer-decode-float object)
      (serialize signif stream symbol-table)
      (serialize signif expon  symbol-table)
      (serialize signif sign   symbol-table)))

  (:method ((object long-float) stream symbol-table)
    (write-byte serialization-code/long-float stream)
    (multiple-value-bind (signif expon sign) (integer-decode-float object)
      (serialize signif stream symbol-table)
      (serialize expon  stream  symbol-table)
      (serialize sign   stream   symbol-table)))
||#

  (:method ((object logical-pathname) stream symbol-table)
    (declare (ignore symbol-table))
    (write-byte serialization-code/logical-pathname stream)
    (serialize (pathname-host object) stream symbol-table)
    (serialize (pathname-device object) stream symbol-table)
    (serialize (pathname-directory object) stream symbol-table)
    (serialize (pathname-name object) stream symbol-table)
    (serialize (pathname-type object) stream symbol-table)
    (serialize (pathname-version object) stream symbol-table))

  (:method ((object pathname) stream symbol-table)
    (declare (ignore symbol-table))
    (write-byte serialization-code/pathname stream)
    (serialize (pathname-host      object) stream symbol-table)
    (serialize (pathname-device    object) stream symbol-table)
    (serialize (pathname-directory object) stream symbol-table)
    (serialize (pathname-name      object) stream symbol-table)
    (serialize (pathname-type      object) stream symbol-table)
    (serialize (pathname-version   object) stream symbol-table))

  (:method ((object media-type) stream symbol-table)
    (write-byte serialization-code/media-type stream)
    (write-fixnum (symbol-table/intern-string symbol-table 
                                              (media-type/primary-type object))
                  stream)
    (write-fixnum (symbol-table/intern-string symbol-table 
                                              (media-type/subtype object))
                  stream))

  (:method ((object string) stream symbol-table)
    (declare (ignore symbol-table))
    (write-byte serialization-code/string stream)
    (write-fixnum (symbol-table/intern-string symbol-table object) stream)
    ;(write-fixnum (length object) stream)
    ;(write-string-data object stream)
    )

  (:method ((object guid) stream symbol-table)
    (declare (ignore symbol-table))
    (write-byte serialization-code/guid stream)
    (write-guid object stream))

  (:method ((object ratio) stream symbol-table)
    (write-byte serialization-code/ratio stream)
    (serialize (numerator object)   stream symbol-table)
    (serialize (denominator object) stream symbol-table))

  (:method ((object package) stream symbol-table)
    (write-byte serialization-code/package stream)
    (write-fixnum (symbol-table/intern-string symbol-table object) stream))

  (:method ((object symbol) stream symbol-table)
    (write-byte serialization-code/symbol stream)
    (write-fixnum (symbol-table/intern-symbol symbol-table object) stream))

  (:method ((object simple-vector) stream symbol-table)
    (write-byte serialization-code/simple-vector stream)
    (write-fixnum (simple-vector-length object) stream)
    (iterate ((element (scan 'vector object)))
      (serialize element stream symbol-table)))

  (:method ((object simple-bit-vector) stream symbol-table)
    (write-byte serialization-code/simple-bit-vector stream)
    (write-fixnum (simple-bit-vector-length object) stream)
    (write-simple-bit-vector object stream))

  (:method ((object cons) stream symbol-table)
    (write-byte serialization-code/cons stream)
    (serialize (car object) stream symbol-table)
    (serialize (cdr object) stream symbol-table))

  (:method ((object initializer) stream symbol-table)
    (write-byte serialization-code/initializer stream)
    (write-fixnum (symbol-table/intern-symbol symbol-table (class-name (initializer/class object))) stream)
    (write-fixnum (initializer/schema-version object) stream)
    (write-fixnum (length (initializer/init-plist object)) stream)
    (iterate (((key value) (scan-plist (initializer/init-plist object))))
      (write-fixnum (symbol-table/intern-symbol symbol-table key) stream)
      (serialize value stream symbol-table)))

  (:method ((object distributed-identifier) stream symbol-table)
    (write-byte serialization-code/distributed-identifier stream)
    (write-fixnum (symbol-table/intern-did symbol-table object) stream))

  )

(defgeneric deserialize-dispatch (code stream symbol-table)
  (:documentation "Convert a stream of bytes into a lisp object.")
;;; Hack for development.
  (:method ((code (eql serialization-code/commit-record)) stream symbol-table)
    'commit-record)

  (:method ((code (eql serialization-code/nil)) stream symbol-table)
     nil)

  (:method ((code (eql serialization-code/t)) stream symbol-table)
     t)

  (:method ((code (eql serialization-code/complex)) stream symbol-table)
    (let* ((realpart (deserialize-dispatch (read-byte stream) stream symbol-table))
           (imagpart (deserialize-dispatch (read-byte stream) stream symbol-table)))
      (complex realpart imagpart)))

  (:method ((code (eql serialization-code/single-float)) stream symbol-table)
    (let* ((signif (deserialize-dispatch (read-byte stream) stream symbol-table))
           (expon  (deserialize-dispatch (read-byte stream) stream symbol-table))
           (sign   (deserialize-dispatch (read-byte stream) stream symbol-table)))
      (* sign (scale-float (float signif 1.0f0) expon))))

  (:method ((code (eql serialization-code/double-float)) stream symbol-table)
    (let* ((signif (deserialize-dispatch (read-byte stream) stream symbol-table))
           (expon  (deserialize-dispatch (read-byte stream) stream symbol-table))
           (sign   (deserialize-dispatch (read-byte stream) stream symbol-table)))
      (* sign (scale-float (float signif 1.0d0) expon))))

#||
  (:method ((code (eql serialization-code/short-float)) stream symbol-table)
    (let* ((signif (deserialize-dispatch (read-byte stream) stream symbol-table))
           (expon  (deserialize-dispatch (read-byte stream) stream symbol-table))
           (sign   (deserialize-dispatch (read-byte stream) stream symbol-table)))
      (* sign (scale-float (float signif 1.0s0) expon))))

  (:method ((code (eql serialization-code/long-float)) stream symbol-table)
    (let* ((signif (deserialize-dispatch (read-byte stream) stream symbol-table))
           (expon  (deserialize-dispatch (read-byte stream) stream symbol-table))
           (sign   (deserialize-dispatch (read-byte stream) stream symbol-table)))
      (* sign (scale-float (float signif 1.0l0) expon))))
||#

  (:method ((code (eql serialization-code/logical-pathname)) stream symbol-table)
    (let* ((the-host (deserialize-dispatch (read-byte stream) stream symbol-table))
           (the-device (deserialize-dispatch (read-byte stream) stream symbol-table))
           (the-directory (deserialize-dispatch (read-byte stream) stream symbol-table))
           (the-name (deserialize-dispatch (read-byte stream) stream symbol-table))
           (the-type (deserialize-dispatch (read-byte stream) stream symbol-table))
           (the-version (deserialize-dispatch (read-byte stream) stream symbol-table)))
      (make-pathname :host the-host
                     :device the-device
                     :directory the-directory
                     :name the-name
                     :type the-type
                     :version the-version
                     :defaults "")))

  (:method ((code (eql serialization-code/pathname)) stream symbol-table)
    (let* ((the-host (deserialize-dispatch (read-byte stream) stream symbol-table))
           (the-device (deserialize-dispatch (read-byte stream) stream symbol-table))
           (the-directory (deserialize-dispatch (read-byte stream) stream symbol-table))
           (the-name (deserialize-dispatch (read-byte stream) stream symbol-table))
           (the-type (deserialize-dispatch (read-byte stream) stream symbol-table))
           (the-version (deserialize-dispatch (read-byte stream) stream symbol-table)))
      (make-pathname :host the-host
                     :device the-device
                     :directory the-directory
                     :name the-name
                     :type the-type
                     :version the-version
                     :defaults "")))

  (:method ((code (eql serialization-code/media-type)) stream symbol-table)
    (let* ((primary-type (symbol-table/resolve-symbol symbol-table (read-fixnum stream)))
           (subtype (symbol-table/resolve-symbol symbol-table (read-fixnum stream))))
      (find-media-type primary-type subtype)))

  (:method ((code (eql serialization-code/simple-vector)) stream symbol-table)
    (let* ((length (read-fixnum stream))
           (result (make-array length)))
      (dotimes (i length)
        (setf (svref result i) (deserialize-dispatch (read-byte stream) stream symbol-table)))
      result))

  (:method ((code (eql serialization-code/simple-bit-vector)) stream symbol-table)
    (let ((length (read-fixnum stream)))
      (read-simple-bit-vector length stream)))

  (:method ((code (eql serialization-code/cons)) stream symbol-table)
    (let* ((the-car (deserialize-dispatch (read-byte stream) stream symbol-table))
           (the-cdr (deserialize-dispatch (read-byte stream) stream symbol-table)))
      (cons the-car the-cdr)))

  (:method ((code (eql serialization-code/string)) stream symbol-table)
    (declare (ignore symbol-table))
    (symbol-table/resolve-string symbol-table (read-fixnum stream))
    ;(let ((length (read-fixnum stream)))
    ;(read-string-data length stream))
    )

  (:method ((code (eql serialization-code/guid)) stream symbol-table)
    (declare (ignore symbol-table))
    (read-guid stream))

  (:method ((code (eql serialization-code/package)) stream symbol-table)
    (declare (ignore symbol-table))
    (debug-message 5 "Deserializing package.")
    (let* ((package-name (symbol-table/resolve-string symbol-table (read-fixnum stream)))
           (package (find-package package-name)))
      (debug-message 5 "Deserializing package is ~s." package)
      (or package
          (error 'changesafe-database-recovery-error
                 :format-control "The package ~s was not found in the lisp image"
                 :format-arguments (list package-name)))))

  (:method ((code (eql serialization-code/symbol)) stream symbol-table)
    (symbol-table/resolve-symbol symbol-table (read-fixnum stream)))

  (:method ((code (eql serialization-code/positive-fixnum)) stream symbol-table)
    (declare (ignore symbol-table))
    (read-fixnum stream))

  (:method ((code (eql serialization-code/negative-fixnum)) stream symbol-table)
    (declare (ignore symbol-table))
    (- (read-fixnum stream)))

  (:method ((code (eql serialization-code/bignum)) stream symbol-table)
    (declare (ignore symbol-table))
    (let* ((remainder (deserialize-dispatch (read-byte stream) stream symbol-table))
           (quotient  (deserialize-dispatch (read-byte stream) stream symbol-table)))
      (+ remainder (* quotient most-positive-fixnum))))

  (:method ((code (eql serialization-code/initializer)) stream symbol-table)
    (let* ((class-name (symbol-table/resolve-symbol symbol-table (read-fixnum stream)))
           (class (or (find-class class-name)
                      (error 'changesafe-database-recovery-error
                             :format-control "Unknown persistent class ~s"
                             :format-arguments (list class-name))))
           (schema-version (read-fixnum stream))
           (init-plist-length (read-fixnum stream)))
      (do ((i 0 (+ i 2))
           (plist '() (let* ((symbol (symbol-table/resolve-symbol symbol-table (read-fixnum stream)))
                             (value  (deserialize-dispatch (read-byte stream) stream symbol-table)))
                        (cons value (cons symbol plist)))))
          ((>= i init-plist-length)
           (make-initializer class schema-version (nreverse plist))))))

  (:method ((code (eql serialization-code/distributed-identifier)) stream symbol-table)
    (symbol-table/resolve-did symbol-table (read-fixnum stream)))

  (:method ((code fixnum) stream symbol-table)
    (error 'changesafe-database-recovery-error
           :format-control "Illegal data code ~d at position ~d."
           :format-arguments (list code (file-position stream))))
  )

(defun deserialize (stream symbol-table offset)
  (file-position stream offset)
  (deserialize-dispatch (read-byte stream) stream symbol-table))

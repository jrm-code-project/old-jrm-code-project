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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Author:        Joe Marshall
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CSF/PERSISTENT-STORE")

(proclaim (standard-optimizations))

;;; API's exported from this module.
(eval-when (:load-toplevel :execute)
  (export '(
            symbol-table/intern-string
            symbol-table/resolve-string
            symbol-table/intern-symbol
            symbol-table/resolve-symbol
            symbol-table/intern-did
            symbol-table/resolve-did
            )))

;;; This is a bit of a misnamed structure.
;;; We use this table to intern a number of things
;;; including GUIDs, strings, symbols, etc.

(defstruct (interning-table
            (:conc-name interning-table/)
            (:constructor %make-interning-table (hash-table vector))
            (:copier nil))

  ;; The index of the last interned object.
  (committed-objects 0)
  ;; Hash table maps objects to indexes.
  hash-table
  ;; Vector maps them back.
  vector)

(defun make-interning-table (element-type hash-test initial-size)
  (%make-interning-table
   (make-hash-table :test hash-test)
   (make-array initial-size
               :adjustable t
               :fill-pointer 0
               :element-type element-type)))

(declaim (ftype (function (interning-table) array-index) interning-table/count)
         (ftype (function (interning-table t) array-index) interning-table/intern)
         (ftype (function (interning-table array-index) t) interning-table/resolve)
         (ftype (function (interning-table) t) interning-table/note-committed)
         (inline interning-table/count)
         (inline interning-table/resolve)
         (inline interning-table/note-committed))

(defun interning-table/count (interning-table)
  (vector-length (interning-table/vector interning-table)))

(defun interning-table/intern (interning-table object)
  (or (gethash object (interning-table/hash-table interning-table))
      (let ((index (interning-table/count interning-table)))
        ;; (debug-message 5 "Object ~s at index ~d" object index)
        (vector-push-extend object (interning-table/vector interning-table))
        (setf (gethash object (interning-table/hash-table interning-table)) index)
        index)))

(defun interning-table/resolve (interning-table index)
  (aref (interning-table/vector interning-table) index))

(defun interning-table/note-committed (interning-table)
  (setf (interning-table/committed-objects interning-table) (interning-table/count interning-table)))

(defun interning-table/write (interning-table stream element-writer)
  (let ((start (interning-table/committed-objects interning-table))
        (limit (interning-table/count             interning-table)))
    ;; (debug-message 5 "Writing interning table elements ~d to ~d" start limit)
    (write-fixnum start stream)
    (write-fixnum limit stream)
    (do ((index start (1+ index)))
        ((>= index limit))
      (funcall element-writer (interning-table/resolve interning-table index) stream))))

(defun interning-table/recover (interning-table stream element-reader)
  (let ((start (read-fixnum stream))
        (limit (read-fixnum stream))
        (vector (interning-table/vector interning-table))
        (table  (interning-table/hash-table interning-table)))
    (when (< (length vector) limit)
      (adjust-array vector (list limit) :fill-pointer limit))
    (do ((index start (1+ index)))
        ((>= index limit))
      (let ((item (funcall element-reader stream)))
        ;; (debug-message 5 "Object ~s => ~d" item index)
        (setf (aref vector index) item
              (gethash item table) index)))))

(defconstant +initial-symbol-table-strings+ 1000
  "Number of symbols initially allowed for in the symbol table.")

(defconstant +initial-symbol-table-symbols+ 1000
  "Number of symbols initially allowed for in the symbol table.")

(defconstant +initial-symbol-table-dids+ 1000
  "Number of symbols initially allowed for in the symbol table.")

;;; A simple persistent symbol table.
(defstruct (symbol-table
            (:conc-name symbol-table/)
            (:constructor make-symbol-table ())
            (:copier nil))

  (interned-strings (make-interning-table 'string #'equal +initial-symbol-table-strings+))
  (interned-symbols (make-interning-table 'symbol #'eq    +initial-symbol-table-symbols+))
  (interned-dids    (make-interning-table 'distributed-identifier #'eq +initial-symbol-table-dids+))
  )

(declaim (ftype (function (symbol-table)        array-index) symbol-table/string-count)
         (ftype (function (symbol-table string) array-index) symbol-table/intern-string)
         (ftype (function (symbol-table array-index) string) symbol-table/resolve-string)
         (ftype (function (symbol-table)        array-index) symbol-table/symbol-count)
         (ftype (function (symbol-table symbol) array-index) symbol-table/intern-symbol)
         (ftype (function (symbol-table array-index) symbol) symbol-table/resolve-symbol)
         (ftype (function (symbol-table)        array-index) symbol-table/did-count)
         (ftype (function (symbol-table symbol) array-index) symbol-table/intern-did)
         (ftype (function (symbol-table array-index) symbol) symbol-table/resolve-did)
         (inline symbol-table/string-count)
         (inline symbol-table/intern-string)
         (inline symbol-table/resolve-string)
         (inline symbol-table/symbol-count)
         ;(inline symbol-table/intern-symbol)
         (inline symbol-table/resolve-symbol)
         (inline symbol-table/did-count)
         ;(inline symbol-table/intern-did)
         (inline symbol-table/resolve-did))

(defun symbol-table/string-count (symbol-table)
  (interning-table/count (symbol-table/interned-strings symbol-table)))

(defun symbol-table/intern-string (symbol-table string)
  (interning-table/intern (symbol-table/interned-strings symbol-table) string))

(defun symbol-table/resolve-string (symbol-table index)
  (interning-table/resolve (symbol-table/interned-strings symbol-table) index))

(defun symbol-table/symbol-count (symbol-table)
  (interning-table/count (symbol-table/interned-symbols symbol-table)))

(defun symbol-table/intern-symbol (symbol-table symbol)
  ;; Ensure the string parts are interned.
  (symbol-table/intern-string symbol-table (package-name (symbol-package symbol)))
  (symbol-table/intern-string symbol-table (symbol-name symbol))
  (interning-table/intern (symbol-table/interned-symbols symbol-table) symbol))

(defun symbol-table/resolve-symbol (symbol-table index)
  (interning-table/resolve (symbol-table/interned-symbols symbol-table) index))

(defun symbol-table/did-count (symbol-table)
  (interning-table/count (symbol-table/interned-dids symbol-table)))

(defun symbol-table/intern-did (symbol-table did)
  ;; Ensure the did components are interned.
  (symbol-table/intern-string symbol-table (did/domain did))
  (symbol-table/intern-string symbol-table (did/repository did))
  (symbol-table/intern-symbol symbol-table (did/class did))

  (interning-table/intern (symbol-table/interned-dids symbol-table) did))

(defun symbol-table/resolve-did (symbol-table index)
  (interning-table/resolve (symbol-table/interned-dids symbol-table) index))

(defun symbol-table/note-committed (symbol-table)
  (interning-table/note-committed (symbol-table/interned-strings symbol-table))
  (interning-table/note-committed (symbol-table/interned-symbols symbol-table))
  (interning-table/note-committed (symbol-table/interned-dids    symbol-table)))

(defun symbol-table/sync-strings (symbol-table stream)
  (interning-table/write
   (symbol-table/interned-strings symbol-table)
   stream
   (lambda (string stream)
     (write-fixnum (string-length string) stream)
     (write-string-data string stream)
     (write-byte *ascii-linefeed* stream))))

(defun symbol-table/recover-strings (symbol-table stream)
  ;; Recover the interned strings
  (interning-table/recover
   (symbol-table/interned-strings symbol-table)
   stream
   (lambda (stream)
     (let* ((string-length (read-fixnum stream))
            (string (read-string-data string-length stream)))
       (read-byte stream) ;; discard the newline
       string))))

(defun symbol-table/sync-symbols (symbol-table stream)
  (interning-table/write
   (symbol-table/interned-symbols symbol-table)
   stream
   (lambda (symbol stream)
     (let ((name    (symbol-name symbol))
           (package (symbol-package symbol)))
        (if (null package)
            (error 'changesafe-database-error
                   "Interning an uninterned symbol ~s" symbol)
            (let ((package-name (package-name package)))
              (write-fixnum (symbol-table/intern-string symbol-table package-name) stream)
              (write-fixnum (symbol-table/intern-string symbol-table name) stream)))))))

(defun symbol-table/recover-symbols (symbol-table stream)
  ;; Recover the interned strings
  (interning-table/recover
   (symbol-table/interned-symbols symbol-table)
   stream
   (lambda (stream)
      (let* ((package-name (let ((stringid (read-fixnum stream)))
                             (symbol-table/resolve-string symbol-table stringid)))
             (symbol-name (let ((stringid (read-fixnum stream)))
                            (symbol-table/resolve-string symbol-table stringid)))
             (package (find-package package-name)))
        (if (null package)
            (error 'changesafe-database-recovery-error
                   "Package ~s is not in the lisp image." package-name)
            (intern symbol-name package))))))

(defun symbol-table/sync-dids (symbol-table stream)
  (interning-table/write
   (symbol-table/interned-dids symbol-table)
   stream
   (lambda (did stream)
     (let ((domain     (did/domain did))
           (repository (did/repository did))
           (class      (did/class did))
           (numeric-id (did/numeric-id did)))
       (write-fixnum (symbol-table/intern-string symbol-table domain) stream)
       (write-fixnum (symbol-table/intern-string symbol-table repository) stream)
       (write-fixnum (symbol-table/intern-symbol symbol-table class) stream)
       (write-fixnum numeric-id stream)))))

(defun symbol-table/recover-dids (symbol-table stream)
  ;; Recover the interned dids
  (interning-table/recover
   (symbol-table/interned-dids symbol-table)
   stream
   (lambda (stream)
     (let* ((domain (let ((stringid (read-fixnum stream)))
                      (symbol-table/resolve-string symbol-table stringid)))
            (repository (let ((stringid (read-fixnum stream)))
                          (symbol-table/resolve-string symbol-table stringid)))
            (class (let ((symid (read-fixnum stream)))
                     (symbol-table/resolve-symbol symbol-table symid)))
            (numeric-id (read-fixnum stream)))
       (make-distributed-identifier
        :domain     domain
        :repository repository
        :class      class
        :numeric-id numeric-id)))))

(defun symbol-table/sync (symbol-table stream)
  ;; Write out a table of symbols to stream.
  ;; Assume that the caller knows where this will end up.
  (debug-message 5 "Synchronizing symbol table.")
  ;; Write out the strings first.
  (symbol-table/sync-strings symbol-table stream)
  ;; Now the symbols
  (symbol-table/sync-symbols symbol-table stream)
  ;; Now the DIDs
  (symbol-table/sync-dids    symbol-table stream)
  )

(defun symbol-table/recover-segment (symbol-table stream)
  (symbol-table/recover-strings symbol-table stream)
  (symbol-table/recover-symbols symbol-table stream)
  (symbol-table/recover-dids    symbol-table stream))

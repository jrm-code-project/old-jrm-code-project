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
            collect-union
            decode-series
            scan-ahead
            16bit->8bit-le
            16bit->8bit-be
            )))

(defun collect-union (series &optional (key #'identity) (test #'eql))
  (declare (optimizable-series-function))
  (collect-fn 'list
              (lambda () '())
              (lambda (set item) (adjoin item set :key key :test test))
              series))

(defmacro scan-ahead (series bindings default &body body)
  "Binds variables in BINDINGS to the series, the series displaced by one,
   the series displaced by two, etc.  In effect, allows a fixed amount of
   lookahead for the body.  The value of default is used to `pad out' the
   tail of the series and will appear as the value of the last elements
   near the end.  Example:

   (scan-ahead #z(foo bar baz quux) (a b c) 'xyzzy  ....)

   will bind A to the series #z(foo bar baz quux)
             B to the series #z(bar baz quux xyzzy) and
             C to the series #z(baz quux xyzzy xyzzy)"
  (let ((n (length bindings)))
    `(MULTIPLE-VALUE-BIND ,bindings
         (CHUNK ,n 1 (CATENATE ,series (SUBSERIES (SERIES ,default) 1 ,n)))
      ,@body)))

(defmacro decode-series (series &key (padding nil) decoder (arity nil))
  "DECODER must be a literal lambda expression with only REQUIRED args.
   The decoder is invoked in turn on each overlapping n-tuple (where n
   is the number of args) in the series.  The decoder should return
   two values:  the decoded value and a `skip' count.  The decoded value
   is placed in the output followed by `skip' copies of PADDING.

   To pass a value straight through, return (VALUES <first arg> 0)

   Example:  Decode the %nn parts of a URI.

   (decode-series uri-characters
     :padding #\null
     :decoder (lambda (c0 c1 c2)     ;; lookahead by two characters
                (if (char/= c0 #\%)
                    (values c0 0)    ;; if not a %, pass it through
                    ;; Otherwise, decode the following characters,
                    ;; and return that value.  Use #\NULL for next two
                    ;; outputs.
                    (values (+ (* (digit-char-p c1 16) 16) (digit-char-p c2 16))
                             2))))
  "
  (destructure-function-lambda
   arity decoder
   (lambda (bindings docstring decls body)
     (declare (ignore docstring decls))
     (with-unique-names (output-code count)
       `(SCAN-AHEAD ,series ,bindings ,padding
          (MULTIPLE-VALUE-BIND (,output-code ,count)
            (COLLECTING-FN '(VALUES T FIXNUM)
                           (LAMBDA () (VALUES ,padding 0))
                           (LAMBDA (,output-code ,count ,@bindings)
                             (IF (> ,count 0)
                                 (VALUES ,padding (1- ,count))
                                 (PROGN ,@body)))
                           ,@bindings)
            ,output-code))))
   (lambda () (error "Literal lambda needed for decode-series."))))

(defun 16bit->8bit-le (16b-series)
  "Convert a series of 16bit-wide values to a series
   of 8bit-wide values, little endian."
  (declare (optimizable-series-function)
           (off-line-port 16b-series)
           (off-line-port 0))
  (producing (8b-out) ((16b-in 16b-series)
                       (current-16b 0)
                       (count 2))
    (declare (type (unsigned-byte 16) current-16b)
             (type (integer 0 2) count))

    (loop
      (tagbody
       (setq current-16b (next-in 16b-in (terminate-producing)))
       (setq count 2)

       :emit-bytes
       (if (zerop count) (go :next-loop))
       (decf count)
       (next-out 8b-out (cond ((= count 1) (ldb (byte 8 0) current-16b))
                              ((= count 0) (ldb (byte 8 8) current-16b))
                              (t (error "Too many bytes?"))))
       (go :emit-bytes)

       :next-loop))))

(defun 16bit->8bit-be (16b-series)
  "Convert a series of 16bit-wide values to a series
   of 8bit-wide values, big endian."
  (declare (optimizable-series-function)
           (off-line-port 16b-series)
           (off-line-port 0))
  (producing (8b-out) ((16b-in 16b-series)
                       (current-16b 0)
                       (count 2))
    (declare (type (unsigned-byte 16) current-16b)
             (type (integer 0 2) count))

    (loop
      (tagbody
       (setq current-16b (next-in 16b-in (terminate-producing)))
       (setq count 2)

       :emit-bytes
       (if (zerop count) (go :next-loop))
       (decf count)
       (next-out 8b-out (cond ((= count 1) (ldb (byte 8 8) current-16b))
                              ((= count 0) (ldb (byte 8 0) current-16b))
                              (t (error "Too many bytes?"))))
       (go :emit-bytes)

       :next-loop))))

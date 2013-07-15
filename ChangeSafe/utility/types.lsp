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

(export '(
          array-index
          array-index+
          ascii-code
          ascii-code+
          bmp-code
          char-16b
          char-8b
          character-code
          character-code+
          fixnump
          negative-fixnum
          negative-integer
          non-negative-fixnum
          non-negative-integer
          non-negative-quarter-fixnum
          non-positive-fixnum
          non-positive-integer
          optional
          positive-fixnum
          positive-integer
          restrict-type
          simple-string-16b
          simple-string-8b
          string-16b
          string-8b
          the-non-negative-fixnum
          ucs-2-code
          ucs-4-code
          uninitialized
          utf-32-code
          simple-vector-16b
          simple-vector-1b
          simple-vector-32b
          simple-vector-64b
          simple-vector-8b
          ))

(deftype array-index ()
  "An integer in the half-open range [0, array-dimension-limit),
   i.e. a potentially valid value for aref."
  `(INTEGER 0 (,array-dimension-limit)))
(deftype array-index+ ()
  "An integer in the closed range [0, array-dimension-limit]"
  `(INTEGER 0 ,array-dimension-limit))
(deftype ascii-code ()
  "An integer in the half-open [0, 128)"
  `(INTEGER 0 (128)))
(deftype ascii-code+ ()
  "An integer in the closed-range [0, 128]"
  `(INTEGER 0 128))
(deftype char-8b ()
  "A character that fits in an 8b lisp string."
  #+lispworks `BASE-CHAR)
(deftype char-16b ()
  "A `wide' character."
  #+lispworks `LISPWORKS:SIMPLE-CHAR)
(deftype character-code ()
  "An integer in the half-open range [0, char-code-limit),
   a valid argument to code-char or return value of char-code."
  `(INTEGER 0 (,char-code-limit)))
(deftype character-code+ ()
  "An integer in the closed range [0, char-code-limit]"
  `(INTEGER 0 ,char-code-limit))

(defun fixnump (thing)
  (typep thing 'fixnum))

(define-compiler-macro fixnump (thing)
  `(typep ,thing 'fixnum))

(deftype positive-integer () `(INTEGER (0) *))
(deftype negative-integer () `(INTEGER * (0)))
(deftype non-negative-integer () `(INTEGER 0 *))
(deftype non-positive-integer () `(INTEGER * 0))

(deftype positive-fixnum ()     `(INTEGER (0) ,most-positive-fixnum))
(deftype negative-fixnum ()     `(INTEGER ,most-negative-fixnum (0)))
(deftype non-negative-fixnum () `(INTEGER 0 ,most-positive-fixnum))
(deftype non-positive-fixnum () `(INTEGER ,most-negative-fixnum 0))

(deftype non-negative-quarter-fixnum () `(INTEGER 0 ,(ash most-positive-fixnum -2)))

(deftype optional      (type) `(OR NULL ,type))
(deftype uninitialized (type) `(OR NULL ,type))

(deftype ucs-2-code () `(integer 0 #xFFFF))
;;; Unicode/ISO 10646 Basic Multilingual Plane
(deftype bmp-code () `(integer 0 #xFFFD))
(deftype bmp-a-zone-code () `(integer #x00000000 #x00004DFF))
(deftype bmp-i-zone-code () `(integer #x00004E00 #x00009FFF))
(deftype bmp-o-zone-code () `(integer #x0000A000 #x0000D7FF))
(deftype bmp-s-zone-code () `(integer #x0000D800 #x0000DFFF))
(deftype bmp-r-zone-code () `(integer #x0000E000 #x0000FFFD))

(deftype ucs-4-code () `(integer 0 #x7FFFFFFF))

(deftype utf-32-code ()
  `(or (integer 0 #x0000D800)
       ;; surrogate area #x0000D800 - #x0000DFFF
       ;; characters are not defined for utf-16
       ;; compatability
       (integer #x0000E000 (#x0000FFFE))
       (integer #x00010000 (#x0001FFFE))
       (integer #x00020000 (#x0002FFFE))
       (integer #x00030000 (#x0003FFFE))
       (integer #x00040000 (#x0004FFFE))
       (integer #x00050000 (#x0005FFFE))
       (integer #x00060000 (#x0006FFFE))
       (integer #x00070000 (#x0007FFFE))
       (integer #x00080000 (#x0008FFFE))
       (integer #x00090000 (#x0009FFFE))
       (integer #x000A0000 (#x000AFFFE))
       (integer #x000B0000 (#x000CFFFE))
       (integer #x000C0000 (#x000BFFFE))
       (integer #x000D0000 (#x000DFFFE))
       (integer #x000E0000 (#x000EFFFE))
       (integer #x000F0000 (#x000FFFFE))
       (integer #x00100000 (#x0010FFFE))))

(deftype simple-vector-1b  (&optional size) `(simple-array (unsigned-byte 1)  (,size)))
(deftype simple-vector-8b  (&optional size) `(simple-array (unsigned-byte 8)  (,size)))
(deftype simple-vector-16b (&optional size) `(simple-array (unsigned-byte 16) (,size)))
(deftype simple-vector-32b (&optional size) `(simple-array (unsigned-byte 32) (,size)))
(deftype simple-vector-64b (&optional size) `(simple-array (unsigned-byte 64) (,size)))

(deftype string-8b (&optional size)         `(array char-8b (,size)))
(deftype string-16b (&optional size)        `(array char-16b (,size)))
(deftype simple-string-8b (&optional size)  `(simple-array char-8b (,size)))
(deftype simple-string-16b (&optional size) `(simple-array char-16b (,size)))

(defun restrict-type (thing type env)
  ;; expands to `(THE ,type ,thing) or something
  ;; more restrictive.
  (cond ((constantp thing env)
         (assert (typep thing type))
         thing)
        ((and (consp thing)
              (eq (car thing) 'the))
         (let ((declared-type (cadr thing))
               (value (caddr thing)))
           (multiple-value-bind (subtype-p valid-p)
               (cl:subtypep type declared-type #-lispworks env)
             (cond ((null valid-p) ;; dunno
                    `(THE ,type ,thing))
                   ((null subtype-p) ;; we are less restrictive.
                    thing)
                   (t ;; we are more restrictive
                    `(THE ,type ,value))))))
        (t `(THE ,type ,thing))))

(defun the-array-index (thing env)
  (restrict-type thing 'array-index env))

(defun the-fixnum (thing env)
  (restrict-type thing 'fixnum env))

(defun the-non-negative-fixnum (thing env)
  (restrict-type thing 'non-negative-fixnum env))

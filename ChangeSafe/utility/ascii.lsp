;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002 - 2004 ChangeSafe, LLC
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;;          ChangeSafe, LLC CONFIDENTIAL and PROPRIETARY material.
;;;;
;;;;          Content Integrity, Inc
;;;;          Braintree Executive Office Park
;;;;          P.O. Box 850942
;;;;          Braintree, MA 02185-0942
;;;;
;;;; This software and information comprise valuable intellectual property
;;;; and trade secrets of ChangeSafe, LLC, developed at substantial
;;;; expense by ChangeSafe, LLC, which ChangeSafe, LLC intends to
;;;; preserve as trade secrets.  This software is furnished pursuant to a
;;;; written license agreement and may be used, copied, transmitted, and
;;;; stored only in accordance with the terms of such license and with the
;;;; inclusion of the above copyright notice.  This software and
;;;; information or any other copies thereof may not be provided or
;;;; otherwise made available to any other person.  NO title to or
;;;; ownership of this software and information is hereby transferred.
;;;; ChangeSafe, LLC assumes no responsibility for the use or reliability
;;;; of this software.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; File Name:     ascii.lsp
;;;; Author:        Joe Marshall
;;;; Creation Date: 07 May 2002
;;;;
;;;; Module Description:
;;;;
;;;; Ascii <-> Lisp encoding.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CSF/UTILITY")

(eval-when (:load-toplevel :execute)
  (export '(
            *ascii-carriage-return*
            *ascii-code-set/alpha*
            *ascii-code-set/alphanumeric*
            *ascii-code-set/decimal-digit*
            *ascii-code-set/hex-digit*
            *ascii-linefeed*
            *ascii-zero*
            ascii-code->lisp-char
            ascii-code-set
            ascii-code-set/member
            ascii-code-set/union
            ascii-line-reader
            ascii-line-writer
            ascii-vector->lisp-string
            lisp-char->ascii-code
            lisp-characters->ascii-code-set
            lisp-string->ascii-vector
            predicate->ascii-code-set
            )))

(proclaim (standard-optimizations))

(defconstant *ascii-linefeed*        10 "The integer that represents the ascii LF character.")
(defconstant *ascii-carriage-return* 13 "The integer that represents the ascii CR character.")
(defconstant *ascii-zero*            48 "The integer that represents the ascii digit 0.")

;;; By defining the mapping in this way, we build in no assumptions as
;;; to the underlying character representation in the Lisp.  The Lisp
;;; could be ASCII or EBCDIC or anything, but this will map ASCII
;;; chars into lisp standard characters.

(defconstant *character-map/ascii->lisp*
  #.(let ((map (make-array 128
                           :element-type 'character-code
                           :initial-element (1- char-code-limit))))
      (iterate ((lisp-code (map-fn 'character-code
                                   #'char-code
                                   (catenate (scan 'string " !\"#$%&'()*+,-./")
                                             (scan 'string "0123456789:;<=>?")
                                             (scan 'string "@ABCDEFGHIJKLMNO")
                                             (scan 'string "PQRSTUVWXYZ[\\]^_")
                                             (scan 'string "'abcdefghijklmno")
                                             (scan 'string "pqrstuvwxyz{|}~"))))
                (ascii-code (scan-range :from 32)))
               (setf (aref map ascii-code) lisp-code))
      (flet ((map-char (name ascii-code)
               (let ((probe (name-char name)))
                 (when probe
                   (setf (aref map ascii-code) (char-code probe))))))
        ;; Semi-standard Lisp characters
        (map-char "Backspace"  8)
        (map-char "Tab"        9)
        (map-char "Linefeed"  10)
        (map-char "Page"      12)
        (map-char "Return"    13)
        (map-char "Space"     32)
        (map-char "Rubout"   127)
        ;; Non-standard characters.  Maybe they exist.
        (map-char "Null"       0) (map-char "NUL" 0)
        (map-char "SOH"        1)
        (map-char "STX"        2)
        (map-char "ETX"        3)
        (map-char "EOT"        4)
        (map-char "ENQ"        5)
        (map-char "ACK"        6)
        (map-char "Bell"       7) (map-char "BEL" 7)
        (map-char "VT"        #x0B)
        (map-char "SO"        #x0E)
        (map-char "SI"        #x0F)
        (map-char "DLE"       #x10)
        (map-char "DC1"       #x11)
        (map-char "DC2"       #x12)
        (map-char "DC3"       #x13)
        (map-char "DC4"       #x14)
        (map-char "NAK"       #x15)
        (map-char "SYN"       #x16)
        (map-char "ETB"       #x17)
        (map-char "CAN"       #x18)
        (map-char "EM"        #x19)
        (map-char "SUB"       #x1A)
        (map-char "ESC"       #x1B)
        (map-char "FS"        #x1C)
        (map-char "GS"        #x1D)
        (map-char "RS"        #x1E)
        (map-char "US"        #x1F)
        map))

  "A vector of 127 elements that maps ascii codes into the
   standard and semi-standard character codes used by the
   Lisp implementation.  ASCII codes that do not have a
   standard corresponding Lisp character will be mapped to
   a (char-code-limit - 1) value!")

(declaim (type (vector character-code 128) *character-map/ascii->lisp*))

;;; Backwards conversion is harder because there is no guarantee
;;; that the Lisp chars form a compact set.  (or even a small set!)

(defvar *character-map/lisp->ascii*)

(eval-when (:load-toplevel :execute)
  (setq *character-map/lisp->ascii*
        (collect-hash (scan '(simple-array character-code (128))
                            *character-map/ascii->lisp*)
                      (scan-range :from 0 :below 128)
                      :size 128)))

;;; High performance inline accessors.
(declaim (ftype (function (ascii-code) character-code) %ascii-code->lisp-code)
         (inline %ascii-code->lisp-code)
         (ftype (function (character-code) ascii-code) %lisp-code->ascii-code)
         (inline %lisp-code->ascii-code))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %ascii-code->lisp-code (ascii)
  (declare #.(performance-optimizations)
           (type ascii-code ascii))
  (elt (the (vector character-code 128) *character-map/ascii->lisp*) ascii))
)

(defun %lisp-code->ascii-code (lisp-code)
  (declare #.(performance-optimizations)
           (type character-code lisp-code))
  (gethash lisp-code *character-map/lisp->ascii* 0)) ;; default to NULs for non-mappable

(defun ascii-code->lisp-code (ascii)
  (check-type ascii ascii-code)
  (%ascii-code->lisp-code ascii))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun ascii-code->lisp-char (ascii)
  (check-type ascii ascii-code)
  (code-char (%ascii-code->lisp-code ascii)))
)

(defun lisp-code->ascii-code (lisp-code)
  (check-type lisp-code character-code)
  (%lisp-code->ascii-code lisp-code))

(defun lisp-char->ascii-code (lisp-char)
  "Return the ascii code corresponding with the standard lisp character."
  (check-type lisp-char standard-char)
  (%lisp-code->ascii-code (char-code lisp-char)))

(defun lisp-string->ascii-vector (string)
  "Return an array of bytes that represents a string of lisp characters
   encoded into 7-bit ASCII."
  (check-type string string)
  (let ((result (make-array (length string) :element-type 'ascii-code)))
    (map-into result #'lisp-char->ascii-code string)
    result))

(defun ascii-vector->lisp-string (vector)
  "Return a string decoded from a vector of ascii codes."
  (check-type vector (vector ascii-code))
  (let ((result (make-string (length vector))))
    (map-into result #'ascii-code->lisp-char vector)
    result))

;;; An ascii code set is represented by a vector of 128 bits.
(deftype ascii-code-set () `(simple-bit-vector 128))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun predicate->ascii-code-set (lisp-character-predicate)
  (let ((set (make-array 128 :element-type 'bit)))
    (dotimes (i 128 set)
      (setf (sbit set i)
            (if (funcall lisp-character-predicate (ascii-code->lisp-char i))
                1
                0)))))
)

(defun lisp-characters->ascii-code-set (character-list)
  (let ((set (make-array 128 :element-type 'bit :initial-element 0)))
    (dolist (c character-list set)
      (setf (sbit set (lisp-char->ascii-code c)) 1))))

(defun ascii-code-set/union (&rest sets)
  (reduce #'bit-ior sets))

(defun ascii-code-set/complement (set)
  (bit-not set))

(defun ascii-code-set/difference (left right)
  (bit-andc2 left right))

(defconstant *ascii-code-set/alpha*
  (load-time-value (predicate->ascii-code-set #'alpha-char-p)))

(defconstant *ascii-code-set/alphanumeric*
  (load-time-value (predicate->ascii-code-set #'alphanumericp)))

(defconstant *ascii-code-set/decimal-digit*
  (load-time-value
   (predicate->ascii-code-set
    (lambda (char) (digit-char-p char 10.)))))

(defconstant *ascii-code-set/hex-digit*
  (load-time-value
   (predicate->ascii-code-set
    (lambda (char) (digit-char-p char 16.)))))

(defun ascii-code-set/member (ascii-code ascii-code-set)
  "Return T iff the ascii-code is a member of the ascii-code-set."
  (check-type ascii-code ascii-code)
  (check-type ascii-code-set ascii-code-set)
  (= (sbit ascii-code-set ascii-code) 1))

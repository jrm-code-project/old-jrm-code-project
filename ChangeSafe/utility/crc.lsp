;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 1999 - 2004 ChangeSafe, LLC
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
;;;; File Name:     crc.lsp
;;;; Author:        Joe Marshall
;;;; Creation Date: August 1999
;;;;
;;;; Module Description:
;;;;
;;;; Calculate 24-bit CRC's as fast as possible.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CSF/UTILITY")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(
            *crc-seed*
            byte-crc
            byte-array-crc
            char-crc
            string-crc
            collect-crc
            )))

(proclaim (standard-optimizations))

(declaim (type (unsigned-byte 24) *crc-seed*))
(defconstant *crc-seed* #x00B704CE "Initial value to use for CRC.")

;;; The generator polynomial is #x864cfb.

(declaim (type (simple-array t (256)) *crc-table*))
(defconstant *crc-table*
    (if (boundp '*crc-table*)
        (symbol-value '*crc-table*)
      (make-array 256
                  :element-type t
                  :initial-contents
                  #(
                    #x000000 #x864cfb #x8ad50d #x0c99f6 #x93e6e1 #x15aa1a #x1933ec #x9f7f17
                    #xa18139 #x27cdc2 #x2b5434 #xad18cf #x3267d8 #xb42b23 #xb8b2d5 #x3efe2e
                    #xc54e89 #x430272 #x4f9b84 #xc9d77f #x56a868 #xd0e493 #xdc7d65 #x5a319e
                    #x64cfb0 #xe2834b #xee1abd #x685646 #xf72951 #x7165aa #x7dfc5c #xfbb0a7
                    #x0cd1e9 #x8a9d12 #x8604e4 #x00481f #x9f3708 #x197bf3 #x15e205 #x93aefe
                    #xad50d0 #x2b1c2b #x2785dd #xa1c926 #x3eb631 #xb8faca #xb4633c #x322fc7
                    #xc99f60 #x4fd39b #x434a6d #xc50696 #x5a7981 #xdc357a #xd0ac8c #x56e077
                    #x681e59 #xee52a2 #xe2cb54 #x6487af #xfbf8b8 #x7db443 #x712db5 #xf7614e
                    #x19a3d2 #x9fef29 #x9376df #x153a24 #x8a4533 #x0c09c8 #x00903e #x86dcc5
                    #xb822eb #x3e6e10 #x32f7e6 #xb4bb1d #x2bc40a #xad88f1 #xa11107 #x275dfc
                    #xdced5b #x5aa1a0 #x563856 #xd074ad #x4f0bba #xc94741 #xc5deb7 #x43924c
                    #x7d6c62 #xfb2099 #xf7b96f #x71f594 #xee8a83 #x68c678 #x645f8e #xe21375
                    #x15723b #x933ec0 #x9fa736 #x19ebcd #x8694da #x00d821 #x0c41d7 #x8a0d2c
                    #xb4f302 #x32bff9 #x3e260f #xb86af4 #x2715e3 #xa15918 #xadc0ee #x2b8c15
                    #xd03cb2 #x567049 #x5ae9bf #xdca544 #x43da53 #xc596a8 #xc90f5e #x4f43a5
                    #x71bd8b #xf7f170 #xfb6886 #x7d247d #xe25b6a #x641791 #x688e67 #xeec29c
                    #x3347a4 #xb50b5f #xb992a9 #x3fde52 #xa0a145 #x26edbe #x2a7448 #xac38b3
                    #x92c69d #x148a66 #x181390 #x9e5f6b #x01207c #x876c87 #x8bf571 #x0db98a
                    #xf6092d #x7045d6 #x7cdc20 #xfa90db #x65efcc #xe3a337 #xef3ac1 #x69763a
                    #x578814 #xd1c4ef #xdd5d19 #x5b11e2 #xc46ef5 #x42220e #x4ebbf8 #xc8f703
                    #x3f964d #xb9dab6 #xb54340 #x330fbb #xac70ac #x2a3c57 #x26a5a1 #xa0e95a
                    #x9e1774 #x185b8f #x14c279 #x928e82 #x0df195 #x8bbd6e #x872498 #x016863
                    #xfad8c4 #x7c943f #x700dc9 #xf64132 #x693e25 #xef72de #xe3eb28 #x65a7d3
                    #x5b59fd #xdd1506 #xd18cf0 #x57c00b #xc8bf1c #x4ef3e7 #x426a11 #xc426ea
                    #x2ae476 #xaca88d #xa0317b #x267d80 #xb90297 #x3f4e6c #x33d79a #xb59b61
                    #x8b654f #x0d29b4 #x01b042 #x87fcb9 #x1883ae #x9ecf55 #x9256a3 #x141a58
                    #xefaaff #x69e604 #x657ff2 #xe33309 #x7c4c1e #xfa00e5 #xf69913 #x70d5e8
                    #x4e2bc6 #xc8673d #xc4fecb #x42b230 #xddcd27 #x5b81dc #x57182a #xd154d1
                    #x26359f #xa07964 #xace092 #x2aac69 #xb5d37e #x339f85 #x3f0673 #xb94a88
                    #x87b4a6 #x01f85d #x0d61ab #x8b2d50 #x145247 #x921ebc #x9e874a #x18cbb1
                    #xe37b16 #x6537ed #x69ae1b #xefe2e0 #x709df7 #xf6d10c #xfa48fa #x7c0401
                    #x42fa2f #xc4b6d4 #xc82f22 #x4e63d9 #xd11cce #x575035 #x5bc9c3 #xdd8538
                    )))
  "Precomputed CRC table.")

;(defmacro crc-step (previous-crc byte)
;  `(THE (UNSIGNED-BYTE 24)
;     (LOGXOR
;      (THE (UNSIGNED-BYTE 24)
;       (ASH (THE (UNSIGNED-BYTE 16) (LOGAND ,previous-crc #x0000FFFF)) 8))
;      (THE (UNSIGNED-BYTE 24)
;       (SVREF *CRC-TABLE*
;              (THE ARRAY-INDEX
;                (LOGXOR (THE (UNSIGNED-BYTE 8) ,byte)
;                        ;; Note, we could (logand ,previous-crc #xFF), but
;                        ;; we shouldn't need to if the ,previous-crc fits in
;                        ;; 24 bits.
;                        (THE (UNSIGNED-BYTE 8) (ASH ,previous-crc -16)))))))))

(defun crc-step (previous-crc byte)
  (LOGXOR

    (ASH (LOGAND previous-crc #x00007FFF) 8)

    (SVREF *CRC-TABLE*
           (LOGXOR  byte
                    ;; Note, we could (logand ,previous-crc #xFF), but
                    ;; we shouldn't need to if the ,previous-crc fits in
                    ;; 24 bits.
                    (logand #xFF (ASH previous-crc -16))))))

(declaim (ftype (function ((unsigned-byte 24) (unsigned-byte 24)) (unsigned-byte 24)) byte-crc))
(defun byte-crc (crc byte)
   "Return a new CRC based upon making a CRC-STEP on CRC and BYTE."
  (declare (type (unsigned-byte 24) crc byte)
           #.(performance-optimizations)
           #+allegro (:explain :calls))
  (crc-step crc byte))

(declaim (ftype (function ((unsigned-byte 24) character) (unsigned-byte 24)) byte-crc))
(defun char-crc (crc char)
   "Return a new CRC based upon making a CRC-STEP on CRC and (char-code CHAR)."
  (declare (type (unsigned-byte 24) crc)
           (type character char)
           #.(performance-optimizations)
           #+allegro (:explain :calls))
  (crc-step crc (char-code char)))

;;; Extremely bummed.
(declaim (ftype (function (array-index
                           (simple-array (unsigned-byte 8) (*))
                           array-index
                           (unsigned-byte 24)
                           ) (unsigned-byte 24)) %byte-array-crc))
(defun %byte-array-crc (end byte-array start crc)
  "Unsafe, high-performance crc update.  Assumes that
   byte-array is (simple-array (unsigned-byte 8) (*)), and that the
   limits are correct."
  (declare (type (unsigned-byte 24) crc)
           (type (simple-array (unsigned-byte 8) (*)) byte-array)
           (type array-index start end)
           #.(performance-optimizations)
           #+allegro (:explain :calls))
  (do ()
      ((= start end) crc)
    (setq crc (crc-step crc (aref byte-array start)))
    (incf start)))

(declaim (ftype (function (array-index
                           simple-string-8b
                           array-index
                           (unsigned-byte 24)
                           ) (unsigned-byte 24)) %simple-string-crc))
(defun %simple-string-crc (end simple-string start crc)
  "Unsafe, high-performance crc update.  Assumes that
   simple-string is a simple-string, and that the
   limits are correct."
  (declare (type (unsigned-byte 24) crc)
           (type simple-string-8b simple-string)
           (type array-index start end)
           #.(performance-optimizations)
           #+allegro (:explain :calls))
  (do ()
      ((= start end) crc)
    (setq crc (crc-step crc (logand #xFF (char-code (schar simple-string start)))))
    (incf start)))

(defun byte-array-crc (crc byte-array &key (start 0) end)
  "Return a new CRC based upon calling CRC-STEP on BYTE-ARRAY between indicies
   START and END."
  (check-type crc (unsigned-byte 24))
  (check-type byte-array simple-vector-8b)
  (let ((limit (or end (vector-length byte-array))))
    (assert (and (<= 0 start)
                 (<= start limit)
                 (<= limit (vector-length byte-array))))
    (%byte-array-crc limit byte-array start crc)))

(defun string-crc (crc string &key (start 0) end)
  "Return a new CRC based upon calling CRC-STEP on STRING between indicies
   START and END."
  (check-type crc (unsigned-byte 24))
  (check-type string simple-string-8b)
  (let ((limit  (or end (string-length string))))
    (assert (and (<= 0 start)
                 (<= start limit)
                 (<= limit (string-length string))))
    (%simple-string-crc limit string start crc)))

(defun collect-crc (string-series)
  "Collect the CRC of a series of simple-string."
  (declare (optimizable-series-function))
  (collect-fn '(unsigned-byte 24)
              (lambda () *crc-seed*)
              (lambda (crc next-input)
                (string-crc (char-crc crc #\newline) next-input))
              string-series))

;(defun test-byte-array-crc ()
;  (let ((foo (make-array 4000 :element-type '(unsigned-byte 8))))
;    (fill foo 42)
;    (time (loop repeat 10000 do (byte-array-crc *crc-seed* foo)))))

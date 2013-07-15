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

(eval-when (:compile-toplevel :load-toplevel :execute)
(export '(hash-string
          hash-simple-string
          hash-byte-vector
          hash-vector-16b)))

(proclaim (standard-optimizations))

;;; Algorithm from
;;; Performance in Practice of String Hashing Functions
;;; M.V. Ramakrishna, Justin Zobel

(eval-when (:compile-toplevel :load-toplevel :execute)

(defconstant *hash-seed-0* #xF0E1D2C3)
;; (defconstant *hash-seed-1* #xBA59687)

(defun hash-step-expansion (env accumulator increment rshift lshift)
  (check-type rshift (integer 2 3))
  (check-type lshift (integer 3 7))
  ;; This labels statement builds the code for the expansion below.
  (labels ((%logxor (left right)
             `(THE NON-NEGATIVE-FIXNUM
                (LOGXOR ,(the-non-negative-fixnum left env)
                        ,(the-non-negative-fixnum right env))))

           (%sum (add1 add2 add3)
             (the-non-negative-fixnum
              `(+ ,add1
                  ,add2
                  ,add3)
              env))

           (%lshift (num shiftamount)
             ;; mask first then shift
             (let ((lomask (ash most-positive-fixnum (- (+ shiftamount 2)))))
               `(THE (INTEGER 0 ,(ash lomask shiftamount))
                  (ASH (THE (INTEGER 0 ,lomask) (LOGAND ,num ,lomask))
                       ,shiftamount))))

           (%rshift (num shiftamount)
             `(THE (INTEGER 0 ,(ash most-positive-fixnum (- shiftamount)))
                (ASH ,num ,(- shiftamount))))

           (%mask-to-quarter-fixnum (thing)
             (let ((quarter-fixnum-mask (ash most-positive-fixnum -2)))
               `(THE (INTEGER 0 ,quarter-fixnum-mask)
                  (LOGAND ,quarter-fixnum-mask ,thing)))))

    (let ((%accumulator (the-non-negative-fixnum accumulator env))
          (%increment   (if (constantp increment env)
			    (logand increment (ash most-positive-fixnum -2))
                            (%mask-to-quarter-fixnum
                             `(THE NON-NEGATIVE-FIXNUM ,increment)))))

      (%logxor
       (%sum (%lshift %accumulator lshift)
             (%rshift %accumulator rshift)
             %increment)
       %accumulator))))

)
(defmacro hash-step-0 (&environment env accumulator number)
  (hash-step-expansion env accumulator number 2 5))

;; (defmacro hash-step-1 (accumulator number)
;;  (hash-step-expansion accumulator number 1 6))

(defmacro define-hash-loop (root-name sequence-type access)
  (let ((inner-name (intern (concatenate 'string "%" (symbol-name root-name))
                            (symbol-package root-name))))
    `(PROGN
       (DEFUN ,inner-name (SEQUENCE START LIMIT)
         (DECLARE ,(performance-optimizations)
                  (TYPE ARRAY-INDEX START LIMIT)
                  (TYPE ,sequence-type SEQUENCE))
         (DO* ((INDEX (- START 1) (THE ARRAY-INDEX (1+ INDEX)))
               (ELEMENT 0 (,access SEQUENCE (THE ARRAY-INDEX INDEX)))
               (HASH ,(logand *HASH-SEED-0* most-positive-fixnum) (HASH-STEP-0 HASH ELEMENT)))
             ((= INDEX LIMIT)
              ;; Post-hash mixing so that last few elements
              ;; get spread around.
              ;(SETQ HASH (HASH-STEP-0 HASH #x800001))
              ;(SETQ HASH (HASH-STEP-0 HASH #x080010))
              ;(SETQ HASH (HASH-STEP-0 HASH #x008100))
              ;(SETQ HASH (HASH-STEP-0 HASH #x001800))
              ;(SETQ HASH (HASH-STEP-0 HASH #x010080))
              ;(SETQ HASH (HASH-STEP-0 HASH #x100008))
              HASH)
           (DECLARE (TYPE (INTEGER -1 ,array-dimension-limit) INDEX)
                    (TYPE NON-NEGATIVE-FIXNUM HASH))))

       ;; And the safe version for the user.
       (DEFUN ,root-name (SEQUENCE &KEY (START 0) END)
         (CHECK-TYPE SEQUENCE ,sequence-type)
         (CHECK-TYPE START ARRAY-INDEX)
         (CHECK-TYPE END (OPTIONAL ARRAY-INDEX))
         (LET ((LEN (LENGTH (THE ,sequence-type SEQUENCE))))
           (IF (NULL END)
               (SETQ END LEN)
               (ASSERT (<= END LEN))))
         (ASSERT (<= 0 START END))
         (,inner-name SEQUENCE START (- END 1))))))

(define-hash-loop hash-string string (lambda (str i) (char-code (char str i))))
(define-hash-loop hash-simple-string simple-string (lambda (str i) (char-code (schar str i))))
(define-hash-loop hash-byte-vector (vector (unsigned-byte 8) *) elt)
(define-hash-loop hash-vector-16b (vector (unsigned-byte 16) *) elt)

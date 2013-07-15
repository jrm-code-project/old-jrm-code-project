;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002 - 2005 ChangeSafe, LLC
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

(in-package "FIXNUM-MATH")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(= /= < > <= >=
            MAX MIN
            MINUSP PLUSP
            FLOOR CEILING
            * + - / 1+ 1-
            ABS EVENP ODDP
            GCD INCF DECF
            LCM ASH
            LOGAND LOGANDC1 LOGANDC2
            LOGEQV LOGIOR LOGNAND
            LOGNOR LOGNOT LOGORC1 LOGORC2
            LOGXOR LOGBITP LOGCOUNT LOGTEST
            DEPOSIT-FIELD DPB LDB LDB-TEST
            MASK-FIELD)))

(proclaim (standard-optimizations))

;;; The FIX* provide for optimized fixnum manipulation, where the inputs AND the outputs
;;; are expected to be fixnums.  They should normally be used in code compiled for performance.

(defun the-fixnum (thing env)
  (csf/utility:restrict-type thing 'fixnum env))

(defun expand-fixnum-form (env operator operand1 operand2 operand-list)
  "Perform an n-ary expansion of a binary fixnum operator.
   (fix:+ a b c) =>
       (the fixnum
         (+ (the fixnum
              (+ (the fixnum a)
                 (the fixnum b)))
            (the fixnum c)))"
  (do ((forms operand-list (cdr forms))
       (result (the-fixnum
                `(,operator ,(the-fixnum operand1 env)
                            ,(the-fixnum operand2 env))
                env)
               (the-fixnum
                `(,operator ,result
                            ,(the-fixnum (car forms) env))
                env)))
      ((null forms) result)))

(defmacro + (&environment env i1 i2 &rest fixnum-forms)
  "Add two fixnums that are expected to return a fixnum result.
   The goal is to have the compiler efficiently inline this expression when suitable
   optimization settings are in effect."
  (expand-fixnum-form env 'cl:+ i1 i2 fixnum-forms))

(defmacro * (&environment env i1 i2 &rest fixnum-forms)
  "Multiply two fixnums that are expected to return a fixnum result.
   The goal is to have the compiler efficiently inline this expression when suitable
   optimization settings are in effect."
  (expand-fixnum-form env 'cl:* i1 i2 fixnum-forms))

(defmacro - (&environment env i1 i2 &rest fixnum-forms)
  "Subtract two fixnums that are expected to return a fixnum result.
   The goal is to have the compiler efficiently inline this expression when suitable
   optimization settings are in effect."
  (expand-fixnum-form env 'cl:- i1 i2 fixnum-forms))

(defmacro < (&environment env l r)
  `(CL:< ,(the-fixnum l env) ,(the-fixnum r env)))

(defmacro > (&environment env l r)
  `(CL:> ,(the-fixnum l env) ,(the-fixnum r env)))

(defmacro <= (&environment env l r)
  `(CL:<= ,(the-fixnum l env) ,(the-fixnum r env)))

(defmacro >= (&environment env l r)
  `(CL:>= ,(the-fixnum l env) ,(the-fixnum r env)))

(defmacro = (l r)
  `(CL:EQ ,l ,r))

(defmacro 1+ (&environment env arg)
  `(CL:1+ ,(the-fixnum arg env)))

(defmacro 1- (&environment env arg)
  `(CL:1- ,(the-fixnum arg env)))

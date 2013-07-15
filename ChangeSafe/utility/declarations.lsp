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


;;; Declarations macros.
;;; This file loads first.
;;;
;;; Every sourcefile should have
;;;
;;; (in-package "...")
;;;
;;; (proclaim (standard-optimizations))
;;;
;;; at the top.

(in-package "CSF/CONFIG")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(
            *debug-compilation-speed-declaration*
            *debug-debug-declaration*
            *debug-fixnum-safety-declaration*
            *debug-float-declaration*
            *debug-interruptable-declaration*
            *debug-safety-declaration*
            *debug-space-declaration*
            *debug-speed-declaration*

            *performance-compilation-speed-declaration*
            *performance-debug-declaration*
            *performance-fixnum-safety-declaration*
            *performance-float-declaration*
            *performance-interruptable-declaration*
            *performance-safety-declaration*
            *performance-space-declaration*
            *performance-speed-declaration*

            *standard-compilation-speed-declaration*
            *standard-debug-declaration*
            *standard-fixnum-safety-declaration*
            *standard-float-declaration*
            *standard-interruptable-declaration*
            *standard-safety-declaration*
            *standard-space-declaration*
            *standard-speed-declaration*

            debug-optimizations
            standard-optimizations
            performance-optimizations
            fixnum-performance-optimizations))

;;; These are defined the way they are so that they behave
;;; somewhat like DEFVARs.  That way, you can put code in
;;; your init file to override the standard settings.

(defparameter *debug-compilation-speed-declaration*
  (if (boundp '*debug-compilation-speed-declaration*)
      *debug-compilation-speed-declaration*
      0)
  "Controls speed of compiler when compiling debuggable code.")

(defparameter *debug-debug-declaration*
  (if (boundp '*debug-debug-declaration*)
      *debug-debug-declaration*
      3)
  "Controls generation of debug information when compiling debuggable code.")

(defparameter *debug-fixnum-safety-declaration*
  (if (boundp '*debug-fixnum-safety-declaration*)
      *debug-fixnum-safety-declaration*
      3)
  "Controls integer range assumptions when compiling debuggable code.")

(defparameter *debug-float-declaration*
  (if (boundp '*debug-float-declaration*)
      *debug-float-declaration*
      3)
  "Controls floating point assumptions when compiling debuggable code.")

(defparameter *debug-interruptable-declaration*
  (if (boundp '*debug-interruptable-declaration*)
      *debug-interruptable-declaration*
      3)
  "Controls interrupt polling when compiling debuggable code.")

(defparameter *debug-safety-declaration*
  (if (boundp '*debug-safety-declaration*)
      *debug-safety-declaration*
      3)
  "Controls type checking when compiling debuggable code.")

(defparameter *debug-space-declaration*
  (if (boundp '*debug-space-declaration*)
      *debug-space-declaration*
      0)
  "Controls size of image when compiling debuggable code.")

(defparameter *debug-speed-declaration*
  (if (boundp '*debug-speed-declaration*)
      *debug-speed-declaration*
      0)
  "Controls performance when compiling debuggable code.")

(defparameter *performance-compilation-speed-declaration*
  (if (boundp '*performance-compilation-speed-declaration*)
      *performance-compilation-speed-declaration*
      0)
  "Controls speed of compiler when compiling high-performance code.")

(defparameter *performance-debug-declaration*
  (if (boundp '*performance-debug-declaration*)
      *performance-debug-declaration*
      0)
  "Controls generation of debug information when compiling high-performance code.")

(defparameter *performance-fixnum-safety-declaration*
  (if (boundp '*performance-fixnum-safety-declaration*)
      *performance-fixnum-safety-declaration*
      0)
  "Controls integer range assumptions when compiling high-performance code.")

(defparameter *performance-float-declaration*
  (if (boundp '*performance-float-declaration*)
      *performance-float-declaration*
      0)
  "Controls floating point assumptions when compiling high-performance code.")

(defparameter *performance-interruptable-declaration*
  (if (boundp '*performance-interruptable-declaration*)
      *performance-interruptable-declaration*
      3)
  "Controls interrupt polling when compiling high-performance code.")

(defparameter *performance-safety-declaration*
  (if (boundp '*performance-safety-declaration*)
      *performance-safety-declaration*
      0)
  "Controls type checking when compiling high-performance code.")

(defparameter *performance-space-declaration*
  (if (boundp '*performance-space-declaration*)
      *performance-space-declaration*
      0)
  "Controls size of image when compiling high-performance code.")

(defparameter *performance-speed-declaration*
  (if (boundp '*performance-speed-declaration*)
      *performance-speed-declaration*
      3)
  "Controls performance when compiling high-performance code.")

(defparameter *standard-compilation-speed-declaration*
  (if (boundp '*standard-compilation-speed-declaration*)
      *standard-compilation-speed-declaration*
      0)
  "Controls speed of compiler when compiling released code.")

(defparameter *standard-debug-declaration*
  (if (boundp '*standard-debug-declaration*)
      *standard-debug-declaration*
      2)
  "Controls generation of debug information when compiling released code.")

(defparameter *standard-fixnum-safety-declaration*
  (if (boundp '*standard-fixnum-safety-declaration*)
      *standard-fixnum-safety-declaration*
      3)
  ;; 3 means check fixnums, 0 means don't check
  "Controls integer range assumptions when compiling released code.")

(defparameter *standard-float-declaration*
  (if (boundp '*standard-float-declaration*)
      *standard-float-declaration*
      3)
  "Controls floating point assumptions when compiling released code.")

(defparameter *standard-interruptable-declaration*
  (if (boundp '*standard-interruptable-declaration*)
      *standard-interruptable-declaration*
      3)
  ;; 3 means poll, 0 means don't poll
  "Controls interrupt polling when compiling released code.")

(defparameter *standard-safety-declaration*
  (if (boundp '*standard-safety-declaration*)
      *standard-safety-declaration*
      2)
  "Controls type checking when compiling released code.")

(defparameter *standard-space-declaration*
  (if (boundp '*standard-space-declaration*)
      *standard-space-declaration*
      0)
  "Controls size of image when compiling released code.")

(defparameter *standard-speed-declaration*
  (if (boundp '*standard-speed-declaration*)
      *standard-speed-declaration*
      3)
  "Controls performance when compiling released code.")
)

(defun debug-optimizations ()
  "Declarations for maximum debugability."
  `(OPTIMIZE
    (COMPILATION-SPEED ,*debug-compilation-speed-declaration*)
    (DEBUG             ,*debug-debug-declaration*)
    (SAFETY            ,*debug-safety-declaration*)
    (SPACE             ,*debug-space-declaration*)
    (SPEED             ,*debug-speed-declaration*)
    #+lispworks (HCL:FIXNUM-SAFETY ,*debug-fixnum-safety-declaration*)
    #+lispworks (FLOAT             ,*debug-float-declaration*)
    #+lispworks (SYS:INTERRUPTABLE ,*debug-interruptable-declaration*)))

(defun standard-optimizations ()
  "Declarations for production code.
   Place (PROCLAIM (STANDARD-OPTIMIZATIONS))
   at the top of every file."
  `(OPTIMIZE
    (COMPILATION-SPEED ,*standard-compilation-speed-declaration*)
    (DEBUG             ,*standard-debug-declaration*)
    (SAFETY            ,*standard-safety-declaration*)
    (SPACE             ,*standard-space-declaration*)
    (SPEED             ,*standard-speed-declaration*)
    #+lispworks (HCL:FIXNUM-SAFETY ,*standard-fixnum-safety-declaration*)
    #+lispworks (FLOAT             ,*standard-float-declaration*)
    #+lispworks (SYS:INTERRUPTABLE ,*standard-interruptable-declaration*)))

(defun performance-optimizations ()
  "Declarations for high-performance code.
   Place (DECLARE #.(performance-optimizations)) in high-performance
   code blocks."
  `(OPTIMIZE
    (COMPILATION-SPEED ,*performance-compilation-speed-declaration*)
    (DEBUG             ,*performance-debug-declaration*)
    (SAFETY            ,*performance-safety-declaration*)
    (SPACE             ,*performance-space-declaration*)
    (SPEED             ,*performance-speed-declaration*)
    #+lispworks (HCL:FIXNUM-SAFETY ,*standard-fixnum-safety-declaration*)
    #+lispworks (FLOAT             ,*performance-float-declaration*)
    #+lispworks (SYS:INTERRUPTABLE ,*performance-interruptable-declaration*)))

(defun fixnum-performance-optimizations ()
  "Declarations for high-performance code that is known
   to never overflow fixnums.
   Place (DECLARE #.(fixnum-performance-optimizations)) in code
   blocks.  Be careful.  When in doubt, use #.(performance-optimizations)
   and explitic fixmun arithmetic."
  `(OPTIMIZE
    (COMPILATION-SPEED ,*performance-compilation-speed-declaration*)
    (DEBUG             ,*performance-debug-declaration*)
    (SAFETY            ,*performance-safety-declaration*)
    (SPACE             ,*performance-space-declaration*)
    (SPEED             ,*performance-speed-declaration*)
    #+lispworks (HCL:FIXNUM-SAFETY ,*performance-fixnum-safety-declaration*)
    #+lispworks (FLOAT             ,*performance-float-declaration*)
    #+lispworks (SYS:INTERRUPTABLE ,*performance-interruptable-declaration*)))

;;; Set everything to standard optimizations.
(eval-when (:load-toplevel :execute)
  (proclaim
   `(OPTIMIZE
     (COMPILATION-SPEED ,*standard-compilation-speed-declaration*)
     (DEBUG             ,*standard-debug-declaration*)
     (SAFETY            ,*standard-safety-declaration*)
     (SPACE             ,*standard-space-declaration*)
     (SPEED             ,*standard-speed-declaration*)
     #+lispworks (HCL:FIXNUM-SAFETY ,*standard-fixnum-safety-declaration*)
     #+lispworks (FLOAT             ,*standard-float-declaration*)
     #+lispworks (SYS:INTERRUPTABLE ,*standard-interruptable-declaration*))))

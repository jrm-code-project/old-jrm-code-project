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
  (export '(
            unwind-protect-without-interrupts
            )))

;; This seemingly useless construct ensures that lispworks
;; autoloads the appropriate macros.
#+lispworks
(eval-when (:load-toplevel :execute)
  (macroexpand '(lw:without-interrupts nil)))

(defmacro unwind-protect-without-interrupts (protected-form &body cleanup-forms)
  "Like UNWIND-PROTECT, but the cleanup forms are run without-interrupts.
   This is structured so that there is no race condition in the cleanup
   forms."
  #+lispworks
  (let ((interrupt-enables (gensym "INTERRUPT-ENABLES-")))
    `(SYSTEM::FAST-MULTIPLE-VALUE-PROG1
      (LET ((,interrupt-enables SYSTEM::*IN-NO-INTERRUPTS*)
           (SYSTEM::*IN-NO-INTERRUPTS* 1))
       (CL:UNWIND-PROTECT
           (LET ((SYSTEM::*IN-NO-INTERRUPTS* ,interrupt-enables))
             (SYSTEM::WITHOUT-INTERRUPT-CHECK-FOR-INTERRUPTS)
             ,protected-form)
         ,@cleanup-forms))
      (SYSTEM::WITHOUT-INTERRUPT-CHECK-FOR-INTERRUPTS)))

  #+(and allegro (not dont-fix-franz))
  (let ((interrupt-enables (gensym (symbol-name :INTERRUPT-ENABLES-))))
    `(LET ((,interrupt-enables EXCL::*WITHOUT-INTERRUPTS*)
           (EXCL::*WITHOUT-INTERRUPTS* t))
       (CL:UNWIND-PROTECT
           (LET ((EXCL::*WITHOUT-INTERRUPTS* ,interrupt-enables))
             ,protected-form)
         ;; interrupts are off during cleanup
         ,@cleanup-forms))))

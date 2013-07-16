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

(in-package "CSF/CORE")

(proclaim (standard-optimizations))

(defgeneric marshal-from-transaction (element)
  (:documentation
   "Given a structure containing references to distributed objects,
   return a similar structure containing references to
   distributed-identifiers.  This is used to convert the result of
   a repository transaction to something that can be used outside
   of a transaction.")

  (:method ((element t))
    (error "Non-distributed object ~s cannot be marshaled outside of transaction." element))

  (:method ((element cons))
    (cons (marshal-from-transaction (car element))
          (marshal-from-transaction (cdr element))))

  (:method ((element distributed-identifier)) element)
  (:method ((element null)) nil)
  (:method ((element guid))     element)
  (:method ((element number))   element)
  (:method ((element pathname)) element)
  (:method ((element string))   element)
  (:method ((element symbol))   element))

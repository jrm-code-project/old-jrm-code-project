;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;	      Copyright (c) 2000 Content Integrity, Inc.
;;;;	      ALL RIGHTS RESERVED.
;;;;
;;;;	      Content Integrity, Inc. CONFIDENTIAL and PROPRIETARY material.
;;;;
;;;;	      Content Integrity, Inc
;;;;	      Braintree Executive Office Park
;;;;          P.O. Box 850942
;;;;	      Braintree, MA 02185-0942
;;;;
;;;; This software and information comprise valuable intellectual property
;;;; and trade secrets of Content Integrity, Inc., developed at substantial
;;;; expense by Content Integrity, which Content Integrity intends to
;;;; preserve as trade secrets.  This software is furnished pursuant to a
;;;; written license agreement and may be used, copied, transmitted, and
;;;; stored only in accordance with the terms of such license and with the
;;;; inclusion of the above copyright notice.  This software and
;;;; information or any other copies thereof may not be provided or
;;;; otherwise made available to any other person.  NO title to or
;;;; ownership of this software and information is hereby transferred.
;;;; Content Integrity assumes no responsibility for the use or reliability
;;;; of this software.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; File Name:	    fake-allegrostore.lsp
;;;; Author:        jrm
;;;; Creation Date: 2001-01
;;;;
;;;; Module Description:
;;;;
;;;; A number of fakes so the product will load without allegrostore. 
;;;; Not normally used.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is a fake allegrostore implementation.
;;; It doesn't work, it simply provides the appropriate symbols
;;; so that we can load non-functional astore code.

(in-package "ALLEGROSTORE")

(eval-when (:load-toplevel :execute)
  (export '(persistent-standard-class persistent-standard-object)))

(defclass persistent-standard-class (standard-class) ())
(defclass persistent-standard-object (standard-object) ())


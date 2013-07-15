;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;	      Copyright (c) 1999 Content Integrity, Inc.
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
;;;; File Name:	    closopt.lsp
;;;; Author:        Dave Bakhash
;;;; Creation Date: 12 Dec 1999
;;;; 
;;;; Module Description:
;;;;
;;;; Code for optimizing the delivery the the Conman application
;;;; This code is used in conjunction with delivery.lsp to tell the
;;;; system generation/packaging tool which packges to 'train' for CLOS.
;;;; The results of the build will leave training information in 
;;;; closopt.fasl.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; By including the following two lines and compiling this module AFTER
;; running relevant portions of the application, 'training' information
;; for clos stuff will be dumped to the closopt.fasl module, and may then
;; be subsequently loaded into the final application.
(in-package :clos)
(preload-forms)

(clos::preload-constructors (:astore
			     :utility
			     :core
			     :web
			     :server
			     :vm
			     :rfm
			     :conman))
(clos::precache-generic-functions (:astore
				   :utility
				   :core
				   :web
				   :server
				   :vm
				   :rfm
				   :conman))



;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright (c) 1999-2000 Content Integrity, Inc.
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
;;;; File Name:	    cli-http-request.sp
;;;; Author:        Arol Ambler
;;;; Creation Date: June 2000
;;;;
;;;; Module Description: Command line alias support.
;;;;
;;;; This module contains tables, update functions, and accessors that
;;;; support the notion of aliases: that is, alternative names for
;;;; certain constructs.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CONMAN")

(proclaim (standard-optimizations))

(defvar *parameter-alias-table* (make-hash-table :test #'equal :size 11)
  "Table to hold parameter aliases")
;; and the other tables, later

(defun find-parameter-alias (name &optional default-value)
  "Find the value for a parameter alias whose name is NAME.
   If there is no such alias, return DEFAULT-VALUE."
  (gethash name *parameter-alias-table* default-value))

#||

(eval-when (:load-toplevel :execute)
  (export '(find-parameter-alias 
	    create-parameter-alias
	    delete-parameter-alias
	    delete-all-parameter-aliases
	    map-parmeter-aliases)))

(defvar *parameter-alias-table* (make-hash-table :test #'equal :size 11)
  "Table to hold parameter aliases")
;; and the other tables, later

(defun find-parameter-alias (name &optional default-value)
  "Find the value for a parameter alias whose name is NAME.
   If there is no such alias, return DEFAULT-VALUE.
  "
  (gethash name *parameter-alias-table* default-value))

(defun delete-parameter-alias (name)
  "Deletes a parameter alias whose name is NAME, if it exists.
   Returns true if there was one, otherwise false.
  "
  (remhash name *parameter-alias-table*))

(defun create-parameter-alias (name value)
  "Create a parameter alias with name NAME and value VALUE.
   If there was already a parameter alias with the given name,
   silently changes the value to the given value.
   Returns the value.
  "
  (setf (gethash name *parameter-alias-table*) value))

(defun delete-all-parameter-aliases ()
  "Delete all parameter aliases, and return nil."
  (clrhash *parameter-alias-table*)
  nil
)

(defun map-parameter-aliases (function)
  "Given a FUNCTION of two arguments, invoke the function on each
   alias and expansion. Return nil.
  "
  (maphash function *parameter-alias-table*)
  )
||#

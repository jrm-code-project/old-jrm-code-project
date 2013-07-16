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

(eval-when (:load-toplevel :compile-toplevel :execute)
  (export '(*repository*
            *transaction*
            *versioned-compare-view*
            *versioned-value-cid-set-override*)))

;;; These variables are pervasively used in the CORE package.

(defvar-unbound *repository* "'Current' repository in which allocations will be performed.")

(defvar-unbound *transaction* "Bound by with-repository-txn to the current transaction.") 

(defvar-unbound *versioned-compare-view*
    "Bound to :before or :after to select one of the two views in a comparison transaction.")

(defvar *versioned-value-cid-set-override* nil
  "When not NIL, should be bound to a CID-SET under which we will
   view all versioned slot contents.  If NIL, we use the CID-SET
   found in the current transaction.")

(defconstant +cid-unassigned+ 0 "Sentinal indicating that CID has not yet been set.")

(defconstant +cvi-ion-invalid+ 0
  "Invalid when used as an ION specifier for a record in a sequence")

(defconstant +cvi-ion-top+ 0
  "Indicates a relative insertion point meaning 'top of file' or start of sequence.")

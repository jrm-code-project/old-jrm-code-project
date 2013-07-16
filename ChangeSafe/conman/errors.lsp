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

(in-package "CONMAN")

(proclaim (standard-optimizations))

;;; Called when the user-supplied change-name is unacceptable.
(defunimplemented conman-error/bad-change-name (change-name))

;;; Called when the user-supplied class-name is unacceptable.
(defunimplemented conman-error/bad-class-name (class-name))

;;; Called when an attempt is made to modify a frozen branch.
(defunimplemented conman-error/branch-frozen (branch))

;;; Called when create-class is called on an existing class.
(defunimplemented conman-error/class-exists (request class))

;;; Called when a master-repository exists where we are trying to
;;; create one.
(defunimplemented conman-error/master-repository-exists (master-name dbpath))

;;; Called when a change-description is omitted.
(defunimplemented conman-error/missing-change-description (&rest args))

;;; Called when a class argument names a class that doesn't
;;; exist.
(defunimplemented conman-error/no-such-class (request class-name))

;;; Called when a directory argument names a directory
;;; that does not exist (but should).
(defunimplemented conman-error/no-such-directory (&rest args))

;;; Called when a named product does not exist.
(defunimplemented conman-error/no-such-product (spec))

;;; Called when a named subsystem does not exist.
(defunimplemented conman-error/no-such-subsystem (spec))

;;; Called when a workspace identifier doesn't name a workspace.
(defunimplemented conman-error/no-such-workspace (workspace-spec))

;;; Called when product-create is called on an existing product.
(defunimplemented conman-error/product-exists (request product))

;;; Called when subsystem-create is called on an existing subsystem.
(defunimplemented conman-error/subsystem-exists (request subsystem))

;;; Called when a subscription to a subsystem already exists.
(defunimplemented conman-error/subsystem-already-subscribed (&rest args))

;;; Called when a workspace-create finds a workspace where
;;; the new workspace should be.
(defunimplemented conman-error/workspace-exists (directory))

;;; Called when a workspace command needs a workspace without
;;; an active change.
(defunimplemented conman-error/workspace-has-active-change (workspace))

;;; Called when a workspace command needs a workspace with
;;; an active change.
(defunimplemented conman-error/workspace-has-no-active-change (workspace))


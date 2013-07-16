;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2003 ChangeSafe, LLC
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
;;;;
;;;; A project reference is a reference to a product and branch that can
;;;; exist outside the database containing the product and branch.


(in-package "CSF/VERSION-MANAGEMENT")

(proclaim (standard-optimizations))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(
            project-reference
            project-reference/project-did
            project-reference/branch-did
            project-reference/version-state
            )))

(defclass project-reference (named-object described-object distributed-object)
  ((project-did :initarg :project-did
                :initform (error "Required initarg :project-did missing.")
                :reader project-reference/project-did
                :type distributed-identifier
                :version-technique :nonversioned)

   (branch-did :initarg :branch-did
                :initform (error "Required initarg :branch-did missing.")
                :reader project-reference/branch-did
                :type distributed-identifier
                :version-technique :nonversioned)

  ;; Here's the tricky part.
  ;; Ideally we'd use version-ref objects, but they aren't designed yet, and also they
  ;; are meant to be non-versioned encodings of branch state.  We really want versioned encodings
  ;; of branch state here for long term general purpose project-context use, since a project-context
  ;; user might like to explicitly alter the version context of the referenced branch under change
  ;; control.

  ;; So we try to specify information here compatible with the tools and techniques used by
  ;; a VM::VersionRef object, and we can then translate current versioned values of *this* VersionRef
  ;; equivalent into an actual VersionRef and use that transiently to manipulate version state.
  ;; In theory.
  
  ;; VERSION-STATE is a scalar form which takes the following values:
  ;; a) :LATEST implying we wish to always use the tip of the branch.
  ;; b) A string did representing a specific VERSION object on the branch, and whose final
  ;;    state is typically used, but which is really under control of the meta-version
  ;;    driving views of the version referenced by string did.  Tread carefully here!
  ;; c) A branch time specification, which is used to identify the branch contents driven by date.

  (version-state :initarg :version-state
                 :initform :latest
                 :accessor project-reference/version-state
                 :version-technique :scalar) ; NOT write-once, though it may be for many users of this class
  )
  (:documentation "PROJECT-REFERENCE: describes without cross-database pointers a branch of a project.")
  (:metaclass versioned-standard-class)
  (:schema-version 0))



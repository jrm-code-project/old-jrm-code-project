;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright (c) 1999-2000 Content Integrity, Inc.
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;;          Content Integrity, Inc. CONFIDENTIAL and PROPRIETARY material.
;;;;
;;;;          Content Integrity, Inc
;;;;          Braintree Executive Office Park
;;;;          P.O. Box 850942
;;;;          Braintree, MA 02185-0942
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
;;;; File Name:     branch.lsp
;;;; Author:        Dave Tenny
;;;;
;;;; Module Description:
;;;;
;;;; Branches, which represent single threads of evolution of versioned
;;;; material by showing major evolution stages as discrete Version
;;;; objects, and by showing minor evolution stages as changes to Version
;;;; state during a Version's mutable lifetime.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :vm)

(eval-when (:load-toplevel :execute)
  (export '(branch

            branch/add-version
            branch/owning-project
            branch/most-recent-version
            branch/most-recent-mutable-version
            branch/scan-versions

#||
            branch-next-version
            branch-get-latest-mutable-version
            branch-checkpoint
            branch-owning-project
            branch-get-most-recent-version
            branch-versions             ;vi-record-stream, no order guaranteed for outside users
            branch-create
            branch-name
            branch-version-list-latest-first    ; list, order guaranteed
            branch-parent-branch
            branch-parent-branch-timestamp
            branch-parent-branch-timestamp-specified-p
||#
            )))

(defclass branch (named-object described-object distributed-object)
  (
  ;; Pointer to project which contains this branch, always unique, not modified after creation
  (owning-project :initarg :owning-project
                  :initform (error "Required initarg :owning-project omitted")
                  :version-technique nil
                  :reader branch/owning-project)
  ;; Pointer to parent branch from which this was derived, if appropriate.
  ;; Root branches in a project won't have a parent branch.
  (parent-branch :version-technique nil)        ; not logged, not versioned, immutable after initialization
  ;; The timestamp at the point on the PARENT-BRANCH from which this
  ;; branch is derived.  This is not necessarily the timestamp of the
  ;; birth cid of the branch since the branch might be based on some
  ;; past view of the parent.  The value is a TIME-STAMP
  (parent-branch-timestamp :version-technique nil) ; not logged, not versioned, immutable after initialization
  ;; NIL indicates that the PARENT-BRANCH-TIMESTAMP passed to
  ;; BRANCH-CREATE when this branch was created was NIL and thus that
  ;; this branch is based on what was the tip of the parent branch at
  ;; the time this branch was made.
  (parent-branch-timestamp-specified-p :version-technique nil)

  ;; *FINISH*: decide how to manage this slot.
  ;; The first version in the list is either latest and mutable, or immutable if the branch
  ;; is frozen.  We push most recent first.
  (versions :initform nil
   :accessor branch/versions
   :version-technique :composite-sequence)
  (version-counter :version-technique :scalar)) ; for demo, not for production
  ;; not yet done...
  ;; (latest-aggregate-set ...) ; not needed if we use meta-version
  (:documentation
     "A branch of a project, representing one versioned view over time.
      Each branch maintains the evolving state of a VERSION, branch ancestry, etc.
      The 'latest' version of a branch is typically used to derive the CID-SET used
      to view SCM-DIRECTORY contents of a PROJECT.
      Branch data is versioned, so that we can view the state of a branch as of a
      particular point in time. ")
  (:metaclass versioned-standard-class)
  (:schema-version 0))

#||
(defun branch-create (branch-name parent-branch parent-branch-timestamp
                      branch-description owning-project &key first-project-branch (branch-type 'branch))
  "Create a BRANCH object with the indicated name.

   PARENT-BRANCH should be NIL for branches with no ancestry.  If there is any ancestry,
   it's very important that you supply the branch.  The initial contents of the (new) initial branch
   version will be set to the contents of the ancestor branch version, under the interpretation of the
   metaversion in context. (I.e. we get the latest version of the ancestor branch under metaversion
   interpretation.)

   PARENT-BRANCH-TIMESTAMP is a TIME-STAMP identifying the point in time on the PARENT-BRANCH
   from which this branch is based, or NIL to indicate the tip of PARENT-BRANCH.  This argument
   should be NIL if PARENT-BRANCH is NIL.

   BRANCH-DESCRIPTION be a string for DESCRIBED-OBJECT protocols, or NIL.

   OWNING-PROJECT is the project which owns this branch.

   FIRST-PROJECT-BRANCH should be TRUE if this branch is the first branch of a project, in which
   case it is not necessary to promote the project creation cid into the initial branch version.

   Return TWO values, the branch, and the CID-DID-STRING which created the branch."
  ;; *FINISH*: CID representations will change, passing string forms is inefficient.
  ;; TBD: may want supply initial CID-SET for version, or take it from parent branch if supplied.
  #+allegro (declare (:fbound project-creation-cid-did-string)) ;forward declaration
  (check-type branch-description string)
  (if parent-branch
      (assert (or (null parent-branch-timestamp)
                  (time-stamp? parent-branch-timestamp)))
    (assert (null parent-branch-timestamp)))
  (let ((branch (make-instance branch-type))
        (ancestor-version (and parent-branch (branch-get-most-recent-version parent-branch))))
    (named-object-initialize branch branch-name)
    (set-branch-parent-branch branch parent-branch)
    (set-branch-parent-branch-timestamp
     branch
     (when parent-branch  ; no timestamp if no parent
       (or parent-branch-timestamp
           (core::cid-master-table-entry-when-start
            (core::txn-context-cid-master-table-entry *txn-context*)))))
    (set-branch-parent-branch-timestamp-specified-p branch
                                                    (when (and parent-branch
                                                               parent-branch-timestamp)
                                                      t))
    (set-branch-owning-project branch owning-project)
    (set-branch-version-counter branch 1)
    (when branch-description
      (set-described-object-text branch branch-description))
    (multiple-value-bind (version creation-cid-did-string) ; is also branch creation cid
        (if ancestor-version
            (version-create branch :ancestor-version ancestor-version)
          (version-create branch))
      (set-branch-versions branch (list version))
      (unless (or parent-branch first-project-branch)
        ;; This is a root branch, give it at least the project creation cid so we can see
        ;; some version of the root directory/project name.  Users can always change this
        ;; by migrating other csets into branch.
        (version-promote-cid version (project-creation-cid-did-string owning-project)))
      (values branch creation-cid-did-string))))

(defun branch-name (branch) ;; note: object may be a PC as well as a branch???
  "Return the Branch/Release object name."
  (named-object-name branch))

||#
(defun branch/add-version (branch version)
  (assert (eq (version/owning-branch version) branch))
  (push version (branch/versions branch)))

(defun branch/most-recent-version (branch)
  "Return the most recent version of a branch, 'latest' if it is available, otherwise the last
   capped version."
  ;; We know we always have at least one version and assume the versions will always
  ;; have at least one value.
  (car (branch/versions branch)))

(defun branch/scan-versions (branch)
  (declare (optimizable-series-function))
  (versioned-object/scan-composite-versioned-slot branch 'versions))

(defmethod branch/most-recent-mutable-version ((branch branch))
  "Return the latest version of a branch, implied mutable, or NIL if there is no mutable latest version"
  (let ((first (branch/most-recent-version branch)))
    (unless (or (null first)
                (version/frozen? first))
      first)))

#||
(defmethod branch-get-latest-mutable-version ((branch branch))
  "Return the latest version of a branch, implied mutable, or NIL if there is no mutable latest version"
  (let ((first (branch-get-most-recent-version branch)))
    (and first (version-mutable? first) first)))

(defmethod branch-next-version ((branch branch) version)
  "Return the next most recent version in BRANCH following VERSION if it exists, or NIL if it doesn't.
   If a bersion is found, it's ancestor version(s) should include VERSION."
  ;; Most recent elements are first
  (let ((vi-record-stream (branch-versions branch)))
    (loop with prev = nil
        while (vi-stream-ready? vi-record-stream)
        as current = (vi-stream-get-value vi-record-stream)
        when (eq current version)
        do (return prev)
        do (setq prev current))))

(defsubst branch-version-list-latest-first (branch)
  "Return the version list of a branch, guaranteeing latest version first, followed
   by older versions in reverse order of that in which they were created."
  (vi-stream-as-list (branch-versions branch)))

(defmethod branch-checkpoint ((branch branch) final-description new-description)
  "Checkpoint the current 'latest' version in the branch, assigning it the indicated final
   description, which should be a non-empty string.  Create a new 'latest' version with
   the initial description NEW-DESCRIPTION, which may be nil or an empty string.
   Return two values, the string name of the checkpointed version, and the string name of the new
   latest version, in that order."
  (let ((latest-version (branch-get-latest-mutable-version branch)))
    (unless latest-version
      (error "Branch '~a' (~a) has no latest version for checkpoint"
             (object-user-name branch)
             (distributed-object-identifier branch)))
    ;; These SETs should be moved into a version-checkpoint routine.  Ah well, it's demo code.
    (set-version-mutable? latest-version nil)
    (update-object-user-name latest-version (format nil "V~d" (branch-version-counter branch)))
    (set-described-object-text latest-version final-description)
    (set-branch-version-counter branch (1+ (branch-version-counter branch)))
    (let ((new-version (version-create branch
                                       :description (and (not (empty-string? new-description))
                                                         new-description)
                                       :ancestor-version latest-version))
          (existing-versions (vi-stream-as-list (branch-versions branch))))
      ;; Need branch-add-version method.
      (push new-version existing-versions)
      (set-branch-versions branch existing-versions)
      (values (object-user-name latest-version) (object-user-name new-version)))))

||#

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
;;;; File Name:     project.lsp
;;;; Author:        Dave Tenny
;;;;
;;;; Module Description:
;;;;
;;;; Think of a PROJECT as managing two subtrees of data.
;;;; In the left subtree we have versioned data of interest to users, such
;;;; as versioned file systems.  In the right subtree we have version
;;;; management data such as branches, versions, and other appropriate
;;;; tools for managing change-sets.
;;;;
;;;; The PROJECT class and associated machinery in the VM package describe
;;;; the right subtree, that is, the version management machinery.
;;;; It is left to subtypes to describe the nature of user data which is
;;;; versioned in the other subtree.
;;;;
;;;; We may ultimately require a third subtree which describes versioned
;;;; data which is also version-management data, in particular, change-set
;;;; aggregates.  However this is not implemented at this time.  If and
;;;; when it is, it belongs in this package since it's primary task is
;;;; version management.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CSF/VERSION-MANAGEMENT")

(eval-when (:load-toplevel :execute)
  (export '(project

            project/add-branch
            project/add-change-set
            project/add-main-branch
            project/branches
            project/change-sets
            project/main-branch
            project/scan-branches
            project/scan-change-sets
#||
            project-get-roots
            project-get-main-branch
            project-lookup-branch-name  ;find branch given branch name
            project-add-branch
            project-branches            ;vi-record-stream of branches
            project-get-branch-list             ;list of branches
            project-map-over-branches
            project-get-latest-mutable-version
            project-initialize
            project-change-sets         ;vi-record-stream of branches
            project-change-set-list     ;list oc change-sets
||#
            )))

(defclass project (named-object described-object distributed-object)
  ((branches :accessor project/branches
             :initform  nil
             :version-technique :composite-set)
   (creation-cid-did-string) ; not versioned, immutable after initial assignment

   ;; NOTE that if the :PROJECT-CHANGE-SETS-UNMAINTAINED feature is
   ;; present then the code which maintains the value of this slot is
   ;; no-oped and the slot's value is not maintained.
   (change-sets :initform nil
                :version-technique :composite-set
                :accessor project/change-sets))
  (:documentation
     "A project introduces a scope for managing a collection of versioned content.
      Projects provide tools for managing versions of content, where a version is ultimately a set
      of cid-sets which are used to view and alter versioned content.

      Projects may contain one or more branches which in turn contain the sequence of versions
      representing the state of versioned at all points in time on that branch.  (A branch
      in this respect is like a time-line on a particular dimension of reality).

      The versioned content is completely subject to user control, and is
      provided in subtypes of PROJECT.  Project is therefore an abstract class.

      UNLESS OTHERWISE SPECIFIED OR ARRANGED BY THE USER, ALL VM PACKAGE INFORMATION
      IS VIEWED WITH THE REPOSITORY-MASTER-CID-SET.  There are subtle implications to this fact
      which are beyond the scope of this documentation.")
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defun project/add-branch (project branch)
  (assert (eq project (branch/owning-project branch)))
  ;; Funky dance here is because the persistence and 
  ;; versioning isn't pervasive; neither would notice
  ;; that we spliced into the middle.
  (let ((branches (project/branches project)))
    (setf (project/branches project)
          (if (null branches)
              (list branch)
              (list* (car branches) branch (cdr branches))))))

(defun project/add-main-branch (project branch)
  (assert (eq project (branch/owning-project branch)))
  (push branch (project/branches project)))

(defun project/scan-branches (project)
  (declare (optimizable-series-function))
  (versioned-object/scan-composite-versioned-slot project 'branches))

(defun project/add-change-set (project change-set)
  "Add a change-set to this project.  Returns the passed change-set."
  ;; Whether or not a change-set is even used by a project depends on many things, and csets
  ;; really belong to repositories, not projects.  However from some user model perspectives it's
  ;; useful to examine project change sets, even though every latest version of the project
  ;; on every branch may have been turned off (and therefore appears not to be used by the project,
  ;; unless you include earlier versions).
  ;; Theoretically we can derive the project change-sets list from versions, except that there
  ;; could be change-sets which are created and never promoted (and may or may not be tracked somewhere
  ;; for such change-sets). Until we decide to search versions and other special places to see
  ;; if a change-set was ever used by a project (yuck), we just store them here, at the request
  ;; of whatever agency decides a change-set goes with a project (such as callers of PROJECT-CHECKIN).

  ;; Gee, that's a lot of commentary for such a trivial operation!
  (push change-set (project/change-sets project)))

(defun project/scan-change-sets (project)
  (declare (optimizable-series-function))
  (versioned-object/scan-composite-versioned-slot project 'change-sets))

#||

(defun project-initialize (project project-name project-description
                           &key (initial-branch-name "main") (branch-type 'branch))
  "Initialize a PROJECT.  A change-transaction must already be active.
   INITIAL-ROOTS, if specified, should be a list of objects which will be used as the current
   project root container children.

   PROJECT-DESCRIPTION must be NIL or a string.  Note that the RFM model
   assumes that a string is required and will often specify an empty string, and that the
   introduction of NIL descriptions will require a check in all RFM interfaces which don't expect
   that back on project queries.  However the requirement that the description be specified
   has been relaxed so that we can determine the difference between no description and a (typically zero
   length) string description which was passed because NIL wasn't allowed.

   This method is called by PROJECT-CREATE, or constructors of project subtypes, if appropriate."
  (check-type project-name string)
  (check-type project-description (or null string))
  (multiple-value-bind (branch creation-cid-did-string)
      (branch-create initial-branch-name nil nil "Main project branch" project :first-project-branch t
                     :branch-type branch-type)
    (named-object-initialize project project-name)
    (when project-description
      (set-described-object-text project project-description))
    (set-project-branches project (list branch))
    ;; We stash the creation cid for the project/branch-version so that we might provide a cid
    ;; with which to view project directory name when we create new root branches (i.e. no ancestor).
    ;; *FINISH*: CID representations will change, string DID forms are very inefficient.
    (set-project-creation-cid-did-string project creation-cid-did-string)
    project))

#+project-change-sets-unmaintained
(defmethod project-change-sets :before ((project project) &optional txn-context)
  (declare (ignore txn-context))
  (error "The ~s slot of ~s is not maintained."
         'change-sets
         (class-name (class-of project))))

#+project-change-sets-unmaintained
(defmethod set-project-change-sets :before ((project project) new-value &optional txn-context)
  (declare (ignore new-value txn-context))
  (error "The ~s slot of ~s is not maintained."
         'change-sets
         (class-name (class-of project))))

(defmethod object-change-interesting-p or ((object project) (slot-name symbol) birth-cid-p)
  (and (not birth-cid-p)
       (eq slot-name 'branches)))

(defmethod object-change-user-semantics or ((object project) (slot-name symbol)
                                            change-type valid-p new-or-old old)
  (declare (ignore change-type valid-p new-or-old old))
  (and (eq slot-name 'branches)
       "add branch"))                   ;sometime we might have 'merge' branch, but never delete...

(defsubst project-change-set-list (project)
  "Return a list of change-sets associated with PROJECT."
  (vi-stream-as-list (project-change-sets project)))

(defun project-add-change-set (project change-set)
  "Add a change-set to this project.  Returns the passed change-set."
  #+project-change-sets-unmaintained
  (declare (ignore project))
  ;; Whether or not a change-set is even used by a project depends on many things, and csets
  ;; really belong to repositories, not projects.  However from some user model perspectives it's
  ;; useful to examine project change sets, even though every latest version of the project
  ;; on every branch may have been turned off (and therefore appears not to be used by the project,
  ;; unless you include earlier versions).
  ;; Theoretically we can derive the project change-sets list from versions, except that there
  ;; could be change-sets which are created and never promoted (and may or may not be tracked somewhere
  ;; for such change-sets). Until we decide to search versions and other special places to see
  ;; if a change-set was ever used by a project (yuck), we just store them here, at the request
  ;; of whatever agency decides a change-set goes with a project (such as callers of PROJECT-CHECKIN).

  ;; Gee, that's a lot of commentary for such a trivial operation!

  ;; Do nothing if the maintenance of the CHANGE-SETS slot of PROJECT
  ;; has been disabled.
  #-project-change-sets-unmaintained
  ;; Ensure that we use the repository master cid-set to update the change-sets slot.
  ;; At least as long as we version this slot...
  (with-txn-context-cid-set (*txn-context* (repository-master-cid-set *repository*))

    (if (debug-level-meets-or-exceeds? 4)
        ;; LONG AND SLOW WAY: SUBSTANTIAL PERFORMANCE IMPACT.
        ;; This check is probalby useful to somebody someday.  It's from legacy e-zchange demo code.
        ;; But the RFM e-zchange demo
        ;; doesn't really need it and isn't going to be used much.  And Conman pays a heavy cost
        ;; for this right now when there are thousands of changes-sets.  So we eliminate the check,
        ;; and CONMAN should really completely suppress the call to project-add-change-set in the first
        ;; place.  However I haven't done this yet (JDT 26-NOV-2000) because conman is still using
        ;; project-change-sets (incorrectly!  must eliminate!)
        (let ((change-sets (vi-stream-as-list (project-change-sets project))))
          ;; Cid-object is a canonical object, can use EQ/EQL here for checks.
          ;; Don't allow duplicate change-sets in the list
          (when (find (change-set-cid-object change-set) change-sets :key #'change-set-cid-object)
            (error "Change-set ~s (~a) is already associated with project ~s (~a)"
                   (named-object-name change-set)
                   (distributed-object-identifier change-set)
                   (named-object-name project)
                   (distributed-object-identifier project)
                   ))                   ;did for high level cset, not low level.
          (push change-set change-sets) ;change-set are most-recent first
          (set-project-change-sets project change-sets))

      ;; This is much more efficient, doesn't pull in whole versioned history.
      (versioned-object-push-composite-values project 'change-sets (list change-set)))

    )
  change-set)

(defmethod vm-txn-note-change-set ((project project) (change-set change-set))
  (project-add-change-set project change-set))

(defsubst project-get-branch-list (project)
  "Return a list of branches on the project"
  (vi-stream-as-list (project-branches project)))

(defun project-map-over-branches (project function)
  "Apply FUNCTION to each branch of PROJECT."
  (vi-stream-for-each function (project-branches project)))

(defsubst project-lookup-branch-name (project branch-name)
  "Search for BRANCH-NAME, which must be a string, in the current versioned view of branch names
   associated with the project.  Return a branch object if a match is found, or NIL if no match is found."
  (vi-stream-find-if (lambda (branch)
                         (string= branch-name (object-user-name branch)))
                     (project-branches project)))

(defun project-add-branch (project branch)
  "Add a branch to the project. Signal an error if another branch with the name exists."
  (let ((branches (vi-stream-as-list (project-branches project))))
    (when (find (named-object-name branch) branches :key #'named-object-name)
      (error "Another branch with name '~a' already exists" (named-object-name branch)))
    (pushlast branch branches)          ;branches are in order of time of creation
    (set-project-branches project branches)))

||#
(defun project/main-branch (project)
  "Return the 'main' branch, which is that branch which was the created with the project.
   We keep this branch in slot 0."
    (car (project/branches project)))

#||
(defsubst project-get-latest-mutable-version (project &optional branch)
  "Return the latest version from the branch. If branch is not specified,
   use the main branch, which is that branch which was the created with
   the project."
  (branch-get-latest-mutable-version (or branch (project-get-main-branch project))))
||#





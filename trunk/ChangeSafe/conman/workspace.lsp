;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright (c) 1999-2000 Content Integrity, Inc.
;;;;          Copyright (c) 2000-2005 ChangeSafe, LLC
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
;;;; File Name:     workspace.lsp
;;;; Author:        Dave Tenny
;;;; Creation Date: October 1999
;;;;
;;;; Module Description:
;;;;
;;;; A workspace encodes information on a user's disk are which corresponds
;;;; to some versioned content in a ChangeSafe repository.  In the case
;;;; of ConMan, a workspace is representing a product-configuration,
;;;; which may span multiple subprojects.   In the case of e-zchange,
;;;; a workspace always refers to a root directory which has a one-to-one
;;;; mapping with a rfm-project instance.
;;;;
;;;; A user may have multiple workspaces.  For conman, a workspace
;;;; may have zero or one active changes associated with it.
;;;; An "active change" is represented by a rfm::change-context object,
;;;; since change-sets are never created until the last moment, at which
;;;; point the rfm::change-context is no longer needed.
;;;;
;;;; This module provides support for a workspace "semi-persistent" repository
;;;; which contains no versioned information.  Workspaces are meant to be
;;;; created, persist for a limited time, and then be deleted.
;;;;
;;;; Since a user may have multiple workspaces, workspaces identify the
;;;; owning user by means determined by the caller (typically a user string did,
;;;; but perhaps not).  Workspaces in turn have unique identifiers which can
;;;; be used to identify them.  Since they're not distributed objects
;;;; in the way that versioned-objects are, they do not have distributed
;;;; identifiers (and derivative string-dids).  The WORKSPACE-IDENTIFIER
;;;; object yields the unique identifier for a workspace, and the
;;;; WORKSPACE-LOOKUP function finds a workspace in a repository given
;;;; that identifier.
;;;;
;;;; A workspace identifier is guaranteed to be unique for all time
;;;; in a workspace repository.  It will also distinguish between
;;;; active, no-longer-active, and never-existed cases for a given identifer.
;;;;
;;;; WORKSPACES MUST NEVER BE GIVEN POINTERS TO OBEJCTS IN OTHER DATABASES,
;;;; AND SHOULD NEVER BE REFERRED TO BY OTHER DATABASES EXCEPT BY THE
;;;; WORKSPACE-IDENTIFIER.
;;;;
;;;; See also: workspace-master.lsp, which does various tricks with workspaces
;;;; in the context of a master repository transaction.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CONMAN")

;;; API's exported from this module.
(eval-when (:load-toplevel :execute)
  (export '(

#||
            ;; classes defined
            workspace
            ;; Predicates
            workspace-identifier?       ;true if it is syntax compatible with unique workspace identifier
            ;; make, find, delete them
            workspace-create
            workspace-modify-for-ws-set
            workspace-modify-for-ws-move
            workspace-delete
            map-over-workspaces
            workspace-lookup            ;search based on unique id
            workspace-resolve-spec      ;broader search criteria for lookup
            ;; Accessors.  In the event you note these are SETFable, do not do so outside this module.
            ;; Use update api's instead.
            workspace-identifier
            workspace-owner-id
            workspace-path
            workspace-persistent-change-context ;should probably be deprecated in favor of next method
            workspace-get-change-context ;retrieve a transient version of the persistent change context
            workspace-pc-did
            workspace-pc-branch-did
            workspace-baseline-timestamp
            workspace-locked-subsystems
            workspace-added-subsystem-cset-tuples
            workspace-removed-class-cset-tuples
            workspace-existing-added-VPB-cset-dids-for-subsystem
            workspace-existing-removed-VPB-cset-dids-for-subsystem
            workspace-VPB-cset-components-active?
            workspace-VPB-cset-components-inactive?

            ;; Workspace transitioning
            workspace-in-transition?
            workspace-transition-mode
            workspace-set-transition-finished
            workspace-abort-transition
            workspace-begin-transition
            workspace-transitional-versionref
            workspace-transitional-added-subsystem-cset-tuples
            workspace-transitional-removed-class-cset-tuples
            workspace-transitional-pc-did
            workspace-transitional-pc-branch-did
            workspace-transitional-timestamp
            workspace-transitional-added-VPB-cset-dids-for-subsystem
            workspace-transitional-removed-VPB-cset-dids-for-subsystem
            workspace-transitional-VPB-cset-components-active?
            workspace-transitional-VPB-cset-components-inactive?

            ;; Update workspace information
            workspace-note-locked-subsystems
            workspace-add-change-context
            workspace-delete-change-context
            workspace-add-subsystem-csets
            workspace-remove-subsystem-csets
            workspace-promote-current-VPB-change
            ;; manage the workspace repositories from a transaction/open standpoint
            workspace-repository-create
            *current-workspace-repository*
            with-open-workspace-repository
            with-workspace-repository-txn

            workspace-has-added-subsystem-cset-tuples
            workspace-has-removed-class-cset-tuples
            workspace-has-vpb-changes

            workspace-has-port-activity-p
            workspace-map-over-port-activity
            workspace-update-port-activity
            workspace-clear-port-activity
            workspace-port-activity-collect-adds-and-deletes-by-subsystem
            workspace-port-activity-collect-cset-rejection-by-subsystem
            workspace-port-activity-entry-action-reject-p
            workspace-act-on-port-activity
            workspace-port-activity-acted-on-p
            workspace-product-directory-p
||#
            )))

(defvar-unbound *current-workspace-repository*
  "When we open a workspace, this is dynamically bound to the workspace repository.
   We should only have a single workspace repository open at any one time.")

(defun call-with-workspace-repository (&key master-repository-dbpath
                                            open-mode
                                            (if-does-not-exist :error)
                                            (if-exists :open)
                                            receiver)
  (if (boundp '*current-workspace-repository*)
      (error "Attempt to open multiple workspaces.")
      (with-open-repository (workspace-repository (conman-workspace-repository-dbpath master-repository-dbpath)
                                                  open-mode
                                                  :error-if-not-cached t
                                                  :repository-type :basic
                                                  :if-does-not-exist if-does-not-exist
                                                  :if-exists if-exists)
        (let ((*current-workspace-repository* workspace-repository))
          (funcall receiver workspace-repository)))))

(defun workspace-transaction-type->open-mode (transaction-type)
  (ecase transaction-type
    (:read-write-nonversioned :update)
    (:read-only-nonversioned :readonly)))

(defun call-with-workspace-repository-transaction (master-repository-dbpath user-id-specifier
                                                                            &key
                                                                            transaction-type
                                                                            reason
                                                                            (if-exists :open)
                                                                            (if-does-not-exist :error)
                                                                            receiver)
  "Open the workspace repository, begin a transaction, resolve the workspace and
   invoke receiver on two arguments, the workspace repository and the workspace.
   The workspace is closed when receiver exits.

   TRANSACTION-TYPE is one of :read-write or :read-only and controls the type of transaction."
  (call-with-workspace-repository
   :master-repository-dbpath master-repository-dbpath
   :open-mode (workspace-transaction-type->open-mode transaction-type)
   :if-exists if-exists
   :if-does-not-exist if-does-not-exist
   :receiver (lambda (workspace-repository)
               (call-with-workspace-transaction
                :reason reason
                :repository workspace-repository
                :transaction-type transaction-type
                :user-id-specifier user-id-specifier
                :receiver (lambda (workspace-transaction)
                            (funcall receiver workspace-repository workspace-transaction))))))

(defun workspace-repository-create (master-repository-dbpath user-id-specifier)
  "Create a workspace repository with the indicated name, which must be compatible with
   REPOSITORY-OPEN or a DB-NAME object.  It is an error if the workspace repository already exists.
   Workspace repositories are NOT VERSIONED, and DO NOT HAVE CHANGE-SETS."
  ;; Ok to use with-open-repository for creation.
  (call-with-workspace-repository-transaction
   master-repository-dbpath user-id-specifier
   :transaction-type :read-write-nonversioned
   :if-exists :error
   :if-does-not-exist :create
   :reason "Create workspace repository"
   :receiver (lambda (workspace-repository workspace-transaction)
               (declare (ignore workspace-transaction))
               (workspace-repository/initialize workspace-repository))))

(defconstant +workspace-allocator-root-key+ 'workspace-allocator
  "Key used for REPOSITORY-GET-LOCALLY-NAMED-ROOT interface and naming the sole instance
   of a workspace-allocator in a semipersistent workspace repository.")

(defun workspace-repository/initialize (workspace-repository)
  "Perform workspace repository initialization.  Return value: N/A. Not exported."
  (let ((workspace-allocator (make-instance 'workspace-allocator)))
    (repository/add-locally-named-root workspace-repository workspace-allocator
                                       +workspace-allocator-root-key+)
    nil))

(defclass workspace-allocator ()
  ((instance-vector :initform (make-instance 'persistent-vector)
                    :reader workspace-allocator/workspace-vector))
  (:documentation
     "An object which manages workspace identifier allocation and extent access in a workspace
     repository.  There is only one workspace allocator defined in a workspace repository.")
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defun scan-workspaces (workspace-repository)
  (declare (optimizable-series-function))
  (scan-persistent-vector
   (workspace-allocator/workspace-vector
    (repository/locally-named-root workspace-repository +workspace-allocator-root-key+))))

(defun workspace-repository/find-workspace (workspace-repository workspace-id)
  "Fetch the workspace associated with a particular workspace-id."
  (etypecase workspace-id
    (array-index (persistent-vector-ref
                  (workspace-allocator/workspace-vector
                   (repository/locally-named-root workspace-repository +workspace-allocator-root-key+))
                  workspace-id))
    (guid (collect-first
           (choose-if (lambda (workspace)
                        (eq (workspace/guid workspace) workspace-id))
                      (scan-workspaces workspace-repository))))))

(defun call-with-workspace (master-repository-dbpath user-id-specifier workspace-id
                                                     &key transaction-type
                                                     reason
                                                     receiver)
  (call-with-workspace-repository-transaction
   master-repository-dbpath user-id-specifier
   :transaction-type transaction-type
   :reason reason
   :receiver (lambda (workspace-repository workspace-transaction)
               (funcall receiver
                        workspace-repository workspace-transaction
                        (workspace-repository/find-workspace workspace-repository workspace-id)))))

(defun workspace-repository/allocate-id (workspace-repository)
  "Allocate a unique identifier in workspace-repository for the workspace object,
   and assign the workspace object to the identifier, and vice versa.

   WORKSPACE-REPOSITORY is assumed to be open for update and in an update transaction.

   Return the unique identifier which was allocated.

   Queries on the returned identifier may be performed with workspace-lookup."
  (persistent-vector-length
   (workspace-allocator/workspace-vector
    (repository/locally-named-root workspace-repository
                                   +workspace-allocator-root-key+))))

(defun workspace-repository/register-workspace (workspace-repository workspace)
  "Registers the workspace in the instance vector.  Returns the workspace."
  ;; push it on the end
  (persistent-vector-push (workspace-allocator/workspace-vector
                           (repository/locally-named-root workspace-repository
                                                          +workspace-allocator-root-key+))
                          workspace))

(defgeneric workspace/added-master-csets (workspace)
  (:documentation "Returns the list of master csets that have been promoted in the virtual private branch."))

(defgeneric workspace/removed-master-csets (workspace)
  (:documentation "Returns the list of master csets that have been demote from the virtual private branch."))

(defgeneric workspace/transitional-added-master-csets (workspace)
  (:documentation "Returns the transitional list of master csets that have been promoted in the virtual private branch."))

(defgeneric workspace/transitional-removed-master-csets (workspace)
  (:documentation "Returns the transitional list of master csets that have been demote from the virtual private branch."))

(defclass workspace ()
  ((identifier :initarg :id
               :initform (error "Required initarg :id omitted.")
               :reader workspace/id)

   (description :initform nil
                :initarg :description
                :reader workspace/description
                :type (or null string))                    ;NIL or string description of the workspace

   ;; needed because of convoluted path through the browser fsa
   (guid :initarg :guid
         :initform (error "Required initarg :guid omitted.")
         :reader workspace/guid)
   (owner-id :initarg :owner-id
             :initform (error "Required initarg :owner-id omitted.")
             :reader workspace/owner-id)
   (path :initarg :path
         :initform (error "Required initarg :path omitted.")
         :reader workspace/path)
   ;; `Flatten' the change context.
   (cset-name :initarg :cset-name
              :initform nil
              :type (or null string)
              :reader workspace/cset-name)
   (cset-description :initarg :cset-description
                     :initform nil
                     :type (or null string)
                     :reader workspace/cset-description)
   (file-additions :initarg :file-additions
                   :initform (make-instance 'persistent-vector :size 0)
                   :reader workspace/file-additions)
   (file-changes :initarg :file-changes
                 :initform (make-instance 'persistent-vector :size 0)
                 :reader workspace/file-changes)
   (file-deletions :initarg :file-deletions
                   :initform (make-instance 'persistent-vector :size 0)
                   :reader workspace/file-deletions)
   (file-renames :initarg :file-renames
                 :initform (make-instance 'persistent-vector :size 0)
                 :reader workspace/file-renames)
   ;; Files that have been added, then renamed.
   (file-add-renames :initarg :file-add-renames
                 :initform (make-instance 'persistent-vector :size 0)
                 :reader workspace/file-add-renames)

   ;; A workspace may be `in transition'.  This means that the state of the workspace on the disk is
   ;; known to be stale.  The tuple of baseline-versionref, added-class-cset-tuples, and
   ;; removed-class-cset-tuples is the `old' state of the workspace, while the tuple of
   ;; transitional-versionref, transitional-added-class-cset-tuples, and
   ;; transitional-removed-class-cset-tuples is the `new' state.
   ;; The command ws_sync attempts to change the workspace from the `old' to the `new' state.
   ;; The ws_sync_commit command replaces the `old' state with the `new' state, and the ws_sync_abort
   ;; command discards the `new' state.
   ;; The transition mode tells ws_sync how to proceed.  It can have the following values:
   ;;
   ;; NIL  the workspace is believed to be up to date.
   ;; :create     the workspace is being created
   ;; :set        the workspace is being set
   ;; :regenerate the workspace is being regenerated
   ;; :update     the workspace is being updated
   ;; :delete     the workspace is being deleted
   ;;   (if there are others, they are pretty obvious)
   (transition-mode :initform nil
                    :initarg :transition-mode
                    :type symbol
                    :reader workspace/transition-mode)

   ;; current state of workspace
   (baseline-versionref :initform nil
                        :initarg :baseline-versionref
                        :reader workspace/baseline-versionref
                        :type versionref)
                                        ;   (points to a PC branch)
   ;; intended state of workspace
   (transitional-versionref :initform nil
                            :initarg :transitional-versionref
                            :reader  workspace/transitional-versionref
                            :type versionref)        ; vm::versionref identifying future state of workspace baseline
                                        ;   (points to a PC branch)

   ;; *** Aren't we risking that the LOCKED-SUBSYSTEMS of a workspace
   ;; can become inconsistent with the LOCK-STATUS of a subsystem.
   ;; This is of particular concern since they are stored in different
   ;; repositories and we don't have cross-repository transaction
   ;; integrity.

   ;; jrm: actually, we will have cross-repository transaction integrity
   ;; if we update the repositories in a single transaction.
   ;; so this will be ok if we are careful.
   (locked-subsystem-dids :initform nil
                          :reader workspace/locked-subsystem-dids)  ;list of subsystem dids which are locked

   ;; The following information is used to synthesize a virtual user branch.
   ;; We apply these mods to the cid-set which results from using the ancestor BASELINE-VERSIONREF
   ;; to derive an descendant version.

   ;; Experimental code, August 2004
   ;; I think we need to track the change sets for the master version.
   ;; This is because `removing' a master change set removes the satellite change sets,
   ;; but didn't seem to actually remove the master.
   (added-master-csets :initarg  :added-master-csets
                       :initform nil
                       :reader   workspace/added-master-csets)

   (transitional-added-master-csets :initarg  :transitional-added-master-csets
                       :initform nil
                       :reader   workspace/transitional-added-master-csets)

   (removed-master-csets :initarg  :removed-master-csets
                       :initform nil
                       :reader   workspace/removed-master-csets)

   (transitional-removed-master-csets :initarg  :transitional-removed-master-csets
                         :initform nil
                         :reader   workspace/transitional-removed-master-csets)

   ;; Pay attention: the following information is not used to develop PC branch versions,
   ;; which is what the baseline-versionref encodes.  Rather, it's used to derive alterations
   ;; to subsystem branches. The following cset additions/deletions are applied to subsystems
   ;; accessible in the PC at the time of a change-add or change-remove.

   ;; ****NOTE**** the information in these slots may have to be adjusted on WS_UPDATE!
   ;; This is not yet implemented.  *FINISH*
   ;; (change_add/remove/close and master_change are all set however...)

   ;; The following lists are are sublists whose car is the subsystem did, and cdrs are cset dids
   ;; added/removed for the indicated subsystem in the current workspace context.

   ;; NOTE that even if there are no added or removed csets in the VPB,
   ;; you can not count on the values of these slots being NIL, since
   ;; there might still be elements which have a SUBSYSTEM DID but no
   ;; CSets.  Use the functions
   ;; WORKSPACE-HAS-ADDED-CLASS-CSET-TUPLES,
   ;; WORKSPACE-HAS-REMOVED-CLASS-CSET-TUPLES or
   ;; WORKSPACE-HAS-VPB-CHANGES to test for this.
   (added-class-cset-tuples :initarg :added-class-cset-tuples
                                :initform nil
                                :reader workspace/added-class-cset-tuples)
   (transitional-added-class-cset-tuples :initarg :transitional-added-class-cset-tuples
                                             :initform nil
                                             :reader workspace/transitional-added-class-cset-tuples)
   (removed-class-cset-tuples :initarg :removed-class-cset-tuples
                                  :initform nil
                                  :reader workspace/removed-class-cset-tuples)
   (transitional-removed-class-cset-tuples :initarg :transitional-removed-class-cset-tuples
                                               :initform nil
                                               :reader workspace/transitional-removed-class-cset-tuples)

   ;; This slot is TRANSIENT.  It is used to accumulate subsystem dids and corresponding satellite cset dids
   ;; for a change_close operation in which we're going to promote this information into the workspace
   ;; virtual private branch (VPB) when we're in the allocation context of the workspace repository.
   ;; It is maintained by code active during satellite transactions, and ultimately its effects
   ;; are promoted to the persistent ADDED-CLASS-CSET-TUPLES list of the workspace object.
   ;; KEY: satellite-repository, value(s): intiially master-subsystem-did, then
   ;; satellite-cset-did is added to the value (so the value has two elements.
   ;; BEWARE of using the satellite-repository object as anything other than an EQ key,
   ;; it won't necessarily be in a valid open repository context.
   (subsystem-cset-VPB-current-dids-alist :initform nil
                                          :transient-only t)

   ;; Usually NIL.  If there is pending port activity for the workspace
   ;; (changes were selected by the user using the web interface but
   ;; were not yet committed to the product) it is represented here as
   ;; a list of tuples of the form (SATELLITE-CSET-DID
   ;; PORT/REJECT/DEFER ADD/DELETE SUBSYSTEM-DID) where
   ;; SATELLITE-CSET-DID is the DID of a satellite change set;
   ;; PORT/REJECT/DEFER is one of the keywords :PORT, :REJECT or
   ;; :DEFER; ADD/DELETE is :ADD if the change is to be added to the
   ;; subsystem or :DELETE if the change is to be dropped from it; and
   ;; SUBSYSTEM-DID is the distributed identifier of the subsystem to
   ;; be alterred.
   (port-activity :initform nil)
   ;; This is a flag that is cleared by operations which alter the
   ;; PORT-ACTIVITY slot and which is set by the "port -act" command.
   ;; It is used to make sure the user's on-disk workspace is
   ;; consistent with their selected port activity before a change is
   ;; committed.
   (port-activity-acted-on-p :initform nil)
   ;; This flag specifies whether or not the workspace is a product reference directory.
   ;; If so, then it is read-only so certain operations are not permitted, e.g. cset_add.
   (product-directory? :initform nil
                       :initarg :product-directory?
                       :reader workspace/product-directory?)

   )
  (:documentation "A persistent representation of a user's disk space which is used to store and
     update versioned content.")
  (:metaclass persistent-standard-class)
  (:schema-version 1))

(defmethod pstore::restore-instance ((class (eql (find-class 'workspace))) (schema (eql 0)) 
                                     persistent-store node-id node-index init-plist)
  (debug-message 2 "Upgrading schema for workspace.")
  ;; This needs work.  The zeros are the OID of NIL.
  (call-next-method class 1 persistent-store node-id node-index
                    (list* :added-master-csets 0
                           :removed-master-csets 0
                           :transitional-added-master-csets 0
                           :transitional-removed-master-csets 0
                           init-plist)))

(defun workspace/scan-file-additions (workspace)
  (declare (optimizable-series-function))
  (scan-persistent-vector (workspace/file-additions workspace)))

(defun workspace/scan-file-deletions (workspace)
  (declare (optimizable-series-function))
  (scan-persistent-vector (workspace/file-deletions workspace)))

(defun workspace/scan-file-changes (workspace)
  (declare (optimizable-series-function))
  (scan-persistent-vector (workspace/file-changes workspace)))

(defun workspace/scan-file-renames (workspace)
  (declare (optimizable-series-function))
  (scan-persistent-vector (workspace/file-renames workspace)))

(defun workspace/begin-change (workspace cset-name cset-description)
  (remake-instance workspace
                   :cset-name cset-name
                   :cset-description cset-description
                   :file-additions (make-instance 'persistent-vector :size 0)
                   :file-deletions (make-instance 'persistent-vector :size 0)
                   :file-changes   (make-instance 'persistent-vector :size 0)
                   :file-renames   (make-instance 'persistent-vector :size 0)
                   ))

(defun workspace/delete-change-context (workspace)
  "Delete the persistent-change-context associated with the workspace.
   Return the workspace.

   It is an error if there is no change-context for the workspace.

   **** WARNING ****
   It is an error to touch or otherwise retain references to the workspace
   persistent change context after calling this function."
  (remake-instance workspace
                   :cset-name nil
                   :cset-description nil
                   :file-additions (make-instance 'persistent-vector :size 0)
                   :file-deletions (make-instance 'persistent-vector :size 0)
                   :file-changes   (make-instance 'persistent-vector :size 0)
                   :file-renames   (make-instance 'persistent-vector :size 0)
                   ))

(defun workspace/clear-added-files (workspace)
  (setf (persistent-vector-length (workspace/file-additions workspace)) 0))

(defun workspace/clear-changed-files (workspace)
  (setf (persistent-vector-length (workspace/file-changes workspace)) 0))

(defun workspace/clear-deleted-files (workspace)
  (setf (persistent-vector-length (workspace/file-deletions workspace)) 0))

(defun workspace/change-add-file (workspace fileadd)
  (persistent-vector-push (workspace/file-additions workspace) fileadd))

(defun workspace/change-modify-file (workspace filechange)
  (persistent-vector-push (workspace/file-changes workspace) filechange))

(defun workspace/change-delete-file (workspace filechange)
  (persistent-vector-push (workspace/file-deletions workspace) filechange))

(defun workspace/in-transition? (workspace)
  "T if the workspace is in transition, NIL otherwise."
  (not (null (workspace/transition-mode workspace))))

(defun workspace/set-transition-finished (workspace)
  "Modifies the workspace to end the transition.  Supposedly, the state on the disk
   now matches the state of the workspace."
  (remake-instance
   workspace
   :transition-mode nil
   :baseline-versionref (workspace/transitional-versionref workspace)
   :transitional-versionref nil
   :added-master-csets (workspace/transitional-added-master-csets workspace)
   :transitional-added-master-csets nil
   :removed-master-csets (workspace/transitional-removed-master-csets workspace)
   :transitional-removed-master-csets nil
   :added-class-cset-tuples (workspace/transitional-added-class-cset-tuples workspace)
   :transitional-added-class-cset-tuples nil
   :removed-class-cset-tuples (workspace/transitional-removed-class-cset-tuples workspace)
   :transitional-removed-class-cset-tuples nil))

(defun workspace/abort-transition (workspace)
  "Modifies the workspace to abort the transition.  Most likely, the state on the disk
   is trashed, but that's life.

   Note one `gotcha'.  If the baseline state is NIL, we can't move backward, so we
   move forward."
  (if (null (workspace/baseline-versionref workspace))
      (workspace/set-transition-finished workspace)
      (remake-instance workspace
                       :transition-mode nil
                       :transitional-versionref nil
                       :transitional-added-master-csets nil
                       :transitional-removed-master-csets nil
                       :transitional-added-class-cset-tuples nil
                       :transitional-removed-class-cset-tuples nil)))

(defun workspace/begin-transition (workspace mode
                                   new-versionref
                                   new-added-master-csets
                                   new-removed-master-csets
                                   new-added-class-cset-tuples
                                   new-removed-class-cset-tuples)
  "Modifies the workspace to begin a transition."
  (debug-message 3 "workspace/begin-transition ~s ~s ~s" workspace mode new-versionref)
  (remake-instance workspace
                   :transition-mode mode
                   :transitional-versionref new-versionref
                   :transitional-added-master-csets new-added-master-csets
                   :transitional-removed-master-csets new-removed-master-csets
                   :transitional-added-class-cset-tuples new-added-class-cset-tuples
                   :transitional-removed-class-cset-tuples new-removed-class-cset-tuples))

(defun workspace/create (workspace-repository workspace-owner-id
                                              workspace-path
                                              workspace-guid
                                              baseline-project-ref baseline-branch-ref baseline-version-ref
                                              baseline-timestamp
                                              description product-directory?)
  "Create and return a workspace object in workspace-repository.

   WORKSPACE-REPOSITORY  is assumed to be open for update and in an active update transaction.

   WORKSPACE-OWNER-ID should be some identifier of the owner of the workspace, typically a string-did
   of a core::core-user subtype.

   WORKSPACE-PATH should be an absolute pathname which is the location of teh workspace.

   BASELINE-* values must be compatible with VERSIONREF-INITIALIZE, and are used to represent
   the state of the branch/version baseline which is represented by the workspace.
   Note that BASELINE-TIMESTAMP must be specified, since it is used to represent the baseline
   version and metaversion reference for the workspace.

   DESCRIPTION may be NIL, or a string which describes the workspace in some way which is
   hopefully meaningful to the user."
  (check-type workspace-path absolute-directory-pathname)
  (check-type workspace-guid guid)
  (check-type description (or null string))
  (check-type baseline-project-ref distributed-identifier)
  (check-type baseline-branch-ref  distributed-identifier)
  (check-type baseline-version-ref distributed-identifier)
  (assert (timestamp? baseline-timestamp))
  ;; Ensure that these versionrefs are consed in the correct repository.
  (with-current-repository (workspace-repository)
    (workspace-repository/register-workspace
     workspace-repository
     (make-instance 'workspace
                    :id (workspace-repository/allocate-id workspace-repository)
                    :guid workspace-guid
                    :owner-id workspace-owner-id
                    :path workspace-path
                    :transition-mode :create
                    :baseline-versionref (make-instance 'versionref
                                                        :project-did baseline-project-ref
                                                        :branch-did  baseline-branch-ref
                                                        :version-did baseline-version-ref
                                                        :timestamp   baseline-timestamp)
                    :transitional-versionref (make-instance 'versionref
                                                            :project-did baseline-project-ref
                                                            :branch-did  baseline-branch-ref
                                                            :version-did baseline-version-ref
                                                            :timestamp   baseline-timestamp)
                    :description description
                    :product-directory? product-directory?))))

(defun workspace/product-did (workspace)
  "Retrieve the did for the product-configuration which is represented by this workspace.
   This information is actually the same as the VersionRef project baseline, but sometimes this is
   easy to forget."
  (versionref/project-did (workspace/baseline-versionref workspace)))

(defun workspace/branch-did (workspace)
  "Retrieve the DID for the product configuration branch represented by this workspace."
  ;; It should always be present.
  (versionref/branch-did (workspace/baseline-versionref workspace)))

(defun workspace/version-did (workspace)
  "Retrieve the DID for the product configuration branch represented by this workspace."
  ;; It should always be present.
  (versionref/version-did (workspace/baseline-versionref workspace)))

(defun workspace/baseline-timestamp (workspace)
  "Return a TIME-STAMP object which describes the repository branch reference point from which the
   virtual private branch (VPB) embodied by this workspace was logically forked.
   This timestamp is used to derive metaversions in both master and satellite repositories."
  (versionref/timestamp (workspace/baseline-versionref workspace)))

(defun workspace/call-with-resolved-versionref (workspace master-repository receiver)
  (funcall receiver
           (repository/resolve-distributed-identifier master-repository (workspace/product-did workspace))
           (repository/resolve-distributed-identifier master-repository (workspace/branch-did workspace))
           (let ((version-spec (workspace/version-did workspace)))
             (if (distributed-identifier? version-spec)
                 (repository/resolve-distributed-identifier master-repository version-spec)
                 version-spec))
           (workspace/baseline-timestamp workspace)))

(defun workspace/transitional-product-did (workspace)
  "Retrieve the did for the product-configuration which is represented by this workspace.
   This information is actually the same as the VersionRef project baseline, but sometimes this is
   easy to forget."
  (versionref/project-did (workspace/transitional-versionref workspace)))

(defun workspace/transitional-branch-did (workspace)
  "Retrieve the DID for the product configuration branch represented by this workspace."
  ;; It should always be present.
  (versionref/branch-did (workspace/transitional-versionref workspace)))

(defun workspace/transitional-version-did (workspace)
  "Retrieve the DID for the version represented by this workspace."
  ;; It should always be present.
  (versionref/version-did (workspace/transitional-versionref workspace)))

(defun workspace/transitional-timestamp (workspace)
  "Return a TIME-STAMP object which describes the repository branch reference point from which the
   virtual private branch (VPB) embodied by this workspace was logically forked.
   This timestamp is used to derive metaversions in both master and satellite repositories."
  (versionref/timestamp (workspace/transitional-versionref workspace)))

(defun workspace/call-with-resolved-transitional-versionref (workspace master-repository receiver)
  (funcall receiver
           (repository/resolve-distributed-identifier master-repository (workspace/transitional-product-did workspace))
           (repository/resolve-distributed-identifier master-repository (workspace/transitional-branch-did workspace))
           (let ((version-spec (workspace/transitional-version-did workspace)))
             (if (distributed-identifier? version-spec)
                 (repository/resolve-distributed-identifier master-repository version-spec)
                 version-spec))
           (workspace/transitional-timestamp workspace)))

(defun workspace/VPB-master-cset-active? (workspace master-cset)
  "This function determines whether or not a master-cset (a.k.a. super-cset) is active
   in the VPB cset additions of the workspace.  Note that a NIL return value does NOT mean
   the cset is inactive in the workspace VPB, since it may be active in the PC branch baseline.
   It only means that it isn't explicitly in the csets-added portion of the VPB.

   Since the workspace, by itself, can't determine the component satellite repository csets which
   make up a master cset, the master cset information is specified in terms of its component
   satellite cset dids.

   If you want to only consider a subsystem-subset of the workspace subsystems which were affected
   by the master cset, prune the list of satellite-cset-dids to be the desired subsystem subset.
   (The FILTER-DID function is useful for this task.)

   Return true if the cset is explicitly activated by the workspace, NIL otherwise.

   PERFORMANCE: this is not an efficient search. Do not call it for large numbers of
   master csets.  If we need to solve the problem of determining for every master cset which
   workspace it resides in, we'll need to build some sort of temporary index. Note that asking
   about master-cset presence in a product configuration isn't quite as bad
   since that's just a 'resolve-did-to-cid and ask if cid is in cid-set' operation.

   It may be the case this routine is only useful to `PC-CHANGE-SET-ACTIVE?'.  Time will tell."

  ;; It would be in good habit to supply subsystem<->satellite-cset associations for all workspace
  ;; VPB predicates.  However in this case it isn't strictly necessary, and the search space is relatively
  ;; small so we can get away with it.  Supplying the association would speed the search because
  ;; we could filter each satellite-cset-did search space by calling
  ;; workspace-existing-added-VPB-cset-dids-for-subsystem

  ;; True if every satellite-cset-did is in the added cset tuples information.

  (and (super-change-set/satellite-change-set-alist master-cset)

       (multiple-value-bind (csf-classes satellite-cset-dids)
           (super-change-set/scan-satellite-change-set-alist master-cset)

         (collect-and
          (map-fn 'boolean (lambda (csf-class satellite-cset-did)
                             (let ((entry (assoc (distributed-object-identifier csf-class)
                                                 (workspace/added-class-cset-tuples workspace))))
                               (debug-message 4 "added-class-cset-tuples entry is ~s" entry)
                               (debug-message 4 "satellite-cset-did is ~s" satellite-cset-did)
                               (member satellite-cset-did (cdr entry))))
                  csf-classes
                  satellite-cset-dids)))))

(defun workspace/VPB-master-cset-inactive? (workspace master-cset)
  "This function determines whether or not a master-cset (a.k.a. super-cset) is active
   in the VPB cset additions of the workspace.  Note that a T return value does NOT mean
   the cset is active in the workspace VPB, since it may not be active in the PC branch baseline.
   It only means that it isn't explicitly in the csets-removed portion of the VPB.

   Since the workspace, by itself, can't determine the component satellite repository csets which
   make up a master cset, the master cset information is specified in terms of its component
   satellite cset dids.

   If you want to only consider a subsystem-subset of the workspace subsystems which were affected
   by the master cset, prune the list of satellite-cset-dids to be the desired subsystem subset.
   (The FILTER-DID function is useful for this task.)

   Return true if the cset is explicitly deactivated by the workspace, NIL otherwise.

   PERFORMANCE: this is not an efficient search. Do not call it for large numbers of
   master csets.  If we need to solve the problem of determining for every master cset which
   workspace it resides in, we'll need to build some sort of temporary index. Note that asking
   about master-cset presence in a product configuration isn't quite as bad
   since that's just a 'resolve-did-to-cid and ask if cid is in cid-set' operation.

   It may be the case this routine is only useful to `PC-CHANGE-SET-ACTIVE?'.  Time will tell."

  ;; It would be in good habit to supply subsystem<->satellite-cset associations for all workspace
  ;; VPB predicates.  However in this case it isn't strictly necessary, and the search space is relatively
  ;; small so we can get away with it.  Supplying the association would speed the search because
  ;; we could filter each satellite-cset-did search space by calling
  ;; workspace-existing-added-VPB-cset-dids-for-subsystem

  ;; True if every satellite-cset-did is in the removed cset tuples information.

  (and (super-change-set/satellite-change-set-alist master-cset)

       (multiple-value-bind (csf-classes satellite-cset-dids)
           (super-change-set/scan-satellite-change-set-alist master-cset)

         (collect-and
          (map-fn 'boolean (lambda (csf-class satellite-cset-did)
                             (let ((entry (assoc (distributed-object-identifier csf-class)
                                                 (workspace/removed-class-cset-tuples workspace))))
                               (debug-message 4 "csf-class is ~s" csf-class)
                               (debug-message 4 "removed-class-cset-tuples entry is ~s" entry)
                               (debug-message 4 "satellite-cset-did is ~s" satellite-cset-did)
                               (member satellite-cset-did (cdr entry))))
                  csf-classes
                  satellite-cset-dids)))))

(defun workspace/VPB-affected-subsystem-dids (workspace)
  (collect-union
   (#M car
       (catenate (scan 'list (workspace/added-class-cset-tuples workspace))
                 (scan 'list (workspace/removed-class-cset-tuples workspace))))))

(defun workspace/note-locked-subsystems (workspace subsystem-dids)
  (remake-instance workspace
                   :locked-subsystem-dids subsystem-dids))

(defun workspace/update-for-master-change (workspace timestamp)
  "A master_change operation is taking place.  Unlike change_close, which simply adds csets
   to the workspace VPB, in this case all VPB elements must be promoted to the master repository
   (product configuration), and the workspace versionref timestamp should be updated.

   TIME-STAMP should be the end time-stamp of the cid which was created in the master repository,
   and which we'll use to update the versionref.

   We assume that master_change processes the workspace added/removed subsystem csets,
   so this function is largely to clear those slots and update the versionref.

   Returns the old time-stamp value, or NIL if there was no old time-stamp value."
  ;; Clear out the change adds/deletes recorded in the workspace, they should be present
  ;; in the master now.
  (remake-instance workspace
                   :baseline-versionref (make-instance 'versionref
                                                        :project-did (workspace/product-did workspace)
                                                        :branch-did  (workspace/branch-did  workspace)
                                                        :version-did (workspace/version-did workspace)
                                                        :timestamp   timestamp)
                   :cset-name nil
                   :cset-description nil
                   :added-master-csets nil
                   :removed-master-csets nil
                   :added-class-cset-tuples nil
                   :removed-class-cset-tuples nil))

#||
(define-tenn-class workspace
    (:documentation
     "A persistent representation of a user's disk space which is used to store and
     update versioned content."
     :astore-persistent t
     :non-persistent-slots (subsystem-cset-VPB-current-dids-alist)
     :no-writer t)                      ; for print-object overload without defining it twice
  (identifier nil)                      ; integer once initialized
  (owner-id nil)                        ; user-specified, typically string-did of core::core-user (or subtype)
  (path nil)                            ; absolute lisp 'pathname' of the workspace
  (persistent-change-context nil)       ; vm::persistent-change-context, if any, associated with this ws.
                                        ; otherwise NIL

  ;; A workspace may be `in transition'.  This means that the state of the workspace on the disk is
  ;; known to be stale.  The tuple of baseline-versionref, added-class-cset-tuples, and
  ;; removed-class-cset-tuples is the `old' state of the workspace, while the tuple of
  ;; transitional-versionref, transitional-added-class-cset-tuples, and
  ;; transitional-removed-class-cset-tuples is the `new' state.
  ;; The command ws_sync attempts to change the workspace from the `old' to the `new' state.
  ;; The ws_sync_commit command replaces the `old' state with the `new' state, and the ws_sync_abort
  ;; command discards the `new' state.
  ;; The transition mode tells ws_sync how to proceed.  It can have the following values:
  ;;
  ;; NIL  the workspace is believed to be up to date.
  ;; :create     the workspace is being created
  ;; :set        the workspace is being set
  ;; :regenerate the workspace is being regenerated
  ;; :update     the workspace is being updated
  ;; :delete     the workspace is being deleted
  ;;   (if there are others, they are pretty obvious)
  (transition-mode nil :type symbol)

  (baseline-versionref nil)             ; vm::versionref identifying state of workspace baseline
                                        ;   (points to a PC branch)
  (transitional-versionref nil)         ; vm::versionref identifying future state of workspace baseline
                                        ;   (points to a PC branch)

  (description nil)                     ;NIL or string description of the workspace
  ;; *** Aren't we risking that the LOCKED-SUBSYSTEMS of a workspace
  ;; can become inconsistent with the LOCK-STATUS of a subsystem.
  ;; This is of particular concern since they are stored in different
  ;; repositories and we don't have cross-repository transaction
  ;; integrity.

  ;; jrm: actually, we will have cross-repository transaction integrity
  ;; if we update the repositories in a single transaction.
  ;; so this will be ok if we are careful.
  (locked-subsystems nil)               ;list of subsystem dids which are locked

  ;; The following information is used to synthesize a virtual user branch.
  ;; We apply these mods to the cid-set which results from using the ancestor BASELINE-VERSIONREF
  ;; to derive an descendant version.

  ;; Pay attention: the following information is not used to develop PC branch versions,
  ;; which is what the baseline-versionref encodes.  Rather, it's used to derive alterations
  ;; to subsystem branches. The following cset additions/deletions are applied to subsystems
  ;; accessible in the PC at the time of a change-add or change-remove.

  ;; ****NOTE**** the information in these slots may have to be adjusted on WS_UPDATE!
  ;; This is not yet implemented.  *FINISH*
  ;; (change_add/remove/close and master_change are all set however...)

  ;; The following lists are are sublists whose car is the subsystem did, and cdrs are cset dids
  ;; added/removed for the indicated subsystem in the current workspace context.

  ;; NOTE that even if there are no added or removed csets in the VPB,
  ;; you can not count on the values of these slots being NIL, since
  ;; there might still be elements which have a SUBSYSTEM DID but no
  ;; CSets.  Use the functions
  ;; WORKSPACE-HAS-ADDED-CLASS-CSET-TUPLES,
  ;; WORKSPACE-HAS-REMOVED-CLASS-CSET-TUPLES or
  ;; WORKSPACE-HAS-VPB-CHANGES to test for this.
  (added-class-cset-tuples nil)
  (transitional-added-class-cset-tuples nil)
  (removed-class-cset-tuples nil)
  (transitional-removed-class-cset-tuples nil)

  ;; This slot is TRANSIENT.  It is used to accumulate subsystem dids and corresponding satellite cset dids
  ;; for a change_close operation in which we're going to promote this information into the workspace
  ;; virtual private branch (VPB) when we're in the allocation context of the workspace repository.
  ;; It is maintained by code active during satellite transactions, and ultimately its effects
  ;; are promoted to the persistent ADDED-CLASS-CSET-TUPLES list of the workspace object.
  ;; KEY: satellite-repository, value(s): intiially master-subsystem-did, then
  ;; satellite-cset-did is added to the value (so the value has two elements.
  ;; BEWARE of using the satellite-repository object as anything other than an EQ key,
  ;; it won't necessarily be in a valid open repository context.
  (subsystem-cset-VPB-current-dids-alist nil) ;NOT PERSISTENT

  ;; Usually NIL.  If there is pending port activity for the workspace
  ;; (changes were selected by the user using the web interface but
  ;; were not yet committed to the product) it is represented here as
  ;; a list of tuples of the form (SATELLITE-CSET-DID
  ;; PORT/REJECT/DEFER ADD/DELETE SUBSYSTEM-DID) where
  ;; SATELLITE-CSET-DID is the DID of a satellite change set;
  ;; PORT/REJECT/DEFER is one of the keywords :PORT, :REJECT or
  ;; :DEFER; ADD/DELETE is :ADD if the change is to be added to the
  ;; subsystem or :DELETE if the change is to be dropped from it; and
  ;; SUBSYSTEM-DID is the distributed identifier of the subsystem to
  ;; be alterred.
  (port-activity nil)
  ;; This is a flag that is cleared by operations which alter the
  ;; PORT-ACTIVITY slot and which is set by the "port -act" command.
  ;; It is used to make sure the user's on-disk workspace is
  ;; consistent with their selected port activity before a change is
  ;; committed.
  (port-activity-acted-on-p nil)
  ;; This flag specifies whether or not the workspace is a product reference directory.
  ;; If so, then it is read-only so certain operations are not permitted, e.g. cset_add.
  (product-directory-p nil)
  )

(define-tenn-class workspace-allocator
    (:documentation
     "An object which manages workspace identifier allocation and extent access in a workspace
     repository.  There is only one workspace allocator defined in a workspace repository."
     :astore-persistent t)
  (instance-count 0 :type integer)      ;number of workspaces allocated, +1 is always next key
  (instance-hashtable (repository-hash-table-create :size 1024)))

(defun workspace-in-transition? (workspace)
  "T if the workspace is in transition, NIL otherwise."
  (not (null (workspace-transition-mode workspace))))

(defun workspace-set-transition-finished (workspace)
  "Modifies the workspace to end the transition.  Supposedly, the state on the disk
   now matches the state of the workspace."
  (let (old-versionref)
    (shiftf old-versionref
            (workspace-baseline-versionref workspace)
            (workspace-transitional-versionref workspace)
            nil)
    (shiftf (workspace-added-class-cset-tuples workspace)
            (workspace-transitional-added-class-cset-tuples workspace)
            nil)
    (shiftf (workspace-removed-class-cset-tuples workspace)
            (workspace-transitional-removed-class-cset-tuples workspace)
            nil)
    (setf (workspace-transition-mode workspace) nil)
    ;; clean up the versionref.
    (when old-versionref
      (delete-object old-versionref))))

(defun workspace-abort-transition (workspace)
  "Modifies the workspace to abort the transition.  Most likely, the state on the disk
   is trashed, but that's life.

   Note one `gotcha'.  If the baseline state is NIL, we can't move backward, so we
   move forward."
  (if (null (workspace-baseline-versionref workspace))
      (workspace-set-transition-finished workspace)
    (let ((old-versionref (workspace-transitional-versionref workspace)))
      (setf (workspace-transitional-versionref workspace)                    nil
            (workspace-transitional-added-class-cset-tuples workspace)   nil
            (workspace-transitional-removed-class-cset-tuples workspace) nil
            (workspace-transition-mode workspace)                            nil)
      (when old-versionref
        (delete-object old-versionref)))))

(defun workspace-begin-transition (workspace mode
                                   new-versionref new-added-class-cset-tuples new-removed-class-cset-tuples)
  "Modifies the workspace to begin a transition."
  (let ((old-versionref (workspace-transitional-versionref workspace)))
    (setf (workspace-transition-mode workspace)                            mode
          (workspace-transitional-versionref workspace)                    new-versionref
          (workspace-transitional-added-class-cset-tuples workspace)   new-added-class-cset-tuples
          (workspace-transitional-removed-class-cset-tuples workspace) new-removed-class-cset-tuples)
    (when old-versionref
      (delete-object old-versionref))))

(defun workspace-create (workspace-repository workspace-owner-id
                         workspace-path
                         baseline-project-ref baseline-branch-ref baseline-version-ref
                         baseline-timestamp
                         description product-directory?)
  "Create and return a workspace object in workspace-repository.

   WORKSPACE-REPOSITORY  is assumed to be open for update and in an active update transaction.

   WORKSPACE-OWNER-ID should be some identifier of the owner of the workspace, typically a string-did
   of a core::core-user subtype.

   WORKSPACE-PATH should be an absolute pathname which is the location of teh workspace.

   BASELINE-* values must be compatible with VERSIONREF-INITIALIZE, and are used to represent
   the state of the branch/version baseline which is represented by the workspace.
   Note that BASELINE-TIMESTAMP must be specified, since it is used to represent the baseline
   version and metaversion reference for the workspace.

   DESCRIPTION may be NIL, or a string which describes the workspace in some way which is
   hopefully meaningful to the user."
  (check-type workspace-path pathname)
  (check-type description (or null string))
  (guarantee-absolute-directory-pathname workspace-path)
  (check-type baseline-project-ref distributed-identifier)
  (check-type baseline-branch-ref distributed-identifier)
  (check-type baseline-version-ref distributed-identifier)
  (assert (time-stamp? baseline-timestamp))
  ;; Ensure that these versionrefs are consed in the correct repository.
  (with-current-repository (workspace-repository)
    (let* ((start-versionref (versionref-create
                              baseline-project-ref baseline-branch-ref baseline-version-ref
                              baseline-timestamp))
           ;; must create two because commit deletes one.
           (base-versionref (versionref-create
                             baseline-project-ref baseline-branch-ref baseline-version-ref
                             baseline-timestamp))
           (workspace
            (make-instance 'workspace
              :workspace-owner-id workspace-owner-id
              :workspace-path workspace-path
              :workspace-baseline-versionref start-versionref
              :workspace-description description
              :workspace-product-directory-p product-directory?
              )))
      (workspace-repository-allocate-id workspace-repository workspace) ;zaps workspace-identifier slot
      (workspace-begin-transition workspace :create base-versionref nil nil)
      workspace)))

(defun workspace-modify-for-ws-set (workspace-repository
                                    workspace
                                    baseline-project-ref baseline-branch-ref baseline-version-ref
                                    baseline-timestamp
                                    description force-p)
  "This is called by CMCTL-WS-SET/CREATE to make the necessary changes to
   the WORKSPACE object to perform a ws_set operation.
   The modified WORKSPACE object is returned."

  ;; Abort if intermediate states exist
  (unless force-p
    (when (or (workspace-has-port-activity-p workspace)
              (workspace-has-vpb-changes workspace))
      (conman-signal-error
       *cm-returns-error-port-activity-in-progress*
       "Your workspace has cset additions and/or removals. Please use ~a if you wish to lose these changes and establish a new workspace configuration."
       *cm-cli-switch-force-syntax*)))

  ;; CMCTL-WS-SET/CREATE was calling WORKSPACE-CREATE even when doing
  ;; a ws_set operation.  This would result in orphaned WORKSPACE
  ;; objects in the workspace repository.
  ;; Here we reinitialize the slots that would be initialized to NIL
  ;; if a new WORKSPACE object were created instead of reusing the
  ;; existing one.  I have no idea of resetting these is the right
  ;; behavior, but it is at least consistent with what the results
  ;; would be if WORKSPACE-CREATE were called as it was before.
  (setf (workspace-added-class-cset-tuples   workspace) nil)
  (setf (workspace-removed-class-cset-tuples workspace) nil)
  (setf (workspace-transitional-added-class-cset-tuples   workspace) nil)
  (setf (workspace-transitional-removed-class-cset-tuples workspace) nil)
  (workspace-delete-change-context workspace)
  ;; Caller should have unlocked everything!
  (assert (null (workspace-locked-subsystems                      workspace)))
  (setf (workspace-subsystem-cset-VPB-current-dids-alist  workspace) nil)

  ;; Now for the slots that would be initialized by WORKSPACE-CREATE:
  ;; USER-NAME and DIRECTORY-SPEC are presumably unchanged by ws_set.
  (let ((old-base-versionref (workspace-baseline-versionref workspace))
        (old-trans-versionref (workspace-transitional-versionref workspace)))
    (when old-base-versionref
      (delete-object old-base-versionref))
    (when old-trans-versionref
      (delete-object old-trans-versionref))
    ;; Ensure these versionrefs are consed in the correct repository.
    (with-current-repository (workspace-repository)
      (let ((new-base-versionref
             (versionref-create
              baseline-project-ref baseline-branch-ref baseline-version-ref
              baseline-timestamp))
            (new-trans-versionref
             (versionref-create
              baseline-project-ref baseline-branch-ref baseline-version-ref
              baseline-timestamp)))
        (setf (workspace-baseline-versionref workspace) new-base-versionref)
        (setf (workspace-description workspace) description)
        (workspace-begin-transition workspace :set new-trans-versionref nil nil)
        workspace))))

(defun workspace-modify-for-ws-move (workspace workspace-owner-id
                                     workspace-path)
  "Modify an existing workspace object in workspace-repository (and return it).

   WORKSPACE-REPOSITORY  is assumed to be open for update and in an active update transaction.

   WORKSPACE-OWNER-ID should be some identifier of the owner of the workspace, typically a string-did
   of a core::core-user subtype.

   WORKSPACE-PATH should be an absolute pathname which is the location of teh workspace."

  (check-type workspace-path pathname)
  (guarantee-absolute-directory-pathname workspace-path)
  (setf (workspace-owner-id workspace) workspace-owner-id)
  (setf (workspace-path workspace) workspace-path)

  workspace)

(defmethod print-object ((workspace workspace) stream)
  ;; Note the use of :NO-READER and :NO-WRITER on class definition to avoid a ACL compilation warning
  ;; about defining PRINT-OBJECT twice in this module.
  (print-unreadable-object (workspace stream :type t) ; print class name, but not address id
    (format stream "Id: ~s, Owner: ~s"
            (workspace-identifier workspace)
            (if *within-regression-tests*
                "(SUPPRESSED)"
              (workspace-owner-id workspace)))))

(defun workspace-identifier? (thing)
  "Return true if THING resembles a unique workspace identifier,
   and NIL if it does not."
  (integerp thing))

(defun workspace-pc-did (workspace)
  "Retrieve the did for the product-configuration which is represented by this workspace.
   This information is actually the same as the VersionRef project baseline, but sometimes this is
   easy to forget."
  (assert-error (versionref-project-did (workspace-baseline-versionref workspace))))

(defun workspace-transitional-pc-did (workspace)
  "Retrieve the did for the product-configuration which is represented by this workspace.
   This information is actually the same as the VersionRef project baseline, but sometimes this is
   easy to forget."
  (assert-error (versionref-project-did (workspace-transitional-versionref workspace))))

(defun workspace-pc-branch-did (workspace)
  "Retrieve the DID for the product configuration branch represented by this workspace."
  ;; It should always be present.
  (assert-error (versionref-branch-did (workspace-baseline-versionref workspace))))

(defun workspace-transitional-pc-branch-did (workspace)
  "Retrieve the DID for the product configuration branch represented by this workspace."
  ;; It should always be present.
  (assert-error (versionref-branch-did (workspace-transitional-versionref workspace))))

(defun workspace-baseline-timestamp (workspace)
  "Return a TIME-STAMP object which describes the repository branch reference point from which the
   virtual private branch (VPB) embodied by this workspace was logically forked.
   This timestamp is used to derive metaversions in both master and satellite repositories."
  (versionref-timestamp (workspace-baseline-versionref workspace)))

(defun workspace-transitional-timestamp (workspace)
  "Return a TIME-STAMP object which describes the repository branch reference point from which the
   virtual private branch (VPB) embodied by this workspace was logically forked.
   This timestamp is used to derive metaversions in both master and satellite repositories."
  (versionref-timestamp (workspace-transitional-versionref workspace)))

(defun workspace-repository-allocate-id (workspace-repository workspace)
  "Allocate a unique identifier in workspace-repository for the workspace object,
   and assign the workspace object to the identifier, and vice versa.

   WORKSPACE-REPOSITORY is assumed to be open for update and in an update transaction.

   Return the unique identifier which was allocated.

   Queries on the returned identifier may be performed with workspace-lookup."
  (let ((workspace-allocator (repository-get-locally-named-root workspace-repository
                                                                +workspace-allocator-root-key+))
        (identifier nil))
    (unless workspace-allocator
      (error "Workspace-allocator is not present in workspace-repository ~s, ~
             which probably means WORKSPACE-REPOSITORY-CREATE wasn't called to create the repository."
             (repository-name workspace-repository)))
    (setq identifier (incf (workspace-allocator-instance-count workspace-allocator)))
    (setf (workspace-identifier workspace) identifier)
    (repository-hash-table-puthash (workspace-allocator-instance-hashtable workspace-allocator)
                                   identifier workspace)
    identifier))

(defun map-over-workspaces (function workspace-repository)
  "Apply FUNCTION to each WORKSPACE of WORKSPACE-REPOSITORY, which is an open repository."
  (let ((workspace-allocator (assert-error
                                 (repository-get-locally-named-root workspace-repository
                                                                    +workspace-allocator-root-key+))))
    (repository-hash-table-maphash
        (workspace-allocator-instance-hashtable workspace-allocator)
      (lambda (key workspace)
        (assert (eql key (workspace-identifier workspace))) ; Sanity check.
        (funcall function workspace)))))

(defun workspace-lookup (workspace-repository workspace-identifier &key (error-if-missing t))
  "Attempt to find a WORKSPACE object which is identified by WORKSPACE-IDENTIFIER (as obtained
   by invoking the workspace-identifier accessor on a workspace object).

   WORKSPACE-REPOSITORY must be open, and in a transaction, for either read or update modes.

   If the identifier names a workspace which is currently active in the repository,
   return the workspace.

   If the identifier does NOT name a workspace which is currently active, we handle the
   situation differently depending on the value of ERROR-IF-MISSING.

   If ERROR-IF-MISSING is T and the workspace search fails, signal an error.

   If ERROR-IF-MISSING is nil, and the workspace search fails, we return one of two
   results:
   a) NIL if the identifier names a workspace which once existed but no longer exists.
   b) :ERROR if the identifier names a workspace which never existed."
  (check-type workspace-identifier integer)
  (let* ((workspace-allocator (assert-error
                               (repository-get-locally-named-root workspace-repository
                                                                  +workspace-allocator-root-key+)))
         (instance-count (workspace-allocator-instance-count workspace-allocator)))
    ;; Check for invalid identifiers
    (unless (and (<= workspace-identifier instance-count)
                 (> workspace-identifier ))
      (if error-if-missing
          (error "Workspace identifier '~s' is not a valid identifier in workspace repository ~s"
                 workspace-identifier (repository-name workspace-repository))
        (return-from workspace-lookup :error)))
    ;;It's either valid and active, or inactive
    (or (repository-hash-table-gethash (workspace-allocator-instance-hashtable workspace-allocator)
                                       workspace-identifier)
        (if error-if-missing
            (error "Workspace identifier '~s' names an deleted workspace." workspace-identifier)
          nil))))

(defun workspace-resolve-spec (cm-session-context workspace-repository
                               &key ws-id ws-path user-name
                                    (error-if-missing t))

  "This routine is similar to WORKSPACE-LOOKUP in that it is attempting to find a workspace and will
   signal an error if the workspace cannot be found.  Unlike that routine however, this one takes a
   broader class of specification for the workspace key.

   cm-session-context is used to determine whether to do a case-sensitive string compare (UNIX) or
   a case-insensitive compare (NT).  If only the key :ws-id is used, this are may be nil.

   The workspace identification information must take one of the following forms:
   1) Specify the unique workspace identifier via WS-ID, which must be an integer.
   2) Specify the workspace pathname and workspace username (strings both), which
      must match corresponding information in the workspace extent.

   Note that the latter technique is a currently a slow linear search of persistent data!

   Return the workspace if found, or return values compatible with workspace-lookup based
   on like processing of the ERROR-IF-MISSING value."
  (cond (ws-id (workspace-lookup workspace-repository ws-id :error-if-missing error-if-missing))
        ((and ws-path user-name)
         ;; *PERFORMANCE* *FINISH*: maintain a hashtable with workspaces keyed by user and/or path.
         (let ((workspace-allocator (assert-error
                                        (repository-get-locally-named-root workspace-repository
                                                                           +workspace-allocator-root-key+))))
           (repository-hash-table-maphash
               (workspace-allocator-instance-hashtable workspace-allocator)
             (lambda (key-ws-id value-workspace)
               (assert (eql key-ws-id (workspace-identifier value-workspace))) ;sanity check
               (when (and (funcall (platform-filename-equal
                                    (cm-session-context-client-platform cm-session-context))
                                   (workspace-owner-id value-workspace) user-name)
                          (funcall (platform-filename-equal
                                    (cm-session-context-client-platform cm-session-context))
                                   (namestring (workspace-path value-workspace)) ws-path))
                 (return-from workspace-resolve-spec value-workspace)))))
         ;; We didn't find it with ws-path and user-name specs
         (if error-if-missing
             (error "A workspace keyed by client path ~s and user name ~s was not found."
                    ws-path user-name)
           nil))
        (t (error "WS-ID, or the combination of WS-PATH and USER-NAME must be specified in call to ~
                  WORKSPACE-RESOLVE-SPEC"))))

;;; Shouldn't there be functions of adding and removing subsystems from this list?
(defun workspace-note-locked-subsystems (workspace locked-subsystem-dids)
  "Record in the workspace a list of those subsystem DIDS which the workspace currently has locked
   via master_lock.

   Return the old value of the locked subsystems slot (which is a list of DIDs)"
  (prog1 (workspace-locked-subsystems workspace)
    (setf (workspace-locked-subsystems workspace) locked-subsystem-dids)))

(defun workspace-note-unlocked-subsystem (workspace subsystem-to-unlock-did)
  "SUBSYSTEM-TO-UNLOCK-DID is the DID of a SUBSYSTEM which is in the
   process of being unlocked.  Causes the WORKSPACE to realize it no longer
   has the subsystem locked."
  (setf (workspace-locked-subsystems workspace)
    (delete subsystem-to-unlock-did (workspace-locked-subsystems workspace))))

(defun error-if-product-reference-workspace (workspace attempted-operation)
  (when (workspace-product-directory-p workspace)
    (conman-signal-error *cm-returns-error-reference-workspace-change-not-permitted*
      "The operation ~A is not permitted on a product reference workspace." attempted-operation)))

(defgeneric workspace-delete (workspace-repository workspace)
  (:documentation
   "Delete workspace from a workspace repository.  Workspace-repository must be open for
    update, and in the middle of an update transaction.
    WORKSPACE may be either a workspace object or a workspace identifier.
    Returns nil")
  (:method (workspace-repository (workspace workspace))
    (workspace-delete workspace-repository (workspace-identifier workspace)))
  (:method (workspace-repository (workspace-identifier integer))
    (let ((workspace-allocator (repository-get-locally-named-root workspace-repository
                                                                  +workspace-allocator-root-key+))
          (workspace-object nil))
      (unless workspace-allocator
        (error "Workspace ~s is not present in the workspace repository ~s."
               workspace-identifier (repository-name workspace-repository)))
      (unless (setq workspace-object
                (repository-hash-table-gethash
                 (workspace-allocator-instance-hashtable workspace-allocator)
                 workspace-identifier))
        (error "Workspace ~s was already deleted from workspace repository ~s."
               workspace-identifier (repository-name workspace-repository)))
      ;; Remove from the hash table, and perform explicit
      ;; delete-object on the object.
      (repository-hash-table-remhash (workspace-allocator-instance-hashtable workspace-allocator)
                                     workspace-identifier)
      (let ((old-baseline-versionref (workspace-baseline-versionref workspace-object))
            (old-transitional-versionref (workspace-transitional-versionref workspace-object)))
        (when old-baseline-versionref
          (delete-object old-baseline-versionref)
          (setf (workspace-baseline-versionref workspace-object) nil))
        (when old-transitional-versionref
          (delete-object old-transitional-versionref)
          (setf (workspace-transitional-versionref workspace-object) nil))
        (delete-object workspace-object))
      nil)))

(defun workspace-get-change-context (workspace)
  "Return a transient change-context object for the workspace's persistent change context,
   or NIL if there is no change-context associated with the workspace."
  (let ((persistent-change-context (workspace-persistent-change-context workspace)))
    (and persistent-change-context
         (conman-change-context-create-restore persistent-change-context))))

(defun workspace-add-change-context (workspace persistent-change-context)
  "Add a persistent-change-context object to this workspace.
   Signal an error if this workspace already contains a change-context.
   Return the workspace."
  (when (workspace-persistent-change-context workspace)
    (error "Workspace ~s already has a persistent change-context." workspace))
  (when persistent-change-context
    (setf (workspace-persistent-change-context workspace) persistent-change-context)))

(defun workspace-delete-change-context (workspace)
  "Delete the persistent-change-context associated with the workspace.
   Return the workspace.

   It is an error if there is no change-context for the workspace.

   **** WARNING ****
   It is an error to touch or otherwise retain references to the workspace
   persistent change context after calling this function."

  (when (workspace-persistent-change-context workspace)
    (delete-object (workspace-persistent-change-context workspace) :recursive t)
    (setf (workspace-persistent-change-context workspace) nil))
  workspace)


(defmacro with-open-workspace-repository ((repository-var repository-name)
                                          &body body)
  "Open a semi-persistent workspace repository named REPOSITORY-NAME (which must be
   compatible with REPOSITORY-OPEN), and must be of repository type *REPOSITORY-BASIC-FILE-TYPE*.
   The repository must exist, and must have been created with workspace-repository-create

   Execute BODY as an implicit PROGN in the bound scope of the repository, with the indicated
   repository as the 'current' repository for purposes of object allocation."
  `(WITH-OPEN-REPOSITORY (,repository-var ,repository-name
                          (CONMAN-REPOSITORY-OPEN-MODE-FROM-OPERATING-MODE)
                          :ERROR-IF-NOT-CACHED t
                          :REPOSITORY-TYPE :BASIC
                          :IF-DOES-NOT-EXIST :ERROR
                          :IF-EXISTS :OPEN)
     (WHEN (and *CURRENT-WORKSPACE-REPOSITORY*
                (not (eq *CURRENT-WORKSPACE-REPOSITORY*
                         ,repository-var)))
           (ERROR "Attempt to open multiple workspace repositories."))
     (LET ((*CURRENT-WORKSPACE-REPOSITORY* ,repository-var))
          ,@body)))

(defmacro with-workspace-repository-txn ((workspace-repository txn-mode reason) &body body)
  "Wrap a transaction around BODY, which is executed as an implicit PROGN.
   TXN-MODE must be :READ-WRITE or :READ-ONLY.
   This macro ensures that there is no attempt at creating change-sets in workspace-repository.
   Do not attempt to allocate versioned objects in BODY."
  (with-macro-variables (unused-version-context)
    `(with-repository-txn (,unused-version-context ,workspace-repository nil :txn-mode ,txn-mode
                           :reason ,reason :suppress-change-transaction t)
       (declare (ignore ,unused-version-context))
       ,@body)))

(defun workspace-promote-current-vpb-change (workspace)
  "Promote the current satellite change-sets which were created, but not promoted into the master,
   via a change_close operation.  They become part of the workspace-added-class-cset-tuples
   information which comprises part of the virtual private branch (VPB) of this workspace.

   This function returns NIL."
  ;; If there aren't any, we shouldn't have been called...  Something went wrong across change-close ops.
  ;; We're assuming that re-resolving an Astore object will cause the transient slot value to be
  ;; NIL (its indicated default).
  (assert (workspace-subsystem-cset-VPB-current-dids-alist workspace))
  (loop for acons in (workspace-subsystem-cset-VPB-current-dids-alist workspace)
      as values = (cdr acons)
      as master-subsystem-did = (first values)
      as satellite-cset-did = (second values)
      do (workspace-add-subsystem-change-sets workspace master-subsystem-did (list satellite-cset-did)))
  ;; You could imagine that we should set the transient slot to nil here
  ;; (subsystem-cset-VPB-current-dids-alist), but we don't, to test the assumption that astore
  ;; initializes transient slot values across re-instantiation in db transactions to the value specified
  ;; in the define-tenn-class form.
  )

(defun workspace-update-for-master-change (workspace time-stamp)
  "A master_change operation is taking place.  Unlike change_close, which simply adds csets
   to the workspace VPB, in this case all VPB elements must be promoted to the master repository
   (product configuration), and the workspace versionref timestamp should be updated.

   TIME-STAMP should be the end time-stamp of the cid which was created in the master repository,
   and which we'll use to update the versionref.

   We assume that master_change processes the workspace added/removed subsystem csets,
   so this function is largely to clear those slots and update the versionref.

   Returns the old time-stamp value, or NIL if there was no old time-stamp value."
  ;; Clear out the change adds/deletes recorded in the workspace, they should be present
  ;; in the master now.
  (when (workspace-added-class-cset-tuples workspace)
    (setf (workspace-added-class-cset-tuples workspace) nil))
  (when (workspace-removed-class-cset-tuples workspace)
    (setf (workspace-removed-class-cset-tuples workspace) nil))

  ;; Update the baseline timestamp
  (versionref-update-timestamp (workspace-baseline-versionref workspace) time-stamp))

(defun workspace-existing-added-VPB-cset-dids-for-subsystem (workspace subsystem-did)
  "Return a list of satellite change-set dids which were added to the indicated subsystem
   as part of the workspace VPB.  Note that this list will not include VPB csets
   which are in the process of being promoted into the subsystem because they're the cset in progress
   in a satellite."
  (cdr (assoc subsystem-did (workspace-added-class-cset-tuples workspace))))

(defun workspace-transitional-added-VPB-cset-dids-for-subsystem (workspace subsystem-did)
  "Return a list of satellite change-set dids which were added to the indicated subsystem
   as part of the workspace VPB.  Note that this list will not include VPB csets
   which are in the process of being promoted into the subsystem because they're the cset in progress
   in a satellite."
  (cdr (assoc subsystem-did (workspace-transitional-added-class-cset-tuples workspace))))

(defun workspace-existing-removed-VPB-cset-dids-for-subsystem (workspace subsystem-did)
  "Return a list of satellite change-set dids which were removed from the indicated subsystem
   as part of the workspace VPB."
  (cdr (assoc subsystem-did (workspace-removed-class-cset-tuples workspace))))

(defun workspace-transitional-removed-VPB-cset-dids-for-subsystem (workspace subsystem-did)
  "Return a list of satellite change-set dids which were removed from the indicated subsystem
   as part of the workspace VPB."
  (cdr (assoc subsystem-did (workspace-transitional-removed-class-cset-tuples workspace))))

(defun workspace-VPB-cset-components-active? (workspace master-cset-component-satellite-cset-dids)
  "This function determines whether or not a master-cset (a.k.a. super-cset) is active
   in the VPB cset additions of the workspace.  Note that a NIL return value does NOT mean
   the cset is inactive in the workspace VPB, since it may be active in the PC branch baseline.
   It only means that it isn't explicitly in the csets-added portion of the VPB.

   Since the workspace, by itself, can't determine the component satellite repository csets which
   make up a master cset, the master cset information is specified in terms of its component
   satellite cset dids.

   If you want to only consider a subsystem-subset of the workspace subsystems which were affected
   by the master cset, prune the list of satellite-cset-dids to be the desired subsystem subset.
   (The FILTER-DID function is useful for this task.)

   Return true if the cset is explicitly activated by the workspace, NIL otherwise.

   PERFORMANCE: this is not an efficient search. Do not call it for large numbers of
   master csets.  If we need to solve the problem of determining for every master cset which
   workspace it resides in, we'll need to build some sort of temporary index. Note that asking
   about master-cset presence in a product configuration isn't quite as bad
   since that's just a 'resolve-did-to-cid and ask if cid is in cid-set' operation.

   It may be the case this routine is only useful to `PC-CHANGE-SET-ACTIVE?'.  Time will tell."

  ;; It would be in good habit to supply subsystem<->satellite-cset associations for all workspace
  ;; VPB predicates.  However in this case it isn't strictly necessary, and the search space is relatively
  ;; small so we can get away with it.  Supplying the association would speed the search because
  ;; we could filter each satellite-cset-did search space by calling
  ;; workspace-existing-added-VPB-cset-dids-for-subsystem

  ;; True if every satellite-cset-did is in the added cset tuples information.

  (every (lambda (satellite-cset-did)
             (loop for tuple in (workspace-added-class-cset-tuples workspace)
                 as subsystem-satellite-cset-dids = (cdr tuple)
                 when (find satellite-cset-did subsystem-satellite-cset-dids)
                 do (return t)))        ;else loop returns nil
         master-cset-component-satellite-cset-dids)
  )

(defun workspace-transitional-VPB-cset-components-active? (workspace master-cset-component-satellite-cset-dids)
  "This function determines whether or not a master-cset (a.k.a. super-cset) is active
   in the VPB cset additions of the workspace.  Note that a NIL return value does NOT mean
   the cset is inactive in the workspace VPB, since it may be active in the PC branch baseline.
   It only means that it isn't explicitly in the csets-added portion of the VPB.

   Since the workspace, by itself, can't determine the component satellite repository csets which
   make up a master cset, the master cset information is specified in terms of its component
   satellite cset dids.

   If you want to only consider a subsystem-subset of the workspace subsystems which were affected
   by the master cset, prune the list of satellite-cset-dids to be the desired subsystem subset.
   (The FILTER-DID function is useful for this task.)

   Return true if the cset is explicitly activated by the workspace, NIL otherwise.

   PERFORMANCE: this is not an efficient search. Do not call it for large numbers of
   master csets.  If we need to solve the problem of determining for every master cset which
   workspace it resides in, we'll need to build some sort of temporary index. Note that asking
   about master-cset presence in a product configuration isn't quite as bad
   since that's just a 'resolve-did-to-cid and ask if cid is in cid-set' operation.

   It may be the case this routine is only useful to `PC-CHANGE-SET-ACTIVE?'.  Time will tell."

  ;; It would be in good habit to supply subsystem<->satellite-cset associations for all workspace
  ;; VPB predicates.  However in this case it isn't strictly necessary, and the search space is relatively
  ;; small so we can get away with it.  Supplying the association would speed the search because
  ;; we could filter each satellite-cset-did search space by calling
  ;; workspace-existing-added-VPB-cset-dids-for-subsystem

  ;; True if every satellite-cset-did is in the added cset tuples information.

  (every (lambda (satellite-cset-did)
             (loop for tuple in (workspace-transitional-added-class-cset-tuples workspace)
                 as subsystem-satellite-cset-dids = (cdr tuple)
                 when (find satellite-cset-did subsystem-satellite-cset-dids)
                 do (return t)))        ;else loop returns nil
         master-cset-component-satellite-cset-dids)
  )

(defun workspace-VPB-cset-components-inactive? (workspace master-cset-component-satellite-cset-dids)
  "This function determines whether or not a master-cset (a.k.a. super-cset) is explicitly
   deactivated by the VPB cset removals in the workspace. Note that a NIL return value does NOT mean
   the cset is inactive in the workspace VPB, since it may be active in the PC branch baseline.
   It only means that it isn't explicitly in the csets-removed portion of the VPB.

   Since the workspace, by itself, can't determine the component satellite repository csets which
   make up a master cset, the master cset information is specified in terms of its component
   satellite cset dids.

   If you want to only consider a subsystem-subset of the workspace subsystems which were affected
   by the master cset, prune the list of satellite-cset-dids to be the desired subsystem subset.
   (The FILTER-DID function is useful for this task.)

   Return true if the cset is explicitly deactivated by the workspace, NIL otherwise.

   PERFORMANCE: This routine suffers the same problems as workspace-VPB-cset-components-active?,
   see that routine for performance comments."

  ;; True if every satellite-cset-did is in the removed cset tuples information.
  (every (lambda (satellite-cset-did)
             (loop for tuple in (workspace-removed-class-cset-tuples workspace)
                 as subsystem-satellite-cset-dids = (cdr tuple)
                 when (find satellite-cset-did subsystem-satellite-cset-dids)
                 do (return t)))        ;else loop returns nil
         master-cset-component-satellite-cset-dids))

(defun workspace-transitional-VPB-cset-components-inactive? (workspace master-cset-component-satellite-cset-dids)
  "This function determines whether or not a master-cset (a.k.a. super-cset) is explicitly
   deactivated by the VPB cset removals in the workspace. Note that a NIL return value does NOT mean
   the cset is inactive in the workspace VPB, since it may be active in the PC branch baseline.
   It only means that it isn't explicitly in the csets-removed portion of the VPB.

   Since the workspace, by itself, can't determine the component satellite repository csets which
   make up a master cset, the master cset information is specified in terms of its component
   satellite cset dids.

   If you want to only consider a subsystem-subset of the workspace subsystems which were affected
   by the master cset, prune the list of satellite-cset-dids to be the desired subsystem subset.
   (The FILTER-DID function is useful for this task.)

   Return true if the cset is explicitly deactivated by the workspace, NIL otherwise.

   PERFORMANCE: This routine suffers the same problems as workspace-VPB-cset-components-active?,
   see that routine for performance comments."

  ;; True if every satellite-cset-did is in the removed cset tuples information.
  (every (lambda (satellite-cset-did)
             (loop for tuple in (workspace-transitional-removed-class-cset-tuples workspace)
                 as subsystem-satellite-cset-dids = (cdr tuple)
                 when (find satellite-cset-did subsystem-satellite-cset-dids)
                 do (return t)))        ;else loop returns nil
         master-cset-component-satellite-cset-dids))

(defun workspace-remove-subsystem-change-sets (workspace subsystem-did satellite-dids
                                               &key (errorp t))
  "Ensure that satellite-dids are not active in the workspace VPB information, and if already
   inactive, that the specified dids are explicitly deactivated by the VPB.

   **** NOTE **** this does NOT ensure that the satellite dids are inactive in the PC branch context!
   (Workspaces necessarily can't know about that, the caller must however.  Change-add and change-remove
   understand this, ws-update and ws-set may not *FINISH*)

   If the dids in question are explicitly listed as 'adds' in the VPB, we remove them.
   If the dids in question are NOT in the 'adds', we make their removal explicit by adding
   them to the 'removes' slot. (where 'adds' and 'removes' are
   WORKSPACE-{ADDED,REMOVED}-SUBSYSTEM-CSET-TUPLES slots in the workspace object.

   SATELLITE-DIDS must be a list of DIDs of change-sets in the satellite repository corresponding
   to SUBSYSTEM.

   Unless ERRORP is NIL, this will signal an error if the SATELLITE-DIDS have already
   been removed from the workspace.

   Returns NIL."
  (workspace-add/remove-subsystem-change-sets workspace subsystem-did satellite-dids
                                              :remove errorp)
  nil)

(defun workspace-add-subsystem-change-sets (workspace subsystem-did satellite-dids
                                            &key (errorp t))
  "If the dids in question are explicitly listed as 'removes' in the VPB, we remove them from that list.
   If the dids in question are NOT in the 'removes', we make their addition explicit by adding
   them to the 'adds' slot. (where 'adds' and 'removes' are
   WORKSPACE-{ADDED,REMOVED}-SUBSYSTEM-CSET-TUPLES slots in the workspace object.

   SATELLITE-DIDS must be a list of DIDs of change-sets in the satellite repository corresponding
   to SUBSYSTEM.

   This code is the mirror operation of `WORKSPACE-REMOVE-SUBSYSTEM-CHANGE-SETS',
   and is moderately brain-hurting in its semantics.  Comments have been stripped from this function's
   implementation because it is the same code as the mirror routine, except that slot manipulation
   is reversed.  So refer to the mirror implementation and  comments in order to better understand
   this routine.

   Unless ERRORP is NIL, this will signal an error if the SATELLITE-DIDS have already
   been added to the workspace.

   Returns NIL."
  (workspace-add/remove-subsystem-change-sets workspace subsystem-did satellite-dids
                                              :add errorp)
  nil)

(defun workspace-add/remove-subsystem-change-sets (workspace subsystem-did satellite-dids
                                                   operation errorp)
  "Common code used by both WORKSPACE-ADD-SUBSYSTEM-CHANGE-SETS and
   WORKSPACE-REMOVE-SUBSYSTEM-CHANGE-SETS.

   OPERATION is either :ADD or :REMOVE."
  (check-type operation (member :add :remove))
  ;; Note that since the code here was lifted from that of
  ;; WORKSPACE-REMOVE-SUBSYSTEM-CHANGE-SETS, the sense of some
  ;; comments should be interpreted with respect to that operation.

  ;; Moderately tricky code alert.  Also, use of 'nset-*' vs 'set-*',
  ;; and 'delete' vs. 'remove', have been carefully considered.
  ;; Altering list structure of input arguments is not permitted.

  ;; Note: we expect that satellite cset dids may be a mix, some being
  ;; removed by virtue of being in add list, and some be being in
  ;; remove list.  So we try to process all dids and signal a warning
  ;; if we have any which fell into neither category.

;  ;; First, we set up the transitional slots to mirror the original slots.
;  (setf (workspace-transitional-versionref workspace) (workspace-baseline-versionref workspace)
;       (workspace-transitional-added-class-cset-tuples workspace) (workspace-added-class-cset-tuples workspace)
;       (workspace-transitional-removed-class-cset-tuples workspace) (workspace-removed-class-cset-tuples workspace))

  ;; Now check to see if they're in the slot they should be removed
  ;; from.  If they are, remove them.
  (flet ((ws-slot-reader ()
           (ecase operation
             (:add    (workspace-removed-class-cset-tuples workspace))
             (:remove (workspace-added-class-cset-tuples   workspace))))
         (ws-slot-writer (new-value)
           (ecase operation
             (:add    (setf (workspace-removed-class-cset-tuples workspace)
                            new-value))
             (:remove (setf (workspace-added-class-cset-tuples   workspace)
                            new-value)))))
    (let* ((current-alist (ws-slot-reader))
           (current-acons (assoc subsystem-did current-alist))
           (current-satellite-dids (cdr current-acons))
           (overlap-dids (intersection satellite-dids current-satellite-dids)))
      ;; If we have overlap dids, then the satellite dids are present by
      ;; virtue of a change_add or change_close in the VPB.  Remove them
      ;; from the 'add' list.
      (when overlap-dids
        (setq current-satellite-dids (nset-difference current-satellite-dids overlap-dids))
        (if current-satellite-dids
            ;; Update the alist structure and zap slot so astore will
            ;; know about it
            (progn
              (setf (cdr current-acons) current-satellite-dids) ;modifies structure of current alist
              (ws-slot-writer current-alist))
          ;; Remove key/val pair from current-alist, since there aren't
          ;; any more values for subsystem in question
          (ws-slot-writer (delete current-acons current-alist)))
        ;; Update the dids remaining to be processed, if any, by
        ;; stripping out those we just processed.
        (setq satellite-dids (set-difference satellite-dids overlap-dids)))))

  ;; Satellite-dids which reach this point aren't in the 'added' slot,
  ;; place them in the 'removed' slot.  It's an error if they're
  ;; already in the 'removed' slot.
  ;; (From a sanity check perspective, it's an error, from a model
  ;; standpoint, we may need a way to clear it from the 'removed'
  ;; list, without thinking of it as an 'add', but you can do this for
  ;; now by calling WORKSPACE-ADD-SUBSYSTEM-CHANGE-SETS).
  ;; NOTE: It's not an error when called from the "port -act" command.  If
  ;; "port -act" had to guard against this error it would be a
  ;; needless complication and a violation of modularity.
  (flet ((ws-slot-reader ()
           (ecase operation
             (:add    (workspace-added-class-cset-tuples   workspace))
             (:remove (workspace-removed-class-cset-tuples workspace))))
         (ws-slot-writer (new-value)
           (ecase operation
             (:add    (setf (workspace-added-class-cset-tuples   workspace)
                            new-value))
             (:remove (setf (workspace-removed-class-cset-tuples workspace)
                            new-value)))))
    (let* ((current-alist (ws-slot-reader))
           (current-acons (assoc subsystem-did current-alist))
           (current-satellite-dids (cdr current-acons))
           (overlap-dids (intersection satellite-dids current-satellite-dids)))
      (when (and errorp overlap-dids)
        (error "Programming error, attempt to remove satellite cset(s) ~s in workspace where ~
             some (~s) have already been removed." satellite-dids overlap-dids))
      ;; Update the tuple and return it.  Note that you have to be careful about updating this
      ;; persistent alist (astore deficiencies).  We must reset the whole slot value, not just zap the
      ;; cdr of the cons we care about if it already existed.
      ;; Now update the slot so it'll be remarshalled into persistent space.
      (if current-acons
          ;; Zap existing list structure, then slot
          (progn
            (setf (cdr current-acons) (nconc current-satellite-dids
                                             (set-difference satellite-dids overlap-dids)))
            (ws-slot-writer current-alist))
        ;; We need a new list tuple in the list.  This will update the persistent 'place' appropriately.
        (ws-slot-writer (cons (cons subsystem-did satellite-dids)
                              (ws-slot-reader))))))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Workspace VPB predicates

(defun workspace-has-added-class-cset-tuples (workspace)
  "True iff the workspace has added csets in its virtual private branch."
  (some #'cdr (workspace-added-class-cset-tuples workspace)))

(defun workspace-has-removed-class-cset-tuples (workspace)
  "True iff the workspace has removed csets in its virtual private branch."
  (some #'cdr (workspace-removed-class-cset-tuples workspace)))

(defun workspace-has-vpb-changes (workspace)
  "True iff the workspace has any added or removed CSets in its virtual private branch."
  (or (workspace-has-added-class-cset-tuples workspace)
      (workspace-has-removed-class-cset-tuples workspace)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Workspace port activity

(defmethod (setf workspace-port-activity) :before (new-value (workspace workspace))
  ;; Whenever the PORT-ACTIVITY slot is changes, the
  ;; PORT-ACTED-ON-FLAG must be cleared.
  (declare (ignore new-value))
  (setf (workspace-port-activity-acted-on-p workspace) nil))

(defun workspace-has-port-activity-p (workspace)
  "Returns true if WORKSPACE has pending port activity.
   There is port activity pending between the time that a user uses the port GUI
   to select changes to port to the workspace and the time he uses the 'port -act'
   command to bring the selected changes into the workspace."
  (not (null (workspace-port-activity workspace))))

(defun workspace-map-over-port-activity (workspace function)
  "FUNCTION is called on each entry in the PORT-ACTIVITY slot of WORKSPACE.
   FUNCTION gets four arguments, a CSET-DID, the DID of the affected subsystem,
   its port action (either :PORT-ADD, :PORT-DELETE, :REJECT or :DEFER) and
   whether the change should be added or dropped (:ADD or :DELETE) from the
   subsystem."
  (loop for (cset-did port/defer/reject add/delete subsystem-did)
        in (workspace-port-activity workspace)
        do (funcall function cset-did subsystem-did port/defer/reject add/delete))
  nil)

(defun workspace-update-port-activity (workspace new-port-activity)
  "NEW-PORT-ACTIVITY is an ALIST similar to that in WORKSPACE-PORT-ACTIVITY,
   each element being a list of a satellite CSET-DID, the corresponding port
   action for that change, and DID of the subsystem to be affected by the port.
   Those entries in NEW-PORT-ACTIVITY are merged into those already present
   in the PORT-ACTIVITY slot of the workspace.  Information in NEW-PORT-ACTIVITY
   is preferred over that for the same change set DID which might already be
   present in the WORKSPACE's PORT-ACTIVITY slot."
  ;; Do some simple validation on NEW-PORT-ACTIVITY
  (loop for (cset-did port/defer/reject add/delete subsystem-did) in new-port-activity
        do
        (check-type cset-did distributed-identifier)
        (check-type port/defer/reject (member :port :defer :reject))
        (check-type add/delete (member :add :delete))
        (check-type subsystem-did distributed-identifier))
  (let ((new-result nil))
    ;; Combine the workspace's new porting activity with it's previous
    ;; porting activity, preferring the more recent information.
    (flet ((merge-it (entry)
             (unless (find (first entry) new-result
                           :key #'first)
               (push entry new-result))))
      (mapc #'merge-it new-port-activity)
      (mapc #'merge-it (workspace-port-activity workspace)))
    (setf (workspace-port-activity workspace) new-result))
  nil)

(defun workspace-clear-port-activity (workspace)
  "Clear the PORT-ACTIVITY slot of WORKSPACE."
  (setf (workspace-port-activity workspace) nil))

(defun workspace-port-activity-collect-adds-and-deletes-by-subsystem (workspace)
  "Look at the PORT-ACTIVITY of the WORKSPACE and collect the satellite changes
   to be added and removed by subsystem.

   Two values are returned.  Each is an ALIST.  The first element of each entry is
   a subsystem DID.  The CDR of each element is a list of CSET DIDs.
   The first value is an alist for the changes to be added to the affected subsystems.
   The second value is an alist of the changes to be dropped from the affected subsystem. "
  (let ((subsystem-add-alist nil)
        (subsystem-remove-alist nil))
    (workspace-map-over-port-activity
     workspace
     (lambda (cset-did subsystem-did port/defer/reject add/delete)
         (when (eq port/defer/reject :port)
           (macrolet ((note-cset (variable subsystem-did cset-did)
                        `(let* ((subsystem-did ,subsystem-did)
                                (cset-did ,cset-did)
                                (found (find subsystem-did ,variable
                                             :key #'first)))
                           (unless found
                             (setq found (list subsystem-did))
                             (push found ,variable))
                           (push cset-did (cdr found)))))
             (ecase add/delete
               (:add (note-cset subsystem-add-alist subsystem-did cset-did))
               (:delete (note-cset subsystem-remove-alist subsystem-did cset-did)))))))
    (values subsystem-add-alist subsystem-remove-alist)))

(defun workspace-port-activity-collect-cset-rejection-by-subsystem (workspace)
  "Returns a list of tuples.  Each tuple is a list of three elements:
   a subsystem DID, a list of satellite cset dids to be rejected
   for that subsystem, and a list of satellite cset dids to
   be unrejected by that subsystem."
  (let ((noted-by-subsystem nil))
    (workspace-map-over-port-activity
     workspace
     (lambda (cset-did subsystem-did port/defer/reject add/delete)
         (declare (ignore add/delete))
         (macrolet ((note-it (cset-did resurrect/reject)
                      `(let ((found (find subsystem-did noted-by-subsystem
                                          :key #'first)))
                         (unless found
                           (setq found (list subsystem-did nil nil))
                           (push found noted-by-subsystem))
                         (push ,cset-did (,(ecase resurrect/reject
                                             (:resurrect 'third)
                                             (:reject 'second))
                                             found)))))
           (ecase port/defer/reject
             ((:port :defer)
              ;; If the change was previously rejected it is now no
              ;; longer so.
              (note-it cset-did :resurrect))
             (:reject
              (note-it cset-did :reject))))))
    noted-by-subsystem))

#||
;;; For efficiency reasons, port activity is now acted upon by
;;; CMCTL-CHANGE-ADD-OR-REMOVE.  This is disgustingly non-modular, but
;;; naha's puny brain was unable to figure out what bits of that
;;; function needed to be done for "port -act" to update the workspace
;;; efficiently.

(defun workspace-act-on-port-activity (workspace)
  "Add and remove changes for the workspace according to WORKSPACE's port activity."
  (multiple-value-bind (subsystem-add-alist subsystem-remove-alist)
      (workspace-port-activity-collect-adds-and-deletes-by-subsystem workspace)
    ;; We pass :ERRORP NIL here when adding and removing change
    ;; sets to/from the workspace because the user can perform
    ;; the "port -act" command several times.
    #||
    (loop for (subsystem-did . cset-dids) in subsystem-add-alist
          do (workspace-add-subsystem-change-sets workspace subsystem-did cset-dids
                                                  :errorp nil))
    (loop for (subsystem-did . cset-dids) in subsystem-remove-alist
          do (workspace-remove-subsystem-change-sets workspace subsystem-did cset-dids
                                                     :errorp nil))
    ||#
    ;; Since there are no added or removed changes other than those
    ;; from the porting activity, we can set the added and removed
    ;; slots of the workspace directly from what's in the port
    ;; activity.  We don't need to do complicated merging
    (setf (workspace-added-class-cset-tuples workspace)
          subsystem-add-alist)
    (setf (workspace-removed-class-cset-tuples workspace)
          subsystem-remove-alist))
  (setf (workspace-port-activity-acted-on-p workspace) t))
||#

(defun workspace-port-activity-entry-action-reject-p (port-activity-entry)
  "PORT-ACTIVITY-ENTRY is an element of WORKSPACE-PORT-ACTIVITY of a WORKSPACE.
   Return true if it represents the rejection of a satellite change from the workspace."
  (eq :reject (second port-activity-entry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
||#

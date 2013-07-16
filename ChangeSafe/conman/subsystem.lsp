;;;; -*- Mode: LISP; coding: iso-8859-1; Syntax: COMMON-LISP; Base: 10; -*-
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
;;;; Module Description:
;;;   Changesafe Commands
;;;;
;;;; Author:        Joe Marshall
;;;; Creation Date: 2003
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CONMAN")

(proclaim (standard-optimizations))

(defconstant +subsystem-subscriber-modes+ '(:WRITE :PORT)
  "Valid modes for a subscriber.  READ mode is always implicit in any subsystem.")

(defclass subsystem-subscriber ()
  ;; We must track the BRANCH of the PC rather than the PC itself,
  ;; since the access modes for the subsystem can vary across
  ;; different branches of the PC.  Thus a SUBSYSTEM must have a
  ;; SUBSYSTEM-SUBSCRIBER object for each PC BRANCH that subscribes to
  ;; it, rather than merely each PC.
  ;; The BRANCH of the PC which subscribes to this subsystem.
  ((branch :initarg :branch
           :initform (error "Required initarg :branch omitted.")
           :reader subscriber/branch
           :version-technique :nonversioned)

   ;; The access permitted the subsystem.  :READ is implicit.
   ;; The following slot may have zero or more of :WRITE and :PORT.
   (modes :initarg :modes
          :initform nil
          :accessor subscriber/modes
          :version-technique :composite-set))

  (:documentation
   "A subscriber to a subsystem.
    This is a helper class to the subsystem class, and is not to be used outside the subsystem module.")
  (:metaclass versioned-standard-class)
  (:schema-version 0))


(defclass subsystem (project-reference)
  (
  ;; Class, owned by master catalog, but to help us open the database/project
  ;; question...
  (csf-class
   :initarg :csf-class
   :initform (error "Required initarg :csf-class omitted.")
   :reader subsystem/csf-class
   :version-technique :nonversioned)

  ;; A bit to indicate that the subsystem is frozen. Set by release_freeze
  ;; on all subsystems in the product. Once frozen, no changes are allowed,
  ;; and gaining write permission should even be disallowed.
   (frozen? :initform nil :version-technique :logged)   ; write-once, remember when.

  ;; List of subscribers to this subsystem (a list of subsystem-subscriber objects).
  ;; The list is moderately dynamic (in terms of add/remove actions).  It is usually viewed
  ;; under the repository-master-cid-set. However I suppose there might be reasonable scenarios
  ;; for more specialized versioned contexts in which this data is used.

  ;; ***TBD*** Note that the PC-SUBSYSTEMS must be maintained in
  ;; quasi-parallel with this slot.  I say quasi-parallel because the
  ;; version of the PC which is altered corresponds only loosely with
  ;; the version of SUBSYSTEM.  This is an open issue which we are
  ;; just fudging for now [2000-02-03 naha].
  (subscribers :initform nil
               :accessor subsystem/subscribers
               :version-technique :composite-set)

  ;; (SUBSYSTEM MODE) pairs from which we pull changes. Versioned, can
  ;; change.  See subsys_inherit_from command.  The contents of this
  ;; slot are maintained using SUBSYSTEM-INHERITANCE-MODE and
  ;; SUBSYSTEM-SET-INHERITANCE-MODE.
  (change-source :initform nil :version-technique :composite-set)

  ;; A list of subsystem-wall objects active on this subsystem.
  ;; This slot isn't versioned, and it isn't under change-set control.
  ;; It it therefore modified under read/write transactions which DO NOT yield csets.
  ;; It currently bears no relationship to LOCK-STATUS, but this may change pending resolution of
  ;; HP question #109.
  (walls :initform nil :version-technique :nonversioned)

  ;; Lock-status is like a temporary wall in which no holes may be placed.
  ;; Performed by master-lock.  We don't version this, and a lock operation is sometimes done
  ;; without creating a change-set, so we can't even log it.  The state is either NIL (unlocked),
  ;; or a workspace identifier of the workspace which has this subsystem locked.
  (lock-status :initform nil
               :version-technique :nonversioned
               :accessor subsystem/lock-status)

  ;; This slot is similar to SATELLITE-CSET-DIDS above except that
  ;; these represent change sets which were rejected during a port
  ;; operation and were thus not ported into this subsystem.  The
  ;; versioning discipline for this slot is similar to that of
  ;; SATELLITE-CSET-DIDS.
  (rejected-satellite-cset-dids :initform nil
                                :version-technique :composite-set)

  ;; *TBD*: we may need an 'all-set' of every satellite cset which ever was propagated into this
  ;; subsystem in order to answer the question as to whether a given cset ever participated in a subsystem.
  ;; The mere fact that a cset applies to the subsystem satellite repository isn't enough to answer
  ;; the question of whether or not it was on this branch.  We could derive the answer to this
  ;; question by examining all IONS for satellite-cset-dids, inactive or active, or we can
  ;; maintain a separate 'all-set' slot whenever we promote into satellite-cset-dids.
  ;; Using an ION based approach eliminates lots of data redundancy and index overhead,
  ;; but requires a CVI and/or VI function to examine ALL values of ALL cids every applied,
  ;; which is not something we have yet.  We have a VI routine (report-changed-slot)
  ;; which gies ALL values for a slot for ONE cid, and it's very high
  ;; overhead (for reporting purposes), rather than allowing you to just examine the data.
  ;; Really want a VI-MAP-ALL-SLOT-VALUES, perhaps governed by a cid-set to limit values.
  ;; Useful only for treating values as a set however, since ordered values are meaningless without
  ;; cid-set context.
   )
  (:documentation
     "A subsystem, mapping one or more product-configuration subscribers to
      a specific branch of a specific project in a satellite repository,
      with specific rights to branch activities.

      ALL data in the subsystem should be viewed with the master cid-set (metaversion).")

  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defun subsystem? (object)
  "Return true if OBJECT is of type SUBSYSTEM."
  (typep object 'subsystem))

(defun subsystem/scan-subscribers (subsystem)
  (declare (optimizable-series-function))
  (versioned-object/scan-composite-versioned-slot subsystem 'subscribers))

(defun subsystem/add-subscriber (subsystem subscriber &key subscriber-modes)
  "Add a subscriber to a subsystem.

   SUBSCRIBER is a branch of a product.
   An error is signalled if subscriber already subscribes to the current view of the subsystem.

   SUBSRIBER-MODES, if specified, is a list containing zero or more values from *SUBSYSTEM-SUBSCRIBER-MODES*.
   By default is is nil, and the subscriber has only read access to the subsystem.

   Returns the passed subsystem."
  (check-type subscriber csf-branch)
  (assert (product? (branch/owning-project subscriber)))
  (if (find subscriber (subsystem/subscribers subsystem) :key #'subscriber/branch)
      (conman-error/branch-already-subscribed subsystem subscriber)
      (pushlast (make-instance 'subsystem-subscriber
                               :branch subscriber
                               :modes subscriber-modes)
                (subsystem/subscribers subsystem))))

(defun subsystem/satellite-repository-dbpath (master-repository-dbpath subsystem)
  (csf-class/satellite-repository-dbpath master-repository-dbpath (subsystem/csf-class subsystem)))

(defun subsystem/subdirectory (subsystem)
  (csf-class/subdirectory (subsystem/csf-class subsystem)))

(defun subsystem/call-with-satellite-repository (master-repository-dbpath subsystem satellite-open-mode receiver)
  "Invoke <receiver> on two arguments, the class associated with <subsystem> and
   the satellite repository associated with that class.  The satellite repository will
   be opened in <open-mode>."
  (let ((csf-class (subsystem/csf-class subsystem)))
    (csf-class/call-with-satellite-repository
     master-repository-dbpath csf-class satellite-open-mode
     (lambda (satellite-repository)
       (funcall receiver csf-class satellite-repository)))))

(defun subsystem/call-with-satellite-transaction (master-repository-dbpath user-id-specifier subsystem
                                                                           &key reason
                                                                           satellite-transaction-type
                                                                           satellite-metaversion
                                                                           vpb-cid-dids-to-add
                                                                           vpb-cid-dids-to-remove
                                                                           receiver)
  (let ((csf-class (subsystem/csf-class subsystem))
        (version-state (project-reference/version-state subsystem)))
    (csf-class/call-with-satellite-transaction
     master-repository-dbpath user-id-specifier csf-class
     :reason reason
     :satellite-transaction-type satellite-transaction-type
     :satellite-metaversion satellite-metaversion
     :satellite-version (cond ((distributed-identifier? version-state) version-state)
                              ((eq version-state :latest) :latest-version)
                              (t (error "Unrecognized version state.")))
     :vpb-cid-dids-to-add vpb-cid-dids-to-add
     :vpb-cid-dids-to-remove vpb-cid-dids-to-remove
     :receiver (lambda (satellite-repository satellite-transaction satellite-project)
                 (let* ((satellite-branch (repository/resolve-distributed-identifier satellite-repository 
                                                                                     (project-reference/branch-did subsystem)))
                        (satellite-version (cond ((distributed-identifier? version-state)
                                                  (repository/resolve-distributed-identifier satellite-repository version-state))
                                                 ((eq version-state :latest)
                                                  (branch/most-recent-version satellite-branch))
                                                 (t (Error "Unrecognized version state.")))))
                   (check-type satellite-branch  satellite-subsystem)
                   (funcall receiver 
                            csf-class
                            satellite-repository
                            satellite-transaction
                            satellite-project
                            satellite-branch
                            satellite-version))))))

(defun subsystem/acquire-lock (subsystem workspace-identifier description)
  "Acquire a lock on a subsystem.  Return true if the lock is acquired for WORKSPACE-IDENTIFIER,
   and NIL if it cannot be acquired. Description is a required string explaining the resaon for the lock.
   Note that if the workspace already has the lock, we consider this operation successful."
  (check-type description string)
  (let ((status (car (subsystem/lock-status subsystem))))
    (cond ((null status)
           (setf (subsystem/lock-status subsystem) (cons workspace-identifier description))
           t)
          ((eq (car status) workspace-identifier)
           (setf (subsystem/lock-status subsystem)
                 (cons workspace-identifier (format nil "~a ~% ~a"
                                                    (cdr (subsystem/lock-status subsystem))
                                                    description)))
           t)
          (t nil))))

(defun subsystem/release-lock (subsystem)
  "Release a lock on a subsystem.  It is an error if the subsystem is not locked.
   Return subsystem."
  (assert (subsystem/lock-status subsystem))
  (setf (subsystem/lock-status subsystem) nil)
  subsystem)

(defun subsystem-lock/description (subsystem)
  "Return the description string associated with the lock on a subsystem. Return NIL if subsystem not locked."
  (let ((status (subsystem/lock-status subsystem)))
    (and status (cdr status))))

(defun subsystem-lock/workspace-id (subsystem)
  "Return the WORKSPACE-IDENTIFIER that acquired the lock on a subsystem. Return NIL if subsystem not locked."
  (let ((status (subsystem/lock-status subsystem)))
    (and status (car status))))

(defun subsystem/satellite-cset-dids (master-repository-dbpath user-id-specifier metaversion subsystem)
  "Retrieve a list of satellite-cset-did objects which are active in the current metaversion
   controlled view of SUBSYSTEM.

   This function needs a metaversion-timestamp to operate properly.  It gets an appropriate value
   from the metaversion-qualifier slot in the *cm-txn-master-repository-txn-context* object.
   This means that you are required to have done a with-cm-master-metaversion call at some point.
   "
  (debug-message 3 "subsystem/satellite-cset-dids ~s ~s" metaversion subsystem)
  (let ((reason (format nil "subsystem/satellite-cset-dids for ~a" (named-object/name subsystem))))
    (call-with-satellite-repository
     (subsystem/satellite-repository-dbpath master-repository-dbpath subsystem)
     :readonly
     (lambda (satellite-repository)
       (call-with-satellite-transaction
        :user-id-specifier user-id-specifier
        :satellite-metaversion metaversion
        :satellite-version (project-reference/branch-did subsystem)
        :reason reason
        :repository satellite-repository
        :transaction-type :read-only
        :receiver
        (lambda (satellite-transaction)
          (let ((active-cid-set (transaction/cid-set satellite-transaction)))
            (debug-message 3 "subsystem/satellite-cset-dids active-cid-set ~s" active-cid-set)
            (collect 'list
                     (map-fn 't (lambda (cid)
                                  (repository/cid-versioned-change-information
                                   satellite-repository cid))
                             (choose-if (lambda (cid)
                                          (cid-set/member active-cid-set cid))
                                        (scan-range :from 1 :upto (cid-set/last-cid active-cid-set))))))))))))

(defun subsystem/extract-changed-files-to-disk (user-id-specifier subsystem
                                                                  satellite-repository file-system
                                                                  old-metaversion
                                                                  old-version
                                                                  new-metaversion
                                                                  new-version
                                                                  &key
                                                                  (change-context nil)
                                                                  (report-only nil)
                                                                  (report-file-system file-system)
                                                                  VPB-old-added-satellite-cset-dids
                                                                  VPB-new-added-satellite-cset-dids
                                                                  VPB-old-removed-satellite-cset-dids
                                                                  VPB-new-removed-satellite-cset-dids)

  "Write changes to a subsystem's files to disk, by binding the subsystem subdirectory
   relative to FILE-SYSTEM, as well as the appropriate project context for the subsystem.

   OLD-TIMESTAMP and NEW-TIMESTAMP are used to specify the date-driven metaversion
   in the satellite repository.  They should be time-stamp objects, or NIL indicating that
   the 'latest' metaversion is desired.

   SATELLITE-REPOSITORY must be open and in-transaction.

   REPORT-FILE-SYSTEM, if specified, is where the publishing noise is directed to.

   CHANGE-CONTEXT is the current change context.  If it is present, it is assumed to describe
   the uncommitted changes in the file-system, and a three-way merge will be done
   when necessary.

   VPB-{added,removed}-satellite-csets, if specified, are lists of cid-sets
   additions/deletions to be made to the version context in viewing satellite repository
   content.  These elements are part of the user's virtual private branch in the
   workspace.

   **** WARNING ****
   See above paragraph.  We're in a satellite repository context for this method!

   This routine is a bit like MASTER-CATALOG-EXTRACT-PC-FILES-TO-DISK ,
   except that we assume the satellite repository is open and in-transaction already,
   and we're only processing one subsystem of a PC, not all subsystems.

   Returns three values:

   The first value is T if a three-way merge of any file is needed.
   The second value is and integer counting the number of merge conflicts
   across all three-way merges.
   The third value is T if a binary file needed to be merged."
  #+allegro (declare (:fbound running-development-changesafe-server-p))

  (debug-message 3 "getting reference dids")
  (let* ((project-did   (call-with-after-view (lambda () (project-reference/project-did   subsystem))))
         (branch-did    (call-with-after-view (lambda () (project-reference/branch-did    subsystem))))
         (version-state (call-with-after-view (lambda () (project-reference/version-state subsystem)))))
    (debug-message 3 "got reference dids")
    ;; All the merge logic is in this function.

    ;; We have the BEFORE state, the AFTER state, and potentially the current
    ;; change-context state to deal with.  If the change-context in NULL, it is a simple update,
    ;; but if we do have a change context, it is a `merge with common ancestor'.

    ;; This code *always* succeeds.  I.E.  the workspace and change-context must always
    ;; have the new changes reflected within them after this is run.  This is because the update
    ;; runs on a subsystem by subsystem basis, and to abort in the middle would leave the
    ;; workspace in an inconsistent state.

    ;; In cases where conflict occurs and it is not obvious what to do, we try to do the
    ;; following:
    ;;   1.  Always incorporate repository changes.
    ;;   2.  Preserve any user changes.
    ;;
    ;; Rule 1 ensures that checking in after an update (even if the update causes conflicts)
    ;;   will preserve the most recent changes in the repository.
    ;;
    ;; Rule 2 ensures that the user can reconstruct his pre-update state.  Note that Rule 1
    ;;   is more important than rule 2, so in the case of unresolvable conflicts, the user's
    ;;   files would have to be renamed.  This makes it more difficult for the user to back
    ;;   out of the update, but that's ok.
    ;;
    ;; If necessary, this code will modify the user's change context.  The following applies:
    ;;
    ;;     If a user deletes a file that has been deleted in the repository, the deletion
    ;;     request is removed from the user's change-context.
    ;;
    ;;     If a user renames a file that has been deleted in the repository, the rename
    ;;     request is removed from the user's change-context.  The file on disk will be
    ;;     renamed with a .DELETED extension.
    ;;
    ;;     If a user modifies a file that has since been deleted, the
    ;;     modified file is UNCO'd (un checked out) and renamed with a
    ;;     .DELETED extension.  (If the user really wants to change the
    ;;     file, he will have to manually remove the change set that
    ;;     deleted the file and re-merge in his changes.)
    ;;
    ;;     If a user renames a file that has been renamed in the
    ;;     repository, the user's rename will take precedence.
    ;;
    ;;     If a user renames a file such that it collides with a new
    ;;     file from the repository, the new file takes precedence.
    ;;     The disk file is renamed with a .RENAMED extension, and the
    ;;     rename request in the change context is modified to reflect
    ;;     this.
    ;;
    ;;     If a user renames a file such that it collides with a
    ;;     renamed file from the repository, the repository file takes
    ;;     precedence.  The disk file is renamed with a .RENAMED extension,
    ;;     and the rename request in the change context is modified to reflect
    ;;     this.
    ;;
    ;;     If a user adds a file such that it collides with a file
    ;;     added in the repository, *and the added file is identical
    ;;     to the one in the repository*, the user's change context is
    ;;     modified to remove the file_add.
    ;;
    ;;     If a user adds a file such that it collides with a file
    ;;     added in the repository, *and the added file is different
    ;;     from the one in the repository*, the user's disk file is
    ;;     renamed with an .ADDED extension, and the user's change
    ;;     context is modified to remove the file_add.
    ;;
    ;;     Files that have been renamed and modified are dealt with
    ;;     under their *new* name.
    ;;
    ;;     If both the user and the repository have modifications to a
    ;;     file, and the results are identical, the user's file is
    ;;     unco'd.
    ;;
    ;;     If both the user and the repository have modifications to a
    ;;     file, and the results are different, a three-way merge is
    ;;     performed.   If the changes are in different sections of
    ;;     the file, the merge is `successful'.  If the change
    ;;     overlap, the merge has `conflicts', and manual merging must
    ;;     be done.  Conflicting merges will have markers placed in
    ;;     the file to indicate where the problems are.
    (debug-message 3 "starting satellite transaction")
    (call-with-satellite-transaction
     :repository satellite-repository
     :transaction-type :read-only-compare
     :user-id-specifier user-id-specifier
     :reason "Extract changed files."

     :satellite-metaversion  old-metaversion
     :satellite-version      old-version
     :vpb-cid-dids-to-add    VPB-old-added-satellite-cset-dids
     :vpb-cid-dids-to-remove VPB-old-removed-satellite-cset-dids

     :aux-satellite-metaversion  new-metaversion
     :aux-satellite-version      new-version
     :aux-vpb-cid-dids-to-add    VPB-new-added-satellite-cset-dids
     :aux-vpb-cid-dids-to-remove VPB-new-removed-satellite-cset-dids

     :receiver
     (lambda (cm-transaction)
       (debug-message 3 "in satellite-transaction")
       (macrolet ((before (&body body)
                    `(CALL-WITH-BEFORE-VIEW
                      (lambda () ,@body)))

                  (after (&body body)
                    `(CALL-WITH-AFTER-VIEW
                      (lambda () ,@body))))

         (debug-message 3 "resolving project, branch, satellite-root-directory, files")
       (let* ((project (repository/resolve-distributed-identifier satellite-repository project-did))
              (branch  (repository/resolve-distributed-identifier satellite-repository branch-did))
              (relevant-changes (cid-set/exclusive-or
                                 (before (transaction/cid-set cm-transaction))
                                 (after  (transaction/cid-set cm-transaction))))
              (satellite-root-directory (after (rfm-project/root-directory project)))
              (old-files (before (file-system-element/descendants satellite-root-directory)))
              (new-files (after (file-system-element/descendants satellite-root-directory))))
         (debug-message 3 "relevant changes = ~s" relevant-changes)

           (before (debug-message 3 "Old files = ~s" old-files))
           (after (debug-message 3 "new files = ~s" new-files))
           (let* ((removed-files (set-difference old-files new-files))
                  (added-files   (set-difference new-files old-files))
                  (common-files  (intersection   new-files old-files))
                  (renamed-files nil)
                  (changed-files nil)

                  total-work
                  (current-work 0)
                  (last-work-factor 0)

                  ;; return values
                  (performed-three-way-merge nil)
                  (total-merge-conflicts       0)
                  (binary-merge              nil))

             (labels ((work-factor ()
                        (* 5 (floor (* (/ current-work total-work) 20.))))

                      (work-tick ()
                        (incf current-work)
                        (unless (= (work-factor) last-work-factor)
                          (setq last-work-factor (work-factor))
                          (file-system/note-progress report-file-system nil (work-factor))))

                      (user-file-name (pathname)
                        (decode-pathname pathname)
                                        ;(pathname->platform-namestring
                                        ; (decode-pathname pathname)
                                        ; (file-system/platform file-system))
                        )

                      (sort-files (filelist)
                        (sort filelist #'string-lessp
                              :key (lambda (rfm-file)
                                     (namestring (rfm::file-system-element/relative-path rfm-file))))))

               (let ((old-common-file-names (before (mapcar #'rfm::file-system-element/relative-path common-files)))
                     (new-common-file-names (after  (mapcar #'rfm::file-system-element/relative-path common-files))))

                 (debug-message 3 "Old common file names = ~s" old-common-file-names)
                 (debug-message 3 "New common file names = ~s" new-common-file-names)
                 (mapc (lambda (common-file old-name new-name)
                         (let ((changed? (cond ((typep common-file 'rfm-file)
                                                ;; A file is changed if the contents are changed, but
                                                ;; name changes are irrelevant, so we look for the magic
                                                ;; slots in the file.
                                                (or (cid-set/intersection?
                                                     relevant-changes
                                                     (versioned-object-slot/cid-set 
                                                      satellite-repository common-file 'rfm::content-type))
                                                    (cid-set/intersection?
                                                     relevant-changes
                                                     (versioned-object-slot/cid-set 
                                                      satellite-repository common-file 'rfm::binary-content))
                                                    (cid-set/intersection?
                                                     relevant-changes
                                                     (versioned-object-slot/cid-set 
                                                      satellite-repository common-file 'rfm::content))))
                                               (t (cid-set/intersection?
                                                   relevant-changes
                                                   (versioned-object/cid-set satellite-repository common-file))))))
                           (unless (equal old-name new-name)
                             (debug-message 3 "File renamed from ~s to ~s" old-name new-name)
                             (push (list common-file old-name new-name) renamed-files))
                           (when changed?
                             (push common-file changed-files))))
                       common-files old-common-file-names new-common-file-names))

               ;; By sorting the files, we make the process handle the
               ;; files in lexicographic order.  This makes it easier
               ;; for the user to understand the report because it is
               ;; easier to find particular files.

               (setq added-files   (after  (sort-files added-files)))
               (setq removed-files (before (sort-files removed-files)))
               (setq changed-files (after  (sort-files changed-files)))
               (setq renamed-files (after  (sort renamed-files #'string-lessp
                                                 :key (lambda (tuple)
                                                        (namestring (third tuple))))))

               ;; Estimate work
               (setq total-work (+ (length removed-files)
                                   (length renamed-files)
                                   (length added-files)
                                   (length changed-files)
                                   1))

               (when removed-files
                 (debug-message 3 "Removing ~d file~:p" (length removed-files)))

               (before
                (dolist (removed-file removed-files)
                  (when (typep removed-file 'rfm-file);; punt on removing directories
                    (let* ((name (rfm::file-system-element/relative-path removed-file))
                           (removed-file-did (distributed-object-identifier removed-file))
                           ;; Check change-context for conflicts.
                           (removal-records  (and change-context (change-context/find-file-removals change-context removed-file-did)))
                           (change-records   (and change-context (change-context/find-file-changes  change-context removed-file-did)))
                           (rename-records   (and change-context (change-context/find-file-renames  change-context removed-file-did))))

                      (cond (removal-records;; user wants to remove it, too.
                             ;; He may want to remove it to make room for another file of the same name.
                             ;; If so, we better keep the removal record.  If not, we should let the
                             ;; prior removal do it.
                             (let ((add-records      (and change-context
                                                          (change-context/find-file-additions
                                                           change-context name
                                                           (distributed-object-identifier project))))
                                   (rename-to-records (and change-context
                                                           (change-context/find-file-renamed-to
                                                            change-context name
                                                            (distributed-object-identifier project)))))

                               (unless (or add-records;; file appearing.
                                           rename-to-records;; no old file renamed to this.
                                           report-only);; really want it deleted.
                                 ;; User is doing redundant delete, get rid of it.
                                 ;; Note that since the user deleted the file, it must not be on the
                                 ;; disk, so we don't have to delete the file here as well.
                                 (change-context/remove-file-undo change-context (car removal-records)))))

                            ;; Otherwise, it should still be there and untouched.
                            ;; Rename it to be deleted.  We must clear the location.
                            ((or change-records rename-records)
                             (let ((current-name (if rename-records
                                                     (cc-filerename/new-pathname (car rename-records))
                                                     name)))
                               (file-system/note-progress
                                report-file-system
                                (format nil " *- ~s is obsolete and will be~:[~; UNCO'd~]~:[~; and~]~:[~; un-renamed~]."
                                        (user-file-name current-name)
                                        change-records
                                        (and change-records rename-records)
                                        rename-records)
                                (work-factor))

                               (unless report-only
                                 ;; Modify change context.
                                 (when change-records
                                   (change-context/change-file-undo change-context (car change-records)))
                                 (when rename-records
                                   (change-context/rename-file-undo change-context (car rename-records)))
                                 (when (file-system/probe-file file-system current-name);; if it isn't on the disk, punt
                                   ;; rename it to a `deleted' file.
                                   (file-system/backup file-system current-name :pattern "DELETED~@[~d~]")))))


                            ;; Either the user didn't modify the file, or this is a two way merge.
                            ;; In either case, we want to simply delete the removed file.
                            ;; It is ok to do this because the file can be reconstructed by simply
                            ;; removing the change that created it.
                            (t
                             (file-system/note-progress report-file-system
                                                        (format nil "  - ~s is obsolete and will be removed."
                                                                (user-file-name name))
                                                        (work-factor))
                             (unless report-only
                               (when (file-system/probe-file file-system name);; if it isn't on the disk, punt
                                 ;; Delete it.
                                 (file-system/delete-file file-system name :force t)))))))
                  (work-tick)))

               (when renamed-files
                 (debug-message 3 "Renaming ~d file~:p" (length renamed-files)))

               (dolist (renamed-file renamed-files)
                 (let* ((file (first renamed-file))
                        (repository-original-name (second renamed-file))
                        (repository-final-name    (third renamed-file))
                        (renamed-file-did (distributed-object-identifier file))
                        ;; user may have renamed, removed, or changed it.
                        (rename-records  (and change-context (change-context/find-file-renames  change-context renamed-file-did)))
                        (removal-records (and change-context (change-context/find-file-removals change-context renamed-file-did)))
                        ;; User may have added or renamed a different file to same target name.
                        (add-records     (and change-context
                                              (change-context/find-file-additions
                                               change-context repository-final-name
                                               (distributed-object-identifier project))))
                        (rename-to-records (and change-context
                                                (change-context/find-file-renamed-to
                                                 change-context repository-final-name
                                                 (distributed-object-identifier project)))))
                   (unless (or removal-records rename-records);; User removed or renamed the file, no work to do.
                     (cond
                       ;; user wants to add a file with the same name, make a collision.
                       (add-records
                        (file-system/note-progress report-file-system
                                                   (format nil "  * ~s addition collides with repository file.  ~
                                                                  Your file will be renamed."
                                                           (user-file-name (cc-fileadd-pathname (car add-records))))
                                                   (work-factor))
                        (unless report-only
                          (let ((new-name (file-system/backup file-system repository-final-name :pattern "ADDED~@[~d~]")))
                            (setf (cc-fileadd-pathname (car add-records)) new-name))))
                       ;; User wants to rename a different file to this name.
                       (rename-to-records
                        (file-system/note-progress report-file-system
                                                   (format nil "  * ~s rename collides with repository file.  ~
                                                                  Your file will be renamed."
                                                           (user-file-name (cc-fileadd-pathname (car add-records))))
                                                   (work-factor))
                        (unless report-only
                          (let ((new-name (file-system/backup file-system repository-final-name :pattern "ADDED~@[~d~]")))
                            (setf (cc-filerename/new-pathname (car rename-to-records)) new-name))))
                       (t nil))
                     ;; Ok, the destination should be clear.
                     ;; We can (in theory) just rename the file.
                     (let ((probe-old (file-system/probe-file file-system repository-original-name))
                           (probe-new (file-system/probe-file file-system repository-final-name)))

                       (when probe-new
                         (file-system/note-progress report-file-system
                                                    (format nil "    ~s is a wild file.  It will be backed up."
                                                            (user-file-name (file-descriptor/path probe-new)))
                                                    (work-factor)))
                       (file-system/note-progress report-file-system
                                                  (format nil "  R ~s will be renamed to ~s"
                                                          (user-file-name repository-original-name)
                                                          (user-file-name repository-final-name))
                                                  (work-factor))
                       (unless report-only
                         (if probe-old
                             (progn
                               (when probe-new
                                 (file-system/backup file-system (file-descriptor/path probe-new) :pattern "WILD~@[~d~]"))
                               (file-system/rename file-system repository-original-name repository-final-name))
                             ;; but if old file is missing, re-extract it.
                             (after
                              (rfm-file/publish file file-system
                                                :when-overwriting-file (publish/backup-if-overwriting "WILD~@[~d~]")
                                                :publish-read-only? t))))))
                   (work-tick)))


               ;; Ensure directory structure exists!
               (after
                (dolist (added-directory
                          ;; Filter and combine.
                          (remove-duplicates
                           (mapcar (lambda (file)
                                     (if (typep file 'rfm-directory)
                                         file
                                         (rfm::file-system-element/directory file)))
                                   added-files)))

                  (unless (or (eq added-directory satellite-root-directory)
                              (file-system/probe-file file-system (rfm::file-system-element/relative-path added-directory)))
                    (file-system/note-progress report-file-system
                                               (format nil "  + ~s (directory) will be created."
                                                       (user-file-name
                                                        (rfm::file-system-element/relative-path added-directory)))
                                               (work-factor))
                    (unless report-only
                      (file-system/ensure-directory-and-parents file-system
                                                                (rfm::file-system-element/relative-path added-directory))))))

               (when added-files
                 (debug-message 3  "Adding ~d file~:p" (length added-files)))


               ;; At this point, files that are new in the repository should be safe to add.
               ;; But we need to ensure that the user isn't adding them as well.

               (after
                (dolist (added-file added-files)
                  (when (typep added-file 'rfm-file)
                    (let* ((name (rfm::file-system-element/relative-path added-file))
                           (add-records     (and change-context
                                                 (change-context/find-file-additions
                                                  change-context name
                                                  (distributed-object-identifier project))))
                           (rename-to-records (and change-context
                                                   (change-context/find-file-renamed-to
                                                    change-context name
                                                    (distributed-object-identifier project))))
                           (colliding-file (file-system/probe-file file-system name))
                           (same? (and colliding-file
                                       (not (rfm::file-content-changed? added-file file-system colliding-file)))))

                      (cond ((and add-records same?);; user re-added the file
                             (file-system/note-progress report-file-system
                                                        (format nil " *= ~s already added, will undo file_add." (user-file-name name))
                                                        (work-factor))
                             (unless report-only
                               (change-context/add-file-undo-no-log change-context (car add-records))))

                            (add-records ; user added different file
                             (file-system/note-progress report-file-system
                                                        (format nil " *+ ~s collides with repository file.  ~
                                                                     Your file will be renamed. The file_add will be undone."
                                                                (user-file-name name))
                                                        (work-factor))
                             (unless report-only
                               (change-context/add-file-undo-no-log change-context (car add-records))
                               (rfm-file/publish added-file file-system
                                                 :when-overwriting-file (publish/backup-if-overwriting "ADDED~@[~d~]")
                                                 :publish-read-only? t)))

                            (rename-to-records;; user renamed a file to this name
                             (file-system/note-progress report-file-system
                                                        (format nil " *+ ~s collides with repository file.  ~
                                                                     Your file will be renamed. The file_rename will modified."
                                                                (user-file-name name))
                                                        (work-factor))
                             (unless report-only
                               (let ((new-name (file-system/backup file-system name :pattern "RENAMED~@[~d~]")))
                                 (setf (cc-filerename/new-pathname (car rename-to-records)) new-name)
                                 ;; (file-system-note-progress report-file-system
                                 ;;                         (format nil "   Renaming ~s to ~s"
                                 ;;                                 (user-file-name name)
                                 ;;                                 (user-file-name new-name))
                                 ;;                         (work-factor))
                                 (rfm-file/publish added-file file-system
                                                   :when-overwriting-file (publish/backup-if-overwriting "RENAMED~@[~d~]")
                                                   :publish-read-only? t))))

			    (t
			     (when colliding-file
			       (file-system/note-progress report-file-system
							  (format nil " *+ ~s is wild.  It will be backed up."
								  (user-file-name name))
							  (work-factor)))
			     (file-system/note-progress report-file-system
							(format nil "  + ~s will be added to your workspace."
								(user-file-name name))
							(work-factor))
			     (unless report-only
			       (rfm-file/publish added-file file-system
                                                 ;; We can encounter a `wild file' during publishing if the user has
                                                 ;; added a cset that adds the file.  HP says, just back it up.
                                                 :when-overwriting-file (publish/backup-if-overwriting "WILD~@[~d~]")
                                                 :publish-read-only? t
                                                 ))))))
                  (work-tick)))

	       ;; Now deal with the changed files.
	       ;; The files, even if they are renamed, should be in the right place (right?).
	       (dolist (changed-file changed-files)
		 (when (typep changed-file 'rfm-file)
		   (let* ((changed-file-did (distributed-object-identifier changed-file))
			  (remove-records  (and change-context (change-context/find-file-removals change-context changed-file-did))))
		     (unless remove-records;; changed in repository but user is deleting it.  Just ignore it.
		       (let* ((change-records (and change-context (change-context/find-file-changes change-context changed-file-did)))
			      (rename-records (and change-context (change-context/find-file-renames change-context changed-file-did)))
			      (current-name (if rename-records
						(cc-filerename/new-pathname (car rename-records))
                                                (after (rfm::file-system-element/relative-path changed-file)))))

			 (cond (change-records
				(let ((probe (file-system/probe-file file-system current-name)))
				  (cond ((null probe)

					 ;; if file is missing, just re-create it
					 (file-system/note-progress report-file-system
								    (format nil "  + ~s is missing, it will be restored."
									    (user-file-name current-name))
								    (work-factor))
					 (unless report-only
					   (after
					    (rfm-file/publish changed-file file-system
                                                              :when-overwriting-file (publish/backup-if-overwriting)
                                                              :filename current-name
                                                              :publish-read-only? nil))))
					((not (after (rfm::file-content-changed? changed-file file-system probe)))
					 (file-system/note-progress report-file-system
								    (format nil "  = ~s is already up to date."
									    (user-file-name current-name))
								    (work-factor)))
					((eql (file-descriptor/content-type probe) :binary)
					 (setq binary-merge t)
					 (file-system/note-progress report-file-system
								    (format nil " *  ~s is binary and cannot be merged.  ~
                                                                                  It will be UNCO'd."
									    (user-file-name current-name))
								    (work-factor))
					 (unless report-only
					   (subsystem-satellite/uncheckout-file (car change-records)
                                                                                change-context
                                                                                satellite-repository
                                                                                file-system
                                                                                nil));; not no-copy (make backup)
					 )
					(t
					 (let ((terminator (file-descriptor/record-terminator probe)))
					   (multiple-value-bind (merged-file-contents merge-conflict-count)
					       ;; Three way merge.
					       (csf/server::call-with-file-descriptor-content probe terminator
                                                                                              (lambda (file-record-stream)
                                                                                                (rsdiff-3-way-merge
                                                                                                 (before (file-text-contents changed-file))
                                                                                                 (after  (file-text-contents changed-file))
                                                                                                 file-record-stream
                                                                                                 :conflict-marker-left-begins
                                                                                                 (format nil "----- Merge conflict, changes in database follow -----")
                                                                                                 :conflict-marker-left-ends-right-begins
                                                                                                 (format nil "----- Changes in workspace follow -----")
                                                                                                 :conflict-marker-right-ends
                                                                                                 (format nil "----- End of merge conflict -----"))))
					     (setq performed-three-way-merge t)
					     (incf total-merge-conflicts merge-conflict-count)

					     (file-system/note-progress
					      report-file-system
					      (format nil " *~:[C~;m~] ~s has changes, merge ~
                                                             ~:[detected ~d conflict~p~;succeeded~*~*~]."
						      (zerop merge-conflict-count)
						      (user-file-name current-name)
						      (zerop merge-conflict-count)
						      merge-conflict-count
						      merge-conflict-count)
					      (work-factor))
					     (unless report-only
					       ;; Save user's changes.
					       (file-system/backup file-system current-name :pattern "USER~@[~d~]")
					       ;; Plop down new version.
					       (after
						(rfm-file/publish changed-file file-system
                                                                  :when-overwriting-file (publish/backup-if-overwriting)
                                                                  :filename (push-pathname-type current-name "REPOSITORY")
                                                                  :publish-read-only? t))
					       ;; Save the ancestor version (if in-house) so we can debug strange merges
					       (when (running-development-changesafe-server-p)
						 (before
						  (rfm-file/publish changed-file file-system
                                                                    :when-overwriting-file (publish/backup-if-overwriting)
                                                                    :filename (push-pathname-type current-name "ANCESTOR")
                                                                    :publish-read-only? t)))
					       ;; Put in the merged version.
					       (with-file-system-stream (stream file-system current-name
                                                                                :direction :output
                                                                                :record-terminator terminator
                                                                                :element-type 'character
                                                                                :if-exists :supersede
                                                                                :if-does-not-exist :create)
						 (vi-stream-for-each (lambda (line)
                                                                       (file-system/write-line file-system stream line))
								     merged-file-contents)))))))))

			       (rename-records
				(let ((new-pathname (cc-filerename/new-pathname (car rename-records))))
				  (file-system/note-progress report-file-system
							     (format nil "    ~s will be updated."
								     (user-file-name new-pathname))
							     (work-factor))
				  (unless report-only
				    (after
				     (rfm-file/publish changed-file file-system
                                                       :when-overwriting-file #'publish/supersede-if-overwriting
                                                       :filename new-pathname
                                                       :publish-read-only? t)))))
			       (t
				(after
				 (file-system/note-progress report-file-system
							    (format nil "    ~s will be updated."
								    (user-file-name
								     (rfm::file-system-element/relative-path changed-file)))
							    (work-factor))
				 (unless report-only
				   (rfm-file/publish changed-file file-system
                                                     :when-overwriting-file #'publish/supersede-if-overwriting
                                                     :publish-read-only? t)))))))))
		 (work-tick)))
             (values
              ;; return values
              performed-three-way-merge
              total-merge-conflicts
              binary-merge))))))))

#||


     (lambda (cm-transaction)



  ;; Can't use subsystem satellite funcall in context because we're switching between
  ;; two contexts.
  (macrolet ((before (&body body)
	       `(WITH-CM-SATELLITE-METAVERSION (OLD-TIMESTAMP)
		  (WITH-VERSION (SATELLITE-VERSION
				 :CSET-DIDS-TO-ADD    VPB-OLD-ADDED-SATELLITE-CSET-DIDS
				 :CSET-DIDS-TO-REMOVE VPB-OLD-REMOVED-SATELLITE-CSET-DIDS)
		    ,@body)))

	     (after (&body body)
	       `(WITH-CM-SATELLITE-METAVERSION (NEW-TIMESTAMP)
		  (WITH-VERSION (SATELLITE-VERSION
				 :CSET-DIDS-TO-ADD    VPB-NEW-ADDED-SATELLITE-CSET-DIDS
				 :CSET-DIDS-TO-REMOVE VPB-NEW-REMOVED-SATELLITE-CSET-DIDS)
		    ,@body))))

    ;; All the merge logic is in this function.

    ;; We have the BEFORE state, the AFTER state, and potentially the current
    ;; change-context state to deal with.  If the change-context in NULL, it is a simple update,
    ;; but if we do have a change context, it is a `merge with common ancestor'.

    ;; This code *always* succeeds.  I.E.  the workspace and change-context must always
    ;; have the new changes reflected within them after this is run.  This is because the update
    ;; runs on a subsystem by subsystem basis, and to abort in the middle would leave the
    ;; workspace in an inconsistent state.

    ;; In cases where conflict occurs and it is not obvious what to do, we try to do the
    ;; following:
    ;;   1.  Always incorporate repository changes.
    ;;   2.  Preserve any user changes.
    ;;
    ;; Rule 1 ensures that checking in after an update (even if the update causes conflicts)
    ;;   will preserve the most recent changes in the repository.
    ;;
    ;; Rule 2 ensures that the user can reconstruct his pre-update state.  Note that Rule 1
    ;;   is more important than rule 2, so in the case of unresolvable conflicts, the user's
    ;;   files would have to be renamed.  This makes it more difficult for the user to back
    ;;   out of the update, but that's ok.
    ;;
    ;; If necessary, this code will modify the user's change context.  The following applies:
    ;;
    ;;     If a user deletes a file that has been deleted in the repository, the deletion
    ;;     request is removed from the user's change-context.
    ;;
    ;;     If a user renames a file that has been deleted in the repository, the rename
    ;;     request is removed from the user's change-context.  The file on disk will be
    ;;     renamed with a .DELETED extension.
    ;;
    ;;     If a user modifies a file that has since been deleted, the
    ;;     modified file is UNCO'd (un checked out) and renamed with a
    ;;     .DELETED extension.  (If the user really wants to change the
    ;;     file, he will have to manually remove the change set that
    ;;     deleted the file and re-merge in his changes.)
    ;;
    ;;     If a user renames a file that has been renamed in the
    ;;     repository, the user's rename will take precedence.
    ;;
    ;;     If a user renames a file such that it collides with a new
    ;;     file from the repository, the new file takes precedence.
    ;;     The disk file is renamed with a .RENAMED extension, and the
    ;;     rename request in the change context is modified to reflect
    ;;     this.
    ;;
    ;;     If a user renames a file such that it collides with a
    ;;     renamed file from the repository, the repository file takes
    ;;     precedence.  The disk file is renamed with a .RENAMED extension,
    ;;     and the rename request in the change context is modified to reflect
    ;;     this.
    ;;
    ;;     If a user adds a file such that it collides with a file
    ;;     added in the repository, *and the added file is identical
    ;;     to the one in the repository*, the user's change context is
    ;;     modified to remove the file_add.
    ;;
    ;;     If a user adds a file such that it collides with a file
    ;;     added in the repository, *and the added file is different
    ;;     from the one in the repository*, the user's disk file is
    ;;     renamed with an .ADDED extension, and the user's change
    ;;     context is modified to remove the file_add.
    ;;
    ;;     Files that have been renamed and modified are dealt with
    ;;     under their *new* name.
    ;;
    ;;     If both the user and the repository have modifications to a
    ;;     file, and the results are identical, the user's file is
    ;;     unco'd.
    ;;
    ;;     If both the user and the repository have modifications to a
    ;;     file, and the results are different, a three-way merge is
    ;;     performed.   If the changes are in different sections of
    ;;     the file, the merge is `successful'.  If the change
    ;;     overlap, the merge has `conflicts', and manual merging must
    ;;     be done.  Conflicting merges will have markers placed in
    ;;     the file to indicate where the problems are.

	  (let* ((project (repository-resolve-distributed-identifier
			   satellite-repository (subsystem-satellite-project-did subsystem)))
		 (branch (repository-resolve-distributed-identifier
			  satellite-repository (subsystem-satellite-project-branch-did subsystem)))
		 (satellite-version (branch-get-most-recent-version branch)) ;; do not rename, macro uses it.

		 (relevant-changes
		  (cid-set-exclusive-or
		   (before (txn-context-cid-set *txn-context*))
		   (after  (txn-context-cid-set *txn-context*))))

		 (satellite-root-directory (after (rfm-project-root-directory project)))

		 (old-files (before
			     ;;(transitive-closure satellite-root-directory #'cons '())
			     (file-system-element-descendants satellite-root-directory)
			     ))
		 (new-files (after
			     ;; (transitive-closure satellite-root-directory #'cons '())
			     (file-system-element-descendants satellite-root-directory)
			     ))

		 (removed-files (set-difference old-files new-files))
		 (added-files   (set-difference new-files old-files))
		 (common-files  (intersection   new-files old-files))
		 (renamed-files nil)
		 (changed-files nil)

		 total-work
		 (current-work 0)
		 (last-work-factor 0)

		 ;; return values
		 (performed-three-way-merge nil)
		 (total-merge-conflicts       0)
		 (binary-merge              nil)
		 )
	    (tail-labels ((work-factor ()
			    (* 5 (floor (* (/ current-work total-work) 20.))))

			  (work-tick ()
			    (incf current-work)
			    (unless (= (work-factor) last-work-factor)
			      (setq last-work-factor (work-factor))
			      (file-system-note-progress report-file-system nil (work-factor))))

			  (user-file-name (pathname)
			    (pathname->platform-namestring
			     (decode-pathname pathname)
			     (file-system-platform file-system)))

			  (sort-files (filelist)
			    (sort filelist #'string-lessp
				  :key (lambda (rfm-file)
					   (namestring (rfm::file-system-element-relative-path rfm-file))))))

	      ;; This is a little funny because it is much better to extract the old names and new names
	      ;; together rather than incrementally as we map across the common-files.
	      (let ((old-common-file-names (before (mapcar #'rfm::file-system-element-relative-path common-files)))
		    (new-common-file-names (after  (mapcar #'rfm::file-system-element-relative-path common-files))))
		(mapc (lambda (common-file old-name new-name)
			  (let ((changed? (cond ((typep common-file 'rfm-file)
						 ;; A file is changed if the contents are changed, but
						 ;; name changes are irrelevant, so we look for the magic
						 ;; slots in the file.
						 (or (cid-set-intersection?
						      relevant-changes
						      (versioned-object-slot-get-cid-set common-file
											 'rfm::mime-content))
						     (cid-set-intersection?
						      relevant-changes
						      (versioned-object-slot-get-cid-set common-file
											 'rfm::binary-contents))
						     (cid-set-intersection?
						      relevant-changes
						      (versioned-object-slot-get-cid-set common-file
											 'rfm::text-contents))))
						(t (cid-set-intersection?
						    relevant-changes
						    (versioned-object-get-cid-set common-file))))))
			    (unless (equal old-name new-name)
			      (push (list common-file old-name new-name) renamed-files))
			    (when changed?
			      (push common-file changed-files))))
		      common-files old-common-file-names new-common-file-names))

	      ;; By sorting the files, we make the process handle the
	      ;; files in lexicographic order.  This makes it easier
	      ;; for the user to understand the report because it is
	      ;; easier to find particular files.

	      (setq added-files   (after  (sort-files added-files)))
	      (setq removed-files (before (sort-files removed-files)))
	      (setq changed-files (after  (sort-files changed-files)))
	      (setq renamed-files (after  (sort renamed-files #'string-lessp
						:key (lambda (tuple)
							 (namestring (third tuple))))))

	      ;; Estimate work
	      (setq total-work (+ (length removed-files)
				  (length renamed-files)
				  (length added-files)
				  (length changed-files)
				  1))

	      (when removed-files
		(debug-message 3 "Removing ~d file~:p" (length removed-files)))

	      (before
	       (dolist (removed-file removed-files)
		 (when (typep removed-file 'rfm-file)  ;; punt on removing directories
		   (let* ((name (rfm::file-system-element-relative-path removed-file))
			  (removed-file-did (distributed-object-identifier removed-file))
			  ;; Check change-context for conflicts.
			  (removal-records  (and change-context (change-context-find-file-removals change-context removed-file-did)))
			  (change-records   (and change-context (change-context-find-file-changes  change-context removed-file-did)))
			  (rename-records   (and change-context (change-context-find-file-renames  change-context removed-file-did))))

		     (cond (removal-records                    ;; user wants to remove it, too.
			    ;; He may want to remove it to make room for another file of the same name.
			    ;; If so, we better keep the removal record.  If not, we should let the
			    ;; prior removal do it.
			    (let ((add-records      (and change-context
							 (change-context-find-file-additions
							  change-context name
							  (distributed-object-identifier project))))
				  (rename-to-records (and change-context
							  (change-context-find-file-renamed-to
							   change-context name
							   (distributed-object-identifier project)))))

			      (unless (or add-records        ;; file appearing.
					  rename-to-records  ;; no old file renamed to this.
					  report-only)       ;; really want it deleted.
				;; User is doing redundant delete, get rid of it.
				;; Note that since the user deleted the file, it must not be on the
				;; disk, so we don't have to delete the file here as well.
				(change-context-remove-file-undo change-context (car removal-records)))))

			   ;; Otherwise, it should still be there and untouched.
			   ;; Rename it to be deleted.  We must clear the location.
			   ((or change-records rename-records)
			    (let ((current-name (if rename-records
						    (cc-filerename-new-pathname (car rename-records))
						  name)))
			      (file-system-note-progress
			       report-file-system
			       (format nil " *- ~s is obsolete and will be~:[~; UNCO'd~]~:[~; and~]~:[~; un-renamed~]."
				       (user-file-name current-name)
				       change-records
				       (and change-records rename-records)
				       rename-records)
			       (work-factor))

			      (unless report-only
				;; Modify change context.
				(when change-records
				  (change-context-change-file-undo change-context (car change-records)))
				(when rename-records
				  (change-context-rename-file-undo change-context (car rename-records)))
				(when (file-system-probe-file file-system current-name) ;; if it isn't on the disk, punt
				  ;; rename it to a `deleted' file.
				  (file-system-backup file-system current-name :pattern "DELETED~@[~d~]")))))


			   ;; Either the user didn't modify the file, or this is a two way merge.
			   ;; In either case, we want to simply delete the removed file.
			   ;; It is ok to do this because the file can be reconstructed by simply
			   ;; removing the change that created it.
			   (t
			    (file-system-note-progress report-file-system
						       (format nil "  - ~s is obsolete and will be removed."
							       (user-file-name name))
						       (work-factor))
			    (unless report-only
			      (when (file-system-probe-file file-system name) ;; if it isn't on the disk, punt
				;; Delete it.
				(file-system-delete-file file-system name :force t)))))))
		 (work-tick)))

	      (when renamed-files
		(debug-message 3 "Renaming ~d file~:p" (length renamed-files)))

	      (dolist (renamed-file renamed-files)
		(let* ((file (first renamed-file))
		       (repository-original-name (second renamed-file))
		       (repository-final-name    (third renamed-file))
		       (renamed-file-did (distributed-object-identifier file))
		       ;; user may have renamed, removed, or changed it.
		       (rename-records  (and change-context (change-context-find-file-renames  change-context renamed-file-did)))
		       (removal-records (and change-context (change-context-find-file-removals change-context renamed-file-did)))
		       ;; User may have added or renamed a different file to same target name.
		       (add-records     (and change-context
					     (change-context-find-file-additions
					      change-context repository-final-name
					      (distributed-object-identifier project))))
		       (rename-to-records (and change-context
					       (change-context-find-file-renamed-to
						change-context repository-final-name
						(distributed-object-identifier project)))))
		  (unless (or removal-records rename-records)  ;; User removed or renamed the file, no work to do.
		    (cond
		     ;; user wants to add a file with the same name, make a collision.
		     (add-records
		      (file-system-note-progress report-file-system
						 (format nil "  * ~s addition collides with repository file.  ~
                                                                  Your file will be renamed."
							 (user-file-name (cc-fileadd-pathname (car add-records))))
						 (work-factor))
		      (unless report-only
			(let ((new-name (file-system-backup file-system repository-final-name :pattern "ADDED~@[~d~]")))
			  (setf (cc-fileadd-pathname (car add-records)) new-name))))
		     ;; User wants to rename a different file to this name.
		     (rename-to-records
		      (file-system-note-progress report-file-system
						 (format nil "  * ~s rename collides with repository file.  ~
                                                                  Your file will be renamed."
							 (user-file-name (cc-fileadd-pathname (car add-records))))
						 (work-factor))
		      (unless report-only
			(let ((new-name (file-system-backup file-system repository-final-name :pattern "ADDED~@[~d~]")))
			  (setf (cc-filerename-new-pathname (car rename-to-records)) new-name))))
		     (t nil))
		    ;; Ok, the destination should be clear.
		    ;; We can (in theory) just rename the file.
		    (let ((probe-old (file-system-probe-file file-system repository-original-name))
			  (probe-new (file-system-probe-file file-system repository-final-name)))

		      (when probe-new
			(file-system-note-progress report-file-system
						   (format nil "    ~s is a wild file.  It will be backed up."
							   (user-file-name (file-descriptor-path probe-new)))
						   (work-factor)))
		      (file-system-note-progress report-file-system
						 (format nil "  R ~s will be renamed to ~s"
							 (user-file-name repository-original-name)
							 (user-file-name repository-final-name))
						 (work-factor))
		      (unless report-only
			(if probe-old
			    (progn
			      (when probe-new
				(file-system-backup file-system (file-descriptor-path probe-new) :pattern "WILD~@[~d~]"))
			      (file-system-rename file-system repository-original-name repository-final-name))
			  ;; but if old file is missing, re-extract it.
			  (after
			   (rfm-file/publish file file-system (publish-backup-if-overwriting "WILD~@[~d~]")
					 :publish-read-only? t))))))
		  (work-tick)))

	      ;; Ensure directory structure exists!
	      (after
	       (dolist (added-directory
			   ;; Filter and combine.
			   (remove-duplicates
			    (mapcar (lambda (file)
					(if (typep file 'rfm-directory)
					    file
					  (rfm::file-system-element-directory file)))
				    added-files)))

		 (unless (or (eq added-directory satellite-root-directory)
			     (file-system-probe-file file-system (rfm::file-system-element-relative-path added-directory)))
		   (file-system-note-progress report-file-system
					      (format nil "  + ~s (directory) will be created."
						      (user-file-name
						       (rfm::file-system-element-relative-path added-directory)))
					      (work-factor))
		   (unless report-only
		     (file-system-ensure-directory-and-parents file-system
							       (rfm::file-system-element-relative-path added-directory))))))

	      (when added-files
		(debug-message 3  "Adding ~d file~:p" (length added-files)))

	      ;; At this point, files that are new in the repository should be safe to add.
	      ;; But we need to ensure that the user isn't adding them as well.

	      (after
	       (dolist (added-file added-files)
		 (when (typep added-file 'rfm-file)
		   (let* ((name (rfm::file-system-element-relative-path added-file))
			  (add-records     (and change-context
						(change-context-find-file-additions
						 change-context name
						 (distributed-object-identifier project))))
			  (rename-to-records (and change-context
						  (change-context-find-file-renamed-to
						   change-context name
						   (distributed-object-identifier project))))
			  (colliding-file (file-system-probe-file file-system name))
			  (same? (and colliding-file
				      (not (rfm::file-content-changed? added-file file-system colliding-file)))))

		     (cond ((and add-records same?) ;; user re-added the file
			    (file-system-note-progress report-file-system
						       (format nil " *= ~s already added, will undo file_add." (user-file-name name))
						       (work-factor))
			    (unless report-only
			      (change-context-add-file-undo-no-log change-context (car add-records))))

			   (add-records ;; user added different file
			    (file-system-note-progress report-file-system
						       (format nil " *+ ~s collides with repository file.  ~
                                                                     Your file will be renamed. The file_add will be undone."
							       (user-file-name name))
						       (work-factor))
			    (unless report-only
			      (change-context-add-file-undo-no-log change-context (car add-records))
			      (rfm-file/publish added-file file-system (publish-backup-if-overwriting "ADDED~@[~d~]")
					    :publish-read-only? t)))

			   (rename-to-records ;; user renamed a file to this name
			    (file-system-note-progress report-file-system
						       (format nil " *+ ~s collides with repository file.  ~
                                                                     Your file will be renamed. The file_rename will modified."
							       (user-file-name name))
						       (work-factor))
			    (unless report-only
			      (let ((new-name (file-system-backup file-system name :pattern "RENAMED~@[~d~]")))
				(setf (cc-filerename-new-pathname (car rename-to-records)) new-name)
				;; (file-system-note-progress report-file-system
				;;                         (format nil "   Renaming ~s to ~s"
				;;                                 (user-file-name name)
				;;                                 (user-file-name new-name))
				;;                         (work-factor))
				(rfm-file/publish added-file file-system (publish-backup-if-overwriting "RENAMED~@[~d~]")
					      :publish-read-only? t))))

			    (t
			     (when colliding-file
			       (file-system-note-progress report-file-system
							  (format nil " *+ ~s is wild.  It will be backed up."
								  (user-file-name name))
							  (work-factor)))
			     (file-system-note-progress report-file-system
							(format nil "  + ~s will be added to your workspace."
								(user-file-name name))
							(work-factor))
			     (unless report-only
			       (rfm-file/publish added-file file-system
					     ;; We can encounter a `wild file' during publishing if the user has
					     ;; added a cset that adds the file.  HP says, just back it up.
					     (publish-backup-if-overwriting "WILD~@[~d~]")
					     :publish-read-only? t
					     ))))))
		   (work-tick)))

	       ;; Now deal with the changed files.
	       ;; The files, even if they are renamed, should be in the right place (right?).
	       (dolist (changed-file changed-files)
		 (when (typep changed-file 'rfm-file)
		   (let* ((changed-file-did (distributed-object-identifier changed-file))
			  (remove-records  (and change-context (change-context-find-file-removals change-context changed-file-did))))
		     (unless remove-records ;; changed in repository but user is deleting it.  Just ignore it.
		       (let* ((change-records (and change-context (change-context-find-file-changes change-context changed-file-did)))
			      (rename-records (and change-context (change-context-find-file-renames change-context changed-file-did)))
			      (current-name (if rename-records
						(cc-filerename-new-pathname (car rename-records))
					      (after (rfm::file-system-element-relative-path changed-file)))))

			 (cond (change-records
				(let ((probe (file-system-probe-file file-system current-name)))
				  (cond ((null probe)

					 ;; if file is missing, just re-create it
					 (file-system-note-progress report-file-system
								    (format nil "  + ~s is missing, it will be restored."
									    (user-file-name current-name))
								    (work-factor))
					 (unless report-only
					   (after
					    (rfm-file/publish changed-file file-system
							  (publish-backup-if-overwriting)
							  :filename current-name
							  :publish-read-only? nil))))
					((not (after (rfm::file-content-changed? changed-file file-system probe)))
					 (file-system-note-progress report-file-system
								    (format nil "  = ~s is already up to date."
									    (user-file-name current-name))
								    (work-factor)))
					((eql (file-descriptor-content-type probe) :binary)
					 (setq binary-merge t)
					 (file-system-note-progress report-file-system
								    (format nil " *  ~s is binary and cannot be merged.  ~
                                                                                  It will be UNCO'd."
									    (user-file-name current-name))
								    (work-factor))
					 (unless report-only
					   (subsystem-satellite-uncheckout-file (car change-records)
								  change-context
								  satellite-repository
								  file-system
								  nil)) ;; not no-copy (make backup)
					 )
					(t
					 (let ((terminator (file-descriptor-record-terminator probe)))
					   (multiple-value-bind (merged-file-contents merge-conflict-count)
					       ;; Three way merge.
					       (csf/server::call-with-file-descriptor-content probe terminator
						 (lambda (file-record-stream)
                                                   (rsdiff-3-way-merge
                                                    (before (file-text-contents changed-file))
                                                    (after  (file-text-contents changed-file))
                                                    file-record-stream
                                                    :conflict-marker-left-begins
                                                    (format nil "----- Merge conflict, changes in database follow -----")
                                                    :conflict-marker-left-ends-right-begins
                                                    (format nil "----- Changes in workspace follow -----")
                                                    :conflict-marker-right-ends
                                                    (format nil "----- End of merge conflict -----"))))
					     (setq performed-three-way-merge t)
					     (incf total-merge-conflicts merge-conflict-count)

					     (file-system-note-progress
					      report-file-system
					      (format nil " *~:[C~;m~] ~s has changes, merge ~
                                                             ~:[detected ~d conflict~p~;succeeded~*~*~]."
						      (zerop merge-conflict-count)
						      (user-file-name current-name)
						      (zerop merge-conflict-count)
						      merge-conflict-count
						      merge-conflict-count)
					      (work-factor))
					     (unless report-only
					       ;; Save user's changes.
					       (file-system-backup file-system current-name :pattern "USER~@[~d~]")
					       ;; Plop down new version.
					       (after
						(rfm-file/publish changed-file file-system
							      (publish-backup-if-overwriting)
							      :filename (push-pathname-type current-name "REPOSITORY")
							      :publish-read-only? t))
					       ;; Save the ancestor version (if in-house) so we can debug strange merges
					       (when (running-development-changesafe-server-p)
						 (before
						  (rfm-file/publish changed-file file-system
								(publish-backup-if-overwriting)
								:filename (push-pathname-type current-name "ANCESTOR")
								:publish-read-only? t)))
					       ;; Put in the merged version.
					       (with-file-system-stream (stream file-system current-name
									 :direction :output
									 :record-terminator terminator
									 :element-type 'character
									 :if-exists :supersede
									 :if-does-not-exist :create)
						 (vi-stream-for-each (lambda (line)
									 (file-system-write-line file-system stream line))
								     merged-file-contents)))))))))

			       (rename-records
				(let ((new-pathname (cc-filerename-new-pathname (car rename-records))))
				  (file-system-note-progress report-file-system
							     (format nil "    ~s will be updated."
								     (user-file-name new-pathname))
							     (work-factor))
				  (unless report-only
				    (after
				     (rfm-file/publish changed-file file-system
						   #'publish-supersede-if-overwriting
						   :filename new-pathname
						   :publish-read-only? t)))))
			       (t
				(after
				 (file-system-note-progress report-file-system
							    (format nil "    ~s will be updated."
								    (user-file-name
								     (rfm::file-system-element-relative-path changed-file)))
							    (work-factor))
				 (unless report-only
				   (rfm-file/publish changed-file file-system
						 #'publish-supersede-if-overwriting
						 :publish-read-only? t)))))))))
		 (work-tick)))
	    (values
	     ;; return values
	     performed-three-way-merge
	     total-merge-conflicts
	     binary-merge))))
||#

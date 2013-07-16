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
;;;; File Name:     subsystem-satellite.lsp
;;;; Author:        Dave Tenny, Joe Marshall.
;;;; Creation Date: November 1999
;;;;
;;;; Module Description:
;;;;
;;;; Methods which specialize on SUBSYSTEM objects, but perform operations
;;;; which are in the transaction and version context of a satellite repository
;;;; rather than in the context of the master repository.
;;;;
;;;; These methods are sensitive to the type of version context required
;;;; to examine subsystem slots.  Non-versioned slots don't care about the
;;;; active cid-set in the satellite transaction.  But versioned slots
;;;; care.   A lot.  For routines which look at versioned slots, they may
;;;; require a MASTER-TXN-CONTEXT binding to the transaction context which was
;;;; established in WITH-CM-MASTER-TXN (implicitly *TXN-CONTEXT*).
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CONMAN")

;;; API's exported from this module.
(eval-when (:load-toplevel :execute)
  (export '(subsystem-satellite-extract-files-to-disk
	    subsystem-satellite-extract-changed-files-to-disk
	    with-subsystem-satellite-metaversion-context
	    subsystem-satellite-metaversion-context
	    subsystem-satellite-funcall-in-context
	    subsystem-satellite-promote-csets
	    subsystem-satellite-uncheckout-file
	    subsystem-satellite-get-cids
	    subsystem-satellite-map-over-transactions
	    subsystem-satellite-map-over-transactions-with-filter
	    subsystem-satellite-map-over-transaction-affects)))

(defun subsystem-satellite/extract-files-to-disk (subsystem satellite-repository parent-file-system
                                                            satellite-project satellite-branch
                                                            &key
                                                            create?
                                                            when-overwriting-directory
                                                            when-overwriting-file
                                                            (report-file-system parent-file-system)
                                                            (publish-read-only? t)
                                                            (clean t)
                                                            unpublish?

;                                                 subsystem satellite-repository parent-file-system
;						  metaversion-timestamp master-metaversion-cid-set
;						  overwrite-directory-function
;						  overwrite-file-function
;						  &key subsystem-file-alist
                                                            )
  (check-type subsystem subsystem)
  (check-type satellite-project satellite-project)
  (check-type satellite-branch satellite-subsystem)

  ;; Ensure that the directory hierarchy up to the subsystem root exists.
  (file-system/note-progress parent-file-system (format nil "Publishing branch ~s" subsystem) nil)
  (file-system/ensure-directory-and-parents
   parent-file-system
   (subsystem/subdirectory subsystem))
  (satellite-subsystem/publish-to-file-system satellite-project satellite-branch
                                              (logical-file-system/change-directory
                                               parent-file-system
                                               (subsystem/subdirectory subsystem))
                                              :create? create?
                                              :publish-read-only? publish-read-only?
                                              :report-file-system report-file-system
                                              :unpublish? unpublish?
                                              :when-overwriting-directory when-overwriting-directory
                                              :when-overwriting-file when-overwriting-file))

(defun subsystem-satellite/extract-file-names-to-list
    (branch

;     subsystem satellite-repository
;     metaversion-timestamp master-metaversion-cid-set
;     &key subsystem-file-alist
;	  VPB-added-satellite-cset-dids
;	  VPB-removed-satellite-cset-dids
;	  no-directories
          )
  ;; Should be renamed to subsystem-satellite-extract-files-to-disk.
  "Populate a subsystem's files to a list, from the appropriate project context for the subsystem.
   This is done by binding the subsystem subdirectory relative to PARENT-FILE-SYSTEM, as well as
   the appropriate project context for the subsystem.

   The entries on the list are relative pathnames.  The list will also include directories unless
   NO-DIRECTORIES is non-nil.

   METAVERSION-TIMESTAMP is used to specify the date-driven metaversion in the satellite repository
   which will govern the state of the subsystem branch and version(s) on that branch which we use.
   It should be a time-stamp object, or NIL indicating that the 'latest' metaversion is desired.

   SATELLITE-REPOSITORY must be open and in-transaction.

   SUBSYSTEM-FILE-ALIST, if specified, is an association list whose keys are subsystems, and whose
   values are file DIDS in that subsystem to be extracted.  Subsystem keys which don't match the SUBSYSTEM
   argument are ignored. (*FINISH*/*TBD*: dids aren't enough if the file could appear in multiple
   subdirectories, would need to distinguish by subsys-relative pathname.)  Nil means use them all.

   MASTER-METAVERSION-CID-SET should be the master repository metaversion cid-set which is necessary to
   view versioned content of master subsystem objects while we grovel around in satellite repositories.

   VPB-{added,removed}-satellite-cset-dids, if specified, are lists of change-set (DID)
   additions/deletions to be made to the version context in viewing satellite repository
   content.  These elements are part of the user's virtual private branch in the
   workspace.

   **** WARNING ****
   See above paragraph.  We're in a satellite repository context for this method!

   This routine is a bit like MASTER-CATALOG-EXTRACT-PC-FILE-NAMES-TO-LIST ,
   except that we assume the satellite repository is open and in-transaction already,
   and we're only processing one subsystem of a PC, not all subsystems.

   Return the list of files.

   See also subsystem-satellite-extract-pc-files-to-disk (from which this was adapted)."

  (satellite-subsystem/publish-to-file-list branch ))

(defun subsystem-satellite/promote-csets (satellite-repository subsystem
                                                    &key 
                                                    added-satellite-change-sets
                                                    removed-satellite-change-sets
                                                    (satellite-subsystem-version (branch/most-recent-version subsystem))
                                                     promote-current-cid)
  "Effect the addition or removal of satellite change-set objects into the satellite branch
   for SUBSYSTEM.

   Note that immediate master repository recording of the satellite change-set isn't possible
   if PROMOTE-CURRENT-CID is specified, because we don't have the satellite cid-set yet.
   In this case, we have to use satellite or master repository close hooks.  Even if we do know the
   change-set name, updating the master repository in the context of a satellite txn is a bitch
   (caching, current repository, etc.)

   Given these facts of life:

   ************************

   ALL CSET CHANGES TO THE SUBSYSTEM IN THE MASTER REPOSITORY ARE DEFERRED UNTIL MASTER
   TRANSACTION CLOSE, AND ARE PRESENT IN THE *SUBSYSTEM-CSET-UPDATES* VARIABLE (ENSCAPSULATED VIA
   APPROPRIATE APIS) UNTIL SUCH TIME AS THE MASTER CATALOG TRANSACTION CLOSE HOOK EXECUTES.

   ************************

   The added/removed cset lists must contain satellite repositiory CHANGE-SET objects, or be empty.

   PROMOTE-CURRENT-CID, if specified, indicates we want to promote the cid in progress, for which
   a change-set does not yet exist.  We don't preclude specifying this with the added and removed
   satellite change-set lists, but this isn't usually the case for conman.

   We assume that we're in the transaction context of a satellite repository.

   Provide SATELLITE-SUBSYSTEM-VERSION if you already have it (though it better be the master-cid-set
   view of the latest version!), otherwise we'll figure it out.

   Returns NIL."
  (assert (eq *repository* satellite-repository)) ;sanity check

  ;; Okay, promote cids into the satellite versions
  (when promote-current-cid
    ;; Update the satellite branch version
    (version/promote-current-cid satellite-subsystem-version))

  (when (or added-satellite-change-sets removed-satellite-change-sets)
    ;; Update satellite version
    (version/add-and-remove-change-sets
     satellite-subsystem-version added-satellite-change-sets removed-satellite-change-sets))
  nil)

(defun subsystem-satellite/project-branch-did (subsystem)
  (project-reference/branch-did subsystem))

#||
(defmacro with-subsystem-satellite-metaversion-context
    ((subsystem satellite-repository metaversion-timestamp
      &key (project (gensym (symbol-name :WITH-SUBSYSTEM-SATELLITE-CONTEXT-PROJECT-)))
	   (branch (gensym (symbol-name :WITH-SUBSYSTEM-SATELLITE-CONTEXT-BRANCH-)))
	   (version (gensym (symbol-name :WITH-SUBSYSTEM-SATELLITE-CONTEXT-VERSION-)))
	   (satellite-metaversion-cid-set (gensym (symbol-name :WITH-SUBSYSTEM-SATELLITE-CONTEXT-CID-SET-))))
     &body body)

  "Given an open transaction on SATELLITE-REPOSITORY for a given SUBSYSTEM,
   Bind any speficied keyword variables to appropriately meta-versioned views of:
   1) The satellite PROJECT object to which subsystem maps.
   2) The satellite BRANCH object to which subsystem maps.
   3) The satellite VERSION which is active on BRANCH.
   4) The actual metaversion cid-set it self, a CID-SET object.

   METAVERSION-TIMESTAMP must be nil, or a time-stamp object.  If NIL, we look at the 'latest'
   values for all versioned information bound in the variables.  If a time-stamp, we
   look at versions of the information as it would have appeared at the given time.

   We execute BODY as an implicit PROGN in this metaversion context.  We do NOT bind the versioned
   subsystem content with the version contents in this macro.  (i.e. no WITH-VERSION is performed).

   This macro returns the values generated by BODY."
  `(multiple-value-bind (,project ,branch ,satellite-metaversion-cid-set)
       (subsystem-satellite-metaversion-context ,subsystem ,satellite-repository ,metaversion-timestamp)
     (declare (ignorable ,project))	; hate to do this, but user can't with BODY...
     ;; We don't retrieve the version in the helper routine to save on having to bind the
     ;; metaversion cid-set twice.
     (with-txn-context-cid-set (*txn-context* ,satellite-metaversion-cid-set)
       (let ((,version (branch-get-most-recent-version ,branch)))
	 ,@body))))

(defun subsystem-satellite-metaversion-context (subsystem satellite-repository metaversion-timestamp)
  "Return three values given a SUBSYSTEM object and a SATELLITE-REPOSITORY which is in transaction
   and represents that SUBSYSTEM.  This routine is really a helper for
   WITH-SUBSYSTEM-SATELLITE-METAVERSION-CONTEXT but might be useful in other places.

   METAVERSION-TIMESTAMP must be nil, or a time-stamp object used to derive the correct satellite repository
   metaversion for that time.

   The three values returned are:

   1) The satellite PROJECT object to which subsystem maps.
   2) The satellite BRANCH object to which subsystem maps.
   3) The metaversion cid-set corresponding to METAVERSION-TIMESTAMP.

   We do not return the VERSION on the branch, since it requires binding the cid-set, and we
   let the caller do that for now."
  (assert (or (null metaversion-timestamp)
	      (time-stamp? metaversion-timestamp)))
  (let ((satellite-repository-metaversion
	 (if metaversion-timestamp
	     (repository-master-cid-set satellite-repository :end-time metaversion-timestamp)
	   ;; We could use (txn-context-cid-set *txn-context*) for the satellite latest master cid-set
	   ;; (which is more efficient than consing a new cid-set) except that the user may need to MODIFY
	   ;; the cid-set.  So we must cons/copy a new one here.  The copy is more efficient
	   ;; than recomputing the repository-master-cid-set (where master here is a generic
	   ;; repository 'master', not a conman 'master'.
	   (cid-set-copy (txn-context-cid-set *txn-context*))))
	;; This resolution logic is uncomfortably copied from subsystem-satellite-funcall-in-context
	(satellite-project (repository-resolve-distributed-identifier
			    satellite-repository (Subsystem-satellite-project-did subsystem)))
	(satellite-branch (repository-resolve-distributed-identifier
			   satellite-repository (subsystem-satellite-project-branch-did subsystem))))
    (values satellite-project satellite-branch satellite-repository-metaversion)))

(defun subsystem-satellite-funcall-in-context (subsystem satellite-repository function
					       &key VPB-added-satellite-cset-dids
						    VPB-removed-satellite-cset-dids)
  ;; ** NOTE ** We probably really want a WITH-SUBSYSTEM-VERSION-CONTEXT macro which resembles
  ;; WITH-SUBSYSTEM-METAVERSION-CONTEXT except that it expects to be executed within that context,
  ;; and therefore is concerned only with binding the WITH-VERSION content view and obtaining
  ;; the directory variable.  So this function and its callers in the current form are somewhat
  ;; dated.
  "Derive the appropriate branch view of the satellite project described by SUBSYSTEM
   and invoke FUNCTION with the following arguments:

   (1) The root rfm-directory of the project

   What is perhaps more interesting is that the appropriate versioned view is in context
   for viewing the rfm-directory when FUNCTION is called.

   SUBSYSTEM is the subsystem in the master repository whose contents are to be processed.
   (The master repository is therefore both open and in a transaction).

   SATELLITE-REPOSITORY is the satellite repository which contains the subsystem project
   and which is both open and in a transaction.  It is an error if there is no transaction active
   on satellite-repository.

   VPB-{added,removed}-satellite-cset-dids, if specified, are lists of change-set (DID)
   additions/deletions to be made to the version context in viewing satellite repository
   content.  These elements are part of the user's virtual private branch in the
   workspace.

   NOTE: the caller is responsible for establishing the correct metaversion for examining the
   satellite repository view of the subsystem branch and version(s) on it.

   Return value: NIL"
  (let* ((project (repository-resolve-distributed-identifier
		   satellite-repository (subsystem-satellite-project-did subsystem)))
	 (branch (repository-resolve-distributed-identifier
		  satellite-repository (subsystem-satellite-project-branch-did subsystem)))
	 (version (branch-get-most-recent-version branch)))
    ;; Invoke function with the appropriate versioned satellite project context
    ;; and file systems.
    (with-version (version
		   :cset-dids-to-add VPB-added-satellite-cset-dids
		   :cset-dids-to-remove VPB-removed-satellite-cset-dids)
      (funcall function (rfm-project-root-directory project)))))

(defun subsystem-satellite-extract-files-to-disk (subsystem satellite-repository parent-file-system
						  metaversion-timestamp master-metaversion-cid-set
						  overwrite-directory-function
						  overwrite-file-function
						  &key subsystem-file-alist
						       (report-file-system parent-file-system)
						       (read-only t)
						       (clean t)
						       VPB-added-satellite-cset-dids
						       VPB-removed-satellite-cset-dids)
  "Populate a subsystem's files to disk, by binding the subsystem subdirectory
   relative to PARENT-FILE-SYSTEM, as well as the appropriate project context for the subsystem.

   METAVERSION-TIMESTAMP is used to specify the date-driven metaversion in the satellite repository
   which will govern the state of the subsystem branch and version(s) on that branch which we use.
   It should be a time-stamp object, or NIL indicating that the 'latest' metaversion is desired.

   SATELLITE-REPOSITORY must be open and in-transaction.

   SUBSYSTEM-FILE-ALIST, if specified, is an association list whose keys are subsystems, and whose
   values are file DIDS in that subsystem to be extracted.  Subsystem keys which don't match the SUBSYSTEM
   argument are ignored. (*FINISH*/*TBD*: dids aren't enough if the file could appear in multiple
   subdirectories, would need to distinguish by subsys-relative pathname.)

   REPORT-FILE-SYSTEM, if specified, is where the publishing noise is directed to.

   CLEAN, if true, causes target directories to be cleaned out before population.

   MASTER-METAVERSION-CID-SET should be the master repository metaversion cid-set which is necessary to
   view versioned content of master subsystem objects while we grovel around in satellite repositories.

   VPB-{added,removed}-satellite-cset-dids, if specified, are lists of change-set (DID)
   additions/deletions to be made to the version context in viewing satellite repository
   content.  These elements are part of the user's virtual private branch in the
   workspace.

   **** WARNING ****
   See above paragraph.  We're in a satellite repository context for this method!

   This routine is a bit like MASTER-CATALOG-EXTRACT-PC-FILES-TO-DISK ,
   except that we assume the satellite repository is open and in-transaction already,
   and we're only processing one subsystem of a PC, not all subsystems.

   See also subsystem-satellite-extract-file-names-to-list which was cloned from this function.

   Return NIL."

  ;; Ensure that the directory hierarchy up to the subsystem root exists.
  (file-system-ensure-directory-and-parents
   parent-file-system
   (with-cm-master-metaversion (master-metaversion-cid-set)
     (subsystem-relative-subdirectory subsystem)))

  ;; Publish files, setting up the appropriate project/branch/version context
  ;; These are "subdir-file-system" relative operations.

  ;; Be sure to get the subsystem-rooted-file-system before establishing the new version context!
  (let ((subdir-file-system (with-cm-master-metaversion (master-metaversion-cid-set)
			      (subsystem-rooted-file-system subsystem parent-file-system)))
	(file-dids-we-care-about (assoc-all-values subsystem subsystem-file-alist)))
    (with-cm-satellite-metaversion (metaversion-timestamp)
      (subsystem-satellite-funcall-in-context subsystem satellite-repository
	(lambda (rfm-directory)
	    (publish-rfm-server-publish-directory-to-file-system
	     rfm-directory
	     subdir-file-system
	     overwrite-file-function
	     overwrite-directory-function
	     t				; create
	     clean			; clean
	     :read-only read-only
	     :file-list (repository-resolve-distributed-identifier-list satellite-repository
									file-dids-we-care-about)
	     :report-file-system report-file-system
	     ))
	:VPB-added-satellite-cset-dids VPB-added-satellite-cset-dids
	:VPB-removed-satellite-cset-dids VPB-removed-satellite-cset-dids)))
  nil)

(defun subsystem-satellite-extract-file-names-to-list
    (subsystem satellite-repository parent-file-system
     metaversion-timestamp master-metaversion-cid-set
     &key subsystem-file-alist
	  VPB-added-satellite-cset-dids
	  VPB-removed-satellite-cset-dids
	  no-directories)
  ;; Should be renamed to subsystem-satellite-extract-files-to-disk.
  "Populate a subsystem's files to a list, from the appropriate project context for the subsystem.
   This is done by binding the subsystem subdirectory relative to PARENT-FILE-SYSTEM, as well as
   the appropriate project context for the subsystem.

   The entries on the list are relative pathnames.  The list will also include directories unless
   NO-DIRECTORIES is non-nil.

   METAVERSION-TIMESTAMP is used to specify the date-driven metaversion in the satellite repository
   which will govern the state of the subsystem branch and version(s) on that branch which we use.
   It should be a time-stamp object, or NIL indicating that the 'latest' metaversion is desired.

   SATELLITE-REPOSITORY must be open and in-transaction.

   SUBSYSTEM-FILE-ALIST, if specified, is an association list whose keys are subsystems, and whose
   values are file DIDS in that subsystem to be extracted.  Subsystem keys which don't match the SUBSYSTEM
   argument are ignored. (*FINISH*/*TBD*: dids aren't enough if the file could appear in multiple
   subdirectories, would need to distinguish by subsys-relative pathname.)  Nil means use them all.

   MASTER-METAVERSION-CID-SET should be the master repository metaversion cid-set which is necessary to
   view versioned content of master subsystem objects while we grovel around in satellite repositories.

   VPB-{added,removed}-satellite-cset-dids, if specified, are lists of change-set (DID)
   additions/deletions to be made to the version context in viewing satellite repository
   content.  These elements are part of the user's virtual private branch in the
   workspace.

   **** WARNING ****
   See above paragraph.  We're in a satellite repository context for this method!

   This routine is a bit like MASTER-CATALOG-EXTRACT-PC-FILE-NAMES-TO-LIST ,
   except that we assume the satellite repository is open and in-transaction already,
   and we're only processing one subsystem of a PC, not all subsystems.

   Return the list of files.

   See also subsystem-satellite-extract-pc-files-to-disk (from which this was adapted)."

  ;; Publish files, setting up the appropriate project/branch/version context
  ;; These are "subdir-file-system" relative operations.

  ;; Be sure to get the subsystem-rooted-file-system before establishing the new version context!
  (let ((subdir-file-system (with-cm-master-metaversion (master-metaversion-cid-set)
			      (subsystem-rooted-file-system subsystem parent-file-system)))
	(file-dids-we-care-about (assoc-all-values subsystem subsystem-file-alist))
	(result-file-list nil)
	(subsys-file-list nil)
	(publish-file-list nil)
	(relative-subdir (with-cm-master-metaversion (master-metaversion-cid-set)
			   (subsystem-relative-subdirectory subsystem))))
    (with-cm-satellite-metaversion (metaversion-timestamp)
      (subsystem-satellite-funcall-in-context
       subsystem satellite-repository
       (lambda (rfm-directory)
	   (debug-message 5 "Publish F-List RFM-Directory: ~s~%" rfm-directory)
	   (setq publish-file-list
		 (publish-rfm-server-publish-directory-to-list
		  rfm-directory
		  subdir-file-system
		  :file-list (repository-resolve-distributed-identifier-list satellite-repository
									     file-dids-we-care-about)
		  ))
	   (dolist (fle publish-file-list)
	     (let ((rfle (merge-pathnames relative-subdir fle)))
	       (debug-message 5 "Subsys F-List ~s~%" rfle)
	       (push rfle subsys-file-list)))

	   (unless no-directories
	     (push relative-subdir subsys-file-list))

	   (setq result-file-list
		 (nconc result-file-list subsys-file-list)))
       :VPB-added-satellite-cset-dids VPB-added-satellite-cset-dids
       :VPB-removed-satellite-cset-dids VPB-removed-satellite-cset-dids))
  result-file-list))

(defun subsystem-satellite-uncheckout-file (cc-filechange change-context
			      satellite-repository
			      file-system no-copy)
  "Remove a file from the checkout list.
   The correct version of file content should be in context, it is up to the caller."
  (let* ((did (cc-filechange-file-did cc-filechange))
	 (file (repository-resolve-distributed-identifier
		satellite-repository
		did))
	 (name-changes (change-context-find-file-renames change-context did))
	 (name (if name-changes
		   (cc-filerename-new-pathname (car name-changes))
		 (file-system-element-relative-path file))))
    (debug-message 3 "Uncheckout ~s " file)
    (change-context-change-file-undo change-context cc-filechange)
    (publish-file file file-system (if no-copy
				       #'publish-overwrite-if-changed
				     ;; By specifying backup-if-changed, we avoid making a backup
				     ;; file in the case that the checked out file
				     ;; contains no changes, the typical case.
				     ;; We still make backups if the user edited the file, though.
				     (publish-backup-if-changed))
		  :filename name
		  :read-only t))
  t)

;;; The cross-reference systems gets bent out of shape
;;; on this function, leading to an incredible increase
;;; in the amount of VM that Lisp needs.
;;; So we disable it for this function.
(eval-when (:load-toplevel :compile :execute)
  (cltl1:compiler-let
      ((excl:*record-xref-info* nil))

(defun subsystem-satellite-extract-changed-files-to-disk (subsystem satellite-repository file-system
							  old-timestamp
							  new-timestamp
							  &key
							  (report-only nil)
							  (report-file-system file-system)
							  change-context
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

    (rfm:call-with-rfm-caching
      (lambda ()
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
			  (let ((changed? (cond ((typep common-file 'file)
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
		 (when (typep removed-file 'file)  ;; punt on removing directories
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
			   (publish-file file file-system (publish-backup-if-overwriting "WILD~@[~d~]")
					 :read-only t))))))
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
		 (when (typep added-file 'file)
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
			      (publish-file added-file file-system (publish-backup-if-overwriting "ADDED~@[~d~]")
					    :read-only t)))

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
				(publish-file added-file file-system (publish-backup-if-overwriting "RENAMED~@[~d~]")
					      :read-only t))))

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
			       (publish-file added-file file-system
					     ;; We can encounter a `wild file' during publishing if the user has
					     ;; added a cset that adds the file.  HP says, just back it up.
					     (publish-backup-if-overwriting "WILD~@[~d~]")
					     :read-only t
					     ))))))
		   (work-tick)))

	       ;; Now deal with the changed files.
	       ;; The files, even if they are renamed, should be in the right place (right?).
	       (dolist (changed-file changed-files)
		 (when (typep changed-file 'file)
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
					    (publish-file changed-file file-system
							  (publish-backup-if-overwriting)
							  :filename current-name
							  :read-only nil))))
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
					       (server::call-with-file-descriptor-content probe terminator
						 (lambda (file-record-stream)
						     (let ((core:*rsdiff-minimum-matched-records-for-synchronization* 2)
							   (core:*rsdiff-exclude-from-match-sync-count* #'blank-string?))
						       (rsdiff-3-way-merge
							(before (file-text-contents changed-file))
							(after  (file-text-contents changed-file))
							file-record-stream
							:conflict-marker-left-begins
							(format nil "----- Merge conflict, changes in database follow -----")
							:conflict-marker-left-ends-right-begins
							(format nil "----- Changes in workspace follow -----")
							:conflict-marker-right-ends
							(format nil "----- End of merge conflict -----")))))
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
						(publish-file changed-file file-system
							      (publish-backup-if-overwriting)
							      :filename (push-pathname-type current-name "REPOSITORY")
							      :read-only t))
					       ;; Save the ancestor version (if in-house) so we can debug strange merges
					       (when (running-development-changesafe-server-p)
						 (before
						  (publish-file changed-file file-system
								(publish-backup-if-overwriting)
								:filename (push-pathname-type current-name "ANCESTOR")
								:read-only t)))
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
				     (publish-file changed-file file-system
						   #'publish-supersede-if-overwriting
						   :filename new-pathname
						   :read-only t)))))
			       (t
				(after
				 (file-system-note-progress report-file-system
							    (format nil "    ~s will be updated."
								    (user-file-name
								     (rfm::file-system-element-relative-path changed-file)))
							    (work-factor))
				 (unless report-only
				   (publish-file changed-file file-system
						 #'publish-supersede-if-overwriting
						 :read-only t)))))))))
		 (work-tick)))
	    (values
	     ;; return values
	     performed-three-way-merge
	     total-merge-conflicts
	     binary-merge))))))
));; end of eval-when, compiler-let

(defun subsystem-satellite-remove-files-from-disk (subsystem satellite-repository parent-file-system
						   metaversion-timestamp master-metaversion-cid-set
						   &key VPB-added-satellite-cset-dids
							VPB-removed-satellite-cset-dids)
  "Remove files from the user's disk if the files exist in the repository.
   This is used to implement ws_delete."
  ;; Be sure to get the subsystem-rooted-file-system before establishing the new version context!
  (let ((subdir-file-system (with-cm-master-metaversion (master-metaversion-cid-set)
			      (subsystem-rooted-file-system subsystem parent-file-system))))
      (with-cm-satellite-metaversion (metaversion-timestamp)
	(subsystem-satellite-funcall-in-context subsystem satellite-repository
	  (lambda (rfm-directory)
	      (publish-rfm-server-publish-directory-to-file-system
	       rfm-directory
	       subdir-file-system
	       (constantly nil)         ; we don't overwrite anything.
	       (constantly nil)
	       nil			; create
	       nil			; clean
	       :unpublish? t))
	  :VPB-added-satellite-cset-dids VPB-added-satellite-cset-dids
	  :VPB-removed-satellite-cset-dids VPB-removed-satellite-cset-dids)))
  nil)

(defun subsystem-satellite-promote-csets (subsystem satellite-repository
					  added-satellite-change-sets removed-satellite-change-sets
					  &key satellite-subsystem-version promote-current-cid)
  "Effect the addition or removal of satellite change-set objects into the satellite branch
   for SUBSYSTEM.

   Note that immediate master repository recording of the satellite change-set isn't possible
   if PROMOTE-CURRENT-CID is specified, because we don't have the satellite cid-set yet.
   In this case, we have to use satellite or master repository close hooks.  Even if we do know the
   change-set name, updating the master repository in the context of a satellite txn is a bitch
   (caching, current repository, etc.)

   Given these facts of life:

   ************************

   ALL CSET CHANGES TO THE SUBSYSTEM IN THE MASTER REPOSITORY ARE DEFERRED UNTIL MASTER
   TRANSACTION CLOSE, AND ARE PRESENT IN THE *SUBSYSTEM-CSET-UPDATES* VARIABLE (ENSCAPSULATED VIA
   APPROPRIATE APIS) UNTIL SUCH TIME AS THE MASTER CATALOG TRANSACTION CLOSE HOOK EXECUTES.

   ************************

   The added/removed cset lists must contain satellite repositiory CHANGE-SET objects, or be empty.

   PROMOTE-CURRENT-CID, if specified, indicates we want to promote the cid in progress, for which
   a change-set does not yet exist.  We don't preclude specifying this with the added and removed
   satellite change-set lists, but this isn't usually the case for conman.

   We assume that we're in the transaction context of a satellite repository.

   Provide SATELLITE-SUBSYSTEM-VERSION if you already have it (though it better be the master-cid-set
   view of the latest version!), otherwise we'll figure it out.

   Returns NIL."
  (assert (eq *repository* satellite-repository)) ;sanity check
  (with-cm-satellite-metaversion ()	;make sure latest metaversion is active
    (unless satellite-subsystem-version
      (setq satellite-subsystem-version
	    (branch-get-most-recent-version
	     (repository-resolve-distributed-identifier
	      satellite-repository (subsystem-satellite-project-branch-did subsystem)))))

    ;; Okay, promote cids into the satellite versions
    (when promote-current-cid
      ;; Update the satellite branch version
      (version-promote-current-cid satellite-subsystem-version)
      )

    (when (or added-satellite-change-sets removed-satellite-change-sets)
      ;; Update satellite version
      (version-add-and-remove-change-sets
       satellite-subsystem-version added-satellite-change-sets removed-satellite-change-sets)
      ))
  nil)

(defun subsystem-satellite-get-cids (subsystem satellite-repository &key master-timestamp)
  "Retrieve a list of satellite-cids which are less recent than the MASTER-TIMESTAMP for
   the SUBSYSTEM.  If MASTER-TIMESTAMP is NIL, all cids are returned.  The order of cids in
   the returned list will be from oldest to newest.  If no cids apply, NIL will be returned.

   Must be done within a with-cm-satellite-repository and a with-cm-satellite-txn (and of
   course within a with-cm-master-repository and a with-cm-master-txn)"
  (with-cm-satellite-metaversion (master-timestamp)
    ;; use the master-timestamp to weed out everything later in time
    (let ((branch (repository-resolve-distributed-identifier
		   satellite-repository
		   (project-context-branch-did subsystem))))
      (with-version ((branch-get-most-recent-version branch))
	(let ((active-cid-set (txn-context-cid-set *txn-context*))
	      cid-list)
	  ;; loop from newest to oldest because push will reverse it
	  (loop for cid from (cid-set-last-cid active-cid-set) downto 1
		do
		(when (and (cid-set-active-cid? active-cid-set cid)
			   (or (null master-timestamp)
			       ;; the timestamp test is probably redundant (given the
			       ;; with-cm-satellite-metaversion above but it can't hurt
			       (time-stamp-less-recent?
				(nth-value 1 (repository-get-cid-information
					      satellite-repository cid))
				master-timestamp)))
		  (push cid cid-list)))
	  cid-list ;; returned result
	  )))))

(defun subsystem-satellite-map-over-transactions (subsystem satellite-repository
						  function
						  &key start-time end-time)
  "Applies FUNCTION to each satellite CSet promotion or demotion
   for SUBSYSTEM that happened between START-TIME and END-TIME
   (both are TIME-STAMPs or NIL).
   The satellite repository for the subsystem should already be open
   and an appropriate (read, mvcc) transaction started.

   FUNCTION is called for each promotion or demotion of a satellite cset,
   not once per transaction.  It's called with these arguments:

      - the CID of the satellite transaction,
      - the TIME-STAMP of the transaction,
      - one of :ADD or :REMOVE,
      - the CID-OBJECT of the satellite cset being added or removed."
  (with-subsystem-satellite-metaversion-context (subsystem
						 satellite-repository
						 nil ; metaversion timestamp
						 :version satellite-version-object)
    (let (cid-time-stamp transaction-cid)
      (flet ((filter-function (cid)
	       (setq transaction-cid cid)
	       (setq cid-time-stamp  ;; this variable is shared with the function below
		     (core::cid-master-table-cid-comparison-timestamp
		      (core::repository-cid-master-table satellite-repository) cid))
	       (time-stamp-in-range-p cid-time-stamp start-time end-time))
	     (mapping-function (add/change/delete valid-p value1 value2)
	       (flet ((do-it (add/remove cid-object)
			(funcall function transaction-cid cid-time-stamp
				 add/remove cid-object)))
		 (ecase add/change/delete
		   (:add (do-it :add value1))
		   (:delete
		    (when valid-p
		      (do-it :remove value1)))
		   (:change
		    (do-it :add value1)
		    (when valid-p
		      (do-it :remove value2)))))))
	(versioned-object-map-all-changes-of-slot satellite-version-object
						  'vm::cid-objects
						  #'filter-function
						  #'mapping-function)))))

(defun subsystem-satellite-map-over-transactions-with-filter (subsystem
							      satellite-repository
							      filter-function
							      change-function)
  "Very similar to subsystem-satellite-map-over-transactions.

   Applies CHANGE-FUNCTION to each satellite CSet promotion or demotion for SUBSYSTEM.
   The satellite repository for the subsystem should already be open
   and an appropriate (read, mvcc) transaction started.

   FILTER-FUNCTION takes 1 arg (a cid) and is called to determine if the CHANGE-FUNCTION
   should be called.

   CHANGE-FUNCTION is called for each promotion or demotion of a satellite cid,
   not once per transaction.  It's called with these arguments:

      1) the symbol :add, :delete, or :change
      2) whether or or the delete value (#3 if :delete, #4 if :change) is valid
      3) the change value (a cid-object)
      4) the delete value (a cid-object) if #2 and (#1 is :delete or #1 is :change)

   it does not return anything in particular"
  (with-subsystem-satellite-metaversion-context (subsystem
						 satellite-repository
						 nil ; metaversion timestamp
						 :version satellite-version-object)
    (versioned-object-map-all-changes-of-slot satellite-version-object
					      'vm::cid-objects
					      filter-function
					      change-function)))

(defun subsystem-satellite-map-over-transaction-affects (subsystem
							 satellite-repository
							 satellite-transaction-cid-number
							 function)
  "Maps over changes to a subsystem for a single transaction.  Otherwise it is
   similar to subsystem-satellite-map-over-transactions(-with-filter).

   SATELLITE-TRANSACTION-CID-NUMBER is the CID of a transaction on SUBSYSTEM's
   satellite branch version in SATELLITE-REPOSITORY."
  (with-subsystem-satellite-metaversion-context (subsystem
						 satellite-repository
						 nil ; metaversion timestamp
						 :version satellite-version-object)
    (let ((cid-time-stamp
	   (core::cid-master-table-cid-comparison-timestamp
	    (core::repository-cid-master-table satellite-repository)
	    satellite-transaction-cid-number)))
      (flet ((mapping-function (add/change/delete valid-p value1 value2)
	       (flet ((do-it (add/remove cid-object)
			(funcall function satellite-transaction-cid-number
				 cid-time-stamp
				 add/remove cid-object)))
		 (ecase add/change/delete
		   (:add (do-it :add value1))
		   (:delete
		    (when valid-p
		      (do-it :remove value1)))
		   (:change
		    (do-it :add value1)
		    (when valid-p
		      (do-it :remove value2)))))))
	(versioned-object-map-changed-slot satellite-version-object 'vm::cid-objects
					   satellite-transaction-cid-number
					   #'mapping-function)))))
||#

#||
(defun test-subsystem-satellite-map-over-transactions
    (subsystem-name &optional
		    (master-repository-db-name
		     (car (available-conman-master-repositories))))
  (with-cached-open-repositories ()
    (call-with-txn-mutex
     (lambda ()
	 (cmctl-boilerplate-setup master-repository-db-name
				  "cm-http-subsystem-report-ui-handler-1" nil)
	 (with-cm-master-repository (master-repository master-repository-db-name)
	   (with-cm-master-txn (master-repository master-catalog nil
						  :read-only "CONMAN subsystem report")
	     (let ((subsystem (block done
				(master-catalog-map-over-subsystems
				 master-catalog
				 (lambda (subsystem)
				     (when (string= subsystem-name
						    (subsystem-name subsystem))
				       (return-from done subsystem)))))))
	       (call-with-subsystem-satellite-repository
		master-repository-db-name master-catalog subsystem
		:satellite-txn-mode :read-only
		:reason "flux report"
		:receiver (lambda (subsystem satellite-repository)
			      (subsystem-satellite-map-over-transactions
			       subsystem satellite-repository
			       (lambda (transaction-cid cid-time-stamp
					  add/remove cid-object)
				   (format t "~&~s ~s ~s ~s"
					   transaction-cid cid-time-stamp
					   add/remove cid-object)))))))))
     *cm-http-reports-server-busy-timeout*
     #'cmctl-ro-cause-busy-redirect)))

;;; And with the database create by TEST-49A:

(test-subsystem-satellite-map-over-transactions "pcl-a")
||#


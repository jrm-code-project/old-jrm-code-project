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
;;;; Module Description:  ConMan Product Configuration information
;;;;
;;;; Product Configurations identify a release of a particular product.  A
;;;; product ultimately consists of one or more VM::PROJECT-CONTEXT
;;;; entities which specify specific versions (and branches) of projects.
;;;; However a freshly created Product Configuration object may not have
;;;; any project-contexts associated with it yet.
;;;;
;;;; Product Configurations (abbrevriated PC) keep track of parent PC's
;;;; from which they're derived, reference directories to which their
;;;; contents are deployed, and other details as described below.
;;;;
;;;; They are managed as a collection by the parent product which they represent,
;;;; and each possesses a branch namesake on the parent product.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(in-package "CONMAN")

(eval-when (:load-toplevel :execute)
  (export '(product
            product?
            product/warn-if-inactive
            )))

(proclaim (standard-optimizations))

(defclass product (project)
  (
   ;; Initialize-only branch of parent pc from which PC was derived.
   ;; Immutable after initialization
   (parent-branch :version-technique nil)

   ;; Immutable after initialization.  The timestamp of the point on
   ;; the parent's timeline from which this PC is based.
   (parent-branch-timestamp :version-technique nil)

   ;; Whether a PARENT-PC-BRANCH-TIMESTAMP was specified in the call to
   ;; PC-CREATE which created this PC.  NIL indicates that this PC is
   ;; based on what was the tip of PARENT-PC-BRANCH at the time this PC
   ;; was made.
   (parent-pc-branch-timestamp-specified-p :version-technique nil)

   ;; Active-p is true if PC is active, nil otherwise.  Initially true.
   ;; Metaversioned content, TBD as to whether or not it remains versioned.
   (active-p :initform t
             :accessor product/active?
             :version-technique :scalar) ;METAVERSIONED CONTENT.

   ;; creation-date is derived from change records, the birth cid of the product configuration.
   ;; parent-base-date, date of parent state for creation, derived from birth-cid date of this PC

   ;; Wall is up or down
   ;; Version context of this slot is TBD, pending implementation of WALL_* commands. *FINISH*
   (wall-status :version-technique :scalar) ;:UP or :DOWN
   )
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defmacro product/branches (product)
  `(PROJECT/BRANCHES ,product))

(defmacro product/scan-branches (product)
  `(PROJECT/SCAN-BRANCHES ,product))

(defsubst product/main-branch (product)
  (project/main-branch product))

(defsubst product? (thing)
  (typep thing 'product))

(defun product/warn-if-inactive (product)
  (unless (product/active? product)
    (conman-warning/inactive-product product)))

(defmacro product/change-sets (product)
  `(PROJECT/CHANGE-SETS ,product))

(defmacro product/scan-change-sets (product)
  `(PROJECT/SCAN-CHANGE-SETS ,product))

(defun product/record-relevant-change-set (product change-set)
  "List <change-set> as being relevant to <product> for the purposes of
   adding and removing changes."
  (product/record-relevant-change-sets product (list change-set)))

(defun product/record-relevant-change-sets (product change-sets)
  "List <change-sets> as being relevant to <product> for the purposes of
   adding and removing changes."
  (let ((old-change-set-list (product/change-sets product)))
    (setf (product/change-sets product) 
          (sort (union change-sets old-change-set-list) #'< 
                :key #'change-set/resident-cid))))

(defun product/promote-master-cset-into-version-scope (product branch change-set)
  "Promote the indicated master-cset into the indicated branch of a PC to establish it as
   something affecting the view of versioned PC content.

   CHANGE-SET must be a cset in the master repository.  It is added to the tip of the branch,
   which must be mutable.

   NOTE: this routine is typically called only as part of the master change-set transaction postlude.
   If you're calling it explicitly, you probably shouldn't be.

   Returns the VERSION object which is updated."
  (assert (member change-set (product/change-sets product)))
  (version/promote-change-set (branch/most-recent-version branch) change-set))

(defun product/promote-master-csets-into-version-scope (product branch change-sets)
  "Promote the indicated master-cset into the indicated branch of a PC to establish it as
   something affecting the view of versioned PC content.

   CHANGE-SET must be a cset in the master repository.  It is added to the tip of the branch,
   which must be mutable.

   NOTE: this routine is typically called only as part of the master change-set transaction postlude.
   If you're calling it explicitly, you probably shouldn't be.

   Returns the VERSION object which is updated."
  (assert (every (lambda (change-set) (member change-set (product/change-sets product))) change-sets))
  (version/promote-change-sets (branch/most-recent-version branch) change-sets))

(defun product/partition-subsystems (master-repository-dbpath
                                     user-id-specifier
                                     product
                                     old-timestamp old-branch old-version
                                     new-timestamp new-branch new-version
                                     &key
                                     VPB-old-added-satellite-cset-dids
                                     VPB-old-removed-satellite-cset-dids
                                     VPB-new-added-satellite-cset-dids
                                     VPB-new-removed-satellite-cset-dids
                                     receiver)
  "Given a PC, BRANCH and two virtual private branches, represented by
   old-timestamp, vpb-old-added-satellite-cset-dids, vpb-old-removed-satellite-cset-dids
   and
   new-timestamp, vpb-new-added-satellite-cset-dids, vpb-new-removed-satellite-cset-dids
   Partition the subsystems of the PC into 4 lists:  Those subsystems that are the same
   under both views, those subsystems that exist only in the second view (added), those
   that exist only in the first view (removed), and those changed between the views.

   RECEIVER is funcalled on those four lists.

   NOTE: this code was cloned into pc-partition-subsystems-and-get-remove-redundant-csets-lists.
   You might need to make duplicate changes among the two functions to keep everything working
   properly."
  (declare (ignore product))
  (let ((old-subsystem-list (call-with-before-view
                             (lambda () (csf-branch/sorted-subsystems old-branch))))
        (new-subsystem-list (call-with-after-view
                             (lambda () (csf-branch/sorted-subsystems new-branch)))))

    ;; Compute which subsystems have been added or removed because of the
    ;; master cid set.
    (let ((subsystems-added         (set-difference new-subsystem-list old-subsystem-list))
          (subsystems-removed       (set-difference old-subsystem-list new-subsystem-list))
          (subsystems-maybe-changed (intersection   old-subsystem-list new-subsystem-list))
          subsystems-unchanged
          subsystems-changed)

      ;; For each subsystem that exists in both, check the satellite cset dids
      ;; against the explicit add and remove lists.
      (dolist (subsystem subsystems-maybe-changed)
        (let* ((class-did (distributed-object-identifier (subsystem/csf-class subsystem)))
               (vpb-old-added-dids   (cdr (assoc class-did
                                                 vpb-old-added-satellite-cset-dids)))
               (vpb-old-removed-dids (cdr (assoc class-did
                                                 vpb-old-removed-satellite-cset-dids)))
               (vpb-new-added-dids   (cdr (assoc class-did
                                                 vpb-new-added-satellite-cset-dids)))
               (vpb-new-removed-dids (cdr (assoc class-did
                                                 vpb-new-removed-satellite-cset-dids))))

          (debug-message 3 "In partition, vpb-old-added-dids = ~s" vpb-old-added-dids)
          (debug-message 3 "In partition, vpb-old-removed-dids = ~s" vpb-old-removed-dids)
          (debug-message 3 "In partition, vpb-new-added-dids = ~s" vpb-new-added-dids)
          (debug-message 3 "In partition, vpb-new-removed-dids = ~s" vpb-new-removed-dids)

          (let ((old-raw-subsystem-cset-dids
                 (call-with-before-view
                  (lambda () 
                    (subsystem/satellite-cset-dids master-repository-dbpath
                                                   user-id-specifier
                                                   old-timestamp
                                                   subsystem))))
                (new-raw-subsystem-cset-dids
                 (call-with-after-view
                  (lambda () 
                    (subsystem/satellite-cset-dids master-repository-dbpath
                                                   user-id-specifier
                                                   new-timestamp
                                                   subsystem)))))

            (debug-message 3 "In partition, old-raw-subsystem-cset-dids = ~s" old-raw-subsystem-cset-dids)
            (debug-message 3 "In partition, new-raw-subsystem-cset-dids = ~s" new-raw-subsystem-cset-dids)

            (let ((old-subsystem-cset-dids (set-difference
                                            (union old-raw-subsystem-cset-dids vpb-old-added-dids)
                                            vpb-old-removed-dids))
                  (new-subsystem-cset-dids (set-difference
                                            (union new-raw-subsystem-cset-dids vpb-new-added-dids)
                                            vpb-new-removed-dids)))

              (if (and (null (set-difference old-subsystem-cset-dids new-subsystem-cset-dids))
                       (null (set-difference new-subsystem-cset-dids old-subsystem-cset-dids)))
                  (push subsystem subsystems-unchanged)
                  (push subsystem subsystems-changed))))))

      (funcall receiver
               subsystems-unchanged
               subsystems-added
               subsystems-removed
               subsystems-changed))))

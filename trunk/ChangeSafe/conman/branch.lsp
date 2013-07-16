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
(in-package "CONMAN")

(eval-when (:load-toplevel :execute)
  (export '(branch
            branch?
            branch/guarantee-not-frozen
            branch/mutable-tip
            )))

(defclass csf-branch (branch)
  ((subsystems :initform nil
               :version-technique :composite-set
               :accessor csf-branch/subsystems))
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defun csf-branch/scan-subsystems (csf-branch)
  (declare (optimizable-series-function))
  (versioned-object/scan-composite-versioned-slot csf-branch 'subsystems))

(defun csf-branch/scan-relevant-subsystems (csf-branch satellite-project-dids)
  (declare (optimizable-series-function))
  (choose-if (lambda (subsystem)
               (member (csf-class/project-did (subsystem/csf-class subsystem)) satellite-project-dids))
             (csf-branch/scan-subsystems csf-branch)))

(defun csf-branch/add-subsystem (csf-branch subsystem)
  "Add a subsystem reference to the versioned branch contents of the product configuration.
   Return the updated list of subsystems."
  (let ((subsystems (csf-branch/subsystems csf-branch)))
    (assert (not (find subsystem subsystems)))
    (if (find (named-object/name subsystem) subsystems
              :test #'string=
              :key #'named-object/name)
        (conman-error/subsystem-already-subscribed csf-branch subsystem)
        (let ((new-subsystem-list (sort (cons subsystem subsystems) #'string-lessp
                                        :key #'named-object/name)))
          (setf (csf-branch/subsystems csf-branch) new-subsystem-list)
          new-subsystem-list))))

(defun csf-branch/sorted-subsystems (branch)
  "Return a (possibly empty) list (not a vi-record-stream) of the subsystems which are in the
   current view of the product configuration."
  (sort (csf-branch/subsystems branch) #'string-lessp :key #'named-object/name))

(defun csf-branch/map-over-branch-subsystem-repositories (master-repository-dbpath
                                                          branch
                                                          satellite-open-mode
                                                          function)
  (dolist (subsystem (csf-branch/subsystems branch))
    (subsystem/call-with-satellite-repository
     master-repository-dbpath subsystem satellite-open-mode
     (lambda (csf-class satellite-repository)
       (funcall function csf-class subsystem satellite-repository)))))

(defun csf-branch/map-over-branch-subsystems (master-repository-dbpath user-id-specifier branch
                                                                       &key satellite-transaction-type
                                                                       satellite-metaversion
                                                                       reason
                                                                       receiver)
  (check-type branch csf-branch)
  (csf-branch/map-over-branch-subsystem-repositories
   master-repository-dbpath branch
   (satellite-transaction-type->open-mode satellite-transaction-type)
   (lambda (csf-class subsystem satellite-repository)
     (let ((subdirectory (subsystem/subdirectory subsystem))
           (project-did  (project-reference/project-did subsystem))
           (branch-did   (project-reference/branch-did subsystem))
           (version-state  (project-reference/version-state subsystem)))
       (call-with-satellite-transaction
        :repository satellite-repository
        :reason reason
        :satellite-metaversion satellite-metaversion
        :satellite-version (cond ((distributed-identifier? version-state) version-state)
                                 ((eq version-state :latest) :latest-version)
                                 (t (error "Unrecognized version state.")))
        :transaction-type satellite-transaction-type
        :user-id-specifier user-id-specifier
        :receiver (lambda (satellite-transaction)
                    (declare (ignore satellite-transaction))
                    (let* ((project (repository/resolve-distributed-identifier satellite-repository project-did))
                           (branch  (repository/resolve-distributed-identifier satellite-repository branch-did))
                           (version (cond ((distributed-identifier? version-state)
                                           (repository/resolve-distributed-identifier satellite-repository version-state))
                                          ((eq version-state :latest)
                                           (branch/most-recent-version branch))
                                          (t (Error "Unrecognized version state.")))))
                      (check-type project satellite-project)
                      (check-type branch  satellite-subsystem)
                    (funcall receiver
                             csf-class
                             subsystem satellite-repository
                             subdirectory project branch version))))))))

(defun csf-branch/map-over-branch-satellite-branches (master-repository-dbpath user-id-specifier branch
                                                                               &key satellite-transaction-type
                                                                               satellite-metaversion
                                                                               reason
                                                                               receiver)
  (csf-branch/map-over-branch-subsystems
   master-repository-dbpath user-id-specifier branch
   :reason reason
   :satellite-metaversion satellite-metaversion
   :satellite-transaction-type satellite-transaction-type
   :receiver receiver))


(defun branch/guarantee-not-frozen (branch &optional (tip (branch/mutable-tip branch)))
  "Signal an error if the tip of the branch is immutable (frozen).
   Otherwise, return the branch.

   Optional TIP keyword argument may be supplied if you happen to
   already know the TIP version.

   This is designed to make routines that should not operate on
   frozen branches have a centralized signaling point."
  (if (null tip)
      (conman-error/branch-frozen branch)
      branch))

(defun branch/mutable-tip (csf-branch)
  "Return the latest version of the BRANCH, unless the branch is frozen.
   In that case, signal an error."
  (let ((tip (branch/most-recent-mutable-version csf-branch)))
    (branch/guarantee-not-frozen csf-branch tip)
    tip))

(defun branch/affected-subsystem-list (branch affected-project-dids)
  (when affected-project-dids
    (collect 'list
             (choose-if (lambda (subsystem)
                          (member (project-reference/project-did subsystem) affected-project-dids))
                        (csf-branch/scan-subsystems branch)))))

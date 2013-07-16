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
;;;; Module Description:
;;;;
;;;; A VersionRef is very much like a PROJECT-CONTEXT, in that it identifies
;;;; a specific project branch and a point in time on the branch.  However
;;;; it is not a versioned object, and is suitable for use in workspaces
;;;; and other semi-persistent contexts.
;;;;
;;;; Version refs never have cross-repository pointers, and must not
;;;; references versioned-object or distributed-object subtypes, since they're
;;;; never managed in change-set driven transactions.
;;;;
;;;; For details on project/branch/version state information, see
;;;; the PROJECT-CONTEXT module.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CSF/VERSION-MANAGEMENT")

(proclaim (standard-optimizations))

(eval-when (:load-toplevel :execute)
  (export '(versionref
            versionref/project-did
            versionref/branch-did
            versionref/version-did
            versionref/timestamp
            versionref/update-timestamp
            )))

(defclass versionref ()
  ((project-did :initarg :project-did
                :initform (error "Required initarg :project-did omitted")
                :reader versionref/project-did
                :type distributed-identifier)

   (branch-did :initarg :branch-did
               :initform (error "Required initarg :branch-did omitted")
               :reader versionref/branch-did
               :type distributed-identifier)

   ;; VERSION-DID takes the following values
   ;; and is compatible with that used in a project-context.
   ;; a) :LATEST implying we wish to always use the tip of the branch.
   ;; b) A DID representing a specific VERSION object on the branch, and whose final
   ;;    state is typically used, but which is really under control of the meta-version
   ;;    driving views of the version referenced by the did, or the TIMESTAMP slot,
   ;;    really up to the user's discresion.  If the version-did refers to a VERSION object,
   ;;    then we really want timestamp to indicate the version of the version (i.e. a meta-version
   ;;    to be derived).

   (version-did :initarg :version-did
                :initform (error "Required initarg :version-state omitted.")
                :reader versionref/version-did
                :type (or symbol distributed-identifier))

   ;; TIMESTAMP is :latest, meaning to use the master-cid-set when viewing the project, branch, or version,
   ;; or a particular time stamp, meaning to use the master cid-set that was active at that time
   ;; to view the branch/version-state information.
   (timestamp :initarg :timestamp
              :initform :latest
              :accessor versionref/timestamp
              :type (or symbol cons)))
  (:documentation
   "VERSIONREF: describes without cross-database pointers a point in time on a branch of a project.
    Pretty much a non-versioned semi-persistent flavor of a project-context object.")
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defun versionref/update-timestamp (versionref new-timestamp)
  "Update the timestamp associated with a versionref object, indicating that we're oriented towards
   a different point in time on the branch to which the versionref applies.
   Return the old time-stamp value, or :latest if there was no time-stamp value."
  (check-type versionref versionref)
  (assert (timestamp? new-timestamp))
  (prog1 (versionref/timestamp versionref)
    (remake-instance versionref :timestamp new-timestamp)))


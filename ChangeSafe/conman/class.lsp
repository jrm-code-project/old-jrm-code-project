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

;;; This object is stored in the master repository and it
;;; refers to the satellite project stored in the satellite repository.

(defclass csf-class (distributed-object described-object named-object)
  ((project-did :initarg :project-did
                :reader csf-class/project-did
                :version-technique :nonversioned)

   ;; A pstore list containing the master-cset that created
   ;; this class.
   (%project-creation-cset :initarg :%project-creation-cset
                           :reader csf-class/%project-creation-cset
                           :version-technique :nonversioned)

   ;; A list of subsystems and their associated creation csets.
   ;; We need these because when we subscribe to a subsystem we
   ;; ought to include the csets necessary to see the thing!
   (subsystem-creation-csets :initarg :subsystem-creation-csets
                             :accessor csf-class/subsystem-creation-csets
                             :version-technique :composite-set)

   ;; The logical subdirectory where subsystems of this class
   ;; will appear in a project.
   (subdirectory :accessor csf-class/subdirectory
                 :initarg :subdirectory
                 :version-technique :scalar)

   ;; The actual relative pathname of the repository associated
   ;; with this class.
   (satellite-relative-pathname
    :reader csf-class/satellite-relative-pathname
    :initarg :satellite-relative-pathname
    :version-technique :nonversioned))

  (:documentation "Reference to a satellite-project stored in another (satellite) repository.")
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defmethod print-object ((obj csf-class) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a ~a ~s"
	    (named-object/name obj)
	    (csf-class/project-did obj)
	    (csf-class/subdirectory obj))))

(defun csf-class/project-creation-cset (csf-class)
  (pstore-list/car (csf-class/%project-creation-cset csf-class)))

(defun (setf csf-class/project-creation-cset) (new-value csf-class)
  (setf (pstore-list/car (csf-class/%project-creation-cset csf-class)) new-value))

(defun csf-class/subsystem-creation-cset (csf-class subsystem)
  (pstore-list/cdr
   (find subsystem
         (csf-class/subsystem-creation-csets csf-class)
         :key #'pstore-list/car)))

(defun csf-class/satellite-repository-dbpath (master-repository-dbpath csf-class)
  "Given the <master-repository-dbpath> and the <class>, return the dbpath to
   to the satellite repository associated with that class."
  (dbpath/merge (csf-class/satellite-relative-pathname csf-class) master-repository-dbpath))

(defun csf-class/call-with-satellite-repository (master-repository-dbpath csf-class satellite-open-mode receiver)
  "Invoke <receiver> on the satellite repository associated with <csf-class>.
   The satellite repository will be opened in <open-mode>."
  (call-with-satellite-repository 
   (csf-class/satellite-repository-dbpath master-repository-dbpath csf-class) satellite-open-mode
   receiver))

(defun csf-class/call-with-satellite-transaction (master-repository-dbpath user-id-specifier csf-class 
                                                                           &key 
                                                                           satellite-transaction-type
                                                                           satellite-metaversion
                                                                           satellite-version
                                                                           vpb-cid-dids-to-add
                                                                           vpb-cid-dids-to-remove
                                                                           reason
                                                                           receiver)
  (csf-class/call-with-satellite-repository
   master-repository-dbpath csf-class (satellite-transaction-type->open-mode satellite-transaction-type)
   (lambda (satellite-repository)
     (call-with-satellite-transaction
      :reason reason
      :repository satellite-repository
      :user-id-specifier user-id-specifier
      :transaction-type satellite-transaction-type
      :satellite-metaversion satellite-metaversion
      :satellite-version satellite-version
      :vpb-cid-dids-to-add vpb-cid-dids-to-add
      :vpb-cid-dids-to-remove vpb-cid-dids-to-remove
      :receiver (lambda (satellite-transaction)
                  (funcall receiver satellite-repository satellite-transaction 
                           (repository/resolve-distributed-identifier satellite-repository (csf-class/project-did csf-class))))))))

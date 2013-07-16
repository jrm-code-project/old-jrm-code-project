;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002 ChangeSafe, LLC
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

(in-package "CSF/CORE")

(proclaim (standard-optimizations))

;;;
;;; CID-DETAIL-TABLE, CID-DETAIL-TABLE-ENTRY
;;; Log the 'What' of change, tracking a reference to every instance of versioned classes
;;; which had changed slots, providing the set of slots which changed.
;;;

(defclass cid-detail-table-entry ()
  ;; object of type VERSIONED-OBJECT whose slots were modified
  ((object-changed :initarg :object-changed
                   :initform (error "Required initarg :object-changed omitted.")
                   :reader cid-detail-table-entry/object-changed)
   ;; list of slot identifiers (symbols compatible with SLOT-VALUE)
   ;; designating slots which changed.
   (slots-modified :initarg :slots-modified
                   :initform (make-instance 'persistent-vector :size 0)
                   :reader cid-detail-table-entry/slots-modified))
  (:documentation
     "Track the 'What' of change, each detail table entry refers to one versioned class instances
    which had one more more slots modified.  It also refers to the slots which were modified.
    We can then query these slots to see the nature of the modification.")
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defmethod print-object ((entry cid-detail-table-entry) stream)
  (print-unreadable-object (entry stream :type t)
    (princ (cid-detail-table-entry/slots-modified entry) stream)
    (princ (cid-detail-table-entry/object-changed entry) stream)))

(defclass cid-detail-table ()
  ((entries :initform (make-instance 'persistent-vector :size 0)
            :reader cid-detail-table/entries))
  (:documentation
   "The CID detail table contains one entry for every object whose attributes were changed by a given CID.")
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defun cid-detail-table/log-change (cid-detail-table versioned-object slot-identifier)
  "Log the change to the specified slot of the specified versioned object"
  ;; PERFORMANCE: needs to be a hashed search keyed by versioned-object so we can find the right
  ;; table entry quickly.  Potentially many table entries will be created, proportional to the number
  ;; of versioned objects affected.

  ;; Find existing entry, or create a new one, as appropriate.
  (let ((table-entry (or (persistent-vector-find versioned-object (cid-detail-table/entries cid-detail-table)
                               :key #'cid-detail-table-entry/object-changed)
                         (let ((new-entry (make-instance 'cid-detail-table-entry
                                                         :object-changed versioned-object)))
                           (persistent-vector-push (cid-detail-table/entries cid-detail-table) new-entry)
                           new-entry))))
    (persistent-vector-pushnew (cid-detail-table-entry/slots-modified table-entry) slot-identifier)))

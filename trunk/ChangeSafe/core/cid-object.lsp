;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002, 2003 ChangeSafe, LLC
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

(eval-when (:load-toplevel :execute)
  (export '(
            cid-object/find-or-create
            cid-object->resident-cid
            cid-objects->cid-set
            repository-transaction/cid-object
            )))

(defclass cid-object (canonical-object)
  ((did :initarg :distributed-identifier
        :initform (error "Required initarg :distributed-identifier omitted.")
        :type distributed-identifier
        :reader cid-object/did)
   (cid :initarg :cid
        :initform nil
        :accessor %cid-object/cid))
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defmethod print-object ((object cid-object) (stream stream))
  ;; As a general rule of thumb, referencer of CID-OBJECTs don't want to print the repository-specific
  ;; CID as an integer.
  (print-unreadable-object (object stream :type t)
    (print-object (cid-object/did object) stream)))

(defmethod objects-equalp ((left cid-object) (right cid-object))
  ;; Used by the canonical-object protocol
  (eq (cid-object/did left) (cid-object/did right)))

(defmethod marshal-from-transaction ((element cid-object))
  (cid-object/did element))

(defun cid-object->cid (repository cid-object)
  "Return the local CID number representing an existent change-set in the current repository.
   It is an error to call this routine if CID-OBJECT-CSET-IMPORTED? returns nil for the indicated CID-OBJECT.
   We also lazily and dynamically resolve the CID-OBJECT to a local number if the cid exists,
   the transaction permits it, and it hasn't previously been resolved."
  (or (%cid-object/cid cid-object)
      (cond ((repository/resolve-distributed-identifier repository
                                                        (cid-object/did cid-object))
             => (lambda (number)
                  ;; See if we can lazily update our numeric field
                  (when (not (and (integerp number) (< 0 number)))
                    (debug-message 1 "cid-object-as-number bad cid: ~a" number))
                  (assert (and (integerp number) (< 0 number)))
                  (when (txn-for-update? *transaction*)
                    (remake-instance cid-object :cid number))
                  number))
            (t nil))))

(defun cid-object->resident-cid (repository cid-object)
  "Return the local CID number representing an existent change-set in the current repository.
   It is an error to call this routine if CID-OBJECT-CSET-IMPORTED? returns nil for the indicated CID-OBJECT.
   We also lazily and dynamically resolve the CID-OBJECT to a local number if the cid exists,
   the transaction permits it, and it hasn't previously been resolved."
  (or (cid-object->cid repository cid-object)
      (error "The change-set represented by CID-OBJECT ~s is not present in the current repository ~
              and it's local cid number cannot be resolved."
             cid-object)))

(defun cid-objects->cid-set (repository cid-objects)
  (list->cid-set
   repository
   (collect 'list
            (choose-if #'identity
                       (map-fn 't (lambda (cid-object)
                                    (cid-object->cid repository cid-object))
                               (scan 'list cid-objects))))))

(defgeneric cid-object/find-or-create (key)
  (:documentation
   "Resolve a canonical CID-OBJECT given either its locally resolved integer key,
   or its string CID-DID key.  The former is meant to be more efficient.")
  (:method ((key string))               ;Key is a CID-DID string, i.e. "foo-repository.CID.1"
    (canonical-object/find-or-create (make-instance 'cid-object :distributed-identifier (parse-did key))))

  (:method ((key integer))              ;Key is a CID integer, locally resolved
    ;; **PERFORMANCE**: The a numeric key indexed vector lookup for this form method binding avoids
    ;; both consing a temporary and potentialy throw-away CID-OBJECT, and avoids hashing it as well.
    ;; Yes, it means this subtype of canonical object is doubly indexed
    ;; (by CID-DID string and CID-integer).  To see why this is important, simply look at how
    ;; CID-SET-BASIS objects are created.

    (assert (/= key +cid-unassigned+))
    (let* ((cid-mst-table (repository/cid-master-table *repository*))
           (pvector (cid-master-table/cid-objects-by-cid cid-mst-table))
           (cid-obj))
      (when (>= key (persistent-vector-length pvector))
        (grow-persistent-vector pvector (1+ key)))
      (setq cid-obj (persistent-vector-ref pvector key))
      (unless cid-obj
        (setq cid-obj
              (canonical-object/find-or-create
               (make-instance 'cid-object
                              :distributed-identifier (repository/cid-distributed-identifier *repository* key)
                              :cid key))))
      cid-obj)))

(defmethod repository/cid-information ((repository repository) (cid cid-object))
  (repository/cid-information repository (cid-object->cid repository cid)))

(defmethod repository/cid-versioned-change-information ((repository repository) (cid cid-object))
  (repository/cid-versioned-change-information repository (cid-object->cid repository cid)))

(defun repository-transaction/cid-object (repository-transaction)
  (check-type repository-transaction versioned-update-repository-transaction)
  (cid-object/find-or-create (repository-transaction/cid repository-transaction)))

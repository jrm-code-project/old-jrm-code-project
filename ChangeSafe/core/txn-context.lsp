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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(
            call-with-before-view
            call-with-after-view

            repository-transaction
            repository-transaction/disposition
            repository-transaction/note-change-set
            repository-transaction/log-change
            repository-transaction/mode
            repository-transaction/reason
            repository-transaction/uid
            versioned-compare-repository-transaction
            versioned-update-repository-transaction

            transaction/cid-set
            )))

(defgeneric repository-transaction/repository (repository-transaction)
  (:documentation "Returns the primary repository associated with a transaction."))

(defclass repository-transaction ()
  ((repository :initarg :repository
               :initform (error "Required initarg :repository omitted.")
               :reader repository-transaction/repository)
   (underlying-transaction :initarg :underlying-transaction
                           :initform (error "Required initarg :underlying-transaction omitted.")
                           :reader repository-transaction/underlying-transaction)
   (uid  :initarg :uid
         :initform (error "Required initarg :uid omitted.")
         :reader repository-transaction/uid
         :type   distributed-identifier))
  (:documentation "Core Transaction Information."))

(defclass nonversioned-repository-transaction (repository-transaction)
  ((mode :initform :nonversioned-read-only :reader repository-transaction/mode))
  (:documentation "Transaction information for a read-only transaction that does not use versioned objects."))

(defclass nonversioned-update-repository-transaction (repository-transaction)
  ((mode :initform :nonversioned-read-write :reader repository-transaction/mode))
  (:documentation "Transaction information for a read-write transaction that does not use versioned objects."))

(defclass versioned-repository-transaction (repository-transaction)
  ((mode :initform :read-only :reader repository-transaction/mode)
   (cid-set :initarg :cid-set
            :initform (error "Required initarg :cid-set omitted.")
            :reader repository-transaction/cid-set))
  (:documentation "Transaction information for a transaction that views versioned objects."))

(defgeneric transaction/cid-set (transaction)
  (:documentation "Return the `raw' cid set of the transaction.")
  (:method ((transaction versioned-repository-transaction))
    (repository-transaction/cid-set transaction)))

(defmethod clos:shared-initialize :after ((instance versioned-repository-transaction) slot-names
                                          &rest initargs)
  ;; Sanity check
  (assert (typep (transaction/cid-set instance) 'cid-set))
  (assert (eq (cid-set/repository (transaction/cid-set instance)) 
              (repository-transaction/repository instance)))
  (debug-message 4 "Current CID-SET is ~s" (transaction/cid-set instance)))

(defclass versioned-compare-repository-transaction (repository-transaction)
  ((mode :initform :read-only :reader repository-transaction/mode)
   (before-cid-set :initarg :before-cid-set
                   :initform (error "Required initarg :before-cid-set omitted.")
                   :reader repository-transaction/before-cid-set)
   (after-cid-set :initarg :after-cid-set
                  :initform (error "Required initarg :after-cid-set omitted.")
                  :reader repository-transaction/after-cid-set))
  (:documentation "Transaction information for a transaction that compares two views of versioned objects."))

(defmethod transaction/cid-set ((transaction versioned-compare-repository-transaction))
  (ecase *versioned-compare-view*
    ((:before) (repository-transaction/before-cid-set transaction))
    ((:after)  (repository-transaction/after-cid-set  transaction))))

(defun call-with-view (view thunk)
  ;; sanity check, ensure the transaction cares 
  (assert (typep *transaction* 'versioned-compare-repository-transaction))
  (let ((*versioned-compare-view* view))
    (funcall thunk)))

(defun call-with-before-view (thunk)
  (call-with-view :before thunk))

(defun call-with-after-view (thunk)
  (call-with-view :after thunk))

(defclass versioned-update-repository-transaction (versioned-repository-transaction)
  ((mode :initform :read-write :reader repository-transaction/mode)
   (cid  :initarg :cid
         :initform (error "Required initarg :cid omitted.")
         :reader repository-transaction/cid)
   (change-set :initform nil
               :accessor repository-transaction/change-set)
   (cid-master-table-entry :initarg :cid-master-table-entry
                           :initform (error "Required initarg :cid-master-table-entry omitted.")
                           :reader repository-transaction/cid-master-table-entry)
   ;; An alist mapping objects to the list of modified slots within them.
   ;; This is used to build the master-table entry upon commit.
   (objects-changed :initform nil
                    :accessor repository-transaction/objects-changed))
  (:documentation "Transaction information for a transaction that modifies versioned objects."))

(defun repository-transaction/disposition (repository-transaction)
  (transaction/disposition (repository-transaction/underlying-transaction repository-transaction)))

(defun repository-transaction/reason (repository-transaction)
  (transaction/reason (repository-transaction/underlying-transaction repository-transaction)))

(defun repository-transaction/note-change-set (repository-transaction change-set)
  "Tell the transaction about the change set, so when it commits the change set
   gets associated with the cid."
  (setf (repository-transaction/change-set repository-transaction) change-set))

(defun repository-transaction/log-change (repository-transaction versioned-object slot-identifier)
  "Update the CID detail table entries for the current transaction, noting the object whose slot
   changed, and which slot changed.
   RETURN VALUE: N/A
   Anonymous CID entries (cid == *cid-unassigned*) are not permitted for master detail table entries."
  ;; If we allow anonymous entries, they they would represent an exception because we would want to
  ;; potentially log different master detail table entries for anonymous updates performed
  ;; in different change transactions.  Which means we have to discern the different txns.
  ;;
  ;; If we admitted anonymous entries and didn't create multiple master table entries,
  ;; then the one entry for *cid-unassigned* would represent all changes in this guise, ever!
  ;; Definitely useless.

  ;; We could push this check to the cid-master-table, but the detection would be noticed late,
  ;; putting the check here uncovers the problem as soon as a VI-SET-VALUE operation occurs.
  (when (eq (repository-transaction/cid repository-transaction) +cid-unassigned+)
    (error 'changesafe-database-error
           :format-control "Attempt to log a cid-master-table entry for an anonymous change transaction"))
  (let ((entry (assoc versioned-object (repository-transaction/objects-changed repository-transaction))))
    (if (null entry)
        (push (cons versioned-object (list slot-identifier))
              (repository-transaction/objects-changed repository-transaction))
        (push slot-identifier (cdr entry))))
  (let ((cmte (repository-transaction/cid-master-table-entry repository-transaction)))
    (if cmte
        (cid-master-table-entry/log-change cmte versioned-object slot-identifier)
        (error 'changesafe-database-error
               :format-control "Attempt to modify a logged versioned object in a read-only or change-suppressed ~
              transaction")))
  )

(defun txn-for-update? (repository-transaction)
  (eq (repository-transaction/mode repository-transaction) :read-write))

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
            cid-master-table/cid-comparison-timestamp)))

(defclass cid-master-table-entry ()
  ((cid :initarg :cid
        :initform +cid-unassigned+
        :reader cid-master-table-entry/cid)     ;*LOCAL* CID which indexes the change
   (cid-detail-table :initform (make-instance 'cid-detail-table)
                     :reader cid-master-table-entry/cid-detail-table) ;CID-DETAIL-TABLE object
   (cid-set-basis    :initarg :cid-set-basis
                     :initform (error "Required initarg :CID-SET-BASIS omitted.")
                     :reader cid-master-table-entry/cid-set-basis)
   (who :initarg :who
        :initform (error "Required initarg :who omitted.")
        ;; Either a CORE USER (if we are in a primary repository)
        ;; or a distributed-identifier to a core user.
        :type (or distributed-identifier core-user))
   
   (when-start :initform (timestamp-allocate)
               :reader cid-master-table-entry/when-start) ;When CID change txn started.
   (when-finish :initarg :when-finish
                :reader cid-master-table-entry/when-finish) ;When CID change txn completed. Generally used in comparisons.
   (why :initarg :why
        :initform (error "Required initarg :why omitted.")
        :reader cid-master-table-entry/why)                 ;text describing change
  ;; The following information represents higher-level change-set information.
  ;; This information is often versioned and mutable, unlike the core change-set information.
  ;; While the information is mutable, the link to the information from the low-level change-set
  ;; information in this master table entry is NOT mutable.  The pointed-to object should always
  ;; be exported/imported when a change-set is migrated.  This would actually happen simply
  ;; by following the detail table records which record the creation of a versioned-object,
  ;; but we also ensure the protocol here to make sure the cloned low-level CMTE has slots appropriately
  ;; set.

  ;; Note that the content of this slot is not in any way interpreted by the CORE package.
  ;; It represents data managed by external packages.  About the only CORE assumption is that the
  ;; versioned information is stored as a VERSIONED-OBJECT subtype.

  ;; This slot is set through the TXN-CONTEXT-SET-VERSIONED-CHANGE-SET-INFORMATION function.
   (versioned-change-information 
    :initarg :versioned-change-information
    :initform nil
    :reader cid-master-table-entry/versioned-change-information))

  (:documentation
     "Track 4/5 W's of change, Who, When, Where, and Why.
    'What' is managed by the CID detail table and the version indexes.
    'Where' is actually derived by mapping the effecting CID to repositories which created the CID.")
  (:metaclass persistent-standard-class)
  (:schema-version 0))

;; Forward references.
(declaim (ftype (function (t) (or null relative-pathname))    repository/parent))

(defmethod initialize-instance :before ((instance cid-master-table-entry) &rest initargs 
                                        &key cid who &allow-other-keys)
  (cond ((= cid 1))
        ((repository/parent *repository*)
         => (lambda (parent)
              (declare (ignore parent)) 
              ;; parent is just a pathname
              ;; I'd like to do a better check, but...
              (check-type who distributed-identifier)
              ;; Yuck.  Abstraction violation.
              (assert (or (eq (did/class who) :core-user)
                          (eq (did/class who) :rfm-user)
                          (eq (did/class who) :conman-user)))))

        (t (check-type who core-user))))

(defun cid-master-table-entry/comparison-timestamp (cid-master-table-entry)
  "Return the timestamp used to compare chronology of CIDs"
  (or (and (slot-boundp cid-master-table-entry 'when-finish)
           (cid-master-table-entry/when-finish cid-master-table-entry))
      (cid-master-table-entry/when-start cid-master-table-entry)
      (error "Where's the timestamp?")))

(defun cid-master-table-entry/log-change (cid-master-table-entry versioned-object slot-identifier)
  "Ensure that the change history is recorded in the CID-DETAIL-TABLE-ENTRIES for this CID"
  (cid-detail-table/log-change
   (cid-master-table-entry/cid-detail-table cid-master-table-entry)
   versioned-object slot-identifier))

(defun cid-master-table-entry/note-finish (entry reason change-set)
  "Note the finish time and possibly update the reason that this cid is in the repository."
  (if (string-equal reason (cid-master-table-entry/why entry))
      ;; Usually, the reason doesn't change.
      (remake-instance entry
                       :when-finish (timestamp-allocate)
                       :versioned-change-information change-set)
      (remake-instance entry
                       :when-finish (timestamp-allocate)
                       :why reason
                       :versioned-change-information change-set)))

;;;
;;; CID-MASTER-TABLE, CID-MASTER-TABLE-ENTRY
;;; Log 4/5 W's of change, Who, When, Where, Why
;;; We create the CID-MASTER-TABLE-ENTRY when we start the change transaction, and
;;; we complete its finish-time and entry into the CID-MASTER-TABLE as the change transaction concludes.
;;;
(defclass cid-master-table ()
  (
   ;; These entries are indexed from 1 to the number of cids in the repository.
   ;; Since cids are always relocated to local and incremental numbers, the vector may be indexed by cid.
   ;; **** WARNING ****
   ;; The entries vector is never sparse in a normal repository, but it may be sparse in a transport package.

   ;; persistent-vector of cid-master-table-entry objects, 
   (entries :initform (make-instance 'persistent-vector)
            :reader cid-master-table/entries)
   ;; The next field also contains redundant info.  It is used to get a handle to a canonical
   ;; cid-object by means of the cid (integer).  This is significantly faster.  In the event
   ;; that our list does not have any info about a particular cid, we will find the info in the
   ;; traditional manner and fill in the table for subsequent fast reference.  It is an
   ;; persistent-vector of cid-objects (or nil)
   (cid-objects-by-cid :initform (make-instance 'persistent-vector)
                       :reader cid-master-table/cid-objects-by-cid))
  (:documentation
   "The CID master table contains one entry per CID which acts upon the system, logging
various information about changes to the system.")
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defmacro cid-master-table-entries/fill-pointer (cmte)
  `(PERSISTENT-VECTOR-LENGTH ,cmte))

(defun cid-master-table/entry-for-cid (cid-master-table cid &key missing-cid-okay)
  "Retrieve the CID-MASTER-TABLE-ENTRY for the specified CID.  Return NIL if it isn't found."
  (let ((entry (and (<= cid (cid-master-table/last-allocated-cid cid-master-table))
		    (persistent-vector-ref (cid-master-table/entries cid-master-table)
                                           cid))))
    (if (or missing-cid-okay entry)
	entry
        (error "Unable to find cid-master-table-entry for cid ~d" cid))))

(defsubst cid-master-table/last-allocated-cid (cid-master-table)
  "Return the last allocated CID in this repository."
  (1- (the array-index (cid-master-table-entries/fill-pointer (cid-master-table/entries cid-master-table)))))

(defun cid-master-table/contains-cid? (cid-master-table cid)
  "Return true if the local cid specification has an entry in the CID-MASTER-TABLE, nil otherwise.
   Returning true says that this CID has full content available.
   NOTE: returning NIL may be a soon-to-be-false negative if the cid in question is queued to
   be imported during the IMPORT-VIEW process."
  (<= cid (cid-master-table/last-allocated-cid cid-master-table)))

(defun cid-master-table/cid-comparison-timestamp (cid-master-table cid)
  "Return the timestamp of the cid used for comparing cid chronology, or NIL if the cid
   is not known to the cid master table"
  (let ((entry (cid-master-table/entry-for-cid cid-master-table cid :missing-cid-okay t)))
    (and entry
	 (cid-master-table-entry/comparison-timestamp entry))))

(defvar *cid-cached-timestamps* nil
  "Vector indexed by CID and yielding a timestamp.  Used only for IMPORT because of
  chicken and egg problems during CID-MASTER-TABLE-MERGE.  Sparse, not all entries will have
  timestamp values.")

(defun cid-master-table/add-entry (cid-master-table new-entry)
  (let ((cid (cid-master-table-entry/cid new-entry)))
    (unless (= cid +cid-unassigned+)
      (let ((master-table-entries (cid-master-table/entries cid-master-table)))
        (cond ((= (persistent-vector-length master-table-entries) cid)
               (persistent-vector-push master-table-entries new-entry))
              ((> (persistent-vector-length master-table-entries) cid)
               (setf (persistent-vector-ref master-table-entries cid) new-entry))
              (t (grow-persistent-vector master-table-entries (1+ cid))
                 (setf (persistent-vector-ref master-table-entries cid) new-entry)))))))

(defun cid-master-table/cid-information (repository cid-master-table cid)
  "Return the following values:
  (1) the cid reason,
  (2) the cid timestamp,
  (3) the versioned change information, if any (or NIL),
  (4) the cid-set-basis"
  (let ((entry (cid-master-table/entry-for-cid cid-master-table cid)))
    (values (if (slot-boundp entry 'why)
                (cid-master-table-entry/why entry)
                "No information present.")
            (cid-master-table-entry/comparison-timestamp entry)
            (if (slot-boundp entry 'versioned-change-information)
                (cid-master-table-entry/versioned-change-information entry)
                nil)
            (if (slot-boundp entry 'cid-set-basis)
                (make-instance 'dense-cid-set
                               :repository repository
                               :bitmap (cid-master-table-entry/cid-set-basis entry))
                nil))))

(defun cid-master-table/cid-versioned-change-information (cid-master-table cid)
  "Return the CID's versioned change information, if any (or NIL)"
  (cid-master-table-entry/versioned-change-information
   (cid-master-table/entry-for-cid cid-master-table cid)))

(defun cid-master-table/active-cids (repository cid-master-table &key end-time zero-list-length metaversion)
  "Turn on all cids which are contained in the master table.
   END-TIME, if specified, should be a timestamp which limits the change-sets included
   in the resulting cid-set.  No cid-sets created after this date are included in the cid-set.
   When ZERO-LIST-LIST is non-nil, construct a list of where the zeros are in the bitmap
   that is up to that length long.  If there are more zeros than that or ZERO-LIST-LENGTH is
   NIL, return  NIL for the list otherwise return the list."
  (declare (ignore zero-list-length metaversion))
  (check-type end-time (optional timestamp))
  ;; PERFORMANCE: have a faster way of turning on all cids...
  (if (null end-time)
      (let ((limit (1+ (cid-master-table/last-allocated-cid cid-master-table))))
        (range->cid-set repository (if (zerop limit) 0 1) limit))
      (let* ((last-active-cid
              (collect-first
               (choose-if (lambda (cid)
                            (timestamp/time-flows-left-to-right?
                             (cid-master-table/cid-comparison-timestamp cid-master-table cid)
                             end-time))
                          (scan-range :from (cid-master-table/last-allocated-cid cid-master-table)
                                      :downto 1
                                      :by -1))))
             (bitmap (simple-vector-1b-allocate (1+ last-active-cid))))
        (iterate ((cid (choose-if (lambda (cid)
                                    (timestamp/time-flows-left-to-right?
                                     (cid-master-table/cid-comparison-timestamp cid-master-table cid)
                                     end-time))
                                  (scan-range :from last-active-cid
                                              :downto 1
                                              :by -1))))
          (setf (simple-vector-1b-ref bitmap cid) 1))
        (make-instance 'dense-cid-set
                       :repository repository
                       :bitmap bitmap))))

#||
  (assert (< (cid-master-table/last-allocated-cid cid-master-table) array-dimension-limit))
  (let ((zero-list nil))
    (if end-time
	(progn
	  (debug-message 4 "Computing master cid-set for ~s (~a)"
			 end-time
			 (universal-time->iso-date-time-string (time-stamp-as-universal-time end-time)))

	  (setq zero-list
		(cid-master-table/activate-cids-by-ts
		 cid-master-table cid-set end-time :zero-list-length zero-list-length))
	  (debug-message 4 "Master cid-set is ~s" cid-set))
        (progn
          ;; No!  Turn them all on!
          (cid-set/cid-range 1 (1+ (cid-master-table/last-allocated-cid cid-master-table)))
          (when zero-list-length
            (setq zero-list :all-ones)));; no zeros in the bitmap
        )
    zero-list)
  ||#




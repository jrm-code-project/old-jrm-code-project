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
;;;;
;;;; File Name: cvi.lsp
;;;; Author:    Dave Tenny
;;;;
;;;; Module Description: Composite Version Index (CVI) implementation.
;;;; This subtype of Versioned Index (VI) complies with the VI interfaces,
;;;; while supporting its unique composite versioning semantics.  See VI.LSP
;;;; for additional details on the versioned index protocols.  A description
;;;; of composite versioning semantics is beyond the scope of this document.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CSF/CORE")

(proclaim (standard-optimizations))

(defclass scalar-versioned-value (versioned-value)
  ((initial-value :initarg :initial-value
                  :initform +vi-value-unassigned+
                  :reader scalar-versioned-value/default-value)
   ;; There are two vectors, the cid-vector and the value-vector.
   ;; Each elt(n) of the cid-vector has as its value the CID that created elt(n) of the value-vector.
   ;; Vectors are ordered such that most recent cids&values are LAST, they're pushed onto the
   ;; end of the extensible vectros.  Values are otherwise managed/marshalled in the same way
   ;; that NVI values are managed.

   ;; Preallocation/growth strategy:
   ;; We preallocate and grow these vectors in parallel.
   ;; We assume that the vast majority of SVIs have only a couple of values in their life.
   ;; Consider file names, which change infrequently and typically average 1 over the life of the file.
   ;; Product, subsystem, class, and other names change even less frequently.
   ;; However SOME SVI attributes change every time a file is changed, like the timestamps we track
   ;; for files. So if we see some growth other than a couple of elements, we assume that the SVI
   ;; will grow frequently, and take a more aggressive preallocation stance when we grow the vector.

   ;; It might be worth having these two things point to the cid and value directly when N=1,
   ;; and allocate vectors only when N>1 values have been applied.  We can do that by
   ;; determining that CID-VECTOR points to a FIXNUM instead of an ASTORE-DISK-VECTOR-SB32.
   ;; This optimization is currently implemented.

                                        ;fixnum/integer or astore-disk-vector-sb32
   (cid-vector :initarg :cid-vector
                  :initform +cid-unassigned+
               :accessor scalar-versioned-value/cid-vector)

   (value-vector :initarg :value-vector
                  :initform +vi-value-unassigned+
                 :accessor scalar-versioned-value/value-vector))

  (:documentation
     "Scalar Versioned Index.  Retains versions of values.  A given CID-set selects the current
      value of a scalar-versioned slot by selecting the latest (chronological) change in
      the CID-set which is found in the scalar version representation.  In systems where
      CIDs numeric ordering corresponds to chronological ordering, this is the highest
      numbered entry in the SVI which matches an active CID in the CID-set.  In systems
      where the CID number doesn't represent chronology, we must have some chronological
      ranking system.

      **** ERROR **** ERROR **** ERROR **** ERROR **** ERROR **** ERROR **** ERROR **** ERROR

      Current implementation assumes that CID ordering implies chronology ordering.
      This isn't true for distributed change-sets, and the ordering maintenance will have to
      be fixed when we start supporting distributed csets in earnest. *FINISH*

      **** ERROR **** ERROR **** ERROR **** ERROR **** ERROR **** ERROR **** ERROR **** ERROR

      All changes to this index type are logged.")

  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defun scan-scalar-versioned-values (svi)
  (declare (optimizable-series-function 2))
  (cotruncate (scan-persistent-vector (scalar-versioned-value/cid-vector svi))
              (scan-persistent-vector (scalar-versioned-value/value-vector svi))))

(defun scalar-versioned-value/active-value-pair (svi cid-set)
  "Search for the active value according to SVI semantics, which is to say
   the most recent CID/VALUE pair corresponding to bits in CID-SET.

   If an active value is found, return two values:
   1) The CID
   2) The VALUE.

   If no active value is found, return NIL."
  (cond ((or (not (slot-boundp svi 'cid-vector))
             (eq (scalar-versioned-value/cid-vector svi) +cid-unassigned+)) (values nil nil))
        ((typep (scalar-versioned-value/cid-vector svi) 'persistent-vector)
         (let ((i (multiple-value-bind (cids indexs)
                      (cotruncate (scan-persistent-vector (scalar-versioned-value/cid-vector svi))
                                  (scan-range :from 0))
                    (collect-last (choose (#m (lambda (cid) (cid-set/member cid-set cid)) cids) indexs)))))
           (values (persistent-vector-ref (scalar-versioned-value/cid-vector svi) i)
                   (persistent-vector-ref (scalar-versioned-value/value-vector svi) i))))

        ((cid-set/member cid-set (scalar-versioned-value/cid-vector svi))
         (values (scalar-versioned-value/cid-vector svi)
                 (scalar-versioned-value/value-vector svi)))
        (t (values nil nil))))

(defmethod versioned-value/cid-list ((svi scalar-versioned-value))
  (when (and (slot-boundp svi 'cid-vector)
             (not (eq (scalar-versioned-value/cid-vector svi) +cid-unassigned+)))
    (if (typep (scalar-versioned-value/cid-vector svi) 'persistent-vector)
        (multiple-value-bind (cids values)
            (scan-scalar-versioned-values svi)
          (collect 'list cids))
        (list (scalar-versioned-value/cid-vector svi)))))

(defmethod versioned-value/cid-set (repository (svi scalar-versioned-value))
  (list->cid-set repository (versioned-value/cid-list svi)))

(defmethod versioned-value/contains-cid? ((svi scalar-versioned-value) cid-to-find)
  (when (and (slot-boundp svi 'cid-vector)
             (not (eq (scalar-versioned-value/cid-vector svi) +cid-unassigned+)))
    (if (typep (scalar-versioned-value/cid-vector svi) 'persistent-vector)
        (multiple-value-bind (cids values)
            (scan-scalar-versioned-values svi)
          (collect-or
           (map-fn 't (lambda (cid value)
                        (declare (ignore value))
                        (= cid cid-to-find))
                   cids values)))
        (= (scalar-versioned-value/cid-vector svi) cid-to-find))))

(defmethod versioned-value/most-recent-cid ((svi scalar-versioned-value) cid-set)
  (when (and (slot-boundp svi 'cid-vector)
             (not (eq (scalar-versioned-value/cid-vector svi) +cid-unassigned+)))
    (cond ((typep (scalar-versioned-value/cid-vector svi) 'persistent-vector)
           (collect-last
            (choose-if (lambda (cid)
                         (cid-set/member cid-set cid))
                       (multiple-value-bind (cids values)
                           (scan-scalar-versioned-values svi)
                         (map-fn 't (lambda (cid value)
                                      (declare (ignore value))
                                      cid)
                                 cids values)))))
          ((cid-set/member cid-set (scalar-versioned-value/cid-vector svi))
           (scalar-versioned-value/cid-vector svi))
          (t nil))))

(defmethod versioned-value/view ((svi scalar-versioned-value) cid-set instance slot)
  (debug-message 4 "Viewing SVI slot ~s under cid-set ~s" slot cid-set)
  (multiple-value-bind (cid value)
      (scalar-versioned-value/active-value-pair svi cid-set)
    (cond (cid value)
          ((and (slot-boundp svi 'initial-value)
                (not (eq (scalar-versioned-value/default-value svi) +vi-value-unassigned+)))
           (scalar-versioned-value/default-value svi))
          (t (slot-unbound (class-of instance) instance slot)))))

(defun scalar-versioned-value/add-value (svi cid new-value)
  (cond ((or (not (slot-boundp svi 'cid-vector))
             (eq (scalar-versioned-value/cid-vector svi) +cid-unassigned+))
         (remake-instance svi
                          :cid-vector cid
                          :value-vector new-value))
        ((typep (scalar-versioned-value/cid-vector svi) 'persistent-vector)
         (persistent-vector-push (scalar-versioned-value/cid-vector svi) cid)
         (persistent-vector-push (scalar-versioned-value/value-vector svi) new-value))
        (t (let ((new-cid-vector (make-instance 'persistent-vector :size 2))
                 (new-value-vector (make-instance 'persistent-vector :size 2)))
             (setf (persistent-vector-ref new-cid-vector 0) (scalar-versioned-value/cid-vector svi)
                   (persistent-vector-ref new-cid-vector 1) cid
                   (persistent-vector-ref new-value-vector 0) (scalar-versioned-value/value-vector svi)
                   (persistent-vector-ref new-value-vector 1) new-value)
             (remake-instance svi
                              :cid-vector new-cid-vector
                              :value-vector new-value-vector)))))

(defmethod versioned-value/update (new-value (svi scalar-versioned-value) transaction instance slot)
  (let* ((cid (repository-transaction/cid transaction))
        (cid-vector (scalar-versioned-value/cid-vector svi))
        (value-vector (scalar-versioned-value/value-vector svi))
        (vectored-p (typep cid-vector 'persistent-vector)))
    (multiple-value-bind (old-cid old-value)
        (scalar-versioned-value/active-value-pair svi (repository-transaction/cid-set transaction))
      ;; Unlike NVI/LNVI, we can't call VI-STORE-UNVERSIONED-VALUE to reuse existing value space.
      ;; We must allocate a new value if we're going to store one.  We could potentially optimize
      ;; case where NO-CHANGE is true to reuse same value for multiple cids, but that gets messy
      ;; if they call SET-VALUE multiple times on the slot, because we'll then try to reuse
      ;; the value which is shared...

      ;; KEY SEMANTIC NOTE:
      ;; We only complain about NO-CHANGE if the new value is the same as the old value
      ;; when the old value is from the non-current CID.
      (when (and old-cid (= old-cid cid))
        ;; Duplicate SET-VALUE attempt on this slot in the same change transaction.
        ;; *** WE DO NOT DELETE THE OLD VALUE, THAT IS UP TO THE USER ***
        (cond (vectored-p ;have vectors to manage
               ;; Nil out the current vector entries and decrement the vector entries counts
               (persistent-vector-pop cid-vector)
               (persistent-vector-pop value-vector))
              (t (setq svi (remake-instance svi
                                            :cid-vector +cid-unassigned+
                                            :value-vector +vi-value-unassigned+))))

        ;; Now re-retrieve the current SVI value after deleting the current cid-txn value for a subsequent
        ;; test of the 'no-change' condition to be performed below.
        (multiple-value-setq (old-cid old-value)
          (scalar-versioned-value/active-value-pair
           svi (repository-transaction/cid-set transaction)))) ; get active pair or NIL

      (debug-message 5 "SVI old-cid is ~s, new-cid is ~s" old-cid cid)
      (when (and old-cid (vi-value-same? new-value old-value))
        (versioned-object-no-change-condition-signal instance slot new-value))

      ;; If we reach this point, either the value was different, or the no-change condition signalled
      ;; did not abort the operation with non-local flow control, so we proceed with the operation.
      ;; Create a new cid/value pair
      ;; (assuming we're not allowing multiple same-CID assignments, checked above)
      (scalar-versioned-value/add-value svi cid new-value)

      ;; Log the change.
      (repository-transaction/log-change transaction instance slot)
      new-value)))

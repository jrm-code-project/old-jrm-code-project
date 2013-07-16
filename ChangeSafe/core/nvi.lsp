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

(defclass nonlogged-versioned-value          (versioned-value)
   ;; Note that the value slot should also be archived in the sb32vec if it's numeric.
   ;; We should perhaps be allocating a 2-entry slot for sb32vec and storing values there
   ;; with sentinels here to indicate whether the value is here, or in the sb32 vector slot.
   ;; *PERFORMANCE* - REALLOCATION ON THE VALUE SLOT FOR NON-PCLOS TYPES
   ((value :initarg :initial-value
           :initform +vi-value-unassigned+
           :reader nonlogged-versioned-value/value)             ;current value of this VI slot
    (cid   :initarg :cid
           :initform +cid-unassigned+
           :reader nonlogged-versioned-value/cid))
  (:documentation
     "Non-Versioned-Index, dosesn't log changes, doesn't do versioning. DOES throw no-change exceptions.
      Most likely for use in those cases where you need to make updates of a frequent nature and
      change management repository logging would pose lock contention or undue space overhead.
      For instance, if you update the repository once every 10 minutes to record some persistent artifact,
      you don't necessarily want a change-set entry made for every update.
      As such, the CID which we store here may be *CID-UNASSIGNED*, for anonymous updates
      with no CID-MASTER-TABLE entries.")
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defmethod versioned-value/most-recent-cid ((versioned-value nonlogged-versioned-value) cid-set)
  (let ((nvicid (nonlogged-versioned-value/cid versioned-value)))
    (and (not (eq nvicid +cid-unassigned+))
         (cid-set/member cid-set nvicid)
         nvicid)))

(defmethod versioned-value/view ((versioned-value nonlogged-versioned-value) cid-set instance slot)
  (declare (ignore cid-set))
  (debug-message 5 "versioned-value/view for ~s, ~s, ~s" versioned-value instance slot)
  (let ((value (nonlogged-versioned-value/value versioned-value)))
    (if (eq value +vi-value-unassigned+)
        (slot-unbound (class-of instance) instance slot) ; signal an error
        value)))

(defmethod versioned-value/update (new-value (versioned-value nonlogged-versioned-value) transaction instance slot)
  ;; Transform the old value to new.

  ;; Check for no change, unless it is in the same transaction
  (unless (= (nonlogged-versioned-value/cid versioned-value)
             (repository-transaction/cid transaction))
    (when (vi-value-same? (nonlogged-versioned-value/value versioned-value)
                          new-value)
      (versioned-object-no-change-condition-signal instance slot new-value)))

  ;; If we reach this point, either the value was different, or the no-change condition signalled
  ;; did not want us to abort the operation.  Go ahead as if the value has changed.
  (remake-instance versioned-value
                   :initial-value new-value
                   :cid   (repository-transaction/cid transaction))

  ;; Note the CID which is responsible.  Since this index isn't logged, we don't have to do more.
  ;; The whole point of this version technique is non-logged value management.  That mostly
  ;; means we don't want to necessarily require a change-set to modify this value.
  ;; This call, while it doesn't modify a cid-detail/master-table-entry, does note that versioned
  ;; and potentially CID-specific change was made in the transaction.  If the transaction was
  ;; allocated a CID, this allows us to implement TXN-CONTEXT-ABORT-IF-NO-CHANGE.
  ;; Normally this is maintained by the TXN-CONTEXT-LOG-CHANGE operation.  THe whole NVI
  ;; thing may be pointless and should possible be replaced with LNVI.  However for now,
  ;; this allows VERSION-OBJECT subtypes to be automatic trackers of whether change occurs,
  ;; and support import/export with full automatic behavior.
  (repository-transaction/log-change transaction instance slot)
  new-value)

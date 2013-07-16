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

(eval-when (:load-toplevel :execute)
  (export '(versioned-value/view
            versioned-value/update)))

(defconstant +vi-value-unassigned+ '|unassigned versioned value|
  "Value of a slot which has no value.  Means no VI managed slot may legitimately have this
   value")

(defclass versioned-value () ()
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defmethod clos:initialize-instance :before ((instance versioned-value) &rest initargs)
  (debug-message 5 "Creating a ~s to hold ~s" instance initargs))

(defgeneric versioned-value/cid-list (versioned-value)
  ;; *TO-DO*: implement VI-CID-LIST via VI-MAP-CIDS to reduce cloned logic.
  (:documentation
   "Reporting function to return a (possibly empty) list of cids which have altered
    the indicated versioned index.
    See also: VI-MAP-CIDS for a non-consing interface."))

(defgeneric versioned-value/cid-set (repository versioned-value)
  ;; *TO-DO*: implement VI-CID-LIST via VI-MAP-CIDS to reduce cloned logic.
  (:documentation
   "Reporting function to return a (possibly empty) cid-set which have altered
    the indicated versioned index."))

(defgeneric versioned-value/contains-cid? (versioned-value cid)
  (:documentation
   "Return true if the versioned value contains content indexed by CID.  Return NIL otherwise.
    NOTE: there is a distinction between *referencing* a cid, and containing content for a cid.
    This method return true only if content for the cid is contained, it will return false
    if there are references to the cid without content in the VI for the CID."))

(defgeneric versioned-value/most-recent-cid (versioned-value cid-set)
  (:documentation
   "Reporting function to return the CID in CID-SET which most recently modified the VI, or NIL.
    Note that this could be computed using VI-CID-LIST, however indexes are structured such that
    by implementing this on a per-index subtype basis for some indexes we can do a more efficient
    search which doesn't require comparing the timestamps of all cids in the view on the index."))

(defgeneric versioned-value/view (versioned-value cid-set instance slot)
  (:documentation
   "Return the realized value of a versioned-value as seen under CID-SET.")
  (:method ((versioned-value persistent-node-id) cid-set instance slot)
    ;; If we're outside a transaction, we can't see a thing.
    ;; This, at least, makes the debugger happy.
    versioned-value))

(defgeneric versioned-value/update (new-value versioned-value transaction instance slot)
  (:documentation
   "Using the value as seen under CID-SET, record the modifications necessary
    to make the value appear as NEW-VALUE."))

(defgeneric vi-value-same? (new-value old-value)
  (:documentation
   "Compare two single-record values (of primitive type).
    Return NON-NIL if the two values are the same, NIL otherwise.
    If NIL is returned, chances are the caller should signal VI-NO-CHANGE-CONDITION if this is
    a VI-SET-VALUE attempt.

    Arguments are as supplied to VI-STORE-UNVERSIONED-VALUE.

    VI-VALUE-SAME? is only suitable for use with non-composite versioning because it doesn't
    return differences between values, only T/NIL.  Composite versioning
    (or other techniques which care about deltas between files) should call VI-VALUE-DIFFERENCE
    to obtain a set of differences, this predicate yields no differences, only T or NIL.

    If the arguments are of type VI-VALUE-STREAM,
    it is up to the caller to rewind (either or both) stream arguments after calling this function.

    WARNING: This function does NOT handle comparison of termination properties for textual values.
    Since this is a property of a stream and not the values yielded by the stream.")
  (:method ((new-value t) (old-value t))
    (equal new-value old-value))
  ;; String case was handled by the array method below but that is very slow due to the call to subtypep.
  ;; Note also that for efficiency some callers inline this method for strings (e.g. see core/rsdiff.lsp)
  (:method ((new-value string) (old-value string))
    (string= new-value old-value))
  (:method ((new-value array) (old-value array)) ; Todo: JDT should fix as need be, cf. prior comment
    (let ((element-type (array-element-type new-value)))
      (and (equalp element-type
                   (array-element-type old-value))
           ;; Don't recursively descend with EQUALP on arrays of objects.
           ;; But since EQUAL doesn't care specialized arrays other than element-type T
           ;; except for bit-vectors and strings, we want to check for arrays of bytes.
           (cond ((or (subtypep element-type 'unsigned-byte)
                      (subtypep element-type 'signed-byte))
                  (equalp new-value old-value))
                 ((subtypep element-type 'character)
                  (string= new-value old-value)) ;redundant use for string (see below), but maybe faster
                 (t
                  (equal new-value old-value)))))) ;handles objects, strings, bit-vectors
  )

(define-condition versioned-object-no-change-condition (changesafe-simple-warning)
  ((object-being-changed
    :initarg :object-being-changed
    :reader versioned-object-no-change-condition/object-being-changed)
   (slot-name
    :initarg :slot-name
    :reader versioned-object-no-change-condition/slot-name)
   (slot-value
    :initarg :slot-value
    :reader versioned-object-no-change-condition/slot-value))
  (:report (lambda (condition stream)
             (format stream
                     "A change operation was requested on the ~s slot of versioned object ~s of type ~s, ~
                but there was no change between the old and new values (~s)."
                     (versioned-object-no-change-condition/slot-name condition)
                     (versioned-object-no-change-condition/object-being-changed condition)
                     (type-of (versioned-object-no-change-condition/object-being-changed condition))
                     (versioned-object-no-change-condition/slot-value condition)))))

(defun versioned-object-no-change-condition-signal (vo slot-name &optional (value '<slot-value-unavailable>))
  "Signal a versioned-index no-change condition
   VO must be a versioned object instance of a class defined with DEFINE-VERSIONED-CLASS.
   SLOT-NAME must be a symbol indicating the slot-name (compatible with SLOT-VALUE) of the class instance."
  ;; This should probably be an error now that we have some usage hindsight.  The problem
  ;; is that this condition, if unhandled and/or declined in handlers, prints a message but
  ;; makes bogus change records in the change indices.
  (warn
   'versioned-object-no-change-condition
     :object-being-changed vo
     :slot-name slot-name
     :slot-value value
     :format-control
     "A change operation was requested on the ~s slot of versioned object ~s of type ~s, ~
                   but there was no change between the old and new values (~s)."
     :format-arguments (list slot-name vo (type-of vo) value)
     ))

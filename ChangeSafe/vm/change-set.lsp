;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright (c) 1999-2000 Content Integrity, Inc.
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;;          Content Integrity, Inc. CONFIDENTIAL and PROPRIETARY material.
;;;;
;;;;          Content Integrity, Inc
;;;;          Braintree Executive Office Park
;;;;          P.O. Box 850942
;;;;          Braintree, MA 02185-0942
;;;;
;;;; This software and information comprise valuable intellectual property
;;;; and trade secrets of Content Integrity, Inc., developed at substantial
;;;; expense by Content Integrity, which Content Integrity intends to
;;;; preserve as trade secrets.  This software is furnished pursuant to a
;;;; written license agreement and may be used, copied, transmitted, and
;;;; stored only in accordance with the terms of such license and with the
;;;; inclusion of the above copyright notice.  This software and
;;;; information or any other copies thereof may not be provided or
;;;; otherwise made available to any other person.  NO title to or
;;;; ownership of this software and information is hereby transferred.
;;;; Content Integrity assumes no responsibility for the use or reliability
;;;; of this software.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; File Name:     change-set.lsp
;;;; Author:        Dave Tenny
;;;;
;;;; Module Description:
;;;;
;;;; Versioned data wrappers for information pertaining to change-sets.
;;;; The basic problem we're solving with this module is that there are
;;;; attributes of change-sets we'd like to have mutable and under version
;;;; control.  However we *really* don't want to complicate the life of the
;;;; CORE CID, CID-SET, and CID-OBJECT representations, which are concerned
;;;; with the non-mutable and everlasting aspects of change-sets.
;;;;
;;;; Soooo, sigh, here we introduce the mutable information of change-sets,
;;;; which requires versioned context in which to view it.  The logical place
;;;; for anything with versioned context, is, ta-da,  projects.
;;;;
;;;; Note that even though we maintain mutable descriptions of
;;;; change-sets here, it's worth keeping the raw and immutable description
;;;; in the core layer because it it reflects some information about the
;;;; cid-set which we probably NEVER want to change, regardless of which
;;;; version of mutable change-set information you're viewing.
;;;;
;;;; NOTE: we currently explicitly support behavior which makes high-level
;;;; change-set use OPTIONAL, including the grouping of change-sets with
;;;; projects.  Low-level change-sets are never optional for change
;;;; transactions however.  To use high-level change-sets and associate them
;;;; with projects, use the WITH-VM-TXN macro, and other elements defined
;;;; in VM-TXN.LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CSF/VERSION-MANAGEMENT")

(proclaim (standard-optimizations))

(eval-when (:load-toplevel :execute)
  (export '(change-set
            super-change-set
            major-change-set
            minor-change-set
            change-set/cid-object
            change-set/resident-cid
            super-change-set/satellite-change-set-alist
            super-change-set/scan-satellite-change-set-alist
#||
            change-set-get-supplementary-info-plist ; list of supplementary information
            change-set-cid-did-string
            change-set-cid-number
            change-set-name
            change-set-get-information
||#
            )))


(defclass change-set (named-object described-object distributed-object)
  ((cid-object :initarg :cid-object
               :initform (error "no cid-object specified for change set.")
               :accessor change-set/cid-object
               :version-technique :nonversioned)
   ;; Cid-set-basis is handled by the low level, and isn't available until the change-transaction completes.
   ;; Cid-set-pedigree implementation is TBD.

   ;; This is for supplementary information provided by various models which create change-sets.
   ;; The only requirements for its use are that it be proper 'property list' and that all keys and
   ;; values must be able to persist within the repository in which the change-set persists.
   (supplementary-info-plist :initform nil :version-technique :composite-set))
  (:documentation
     "Mutable and versioned information associated with change-sets, the immutable
      components of which are maintained by CORE services.  This is just versioned
      window dressing and support for various SCM processes.

      THERE SHOULD NEVER BE MORE THAN ONE CHANGE-SET CORRESPONDING TO A GIVEN CID.

      BEWARE: the distributed-object-identifier of a high-level change-set is different from
      that of a low-level change-set. Whether it's useful or should not be used is TBD.
      (we could overload the method and return the CID-DID instead of the CHANGE-SET DID).")
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defclass major-change-set (change-set)
  ()
  (:documentation "affects user data rooted in project root directory")
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defclass minor-change-set (change-set)
  ()
  (:documentation "affects project/branch/version attributes, i.e. version machinery")
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defclass super-change-set (change-set)
  ;; A super change set encompasses changes associated with satellite
  ;; repositories.  It is atomic across all the repositories, so we
  ;; can allocate an alist to map the satellite change sets here.
  ((satellite-change-set-alist 
    :initarg :satellite-change-set-alist
    :version-technique :nonversioned
    :reader super-change-set/satellite-change-set-alist))
  (:documentation "change-set applies to a master-repository")
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defmethod print-object ((change-set change-set) stream)
  (print-unreadable-object (change-set stream)
    (format stream "~a most recently named ~s"
            (distributed-object-identifier change-set)
            (named-object/name change-set))))

(defun super-change-set/scan-satellite-change-set-alist (super-change-set)
  "Returns two series, the csf-class and the satellite-cset-did associated with that class."
  (declare (optimizable-series-function 2))
  (map-fn '(values t t)
          (lambda (entry)
            (values (pstore-list/car entry) 
                    (pstore-list/cdr entry)))
          (scan-pstore-list 
           (super-change-set/satellite-change-set-alist super-change-set))))

(defsubst change-set/resident-cid (change-set)
  "Return the numeric repository-local CID of the change-set, often require when querying low level
   change-set information from the core repository interfaces."
  ;; TBD: semantics if low-level cid isn't in repository...
  (cid-object->resident-cid *repository* (change-set/cid-object change-set)))

#||
;;; TO-DO: At some point, we'll want to have hashed lookup of change-set objects given CIDs, DIDs, etc..

;;; We currently support grouping rudimentary classification of change-sets
;;; into major and minor change-sets.  Major csets alter the versioned data reachable via
;;; the project root container.  Minor csets other properties of the version machinery,
;;; such as the project name.

(defconstant *change-set-types*
    '(:major                            ;affects user data rooted in project root directory
      :minor                            ;affects project/branch/version attributes, i.e. version machinery
      :super-cset)                      ;change-set applies to a master-repository
  "Classification of change-sets used to filter interesting from uninteresting change-sets
   according to the type of data they change.  Note that major can imply minor, but not vice versa.")

(define-versioned-class change-set
    (:documentation
     "Mutable and versioned information associated with change-sets, the immutable
      components of which are maintained by CORE services.  This is just versioned
      window dressing and support for various SCM processes.

      THERE SHOULD NEVER BE MORE THAN ONE CHANGE-SET CORRESPONDING TO A GIVEN CID.

      BEWARE: the distributed-object-identifier of a high-level change-set is different from
      that of a low-level change-set. Whether it's useful or should not be used is TBD.
      (we could overload the method and return the CID-DID instead of the CHANGE-SET DID)."
     :super-classes (named-object described-object))
  ;; Type could be inferred for more accurate and safe results. For now, we let the user decide
  ;; and do it manually.  But we could verify the low level cid change records to verify that type
  ;; is correct if we want to.
  (type :initial-value nil :version-technique nil) ; one of *change-set-types*, immutable once assigned
  (cid-object :initial-value nil :version-technique nil) ; immutable once assigned
  ;; Cid-set-basis is handled by the low level, and isn't available until the change-transaction completes.
  ;; Cid-set-pedigree implementation is TBD.

  ;; This is for supplementary information provided by various models which create change-sets.
  ;; The only requirements for its use are that it be proper 'property list' and that all keys and
  ;; values must be able to persist within the repository in which the change-set persists.
  (supplementary-info-plist :initial-value nil :version-technique :composite)
  )

(defun change-set-create (cid-object type &key name description supplementary-info-plist)
  "Create a CHANGE-SET object which wrappers the low-level change-set implementation.
   CID-OBJECT must be of type CID-OBJECT, and represents the handle to the low-level information.

   NAME is the versioned name for the change-set and is optional.
   TYPE is one of *CHANGE-SET-TYPES*, and states the class of data manipulated by the change-set.
   DESCRIPTION is the versioned change-set description.
   SUPPLEMENTARY-CHANGE-PLIST is a (possibly empty) plist which is used to provide
   supplementary information attached to change-sets which are created for change transactions."
  ;; Assuming for now there's no problem getting a CID-OBJECT for the current TXN cid which
  ;; is being created, since we'd prefer to create the CHANGE-SET object during creation of the
  ;; low-level change-set which it represents, to ensure that the high level change-set is
  ;; importaned/exported with the low-level cid-index change-set implementation.  In fact, we enforce
  ;; this requirement!
  (unless (= (txn-context-cid *txn-context*)
             (cid-object-as-number cid-object))
    (error "Attempt to create an SCM::CHANGE-SET object outside of the low-level change-set
            which it represents."))
  (unless description
    (setq description (repository-get-cid-information *repository* (cid-object-as-number cid-object))))
  (let ((change-set (make-instance 'change-set)))
    (set-change-set-cid-object change-set cid-object)
    (when description
      (set-described-object-text change-set description))
    (when type
      (set-change-set-type change-set type))
    (when name
      (named-object-initialize change-set name))
    (when supplementary-info-plist
      (assert (evenp (length supplementary-info-plist)))
      (set-change-set-supplementary-info-plist change-set supplementary-info-plist))
    change-set))

(defsubst change-set-get-supplementary-info-plist (change-set)
  "Return the SUPPLEMENTARY-INFO-PLIST slot of a change-set.  See slot definition comments for details."
  (vi-stream-as-list (change-set-supplementary-info-plist change-set)))

(defsubst change-set-cid-did-string (change-set)
  "Return the preferred canonical form of ID to be used in referencing change-sets,
   which currently defaults to the DID-string of its low-level repository CID."
  ;; ***** NOTE *****
  ;; YOU MAY THINK OF OVERRIDING THE DISTRIBUTED-OBJECT-IDENTIFIER METHOD FOR THE CHANGE-SET CLASS
  ;; TO RETURN THE LOW-LEVEL CID-DID. DON'T DO IT.  IF YOU WANT THE LOW LEVEL CID-DID FOR A HIGH LEVEL
  ;; CHANGE-SET, USE THIS METHOD, which returns a string form.  It would be highly inconsistent
  ;; and inappropriate to break the ability to get DIDs for high-level change-sets which can be used
  ;; as a handle to access the high-level object.
  (cid-object-did-string (change-set-cid-object change-set)))

(defsubst change-set-cid-number (change-set)
  "Return the numeric repository-local CID of the change-set, often require when querying low level
   change-set information from the core repository interfaces."
  ;; TBD: semantics if low-level cid isn't in repository...
  (cid-object-as-number (change-set-cid-object change-set)))

(defsubst change-set-name (change-set)
  "Return the versioned name of a change-set."
  (object-user-name change-set))

(defmethod print-object ((change-set change-set) stream)
  (print-unreadable-object (change-set stream)
    (format stream "~a most recently named ~s"
            (did->string-did (distributed-object-identifier change-set))
            (change-set-name change-set))))

(defmethod change-set-get-information (repository (change-set change-set))
  "Gets the CID information for CHANGE-SET's CID."
  ;; Trampoline from the CHANGE-SET to its CID
  (multiple-value-bind (cid-reason time-stamp)
      (repository-get-cid-information repository (change-set-cid-object change-set))
    (declare (ignore cid-reason))
    (values (described-object-text change-set)
            time-stamp)))
||#


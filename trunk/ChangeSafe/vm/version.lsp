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
;;;; File Name:     version.lsp
;;;; Author:        Dave Tenny
;;;;
;;;; Module Description:
;;;;
;;;; The Version class, which is used to group change-sets and represent
;;;; well known states of branch evolution over time.
;;;;
;;;; This material is arguably the worse and most demo-oriented of the
;;;; VM package, and will need lots of revisions to support cset aggregation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CSF/VERSION-MANAGEMENT")

(proclaim (standard-optimizations))

(eval-when (:load-toplevel :execute)
  (export '(version
            version?
            version/add-and-remove-change-sets
            version/cid-objects
            version/last-update-timestamp
            version/owning-branch
            version/promote-cid
            version/promote-current-cid
            version/promote-change-set
            version/promote-change-sets
            version/scan-cid-objects
            version/frozen?
            version/freeze
#||
            version-get-cid-list-meta-cids
            version-get-resident-cid-set
            version-get-project
            version-owning-branch
            version-promote-current-cid ;deprecated outside the VM package.
            version-promote-change-set  ;preferred
            version-update-change-sets  ;misnamed, doesn't take change-sets as arguments.
            version-add-and-remove-change-sets
            version-last-update-timestamp
            version-list-cset-changes
            version-list-cid-information
            version-list-cid-information-active-and-inactive
            version-ancestor-version
            version-mutable?
            version-freeze
            with-version
            *with-version-context*
            *with-version-cid-set-cache*
            *with-version-performance-tests*
            version-context-assert
||#
            )))

(defclass version (named-object described-object distributed-object)
  (
   (frozen? :initform nil
            :version-technique :logged
            :reader version/frozen?) ;once t, forever t.

   ;; Never store CID numbers in non-CORE routines, they must be relocated for distributed cset capabilities.
   ;; Use CID-OBJECT instances to represent CIDs.

   ;; **** WARNING ********* WARNING ********* WARNING ********* WARNING ********* WARNING *****
   ;; Cid-objects may reflect cids which aren't imported!  I.e. you can export a cid-object reference
   ;; to a cid and import it, without importing the cid which is referenced by the cid-object.
   ;; These are the cid-objects in this version.
   (cid-objects :initform nil
                :version-technique :composite-sequence
                :accessor version/cid-objects)

   (owning-branch :initarg :owning-branch
                  :initform (error "Required initarg :owning-branch omitted.")
                  :version-technique :nonversioned
                  :reader version/owning-branch)

   (ancestor-version :initarg :ancestor-version
                     :initform nil
                     :version-technique :nonversioned
                     :reader version/ancestor-version)

   (creation-cid-object :initform (repository-transaction/cid-object *transaction*)
                        :version-technique :nonversioned
                        :reader version/creation-cid-object))

  (:default-initargs :name "Latest")
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defmethod clos:shared-initialize :after ((instance version) slot-names &rest initargs
                                          &key ancestor-version &allow-other-keys)
  (when ancestor-version
    (setf (version/cid-objects instance)
          (version/cid-objects ancestor-version))))

(defun version? (thing)
  (typep thing 'version))

(defun version/freeze (version)
  (unless (version/frozen? version)
    (setf (slot-value version 'frozen?) t)))

(defun version/scan-cid-objects (version)
  (declare (optimizable-series-function))
  (versioned-object/scan-composite-versioned-slot version 'cid-objects))

(defun version/promote-current-cid (version)
  "Promote the CID from the current change transaction into VERSION.
   Return the new cid-did-string and the CID-OBJECT which represents it (2 values)."
  (let ((cid-object (repository-transaction/cid-object
                     csf/core::*transaction*)))
    (values (version/promote-cid version cid-object) cid-object)))

#||
(defclass version (named-object described-object)
  ;; The mutable property will change only once for a version.
  ;; YOu might think we'd have to store value as :SCALAR for time travel.  But if you view
  ;; a branch which lists the version as active, even though it is in fact now inactive,
  ;; we want to make sure you can't accidentially modify it.  Immutability is forever.
  (

(mutable? :initform t :version-technique :logged) ;once nil, forever nil.

   ;; The name property is like to the MUTABLE? property, it changes only once after initialization.
   ;; Unlike the mutable property, we may wish to have this be logged :scalar to allow time-travel
   ;; views to see it as latest when it's reported as latest, as long as we don't allow it to mutate.
   ;; (name :version-technique :scalar)  ; now inherited from named-object

   ;; Never store CID numbers in non-CORE routines, they must be relocated for distributed cset capabilities.
   ;; Use CID-OBJECT instances to represent CIDs.

   ;; **** WARNING ********* WARNING ********* WARNING ********* WARNING ********* WARNING *****
   ;; Cid-objects may reflect cids which aren't imported!  I.e. you can export a cid-object reference
   ;; to a cid and import it, without importing the cid which is referenced by the cid-object.
   ;; These are the cid-objects in this version.
   (cid-objects :initform nil :version-technique :composite-set)


   ;; Owning-branch is the branch that contains this version.  Note that we may need to support
   ;; just a direct owning-project reference for floating versions which aren't part of a branch.
   ;; for demo, this will always refer to the branch, later we could let it switch hit.
   (owning-branch :version-technique nil) ; doesn't change after initialization
   ;; Version from which this version was derived, or NIL if there was no ancestor version
   (ancestor-version :version-technique nil) ;doesn't change after initialization
   ;; CID-OBJECT representing CID which created this version.  Sometimes useful, should perhaps
   ;; be generalized for all versioned objects.
   (creation-cid-object :version-technique nil)

   ) ;doesn't change after initialization
  (:documentation
   "Version belonging to a branch.  Currently just a set of CIDs for the demo."
   )                                    ;checkpoint description
  (:metaclass versioned-class)
  )
   ||#
#||
(defun version-freeze (version)
  (set-version-mutable? version nil))

(defun version-create (owning-branch &key description ancestor-version)
  "Create a VERSION object.  DESCRIPTION, if present, should be a string.

   If ANCESTOR-VERSION is specified, we inherit the current metaversion-governed view of its
   active change-sets as the initial set of change-sets for the newly created version.

   Return TWO values, the version, and the cid-did-string of the cid which created the version."
  (let ((version (make-instance 'version)))
    (named-object-initialize version "Latest")
    (set-version-owning-branch version owning-branch)
    (when description
      (set-described-object-text version description))
    (set-version-ancestor-version version ancestor-version)
    (when ancestor-version
      (version-inherit-cid-set version ancestor-version))
    (let ((current-cid-object (cid-object-find-or-create (txn-context-cid *txn-context*))))
      (set-version-creation-cid-object version current-cid-object)
      (values version (cid-object-did-string current-cid-object)))))

(defmethod object-change-interesting-p or ((object version) (slot-name symbol) birth-cid-p)
  (and (not birth-cid-p)
       (find slot-name '(mutable? cid-objects) :test #'eq)))

(defmethod object-change-user-semantics or ((object version) (slot-name symbol)
                                            change-type valid-p new-or-old old)
  (declare (ignore valid-p new-or-old old))
  (case slot-name
    (mutable? (cons "cap version" nil)) ;assuming we don't call this for birth cid of version...
                                        ; also indicating via cons that the actual changes values are NOT
                                        ; of interest to the user, and that they shouldn't be reported.
    (cid-objects (format nil "~(~a~) change-set" change-type)))) ; convert keyword to lower case

(defun version-inherit-cid-set (new-version old-version)
  "Propagate CID-SET in old version to NEW-VERSION, clobbering contents of new-version.
   Demo code, will likely change quite a bit."
  ;; Strictly speaking, we should be able to set new cid objects directly from the
  ;; vi-record-stream which results from accessing the prior cvi value.  Try it sometime... *FINISH*
  (set-version-cid-objects new-version (version-get-cid-object-list old-version)))

(defun version-promote-current-cid (version)
  "Promote the CID from the current change transaction into VERSION.
   Return the new cid-did-string and the CID-OBJECT which represents it (2 values)."
  (let ((cid-object (cid-object-find-or-create (txn-context-cid *txn-context*))))
    (values (version-promote-cid version cid-object) cid-object)))

||#

(defsubst version/promote-change-set (version change-set)
  "Promote CHANGE-SET, which must be a change-set object, into VERSION.
   This is more desirable than exposing CID interfaces outside the version module.
   Return the version."
  (version/promote-cid version (change-set/cid-object change-set))
  version)

(defsubst version/promote-change-sets (version change-sets)
  "Promote CHANGE-SETS, which must be change-set objects, into VERSION.
   This is more desirable than exposing CID interfaces outside the version module.
   Return the version."
  (version/promote-cids version (map 'list #'change-set/cid-object change-sets))
  version)

(defun version/promote-cid (version cid-object)
  (version/promote-cids version (list cid-object))
  cid-object)

(defun version/promote-cids (version cid-objects)
  (let ((old-cid-objects (version/cid-objects version)))
    (setf (version/cid-objects version)
          (sort (union cid-objects old-cid-objects) #'< 
                :key (lambda (cid-object)
                       (cid-object->resident-cid *repository* cid-object))))
    cid-objects))

(defun version/add-and-remove-cid-objects (version cid-objects-to-add cid-objects-to-remove)
  (let ((current-cid-objects (version/cid-objects version)))
    (when (version/frozen? version)
      (error "Version ~s is capped and cannot be altered (an attempt was made to update its change-set list)."
             (named-object/name version)))
    (when (intersection cid-objects-to-add cid-objects-to-remove)
      (error "Attempt to both add AND remove at least one change-set.  Adding ~s, removing ~s."
             cid-objects-to-add cid-objects-to-remove))
    (when (intersection cid-objects-to-add current-cid-objects)
      (error "Attempt to add cid-objecs which are already present (one or more of ~s)"
             cid-objects-to-add))
    ;; Currently not signalling an error if objects to remove aren't already present.
    (setf (version/cid-objects version)
          (nconc cid-objects-to-add
                 (set-difference current-cid-objects cid-objects-to-remove)))))

(defun version/add-and-remove-change-sets (version change-sets-to-add change-sets-to-remove)
  "Given VERSION, which must be mutable and which must be viewed with the 'latest' metaversion,
   add and remove the indicated change-sets.

   CHANGE-SETS-TO-ADD is a (possibly empty) list of change-set objects to be added to the version.
   CHANGE-SETS-TO-REMOVE is a (possibly empty) list of change-set objects to be removed from the version.

   Returns NIL."
  (version/add-and-remove-cid-objects version
                                      (mapcar #'change-set/cid-object change-sets-to-add)
                                      (mapcar #'change-set/cid-object change-sets-to-remove)))

(defun version/last-update-timestamp (version metaversion-cid-set repository)
  "Return the timestamp of the change-set which last updated the version by altering its
   content cid-set, or NIL if there is no cid in metaversion-cid-set which updated the version.

   METAVERSION-CID-SET must be a cid-set.

   WARNING: REPOSITORY must be that repository which contains VERSION, and METAVERSION-CID-SET
   must apply to the same REPOSITORY. "
  (let ((cid (versioned-object/most-recent-slot-cid version 'cid-objects metaversion-cid-set)))
    (and cid
         ;; yuck
         (cid-master-table/cid-comparison-timestamp
          (repository/cid-master-table repository)
          cid))))

#||
(defgeneric version-promote-cid (version cid)
  (:documentation
   "Promote the CID identified by CID-DID-STRING into VERSION.
     Return the cid-did-string of the new cid.")
  (:method ((version version) (cid-did-string string))
    (version-promote-cid version (cid-object-find-or-create cid-did-string)))

  (:method ((version version) (new-cid-object cid-object))
    (if (debug-level-meets-or-exceeds? 4)
        ;; If we're paranoid, do the long check.  NOTE: this has significant performance impact on
        ;; repositories  with lots of change-sets.
        (let ((current-cid-objects (vi-stream-as-list (version-cid-objects version))))
          (when (find new-cid-object current-cid-objects)
            (error "~a is already in the version" new-cid-object))
          (push new-cid-object current-cid-objects)
          (set-version-cid-objects version current-cid-objects))
      ;; Quick way, no safeties
      (versioned-object-push-composite-values version 'cid-objects (list new-cid-object)))
    ;; Return value
    (cid-object-did-string new-cid-object)))

(defun version-add-and-remove-change-sets (version change-sets-to-add change-sets-to-remove)
  "Given VERSION, which must be mutable and which must be viewed with the 'latest' metaversion,
   add and remove the indicated change-sets.

   CHANGE-SETS-TO-ADD is a (possibly empty) list of change-set objects to be added to the version.
   CHANGE-SETS-TO-REMOVE is a (possibly empty) list of change-set objects to be removed from the version.

   Returns NIL."
  (unless (version-mutable? version)
    (error "Version ~s is capped and cannot be altered (an attempt was made to update its change-set list)."
           (object-user-name version)))
  (let ((cid-objects-to-add (mapcar #'change-set-cid-object change-sets-to-add))
        (cid-objects-to-remove (mapcar #'change-set-cid-object change-sets-to-remove))
        (current-cid-objects (vi-stream-as-list (version-cid-objects version))))
    (when (intersection cid-objects-to-add cid-objects-to-remove)
      (error "Attempt to both add AND remove at least one change-set.  Adding ~s, removing ~s."
             change-sets-to-add change-sets-to-remove))
    (when (intersection cid-objects-to-add current-cid-objects)
      (error "Attempt to add change-sets which are already present (one or more of ~s)"
             change-sets-to-add))
    ;; Currently not signalling an error if objects to remove aren't already present.
    (set-version-cid-objects version
                             (nconc cid-objects-to-add
                                    (set-difference current-cid-objects cid-objects-to-remove)))))

(defsubst version-get-cid-object-list (version)
  "Return CID-OBJECTs currently in this version"
    (vi-stream-as-list (version-cid-objects version)))

(defsubst version-get-cid-did-string-list (version)
  "Return the CID-DID strings for cids contained by the version."
  (mapcar #'did->string-did (version-get-cid-object-list version)))

(defgeneric version-aux-list-cid-object-information (cid-rep &optional cid-did-string)
  (:documentation
   "Utility routine to provide the following information about low-level change-sets (cids) for various
   purposes.  Arguably useful as a method on CID-OBJECT, which resides in CORE, except that
   we retrieve versioned information from change-set, which resides in VM.

   CID-REP is either a CID-OBJECT or a CID, and is prepared to handle cid-objects representing
   change-sets which aren't present in the repository.

   CID-DID-STRING, if known and specified, can speed up results of the CID-driven method.

   Return a list with the following information for each CID-REP:
   a) the cid string did
   b) the versioned change-set description, or cid description (reason) if the former is unavailable
   c) the cid finish timestamp or NIL if the CID isn't present in the repository.
   d) the change-set user name if available, or NIL otherwise.")
  (:method ((cid integer) &optional cid-did-string)
    ;; If we have a cid, we KNOW the change-set is in the repository.
    (multiple-value-bind (cid-description cid-timestamp change-set)
        (repository-get-cid-information (txn-context-repository *txn-context*) cid)
      ;; Use the cid-did-string if supplied, it's cheaper than resynthesizing the cid-did-string
      (list (or cid-did-string
                (did->string-did (repository-get-cid-distributed-identifier
                                  (txn-context-repository *txn-context*) cid)))
            (or (and change-set
                     (described-object-text change-set))
                cid-description)
            cid-timestamp
            (and change-set
                 (object-user-name change-set)))))
  (:method ((cid-object cid-object) &optional (cid-did-string (cid-object-did-string cid-object)))
    (if (cid-object-cset-imported? cid-object)
        (version-aux-list-cid-object-information (cid-object-as-number cid-object) cid-did-string)
      ;; We can't get information about the cid, it isn't present in the repository.
      (list cid-did-string "CID not imported: N/A" nil nil))))

(defun version-list-cid-information (version)
  "Return two values, (1) the version user name, (2) a list of sublists, each of which contains:
   a) the cid string did
   b) the versioned change-set description, or cid description (reason) if the former is unavailable
   c) the cid finish timestamp or NIL if the CID isn't present in the repository.
   d) the change-set user name if available, or NIL otherwise.

   NOTE: returned list is in no particular order, though we give an approximate preference
   for most-recent-cids first."
  (values (object-user-name version)
          (let ((result nil))
            (dolist (cid-object (nreverse (version-get-cid-object-list version)))
              (push (version-aux-list-cid-object-information cid-object) result))
            result)))

(defun version-list-cid-information-active-and-inactive (version)
  "Similar to VERSION-LIST-CID-INFORMATION. We return two tuple lists, one for active csets in the version,
   and one for csets in the version's owning project which are NOT active in the version.

   There are two return values, each a list of sublists, where each sublist contains:

   a) the cid string did
   b) the versioned change-set description, or cid description (reason) if the former is unavailable
   c) the cid finish timestamp or NIL if the CID isn't present in the repository.
   d) the change-set user name if available, or NIL otherwise.

   The first list represents active csets, the second list represents inactive csets."
  #+allegro (declare (:fbound project-change-set-list))
  (let* ((project (version-get-project versioN))
         (active-cid-objects (version-get-cid-object-list version))
         (project-cid-objects (mapcar #'change-set-cid-object (project-change-set-list project)))
         (inactive-cid-objects (set-difference project-cid-objects active-cid-objects :test #'eq)))
    (values (mapcar #'version-aux-list-cid-object-information active-cid-objects)
            (mapcar #'version-aux-list-cid-object-information inactive-cid-objects))))

(defun version-get-active-resident-cids (version &optional cid-object-list)
  "Call this if you want to get the list of cids referenced by a version, but are uncertain
   as to whether all change-sets referenced in the version have actually been imported in the repository.
   If CID-OBJECT-LIST is specified, we use that instead of [re]-retrieving it from the version."
  (loop for cid-object in (or (version-get-cid-object-list version) cid-object-list)
      when (cid-object-cset-imported? cid-object)
      collect (cid-object-as-number cid-object)))

(defun version-get-resident-cid-set (version)
  "Returns a CID-SET that represents the cids referenced by this version that have been
imported into the repository.  If the version refers to change-sets that have *not* been
imported, these changes cannot be mapped to CIDs, and thus cannot be placed in a CID-SET."
  (let ((result (cid-set-create-empty :DENSE)))
    (vi-stream-for-each
     (lambda (cid-object)
         (when (cid-object-cset-imported? cid-object)
           (cid-set-activate-cid result (cid-object-as-number cid-object)))
         )
     (version-cid-objects version))
    result))

(defun version-update-change-sets (version cid-list)
  ;; Misnamed, doesn't take change-sets as arguments.
  "Examine CID-LIST to determine requests for additions and deletions of change-sets to a version.
   If there are none, return NIL.
   If the version is capped, signal an error.

   If additions or deletions are requested, update the cset contents of the version,
   and return TWO values:

   1) A list of cset tuples for added csets.
   2) A list of cset tuples for deleted csets.

   Each sublist in the tuple lists will contain
   information as per VERSION-AUX-LIST-CID-OBJECT-INFORMATION, which is to say:

   a) the cid string did
   b) the versioned change-set description, or cid description (reason) if the former is unavailable
   c) the cid finish timestamp or NIL if the CID isn't present in the repository.
   d) the change-set user name if available, or NIL otherwise."
  ;; This function works safely if any cid-objects in referenced by version aren't present in the
  ;; repository.
  (unless (version-mutable? version)
    (error "Version ~s is capped and cannot be altered (an attempt was made to update its change-set list."
           (object-user-name version)))
  (let* ((active-cid-objects (version-get-cid-object-list version))
         (active-cids (version-get-active-resident-cids version active-cid-objects))
         (cids-to-add (set-difference cid-list active-cids :test #'=))
         (cids-to-delete (set-difference active-cids cid-list :test #'=)))
    (when cids-to-delete
      ;; Process deletions
      (setq active-cid-objects
        (loop for cid-object in active-cid-objects
            unless (and (cid-object-cset-imported? cid-object)
                        (find (cid-object-as-number cid-object) cids-to-delete :test #'=))
            collect cid-object)))
    ;; Process additions
    (dolist (cid cids-to-add)
      ;; Similar to version-promote-cid, without expensive pre-existence check
      (push (cid-object-find-or-create cid) active-cid-objects))
    ;; Update cid-object slot if there were changes, and return the appropriate result,
    ;; otherwise just return nil.
    (if (or cids-to-add cids-to-delete)
        (progn
          (set-version-cid-objects version active-cid-objects)
          (values (mapcar #'version-aux-list-cid-object-information cids-to-add)
                  (mapcar #'version-aux-list-cid-object-information cids-to-delete)))
      (values nil nil))))

(defun version-last-update-timestamp (version metaversion-cid-set
                                      &key (repository *repository*))
  "Return the timestamp of the change-set which last updated the version by altering its
   content cid-set, or NIL if there is no cid in metaversion-cid-set which updated the version.

   METAVERSION-CID-SET must be a cid-set.

   WARNING: REPOSITORY must be that repository which contains VERSION, and METAVERSION-CID-SET
   must apply to the same REPOSITORY. "
  (let ((cid (versioned-object-most-recent-slot-cid version 'cid-objects metaversion-cid-set)))
    (and cid
         ;; I use multiple-value-bind here instead of multiple-value-list in order to
         ;; avoid consing.
         (multiple-value-bind (cid-description cid-timestamp)
             (repository-get-cid-information repository cid)
           (declare (ignore cid-description))
           cid-timestamp))))

(defun version-get-cid-list-meta-cids (version &key include-creation-cid)
  "Return a list of all CIDs which have operated on the cid-list for this version
   i.e. the CID-OBJECTS slot.  These aren't the cids which are officially active in the version,
   rather, these are cids which have modified the officially active cid list.

   NOTE: minor trickiness: reread that last paragraph.
   This is useful if you want to do cid-driven analysis of changes to the version cid list.

   INCLUDE-CREATION-CID, if true, includes the cid which created the cid-list initially,
   and which is typically responsible for the initial add of all CIDs to this version which were
   implied by the ancestor version for this version.

   This routine returns a list of CIDs, not CID-OBJECTs, since that's what the low-level services
   provide and we don't necessarily need to convert them."
  (let ((vi-slot-cids (versioned-object-slot-cids version 'cid-objects)))
    (if include-creation-cid
        vi-slot-cids
      (delete (cid-object-as-number (version-creation-cid-object version))
              vi-slot-cids :test #'=))))

(defun version-list-cset-changes (version)
  "Return values as specified by RFM-SERVER-LIST-VERSION-CSET-CHANGES,
   reporting on changeset activation and deactivation in this version."
  (let ((result nil)
        (meta-cids (version-get-cid-list-meta-cids version)))
    ;; Here we use the CVI mechanisms to determine cid-objects added and deleted to this
    ;; version since its creation.  Neat, if slighly tricky.  The other way to do it is
    ;; compute them from the ancestor version, hash new and old ones, and determine what's new and
    ;; what's missing.
    (loop for cid in meta-cids
                     ;; Meta-cids won't include the version-creation-cid which initially populated the list (by request)
        do (versioned-object-map-changed-slot
            version 'cid-objects cid
            (lambda (change-type valid-p added-deleted-or-new-value maybe-deleted-value)
                ;; Values in question are CID-OBJECT entities.
                (flet ((doit (cid-object change-type)
                         ;; Packages up change information as a tuple in RESULT
                         (multiple-value-bind (cid-description cid-timestamp change-set)
                             (repository-get-cid-information *repository* cid-object)
                           (push (list (cid-object-distributed-identifier added-deleted-or-new-value)
                                       change-type
                                       (or (and change-set (described-object-text change-set))
                                           cid-description)
                                       cid-timestamp
                                       (and change-set (object-user-name change-set)))
                                 result))))
                  (ecase change-type
                    ((:add :delete)
                     (doit added-deleted-or-new-value change-type))
                    (:change
                     ;; Convert this to :add and :delete
                     (doit added-deleted-or-new-value :add)
                     (assert valid-p)
                     (doit maybe-deleted-value :delete)))))))
    ;; Result has the tuples, return all the necessary values
    (values (object-user-name version)
            (let ((previous-version (version-ancestor-version version)))
              (and previous-version (object-user-name previous-version)))
            result)))

(defun version-get-project (version)
  "Return the project which hosts this version, or NIL if there is no such project
   (typically NIL is returned only for a version which describes some user-defined or
   VIEW-based version. See TRUEframework architecture for more details."
  #+allegro (declare (:fbound branch-owning-project)) ;forward declaration
  (let ((branch (version-owning-branch version)))
    (and branch (branch-owning-project branch))))

(defvar *with-version-debug* nil
  "True if you want to see CID-SET information when binding new view using WITH-VERSION")

(defvar *with-version-context* nil
  "This variable is bound to the value of the :VERSION-CONTEXT in the WITH-VERSION macro.
   It is useful to assert that the context is as expected in defensive programming.
   I.e., a WITH-VERSION caller establishes a context, and a piece of code which expects a
   particular version context to exist can verify that it does in fact exist by calling
   the VERSION-CONTEXT-ASSERT function.")

(defvar *with-version-cid-set-cache* nil
  "A cache-mgr object to handle cached versioned cid-sets.
   The key is (list repository-db-name repository-persistent-data-identifier-key version-did).
   The value is (list metaversion-timestamp metaversion-max-cid cid-set metaversion-cid-set).")

(defparameter *with-version-cid-set-cache-size* 100
  "initial size of primary section of the *with-version-cid-set-cache*, NIL or non-positive
   means don't use the cache at all")

(defparameter *with-version-cid-set-cache-stickiness-reduction-factor* 10
  "multiplicative factor (times 1.5 times *with-version-cid-set-cache-size*) of how often we will
   reduce the cache stickiness factor")

(defun with-version-start-new-cid-set-cache (size stickiness)
  "shut down any existing cache.  Create a new cache of the size requested.
   NIL for either arg shuts it down but will not restart it."

  ;; shut down existing cache
  (setq *with-version-cid-set-cache* nil)

  ;; see if it is to be permanent
  (unless size
    (setq *with-version-cid-set-cache-size* nil))
  (assert *with-version-cid-set-cache-stickiness-reduction-factor*)

  ;; start new one if parameters are ok
  (when size
    (setq *with-version-cid-set-cache*
          (cache-mgr-initialize size (max 1 (floor size 2))
                                (1+ (* stickiness
                                       (+ size (max 1 (floor size 2)))))))
    (setq *with-version-cid-set-cache-size* size)
    (setq *with-version-cid-set-cache-stickiness-reduction-factor* stickiness)
    ;;(unless core::*debug-noise-level*
    ;;(setq core::*debug-noise-level* 0))
    (debug-message 2 "WVer Cache created, size: ~a" *with-version-cid-set-cache-size*))
  )

(defvar *with-version-performance-tests* nil
  "normally nil, T when we are doing performances tests, controls whether or not we do
   extra sanity tests on the cache")

(defparameter *version-cid-set-cache-sanity-check* nil
  "forces the with-version-cid-set-cache sanity checks when non-NIL")

(define-tenn-class wv-cache-value
    (:documentation
     "the value of a *with-version-cid-set-cache* key/value pair.
      We make copies of all of the lists that could be modified by outsiders to try
      and isolate ourselves from random perturbations"
     :astore-persistent nil)
  (result-cid-set nil) ;; the answer & why we are doing this
  ;; for 50k csets, this will take ~6.25kb to hold (it is a bitmap)

  (ion-to-cid-vector nil) ;; an optimization for unplaying cvi indices
  ;; it takes a metaversion-ion as an index and yields a result cid number which is used
  ;; when unplaying to turn on a 'deleted' cid
  ;; for 50k csets, this will take ~200kb to hold (it is an array of fixnums)

  ;; metaversion slots, the mv-cid-set is a bitmap which is the ultimate arbitor
  ;; but the others are used to avoid diddling the bitmap unless we have to.  All
  ;; values are as they were in the metaversion-cid-set at the time we cached the
  ;; data
  (mv-cid-set nil)     ;; my private copy of the metaversion-cid-set-cid-set
  ;; from the *txn-context* object
  )

(defmacro with-version ((version &key include-current
                                      cset-dids-to-add cset-dids-to-remove
                                      (version-context :unknown)) &body body)
  "Derive a CID-SET from VERSION, bind the global view space in *TXN-CONTEXT*, and
   execute BODY as an implicit progn in that view.  This macro assumes a transaction
   is in progress, and that *TXN-CONTEXT* is bound.
   If INCLUDE-CURRENT is true, we'll mix the current transaction CID into the version
   used (typically a LATEST version).

   CSET-DIDS-TO-{ADD,REMOVE} are (possibly empty) lists of change-set DIDs which are to be
   added to or removed from the resulting cid-set in order to view content.  These modifiers
   are typically derived from workspace virtual private branches.
   (repeat: CHANGE-SET DIDS, not cid-object dids, or cids ... sigh ...
   If we want to pass cids or cid-objects, we can do that too, though  they shouldn't be used
   outside the core or VM packages as a matter of principle.)

   VERSION-CONTEXT is used to facilitate defensive programming and ensure that
   the expected version context is active.  It is typically a keyword value which
   may be chacked in BODY via the VERSION-CONTEXT-ASSERT function."
  ;; The biggest trick here is converting the version CID representations to a CID-SET.
  ;; Especially for DEMO where the representation in VERSION objects is really stupid.
  (with-macro-variables (cid-set)
    `(let* ((,cid-set (with-version-cid-set-aux ,version ,include-current
                                                ,cset-dids-to-add ,cset-dids-to-remove))
            (*with-version-context* ,version-context))
       (when (or *with-version-debug* (debug-level-meets-or-exceeds? 3))
         (format *debug-io* "~%*WITH-VERSION-DEBUG*: cid-set BEFORE with-version:")
         (csf/core::dump (csf/core::txn-context-cid-set *txn-context*) *debug-io*))
       (with-txn-context-cid-set (*txn-context* ,cid-set)
         (when (or *with-version-debug* (debug-level-meets-or-exceeds? 3))
           (format *debug-io* "~%*WITH-VERSION-DEBUG*: cid-set AFTER with-version:")
           (csf/core::dump (csf/core::txn-context-cid-set *txn-context*) *debug-io*))
         (LOCALLY ,@body)))))

(defun with-version-cid-set-aux-adjust-cid-set (cache-val txn-context new-mv-cid-set version)
  "when we get here, we know the metaversion cid-sets don't match and we need to:
   1) determine which cids are different in the 2 metversion-cid-sets
   2) find the earliest cid in the difference list
   3) un-play the old metaversion-cid-set (against the answer cid-set) back to the
      earliest cid
   4) play the new metaversion-cid-set (against the answer cid-set) forward from
      the earliest cid
   5) we're done!

   CACHE-VAL is a wv-cache-value object from the *with-version-cid-set-cache*.
   TXN-CONTEXT is a normal txn-context object.
   NEW-MV-CID-SET is a normal cid-set (that we are aiming towards).
   VERSION is a version object.

   Returns T when the CACHE-VAL has been updated (and thus contains usable stuff).
   Returns NIL when you need to recalculate from scratch (and toss the cache-value).
   "
  (macrolet ((check-ts (from-where)
               `(let ((cmt-days (funcall days-get days cid)))
                 (when (or (not earliest-ts-days)
                        (> earliest-ts-days cmt-days)
                        (and (= earliest-ts-days cmt-days)
                         (> earliest-ts-seconds (funcall millis-get millis cid))))
                   (when (or *with-version-debug* (debug-level-meets-or-exceeds? 4))
                     (debug-message 4 "with-version-cid-set-aux-adjust-cid-set check-ts earlier time from ~a"
                      ,from-where)
                     (debug-message 4 "     previous cid    ~a new cid ~a" earliest-cid cid)
                     (debug-message 4 "     previous days   ~a new days ~a" earliest-ts-days cmt-days)
                     (debug-message 4 "     previous millis ~a new millis ~a" earliest-ts-seconds
                      (funcall millis-get millis cid)))
                   (setq earliest-ts-days cmt-days)
                   (setq earliest-ts-seconds (funcall millis-get millis cid))
                   (setq earliest-cid cid)))
               ))
    (let* (earliest-cid
           earliest-ts-days
           earliest-ts-seconds
           result-cid-set
           (cmt (csf/core::repository-persistent-information-cid-master-table
                 (csf/core::repository-persistent-data (txn-context-repository txn-context))))
           (days (csf/core::cid-master-table-timestamp-days cmt))
           (millis (csf/core::cid-master-table-timestamp-millis cmt))
           (days-get (astore-vector-get-aref-function days))
           (millis-get (astore-vector-get-aref-function millis))
           (old-mv-cs (wv-cache-value-mv-cid-set cache-val))
           (old-last-cid (csf/core::cid-set-highest-active-cid old-mv-cs))
           (new-last-cid (csf/core::cid-set-highest-active-cid new-mv-cid-set))
           (min-last-cid (if (< old-last-cid new-last-cid)
                             old-last-cid new-last-cid))
           (old-csac? (csf/core::cid-set-active-cid?-function old-mv-cs))
           (new-csac? (csf/core::cid-set-active-cid?-function new-mv-cid-set))
           )

      ;; 1) & 2) determine which cids are different and which is the earliest
      ;; no need to save all of the differences, just the oldest
      (loop for cid from 1 to min-last-cid
            do
            (unless (eq (funcall old-csac? cid) (funcall new-csac? cid))
              (check-ts "Old & New")
              ))

      (unless (= old-last-cid new-last-cid)
        (if (> old-last-cid new-last-cid)
            (loop for cid from (1+ new-last-cid) to old-last-cid ;; old is longer
                  do
                  (when (funcall old-csac? cid)
                    (check-ts "Old"))
                  )
          (loop for cid from (1+ old-last-cid) to new-last-cid ;; new is longer
                do
                (when (funcall new-csac? cid)
                  (check-ts "New"))
                )))

      ;; if we did not find an appropriate cid/ts, we should not have called this func
      (assert (and earliest-cid earliest-ts-days earliest-ts-seconds))
      (assert (and (< 0 earliest-cid) (< 0 earliest-ts-days) (<= 0 earliest-ts-seconds)))

      ;; someday we may want to decide whether or not to continue based on what the earliest
      ;; timestamp turns out to be.  If so, this is the place to add that code.  If we decide
      ;; not to continue (probably for cost reasons), the function needs to return a nil

      ;; step 3) & 4)
      (let ((new-ion-to-cid-vector
             (cid-set-unplay-replay-metaversions earliest-ts-days earliest-ts-seconds
                                                 old-mv-cs
                                                 new-mv-cid-set
                                                 (wv-cache-value-result-cid-set cache-val)
                                                 (wv-cache-value-ion-to-cid-vector cache-val)
                                                 version
                                                 'cid-objects
                                                 cmt)))
        ;; someday we may want to decide whether or not to continue based on what gets
        ;; returned from cid-set-unplay-replay-metaversions.  Our function must return
        ;; NIL if cid-set-unplay-replay-metaversions gives up.  cid-set-unplay-replay-metaversions
        ;; will signal that it has given up by returning a nil itself
        (when (or *with-version-debug* (debug-level-meets-or-exceeds? 4))
          (let ((*print-length* nil) ;; print it all
                (*print-level* nil)) ;; however deep
            (debug-message 4 "with-version-cid-set-aux-adjust-cid-set ion-to-cid-vector (updated)~%~a"
                           new-ion-to-cid-vector)))
        (assert new-ion-to-cid-vector)
        (setf (wv-cache-value-ion-to-cid-vector cache-val) new-ion-to-cid-vector)
        (setq result-cid-set (wv-cache-value-result-cid-set cache-val)))

      result-cid-set ;; returned value
      )))

(defun with-version-cid-set-aux (version include-current cset-dids-to-add cset-dids-to-remove)
  "Module-private helper routine for use in the WITH-VERSION macro.  Performs most of the side-effect
   checking, computes and returns the final cid-set which is bound to view content.

   All arguments are as documented for WITH-VERSION.

   Returns a result cid-set.

   May use a cached value for the result cid-set or it may recompute the result cid-set if it is
   determined that the cached value is not suitable.  It may also try to edit the cached value by
   unplaying the old metaversion-cid-set (back to the earliest difference with the new metaversion
   cid-set) and then replaying the new metaversion-cid-set from that point onward.  The cache key
   is based on the repository name & timestamp and the version-did.

   Local edits such as the INCLUDE-CURRENT cid or CSET-DIDS-TO-ADD or CSET-DIDS-TO-REMOVE are not
   cached.  Instead they are always applied to the result.

   Note that we always return a copy of the result cid-set so that we can diddle our cached copy
   in the future without worrying about who has retained references to it.
   "

  ;; see if it is in the cache
  ;; but first see if we have a cache size
  (unless *with-version-cid-set-cache*
    (with-version-start-new-cid-set-cache *with-version-cid-set-cache-size*
      *with-version-cid-set-cache-stickiness-reduction-factor*))

  ;; now see if it is in the cache
  (let* ((cache-key nil)
         (cache-val nil)
         (cache-update-needed nil)
         (cache-toss nil)
         (new-mv-cid-set-update-needed nil) ;; a subset of cache-update-needed
         (result-cid-set nil)
         (new-mv-cid-set (cid-set-copy (txn-context-cid-set *txn-context*))) ;; copy so
         ;; outside scribblers can't affect me for later comparisons from my cached version of this
         (rw-txn? (txn-for-update? *txn-context*))
         ion-to-cid-vector
         (repository (txn-context-repository *txn-context*))
         (current-cid (txn-context-cid *txn-context*))
         ;; we try to cache as much cid-set info as we can BUT we explicitly do not cache anything
         ;; that is mutable in the current transaction.  This means we can freely cache data about
         ;; r/o transactions and we can freely cache data about r/w transaction that have no current
         ;; cid (regardless of the INCLUDE-CURRENT value).  However, when we are dealing with a R/W
         ;; transaction with a current cid, we must be careful.  If we already have a cached value
         ;; for the key, we simply do not update it.  If we don't have a cached value for the key,
         ;; we create a value for caching by taking the mutable parts out of the new metaversion
         ;; cid set and playing the pieces forward.
         ;; Of course for all of these, the result-cid-set always includes the mutable pieces.
         (cache-update-allowed (and *with-version-cid-set-cache*
                                    (or (not rw-txn?)
                                        (not current-cid)
                                        (= 0 current-cid))))
         ;; Note that cache-update-allowed will generally (always?) be true when
         ;; *with-version-cid-set-cache* because the conditions above mesh so nicely
         ;; with the diddling conditions below
         (cache-update-with-diddled-metaversion nil)
         )

    ;; general debug info
    (when (or *with-version-debug* (debug-level-meets-or-exceeds? 3))
      (debug-message 3 "with-version-cid-set-cache new metaver-cid-set:")
      (csf/core::dump new-mv-cid-set *debug-io*)
      (debug-message 3 "    cs-current-cid ~a (include ~a R/W? ~a)"
                     current-cid include-current rw-txn?)
      (debug-message 3 "    *txn-context* ~a" *txn-context*))

    ;; try the cache first
    (when *with-version-cid-set-cache*
      (setq cache-key (list (db-name-namestring (csf/core::repository-db-name repository))
                            (csf/core::repository-persistent-information-identifier-key
                             (csf/core::repository-persistent-data repository))
                            (distributed-object-identifier version)))
      (setq cache-val (cache-mgr-lookup *with-version-cid-set-cache* cache-key))
      (setq cache-update-with-diddled-metaversion (and rw-txn? current-cid (< 0 current-cid)))
      (unless cache-update-allowed
        (setq cache-update-allowed cache-update-with-diddled-metaversion))
      (debug-message 4 "with-version-cid-set-cache cache-update-allowed ~a" cache-update-allowed))

    ;; diddled the new metaversion-cid-set if needed
    (when cache-update-with-diddled-metaversion
      (cid-set-deactivate-cid new-mv-cid-set current-cid)
      (when (or *with-version-debug* (debug-level-meets-or-exceeds? 3))
        (debug-message 3 "with-version-cid-set-cache diddled new-mv-cid-set @ ~a" current-cid)
        (debug-message 3 "with-version-cid-set-cache new metaver-cid-set:")
        (csf/core::dump new-mv-cid-set *debug-io*)))

    ;; make sure that cache value is usable (has the metaversion context changed
    ;; since we cached the cid-set?)
    (when cache-val
      (when (or *with-version-debug* (debug-level-meets-or-exceeds? 3))
        (debug-message 3 "with-version-cid-set-cache cached (old) mv-cid-set:")
        (csf/core::dump (wv-cache-value-mv-cid-set cache-val) *debug-io*)
        (debug-message 3 "with-version-cid-set-cache cached (result) cid-set:")
        (csf/core::dump (wv-cache-value-result-cid-set cache-val) *debug-io*))

      (let ((old-mv-cid-set (wv-cache-value-mv-cid-set cache-val))  ;; previously cached metaversion
            mv-cs-eq
            mv-cs-eq-expensive
            )

        (cid-set-as-bitmap new-mv-cid-set :scribbled nil) ;; force a local bitmap

        (multiple-value-setq (mv-cs-eq mv-cs-eq-expensive)
          (cid-set-equal? new-mv-cid-set old-mv-cid-set))
        (when (or *with-version-debug* (debug-level-meets-or-exceeds? 4))
          (let ((*print-length* nil) ;; however long
                (*print-level* nil)) ;; however deep
            (debug-message 4 "with-version-cid-set-cache cid-set-eq ~a" mv-cs-eq)
            (debug-message 4 "with-version-cid-set-cache cid-set-last-cid ~a"
                           (cid-set-last-cid old-mv-cid-set))
            (debug-message 4 "with-version-cid-set-cache old ion-to-result-cid-vector ~a"
                           (wv-cache-value-ion-to-cid-vector cache-val))))

        (setq result-cid-set (wv-cache-value-result-cid-set cache-val))
        ;; see if we need to extend the cached cid-set to avoid later bitmap indexing problems
        (when (and cache-update-allowed
                   (< (cid-set-last-cid result-cid-set)
                      (max current-cid (cid-set-last-cid new-mv-cid-set))))
          (cid-set-extend result-cid-set (+ 1 (- (max current-cid (cid-set-last-cid new-mv-cid-set))
                                                 (cid-set-last-cid result-cid-set))
                                            csf/core::*metaversion-cid-set-bitmap-extra-space*))
          (setq cache-update-needed t))

        (if mv-cs-eq ;; cid-sets are equal
            (progn
              ;; easy way, nothing has changed since caching
              (debug-message 3 "with-version-cid-set-cache had hit & reused cid-set for ~a" cache-key)

              ;; assume that if it was expensive to determine equality that we should
              ;; update the meteaversion portion of the cache so it will be (?) cheaper
              ;; next time
              (setq new-mv-cid-set-update-needed (and cache-update-allowed
                                                      mv-cs-eq-expensive))
              )
          (progn
            ;; metaversion cid-sets are different,  this is probably the most common case
            ;; must determine what has changed and handle it
            ;; for now, fall thru into the build-it-from-scratch case]
            (debug-message 2 "with-version-cid-set-cache had hit but can't reuse cid-set (MV diffs) for ~a" cache-key)

            ;; Now adjust the cached result-cid-set by unplaying the old metaversion-cid-set
            ;; and replaying the new-metaversion-cid-set against the cached result-cid-set
            (if (with-version-cid-set-aux-adjust-cid-set cache-val *txn-context*
                                                         new-mv-cid-set version)
                (progn
                  (incf (utility::cache-mgr-object-num-hits-edits *with-version-cid-set-cache*))
                  ;; we decided we could unplay/replay the cached value so refetch a
                  ;; potentially changed result-cid-set
                  (setq result-cid-set (wv-cache-value-result-cid-set cache-val)))

              (progn
                ;; otherwise we make cid-set nil and fall thru into doing a newly created cid-set
                (setq cache-toss t
                      result-cid-set nil)))

            (setq cache-update-needed cache-update-allowed)
            (setq new-mv-cid-set-update-needed cache-update-allowed)
            ))))

    (when (or (not result-cid-set) ;; nothing useful cached, must build from scratch
              ;; the following tests only force a sanity check that the cached cid-set and
              ;; a newly created cid-set match
              *version-cid-set-cache-sanity-check*
              (debug-level-meets-or-exceeds? 4) ;; verify that cid-sets are the same
              (and csf/core::*within-regression-tests*     ;; verify that cid-sets are the same
                   (not *with-version-performance-tests*)))  ;; but not if doing performance tests
      (if result-cid-set
          (debug-message 2 "with-version-cid-set-cache ok, doing sanity test")
        (progn
          (setq new-mv-cid-set-update-needed cache-update-allowed)
          (setq cache-update-needed cache-update-allowed)
          (when cache-toss
            (incf (utility::cache-mgr-object-num-hits-tosses *with-version-cid-set-cache*)))
          (debug-message 2 "with-version-cid-set-cache miss, building cid-set from scratch for ~a"
                         cache-key)))

      ;; find it the old way (and then cache it?)
      (let (cid-list
            cid-object-list
            (found-cid-set result-cid-set)
            new-result-cid-set
            new-ion-to-cid-vector)

        ;; Note that we may have to use a diddled new-mv-cid-set to create the cid-set from scratch
        ;; if so, we have already fixed it above by cache-update-with-diddled-metaversion

        (progn
          ;; NOTE: this section may be well be redundant (given the next section where
          ;;       we calculate the ion-to-result-cid-vector).  Once we have determined
          ;;       that that section can calculate the result-cid-set properly, this
          ;;       section can be dumped
          (with-txn-context-cid-set (*txn-context* new-mv-cid-set) ;; cache-update-with-diddled-metaversion
            (setq cid-object-list (version-get-cid-object-list version))
            (setq cid-list (mapcar #'cid-object-as-number cid-object-list)))

          (setq result-cid-set (repository-create-cid-set-from-cid-list
                                repository cid-list)))

        (unless found-cid-set ;; only if we are building from scratch
          (setq new-ion-to-cid-vector
                (csf/core::cvi-ion-to-cid-array-create
                 ;; guess at the initial number of ions
                 (+ 1 (csf/core::cvi-max-ion-of-versioned-slot version 'cid-objects)
                    csf/core::*metaversion-cid-set-bitmap-extra-space*)))
          (setq new-result-cid-set (csf/core::fixed-size-cid-set-create
                                    :dimension (cid-set-last-cid new-mv-cid-set)))

          (setq new-ion-to-cid-vector
                (cid-set-unplay-replay-metaversions nil nil ;; earliest-ts-days earliest-ts-seconds
                                                    nil ;; old-mv-cs
                                                    new-mv-cid-set
                                                    new-result-cid-set
                                                    new-ion-to-cid-vector
                                                    version
                                                    'cid-objects
                                                    (csf/core::repository-persistent-information-cid-master-table
                                                     (csf/core::repository-persistent-data
                                                      (txn-context-repository *txn-context*)))))

          ;; do another sanity test - the two result cid-sets should be the same
          (unless (cid-set-equal? new-result-cid-set result-cid-set)
            (when (or *with-version-debug* (debug-level-meets-or-exceeds? 3))
              (format *debug-io* "with-version-cid-set-aux result-cid-set mismatch:~%cid-set created the 1st way:")
              (csf/core::dump result-cid-set *debug-io*)
              (format *debug-io* "cid-set created the 2nd way:")
              (csf/core::dump new-result-cid-set *debug-io*))
            (error "with-version-cid-set-aux result-cid-set mismatch (1st: ~a), (2nd: ~a)"
                   result-cid-set new-result-cid-set))

          (setq ion-to-cid-vector new-ion-to-cid-vector))

        ;; A little debugging info
        (when (or *with-version-debug* (debug-level-meets-or-exceeds? 3))
          (format *debug-io* "~%*WITH-VERSION-DEBUG*: mv-cid-set BEFORE with-version-aux:")
          (csf/core::dump (csf/core::txn-context-cid-set *txn-context*) *debug-io*)
          (format *debug-io* "~%*WITH-VERSION-DEBUG*: uneditted result cid-set AFTER  with-version-aux newly created:")
          (csf/core::dump result-cid-set *debug-io*)
          (when found-cid-set
            (format *debug-io* "~%*WITH-VERSION-DEBUG*: uneditted result cid-set AFTER  with-version-aux cache:")
            (csf/core::dump found-cid-set *debug-io*)))

        ;; Do some major sanity tests?
        (when found-cid-set ;; we found one so we need to check against the created one
          (let* ((old-vec (wv-cache-value-ion-to-cid-vector cache-val))
                 (old-len (length old-vec))
                 (new-len (length new-ion-to-cid-vector))
                 (min-len (min old-len new-len)))
            (loop for idx from 0 below min-len
                  do
                  (when (/= (aref old-vec idx) (aref new-ion-to-cid-vector idx))
                    (when (and (/= (aref old-vec idx) csf/core::*core-cvi-ion-to-cid-array-uninitialized-value*)
                               (/= (aref new-ion-to-cid-vector idx) csf/core::*core-cvi-ion-to-cid-array-uninitialized-value*))
                      (let ((*print-length* nil) ;; however long
                            (*print-level* nil)) ;; however deep
                        (debug-message 4 "with-version-cid-set-aux ion->cid vectors differ at ~a (~a vs. ~a)"
                                       idx (aref old-vec idx) (aref new-ion-to-cid-vector idx))
                        (debug-message 4 "with-version-cid-set-aux old ion->cid vector~%~a" old-vec)
                        (debug-message 4 "with-version-cid-set-aux new ion->cid vector~%~a" new-ion-to-cid-vector))
                      (error "with-version-cid-set-aux sanity check failed, ion->cid mismatch ~a/~a at ion ~a"
                             (aref old-vec idx) (aref new-ion-to-cid-vector idx) idx))
                    (when (and (= (aref old-vec idx) csf/core::*core-cvi-ion-to-cid-array-uninitialized-value*)
                               (/= (aref new-ion-to-cid-vector idx) csf/core::*core-cvi-ion-to-cid-array-uninitialized-value*))
                      ;; save the new info
                      (setf (aref old-vec idx) (aref new-ion-to-cid-vector idx)
                            cache-update-needed cache-update-allowed
                            ion-to-cid-vector old-vec))))
            ;; note that we do not care if either vector is longer nor do we care if
            ;; the old vector has a value and the new vector does not.  This is because
            ;; we are updating the old vector with every ion we see and the new vector
            ;; is only updated with the currently active ions.  This means that over time
            ;; the old vector is likely to be (much?) bigger or have a lot more values
            ;; filled in (not -1).  On the other hand, it is also possible that the
            ;; currently active ions will be larger than what we had before so that the
            ;; new vector will have extra info the old does not have (and that's ok too)
            )

          (when  (not (cid-set-equal? found-cid-set result-cid-set))
            (error "with-version cached cid-set (~a) does not match computed cid-set (~a)"
                   found-cid-set result-cid-set))

          (when (and (or cache-update-needed
                         new-mv-cid-set-update-needed)
                     (< (cid-set-last-cid found-cid-set)  ;; will get array-index errors later if not
                        (cid-set-last-cid result-cid-set)))
            (debug-message 4 "with-version cache cid-set size (~a) does not match computed cid-set size (~a)"
                           (cid-set-last-cid found-cid-set) (cid-set-last-cid result-cid-set))
            (error "with-version cache cid-set size (~a) does not match computed cid-set size (~a)"
                   (cid-set-last-cid found-cid-set) (cid-set-last-cid result-cid-set)))

          (setq result-cid-set found-cid-set)) ;; retstore the cached version in case we extended it earlier
        ))

    ;; add the cid-set (& other info) to the cache
    (when (or cache-update-needed
              new-mv-cid-set-update-needed)
      (let ((cv (or cache-val
                    (make-instance 'wv-cache-value))))
        (when new-mv-cid-set-update-needed
          ;; make the mv-cid-set ok across transactions
          ;; we save the new metaversion-cid-set to avoid marking the old one as scribbled
          ;; so we can still do a timestamp comparison
          (cid-set-as-bitmap new-mv-cid-set :scribbled nil)
          (setf (wv-cache-value-mv-cid-set cv) new-mv-cid-set))

        (when ion-to-cid-vector
          (setf (wv-cache-value-ion-to-cid-vector cv) ion-to-cid-vector))

        (setf (wv-cache-value-result-cid-set cv) result-cid-set)

        (cache-mgr-lookup *with-version-cid-set-cache* cache-key
                          :update t :value cv)
        (debug-message 3 "with-version-cid-set-cache UPDATED!~%")))

    ;; return a COPY of the cid-set because I need to scribble on the cached copy
    ;; later and the callers might have retained a reference to what I have cached
    ;; and this would (incorrectly) cause their copy to change.
    (setq result-cid-set (cid-set-copy result-cid-set))

    ;; sanity check on what we are about to return
    ;; must have enough room to hold the current cid
    (assert (>= (cid-set-last-cid result-cid-set) current-cid))

    ;; handle local edits for VPB and current cid

    ;; deal possible current-cid
    (when (and include-current
               (< 0 current-cid))
      (cid-set-activate-cid result-cid-set current-cid)
      (debug-message 4 "with-version-cid-set-cache added current cid ~a" current-cid))
    (when include-current  ;; remember that it is possible for the current-cid to be 0
      (assert (cid-set-active-cid? result-cid-set current-cid)))

    ;; Add/remove VPB cid indications
    (dolist (did cset-dids-to-add)
      (cid-set-activate-cid result-cid-set (change-set-cid-number
                                            (repository-resolve-distributed-identifier repository did))))

    (dolist (did cset-dids-to-remove)
      (cid-set-deactivate-cid result-cid-set (change-set-cid-number
                                              (repository-resolve-distributed-identifier repository did))))

    (when (or *with-version-debug* (debug-level-meets-or-exceeds? 4))
      (debug-message 4 "with-version-cid-set-cache editted &final result cid-set: (last-cid ~a)"
                     (cid-set-last-cid result-cid-set))
      (csf/core::dump result-cid-set *debug-io*))
    result-cid-set
    ))

(defun version-context-assert (version-context)
  "Assert that the version context currently in scope matches that of VERSION-CONTEXT,
   which should be the same value passed to WITH-VERSION in some stack-upstream context.
   VERSION-CONTEXT is typically a keyword.

   Returns nil or signals an error."
  (unless (eq version-context *with-version-context*)
    (error "A ~s version context was expected where a ~s version context was active."
           version-context *with-version-context*))
  nil)
||#

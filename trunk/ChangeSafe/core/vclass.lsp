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
  (export '(call-with-cid-set-view
            call-comparing-views
            versioned-standard-class
            versioned-standard-object
            versioned-value/view
            versioned-value/update
            versioned-object/cid-set
            versioned-object/most-recent-slot-cid
            versioned-object/scan-composite-versioned-slot
            versioned-object-slot/cid-set)))

(defconstant *version-techniques*
  '(:nonversioned
    :scalar :logged :nonlogged
    :composite-set :composite-sequence :composite-file))

(deftype version-technique () `(member ,@*version-techniques*))

(defun version-technique->versioned-value-class (version-technique)
  "Given a version-technique, return a symbol naming the CLOS class
   that will implement it."
  (ecase version-technique
    ((nil :nonversioned) nil)
    (:scalar             'scalar-versioned-value)
    (:logged             'logged-versioned-value)
    (:nonlogged          'nonlogged-versioned-value)
    (:composite-set      'cvi)
    (:composite-sequence 'cvi)
    (:composite-file     'cvi)))

(defclass versioned-standard-class (persistent-standard-class)
  ()
  (:documentation "The prototype class for versioned objects."))

;; Oh well.
;; We'll be overriding the accessors.
(defmethod clos::can-optimize-access-using-class ((class versioned-standard-class))
  nil)

(defun slot-name->versioned-initarg (slot-name)
  (intern (concatenate 'string
                       "VERSIONED-INITARG-FOR-"
                       (symbol-name slot-name))
          (find-package "CSF/CORE")))

(defclass versioned-slot-definition (persistent-slot-definition)
  ((versioned-initarg :initarg :versioned-initarg
                      :reader versioned-slot-definition/versioned-initarg)
   (version-technique :initarg :version-technique
                      :initform :nonversioned
                      :accessor slot-definition-version-technique
                      :type version-technique)))

(defmethod clos:shared-initialize :around ((instance versioned-slot-definition) slot-names
                                           &rest initargs &key name &allow-other-keys)
  (apply #'call-next-method instance slot-names
         :versioned-initarg (slot-name->versioned-initarg name)
         initargs))

(defmethod clos:slot-definition-initargs ((slot-definition versioned-slot-definition))
  (cons (versioned-slot-definition/versioned-initarg slot-definition)
        (call-next-method)))

(defmethod slot-definition-standard-initargs ((slot-definition versioned-slot-definition))
  (cdr (clos:slot-definition-initargs slot-definition)))

;;; In theory, the inheritance list ought to be
;;; (versioned-slot-definition clos:direct-slot-definition) but there
;;; is apparently an initialization on
;;; clos:standard-direct-slot-definition that we need.
(defclass versioned-direct-slot-definition (versioned-slot-definition clos:standard-direct-slot-definition)
  ())

(defmethod clos:direct-slot-definition-class
  ;; lispworks bug: arglist is nonstandard
  #+lispworks ((class versioned-standard-class) slot-specification)
  (let ((technique (getf slot-specification :version-technique)))
    (if (and technique (not (eq technique :nonversioned)))
        (find-class 'versioned-direct-slot-definition)
        (call-next-method))))

(defclass versioned-effective-slot-definition (versioned-slot-definition clos:standard-effective-slot-definition)
  ())

(defmethod clos:effective-slot-definition-class
    ;; lispworks bug: arglist is nonstandard
    #+lispworks ((class versioned-standard-class) slot-specifications)
    (if (typep (car slot-specifications) 'versioned-slot-definition)
        (find-class 'versioned-effective-slot-definition)
        (call-next-method)))

(defmethod clos:compute-effective-slot-definition :around ((class versioned-standard-class) name direct-slot-definitions)
  (let ((result (call-next-method)))
    (when (typep result 'versioned-slot-definition)
      (setf (slot-definition-version-technique result)
            (slot-definition-version-technique (car direct-slot-definitions))))
    result))

(defun scan-class-versioned-effective-slots (class)
  (declare (optimizable-series-function))
  (choose-if (lambda (slot)
               (typep slot 'versioned-slot-definition))
             (scan 'list (clos:class-effective-slots class))))

;;; For lispworks, we need to wrap this method.
#+lispworks
(defmethod clos::canonicalize-defclass-slot :around ((prototype versioned-standard-class) slot)
  ;; make sure the slot is a list.
  (unless (consp slot)
    (setq slot (list slot)))
  (let ((version-technique (or (getf (cdr slot) :version-technique)
                               :nonversioned)))
    (check-type version-technique version-technique)
    (remf (cdr slot) :version-technique)
    `(,@(call-next-method prototype slot)
        :version-technique ,version-technique)))

;;; All versioned objects inherit from persistent-object
(defclass versioned-standard-object (persistent-standard-object)
  (

  ;; The following slot isn't necessary for any core operations, but is very useful for reporting
  ;; changes when you want to exclude changes related to the birth of an object.
  ;; we could maintain this information in other bindings, either a central dictionary or
  ;; a search on the cid-master-table.  However finding the cid by searching the CMT is a linear search
  ;; and would be prohibitively expensive.

  ;; Finally, in order to preserve the extreme flexibility of the CORE import/export capabilities,
  ;; we use a CID-OBJECT and not a CID to represent the birth cid.  The reason is that
  ;; at the time of this writing, the birth cid may not exist in the importing repository
  ;; when the distributed entity is created in response to importing some cid other than the birth
  ;; cid on the versioned object in question.  While we may pragmatically reject any attempt
  ;; to import change-sets in absense of the birth cid's residency at some higher level or at a later
  ;; date, right now we allow it.  Thus the birth-cid-object may reflect a cid which is not
  ;; necessarily present in the repository!

  ;; But as it turns out, this is a major bottleneck when restoring (opening)
  ;; the repository in the first place.  So we flush it!

  ;; Repository local cid which gave birth to this versioned object
   (birth-cid-object
    :initarg :birth-cid-object
    :reader versioned-object/birth-cid-object)
  ))

;;; Only versioned-standard-class and its subclasses are valid
;;; superclasses for versioned-objects
(defmethod clos:validate-superclass ((class versioned-standard-class) superclass)
  (subtypep superclass 'versioned-standard-object))

(defmethod clos:shared-initialize :around ((class versioned-standard-class) slot-names &rest initargs)
  "When initializing a versioned-standard-class, ensure that
   versioned-standard-object appears in the direct-superclasses."
  (let ((direct-superclasses (getf initargs :direct-superclasses)))
    (when (or (null direct-superclasses)
              (and (null (cdr direct-superclasses))
                   (eq (car direct-superclasses) (find-class 'standard-object)))
              (and (null (cdr direct-superclasses))
                   (eq (car direct-superclasses) (find-class 'persistent-standard-object))))
      (setf (getf initargs :direct-superclasses) (list (find-class 'versioned-standard-object))))
    (call-next-method)))

(defvar *disable-versioning* nil
  "When T, slot-value and (setf slot-value) work on the actual slot
   contents (i.e., the versioned-value structure) rather than the
   current view.")

(defun call-with-cid-set-view (cid-set receiver)
  "Call receiver in a context where the current cid set has been
   overridden.  You should avoid using this in read-write transactions."
  (let ((*versioned-value-cid-set-override* cid-set))
    (debug-message 4 "Temporarily viewing under CID-SET ~s" cid-set)
    (multiple-value-prog1 (funcall receiver cid-set)
      (debug-message 4 "Back to normal view."))))

(defun call-comparing-views (left-cid-set right-cid-set thunk)
  "Call thunk twice, once with left-cid-set, once with right-cid-set,
   and return both values."
  (values (call-with-cid-set-view left-cid-set thunk)
          (call-with-cid-set-view right-cid-set thunk)))

(defmethod slot-value-using-class-unversioned ((class versioned-standard-class)
                                               (object versioned-standard-object)
                                               slot)
  (let ((*disable-versioning* t))
    (clos:slot-value-using-class class object slot)))

(defmethod (setf slot-value-using-class-unversioned) (new-value (class versioned-standard-class)
                                                                (object versioned-standard-object)
                                                                slot)
  (let ((*disable-versioning* t))
    (setf (clos:slot-value-using-class class object slot) new-value)))

(defmethod slot-value-unversioned ((instance versioned-standard-object) slot)
  (slot-value-using-class-unversioned (class-of instance) instance slot))

(defmethod (setf slot-value-unversioned) (new-value (instance versioned-standard-object) slot)
  (setf (slot-value-using-class-unversioned (class-of instance) instance slot) new-value))

;;; The versioned slots in a versioned object actually contain a
;;; versioned value.
(defmethod clos:slot-value-using-class ((class versioned-standard-class)
                                        (instance versioned-standard-object)
                                        slot)
  (declare #.(performance-optimizations))
  (cond ((or *disable-versioning*
             (let ((slot-definition (find slot (clos:class-effective-slots class) :key #'clos:slot-definition-name)))
               (not (typep slot-definition 'versioned-slot-definition))))
         (call-next-method))
        (*versioned-value-cid-set-override*
         (debug-message 4 "Using override cid-set to view slot ~s." slot)
         (versioned-value/view (call-next-method)
                               *versioned-value-cid-set-override*
                               instance slot))
        (t
         (versioned-value/view (call-next-method)
                               (transaction/cid-set *transaction*)
                               instance slot))))

(defmethod (setf clos:slot-value-using-class) (new-value (class versioned-standard-class)
                                                         (instance versioned-standard-object)
                                                         slot)
  (declare #.(performance-optimizations))
  (cond (*versioned-value-cid-set-override*
         (error "Attempt to modify a slot in an overridden view."))
        ((or *disable-versioning*
             (let ((slot-definition (find slot (clos:class-effective-slots class) :key #'clos:slot-definition-name)))
               (not (typep slot-definition 'versioned-slot-definition))))
         (call-next-method))
        (t
         (versioned-value/update new-value
                                 (slot-value-using-class-unversioned class instance slot)
                                 *transaction* instance slot))))

(defun versioned-object/scan-composite-versioned-slot (instance slot)
  (declare (optimizable-series-function))
  (composite-versioned-value/scan
   (slot-value-unversioned instance slot)
   (if *versioned-value-cid-set-override*
       (progn
         (debug-message 4 "Using override cid-set to scan slot ~s" slot)
         *versioned-value-cid-set-override*)
       (transaction/cid-set *transaction*))
   instance slot))

(declaim (ftype (function (versioned-slot-definition)
                          (values list t symbol)) effective-slot-versioning-info)
         (inline effective-slot-versioning-info))

(defun effective-slot-versioning-info (slot)
  "Return 4 values of interest in initializing this slot:

   First, the user-supplied initargs for this slot.  These will
   be matched against the initargs supplied to shared-initialize.

   Second, the initfunction, which will be called if none of the
   initargs match.

   Third, the versioned-initarg which will be passed to the next
   layer down to stuff in a value."
  (declare #.(performance-optimizations))
  (let ((initargs (clos:slot-definition-initargs slot)))
    (values (cdr initargs)
            (clos:slot-definition-initfunction slot)
            (slot-definition-version-technique slot)
            (car initargs))))

(defun compute-versioned-slot-initargs (class initargs)
  (declare #.(performance-optimizations))
  (multiple-value-bind (slot-initargs slot-initfunctions slot-version-techniques slot-versioned-initargs)
      (map-fn '(values t t symbol symbol)
              #'effective-slot-versioning-info
              (scan-class-versioned-effective-slots class))
    (collect-plist
     slot-versioned-initargs
     (map-fn 'versioned-value
             (lambda (slot-initargs slot-initfunction slot-version-technique)
               (let* ((no-init-value (cons nil nil))
                      (initial-value (slot-initial-value initargs slot-initargs slot-initfunction no-init-value))
                      (wrapper-class (version-technique->versioned-value-class slot-version-technique)))
                 (if wrapper-class
                     (if (eq initial-value no-init-value)
                         (make-instance (find-class wrapper-class))
                         (make-instance (find-class wrapper-class) :initial-value initial-value))
                     initial-value)))
             slot-initargs slot-initfunctions slot-version-techniques))))

;; Override the primary initialize-instance method in order to
;; deal with versioned slots.
(defmethod clos:initialize-instance ((instance versioned-standard-object) &rest initargs)
  (let* ((class (class-of instance))
         (init-plist (compute-versioned-slot-initargs class initargs)))
    (apply #'call-next-method instance
           :birth-cid-object (cid-object/find-or-create (repository-transaction/cid *transaction*))
           (nconc init-plist initargs))))

(defun scan-versioned-object-slots (versioned-object)
  (declare (optimizable-series-function 2))
  (map-fn '(values t versioned-value)
          (lambda (slot)
            (values slot (slot-value-unversioned versioned-object (clos:slot-definition-name slot))))
          (scan-class-versioned-effective-slots (class-of versioned-object))))

(defun versioned-object/most-recent-slot-cid (versioned-object slot-name cid-set)
  "Return the cid which performed the most recent update to the indicated slot of the versioned-object,
   or NIL if there is no CID in CID-SET which touched the specified slot of versioned-object.

  SLOT-NAME must be a symbol naming a slot as specified to DEFINE-VERSIONED-CLASS.

  CID-SET restricts the set of cids considered. "
  (versioned-value/most-recent-cid
   (slot-value-unversioned versioned-object slot-name)
   cid-set))

(defun versioned-object-slot/cid-set (repository versioned-object slot-name)
  "Return the set of cids that affect the value in this slot."
  (versioned-value/cid-set
   repository
   (slot-value-unversioned versioned-object slot-name)))

(defun versioned-object/cid-set (repository versioned-object)
  "Return the set of cids that affect the values of *all* slots."
  (multiple-value-bind (slot unversioned-slot-value)
      (scan-versioned-object-slots versioned-object)
    (collect-cid-set-union
     repository
     (map-fn 't (lambda (slot usv)
                  (debug-message 5 "Unversioned-slot-value for ~s is ~s" slot usv)
                  (versioned-value/cid-set repository usv))
             slot
             unversioned-slot-value))))

;; Forward reference
(declaim (ftype (function (t integer integer) boolean) repository/cid-more-recent?))

(defun versioned-object/most-recent-cid (versioned-object cid-set &key (repository *repository*))
  "Return the cid present in cid-set which last modified a versioned object.
   This really means the cid which last modified any of the versioned index slots of the object.
   Arguments must be of the indicated type.

   This function is purely for reporting use, and isn't intrinsic to VO or VI operation.

   WARNING: REPOSITORY MUST CONTAIN CIDS IN CID-SET AND VERSIONED-OBJECT.

   Return NIL if no cids in CID-SET have modified object."
  ;; PERFORMANCE: *FINISH*: make a cid-indexed timestamp vector approach so cid timestamp lookup is
  ;; efficient.
  (assert (eq (cid-set/repository cid-set) repository))
  (let ((candidate-cids
         (multiple-value-bind (slots values)
             (scan-versioned-object-slots versioned-object)
           (collect 'list
                    (choose-if #'identity
                               (map-fn 't (lambda (slot versioned-value)
                                            (declare (ignore slot))
                                            (versioned-value/most-recent-cid versioned-value cid-set))
                                       slots
                                       values))))))
    ;; Ok, we have candidate cids from all indexes, see which is most recent.
    (first (sort candidate-cids
                 (lambda (cid-1 cid-2)
                     (repository/cid-more-recent? repository cid-1 cid-2))))))

;; Forward reference
(declaim (ftype (function (t integer) t) repository/cid-comparison-timestamp))

(defun versioned-object/last-update-timestamp (versioned-object
                                               &optional (cid-set (repository-transaction/cid-set *transaction*)))
  "Return the time-stamp of the most recent updating CID in CID-SET
  to any attribute of VERSIONED-OBJECT."
  (let ((most-recent-cid (versioned-object/most-recent-cid versioned-object cid-set)))
    (and most-recent-cid
         (repository/cid-comparison-timestamp *repository* most-recent-cid))))

#||
(defun cvi-active-ion-vector (cvi cidset)
  "Return the sequence of IONS (a vector) which are active, in their proper
   order, such that ordered traversal of the returned sequence generates the
   ION of the next record to return in synthesizing a view.

   This 'active ion sequence' is slightly anachronistic, because it is a simple vector,
   instead of being a proper CVI-ION-SEQUENCE object.
   The CVI-ION-SEQUENCE object is persistent, but anywhere we have an 'active ion sequence'
   we will expect a simple-vector.  We allow 1-based indexing into this vector, just as we do
   for a real CVI-ION-SEQUENCE.

   Return two values:
   (1) NIL if there were no applicable change-records matching active CIDs in the txn CID-SET.
       In this case, chances are a SLOT-UNBOUND error should be thrown.  Return a simple-vector
       of ions otherwise, possibly empty.
   (2) NIL if there were no applicable change-records for the cid-set, or TRUE if there
       were applicable change-records, indicating the view is bound even though it has no
       active ions.
   REPEAT: the return value isn't just a list of IONS which are active, but a list of IONS
   in the order in which they're visible, which are active."
  (declare #.(performance-optimizations))
  (let* ((ion-list (cons 0 nil))
         (applicable-change-records nil)
         (maxion (cvi/max-ion cvi)))


         (dummy (cons nil nil))
         (ion->cns (make-array (1+ maxion) :initial-element dummy))
         (cid 0)
         (last-ion 0)
         (last-cns ion-list)
         (active-cid-func (cid-set-active-cid?-function cidset))
         )
    (declare (type cons ion-list))
    (declare (type (simple-array t (*)) ion->cns)
             (type array-index maxion)
             (type cons dummy))
    (declare (type array-index last-ion cid)
             (type cons last-cns))
    (setf (aref ion->cns last-ion) last-cns)
    (macrolet ((ins-rec-loop (what)
                             `(loop for ir being the cvi-insertion-records of cr
                                do
                                (setq last-ion
                                      (cvi-insertion-record-insertion-point ir))
                                        ;(assert (<= 0 last-ion maxion))
                                (setq last-cns (aref ion->cns last-ion))
                                (assert (not (eq last-cns dummy)))
                                        ;(assert (<= 1 (cvi-insertion-record-start-ion ir) maxion))
                                        ;(assert (<= 1 (cvi-insertion-record-last-ion ir) maxion))
                                (loop for ion of-type array-index
                                  from (cvi-insertion-record-start-ion ir)
                                  to   (cvi-insertion-record-last-ion ir)
                                  do
                                  (assert (eq (aref ion->cns ion) dummy))
                                  (setq ; last-ion ion
                                   last-cns
                                   (setf (cdr last-cns)
                                         (setf (aref ion->cns ion)
                                               (cons ,what (cdr last-cns)))))
                                  )
                                )
                             ))

      (cvi-change-record-map-macro
       (cvi cr)
       (setq cid (cvi-change-record-cid cr))
       (assert (> cid 0))
       (if (funcall active-cid-func cid)
           (progn
             (setq applicable-change-records t)
             ;; Insert ions into ion-list as active.
             ;; Note we must process ALL insertion records, not just
             ;; the active ones, because a later active insertion
             ;; record could use an inactive ion as an insertion
             ;; starting point.
             ;; Note that every ion should be NEW, never before seen.
             ;; There cannot legitimately be two different insertion
             ;; records for the same ion, because it would have two
             ;; distinct insertion points (even if the insertion points
             ;; were supposedly the same!) So the inner loop assertion
             ;; is valid. If we ever start placing the same ion in
             ;; multiple points in the sequence, the inner loop assertion
             ;; must be removed, but we have problems then anyway, because
             ;; the insertion point of subsequent things will be ambiguous.
             ;; Also, the insertion point must have been previously
             ;; inserted, although it may be inactive, or for cid 0.
             (ins-rec-loop ion)
             ;; Inactivate the ions for the ACTIVE deletion records.
             ;; Note that there SHOULDN'T be an ion mentioned in a
             ;; deletion-record that wasn't in an earlier
             ;; insertion-record. There may be active deletions of
             ;; inactive ions, but this code doesn't care.
             ;; In all cases, there is a cons cell hanging from the
             ;; ion->cns vector, for any ion.
             (loop for dr being the cvi-deletion-records of cr
               do
                                        ;(assert (<= 1 (cvi-deletion-record-start-ion dr) maxion))
                                        ;(assert (<= 1 (cvi-deletion-record-end-ion dr) maxion))
                                        ;(assert (<= (cvi-deletion-record-start-ion dr)
                                        ;        (cvi-deletion-record-end-ion dr)))
               (loop for ion
                 of-type array-index
                 from (cvi-deletion-record-start-ion dr)
                 to (cvi-deletion-record-end-ion dr)
                 do
                 ;; The following commented out assert is
                 ;; actually not ALWAYS true. It may be the
                 ;; case that a change record has been imported
                 ;; without importing the change that inserted
                 ;; the record being deleted by this ion.
                 ;; However, in that case, we can still set
                 ;; the car of the dummy cons to nil, and doing so
                 ;; unconditionally is faster than checking for the
                 ;; unusual case.
                 ;;(assert (not (eq (aref ion->cns ion) dummy)))
                 (setf (car (aref ion->cns ion)) nil))
               ))
           ;; else for inactive changes, we still modify the ion-list
           ;; for the inactive insertion
           ;; records, since later we may need to splice into the
           ;; ion-list at an ion in these records. This loop is written
           ;; as a macro with 2 invocations to avoid
           ;; slowing down the innermost loop.  The other copy must
           ;; record the ion in the cons cell, this one must record nil,
           ;; since these are inactive insertion records.
           (ins-rec-loop nil)
           ))))

  ;; Done with construction of the full ion-list. We have let the gc have
  ;; the ion->cns array, since we no longer need it.
  ;; Now we fix up the ion-list to not contain the inactive cells,
  ;; and count how many active cells there are, so we can allocate
  ;; the right size of active ion vector.
                                        ;(assert (list-length ion-list))
  (values
   ;; first value
   (and applicable-change-records
        (let* (
               (last ion-list)
               (cur  (cdr last))
               (cnt  1)                 ; 1 for cid 0
               )
          (declare (type cons last)
                   (type list cur)
                   (type array-index cnt))
          ;; We should not have inactivated cid 0
          (assert (car last))
          ;; Could have been done with
          ;;   (setq ion-list (delete nil ion-list))
          ;;   and (length ion-list) to get the cnt,
          ;;  but this should be faster, since it visits
          ;; the cells only once, so does fewer tests
          ;; for end of list and fewer cdr operations.
          (loop
            (if cur
                (progn
                  (if (car (the cons cur))
                      (setf (cdr last) cur
                            last cur
                            cnt (1+ cnt)))
                  (setq cur (cdr (the cons cur))))
                (return)))
          (setf (cdr last) nil)
          (let ((result (make-array cnt :initial-contents ion-list)))
            (cvi-debug "~%CVI gaiv vector ~a found ~a"
                       result applicable-change-records)
            result)
          ))
   ;; second value
   applicable-change-records)

  ))
||#

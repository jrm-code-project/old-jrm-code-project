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
;;;; 04-JUL-1998: Support assignment to zero records to CVI, support wasn't
;;;; living up to expectations in CVI-POPULATE-EMPTY-VIEW. Tested by TEST-12A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CSF/CORE")

(proclaim (standard-optimizations))

(defconstant *cvi-change-record-minimum-preallocation* 10
  "Preallocation amount minimally used in allocating the change-record vector for a CVI.")

(defconstant *cvi-change-record-vector-minimum-growth* 100
  "Minimum growth amount for preallocated change-record vectors after the initial allocation.")

#+cvi-usage-stats
(defvar *cvi-usage-stats-cvi-serial-number* 0
  "used to keep track of cvi objects for a hash table (cleared by explicit calls)")

;(defconstant +cvi-ion-invalid+ 0
;  "Invalid when used as an ION specifier for a record in a sequence")

;(defconstant +cvi-ion-top+ 0
;  "Indicates a relative insertion point meaning 'top of file' or start of sequence.")

;;;
;;; CVI-INSERTION-RECORD
;;;

(defclass cvi-insertion-record ()
  ( ;; ION of first definition inserted (inclusive)
   (start-ion :initarg :start-ion
              :initform +cvi-ion-invalid+
              :type fixnum
              :reader cvi-insertion-record/start-ion)
   ;; ION after which our first record was inserted
   (insertion-point :initarg :insertion-point
                    :initform +cvi-ion-invalid+
                    :type fixnum
                    :reader cvi-insertion-record/insertion-point)
   ;; Array of record values corresponding to the ions.
   (values :initarg :values
           :initform nil
           :reader cvi-insertion-record/values))
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defun cvi-insertion-record/get-value-for-ion (cvi-insertion-record ion)
  (persistent-vector-ref (cvi-insertion-record/values cvi-insertion-record)
                         (- ion (cvi-insertion-record/start-ion cvi-insertion-record))))


;;;
;;; CVI-DELETION-RECORD
;;;
(defclass cvi-deletion-record ()
  ( ;; ION of first deleted record (inclusive)
   (start-ion :initarg :start-ion
              :initform +cvi-ion-invalid+
              :type fixnum
              :reader cvi-deletion-record/start-ion)
   ;; 1+ ION of last deleted record in sequential ION set (therefore exclusive)
   (limit-ion :initarg :limit-ion
            :initform +cvi-ion-invalid+
            :type fixnum
            :reader cvi-deletion-record/limit-ion))
  (:metaclass persistent-standard-class)
  (:schema-version 0))


;;;
;;; CVI-CHANGE-RECORD
;;;
(defclass cvi-change-record ()
  ((cid :initarg :cid
        :initform (error "Required initarg :cid omitted.")
        :type integer
        :reader cvi-change-record/cid) ; the cid associated with this change record
   (insertion-records :initarg :insertion-records
                      :initform nil
                      :reader cvi-change-record/insertion-records)
   (deletion-records  :initform nil
                      :initarg :deletion-records
                      :reader cvi-change-record/deletion-records))
  (:documentation
   "A change record records all physical changes which have been made CVI
    as part of a single logical change which is correlated with a given CID.
    A change record with a given identifier will always by unique with in a
    single CVI object instance, but change records attached to different
    CVI objects may have the same CID, which is what correlates them
    as belonging to the same change-set.")
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defclass cvi (versioned-value)
  ((ion-site-map
    :initform (make-instance 'integer-range-mapper
                             :mapping-level "Pseudo-class ion"
                             :pseudo-class 'ion
                             :repository-mapper (repository/local-mapper *repository*))
    :reader cvi/ion-site-map)
   (change-records
    :initform (make-instance 'persistent-vector :size 0
                             ; :size *cvi-change-record-minimum-preallocation*
                             )
    :reader cvi/change-records
    ;(make-array *cvi-change-record-minimum-preallocation*
    ;                      :fill-pointer 0
    ;                      :initial-element nil)
    )
   (default-allowed?
     :initarg :default-allowed?
     :initform nil
     :reader cvi/default-allowed)

  ;; cvi-usage-stats - tracking cvi objects
  #+cvi-usage-stats (cvi-usage-stats-serial-number (incf *cvi-usage-stats-cvi-serial-number*))
  #+cvi-usage-stats (cvi-usage-stats-object-type nil)
  #+cvi-usage-stats (cvi-usage-stats-slot-name nil))

  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defclass composite-set-versioned-value      (versioned-value) ()
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defmethod clos:initialize-instance ((instance cvi) &rest initargs
                                     &key (initial-value nil initial-value-supplied-p)
                                     &allow-other-keys)
  "Check the initial value argument and initialize :default-allowed?
   based on initial value."
  (apply #'call-next-method instance
         :default-allowed? (when initial-value-supplied-p
                             (or (null initial-value)
                                 (error "Initial value for CVI must be NIL, if supplied.")))
         initargs))

(defun scan-cvi-change-records (cvi)
  (declare (optimizable-series-function))
  (scan-persistent-vector (cvi/change-records cvi)))

(defun scan-cvi-change-records-reverse (cvi)
  (declare (optimizable-series-function))
  (scan-persistent-vector-reverse (cvi/change-records cvi)))

(defun cvi/max-ion (cvi)
  "Return the maximum ion currently in the CVI. Don't use this as the
   basis for new ion allocations, use CVI-ALLOCATE-ION-BLOCK-START for that."
  (1- (integer-range-mapper/next-available-integer (cvi/ion-site-map cvi))))

(defun cvi/allocate-ion-block (cvi ion-count)
  "Allocate the next ion available for use.  Assumes that INSERTION-RECORDS are processed
   successfully if the ion is used, and before the next call to CVI-ALLOCATE-ION-BLOCK-START
   since we must reflect the additions in the ion sequence before this may again be called
   with its current implementation."
  (unless (zerop ion-count)
    (integer-mapper/allocate-integers (cvi/ion-site-map cvi) ion-count (repository/local-mapper *repository*))))

(defun scan-cvi-records (active-ion-vector record-index)
  (declare (optimizable-series-function))
  (map-fn 't
          (lambda (ion-sought)
            ;; (debug-message 5 "Seeking ion ~s" ion-sought)
            (cvi-insertion-record/get-value-for-ion
             (svref record-index ion-sought)
             ion-sought))
          (subseries (scan 'simple-vector active-ion-vector) 1)))

(defun cvi/active-ion-vector (cvi cidset)
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
  ; (declare #.(performance-optimizations))
  (debug-message 4 "CVI/ACTIVE-ION-VECTOR beginning")
  (let* ((ion-list (cons 0 nil))
         (applicable-change-records nil)
         ;; these variables are only used in the loop over the change
         ;; records, whose purpose is to create the full ion-list by
         ;; splicing in new cons cells to the ion-list, in the PROPER
         ;; place.
         (maxion (cvi/max-ion cvi))
         (dummy (cons nil nil))
         (ion->cns (make-array (1+ maxion) :initial-element dummy))
         (cid 0)
         (last-ion 0)
         (last-cns ion-list))
    (declare ;(type (simple-array t (*)) ion->cns)
             ;(type array-index maxion last-ion cid)
             ;(type cons dummy last-cns)
             )
    (assert (typep ion->cns '(simple-array t (*))))
    (assert (typep maxion 'array-index))
    (assert (typep last-ion 'array-index))
    (assert (typep cid 'array-index))
    (assert (typep dummy 'cons))
    (assert (typep last-cns 'cons))
    (setf (aref ion->cns last-ion) last-cns)
    (macrolet ((ins-rec-loop (change-record what)
                 `(iterate ((insertion-record (scan-persistent-vector
                                               (cvi-change-record/insertion-records ,change-record))))
                   ;(debug-message 5 "Doing insertion-record ~s" insertion-record)
                   (setq last-ion (cvi-insertion-record/insertion-point insertion-record))
                          (assert (<= 0 last-ion maxion))
                          (setq last-cns (aref ion->cns last-ion))
                          (assert (not (eq last-cns dummy)))
                          (assert (<= 1
                                      (cvi-insertion-record/start-ion insertion-record)
                                      maxion))
;                           (debug-message 5 "Scanning ~s ions from ~s"
;                                         (persistent-vector-length
;                                          (cvi-insertion-record/values insertion-record))
;                                         (cvi-insertion-record/start-ion insertion-record))

                          (iterate ((ion (scan-range :from (cvi-insertion-record/start-ion insertion-record)
                                                     :length (persistent-vector-length
                                                              (cvi-insertion-record/values insertion-record)))))
                            ;(debug-message 5 "Ion ~s" ion)
                            (assert (eq (aref ion->cns ion) dummy))
                            (setq       ; last-ion ion
                             last-cns
                             (setf (cdr last-cns)
                                   (setf (aref ion->cns ion)
                                         (cons ,what (cdr last-cns)))))))))
      ;(debug-message 5 "Scanning change records ~s" (collect 'list (scan-cvi-change-records cvi)))
      (iterate ((change-record (scan-cvi-change-records cvi)))
        ;(debug-message 5 "Scanning change-record ~s" change-record)
         (setq cid (cvi-change-record/cid change-record))
         (assert (> cid 0))
         (if (cid-set/member cidset cid)
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
;                (debug-message 5 "Insertion record loop ~s"
;                              (collect 'list (scan-persistent-vector
;                                               (cvi-change-record/insertion-records change-record))))
               (ins-rec-loop change-record ion)
;               (debug-message 5 "Done with insertion-record-loop")
               ;; Inactivate the ions for the ACTIVE deletion records.
               ;; Note that there SHOULDN'T be an ion mentioned in a
               ;; deletion-record that wasn't in an earlier
               ;; insertion-record. There may be active deletions of
               ;; inactive ions, but this code doesn't care.
               ;; In all cases, there is a cons cell hanging from the
               ;; ion->cns vector, for any ion.
               (when (cvi-change-record/deletion-records change-record)
;                  (debug-message 5 "Deletion record loop ~s"
;                                (collect 'list (scan-persistent-vector
;                                                (cvi-change-record/deletion-records change-record))))
               (iterate ((deletion-record (scan-persistent-vector (cvi-change-record/deletion-records change-record))))
;                  (debug-message 5 "Deletion record ~s" deletion-record)
                     (assert (<= 1
                                 (cvi-deletion-record/start-ion deletion-record)
                                 (cvi-deletion-record/limit-ion deletion-record)
                                 (1+ maxion)))
                     (iterate ((ion (scan-range :from (cvi-deletion-record/start-ion deletion-record)
                                                :below (cvi-deletion-record/limit-ion deletion-record))))
;                      (debug-message 5 "Deleting ion ~s" ion)
                           ;; The following commented out assert is
                           ;; actually not ALWAYS true. It may be the
                           ;; case that a change record has been imported
                           ;; without importing the change that inserted
                           ;; the record being deleted by this ion.
                           ;; However, in that case, we can still set
                           ;; the car of the dummy cons to nil, and doing so
                           ;; unconditionally is faster than checking for the
                           ;; unusual case.
                           (assert (not (eq (aref ion->cns ion) dummy)))
                           (setf (car (aref ion->cns ion)) nil))
                     )))
           ;; else for inactive changes, we still modify the ion-list
           ;; for the inactive insertion
           ;; records, since later we may need to splice into the
           ;; ion-list at an ion in these records. This loop is written
           ;; as a macro with 2 invocations to avoid
           ;; slowing down the innermost loop.  The other copy must
           ;; record the ion in the cons cell, this one must record nil,
           ;; since these are inactive insertion records.
           (ins-rec-loop change-record nil)
           )))
    (debug-message 5 "Full ion list is ~s" ion-list)
    ;; Done with construction of the full ion-list. We have let the gc have
    ;; the ion->cns array, since we no longer need it.
    (setq ion->cns #())
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
                (cnt  1)                ; 1 for cid 0
                )
           (declare ;(type cons last)
                    ;(type list cur)
                    ;(type array-index cnt)
                    )
           (assert (typep last 'cons))
           (assert (typep cur 'list))
           (assert (typep cnt 'array-index))
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
             (debug-message 5 "CVI/ACTIVE-ION-VECTOR ~a found ~a"
                        result applicable-change-records)
             result)
           ))
     ;; second value
     applicable-change-records)
    ))

(defun cvi-create-insertion-record-ion-index (cvi)
  "Return a vector indexed by ION which yields the CVI-INSERTION-RECORD which contains the
   ION.  This is typically used for retrieving record values corresponding to an ION.
   NOTE: a CVI may have deletion records specific which name IONS, which are used as keys to
   the resulting index, which are NOT resident in the repository (in the case of imports
   with soft deletion dependencies which aren't enforced)."
  (declare #.(performance-optimizations))
  (let ((result (make-array (1+ (the array-index (cvi/max-ion cvi))) :initial-element nil)))
    ;; Triple nested loop is performance problem.
    (iterate ((change-record (scan-cvi-change-records cvi)))
      (iterate ((insertion-record (scan-persistent-vector (cvi-change-record/insertion-records change-record))))
        (iterate ((index (scan-range :from  (cvi-insertion-record/start-ion insertion-record)
                                     :length (persistent-vector-length
                                              (cvi-insertion-record/values insertion-record)))))
          (setf (svref result index) insertion-record))))
    result))

(defun cvi-get-ion-vector-and-index (cvi cid-set)
  (multiple-value-bind (active-ion-vector bound-view)
      (cvi/active-ion-vector cvi cid-set)
    (values active-ion-vector
            bound-view
            (cvi-create-insertion-record-ion-index cvi))))

(defun cvi/scan-reconstructed-value (cvi cid-set instance slot-name)
  (declare (optimizable-series-function))
  (multiple-value-bind (active-ion-vector bound-view ion-index)
      (cvi-get-ion-vector-and-index cvi cid-set)
    ;; The active-ion-vector will be NIL if there were no applicable change-sets.
    ;; In this case, a SLOT-UNBOUND error is called for unless the index allows defaults,
    (unless active-ion-vector
      ;; The fact that no ions are visible doesn't mean the slot is unassigned.
      ;; If deletes equal inserts, then the view is still assigned.
      ;; For SLOT-UNBOUND to be signalled, there must be no deletes or inserts whatsoever.
      ;; (i.e. no change-records with a cid in the cid-set)
      ;; Unless defaults are permitted, signal SLOT-UNBOUND here.
      (unless (or bound-view
                  (cvi/default-allowed cvi))
        (slot-unbound (class-of instance) instance slot-name)))
    ;; We're ready to roll.  If there are no applicable records, because of slot unbound or
    ;; just all cancelled, we check for index >= length of sequence for termination of record stream.
    (debug-message 5 "Active ion vector for retrieval:  ~s" active-ion-vector)
    (scan-cvi-records (or active-ion-vector #()) ion-index)))

(defun cvi/reconstruct-value (cvi cid-set instance slot-name)
  "Create a sequence of the CVI records as seen under CID-SET.

   VERSIONED-OBJECT and VO-SLOT-NAME are passed for the creation of the SLOT-UNBOUND condition which may
   be signalled here."
  (collect 'list (cvi/scan-reconstructed-value cvi cid-set instance slot-name)))

(defun cvi/process-insertion-diff (indel values active-ion-vector next-ion)
  "List the CVI-INSERTION-RECORD which records the effect of the insertion diff described by DIFF-RECORD.

   ACTIVE-ION-VECTOR is the active ion sequence for the synthesized view
   (the old record stream fed to diff)
   and is used to determine the relative position in a view of inserted/deleted lines.

   NEXT-ION is the next ion to use for numbering newly inserted records."
  (let* ((n-records (- (indel/right-subseq-limit indel)
                       (indel/right-subseq-start indel)))
         ;; SSN where definition insertion took place.  Zero-based.
         (record-start-ssn (indel/left-subseq-start indel))
         ;; Map SSN to relative ION.  While one based, zero indicates top-of-sequence situation,
         ;; which needs no special handling since  active-ion-vector[0] = *cvi-ion-top* (= 0)
         (start-ion (cond ((< record-start-ssn (vector-length active-ion-vector))
                           (aref active-ion-vector record-start-ssn))
                          ((zerop record-start-ssn) +cvi-ion-top+)
                          (t
                           ;; (debug-message 5 "FOO!  ~s ~s" record-start-ssn (vector-length active-ion-vector))
                           (error "Active ion vector shouldn't be empty for insertions which ~
                                 aren't initial top-of-file content")))))
    (debug-message 5 "CVI/process-insertion-diff  Insert ~d record~:P at SSN ~d, ION ~d "
                   n-records record-start-ssn start-ion)
    (make-instance 'cvi-insertion-record
                   :start-ion       next-ion
                   :insertion-point start-ion
                   :values (->persistent-vector values))))

(defun cvi/process-deletion-diff (indel active-ion-vector)
  "List the cvi-deletion-records which record the effect of the deletion diff described by DIFF-RECORD.
  ACTIVE-ION-VECTOR is the active ion sequence for the view of CVI being modified, from which we obtain
  relative positions expressed in ION form, translated from the diff-record SSN form."
  ;; The CVI-ION-SEQUENCE is unaffected, we never delete definitions and their IONS, it's a one-way trip.
  ;; NOTE: from the user's perspective, a diff delete record is just N lines.
  ;; However we have to break that up into contiguous ION ranges, so we may generate
  ;; more than one change deletion record (in terms of ions) for a given diff record (in terms
  ;; of definition positions).
  ;; Loop over deleted lines and break them up into contiguous ion ranges, each range generating
  ;; a CVI-DELETION-RECORD
  (let ((record-start-ssn (1+ (indel/left-subseq-start indel)))
        (n-records        (- (indel/left-subseq-limit indel)
                             (indel/left-subseq-start indel)))
        (ion-range-start +cvi-ion-invalid+)
        (ion-range-limit +cvi-ion-invalid+)
        (deletions nil))
    (debug-message 5 "CVI Delete ~d record~:P at ssn ~d" n-records record-start-ssn)
    (iterate ((record-number (scan-range :from record-start-ssn
                                         :length n-records)))
      (let ((current-record-ion (aref active-ion-vector record-number)))
        (cond ((= ion-range-start +cvi-ion-invalid+)
               ;; start a new ion range
               (setq ion-range-start current-record-ion
                     ion-range-limit (1+ current-record-ion)))
              ((or (< current-record-ion ion-range-start)
                   (>= current-record-ion ion-range-limit))
               ;; Save range of deleted ions in a cvi-deletion-record.
               (debug-message 5 "Making deletion record [~d, ~d)" ion-range-start ion-range-limit)
               (push (make-instance 'cvi-deletion-record
                                    :start-ion ion-range-start
                                    :limit-ion ion-range-limit)
                     deletions)
               ;; Mark the beginning of the new range
               (setq ion-range-start current-record-ion))
              (t nil))
        (setq ion-range-limit (1+ current-record-ion))))
    ;; Process any final deletion range which was accumulated but not stored
    (unless (= ion-range-start +cvi-ion-invalid+)
      (debug-message 5 "Making deletion record [~d, ~d)" ion-range-start ion-range-limit)
      (push (make-instance 'cvi-deletion-record
                           :start-ion ion-range-start
                           :limit-ion ion-range-limit)
            deletions))
    (debug-message 5 "CVI-process-deletion-record  deletion ion range: ~d to ~d" ion-range-start ion-range-limit)
    (nreverse deletions)))

(defun cvi/integrate-diffs (cvi transaction old-record-sequence new-record-sequence active-ion-vector)
  "Call the diff engine with the two record sequences and process the diffs.
   If change records exist, we create the persistent CVI-CHANGE-RECORD, populate it with
   insertion and/or deletion records, and return it.  Otherwise we return NIL.
   ACTIVE-ION-VECTOR is the set of ordered ions which are active and represented by OLD-RECORD-SEQUENCE.
   It will have zero elements if the old view has no records."
  ;; SEMANTICS: this routine will cause a NO-CHANGE situation to be signalled if
  ;; there is no change between old view and new view, regardless of boundness of old view.

  ;; Build a single change record.  We don't install it unless we actually find diffs.
  ;; See the no-change signalling caveats noted in CVI-SET-VALUE.
  ;; Allegrostore PERFORMANCE: assumes caching scheme is in place for slot references
  (let* ((diffs (edit-path (coerce old-record-sequence 'vector) 0 (length old-record-sequence)
                           (coerce new-record-sequence 'vector) 0 (length new-record-sequence)
                           #'objects-equalp))
         (ion (cvi/allocate-ion-block cvi (collect-sum (#m (lambda (indel)
                                                             (- (indel/right-subseq-limit indel)
                                                                (indel/right-subseq-start indel)))
                                                           (scan 'list diffs)))))
         (insertion-records nil)
         (deletion-records nil))
    (iterate ((indel (scan 'list diffs)))
      (let ((deletions (- (indel/left-subseq-limit indel)
                          (indel/left-subseq-start indel))))
        (unless (zerop deletions)
          ;; ugh
          (setq deletion-records (nconc deletion-records (cvi/process-deletion-diff indel active-ion-vector)))))
      (let ((insertions (- (indel/right-subseq-limit indel)
                           (indel/right-subseq-start indel))))
        (unless (zerop insertions)
          (push (cvi/process-insertion-diff indel
                                            (subseq new-record-sequence
                                                    (indel/right-subseq-start indel)
                                                    (indel/right-subseq-limit indel))
                                            active-ion-vector ion) insertion-records)
          (incf ion insertions))))
    (let ((change-record (make-instance 'cvi-change-record
                                        :cid (repository-transaction/cid transaction)
                                        :insertion-records (->persistent-vector (nreverse insertion-records))
                                        :deletion-records (->persistent-vector deletion-records))))

      (debug-message 5 "Pushing change record ~s" change-record)
      (persistent-vector-push (cvi/change-records cvi) change-record)
      cvi)))

(defun cvi/populate-empty-view (cvi transaction value)
  "The current view specified view of CVI is unbound.
   So the sequence VALUE represents one big insert in this view.
   Avoid the DIFF, and populate the current view.  Note that just because we don't have
   any slot-boundness in this view doesn't mean that we don't already have change records,
   ion sequences, etc..

   IMPORTANT SEMANTICS:
   This method will create a change record even if value is an empty sequence.
   In this case, the change record has no insertion records.  However the subtle
   difference between having a change-record with no insertions and no change-record
   whatsoever is that the view is expressly BOUND.

   Return the change-record created, so that results can be processed as they are
   for CVI-INTEGRATE-DIFFS."

  ;; Unlike integrate-diffs, we always create change record UNLESS the new view
  ;; attempts to provide zero records and there is a default, since that is a NO-CHANGE situation.
  (let ((insertion-records nil))
    (unless (zerop (length value))
      (multiple-value-bind (start-ion limit)
          (cvi/allocate-ion-block cvi (length value))
        (declare (ignore limit))
        (setq insertion-records (make-instance 'persistent-vector :size 1))
        (setf (persistent-vector-ref insertion-records 0)
              (make-instance 'cvi-insertion-record
                             :start-ion start-ion
                             :insertion-point +cvi-ion-top+
                             :values (->persistent-vector value))))
      ;; (debug-message 5 "Insertion records are ~s ~s ~s ~s"
      ;;               insertion-records
      ;;               (persistent-vector-ref insertion-records 0)
      ;;               (cvi-insertion-record/values (persistent-vector-ref insertion-records 0))
      ;;               (persistent-vector-ref (cvi-insertion-record/values (persistent-vector-ref insertion-records 0)) 0))

      (let ((change-record (make-instance 'cvi-change-record
                                          :cid (repository-transaction/cid transaction)
                                          :insertion-records insertion-records)))
        (debug-message 5 "Change record is ~s ~s" change-record (cvi-change-record/insertion-records change-record))
        (persistent-vector-push (cvi/change-records cvi) change-record)
        change-record))))

(defconstant *rsdiff-enable-ADC-import* nil)

(defmethod versioned-value/view ((cvi cvi) cid-set instance slot)
  (debug-message 4 "Viewing CVI slot ~s under cid-set ~s" slot cid-set)
  (cvi/reconstruct-value cvi cid-set instance slot))

(defmethod versioned-value/update (new-value (cvi cvi) transaction instance slot-name)
  "Update a CVI to reflect the change in structure between that view specified by the cid-set
  in TRANSACTION and the view specified in NEW-VALUE, which must be of type SEQUENCE."
  (debug-message 4 "Assigning CVI slot ~s.  Value:  ~s" slot-name new-value)

  (cvi-expunge-any-cid-change-record cvi (repository-transaction/cid transaction))

  (let ((old-cid-set
         ;; Ensure that we use a cid-set in which the current cid is not included!
         (cid-set/remove (repository-transaction/cid-set transaction)
                         (repository-transaction/cid transaction))))

    (debug-message 5 "Transaction cid-set is ~s" (repository-transaction/cid-set transaction))
    (debug-message 5 "Transaction cid is ~s" (repository-transaction/cid transaction))
    (debug-message 5 "Basis cid set is ~s" old-cid-set)

    (multiple-value-bind (active-ion-vector bound-view ion-index)
        (cvi-get-ion-vector-and-index cvi old-cid-set)
      (declare (ignore bound-view))
      (let ((prior-value (collect 'list (scan-cvi-records (or active-ion-vector #()) ion-index))))

        #||                             ;
        (old-record-stream nil)
        (active-ion-vector nil)         ;active ordered ions for this view, possibly empty vector
        ;; Convert the value, any value, to a record stream from which CVI processes operate.
        ;; VI-RECORD-STREAM-CREATE will signal an error if the value isn't compatible with record streams.

        (value-stream (vi-record-stream-create value t))
        )

        ;; Create a record stream which represents the "old" version.
        ;;    (cid-set/deactivate-cid old-cid-set (repository-transaction/cid transaction))

        ;; HACK!  When we're expecting the input stream to be in diff -n (ADC diff format)
        ;; used soley for pushing new cvi records to head of sequence,
        ;; don't create a CVI-record-stream for the "old" view to diff, since the diff engine
        ;; isn't going to use it, and it's very expensive to create these because they fetch
        ;; ion sequences, build the ion->insertion-record bitmap, etc.
      (cond ((and *rsdiff-enable-ADC-import* *cvi-pushing-elements*)
        (setq old-record-stream (vi-null-stream-create))
        (setq active-ion-vector #()))

        (t
        (setq old-record-stream
        (cvi-record-stream-create cvi old-cid-set versioned-object vo-slot-name :allow-unbound t))
        (setq active-ion-vector (cvi-record-stream-active-ion-vector old-record-stream))))
      ||#

        ;; For now, we don't store any change records for NO-CHANGE scenarios.
        ;; We signal the no-change condition,  it is intercepted or not, and life goes on.
        ;; If we want to store a change-record, we may have to do a bit of work to handle
        ;; zero insertion records and zero added ions.

        ;; ALSO: Check for this being an assignment to an unassigned slot in the requested view.
        ;; If it is, call a special population routine instead of doing diff, and creating a stream
        ;; for the old empty view, unless we're doing the first ADC-import.
        ;; Note that OLD-RECORD-STREAM isn't used if we have empty view.
        ;; Note the ADC check is an abnormal use of this function, because normally the active-ion-vector
        ;; is established because this isn't an initial load, but for ADC diff mode, this may be the initial load.
      (let* ((empty-view? (or (null active-ion-vector)
                              (zerop (vector-length active-ion-vector))))
             (change-record
              (if (and empty-view?
                       (not *rsdiff-enable-ADC-import*))
                  (progn
                    (debug-message 4 "Slot was empty, new value installed.")
                    (cvi/populate-empty-view cvi transaction new-value))
                  (progn
                    (debug-message 4 "Slot had prior value of ~s." prior-value)
                    (cvi/integrate-diffs cvi transaction prior-value new-value (or active-ion-vector #()))))))
        (if change-record
            ;; here, I put in a t for the then case, removing the
            ;; earlier cvi-update-ion-sequence call.
            ;; We no longer HAVE a cvi-ion-sequence.
            t
            ;; No diffs were found
            (unless (and empty-view? *rsdiff-enable-ADC-import*) ; stop no-change warn for ADC empty file
              (versioned-object-no-change-condition-signal instance slot-name new-value))))

      ;; Log the change.
      ;; Note that this will log changes for which no records will exist in the index
      ;; if the no-change condition handler doesn't do non-local control flow.
      (repository-transaction/log-change transaction instance slot-name)
      new-value))))

(defun cvi-expunge-any-cid-change-record (cvi cid)
  "An attempt is being made to assign contents to an already assigned CID.
   This is permitted only for a change-transaction CID, as it is with SCALAR and other versioning
   techniques.  We must expunge the existing CID indexed records before we continue.
   No expunge of CID-master/detail tables is necessary.
   This function returns NIL."
  ;; JDT speculates that since this is only called from vi-set-value and
  ;; we are always working with the most current cid in vi-set-value and
  ;; since in a single RW server situation that means it will always be the
  ;; most recent cid -- we dont need to have cvi-find-cid-change-record look
  ;; at all of the cids  -- there may be an issue if we allowed concurrent
  ;; r/w transactions

  (multiple-value-bind (change-record change-record-vector-index)
      (cvi-find-cid-change-record cvi cid :most-recent-only t)
    (when change-record
      (debug-message 5 "Expunging change record ~s at index ~d" change-record change-record-vector-index)
      ;; (deletef change-record (cvi-change-records cvi)) - what we used to do before astore-vector
      ;; (astore-vector-delete-element (cvi/change-records cvi) change-record-vector-index nil)
      (setf (persistent-vector-ref (cvi/change-records cvi) change-record-vector-index) nil)

      ;; Call ALLEGROSTORE:DELETE-INSTANCE, and delete whole CVI structure tree rooted here.
      ;; WARNING!! NOTE!! We want to delete the CVI structures, but NOT the values which were
      ;; in the slot. That is the user's responsibility.
      ;; (cvi-change-record-delete-structure change-record)
      )
    nil))

(defun cvi-find-cid-change-record (cvi cid &key most-recent-only)
  "Find in CVI any change-record associated with the local CID value.
   Returns two values: the change-record and the index in the cvi/change-records at which
   it was found.  If no matching record is found, only NIL is returned.
   if checking MOST-RECENT-ONLY, processing will stop if we don't match at end
   The only problem with this might be if we allowed concurrent r/w transactions."
  ;(declare #.(performance-optimizations))
  (let* ((vector (cvi/change-records cvi))
         (aref-function #'persistent-vector-ref)
         (limit (- (persistent-vector-length vector) 1)))
    (when (< limit 0) ;; nothing to check
      (return-from cvi-find-cid-change-record (values nil nil)))
    (when most-recent-only
      (let ((chg-rec (funcall aref-function vector limit)))
        (if (= (cvi-change-record/cid chg-rec) cid)
            (return-from cvi-find-cid-change-record (values chg-rec limit))
          ;; else quit, no possible entry below here (brand new cid)
          (return-from cvi-find-cid-change-record (values nil nil)))
        ))
    ;; may get here it is not the newest cid
    (loop for x from limit downto 0  ;; count down from most recent
          as cvi-change-record = (funcall aref-function vector x)
          when (= (cvi-change-record/cid cvi-change-record) cid)
          do (return-from cvi-find-cid-change-record (values cvi-change-record x)))))

;;; It only makes sense to scan CVI
(defun composite-versioned-value/scan (versioned-value cid-set instance slot)
  (declare (optimizable-series-function))
  (cvi/scan-reconstructed-value versioned-value
                                cid-set
                                instance slot))

(defmethod versioned-value/most-recent-cid ((cvi cvi) cid-set)
  (collect-first
   (choose-if (lambda (cid)
                (cid-set/member cid-set cid))
              (#m cvi-change-record/cid
                  (scan-cvi-change-records-reverse cvi)))))

(defmethod versioned-value/cid-list ((cvi cvi))
  (collect 'list (#m cvi-change-record/cid (scan-cvi-change-records-reverse cvi))))

(defmethod versioned-value/cid-set (repository (cvi cvi))
  (list->cid-set repository (versioned-value/cid-list cvi)))

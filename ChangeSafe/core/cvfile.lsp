;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2003 ChangeSafe, LLC
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

(defclass cvfile (versioned-value)
  ((guid :initarg :guid
         :initform (error "Required initarg :guid omitted.")
         :reader cvfile/guid)
   (vfile-fuid :initarg :vfile-fuid
               :initform (error "Required initarg :vfile-fuid omitted.")
               :reader cvfile/fuid)
   ;; These two vectors hold the mapping from repository CID
   ;; to vfile CID.
   (cid-vector :initarg :cid-vector
               :initform (make-instance 'persistent-vector :size 0)
               :reader cvfile/cid-vector)
   (file-cid-vector :initarg :file-cid-vector
                    :initform (make-instance 'persistent-vector :size 0)
                    :reader cvfile/file-cid-vector))
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defmethod clos:initialize-instance ((instance cvfile) &rest initargs)
  (let* ((cid-vector      (make-instance 'persistent-vector :size 1))
         (file-cid-vector (make-instance 'persistent-vector :size 1))
         (guid (generate-guid))
         (vfile-transaction (vfile-transaction/make (repository/vfiles *repository*) 0 nil))
         (vfile-fuid (vfile-create-file vfile-transaction +root-directory-duid+ (prin1-to-string guid))))

    (vfile/create-from-fuid vfile-transaction vfile-fuid #())
    (setf (persistent-vector-ref cid-vector 0)      (repository-transaction/cid *transaction*)
          (persistent-vector-ref file-cid-vector 0) (vfile-transaction/cid vfile-transaction))

    (apply #'call-next-method instance
           :guid            guid
           :vfile-fuid      vfile-fuid
           :cid-vector      cid-vector
           :file-cid-vector file-cid-vector
           initargs)))

(defun scan-cvfile-cid-mapping (cvfile)
  (declare (optimizable-series-function 2))
  (cotruncate (scan-persistent-vector (cvfile/cid-vector cvfile))
              (scan-persistent-vector (cvfile/file-cid-vector cvfile))))

(defun cvfile/map-cid-set (cvfile cid-set)
  "Map a repository cid set into a vfile cid set."
  (multiple-value-bind (cids file-cids)
      (scan-cvfile-cid-mapping cvfile)
    (collect 'list
             (choose 
              (map-fn 't 
                      (lambda (cid)
                        (cid-set/member cid-set cid))
                      cids)
              file-cids))))

(defmethod versioned-value/cid-set (repository (cvfile cvfile))
  (debug-message 4 "Computing cid-set for cvfile ~s" cvfile)
  (list->cid-set repository (collect 'list (scan-persistent-vector (cvfile/cid-vector cvfile)))))

(defmethod versioned-value/most-recent-cid ((cvfile cvfile) cid-set)
  (debug-message 4 "Finding most recent cid for cvfile ~s" cvfile)
  (collect-last
   (choose-if (lambda (cid)
                (cid-set/member cid-set cid))
              (multiple-value-bind (cids file-cids)
                  (scan-cvfile-cid-mapping cvfile)
                (map-fn 't (lambda (cid file-cid)
                             (declare (ignore file-cid))
                             cid)
                        cids file-cids)))))

(defmethod versioned-value/view ((cvfile cvfile) cid-set instance slot)
  (debug-message 4 "Viewing CVFILE slot ~s under cid-set ~s" slot cid-set)
  (let ((transaction nil))
    (unwind-protect
        (multiple-value-prog1
            (setq transaction (vfile-transaction/make (repository/vfiles *repository*) (cvfile/map-cid-set cvfile cid-set) 't))
          (vfile/get-lines transaction (cvfile/fuid cvfile)))
      "Clean up transaction."
      (when transaction (vfile-transaction/commit transaction)))))

(defmethod versioned-value/update (new-value (cvfile cvfile) transaction instance slot-name)
  (debug-message 4 "Assigning CVFILE slot ~s.  Value:  ~s" slot-name new-value)
  
  (let* ((old-cid-set
          ;; Ensure that we use a cid-set in which the current cid is not included!
          (cid-set/remove (repository-transaction/cid-set transaction)
                          (repository-transaction/cid transaction)))
         (old-file-cid-set (cvfile/map-cid-set cvfile old-cid-set))
         (vfile-transaction nil))
    (debug-message 5 "Transaction cid-set is ~s" (repository-transaction/cid-set transaction))
    (debug-message 5 "Transaction cid is ~s" (repository-transaction/cid transaction))
    (debug-message 5 "Basis cid set is ~s" old-cid-set)
    (debug-message 5 "Mapped basis cid set is ~s" old-file-cid-set)

    (let ((transaction (vfile-transaction/make (repository/vfiles *repository*) old-file-cid-set nil)))
      (vfile/update-from-fuid transaction (cvfile/fuid cvfile) (coerce new-value 'vector))
      ;; Ok to commit.  If we subseqently abort, we just lose a cid.
      (vfile-transaction/commit transaction)
      ;; now update the mapping
      (let ((probe (persistent-vector-find (repository-transaction/cid transaction) (cvfile/cid-vector cvfile))))
        (if probe
            ;; overwrite the old value
            (setf (persistent-vector-ref (cvfile/file-cid-vector cvfile) probe) (vfile-transaction/cid vfile-transaction))
            ;; push a new pair
            (progn (persistent-vector-push (cvfile/cid-vector cvfile) (repository-transaction/cid transaction))
                   (persistent-vector-push (cvfile/file-cid-vector cvfile) (vfile-transaction/cid vfile-transaction)))))))
      ;; Log the change.
      ;; Note that this will log changes for which no records will exist in the index
      ;; if the no-change condition handler doesn't do non-local control flow.
      (repository-transaction/log-change transaction instance slot-name)
  new-value)

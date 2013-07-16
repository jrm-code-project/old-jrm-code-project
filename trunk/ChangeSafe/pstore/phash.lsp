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

;;; Jrm's log-based persistent store
(in-package "CSF/PERSISTENT-STORE")

(proclaim (standard-optimizations))

(eval-when (:compile-toplevel :load-toplevel)
  (export '(csf/config::*default-persistent-hash-table-size*)
          "CSF/CONFIG")
  (export '(grow-persistent-vector
            persistent-vector
            persistent-vector-ref
            persistent-vector-length
            persistent-vector-push
            persistent-vector-push2
            persistent-vector-pushnew
            persistent-vector-pop
            persistent-vector-find
            scan-persistent-vector
            scan-persistent-vector-reverse
            ->persistent-vector
            persistent-hash-table
            persistent-hash-table/gethash
            persistent-list
            pstore-list
            pstore-list/car
            pstore-list/cdr
            pstore-list-cons
            pstore-list-push
            pstore-list-reclaim
            lisp-list->pstore-list
            pstore-list->lisp-list
            scan-pstore-list)))

;;; A persistent vector is a mapping from a compact set of
;;; non-negative integers less than the vector length to objects.


;;; The persistent vector itself simply contains the node id of the
;;; underlying object.  This allows us to create the illusion of
;;; mutability by remaking the underlying instance.  The cost is
;;; a trip through the object map.

(defclass persistent-vector ()
  ((vector-node-id :initarg :vector-node-id
                   :reader persistent-vector/vector-node-id))
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defmethod print-object ((object persistent-vector) stream)
  (print-unreadable-object (object stream :Type t :identity t)))

(defun %persistent-vector-ref (pvector index)
  (persistent-object/find
   (persistent-standard-object/pstore pvector)
   (persistent-object/vector-ref
    (persistent-standard-object/pstore pvector)
    (persistent-vector/vector-node-id pvector)
    index)))

(defun (setf %persistent-vector-ref) (new-value pvector index)
  (persistent-object/vector-set
   (persistent-standard-object/pstore pvector)
   (persistent-vector/vector-node-id pvector)
   index
   new-value))

(defun persistent-vector-length (pvector)
  (persistent-object/vector-ref
   (persistent-standard-object/pstore pvector)
   (persistent-vector/vector-node-id pvector)
   -1))

(defun (setf persistent-vector-length) (new-value pvector)
  (persistent-object/vector-set
   (persistent-standard-object/pstore pvector)
   (persistent-vector/vector-node-id pvector)
   -1
   new-value))

(defmethod make-instance ((class (eql (find-class 'persistent-vector))) &rest initargs
                                  &key (persistent-store *default-persistent-store*)
                                  (size 0) (initial-element 0) &allow-other-keys)

  (let ((vector-node (persistent-object/allocate-vector persistent-store)))
    (persistent-object/vector-set persistent-store vector-node -1 size) ;; size is stored here
    (dotimes (i size)
      (persistent-object/vector-set persistent-store vector-node i initial-element))

    (apply #'call-next-method class
           :vector-node-id vector-node
           initargs)))

(defun persistent-vector-ref (pvector index)
  (when-debugging 4
      (when (> index (persistent-vector-length pvector))
        (error "Index out of range")))
  (%persistent-vector-ref pvector index))

(defun scan-persistent-vector (pvector)
  (declare (optimizable-series-function))
  (map-fn 't (lambda (index)
               (%persistent-vector-ref pvector index))
          (scan-range :from 0 :below (persistent-vector-length pvector))))

(defun scan-persistent-vector-reverse (pvector)
  (declare (optimizable-series-function))
  (map-fn 't (lambda (index)
               (%persistent-vector-ref pvector index))
          (scan-range :from (1- (persistent-vector-length pvector)) 
                      :downto 0
                      :by -1)))

(defun persistent-vector-find (object pvector &key (key #'identity))
  (collect-first
   (choose-if (lambda (entry)
                (eql object
                     (funcall key entry)))
              (scan-persistent-vector pvector))))

(defun ensure-same-repository (pvector element)
  (when (typep element 'persistent-standard-object)
    (unless (eq (persistent-standard-object/pstore pvector)
                (persistent-standard-object/pstore element))
      (error 'changesafe-database-error
             :format-control "Attempt to create a cross-repository pointer from ~s to ~s."
             :format-arguments (list pvector element)))))

(defun check-persistent-vector-index (pvector index)
  (unless (< index (persistent-vector-length pvector))
    (error "Attempt to set index out of range in persistent-vector.")))

(defun (setf persistent-vector-ref) (new-value pvector index)
  (ensure-same-repository pvector new-value)
  (check-persistent-vector-index pvector index)
  (setf (%persistent-vector-ref pvector index)
        (if (typep new-value 'persistent-standard-object)
            (persistent-standard-object/node-id new-value)
            (persistent-object/save new-value (persistent-standard-object/pstore pvector))))
  new-value)

(defun persistent-vector-push (pvector new-value)
  "Extend persistent-vector by one and place new-value in the last slot."
  (ensure-same-repository pvector new-value)
  (let* ((next-index (persistent-vector-length pvector)))
    (setf (persistent-vector-length pvector) (1+ next-index))
    (setf (%persistent-vector-ref pvector next-index)
        (if (typep new-value 'persistent-standard-object)
            (persistent-standard-object/node-id new-value)
            (persistent-object/save new-value (persistent-standard-object/pstore pvector))))
    new-value))

(defun persistent-vector-push2 (pvector new-value1 new-value2)
  "Extend persistent-vector by two and place new-values in the last slots."
  (ensure-same-repository pvector new-value1)
  (ensure-same-repository pvector new-value2)
  (let* ((next-index (persistent-vector-length pvector)))
    (setf (persistent-vector-length pvector) (+ next-index 2))
    (setf (%persistent-vector-ref pvector next-index)
        (if (typep new-value1 'persistent-standard-object)
            (persistent-standard-object/node-id new-value1)
            (persistent-object/save new-value1 (persistent-standard-object/pstore pvector))))
    (setf (%persistent-vector-ref pvector (1+ next-index))
        (if (typep new-value2 'persistent-standard-object)
            (persistent-standard-object/node-id new-value2)
            (persistent-object/save new-value2 (persistent-standard-object/pstore pvector))))))

(defun persistent-vector-pushnew (pvector object)
  (unless (persistent-vector-find object pvector)
    (persistent-vector-push pvector object)))

(defun persistent-vector-pop (pvector)
  "Shorten persistent-vector by one return value that was in the last slot."
  (let* ((next-length (1- (persistent-vector-length pvector)))
         (value       (persistent-vector-ref pvector next-length)))
    (setf (persistent-vector-length pvector) next-length)
    value))

(defun grow-persistent-vector (pvector new-length)
  (let ((old-length (persistent-vector-length pvector)))
    (setf (persistent-vector-length pvector) new-length)
    (do ((i old-length (1+ i)))
        ((>= i new-length))
      (setf (persistent-vector-ref pvector i) nil))))

(defun ->persistent-vector (sequence)
  (let* ((length (length sequence))
         (pv (make-instance 'persistent-vector)))
    (setf (persistent-vector-length pv) length)
    (dotimes (i length)
      (setf (persistent-vector-ref pv i) (elt sequence i)))
    pv))

(defclass persistent-hash-table-bucket-entry ()
  ((key :initarg :key
        :reader persistent-hash-table-bucket-entry/key)
   (value :initarg :value
          :reader persistent-hash-table-bucket-entry/value)
   (next-entry :initarg :next-entry
               :initform (error "Required initarg :next-entry omitted.")
               :reader persistent-hash-table-bucket-entry/next-entry))
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defun scan-persistent-hash-table-bucket (first-bucket)
  (declare (optimizable-series-function))
  (scan-fn 'persistent-hash-table-bucket-entry
           (lambda () first-bucket)
           #'persistent-hash-table-bucket-entry/next-entry
           #'null))

(defun persistent-hash-table-bucket-entry/find (key bucket test)
  (collect-first
   (choose-if (lambda (bucket-entry)
                (funcall test key (persistent-hash-table-bucket-entry/key bucket-entry)))
              (scan-persistent-hash-table-bucket bucket))))

(defclass persistent-hash-table ()
  ((hash-table-test :initarg :test
                    :initform (error "Required initarg :test omitted.")
                    :reader persistent-hash-table/test)
   (bucket-vector :initarg :bucket-vector
                  :reader persistent-hash-table/bucket-vector))
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defparameter *default-persistent-hash-table-size* 5)

(defmethod make-instance ((class (eql (find-class 'persistent-hash-table))) &rest initargs
                          &key
                          (size *default-persistent-hash-table-size*)
                          (test 'eq)
                          (bucket-vector (make-instance 'persistent-vector :size (or size
                                                                                     *default-persistent-hash-table-size*)))
                          &allow-other-keys)
  (apply #'call-next-method class
         :bucket-vector bucket-vector
         :test test
         initargs))

(defun persistent-hash-table/test-function (phashtb)
  (ecase (persistent-hash-table/test phashtb)
    (eq #'eq)
    (= #'=)
    (string= #'string=)
    (string-equal #'string-equal)))

(defun persistent-hash-table/gethash (phashtb key)
  (let* ((buckets (persistent-hash-table/bucket-vector phashtb))
         (length  (persistent-vector-length buckets))
         (hkey    (mod (sxhash key) length))
         (entry  (persistent-hash-table-bucket-entry/find
                  key (persistent-vector-ref buckets hkey)
                  (persistent-hash-table/test-function phashtb))))

    (when entry
      (persistent-hash-table-bucket-entry/value entry))))

(defun persistent-hash-table/puthash (phashtb key value)
  (let* ((buckets (persistent-hash-table/bucket-vector phashtb))
         (length  (persistent-vector-length buckets))
         (hkey    (mod (sxhash key) length))
         (entry  (persistent-hash-table-bucket-entry/find key (persistent-vector-ref buckets hkey)
                                                          (persistent-hash-table/test-function phashtb))))
    (if entry
        (remake-instance entry
                         :key key
                         :value value
                         :next-entry (persistent-hash-table-bucket-entry/next-entry entry))
        (setf (persistent-vector-ref buckets hkey)
              (make-instance 'persistent-hash-table-bucket-entry
                             :key key
                             :value value
                             :next-entry (persistent-vector-ref buckets hkey))))))

(defun (setf persistent-hash-table/gethash) (new-value phashtb key)
  (persistent-hash-table/puthash phashtb key new-value))

;;; Cheesy, but effective.  A pstore-list is a 2-element pstore vector.
(defclass pstore-list ()
  ((cell-node-id :initarg :cell-node
                 :initform (error "Required initarg :cell-node omitted")
                 :reader pstore-list/cell-node-id))
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defmethod print-object ((object pstore-list) stream)
  (print-unreadable-object (object stream :type t :identity t)))

(defun %pstore-car (pstore-list)
  (persistent-object/find
   (persistent-standard-object/pstore pstore-list)
   (persistent-object/vector-ref
    (persistent-standard-object/pstore pstore-list)
    (pstore-list/cell-node-id pstore-list)
    1)))

(defun %pstore-cdr (pstore-list)
  (persistent-object/find
   (persistent-standard-object/pstore pstore-list)
   (persistent-object/vector-ref
    (persistent-standard-object/pstore pstore-list)
    (pstore-list/cell-node-id pstore-list)
    2)))

(defun (setf %pstore-car) (new-value pstore-list)
  (persistent-object/vector-set
   (persistent-standard-object/pstore pstore-list)
   (pstore-list/cell-node-id pstore-list)
   1
   new-value))

(defun (setf %pstore-cdr) (new-value pstore-list)
  (persistent-object/vector-set
   (persistent-standard-object/pstore pstore-list)
   (pstore-list/cell-node-id pstore-list)
   2
   new-value))

(defmethod make-instance ((class (eql (find-class 'pstore-list))) &rest initargs
                          &key (persistent-store *default-persistent-store*)
                          pcar-node pcdr-node)
  (let ((cell-node (persistent-object/allocate-vector persistent-store)))
    (persistent-object/vector-set
     persistent-store cell-node 1 pcar-node)
    (persistent-object/vector-set
     persistent-store cell-node 2 pcdr-node)
    (apply #'call-next-method class
           :cell-node cell-node
           initargs)))

(defun pstore-list-cons (pcar pcdr)
  (let ((car-store (and (typep pcar 'persistent-standard-object)
                        (persistent-standard-object/pstore pcar)))

        (cdr-store (and (typep pcar 'persistent-standard-object)
                        (persistent-standard-object/pstore pcar))))

    (cond ((and car-store cdr-store)
           (unless (eq car-store cdr-store)
             (error 'changesafe-database-error
                    :format-control "Attempt to create a cross-repository pointer."
                    :format-arguments (list pcar pcdr)))
           (make-instance 'pstore-list
                          :persistent-store car-store
                          :pcar-node (if (typep pcar 'persistent-standard-object)
                                         (persistent-standard-object/node-id pcar)
                                         (persistent-object/save pcar car-store))
                          :pcdr-node (if (typep pcdr 'persistent-standard-object)
                                         (persistent-standard-object/node-id pcdr)
                                         (persistent-object/save pcdr car-store))))
          (car-store
           (make-instance 'pstore-list
                          :persistent-store car-store
                          :pcar-node (if (typep pcar 'persistent-standard-object)
                                         (persistent-standard-object/node-id pcar)
                                         (persistent-object/save pcar car-store))
                          :pcdr-node (if (typep pcdr 'persistent-standard-object)
                                         (persistent-standard-object/node-id pcdr)
                                         (persistent-object/save pcdr car-store))))
          (cdr-store
           (make-instance 'pstore-list
                          :persistent-store cdr-store
                          :pcar-node (if (typep pcar 'persistent-standard-object)
                                         (persistent-standard-object/node-id pcar)
                                         (persistent-object/save pcar cdr-store))
                          :pcdr-node (if (typep pcdr 'persistent-standard-object)
                                         (persistent-standard-object/node-id pcdr)
                                         (persistent-object/save pcdr cdr-store))))
          (t (make-instance 'pstore-list
                            :persistent-store *default-persistent-store*
                            :pcar-node (if (typep pcar 'persistent-standard-object)
                                         (persistent-standard-object/node-id pcar)
                                         (persistent-object/save pcar *default-persistent-store*))
                            :pcdr-node  (if (typep pcdr 'persistent-standard-object)
                                         (persistent-standard-object/node-id pcdr)
                                         (persistent-object/save pcdr *default-persistent-store*)))))))

(defun pstore-list/car (pstore-list)
  (%pstore-car pstore-list))

(defun pstore-list/cdr (pstore-list)
  (%pstore-cdr pstore-list))

(defun scan-pstore-list-cdrs (pstore-list)
  (declare (optimizable-series-function))
  (scan-fn '(optional pstore-list)
           (lambda () pstore-list)
           (lambda (previous-list) (pstore-list/cdr previous-list))
           (lambda (previous-list) (null previous-list))))

(defun scan-pstore-list (pstore-list)
  (declare (optimizable-series-function))
  (#M pstore-list/car (scan-pstore-list-cdrs pstore-list)))

(defun (setf pstore-list/car) (new-value pstore-list)
  (ensure-same-repository pstore-list new-value)
  (setf (%pstore-car pstore-list)
        (if (typep new-value 'persistent-standard-object)
            (persistent-standard-object/node-id new-value)
            (persistent-object/save new-value (persistent-standard-object/pstore pstore-list))))
  new-value)

(defun (setf pstore-list/cdr) (new-value pstore-list)
  (ensure-same-repository pstore-list new-value)
  (setf (%pstore-cdr pstore-list)
        (if (typep new-value 'persistent-standard-object)
            (persistent-standard-object/node-id new-value)
            (persistent-object/save new-value (persistent-standard-object/pstore pstore-list))))
  new-value)

(defun pstore-list-reclaim (pstore-list)
  ;; We need a GC
  (setf (%pstore-car pstore-list) nil)
  (setf (%pstore-cdr pstore-list) nil))

(defun lisp-list->pstore-list (lisp-list)
  (if (null lisp-list)
      nil
      (pstore-list-cons (car lisp-list)
                        (lisp-list->pstore-list (cdr lisp-list)))))

(defun persistent-list (&rest args)
  (lisp-list->pstore-list args))

(defun pstore-list->lisp-list (pstore-list)
  (collect 'list (scan-pstore-list pstore-list)))

(defmacro pstore-list-push (object pstore-list)
  `(SETF ,pstore-list (PSTORE-LIST-CONS ,object ,pstore-list)))

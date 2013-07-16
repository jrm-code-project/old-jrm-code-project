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

(in-package "CSF/PERSISTENT-STORE")

(proclaim (standard-optimizations))

;;; This file defines a regression test suite
(eval-when (:load-toplevel :execute)
  (make-instance 'regression-test-suite
                 :name "PERSISTENT-STORE"
                 :directory (translate-logical-pathname "CSF:PSTORE;")))

(define-regression-test test-persistent-store ()
  (call-with-temporary-file "db"
   (lambda (pathname)
     (let ((store (persistent-store/open pathname))
           id
           vid
           (object1 '(this is a list))
           (object2 #(this is a vector))
           (object3 (list 1.0d0 -1.0d0 0.125d0 -0.125d0 1024.0d0 -1024.0d0 .3d0 -.3d0))
           (object4 (list #x87654321 #x12345678 1 -1 2147483647 -2147483648
                          #*101010101010101010101010101
                          #*100000000000000000000000000
                          #*000000000000000000000000001))
           o1id
           o2id
           o3id
           o4id)
       (setq store (persistent-store/close store))
       (setq store (persistent-store/open pathname))
       (call-with-transaction
        store :read-write "Intern test symbol"
        (lambda (transaction)
          (declare (ignore transaction))
          (setq id (persistent-object/save :test-symbol store))
          (assert (eq (persistent-object/find store id) :test-symbol))))
       (setq store (persistent-store/close store))
       (setq store (persistent-store/open pathname))
       (call-with-transaction
        store :read-only "Check test symbol."
        (lambda (transaction)
          (declare (ignore transaction))
          (assert (eq (persistent-object/find store id) :test-symbol))))

       (call-with-transaction
        store :read-write "Intern CL-USER symbols."
        (lambda (transaction)
          (declare (ignore transaction))
          (do-symbols (s (find-package "CL-USER"))
            (let ((id (persistent-object/save s store)))
              (assert (eq (persistent-object/find store id) s))))))
       (call-with-transaction
        store :read-write "Save some objects."
        (lambda (transaction)
          (declare (ignore transaction))
          (setf o1id (persistent-object/save object1 store)
                o2id (persistent-object/save object2 store)
                o3id (persistent-object/save object3 store)
                o4id (persistent-object/save object4 store))))
       (call-with-transaction
        store :read-write "Save a vector and some dids."
        (lambda (transaction)
          (declare (ignore transaction))
          (setf vid (persistent-object/allocate-vector store))
          (dotimes (i 10)
            (persistent-object/vector-set 
             store vid i 
             (make-distributed-identifier
              :domain "test-suite"
              :repository "repository"
              :class :test-class
              :numeric-id i)))))

       (setq store (persistent-store/close store))
       (setq store (persistent-store/open pathname))

       (call-with-transaction
        store :read-only "Verify objects."
        (lambda (transaction)
          (declare (ignore transaction))
          (verify
           (verify-one-return-value (verify-value-equal object1))
           (persistent-object/find store o1id))
          (verify
           (verify-one-return-value (verify-value-equalp object2))
           (persistent-object/find store o2id))
          (verify
           (verify-one-return-value (verify-value-equal object3))
           (persistent-object/find store o3id))
          (verify
           (verify-one-return-value (verify-value-equalp object4))
           (persistent-object/find store o4id))
          (dotimes (i 10)
            (verify
             (verify-one-return-value (verify-value-eql (make-distributed-identifier
                                                        :domain "test-suite"
                                                        :repository "repository"
                                                        :class :test-class
                                                        :numeric-id i)))
             (persistent-object/vector-ref store vid i)))
          (persistent-store/close store)))))))

(defclass test-class ()
  ((name :initarg :name
         :initform 'zippy
         :reader test-class/name))
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defmethod print-object ((object test-class) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (test-class/name object) stream)))

(defclass test-class-1 ()
  ((other :initarg :other
          :initform (make-instance 'test-class)
          :reader test-class-1/other))
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defun test-class-1-name (object)
  (if (slot-boundp object 'other)
      (let ((it (test-class-1/other object)))
        (if (typep it 'test-class)
            (test-class/name it)
            'a-random-other))
      'test-class-1-name-unbound))

(defmethod print-object ((object test-class-1) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (test-class-1-name object) stream)))

(defclass test-class-2 (test-class-1)
  ((xyzzy :initarg :xyzzy
          :initform (make-instance 'test-class-1
                                   :other (make-instance 'test-class
                                                         :name 'nested)))
   (other :initform "Yow"))
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defmethod print-object ((object test-class-2) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (if (slot-boundp object 'xyzzy)
               (slot-value object 'xyzzy)
               "xyzzy-not-bound") stream)
    (princ (if (slot-boundp object 'other)
               (slot-value object 'other)
               "other-not-bound") stream)))

(defclass test-class-3 ()
  ((an-unbound-slot))
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(define-regression-test test-persistent-classes ()
  (call-with-temporary-file "db"
   (lambda (file)
     (let ((store (persistent-store/open file))
           (oi9))
       (call-with-transaction
        store :read-write "Create some persistent instances."
        (lambda (transaction)
          (declare (ignore transaction))
          (let* ((i1 (make-instance 'test-class))
                 (i2 (make-instance 'test-class :name 'griffy))
                 (i3 (make-instance 'test-class-1 :other i2))
                 (i4 (make-instance 'test-class-1 :other i1))
                 (i5 (make-instance 'test-class-1))
                 (i6 (make-instance 'test-class-2 :xyzzy i5))
                 (i7 (make-instance 'test-class-2))
                 (i8 (make-instance 'test-class-2 :other i6))
                 (i9 (make-instance 'test-class-3)))
            (declare (ignore i8 i7 i4 i3))
            (setq oi9 (persistent-standard-object/node-id i9))
            (assert (not (slot-boundp i9 'an-unbound-slot)))
            )))
       (persistent-store/close store)
       (setq store (persistent-store/open file))
       (call-with-transaction
        store :read-only "Just a random transaction."
        (lambda (transaction)
          (declare (ignore transaction))
          (assert (not (slot-boundp (persistent-object/find store oi9) 'an-unbound-slot)))))
       (persistent-store/close store)
       ))))

(define-regression-test test-nested-transactions ()
  (call-with-temporary-file "db"
   (lambda (file)
     (let ((store (persistent-store/open file))
           (oi9))
       (call-with-transaction
        store :read-write "Create an outer transaction."
        (lambda (transaction)
          (declare (ignore transaction))
          (let* ((i1 (make-instance 'test-class))
                 (i2 (make-instance 'test-class :name 'griffy))
                 (i3 (make-instance 'test-class-1 :other i2))
                 (i4 (make-instance 'test-class-1 :other i1)))
            (declare (ignore i3 i4))
            (call-with-transaction
             store :read-write "Create some persistent instances."
             (lambda (transaction)
               (declare (ignore transaction))
               (let* ((i5 (make-instance 'test-class-1))
                      (i6 (make-instance 'test-class-2 :xyzzy i5))
                      (i7 (make-instance 'test-class-2))
                      (i8 (make-instance 'test-class-2 :other i6))
                      (i9 (make-instance 'test-class-3)))
                 (declare (ignore i8 i7))
                 (setq oi9 (persistent-standard-object/node-id i9))
                 (assert (not (slot-boundp i9 'an-unbound-slot)))
                 ))))))
       (persistent-store/close store)
       (setq store (persistent-store/open file))
       (call-with-transaction
        store :read-only "Just a random transaction."
        (lambda (transaction)
          (declare (ignore transaction))
          (assert (not (slot-boundp (persistent-object/find store oi9) 'an-unbound-slot)))))
       (persistent-store/close store)
       ))))

(define-regression-test test-two-phase-transactions ()
  (call-with-temporary-file "db"
   (lambda (file1)
     (call-with-temporary-file "db"
      (lambda (file2)
        (let (oi9)
          (let ((store1 (persistent-store/open file1)))
            (call-with-transaction
             store1 :read-write "Create an outer transaction."
             (lambda (transaction)
               (declare (ignore transaction))
               (let* ((i1 (make-instance 'test-class))
                      (i2 (make-instance 'test-class :name 'griffy))
                      (i3 (make-instance 'test-class-1 :other i2))
                      (i4 (make-instance 'test-class-1 :other i1)))
                 (declare (ignore i3 i4))
                 (let ((store2 (persistent-store/open file2)))
                   (call-with-transaction
                    store2 :read-write "Create some persistent instances."
                    (lambda (transaction)
                      (declare (ignore transaction))
                      (let* ((i5 (make-instance 'test-class-1))
                             (i6 (make-instance 'test-class-2 :xyzzy i5))
                             (i7 (make-instance 'test-class-2))
                             (i8 (make-instance 'test-class-2 :other i6))
                             (i9 (make-instance 'test-class-3)))
                        (declare (ignore i8 i7))
                        (setq oi9 (persistent-standard-object/node-id i9))
                        (assert (not (slot-boundp i9 'an-unbound-slot)))
                        )))
                   ;; Tricky.  This can't really be closed until outer commits.
                   (persistent-store/close store2)))))
            (persistent-store/close store1))
          (let ((store1 (persistent-store/open file1))
                (store2 (persistent-store/open file2)))
            (call-with-transaction
             store2 :read-only "Just a random transaction."
             (lambda (transaction)
               (declare (ignore transaction))
               (assert (not (slot-boundp (persistent-object/find store2 oi9) 'an-unbound-slot)))))
            (persistent-store/close store1)
            (persistent-store/close store2)
            )))))))

(define-regression-test test-persistent-vector-simple ()
  (call-with-temporary-file "db"
   (lambda (file)
     (let ((store (persistent-store/open file))
           v)
       (call-with-transaction
        store :read-write "Create persistent vector."
        (lambda (transaction)
          (declare (ignore transaction))
          (let ((v1 (make-instance 'persistent-vector :size 5)))
            (setq v (persistent-standard-object/node-id v1))
            (dotimes (i 5)
              (setf (persistent-vector-ref v1 i) (make-instance 'test-class :name (cons 'foo i)))))))
       (call-with-transaction
        store :read-write "Read persistent vector."
        (lambda (transaction)
          (declare (ignore transaction))
          (let ((v1 (persistent-object/find store v)))
            (dotimes (i 5)
              (verify
               (verify-one-return-value (verify-value-eql i))
               (cdr (test-class/name (persistent-vector-ref v1 i))))))))
       (persistent-store/close store)))))

(define-regression-test test-persistent-vector ()
  (call-with-temporary-file "db"
   (lambda (file)
     (let ((store (persistent-store/open file))
           v1
           v2
           v3
           o1
           o2)
       (call-with-transaction
        store :read-write "Create persistent vector."
        (lambda (transaction)
          (declare (ignore transaction))
          (setq v1 (make-instance 'persistent-vector :size 5))
          (dotimes (i 5)
            (setf (persistent-vector-ref v1 i) i))))

       (call-with-transaction
        store :read-write "Create another persistent vector."
        (lambda (transaction)
          (declare (ignore transaction))
          (setq v2 (make-instance 'persistent-vector :size 0))
          (dotimes (i 5)
            (persistent-vector-push v2 (- 5 i)))))

       (call-with-transaction
        store :read-write "Create another persistent vector."
        (lambda (transaction)
          (declare (ignore transaction))
          (setq v3 (make-instance 'persistent-vector :size 0))
          (dotimes (i 5)
            (persistent-vector-push v3 v1))))

       (catch 'abort
         (call-with-transaction
          store :read-write "Abort modification of persistent vector."
          (lambda (transaction)
            (declare (ignore transaction))
            (dotimes (i 5)
              (setf (persistent-vector-ref v2 i) (* i i)))
            (throw 'abort nil))))

       (setq o1 (persistent-standard-object/node-id v1)
             o2 (persistent-standard-object/node-id v2))
       (persistent-store/close store)
       (setq store (persistent-store/open file))
       (call-with-transaction
        store :read-only "Verify persistent vector."
        (lambda (transaction)
          (declare (ignore transaction))
          (iterate (((val1 val2 index) (cotruncate (scan-persistent-vector
                                                    (persistent-object/find store o1))
                                                   (scan-persistent-vector
                                                    (persistent-object/find store o2))
                                                   (scan-range :from 0))))
              (format t "~& ~d ~s ~s" index val1 val2))))
       (persistent-store/close store)))))

(define-regression-test test-persistent-list ()
  (call-with-temporary-file "db"
   (lambda (file)
     (let ((store (persistent-store/open file))
           v1
           v2
           o1
           o2)
       (call-with-transaction
        store :read-write "Create persistent list."
        (lambda (transaction)
          (declare (ignore transaction))
          (setq v1 (lisp-list->pstore-list '(a b c d e)))))

       (call-with-transaction
        store :read-write "Create another persistent list."
        (lambda (transaction)
          (declare (ignore transaction))
          (setq v2 (lisp-list->pstore-list '(1 2 3 4 5)))))

       (call-with-transaction
        store :read-write "Modify a persistent list"
        (lambda (transaction)
          (declare (ignore transaction))
          (setf (pstore-list/car v1) 'yy)))

       (catch 'abort
         (call-with-transaction
          store :read-write "Abort modification of persistent vector."
          (lambda (transaction)
            (declare (ignore transaction))
            (setf (pstore-list/car v2) 'xx)
            (throw 'abort nil))))

       (setq o1 (persistent-standard-object/node-id v1)
             o2 (persistent-standard-object/node-id v2))
       (persistent-store/close store)
       (setq store (persistent-store/open file))
       (call-with-transaction
        store :read-only "Verify persistent list."
        (lambda (transaction)
          (declare (ignore transaction))
          (format t "~& ~s ~s" 
                  (pstore-list->lisp-list (persistent-object/find store o1))
                  (pstore-list->lisp-list (persistent-object/find store o2)))))
       (persistent-store/close store)))))

(define-regression-test test-persistent-hash-table ()
  (call-with-temporary-file "db"
   (lambda (file)
     (let ((store (persistent-store/open file))
           ht1
           o1)
       (call-with-transaction
        store :read-write "Create persistent hash table."
        (lambda (transaction)
          (declare (ignore transaction))
          (setq ht1 (make-instance 'persistent-hash-table :size 5))
          (iterate (((key value) (scan-plist '(:skidoo 23
                                               :jrm 38
                                               :route 66
                                               :percent 110
                                               :aardvark 1
                                               :zither 26))))
            (persistent-hash-table/puthash ht1 key value))
          (dolist (key '(:skidoo :jrm :route :percent :aardvark))
            (debug-message 5 "~s ~s" key (persistent-hash-table/gethash ht1 key)))))

       (catch 'abort
         (call-with-transaction
          store :read-write "Abort modify persistent hash table."
          (lambda (transaction)
            (declare (ignore transaction))
            (dolist (key '(:skidoo :jrm :route))
              (persistent-hash-table/puthash ht1 key -1))
            (dolist (key '(:skidoo :jrm :route :percent :aardvark))
              (debug-message 5 "~s ~s" key (persistent-hash-table/gethash ht1 key)))
            (abort-current-transaction "For the heck of it.")
            (throw 'abort nil))))
       (setq o1 (persistent-standard-object/node-id ht1))
       (persistent-store/close store)
       (setq store (persistent-store/open file))
       (call-with-transaction
        store :read-only "Verify persistent vector."
        (lambda (transaction)
          (declare (ignore transaction))
          (let ((ht11 (persistent-object/find store o1)))
          (dolist (key '(:skidoo :jrm :route :percent :aardvark))
            (debug-message 5 "~s ~s" key (persistent-hash-table/gethash ht11 key))))))
       (persistent-store/close store)))))

(define-regression-test test-many-objects ()
  (call-with-temporary-file "db"
   (lambda (file)
     (let ((store (persistent-store/open file)))
       (call-with-transaction
        store :read-write "Create a lot of objects"
        (lambda (transaction)
          (declare (ignore transaction))
          (debug-message 3 "Creating 10000 instances.")
          (dotimes (i 100)
            (debug-message 3 "~d" (* i 100))
            (dotimes (j 100)
              (make-instance 'test-class :name (+ i j))))))
       (persistent-store/close store)
       (setq store (persistent-store/open file))
       (persistent-store/close store)))))

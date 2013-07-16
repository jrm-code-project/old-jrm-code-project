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

;;; This file defines a regression test suite
(eval-when (:load-toplevel)
  (make-instance 'regression-test-suite
                 :name "CORE"
                 :directory (translate-logical-pathname "CSF:CORE;")))

(defclass test-class ()
  ((nvi-slot  :version-technique :nonlogged
              :accessor test-class/nvi-slot)
   (lnvi-slot :version-technique :logged
              :accessor test-class/lnvi-slot)
   (svi-slot  :version-technique :scalar
              :accessor test-class/svi-slot))
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defun call-with-temporary-repository-name (name receiver)
  (call-with-temporary-directory
   (lambda (dir)
     (debug-message 5 "Creating temporary repository in ~s" dir)
     (funcall receiver (->dbpath (merge-pathnames name dir))))))

(define-regression-test test-1a ()
  "Test creation of a repository and some basic versioned-class behavior"
  (call-with-temporary-repository-name "test-db"
   (lambda (repository-name)
     (let ((cid-set-bitmap nil)
           (time-stamps nil))                                        ;time-stamps of cids-created
       (debug-message 5 "test-1a repository is ~s" repository-name)
       (with-open-repository (repository repository-name :update
                                         :if-does-not-exist :create
                                         :if-exists :supersede)
         (call-with-repository-transaction
          :meta-cid-set-specifier :latest-metaversion
          :cid-set-specifier :latest-version
          :repository repository
          :transaction-type :read-write
          :user-id-specifier :nobody
          :reason "test first txn"
          :receiver
          (lambda (txn)
            (format t "~%Starting first transaction, effective CID: ~d" (repository-transaction/cid txn))
            (let ((test-instance (make-instance 'test-class)))
              ;; Haven't updated test-instance, and time-stamp predicate doesn't currently include birthdate
              (assert (null (versioned-object/last-update-timestamp test-instance (repository-transaction/cid-set txn))))
              ;; Put the object in the repository named roots
              (repository/add-locally-named-root repository test-instance 'test)
                ;; birth cids no longer automatically recorded
              ;; (format t "~%Birth Cid: ~a" (versioned-object/birth-cid-object test-instance))
              ;; Test first slot assignment.
              ;; Note that if we don't bind *TXN-CONTEXT* above, we must pass it here.
              ;; Ditto for the accessor function.
              (setf (test-class/nvi-slot  test-instance) 'nvi-value)
              (setf (test-class/lnvi-slot test-instance) "lnvi-value")
              (setf (test-class/svi-slot  test-instance) 1)
              (verify (verify-one-return-value
                       (verify-value-eql 'nvi-value))
                      (test-class/nvi-slot test-instance))
              (verify (verify-one-return-value
                       (verify-value-equal "lnvi-value"))
                      (test-class/lnvi-slot test-instance))
              (verify (verify-one-return-value
                       (verify-value-eql 1))
                      (test-class/svi-slot test-instance))
              ;; Token pathetic attempt to invoke these functions to make sure I didn't
              ;; break them while vectorizing SVI.
              (let ((*disable-versioning* t))
                (format t "~%SVI contains Cid: ~a, Most recent cid:  ~s:  Cid list:  ~s"
                        (versioned-value/contains-cid? (slot-value test-instance 'svi-slot) 1)
                        (versioned-value/most-recent-cid (slot-value test-instance 'svi-slot)
                                                         (repository-transaction/cid-set *transaction*))
                        (versioned-value/cid-list (slot-value test-instance 'svi-slot))))
              ;; Test NO-CHANGE situation.    This test only works if NO-CHANGE doesn't care about
              ;; resetting same value on same side, as to same value on different cids.
              (handler-bind
                  ((versioned-object-no-change-condition
                    (lambda (condition)
                      ;; Bug filed: Why doesn't this invoke the PRINT-OBJECT method designated with :REPORT?
                      ;;(let ((*print-escape* nil)) (print condition))
                      (format t "~%Unexpected condition: ~?"
                              (simple-condition-format-control condition)
                              (simple-condition-format-arguments condition))
                      )))
                ;; None of these will complain about a duplicate value which reflects current CID value.
                ;; Complains only occur if the new change differs from the value which would be there
                ;; by effect of prior cid.
                (setf (test-class/nvi-slot  test-instance) 'nvi-value)
                (setf (test-class/lnvi-slot test-instance) "lnvi-value")
                (setf (test-class/svi-slot  test-instance) 1))
              ;; Now set a new value
              (setf (test-class/nvi-slot        test-instance) 'nvi-new-value)
              (setf (test-class/lnvi-slot test-instance) 'lnvi-new-value)
              (setf (test-class/svi-slot        test-instance) 'svi-new-value)
              (verify (verify-one-return-value
                       (verify-value-eql 'nvi-new-value))
                      (test-class/nvi-slot test-instance))
              (verify (verify-one-return-value
                       (verify-value-eql 'lnvi-new-value))
                      (test-class/lnvi-slot test-instance))
              (verify (verify-one-return-value
                       (verify-value-eql 'svi-new-value))
                      (test-class/svi-slot test-instance))
              ;; Note that the txn-context has a cid-set with CID 1 active.    But our initial cid-set
              ;; won't reflect this change, since the repository doesn't modify the initial input.
              ;; So we want this change, because we'll want that bit-1 on for the second txn later on.
              (let ((cid-set (repository-transaction/cid-set *transaction*)))
                (setq cid-set-bitmap (cid-set->bitmap cid-set))
                (push (versioned-object/last-update-timestamp test-instance cid-set) time-stamps)
                (assert (timestamp? (car time-stamps)))))))

         ;; Start a second R/w txn
         (call-with-repository-transaction
          :meta-cid-set-specifier :latest-metaversion
          :cid-set-specifier (lambda () (bitmap->cid-set repository cid-set-bitmap))
          :repository repository
          :transaction-type :read-write
          :user-id-specifier :nobody
          :reason "test second txn"
          :receiver
          (lambda (txn)
            (format t "~%Starting second transaction, effective CID: ~d" (repository-transaction/cid txn))
            (let ((test-instance (repository/locally-named-root repository 'test)))
              ;; Test NO-CHANGE situation.
              (handler-bind
                ((versioned-object-no-change-condition
                  (lambda (condition)
                    (format t "~%Expected Test Warning A (3 instances expected):~%  ~?"
                            (simple-condition-format-control condition)
                            (simple-condition-format-arguments condition))
                    )))
                ;; birth cids no longer automatically recorded
              ;; (format t "~%Birth Cid: ~a" (versioned-object/birth-cid-object test-instance))
              ;; These no-change situations try to set the values which are already established from
              ;; the prior transaction.
              ;; Note that next three assertions are valid only if we don't throw in handler.
              (setf (test-class/nvi-slot  test-instance) 'nvi-new-value)
              (setf (test-class/lnvi-slot test-instance) 'lnvi-new-value)
              (setf (test-class/svi-slot  test-instance) 'svi-new-value))
            ;; Set different values for this txn
            (setf (test-class/nvi-slot  test-instance) 'nvi-value)
            (setf (test-class/lnvi-slot test-instance) "lnvi-value")
            (setf (test-class/svi-slot  test-instance) 1)
            ;; Attempt to set back old redundant values
            ;; Note that the non-versioned indexes can't discern change back to old value,
            ;; so only SVI index will complain about change.        I.e. one message from the following form.
            (format t "~&Expect 1 test warning B here.")
            (handler-bind
                ((versioned-object-no-change-condition
                  (lambda (condition)
                    (format t "~%Expected Test Warning B (1 instance expected):~% ~?"
                            (simple-condition-format-control condition)
                            (simple-condition-format-arguments condition))
                    )))
              (setf (test-class/nvi-slot  test-instance) 'nvi-new-value)
              (setf (test-class/lnvi-slot test-instance) 'lnvi-new-value)
              (setf (test-class/svi-slot  test-instance) 'svi-new-value))
            (push (versioned-object/last-update-timestamp test-instance (repository-transaction/cid-set *transaction*))
                  time-stamps)
            (assert (timestamp/more-recent? (first time-stamps) (second time-stamps)))
            )) ; end of second txn
          )))
     ;;(break) ;uncomment this to examine the database before it disappears.
     )))

(defclass test-cvi-class ()
  ((cvi-slot-a :version-technique :composite-sequence
               :accessor test-cvi-class/cvi-slot-a)
   (cvi-slot-b :version-technique :composite-sequence)
   (cvi-slot-c :version-technique :composite-sequence :initform nil))
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defclass test-cvi-class-as-set ()
  ((cvi-slot-a :version-technique :composite-set
               :accessor test-cvi-class-as-set/cvi-slot-a)
   (cvi-slot-b :version-technique :composite-set)
   (cvi-slot-c :version-technique :composite-set :initform nil))
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defun test-input-coerce (input element-type)
  (ecase element-type
    (string (mapcar #'princ-to-string input))
    ((nil) input)))

(define-regression-test test-cvi (&key element-type (collection-type :sequence))
  "Classic basic CVI test."
  (call-with-temporary-repository-name "test-db"
   (lambda (repository-name)
    (flet ((get-slot-a (instance)
             (ecase collection-type
               (:set (test-cvi-class-as-set/cvi-slot-a instance))
               (:sequence       (test-cvi-class/cvi-slot-a instance))))

           (set-slot-a (instance value)
             (ecase collection-type
               (:set (setf (test-cvi-class-as-set/cvi-slot-a instance) value))
               (:sequence (setf (test-cvi-class/cvi-slot-a instance) value)))))
      (let ((cid-set-bitmap nil))

      (with-open-repository (repository repository-name :update
                              :if-does-not-exist :create
                              :if-exists :supersede)
         ;; Start first R/w txn
         (call-with-repository-transaction
          :meta-cid-set-specifier :latest-metaversion
          :cid-set-specifier :latest-version
          :repository repository
          :transaction-type :read-write
          :user-id-specifier :nobody
          :reason "test first txn"
          :receiver
          (lambda (txn)
            (let ((test-instance (make-instance (ecase collection-type
                                                  (:set 'test-cvi-class-as-set)
                                                  (:sequence 'test-cvi-class)))))
              ;; Put the object in the repository named roots
              (repository/add-locally-named-root repository test-instance 'test)
              ;; Assign first value
              (set-slot-a test-instance (test-input-coerce '(a b c) element-type)) ; CID-2
              (print (get-slot-a test-instance))
              (setq cid-set-bitmap (cid-set->bitmap (repository-transaction/cid-set txn)))
              nil)))
         (dolist (input '((a b c d)     ; +d CID-3
                          (0 a b c d)   ; +0 CID-4
                          (0 a b c)     ; -d CID-5
                          (a b c)       ; -0 CID-6
                          (-1 0 1 a a1 b b1 c d e f) ; + '-1' 0 1 a1 b1 d e f         CID-7
                          (-1 1 a a1 b b1 c d e f g h) ; -0 +g +h    CID-8
                          (1 a a1 b b1 e f g h i) ;  - -1 'e f' + i  CID-9
                          (0 1 2 a a1 b b1 f g h i j)
                          ))
         (call-with-repository-transaction
          :meta-cid-set-specifier :latest-metaversion
          :cid-set-specifier (lambda () (bitmap->cid-set repository cid-set-bitmap))
          :repository repository
          :transaction-type :read-write
          :user-id-specifier :nobody
          :reason "test first txn"
          :receiver
          (lambda (txn)
            (let ((test-instance (repository/locally-named-root repository 'test)))
              (set-slot-a test-instance (test-input-coerce input element-type))
              (let ((*print-pretty* t))
                (print (get-slot-a test-instance)))
              (setq cid-set-bitmap (cid-set->bitmap (repository-transaction/cid-set txn)))
              nil)))))

      ;; Now for a couple of random views
      (with-open-repository (repository repository-name :readonly
                              :if-does-not-exist :create
                              :if-exists :supersede)
        (flet ((check-view (cid-set-bitmap expected-result)
                 ;; Use a r/w txn
                 (call-with-repository-transaction
                  :meta-cid-set-specifier :latest-metaversion
                  :cid-set-specifier (lambda () (make-instance 'dense-cid-set 
                                                               :repository repository
                                                               :bitmap (subseq cid-set-bitmap 1)))
                  :repository repository
                  :transaction-type :read-only
                  :user-id-specifier :nobody
                  :reason "test first txn"
                  :receiver
                  (lambda (txn)
                    (declare (ignore txn))
                    (let ((test-instance (repository/locally-named-root repository 'test)))
                      (let ((*print-pretty* t))
                        (verify (verify-one-return-value
                                 (verify-value-equal expected-result))
                                (values (get-slot-a test-instance)))))))))
            ;; Find the 'd' inserted.
            (check-view #*00001000000 '(d))
            ;; Find the '0' inserted.
            (check-view #*00000100000 '(0))
            ;; Find the 'abc' inserted
            (check-view #*00010000000 '(a b c))
            ;; Find the '0' and 'd'
            (check-view #*00001100000 '(0 d))
            ;; Find the empty view     -- BUG? Should signal unassigned?  Maybe not. Currently doesn't.
            (check-view #*00000010000 '())
            ;; Find '-1 0 1 a1 b1 d e f'
            (check-view #*00000000100 '(-1 0 1 a a1 b1 c d e f))
            ;; Find 'gh', CID-6 is a nop here
            (check-view #*000000010100 '(g h))
            ;; -1 1 a1 b1 d e f g h
            (check-view #*000000001100 '(-1 1 a a1 b1 c d e f g h))
            ;; Find -1 0 1 0 a a1 b b1 c d e f d
            ;; See the comment below in TEST-4C
            (check-view #*000111001000 '(-1 0 1 a a1 0 b b1 c d e f d))
            ))


         ))
    ;;(break)
    )))

(defclass test-vfile-class ()
  ((vfile-slot :version-technique :composite-file
               :accessor test-vfile-class/vfile-slot))
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(define-regression-test test-vfile ()
  "Versioned composite file test."
  (call-with-temporary-repository-name 
   "test-db"
   (lambda (repository-name)
     (let ((cid-set-bitmap nil))
       (with-open-repository (repository repository-name :update
                                         :if-does-not-exist :create
                                         :if-exists :supersede)

         ;; Start first R/w txn
         (call-with-repository-transaction
          :meta-cid-set-specifier :latest-metaversion
          :cid-set-specifier :latest-version
          :repository repository
          :transaction-type :read-write
          :user-id-specifier :nobody
          :reason "test first txn"
          :receiver
          (lambda (txn)
            (let ((test-instance (make-instance 'test-vfile-class)))
              (repository/add-locally-named-root repository test-instance 'test)
              (setf (test-vfile-class/vfile-slot test-instance) #("line1"
                                                                  "line2 and other stuff"
                                                                  "line3"))
              (setq cid-set-bitmap (cid-set->bitmap (repository-transaction/cid-set txn)))
              nil))))
       ;; Now for a couple of random views
       (with-open-repository (repository repository-name :readonly
                                         :if-does-not-exist :create
                                         :if-exists :supersede)
         (call-with-repository-transaction
          :meta-cid-set-specifier :latest-metaversion
          :cid-set-specifier (lambda () (bitmap->cid-set repository cid-set-bitmap))
          :repository repository
          :transaction-type :read-only
          :user-id-specifier :nobody
          :reason "test first txn"
          :receiver
          (lambda (txn)
            (declare (ignore txn))
            (let ((test-instance (repository/locally-named-root repository 'test)))
              (format t "~&~S" (test-vfile-class/vfile-slot test-instance))))))))))


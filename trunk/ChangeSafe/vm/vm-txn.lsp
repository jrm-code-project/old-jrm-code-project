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
;;;; File Name:     vm-txn.lsp
;;;; Author:        Dave Tenny
;;;;
;;;; Module Description:
;;;;
;;;; This module defines version management change transaction semantics
;;;; above and beyond those provided in the CORE package.  The facilities
;;;; here are optional for use by consumers of the VM package, however if
;;;; any of the facilities in this module are used, ALL of the faciilities
;;;; in this module must be used.
;;;;
;;;; Definitions here are meant to be used in place of equivalent CORE
;;;; package definitions.  Use of these constructs assures that high-level
;;;; CHANGE-SET objects and features are available. Failure to use these
;;;; mechanisms means that only low-level CORE-provided change-set
;;;; information is available.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CSF/VERSION-MANAGEMENT")

(proclaim (standard-optimizations))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(call-with-vm-transaction
#||
            call-with-vm-transaction/no-change-set
            vm-txn-subscribe-to-change-set ;call this if you want to be notified about cset creation
            vm-txn-note-change-set      ;overload, but do not call, calling reserved for VM internal use
||#
            )))

(defclass vm-transaction ()
  ((underlying-transaction :initarg :underlying-transaction
                           :reader vm-transaction/underlying-transaction))
  (:documentation "Version management level transaction."))

;;; DO NOT ALLOW NIL OR T IN THIS.
;;; It is too confusing a default.
(defun metaversion->cid-set-specifier (metaversion)
  (etypecase metaversion
    (symbol (if (eq metaversion :latest-metaversion)
                metaversion
                (error "Illegal metaversion ~s specified." metaversion)))
    (timestamp  metaversion)
    (cid-set    metaversion)
    ;; others will go here.
    ))

(defmethod transaction/cid-set ((transaction vm-transaction))
  (transaction/cid-set (vm-transaction/underlying-transaction transaction)))

(defun vm-transaction/cid-object (vm-transaction)
  (repository-transaction/cid-object
   (vm-transaction/underlying-transaction vm-transaction)))

(defun vm-transaction/disposition (vm-transaction)
  (repository-transaction/disposition
   (vm-transaction/underlying-transaction vm-transaction)))

(defun vm-transaction/reason (vm-transaction)
  (repository-transaction/reason
   (vm-transaction/underlying-transaction vm-transaction)))

(defconstant *vm-transaction-types* '(:read-cons
                                      :read-only
                                      :read-write
                                      :read-cons/no-change-set
                                      :read-write/no-change-set
                                      :read-only-compare
                                      :read-cons-nonversioned
                                      :read-only-nonversioned
                                      :read-write-nonversioned))

(deftype vm-transaction-type () `(member ,@*vm-transaction-types*))

(defun vm-transaction-type->core-transaction-type (vm-transaction-type)
  (case vm-transaction-type
    ((:read-cons/no-change-set)  :read-only)
    ((:read-write/no-change-set) :read-only)
    (t vm-transaction-type)))


;;; Receiver for this is tricky.  It has to be a thunk that returns a thunk like this:
;;;
;;;  (lambda (vm-transaction)
;;;      ..... make some changes ....
;;;     ;; return two values, change-set initarg list and a thunk
;;;      (values cset-type
;;;              cset-initargs
;;;          (lambda (change-set)
;;;               ... promote the change set ....
;;;              )))

(defun call-with-vm-transaction (&key repository
                                      transaction-type
                                      user-id-specifier 
                                      reason

                                      metaversion
                                      version-specifier
                                      ;; Override the cids in the version specifier, if desired.
                                      vpb-cid-dids-to-add
                                      vpb-cid-dids-to-remove

                                      aux-metaversion
                                      aux-version-specifier
                                      aux-vpb-cid-dids-to-add
                                      aux-vpb-cid-dids-to-remove
                                      receiver
                                      (change-set-type 'change-set))

  (check-type transaction-type vm-transaction-type)
  (assert version-specifier)
  (debug-message 2 "Starting ~s VM transaction to ~a" transaction-type reason)
  (let ((metaversion-cid-set-specifier (metaversion->cid-set-specifier metaversion))
        (aux-metaversion-cid-set-specifier (and aux-metaversion
                                                (metaversion->cid-set-specifier aux-metaversion))))

    (call-with-repository-transaction
     :repository repository
     :transaction-type  (vm-transaction-type->core-transaction-type transaction-type)
     :user-id-specifier user-id-specifier
     :reason reason
     :meta-cid-set-specifier metaversion-cid-set-specifier
     :cid-set-specifier (lambda ()
                          (compute-cid-set-specifier repository 
                                                   metaversion-cid-set-specifier
                                                   version-specifier
                                                   vpb-cid-dids-to-add
                                                   vpb-cid-dids-to-remove))

     :aux-meta-cid-set-specifier aux-metaversion-cid-set-specifier
     :aux-cid-set-specifier (lambda ()
                              (and aux-metaversion
                                   (compute-cid-set-specifier repository
                                                              aux-metaversion-cid-set-specifier
                                                              aux-version-specifier
                                                              aux-vpb-cid-dids-to-add
                                                              aux-vpb-cid-dids-to-remove)))
     :receiver (lambda (core-txn)
                 (let ((vm-txn (make-instance 'vm-transaction
                                              :underlying-transaction core-txn)))
                   (case transaction-type
                     ((:read-write :read-cons) (call-creating-change-set vm-txn change-set-type receiver))
                     (t (funcall receiver vm-txn))))))))

(defun compute-cid-set-specifier (repository
                                  metaversion-cid-set-specifier
                                  version-specifier
                                  vpb-cid-dids-to-add
                                  vpb-cid-dids-to-remove)
  (let* ((specifier (if (distributed-identifier? version-specifier)
                        (repository/resolve-distributed-identifier repository version-specifier)
                        version-specifier))
         (base-cid-objects
          (etypecase specifier
            (branch (version/cid-objects (branch/most-recent-version specifier)))
            (version (version/cid-objects specifier))
            (symbol (if (eq specifier :latest-version)
                        ;; Avoid getting cids after the 
                        ;; metaversion timestamp
                        (etypecase metaversion-cid-set-specifier
                          (symbol (if (eq metaversion-cid-set-specifier :latest-metaversion)
                                      (repository/master-cid-set repository)
                                      (error "Illegal metaversion-cid-set-specifier ~s" metaversion-cid-set-specifier)))
                          (timestamp (repository/master-cid-set repository :end-time metaversion-cid-set-specifier))
                          (cid-set (cid-set/intersection
                                    metaversion-cid-set-specifier
                                    (repository/master-cid-set repository))))
                        (error "bad version specifier")))))
         (additional-cid-objects (map 'list (lambda (cid-did)
                                              (repository/resolve-distributed-identifier
                                               repository
                                               cid-did))
                                      vpb-cid-dids-to-add))
         (restricted-cid-objects (map 'list (lambda (cid-did)
                                              (repository/resolve-distributed-identifier
                                               repository
                                               cid-did))
                                      vpb-cid-dids-to-remove)))
    (if (cid-set? base-cid-objects)
        (named-let loup ((result-cid-set base-cid-objects)
                         (added          additional-cid-objects)
                         (removed        restricted-cid-objects))
          (cond (removed (loup (cid-set/remove result-cid-set (change-set/resident-cid (car removed)))
                               added
                               (cdr removed)))
                (added (loup (cid-set/adjoin result-cid-set (change-set/resident-cid (car added)))
                             (cdr added)
                             removed))
                (t result-cid-set)))
        (cid-objects->cid-set
         repository
         (union (set-difference base-cid-objects restricted-cid-objects)
                additional-cid-objects)))))

(defun call-creating-change-set (vm-txn change-set-type receiver)
  (multiple-value-bind (change-set-initargs change-set-observer)
      (funcall receiver vm-txn)
      ;; We get here if we exit the form.
      ;; The transaction will be in one of three states:
      ;; :running :committing or :aborting
    (when (or (eq (vm-transaction/disposition vm-txn) :running)
              (eq (vm-transaction/disposition vm-txn) :committing))
      (funcall change-set-observer
               (when (typep (vm-transaction/underlying-transaction vm-txn)
                            'versioned-update-repository-transaction)
                 (let ((change-set (apply #'make-instance
                                          change-set-type
                                          :cid-object (vm-transaction/cid-object vm-txn)
                                          change-set-initargs)))
                   ;; Now that we have a change set, we should notify the
                   ;; core txn layer about it so it can record it.
                   (repository-transaction/note-change-set
                    (vm-transaction/underlying-transaction vm-txn)
                    change-set)
                   ;; And let the caller sniff at it.
                   change-set))))))

;;; Receiver for this is NOT tricky because no change-set is created.
;;;
;;;  (lambda (vm-transaction)
;;;      ..... make some changes ....
;;;          )

;(defun call-with-vm-transaction/no-change-set (&key metaversion
;                                                    version
;                                                    repository user-id-specifier transaction-type
;                                                    reason
;                                                    receiver)


;  (debug-message 2 "Starting VM transaction (no change set) to ~a" reason)
;  (call-with-repository-transaction
;   :repository repository
;   :meta-cid-set-specifier (metaversion->cid-set-specifier metaversion)
;   :cid-set-specifier t
;   :user-id-specifier user-id-specifier
;   :transaction-type  transaction-type
;   :reason reason
;   :receiver (lambda (core-txn)
;               (funcall receiver (make-instance 'vm-transaction
;                                                :underlying-transaction core-txn)))))

#||
(defun vm-txn-subscribe-to-change-set (object)
  "Let OBJECT subscribe to the creation of a change-set object in the current transaction.
   Once subscribed, the object will recive a VM-TXN-NOTE-CHANGE-SET method invocation upon transaction
   closure.  If subscribed more than once, the object will receive only ONE notification.

   it is an error if there is no change transaction in progress when this method is called.
   Returns OBJECT."
  (unless (and *txn-context*
               (txn-for-update? *txn-context*))
    (error "No change transaction is active at this time, a ~s cannot subscribe to the change-set."
           (type-of object)))
  (push object *vm-txn-change-set-subscribers*)
  object)

(defgeneric vm-txn-note-change-set (object change-set)
  (:documentation
   "Notify OBJECT that CHANGE-SET has been created, and we're about to finish the change transaction.
    Return value is ignored.  Objects defining methods on this function may in turn queue more
    objects for notification via VM-TXN-SUBSCRIBE-TO-CHANGE-SET.  It is considered poor form
    for them to call VM-TXN-NOTE-CHANGE-SET directly.

    REPEAT: OVERLOAD THIS METHOD, BUT DO NOT CALL IT YOURSELF."))

(defun vm-txn-notify-change-set-subscribers (change-set)
  "Notify all subscribers to change-set creation.  Do so in such a way that subscribers
   can queue additional subscribers and that we will process those queued during the notification process,
   and never process any subscriber more than once.
   Return value: n/a."
  (loop with hashtable = (make-hash-table :test #'eq)
      while *vm-txn-change-set-subscribers*
      as subscriber = (car *vm-txn-change-set-subscribers*)
      as did-subscriber? = (gethash subscriber hashtable)
      do
        (setq *vm-txn-change-set-subscribers* (cdr *vm-txn-change-set-subscribers*))
        (unless did-subscriber?
          (setf (gethash subscriber hashtable) t) ;don't notify this object again
          (funcall #'vm-txn-note-change-set subscriber change-set))))

(defun vm-txn-secret-macro-pain-avoidance (cset-name cset-var-name)
  "A little function to dynamically evaluate what the compiler would warn about as
   (cond (\"abc\") (t #:78811))  -- an unreachable clause, in with-vm-txn,
   if a constant cset name were provided and we tried to expand (or ,cset-name ,cset-var-name)"
  (or cset-name cset-var-name))

(defmacro with-vm-txn ((repository uid txn-mode reason
                        &rest all-keys
                              ;; Keywords specific to repository-begin-txn for documentation purposes
                        &key suppress-change-transaction
                             ;; keywords specific to this macro, not passed to repository-open
                             (project-var (gensym (symbol-name :WITH-VM-TXN-PROJECT-))) ; give it some value we can
                                        ; use for r/o LET binding
                             no-project
                             cset-name
                             (cset-name-var (gensym (symbol-name :WITH-VM-TXN-CSET-NAME-)))
                             (cset-type :minor)
                             supplementary-change-plist)
                       &body body)
  "Similar to and in replacement of CORE::WITH-REPOSITORY-TXN.

   UID should be a DID or string-did for a CORE::USER subtype, or, if absolutely necessary,
   NIL, indicating there is no appropriate user for the transaction (administrator/root/anonymous access).

   The txn-context-identifier argument is not required, it defaults to *TXN-CONTEXT*.
   The txn-mode IS required, and should be :READ-ONLY or :READ-WRITE.

   REPOSITORY should be bound to a repository-object established with REPOSITORY-OPEN
   (or with-open-repository, etc..)

   The following arguments may be specified for :read-write transactions unless
   :SUPPRESS-CHANGE-TRANSACTION is specified.  They are used as arguments to
   CHANGE-SET-CREATE, which is used to build a high level change-set and associate it with
   the cid in progress.

   PROJECT-VAR, if specified, should name a variable to be bound to NIL prior to
   BODY's execution, and to be bound to a live repository PROJECT object during the
   transaction for any R/W txn in which the resultant change-set is to be associated
   with a specific project.  There are we could require the use of a function to provide
   this information, but this seems more in-your-face for a very critical behavior.

   If a project is assigned to PROJECT-VAR, the resulting change-set will be associated with
   the project via PROJECT-ADD-CHANGE-SET. If no project is assigned prior upon exit from BODY,
   and a change transaction is in progress, a warning will be signalled unless NO-PROJECT is true.

   NO-PROJECT should be specified as true if it is to be expected that no project will be considered
   a binding scope change-sets created by r/w transactions, and that PROJECT-VAR willt therefore
   be unassigned for the transaction.

   CSET-NAME is optional, but should probably be specified for read/write transactions.
   CSET-TYPE is a keyword which classifies the change-set nature. It must be one of the
   values in the *CHANGE-SET-TYPES* list.

   CSET-NAME-VAR is useful if you want to specify the cset-name but need to generate
   the name based on things done in the transaction, in which case you can't know it on transaction
   open.  If cset-name-var is bound to something other NIL when BODY is done, and CSET-NAME is nil, then
   we use the value of CSET-NAME-VAR instead of CSET-NAME to decorate the change-set.

   SUPPLEMENTARY-CHANGE-PLIST is a (possibly empty) plist which is used to provide
   supplementary information attached to change-sets which are created for change transactions.
   It must contain values which will be able to persist in the repository of the change-set
   being created.

   REASON is required in all cases, and is used for general transaction identification,
   (even r/o txns), as well as the cset description for r/w transactions. "
  ;; Strip keywords specific to this macro from those we'll pass to with-repository-txn
  (progn
    (loop for key in '(:project-var :cset-name :cset-type :no-project :supplementary-change-plist
                       :cset-name-var)
        do (remf all-keys key))
    (with-unique-names (with-vm-txn-cset-var with-vm-txn-reason-var)
      `(let ((,with-vm-txn-reason-var ,reason)
             (,project-var nil)
             (,cset-name-var nil)
             (*vm-txn-change-set-subscribers* nil))
         (DEBUG-MESSAGE 2 "Beginning transaction to \"~a\"" ,with-vm-txn-reason-var)
         ;; Only legitimate use of with-repository-txn in the rfm package.
         (with-repository-txn (*txn-context* ,repository ,uid
                               :txn-mode ,txn-mode
                               :reason ,with-vm-txn-reason-var
                               ,@all-keys)
           (multiple-value-prog1
               (LOCALLY ,@body)
             ;; If we reach this point, create the change-set.  These semantics are particularly cautious
             ;; because BODY may have called (TXN-CONTEXT-ABORT-IF-NO-CHANGE *TXN-CONTEXT*).
             ;; If we want to create the cset in advance of body, we need to require users to
             ;; call an SCM replacement for TXN-CONTEXT-ABORT-IF-NO-CHANGE which will exclude
             ;; CHANGE-SET and PROJECT object creation/modificiation from its consideration of
             ;; effective changes. EXCLUDING PROJECT OBJECTS IS NOT A WISE CHOICE.  You might lose,
             ;; for instance, file additions to the root directory, if the project is a directory
             ;; (as it is now).
             (when (and (txn-for-update? *txn-context*)
                        (not ,suppress-change-transaction))
               ;; In case the transaction reason has been changed, use the reason which is currently
               ;; associated with *txn-context*, which may be changed via txn-context-change-reason
               ;; for the cset description. Note however that if the reason is changed in
               ;; the cset-subscription hooks, it won't be bound to the change-set.
               (let ((,with-vm-txn-cset-var
                         (change-set-create (cid-object-find-or-create
                                             (txn-context-cid *txn-context*))
                                            ,cset-type
                                            :name (vm-txn-secret-macro-pain-avoidance
                                                   ,cset-name ,cset-name-var)
                                            :description (txn-context-reason *txn-context*)
                                            :supplementary-info-plist ,supplementary-change-plist)))
                 (txn-context-set-versioned-change-set-information *txn-context* ,with-vm-txn-cset-var)
                 ;; Add the change-set to the project.  There's no need for this except to support
                 ;; certain user expectations about csets 'belonging' to a project.
                 ;; Note: if we're creating a project, there is unlikely to be a project argument!
                 (unless ,no-project
                   (if ,project-var
                       (vm-txn-subscribe-to-change-set ,project-var)
                     (warn "PROJECT-VAR is not bound in WITH-VM-TXN for R/W transaction.")))
                 ;; Notify all change-set subscribers
                 (vm-txn-notify-change-set-subscribers ,with-vm-txn-cset-var)
                 ))))))))
||#

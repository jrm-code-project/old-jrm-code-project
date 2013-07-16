;;;; -*- Mode: LISP; coding: iso-8859-1; Syntax: COMMON-LISP; Base: 10; -*-
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
;;;;
;;;; Module Description:
;;;   Changesafe Commands
;;;;
;;;; Author:        Joe Marshall
;;;; Creation Date: 2003
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CONMAN")


;;; Note that MASTER-CATALOG.LSP is licensed to use a couple of these variables, but unless they're
;;; in this list, they aren't for general consumption even within the same package.

(eval-when (:load-toplevel :execute)
  (export '(
            call-with-master-repository
            call-with-master-transaction
            call-with-master-repository-transaction
            call-with-satellite-repository
            call-with-workspace-repository
            call-with-workspace-transaction
            #||
            with-cm-master-repository
            with-cm-master-txn
            with-cm-master-txn-NO-CSET
            with-cm-master-metaversion
            cm-txn-latest-master-metaversion-p
            with-cm-satellite-repository
            with-cm-satellite-txn
            with-cm-satellite-metaversion
            with-cm-workspace-repository
            cm-txn-start-timestamp
            ||#
            )))

(proclaim (standard-optimizations))

(defun master-transaction-type->open-mode (transaction-mode)
  (ecase transaction-mode
    ((:read-only :read-only-compare  :read-only-nonversioned)   :readonly)
    ((:read-write 
      :read-write/no-change-set
      :read-write-nonversioned 
      :read-cons
      :read-cons/no-change-set
      :read-cons-nonversioned)  :update)))

(defun call-with-master-repository (repository-name open-mode receiver)
  "Open a master repository named by REPOSITORY-NAME, which must be compatible with REPOSITORY-OPEN
   and must be a of repository-type :MASTER.  Invoke receiver on the repository.

   Unlike WITH-OPEN-REPOSITORY, the master repository named by REPOSITORY-NAME must already exist.

   Opening the master repository with this macro may or may not open satellite repositories.
   This is left to the implementation and is based on cache management and other efficiency strategies.
   The key thing is that the user never has to explicitly open satellite repositories, only the master.
   To manipulate satellite repositories, use the WITH-SATELLITE-REPOSITORY macro, which will
   open the satellite if necessary, and which uses known search keys for locating the satellite
   repository."
  (with-open-repository (master-repository repository-name open-mode
                                           ;;(conman-repository-open-mode-from-operating-mode)
                                           :repository-type   :master
                                           :error-if-not-cached t
                                           :if-exists         :open
                                           :if-does-not-exist :error)
    (funcall receiver master-repository)))

(defun satellite-transaction-type->open-mode (transaction-mode)
  (ecase transaction-mode
    ((:read-only :read-only-compare :read-only-nonversioned)   :readonly)
    ((:read-write 
      :read-write/no-change-set
      :read-write-nonversioned 
      :read-cons
      :read-cons/no-change-set
      :read-cons-nonversioned)  :update)))

(defun call-with-satellite-repository (satellite-repository-dbpath open-mode receiver)
  (with-open-repository (satellite-repository satellite-repository-dbpath open-mode
                                              :repository-type   :satellite
                                              :error-if-not-cached t
                                              :if-exists         :open
                                              :if-does-not-exist :error)
    (funcall receiver satellite-repository)))

(defclass cm-transaction ()
  ((underlying-transaction :initarg :underlying-transaction
                           :reader cm-transaction/underlying-transaction))
  (:documentation "Base class of conman transactions."))

(defmethod transaction/cid-set ((transaction cm-transaction))
  (transaction/cid-set (cm-transaction/underlying-transaction transaction)))

(defclass cm-master-transaction (cm-transaction)
  ()
  (:documentation "Conman master transaction."))

(defclass cm-satellite-transaction (cm-transaction)
  ()
  (:documentation "Conman satellite transaction."))

(defclass cm-workspace-transation (cm-transaction)
  ()
  (:documentation "Conman workspace transaction."))

(defun call-with-master-transaction (&key repository
                                          transaction-type
                                          user-id-specifier
                                          reason
                                          master-metaversion
                                          version
                                          aux-master-metaversion
                                          aux-version
                                          receiver
                                          )
  (debug-message 2 "Starting master transaction to ~a" reason)
  (call-with-vm-transaction
   :repository repository
   :transaction-type transaction-type
   :user-id-specifier user-id-specifier
   :reason reason
   :metaversion master-metaversion
   :version-specifier version
   :aux-metaversion aux-master-metaversion
   :aux-version-specifier aux-version
   :change-set-type 'super-change-set
   :receiver (lambda (vm-transaction)
               (funcall receiver 
                        (make-instance 'cm-master-transaction
                                       :underlying-transaction vm-transaction)))))

(defun call-with-master-repository-transaction (master-repository-dbpath user-id-specifier
                                                                         &key
                                                                         transaction-type
                                                                         reason

                                                                         master-metaversion
                                                                         version

                                                                         aux-master-metaversion
                                                                         aux-version

                                                                         receiver)
  (assert version)
  (call-with-master-repository
   master-repository-dbpath (master-transaction-type->open-mode transaction-type)
   (lambda (master-repository)
     (call-with-master-transaction
      :repository         master-repository
      :transaction-type   transaction-type
      :user-id-specifier  user-id-specifier
      :reason             reason
      :master-metaversion master-metaversion
      :version            version
      :aux-master-metaversion aux-master-metaversion
      :aux-version aux-version
      :receiver (lambda (master-transaction)
                  (funcall receiver master-repository master-transaction))))))


(defun call-with-satellite-transaction (&key repository
                                             transaction-type
                                             user-id-specifier
                                             reason

                                             satellite-metaversion
                                             satellite-version
                                             ;; Override the cids in the version specifier, if desired.
                                             vpb-cid-dids-to-add
                                             vpb-cid-dids-to-remove

                                             aux-satellite-metaversion
                                             aux-satellite-version
                                             ;; Override the cids in the version specifier, if desired.
                                             aux-vpb-cid-dids-to-add
                                             aux-vpb-cid-dids-to-remove
                                             
                                             receiver
                                             )
  (debug-message 3 "Starting satellite transaction to ~a" reason)
  (when (null satellite-metaversion)
    (error "Required argument :METAVERSION not supplied."))
  (call-with-vm-transaction
   :repository repository
   :transaction-type transaction-type
   :user-id-specifier user-id-specifier
   :reason reason
   :change-set-type 'minor-change-set

   :metaversion satellite-metaversion
   :version-specifier satellite-version
   :vpb-cid-dids-to-add vpb-cid-dids-to-add
   :vpb-cid-dids-to-remove vpb-cid-dids-to-remove

   :aux-metaversion aux-satellite-metaversion
   :aux-version-specifier aux-satellite-version
   :aux-vpb-cid-dids-to-add aux-vpb-cid-dids-to-add
   :aux-vpb-cid-dids-to-remove aux-vpb-cid-dids-to-remove

   :receiver (lambda (vm-transaction)
               (funcall receiver 
                        (make-instance 'cm-satellite-transaction
                                       :underlying-transaction vm-transaction)))))


(defun call-with-workspace-transaction (&key reason repository receiver transaction-type user-id-specifier)
  (debug-message 3 "Starting workspace transaction to ~a" reason)
  (call-with-repository-transaction
   :meta-cid-set-specifier :latest-metaversion   ;; workspace doesn't do metaversions
   ;; Don't specify version because this should be a non-versioned transaction.
   :reason            reason
   :repository        repository
   :transaction-type  transaction-type
   :user-id-specifier user-id-specifier
   :receiver  (lambda (transaction)
                (funcall receiver 
                         (make-instance 'cm-workspace-transation
                                        :underlying-transaction transaction)))))

#||
(defvar *cm-txn-satellite-user* :unbound
  "A user specification for transactions on satellite repositories which is established
   by calls to WITH-CM-MASTER-TXN.")

(eval-when (:load-toplevel :execute)
  (when (and (boundp '*cm-txn-satellite-user*) (eq *cm-txn-satellite-user* :unbound))
    (makunbound '*cm-txn-satellite-user*)))

(defvar *cm-txn-change-sets* :unbound
  "This special variable is used to accumulate string-did references to satellite repository
     change-set objects (not cids, not cid-objects, change-set object string dids).
     Upon completion of the master transaction, we'll bundle up this information in an entry
     in the master catalog.  This information is maintained by the master catalog, which acts
     as a subscriber to all change transactions.")

(eval-when (:load-toplevel :execute)
  (when (and (boundp '*cm-txn-change-sets*) (eq *cm-txn-change-sets* :unbound))
    (makunbound '*cm-txn-change-sets*)))

(defvar *cm-txn-master-cset-close-hooks* nil
  "This special variable is used to accumulate a list of functions which are invoked for update transactions
   AFTER the master catalog has updated it's generic information (like the information gathered
   in *cm-txn-change-sets*, and before final transaction close on the master repository.
   Each function is called with two arguments, the master catalog, and the change-set which
   is being created.

   The hooks then have the opportunity to update other information, typically related to
   decoration and use of the resulting change-sets from the transaction.

   NOTE: because of the current structure of WITH-VM-TXN, you should not call TXN-CONTEXT-ABORT-IF-NO-CHANGE
   from within hooks.

   HOOKS MUST NOT INITIATE REPOSITORY TRANSACTIONS OF ANY KIND.  They may only update information
   in the master repository, not any workspace repositories, satellite repositories, etc.")

(defmacro with-cm-master-repository ((repository-var repository-name)
                                     &body body)
  "Open a master repository named by REPOSITORY-NAME, which must be compatible with REPOSITORY-OPEN
   and must be a of repository-type :MASTER.  Bind the resulting repository to REPOSITORY-VAR,
   and execute BODY as an implicit PROGN in the bound scope of the repository.

   Unlike WITH-OPEN-REPOSITORY, the master repository named by REPOSITORY-NAME must already exist.

   Opening the master repository with this macro may or may not open satellite repositories.
   This is left to the implementation and is based on cache management and other efficiency strategies.
   The key thing is that the user never has to explicitly open satellite repositories, only the master.
   To manipulate satellite repositories, use the WITH-SATELLITE-REPOSITORY macro, which will
   open the satellite if necessary, and which uses known search keys for locating the satellite
   repository."
  `(with-open-repository (,repository-var ,repository-name
                          (conman-repository-open-mode-from-operating-mode)
                          :repository-type   :master
                          :error-if-not-cached t
                          :if-exists         :open
                          :if-does-not-exist :error)
     ,@body))

(defmacro with-cm-satellite-repository ((satellite-repository-var satellite-repository-path)
                                        &body body)
  "Like WITH-OPEN-REPOSITORY, however it is done only in the context of a master repository.

   SATELLITE-REPOSITORY-PATH is the namestring or pathname compatible with REPOSITORY-OPEN.
   REPOSITORY-OPEN-ARGS are all the other keywords supported by REPOSITORY-OPEN, refer to that function
   for details."
  `(with-open-repository (,satellite-repository-var ,satellite-repository-path
                          (conman-repository-open-mode-from-operating-mode)
                          :repository-type :satellite
                          :error-if-not-cached t
                          :if-exists         :open
                          :if-does-not-exist :error)
     ,@body))

(defvar *cm-txn-master-repository-txn-context* :unbound
  "This variable is bound to a txn-context object which applies to the master repository.
   It is occasionally useful to access this information when it has been rebound for satellite repository
   access.")

(eval-when (:load-toplevel :execute)
  (when (and (boundp '*cm-txn-master-repository-txn-context*)
             (eq *cm-txn-master-repository-txn-context* :unbound))
    (makunbound '*cm-txn-master-repository-txn-context*)))

(defvar *cm-txn-master-repository-latest-cid-set* :unbound
  "This variable is bound to the latest master cid-set which is present upon initiating a transaction
   in the master repository. It is convient and efficient to cache this data for viewing
   various elements under the master cid-set.

   This variable isn't exported, it is generally used by invoking the WITH-CM-MASTER-METAVERSION
   macro.

   **** WARNING **** do not alter this cid-set or re-bind this variable!  If you wish to use
   cid-sets which aren't the latest master cid-set, derive a new cid-set and use
   WITH-CM-MASTER-METAVERSION.")

(eval-when (:load-toplevel :execute)
  (when (and (boundp '*cm-txn-master-repository-latest-cid-set*)
             (eq *cm-txn-master-repository-latest-cid-set* :unbound))
    (makunbound '*cm-txn-master-repository-latest-cid-set*)))

(defun cm-txn-latest-master-metaversion-p ()
  "Return true if metaversion is the latest master metaversion, NIL otherwise."
  (eq (txn-context-cid-set *txn-context*) *cm-txn-master-repository-latest-cid-set*))

(defmacro with-cm-master-metaversion ((&optional metaversion-qualifier) &body body)
  "Bind *TXN-CONTEXT* to a transaction context known to be allocated for the master repository,
   and bind to it a meta-version which is appropriate given the argument to this macro.
   This context is active for the execution of BODY which is executed as an implicit PROGN
   and whose values are returned by this macro.

   Note that a with-cm-master-txn is also the equivalent of a (with-cm-master-metaversion () ...).

   If metaversion-qualifier is NIL, we will use the latest metaversion cid-set for the master repository.
   If metaversion-qualifier is a CID-SET, we will use that cid-set as the metaversion.
   If metaversion-qualifier is a TIME-STAMP, we will compute a metaversion which includes all change-sets
   up to but not exceeding the indicated time-stamp.

   **NOTE** this macro may be used only within the dynamic scope of a WITH-CM-MASTER-TXN call.

   Another note: the parameter conventions for this macro are distasteful, see code comments."
  ;; I don't usually like arguments whose type isn't clear from the name of the parameter,
  ;; but in this case it makes the code look nicer and was backward compatible (by overloading
  ;; the range of types permissible for the original single parameter).
  `(let ((*txn-context* *cm-txn-master-repository-txn-context*))
     (with-txn-context-cid-set (*txn-context*
                                (with-cm-metaversion-aux ,metaversion-qualifier
                                  *cm-txn-master-repository-txn-context*
                                  *cm-txn-master-repository-latest-cid-set*
                                  :master))
       ,@body)))

(defun with-cm-metaversion-aux (metaversion-qualifier txn-context latest-metaversion repository-type)
  "This is a private helper routine for the WITH-CM-{MASTER,SATELLITE}-METAVERSION macros.

   METAVERSION-QUALIFIER is as follows:

   If metaversion-qualifier is NIL, we will use the latest metaversion cid-set for the
   repository associated with TXN-CONTEXT.

   If metaversion-qualifier is a CID-SET, we will use that cid-set as the metaversion.

   If metaversion-qualifier is a TIME-STAMP, we will compute a metaversion which includes all change-sets
   up to but not exceeding the indicated time-stamp.

   Note that T is NOT a legal metaversion-qualifier value.  If the code is ever changed to
   allow T as a legal value, fix the default value of the metaversion-qualifier slot of a
   txn-context object (and every place that checks for a value of T).

   LATEST-METAVERSION is a cid-set to be used as the latest metaversion in the repository associated
   with TXN-CONTEXT.  This is an optimization to avoid repeated recalculation of the 'latest' metaversion.

   TXN-CONTEXT is the appropriate master or satellite repository transaction context against
   which the resulting metaversion will be derived and subsequently applied.

   REPOSITORY-TYPE typically a keyword indicator of whether we're setting the master or satellite metaversion
   for debugging purposes.

   Return the metaversion cid-set to be used."
  (setf (txn-context-metaversion-qualifier txn-context) metaversion-qualifier)
  (cond ((null metaversion-qualifier)
         (debug-message 3 "Setting ~a metaversion to latest." repository-type)
         latest-metaversion)

        ((typep metaversion-qualifier 'cid-set)
         (debug-message 3 "Setting ~a metaversion to cid-set ~s" repository-type metaversion-qualifier)
         metaversion-qualifier)

        ((time-stamp? metaversion-qualifier)
         (debug-message 3 "Setting ~a metaversion to time-stamp ~s (~a)"
                        repository-type
                        metaversion-qualifier
                        (universal-time->iso-date-time-string
                         (time-stamp-as-universal-time metaversion-qualifier)))
         (repository-master-cid-set (txn-context-repository txn-context)
                                    :end-time metaversion-qualifier
                                    :metaversion t))

        (t ;; must always be an error, see documentation above
         (error "~a metaversion qualifier supplied to with-cm-metaversion-aux is not valid. ~
                   Expected NIL, a CID-SET, or a TIME-STAMP, but received a ~s."
                repository-type
                (type-of metaversion-qualifier)))))

(defmacro with-cm-master-txn
    ((master-repository master-catalog-var cm-session-context txn-mode reason
      &key super-cset-name
           (super-cset-name-var (gensym (symbol-name :WITH-CM-MASTER-TXN-SUPER-CSET-)))
           supplementary-change-plist)
     &body body)
  "With a few exceptions, all ConMan transactions should use this macro to access master
   repository contents.  WITH-CM-MASTER-TXN should span all satellite operations, whose
   transaction scopes should in turn be managed via WITH-CM-SATELLITE-TXN.

   Note that with-cm-master-txn is also the equivalent of a (with-cm-master-metaversion () ...).

   This macro is the CONMAN package subtype equivalent of
   WITH-VM-TXN which is defined in the VM package, and which is ultimately used by this macro.

   MASTER-REPOSITORY should be bound to a repository-object as by WITH-MASTER-REPOSITORY,
   it is ultimately passed to WITH-VM-TXN.

   MASTER-CATALOG-VAR is bound to the MASTER-CATALOG object in MASTER-REPOSITORY.

   CM-SESSION-CONTEXT should be bound to a CM-SESSION-CONTEXT object, and is used to obtain user-id
   information, and maybe other things.

   TXN-MODE should be one of :READ-WRITE or :READ-ONLY
   It is ultimately passed to WITH-VM-TXN.

   REASON must be a string describing the nature of the transaction.  It is also used as the
   change-set description in the master, if this is a R/W transaction.
   It is ultimately passed to WITH-VM-TXN.

   A txn-context-identifier argument is not required, it defaults to *TXN-CONTEXT*.

   SUPER-CSET-NAME is optional, but should probably be specified for read/write transactions,
   it names the change-set in the master which acts as the super-cset.

   SUPER-CSET-NAME-VAR is useful if you want to specify the SUPER-CSET-NAME but need to generate
   the name based on things done in the transaction, in which case you can't know it on transaction
   open.  If SUPER-CSET-NAME-var is bound to something other NIL when BODY is done,
   and SUPER-CSET-NAME is nil, then
   we use the value of SUPER-CSET-NAME-VAR instead of SUPER-CSET-NAME to decorate the change-set.

   SUPPLEMENTARY-CHANGE-PLIST is a (possibly empty) plist which is used to provide
   supplementary information attached to change-sets which are created for change transactions.

   This macro returns the value of BODY."
  ;; Strip keywords specific to this macro from those we'll pass to with-repository-txn
  (with-macro-variables (with-cm-master-txn-uid-var)
    `(let ((,with-cm-master-txn-uid-var nil)) ;*FINISH* - bind based on value in cm-session-context
       ,cm-session-context              ;*FINISH*; just a reference to eliminate warnings...
                                        ; use it to establish WITH-CM-MASTER-TXN-UID-VAR binding...
       ;; Bind *cm-txn-change-sets* for duration of master transaction, so it has a fresh (empty)
       ;; value for every master transaction.  Bind it outside with-vm-txn, since we're using
       ;; it in a grey area of code between BODY and stuff which is done after BODY by WITH-VM-TXN,
       ;; via a 'subscription' to the change-set.  Yucky side effect stuff.
       (let ((*cm-txn-change-sets* nil)
             (*cm-txn-master-cset-close-hooks* nil)
             ;; We bind these here, rather than in body of with-vm-txn, because we'd like available
             ;; for VM change transaction postlude hooks.  If we don't bind then here, then they'd
             ;; be nil in the postlude hooks.
             (*cm-txn-master-repository-txn-context* nil) ;set below
             (*cm-txn-master-repository-latest-cid-set* nil)) ;set below
         (DEBUG-MESSAGE 2 "Starting master transaction")
         (with-vm-txn (,master-repository ,with-cm-master-txn-uid-var ,txn-mode ,reason
                       :no-project t :cset-name ,super-cset-name
                       :cset-name-var ,super-cset-name-var
                       :cset-type :super-cset
                       :supplementary-change-plist ,supplementary-change-plist)
           (let ((,master-catalog-var (master-catalog-retrieve master-repository))
                 ;; *FINISH*: need to bind to a user spec valid in satellite repository
                 (*cm-txn-satellite-user* nil))
             ;; note that these set*s are covered by the let way above
             (setq *cm-txn-master-repository-txn-context* *txn-context*) ;*txn-context* bound by with-vm-txn
             (setf (txn-context-metaversion-qualifier *cm-txn-master-repository-txn-context*) nil) ;; assume latest
             (setq *cm-txn-master-repository-latest-cid-set* (txn-context-cid-set *txn-context*))
             ;; Don't really expect update txns with suppress-change-transaction in
             ;; cset-bearing repositories.
             (when (txn-for-update? ,txn-mode)
               ;; We'll gather up super-cset information and stash in master catalog.
               (vm-txn-subscribe-to-change-set ,master-catalog-var))
             (LOCALLY ,@body))
           ;; Beware, postlude logic occurs here via hooks.
           )))))

(defmacro with-cm-master-txn-NO-CSET
    ((master-repository master-catalog-var cm-session-context txn-mode reason)
     &body body)
  "This macro is for rare exceptions where WITH-CM-MASTER-TXN is not appropriate because
   the transaction is updating a non-versioned lock state, where no change-set should be created.
   This means that all slot *updates* to versioned objects must be non-versioned!

   Note that with-cm-master-txn-NO-CSET is also the equivalent of a (with-cm-master-metaversion () ...).

   Arguments are as for `with-cm-master-txn', though TXN-MODE must be :READ-WRITE,
   never read-only.  Read-only transactions can use `with-cm-master-txn'.

   This macro returns the value of BODY."
  ;; Strip keywords specific to this macro from those we'll pass to with-repository-txn
  (with-macro-variables (with-cm-master-txn-NO-CSET-uid-var with-cm-master-txn-NO-CSET-txn-mode-var)
    `(let ((,with-cm-master-txn-NO-CSET-uid-var nil) ;*FINISH* - bind based on value in cm-session-context
              (,with-cm-master-txn-NO-CSET-txn-mode-var ,txn-mode))
       ,cm-session-context              ;*FINISH*; just a reference to eliminate warnings...
                                        ; use it to establish UID-VAR binding...
       (assert (eq ,with-cm-master-txn-NO-CSET-txn-mode-var :read-write))
       ;; *CM-TXN-CHANGE-SETS* should remain empty during this transaction.
       (let ((*cm-txn-change-sets* nil)
             (*cm-txn-master-cset-close-hooks* nil))
         (DEBUG-MESSAGE 2 "Starting master transaction, NO-CSET")
         (with-vm-txn (,master-repository ,with-cm-master-txn-NO-CSET-uid-var
                       ,with-cm-master-txn-NO-CSET-txn-mode-var ,reason
                       :no-project t :suppress-change-transaction t)
           (let ((,master-catalog-var (master-catalog-retrieve master-repository))
                 ;; *FINISH*: need to bind to a user spec valid in satellite repository
                 (*cm-txn-satellite-user* nil)
                 (*cm-txn-master-repository-txn-context* *txn-context*) ;*txn-context* bound by with-vm-txn
                 (*cm-txn-master-repository-latest-cid-set*
                  (txn-context-cid-set *txn-context*)))
             (declare (ignorable ,master-catalog-var))
             ;; note that this set is covered by the let above
             (setf (txn-context-metaversion-qualifier *cm-txn-master-repository-txn-context*) nil) ;; assume latest
             ;; No subscriber hooks to cset, shouldn't be any csets
             (multiple-value-prog1
                 (locally ,@body)
               (assert (null *cm-txn-change-sets*))
               )))))))

(defvar *cm-txn-satellite-repository-txn-context* :unbound
  "This variable is bound to a txn-context object which applies to the satellite repository.
   It is occasionally useful to access this information when it has been rebound for master repository
   access.")

(eval-when (:load-toplevel :execute)
  (when (and (boundp '*cm-txn-satellite-repository-txn-context*)
             (eq *cm-txn-satellite-repository-txn-context* :unbound))
    (makunbound '*cm-txn-satellite-repository-txn-context*)))

(defvar *cm-txn-satellite-repository-latest-cid-set* :unbound
  "This variable is bound to the latest satellite repository master-cid-set (in the CORE package sense)
   which is present upon initiating a transaction in the satellite repository.
   It is convient and efficient to cache this data for viewing
   various elements under the master cid-set.

   This variable isn't exported, it is generally used by invoking the WITH-CM-SATELLITE-METAVERSION
   macro.

   **** WARNING **** do not alter this cid-set or re-bind this variable!  If you wish to use
   cid-sets which aren't the latest satellite cid-set, derive a new cid-set and use
   WITH-CM-SATELLITE-METAVERSION.")

(eval-when (:load-toplevel :execute)
  (when (and (boundp '*cm-txn-satellite-repository-latest-cid-set*)
             (eq *cm-txn-satellite-repository-latest-cid-set* :unbound))
    (makunbound '*cm-txn-satellite-repository-latest-cid-set*)))

(defmacro with-cm-satellite-metaversion ((&optional metaversion-qualifier) &body body)
  "Bind *TXN-CONTEXT* to a transaction context known to be allocated for the satellite repository,
   and bind to it a meta-version which is appropriate given the argument to this macro.
   This context is active for the execution of BODY which is executed as an implicit PROGN
   and whose values are returned by this macro.

   Note that with-cm-satellite-txn is also the equivalent of a (with-cm-satellite-metaversion () ...).

   If metaversion-qualifier is NIL, we will use the latest metaversion cid-set for the satellite repository.
   If metaversion-qualifier is a CID-SET, we will use that cid-set as the metaversion.
   If metaversion-qualifier is a TIME-STAMP, we will compute a metaversion which includes all change-sets
   up to but not exceeding the indicated time-stamp.

   **NOTE** this macro may be used only within the dynamic scope of a WITH-CM-SATELLITE-TXN call."
  `(let ((*txn-context* *cm-txn-satellite-repository-txn-context*))
     (with-txn-context-cid-set (*txn-context*
                                (with-cm-metaversion-aux ,metaversion-qualifier
                                  *cm-txn-satellite-repository-txn-context*
                                  *cm-txn-satellite-repository-latest-cid-set*
                                  :satellite))
       ,@body)))

(defmacro with-cm-satellite-txn ((satellite-repository master-catalog txn-mode reason
                                  &rest with-vm-txn-keys) &body body)
  "Similar to WITH-VM-TXN, however this is the preferred macro to use for all transactions on
   all satellite repositories, in order to ensure that we maintain relationship information
   pertaining to master csets and satellite csets.

   Note that with-cm-satellite-txn is also the equivalent of a (with-cm-satellite-metaversion () ...).

   It differs from WITH-VM-TXN in calling sequence in that it does not require
   passing a USER context, which is instead established by the required outer call to WITH-CM-MASTER-TXN,
   and used here implicitly, and in that it requires a MASTER-CATALOG object.

   SATELLITE-REPOSITORY is any satellite repository object, as established via WITH-OPEN-REPOSITORY.
   TXN-MODE is as for WITH-VM-TXN, and should be either :READ-WRITE or :READ-ONLY.
   REASON is as for WITH-VM-TXN, and must be a string.

   All keywords which are valid for WITH-VM-TXN may be specified in this macro as well.

   BODY is executed as per WITH-VM-TXN, and it's return value is compatible as well."
  `(PROGN
     (DEBUG-MESSAGE 2 "Starting satellite transaction.")
     (with-vm-txn (,satellite-repository *cm-txn-satellite-user* ,txn-mode ,reason ,@with-vm-txn-keys)
       (let ((*cm-txn-satellite-repository-txn-context* *txn-context*) ;*txn-context* bound by with-vm-txn
             (*cm-txn-satellite-repository-latest-cid-set*
              (txn-context-cid-set *txn-context*)))
         (setf (txn-context-metaversion-qualifier *cm-txn-satellite-repository-txn-context*) nil) ;; assume latest
         ;; Don't really expect update txns with suppress-change-transaction in cset-bearing repositories.
         (when (txn-for-update? ,txn-mode)
           (vm-txn-subscribe-to-change-set ,master-catalog))
         (LOCALLY ,@body)))))

(defun cm-txn-start-timestamp (&optional (txn-context *txn-context*))
  "Returns the start TIME-STAMP of the specified transaction or for the current
  transaction.  It is an error to call this then no transaction is in progredd."
  (check-type txn-context txn-context)
  (core::cid-master-table-entry-when-start
   (core::txn-context-cid-master-table-entry txn-context)))

||#

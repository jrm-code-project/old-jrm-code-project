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
  (export '(csf/config::*enable-repository-caching*
            csf/config::*repository-cache-limit*
            csf/config::*repository-cache-timeout*)
          "CSF/CONFIG")
  (export '(
            *SUPPRESS-CACHED-REPOSITORY-REOPEN-WARNINGS*

            +repository/basic-file-type+
            +repository/master-file-type+
            +repository/satellite-file-type+
            +repository/workspace-file-type+

            call-with-repository-transaction
            repository/add-locally-named-root
            repository/add-satellite-repository
            repository/cid-master-table
            repository/cid-information
            repository/cid-versioned-change-information
            repository/locally-named-root
            repository/master-cid-set
            repository/name
            repository/resolve-distributed-identifier
            repository/satellite-repositories
            repository/type

            with-cached-open-repositories
            with-open-repository        ;open/create a repository, preferred over repository-{open,create}
            ;; with-repository-txn              ;preferred over repository-begin-txn
            with-current-repository     ;make *repository* current allocation repository
            ;; *repository*             ;'current' repository


            call-with-new-change
            )))

(defconstant *repository-types* '(:basic :master :satellite :transport :extent :workspace)
  "Type of repositories.  Note that all but :EXTENT types of repositories
   serve as root extents for databases which have multiple extents, and therefore imply extent.")

(deftype repository-type () `(member ,@*repository-types*))

;;; For some reason, extensions starting with Y are very unpopular.

(defconstant +repository/basic-file-type+     "ydb"
  "File type for basic repositories, for the initial extent.")
(defconstant +repository/master-file-type+    "ydm"
  "File type for master repositories, for the initial extent.")
(defconstant +repository/satellite-file-type+ "yds"
  "File type for satellite repositories, for the initial extent.")
(defconstant +repository/transport-file-type+ "ydt"
  "File type for transport packages, for the initial extent.")
(defconstant +repository/extent-file-type+    "ydx"
  "File type for repository extent databases, used for all non-initial extents.")
(defconstant +repository/workspace-file-type+ "ydw"
  "File type for workspace repositories, for the initial extent.")

(defun repository-type-keyword->string-extension (type)
  "For a keyword found in *repository-types*, return the appropriate string extension
   to be used as a pathname file type component.  E.g. :EXTENT -> \"TDB\""
  (ecase type
    (:basic     +repository/basic-file-type+)
    (:extent    +repository/extent-file-type+)
    (:master    +repository/master-file-type+)
    (:satellite +repository/satellite-file-type+)
    (:transport +repository/transport-file-type+)
    (:workspace +repository/workspace-file-type+)))

(defvar *repository-caching-debug* nil
  "Bind to a stream if you want diagnostics about what happens when WITH-CACHED-OPEN-REPOSITORIES
   is in effect.")

(defmacro repository-cache-debug (format-string &rest args)
  "Print a diagnostic about open repository caching, if diagnostics are enabled."
  `(LET ((FORMAT-STRING ,format-string)
         (FORMAT-ARGS (LIST ,@args)))
    (WHEN *REPOSITORY-CACHING-DEBUG*
      (FORMAT *REPOSITORY-CACHING-DEBUG* "~&~?" FORMAT-STRING FORMAT-ARGS))
    (DEBUG-MESSAGE 4 "Repository cache:  ~?" FORMAT-STRING FORMAT-ARGS)))

(defvar *repository-schema-upgrade-string-out* nil
  "ptr to function to print a string when schema upgrade is in progress, nil at all other times")

(defvar *repository-schema-upgrade-in-progress* nil
  "T when a repository schema upgrade is in progress, nil at all other times")

(defvar *repository-schema-upgrade-had-class-schema-change* nil
  "Did our open have a class schema change (detected by Franz).  Only valid for the 'last'
   repository-open.")

(defclass repository ()
  (
  ;; The NAME, DOMAIN, slots describe the distributed identifier components
  ;; of the repository.  The REPOSITORY-PRETTY-NAME method provides a pretty interpretation of these
  ;; slots, including whether or not the repository is a transport repository.
  ;; The NAME component never has extent information, file type, or other pathname encoding beyond that
  ;; of the root prefix in the file name which excludes the extent information.
  ;; Note that current use of core services don't yet make use of domain information.
  ;; and that distributed identifier use of this information hasn't yet been tested.
  ;; These four values should be indelibly defined at repository creation time.
  ;; Most of these are transisent copies of the persistent data, nice to have to avoid requiring a
  ;; transaction when we want to do certain manipulations of repositories.
   (domain :initarg :domain
           :initform ""
           :reader repository/domain
           :type string)

   (name   :initarg :name
           :initform (error "Required initarg :name omitted.")
           :reader repository/name
           :type string)
   ;; ONCE ... this was the full pathname of the repository as it
   ;; exists relative to some server accessing
   ;; the repository.  It is used as the basis of repository caching too, and should fully distinguish
   ;; any two repositories relative to the active execution of a single server process.
   ;; THEN ... this is a 'dbpath' object OR a pathname object.
   ;; NOW ... this is a 'dbpath' object
   (dbpath :initarg :dbpath
            :initform (error "Required initarg :dbpath omitted.")
            :reader repository/dbpath
            :type dbpath)

   (type  :initarg :type
          :initform (error "Required initarg :type omitted.")
          :reader repository/type
          :type repository-type)

   (pstore :initarg :pstore
           :initform (error "Required initarg :pstore omitted.")
           :accessor repository/pstore
           :type persistent-store)

   ;;   (vfiles :initarg :vfiles
   ;;           :initform (error "Required initarg :vfiles omitted.")
   ;;           :accessor repository/vfiles)

   ;; This information is only present if certain repository information management calls are done
   ;; in a transaction.
   (persistent-data ;persistent repository information, valid only in an open transaction
    :accessor repository/persistent-data)

  ;; When pstore slot is non-nil, this indicates the repository open mode, and records the OPEN-MODE
  ;; argument to REPOSITORY-OPEN.
  (open-mode :initarg :open-mode
             :initform (error "Required initarg :open-mode omitted.")
             :reader repository/open-mode)

  ;; We support the notion of concurrent users of the repository object, and every user may have nested
  ;; transactions in progress.  UID-TXN-STACKS lists both the active users, and their txn-context stacks.
  ;; We don't expect too many users, so an ALIST is fine for now.  Read/write systems will most likely have
  ;; only one user at a time, but this isn't absolute!
  (uid-txn-stacks :initform nil
                  :accessor repository/uid-txn-stacks)
                                        ; ALIST: car=CORE-USER (UID), cdr=stack of txn-contexts for that UID.
                                        ; HOWEVER: treating CDR of ACONS as LIST is problematic,
                                        ; So: CAR=UID, remaining elts are TXN-CONTEXT, most-to-least recent.
                                        ; e.g. ((0 txn-context2 txn-context1) (1 txn-context1))

  ;; list of functions to call when the repository is finally closed
  ;; funcs are added by repository-open via the :file-close-hook key parameter
  ;; this value will be a procedure taking one arg
  ;; the first arg to the function will be this repository object and will be supplied
  ;; the functions will be called in a lifo manner from the list
  (file-close-hooks :initform nil
                    :accessor repository/file-close-hooks)

  );; slots end

  (:documentation
   "Object which coordinates all activity in a given logical repository.  This includes all
    transaction management, coordination of multiple underlying physical databases, etc.."))

(defmethod print-object ((rep repository) stream)
  (print-unreadable-object (rep stream :type t)
  (format stream ;; "~@<#<~;~W~2I ~_~/pprint-fill/~;>~:>"
          "~s ~s ~s"
          (list
           (repository/domain rep)
           (repository/name rep)
           ;; (repository-dbpath-host (repository/dbpath rep))
           (dbpath/namestring (repository/dbpath rep))))))

(defun repository/txn-stack-for-uid-spec (repository uid-spec)
  (assert (not (stringp uid-spec)))
  (cdr (assoc uid-spec (repository/uid-txn-stacks repository))))

(defun repository/innermost-txn-for-uid-spec (repository uid-spec)
  "Return the innermost (current) transaction in progress for UID-spec.
   UID-spec must be NIL or a CORE-USER DID."
  (first (repository/txn-stack-for-uid-spec repository uid-spec)))

(defun repository/outermost-txn-context-for-uid-spec (repository uid-spec)
  "Return the TXN-CONTEXT stack if one exists for UID, or NIL if UID has no active transactions.
   UID must be a CORE-USER distributed-identifier (DID), or NIL."
  (car (last (repository/txn-stack-for-uid-spec repository uid-spec))))

(defun repository/add-transaction (repository transaction)
  "Add the txn-context to the UID-indexed stack of them.  The UID is obtained from the TXN-CONTEXT"
  ;; We could potentially check for *uid-unassigned* here, however it's possible that we
  ;; may want to let the server manage certain unowned transactions with this id (or we need to create
  ;; as separate server id).
  (let ((uid (repository-transaction/uid transaction)))
    (cond ((assoc uid (repository/uid-txn-stacks repository))
           => (lambda (probe) (push transaction (cdr probe))))
          (t (push (cons uid (list transaction)) (repository/uid-txn-stacks repository)))))
  transaction)

(defun repository/remove-transaction (repository transaction)
  "Remove the txn-context from the UID-index stack of them.  The UID is obtained from the TXN-CONTEXT."
  (let ((uid (repository-transaction/uid transaction)))
    (cond ((assoc uid (repository/uid-txn-stacks repository))
           => (lambda (probe)
                ;; We've found the UID-specific txn-context stack, make sure TXN-CONTEXT is in it and remove it
                ;; For now, we don't do an exhaustive search, we assume that Txn's are removed LIFO, and that
                ;; the txn-context must be first in the stack.
                (unless (eq (second probe) transaction) ;not a real alist... cdr is a
                  (error 'changesafe-database-error
                         :format-control "Attempt to remove txn-context in non-LIFO order, ~
                                          or invalid txn-context specification.~%~
                                          TXN-CONTEXT: ~s"
                         :format-arguments (list transaction)))
                (pop (cdr probe)) ;remove second element
                ;; If we removed the last txn-context for a UID, excise the UID from the active stacks
                (unless (cdr probe)
                  (deletef probe (repository/uid-txn-stacks repository)))))
          (t
           ;; We didn't find the UID-specific TXN-CONTEXT stack, something is definitely amiss...
           (error 'changesafe-database-error
                  :format-control "Unable to locate UID ~s in repository transaction stacks for ~
                                   REPOSITORY-REMOVE-TXN-CONTEXT.~%TXN-CONTEXT: ~s"
                  :format-arguments (list (repository-transaction/uid transaction) transaction)))))
  transaction)

(defclass repository-persistent-information ()
  (
   (type  :initarg :type
          :initform (error "Required initarg :type omitted.")
          :reader repository-persistent-information/type
          :type repository-type)

   ;; Database parent is the root extent for an extent database, or the master database for a satellite.
   ;; Root extents or master repositories won't have a parent
   (parent-repository :initarg :parent-repository
                      :initform nil
                      :reader repository-persistent-information/parent
                      :type (optional relative-pathname))

   ;; Satellite repositories is non-nil only for master repositories.
   (satellite-repositories :initform nil
                           :initarg :satellite-repositories
                           :accessor repository-persistent-information/satellite-repositories)

   (canonical-class-dictionary :initform (make-instance 'canonical-class-dictionary)
                               :reader repository-persistent-information/canonical-class-dictionary)
   (cid-master-table :initform (make-instance 'cid-master-table)
                     :reader repository-persistent-information/cid-master-table)
   (root-mapper  :initarg :root-mapper
                 :initform (error "Required initarg :root-mapper omitted.")
                 :reader repository-persistent-information/root-mapper)
   (cid-mapper   :initarg :cid-mapper
                 :initform (error "Required initarg :cid-mapper omitted.")
                 :reader repository-persistent-information/cid-mapper)
   (local-mapper :initarg :local-mapper
                 :initform (error "Required initarg :local-mapper omitted.")
                 :reader repository-persistent-information/local-mapper)
   (locally-named-roots :initarg :locally-named-roots
                        :initform (error "Required initarg :locally-named-roots omitted.")
                        :reader repository-persistent-information/locally-named-roots)
   (anonymous-user :initarg :anonymous-user
                   :initform nil
                   :reader repository-persistent-information/anonymous-user))
  (:default-initargs :node-id +object-id-of-root+)  ;; force this to always be the root object.
  (:documentation "Persistent information describing a repositiory, and stored in the repository")
  (:metaclass persistent-standard-class)
  (:schema-version 0))

;;;
;;; How to get at the persistent data
;;; from the transient repository handle.
;;;

(defun call-with-repository-bound-persistent-data (pstore-transaction repository thunk)
  "Establish the slot binding for persistent data in the repository."
  (unwind-protect
      (progn (setf (repository/persistent-data repository)
                   (transaction/find-root pstore-transaction (repository/pstore repository)))

             (unless (eq (repository/type repository)
                         (repository/persistent-type repository))
               (error 'changesafe-database-error
                      :format-control "Repository type mismatch."))

             (funcall thunk))
    "Removing persistent data"
    (slot-makunbound repository 'persistent-data)))

(defun repository/satellite-repositories (repository)
  "Return the list of satellite repositories for this repository."
  (repository-persistent-information/satellite-repositories
   (repository/persistent-data repository)))

(defun repository/add-satellite-repository (repository satellite)
  "Add an element to the list of satellite repositories for this repository."
  (let ((pdata (repository/persistent-data repository)))

    (remake-instance pdata
                     :satellite-repositories (cons satellite
                                                   (repository-persistent-information/satellite-repositories pdata)))))

(defun repository/canonical-class-dictionary (repository)
  "Return the canonical-class-dictionary for this repository."
  (repository-persistent-information/canonical-class-dictionary
   (repository/persistent-data repository)))

(defun repository/root-mapper (repository)
  "Return the ROOT-MAPPER object for this repository."
  (repository-persistent-information/root-mapper
   (repository/persistent-data repository)))

(defun repository/cid-mapper (repository)
  "Return the CID-MAPPER object for this repository."
  (repository-persistent-information/cid-mapper
   (repository/persistent-data repository)))

(defun repository/cid-master-table (repository)
  "Retrieve persistent CID master table"
  (repository-persistent-information/cid-master-table
   (repository/persistent-data repository)))

(defun repository/local-mapper (repository)
  "Return the portion of the mapper hierarchy which represents this repository.
   We typically arrange that we can find this quickly."
  (repository-persistent-information/local-mapper
   (repository/persistent-data repository)))

(defun repository/locally-named-roots (repository)
  (repository-persistent-information/locally-named-roots
   (repository/persistent-data repository)))

(defun repository/anonymous-user (repository)
  (repository-persistent-information/anonymous-user
   (repository/persistent-data repository)))

(defun repository/parent (repository)
  (repository-persistent-information/parent
   (repository/persistent-data repository)))

(defun repository/persistent-type (repository)
  (repository-persistent-information/type
   (repository/persistent-data repository)))

(defun repository/cid-distributed-identifier (repository cid)
  "Return a DISTRIBUTED-IDENTIFIER which describes CID in fully qualified form.
   CID is interpreted as local to the indicated REPOSITORY."
  (integer-mapper/distributed-identifier (repository/cid-mapper repository) cid))

(defmethod repository/locally-named-root ((repository repository) (name symbol))
  "Return the persistent object associated with name, or NIL if no such object exists.
   NAME names a LOCAL name, and the returned handle may or not also be that of
   a DISTRIBUTED-OBJECT with a DISTRIBUTED-IDENTIFIER.  I.e., locally named objects
   aren't necessarily distributed named objects."
  (persistent-hash-table/gethash
   (repository/locally-named-roots repository)
   name))

(defmethod repository/add-locally-named-root ((repository repository) object (name symbol)
                                              &key (if-exists :error))
  "Add a named object to the persistent repository named roots, and return the object.
   If the name already has a binding, action is determined by IF-EXISTS.
   IF-EXISTS, if :ERROR, will cause an error to be signalled if the name already exists.
   IF-EXISTS, if :SUPERSEDE, will quietly give the name a new value."
  (let ((previous-binding (repository/locally-named-root repository name)))
    (when (and previous-binding
               (eq if-exists :error))
      (error 'changesafe-database-error
             :format-control  "Attempt to add a named repository root for ~s, when one already exists."
             :format-arguments (list name)))
    (setf (persistent-hash-table/gethash (repository/locally-named-roots repository) name) object)))

(defmacro with-current-repository ((repository) &rest body)
  "Execute BODY with the 'current' repository (for allocations, etc..)
   bound to the indicated repository.  Use of this macro is required."
  `(LET ((*REPOSITORY* ,repository))
     ,@body))

(defmacro with-open-repository ((repository-var &rest repository-open-args) &body body)
  "Open a repository with the given identifier.  Bind REPOSITORY-VAR to an instance
   of the repository.  Note that the bound repositiory object has both transient and
   persistent components, and may be used outside of transactions as well as in-transactions.
   All arguments following REPOSITORY-VAR are those used by REPOSITORY-OPEN.
   WITH-OPEN-REPOSITORY makes the newly opened repository 'current'."
  ;; We bind allegrostore::*assume-superclasses-unchanged* incase repository-open logic
  ;; wants to perform transactions and doesn't use WITH-REPOSITORY-TXN.
  (with-unique-names (with-open-repository-body)
    `(FLET ((,with-open-repository-body (,repository-var) ,@body))
       ;;(DECLARE (DYNAMIC-EXTENT #',with-open-repository-body))
       (with-open-repository-1 #',with-open-repository-body ,@repository-open-args))))

(defparameter +repository-reconnect-max-retries+ 3
  "This is the maximum number of retries that WITH-OPEN-REPOSITORY-1 should
   attempt before giving up.")

(defun with-open-repository-1 (body-continuation repository-dbpath open-mode
                               &rest repository-open-keyargs)
  "Conditionally bind the RETRY-OUTERMOST-REPOSITORY-OPEN restart if this is the
   outermost invocation of WITH-OPEN-REPOSITORY-1.

   Invoking the retsart should retry."
  (check-type repository-dbpath dbpath)
  (debug-message 5 "with-open-repository-1 ~s" repository-dbpath)
  (let ((failed-attempts 0)
        (max-retries +repository-reconnect-max-retries+))
    (flet ((do-it ()
             (apply #'with-open-repository-2
                    body-continuation repository-dbpath open-mode
                    repository-open-keyargs))
           (restart-test (condition)
             ;; The restart is disabled if we've exceeded the maximum
             ;; number of retries allowed.
             (declare (ignore condition))
             (<= failed-attempts max-retries)))
      (if (find-restart 'retry-outermost-repository-open)
          (do-it)
        ;; We only establish the restart handler if this is the
        ;; outermost invocation.
        (loop
          (restart-case
              (return (do-it))
            (retry-outermost-repository-open (&rest ignore)
                :test restart-test
              (declare (ignore ignore))
              (format *error-output* "~&WITH-OPEN-REPOSITORY-1: attempt ~d failed for ~s.~%"
                      failed-attempts repository-dbpath)
              (incf failed-attempts))))))))

(defun with-open-repository-2 (body-continuation repository-dbpath open-mode
                                                 &rest repository-open-keyargs)
  (debug-message 5 "with-open-repository-2 ~s" repository-dbpath)
  (let ((repository nil)
        (repository-open-type nil)
        (opened-ok nil)
        (ran-ok nil))
    (unwind-protect
        (progn
          (apply #'open-repository repository-dbpath open-mode 
                 (lambda (repository-value repository-open-type-value)
                   (setq repository           repository-value
                         repository-open-type repository-open-type-value
                         opened-ok            t))
                 repository-open-keyargs)
          ;; do NOT execute the body code when we had an unexpected schema upgrade
          ;; We have to treat the unexpected schema upgrade as 'normal'
          ;; repository-open because of a Franz bug (no spr yet) (CII 001117-0000)
          ;; Franz spr 22913 & spr 22998 & spr23035
          (assert (not (eq repository-open-type :file-schema-upgrade-failed)))
          (multiple-value-prog1 ;; this is too ensure that we return the multi-values from body
              (progn
                (with-current-repository (repository)
                  (funcall body-continuation repository)))
              (setq ran-ok t)))
      "closing repository & clean up"
      (when repository
        (repository-close repository
                          :force (if (or (not opened-ok)
                                         (and (not ran-ok)
                                              (or (eq repository-open-type :file-opened)
                                                  (eq repository-open-type :file-closed-and-opened)))
                                         ;; schema-upgrade is always an immediate close
                                         (eq repository-open-type :file-schema-upgrade-failed)
                                         (eq repository-open-type :file-schema-upgrade))
                                     t nil)))
      ;; We have to treat the unexpected schema upgrade as 'normal'
      ;; repository-open because of a Franz bug (no spr yet) (CII 001117-0000)
      ;; Franz spr 22913 & spr 22998 & spr23035
      (assert (not (eq repository-open-type :file-schema-upgrade-failed)))
      )))

;;(defun repository-pathname->vfile-log-directory-pathname (repository-pathname)
;;  (check-type repository-pathname pathname)
;;  (merge-pathnames (make-pathname
;;                    :directory `(:relative ,(concatenate 'string (pathname-name repository-pathname) "-logs"))
;;                    :defaults "")
;;                   (make-pathname
;;                    :name nil
;;                    :type nil
;;                    :defaults repository-pathname)))

(defun repository-pstore-open-database (domain repository-dbpath repository-type
                                               parent-repository-dbpath open-mode
                                               receiver)
  (check-type repository-dbpath dbpath)
  (check-type parent-repository-dbpath (optional dbpath))

  (let* ((pathname (dbpath/pathname repository-dbpath))
         ;; (vfile-log-directory-pathname (repository-pathname->vfile-log-directory-pathname pathname))
         (parent-pathname (and parent-repository-dbpath (dbpath/pathname parent-repository-dbpath)))
         (parent-relative-pathname (and parent-pathname (enough-pathname parent-pathname pathname)))
         (name-for-user (pathname-name pathname)))

    (multiple-value-bind (store store-id create?)
        (persistent-store/open pathname :mode (ecase open-mode
                                                ((:readonly :mvcc) :read-only)
                                                ((:update) :read-write)))
      (declare (ignore store-id))
      (if (not create?)
          ;; At this point, we will be creating a transient repository object.
          ;; This must be created and received in a deferred context so we
          ;; don't lose it.
          (call-with-deferred-interrupts 
           "Instantiating transient repository"
           (lambda (restore)
             (declare (ignore restore))
             (funcall receiver
                      (make-instance 'repository
                                     :type      repository-type
                                     :domain    (or domain "")
                                     :name      name-for-user
                                     :dbpath    repository-dbpath
                                     :pstore    store
                                     ;; :vfiles    (open-vfiles vfile-log-directory-pathname open-mode)
                                     :open-mode open-mode))))
          (let* (
                 persistent-information

                 (repository
                  (call-with-transaction
                   store :read-write (format nil "Create repository ~a" (pathname-name pathname))
                   (lambda (transaction)
                     (declare (ignore transaction))
                     (multiple-value-bind (root-mapper local-mapper)
                         (distributed-identifier-create-mapper-hierarchy
                          (make-distributed-identifier
                           :domain (or domain "")
                           :repository name-for-user))

                       (setq persistent-information
                             (progn (debug-message 1 "allocating repository persistent info for ~s" store)
                                    (make-instance 'repository-persistent-information
                                                   :type repository-type
                                                   :repository-type repository-type
                                                   :parent-repository parent-relative-pathname
                                                   :root-mapper  root-mapper
                                                   :local-mapper local-mapper
                                                   :cid-mapper (make-instance 'integer-range-mapper
                                                                              :mapping-level "Pseudo-class CID"
                                                                              :pseudo-class :cid
                                                                              :repository-mapper local-mapper)
                                                   :locally-named-roots (make-instance 'persistent-hash-table
                                                                                       :size 5)))))
                     (call-with-deferred-interrupts 
                      "Instantiating transient repository"
                      (lambda (restore)
                        (declare (ignore restore))
                        (let ((transient
                               (make-instance 'repository
                                              :type      repository-type
                                              :domain    (or domain "")
                                              :name      name-for-user
                                              :dbpath    repository-dbpath
                                              :pstore    store
                                              ;; :vfiles    (create-vfiles vfile-log-directory-pathname open-mode)
                                              :open-mode open-mode)))
                          (funcall receiver transient)
                          transient)))))))

            (with-current-repository (repository)
              (call-with-repository-transaction
               :repository repository
               :user-id-specifier :nobody
               :meta-cid-set-specifier :latest-metaversion
               :cid-set-specifier :latest-version
               :transaction-type :read-write
               :reason (format nil "Initialize repository ~a" (car (last (pathname-directory pathname))))
               :receiver (lambda (repository-txn)
                           (declare (ignore repository-txn))
                           (remake-instance persistent-information
                                            :anonymous-user (make-instance 'core-user :name "Nemo"))
                           nil)))

            repository)) )))

(defun repository-open-readonly (domain repository-dbpath repository-type receiver)
  (repository-pstore-open-database
   domain repository-dbpath repository-type nil :readonly receiver))

(defun repository-open-update (domain
                               repository-dbpath
                               repository-type
                               receiver
                               parent-repository-dbpath
                               satellite-repository-dbpaths
                               #||
                               database-pathname
                               if-does-not-exist if-exists mode use warn channel
                               description transport-info
                               repository-type
                               ||#

                               )
  "Open a repository for update, creating a new repository if necessary.
   The resulting repository database first extent is created in the file named by DATABASE-PATHNAME,
   which must be a pathname.  The transient repository is created as with REPOSITORY-CREATE and is
   filled in during the course of opening/creating the repository.
   We ensure that the repository-persistent-information is initialized and present as appropriate.
   Note that the persistent actions require that this routine begin and end a transaction.

   Return value: The passed transient repository instance."
  (declare (ignore channel))
  ;; Validate some args
  (check-type repository-dbpath dbpath)
  (check-type repository-type repository-type)
  (check-type parent-repository-dbpath (optional dbpath))
  (when satellite-repository-dbpaths
    (assert (every #'dbpath? satellite-repository-dbpaths)))
  ;; Open the database for update.

  (repository-pstore-open-database domain repository-dbpath repository-type
                                   parent-repository-dbpath
                                   :update
                                   receiver)
#||
  ;; Find the persistent data if this is an existing database, create a new one if it is a new
  ;; database.  We actually use the persistent data find process to determine if this is
  ;; new or created.
  ;; Start a transaction, and read in a pointer to the persistent data portion
  (let ((created-p nil))
    (unwind-protect
        (allegrostore:with-current-database (repository-open-database transient-repository)
          (allegrostore:with-transaction ()
            (multiple-value-bind (repository-persistent-information repository-created-p)
                (repository-retrieve-persistent-information
                 transient-repository :error-if-missing (not (eq if-does-not-exist :create))
                 :description description :transport-info transport-info
                 :repository-type repository-type
                 :parent-database-pathstring parent-database-pathstring
                 :satellite-repository-pathstrings satellite-repository-pathstrings)
              (setq created-p repository-created-p)
              (repository-persistent-information-validate-versions repository-persistent-information
                                                                   :repository-dbpath database-pathname)
              (repository-copy-persistent-info transient-repository repository-persistent-information))
            (astore-flush-changes :verbose t
                                  :database (repository-open-database transient-repository))))
      (progn
        (setf (repository-persistent-data transient-repository) nil) ; not valid outside txn
    ))
    ;; Here's some chicken an egg stuff.  If this was the open to create a database,
    ;; start a first transaction here, which will create CID 1, and which assigns the
    ;; anonymous user for this repository.  Do not create an anonymous user (and associated CMTE)
    ;; for a transport repository.
    (when (and created-p (not transport-info))
      ;; A bit of magic copied from with-open-repository .... unfortunately
      (with-current-repository (transient-repository)
        (with-repository-txn (*txn-context* transient-repository nil :txn-mode :read-write
                              :reason "Initialize Repository")
          (repository-create-anonymous-user transient-repository)))))
||#
  )

(defvar *recently-opened-repositories* nil
  "A list of repositories opened in the current transaction.

   If the outermost transaction aborts, we must close these.")

(defun close-recently-opened-repositories ()
  (mapc (lambda (repository)
            (repository-close repository :force t))
        ;; Repository cache close will mutate this list,
        ;; so we snarf it first.
        (prog1 *recently-opened-repositories*
          (setq *recently-opened-repositories* nil))))

(defun call-with-recently-opened-repository-management (pstore-transaction thunk)
  "Invoke thunk such that recently opened repositories are closed
   upon abort."
  (unwind-protect
      (progn (setq *recently-opened-repositories* nil)
             (funcall thunk))
    "Close recently opened repositories."
    (when (eq (transaction/disposition pstore-transaction) :aborting)
      (close-recently-opened-repositories))))

(defvar *open-repositories* nil
  "When a repository is open, and in use, it is placed in this list.

   Each element of this list in a pair of (repository . open-count), where
   the open-count is the number of times the repository has been
   recursively opened.  (Recursive open is *not* officially supported,
   and shouldn't be used, but CONMAN made use of a loophole in the prior
   code that allowed this.)")

(defvar *suppress-cached-repository-reopen-warnings* nil)

(defun open-repository (repository-dbpath open-mode receiver
                                        &key (if-does-not-exist :error) (if-exists :open)
                                        (mode #o666) (use :ask) (warn t) channel description transport-info
                                        (repository-type (if transport-info :transport :basic))
                                        parent-repository satellite-repositories
                                        domain
                                        file-close-hook
                                        error-if-not-cached)

  ;; NOTE:  The reciver argument must be a procedure of two arguments.
  ;; It will be called in an interrupts-deferred context on the repository
  ;; and a keyword indicating how the repository was opened.
  ;; We write it this way to ensure that resources are not lost.

  (check-type repository-dbpath dbpath)
  (check-type repository-type repository-type)
  (debug-message 4 "open-repository ~s ~s" repository-dbpath open-mode)
  (cond
   ;; See if the requested repository is in the cache.
   ;; Right now we use the repository name.  Note that this could be ambiguous if we have
   ;; multiple repositories on the same host with the same name.  However this is also bad
   ;; for distributed identifiers, and we simply must not allow this to happen.

   ((find-if (lambda (entry)
               (objects-equalp (repository/dbpath (car entry))
                               repository-dbpath))
             *open-repositories*)
    => (lambda (already-open-entry)
         (unless (eq use :ask);; *repository-schema-upgrade-in-progress*
           (error 'changesafe-database-error
                  :format-control "The database (~a) may not be already open when doing a schema upgrade."
                  :format-arguments (list repository-dbpath)))

         (let ((repository (car already-open-entry)))
           (unless (and
                    ;; The repository is already opened, verify that the currently requested open mode
                    ;; matches the previously requested open-mode.
                    (eq (repository/open-mode repository) open-mode)

                    ;; If the mode is update, it's only compatible if
                    ;; we're supposed to OPEN the old database.  (as
                    ;; opposed to supersede or throw an error)
                    (not (and (eq open-mode :update)
                              (not (eq if-exists :open)))))
             ;; Problem.  We are recursively opening the repository, but the mode is wrong.
             (error 'changesafe-database-error
                    :format-control "Mode mismatch on recursive REPOSITORY-OPEN on repository ~s~"
                    :format-arguments (list repository)))

           (incf (cdr already-open-entry));; bump the recursive open count
           (repository-cache-debug "repository-open ALREADY for ~s"
                                   repository-dbpath)
           (when file-close-hook
             (push file-close-hook (repository/file-close-hooks repository)))
           (funcall receiver repository :from-open-list))))

   ((find-cached-repository repository-dbpath)
    => (lambda (cached-repository)
         (unless (eq use :ask);; *repository-schema-upgrade-in-progress*
           (error 'changesafe-database-error
                  :format-control "The database (~a) may not be already open when doing a schema upgrade."
                  :format-arguments (list repository-dbpath)))

         (if (and
              ;; The repository is already opened, verify that the currently requested open mode
              ;; matches the previously requested open-mode.
              (eq (repository/open-mode cached-repository) open-mode)
              ;; If the mode is update, it's only compatible if
              ;; we're supposed to OPEN the old database.  (as
              ;; opposed to supersede or throw an error)
              (not (and (eq open-mode :update)
                        (not (eq if-exists :open)))))
             (progn
               ;; The requested repository is already open in a compatible mode, return it,
               ;; don't open it again.
               (push (cons cached-repository 1) *open-repositories*);; start open count at 1.
               (repository-cache-debug "repository-open HIT for ~s" repository-dbpath)
               (when file-close-hook
                 (push file-close-hook (repository/file-close-hooks cached-repository)))
               (funcall receiver cached-repository :from-cache))

             (progn
               ;; otherwise, it's the wrong mode, throw it away.
               #| -- JDT: this requires some extensive testing and blessed build before introduction. ;
               ;; If the mode is readonly, consider readonly or update databases as compatible.
               ;; Prior to Franz getting read-only txn's right we didn't use to allow this.  Now we do,
               ;; and it's convenient for conman too, where conman::test-5a on subsys_create will fail
               ;; otherwise because we'd close databases which are open in nested parent txn's otherwise.
               ;; Note that we don't handle :MVCC yet.
               |#
               (when (and (eq (repository/open-mode cached-repository) :update)
                          (eq open-mode :readonly))
                 (repository-cache-debug "~%Repository cache debug: repository-open HIT (readonly) for ~s~%"
                                         repository-dbpath)
                 (return-from open-repository (funcall receiver cached-repository :from-cache)))

             (repository-cache-debug
              "repository-open HIT, but mode MISS (hence database REOPEN) for ~s"
              repository-dbpath)
            (unless (or *suppress-cached-repository-reopen-warnings*
                        (eq (repository/open-mode cached-repository) open-mode))
              (warn "WITH-CACHED-OPEN-REPOSITORIES is active, and the current open mode (~s) for ~
                    repository ~s does not match the previous open mode (~s). ~
                    The database will be closed and re-opened."
                       open-mode
                       repository-dbpath
                       (repository/open-mode cached-repository)))
            (repository-close cached-repository :force t)
            (open-repository repository-dbpath open-mode receiver
                             :if-does-not-exist if-does-not-exist
                             :if-exists if-exists
                             :mode mode
                             :use use
                             :warn warn
                             :channel channel
                             :description description
                             :transport-info transport-info
                             :repository-type repository-type
                             :parent-repository parent-repository
                             :satellite-repositories satellite-repositories
                             :domain domain
                             :file-close-hook file-close-hook
                             :error-if-not-cached error-if-not-cached)))))

   (t
    (let ((repository-open-type :file-opened))
      ;; The requested repository isn't open. (We may have just closed it because the mode was wrong).

      ;; give error if it must be cached (and isnt) unless we are about to create it
      ;;      (when (and error-if-not-cached
      ;;                 (eq repository-open-type :file-opened))
      ;;        (unless (and (eq if-does-not-exist :create)
      ;;                     (eq if-exists :error)
      ;;                     (eq open-mode :update))
      ;;          (core-error (format nil
      ;; "At this time, the repository ~s may only be opened from the cache."
      ;;                              repository-dbpath))))

      (when *repository-schema-upgrade-in-progress*
        (assert (not (eq use :ask)))
        (setq repository-open-type :file-schema-upgrade))

      (let ((opened-ok nil)
            (transient-repository nil))
        (flet ((wrapped-receiver (transient-repository-value)
                 (setq transient-repository transient-repository-value
                       opened-ok t)

                 ;; Mark it as recently opened.
                 (pushnew transient-repository *recently-opened-repositories*)
                 (push (cons transient-repository 1) *open-repositories*);; begin open count at 1.

                 (funcall receiver transient-repository-value repository-open-type)))
        (unwind-protect
             (ecase open-mode
               ((:readonly :mvcc)
                (repository-open-readonly
                 domain repository-dbpath repository-type
                 #'wrapped-receiver
                 ))
               (:update
                (repository-open-update
                 domain repository-dbpath repository-type
                 #'wrapped-receiver
                 parent-repository
                 satellite-repositories
                 #||                    ;
                 if-does-not-exist
                 if-exists mode use warn channel
                 description transport-info
                 ||#
                 )))
          "clean up the open db"
          (unless opened-ok
            (when transient-repository
              (repository-really-close transient-repository)))))


      ;; Return the transient repository.
      (repository-cache-debug "repository-open ~a ~a for ~s"
                              repository-open-type open-mode repository-dbpath)
      (when file-close-hook
                (push file-close-hook (repository/file-close-hooks transient-repository))))))))

(defparameter *enable-repository-caching* t
  "If NIL, repository caching will be disabled.

   Note that those repositories already cached may continue to be cached.")

(defparameter *repository-cache-limit* nil
  "Number of concurrent open repositories that are maintained.  If NIL, there
   is no limit to the number of repositories that may be opened.")

(defparameter *repository-cache-timeout* nil
  "If a number, the number of seconds during which a repository will be retained
   in an open state if it has been 'closed', but not re-opened.  If NIL, the repository
   will remain open indefinitely.  This variable works in conjunction with
   *repository-cache-limit*, so a repository may be closed even if it is `young',
   it may also stay open if it is 'old', but older repositories will be closed
   sooner than younger ones.")

(defvar *repository-cache-active* nil
  "If T, WITH-CACHED-OPEN-REPOSITORIES is in effect.  This will *never* be T
   if *enable-repository-caching* is NIL.")

(defvar *open-repository-cache* nil
  "The list is in the form of (repository pathname-name . timestamp), where
   timestamp is the universal time when the repository was `closed', and where
   pathname-name is name component of pathname to repository (aids fast lookup).

   This is a list of repositories that have been `closed', but are being retained
   open in the cache.  When repository-open is called, if the repository is in
   this list it need not be opened because it was never closed.

   This gives us a performance advantage, but it does keep files open longer
   than necessary.  To control this we provide 3 parameters.

   *ENABLE-REPOSITORY-CACHING*, if NIL will disable all caching of repositories.

   *REPOSITORY-CACHE-LIMIT*, if a number, will limit this list of repositories
   to that number or less.

   *REPOSITORY-CACHE-TIMEOUT*, if a number, will limit this list to those
   repositories that have been used recently.

   The repositories in this list may be closed at any time, so repositories that are
   in use are removed from this list.")

(defun clean-repository-cache (&key (all nil))
  "Walks the repository cache closing stale and extra repositories.

   If keyword argument :ALL is T (default is NIL), the entire cache
   will be flushed."

  (if all
      (progn
        (dolist (entry *open-repository-cache* (setq *open-repository-cache* nil))
          (repository-cache-debug "repository-clean-all (REAL) for ~s" entry)
          (multiple-value-bind (result error-condition)
              (ignore-errors (repository-really-close (car entry))) ; do our best to close all repositories
            (when (and (null result) error-condition)
              (warn "An error was suppressed while closing a repository from ~
                          WITH-CACHED-OPEN-REPOSITORIES: ~a" error-condition))))
        (repository-cache-debug "repository-clean-all (cache) for ~s"
                                *open-repository-cache*))
    (progn

      ;; First, process the timeouts.
      (when (numberp *repository-cache-timeout*)
        (let ((stamp (- (get-universal-time) *repository-cache-timeout*)))
          (setq *open-repository-cache*
                (delete-if
                 (lambda (entry)
                     (when (< (cddr entry) stamp)
                       (repository-cache-debug "Closing stale repository ~s" (car entry))
                       (repository-really-close (car entry))
                       t))
                 *open-repository-cache*))))

      ;; Second, process the limit.
      (when (numberp *repository-cache-limit*)
        (let* ((tail (nthcdr (1- *repository-cache-limit*) *open-repository-cache*))
               (extra-open-repositories (cdr tail)))
          (when extra-open-repositories
            (repository-cache-debug "Closing extra open repositories ~s"
                                    extra-open-repositories)
            (setf (cdr tail) nil) ;; truncate the list.
            (mapc (lambda (entry)
                      (repository-really-close (car entry)))
                  extra-open-repositories)))))))

(defun cache-open-repository (repository)
  "This function places an open repository in the *OPEN-REPOSITORY-CACHE*.

   It is called by repository-close."
  (repository-cache-debug "repository-close (LOGICAL) for ~s"
                          (repository/dbpath repository))
  (push (list* repository
               (pathname-name (dbpath/pathname (repository/dbpath repository)))
               (get-universal-time))
        *open-repository-cache*)
  (clean-repository-cache))

(defun find-cached-repository (dbpath &aux (name (pathname-name (dbpath/pathname dbpath))) entry)
  (declare #.(performance-optimizations)) ;; must be FAST for product history report
  ;; *TO-DO*: use a distributed or more qualified name in case server hosts more than one repository
  ;; named "FOO" perhaps hosting different host/site qualified repositories as things evolve over time.
  (do ((tail *open-repository-cache* (cdr tail))
       (prior-tail      nil          tail))
      ((null tail)
       (repository-cache-debug "repository-open MISS for ~s" dbpath)
       nil)
    (setq entry (car tail)) ;; = (repos repos-pathname-name . timestamp)
    (when (and (string-equal name (second entry)) ;; fail FAST before calling expensive objects-equalp
               (objects-equalp dbpath (repository/dbpath (first entry))))
      (return (prog1 (first entry)
                ;; delete repos from cache (destructively) then clean cache
                (if prior-tail
                    (setf (cdr prior-tail) (cddr prior-tail))
                  (pop *open-repository-cache*))
                (clean-repository-cache))))))

(defmacro with-cached-open-repositories ((&key suppress-warnings) &rest body)
  "Execute one or more REPOSITORY-OPEN and REPOSITORY-CLOSE calls
   (via WITH-OPEN-REPOSITORY or through other means).  All repositories opened may remain open
   through the extent of this macro.  Upon macro exit, all open repositories are closed.
   The effect is to keep ObjectStore pages in memory cache across transactions, which requires
   that the database not be closed.  This macro should be called only once somewhere near
   the top level of your logic.  There should be no repositories open when this is invoked.

   For now, there is a restriction that multiple OPEN calls to the same repository occurring
   in the extent of this macro body must have the same open mode.   We can't cache repository pages
   if we have mixed read/write cases.  We could possibly mix MVCC and non-MVCC, and could possibly
   treat all opens as READ-WRITE if we were willing to risk unintentional updates to logically
   read-only databases.

   The return value of this macro is N/A."
  (with-unique-names (with-cached-open-repositories-body)
    `(PROGN
      (WHEN *OPEN-REPOSITORY-CACHE*
            (PROGN
             (repository-cache-debug "recursion-error still open (cache): ~s"
                                     *OPEN-REPOSITORY-CACHE*)
             (ERROR 'CHANGESAFE-DATABASE-ERROR
                    :format-control "Attempt to invoke WITH-CACHED-OPEN-REPOSITORIES recursively, or after failed cleanup ~
              attempt after last invocation.")))
       (WHEN (boundp '*REPOSITORY*)
         (ERROR 'CHANGESAFE-DATABASE-ERROR
                :format-control  "Attempt to invoke WITH-CACHED-OPEN-REPOSITORIES when a repository is already open, ~
              or after a failure occured in closing previously opened repositories."))
       (FLET ((,with-cached-open-repositories-body ()
                ,@body))
         ;; (DECLARE (DYNAMIC-EXTENT ,with-cached-open-repositories-body))
         (IF *ENABLE-REPOSITORY-CACHING*
             (PROGN
              (repository-cache-debug "Caching is enabled (warnings: ~a)"
                                     ,suppress-warnings)
               (UNWIND-PROTECT
                   (LET ((*SUPPRESS-CACHED-REPOSITORY-REOPEN-WARNINGS* ,suppress-warnings))
                     (setq *REPOSITORY-CACHE-ACTIVE* t) ;; DO NOT BIND THIS!
                     (,with-cached-open-repositories-body))
                 "closing cached open repositories"
                 (REPOSITORY-CACHED-CLOSE)
                 (setq *repository-cache-active* nil)
                 ))
           ;; If the enable flag is off, we don't do anything special.
           (,with-cached-open-repositories-body))))))

(defun repository-really-close (repository)
  "Close an open repository.  This may involve closing of multiple physical database extents."
  (repository-cache-debug "Repository cache debug: repository-close (REAL) for ~s"
                          (repository/dbpath repository))

  ;; call all of the file-close functions
  (dolist (hook (repository/file-close-hooks repository))
    (funcall hook repository))
  (setf (repository/file-close-hooks repository) nil)

  ;; close the file
  (persistent-store/close (repository/pstore repository))
  (deletef repository *recently-opened-repositories*)
  (setf (repository/pstore repository) nil))

(defun repository-close (repository &key (force nil))
  "Close an open repository.  This may involve closing of multiple physical database extents.
   Note that if repository page caching is enabled, we don't actually close the repository
   unless the FORCE flag is T."
  (check-type repository repository)
  (flet ((close-or-cache ()
           (if (or force
                   (not *repository-cache-active*))
               ;; We *really* want to close this sucker.
               (repository-really-close repository)
             ;; We want to logically close the database, but we actually keep it open.
             (cache-open-repository repository))
           (assert (null (slot-boundp repository 'persistent-data))))) ; should be nil after every txn end.

  (let ((entry (assoc repository *open-repositories*)))
    (if entry
        (if (> (cdr entry) 1)
            (if force ;; better be off
                (error 'changesafe-database-error
                       :format-control  "Force REPOSITORY-CLOSE on recursively opened repository ~s"
                       :format-arguments (list repository))
              (decf (cdr entry)))
          (progn
            (deletef entry *open-repositories*)
            (close-or-cache)))
      (close-or-cache)))))

(defun repository-cached-close ()
  "Close all repositories, even if they were cached for open/close behaviors.
   Generally only called by with-cached-open-repositories.  But sometimes you may
   wish to call this routine in order to delete a repository or something.  If so,
   we'll need to augment it for the case where we want to 'really close' only one repository."
  (clean-repository-cache :all t))

(defun repository/open-mode-as-txn-mode (repository)
  "Translate repository open mode to equivalent keywords for txn modes."
  (if (eq (repository/open-mode repository) :update)
      :read-write
    :read-only))

(defun repository/resolve-uid-specifier (repository specifier)
  "Given specifier, return the CORE-USER it represents."
  (etypecase specifier
    (keyword (if (eq specifier :nobody)
                 (repository/anonymous-user repository)
                 (error "Illegal uid specifier ~s." specifier)))

    (distributed-identifier (if (and (string= (did/domain specifier) (or (repository/domain repository) ""))
                                     (string= (did/repository specifier) (repository/name repository)))
                                (repository/resolve-distributed-identifier repository specifier)
                                specifier))))

(defun repository/resolve-meta-cid-set-specifier (repository specifier)
  "Given specifier, return the META-CID-SET it represents.

   Specifier may be the symbol T, meaning all CIDS,
   or a list of CID-DIDS."
  (cond ((null specifier) (error 'changesafe-database-error
                                 :format-control "Unspecified META-CID-SET for repository transaction."))
        ((eq specifier :latest-metaversion) (repository/master-cid-set repository))
        ((timestamp? specifier) (repository/master-cid-set repository :end-time specifier))
        ((cid-set? specifier) specifier)
        (t (error "Other metaversion cid-sets unimplemented."))))

(defun repository/resolve-cid-set-specifier (repository specifier)
  "Given specifier, return the CID-SET it represents.

   Specifier may be the symbol T, meaning all CIDS,
   or a list of CID-DIDS."
  (cond ((null specifier) (error 'changesafe-database-error
                                 :format-control "Unspecified CID-SET for REPOSITORY/BEGIN-TXN."))
        ;; yuck.  For core tests, we let the cid-set escape the transaction.
        ;; NOT portable.
        ;; This is just too dangerous.  Remove it altogether.
        ;; ((typep specifier 'cid-set) specifier)

        ((eq specifier :latest-version) (repository/master-cid-set repository))
        ;; If it is a function, we funcall it to get the cid-set.
        ;; This will happen within the meta-transaction phase.
        ((typep specifier 'function) (funcall specifier))
        (t (error "Other cid-sets unimplemented."))))

(defun repository/begin-txn (&key repository transaction-type underlying-transaction user
                                  cid-set-specifier
                                  aux-cid-set-specifier)
  (ecase transaction-type
    (:read-only-nonversioned
     (let ((transaction (make-instance 'nonversioned-repository-transaction
                                       :repository repository
                                       :underlying-transaction underlying-transaction
                                       :uid user)))
       (repository/add-transaction repository transaction)
       transaction))

    (:read-write-nonversioned
     (let ((transaction (make-instance 'nonversioned-update-repository-transaction
                                       :repository repository
                                       :underlying-transaction underlying-transaction
                                       :uid user)))
       (repository/add-transaction repository transaction)
       transaction))

    (:read-only
     (let ((transaction (make-instance 'versioned-repository-transaction
                                       :repository repository
                                       :cid-set (repository/resolve-cid-set-specifier repository cid-set-specifier)
                                       :underlying-transaction underlying-transaction
                                       :uid user)))
       (repository/add-transaction repository transaction)
       transaction))

    (:read-only-compare
     (let ((transaction (make-instance 'versioned-compare-repository-transaction
                                       :repository repository
                                       :before-cid-set (call-with-before-view
                                                        (lambda ()
                                                          (repository/resolve-cid-set-specifier 
                                                           repository cid-set-specifier)))
                                       :after-cid-set (call-with-after-view
                                                       (lambda ()
                                                         (repository/resolve-cid-set-specifier
                                                          repository aux-cid-set-specifier)))
                                       :underlying-transaction underlying-transaction
                                       :uid user)))
       (repository/add-transaction repository transaction)
       transaction))

    (:read-write
     (let* ((cid (repository/allocate-cid repository))
            (basis (repository/resolve-cid-set-specifier repository cid-set-specifier))
            (master-table-entry (make-instance 'cid-master-table-entry
                                               :cid cid
                                               :cid-set-basis (cid-set->bitmap basis)
                                               :why (transaction/reason underlying-transaction)
                                               :who user))
            (transaction (make-instance 'versioned-update-repository-transaction
                                        :cid cid
                                        :cid-master-table-entry master-table-entry
                                        :repository repository
                                        ;; put our cid into the cid-set so we can
                                        ;; see what we write.
                                        :cid-set (cid-set/adjoin basis cid)
                                        :underlying-transaction underlying-transaction
                                        :uid user)))
       (repository/add-transaction repository transaction)
       (cid-master-table/add-entry (repository/cid-master-table repository) master-table-entry)
       transaction))))

(defun repository/end-txn (repository transaction)
  (check-type transaction repository-transaction)
  (unless (eq transaction (repository/innermost-txn-for-uid-spec repository (repository-transaction/uid transaction)))
    (error 'changesafe-database-error :format-control "Attempt to end a transaction out of sequence."))
  (repository/remove-transaction repository transaction)
  (when (and (typep transaction 'versioned-update-repository-transaction)
             (not (eq (repository-transaction/disposition transaction) :aborting))
             (repository-transaction/cid-master-table-entry transaction))
    (cid-master-table-entry/note-finish
     (repository-transaction/cid-master-table-entry transaction)
     (repository-transaction/reason transaction)
     (repository-transaction/change-set transaction))))

(defconstant *repository-transaction-types* '(:read-only
                                              :read-write
                                              :read-cons
                                              :read-only-compare
                                              :read-cons-nonversioned
                                              :read-only-nonversioned
                                              :read-write-nonversioned))

(deftype repository-transaction-type () `(member ,@*repository-transaction-types*))

(defun repository-transaction-type->pstore-transaction-type (repository-transaction-type)
  (ecase repository-transaction-type
    ((:read-cons  :read-cons-nonversioned)  :read-cons)
    ((:read-only  :read-only-compare :read-only-nonversioned)  :read-only)
    ((:read-write :read-write-nonversioned) :read-write)))

(defun call-with-repository-transaction (&key repository 
                                              transaction-type
                                              user-id-specifier
                                              reason

                                              ;; generally, you only want to specify these two
                                              meta-cid-set-specifier
                                              cid-set-specifier 
                                              ;; but if you are doing a comparison,
                                              ;; specify these as well
                                              aux-meta-cid-set-specifier
                                              aux-cid-set-specifier 

                                              receiver)
  (check-type user-id-specifier (or keyword distributed-identifier))
  (check-type transaction-type repository-transaction-type)
  (check-type reason string)
  (call-with-transaction
   (repository/pstore repository)
   (repository-transaction-type->pstore-transaction-type transaction-type) reason
   (lambda (pstore-transaction)
     (call-with-recently-opened-repository-management
      pstore-transaction
      (lambda ()
        (with-current-repository (repository)
          (call-with-repository-bound-persistent-data
           pstore-transaction repository
           (lambda ()
             (let* ((resolved-uid (repository/resolve-uid-specifier repository user-id-specifier))
                    ;; disable any overrides
                    (*versioned-value-cid-set-override* nil)
                    ;; establish a meta-transaction for resolving the cid set
                    (*transaction*
                     (if (eq transaction-type :read-only-compare)
                         (make-instance 'versioned-compare-repository-transaction
                                        :repository repository
                                        :underlying-transaction pstore-transaction
                                        :uid resolved-uid
                                        :before-cid-set (repository/resolve-meta-cid-set-specifier 
                                                         repository meta-cid-set-specifier)
                                        :after-cid-set (repository/resolve-meta-cid-set-specifier 
                                                        repository aux-meta-cid-set-specifier))
                         (make-instance 'versioned-repository-transaction
                                        :repository repository
                                        :underlying-transaction pstore-transaction
                                        :uid     resolved-uid
                                        :cid-set (repository/resolve-meta-cid-set-specifier 
                                                  repository meta-cid-set-specifier)))))
               (let ((repository-txn nil))
                 (unwind-protect
                      (progn (with-deferred-interrupts "beginning repository txn"
                               (setq repository-txn (repository/begin-txn
                                                     :repository repository
                                                     :transaction-type transaction-type
                                                     :underlying-transaction pstore-transaction
                                                     :user resolved-uid
                                                     :cid-set-specifier cid-set-specifier
                                                     :aux-cid-set-specifier aux-cid-set-specifier)))
                             (let ((*versioned-value-cid-set-override* nil)
                                   (*transaction* repository-txn))

                               ;; Have to do the marshalling *before* we end
                               ;; the transaction.
                               (values-list
                                (marshal-from-transaction
                                 (multiple-value-list
                                     (funcall receiver repository-txn))))))
                   "ending repository transaction"
                   (when repository-txn
                     (repository/end-txn repository repository-txn)))))))))))))


;;
;; Repository CID management
;;

(defun repository/allocate-cid (repository)
  "Allocate a CID for a change transaction.  Must be unique in the repository."
  (integer-mapper/allocate-integer (repository/cid-mapper repository)
                                   (repository/local-mapper repository)))

(defun repository/contains-cid? (repository local-cid)
  "Return true if repository contains full CID content for the specified CID, and NIL if it doesn't.
   NOTE: this routine may return NIL for cids which are in the process of being imported."
  (cid-master-table/contains-cid? (repository/cid-master-table repository) local-cid))

(defun repository/cid-more-recent? (repository cid-1 cid-2)
  "Return true if CID-1 is more recent, chronologically, than CID-2.  Both cids must be local.
   Without import/export, this would be just CID-1 being greater than CID-2.
   However, with import/export, a local cid may represent a remote cid, and while the local
   cid is higher than some other local cid, it's change may have occurred at any time.

   The CID-MASTER-TABLE is responsible for tracking age of CIDS, we'll ask it..."
  (let ((time-1 (repository/cid-comparison-timestamp repository cid-1))
        (time-2 (repository/cid-comparison-timestamp repository cid-2)))
    (assert (and time-1 time-2))
    (timestamp/more-recent? time-1 time-2)))

(defun repository/cid-comparison-timestamp (repository cid)
  "Return the timestamp of the CID which is used for comparisons of CIDs for chronological
   ordering.  Note that it may someday become necessary to adjust adjust inter-repository
   date comparisons if one repository is known to run with a clock vastly out of sync with
   respect to another.  (Not timezones, but just plain clock/date correctness).

   Return NIL if the CID time information is not present in the repository"
  (or (and *cid-cached-timestamps*      ;import chicken/egg problem, imported Cid timestamps here
           (svref *cid-cached-timestamps* cid))
      (cid-master-table/cid-comparison-timestamp (repository/cid-master-table repository) cid)))

(defun repository/next-available-cid (repository)
  "Return the largest CID (numerically) in the repository."
  (integer-range-mapper/next-available-integer (repository/cid-mapper repository)))

(defun repository/last-allocated-cid (repository)
  (cid-master-table/last-allocated-cid (repository/cid-master-table repository)))

(defun repository/master-cid-set (repository &key end-time)
  (cid-master-table/active-cids repository (repository/cid-master-table repository) :end-time end-time))

(defun repository/resolve-distributed-identifier (repository did &key (error-if-missing t))
  "Master entry for resolving distributed identifiers with global scope in a repository.
   In general, only this routine should invoke the DISTRIBUTED-IDENTIFIER-RESOLVE generic function.

   **NOTE: if you have failures to resolve a DID based on the CLASS, and you're pretty sure
   the class is in fact mapped (as can be shown with REPOSITORY-MAPPER-DUMP), chances are
   you've got symbol package conflict problems, where the symbol in the map is from one package,
   say CORE, and the symbol being searched is from another package, say SCM, and there are two
   distinct symbols because core doesn't export the symbol to SCM.  It's a good argument for
   using strings for the class component instead of symbols.  Maybe another time."
  (check-type did distributed-identifier)
  ;; Switch to repository to avoid astore misfeature.
  ;; When looking up a symbol, it tries to intern it in the `current' repository.
  ;; So you can have all sorts of fun writing to read-only databases if you happen
  ;; to have the wrong current repository when you resolve a DID.

  (with-current-repository (repository)
    (cond ((eq (did/class did) :cid)
           ;; Resolve a pseudo-class reference
           ;; Note that we may not evey have the partial mapper hierarchy for foreign
           ;; references.
           (let ((mapper
                  (distributed-identifier/resolve did (repository/root-mapper repository)
                                                  :repository-only t :error-if-missing error-if-missing)))
             ;; Now call the cid mapper to resolve the rest
             ;; We consider it an error if the reference doesn't resolve, since
             ;; all cids should be added to map on IMPORT.  The called routine will
             ;; signal an error if necessary...
             (and mapper
                  (or (integer-mapper/resolve-distributed-identifier
                       (repository/cid-mapper repository) did mapper)
                      (if error-if-missing
                          (error 'changesafe-database-error
                                 :format-control  "Unable to resolve ~s ~s"
                                 (list did (did->list did)))
                          nil)))))
          (t                            ;resolve a normal class reference
           (distributed-identifier/resolve did (repository/root-mapper repository)
                                           :error-if-missing error-if-missing)))))


#||
(defmacro with-repository-txn ((repository uid-spec
                                &rest repository/begin-txn-keyargs)
                               &rest body)
  "Execute BODY in in the context of a new transaction, creating a new TXN-CONTEXT object which
   is bound to the identifier specified by TXN-CONTEXT-IDENTIFIER.  Unless abnormal control flow
   results, the value returned by this macro is the value of the last form in BODY.

   BODY is executed within an unwind-protect handler to ensure that the transaction will end
   once started in the event of normal or abnormal exit.

   **NOTE** The lambda list of this macro is not at all indicative of the arguments required.
   This is a historic artifact and a bad lambda list convention, please don't follow it.
   To help you see what *is* required, refer to the example below:

   Example: (with-repository-txn (txn-context repository :read-write \"mydb.rfm-user.1\"
                                              :cid-set *current-cid-set*)
               ... do stuff with txn-contex and repository variables ...)

   REPOSITORY and  UID-SPEC are as for REPOSITORY-BEGIN-TXN."
  ;; *FRANZ* *FINISH*: right now we're using ALLEGROSTORE:WITH-TRANSACTION for ostore txn support.
  ;; What we really wanted was non-lexical transaction support to be used in REPOSITORY-BEGIN-TXN
  ;; We can't finish this until they expose non-lexical txn capabilities.

  ;; Binding *assume-superclasses-unchanged* necessary for read-only txns ...  [ASSPR666]
  ;; Should be T for R/O, NIL for R/W.  It may be redundant with the binding by WITH-OPEN-REPOSITORY,
  ;; but that's okay, I'm paranoid.

  ;; Unfortunately, the database transaction semantics which occur in a global transaction
  ;; space don't reflect the repository update semantics.  So we must consider them separately.
  ;; Translate repository mode to a logical txn mode, and use the physical txn-mode specified
  ;; only for the physical database transaction argument.
  `(LET ((WITH-REPOSITORY-TXN-BODY (LAMBDA (*TRANSACTION*) ,@body))
         (REPOSITORY ,repository)
         (UID-SPEC ,uid-spec))
        ;; (DECLARE (DYNAMIC-EXTENT WITH-REPOSITORY-TXN-BODY))
     (WITH-REPOSITORY-TXN-1 WITH-REPOSITORY-TXN-BODY REPOSITORY UID-SPEC ,@repository/begin-txn-keyargs)))

(defun close-recently-opened-repositories ()
  (mapc (lambda (repository)
            (repository-close repository :force t))
        ;; Repository cache close will mutate this list,
        ;; so we snarf it first.
        (prog1 *recently-opened-repositories*
          (setq *recently-opened-repositories* nil))))

(defun with-repository-txn-1 (body-continuation repository uid-spec
                              &rest repository/begin-txn-keyargs
                              &key txn-mode &allow-other-keys)
  "The function which implements WITH-REPOSITORY-TXN."
  (call-with-transaction (repository/pstore repository) txn-mode
    (lambda (pstore-transaction)
      (let ((repository-txn nil))
        (unwind-protect
            (progn (with-deferred-interrupts "beginning repository txn"
                     (setq repository-txn
                           (apply #'repository/begin-txn repository pstore-transaction
                                  uid-spec repository/begin-txn-keyargs)))
                   (funcall body-continuation repository-txn))
          "ending repository txn"
          (when (eq (transaction/disposition pstore-transaction) :aborted)
            (close-recently-opened-repositories))
          (if repository-txn
              (unwind-protect
                  (repository/end-txn repository repository-txn)
                ;; Paranoia, repository/end-txn should have cleared this.
                (when (slot-boundp repository 'persistent-data)
                  (slot-makunbound repository 'persistent-data)
                  (error 'changesafe-database-error
                         :format-control "Repository persistent data not cleared when ending transaction.")))
              ;; Paranoia, repository/begin-txn should not have set this
              ;; if it didn't complete.
              (when (slot-boundp repository 'persistent-data)
                (slot-makunbound repository 'persistent-data)
                (error 'changesafe-database-error
                       :format-control "Repository persistent set, but no transaction created."))))))))
||#

(defgeneric repository/cid-information (repository cid)
  (:documentation
   "Return the following bits of information related to the indicated CID via VALUES:
    (1) the cid reason,
    (2) the cid timestamp,
    (3) the value of the CMTE versioned-change-information slot
    (4) the cid-set-basis.")
  (:method ((repository repository) (cid integer))
    (cid-master-table/cid-information repository (repository/cid-master-table repository) cid)))

(defgeneric repository/cid-versioned-change-information (repository cid)
  (:documentation
   "Like REPOSITORY-GET-CID-INFORMATION, but only returns value of
    the CMTE versioned-change-information slot.")
  (:method ((repository repository) (cid integer))
    (cid-master-table/cid-versioned-change-information
     (repository/cid-master-table repository) cid)))

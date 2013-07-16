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
;;;; Module Description: ConMan controller layer.
;;;;
;;;; This is the controller layer for a model/view/controller layer.
;;;; It separates the view provided by CM-CLI from the model in various
;;;; other modules.  It is the boundary for all transactions.  All transactions
;;;; which operate on the conman model are opened in this module.
;;;;
;;;; There is actually a two-level controller layer in ConMan.  The CMCTL
;;;; layer is responsible for operations on the master repository.  The
;;;; MASTER-CATALOG module manages transactions on satellite and workspace
;;;; repositories, neither of which should be opened in this module.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Author:        Joe Marshall
;;;; Creation Date: 2003
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CONMAN")

(proclaim (standard-optimizations))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(
            cmctl/query-workspace-changes
            )))


(defvar *cmctl-dot-conman-file-name*  "REPOSITORY:;PRD-CSF"
  "The repository relative name of the .csf file.  Note how it is encoded.
   This variable may be bound by the test suite to avoid trashing the real
   .csf files.

   THIS VARIABLE SHOULD NOT BE BOUND BY ANY CODE BUT THE TEST SUITES.")

(defconstant +cmctl-dot-conman-file-name-default+ "REPOSITORY:;PRD-CSF"
  "the default value of *cmctl-dot-conman-file-name* - should be exactly the same
   in the source code.")

;;; We'll want to relax this for unicode, but for now....
(defun valid-name-char? (char)
  "Given a character, determine if it is valid as part of a cset-name"
  (and (standard-char-p char)
       (or (alphanumericp char)
           (find char "=._-+"))))

(defun valid-name? (name)
  "Given a string, determine if it is valid as a cset-name (short, user-supplied kind)
   according to the syntax rules."
  ;; Rules: 1st char must be a-zA-Z0-9
  ;; other chars must all be one of those or else one of
  ;; =, ., _, -, +

  ;; Note this works BECAUSE cmctl-valid-change-name-char? tests standard-char-p, so
  ;; any alphanumeric chars that are not a-zA-Z0-9 are excluded by standard-char-p.
  (and (stringp name)
       (locally (declare (string name))
         (and (< 0 (length name))
              (alphanumericp (char name 0))
              (every #'valid-name-char? name)))))

;;;
;;; The following two functions are co-dependent
;;;
(defun cmctl/generate-cset-name (user-change-name user-name pc-name)
  "Generate a change name according to HP desires...  All arguments are strings.
   USER-CHANGE-NAME is the cset name as specified by the user.
   USER-NAME is string userid information, not currently a canonical core-user object.
   PC-NAME is a product-configuration name."
  (concatenate 'string (etypecase user-name
                         (string user-name)
                         (distributed-identifier (did->string user-name)))
                         "_"
               (numeric-date-string) "_"
               pc-name ":"
               user-change-name))

(defun cmctl/disassemble-cset-name (augmented-cset-name)
  "Given a name returned by CONMAN-GENERATE-CSET-NAME, return its constituent components as
  multiple values chiefly (1) the user change name, (2) the user name (userid), (3) the product configuration
  (4) the date of the change.

  For now, it's an error if we can't decompose the name."
  (let* ((colon-pos (position #\: augmented-cset-name :test #'char=))
         (date-rpos (and colon-pos
                         (position #\_ augmented-cset-name :test #'char= :from-end t :end colon-pos)))
         (username-rpos (and date-rpos
                             (position #\_ augmented-cset-name :test #'char= :from-end t :end date-rpos))))
    (if username-rpos
        (values (subseq augmented-cset-name (1+ colon-pos)) ; user-change-name
                (subseq augmented-cset-name 0 username-rpos) ;username
                (subseq augmented-cset-name (1+ date-rpos) colon-pos) ; pc-name
                (subseq augmented-cset-name (1+ username-rpos) date-rpos)) ; date string
      (values nil nil nil nil))))

(defun cmctl/call-with-user-transaction (master-repository-dbpath user-id-specifier
                                                                  &key workspace-id
                                                                  reason
                                                                  transaction-type
                                                                  master-metaversion
                                                                  version
                                                                  receiver
                                                                  update-workspace)
  "Establishes a standard workspace and master-repository transaction for
   standard user commands.

   If master-metaversion is specified, it is used as is.  If not, the
   master-metaversion is taken from the workspace baseline timestamp.
   "
  (call-with-workspace
   master-repository-dbpath user-id-specifier workspace-id
   :transaction-type (ecase transaction-type
                       ((:read-write :read-write/no-change-set) :read-write-nonversioned)
                       ((:read-only :read-only/no-change-set)  :read-only-nonversioned))
   :reason reason
   :receiver
   (lambda (workspace-repository
            workspace-transaction
            workspace)
     (multiple-value-prog1
         (call-with-master-catalog-transaction
          master-repository-dbpath user-id-specifier
          :transaction-type transaction-type
          :master-metaversion (or master-metaversion
                                  (workspace/baseline-timestamp workspace))
          :version (or version
                       (versionref/version-did (workspace/baseline-versionref workspace)))
          :reason reason
          :receiver
          (lambda (master-repository master-transaction master-catalog)
            (workspace/call-with-resolved-versionref
             workspace master-repository
             (lambda (product branch version timestamp)
               (funcall receiver
                        master-repository
                        master-transaction
                        master-catalog
                        workspace
                        product branch version timestamp)))))
       (funcall update-workspace workspace-repository workspace-transaction workspace)))))

(defun cmctl/create-master (conman-request master-db-directory master-name)
  "Populate and optionally create a database directory named by MASTER-DB-DIRECTORY, which
   must be a DBPATH, so that it has a master ChangeSafe repository distinguished
   by the name 'master-name', which must also be a string.

   If the directory doesn't exist and FORCE-P is nil, signal an error.
   If the directory doesn't exist and FORCE-P is true, create the directory.
   Note that this routine will only create leaf directories, it won't create an entire path.
   Also note that it is unable to test for the existence of the directory or database
   files on remote database servers.

   (cm-session-context)
   MASTER-NAME is used as an embedded token in a generated master repository name within the
   directory specified by MASTER-DB-DIRECTORY.
   It may also be used to construct satellite repository names as well.

   If the directory already has a master repository with the indicated name, signal an error.
   Note that we are unable to test for this condition on remote database servers.
   If there are any problems creating the master repository or the directory, signal an error.

   Returns T if successful, or throws an exception."
  (declare (ignore conman-request))
  (check-type master-db-directory dbpath)
  (let* ((fs (make-instance 'lisp-file-system))
         (dir-pathname (dbpath/pathname master-db-directory))
         ;; Recall non-local names have syntax HOST:absolute-path and
         ;; specify a non-local machine running an Objectsore server
         (local-p (dbpath/local-p master-db-directory)))

    ;; We can only ensure directory exists if it's on the local machine.
    ;; currently we don't support remote databases anyway
    (if local-p
        (unless (file-system/directory-exists-p fs dir-pathname)
          (conman-error/no-such-directory "Database directory ~S does not exist" dir-pathname))
      #||
      ;; This is commented out per Jack's request, and Dave said "because there is nothing the user
      ;; can do to avoid the warning" It would be nice, Dave said, if an informational message were
      ;; given, but we have no infrastructure in place currently for that.
      (conman-signal-warning *cm-returns-warning-default*
                             "Unable to verify that database directory exists on remote server ~A."
                             (db-host-name (dbpath-host master-db-directory)))
      ||# )
    ;; Have the directory, check for presence of master db.
    (let ((mdb-dbpath (dbpath/merge (make-pathname :name master-name
                                                     :type +repository/master-file-type+
                                                     :defaults "")
                                      master-db-directory)))
      (if local-p
          (when (file-system/probe-file fs (dbpath/pathname mdb-dbpath))
            (conman-error/master-repository-exists master-name dir-pathname))
        #||(conman-signal-warning *cm-returns-warning-default*
                                  "We are unable to determine if the specified database already exists because is ~
           resides on a remote server.")
        ||# )
      (with-open-repository (repository mdb-dbpath :update
                                        :repository-type :master
                                        :error-if-not-cached t ;; t is ok for create
                                        :if-exists :error
                                        :if-does-not-exist :create)
        ;; Create the master catalog (it gets stashed in a well known
        ;; root in this operation).  We have a cset transaction here,
        ;; but not a vm-txn, there is no project which manages changes
        ;; to the master.
        (call-with-repository-transaction
         :meta-cid-set-specifier :latest-metaversion
         :cid-set-specifier :latest-version
         :reason "Create master repository and master catalog"
         :repository repository
         :transaction-type :read-write
         :user-id-specifier :nobody
         :receiver
         (lambda (repository-transaction)
           (declare (ignore repository-transaction))
           (master-catalog/install-in-repository repository))))

      ;; Initialize the workspace repository as well.
      ;; Note that this is *outside* the master transaction.
      (workspace-repository-create  mdb-dbpath :nobody))))

(defun cmctl/create-product (conman-request product-name main-branch-name product-description)
  "Create a product configuration and return true, or signal an appropriate error.
   See CM-CLI-PRODUCT-CREATE documentation for more details.

   This corresponds to a set of rows in the project/class matrix.
   (Each branch is a row)."
  (check-type conman-request conman-request)
  (check-type product-name string)
  (check-type product-description (optional string))
  (let ((reason (format-in-language
                 (conman-request/client-locale conman-request) nil
                 :create-product-reason product-name))
        (userid (conman-request/user-name conman-request)))
    (call-with-master-catalog-transaction
     (conman-request/repository-dbpath conman-request)
     userid
     :master-metaversion :latest-metaversion
     :version :latest-version
     :reason reason
     :transaction-type :read-write
     :receiver (lambda (master-repository
                        master-transaction
                        master-catalog)
                 (declare (ignore master-repository master-transaction))
                 (cond ((master-catalog/find-product master-catalog product-name)
                        => (lambda (existing)
                             (conman-error/product-exists conman-request existing)))
                       (t (master-catalog/create-product userid
                                                         reason
                                                         master-catalog
                                                         product-name
                                                         main-branch-name
                                                         product-description)))))))

(defun cmctl/create-class (conman-request class-name description subdirectory)
  "Create a ChangeSafe class, which is really just a database full of subsystems.
   This corresponds to a column in the project/class matrix.

   Classes are rfm::project objects managed in satellite repositories, and which are referenced
   by product configurations via subsystems in the master repository.

   When we create a class, we create a satellite repository in the same directory as the master,
   and chain it to the master.

   The subdirectory associated with the satellite-project-ref is OS NEUTRAL.

   See CM-CLI-CLASS-CREATE documentation for more details."
  (check-type class-name (optional string))
  (check-type description (optional string))
  (check-type subdirectory relative-directory-pathname)
  (unless (valid-name? class-name)
    (conman-error/bad-class-name class-name))
  (let ((master-repository-name (conman-request/repository-dbpath conman-request))
        (reason (format-in-language (conman-request/client-locale conman-request) nil
                                    :create-class-reason class-name))
        (userid (conman-request/user-name conman-request)))
    (call-with-master-catalog-transaction
     master-repository-name
     userid
     :master-metaversion :latest-metaversion
     :version :latest-version
     :reason reason
     :transaction-type :read-write
     :receiver (lambda (master-repository master-transaction master-catalog)
                 (declare (ignore master-transaction))
                 (cond ((master-catalog/find-class master-catalog class-name)
                        => (lambda (existing)
                             (conman-error/class-exists conman-request existing)))
                       (t
                        (multiple-value-bind (csf-class satellite-cset-did)
                            (master-catalog/create-class
                             master-repository-name master-repository master-catalog
                             reason userid
                             class-name description
                             (pathname->logical-pathname subdirectory))
                        (values (list :name (cmctl/generate-cset-name "create-class" userid "")
                                      :description reason
                                      :satellite-change-set-alist (persistent-list
                                                                   (pstore-list-cons csf-class
                                                                                     satellite-cset-did)))
                                (lambda (master-change-set)
                                  ;; Make sure we can find the cset that created the
                                  ;; satellite class.
                                  (setf (csf-class/project-creation-cset csf-class) master-change-set)
                                  nil)))))))))

(defun cmctl/list-classes (conman-request)
  "cheesy hack to list the classes"
  (let ((master-repository-name (conman-request/repository-dbpath conman-request))
        (reason "list classes")
        (userid (conman-request/user-name conman-request)))
    (call-with-master-catalog-transaction
     master-repository-name
     userid
     :master-metaversion :latest-metaversion
     :version :latest-version
     :reason reason
     :transaction-type :read-only
     :receiver (lambda (master-repository master-transaction master-catalog)
                 (declare (ignore master-repository master-transaction))
                 (collect 'list
                          (map-fn 't (lambda (class)
                                       (list (named-object/name class)
                                             (described-object/description class)
                                             (csf-class/project-did class)
                                             (csf-class/subdirectory class)
                                             (csf-class/satellite-relative-pathname class)))
                                  (master-catalog/scan-classes master-catalog)))))))

(defun cmctl/list-products (conman-request)
  "cheesy hack to list the products"
  (let ((master-repository-name (conman-request/repository-dbpath conman-request))
        (reason "list products")
        (userid (conman-request/user-name conman-request)))
    (call-with-master-catalog-transaction
     master-repository-name
     userid
     :master-metaversion :latest-metaversion
     :version :latest-version
     :reason reason
     :transaction-type :read-only
     :receiver (lambda (master-repository master-transaction master-catalog)
                 (declare (ignore master-repository master-transaction))
                 (collect 'list
                          (map-fn 't (lambda (product)
                                       (list (distributed-object-identifier product)
                                             (named-object/name product)
                                             (described-object/description product)))
                                  (master-catalog/scan-products master-catalog)))))))

(defun cmctl/create-subsystem (conman-request class-name subsystem-name subsystem-description)
  "Create a subsystem, which is a realizable element in the product/class grid.

   This will have a lot more features in the future, but for now...."
  (let ((master-repository-name (conman-request/repository-dbpath conman-request))
        (reason (format-in-language (conman-request/client-locale conman-request) nil
                                    :create-subsystem-reason subsystem-name))
        (userid (conman-request/user-name conman-request)))
    (call-with-master-catalog-transaction
     master-repository-name
     userid
     :master-metaversion :latest-metaversion
     :version :latest-version
     :reason reason
     :transaction-type :read-write
     :receiver (lambda (master-repository master-transaction master-catalog)
                 (declare (ignore master-repository master-transaction))
                 (cond ((master-catalog/find-subsystem master-catalog subsystem-name)
                        => (lambda (existing)
                             (conman-error/subsystem-exists conman-request existing)))
                       ((master-catalog/find-class master-catalog class-name)
                        => (lambda (csf-class)
                             (multiple-value-bind (subsystem satellite-change-set-did)
                                    (master-catalog/create-subsystem master-repository-name master-catalog
                                                                     reason userid
                                                                     csf-class :latest-metaversion
                                                                     subsystem-name subsystem-description
                                                                     (csf-class/subdirectory csf-class))
                               (values (list :name (cmctl/generate-cset-name "create-subsystem" userid "")
                                             :description reason
                                             :satellite-change-set-alist (persistent-list
                                                                          (pstore-list-cons csf-class
                                                                                            satellite-change-set-did)))
                                       (lambda (master-change-set)
                                         ;; Promote this change set to the csf-class.
                                         (push (pstore-list-cons subsystem master-change-set)
                                               (csf-class/subsystem-creation-csets csf-class))
                                         nil)))))
                       (t (conman-error/no-such-class conman-request class-name)))))))

(defun cmctl/list-subsystems (conman-request)
  "cheesy hack to list the subsystems"
  (let ((master-repository-name (conman-request/repository-dbpath conman-request))
        (reason "list subsystems")
        (userid (conman-request/user-name conman-request)))
    (call-with-master-catalog-transaction
     master-repository-name
     userid
     :master-metaversion :latest-metaversion
     :version :latest-version
     :reason reason
     :transaction-type :read-only
     :receiver (lambda (master-repository master-transaction master-catalog)
                 (declare (ignore master-repository master-transaction))
                 (collect 'list
                          (map-fn 't (lambda (subsystem)
                                       (list (named-object/name subsystem)
                                             (described-object/description subsystem)
                                             (project-reference/branch-did subsystem)))
                                  (master-catalog/scan-subsystems master-catalog)))))))

(defun cmctl/subscribe-to-subsystem (conman-request branch-specifier subsystem-specifier subscription-mode)
  "Attach the subsystem to the branch.

   This is how we create a mapping from a grid point (branch x class) to an actual
   subsystem."
  (master-catalog/create-subscription conman-request
                                      (conman-request/repository-dbpath conman-request)
                                      branch-specifier subsystem-specifier subscription-mode))

(defun cmctl/create-user (conman-request user-name description)
  (call-with-master-repository-transaction
   (conman-request/repository-dbpath conman-request)
   (conman-request/user-name conman-request);; user who authorized this user
   :master-metaversion :latest-metaversion
   :version :latest-version
   :reason "Create user"
   :transaction-type :read-write
   :receiver (lambda (master-repository master-transaction)
               (declare (ignore master-repository master-transaction))
               (let ((user (make-instance 'conman-user
                                          :name user-name
                                          :description description
                                          :guid (generate-guid)
                                          :power nil
                                          :password nil)))

                 (values (list :name (cmctl/generate-cset-name "create-user" user-name "")
                               :description (format nil "create user ~s" user-name)
                               :satellite-change-set-alist '())
                         (constantly user))))))

(defun cmctl/create-workspace (master-repository-dbname fsa user-name query &key hatcheck-ticket hatcheck-value)
  (declare (ignore query))
  (check-type master-repository-dbname dbpath)
  (check-type fsa file-system)
  (check-type user-name distributed-identifier)

  (let* ((client-platform (file-system/platform fsa))
         (client-locale   (file-system/locale   fsa))
         (directory-name  (uri-query/lookup hatcheck-value :directory))
         (product         (uri-query/lookup hatcheck-value :product))
         (product-directory? (uri-query/lookup hatcheck-value :product-directory-p))
         (description     (uri-query/lookup hatcheck-value :description))
         (workspace-guid  (uri-query/lookup hatcheck-value :workspace-guid))
         (workspace-directory (and directory-name (platform/parse-directory-namestring client-platform directory-name)))
         (reason (format-in-language client-locale nil
                                     :create-workspace-reason
                                     workspace-directory
                                     user-name))
         (workspace-id    nil))

    (check-type directory-name string)
    (check-type description string)
    (check-type product distributed-identifier)

    ;; Stuff it back for the next guy.
    (hatcheck/save hatcheck-value hatcheck-ticket)

    (unwind-protect
        (multiple-value-bind (product-did branch-did version-did baseline-timestamp master-metaversion-cid-set)
            (call-with-master-catalog-transaction
             master-repository-dbname
             user-name
             :master-metaversion :latest-metaversion
             :version :latest-version
             :reason reason
             :transaction-type :read-only
             :receiver (cmctl/ws-set-create-master-transaction-function
                        user-name
                        fsa
                        reason
                        nil
                        product
                        nil
                        workspace-directory
                        ;; Create the workspace directory.  We delay writing
                        ;; the .csf file until we can put the
                        ;; workspace identifier into it.
                        (lambda ()
                          (file-system/create-directory fsa workspace-directory)
                          ;; Do this to get the canonical name.
                          (setq workspace-directory
                                (file-descriptor/path
                                 (file-system/probe-file fsa workspace-directory))))))
          (declare (ignore master-metaversion-cid-set))
          ;; Now update the workspace object -- Note that this must happen outside of the
          ;; cmctl-call-with-master-repository-txn

          (debug-message 3 "Project-did is ~s" product-did)
          (debug-message 3 "branch-did is ~s" branch-did)
          (debug-message 3 "version-did is ~s" version-did)
          (debug-message 3 "baseline timestamp is ~s" baseline-timestamp)

          ;; It's arguable, but we should probably open the master repository read-only
          ;; for workspace creation, since it's only the workspace repository which is updated.
          ;; However... doing so means that we can't update the workspace repository, because the outermost
          ;; transaction mode drives what we can do in nested transactions.  So if we want to open
          ;; the workspace repository in a nested transaction, we must open the outermost txn for update.
          ;; It's always a shame to sit on locks we don't need, so rather than opening the entire thing
          ;; R/W for what is essentially an R/O operation, we perform two separate transactions.
          ;; The R/O one for the versioned repositories, and the R/W one for
          ;; the non-versioned workspace repository.

          ;; Note that we'd really like the R/O txn to occur in MVCC mode.  This particular piece of txn logic
          ;; falls into a V-MVCC NV-RW territory I think we'll see elsewhere, and for which we should
          ;; optimize in 2-server configurations.

          ;; Note also that if we want to retain a LOCK on the master while creating the workspace,
          ;; we'll need to use the nested TXNs, and therefore a R/W txn on the master.
          (call-with-workspace-repository-transaction
           master-repository-dbname :nobody
           :transaction-type :read-write-nonversioned
           :reason reason
           :receiver
           (lambda (workspace-repository workspace-transaction)
             (declare (ignore workspace-transaction))
             (setq workspace-id (workspace/id (cmctl/ws-set-create-workspace-update
                                               :create
                                               workspace-repository
                                               user-name
                                               workspace-directory
                                               workspace-guid
                                               product-did branch-did version-did baseline-timestamp
                                               description
                                               product-directory?
                                               nil
                                               nil)))))

          ;; Write out the `.conman' file
          (cmctl/ws-set-create-write-conman-file :create
                                                 master-repository-dbname
                                                 fsa
                                                 workspace-id
                                                 workspace-directory))
      "perform the after transaction step"
      (cmctl/ws-set-create-after-transaction :create master-repository-dbname fsa workspace-id workspace-directory))
    t))

(defun cmctl/ws-set-create-master-transaction-function (user-id-specifier
                                                        fsa
                                                        reason
                                                        metaversion-timestamp
                                                        branch-specifier
                                                        version-specifier
                                                        workspace-directory
                                                        directory-maker)
  "Common code shared by CMCTL-WS-SET and CMCTL-WS-CREATE.

   Return a function to be used as the master repository transaction
   function for ws_set and ws_create.

   This function returns 5 values: PC-DID, PC-BRANCH-DID,
   PC-VERSION-DID, WS-BASELINE-TIMESTAMP, and the MASTER-METAVERSION-CID-SET.
   These values may be needed by the CMCTL-WS-SET/CREATE-WORKSPACE-UPDATE-FUNCTION function."
  (lambda (master-repository master-transaction master-catalog)
    (multiple-value-bind (product branch version)
        (master-catalog/resolve-version master-repository master-catalog branch-specifier version-specifier)
      (funcall directory-maker)

      ;; Populate the user's file hierarchy using the file-system
      ;; Create the workspace in the semipersistent database.  Note that this
      ;; operation is examining multiple satellite repository contents and should do so in a
      ;; read-only transaction.

      ;; *FINISH*: either now or in november, code this whole
      ;; thing so that we grab the information we need from the repositories and populate the
      ;; user's disk outside of a transaction.  Then do a final transaction to create the
      ;; semipersistent workspace record.
      ;; Assume that if we've reached this point that the disk management was successful.

      ;; *FINISH*: grab the correct branch and version of the PC and use them (ala WITH-VERSION),
      ;; and pass them to the following routine.

      ;; This creates and populates the subdirectories. (but will not clean them).
      (multiple-value-bind (baseline-timestamp master-metaversion-cid-set)
          (master-catalog/setup-workspace master-repository user-id-specifier
                                          master-transaction
                                          master-catalog
                                          (make-instance 'logical-file-system
                                                         :root workspace-directory
                                                         :underlying-file-system fsa)
                                          reason metaversion-timestamp
                                          product branch
                                          nil;; don't populate now.  Defer to sync time.
                                          )
        (values product branch version baseline-timestamp master-metaversion-cid-set)))))

(defun cmctl/ws-set-create-workspace-update (operation
                                             workspace-repository
                                             user-name
                                             directory-spec
                                             guid
                                             pc-did pc-branch-did pc-version-did ws-baseline-timestamp
                                             description
                                             product-directory?
                                             workspace
                                             force-p)
  "Common code shared by CMCTL-WS-SET and CMCTL-WS-CREATE to
   perform the updating of the workspace object.

   MASTER-TRANSACTION-RESULTS-GETTER should return these values:  PC-DID, PC-BRANCH-DID,
   PC-VERSION-DID, and WS-BASELINE-TIMESTAMP, which will have been provided by the
   RESULTS-CONTINUATION argument to CMCTL-WS-SET/CREATE-MASTER-TRANSACTION-FUNCTION.

   Should be called from within a read/write transaction on the workspace repository."
  (ecase operation
    (:create
     (if workspace
         (conman-error/workspace-exists directory-spec)
         (workspace/create workspace-repository
                           user-name
                           directory-spec
                           guid
                           pc-did pc-branch-did pc-version-did
                           ws-baseline-timestamp
                           description product-directory?)))
    (:set
     (if workspace
         (workspace/modify-for-ws-set workspace-repository
                                      workspace
                                      pc-did pc-branch-did pc-version-did
                                      ws-baseline-timestamp
                                      description force-p)
         (conman-error/no-such-workspace directory-spec)))))

(defun cmctl/ws-set-create-after-transaction (operation master-repository-dbname fsa workspace-id directory-spec)
  "Common things to be done by ws_set and ws_create after the database
   transaction is finished."
  ;; Create the `.csf' file at the workspace root directory with
  ;; the relevant context.
  (ecase operation
    (:create
     ;; nothing to do, `.conman' already created
     )
    (:set
     (cmctl/ws-set-create-write-conman-file operation master-repository-dbname fsa workspace-id directory-spec))))

(defun cmctl/ws-set-create-write-conman-file (operation master-repository-dbname fsa workspace-id directory-spec)
  "Create or overwrite `.conman' file in workspace with new or updated information"
  (cmctl/write-conman-rc master-repository-dbname fsa workspace-id directory-spec
                         :if-exists (ecase operation
                                      (:set :supersede)
                                      (:create :error))
                         :if-does-not-exist (ecase operation
                                              (:set :error)
                                              (:create :create))))

(defun cmctl/write-conman-rc (master-repository-dbname
                              fsa
                              workspace-id
                              directory-spec
                              &key (if-exists :error) (if-does-not-exist :create))
  "Write interesting parts of cm-session-context to a .csf file in the directory
   indicated by DIRECTORY-SPEC and accessed via FILE-SYSTEM.  This should become the canonical
   function for creating/updating the .csf file, though it will undoubtedly need work.

   Note that only portions of CM-SESSION-CONTEXT which are least-common-denominator in
   nature are written, so that we don't put things in the .csf file which would fail to
   work with some commands.

   IF-EXISTS should be either :ERROR or :SUPERSEDE, and defaults to :ERROR.
   This is atypical of most :IF-EXISTS uses, but we don't necessarily want to overwrite
   a user's .csf file.

   IF-DOES-NOT-EXIST should be either :CREATE, :OPEN, or :ERROR, and defaults to :CREATE.

   Returns the cm-session-context."
  ;; The perhaps poorly named LOGICAL-FILE-SYSTEM-CHANGE-DIRECTORY function actually CREATES a logical file
  ;; system mapped to a specific directory.

  ;; We don't use cm-session-context-workspace-rooted-file-system here because we may not
  ;; have a workspace root yet.
  (let ((logical-file-system (make-instance 'logical-file-system
                                            :root directory-spec
                                            :underlying-file-system fsa)))
    (with-file-system-stream (stream logical-file-system *cmctl-dot-conman-file-name*
                              :direction :output
                              :element-type 'character
                              :record-separator (file-system/record-separator logical-file-system)
                              :if-exists if-exists
                              :if-does-not-exist if-does-not-exist)
      (flet ((doit (key val)
               (when val
                 (file-system/write-line logical-file-system stream
                                         ;; Beware, not all vals are strings! Some are numbers.
                                         ;; (this was once a concatenate)
                                         (format nil "~a ~a" key val)))))
        (doit +cm-cli-keyword/master+
              (dbpath/namestring master-repository-dbname))
        ;; NOTE: we do NOT want to write out the product name or the release name to the
        ;; .csf file, since those would be redundantly stored in the workspace, leading
        ;; to inconsistent data.  Worse, the user could type them in wrongly on the command
        ;; line, leading to insanity. Finally, renaming of the release and product could cause
        ;; additional headaches.  The ws-id and master repository name are sufficient to find
        ;; the workspace, which allows finding the baseline-version, which finds branch and
        ;; then pc.  Further, the (current) names can be found for the pc and branch.

        ;;      (doit *cm-cli-keyword-pc-name-syntax* (cm-session-context-pc-name cm-session-context))
        (doit +cm-cli-keyword/ws-id+ workspace-id)
        ;;      (doit *cm-cli-keyword-release-name-syntax* (cm-session-context-release-name cm-session-context))
        ))))

(defun cmctl/regenerate-workspace (master-repository-dbpath user-id-specifier query)
  "Make sure that the disk contents reflect the actual state of the workspace.

   This assumes that the workspace has been `trashed'."
  (let* ((workspace-id (uri-query/lookup query :workspace-id))
         (reason (format-in-language (server-locale) nil :regenerate-workspace-reason user-id-specifier workspace-id)))
    (call-with-workspace
     master-repository-dbpath user-id-specifier workspace-id
     :reason reason
     :transaction-type :read-write-nonversioned
     :receiver (lambda (workspace-repository workspace-transaction workspace)
                 (declare (ignore workspace-repository workspace-transaction))
                 (if (workspace/in-transition? workspace)
                     ;; If user called regenerate while in transition,
                     ;; simply regenerate the new state.
                     ;; This probably won't work right away.
                     (workspace/begin-transition workspace :regenerate
                                                 (make-instance 'versionref
                                                                :project-did (workspace/transitional-product-did workspace)
                                                                :branch-did  (workspace/transitional-branch-did workspace)
                                                                :version-did (workspace/transitional-version-did workspace)
                                                                :timestamp   (workspace/transitional-timestamp workspace))
                                                 (workspace/transitional-added-master-csets workspace)
                                                 (workspace/transitional-removed-master-csets workspace)
                                                 (workspace/transitional-added-class-cset-tuples workspace)
                                                 (workspace/transitional-removed-class-cset-tuples workspace))
                     ;; Set new mode to old mode and sync.
                     (workspace/begin-transition workspace :regenerate
                                                 (make-instance 'versionref
                                                                :project-did (workspace/product-did workspace)
                                                                :branch-did  (workspace/branch-did workspace)
                                                                :version-did (workspace/version-did workspace)
                                                                :timestamp   (workspace/baseline-timestamp workspace))
                                                 (workspace/added-master-csets workspace)
                                                 (workspace/removed-master-csets workspace)
                                                 (workspace/added-class-cset-tuples workspace)
                                                 (workspace/removed-class-cset-tuples workspace)))
                 t))))

(defun cmctl/update-workspace (master-repository-dbpath fsa user-id-specifier query &key hatcheck-ticket hatcheck-value)
  "Bring workspace up to date with regard to repository.  This is the entry point
   for the user command ws_update.  It validates arguments and continues with
   CMCTL-UPDATE-WORKSPACE, which is the main entry point for updating a workspace."
  (declare (ignore fsa hatcheck-ticket hatcheck-value))
  (let* ((workspace-id (uri-query/lookup query :workspace-id))
         (reason (format-in-language (server-locale) nil :update-workspace-reason user-id-specifier workspace-id))

          ;; *FINISH*: use release, label, whatever
         (new-timestamp (timestamp-allocate)))
    (call-with-workspace
     master-repository-dbpath user-id-specifier workspace-id
     :reason reason
     :transaction-type :read-write-nonversioned
     :receiver
     (lambda (workspace-repository workspace-transaction workspace)
       (declare (ignore workspace-repository workspace-transaction))
       (workspace/begin-transition workspace :update
                                   (make-instance 'versionref
                                                  :project-did (workspace/product-did workspace)
                                                  :branch-did  (workspace/branch-did  workspace)
                                                  :version-did (workspace/version-did workspace)
                                                  :timestamp   new-timestamp)
                                   (workspace/added-master-csets workspace)
                                   (workspace/removed-master-csets workspace)
                                   (workspace/added-class-cset-tuples workspace)
                                   (workspace/removed-class-cset-tuples workspace))
       t))))

(defun cmctl/query-product (master-repository-dbpath user-id-specifier product-did)
  "Return info about a particular product."
  (let ((reason (format-in-language (server-locale) nil :query-product-reason user-id-specifier product-did)))
    (call-with-master-repository-transaction
     master-repository-dbpath user-id-specifier
     :transaction-type :read-only
     :master-metaversion :latest-metaversion
     :version :latest-version
     :reason reason
     :receiver (lambda (master-repository master-transaction)
                 (declare (ignore master-transaction))
                 (let ((product (repository/resolve-distributed-identifier master-repository product-did)))
                   (values product-did 
                           (named-object/name product)
                           (described-object/description product)
                           (product/active? product)
                           (collect 'list (#m (lambda (branch)
                                                (list (distributed-object-identifier branch)
                                                      (named-object/name branch)
                                                      (described-object/description branch)))
                                              (product/scan-branches product)))
                           (product/change-sets product)))))))

(defun cmctl/query-branch (master-repository-dbpath user-id-specifier branch-did)
  "Return info about a particular branch."
  (let ((reason (format-in-language (server-locale) nil :query-branch-reason user-id-specifier branch-did)))
    (call-with-master-repository-transaction
     master-repository-dbpath user-id-specifier
     :transaction-type :read-only
     :master-metaversion :latest-metaversion
     :version :latest-version
     :reason reason
     :receiver (lambda (master-repository master-transaction)
                 (declare (ignore master-transaction))
                 (let ((branch (repository/resolve-distributed-identifier master-repository branch-did)))
                   (values branch-did
                           (named-object/name branch)
                           (described-object/description branch)
                           (collect 'list (#m (lambda (version)
                                                (list (distributed-object-identifier version)
                                                      (named-object/name version)
                                                      (described-object/description version)))
                                              (branch/scan-versions branch)))))))))

(defun cmctl/query-version (master-repository-dbpath user-id-specifier version-did)
  "Return info about a particular version."
  (let ((reason (format-in-language (server-locale) nil :query-version-reason user-id-specifier version-did)))
    (call-with-master-repository-transaction
     master-repository-dbpath user-id-specifier
     :transaction-type :read-only
     :master-metaversion :latest-metaversion
     :version :latest-version
     :reason reason
     :receiver (lambda (master-repository master-transaction)
                 (declare (ignore master-transaction))
                 (let ((version (repository/resolve-distributed-identifier master-repository version-did)))
                   (values version-did
                           (named-object/name version)
                           (described-object/description version)
                           (version/last-update-timestamp version 
                                                          (repository/master-cid-set master-repository)
                                                          master-repository)
                           (collect 'list
                                    (#m (lambda (cid-object)
                                          (list (csf/core::cid-object/did cid-object)
                                                (csf/core::repository/cid-information *repository* cid-object)))
                                        (version/scan-cid-objects version)))))))))

(defun cmctl/query-workspace (master-repository-dbpath user-name workspace-id)
  "Return copious amounts of information about a particular workspace."
  (let ((reason (format-in-language (server-locale) nil :query-workspace-reason user-name workspace-id))
        (product-info nil)
        (branch-info  nil)
        (version-info nil)
        (branch-timestamp nil))
    (cmctl/call-with-user-transaction
     master-repository-dbpath user-name
     :workspace-id workspace-id
     :reason reason
     :transaction-type :read-only
     :master-metaversion :latest-metaversion
     :receiver
     (lambda (master-repository master-transaction master-catalog workspace product branch version workspace-timestamp)
       (declare (ignore master-catalog))
       (setq product-info (list (distributed-object-identifier product)
                                (named-object/name product)
                                (described-object/description product))
             branch-info  (list (distributed-object-identifier branch)
                                (named-object/name branch)
                                (described-object/description branch))
             version-info (if (version? version)
                              (list (distributed-object-identifier version)
                                    (named-object/name version)
                                    (described-object/description version))
                              version)
             branch-timestamp (let ((latest-version (branch/mutable-tip branch)))
                                (version/last-update-timestamp latest-version
                                                               (transaction/cid-set master-transaction)
                                                               master-repository)))
       (values (workspace/id               workspace)
               (workspace/guid             workspace)
               (workspace/transition-mode  workspace)
               (workspace/description      workspace)
               (workspace/cset-name        workspace)
               (workspace/cset-description workspace)

               product-info branch-info version-info
               workspace-timestamp
               branch-timestamp

               (and (workspace/file-additions workspace)
                    (not (zerop (persistent-vector-length (workspace/file-additions workspace))))
                    (collect 'list
                             (#m change-context/pathname
                                 (scan-persistent-vector
                                  (workspace/file-additions workspace)))))
               (and (workspace/file-changes workspace)
                    (not (zerop (persistent-vector-length (workspace/file-changes workspace))))
                    (collect 'list
                             (#m change-context/file-did
                                 (scan-persistent-vector
                                  (workspace/file-changes workspace)))))
               (and (workspace/file-deletions workspace)
                    (not (zerop (persistent-vector-length (workspace/file-deletions workspace))))
                    (collect 'list
                             (#m change-context/file-did
                                 (scan-persistent-vector
                                  (workspace/file-deletions workspace)))))
               (workspace/added-class-cset-tuples workspace)
               (workspace/removed-class-cset-tuples workspace)
               ))

     :update-workspace (constantly nil))))

(defun cmctl/query-workspace-changes (master-repository-dbpath user-name workspace-id)
  "Return information about the current visible changes in workspace."
  (let ((reason (format-in-language (server-locale) nil :query-workspace-changes-reason user-name workspace-id)))
    (cmctl/call-with-user-transaction
     master-repository-dbpath user-name
     :workspace-id workspace-id
     :reason reason
     :transaction-type :read-only
     :master-metaversion :latest-metaversion
     :receiver
     (lambda (master-repository master-transaction master-catalog workspace product branch version workspace-timestamp)
       (declare (ignore master-repository master-transaction master-catalog branch workspace-timestamp))
       (let ((active-cid-objects (version/cid-objects version))
             ;; Get a list of the change sets associated with the project
             ;; whether the workspace has them or not.  Get the name and description, too.
             (product-change-set-tuples
              (collect 'list 
                       (map-fn 't (lambda (change-set)
                                    (list change-set 
                                          (distributed-object-identifier change-set)
                                          (named-object/name change-set)
                                          (described-object/description change-set)))
                               (product/scan-change-sets product)))))

         (nreverse (collect 'list 
                            (map-fn 't (lambda (tuple)
                                         (destructuring-bind (change-set did name description) tuple
                                           (list (cond ((workspace/VPB-master-cset-inactive? workspace change-set) nil)
                                                       ((workspace/VPB-master-cset-active?   workspace change-set) t)
                                                       ((member (change-set/cid-object change-set) active-cid-objects) t)
                                                       (t nil))
                                                 did
                                                 name
                                                 description)))
                                    (scan 'list product-change-set-tuples))))))
     :update-workspace (constantly nil))))

;; THE FOLLOWING LOGIC APPLIES EQUALLY TO CHANGE_ADD and CHANGE_REMOVE
;; (a) If there are no changes in the user's workspace, this is easy, and we simply
;; use ws-baseline + new change{add/remove} activity as a descendant for update.
;; (b) If there are user files in the workspace which would be overwritten by the
;; update, but which aren't part of a change context, copy the files to '.bak' files
;; and warn the users.  This isn't too hard.  A warning level should be noted.
;; (c) The hard case is handling conflicts arising from active entities in a change context.
;; Like other cases, we have to copy files which would be modified to '.bak' files.
;; Then we have to handle conflict on a case by case basis.
;; The general approach is that there is the workspace baseline(ancestor version)
;; (vesionref, plus any previously effected change_{add,remove} actions) -
;; a.k.a. virtual private branch.
;; Descendant one is the baseline plus the current change_{add,remove} cset(s).
;;   (D1 may have conflict purely on this basis alone, typically two files with same name)
;; Descendant two is the baseline plus current changes in the workspace.
;;
;;                             **** USE THIS APPROACH ****
;;
;; In general, if change_add would cause to appear two files with the same name, an error
;; is signalled.  The user will have to create a change against the current conflict and rename the file,
;; before change_add can succeed.  This is the simple approach we should take.
;; A more automated approach is described in the case below, but may not ever be implemented.
;;
;;                      **** CONSIDER THIS APPROACH LONG TERM ****
;;                     (or if the general approach is insufficient)
;;
;; (c1) If the workspace file is checked out, merge the renamed file with the new view
;;   as if by ws_update using the A/D/D relationships above, aftering copying the user's
;;   work (if it differs from repository version).
;; (c2) If the workspace file is renamed (as by file_rename), there are several scenarios:
;;      (actions for these cases not yet rectified - *TBD*:
;;       I'm not sure I've even cited cases correctly)
;; (c2.1) The change_add/remove would activate a file as the OLD file name,
;;        and the old name now exists as a wild file.
;;        SOLUTION: copy the old file, warn the user.
;; (c2.2) The change_add/remove would activate a file as the OLD file name,
;;        and the old name is not used.
;;        SOLUTION: general merge logic, file is renamed in change context
;;        I.e. extract as new file name, which must be derived from workspace records.
;; (c2.3) The change_add/remove would activate a file as the NEW file name,
;;        SOLUTION: signal an error, the user must rename the file first.
;; (c3) If the workspace file is removed, effects by incorporated csets are
;;      ignored.  This is a nop, the current change supersedes.
;; (c4) If the workspace file is added, and the add/removed cset would add a file
;;      by the same name, cancel the file_add, copy the file as a wild file,
;;      inform the user that they need to file_add it under another name.
;;      Might want to just signal an error for now, and make the user cancel the file_add, rename
;;      and re-add if appropriate.  Automated would be nicer, but this'll do at first.
;; (c5) If a wild file is in the way of a file to be added, the wild file is copied, unless it matches
;;      the contents of the repository file which would overwrite it.
;;
;; There is also a recognized problem in allowing only one cset at a time to be added/removed.
;; If bringing in a cset would cause a conflict and I don't have write access to the subsystem
;; in which case I have to bring in both of another's changes to reconcile conflict at the same
;; time.  So we anticipate allowing this, although we didn't when starting this routine.
;; In point of fact, The command to add/remove csets should allow both operations, on multiple csets,
;; at one time.

(defun cset-tuple-union (&optional cset-tuples-left cset-tuples-right)
  "Given two cset-tuple of the form used in workspaces,
   ((satellite-did . cset-did*)*
   return the logical union of them."
  (let ((answer))
    (mapcar (lambda (subsystem)
                (let ((set-union (union (cdr (assoc subsystem cset-tuples-left))
                                        (cdr (assoc subsystem cset-tuples-right)))))
                  (if set-union
                      (push (cons subsystem set-union) answer)
                    )))
            (union (mapcar #'car cset-tuples-left)
                   (mapcar #'car cset-tuples-right)))
    answer))

(defun cmctl/add-and-remove-changes (master-repository-dbpath fsa user-id-specifier query &key hatcheck-ticket hatcheck-value)
  (declare (ignore query))
  ;; A bit of a misnomer, this is actually all the changes we want to be visible
  ;; in the workspace, not just the additional ones.
  (debug-message 5 "Changes to add: ~s" (uri-query/lookup hatcheck-value :new-changes))
  (let* ((workspace-id (uri-query/lookup hatcheck-value :workspace-id))
         (reason (format-in-language (server-locale) nil :change-add-and-remove-reason user-id-specifier workspace-id)))
    (hatcheck/save hatcheck-value hatcheck-ticket)

    (call-with-workspace
     master-repository-dbpath user-id-specifier workspace-id
     :transaction-type :read-write-nonversioned
     :reason reason
     :receiver
     (lambda (workspace-repository workspace-transaction workspace)
       (declare (ignore workspace-repository workspace-transaction))
       (multiple-value-bind (added-cset-tuples removed-cset-tuples)

           (call-with-master-catalog-transaction
            master-repository-dbpath user-id-specifier
            :transaction-type :read-only-compare
            :master-metaversion (workspace/baseline-timestamp workspace)
            :version :latest-version
            :aux-master-metaversion (workspace/baseline-timestamp workspace)
            :aux-version :latest-version
            :reason reason
            :receiver
            (lambda (master-repository master-transaction master-catalog)
              (declare (ignore master-transaction))
              (workspace/call-with-resolved-versionref
               workspace master-repository
               (lambda (product branch version timestamp)
                 (let* ((changes-to-include (map 'list (lambda (change-specifier)
                                                         (repository/resolve-distributed-identifier 
                                                          master-repository change-specifier))
                                                 (uri-query/lookup hatcheck-value :new-changes)))

                        (changes-to-exclude (set-difference (call-with-after-view
                                                             (lambda () (product/change-sets product)))
                                                            changes-to-include))

                        (active-cid-objects (call-with-after-view
                                             (lambda () (version/cid-objects version))))

                        (changes-to-explicitly-include
                         (call-with-after-view
                          (lambda ()
                            (reduce #'cset-tuple-union
                                    (collect 'list
                                             (#M (lambda (scs)
                                                   (map 'list (lambda (element)
                                                                (list (distributed-object-identifier (pstore-list/car element))
                                                                      (pstore-list/cdr element)))
                                                        (pstore-list->lisp-list
                                                         (super-change-set/satellite-change-set-alist scs))))
                                                 (choose-if (lambda (super-change-set)
                                                              (not (member (change-set/cid-object super-change-set) active-cid-objects)))
                                                            (scan 'list changes-to-include))))))))

                        (changes-to-explicitly-exclude
                         (call-with-after-view
                          (lambda ()
                            (reduce #'cset-tuple-union
                                    (collect 'list
                                             (#M (lambda (scs)
                                                   (map 'list (lambda (element)
                                                                (list (distributed-object-identifier (pstore-list/car element))
                                                                      (pstore-list/cdr element)))
                                                        (pstore-list->lisp-list (super-change-set/satellite-change-set-alist scs))))
                                                 (choose-if (lambda (super-change-set)
                                                              (and (member (change-set/cid-object super-change-set) active-cid-objects) t))
                                                            (scan 'list changes-to-exclude)))))))))
                   (call-with-after-view
                    (lambda ()

                      (debug-message 3 "Changes to include ~s" changes-to-include)
                      (debug-message 3 "Changes to exclude ~s" changes-to-exclude)
                      (debug-message 3 "Changes to explicitly include ~s" changes-to-explicitly-include)
                      (debug-message 3 "Changes to explicitly exclude ~s" changes-to-explicitly-exclude)))

                   (master-catalog/update-file-system
                    master-repository-dbpath
                    user-id-specifier
                    master-catalog
                    (make-instance 'logical-file-system
                                   :root (workspace/path workspace)
                                   :underlying-file-system fsa)
                    "workspace"
                    timestamp
                    product
                    branch
                    version
                    timestamp
                    product
                    branch
                    version

                    :VPB-old-added-satellite-cset-dids   (workspace/added-class-cset-tuples workspace)
                    :VPB-old-removed-satellite-cset-dids (workspace/removed-class-cset-tuples workspace)
                    :VPB-new-added-satellite-cset-dids   changes-to-explicitly-include
                    :VPB-new-removed-satellite-cset-dids changes-to-explicitly-exclude)
                   (values changes-to-explicitly-include
                           changes-to-explicitly-exclude))))))

         (debug-message 3 "Added cset-tuples ~s" added-cset-tuples)
         (debug-message 3 "Removed cset-tuples ~s" removed-cset-tuples)
       (remake-instance workspace
                        :added-class-cset-tuples added-cset-tuples
                        :removed-class-cset-tuples removed-cset-tuples)
       t)))))

#||
     (cond ((and (eq require-active-change :REQUIRE-ACTIVE-CHANGE)
                 (null persistent-change-context))
               (conman-error/no-current-change workspace))
           ((and (eq require-active-change :PROHIBIT-ACTIVE-CHANGE)
                 persistent-change-context)
            (conman-error/change-already-active workspace))
           (t
            )))))

     (cond ((and (eq require-active-change :REQUIRE-ACTIVE-CHANGE)
                 (null persistent-change-context))
               (conman-error/no-current-change workspace))
           ((and (eq require-active-change :PROHIBIT-ACTIVE-CHANGE)
                 persistent-change-context)
            (conman-error/change-already-active workspace))
           (t
||#

(defun cmctl/sync-commit (master-repository-dbname user-name query &key hatcheck-ticket hatcheck-value)
  (declare (ignore hatcheck-ticket))
  (let ((workspace-id (uri-query/lookup query :workspace-id))
        (guid         (uri-query/lookup hatcheck-value :workspace-guid)))

    (call-with-workspace
     master-repository-dbname user-name (or workspace-id guid)
     :transaction-type :read-write-nonversioned
     :reason "Mark workspaces synched."
     :receiver (lambda (workspace-repository workspace-transaction workspace)
                 (declare (ignore workspace-repository workspace-transaction))
                 (workspace/set-transition-finished workspace)
                 t))))

(defun cmctl/sync-workspace (master-repository-dbpath fsa user-id-specifier query &key hatcheck-ticket hatcheck-value)
  (let ((workspace-id (uri-query/lookup query :workspace-id))
        (guid         (uri-query/lookup hatcheck-value :workspace-guid))
        (product      (uri-query/lookup hatcheck-value :product-did))
        (directory    (uri-query/lookup hatcheck-value :directory))
        (description  (uri-query/lookup hatcheck-value :description)))
    (declare (ignore product directory description))
    ;; Save it for the next guy
    (hatcheck/save hatcheck-value hatcheck-ticket)

    (call-with-workspace
     master-repository-dbpath user-id-specifier (or workspace-id guid)
     :reason "sync workspace"
     :transaction-type :read-only-nonversioned
     :receiver
     (lambda (workspace-repository workspace-transaction workspace)
       (declare (ignore workspace-repository workspace-transaction))
       (ecase (workspace/transition-mode workspace)
         ((:create :set) (let ((reason (format-in-language (server-locale) nil
                                                           (ecase (workspace/transition-mode workspace)
                                                             ((:create)     :create-workspace-reason)
                                                             ((:set)        :set-workspace-reason))
                                                           user-id-specifier
                                                           workspace-id))
                               (metaversion (workspace/transitional-timestamp workspace))
                               (version-did (workspace/transitional-version-did workspace)))
                           (call-with-master-catalog-transaction
                            master-repository-dbpath user-id-specifier
                            :master-metaversion metaversion
                            :version version-did
                            :reason reason
                            :transaction-type :read-only
                            :receiver (lambda (master-repository master-transaction master-catalog)
                                        (workspace/call-with-resolved-transitional-versionref
                                         workspace master-repository
                                         (lambda (project branch version workspace-timestamp)
                                           (declare (ignore project version workspace-timestamp))
                                           (master-catalog/populate-workspace
                                            master-repository-dbpath user-id-specifier
                                            master-repository master-catalog
                                            (make-instance 'logical-file-system
                                                           :root (workspace/path workspace)
                                                           :underlying-file-system fsa)
                                            reason branch
                                            metaversion
                                            (transaction/cid-set master-transaction))))
                                        nil))))

         ((:regenerate) (let ((reason (format-in-language (server-locale) nil
                                                          :regenerate-workspace-reason
                                                          user-id-specifier
                                                          workspace-id))
                              (metaversion (workspace/transitional-timestamp workspace))
                              (version-did (workspace/transitional-version-did workspace)))
                          (call-with-master-catalog-transaction
                           master-repository-dbpath user-id-specifier
                           :master-metaversion metaversion
                           :version version-did
                           :reason reason
                           :transaction-type :read-only
                           :receiver (lambda (master-repository master-transaction master-catalog)
                                       (declare (ignore master-transaction))
                                       (workspace/call-with-resolved-transitional-versionref
                                        workspace master-repository
                                        (lambda (project branch version workspace-timestamp)
                                          (declare (ignore version))
                                          (master-catalog/extract-project-files-to-disk
                                           master-repository-dbpath user-id-specifier master-catalog
                                           project branch reason workspace-timestamp
                                           (make-instance 'logical-file-system
                                                          :root (workspace/path workspace)
                                                          :underlying-file-system fsa)
                                           :workspace workspace
                                           :clean nil)))
                                       nil))))

         ((:update) (let ((reason (format-in-language (server-locale) nil
                                                      :update-workspace-reason
                                                      user-id-specifier
                                                      workspace-id))
                          (before-metaversion (workspace/baseline-timestamp workspace))
                          (before-version-did (workspace/version-did workspace))
                          (after-metaversion  (workspace/transitional-timestamp workspace))
                          (after-version-did  (workspace/transitional-version-did workspace)))
                      (call-with-master-catalog-transaction
                       master-repository-dbpath user-id-specifier
                       :transaction-type :read-only-compare
                       :reason reason
                       :master-metaversion before-metaversion
                       :version before-version-did
                       :aux-master-metaversion  after-metaversion
                       :aux-version  after-version-did
                       :receiver (lambda (master-repository master-transaction master-catalog)
                                   (declare (ignore master-transaction))
                                   (workspace/call-with-resolved-versionref
                                    workspace master-repository
                                    (lambda (old-project old-branch old-version old-workspace-timestamp)
                                      (workspace/call-with-resolved-transitional-versionref
                                       workspace master-repository
                                       (lambda (new-project new-branch new-version new-workspace-timestamp)
                                         (master-catalog/update-file-system
                                          master-repository-dbpath
                                          user-id-specifier
                                          master-catalog
                                          (make-instance 'logical-file-system
                                                         :root (workspace/path workspace)
                                                         :underlying-file-system fsa)
                                          "workspace"
                                          old-workspace-timestamp
                                          old-project
                                          old-branch
                                          old-version

                                          new-workspace-timestamp
                                          new-project
                                          new-branch
                                          new-version

                                          :VPB-old-added-satellite-cset-dids (workspace/added-class-cset-tuples workspace)
                                          :VPB-old-removed-satellite-cset-dids (workspace/removed-class-cset-tuples workspace)
                                          :VPB-new-added-satellite-cset-dids (workspace/transitional-added-class-cset-tuples workspace)
                                          :VPB-new-removed-satellite-cset-dids (workspace/transitional-removed-class-cset-tuples workspace))))))
                                   nil)))))
         t))))

(defun cmctl/change-create (master-repository-dbname fsa user-name query &key hatcheck-ticket hatcheck-value)
  (declare (ignore query))
  (let ((workspace-id (or (uri-query/lookup hatcheck-value :workspace-id)
                          (uri-query/lookup hatcheck-value :workspace-guid)))
        (change-name  (uri-query/lookup hatcheck-value :change-name))
        (description  (uri-query/lookup hatcheck-value :description)))

    ;; Save it for the next guy
    (hatcheck/save hatcheck-value hatcheck-ticket)

    (unless (valid-name? change-name)
      (conman-error/bad-change-name change-name))

    (let ((reason (format nil "Create cset for user ~s" user-name))
          (system-change-name nil)
          (version-timestamp nil))

      (cmctl/call-with-user-transaction
       master-repository-dbname user-name
       :workspace-id workspace-id
       :reason reason
       :master-metaversion :latest-metaversion
       :transaction-type :read-write/no-change-set
       :receiver
       (lambda (master-repository master-transaction master-catalog workspace wksp-product branch wksp-version workspace-timestamp)
            (declare (ignore wksp-product wksp-version)) ;; ? why do we do this?

         (unless (null (workspace/cset-name workspace))
           (conman-error/workspace-has-active-change workspace))

            (let* ((workspace-root-fs
                    (make-instance 'logical-file-system
                                   :root (workspace/path workspace)
                                   :underlying-file-system fsa))
                   (version (branch/mutable-tip branch))
                   (product (branch/owning-project branch)))
              (setq system-change-name (cmctl/generate-cset-name
                                        change-name user-name (named-object/name product)))
              (when (master-catalog/resolve-qualified-change-set-name master-catalog
                                                                      system-change-name)
                (conman-warning/change-name-in-use system-change-name))
              (product/warn-if-inactive product)
              (setq version-timestamp (version/last-update-timestamp version
                                                                     (transaction/cid-set master-transaction)
                                                                     master-repository))
              (debug-message 5 "Version timestamp:  ~s" version-timestamp)
              (debug-message 5 "Workspace timestamp:  ~s" workspace-timestamp)
              (when (timestamp/time-flows-left-to-right? workspace-timestamp version-timestamp)
                (cmctl/ws-update-and-merge version-timestamp
                                           nil
                                           workspace
                                           workspace-root-fs
                                           master-repository-dbname
                                           master-repository
                                           master-catalog))
              nil))
       :update-workspace
       ;; We're out of master repository scope and back in workspace repository scope.  Create the
       ;; persistent change context and update the workspace.
       (lambda (workspace-repository workspace-transaction workspace)
         (declare (ignore workspace-repository workspace-transaction))
         (versionref/update-timestamp (workspace/baseline-versionref workspace) version-timestamp)
         (workspace/begin-change workspace system-change-name description))))
    t))

(defun cmctl/change-delete (master-repository-dbpath fsa user-name query &key hatcheck-ticket hatcheck-value)
  "Toss out the current change context and revert the changes."
  (declare (ignore fsa query hatcheck-ticket))
  (let* ((workspace-id (uri-query/lookup hatcheck-value :workspace-id))
         (reason (format-in-language (server-locale) nil :change-delete-reason user-name workspace-id)))
    (cmctl/call-with-user-transaction
     master-repository-dbpath user-name
     :workspace-id workspace-id
     :reason reason
     :transaction-type :read-write/no-change-set
     :receiver (lambda (master-repository
                        master-transaction
                        master-catalog
                        workspace project branch version timestamp)
                 (declare (ignore master-repository
                                  master-transaction
                                  master-catalog
                                  workspace
                                  project
                                  branch
                                  version
                                  timestamp))
                 ;; need to revert the workspace here!!!!
                 (constantly nil))
     :update-workspace
     (lambda (workspace-repository workspace-transaction workspace)
       (declare (ignore workspace-repository workspace-transaction))
       (workspace/delete-change-context workspace))))
  t)

(defun cmctl/change-files (master-repository-dbpath fsa user-name query &key hatcheck-ticket hatcheck-value)
  "Examine the user's workspace and determine what files are what."
  (let* ((reason (format nil "Examine user files."))
         (workspace-id (uri-query/lookup query :workspace-id)))
    (hatcheck/save hatcheck-value hatcheck-ticket)
    (cmctl/call-with-user-transaction
     master-repository-dbpath user-name
     :workspace-id workspace-id
     :reason reason
     ;; don't supply master metaversion.  We use the one in the workspace.
     ;; ditto on the version
     :transaction-type :read-only
     :receiver
     (lambda (master-repository master-transaction master-catalog workspace project branch version timestamp)
       (declare (ignore master-repository master-transaction master-catalog project version))
       (unless (workspace/cset-name workspace)
         (conman-error/workspace-has-no-active-change workspace-id))
       (let* ((workspace-root-fs
               (make-instance 'logical-file-system
                              :root (workspace/path workspace)
                              :underlying-file-system fsa))
              (file-info nil))
         ;; In file-info, we will collect a list of tuples.
         ;; The tuples will have different formats depending...
         (csf-branch/map-over-branch-satellite-branches
          master-repository-dbpath user-name branch
          :reason reason
          :satellite-metaversion timestamp
          :satellite-transaction-type :read-only
          :receiver
          (lambda (csf-branch
                   subsystem
                   satellite-repository
                   relative-subdirectory
                   satellite-project
                   satellite-branch
                   satellite-version)
            (declare (ignore csf-branch subsystem satellite-repository satellite-version))

            (file-system/note-progress workspace-root-fs
                                       (format nil "scanning ~s" relative-subdirectory)
                                       nil)

            (let ((files-in-workspace (when (file-system/probe-file workspace-root-fs relative-subdirectory)
                                        (collect 'list
                                                 (map-fn 't (lambda (file)
                                                              (debug-message 5 "Found ~s" file)
                                                              file)
                                                 (transitive-scan (logical-file-system/change-directory
                                                                   workspace-root-fs
                                                                   relative-subdirectory))))))

                  (files-in-repository
                   (collect 'list (rfm-project/scan-files satellite-project)))

                  (files-already-added
                   (collect 'list
                            (choose-if (lambda (addition)
                                          (change-context/applies-to-project? addition satellite-project))
                                        (workspace/scan-file-additions workspace))))

                  (files-already-deleted
                   (collect 'list
                            (choose-if (lambda (deletion)
                                          (change-context/applies-to-project? deletion satellite-project))
                                        (workspace/scan-file-deletions workspace))))

                  (files-already-changed
                   (collect 'list
                            (choose-if (lambda (change)
                                          (change-context/applies-to-project? change satellite-project))
                                        (workspace/scan-file-changes workspace))))

                  (files-already-renamed
                   (collect 'list
                            (choose-if (lambda (rename)
                                          (change-context/applies-to-project? rename satellite-project))
                                        (workspace/scan-file-renames workspace)))))


            (debug-message 3 "Satellite project is ~s" satellite-project)
            (debug-message 3 "Satellite branch is ~s" satellite-branch)


            (debug-message 3 "Files in workspace ~s" files-in-workspace)
            (debug-message 3 "Files in repository ~s" files-in-repository)
            (debug-message 3 "Files already added ~s" files-already-added)
            (debug-message 3 "Files already deleted ~s" files-already-deleted)
            (debug-message 3 "Files already changed ~s" files-already-changed)
            (debug-message 3 "Files already renamed ~s" files-already-renamed)

            (push
             (sort
              (map 'list
                   (lambda (pathname)
                     (cond ((find pathname files-in-workspace 
                                  :test (lambda (name file-descriptor)
                                          ;(debug-message 5 "compare ~s ~s" name file-descriptor)
                                          (equal (namestring name)
                                                 (namestring (file-descriptor/path file-descriptor)))))
                            => (lambda (workspace-fd)
                                 (cond ((find pathname files-in-repository 
                                              :test (lambda (name file-system-element)
                                                      ;(debug-message 5 "compare ~s ~s" name file-system-element)
                                                      (equal (namestring name)
                                                             (namestring
                                                              (file-system-element/relative-path
                                                               file-system-element)))))
                                        => (lambda (repository-file)
                                             ;; It's in the workspace *and* in the repository.
                                             (cond ((find (distributed-object-identifier repository-file) files-already-changed
                                                          :test (lambda (did filechange)
                                                                  ;(debug-message 5 "compare ~s ~s" did filechange)
                                                                  (change-context/changes-file?
                                                                   filechange
                                                                   did)))
                                                    => (lambda (filechange)
                                                         (list (list :unchange)
                                                               (list (distributed-object-identifier satellite-project)
                                                                     (distributed-object-identifier repository-file))
                                                               (file-system/friendly-name
                                                                (merge-pathnames
                                                                 (file-descriptor/path workspace-fd)
                                                                 relative-subdirectory)
                                                                workspace-root-fs))))
                                                   (t (list (list :change :delete)
                                                            (list (distributed-object-identifier satellite-project)
                                                                  (distributed-object-identifier repository-file))
                                                            (file-system/friendly-name
                                                             (merge-pathnames
                                                              (file-descriptor/path workspace-fd)
                                                              relative-subdirectory)
                                                             workspace-root-fs))))))

                                       ;; It is in the workspace, but not in the repository.
                                       ((find pathname files-already-added :test (lambda (name fileadd)
                                                                                   ;(debug-message 5 "compare ~s ~s" name fileadd)
                                                                                   (change-context/adds-file?
                                                                                    fileadd
                                                                                    name
                                                                                    (distributed-object-identifier satellite-project))))
                                        => (lambda (addition)
                                             ;; It's in the workspace, not in the repository, and user
                                             ;; suggested adding it.  Offer to `unadd' it.
                                             (list (list :unadd)
                                                   (list (distributed-object-identifier satellite-project)
                                                         (file-descriptor/path workspace-fd)
                                                         (file-descriptor/path workspace-fd)
                                                         (file-descriptor/content-type workspace-fd))
                                                   (file-system/friendly-name
                                                    (merge-pathnames
                                                     (file-descriptor/path workspace-fd)
                                                     relative-subdirectory)
                                                    workspace-root-fs))))
                                       ;; Offer to add it.
                                       (t (list (list :add)
                                                (list (distributed-object-identifier satellite-project)
                                                      (file-descriptor/path workspace-fd)
                                                      (file-descriptor/path workspace-fd)
                                                      (file-descriptor/content-type workspace-fd))
                                                (file-system/friendly-name
                                                 (merge-pathnames
                                                  (file-descriptor/path workspace-fd)
                                                  relative-subdirectory)
                                                 workspace-root-fs))))))

                           ;; It wasn't in the workspace, maybe it's in the repository?
                           ((find pathname files-in-repository :test (lambda (name file-system-element)
                                                                       ;(debug-message 5 "compare ~s ~s" name file-system-element)
                                                                       (equal (namestring name)
                                                                              (namestring
                                                                               (file-system-element/relative-path
                                                                                file-system-element)))))
                            => (lambda (repository-file)
                                 ;; Wasn't in the workspace, but is in the repository.
                                 ;; See if the user deleted it.
                                 (cond ((find repository-file files-already-deleted :test (lambda (file filedelete)
                                                                                            ;(debug-message 5 "compare ~s ~s" file filedelete)
                                                                                            (change-context/deletes-file?
                                                                                             filedelete
                                                                                             file
                                                                                             (distributed-object-identifier satellite-project))))
                                        => (lambda (deletion)
                                             ;; It's been deleted.  Offer to undelete it.
                                             (list (list :undelete)
                                                   (list (distributed-object-identifier satellite-project)
                                                         (distributed-object-identifier repository-file))
                                                   (file-system/friendly-name
                                                    (merge-pathnames
                                                     (file-system-element/relative-path repository-file)
                                                     relative-subdirectory)
                                                    workspace-root-fs))))
                                       ;; It disappeared from the user's workspace, but he didn't
                                       ;; seem to request a delete.  Offer to delete it.
                                       ;; It should regenerate if user doesn't agree to delete it.
                                       (t (list (list :delete)
                                                (list (distributed-object-identifier satellite-project)
                                                      (distributed-object-identifier repository-file))
                                                (file-system/friendly-name
                                                 (merge-pathnames
                                                  (file-system-element/relative-path repository-file)
                                                  relative-subdirectory)
                                                 workspace-root-fs))))))

                           (t (error "Don't know what to do with this ~s" pathname))))

                   (collect-union
                    (catenate
                     (#M file-descriptor/path (scan 'list files-in-workspace))
                     (#M file-system-element/relative-path (scan 'list files-in-repository)))
                    #'namestring
                    #'equal))
              #'string-lessp
              :key #'third)
             file-info)

            (debug-message 3 "additions ~s" (car file-info)))

            nil))

         ;; Done mapping over each subsystem.
         (debug-message 3 "file-info: ~{~&~s~}" file-info)
         (hatcheck/save (nreverse file-info) hatcheck-ticket)
         nil))

     :update-workspace (constantly nil))
    t))

(defun cmctl/change-selected (master-repository-dbpath fsa user-name query &key hatcheck-ticket hatcheck-value)
  (declare (ignore query))
  (debug-message 3 "Files to add: ~s" (uri-query/lookup hatcheck-value :add-files))
  (debug-message 3 "Files to change: ~s" (uri-query/lookup hatcheck-value :change-files))
  (debug-message 3 "Files to delete: ~s" (uri-query/lookup hatcheck-value :delete-files))
  (let* ((workspace-id (uri-query/lookup hatcheck-value :workspace-id))
         (reason (format nil "Update change context for workspace.")))
    (hatcheck/save hatcheck-value hatcheck-ticket)
    (cmctl/call-with-user-transaction
     master-repository-dbpath user-name
     :workspace-id workspace-id
     :reason reason
     :transaction-type :read-write/no-change-set
     :receiver
     (lambda (master-repository master-transaction master-catalog workspace project branch version timestamp)
       (declare (ignore master-repository
                        master-transaction
                        master-catalog
                        project version))
       ;; sanity check, shouldn't be able to get here
       (unless (workspace/cset-name workspace)
         (conman-error/workspace-has-no-active-change workspace-id))
       (let* ((workspace-root-fs
               (make-instance 'logical-file-system
                              :root (workspace/path workspace)
                              :underlying-file-system fsa)))
         (csf-branch/map-over-branch-satellite-branches
          master-repository-dbpath user-name branch
          :reason reason
          :satellite-metaversion timestamp
          :satellite-transaction-type :read-only
          :receiver
          (lambda (csf-branch
                   subsystem
                   satellite-repository
                   relative-subdirectory
                   satellite-project
                   satellite-branch
                   satellite-version)
            (declare (ignore csf-branch subsystem satellite-branch satellite-version))
            (file-system/note-progress workspace-root-fs
                                       (format nil "scanning ~s" relative-subdirectory)
                                       nil)
            (let ((subdir-file-system (logical-file-system/change-directory
                                       workspace-root-fs
                                       relative-subdirectory)))
              (iterate ((fd (map-fn 'file-descriptor
                                    (lambda (path)
                                      (debug-message 5 "Probing file")
                                      (file-system/probe-file subdir-file-system path))
                                    (#M file-system-element/relative-path
                                        (map-fn 'rfm::file-system-element
                                                (lambda (did)
                                                  (debug-message 5 "Resolving did ~s" did)
                                                  (repository/resolve-distributed-identifier satellite-repository did))
                                                (#M cadr
                                                    (choose-if (lambda (file-change)
                                                                 (eq (car file-change)
                                                                     (distributed-object-identifier satellite-project)))
                                                               (scan 'list
                                                                     (uri-query/lookup hatcheck-value :change-files)))))))))
                (debug-message 5 "changing file ~s" fd)
                (setf (file-descriptor/read-only? fd) nil)))
            nil))
         nil))
     :update-workspace
     ;; We're out of master repository scope and back in workspace repository scope.  Create the
     ;; persistent change context and update the workspace.
     (lambda (workspace-repository workspace-transaction workspace)
       (declare (ignore workspace-repository workspace-transaction))

       (workspace/clear-added-files workspace)
       (dolist (file-addition (uri-query/lookup hatcheck-value :add-files))
         (destructuring-bind (subsystem-project-did repository-file-name file-to-add content-type) file-addition
           (debug-message 5 "Add ~s of type ~s to ~s" file-to-add content-type subsystem-project-did)
           (workspace/change-add-file workspace
                                      (make-instance 'change-context-fileadd
                                                     :affected-project-did subsystem-project-did
                                                     :content-type content-type
                                                     :other-stuff nil
                                                     :pathname repository-file-name))))

       (workspace/clear-changed-files workspace)
       (dolist (file-change (uri-query/lookup hatcheck-value :change-files))
         (destructuring-bind (subsystem-project-did file-did) file-change
           (debug-message 5 "Change file ~s in ~s" file-did subsystem-project-did)
           (workspace/change-modify-file workspace
                                         (make-instance 'change-context-filechange
                                                        :affected-project-did subsystem-project-did
                                                        :file-did file-did
                                                        :other-stuff nil))))
       (workspace/clear-deleted-files workspace)
       (dolist (file-deletion (uri-query/lookup hatcheck-value :delete-files))
         (destructuring-bind (subsystem-project-did file-did) file-deletion
           (debug-message 5 "Delete file ~s in ~s" file-did subsystem-project-did)
           (workspace/change-delete-file workspace
                                         (make-instance 'change-context-fileremove
                                                        :affected-project-did subsystem-project-did
                                                        :file-did file-did
                                                        :other-stuff nil))))

       )))
  t)

(defun cmctl/master-change (master-repository-dbpath fsa user-name query &key hatcheck-ticket hatcheck-value)
    ;; It makes no sense to lock/unlock in here.
    ;; If everything proceeds correctly, the subsystems will be
    ;; unlocked at the end.  If not, the transaction fails and
    ;; the subsystems are unmodified.  So in either case there's
    ;; no visible effect!
  (declare (ignore hatcheck-ticket))
  (let ((workspace-id (or (uri-query/lookup query :workspace-id)
                          (uri-query/lookup query :workspace-guid)
                          (uri-query/lookup hatcheck-value :workspace-id)
                          (uri-query/lookup hatcheck-value :workspace-guid)))
        (reason (format nil "Master change for ~s" user-name)))

    (call-with-workspace
     master-repository-dbpath user-name workspace-id
     :reason reason
     :transaction-type :read-write-nonversioned
     :receiver
     (lambda (workspace-repository workspace-transaction workspace)
       (declare (ignore workspace-repository workspace-transaction))
       (when (null (workspace/cset-name workspace))
         (conman-error/workspace-has-no-active-change workspace))
       (when (null (workspace/cset-description workspace))
         (conman-error/missing-change-description workspace))
       (call-with-master-catalog-transaction
        master-repository-dbpath user-name
        :master-metaversion :latest-metaversion ;; by definition
        :version :latest-version
        :reason reason
        :transaction-type :read-write
        :receiver
        (lambda (master-repository master-transaction master-catalog)
          (declare (ignore master-transaction))
          (workspace/call-with-resolved-versionref
           workspace master-repository
           (lambda (product branch version timestamp)
             (declare (ignore version timestamp))
             (let* ((satellite-change-set-alist '())
                    (file-additions (collect 'list (scan-persistent-vector (workspace/file-additions workspace))))
                    (file-changes   (collect 'list (scan-persistent-vector (workspace/file-changes workspace))))
                    (file-removals (collect 'list (scan-persistent-vector (workspace/file-deletions workspace)))))
               (master-catalog/close-change
                master-repository-dbpath master-catalog user-name reason
                branch
                (make-instance 'logical-file-system
                               :root (workspace/path workspace)
                               :underlying-file-system fsa)
                :latest-metaversion
                (workspace/cset-name workspace)
                :file-additions file-additions
                :file-changes   file-changes
                :file-removals  file-removals
                :added-class-cset-tuples (workspace/added-class-cset-tuples workspace)
                :removed-class-cset-tuples (workspace/removed-class-cset-tuples workspace)
                :promoter (lambda (csf-class satellite-repository satellite-branch satellite-version satellite-change-set)
                            ;; Promote into subsystems visible in master.  We let the caller
                            ;; handle workspace promotion so that we'll update its versionref.
                            (subsystem-satellite/promote-csets
                             satellite-repository satellite-branch
                             ;; Add/remove any workspace cset changes which are to be promoted.
                             ;; NOTE: if PROMOTE isn't set, this has to be done by other subsystem-mapping
                             ;; logic as part of master_change.  Doing this here saves having to re-map
                             ;; satellites, but it's doing the same work.  Also note that we're not
                             ;; altering the subsystem mirror of csets here, just satellite repositories.

                                        ;:added-satellite-change-sets added-VPB-csets
                                        ;:removed-satellite-change-sets removed-VPB-csets
                             :promote-current-cid t
                             :satellite-subsystem-version satellite-version)
                            (push (cons csf-class
                                        (distributed-object-identifier satellite-change-set))
                                  satellite-change-set-alist)))

               (values (list :name        (workspace/cset-name workspace)
                             :description (workspace/cset-description workspace)
                             :satellite-change-set-alist
                             (lisp-list->pstore-list
                              (map 'list (lambda (entry)
                                           (pstore-list-cons (car entry) (cdr entry)))
                                   satellite-change-set-alist)))
                       (lambda (master-change-set)
                         (product/record-relevant-change-set product master-change-set)
                         (master-catalog/promote-cset-to-branch-subsystems
                          master-catalog master-change-set product branch))))))))

       ;; Get the new baseline timestamp.
       ;; (...)
       (workspace/update-for-master-change workspace (timestamp-allocate)); wrong, but...
       t))))

(defun cmctl/call-with-master-lock (master-repository-dbpath user-id-specifier workspace-id thunk)
  (let ((already-locked-p        nil)
        ;(affected-subsystem-dids nil)
        (change-completed-p      nil))
    (unwind-protect
        (progn (multiple-value-setq (already-locked-p affected-subsystem-dids)
                 (cmctl/master-lock master-repository-dbpath user-id-specifier workspace-id))
               (multiple-value-prog1 (funcall thunk)
                 (setq change-completed-p t)))
      "unlocking master"
      (debug-message 2 "~:[Performing~;Not performing~] master unlock."
                              (and (eq already-locked-p :already-locked)
                                   (not change-completed-p)))
      (unless (and (eq already-locked-p :already-locked)
                   (not change-completed-p))
        (cmctl/master-unlock master-repository-dbpath user-id-specifier workspace-id)))))

(defun cmctl/master-lock (master-repository-dbpath user-id-specifier workspace-id)
  "Lock all subsystems affected by the change in the current workspace.
   Note that 'change in the current workspace' must also reflect the effects
   of change_add and change_remove, which aren't necessarily accompanied by a change context.
   This is because we DO have to lock the subsystems into which we'll be promoting change-sets.

   We represent this lock by two bits of information:
   1) A lock record on the subsystem object(s).
   2) A lock record on the workspace which is locking the subsystems.

   Returns TWO values:
   1) T if successful and locks were not previously held,
      or :already-locked if successful but locks *were* previously held (i.e. was a NOP or upgraded locks),
      otherwise this function signals a terminating condition and lock status remains unchanged
   2) The list of subsystem-dids affected by the session context.
      **NOTE**: this list isn't necessarily suitable for use by logic which is trying to process
      subsystems affected by the change-context (i.e. files altered) and NOT the workspace VPB context
      (i.e. csets added/removed).  It includes subsystems affected by both change context and VPB actions."
  (let ((reason (format nil "master lock for workspace ~s" workspace-id)))
    (call-with-workspace
     master-repository-dbpath user-id-specifier workspace-id
     :reason reason
     :transaction-type :read-write-nonversioned
     :receiver
     (lambda (workspace-repository workspace-transaction workspace)
       (declare (ignore workspace-repository workspace-transaction))
       (let ((return-value t)
             (locked-subsystem-dids '()))
         (call-with-master-repository-transaction
          master-repository-dbpath user-id-specifier
          :transaction-type :read-write
          :reason reason
          :master-metaversion :latest-metaversion ;; was (workspace/baseline-timestamp workspace)
          :version t
          :receiver (lambda (master-repository master-transaction)
                      (declare (ignore master-repository master-transaction))
                      (break)))


         (workspace/note-locked-subsystems workspace locked-subsystem-dids)
         (values return-value affected-subsystem-dids))))))

(defun cmctl/master-unlock (master-repository-dbpath user-id-specifier workspace-id)
  (let ((reason (format nil "master unlock for workspace ~s" workspace-id)))
    (call-with-workspace
     master-repository-dbpath user-id-specifier workspace-id
     :reason reason
     :transaction-type :read-write-nonversioned
     :receiver
     (lambda (workspace-repository workspace-transaction workspace)
       (declare (ignore workspace-repository workspace-transaction)) 
       (call-with-master-repository-transaction
        master-repository-dbpath user-id-specifier
        :transaction-type :read-write
        :master-metaversion :latest
        :version t
        :receiver (lambda (master-repository master-transaction)
                    (declare (ignore master-repository))
                    (iterate ((subsystem (map-fn 't
                                                 (lambda (did)
                                                   (repository/resolve-distributed-identifier
                                                    master-transaction
                                                    did))
                                                 (scan 'list (workspace/locked-subsystems workspace)))))
                      (subsystem/release-lock subsystem))))
       (workspace/note-locked-subsystems workspace nil)))))

(defun cmctl/list-changes (master-repository-dbpath user-id)
  (call-with-master-catalog-transaction
   master-repository-dbpath user-id
   :master-metaversion :latest-metaversion
   :version :latest-version
   :reason (format nil "List repository changes.")
   :transaction-type :read-write ;; kludge
   :receiver
   (lambda (master-repository master-transaction master-catalog)
     (declare (ignore master-transaction master-catalog))
     (let ((answer
            (collect 'list
                     (map-fn '(values t t t t)
                             (lambda (cid)
                               (multiple-value-bind (why comparison-timestamp cset-info basis)
                                   (csf/core::repository/cid-information master-repository cid)
                                 (declare (ignore cset-info basis))
                                 (list (csf/core::cid-object/find-or-create cid)
                                       comparison-timestamp
                                       why)))
                             (scan-range :from 1 :upto (csf/core::repository/last-allocated-cid master-repository))))))
       (constantly answer)))))

(defparameter +test-temp-directory+ #p"C:\\TEMP\\")

(defun test-create-master ()
  (ensure-directories-exist (merge-pathnames #p"ChangeSafe Blank\\" +test-temp-directory+))
  (cmctl/create-master
   (test-make-conman-request)
   (dbpath/parse (namestring (merge-pathnames "ChangeSafe Blank\\" +test-temp-directory+)))
   "repository"))

(defparameter +test-user-name+ "jrm"
  "The name of the user who is setting up the test database.")

(defparameter +test-user-description+ "foo")

(defparameter +test-user-homedir+ "C:\\Home\\Jrm\\"
  "The home directory of the user who is setting up the test database.")

(defun test-make-conman-request (&key 
                                 (locale (parse-locale "en-us"))
                                 (client-platform (server-platform))
                                 (current-directory 
                                  (parse-client-directory-namestring
                                   (namestring +test-temp-directory+)
                                   :winnt))
                                 (home-directory (user-homedir-pathname))
                                 (verbosity 4)
                                 (user-name :nobody)
                                 (master-repository
                                  (dbpath/parse 
                                   (namestring 
                                    (merge-pathnames "ChangeSafe Blank\\repository.ydm"
                                                     +test-temp-directory+))))
                                 branch-specifier)
   (make-instance 'conman-request
                  :locale locale
                  :client-platform client-platform
                  :current-directory current-directory
                  :home-directory home-directory
                  :verbosity verbosity
                  :user-name user-name
                  :master-repository master-repository
                  :branch-specifier branch-specifier))

(defun test-create-user ()
  (cmctl/create-user (test-make-conman-request)
                     +test-user-name+ +test-user-description+))

(defun test-create-product (user-did)
  (cmctl/create-product 
   (test-make-conman-request :home-directory (parse-client-directory-namestring
                                              (namestring +test-user-homedir+)
                                              :winnt)
                             :user-name user-did)
   "ChangeSafe"
   "Main Branch"
   "The ChangeSafe product.")
  (cmctl/create-product
   (test-make-conman-request :home-directory (parse-client-directory-namestring
                                              (namestring +test-user-homedir+)
                                              :winnt)
                             :user-name user-did)
   "myWMS"
   "Main Branch"
   "The Warehouse Management System product."))

(defparameter +test-changesafe-classes+ 
  '(
    ("allegroserve" "Class for allegroserve." "allegroserve")
    ("ansi-series" "Class for ansi-series." "ansi-series")
    ("build" "Class for build." "build")
    ("conman" "Class for conman." "conman")
    ("core" "Class for core." "core")
    ("defsystem" "Class for defsystem." "defsystem")
    ("Delivery" "Class for Delivery." "Delivery")
    ("Documentation" "Class for documentation." "docs")
    ("java" "Class for java." "java")
    ("misc" "Class for misc." "misc")
    ("readme" "Class for readme." "readme")
    ("rfm" "Class for rfm." "rfm")
    ("server" "Class for server." "server")
    ("source-compare" "Class for source-compare." "source-compare")
    ("Source" "Class for source." "src")
    ("test-data" "Class for test-data." "test-data")
    ("tests" "Class for tests." "tests")
    ("utility" "Class for utility." "utility")
    ("vm" "Class for vm." "vm")
    ("web-content" "Class for web-content." "web-content")
    ("win32" "Class for win32." "win32")
    ))

(defun test-create-class (user-did)
  (let ((request (test-make-conman-request
                  :home-directory (parse-client-directory-namestring
                                   (namestring +test-user-homedir+)
                                   :winnt)
                  :user-name user-did)))
    (dolist (class +test-changesafe-classes+)
      (cmctl/create-class
       request
       (car class)
       (cadr class)
       (parse-client-directory-namestring (caddr class) :winnt)))))

(defun test-list-classes ()
  (cmctl/list-classes
   (test-make-conman-request :home-directory (parse-client-directory-namestring
                                              (namestring +test-user-homedir+)
                                              :winnt)
                             :user-name +test-user-name+)))

(defun test-create-subsystem (user-did)
  (let ((request (test-make-conman-request
                  :home-directory (parse-client-directory-namestring
                                   (namestring +test-user-homedir+)
                                   :winnt)
                  :user-name user-did)))
      (dolist (class 
               '(
                 ("allegroserve" "Subsystem for allegroserve." "allegroserve")
                 ("ansi-series" "Subsystem for ansi-series." "ansi-series")
                 ("build" "Subsystem for build." "build")
                 ("conman" "Subsystem for conman." "conman")
                 ("core" "Subsystem for core." "core")
                 ("defsystem" "Subsystem for defsystem." "defsystem")
                 ("Delivery" "Subsystem for Delivery." "Delivery")
                 ("Documentation" "Subsystem for ChangeSafe documentation."
                                  "changesafe-documentation")
                 ("Documentation" "Subsystem for myWMS documentation."
                                  "mywms-documentation")
                 ("java" "Subsystem for java." "java")
                 ("misc" "Subsystem for misc." "misc")
                 ("readme" "Subsystem for readme." "readme")
                 ("rfm" "Subsystem for rfm." "rfm")
                 ("server" "Subsystem for server." "server")
                 ("source-compare" "Subsystem for source-compare." "source-compare")
                 ("Source" "Subsystem for source." "src")
                 ("test-data" "Subsystem for test-data." "test-data")
                 ("tests" "Subsystem for tests." "tests")
                 ("utility" "Subsystem for utility." "utility")
                 ("vm" "Subsystem for vm." "vm")
                 ("web-content" "Subsystem for web-content." "web-content")
                 ("win32" "Subsystem for win32." "win32")
                 )+test-changesafe-classes+)
        (destructuring-bind (class description subsystem-name) class
            ;; now create a subsystem for it
            (cmctl/create-subsystem request
                                    class
                                    (concatenate 'string subsystem-name "-subsystem")
                                    description)))))

(defun test-list-subsystems (user-did)
  (cmctl/list-subsystems
   (test-make-conman-request
    :home-directory (parse-client-directory-namestring
                     (namestring +test-user-homedir+)
                     :winnt)
    :user-name user-did)))

(defun test-create-subscriptions (user-did)

    (let ((request (test-make-conman-request
                    :home-directory (parse-client-directory-namestring 
                                     (namestring +test-user-homedir+)
                                     :winnt)
                    :user-name user-did)))
      (dolist (subsystem '(
                           "allegroserve-subsystem"
                           "ansi-series-subsystem"
                           "build-subsystem"
                           "conman-subsystem"
                           "core-subsystem"
                           "defsystem-subsystem"
                           "Delivery-subsystem"
                           "changesafe-documentation-subsystem"
                           "java-subsystem"
                           "misc-subsystem"
                           "readme-subsystem"
                           "rfm-subsystem"
                           "server-subsystem"
                           "source-compare-subsystem"
                           "test-data-subsystem"
                           "tests-subsystem"
                           "utility-subsystem"
                           "vm-subsystem"
                           "web-content-subsystem"
                           "win32-subsystem"
                           ))
        (cmctl/subscribe-to-subsystem request "ChangeSafe" subsystem :write))
      (dolist (subsystem '(
                           "mywms-documentation-subsystem"
                           "src-subsystem"
                           ))
        (cmctl/subscribe-to-subsystem request "myWMS" subsystem :write))))

(defun test-create-workspace (user-did)
  (cmctl/create-workspace
   (test-make-conman-request :user-name user-did
                             :home-directory (parse-client-directory-namestring
                                              (namestring +test-user-homedir+) :winnt)
                             :branch-specifier "ChangeSafe")
   (make-instance 'lisp-file-system)
   (parse-client-directory-namestring
    (namestring (merge-pathnames "workspace" +test-temp-directory+))
    :winnt)
   "A workspace for initial load of ChangeSafe."
   :latest))

(defun cmctl/list-workspaces (master-repository-db-path)
  (call-with-workspace-repository-transaction
   master-repository-db-path :nobody
   :reason "list workspaces"
   :transaction-type :read-only-nonversioned
   :receiver (lambda (workspace-repository workspace-transaction)
               (declare (ignore workspace-transaction))
               (collect 'list
                        (map-fn 't (lambda (workspace)
                                     (list (workspace/id workspace)
                                           (workspace/owner-id workspace)
                                           (workspace/path workspace)
                                           (workspace/transition-mode workspace)
                                           (workspace/cset-name   workspace)
                                           (workspace/description workspace)))
                                (scan-workspaces workspace-repository))))))

(defun xyzzy ()
  (with-cached-open-repositories ()
    (test-create-master)
    (let ((user-did (test-create-user)))
      (format t "~&USER-DID: ~s" user-did)
      (test-create-product user-did)
      (test-create-class user-did)
      (test-create-subsystem user-did)
      (test-create-subscriptions user-did))))


#||

(defvar *cmctl-dot-conman-file-name*  "REPOSITORY:;PRD-CSF"
  "The repository relative name of the .csf file.  Note how it is encoded.
   This variable may be bound by the test suite to avoid trashing the real
   .csf files.

   THIS VARIABLE SHOULD NOT BE BOUND BY ANY CODE BUT THE TEST SUITES.")

(defconstant +cmctl-dot-conman-file-name-default+ "REPOSITORY:;PRD-CSF"
  "the default value of *cmctl-dot-conman-file-name* - should be exactly the same
   in the source code.")

(defun cmctl-read-dot-csf-file (cm-session-context
                                directory-spec
                                results-cm-session-context
                                &key ws-id dbpath)
  "Reads the .csf file in the directory (or gives error if problems).  It puts the info
   into the RESULTS-CM-SESSION-CONTEXT so it will not mess up the regular cm-session-context.

   This only reads the basics of what can be put into a .csf file.  If you need more, add
   to the list below

   If the ws-id or dbpath args are given, they will be compared with what we extracted
   from the .csf file."
  (debug-message 3 "cmctl-read-dot-csf-file entered.")
  (let* ((reason (format nil "Read .csf contents from ~s" directory-spec))
         (logical-file-system (logical-file-system-create
                               directory-spec
                               (cm-session-context-file-system-agent cm-session-context)))
         (file-contents (tokenize-string-cli
                         (cmctl-retrieve-abstract-file-contents cm-session-context
                                                                logical-file-system
                                                                *cmctl-dot-conman-file-name*)))
         (next-is-lead-token t)
         (token-type nil)
         (last-token nil))
    (declare (ignore reason))
    (unless file-contents
      (conman-signal-error *cm-returns-error-empty-abstract-file*
                           "The (~s) file must exist for the workspace and have something in it."
                           *cmctl-dot-conman-file-name*))

    (dolist (token file-contents t)
      (debug-message 4 ".csf element: ~a (~s)~%"
                     token (if next-is-lead-token "leading" "trailing"))
      (if next-is-lead-token
          (progn (cond ((string-equal token *cm-cli-keyword-master-syntax*)
                        (setq token-type :dbpath)
                        (setq next-is-lead-token nil))
                       ((string-equal token *cm-cli-keyword-ws-id-syntax*)
                        (setq token-type :ws-id)
                        (setq next-is-lead-token nil))
                       (t ;; ignore other tokens
                        )
                       )
                 (setq last-token token))
        (progn (ecase token-type
                 (:dbpath
                  (cm-session-context-add results-cm-session-context :repository-name token))
                 (:ws-id
                  (let ((integer (parse-integer token :junk-allowed t)))
                    (unless integer
                      (conman-signal-error *cm-returns-error-workspace-id-must-be-integer*
                                           "The workspace id value (~s) must be an integer"
                                           token))
                    (cm-session-context-add results-cm-session-context :ws-id integer)))
                 )
               (setq next-is-lead-token t))))
    (unless next-is-lead-token
      (conman-signal-error *cm-returns-error-too-few-arguments*
                           "~s must be followed by a value in the ~s file."
                           last-token *cmctl-dot-conman-file-name*))

    ;; (maybe) check for good ws-id and dbpath
    (when ws-id
      (unless (= ws-id (cm-session-context-ws-id results-cm-session-context))
        (conman-signal-error *cm-returns-error-workspace-id-mismatch*
                             "Workspace identifier mis-match (db: ~s vs. disk: ~a) for ~s"
                             ws-id (cm-session-context-ws-id results-cm-session-context)
                             (namestring directory-spec))))

    (when dbpath
      (unless (objects-equalp (dbpath cm-session-context dbpath)
                              (dbpath cm-session-context
                                       (cm-session-context-repository-name results-cm-session-context)))
        (conman-signal-error *cm-returns-error-database-name-mismatch*
                             "Database name mis-match (arg: ~s vs. old-ws: ~s) for ~s"
                             dbpath
                             (cm-session-context-repository-name results-cm-session-context)
                             (namestring directory-spec))))
    ))

(defun cmctl-guarantee-branch-not-frozen (branch &key (tip (branch-get-latest-mutable-version branch)))
  "Signal an error if the tip of the branch is immutable (frozen).
   Otherwise, return the branch.

   Optional TIP keyword argument may be supplied if you happen to
   already know the TIP version.

   This is designed to make routines that should not operate on
   frozen branches have a centralized signaling point."
  (when (null tip)
    (conman-signal-error *cm-returns-error-release-is-frozen*
                         "Operation not permitted because this release is frozen."))
  branch)

(defun conman-branch-get-mutable-tip (branch)
  "Return the latest version of the BRANCH, unless the branch is frozen.
   In that case, signal an error."
  (let ((tip (branch-get-latest-mutable-version branch)))
    (cmctl-guarantee-branch-not-frozen branch :tip tip)
    tip))

(defun cmctl-version-spec-not-implemented (cm-session-context &key time-done release-done)
  "Temporary routine to throw an error where version specs on a branch are accepted parms, but not
   are not yet implemented.  TIME-DONE is true if the caller has implemented time support and
   we shouldn't flag it here.
   Similarly, RELEASE-DONE is true if the caller has implemented release support."
  ;; *FINISH*: use label, release, and time-spec.  Every caller of this routine.
  (when (cm-session-context-label-name cm-session-context)
    (conman-signal-error *cm-returns-error-unimplemented*
                         "LABEL specifier is not yet supported."))
  (unless (or release-done
              (equal
               (or (cm-session-context-release-name cm-session-context)
                   +pc-main-branch-name+)
               +pc-main-branch-name+))
    (conman-signal-error *cm-returns-error-unimplemented*
                         "RELEASE specifier is not yet supported in this command context."))
  (when (and (not time-done)
             (cm-session-context-time-spec cm-session-context))
    (conman-signal-error *cm-returns-error-unimplemented*
                         "TIME specifications are not yet implemented in this command context.")))


;;; Busy redirection

(defconstant +cmctl-busy-timeout-default+ 2
  "initial, default value for *busy-timeout*")

(defparameter *busy-timeout* +cmctl-busy-timeout-default+
  "Timeout to consider a server busy.  If the server hasn't gotten to the request in this amount
   of time, signal a busy redirect.")

(define-condition busy-redirect
    (condition)
  ;; This arg isn't used, but maybe we want it in the future?
  ((queue-length :initarg :queue-length
                 :reader busy-redirect/queue-length))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "The server has issued a BUSY REDIRECT"))))

(defun call-with-busy-redirection (conman-session-context reason setup-options thunk)
  "If the server is not busy, or if busy deferrals are not allowed for this request,
   invoke thunk.  This may cause enqueueing of the process.

   If the server *is* busy, and busy deferrals are allowed, signal for a busy deferral."
  (let ((queue-length nil)
        ;; Open all the repositories just before calling the user's thunk.
        (wrapped-thunk (lambda ()
                           (cmctl-boilerplate-setup conman-session-context reason setup-options)
                           (funcall thunk))))

    (tail-labels ((redirect-noise (message)
                    (if *conman-server-log*
                        (conman-server-log-string
                         (cm-session-context-user-name conman-session-context)
                         'busy-redirect
                         (format nil "Process ~s ~s.~&~s" *current-process* message core::*txn-mutex-queue*))
                      (debug-message 0 "Process ~s ~s.~&~s" *current-process* message core::*txn-mutex-queue*)))

                  (prohibit-redirect ()
                    (handler-bind ((awaiting-txn-mutex
                                    (lambda (condition)
                                      (redirect-noise "enqueued for service, no redirect")
                                      (signal condition)))

                                   (granted-txn-mutex
                                    (lambda (condition)
                                      (redirect-noise "granted txn-mutex")
                                      (signal condition))))

                      (call-with-txn-mutex wrapped-thunk nil #'timeout-error)))

                  (timeout-error ()
                    (error "Timeout continuation called when timeout was nil.  This should not be possible."))

                  (allow-redirect ()
                    (handler-bind ((awaiting-txn-mutex
                                    (lambda (condition)
                                      ;; Well, we didn't get it right away, so find out how
                                      ;; many are in the queue.
                                      (redirect-noise "enqueued for busy test")
                                      (setq queue-length (awaiting-txn-mutex-condition/number condition))
                                      (continue))) ;; Don't bother reporting to user, we're only pausing.

                                   (granted-txn-mutex
                                    (lambda (condition)
                                      (declare (ignore condition))
                                      (redirect-noise "obtain mutex during busy test")
                                      ;; Hey, we got the mutex within the busy timeout.
                                      ;; Well, let's not waste it.
                                      (continue))) ;; Don't bother reporting to user.
                                   )
                      (call-with-txn-mutex wrapped-thunk *busy-timeout* #'server-is-busy)))

                  (server-is-busy ()
                    (redirect-noise "signalling busy redirect")
                    ;; called when busy-timeout has expired.
                    (signal 'busy-redirect :queue-length queue-length)
                    (error "No handler found for busy redirect.")))

      (if (and (cm-session-context-allow-busy-redirect conman-session-context)
               (numberp *busy-timeout*)
               (>= *busy-timeout* 0))
          (allow-redirect)
        (prohibit-redirect)))))


;; Caching control & handling
(defun cmctl-boilerplate-setup (cm-session-context-or-rep-name reason setup-options)

;;; JRM SAYS:  I NEVER WANT TO SEE BOILERPLATE!
;;; Abstract this somehow.

  "This function should be called once from every function callable from cm-cli.lsp.
   It sets up whatever stuff that must be done for every command at the beginning.

   The process should have the txn-mutex before this function is called.

   Since it must be called from a location that has the mutex, you should probably call this
   first thing when seizing the mutex.  The standard wrappers will do that if necessary."
  (declare (ignore cm-session-context-or-rep-name setup-options))
  (debug-message 2 "Command: ~a" reason)

  (when (or *repository-cache-limit* *repository-cache-timeout*)
    (error "*repository-cache-limit* (~a) and *repository-cache-timeout* (~a) must have nil values"
           *repository-cache-limit* *repository-cache-timeout*))
  (unless (and *enable-repository-caching* *repository-cache-active*)
    (error "Server caching must be enabled. *enable-repository-caching* and *repository-cache-active* are nil"))

  ;; No longer needed due to  close-recently-opened-repositories
  ;; open all of the files in a repository
  ;;(cmctl-open-all-repositories cm-session-context-or-rep-name reason setup-options)
  )

(defun cmctl-open-all-repositories (cm-session-context-or-rep-name reason setup-options)
  "Opens a master-repository, all satellite dbs, and the workspace (semi-persistent) db.
   returns the master-repository object.

   This function is needed so that all DBs in a repository set (master, satellite, ws)
   are opened outside of a transaction (particularly an ODI transaction).  If these files
   are opened inside an ODI transaction and there is azn error, then the DB handles
   become stale and corrupted and are unusable.

   Every cmctl function callable from cm-cli as well as those top level functions in the
   report server code (cm-reports.lsp and cm-http.lsp) needs to call this routine before
   doing any transactions.

   The special variables core:*repository-cahce-limit* and core:*repository-cache-timeout* must
   be nil so that we never actually close the files.  Furthermore *enable-repository-caching*
   and *repository-cache-active* must be non-nil to enable the cache at all.

   Keep in mind that since with-open-repository also closes (puts in the cache) the repository before
   exiting the macro, all this function is doing is opening the physical files so that they can be
   found later in the cache when someone opens the individual repository.

   Note that a master transaction is done to get the list."
  (declare (ignore reason))
  (let* ((csc-is-name (not (typep cm-session-context-or-rep-name 'cm-session-context)))
         (master-repository-dbpath (etypecase cm-session-context-or-rep-name
                                      (dbpath cm-session-context-or-rep-name)
                                      (cm-session-context
                                       (cm-session-context-repository-name cm-session-context-or-rep-name)))))

    (when (member :create-master-repository setup-options)
      (return-from cmctl-open-all-repositories nil))

    (unless (or csc-is-name
                (and (member :pretend-to-work setup-options)
                     (null (cm-session-context-repository-name cm-session-context-or-rep-name))))

      ;; don't bother to check except when it is a session-context and we are not creating a new Repository
      ;; or when it is Joe's sleepy command (cmctl-pretend-to-work)
      (cm-session-context-require cm-session-context-or-rep-name :repository-name))

    (when (or *repository-cache-limit* *repository-cache-timeout*)
      (error "*repository-cache-limit* (~a) and *repository-cache-timeout* (~a) must have nil values"
             *repository-cache-limit* *repository-cache-timeout*))
    (unless (and *enable-repository-caching* *repository-cache-active*)
      (error "Server caching must be enabled. *enable-repository-caching* and *repository-cache-active* are nil"))

    ;; *FINISH* PWW: should we add code to not bother to open the subrepositories everytime the master
    ;; is repository is opened (i.e. it is already in the cache), the clean up code at the end of this
    ;; function will probably need to be beefed up.
    ;; Also in this situation, we need to be aware that a Class_Create command will instantiate a new
    ;; satellite db.  In that situation, a r/o server (like the report server) will need to open the
    ;; satellite db even though the master db is already opened.  This means we will probably have to
    ;; maintain a list of files for each repository and open anything not on the list.
    ;; for the moment, we avoid this problem by always opening every file (with the attendent issues
    ;; around performance that this approach brings).

    (let* ((dblist nil)
           (all-reps-opened nil)
           (master-opened nil))
      (when master-repository-dbpath
        (unwind-protect
            (progn
              (debug-message 3 "cmctl-open-all-repositories Open Master: ~a mode: ~a"
                             master-repository-dbpath
                             (conman-repository-open-mode-from-operating-mode))
              ;; Ok to use with-open-repository because we are caching.
              (with-open-repository (master-repository
                                     master-repository-dbpath
                                     (conman-repository-open-mode-from-operating-mode)
                                     :error-if-not-cached nil ;; only spots where nil is ok
                                     :repository-type :master
                                     :if-exists :open
                                     :if-does-not-exist :error)
                (setq master-opened t)

                ;; get all of the satellite dbs
                (with-cm-master-txn (master-repository master-catalog nil
                                     :read-only "list satellite repositories")
                  (setq dblist (repository-get-satellite-repository-pathstrings master-repository)))

                ;; loop over the DBs, opening each one
                (dolist (rep-name dblist)
                  (let ((sat-name (dbpath-merge rep-name master-repository-dbpath)))
                    ;; Ok to use with-open-repository because we are caching.
                    (with-open-repository (satellite-repository
                                           sat-name
                                           (conman-repository-open-mode-from-operating-mode)
                                           :error-if-not-cached nil ;; only spots where nil is ok
                                           :repository-type :satellite
                                           :if-exists :open
                                           :if-does-not-exist :error)
                      (declare (ignore satellite-repository))
                      (debug-message 4 "cmctl-open-all-repositories Open Satellite: ~a" sat-name)
                      )))

                ;; get the workspace (semi-persistent) db
                (let ((ws-name (conman-workspace-repository-name master-repository-dbpath)))
                  ;; Ok to use with-open-repository because we are caching.
                  (with-open-repository (workspace-repository
                                         ws-name
                                         (conman-repository-open-mode-from-operating-mode)
                                         :error-if-not-cached nil ;; only spots where nil is ok
                                         :repository-type :basic
                                         :if-exists :open
                                         :if-does-not-exist :error)
                    (declare (ignore workspace-repository))
                    (debug-message 4 "cmctl-open-all-repositories Open Workspace: ~a" ws-name)
                    ))

                ;; if we get this far, all of the repositories (except the master) have been successfully opened
                ;; and closed, so get rid of the cleanup list
                (setq all-reps-opened t)

                ))

          ;; do any needed cleanup
          (when (and master-opened (not all-reps-opened))
            #||
            ;; do repository clean up by force closing the master
            ;; this will ensure that the next time thru we will again try to open all of the repositories

            PWW: I don't think we actually need to do the following since, at the moment, we are taking
            no short cuts to avoid opening all the subrepositories every time this fuinction is called

            ;; Ok to use with-open-repository because we are caching.
            (with-open-repository (master-repository
                                   repository-name
                                   (conman-repository-open-mode-from-operating-mode)
                                   :error-if-not-cached nil ;; only spots where nil is ok
                                   :if-exists :open
                                   :if-does-not-exist :error
                                   :repository-type :master)
              (repository-close master-repository :force t))
            ||#
            )))
      )
    ))

;; testing stub
(defun cmctl-pretend-to-work (cm-session-context reporter)
  (call-with-busy-redirection cm-session-context "cmctl-pretend-to-work" '(:pretend-to-work)
    (lambda ()
        (funcall reporter))))


;;; Standard txn wrappers

(defun cmctl-call-with-workspace (conman-session-context
                                  &key reason
                                       txn-mode
                                       (require-workspace t)
                                       (transition-action :error)
                                       receiver)
  "Open the workspace repository for update, begin a transaction, resolve the workspace and
   invoke receiver on two arguments, the workspace repository and the workspace.
   The workspace is closed when receiver exits.

   TXN-MODE is one of :read-write or :read-only and controls the type of transaction.

   If REQUIRE-WORKSPACE is NIL then it's not an error if no workspace is found."
  (assert (member txn-mode '( :read-write :read-only)))
  (call-with-busy-redirection conman-session-context reason nil
    (lambda ()
        (with-open-workspace-repository (ws-repository
                                         (conman-workspace-repository-name
                                          (cm-session-context-repository-name
                                           conman-session-context)))
          (with-workspace-repository-txn (ws-repository txn-mode reason)
            (let* ((ws-id (cm-session-context-ws-id conman-session-context))
                   (workspace
                    (let (id path ws)
                      (if (workspace-identifier? ws-id)
                          (setq id ws-id)
                        (setq path ws-id))
                      (setq ws (workspace-resolve-spec conman-session-context ws-repository
                                                       :ws-id id
                                                       :ws-path path
                                                       :user-name (cm-session-context-user-name conman-session-context)
                                                       :error-if-missing require-workspace))
                      (if (typep ws 'workspace) ws nil))))
              (if workspace
                  (cm-session-context-add conman-session-context :ws-id (workspace-identifier workspace))
                ;; Should we clear the WS-ID from the session context if the workspace lookup failed?
                )
              (cond ((and workspace
                          (eq transition-action :error)
                          (workspace-in-transition? workspace))
                     (conman-error-workspace-in-transition))
                    ((and (eq transition-action :error-if-not)
                          (or (null workspace)
                              (not (workspace-in-transition? workspace))))
                     (conman-error-workspace-not-in-transition))
                    (t
                     (funcall receiver ws-repository workspace)))))))))



(defun cmctl-call-with-master-repository-txn (cm-session-context
                                              &key reason
                                                   txn-mode
                                                   (make-cset :make-cset)
                                                   receiver)
  "Begin a transaction within the master repository.

   TXN-MODE should be :read-only or :read-write.
   MAKE-CSET should be :MAKE-CSET or :NO-CSET
   (Unless you really know what you're doing, this had better be :MAKE-CSET)

   Invoke receiver on three arguments, the master-repository-name,
   the master-repository and the master-catalog."
  (assert (member txn-mode '( :read-write :read-only)))
  (let ((master-repository-name (cm-session-context-repository-name cm-session-context)))
    (call-with-busy-redirection cm-session-context reason nil
      (lambda ()
        (with-cm-master-repository (master-repository master-repository-name)
          (ecase make-cset
            (:make-cset
             (with-cm-master-txn (master-repository master-catalog cm-session-context
                                  txn-mode
                                  reason)
               (funcall receiver master-repository-name master-repository master-catalog)))
            (:no-cset
             (with-cm-master-txn-NO-CSET (master-repository master-catalog cm-session-context
                                          txn-mode
                                          reason)
               (funcall receiver master-repository-name master-repository master-catalog)))))))))

(defun cmctl-call-with-user-txn (cm-session-context
                                 &key reason
                                      txn-mode
                                      require-active-change
                                      (transition-action :error)
                                      receiver
                                      (update-workspace #'cmctl-call-with-user-txn-update-workspace-no-op))
  "Establishes a standard workspace and master-repository transaction for
   standard user commands.

   REQUIRE-ACTIVE-CHANGE is one of :REQUIRE-ACTIVE-CHANGE,
   :PROHIBIT-ACTIVE-CHANGE, or NIL.

   RECEIVER is a function of 6 arguments, the workspace repository, the
   workspace and the change context, master repository name, master repository,and master-catalog plucked
   from the master repository.

   UPDATE-WORKSPACE is a function of three arguments (workspace repository, workspace object,
   and change context).  This is generally used to update the workspace on disk.
   If you don't need to do anything in the UPDATE-WORKSPACE phase, use
   CMCTL-CALL-WITH-USER-TXN-UPDATE-WORKSPACE-NO-OP."
  (assert (member txn-mode '( :read-write :read-only)))
  (cmctl-call-with-workspace-change-context cm-session-context
    :reason reason
    :txn-mode txn-mode
    :require-active-change require-active-change
    :transition-action transition-action
    :receiver
    (lambda (ws-repository workspace change-context)
      (multiple-value-prog1
          (cmctl-call-with-master-repository-txn cm-session-context
              :reason reason
              :txn-mode :read-only
              :receiver
              (lambda (master-repository-name master-repository master-catalog)
                (funcall receiver
                         ws-repository
                         workspace change-context
                         master-repository-name master-repository master-catalog)))
        (funcall update-workspace ws-repository workspace change-context)
        ))))

(defun cmctl-call-with-user-txn-update-workspace-no-op (workspace-repository workspace change-context)
  "This can be used as the UPDATE-WORKSPACE argument to CMCTL-CALL-WITH-USER-TXN
   if there's noting useful to do."
  (declare (ignore workspace-repository workspace change-context))
  nil)

;;;
;;; Control layer interfaces.  Note that much of the context is passed in cm-session-context.
;;; Rather than document all the required session context in doc strings, we often leave it to
;;; the manifest requirements enforced by CM-SESSION-CONTEXT-REQUIRE in each control interface routine.
;;;

(defun cmctl-create-master (cm-session-context directory-dbpath master-name)
  "Populate and optionally create a database directory named by DIRECTORY-DBPATH, which
   must be a DBPATH, so that it has a master ChangeSafe repository distinguished
   by the name 'master-name', which must also be a string.

   If the directory doesn't exist and FORCE-P is nil, signal an error.
   If the directory doesn't exist and FORCE-P is true, create the directory.
   Note that this routine will only create leaf directories, it won't create an entire path.
   Also note that it is unable to test for the existence of the directory or database
   files on remote database servers.

   (cm-session-context)
   MASTER-NAME is used as an embedded token in a generated master repository name within the
   directory specified by DIRECTORY-DBPATH.
   It may also be used to construct satellite repository names as well.

   If the directory already has a master repository with the indicated name, signal an error.
   Note that we are unable to test for this condition on remote database servers.
   If there are any problems creating the master repository or the directory, signal an error.

   Returns T if successful, or throws an exception."
  (check-type directory-dbpath dbpath)
  (let* ((fs (lisp-file-system-create))
         (dir-pathname (dbpath-pathname directory-dbpath))
         ;; Recall non-local names have syntax HOST:absolute-path and
         ;; specify a non-local machine running an Objectsore server
         (local-p (dbpath-local-p directory-dbpath)))

    ;; We can only ensure directory exists if it's on the local machine.
    (if local-p
        (unless (file-system-directory-exists-p fs dir-pathname)
          (conman-error-invalid-database-directory "Database directory ~S does not exist" dir-pathname))
      #||
      ;; This is commented out per Jack's request, and Dave said "because there is nothing the user
      ;; can do to avoid the warning" It would be nice, Dave said, if an informational message were
      ;; given, but we have no infrastructure in place currently for that.
      (conman-signal-warning *cm-returns-warning-default*
                             "Unable to verify that database directory exists on remote server ~A."
                             (db-host-name (dbpath-host directory-dbpath)))
      ||# )
    ;; Have the directory, check for presence of master db.
    (let ((mdb-dbpath (dbpath-merge (make-pathname :name master-name
                                                     :type +repository/master-file-type+
                                                     :defaults nil)
                                      directory-dbpath)))
      (declare (ignore repository-open-type))
      (if local-p
          (when (file-system-probe-file fs (dbpath-pathname mdb-dbpath))
            (conman-signal-error *cm-returns-error-master-repository-already-exists*
                                 "A CM master repository named ~s already exists in directory ~s."
                                 master-name dir-pathname))
        #||(conman-signal-warning *cm-returns-warning-default*
                                  "We are unable to determine if the specified database already exists because is ~
           resides on a remote server.")
        ||# )
      (cm-session-context-add cm-session-context :repository-name mdb-dbpath)
      (call-with-busy-redirection cm-session-context "cmctl-create-master" '(:create-master-repository)
        (lambda ()
            ;; Create master repository.
            ;; Ok to use with-open-repository because we are caching.
            (with-open-repository (repository mdb-dbpath :update
                                   :repository-type :master
                                   :error-if-not-cached t ;; t is ok for create
                                   :if-exists :error
                                   :if-does-not-exist :create)
              ;; Create the master catalog (it gets stashed in a well known root in this operation).
              ;; We have a cset transaction here, but not a vm-txn, there is no project which manages
              ;; changes to the master.  Perhaps a bad idea, perhaps not.
              (with-repository-txn (*txn-context* repository nil
                                    :reason "Create master repository and master catalog."
                                    :txn-mode :read-write)
                (master-catalog-create repository)
                ;; Initialize the workspace repository as well.
                (workspace-repository-create (conman-workspace-repository-name mdb-dbpath)))
              t))))))

(defun cmctl-validate-product-configuration-argument-interaction
    (base-product-configuration-name copy-none-p copy-all-p copy-file)
  "Validate some of the parameters bound for cmctl-create-product-configuration which have
   various semantic interdependencies.  Return NIL or signal appropriate errors."
  ;; Must specify -COPY* if -BASE is given
  (when base-product-configuration-name
    (unless (or copy-none-p copy-all-p copy-file)
      (conman-signal-error
       *cm-returns-error-base-product-requires-subsystem-copy-specifier*
       "The ~a command requires one of ~a, ~a, or ~a
        when the ~a argument is specified."
       *cm-cli-command-product-create*
       *cm-cli-switch-base-pc-copy-none-syntax*
       *cm-cli-switch-base-pc-copy-all-syntax*
       *cm-cli-keyword-base-pc-copy-file-syntax*
       *cm-cli-keyword-base-pc-name-syntax*
       )))
  ;; Must specify -BASE if -COPY* is given.
  (when (and (or copy-none-p copy-all-p copy-file)
             (not base-product-configuration-name))
    (conman-signal-error
     *cm-returns-error-copy-parameter-requires-base-parameter-for-product-create*
     "The ~a command requires the ~a argument if any of ~a, ~a, or ~a are specified."
     *cm-cli-command-product-create*
     *cm-cli-keyword-base-pc-name-syntax*
     *cm-cli-switch-base-pc-copy-none-syntax*
     *cm-cli-switch-base-pc-copy-all-syntax*
     *cm-cli-keyword-base-pc-copy-file-syntax*))
  ;; COPY parms are mutually exclusive
  (when (> (count-if #'identity (list copy-none-p copy-all-p copy-file)) 1)
    (conman-signal-error
     *cm-returns-error-mutually-exclusive-arguments*
     "~a, ~a, and ~a arguments to the ~a command are mutually exclusive."
     *cm-cli-switch-base-pc-copy-none-syntax*
     *cm-cli-switch-base-pc-copy-all-syntax*
     *cm-cli-keyword-base-pc-copy-file-syntax*
     *cm-cli-command-product-create*
     ))
  )

(defun cmctl-retrieve-product-config-copy-list (cm-session-context copy-file-spec)
  "If COPY-FILE-SPEC is not nil, attempt to open, read, and return its contents as a list of strings,
   delimited by whitespace.

   If the copy file is specified, an error is signalled if we are unable to read its contents.
   COPY-FILE-SPEC must be NIL, or a string specifying a client path string accessible via FILE-SYSTEM.

   The copy file need not lie within a workspace!"
  (let ((fd (file-system-probe-file (cm-session-context-file-system-agent cm-session-context) copy-file-spec)))
    (unless fd
      (conman-signal-error *cm-returns-error-non-existent-disk-file*
                           "Unable to locate -copy-file ~S on disk" copy-file-spec))
    ;; Guaranteed by above layers
    ;;   (unless (file-descriptor-is-file? fd)
    ;;     (conman-signal-error *cm-returns-error-invalid-disk-file-spec*
    ;;                          "-copy-file ~S is not a file." copy-file-spec))
    (let ((content-type (file-descriptor-content-type fd)))
      (call-with-file-descriptor-content
          fd (file-descriptor-record-terminator fd)
        (lambda (content)
            (ecase content-type
              (:zero   (conman-signal-error *cm-returns-error-empty-product-copy-file*
                                            "-copy-file ~S is empty." copy-file-spec))
              (:binary (conman-signal-error *cm-returns-error-binary-product-copy-file*
                                            "-copy-file ~S appears to have binary content." copy-file-spec))
              (:text   (mapcan #'split-string (vi-stream-as-list content)))))))))

(defun cmctl-create-product-configuration (cm-session-context
                                           base-product-configuration-name
                                           copy-none-p copy-all-p copy-file
                                           description ;string or nil
                                           )
  "Create a product configuration and return true, or signal an appropriate error.
   See CM-CLI-PRODUCT-CREATE documentation for more details. "
  ;; These two checks will disappear when implementation of the command is complete.
  (cmctl-version-spec-not-implemented cm-session-context :time-done t :release-done t)
  ;;*FINISH* use release and label specs
  ;; Argument validation.
  (cm-session-context-require cm-session-context :repository-name :pc-name)
  (check-type base-product-configuration-name (optional string))
  (check-type description (optional string))
  (cmctl-validate-product-configuration-argument-interaction
   base-product-configuration-name copy-none-p copy-all-p copy-file)
  (when (and (cm-session-context-time-spec cm-session-context)
             (not copy-all-p))
    (conman-signal-error *cm-returns-error-copy-all-required*
                         "The -copy-all switch is required when a time is specified."))
  (let* ((product-configuration-name (cm-session-context-pc-name cm-session-context))
         (reason (format nil "Create the ~a product" product-configuration-name)))
    (cmctl-call-with-master-repository-txn cm-session-context
        :reason reason
        :txn-mode :read-write
        :receiver
        (lambda (master-repository-name master-repository master-catalog)
          (declare (ignore master-repository))
          ;; Verify that the product-configuration-name doesn't already exist.
          ;; Also resolve the base-product-configuration-name if it was specified.
          (let ((base-pc nil)
                (new-pc nil))
            (when (master-catalog-pc-name-lookup master-catalog product-configuration-name)
              (conman-error-product-configuration-name-exists product-configuration-name))
            (when base-product-configuration-name
              (unless (setq base-pc (master-catalog-pc-name-lookup master-catalog
                                                                   base-product-configuration-name))
                (conman-error-invalid-base-product base-product-configuration-name)))
            ;; Create the product configuration.  It will create new subsystems if necessary.
            (setq new-pc
                  (master-catalog-pc-create master-catalog master-repository-name product-configuration-name
                                            base-pc
                                            (and base-pc
                                                 ;; This is correct in this case, since we
                                                 ;; have the release name of the base-branch
                                                 ;; in the cm-session-context
                                                 (or (cm-session-context-release-name cm-session-context)
                                                     +pc-main-branch-name+))
                                            description
                                            (cm-session-context-time-spec cm-session-context)
                                            copy-all-p copy-none-p
                                            (when copy-file
                                              (cmctl-retrieve-product-config-copy-list cm-session-context copy-file))))
            ;; Promote the changes to the PC so we can see them.
            ;; Note that in this case, promoting onto the main branch of the created PC is the correct
            ;; behavior.  We want it to see it's subsystems, after all.
            (master-catalog-promote-transaction-cset master-catalog reason new-pc
                                                     (pc-lookup-branch-ok new-pc +pc-main-branch-name+))))))
  t)

(defun cmctl-class-create (cm-session-context subdirectory description)
  "Create a ChangeSafe class, which is really just a project.

   Classes are rfm::project objects managed in satellite repositories, and which are referenced
   by product configurations via subsystems in the master repository.

   When we create a class, we create a satellite repository in the same directory as the master,
   and chain it to the master.

   The subdirectory associated with the satellite-project-ref is OS NEUTRAL.

   See CM-CLI-CLASS-CREATE documentation for more details."
  (cm-session-context-require cm-session-context :repository-name :class-name)
  (check-type description (optional string))
  (let ((class-name (cm-session-context-class-name cm-session-context)))
    (unless (cmctl-valid-name? class-name)
      (conman-signal-error *cm-returns-error-invalid-name*
                           "The name given, ~S, is invalid." class-name))
    (cmctl-call-with-master-repository-txn cm-session-context
        :reason (format nil "Create class ~s" class-name)
        :txn-mode :read-write
        :receiver
        (lambda (master-repository-name master-repository master-catalog)
            (with-cm-master-metaversion ()      ; latest
              ;; Create a satellite repository which will hold a project named CLASS-NAME,
              ;; and link up a subproject reference to the resulting repository/project.
              ;; This routine verifies that the class does not already exist.
              (master-catalog-create-satellite-project-ref
               master-catalog master-repository master-repository-name class-name
               (pathname->logical-pathname subdirectory)
               description)
              t)))))

#||
(defun cmctl-main-branch-only (cm-session-context)
  "Temporary hack routine to avoid update of the reference area for
   branches that are not the main branch.  Remove when Bill finishes
   killing off reference areas.  Returns nil if this is not the
   main branch, otherwise true."
  (let ((x (cm-session-context-release-name cm-session-context)))
    (or (null x)
        (string-equal x +pc-main-branch-name+)
        )))
||#

(defun cmctl-map-over-every-dbname-for-master (do-it master-repository-name master-repository &key (add-mdb t))
  "This funcalls doit with the name of every database associated with
   the passed session context.  NIL is returned.
   It calls DOIT once for each database associated with the master-repository including
   the master db and the semi-persistent (workspace) db."
  (when add-mdb
    (funcall do-it master-repository-name))
  (funcall do-it (conman-workspace-repository-name master-repository-name))
  (mapc (lambda (satellite)
          (funcall do-it (dbpath-merge satellite master-repository-name)))
        (repository-get-satellite-repository-pathstrings master-repository)))

(defun cmctl-make-dbpaths-file (master-repository-name master-repository filename)
  "Write a file with all the dbnames for a given master repository"
  (with-open-file (stream filename
                   :if-exists :supersede
                   :direction :output)
    (cmctl-map-over-every-dbname-for-master
     (lambda (dbname)
       (write-line (dbpath-namestring dbname) stream))
     master-repository-name master-repository))
  nil)

(defvar *osbackup* "osbackup" "Path to the osbackup command")

(defun cmctl-database-backup (cm-session-context image-file)
  (cm-session-context-require cm-session-context :repository-name)
  (let* ((import-filename (format nil "~a.dbn" image-file))
         (cmdline (format nil "~a -a -I ~a -f ~a"
                          *osbackup* import-filename image-file))
         )
    (cmctl-call-with-master-repository-txn cm-session-context
        :reason "database dump"
        :txn-mode :read-only
        :receiver
        (lambda (master-repository-name master-repository master-catalog)
          (declare (ignore master-catalog))
          (cmctl-make-dbpaths-file
           (merge-pathnames
            ;; Discard the host component.
            (dbpath-pathname master-repository-name)
            (make-pathname :type
                           core:+repository/master-file-type+))
           master-repository
           import-filename)))
    (multiple-value-bind (res exit)
        (run-subprocess cmdline)
      (declare (ignore res))
      (unless (zerop exit)
        (conman-signal-error *cm-returns-error-osbackup-nonzero-exitcode*
                             "osbackup exited with code ~s" exit)))
    t))

(defun cmctl-subsys-create (cm-session-context local-file-system
                            base-product-configuration-name base-subsystem-name
                            description subdirectory)
  "Create a subsystem and install a reference to it in the specified product configuration.
   See CM-CLI-SUBSYS-CREATE documentation and HIERARCHY.TXT for more details."
  (declare (ignore local-file-system) (special *cm-cli-keyword-base-subsystem-name-syntax*))
  (cm-session-context-require cm-session-context :repository-name :class-name :pc-name :subsystem-name)
  (check-type description (optional string))
  (check-type base-product-configuration-name (optional string))
  (check-type base-subsystem-name (optional string))

  ;; Unimplemented items... *FINISH*
  (cmctl-version-spec-not-implemented cm-session-context
                                      :time-done t
                                      :release-done t) ;*FINISH*: use label

  (let* ((class-name (cm-session-context-class-name cm-session-context))
         (subsystem-name (cm-session-context-subsystem-name cm-session-context))
         (from-release-name (or (cm-session-context-release-name cm-session-context)
                                +pc-main-branch-name+))
         (source-subsystem nil))
    (cmctl-call-with-master-repository-txn cm-session-context
        :reason (format nil "Create subsystem ~s" subsystem-name)
        :txn-mode :read-write
        :receiver
        (lambda (master-repository-name master-repository master-catalog)
            (declare (ignore master-repository))
            ;; Resolve the product configuration.
            ;; An error will be signalled if we are unable to resolve them.
            (let* ((pc (master-catalog-pc-name-lookup master-catalog cm-session-context :error-if-missing t))
                   (pc-branch
                    ;;; ***** Someday there will be a way for the user
                    ;;; to tell us which branch they want to add the
                    ;;; subsystem to, until then, assume Main.
                    (pc-lookup-branch pc +pc-main-branch-name+))
                   (pre-existing-subsystem (master-catalog-subsystem-name-lookup
                                            master-catalog cm-session-context :error-if-missing nil)))
              ;; See if the subsystem already exists, signal an error if this happens.
              (when pre-existing-subsystem
                (conman-signal-error *cm-returns-error-subsystem-already-exists*
                                     "A subsystem named ~s already exists." subsystem-name))
              ;; If the subsystem inherits from another, find that one
              (when (or base-subsystem-name base-product-configuration-name)
                (cond (base-subsystem-name
                       (setq source-subsystem
                             (master-catalog-subsystem-name-lookup master-catalog base-subsystem-name
                                                                   :error-if-missing t))
                       ;; SOURCE-SUBSYSTEM must have the same class as
                       ;; the subsystem being created
                       (unless (string-equal (subsystem-class-name source-subsystem)
                                             class-name)
                         (conman-signal-error
                          *cm-returns-error-subsystem-classes-differ*
                          "The specified base subsystem ~s is not of class ~s."
                          base-subsystem-name class-name)))
                      (base-product-configuration-name
                       (let ((base-pc (master-catalog-pc-name-lookup master-catalog
                                                                     base-product-configuration-name
                                                                     :error-if-missing t)))
                         ;; Should probably use date-time metaversion context for subsystem name
                         ;; examination here, but it's ok and even possibly preferable to resolve subsystem
                         ;; and class names in the latest metaversion given the time-based uniqueness
                         ;; constraints in the system for classes and subsystems.
                         (setq source-subsystem
                               (progn ;;with-version ((branch-get-most-recent-version (pc-lookup-branch-ok base-pc from-release-name)) :version-context :pc)
                                 (pc-find-subsystem-for-class (pc-lookup-branch-ok base-pc from-release-name)
                                                              class-name :error-if-missing t)))))))
              ;; Create the subsystem.  Note that this will verify that the class exists.
              (master-catalog-create-subsystem master-catalog master-repository-name
                                               subsystem-name class-name pc-branch
                                               description source-subsystem
                                               (cm-session-context-time-spec cm-session-context)
                                               ;; Created subsystem initially has write access
                                               '(:WRITE)
                                               :subdirectory (if subdirectory
                                                                 (pathname->logical-pathname subdirectory)
                                                               (if source-subsystem
                                                                   (subsystem-relative-subdirectory
                                                                    source-subsystem)
                                                                 nil)))
              ;; Promote the changes to the PC (and branch) to which the subsystem is added so that we can
              ;; see the new subsystem.
              (master-catalog-promote-transaction-cset
               master-catalog description pc
               ;; YES: the main branch, unless we add a "to-release-name" capability
               (pc-lookup-branch-ok pc +pc-main-branch-name+))

              ))))
  t)

(defun cmctl-subsys-users-list (cm-session-context product-name release-name mode from-where)
  "Implements the subsys_users_list ChangeSafe command.

   PRODUCT-NAME names the product configuration whose usage pattern is being modified.

   MODE is one of :READ if the product is not allowed to modify the subsystem it
   will be using, :WRITE if it is allowed to modify it, or NIL if the product will no
   longer be using the subsystem.

   FROM-WHERE identifies the subsystem that the product will be using.  It is either
   a string naming a subsystem or a list consisting of a product name and a class name.
   FROM-WHERE should be NIL if MODE is NIL."
  (check-type product-name string)
  (check-type release-name string)
  (check-type mode (member nil :read :write))
  (cm-session-context-require cm-session-context :repository-name :pc-name :release-name)
  (let (from-where-string
        from-subsystem-name from-product-name class-name)
    (if (listp from-where)
        (progn
          (setq from-product-name (first from-where))
          (setq class-name (second from-where))
          (check-type from-product-name (or null string))
          (check-type class-name string)
          (assert (null (cddr from-where))))
      (progn
        (setq from-subsystem-name from-where)
        (check-type from-subsystem-name string)))
    #||
    (format *trace-output* "~&*** from-subsystem-name ~s~&    from-product-name ~s~&    class-name ~s"
            from-subsystem-name from-product-name class-name)
    ||#
    (cmctl-call-with-master-repository-txn cm-session-context
        :reason "change subsys users list"
        :txn-mode :read-write
        :receiver
        (lambda (master-repository-name master-repository master-catalog)
          (declare (ignore master-repository-name master-repository))
          (with-cm-master-metaversion ()        ; latest
            (let* ((pc (master-catalog-pc-name-lookup master-catalog product-name
                                                      :error-if-missing t))
                   (pc-branch (project-lookup-branch-name pc release-name))
                   new-subsystem
                   old-subsystem)
              (when (null pc-branch)
                (conman-signal-error
                 *cm-returns-error-invalid-release-name*
                 "Product ~a has no release named ~a."
                 product-name release-name))
              (cmctl-guarantee-branch-not-frozen pc-branch)
              (when mode
                ;; We're not dropping a subsystem
                (setq new-subsystem
                      (if from-subsystem-name
                          (let ((subsystem
                                 (master-catalog-subsystem-name-lookup master-catalog
                                                                       from-subsystem-name
                                                                       :error-if-missing t)))
                            (setq class-name (subsystem-class-name subsystem))
                            subsystem)
                        (let ((base-pc (master-catalog-pc-name-lookup master-catalog
                                                                      from-product-name
                                                                      :error-if-missing t)))
                          (assert (stringp class-name))
                          ;; *** There should be a way for the user to specify the branch.
                          (progn ;;with-version ((branch-get-most-recent-version (project-get-main-branch base-pc)) :version-context :pc)
                            #||
                            (format *trace-output* "~&*** looking up subsystem for ~s in ~s"
                                    class-name (pc-name base-pc))
                            ||#
                            (pc-find-subsystem-for-class (project-get-main-branch base-pc) class-name
                                                         :error-if-missing t))))))
              #||
              (format *trace-output* "~&*** new-subsystem is ~a ~s"
                      (when new-subsystem (subsystem-name new-subsystem))
                      new-subsystem)
              ||#
              (setq old-subsystem
                    (cond (class-name
                           (progn ;;with-version (pc-version :include-current t :version-context :pc)
                             (pc-find-subsystem-for-class pc-branch class-name
                                                          :error-if-missing nil)))
                          (from-subsystem-name
                           (master-catalog-subsystem-name-lookup master-catalog
                                                                 from-subsystem-name
                                                                 :error-if-missing t))))
              #||
              (format *trace-output* "~&*** old-subsystem is ~a ~s"
                      (when old-subsystem (subsystem-name old-subsystem))
                      old-subsystem)
              ||#
              (when new-subsystem
                (setq from-where-string (subsystem-name new-subsystem)))
              (progn ;;with-version (pc-version :include-current t)
                (let ((pc-subsystems (pc-get-subsystem-list pc-branch))
                      (subsystems-changed-p nil))
                  (flet ((unuse-subsystem (subsystem)
                           (unless (member subsystem pc-subsystems)
                             (conman-signal-error
                              *cm-returns-error-pc-subsystem-not-found*
                              (format nil "the product does not use subsystem ~a"
                                      (subsystem-name subsystem))))
                           ;; We're currently in the master metaversion context
                           (setq subsystems-changed-p t)
                           (setq pc-subsystems (remove subsystem pc-subsystems))
                           (let ((subscription
                                  (subsystem-find-subscription-for-pc-branch
                                   subsystem pc-branch)))
                             (when subscription
                               (subsystem-remove-subscription subsystem subscription))))
                         (use-subsystem (subsystem mode)
                           ;; We're currently in the master metaversion context
                           (when (and (eq mode :write)
                                      (subsystem-frozen? subsystem))
                             (conman-signal-error
                              *cm-returns-error-subsystem-is-frozen*
                              "That subsystem is frozen, so write permission cannot be granted."))
                           (let ((existing-subscription
                                  (subsystem-find-subscription-for-pc-branch subsystem
                                                                             pc-branch))
                                 (new-modes (ecase mode
                                              (:read nil)
                                              (:write '(:write)))))
                             (if existing-subscription
                                 (set-subsystem-subscriber-modes existing-subscription
                                                                 new-modes)
                               ;; We need to create a subscriber object
                               (subsystem-add-subscriber subsystem pc-branch
                                                         :subscriber-modes new-modes)))
                           (setq subsystems-changed-p t)
                           (setq pc-subsystems (adjoin subsystem pc-subsystems))))
                    (with-cm-master-metaversion ()
                      (if mode
                          (progn
                            ;; From the spec: "If the product is added to the
                            ;; list, it is automatically removed from the list of
                            ;; any other subsys in the same class.  A product can
                            ;; only use one member of a class."
                            (when (and old-subsystem (neq old-subsystem new-subsystem))
                              (unuse-subsystem old-subsystem))
                            (use-subsystem new-subsystem mode))
                        (unuse-subsystem old-subsystem))))
                  (when subsystems-changed-p
                    (set-pc-subsystems pc-branch
                                       (sort pc-subsystems #'string-lessp :key #'subsystem-name)))))
              ;; Promote the changes to the PC so we can see them.
              (master-catalog-promote-transaction-cset master-catalog
                                                       (format nil "subsys-users-list ~a ~a from ~a"
                                                               product-name mode
                                                               from-where-string)
                                                       pc pc-branch))))))
  t)

(defun cmctl-subsys-inherit-from (cm-session-context
                                  ;; The inheriting subsystem
                                  subsystem-identifier-subsys-or-product
                                  subsystem-identifier-nil-or-class
                                  ;; the inherited subsystem
                                  from-subsystem-how ; :SUBSYSTEM or :PRODUCT
                                  from-subsystem-identifier ;the name
                                  ;; the mode
                                  inheritance-mode)
  "Implementation of subsys_inherit_from.

   INHERITANCE-MODE is :ALL, :SELECT or NIL.  NIL means that the
   subsystem should no longer inherit from the from-subsystem.

   If SUBSYSTEM-IDENTIFIER-NIL-OR-CLASS is NIL then
   SUBSYSTEM-IDENTIFIER-SUBSYS-OR-PRODUCT is the name of a subsystem
   otherwise it's the name of a product, which in conjunction with
   the class named by SUBSYSTEM-IDENTIFIER-NIL-OR-CLASS identifies the
   inheriting subsystem.

   FROM-SUBSYSTEM-HOW determines the interpretation of
   FROM-SUBSYSTEM-IDENTIFIER as either a subsystem name or product name."
  (cm-session-context-require cm-session-context :repository-name)
  (check-type subsystem-identifier-subsys-or-product string)
  (check-type subsystem-identifier-nil-or-class (or null string))
  (check-type from-subsystem-how (member :subsystem :product))
  (check-type from-subsystem-identifier string)
  (check-type inheritance-mode (member nil :all :select))

  (cmctl-call-with-master-repository-txn cm-session-context
      :reason "change subsystem inheritance"
      :txn-mode :read-write
      :receiver
      (lambda (master-repository-name master-repository master-catalog)
          (declare (ignore master-repository-name master-repository))
          (with-cm-master-metaversion ()        ; latest
            (let (;; some of these might be NIL
                  (subsystem-name (cm-session-context-subsystem-name cm-session-context))
                  (class-name (cm-session-context-class-name cm-session-context))
                  (pc-name (cm-session-context-pc-name cm-session-context))
                  subsystem from-subsystem)
              (setq subsystem
                    (if subsystem-name
                        (master-catalog-subsystem-name-lookup master-catalog subsystem-name
                                                              :error-if-missing t)
                      (let ((pc (master-catalog-pc-name-lookup master-catalog pc-name)))
                        (progn ;;with-version ((branch-get-most-recent-version (project-get-main-branch pc)) :version-context :pc)
                          (pc-find-subsystem-for-class (project-get-main-branch pc) class-name
                                                       :error-if-missing t)))))
              (setq from-subsystem
                    (ecase from-subsystem-how
                      (:subsystem
                       (master-catalog-subsystem-name-lookup master-catalog from-subsystem-identifier))
                      (:product
                       (let ((inherit-from-pc
                              (master-catalog-pc-name-lookup master-catalog from-subsystem-identifier))
                             (class-name (subsystem-class-name subsystem)))
                         (progn ;;with-version ((branch-get-most-recent-version (project-get-main-branch inherit-from-pc)) :version-context :pc)
                           (pc-find-subsystem-for-class (project-get-main-branch inherit-from-pc) class-name
                                                        :error-if-missing t))))))
              (let ((from-class (subsystem-class-name from-subsystem))
                    (to-class (subsystem-class-name subsystem)))
                (unless (string= from-class to-class)
                  (conman-signal-error
                   *cm-returns-error-subsystem-classes-differ*
                   "The 'from' subsystem's class, \"~a\", doesn't match the 'to' subsystem's class, \"~a\"."
                   from-class to-class)))
              (subsystem-set-inheritance-mode subsystem from-subsystem inheritance-mode)))))
  t)

(defun cmctl-write-conman-rc (cm-session-context directory-spec
                              &key (if-exists :error) (if-does-not-exist :create))
  "Write interesting parts of cm-session-context to a .csf file in the directory
   indicated by DIRECTORY-SPEC and accessed via FILE-SYSTEM.  This should become the canonical
   function for creating/updating the .csf file, though it will undoubtedly need work.

   Note that only portions of CM-SESSION-CONTEXT which are least-common-denominator in
   nature are written, so that we don't put things in the .csf file which would fail to
   work with some commands.

   IF-EXISTS should be either :ERROR or :SUPERSEDE, and defaults to :ERROR.
   This is atypical of most :IF-EXISTS uses, but we don't necessarily want to overwrite
   a user's .csf file.

   IF-DOES-NOT-EXIST should be either :CREATE, :OPEN, or :ERROR, and defaults to :CREATE.

   Returns the cm-session-context."
  (declare (special *cm-cli-keyword-master-syntax* ;forward references defined in cm-cli.lsp
                    *cm-cli-keyword-pc-name-syntax*
                    *cm-cli-keyword-ws-id-syntax*
                    *cm-cli-keyword-release-name-syntax*))
  ;; The perhaps poorly named LOGICAL-FILE-SYSTEM-CHANGE-DIRECTORY function actually CREATES a logical file
  ;; system mapped to a specific directory.

  ;; We don't use cm-session-context-workspace-rooted-file-system here because we may not
  ;; have a workspace root yet.
  (let ((logical-file-system (logical-file-system-create directory-spec
                                                         (cm-session-context-file-system-agent cm-session-context))))
    (with-file-system-stream (stream logical-file-system *cmctl-dot-conman-file-name*
                              :direction :output
                              :element-type 'character
                              :record-terminator (file-system-record-terminator logical-file-system)
                              :if-exists if-exists
                              :if-does-not-exist if-does-not-exist)
      (flet ((doit (key val)
               (when val
                 (file-system-write-line logical-file-system stream
                                         ;; Beware, not all vals are strings! Some are numbers.
                                         ;; (this was once a concatenate)
                                         (format nil "~a ~a" key val)))))
        (doit *cm-cli-keyword-master-syntax*
              (dbpath-namestring (cm-session-context-repository-name cm-session-context)))
        ;; NOTE: we do NOT want to write out the product name or the release name to the
        ;; .csf file, since those would be redundantly stored in the workspace, leading
        ;; to inconsistent data.  Worse, the user could type them in wrongly on the command
        ;; line, leading to insanity. Finally, renaming of the release and product could cause
        ;; additional headaches.  The ws-id and master repository name are sufficient to find
        ;; the workspace, which allows finding the baseline-version, which finds branch and
        ;; then pc.  Further, the (current) names can be found for the pc and branch.

        ;;      (doit *cm-cli-keyword-pc-name-syntax* (cm-session-context-pc-name cm-session-context))
        (doit *cm-cli-keyword-ws-id-syntax* (cm-session-context-ws-id cm-session-context))
        ;;      (doit *cm-cli-keyword-release-name-syntax* (cm-session-context-release-name cm-session-context))
        )))
  cm-session-context)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ws_sync_commit and ws_sync_abort
(defun cmctl-ws-sync-commit (cm-session-context)
  (cmctl-call-with-workspace cm-session-context
    :reason "workspace sync commit"
    :txn-mode :read-write
    :transition-action :error-if-not
    :receiver (lambda (ws-repository workspace)
                  (declare (ignore ws-repository))
                  (workspace-set-transition-finished workspace)
                  t)))

(defun cmctl-ws-sync-abort (cm-session-context)
  (cmctl-call-with-workspace cm-session-context
    :reason "workspace sync abort"
    :txn-mode :read-write
    :transition-action :error-if-not
    :receiver (lambda (ws-repository workspace)
                  (declare (ignore ws-repository))
                  (workspace-abort-transition workspace)
                  t)))

(defun cmctl-ws-sync (cm-session-context server-relative)
  (cm-session-context-require cm-session-context :user-name :repository-name :ws-id)
  (cmctl-call-with-workspace-change-context cm-session-context
    :reason "workspace sync"
    :txn-mode :read-only
    :transition-action :error-if-not
    :receiver
    (lambda (ws-repository workspace change-context)
        (declare (ignore ws-repository))
        (let ((reason (format nil "~s workspace for user ~s"
                              (workspace-transition-mode workspace)
                              (cm-session-context-user-name cm-session-context))))
          (cmctl-call-with-master-repository-txn cm-session-context
              :reason reason
              :txn-mode :read-only
              :receiver
              (lambda (master-repository-name master-repository master-catalog)
                  (ecase (workspace-transition-mode workspace)
                    ((:CREATE :SET)
                     (let ((master-metaversion (workspace-transitional-timestamp workspace)))
                       (with-cm-master-metaversion (master-metaversion)
                         (let* ((master-metaversion-cid-set (txn-context-cid-set *txn-context*))
                                (pc-branch (workspace-master-transitional-pc-branch workspace master-repository))
                                (pc (branch-owning-project pc-branch))
                                ;;(pc-version (branch-get-most-recent-version pc-branch))
                                )
                           (debug-message 3 "cmctl-ws-sync: Current pc-branch version contents: ~s"
                                          (vm::version-get-active-resident-cids
                                           (branch-get-most-recent-version pc-branch)))
                           (master-catalog-populate-workspace-mvcc
                            master-repository-name master-catalog
                            (cm-session-context-workspace-rooted-file-system cm-session-context server-relative workspace)
                            pc pc-branch reason
                            master-metaversion-cid-set
                            master-metaversion)))))
                    ((:REGENERATE)
                     (cmctl-regenerate-inner cm-session-context server-relative reason
                                             workspace change-context
                                             master-repository-name master-repository master-catalog))
                    ((:UPDATE)
                     (let* ((vpb-baseline-timestamp (workspace-baseline-timestamp workspace))
                            (pc-branch (with-cm-master-metaversion (vpb-baseline-timestamp)
                                         (workspace-master-pc-branch workspace master-repository)))
                            (pc (branch-owning-project pc-branch)))
                       ;; We are moving to a new state, do a real update.
                       (master-catalog-update-file-system
                        master-catalog master-repository-name
                        (cm-session-context-workspace-rooted-file-system cm-session-context server-relative workspace)
                        "workspace"
                        pc pc-branch
                        vpb-baseline-timestamp
                        (workspace-transitional-timestamp workspace)
                        :report-only nil
                        :change-context change-context
                        :VPB-old-added-satellite-cset-dids   (workspace-added-class-cset-tuples workspace)
                        :VPB-old-removed-satellite-cset-dids (workspace-removed-class-cset-tuples workspace)
                        :VPB-new-added-satellite-cset-dids   (workspace-transitional-added-class-cset-tuples workspace)
                        :VPB-new-removed-satellite-cset-dids (workspace-transitional-removed-class-cset-tuples workspace))))))))
        t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ws_set and ws_create

;;; We try to share as much code as possible between CMCTL-WS-SET and
;;; CMCTL-WS-CREATE.  The tricky part is that ws_set requires that
;;; there be a workspace while for ws_create the workspace should not
;;; already exist.  Because of this, we need to use different
;;; workspace transaction helper functions for each one.

(defvar *cmctl-ws-create-reason-root* "ChangeSafe: Create workspace ")
(defvar *cmctl-ws-set-reason-root* "ChangeSafe: Set workspace ")

(defun cmctl-ws-set-get-description (reason workspace-description user-description)
  "Figures out what to use for the ws_set description.
   First: Use the user's description if there is one.
   Second: Use the existing workspace description but only if it does not
           start as one of the two reason-root's above.
   Third: Use the reason.

   The appropriate description is returned."
  (if user-description
      user-description ;; First
    (if (null workspace-description)
        reason ;; Third
      (let* ((wsd workspace-description)
             (len-wsd (length wsd))
             (len-set (length *cmctl-ws-set-reason-root*)))
        (if (and (> len-wsd len-set)
                 (string= wsd *cmctl-ws-set-reason-root*
                          :start1 0 :end1 len-set
                          :start2 0 :end2 len-set))
            reason ;; Third
          (let ((len-crt (length *cmctl-ws-create-reason-root*)))
            (if (and (> len-wsd len-crt)
                     (string= wsd *cmctl-ws-create-reason-root*
                              :start1 0 :end1 len-crt
                              :start2 0 :end2 len-crt))
                reason ;; Third
              wsd ;; Second
              )))))))

(defun cmctl-ws-set (cm-session-context directory-spec description force-p server-relative)
  "Populate a workspace hierarchy named by DIRECTORY-SPEC
   Blows away the state that was there."
  (cm-session-context-require cm-session-context :repository-name)
  (let ((workspace-product-name
         ;; Get existing product-name for workspace
         (cmctl-call-with-user-txn cm-session-context
           :reason "ws_set find product-name"
           :txn-mode :read-only
           :require-active-change nil
           :receiver
           (lambda (ws-repository workspace change-context
                    master-repository-name master-repository master-catalog)
             (declare (ignore ws-repository change-context master-repository-name master-catalog))
             (with-cm-master-metaversion () ;; looking for current name
               (pc-name (workspace-master-pc workspace master-repository)))))))

    (flet ((prepare-for-different-product ()
             ;; If we are switching products, we may need to clean up (erase) files that are
             ;; part of the 'old' product but not part of the 'new' product.  So we have to
             ;; construct a list of 'known' files for each product, do a set-difference on
             ;; the two lists (to get those files unique to the 'old' product) and delete the
             ;; left over files.  This is done before we populate with the files for the 'new'
             ;; product.  Empty directories are also deleted.  Wild files are left alone.
             (debug-message 3 "WS_SET Product ~s overwrites Product ~s"
                            (cm-session-context-pc-name cm-session-context)
                            workspace-product-name)

             ;; *FINISH* some effort needs to be made to combine this transaction (cmctl-call-with-user-txn ...)
             ;; with the main one below (cmctl-call-with-workspace-persistent-change-context ...) so that we
             ;; have fewer transactions going in the long run
             (let ((reason (format nil "ws_set make file lists for ~a and ~a"
                                   workspace-product-name
                                   (cm-session-context-pc-name cm-session-context))))
               (cmctl-call-with-user-txn cm-session-context
                 :reason reason
                 :txn-mode :read-only
                 :require-active-change nil ;; was :prohibit-active-change  todo: Joe?
                 :receiver
                 (lambda (ws-repository workspace change-context
                          master-repository-name master-repository master-catalog)
                   (declare (ignore ws-repository change-context))
                   (let* ((ws-file-system (cm-session-context-workspace-rooted-file-system
                                           cm-session-context server-relative workspace))
                          (old-pc (workspace-master-pc workspace master-repository))
                          (new-pc (master-catalog-pc-name-lookup master-catalog
                                                                 cm-session-context :error-if-missing t))
                          (old-ws-file-list
                           (master-catalog-extract-pc-file-names-to-list
                            master-catalog master-repository-name
                            old-pc
                            ;; TODO: is this the correct pc-branch?
                            (with-cm-master-metaversion ((workspace-baseline-timestamp workspace))
                              (workspace-master-pc-branch workspace master-repository))
                            ws-file-system
                            reason
                            (workspace-baseline-timestamp workspace)
                            :workspace workspace
                            :no-directories t))
                          (new-ws-file-list
                           (master-catalog-extract-pc-file-names-to-list
                            master-catalog master-repository-name
                            new-pc
                            ;; TODO: is this the correct pc-branch?
                            (pc-lookup-branch-ok new-pc
                                                 (or (cm-session-context-release-name cm-session-context)
                                                     +pc-main-branch-name+))
                            ws-file-system
                            reason
                            (cm-session-context-time-spec cm-session-context)
                            :workspace nil
                            :no-directories t))

                          (files (set-difference old-ws-file-list new-ws-file-list
                                                 :test #'string=
                                                 :key #'namestring)))

                     (debug-message 4 "WS_SET PC Files(old) ~s" old-ws-file-list)
                     (debug-message 4 "WS_SET PC Files(new) ~s" new-ws-file-list)

                     (mapc (lambda (file)
                             (debug-message 4 "WS_SET PC Delete ~s" file)
                             (file-system-delete-file ws-file-system file)
                             ;; ugh
                             (do ((dir (pathname-syntactic-parent file)
                                       (logical-pathname-parent-directory dir)))
                                 ((or (null dir) ;; hit the root
                                      (file-system-probe-directory ws-file-system dir))) ;; non-empty dir
                               (debug-message 4 "WS_SET PC Delete ~s" dir)
                               (file-system-delete-directory ws-file-system dir)))
                           files)))))))

      (when (and (null (cm-session-context-pc-name cm-session-context))
                 workspace-product-name)
        (cm-session-context-add cm-session-context :pc-name workspace-product-name))

      (cmctl-ws-set/create-before-transaction :set cm-session-context directory-spec description)
      ;; make sure we clean up any locks we abandon.
      (cmctl-master-unlock cm-session-context :if-none-locked :ignore)

      ;; see if existing matches given arg for product name
      (when (and (cm-session-context-pc-name cm-session-context)
                  (not (string-equal (cm-session-context-pc-name cm-session-context)
                                     workspace-product-name)))
        (prepare-for-different-product))

      ;; (cmctl-ws-set/create :set cm-session-context directory-spec description)
      (let ((reason (format nil "~a~a for user ~a" *cmctl-ws-set-reason-root*
                            (enough-namestring directory-spec
                                               (namestring (cm-session-context-current-directory cm-session-context)))
                            (cm-session-context-user-name cm-session-context))))
        (cmctl-call-with-workspace-persistent-change-context cm-session-context
          :reason reason
          :txn-mode :read-write
          :receiver
          (lambda (ws-repository workspace persistent-change-context)

            (if (file-system-probe-file (cm-session-context-file-system-agent cm-session-context) directory-spec)
                (when persistent-change-context
                  (conman-signal-error
                   *cm-returns-error-change-not-permitted*
                   "An active change context exists in workspace ~a, and this prohibits ~
                  use of the ws_set operation."
                   (format nil "~s" workspace)))
              (error "Missing workspace."))

            (let ((metaversion-timestamp (cm-session-context-time-spec cm-session-context)))
              (multiple-value-bind (pc pc-branch pc-version ws-baseline-timestamp) ;; master-metaversion-cid-set)
                  ;; (declare (ignore master-metaversion-cid-set))
                  (cmctl-call-with-master-repository-txn cm-session-context
                      :reason reason
                      :txn-mode :read-only
                      :receiver
                      (cmctl-ws-set/create-master-transaction-function
                       cm-session-context
                       directory-spec
                       (constantly nil)
                       reason metaversion-timestamp))
                ;; Now update the workspace
                (cmctl-ws-set/create-workspace-update :set cm-session-context
                                                      directory-spec
                                                      (cmctl-ws-set-get-description
                                                       reason (workspace-description workspace)
                                                       description)
                                                      ws-repository workspace
                                                      (distributed-object-identifier pc)
                                                      (distributed-object-identifier pc-branch)
                                                      (distributed-object-identifier pc-version)
                                                      ws-baseline-timestamp force-p nil))))))))
  (cmctl-ws-set/create-after-transaction  :set cm-session-context directory-spec)
  t)




(defun cmctl-ws-create-master-transaction-populate-function (cm-session-context
                                                             directory-spec
                                                             reason metaversion-timestamp
                                                             master-metaversion-cid-set
                                                             pc pc-branch
                                                             ws-baseline-timestamp)
  "Return a function to be used as the master repository transaction
   function for ws_create.

   DIRECTORY-SPEC is the workspace root directory.

   REASON is a descriptive string about the WS_CREATE.

   WS-BASELINE-TIMESTAMP is the time to help decide which files are needed.

   It is VERY important that this function (and anything it calls) not recalculate ANY settings
   that would affect the view that the transaction sees.  It must use the same view as the
   original transaction for WS_CREATE.

   This function causes the client file-system workspace representation to be populated."
  (lambda (master-repository-name master-repository master-catalog)
      (declare (ignore master-repository))
      (with-cm-master-metaversion (metaversion-timestamp)

        ;; Populate the user's file hierarchy using the file-system
        ;; The workspace has ALREADY been created in the semipersistent database.  Note that this
        ;; operation is examining multiple satellite repository contents and should do so in a
        ;; read-only transaction.

        ;; This creates and populates the subdirectories. (but will not clean them).
        (master-catalog-populate-workspace-mvcc master-repository-name master-catalog
                                                (logical-file-system-create
                                                 directory-spec
                                                 (cm-session-context-file-system-agent cm-session-context))
                                                pc pc-branch reason
                                                master-metaversion-cid-set
                                                ws-baseline-timestamp)
        )))

(defun cmctl-ws-set/create-workspace-update (operation cm-session-context
                                             directory-spec
                                             description
                                             ws-repository workspace
                                             pc-did pc-branch-did pc-version-did ws-baseline-timestamp
                                             force-p product-directory?)
  "Common code shared by CMCTL-WS-SET and CMCTL-WS-CREATE to
   perform the updating of the workspace object.

   MASTER-TRANSACTION-RESULTS-GETTER should return these values:  PC-DID, PC-BRANCH-DID,
   PC-VERSION-DID, and WS-BASELINE-TIMESTAMP, which will have been provided by the
   RESULTS-CONTINUATION argument to CMCTL-WS-SET/CREATE-MASTER-TRANSACTION-FUNCTION.

   Should be called from within a read/write transaction on the workspace repository."
  (ecase operation
    (:create
     (if workspace
         (conman-signal-error *cm-returns-error-create-existing-workspace*
                              "The directory ~s is already in a workspace."
                              directory-spec)
       (setq workspace
             (workspace-create ws-repository
                               (cm-session-context-user-name cm-session-context)
                               directory-spec
                               pc-did pc-branch-did pc-version-did
                               ws-baseline-timestamp
                               description product-directory?))))
    (:set
     (if workspace
         (workspace-modify-for-ws-set ws-repository
                                      workspace
                                      pc-did pc-branch-did pc-version-did
                                      ws-baseline-timestamp
                                      description force-p)
       (conman-signal-error *cm-returns-error-no-such-workspace*
                            "The directory ~s is not in any workspace."
                            directory-spec))))
  ;; Upgrade the context for subsequent writing to the .csf file.
  (cm-session-context-add cm-session-context :ws-id (workspace-identifier workspace)))

(defun cmctl-ws-set/create-after-transaction (operation cm-session-context directory-spec)
  "Common things to be done by ws_set and ws_create after the database
   transaction is finished."
  ;; Create the `.csf' file at the workspace root directory with
  ;; the relevant context.
  (ecase operation
    (:create
     ;; nothing to do, `.conman' already created
     )
    (:set
     (cmctl-ws-set/create-write-conman-file operation cm-session-context directory-spec))))

(defun cmctl-ws-set/create-write-conman-file (operation cm-session-context directory-spec)
  "Create or overwrite `.conman' file in workspace with new or updated information"
  (cmctl-write-conman-rc cm-session-context
                         directory-spec
                         :if-exists (ecase operation
                                      (:set :supersede)
                                      (:create :error))
                         :if-does-not-exist (ecase operation
                                              (:set :error)
                                              (:create :create))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmctl-ws-regenerate (cm-session-context server-relative)
  "Make sure that the disk contents reflect the actual state of the workspace.

   This assumes that the workspace has been `trashed'."
  (declare (ignore server-relative))
  (cm-session-context-require cm-session-context :user-name :repository-name :ws-id)
  (cmctl-call-with-workspace cm-session-context
    :reason "workspace regenerate"
    :txn-mode :read-write
    :transition-action :error
    :receiver (lambda (ws-repository workspace)
                  (declare (ignore ws-repository)) ;; we're already in it.
                  (if (workspace-in-transition? workspace)
                      ;; If user called regenerate while in transition,
                      ;; simply regenerate the new state.
                      ;; This probably won't work right away.
                      (workspace-begin-transition workspace :regenerate
                                                  (versionref-create
                                                   (workspace-transitional-pc-did workspace)
                                                   (workspace-transitional-pc-branch-did workspace)
                                                   (versionref-version-state
                                                    (workspace-transitional-versionref workspace))
                                                   (workspace-transitional-timestamp workspace))
                                                  (workspace-transitional-added-master-csets workspace)
                                                  (workspace-transitional-removed-master-csets workspace)
                                                  (workspace-transitional-added-class-cset-tuples workspace)
                                                  (workspace-transitional-removed-class-cset-tuples workspace))
                    ;; Set new mode to old mode and sync.
                    (workspace-begin-transition workspace :regenerate
                                                (versionref-create
                                                 (workspace-pc-did workspace)
                                                 (workspace-pc-branch-did workspace)
                                                 (versionref-version-state
                                                  (workspace-baseline-versionref workspace))
                                                 (workspace-baseline-timestamp workspace))
                                                (workspace-added-class-cset-tuples workspace)
                                                (workspace-removed-class-cset-tuples workspace)))
                  t)))

(defun cmctl-ws-regenerate-immediately (cm-session-context server-relative)
  "Make sure that the disk contents reflect the actual state of the workspace.

   This assumes that the workspace has been `trashed'."
  (cm-session-context-require cm-session-context :user-name :repository-name :ws-id)
  (let ((reason (format nil "Refresh workspace for user ~s" (cm-session-context-user-name cm-session-context))))
    (cmctl-call-with-user-txn cm-session-context
      :reason reason
      :txn-mode :read-only
      :require-active-change nil ;; was :prohibit-active-change  todo: Joe?
      :receiver
      (lambda (ws-repository workspace change-context
                 master-repository-name master-repository master-catalog)
          (declare (ignore ws-repository))
          (cmctl-regenerate-inner cm-session-context server-relative reason
                                  workspace change-context
                                  master-repository-name master-repository master-catalog)))
    t))

(defun cmctl-regenerate-inner (cm-session-context server-relative reason
                               workspace change-context
                               master-repository-name master-repository master-catalog)
  (with-cm-master-metaversion ((workspace-baseline-timestamp workspace))
    (let* ((pc (workspace-master-pc workspace master-repository))
           (pc-branch (pc-lookup-branch-ok pc +pc-main-branch-name+))
           ;;(pc-version (branch-get-most-recent-version pc-branch))
           )
      (progn ;;with-version (pc-version)
        (master-catalog-extract-pc-files-to-disk
         master-catalog master-repository-name
         pc pc-branch
         (cm-session-context-workspace-rooted-file-system cm-session-context server-relative workspace)
         reason
         (workspace-baseline-timestamp workspace)
         :workspace workspace
         :change-context change-context
         ;; only read-only if not checked out
         :read-only (if change-context
                        (lambda (file-system-element filesystem)
                            (declare (ignore filesystem))
                            (not (change-context-find-file-changes
                                  change-context
                                  (distributed-object-identifier
                                   file-system-element))))
                      t)
         :clean nil)))))

;;; First pass of two-phase update.
(defun cmctl-ws-update (cm-session-context server-relative report-only &rest keys)
  "Bring workspace up to date with regard to repository.  This is the entry point
   for the user command ws_update.  It validates arguments and continues with
   CMCTL-UPDATE-WORKSPACE, which is the main entry point for updating a workspace."
  (cm-session-context-require cm-session-context :user-name :repository-name :ws-id)
  (let* ((reason (format nil "Update workspace for user ~s"
                         (cm-session-context-user-name cm-session-context)))
         ;; *FINISH*: use release, label, whatever
         ;; old: (new-timestamp (time-stamp-allocate))
         (user-time-spec (cm-session-context-time-spec cm-session-context))
         (new-timestamp
          ;; *FINISH*: use release, label, whatever
          (cond (user-time-spec (time-stamp-create user-time-spec))
                (t (time-stamp-allocate)))))
    (if report-only
        (apply #'cmctl-update-workspace cm-session-context reason new-timestamp server-relative report-only
               keys)
      (cmctl-call-with-workspace cm-session-context
        :reason reason
        :txn-mode :read-write
        :transition-action :error
        :receiver (lambda (ws-repository workspace)
                      (declare (ignore ws-repository)) ; we are already in it.
                      ;; Consed in the correct repository because
                      ;; we are doing cmctl-call-with-workspace
                      (workspace-begin-transition workspace :update
                                                  (versionref-create
                                                   (workspace-pc-did workspace)
                                                   (workspace-pc-branch-did workspace)
                                                   (versionref-version-state (workspace-baseline-versionref workspace))
                                                   new-timestamp)
                                                  (workspace-added-class-cset-tuples workspace)
                                                  (workspace-removed-class-cset-tuples workspace))
                      t)))))

(defun cmctl-update-workspace (cm-session-context reason new-timestamp server-relative report-only
                               &key merge-handler)
  "This is the main entry point for updating a workspace."
  (let ((three-way-merge nil)
        (merge-conflicts 0)
        (merged-binary-files nil))

    (cmctl-call-with-user-txn cm-session-context
      :reason reason
      :txn-mode :read-write
      :require-active-change nil                ; no active change required
      :receiver
      (lambda (ws-repository workspace change-context
                 master-repository-name master-repository master-catalog)
          (declare (ignore ws-repository))
          (let* ((workspace-root-fs (cm-session-context-workspace-rooted-file-system cm-session-context
                                                                                     server-relative workspace)))
            (multiple-value-setq (three-way-merge merge-conflicts merged-binary-files)
              (cmctl-ws-update-and-merge new-timestamp report-only
                                         workspace workspace-root-fs
                                         change-context
                                         master-repository-name master-repository master-catalog))
            ))
      :update-workspace
      (lambda (workspace-repository workspace change-context)
          (unless report-only
            ;; save all the (potential) changes to the db
            ;; even if change-context is nil (which can happen if we UNCO all of the files)
            (when (workspace-persistent-change-context workspace)
              (make-change-context-persistent-in-workspace workspace-repository workspace change-context))
            ;; update the workspace timestamp (even though we may have had errors)
            (versionref-update-timestamp (workspace-baseline-versionref workspace) new-timestamp))
          ))
    (when merged-binary-files
      (conman-signal-error *cm-returns-error-cant-merge-binary-files*
                           "No automatic merges on binary files.  The conflicting files have been UNCO'd."
                           nil))
    (when merge-handler
      (funcall merge-handler three-way-merge merge-conflicts merged-binary-files))
    t))

(defun cmctl-ws-update-validate-new-time-stamp (new-time-stamp workspace-baseline-timestamp)
  "If NEW-TIME-STAMP is older than WORKSPACE-BASELINE-TIMESTAMP then signal an error."
  (when (time-stamp-less-recent? new-time-stamp workspace-baseline-timestamp)
    (conman-signal-error
     *cm-returns-error-time-spec-not-valid*
     "The time specified, ~a is older than the workspace baseline ~a.")))

(defun cmctl-ws-update-and-merge (new-timestamp report-only
                                  workspace workspace-root-fs
                                  change-context
                                  master-repository-name master-repository master-catalog)
  "Bring changes from the master into the workspace.

   User's disk area is incrementally brought up to date."
  (let* ((vpb-baseline-timestamp (workspace-baseline-timestamp workspace))
         (pc-branch (with-cm-master-metaversion (vpb-baseline-timestamp)
                      (workspace-master-pc-branch workspace master-repository)))
         (pc (branch-owning-project pc-branch))
         (vpb-added-satellite-cset-tuples   (workspace-added-class-cset-tuples workspace))
         (vpb-removed-satellite-cset-tuples (workspace-removed-class-cset-tuples workspace)))
    (cmctl-ws-update-validate-new-time-stamp new-timestamp vpb-baseline-timestamp)
    (master-catalog-update-file-system
     master-catalog
     master-repository-name
     workspace-root-fs
     "workspace"
     pc
     pc-branch
     vpb-baseline-timestamp
     new-timestamp
     :report-only report-only
     :change-context change-context
     :VPB-old-added-satellite-cset-dids   vpb-added-satellite-cset-tuples
     :VPB-old-removed-satellite-cset-dids vpb-removed-satellite-cset-tuples
     :VPB-new-added-satellite-cset-dids   vpb-added-satellite-cset-tuples
     :VPB-new-removed-satellite-cset-dids vpb-removed-satellite-cset-tuples)))

(defun cmctl-remove-subsystem (cm-session-context old-subsystem change-context)
  (declare (ignore cm-session-context old-subsystem change-context))
  (error "Don't know how to remove subsystems yet."))

(defun cmctl-ws-delete-directory (cm-session-context workspace-directory server-relative force keep-files)
  "Delete the workspace designated by workspace-directory.  Your current directory may be in the
   given workspace-directory (in which case, not all of the directories will be deleted) or not.

   A .csf file must be present in the workspace-directory and its ws-id must match what we found
   in the database for the workspace.  Also the workspace name must match.

   This code has two successive transactions on the workspace-repository.  The first is r/o, the
   second is r/w.

   Any files which are from the repository are deleted from the user's disk.
   Any wild files are left alone.  And empty directories are deleted."
  (declare (special *cm-cli-switch-force-syntax*))
  (guarantee-absolute-directory-pathname workspace-directory)

  (cm-session-context-require cm-session-context :user-name :current-directory)
  (let ((reason (format nil "Delete workspace ~s for user ~s"
                        (namestring workspace-directory)
                        (cm-session-context-user-name cm-session-context)))
        workspace-lfs workspace-conman-file-descriptor)

    ;; get the .csf file data if we need it
    (unless (cm-session-context-repository-name cm-session-context)
      (let ((temp-cm-session-context (make-instance 'cm-session-context)))
        (cmctl-read-dot-csf-file cm-session-context
                                 workspace-directory
                                 temp-cm-session-context
                                 :ws-id (cm-session-context-ws-id cm-session-context))
        (cm-session-context-add cm-session-context
                                :repository-name (cm-session-context-repository-name temp-cm-session-context)
                                :ws-id (cm-session-context-ws-id temp-cm-session-context))
        ))
    (cm-session-context-require cm-session-context :repository-name)

    ;; For the CC-FILECHANGEs we're supposed to do the equivalent of a CM unco, but since
    ;; the files are going to be deleted anyway, we don't need to restore unmodified versions.
    ;; ***TBD*** Do we need to treat modified files as wild files or can they be deleted
    ;; like regular repository files are?
    ;; For each of the CC-FILERENAMES, we need to un-rename the files so that when we go to
    ;; delete them they will have the names we expect.  Alternatively, we could just delete them
    ;; based on what the CC-FILERENAMEs say.
    (cmctl-call-with-user-txn cm-session-context
      :reason reason
      :txn-mode :read-write
      :require-active-change nil
      :receiver
      (lambda (ws-repository workspace change-context
                 master-repository-name master-repository master-catalog)
          (declare (ignore ws-repository master-repository-name))

          ;; If the workspace has an open delta then -force is required
          (when (and change-context (not force))
            (conman-signal-error *cm-returns-error-change-not-permitted*
              "The workspace has an open change.  Use ~a to delete it." *cm-cli-switch-force-syntax*))
         (when (eq keep-files :wild)
          ;; Ensure that there is a .csf file and the ws-ids & dbnames match
          (let ((temp-cm-session-context (make-instance 'cm-session-context)))
            (cmctl-read-dot-csf-file cm-session-context workspace-directory temp-cm-session-context
                                     :ws-id   (workspace-identifier workspace)
                                     :dbpath (cm-session-context-repository-name cm-session-context)))

          (with-cm-master-metaversion ((workspace-baseline-timestamp workspace))
            ;; Undo the file system operations (delete from disk changed and renamed files)
            (let ((branch (workspace-master-pc-branch workspace master-repository)))
              (master-catalog-revert-change-context
               master-catalog
               cm-session-context
               workspace
               change-context
               (branch-owning-project branch)
               branch
               (cm-session-context-workspace-rooted-file-system cm-session-context server-relative workspace)
               reason
               ;; delete from disk all repository-based files in the workspace
               :deleting-workspace-p t)))))
      :update-workspace
      (lambda (workspace-repository workspace change-context)       ;; Toss out the change context.
          (declare (ignore change-context))
          (workspace-delete-change-context workspace)
          ;; Should we delete the workspace object?  Its .csf file?
          (setq workspace-lfs (cm-session-context-workspace-rooted-file-system cm-session-context server-relative workspace))
          (setq workspace-conman-file-descriptor
                (file-system-probe-file workspace-lfs *cmctl-dot-conman-file-name*))
          (workspace-delete workspace-repository workspace)))

   (if (not keep-files) ;; delete all files unless keeping :all or :wild files
       ;; logical-file-system-change-directory  logical-pathname-parent-directory  pathname-syntactic-parent
      (ignore-errors
       (let* ((root (logical-file-system-root workspace-lfs))
              (parent-lfs (logical-file-system-change-directory workspace-lfs (pathname-syntactic-parent root))))
         (file-system-delete-directory parent-lfs
                                       (file-system-probe-file parent-lfs
                                                               (merge-pathnames (first (last (pathname-directory root)))
                                                                                #p"REPOSITORY:;"))
                                       :recursive t :force t)))
     ;; We don't delete the .csf file until after the repository transactions commit.
     ;; That way, if there's a problem with the transaction, there's still a handle
     ;; to the workspace in the user's filesystem.
     (when workspace-conman-file-descriptor
      (file-system-delete-file workspace-lfs workspace-conman-file-descriptor :force t)))

    ;; Delete any directories which are now empty
    (let ((not-deleted nil)                     ; list of DIRECTORY-DESCRIPTORs
          (root (file-system-probe-file workspace-lfs #p"REPOSITORY:;")))
      (handler-bind ((file-system-condition
                      (lambda (condition)
                          (when (eq :delete-directory-failed (file-system-condition-reason condition))
                            (let ((restart (find-restart :delete-directory-punt)))
                              (when restart
                                (push (getf (server::file-system-condition-other-args condition)
                                            :file-descriptor)
                                      not-deleted)
                                (invoke-restart restart)))))))
        (when (and root keep-files)
          (file-system-prune-empty-directories workspace-lfs root)))
      (when not-deleted
        (conman-signal-warning *cm-returns-warning-unable-to-delete-directory*
         "Can't delete these directories: ~{~a~^, ~}."
         (mapcar (lambda (fd)
                     ;; Could use FILE-SYSTEM-FRIENDLY-NAME here but it will return paths
                     ;; relative to the workspace root which the user might misinterpret as
                     ;; being relative to his working directory and be confused.
                     (server::resolve-relative-path (file-descriptor-path fd)
                                                    (file-descriptor-file-system fd)))
                 not-deleted)))))
  t)

(defun cmctl-ws-move (cm-session-context old-workspace-root new-workspace-root)
  "Move a workspace object from one directory to another on the same computer.  This is basically
   the UNIX \"mv\" command with the additional twist of having to remember the new location in the
   semipersistent repository.

   Nearly all work (as much as possible) will be done on the client side computer.  We must also
   move files that the central, master repository does not necessarily know about like temporary
   files and, more importantly, files that are part of an active CSET.  Also the actual move
   should either be outside of a transaction or in a read-only mode so as to tie up the server the
   le ast amount.  Obviously, we will need a r/w transaction to save the new location.

   See CM-CLI-WS-MOVE documentation for more details."
  (guarantee-absolute-directory-pathname old-workspace-root)
  (guarantee-absolute-directory-pathname new-workspace-root)

  (cm-session-context-require cm-session-context :user-name :current-directory)
  (let ((reason (format nil "Move workspace ~s to ~s for user ~s"
                        (namestring old-workspace-root)
                        (namestring new-workspace-root)
                        (cm-session-context-user-name cm-session-context)))
        )

    ;; get the .csf file data if we need it
    (unless (cm-session-context-repository-name cm-session-context)
      (let ((temp-cm-session-context (make-instance 'cm-session-context)))
        (cmctl-read-dot-csf-file cm-session-context
                                 old-workspace-root
                                 temp-cm-session-context
                                 :ws-id (cm-session-context-ws-id cm-session-context))
        (cm-session-context-add cm-session-context
                                :repository-name (cm-session-context-repository-name temp-cm-session-context)
                                :ws-id (cm-session-context-ws-id temp-cm-session-context))
        ))

    (cm-session-context-require cm-session-context :repository-name)

    ;; It's arguable, but we should probably open the master repository read-only
    ;; for workspace creation, since it's only the workspace repository which is updated.
    ;; However... doing so means that we can't update the workspace repository, because the outermost
    ;; transaction mode drives what we can do in nested transactions.  So if we want to open
    ;; the workspace repository in a nested transaction, we must open the outermost txn for update.
    ;; It's always a shame to sit on locks we don't need, so rather than opening the entire thing
    ;; R/W for what is essentially an R/O operation, we perform two separate transactions.
    ;; The R/O one for the versioned repositories, and the R/W one for
    ;; the non-versioned workspace repository.

    ;; Note that we'd really like the R/O txn to occur in MVCC mode.  This particular piece of txn logic
    ;; falls into a V-MVCC NV-RW territory I think we'll see elsewhere, and for which we should
    ;; optimize in 2-server configurations.

    ;; Note also that if we want to retain a LOCK on the master while creating the workspace,
    ;; we'll need to use the nested TXNs, and therefore a R/W txn on the master.
    (call-with-busy-redirection cm-session-context reason nil
      (lambda ()
          (with-open-workspace-repository (ws-repository
                                           (conman-workspace-repository-name
                                            (cm-session-context-repository-name
                                             cm-session-context)))
            (with-workspace-repository-txn (ws-repository :read-write reason)
              (let* (
                     ;; This section should (more/less) match ws_set for detecting whether or not the
                     ;; old workspace path is ok to use
                     (old-workspace (workspace-resolve-spec cm-session-context ws-repository
                                                            :ws-path (namestring old-workspace-root)
                                                            :user-name (cm-session-context-user-name cm-session-context)
                                                            :error-if-missing t))

                     ;; This section should (more/less) match ws_create for detecting whether or not
                     ;; the new workspace path is ok to use
                     (new-workspace
                      (block existing-workspace-p
                        ;; Look for a workspace corresponding to or
                        ;; containing the current (client side) directory.
                        (map-over-workspaces
                         (lambda (new-workspace)
                             ;; ***** How do we deal with several different
                             ;; hosts having workspaces with the same
                             ;; paths, i.e. the workspace pathnames are the
                             ;; same except for PATHNAME-HOST?
                             (let ((ws-root-path (make-wild-pathname (workspace-path new-workspace))))
                               (when (or (pathname-match-p (cm-session-context-current-directory cm-session-context)
                                                           ws-root-path)
                                         (pathname-match-p new-workspace-root ws-root-path))
                                 (return-from existing-workspace-p new-workspace))))
                         ws-repository)
                        nil)
                      ))
                (when new-workspace
                  (conman-signal-error *cm-returns-error-create-existing-workspace*
                                       "The directory ~s is already in a workspace."
                                       (namestring (workspace-path new-workspace))))

                ;; make sure that the old-workspace-root does not contain the current-directory
                ;; or the other combinations with the new-workspace-root
                (when (string-equal (namestring old-workspace-root)
                                    (namestring new-workspace-root))
                  (conman-signal-error *cm-returns-error-ws-move-current-dir-in-old-ws*
                                       "Your new directory (~s) can not be the same as the existing workspace (~s)"
                                       (namestring new-workspace-root)
                                       (namestring old-workspace-root)))
                (when (directory-is-prefix? old-workspace-root
                                            (cm-session-context-current-directory cm-session-context))
                  (conman-signal-error *cm-returns-error-ws-move-current-dir-in-old-ws*
                                       "Your current directory (~s) can not be within the existing workspace (~s)"
                                       (cm-session-context-current-directory cm-session-context)
                                       (namestring old-workspace-root)))
                (when (directory-is-prefix? old-workspace-root new-workspace-root)
                  (conman-signal-error *cm-returns-error-ws-move-current-dir-in-old-ws*
                                       "Your new directory (~s) can not be within the existing workspace (~s)"
                                       (namestring new-workspace-root)
                                       (namestring old-workspace-root)))
                (when (directory-is-prefix? new-workspace-root old-workspace-root)
                  (conman-signal-error *cm-returns-error-ws-move-current-dir-in-old-ws*
                                       "Your existing directory (~s) can not be within the new workspace (~s)"
                                       (namestring old-workspace-root)
                                       (namestring new-workspace-root)))

                ;; make sure that there is a .csf file and the ws-ids & dbnames match
                (let ((temp-cm-session-context (make-instance 'cm-session-context)))
                  (cmctl-read-dot-csf-file cm-session-context
                                           old-workspace-root
                                           temp-cm-session-context
                                           :ws-id (workspace-identifier old-workspace)
                                           :dbpath (cm-session-context-repository-name cm-session-context)))

                (when (file-system-probe-file (cm-session-context-file-system-agent cm-session-context)
                                              new-workspace-root)
                  (conman-signal-error *cm-returns-error-create-existing-workspace*
                                       "The new Workspace directory (~s) already exists."
                                       (namestring new-workspace-root)))

                ;; Now update the workspace object -- Note that this must happen outside of the
                ;; cmctl-call-with-master-repository-txn but inside of the with-open-ws-repository
                (workspace-modify-for-ws-move old-workspace
                                              (cm-session-context-user-name cm-session-context)
                                              new-workspace-root)
                )))))

    ;; Move the files to the new location.  This is especially done outside the transaction blocks
    ;; If anything goes wrong the user can redo it by hand at this point
    (handler-bind ((file-system-condition
                    (lambda (condition)
                        (format *error-output* "~&~A~%~s~&" condition
                                "You will need to restart the directory rename by hand to finish this ws_move operation."))))
      (file-system-rename (cm-session-context-file-system-agent cm-session-context)
                          old-workspace-root new-workspace-root))
    )
  t)

(defun cmctl-ws-query-list (cm-session-context user)
  "List workspaces for 'ws_query -list-ws'.
   USER is either a string naming a user or the keyword :ALL.
   Either all workspaces are listed or only those owned by the specified user."
  (let ((reason (format nil "list workspaces ~s for user ~s"
                        user (cm-session-context-user-name cm-session-context))))
    (call-with-busy-redirection
        cm-session-context
        reason
      nil
      (lambda ()
          (with-open-workspace-repository (ws-repository
                                           (conman-workspace-repository-name
                                            (cm-session-context-repository-name
                                             cm-session-context)))
            (with-workspace-repository-txn (ws-repository :read-only reason)
              (workspace-collection-get-workspaces
               ws-repository
               :collection-predicate
               (if (eq user :all)
                   #'true
                 (workspace-collection-make-predicate :user-name user)))))))))


(defun cmctl-retrieve-abstract-file-contents (cm-session-context file-system abstract-file-spec)
  "Retrieve the contents of the abstract file.

   An error is signalled if we are unable to read its contents.
   ABSTRACT-FILE-SPEC must be NIL, or a string specifying a client path string accessible via FILE-SYSTEM.

   We go to some trouble to ensure that the resulting string is a simple string (for persistent storage
   reasons). Let us hope it isn't too long!

   The abstract file need not lie within a workspace!"
  ;; Note that this function is also used for other purposes (e.g. csets-from-file) and hence the
  ;; error messages need to be generic

  (declare (ignore cm-session-context))
  (debug-message 4 (format nil "cmctl-rafc fs: ~s, afs: ~s~%"
                           file-system abstract-file-spec))
  (let ((fd (file-system-probe-file file-system abstract-file-spec))
        (result nil))
    (unless fd
      (conman-signal-error *cm-returns-error-non-existent-disk-file*
                           "Unable to locate textual file specification ~s on disk"
                           abstract-file-spec))
    ;; Guaranteed by upper level
    ;; (unless (file-descriptor-is-file? fd)
    ;;   (conman-signal-error *cm-returns-error-invalid-disk-file-spec*
    ;;    "Textual file specification ~s is not a file." abstract-file-spec))
    ;; Read the file
    (let ((content-type (file-descriptor-content-type fd)))
      (call-with-file-descriptor-content
          fd (file-descriptor-record-terminator fd)
        (lambda (content)
            (ecase content-type
              (:zero
               (conman-signal-error *cm-returns-error-empty-abstract-file*
                                    "Textual file ~s is empty." abstract-file-spec))
              (:binary
               (conman-signal-error *cm-returns-error-binary-abstract-file*
                                    "The textual file ~s appears to have binary content."
                                    abstract-file-spec))
              (:text
               ;; content is a record stream  Stuff it into result
               ;; Yuck.  This just concatenates all the lines.
               (let* ((records (vi-stream-as-list content))
                      (record-lengths (mapcar #'length records)))
                 (setq result (make-string (reduce #'+ (mapcar
                                                        (lambda (record) (1+ (length record)))
                                                        records))))
                 (loop with buffer-position = 0
                       for record in records
                       for record-length in record-lengths
                       do
                       (array-block-copy-char record result :to-index buffer-position)
                       (incf buffer-position record-length)
                       ;; Stuff a newline between the records.
                       ;; There's an issue of what line-break
                       ;; convention to use here.  Clearly a
                       ;; Newline Character is better than Space
                       ;; because it at least remembers that
                       ;; there was a linebreak there.
                       (setf (schar result buffer-position) #\Newline)
                       (incf buffer-position)
                       )))))))
    result))

(defun cmctl-valid-name-char? (char)
  "Given a character, determine if it is valid as part of a cset-name"
  (and (standard-char-p char)
       (or (alphanumericp char)
           (find char "=._-+"))))

(defun cmctl-valid-name? (change-name)
  "Given a string, determine if it is valid as a cset-name (short, user-supplied kind)
according to the syntax rules."
  ;; Rules: 1st char must be a-zA-Z0-9
  ;; other chars must all be one of those or else one of
  ;; =, ., _, -, +

  ;; Note this works BECAUSE cmctl-valid-change-name-char? tests standard-char-p, so
  ;; any alphanumeric chars that are not a-zA-Z0-9 are excluded by standard-char-p.
  (and (stringp change-name)
       (locally (declare (string change-name))
         (and (< 0 (length change-name))
              (alphanumericp (char change-name 0))
              (every #'cmctl-valid-name-char? change-name)))))


(defun cmctl-change-create (cm-session-context change-name description abstract-file no-update? pathnames
                            &key server-relative)
  "Create a change associated with a workspace.

   Remember that WS-ID may be an integer which uniquely describes a workspace, or it may
   be a directory path which we need to resolve to a unique ws-id.

   See CM-CLI-CHANGE-CREATE documentation for more details.

   Note that if PATHNAMES is non-nil, this function will call the CO command (cmctl-file-checkout) at
   almost the very end of the command.

   TBD: whether or not to place change context information in the .csf file.  For now we pass
   and store it only in the workspace repository."

  ;; PERFORMANCE/TXN DILEMMA: we'd like to separate transaction spaces for workspace maintenance and
  ;; checkout.   Yet the requirements don't want change creation (in the workspace) to succeed unless
  ;; checkout succeeds.  If it was ok to create change without checkout succeeding, we could do this
  ;; more efficiently by verifying that files exist, creating the change, then doing the checkout
  ;; in a separate transaction.

  (declare (special *cm-cli-command-change-create*)) ;foward reference
  (cm-session-context-require cm-session-context :user-name :repository-name :ws-id)
  (check-type description (or null string))
  (check-type change-name string)
  (unless (cmctl-valid-name? change-name)
    (conman-signal-error *cm-returns-error-invalid-change-name*
                         "The cset name, ~S, specified was invalid." change-name))
  (let* ((user-name (cm-session-context-user-name cm-session-context))
         (reason        (format nil "Create cset for user ~s" user-name))
         (new-change-context nil)
         (time-stamp nil) ;; filled in below
         (abstract-file-contents
          (when abstract-file
            (cmctl-retrieve-abstract-file-contents cm-session-context
                                                   (cm-session-context-file-system-agent cm-session-context)
                                                   abstract-file))))
    ;; Strictly speaking, we ought to perform MVCC/RW logic similar to CMCTL-WS-CREATE,
    ;; since this operation doesn't need (currently) to modify the master, only the workspace repository.
    ;; (JDT: true? or want versioned records in master recording change creation?  *TBD*)
    ;; *FINISH*: rework txn logic to separate checkout and change-create so that checkout proceeds
    ;; largely in MVCC txns on master/satellites?
    (cmctl-call-with-user-txn cm-session-context
      :reason reason
      :txn-mode :read-write
      :require-active-change :prohibit-active-change
      :receiver
      (lambda (ws-repository workspace change-context
                 master-repository-name master-repository master-catalog)
          (declare (ignore ws-repository change-context))
          (error-if-product-reference-workspace workspace "change create")
          #||
          ;; HP want's the user to be able to create csets while there
          ;; is port activity.
          (when (workspace-has-port-activity-p workspace)
            (conman-signal-error
             *cm-returns-error-port-activity-in-progress*
             "You can't start a cset while the workspace is undergoing port activity."))
          ||#
          (with-cm-master-metaversion ()        ; latest
            ;; Validate any specified files in the subsystems of the PC's
            ;; An error is signalled if the files don't exist.
            ;; *TBD*: this transaction structure is pretty screwy, we're sitting on a R/W
            ;; workspace repository lock while we potentially grovel over subsystem repositories.
            (let* ((workspace-root-fs
                    (cm-session-context-workspace-rooted-file-system cm-session-context server-relative workspace))

                   (branch (workspace-master-pc-branch workspace master-repository))

                   (version (conman-branch-get-mutable-tip branch))

                   (pc (branch-owning-project branch))
                   ;; *FINISH*: validate that this name is unique among changes.
                   (system-change-name (conman-generate-HP-cset-name
                                        change-name user-name (pc-name pc))))
              (if (master-catalog-resolve-qualified-change-set-name master-catalog
                                                                    system-change-name
                                                                    :error-if-missing nil)
                  (conman-signal-warning
                   *cm-returns-warning-duplicate-change-name*
                   "The fully qualified cset name (~s) is already in use; you will need to choose another name."
                   system-change-name))
              (pc-warn-if-inactive pc)
              (setq time-stamp (version-last-update-timestamp version (txn-context-cid-set *txn-context*)))

              (unless (or (branch-workspace-up-to-date-p branch workspace)
                          no-update?)
                (cmctl-ws-update-and-merge time-stamp ;; new timestamp is NOW
                                           nil ;; not report only
                                           workspace
                                           workspace-root-fs
                                           nil ;; no change context yet
                                           master-repository-name
                                           master-repository
                                           master-catalog))

              ;; Create the TRANSIENT change-context for later addition to workspace in ws repository
              ;; after we exit the master repository scope
              (setq new-change-context (conman-change-context-wrapper-create
                                        (change-context-create
                                         :cset-name system-change-name
                                         :cset-description description
                                         :cset-abstract abstract-file-contents
                                         ))))))
      :update-workspace
      ;; We're out of master repository scope and back in workspace repository scope.  Create the
      ;; persistent change context and update the workspace.
      (lambda (workspace-repository workspace change-context)
          (declare (ignore workspace-repository change-context))
          (unless no-update?
            (versionref-update-timestamp (workspace-baseline-versionref workspace) time-stamp))
          (workspace-add-change-context workspace (persistent-change-context-create new-change-context))

          (when pathnames
            ;; Extract to be checked-out files
            ;; *PERFORMANCE* *FINISH* NAHA: we're doing a satellite txn per file here!
            ;; This is not acceptable long term.  Command logic should strive for
            ;; one transaction on the master, and one per satellite (where necessary).

            ;; **WARNING** this call, which initiates a second transaction on satellites within
            ;; the parent (master) transaction, assumes that we don't really re-open the database.
            ;; Which is ok with 0.87+6 in which transaction logic is reworked so that databases
            ;; are opened only once for a given master transaction (and in fact, server process
            ;; lifetime if things go well).

            ;; Note that we flush the db changes first to make sure that everyone will be working
            ;; with the same data
            (astore-flush-changes :verbose nil)
            (cmctl-file-checkout cm-session-context pathnames
                                 :server-relative server-relative))
          )))
  t)

(defun cmctl-change-uncreate (cm-session-context)
  "Toss out the current change context and revert the changes."
  (let ((reason (format nil "Uncreate change for user ~s" (cm-session-context-user-name cm-session-context))))
    (cmctl-call-with-user-txn cm-session-context
      :reason reason
      :txn-mode :read-write
      :require-active-change :require-active-change
      :receiver
      (lambda (ws-repository workspace change-context
                 master-repository-name master-repository master-catalog)
          (declare (ignore ws-repository master-repository-name))
          (with-cm-master-metaversion ((workspace-baseline-timestamp workspace))
            ;; Undo the file system operations.
            (let ((branch (workspace-master-pc-branch workspace master-repository)))
              (master-catalog-revert-change-context
               master-catalog
               cm-session-context
               workspace
               change-context
               (branch-owning-project branch)
               branch
               (cm-session-context-workspace-rooted-file-system cm-session-context)
               reason))))
      :update-workspace
      (lambda (workspace-repository workspace change-context)       ;; Toss out the change context.
          (declare (ignore workspace-repository change-context))
          (workspace-delete-change-context workspace))))
  t)

;; THE FOLLOWING LOGIC APPLIES EQUALLY TO CHANGE_ADD and CHANGE_REMOVE
;; (a) If there are no changes in the user's workspace, this is easy, and we simply
;; use ws-baseline + new change{add/remove} activity as a descendant for update.
;; (b) If there are user files in the workspace which would be overwritten by the
;; update, but which aren't part of a change context, copy the files to '.bak' files
;; and warn the users.  This isn't too hard.  A warning level should be noted.
;; (c) The hard case is handling conflicts arising from active entities in a change context.
;; Like other cases, we have to copy files which would be modified to '.bak' files.
;; Then we have to handle conflict on a case by case basis.
;; The general approach is that there is the workspace baseline(ancestor version)
;; (vesionref, plus any previously effected change_{add,remove} actions) -
;; a.k.a. virtual private branch.
;; Descendant one is the baseline plus the current change_{add,remove} cset(s).
;;   (D1 may have conflict purely on this basis alone, typically two files with same name)
;; Descendant two is the baseline plus current changes in the workspace.
;;
;;                             **** USE THIS APPROACH ****
;;
;; In general, if change_add would cause to appear two files with the same name, an error
;; is signalled.  The user will have to create a change against the current conflict and rename the file,
;; before change_add can succeed.  This is the simple approach we should take.
;; A more automated approach is described in the case below, but may not ever be implemented.
;;
;;                      **** CONSIDER THIS APPROACH LONG TERM ****
;;                     (or if the general approach is insufficient)
;;
;; (c1) If the workspace file is checked out, merge the renamed file with the new view
;;   as if by ws_update using the A/D/D relationships above, aftering copying the user's
;;   work (if it differs from repository version).
;; (c2) If the workspace file is renamed (as by file_rename), there are several scenarios:
;;      (actions for these cases not yet rectified - *TBD*:
;;       I'm not sure I've even cited cases correctly)
;; (c2.1) The change_add/remove would activate a file as the OLD file name,
;;        and the old name now exists as a wild file.
;;        SOLUTION: copy the old file, warn the user.
;; (c2.2) The change_add/remove would activate a file as the OLD file name,
;;        and the old name is not used.
;;        SOLUTION: general merge logic, file is renamed in change context
;;        I.e. extract as new file name, which must be derived from workspace records.
;; (c2.3) The change_add/remove would activate a file as the NEW file name,
;;        SOLUTION: signal an error, the user must rename the file first.
;; (c3) If the workspace file is removed, effects by incorporated csets are
;;      ignored.  This is a nop, the current change supersedes.
;; (c4) If the workspace file is added, and the add/removed cset would add a file
;;      by the same name, cancel the file_add, copy the file as a wild file,
;;      inform the user that they need to file_add it under another name.
;;      Might want to just signal an error for now, and make the user cancel the file_add, rename
;;      and re-add if appropriate.  Automated would be nicer, but this'll do at first.
;; (c5) If a wild file is in the way of a file to be added, the wild file is copied, unless it matches
;;      the contents of the repository file which would overwrite it.
;;
;; There is also a recognized problem in allowing only one cset at a time to be added/removed.
;; If bringing in a cset would cause a conflict and I don't have write access to the subsystem
;; in which case I have to bring in both of another's changes to reconcile conflict at the same
;; time.  So we anticipate allowing this, although we didn't when starting this routine.
;; In point of fact, The command to add/remove csets should allow both operations, on multiple csets,
;; at one time.

(defun cset-tuple-union (cset-tuples-left cset-tuples-right)
  "Given two cset-tuple of the form used in workspaces,
   ((satellite-did . cset-did*)*
   return the logical union of them."
  (let ((answer))
    (mapcar (lambda (subsystem)
                (let ((set-union (union (cdr (assoc subsystem cset-tuples-left))
                                        (cdr (assoc subsystem cset-tuples-right)))))
                  (if set-union
                      (push (cons subsystem set-union) answer)
                    )))
            (union (mapcar #'car cset-tuples-left)
                   (mapcar #'car cset-tuples-right)))
    answer))

(defun cset-tuple-difference (cset-tuples-left cset-tuples-right)
  "Given two cset-tuple of the form used in workspaces,
   ((satellite-did . cset-did*)*
   return the logical difference of them."
  (let ((answer))
    (mapcar (lambda (subsystem)
                (let ((set-diff (set-difference (cdr (assoc subsystem cset-tuples-left))
                                                (cdr (assoc subsystem cset-tuples-right)))))
                  (if set-diff
                      (push (cons subsystem set-diff) answer)
                    )))
            (union (mapcar #'car cset-tuples-left)
                   (mapcar #'car cset-tuples-right)))
    answer))

(defun cmctl-change-add-or-remove (cm-session-context
                                   change-names-to-add
                                   change-names-to-remove
                                   class-name
                                   update-workspace?
                                   &key (merge-port-activity nil))
  "Add or remove the named change-set to the current product view in cm-session-context and update
   the workspace.

   CHANGE-NAMES-TO-ADD names master change-sets to be added to the PC in cm-session-context.
   They must be strings, which match either the fully system-augmented change name, or an
   unambiguous user-specified portion of a change name.

   CHANGE-NAMES-TO-REMOVE names master change-sets to be deactivated in the PC in cm-session-context.
   Name match criteria are the same as for CHANGE-NAMES-TO-ADD

   CLASS-NAME is nil if there is no subsystem qualification, otherwise we fetch csets only for
   subsystem which map to the class. (it must be a string naming the class).
   *TBD*: we may someday allow multiple subsystems in a PC which map to the same class,
   in which case the CLASS-NAME parameter could be further refined through the addition of a
   SUBSYSTEM-NAME-LIST parameter.

   If update-workspace? is NIL, we do not bother to bring the workspace into sync with
   the new changes.  This is used to quickly load the database and should not be used
   during normal operation.

   Return true if we complete successfully, otherwise signal an approprate condition."
  (cm-session-context-require cm-session-context :repository-name :ws-id)
  (mapc (lambda (string)
            (check-type string string))
        (append change-names-to-add change-names-to-remove))
  (check-type class-name (or null string))
  (let* ((reason (if merge-port-activity
                     (format nil "Port changes: to workspace ~s"
                             (cm-session-context-ws-id cm-session-context))
                   (format nil "Process changes: add ~s, remove ~s~@[ (for class ~s)~] to workspace ~s"
                           change-names-to-add change-names-to-remove
                           class-name           ;above conditional include subsys only if present
                           (cm-session-context-ws-id cm-session-context))))
         ;; Alists keyed by subsystem did and valued by csets added to the subsystem
         ;; to be used in calls to workspace-{add,remove}-subsystem-change-sets
         (subsystem-add-alist nil)
         (subsystem-remove-alist)
         ;;(merge-context (make-instance 'conman-merge-context))
         (performed-three-way-merge nil)
         (merge-conflict-count 0)
         (merged-binary-files nil)
         )
    (declare (ignore performed-three-way-merge merge-conflict-count))

    (flet ((update-VPB (workspace-repository workspace change-context)
             (declare (ignore workspace-repository change-context))
             ;; Perform the actual workspace update and return true.
             (when merge-port-activity
               (setf (workspace-port-activity-acted-on-p workspace) t))
             (loop for (subsystem-did . cset-dids) in subsystem-add-alist
                   do (workspace-add-subsystem-change-sets workspace subsystem-did cset-dids))
             (loop for (subsystem-did . cset-dids) in subsystem-remove-alist
                   do (workspace-remove-subsystem-change-sets workspace subsystem-did cset-dids))))

      ;; *PERFORMANCE* want to do FS management in MVCC txn, bounded by WS txn, conceptually.
      ;; We need the workspace object to relativize pathnames.  We probably don't want to sit on
      ;; the ws-rep lock while updating the disk, but we do for now.

      (cmctl-call-with-user-txn cm-session-context
        :reason reason
        :txn-mode :read-write
        :require-active-change nil
        ;; If not updating the workspace, don't generate workspace out-of-sync errors.
        ;; Needed for  csets_from_file -punt-workspace-update
        :transition-action (and update-workspace? :error)
        :receiver
        (lambda (workspace-repository workspace change-context
                   master-repository-name master-repository master-catalog)
            (declare (ignore workspace-repository))
            (error-if-product-reference-workspace workspace
                                                  (if merge-port-activity
                                                      "port"
                                                    "change add/remove"))
            #||
            ;; HP wants to be able to use cset_add and cset_remove
            ;; while there is porting activity.
            (when (workspace-has-port-activity-p workspace)
              (conman-signal-error
               *cm-returns-error-port-activity-in-progress*
               "You can't add or remove changes while the workspace is undergoing port activity."))
            ||#
            (let* ((vpb-baseline-timestamp (workspace-baseline-timestamp workspace)))
              (with-cm-master-metaversion (vpb-baseline-timestamp)
                (let* ((pc-branch (workspace-master-pc-branch workspace master-repository))
                       (pc (branch-owning-project pc-branch))
                       (subsystem nil)
                       (master-csets-to-add
                        (mapcar (lambda (change-name)
                                    (master-catalog-resolve-change-set-name master-catalog change-name))
                                change-names-to-add))
                       (master-csets-to-remove
                        (mapcar (lambda (change-name)
                                    (master-catalog-resolve-change-set-name master-catalog change-name))
                                change-names-to-remove))
                       (bad-cset nil))

                  (cmctl-guarantee-branch-not-frozen pc-branch)


                  ;; *FINISH*: pc-find-subsystem should be qualified by pc-branch.
                  ;; Scope master view by workspace VPB

                  ;; Do the pc-find-subsystem-for-class in correct version context
                  ;; Note that we should potentially pass in the correct metaversion for
                  ;; subsystem name resolution in this call to pc-find-subsystem-for-class,
                  ;; but the time-based uniqueness of subsystem names means that the latest
                  ;; metaversion will be ok too.
                  (setq subsystem (and class-name (pc-find-subsystem-for-class
                                                   pc-branch class-name :error-if-missing t)))
                  ;; Figure out if the cset is already added/removed from product baseline or workspace
                  ;; VPB. If so, we don't have to do anything.
                  (when (setq bad-cset
                              (some (lambda (master-cset)
                                        (and (workspace-master-VPB-cset-active?
                                              workspace master-repository master-catalog master-cset
                                              subsystem pc-branch)
                                             master-cset)) ;return value for SOME predicate
                                    master-csets-to-add))
                    (conman-signal-error *cm-returns-error-change-already-present*
                                         "The requested change, ~s, is already active in your workspace."
                                         (with-cm-master-metaversion ()
                                           (object-user-name bad-cset))))
                  (when (setq bad-cset
                              (some (lambda (master-cset)
                                        (and (not (workspace-master-VPB-cset-active?
                                                   workspace master-repository master-catalog master-cset
                                                   subsystem pc-branch))
                                             master-cset))
                                    master-csets-to-remove))
                    (conman-signal-error *cm-returns-error-change-already-absent*
                                         "The requested change, ~s, is already inactive in your workspace."
                                         (with-cm-master-metaversion ()
                                           (object-user-name bad-cset))))

                ;; Derive ancestor and descendant views of the workspace.
                ;; Execute Joe's merge logic in the supplied function, stash any results
                (with-cm-master-metaversion ()
                  (let* ((subsystems
                          (if subsystem
                              (list subsystem)
                            ;; Establish meta-version, then establish version, then get subsystems
                            (with-cm-master-metaversion (vpb-baseline-timestamp)
                              (pc-get-subsystem-list pc-branch))))

                         (added-satellite-csets
                          (mapcan (lambda (addition)
                                      (master-catalog-list-satellite-cset-dids master-catalog addition))
                                  master-csets-to-add))
                         (removed-satellite-csets
                          (mapcan (lambda (removal)
                                      (master-catalog-list-satellite-cset-dids master-catalog removal))
                                  master-csets-to-remove))
                         (vpb-old-added-satellite-cset-tuples
                          (workspace-added-class-cset-tuples workspace))
                         (vpb-old-removed-satellite-cset-tuples
                          (workspace-removed-class-cset-tuples workspace))
                         (port-activity-adds-and-removes
                          (when merge-port-activity
                            (multiple-value-bind (adds-alist removes-alist)
                                (workspace-port-activity-collect-adds-and-deletes-by-subsystem
                                 workspace)
                              ;; Ignore those that were already
                              ;; added.  They are still in the
                              ;; port activity of the wprkspace
                              ;; but don't need to be added to
                              ;; the vpb changes again.
                              (setq adds-alist
                                    (cset-tuple-difference adds-alist
                                                           vpb-old-added-satellite-cset-tuples))
                              (setq removes-alist
                                    (cset-tuple-difference removes-alist
                                                           vpb-old-removed-satellite-cset-tuples))
                              (cons adds-alist removes-alist))))
                         (satellite-cset-tuples-to-add
                          (cset-tuple-union
                           (car port-activity-adds-and-removes)
                           (mapcan (lambda (subsystem)
                                       (let ((additions
                                              (remove-if (lambda (cset)
                                                             (not (subsystem-cset-may-affect-subsystem-p
                                                                   cset
                                                                   subsystem)))
                                                         added-satellite-csets)))
                                         (if additions
                                             (list (cons (distributed-object-identifier subsystem)
                                                         additions))
                                           nil)))
                                   subsystems)))
                         (satellite-cset-tuples-to-remove
                          (cset-tuple-union
                           (cdr port-activity-adds-and-removes)
                           (mapcan (lambda (subsystem)
                                       (let ((removals
                                              (remove-if (lambda (cset)
                                                             (not (subsystem-cset-may-affect-subsystem-p
                                                                   cset
                                                                   subsystem)))
                                                         removed-satellite-csets)))
                                         (if removals
                                             (list (cons (distributed-object-identifier subsystem)
                                                         removals))
                                           nil)))
                                   subsystems)))
                         (vpb-new-added-satellite-cset-tuples
                          (cset-tuple-union (cset-tuple-difference vpb-old-added-satellite-cset-tuples
                                                                   satellite-cset-tuples-to-remove)
                                            satellite-cset-tuples-to-add))
                         (vpb-new-removed-satellite-cset-tuples
                          (cset-tuple-union (cset-tuple-difference vpb-old-removed-satellite-cset-tuples
                                                                   satellite-cset-tuples-to-add)
                                            satellite-cset-tuples-to-remove))
                         remove-from-remove-list
                         remove-from-add-list)

                    (setq subsystem-add-alist satellite-cset-tuples-to-add)
                    (setq subsystem-remove-alist satellite-cset-tuples-to-remove)

                    (debug-message 3 "~%added-satellite-csets: ~s~
                                        ~%removed-satellite-csets: ~s~
                                        ~%satellite-cset-tuples-to-add: ~s~
                                        ~%satellite-cset-tuples-to-remove: ~s~
                                        ~%vpb-old-added-class-cset-tuples (ws): ~s~
                                        ~%vpb-old-removed-class-cset-tuples (ws): ~s~
                                        ~%vpb-new-added-satellite-cset-tuples: ~s~
                                        ~%vpb-new-removed-satellite-cset-tuples: ~s"
                                   added-satellite-csets
                                   removed-satellite-csets
                                   satellite-cset-tuples-to-add
                                   satellite-cset-tuples-to-remove
                                   vpb-old-added-satellite-cset-tuples
                                   vpb-old-removed-satellite-cset-tuples
                                   vpb-new-added-satellite-cset-tuples
                                   vpb-new-removed-satellite-cset-tuples)

                    (multiple-value-setq (remove-from-remove-list remove-from-add-list)
                      (pc-partition-subsystems-and-get-remove-redundant-csets-lists
                       pc pc-branch
                       vpb-baseline-timestamp
                       vpb-baseline-timestamp
                       :subsystem subsystem
                       :VPB-old-added-satellite-cset-dids   vpb-old-added-satellite-cset-tuples
                       :VPB-old-removed-satellite-cset-dids vpb-old-removed-satellite-cset-tuples
                       :VPB-new-added-satellite-cset-dids   vpb-new-added-satellite-cset-tuples
                       :VPB-new-removed-satellite-cset-dids vpb-new-removed-satellite-cset-tuples))
                    (when remove-from-remove-list
                      (setq vpb-new-removed-satellite-cset-tuples
                            (cset-tuple-difference vpb-new-removed-satellite-cset-tuples
                                                   remove-from-remove-list)))
                    (when remove-from-add-list
                      (setq vpb-new-added-satellite-cset-tuples
                            (cset-tuple-difference vpb-new-added-satellite-cset-tuples
                                                   remove-from-add-list)))
                    (debug-message 4 "Final vpb-new-added-satellite-cset-tuples   ~s~%"
                                   vpb-new-added-satellite-cset-tuples)
                    (debug-message 4 "Final vpb-new-removed-satellite-cset-tuples ~s~%"
                                   vpb-new-removed-satellite-cset-tuples)

                    (when update-workspace?
                      (multiple-value-setq (performed-three-way-merge
                                            merge-conflict-count
                                            merged-binary-files)
                        (master-catalog-update-file-system
                         master-catalog
                         master-repository-name
                         (cm-session-context-workspace-rooted-file-system cm-session-context)
                         "workspace"
                         pc
                         pc-branch
                         vpb-baseline-timestamp
                         vpb-baseline-timestamp
                         :change-context change-context
                         :VPB-old-added-satellite-cset-dids   vpb-old-added-satellite-cset-tuples
                         :VPB-old-removed-satellite-cset-dids vpb-old-removed-satellite-cset-tuples
                         :VPB-new-added-satellite-cset-dids   vpb-new-added-satellite-cset-tuples
                         :VPB-new-removed-satellite-cset-dids vpb-new-removed-satellite-cset-tuples)))
                    ))))))
        :update-workspace #'update-vpb)
      (when merged-binary-files
        (conman-signal-error *cm-returns-error-cant-merge-binary-files*
                             "No automatic merges on binary files.  The conflicting files have been UNCO'd."
                             nil))
      t)))

(defun cmctl-csets-from-file (cm-session-context
                              pathname
                              class-name
                              update-workspace?)
  "Read through the given file.  Find the cset names prepended by + or - (+ meaning add and
   - meaning remove).  The cset names will be separated by whitespace and/or newlines.  There
   will be no other information in the file.

   For each cset-name given in the file, add or remove that cset as appropriate.  All such
   additions or removals are done in a view (a single transaction on the database).

   This is done by constructing a list of csets to add and a list of those to remove.  The
   two lists are then passed to cmctl-change-add-or-remove for the actual processing.  This
   means that the adds and removes will be done at separate times.  If the user had them
   interspersed, he may be surprised by this.  They will otherwise be done in his order.

   FILE_NAME is the name of the file to read to get the information from.

   CLASS-NAME is nil if there is no subsystem qualification, otherwise we fetch csets only for
   subsystem which map to the class. (it must be a string naming the class).
   *TBD*: we may someday allow multiple subsystems in a PC which map to the same class,
   in which case the CLASS-NAME parameter could be further refined through the addition of a
   SUBSYSTEM-NAME-LIST parameter.

   Return true if we complete successfully, otherwise signal an approprate condition."
  (cm-session-context-require cm-session-context :repository-name :ws-id)
  (check-type class-name (or null string))
  (let* ((reason (format nil "Add or Remove csets named in the file ~s~@[ (for class ~s)~] to workspace ~s"
                         pathname
                         class-name             ;above conditional include subsys only if present
                         (cm-session-context-ws-id cm-session-context)))
         ;; read the contents of the file into the file-contents list for processing
         (file-contents (tokenize-string-cli
                         (cmctl-retrieve-abstract-file-contents cm-session-context
                                                                (cm-session-context-file-system-agent cm-session-context)
                                                                pathname)))
         (cset-add-list nil)
         (cset-remove-list nil)
         )
    (declare (ignore reason))                   ; Do you think it is wise to ignore reason?
    (unless file-contents
      (conman-signal-error *cm-returns-error-missing-change-name*
                           "Csets_from_file expects at least one cset name in the file"))

    (dolist (token file-contents t)
      (debug-message 4 "list element: ~a~%" token)
      (let ((toke (subseq token 1)))

        ;; test for +/- separated from the cset name
        (if (string= toke "")
            (conman-signal-error *cm-returns-error-bogus-arguments*
                                 "Csets_from_file expects a + or - to immediately precede a cset name (~s) in the file."
                                 token))

        ;; test for duplicate entries, checking BEFORE we add to the lists
        ;; (format t "Looking for: ~s~%" toke)
        ;; (format t "   in: ~s~%" cset-both-list)
        (if (or (member toke cset-add-list :test #'string=)
                (member toke cset-remove-list :test #'string=))
            (conman-signal-error *cm-returns-error-change-already-present*
                                 "Csets-from-file found a duplicate cset (~s) in the list" toke))

        (cond ((char= (char token 0) #\+)
               (setq cset-add-list (push toke cset-add-list)))

              ((char= (char token 0) #\-)
               (setq cset-remove-list (push toke cset-remove-list)))

              ;; Should we allow comments in the file?  Using "#" or ";" ???
              ;;   How do we determine end-of-line?
              (t (conman-signal-error *cm-returns-error-bogus-arguments*
                                      "Csets_from_file expects a + or - to immediately precede a cset name (~s) in the file."
                                      token))
              )))

    ;; reverse the lists so they are in the user's order (since we constructed them by pushing)
    (setq cset-add-list (nreverse cset-add-list))
    (setq cset-remove-list (nreverse cset-remove-list))

    (debug-message 4 "cset-add-list:    ~a~%" cset-add-list)
    (debug-message 4 "cset-remove-list: ~a~%" cset-remove-list)
    (unless (or cset-add-list cset-remove-list)
      (conman-signal-error *cm-returns-error-missing-change-name*
                           "Csets_from_file expects at least one cset name in the file"))

    (cmctl-change-add-or-remove cm-session-context
                                cset-add-list
                                cset-remove-list
                                class-name
                                update-workspace?)
    ))

(defun cmctl-workspace-relative-logical-pathnames (pathnames workspace cm-session-context server-relative)
  "Convert absolute pathnames to workspace-relative pathnames and return them.

  PATHNAMES should be a single pathname, or a list of pathnames.

  If they are server-relative, extract the workspace directory from the workspace and relativize them.
  If they are client-relative, get the client's idea of the workspace
  root and relativize it.  It would be nice to relativize them earlier,
  but we require access to the workspace object in the workspace repository to do this."
  (let ((root-path (if server-relative
                       (workspace-path workspace)
                     (guarantee-absolute-directory-pathname
                      (cm-session-context-workspace-root cm-session-context)))))
    (flet ((xform (pathname) (neutralize-pathname pathname root-path)))
      (if (listp pathnames)
          (mapcar #'xform pathnames)
        (xform pathnames)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data structure for keeping track of files that the file-* commands
;;; are operating on.

;;; **** NOTE **** CMCTL-PATHNAME-WORK **** NOTE ****
;;; THIS SHOULD BE CONVERTED TO USE DEFINE-TENN-CLASS, it is not conformant with project standards.
;;; Refer to \ts50\coding-conventions for details.

(defclass cmctl-pathname-work ()
  ((user-pathname
    :initarg :user-pathname
    :initform (error ":USER-PATHNAME must be specified")
    :reader pw-user-pathname
    :documentation
    "The pathname as the user entered it.")
   (workspace-relative-pathname
    :initarg :workspace-relative-pathname
    :initform (error ":WORKSPACE-RELATIVE-PATHNAME must be specified.")
    :accessor pw-workspace-relative-pathname
    :documentation
    "The result of calling CMCTL-WORKSPACE-RELATIVE-LOGICAL-PATHNAMES on the USER-PATHNAME.")
   (satellite-project-did
    :initform nil
    ;; slot value would remain NIL if
    ;; MASTER-CATALOG-FILE-NAMES-RESOLVER was unable to associate a
    ;; subsystem with the directory of the pathname.
    :accessor pw-satellite-project-did
    :documentation
    "DID of the satellite project of the file, as would be determined by
     MASTER-CATALOG-RESOLVE-DISK-FILE-NAME or MASTER-CATALOG-FILE-NAMES-RESOLVER.")
   (satellite-project-logical-subdirectory
    :accessor pw-satellite-project-logical-subdirectory)
   (file-did
    :accessor pw-file-did
    :initform nil
    :documentation
    "The FILE DID corresponing to the so-named file, as would be determined
     by MASTER-CATALOG-FILE-NAMES-RESOLVER.")
   (subsystem-relative-pathname
    :initform nil
    :accessor pw-subsystem-relative-pathname
    :documentation
    "Logical pathname of the file expressed relative to the subsystem's root directory.")
   (subsystem
    :initform nil
    :accessor pw-subsystem
    :documentation "Active SUBSYSTEM in the MASTER repository to which satellite data applies.")
   (users-with-open-changes
    :initform nil
    :accessor pw-users-with-open-changes
    :documentation
    "A place to collect the DIDs of users who also have a change open for this file.")
   (change-context-fileadd
    :reader pw-change-context-fileadd
    :documentation
    "A place to cache a relevant CC-FILEADD record from the current change context")
   (change-context-fileremove
    :reader pw-change-context-fileremove
    :documentation
    "A place to cache a relevant CC-FILEREMOVE record from the current change context")
   (change-context-filerename-old-name
    :reader pw-change-context-filerename-old-name
    :documentation
    "A place to cache a CC-FILERENAME record from the current change context for which
     the user specified pathname represents the old filename")
   (change-context-filerename-new-name
    :reader pw-change-context-filerename-new-name
    :documentation
    "A place to cache a CC-FILERENAME record from the current change context for which
     the user specified pathname represents the new filename")
   )
  (:documentation
   "This data structure is just used for bookkeeping by the file_add, file_remove,
    file_change and file_rename commands.  None of these survive the lifespan of
    a single command, nor should they survive the life of cmctl-operate-on-files,
    since they represent data which is bounded by that function
    (repository isntantiations, transaction-bounded data, etc.)

    Given a bunch of file names as the user typed them, we need to translate them
    into various pathname spaces (workspace relative, subsystem relative) and
    ultimately to a FILE-DID if the file exists in the repository.  If there is a
    problem then what we report to the user should use the original file name that
    they typed.  Therefore, it is useful to keep an association among all of the
    different representations for a given file during the course of a single command."))

(defmethod pw-unknown-subsys-dir-p ((pw cmctl-pathname-work))
  "If true, CMCTL-OPERATE-ON-FILES and MASTER-CATALOG-FILE-NAMES-RESOLVER were
   not able to determine whay subsystem the user supplied pathname was in."
  (not (pw-satellite-project-did pw)))

(defmethod print-object ((pw cmctl-pathname-work) stream)
  (print-unreadable-object (pw stream :type t :identity t)
    (format stream "~a ~a"
            (pw-file-did pw)
            (pw-workspace-relative-pathname pw))))

(defmethod pw-find-in-change-context-additions ((pw cmctl-pathname-work) change-context)
  "Returns a list of CC-FILEADDs from CHANGE-CONTEXT
   corresponding to the file identified in PW."
  (if (slot-boundp pw 'change-context-fileadd)
      (slot-value  pw 'change-context-fileadd )
    (let ((found (change-context-find-file-additions change-context
                                                     (pw-subsystem-relative-pathname pw)
                                                     (pw-satellite-project-did pw))))
      (assert (null (cdr found)))
      (setf (slot-value pw 'change-context-fileadd) found)
      found)))

(defmethod pw-find-in-change-context-changes ((pw cmctl-pathname-work) change-context)
  "Returns a list of CC-FILECHANGEs from CHANGE-CONTEXT
   corresponding to the file identified in PW."
  (when (pw-file-did pw)
    (let ((found (change-context-find-file-changes change-context (pw-file-did pw))))
      (assert (null (cdr found)))
      found)))

(defmethod pw-find-in-change-context-removals ((pw cmctl-pathname-work) change-context)
  "Returns a list of CC-FILEREMOVEs from CHANGE-CONTEXT
   corresponding to the file identified in PW."
  (if (slot-boundp pw 'change-context-fileremove)
      (slot-value  pw 'change-context-fileremove)
    (setf (slot-value pw 'change-context-fileremove)
          (when (pw-file-did pw)
            (let ((found (change-context-find-file-removals change-context (pw-file-did pw))))
              (assert (null (cdr found)))
              found)))))

(defmethod pw-find-in-change-context-renames ((pw cmctl-pathname-work) change-context)
  "Returns a list of CC-FILERENAMEs from CHANGE-CONTEXT
   whose FILE-DIDs correspond to the file identified in PW."
  ;; Cache the result in the slot provided for that purpose
  (if (slot-boundp pw 'change-context-filerename-old-name)
      (slot-value  pw 'change-context-filerename-old-name)
    (setf (slot-value pw 'change-context-filerename-old-name)
          (when (pw-file-did pw)
            (let ((found (change-context-find-file-renames change-context (pw-file-did pw))))
              (assert (null (cdr found)))
              found)))))

(defmethod pw-find-in-change-context-renamed-to ((pw cmctl-pathname-work) change-context)
  "Returns a list of CC-FILERENAMEs from CHANGE-CONTEXT
   whose new names correspond to the file identified in PW."
  ;; Cache the result in the slot provided for that purpose
  (if (slot-boundp pw 'change-context-filerename-new-name)
      (slot-value  pw 'change-context-filerename-new-name)
    (let ((found (change-context-find-file-renamed-to change-context
                                                      (pw-subsystem-relative-pathname pw)
                                                      (pw-satellite-project-did pw))))
      (assert (null (cdr found)))
      (setf (slot-value pw 'change-context-filerename-new-name) found)
      found)))

(defmethod pw-find-in-change-context ((pw cmctl-pathname-work) change-context)
  ;; NOTE:  See comment in PW-RESOLVE-WITH-RESPECT-TO-CHANGE-CONTEXT.
  (pw-find-in-change-context-additions  pw change-context)
  (pw-find-in-change-context-removals   pw change-context)
  (pw-find-in-change-context-renames    pw change-context)
  (pw-find-in-change-context-renamed-to pw change-context))


(defmethod pw-resolve-with-respect-to-change-context ((pw cmctl-pathname-work) change-context)
  ;; We assume that the filenames that the user gives us correspond to
  ;; the state of their file system as we've updated it to conform to
  ;; their changes.
  ;; If a file name refers to both the old name of one FILE and the
  ;; new name of another FILE, we assume that the FILE object
  ;; associated with the new name is the correct one.
  ;; If they remove a file and then add a new file with the same name
  ;; in a single delta, we assume that that case will be caught as an
  ;; error by the implementation of the command.
  ;; Since we need to know the pathname relative to the subsystem root
  ;; in order to lookup the change context records relevant to the
  ;; file, this must be called after master catalog resolution.
  ;; Since master catalog resolution might have determined the wrong
  ;; FILE DID anyway (it being unaware of any pending file renames),
  ;; we just overwrite the PW-FILE-DID with the value we believe to be
  ;; correct.
  (pw-find-in-change-context pw change-context)
  (let ((cc-filerename (car (pw-change-context-filerename-new-name pw))))
    (when cc-filerename
      ;; pw is new name of a renamed repository file: fetch its did from cc-filerename
      (setf (pw-file-did pw) (cc-filerename-file-did cc-filerename))))
  pw)

(defmethod pw-note-user-with-open-change ((pw cmctl-pathname-work) user-did)
  (pushnew user-did (pw-users-with-open-changes pw)))

(defmethod pw-determine-workspace-relative-pathname-from-user-pathname
    ((pw cmctl-pathname-work) cm-session-context workspace server-relative)
  ;; Must be called from within the context of a workspace repository transaction.
  (setf (pw-workspace-relative-pathname pw)
        (cmctl-workspace-relative-logical-pathnames (pw-user-pathname pw) workspace
                                                    cm-session-context server-relative))
  pw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmctl-who-has-em-checked-out (things workspace-repository
                                     &key (file-did-accessor #'pw-file-did)
                                          note-users-function)
  "
  ;; There's an element of THINGS for each file you want to know
  ;; about.  THINGS would typically be a list of CC-FILECHANGES from a
  ;; WORKSPACE's CHANGE-CONTEXT.

  ;; This function is used to find out what WORKSPACEs (and users)
  ;; have the files identified by THINGS checked out.

  ;; FILE-DID-ACCESSOR returns the FILE-DID of a file, given some
  ;; element of THINGS.  If it returns NIL, then that entry of THINGS
  ;; is ignored.

  ;; NOTE-USERS-FUNCTION is called on a member of THINGS, a WORKSPACE
  ;; and a user reference (currently the user name as a string but
  ;; eventually the DID of a user object) to tell the caller that that
  ;; user has that file checked out.

  ;; Returns a duplicate-removed list of userid information, where
  ;; userids is currently strings and later will be core::user
  ;; objects.
  "
  ;; **NOTE** **FINISH** currently conman uses string userids, later
  ;; we will return core::user objects.

  ;; For each workspace in workspace repository
  (debug-message 3 "cmctl-who-has-em-checked-out entered")
  (let ((result-users nil))
    (map-over-workspaces
     (lambda (workspace)
         ;; Derive change context, and owner
         (debug-message 4 "cmctl-who-has-em-checked-out workspace: ~s" workspace)
         (let ((change-context (workspace-persistent-change-context workspace)))
           (debug-message 4 "cmctl-who-has-em-checked-out change-context: ~s" change-context)
           (when change-context
             (dolist (thing things)
               (debug-message 4 "cmctl-who-has-em-checked-out ref-filechange: ~s" thing)
               ;; Derive ref-file-did
               (let ((ref-file-did (funcall file-did-accessor thing)))
                 (debug-message 4 "cmctl-who-has-em-checked-out ref-file-did: ~s" ref-file-did)
                 (when ref-file-did
                   (when (change-context-find-file-changes change-context ref-file-did)
                     (let ((ws-owner (workspace-owner-id workspace)))
                       (when note-users-function
                         (funcall note-users-function thing workspace ws-owner))
                       (push ws-owner result-users)))))))))
     workspace-repository)
    (remove-duplicates result-users :test #'string=)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-change-context-persistent-in-workspace (workspace-repository workspace change-context)
  "Given a workspace-repository (which is ignored),
   the workspace, and
   the change-context,
   update the persistent version of the workspace's change-context"
  (declare (ignore workspace-repository))
  ;; Outside the master txn, so that the current repository is the workspace repository,
  ;; we update the persistent workspace data with the transient information in change-context
  (persistent-change-context-update
   (workspace-persistent-change-context workspace) change-context))

(defun cmctl-operate-on-files-guts (cm-session-context reason
                                    master-repository master-repository-name master-catalog
                                    pathnames-work pc branch
                                    change-context workspace ws-repository
                                    func)
  "this is the guts of the cmctl-operate-on-filfes function.  It has been separated into
   a new function so that it can also be called (in a r/o manner) fro cmctl-uncheckout-file.

   See cmctl-operate-on-files for details of the arguments.  They are all standard except
   for PATHNAMES_WORK which is a list of files to operate on (may or may not be wild files -
   you will have to check) and FUNC which is what ever the caller decides."
  (cmctl-guarantee-branch-not-frozen branch)
  ;; For each file, determine its satellite project, and its FILE DID, if any.
  (master-catalog-file-names-resolver
      master-catalog (cm-session-context-repository-name cm-session-context)
      pc branch pathnames-work
      (lambda (pathname-work file-did satellite-project-did subsystem subsys-relative-pathname)
          (setf (pw-satellite-project-did pathname-work) satellite-project-did)
          ;; Note: if filename is the new name of a renamed file, its DID is fetched below
          ;; from the CC-FILERENAME record via pw-resolve-with-respect-to-change-context
          (setf (pw-file-did  pathname-work) file-did)
          (setf (pw-subsystem pathname-work) subsystem)
          (setf (pw-subsystem-relative-pathname pathname-work) subsys-relative-pathname)
          (pw-resolve-with-respect-to-change-context pathname-work change-context))
      :pathname-key #'pw-workspace-relative-pathname
      :workspace workspace
      :errorp nil)
  ;; This is totally wrong from a txn perspective, the above file-names-resolver
  ;; is opening satellite transactions, and now FUNC might have to do it again.
  ;; *PERFORMANCE* *FINISH*
  (funcall func pathnames-work change-context
           master-repository master-repository-name master-catalog pc reason
           workspace ws-repository))

(defun cmctl-operate-on-files (command-name cm-session-context
                               pathnames server-relative reason func)
  "Common code for FILE-ADD, FILE-REMOVE, FILE-CHECKOUT, FILE-RENAME, etc.

   FUNC will be called with a list of CMCTL-PATHNAME-WORK objects
   constructed and initialized from PATHNAMES and with a CONMAN-CHANGE-CONTEXT,
   among other things.

   The complete parameter list for FUNC is:
   1) cmctl-pathname-work-list          ;list of cmctl-pathname-work objects
   2) cmctl-change-context
   3) master-repository                 ;open for read access
   4) master-repository-name (used to open satellite repositories)
   5) master-catalog
   6) pc
   7) txn reason used to open master/workspace repositories
   8) workspace object active int he open worksapce repository
   9) workspace-repository currently open and in a write transaction

   You might expect that many of the above args could be passed in the cmctl-pathname-work
   object, but they really span the unit of work described by many cmctl-pathname-work objects,
   at least for now, and so they're passed outside of that object.

   To the extent that the elements of PATHNAMES might be associated with FILEs
   in the repository, or at least fall in to satellite project subdirectories,
   the PATHNAME-WORK objects will be initialized accordingly.
   FUNC is called from within the context of a master repository transaction."
  (cm-session-context-require cm-session-context
                              :user-name :repository-name :ws-id :current-directory)
  (unless pathnames
    (conman-warning-no-files-specified command-name))
  ;; *FINISH*: rework transaction logic for MVCC/RO master/satellites, and R/W only on workspace repository
  (cmctl-call-with-user-txn cm-session-context
    :reason reason
    :txn-mode :read-write
    :require-active-change :require-active-change
    :receiver
    (lambda (ws-repository workspace change-context
               master-repository-name master-repository master-catalog)
        (with-cm-master-metaversion ((workspace-baseline-timestamp workspace))
          (error-if-product-reference-workspace workspace "file add/checkout/remove/rename")
          (let* ((pathnames-work
                  (mapcar (lambda (user-pathname)
                              (make-instance 'cmctl-pathname-work
                                :user-pathname user-pathname
                                :workspace-relative-pathname
                                (cmctl-workspace-relative-logical-pathnames
                                 user-pathname workspace cm-session-context server-relative)))
                          pathnames))
                 (branch (workspace-master-pc-branch workspace master-repository))
                 (pc (branch-owning-project branch))
                 )
            (cmctl-operate-on-files-guts cm-session-context reason
                                         master-repository master-repository-name master-catalog
                                         pathnames-work pc branch
                                         change-context workspace ws-repository
                                         func)
            )))
    :update-workspace
    #'make-change-context-persistent-in-workspace)
  t)

(defun cmctl-file-add (cm-session-context description pathnames content-type-declared
                       &key server-relative)
  "Add one or more files to the change-context associated with a workspace.
   Signal an error if the files don't exist, etc.

   PATHNAMES are absolute paths to the files.

   Remember that WS-ID may be an integer which uniquely describes a workspace, or it may
   be a directory path which we need to resolve to a unique ws-id.

   See CM-CLI-FILE-ADD documentation for more details.
   Return value unimportant as long as it isn't :QUIT.  We return T."
  (cm-session-context-require cm-session-context
                              :user-name :repository-name :ws-id :current-directory)
  (check-type description (or null string))

  (unless pathnames
    (conman-signal-error *cm-returns-error-bogus-arguments*
                         "No file names were specified for addition to the current change."))

  (let (file-descriptor)
    ;; For each file, verify that the file exists, and that its position is rooted
    ;; by a subsystem.  If we pass muster, store the canonicalized path in the
    ;; file-additions portion of the workspace change context.
    (cmctl-operate-on-files "file_add" cm-session-context pathnames server-relative
                            (format nil "Add files for user ~s, workspace ~s"
                                    (cm-session-context-user-name cm-session-context)
                                    (cm-session-context-ws-id cm-session-context))
      (lambda (cmctl-pathname-work-list change-context
                 master-repository master-repository-name master-catalog
                 pc reason workspace workspace-repository)
          (declare (ignore master-repository master-repository-name master-catalog
                           pc reason))
          (dolist (pw cmctl-pathname-work-list)
            (debug-message 3 "File_add operating on ~s" pw)
            (cond
             ;; MASTER-CATALOG-FILE-NAMES-RESOLVER was unable to resolve the file
             ((pw-unknown-subsys-dir-p pw)
              (conman-signal-error *cm-returns-error-file-not-in-any-subsystem-directory*
                                   "The file ~A is not in any known subsystem directory." (pw-user-pathname pw)))
             ;; File already in repository (including file is new name of repository file)
             ;; What if repos file has pending rename or remove in change-context?
             ;; Renames are handled by pw-resolve-with-respect-to-change-context (see below
             ;; comment) but removes go unnoticed here, effectively disallowing name reuse.
             ;; e.g. you can't do:  rename foo,bar; add foo This could instead be done via:
             ;; copy foo,bar; change foo,  but the two may differ in semantics. In the first
             ;; case the add'ed foo may be unrelated to the original foo, whereas in the
             ;; latter the changed foo is an evolution of the same object.
             ((pw-file-did pw)
              ;; If user specified path is new name of an repository file we also get here
              ;; since user path has been pw-resolve'd with respect to the change context.
              ;; todo: spec 3.3 says to tell user to undo file_remove or -force etc, but
              ;; that is not done here. Is spec obsolete here?
              (conman-warning-file-already-exists-in-repository (pw-user-pathname pw)))
             ;; Warn and skip if file not present on disk
             ((not (setq file-descriptor
                         (file-system-probe-file (cm-session-context-workspace-rooted-file-system cm-session-context
                                                                                                  server-relative workspace)
                                                 (pw-workspace-relative-pathname pw))))
              (conman-warning-file-not-on-disk (pw-user-pathname pw)))
             ;; If the pathname names a directory then skip it and warn.
             ((file-descriptor-is-directory? file-descriptor)
              (conman-signal-warning *cm-returns-warning-directory-filename-ignored*
                                     "~S will not be added because it is a directory." (pw-user-pathname pw)))
             ;; File already added in current delta
             ((pw-change-context-fileadd pw)
              (conman-warning-file-already-added-in-current-change (pw-user-pathname pw)))
             ;; File larger than implementation limit.
             ((and (eq (file-descriptor-content-type file-descriptor) :binary)
                   (>= (file-descriptor-size file-descriptor) +binary-file-absolute-size-limit+))
              (conman-warning-file-way-too-big (pw-user-pathname pw)))
             ;; File larger than `normal'.
             ((and (eq (file-descriptor-content-type file-descriptor) :binary)
                   (>= (file-descriptor-size file-descriptor) *binary-file-size-limit*))
              (conman-warning-file-too-big (pw-user-pathname pw)))
             ;; Normal case: adjoining file to list of added files in change context
             (t (let ((new (cc-fileadd-create (pw-subsystem-relative-pathname pw)
                                              :project-did (pw-satellite-project-did pw)
                                              :content-type content-type-declared)))
                  (setf (cc-file-base-change-subsystem-did new)
                        (distributed-object-identifier (pw-subsystem pw)))
                  ;; cons in correct repository (cc log entries are pushed to a pclos astore-list)
                  (with-current-repository (workspace-repository)
                    (change-context-add-file change-context new)))))))))
  t)

(defun get-subsequent-log-entries (workspace-repository change-context pw)
  "Find pw's most recent log entry in change-context (skipping new-filrenames and newfileremoves)
   and return the tail of the log from that point on, i.e. all subsequent log entries."
  (with-current-repository (workspace-repository)
    (tail-labels ((luup (entries subsequent)
                    (if (null entries) nil      ; pw not found in log
                      (let ((this-entry (astore-list-car entries)))
                        (if (etypecase this-entry
                              (cc-fileadd (equal (cc-fileadd-pathname this-entry) ; found it!
                                                 (pw-subsystem-relative-pathname pw)))
                              (cc-new-filerename nil)
                              (cc-new-fileremove nil) ; from (new)file-remove, add-file-undo
                              ;; At log time of next 3 cases file was in repos, so no prior file_adds
                              (cc-filechange (eq (cc-filechange-file-did this-entry) (pw-file-did pw)))
                              (cc-fileremove (eq (cc-fileremove-file-did this-entry) (pw-file-did pw)))
                              (cc-filerename (eq (cc-filerename-file-did this-entry) (pw-file-did pw))))
                            ;; Found latest pw change entry; return it with subsequent log entries
                            (cons this-entry subsequent)
                          ;; Not a pw entry: continue searching the log
                          (luup (astore-list-cdr entries) (cons this-entry subsequent)))))))
      (luup (rfm::change-context-log change-context) () ))))

(defun cmctl-undo-file-add (cm-session-context pathnames server-relative)
  "Nullify the effect of a recent CMCTL-FILE-ADD on the CHANGE-CONTEXT."
  (cmctl-operate-on-files "undo file_add" cm-session-context pathnames server-relative
                          (format nil "Undo of add files for user ~s, workspace ~s"
                                  (cm-session-context-user-name cm-session-context)
                                  (cm-session-context-ws-id cm-session-context))
    (lambda (pathnames-work change-context
               master-repository master-repository-name master-catalog
               pc reason workspace workspace-repository)
        (declare (ignore master-repository master-repository-name master-catalog
                         pc reason workspace))
        ;; Don't try to be clever, just process them in order, so undo the add
        (dolist (pw pathnames-work)
          (when (pw-unknown-subsys-dir-p pw)
            ;; MASTER-CATALOG-FILE-NAMES-RESOLVER was unable to resolve the file
            (conman-signal-error *cm-returns-error-file-not-in-any-subsystem-directory*
                                 "The file ~A is not in any known subsystem directory." (pw-user-pathname pw)))
          ;; A problem with file_add is that the file may have been subsequently
          ;; renamed or deleted.  We must search for these operations in the log.
          (tail-labels ((luup (file-name subsequent)
                          (cond ((null subsequent)
                                 ;; Ok to undo the file_add since file wasn't subsequently removed
                                 (debug-message 3 "Undoing file_add ~s" file-name)
                                 (let ((additions (or (change-context-find-file-additions change-context
                                                                                          file-name (pw-satellite-project-did pw))
                                                      (conman-error-no-add-to-undo (pw-user-pathname pw)))))
                                   (assert (null (cdr additions)))
                                   (with-current-repository (workspace-repository)
                                     (change-context-add-file-undo change-context (first additions)))))
                                ;; Error if file was removed or add-file-undo'ne (or merge:remove-fileadd?)
                                ((and (typep (car subsequent) 'cc-new-fileremove)
                                      (equal file-name (cc-new-fileremove-pathname (car subsequent))))
                                 (conman-error-no-add-to-undo (pw-user-pathname pw)))
                                ((and (typep (car subsequent) 'cc-new-filerename)
                                      (equal file-name (cc-new-filerename-old-pathname (car subsequent))))
                                 (debug-message 3 "Undoing file_add of renamed file ~A ~A" file-name
                                                (cc-new-filerename-new-pathname (car subsequent)))
                                 ;; Someone file_renamed it: continue searching with new name
                                 (luup (cc-new-filerename-new-pathname (car subsequent))
                                       (cdr subsequent)))
                                (t (luup file-name (cdr subsequent))))))
            (luup (pw-subsystem-relative-pathname pw)
                  (or (get-subsequent-log-entries workspace-repository change-context pw)
                      (conman-error-no-add-to-undo (pw-user-pathname pw))))))))
  t)

(defun cmctl-undo-file-remove (cm-session-context pathnames server-relative)
  "Nullify the effect of a recent CMCTL-FILE-REMOVE on the CHANGE-CONTEXT."
  (cmctl-operate-on-files "undo file_remove" cm-session-context pathnames server-relative
                          (format nil "Undo of remove files for user ~s, workspace ~s"
                                  (cm-session-context-user-name cm-session-context)
                                  (cm-session-context-ws-id cm-session-context))
    (lambda (pathnames-work change-context master-repository master-repository-name master-catalog
               pc txn-reason workspace workspace-repository)
        (declare (ignore master-repository workspace-repository))
        (dolist (pw pathnames-work)
          (cond ((pw-unknown-subsys-dir-p pw)
                 ;; MASTER-CATALOG-FILE-NAMES-RESOLVER was unable to resolve the file
                 (conman-signal-error
                  *cm-returns-error-file-not-in-any-subsystem-directory*
                  "The file ~a is not in any known subsystem directory."
                  (pw-user-pathname pw)))
                ((pw-change-context-filerename-new-name pw) ; pw is new name of a repos file
                 (error "Internal error: undo file_remove")) ; todo
                ((pw-change-context-fileremove pw)
                 ;; Delete the file from change-context-file-removals
                 (change-context-remove-file-undo change-context (car (pw-change-context-fileremove pw)))
                 ;; Now extract file to disk like cmctl-file-checkout, except read-only. See comments there.
                 (master-catalog-extract-pc-files-to-disk
                  master-catalog master-repository-name
                  pc nil ;; no pc-branch needed because subsystem-list is supplied below
                  (cm-session-context-workspace-rooted-file-system cm-session-context
                                                                   server-relative workspace)
                  txn-reason
                  (workspace-baseline-timestamp workspace) ;to derive appropriate satellite metaversions
                  :workspace workspace          ;use workspace VPB info to compute file content view
                  :subsystem-list (list (pw-subsystem pw))
                  :subsystem-file-alist (list (cons (pw-subsystem pw) (pw-file-did pw)))
                  :read-only t
                  :clean nil))                  ;do NOT clean out directories first please...
                (t (conman-error-no-remove-to-undo (pw-user-pathname pw)))))))
  t)

(defun cmctl-undo-file-rename (cm-session-context pathnames server-relative)
  "Nullify the effect of a recent CMCTL-FILE-RENAME on the CHANGE-CONTEXT."
  (unless pathnames
    (conman-signal-error *cm-returns-error-too-few-arguments*
                         "No filename was given to the command 'undo file_rename'."))
  (when (cddr pathnames)
    (conman-signal-error *cm-returns-error-too-many-arguments*
                         "More than two filenames were given to the command 'undo file_rename'."))
  ;; Currently we ignore the 2nd arg (= newname of the file rename being undone).
  ;; Todo: should we verify that the 2nd arg is the same as the current name?
  (setq pathnames (list (first pathnames)))     ; ignore 2nd arg, see prior comment
  (cmctl-operate-on-files "undo file_rename" cm-session-context pathnames server-relative
                          (format nil "Undo of rename file for user ~s, workspace ~s"
                                  (cm-session-context-user-name cm-session-context)
                                  (cm-session-context-ws-id cm-session-context))
    (lambda (pathnames-work change-context master-repository master-repository-name master-catalog
               pc reason workspace
               &rest unused-args &aux rename)
        (declare (ignore master-repository master-repository-name master-catalog pc reason unused-args))
        (let ((workspace-file-system (cm-session-context-workspace-rooted-file-system
                                      cm-session-context server-relative workspace))
              (pw (car pathnames-work)))
          (cond ((pw-unknown-subsys-dir-p pw)
                 ;; MASTER-CATALOG-FILE-NAMES-RESOLVER was unable to resolve the file
                 (conman-signal-error *cm-returns-error-file-not-in-any-subsystem-directory*
                                      "The file ~a is not in any known subsystem directory."
                                      (pw-user-pathname pw)))
                ;; File F is a renamed repos file, and user did:  undo file_rename F ...
                ;; Undo rename on disk, and remove F's file-rename from the change-context.
                ((and (pw-file-did pw)
                      (setq rename (car (pw-find-in-change-context-renames pw change-context))))
                 ;; undo the rename on disk (error if original repos named file exists on disk)
                 (let* ((oldname (pw-workspace-relative-pathname pw))
                        (newname (merge-pathnames (cc-filerename-new-pathname rename)
                                                  oldname))) ; merge in subsystem etc.
                   (when (file-system-probe-file workspace-file-system oldname)
                     (conman-error-new-file-name-present newname (pw-user-pathname pw)))
                   (file-system-rename workspace-file-system newname oldname))
                 ;; todo: log should be annotated if/when we implement a more general undo
                 (change-context-rename-file-undo change-context rename))
                (t (conman-signal-error *cm-returns-error-undo-not-done*
                                        "There isn't a pending file_rename for ~S."
                                        (pw-user-pathname pw)))))))
  t)

(defparameter *checkout-file-content-changed-criteria* :crc-only
  "Allows weaker criteria on checkout as an optimization.  Not recommended.")

(defun cmctl-file-checkout (cm-session-context pathnames &key server-relative)
  "Add one or more files to the change-context associated with a workspace.
   Signal an error if the files don't exist, etc.
   Email is sent to other users who have the same file checked out.
   PATHNAMES are absolute paths to the files.
   Remember that WS-ID may be an integer which uniquely describes a workspace, or it may
   be a directory path which we need to resolve to a unique ws-id.
   See CM-CLI-FILE-ADD documentation for more details.
   Return value unimportant as long as it isn't :QUIT.  We return T."

  (let ( users  ;;userid strings for now, core::user objects later
        ;; Hash table keyed by PATHNAME-WORK object
        (files-and-users (make-tree #'string= #'string<)))
    (cmctl-operate-on-files
        "file_checkout" cm-session-context pathnames server-relative
        (format nil "Checkout files for user ~s, workspace ~s"
                (cm-session-context-user-name cm-session-context)
                (cm-session-context-ws-id cm-session-context))
      ;; The following lambda is executed in the context of a master repository transaction.
      (lambda (cmctl-pathname-work-list
                 change-context master-repository master-repository-name master-catalog pc txn-reason
                 workspace workspace-repository)
          (declare (ignore master-repository))
          (let ((file-system
                 (cm-session-context-workspace-rooted-file-system cm-session-context
                                                                  server-relative workspace)))
            ;; Check to see if file already checked out by other users
            ;; Send any users found an email informing them of this check out
            ;; do this outside the loop over the file list so we only generate the mail once
            (setq users
                  (cmctl-who-has-em-checked-out
                   cmctl-pathname-work-list
                   workspace-repository
                   :file-did-accessor #'pw-file-did
                   :note-users-function
                   (lambda (pw workspace user)
                       (rb-tree/push! files-and-users
                                      (file-system-friendly-name
                                       (pw-workspace-relative-pathname pw)
                                       file-system)
                                      (list user (workspace-path workspace))))))
            (debug-message 3 "Checkout: users: ~s" users)
            (dolist (pw cmctl-pathname-work-list)
              (if (pw-unknown-subsys-dir-p pw)
                  ;; MASTER-CATALOG-FILE-NAMES-RESOLVER was unable to resolve the file
                  (conman-signal-error *cm-returns-error-file-not-in-any-subsystem-directory*
                                       "The file ~a is not in any known subsystem directory."
                                       (pw-user-pathname pw))
                ;; Note that it is NOT an error if the file isn't present on disk.
                (if (pw-file-did pw)
                    (let* ((file-already-checked-out (pw-find-in-change-context-changes pw change-context))
                           (file-already-checked-out-cc-filechange file-already-checked-out)
                           (probe (file-system-probe-file file-system (pw-workspace-relative-pathname pw))))
                      (if (and file-already-checked-out (not probe))
                          ;; make sure that it is still in existence in the WS (RS03)
                          (setq file-already-checked-out nil)) ;; just ignore previous CO

                      (cond

                       ;; If the file has already been checked out and still exists, give error (rs04)
                       (file-already-checked-out
                        (conman-signal-error *cm-returns-error-file-already-checked-out*
                                             "You have already checked out ~a."
                                             (pw-user-pathname pw)))

                       ;; If the file is scheduled for removal, tell the user he's a bozo.
                       ((pw-change-context-fileremove pw)
                        (conman-error-checkout-removed-file (pw-user-pathname pw)))
                       ;; Ok.  Do it.
                       (t
                        ;; If file on disk is not read only, back it up.
                        (progn
                          (when (and probe
                                     (not (file-descriptor-read-only? probe)))
                            ;; This is an error as of rs05
                            (conman-signal-error *cm-returns-error-file-is-readwrite-on-check-out*
                                                 "The file ~a is already writable.  You may not check it out."
                                                 (pw-user-pathname pw))
                            ;; the old way (pre rs05)
                            ;; File on disk exists, but is not read only.  Save it.
                            ;; We save it *even if it is the same* as what we are checking out
                            ;; because we don't want to do a subsystem transaction just to check
                            ;; the contents.
                            ;;(file-system-backup file-system (pw-workspace-relative-pathname pw))
                            )

                          (if file-already-checked-out-cc-filechange ;; don't need to make a new one
                              (conman-signal-warning
                               *cm-returns-warning-file-already-checked-out*
                               "You've already checked out ~s but it will be re-created on disk since it is missing."
                               (enough-namestring (pw-user-pathname pw)
                                                  (namestring (cm-session-context-current-directory cm-session-context))))
                            ;; Mark the files changed/checked-out in the c2hange context4
                            (let ((new (cc-filechange-create (pw-file-did pw)
                                                             :project-did (pw-satellite-project-did pw))))
                              (setf (cc-file-base-change-subsystem-did new)
                                    (distributed-object-identifier (pw-subsystem pw)))
                              ;; cons in the correct repository
                              (with-current-repository (workspace-repository)
                                (change-context-change-file change-context new))))

                          ;; The user specified file might be the target of a rename. Covered here???

                          ;; Making file read/write is done by the publish-file interface.

                          ;; Actually extract the correct file contents to disk.
                          ;; Strictly speaking, we shouldn't have to do this, we should only have to change
                          ;; mode.  But if the file is missing, the file should be recreated, and besides,
                          ;; we want to ensure it has the correct content.

                          ;; *TBD*: are we handling the case where the file has been modified by the user
                          ;; by clearing the r/o bit and editing prior to checkin?

                          ;; *PERFORMANCE* *FINISH* NAHA: we're doing a satellite txn per file here!
                          ;; This is not acceptable long term.  Command logic should strive for
                          ;; one transaction on the master, and one per satellite (where necessary).
                          ;; At least complain about it it in some context.  We could print it to *debug-io*,
                          ;; but we don't want to clutter the server log.   It will show in regression output though.
                          (when *within-regression-tests*
                            (format t "~%checkout: transaction logic stinks, and need to resolve hp question #63. ~
                           *FINISH*"))

                          ;; **WARNING** this call, which initiates a second transaction on satellites within
                          ;; the parent (master) transaction, assumes that we don't really re-open the database.
                          ;; Which is ok with 0.87+6 in which transaction logic is reworked so that databases
                          ;; are opened only once for a given master transaction (and in fact, server process
                          ;; lifetime if things go well).
                          (let ((*file-content-changed-criteria* *checkout-file-content-changed-criteria*))
                            (master-catalog-extract-pc-files-to-disk
                             master-catalog master-repository-name
                             pc nil ;; no pc-branch needed because subsystem-list is supplied below
                             (cm-session-context-workspace-rooted-file-system cm-session-context server-relative workspace)
                             txn-reason
                             (workspace-baseline-timestamp workspace) ;to derive appropriate satellite metaversions
                             :workspace workspace ;use workspace VPB info to compute file content view
                             :subsystem-list (list (pw-subsystem pw))
                             :subsystem-file-alist (list (cons (pw-subsystem pw) (pw-file-did pw)))
                             :read-only nil
                             :clean nil))))))   ;do NOT clean out directories first please...
                  (cond
                   ;; It might be in a fileadd, they don't need to check it out.
                   ((pw-change-context-fileadd pw)
                    (conman-warning-checkout-newly-added-file (pw-user-pathname pw)))
                   ;; File is completely unknown.
                   (t ;; rs09 error - wild file
                    (conman-error-checkout-unknown-file (pw-user-pathname pw)))
                   )))))))
    (handler-case
        (when users
          (let ((message (with-output-to-string (stream)
                           (rb-tree/foreach
                            files-and-users
                            (lambda (file-name ws-list)
                                (format stream "The file ~a is checked out by~%"
                                        file-name)
                                (loop for (user ws-path) in (sort (copy-list ws-list)
                                                                  #'string<
                                                                  :key #'car)
                                      do
                                      (format stream "  user ~10a workspace ~a~%"
                                              user ws-path)))))))
            (if *within-regression-tests*
                (format t "~&Email message body:~%~a~%" message)
              (with-input-from-string (message-stream message)
                ;; *** Should delay sending the email
                ;; message until outside the
                ;; transaction.
                (conman-send-email (cm-session-context-user-name cm-session-context)
                                   users
                                   "Check out notification"
                                   message-stream)))

            ))
      (error (condition)
        (declare (ignore condition))
        (conman-signal-warning *cm-returns-warning-notification-failed*
                               "Email notification failed.  Reason unknown."))))
  t)

(defun cmctl-uncheckout (cm-session-context
                         all class-name subsystem-name filenames
                         server-relative
                         no-copy)
  "Uncheckout the specified FILES, only the files in the specified CLASS or SUBSYSTEM
   or ALL files in the workspace."
  ;; We will warn about a file not having been checked out only if it
  ;; was explitly named in FILES.
  (let ((reason (format nil "Uncheckout files for user ~s, workspace ~s"
                        (cm-session-context-user-name cm-session-context)
                        (cm-session-context-ws-id cm-session-context)))
        (number-of-files-unco-ed 0)
        (unco-ed-files-warned 0))

    (cmctl-call-with-user-txn cm-session-context
      :reason reason
      :txn-mode :read-write
      :require-active-change :require-active-change
      :receiver
      (lambda (ws-repository workspace change-context
               master-repository-name master-repository master-catalog)

        (let* ((pathnames-work
                (and filenames
                     (mapcar (lambda (user-pathname)
                                 (make-instance 'cmctl-pathname-work
                                   :user-pathname user-pathname
                                   :workspace-relative-pathname
                                   (cmctl-workspace-relative-logical-pathnames
                                    user-pathname workspace cm-session-context server-relative)))
                             filenames)))
               (branch (workspace-master-pc-branch workspace master-repository))
               (pc (branch-owning-project branch)))

          (cmctl-guarantee-branch-not-frozen branch)

          ;; make sure that the class and subsystem (if given) are "known" to the repository
          (when class-name
            (let* ((satellite-project-ref (master-catalog-lookup-satellite-project-ref master-catalog class-name)))
              (unless satellite-project-ref
                (conman-signal-error *cm-returns-error-project-not-found*
                                     "UNCO class (~s) not found."
                                     class-name))))
          (when subsystem-name
            (master-catalog-subsystem-name-lookup master-catalog subsystem-name
                                                  :two-phase-search nil ;must be current name
                                                  :search-all-names nil
                                                  :error-if-missing t))

          ;; Make sure that we don't have any wild files -- do this before we have actually
          ;; done any UNCO's so we do not have clean up issues in the guy's workspace (like
          ;; files we UNCO'd, marked readonly, and then rolled back to being CO'd w/o fixing
          ;; the readonly bits).
          (when pathnames-work
            (with-cm-master-metaversion ((workspace-baseline-timestamp workspace))
              (error-if-product-reference-workspace workspace "file uncheckout wild files test")

              (cmctl-operate-on-files-guts
               cm-session-context
               (format nil "UnCheckOut Wild files test for user ~s, workspace ~s"
                       (cm-session-context-user-name cm-session-context)
                       (cm-session-context-ws-id cm-session-context))
               master-repository master-repository-name master-catalog
               pathnames-work pc branch
               change-context workspace ws-repository
               ;; The following lambda is executed in the context of a master repository transaction.
               (lambda (cmctl-pathname-work-list
                          change-context master-repository master-repository-name
                          master-catalog pc txn-reason
                          workspace workspace-repository)
                   (declare (ignore master-repository
                                    master-repository-name master-catalog
                                    pc txn-reason
                                    workspace workspace-repository))
                   (dolist (pw cmctl-pathname-work-list)
                     (progn
                       (debug-message 5 "UNCO Wild check: ~s" (pw-user-pathname pw))
                       (if (pw-unknown-subsys-dir-p pw)
                           ;; MASTER-CATALOG-FILE-NAMES-RESOLVER was unable to resolve the file
                           (conman-signal-error *cm-returns-error-file-not-in-any-subsystem-directory*
                                                "The file ~a is not in any known subsystem directory."
                                                (enough-namestring (pw-user-pathname pw)
                                                                   (namestring
                                                                    (cm-session-context-current-directory cm-session-context))))
                         ;; Note that it is NOT an error if the file isn't present on disk.
                         (if (pw-file-did pw)
                             (unless (pw-find-in-change-context-changes pw change-context)
                               (progn
                                 (setq unco-ed-files-warned (+ unco-ed-files-warned 1))
                                 (conman-signal-warning
                                  *cm-returns-warning-file-not-already-checked-out*
                                  "The file: ~s is not part of the current change and may not be UNCO'd."
                                  (enough-namestring (pw-user-pathname pw)
                                                     (namestring (cm-session-context-current-directory cm-session-context))))))
                           (unless (pw-change-context-fileadd pw)  ;; nil => wild file
                             (conman-error-checkout-unknown-file (enough-namestring (pw-user-pathname pw)
                                                                                    (namestring (cm-session-context-current-directory
                                                                                                 cm-session-context)))))))))
                   ))))

          (with-cm-master-metaversion ((workspace-baseline-timestamp workspace))
            (master-catalog-map-pc-subsystems
                master-catalog master-repository-name pc branch
                :satellite-txn-mode :read-write
                :reason reason
                :function (lambda (subsystem satellite-repository)
                              (with-cm-master-metaversion ((workspace-baseline-timestamp workspace))
                                (let* ((logical-subdirectory (subsystem-relative-subdirectory subsystem))
                                       (satellite-project-did (satellite-project-ref-project-did
                                                               (subsystem-satellite-project-ref subsystem)))
                                       (satellite-project (repository-resolve-distributed-identifier
                                                           satellite-repository
                                                           satellite-project-did))
                                       (this-subsys-name (object-user-name subsystem))
                                       (subsystem-did (distributed-object-identifier subsystem))
                                       (VPB-added-satellite-cset-dids
                                        (workspace-existing-added-VPB-cset-dids-for-subsystem
                                         workspace subsystem-did))
                                       (VPB-removed-satellite-cset-dids
                                        (workspace-existing-removed-VPB-cset-dids-for-subsystem
                                         workspace subsystem-did))
                                       (this-subsys-class nil)) ; can't resolve this var in master metaversion
                                  (with-cm-satellite-metaversion ((workspace-baseline-timestamp workspace))
                                    (setq this-subsys-class (object-user-name satellite-project))
                                    (debug-message 4 "Scanning ~s, class ~s"
                                                   this-subsys-name
                                                   this-subsys-class)

                                    ;; Decide whether to proceed on this subsystem.
                                    (when (or all
                                              (and subsystem-name
                                                   this-subsys-name
                                                   (string= subsystem-name this-subsys-name))
                                              (and class-name
                                                   ;; Shouldn't need to check, but this is missing sometimes!
                                                   this-subsys-class
                                                   (string= class-name this-subsys-class))
                                              (and pathnames-work
                                                   (some (lambda (pw)
                                                             (directory-is-prefix? logical-subdirectory
                                                                                   (pw-workspace-relative-pathname pw)))
                                                         pathnames-work)))
                                      (debug-message 4 "processing ~s, class ~s" this-subsys-name
                                                     this-subsys-class)
                                      ;; Establish correct version context of file names/contents
                                      (subsystem-satellite-funcall-in-context
                                          subsystem satellite-repository
                                        (lambda (root-rfm-directory)
                                            (declare (ignore root-rfm-directory))
                                            (let* ((files-checked-out
                                                    ;; Gather those files checked out in the current subsystem
                                                    (remove-if (lambda (cc-filechange)
                                                                   (not (cc-file-base-change-applies-to-project-p
                                                                         cc-filechange
                                                                         satellite-project-did)))
                                                               (change-context-file-changes change-context)))
                                                   (files-to-unco
                                                    ;; Select files specified for unco which are co'd in this subsys
                                                    (if pathnames-work
                                                        (remove-if
                                                         (lambda (cc-filechange)
                                                             (let* ((file-did (cc-filechange-file-did cc-filechange))
                                                                    (rfm-file (repository-resolve-distributed-identifier
                                                                               satellite-repository
                                                                               file-did))
                                                                    (file-name (file-system-element-relative-path rfm-file))
                                                                    (full-file-path
                                                                     (merge-pathnames file-name logical-subdirectory )))
                                                               (not (member full-file-path pathnames-work
                                                                            :test #'equal
                                                                            :key #'pw-workspace-relative-pathname))))
                                                         files-checked-out)
                                                      ;; No explicit pathnames, select all files in the subsystem
                                                      ;; which were checked out
                                                      files-checked-out)))
                                              (debug-message 4 "Files in this subsystem: ~s" files-checked-out)
                                              (debug-message 4 "Files to unco: ~s" files-to-unco)
                                              ;; JDT says: it would be nice to complain about files the user
                                              ;; specified and that we couldn't find.
                                              ;; This test is currently missing.
                                              (mapc (lambda (file)
                                                        (subsystem-satellite-uncheckout-file
                                                         file
                                                         change-context
                                                         satellite-repository
                                                         (logical-file-system-change-directory
                                                          (cm-session-context-workspace-rooted-file-system
                                                           cm-session-context server-relative workspace)
                                                          logical-subdirectory)
                                                         no-copy)
                                                        (setq number-of-files-unco-ed (+ number-of-files-unco-ed 1)))
                                                    files-to-unco)
                                              ))
                                        :VPB-added-satellite-cset-dids VPB-added-satellite-cset-dids
                                        :VPB-removed-satellite-cset-dids VPB-removed-satellite-cset-dids
                                        )))
                                  satellite-project)))))))
      :update-workspace
      #'make-change-context-persistent-in-workspace)
    (when (and (= unco-ed-files-warned 0) (= number-of-files-unco-ed 0))
      (conman-signal-warning *cm-returns-warning-no-file-was-unco-ed*
                             "No files in your ~s were UNCO'd (because there were none that could be)."
                             (if class-name "class"
                               (if subsystem-name "subsystem" "cset")))))
  t)

(defun cmctl-file-remove (cm-session-context pathnames &key server-relative)
  "Remove one or more files to the change-context associated with a workspace.
   Signal an error if the files don't exist, etc.

   PATHNAMES are absolute paths to the files.

   Remember that WS-ID may be an integer which uniquely describes a workspace, or it may
   be a directory path which we need to resolve to a unique ws-id.

   See CM-CLI-FILE-REMOVE documentation for more details.
   Return value unimportant as long as it isn't :QUIT.  We return T."
  (cmctl-operate-on-files  "file_remove" cm-session-context pathnames server-relative
                           (format nil "Remove files for user ~s, workspace ~s"
                                   (cm-session-context-user-name cm-session-context)
                                   (cm-session-context-ws-id cm-session-context))
    (lambda (cmctl-pathname-work-list change-context
               master-repository master-repository-name master-catalog
               pc reason workspace workspace-repository)
        (declare (ignore master-repository master-repository-name master-catalog
                         pc reason ))
        (let ((workspace-file-system (cm-session-context-workspace-rooted-file-system cm-session-context
                                                                                      server-relative workspace)))
          (flet ((remove-from-disk (pw)
                   (if (file-system-probe-file workspace-file-system
                                               (pw-workspace-relative-pathname pw))
                       (file-system-delete-file workspace-file-system
                                                (pw-workspace-relative-pathname pw) :force t)
                     (conman-warning-removed-file-not-present (pw-user-pathname pw)))))

            (dolist (pw cmctl-pathname-work-list)
              (debug-message 3 "Removing ~s" pw)
              (when (pw-unknown-subsys-dir-p pw)
                ;; MASTER-CATALOG-FILE-NAMES-RESOLVER was unable to resolve the file
                (conman-signal-error *cm-returns-error-file-not-in-any-subsystem-directory*
                                     "The file ~a is not in any known subsystem directory." (pw-user-pathname pw)))
              (let ((satellite-project-did (pw-satellite-project-did pw))
                    filerenames1 filerenames2 fileadds)
                (if (pw-file-did pw)
                    (cond
                     ;; Error if file is currently checked out. User must unco beforehand.
                     ((pw-find-in-change-context-changes pw change-context)
                      (conman-error-removed-checked-out-file (pw-user-pathname pw)))
                     ;; Error if file is old name of a file renamed in current delta.
                     ;; If they want to delete it they must use the new name.
                     ((setq filerenames2 (pw-find-in-change-context-renames pw change-context))
                      (conman-error-remove-renamed-old-name (pw-user-pathname pw)
                                                            (cc-filerename-new-pathname (car filerenames2))))
                     ;; Warn if file has already been removed in the current delta.
                     ((pw-find-in-change-context-removals pw change-context)
                      (conman-warning-file-already-removed (pw-user-pathname pw)))
                     ;; Ok. Do the remove-file, first undoing any rename
                     (t
                      (when (setq filerenames1 (pw-change-context-filerename-new-name pw))
                        ;; File to remove is new name of a file renamed in current delta.
                        ;; todo (change-context-rename-file-undo change-context filerenames1)
                        (setf (change-context-file-renames change-context)
                              (set-difference (change-context-file-renames change-context)
                                              filerenames1))
                        (conman-warning-remove-renamed (pw-user-pathname pw)))
                      (let ((new (cc-fileremove-create (pw-file-did pw)
                                                       :project-did satellite-project-did)))
                        (setf (cc-file-base-change-subsystem-did new)
                              (distributed-object-identifier (pw-subsystem pw)))
                        ;; cons in the correct repository
                        (with-current-repository (workspace-repository)
                          (change-context-remove-file change-context new)))
                      (remove-from-disk pw)))
                  ;; No such file in repository.  Maybe it was added in the current change?
                  (cond
                   ;; File was added in current delta.
                   ((setq fileadds (pw-find-in-change-context-additions pw change-context))
                    ;; remove the corresponding CC-FILEADD record
                    ;; Note: unlike repos file, file is NOT removed from disk. todo: log this?
                    (conman-warning-remove-newly-added-file (pw-user-pathname pw))
                    ;; todo: (change-context-add-file-undo change-context (first fileadds))
                    (setf (change-context-file-additions change-context)
                          (set-difference (change-context-file-additions change-context)
                                          fileadds)))
                   (t
                    ;; JDT: Cleaned up a mixed-mode error (signalling error with warning code.
                    ;; Not sure if this needs to signal an error or a warning.  A warning is preferred,
                    ;; but an error may be necessary to terminate transaction depending on state
                    ;; of changes made above which I don't fully understand (Naha: please examine.)
                    (conman-signal-error *cm-returns-error-invalid-repository-file-spec*
                                         "The file to be removed (~s) is not in the versioned view ~
                                           of the respository."
                                         ;; *FINISH*: need to present client-relative nice form of
                                         ;; pathname, not a #P representation or server-relative
                                         ;; interpretation of the path which differs from client
                                         ;; conventions
                                         (namestring (pw-user-pathname pw))))))))))))
  t)


(defun cmctl-file-rename (cm-session-context
                          src-pathname dst-pathname &key server-relative)
  "Rename file in the change-context associated with a workspace.
   Signal an error if what?

   PATHNAMES are absolute paths to the files.

   Remember that WS-ID may be an integer which uniquely describes a workspace, or it may
   be a directory path which we need to resolve to a unique ws-id.

   See CM-CLI-FILE-ADD documentation for more details.
   Return value unimportant as long as it isn't :QUIT.  We return T."
  ;; file_rename gets two file names: the current name of a file and
  ;; the new name.  First we resolve them both.
  (cmctl-operate-on-files "file_rename" cm-session-context (list src-pathname dst-pathname)
                          server-relative
                          (format nil "Rename file for user ~s, workspace ~s"
                                  (cm-session-context-user-name cm-session-context)
                                  (cm-session-context-ws-id cm-session-context))
    (lambda (cmctl-pathname-work-list change-context
               master-repository master-repository-name master-catalog
               pc reason workspace workspace-repository)
        (declare (ignore master-repository master-repository-name master-catalog
                         pc reason))
        (let ((workspace-file-system (cm-session-context-workspace-rooted-file-system cm-session-context
                                                                                      server-relative workspace)))
          (destructuring-bind (src-pw dst-pw) cmctl-pathname-work-list
            ;; Was MASTER-CATALOG-FILE-NAMES-RESOLVER able to resolve the file names?
            (when (pw-unknown-subsys-dir-p src-pw)
              (conman-signal-error
               *cm-returns-error-file-not-in-any-subsystem-directory*
               "The file ~a is not in any known subsystem directory."
               (pw-user-pathname src-pw)))
            (when (pw-unknown-subsys-dir-p dst-pw)
              (conman-signal-error
               *cm-returns-error-file-not-in-any-subsystem-directory*
               "The file ~a is not in any known subsystem directory."
               (pw-user-pathname dst-pw)))
            (when (file-system-probe-file workspace-file-system
                                          (pw-workspace-relative-pathname dst-pw))
              (if (file-system-probe-file workspace-file-system
                                          (pw-workspace-relative-pathname src-pw))
                  ;; Error!  A file with the new name is already present on disk.
                  (conman-error-new-file-name-present
                   (pw-user-pathname src-pw) (pw-user-pathname dst-pw))
                ;; Dst exists in ws & src does not, assume user already did a rename?
                (conman-warning-file-already-renamed (pw-user-pathname src-pw) (pw-user-pathname dst-pw))))
            (flet ((rename-on-disk ()
                     ;; If dst exists in ws then we assume user has already performed rename
                     (unless (file-system-probe-file workspace-file-system
                                                     (pw-workspace-relative-pathname dst-pw))
                       (file-system-rename workspace-file-system
                                           (pw-workspace-relative-pathname src-pw)
                                           (pw-workspace-relative-pathname dst-pw)))))
              (let (fileadds2 filerenames2 renames)
                ;; Branch based on dst,src in repos or change-context additions/removals
                ;; Error if dst already in repos (including old/new names and file-adds)
                ;; Error if src is a removed file
                (cond ((and (pw-file-did dst-pw)
                            (not (pw-find-in-change-context-removals dst-pw change-context)))
                       ;; Error if repository already knows about a file with the new name ("known"
                       ;; names include new names of a renamed repository files, see pw-resolve...)
                       (conman-error-new-file-name-exists (pw-user-pathname src-pw)
                                                          (pw-user-pathname dst-pw)))
                      ;; todo: Do we care if *new* name was same as a file removed by current change?
                      ;;((pw-find-in-change-context-removals dst-pw change-context) ...)
                      ((pw-find-in-change-context-additions dst-pw change-context)
                       ;; Error if new name was the subject of a previous file-add.
                       (conman-error-new-file-name-matches-file-added (pw-user-pathname dst-pw)))
                      ;; todo: what does this code do if src is in changes? (checkouts)
                      ((pw-find-in-change-context-removals src-pw change-context)
                       ;; Error if trying to rename a file that was previously removed.
                       (conman-error-rename-removed-file (pw-user-pathname src-pw)))
                      ((pw-file-did src-pw)
                       (cond
                        ((setq renames (pw-find-in-change-context-renames src-pw change-context))
                         ;; Old name of this rename operation matches an
                         ;; old name in a previous rename operation of the current delta.
                         (conman-error-renaming-old-filename-again (pw-user-pathname src-pw)
                                                                   (cc-filerename-new-pathname (car renames))))
                        ;; Old name of this rename operation matches the
                        ;; new name of a rename operation in the current
                        ;; delta.
                        ((setq filerenames2
                               (pw-find-in-change-context-renamed-to src-pw change-context))
                         (setf (cc-filerename-new-pathname (car filerenames2))
                               (pw-subsystem-relative-pathname dst-pw))
                         (rename-on-disk))      ;; do ws rename (unless dst already exists)
                        (t ;; Normal case: file is in repos and has no renames
                         (let ((new (cc-filerename-create (pw-file-did src-pw)
                                                          (pw-subsystem-relative-pathname dst-pw)
                                                          :project-did (pw-satellite-project-did src-pw))))
                           (setf (cc-file-base-change-subsystem-did new)
                                 (distributed-object-identifier (pw-subsystem src-pw)))
                           ;; cons in the correct repository
                           (with-current-repository (workspace-repository)
                             (change-context-rename-file change-context new))) ;push to cc file-renames & log
                         (rename-on-disk))))    ;; do ws rename (unless dst already exists)
                      ;; If the old filename is in a file_add: remove old one, create a new one.
                      ;; Don't mutate the old fileadd - the log has a pointer to it.
                      ((setq fileadds2 (pw-find-in-change-context-additions src-pw change-context))
                       ;; remove old fileadd, insert new one; don't log either operation
                       (change-context-add-file-undo-no-log change-context (car fileadds2))
                       (change-context-add-file-no-log
                        change-context
                        (cc-fileadd-create
                         (pw-subsystem-relative-pathname dst-pw)
                         :content-type (cc-fileadd-content-type (car fileadds2))
                         :project-did  (cc-file-base-change-affected-project-did (car fileadds2))
                         :other-stuff  (cc-file-base-change-other-stuff (car fileadds2))))
                       ;; log the rename
                       (with-current-repository (workspace-repository)
                         (change-context-add-log-entry change-context
                                                       (cc-new-filerename-create
                                                        (cc-fileadd-pathname (car fileadds2))
                                                        (pw-subsystem-relative-pathname dst-pw)
                                                        :project-did (pw-satellite-project-did dst-pw))))
                       (conman-warning-rename-newly-added (pw-user-pathname src-pw))
                       (rename-on-disk))        ;; do ws rename (unless dst already exists)
                      (t (error "don't know what to do")))))))))
  t)

#||
(defun cmctl-cset-secret-date-time-zapper (cm-session-context master-repository
                                           his-reason cset-name secret-date-time)
  "Zap the cset's date-time with the secret-date-time given by the user (when non-nil).  Assumes
   that the caller has done a with-cm-master-repository"
  (let ((reason (format nil "~s, zapping date" his-reason)))
    (when secret-date-time
      (with-cm-master-txn-no-cset (master-repository
                                   master-catalog
                                   cm-session-context
                                   :read-write
                                   reason)
        (let* ((cset (master-catalog-resolve-change-set-name master-catalog cset-name))
               (cset-cid (change-set-cid-number cset)))
          (core::repository-zap-cset-dates-no-txn master-repository (list (cons cset-cid secret-date-time)))
          )))
    ))
||#

(defun cmctl-check-in-email-message (command-name cset-name user-name master-cid-did-str
                                     message-so-far product-list cset-query-str)
  "send an email message about the code check-in for the COMMAND-NAME (master_change or
   cset_close) and the CSET-NAME.  Add to the MESSAGE-SO-FAR and send it to the users
   designated by the PRODUCT-LIST.  CSET-QUERY-STR is the result of doing a 'csf
   ws_query -show-work' on the cset and will be appended to the email msg."
  ;; bug 001025-0005
  (when message-so-far
    (let* ((message (concatenate 'string "~%" command-name " of " cset-name "~%"
                                 ;; don't put out a pretty date that would mess up regressions
                                 (unless *within-regression-tests* "Time: ")
                                 (unless *within-regression-tests* (time-string))
                                 (when   *within-regression-tests* ;; put out an ugly date
                                   (universal-time->iso-date-time-string
                                    (get-universal-time)))
                                 "~%Master DB Distributed Identifier: "
                                 master-cid-did-str "~%"
                                 message-so-far))
           (prod-list (sort product-list #'string<))
           (to-list (conman-check-in-email-recipients prod-list)))

      (when prod-list ;; add product names to the message
        (let ((prods "~%Affecting Products:~%"))
          (dolist (pn prod-list)
            (setq prods (concatenate 'string prods "    " pn "~%")))
          (setq message (concatenate 'string message prods))))

      ;; add in the files of the change ('ws_query -show-work' output)
      (setq message (concatenate 'string message "~%csf ws_query -show-work~%" cset-query-str))

      (debug-message 4 "check-in mail list: ~a" to-list)

      (if *within-regression-tests*
          (progn
            (format t "~&Email message body:~%")
            (format t message)
            (format t "~%"))
        (when to-list
          (let ((formatted-message (with-output-to-string (out-stream)
                                     (format out-stream message))))
            (with-input-from-string (string-stream formatted-message)
              (conman-send-email user-name to-list
                                 (format nil "~a of ~a" command-name cset-name)
                                 string-stream)
              )))))))

(defun cmctl-change-close (cm-session-context change-name abstract-file
                           description secret-cset-name cset-query-str)
  "Take the active change-context in the current workspace and check it in, creating
   a change-set in the master repository.  An error is signalled if there is no change context.

   CHANGE-NAME, if specified (i.e. is non-nil), is a string which overrides the one originally
   specified in change-create, and must be a string.  It might also be a fully qualified cset
   name (see SECRET-CSET-NAME).

   ABSTRACT-FILE should be nil, or a pathname to the file.

   DESCRIPTION, if specified, is the change description.  It should be a string if non-nil.

   SECRET-CSET-NAME if present, allows the user of override the regular cset name with a fully
   qualified cset name (in the CHANGE-NAME arg) on the cset but only if
   *conman-enabled-secret-cset-zapping* is non-nil.

   CSET-QUERY-STR is either nil or a string containing the results of doing a
   'csf ws_query -show-work' on this cset being closed.  It will be appended to
   the email msg (if one is produced)

   SEMANTICS: we must be able to derive a change description or abstract, either from
   the pre-existing change-context of from the abstract-file and description arguments.
   If the arguments are specified, they replace any value which was previously set in the
   change-context."
  (cm-session-context-require cm-session-context :user-name :repository-name :ws-id)
  (check-type change-name   (or null string))
  (check-type abstract-file (or null pathname))
  (check-type description   (or null string))

  (let* ((master-repository-name (cm-session-context-repository-name cm-session-context))
         (abstract-file-contents (when abstract-file
                                   (cmctl-retrieve-abstract-file-contents
                                    cm-session-context
                                    (cm-session-context-file-system-agent cm-session-context)
                                    abstract-file)))
         (user-name (cm-session-context-user-name cm-session-context))
         (ws-id (cm-session-context-ws-id cm-session-context))
         (cc-cset-name t)
         (cc-email-msg nil)
         (cc-email-products nil)
         (master-cid-did-string nil)
         (reason (format nil "Close cset for user ~s, workspace ~s" user-name ws-id)))
    ;; *FINISH*: rework transaction logic for MVCC/RO master/satellites, R/W only on workspace repository

    (setq cc-email-msg (if (conman-send-email-for-check-ins)
                           (format nil "~%~a~%~%" reason)
                         nil)) ;; if nil, do not produce a msg

    (cmctl-call-with-workspace-change-context cm-session-context
      :reason reason
      :txn-mode :read-write
      :require-active-change :require-active-change
      :receiver
      (lambda (ws-repository workspace change-context)
          (declare (ignore ws-repository))
          ;; active change required here
          (let* ((old-change-name (and change-context (change-context-cset-name change-context)))
                 )
            (unless (or description (change-context-cset-description change-context))
              (conman-signal-error *cm-returns-error-missing-change-description*
                                   "A cset description is required and was not specified."))
            (unless (or (null change-name)
                        secret-cset-name ;; change-name is fully qualified (and won't pass)
                        (cmctl-valid-name? change-name))
              (conman-signal-error *cm-returns-error-invalid-change-name*
                                   "The cset name, ~S, specified was invalid." change-name))
            (unless (or old-change-name change-name)
              (conman-signal-error *cm-returns-error-missing-change-name*
                                   "A cset name is required and was not specified."))
            ;; abstract required at cset-close else cset-create (not required if no cset is open)
            (unless (or abstract-file-contents  ; abstract at cset close overrides any from cset-create
                        (setq abstract-file-contents (and change-context ; get any abstract from cset-create
                                                          (change-context-cset-abstract change-context)))
                        (null change-context))  ; abstract not required if no cset open
              (conman-error-missing-abstract))

            (when cc-email-msg
              ;; add in description & abstract file
              (setq cc-email-msg
                    (concatenate 'string cc-email-msg
                                 "Description: \""
                                 (or description (change-context-cset-description change-context)) "\"~%~%"
                                 "Abstract:~%"
                                 abstract-file-contents "~%")))

            ;; Note that the reason on the master repository will be used for the change-set description
            (with-cm-master-repository (master-repository master-repository-name)
              (with-cm-master-txn (master-repository master-catalog cm-session-context :read-write
                                   (or description (change-context-cset-description change-context))
                                   :super-cset-name-var final-cset-name
                                   :supplementary-change-plist
                                   (and abstract-file-contents (list :abstract abstract-file-contents)))
                (with-cm-master-metaversion ((workspace-baseline-timestamp workspace))
                  (let* ((branch (workspace-master-pc-branch workspace master-repository))
                         (pc (branch-owning-project branch)))
                    ;; *FINISH*(November): need to select appropriate PC branch and version to scope project
                    ;; content subsystem list!
                    (setq final-cset-name (or (and secret-cset-name
                                                   change-name) ;; already fully qualified
                                              (and change-name
                                                   (conman-generate-HP-cset-name
                                                    change-name user-name (pc-name pc)))
                                              old-change-name))

                    (when description
                      ;; Note that this is a perhaps useless formality,
                      ;; the cset description is fed to the txn.
                      (change-context-set-description change-context description))
                    (setq cc-email-products
                          (master-catalog-close-change master-catalog cm-session-context
                                                       workspace change-context pc branch
                                                       (cm-session-context-workspace-rooted-file-system
                                                        cm-session-context)
                                                       reason final-cset-name)))

                  (setq cc-cset-name final-cset-name)
                  (cmctl-commit-change-consider-port-activity workspace master-repository)
                  (setq master-cid-did-string
                                  (cid-object-did-string
                                   (cid-object-find-or-create (txn-context-cid *txn-context*))))
                  )))

            ;; Done with the master repository.
            ;; Clear the change context from the workspace.
            (workspace-delete-change-context workspace)
            ;; Promote the current change into the workspace VPB since this is not a master_change.
            (workspace-promote-current-vpb-change workspace)
            )))

    (cmctl-check-in-email-message *cm-cli-command-change-close* cc-cset-name
                                  user-name master-cid-did-string
                                  cc-email-msg cc-email-products cset-query-str)

  cc-cset-name))


(defun cmctl-aux-master-change-ensure-subsystem-cset-wall-passage (master-catalog master-repository
                                                                   pc branch workspace change-context
                                                                   affected-subsystems-list)
  "Ensure that the list of master-csets (change-sets in the master repository) are permitted to
   pass through any holes erected in the list of affected subsystems if the component csets of the
   respective master-csets apply to the indicated subsystem.

   If a subsystem has no walls, then passage is assured.  If the subsystem has walls, then there
   must be a hole in EACH product wall on the subsystem that names the cset to be passed.

   This call assumes latest metaversion context, but may temporarily bind :PC context to do its work.

   AFFECTED-SUBSYSTEMS-LIST is the list of ALL affected subsystems in the master_change environment,
   including both changes from a potential change-context (i.e. file changes), and VPB promotions
   and demotions.  It's an efficiency, we could recalculate it, but the caller already has it.

   CHANGE-CONTEXT is that which has already been pulled from the workspace, and it may be NIL.

   If there are any csets which can't be passed, we signal a transaction terminating error.
   Assuming that there aren't any errors signalled, we return a information
   on holes in walls which need to be sealed upon successful completion of the master change operation.

   The return information is a list of sublists, where each tuple/sublist contains:
   1) the subsystem object containing a wall with holes
   2) the cset for which a hole existed in that subsystem's walls that must be sealed."

  (cmctl-guarantee-branch-not-frozen branch)
  (let (open-cset-subsystem-conflicts           ;subsystems which have change-context file changes
        vpb-subsystem-conflicts                 ;list of (subsystem component-cset-did :add|:remove) sublists
        holes-to-close)                         ;subsys walls with holes to close later in master-change

    (unless (cm-txn-latest-master-metaversion-p)
      (error "cmctl-aux-master-change-ensure-subsystem-cset-wall-passage ~
            assumes the latest-metaversion in effect, and an assertion to that effect fails."))

    ;; Filter out those subsystems which don't have walls.
    (setq affected-subsystems-list (remove-if-not #'subsystem-has-walls? affected-subsystems-list))

    ;; Process subsystems with walls.  If there aren't any, we're done.
    ;; We gather up all the problem subsystems so we can report as much information in the error message
    ;; as possible.

    ;; Check for an active change which may touch the subsystems with walls
    (when change-context
      (let ((file-affected-subsystems (pc-affected-subsystem-list-from-change-context branch change-context)))
        (dolist (subsystem file-affected-subsystems)
          (when (find subsystem affected-subsystems-list)
            ;; There can't be any hole for an open cset, we have a problem
            (push subsystem open-cset-subsystem-conflicts)))))

    ;; Check for VPB changes
    (dolist (subsystem affected-subsystems-list)
      (let* ((subsystem-did (distributed-object-identifier subsystem))
             ;; Keeping these separate lists only for purposes of potentially telling user whether
             ;; the un-holed csets which hold up a transaction are being added or removed.
             (added-satellite-cset-dids-for-subsystem
              (workspace-existing-added-VPB-cset-dids-for-subsystem workspace subsystem-did))
             (removed-satellite-cset-dids-for-subsystem
              (workspace-existing-removed-VPB-cset-dids-for-subsystem workspace subsystem-did)))
        ;; If we have any adds or removes, and there isn't a wall for the csets in question,
        ;; then we have a situation which blocks master_change
        (dolist (satellite-cset-did added-satellite-cset-dids-for-subsystem)
          (if (subsystem-has-hole-for-cset? subsystem satellite-cset-did)
              (push (list subsystem satellite-cset-did) holes-to-close)
            (push (list subsystem satellite-cset-did :add) vpb-subsystem-conflicts)))
        (dolist (satellite-cset-did removed-satellite-cset-dids-for-subsystem)
          (if (subsystem-has-hole-for-cset? subsystem satellite-cset-did)
              (push (list subsystem satellite-cset-did) holes-to-close)
            (push (list subsystem satellite-cset-did :remove) vpb-subsystem-conflicts)))))

    ;; Signal errors if appropriate
    (when (or open-cset-subsystem-conflicts
              vpb-subsystem-conflicts)
      (conman-signal-error
       *cm-returns-error-wall-hole-required*
       "Subsystem(s) in the ~s product have one or more walls raised.~%~
        ~@[An open cset exists and conflicts with walls for subsystems ~s.~]~
        ~@[WALL_HOLE commands may be necessary for the following subsystems and csets: ~s.~]"
       (object-user-name pc)
       (mapcar #'object-user-name open-cset-subsystem-conflicts)
       (mapcar (lambda (tuple)
                   (destructuring-bind (subsystem satellite-cset-did mode)
                       tuple
                     (list :subsystem (object-user-name subsystem)
                           :cset-name
                           ;; Retrieve the cset name of the master cset
                           (let* ((master-cset-did
                                   (master-catalog-find-master-cset-did-for-satellite-cset
                                    master-catalog satellite-cset-did))
                                  (master-cset (repository-resolve-distributed-identifier master-repository
                                                                                          master-cset-did)))
                             (object-user-name master-cset))
                           :cset-action
                           mode)))
               vpb-subsystem-conflicts)))

    ;; If we reach this point, all is clear for master_change to proceed
    holes-to-close))

(defun cmctl-master-change (cm-session-context change-name abstract-file
                            description secret-cset-name ws-update
                            cset-query-str)
  "Close the active change, if necessary, and promote effects of change_close, change_add, change_remove
   and change_port into the tip of the branch upon which the workspace is operating.
   a change-set in the master repository.  An error is signalled if there is no change context.

   There is a complicated set of conditions that must ultimately be enforced by this module,
   they are described in the document ts50\conman\master-change.txt.

   Arguments are as per change-close."

  ;; *FINISH* (JUNE): need high level database locks here corresponding to master-change.txt steps
  ;; 1 and 7.  This should be done as part of general concurrent r/w server operation.
  (cm-session-context-require cm-session-context :user-name :repository-name :ws-id)
  (check-type change-name (or null string))
  (check-type abstract-file (or null pathname))
  (check-type description (or null string))

  ;; If -update switch specified then perform automatic ws_update, but abort master_change if any merges
  (when ws-update
    (let* ((reason (format nil "Update workspace for user ~s"
                           (cm-session-context-user-name cm-session-context)))
           ;; *FINISH*: use release, label, whatever
           ;; old: (new-timestamp (time-stamp-allocate))
           (user-time-spec (cm-session-context-time-spec cm-session-context))
           (new-timestamp
            ;; *FINISH*: use release, label, whatever
            (cond (user-time-spec (time-stamp-create user-time-spec))
                  (t (time-stamp-allocate)))))
      (cmctl-update-workspace
       cm-session-context reason new-timestamp nil nil
       :merge-handler
       (lambda (three-way-merge merge-conflicts merged-binary-files)
           (declare (ignore merged-binary-files)) ; already handled by cmctl-update-workspace
           (when (or three-way-merge merge-conflicts)
             (conman-signal-error
              *cm-returns-error-merge-in-master-change-ws-update*
              "~a failed because a merge occurred during the automatic ~a"
              *cm-cli-command-master-change* *cm-cli-command-workspace-update*))))))

  (let* ((master-repository-name (cm-session-context-repository-name cm-session-context))
         (user-name (cm-session-context-user-name cm-session-context))
         (master-cid nil)               ;cid of cset in master which will be created, filled in below
         (master-cid-did-string nil)
         (master-timestamp nil)         ;time-stamp of above cid, filled in below
         (ws-id (cm-session-context-ws-id cm-session-context))
         (abstract-file-contents (when abstract-file
                                   (cmctl-retrieve-abstract-file-contents
                                    cm-session-context
                                    (cm-session-context-file-system-agent cm-session-context)
                                    abstract-file)))
         (change-completed-p nil)       ;true if master_change completes more or less successfully
         (mc-cset-name t)
         (mc-email-products nil)
         (reason (format nil "Master change for user ~s, workspace ~s" user-name ws-id))
         (already-locked-p nil)
         (affected-subsystem-dids nil)
         (mc-email-message (when (conman-send-email-for-check-ins)
                             (format nil "~%~a~%~%" reason)))
         )
    ;; Acquire necessary locks, potentially signal a terminating condition if necessary.
    ;; This isn't the most transaction-efficient way, but it is code efficient for now.
    (call-with-busy-redirection
     cm-session-context reason nil
     (lambda ()
       (unwind-protect
           (progn
             (multiple-value-setq (already-locked-p affected-subsystem-dids)
               (cmctl-master-lock cm-session-context description))

             ;; *FINISH*: rework transaction logic for MVCC/RO master/satellites,
             ;; and R/W only on workspace repository
             (cmctl-call-with-workspace-change-context
                cm-session-context
                :reason reason
                :txn-mode :read-write
                :require-active-change nil
                :receiver
                (lambda (ws-repository workspace change-context)
                  (declare (ignore ws-repository))
                  (let* ((old-change-name (and change-context (change-context-cset-name change-context)))
                         (change-context-description
                          (and change-context
                               (change-context-cset-description change-context)))
                         )
                    (unless (or description change-context-description)
                      (conman-signal-error *cm-returns-error-missing-change-description*
                                           "A change description is required and was not specified."))
                    (unless (or (null change-name)
                                secret-cset-name ;; change-name is fully qualified (and won't pass)
                                (cmctl-valid-name? change-name))
                      (conman-signal-error *cm-returns-error-invalid-change-name*
                                           "The cset name, ~S, specified was invalid." change-name))
                    (unless (or old-change-name change-name)
                      (conman-signal-error *cm-returns-error-missing-change-name*
                                           "A change name is required and was not specified."))
                    ;; abstract required at master-change else cset-create (not required if no cset is open)
                    (unless
                        (or abstract-file-contents ; abstract at master-change overrides any from cset-create
                            (setq abstract-file-contents
                                  (and change-context ; get any abstract from cset-create
                                       (change-context-cset-abstract change-context)))
                            (null change-context)) ; abstract not required if no cset open
                      (conman-error-missing-abstract))

                    (when mc-email-message
                      ;; add in description & abstract file
                      (setq mc-email-message
                            (concatenate 'string mc-email-message
                                         "Description: \""
                                         (or description change-context-description) "\"~%~%"
                                         "Abstract:~%"
                                         abstract-file-contents "~%")))

                    (with-cm-master-repository (master-repository master-repository-name)
                      (with-cm-master-txn
                          (master-repository
                           master-catalog cm-session-context :read-write
                           (or description (change-context-cset-description change-context))
                           :super-cset-name-var final-cset-name
                           :supplementary-change-plist
                           (and abstract-file-contents (list :abstract abstract-file-contents)))
                        ;; By definition this (master_change)must be latest metaverison,
                        ;; therefore a with-cm-master-metaversion should be unnecessary
                        (let*((branch (workspace-master-pc-branch workspace master-repository))
                              (pc (branch-owning-project branch))
                              (holes-to-close nil) ;list of sublists, each (subsystem satellite-cset-did)
                              (affected-subsystems (repository-resolve-distributed-identifier-list
                                                    master-repository affected-subsystem-dids)))
                          ;; Don't alter frozen branches
                          (branch-enforce-workspace-eligible-for-master-change branch workspace)
                          ;; Don't alter subsystems with walls up for which you don't have holes
                          (setq holes-to-close
                                (cmctl-aux-master-change-ensure-subsystem-cset-wall-passage
                                 master-catalog master-repository
                                 pc branch workspace change-context affected-subsystems))
                          ;; Qualify the user-supplied cset name
                          (setq final-cset-name  (or (and secret-cset-name
                                                          change-name) ;; already fully qualified
                                                     (and change-name
                                                          (conman-generate-HP-cset-name
                                                           change-name user-name (pc-name pc)))
                                                     old-change-name))

                          (if change-context
                              ;; If there is a change active, our arguments will be fed to the change-close logic.
                              (progn
                                (when description
                                  ;; This is a perhaps useless formality, the cset description
                                  ;; is fed to the txn.
                                  (change-context-set-description change-context description))
                                ;; Promotes closed change-set into subsystems & satellites (:promote t)
                                ;; :PROMOTE T also promotes workspace VPB cset alterations
                                ;; **NOTE** Don't pass in affected subsystems reported by
                                ;; cmctl-master-lock as being the set of subsystems with
                                ;; file changes, it may be a superset. (they reflect cset
                                ;; additions/deletions as well as just subsystem file changes,
                                ;; parts of master-catalog-close-change process only file changes).
                                (setq mc-email-products
                                      (master-catalog-close-change
                                       master-catalog
                                       cm-session-context workspace change-context pc branch
                                       (cm-session-context-workspace-rooted-file-system
                                        cm-session-context)
                                       reason final-cset-name
                                       :file-affected-subsystems nil
                                       :all-affected-subsystems affected-subsystems
                                       :promote t)))
                            ;; else make sure we don't have a duplicate name
                            (when (master-catalog-resolve-qualified-change-set-name
                                   master-catalog
                                   final-cset-name
                                   :error-if-missing nil)
                              (conman-signal-error
                               *cm-returns-error-duplicate-change-name*
                               "The fully qualified cset name (~s) is already in use; choose another name."
                               final-cset-name)))

                          (setq mc-cset-name final-cset-name)
                          ;; promote satellite cids
                          ;; If we didn't call master-catalog-close-change, there may still be csets
                          ;; to promote or demote in the workspace, which will have to be applied to
                          ;; satellite branches and subsystem mirrors.  Do so now.
                          (unless change-context
                            (master-catalog-process-subsystem-cset-updates-for-workspace
                             master-catalog master-repository-name pc branch affected-subsystems workspace))
                          ;; If there was port activity, note the
                          ;; rejected changes for each subsystem
                          (cmctl-commit-change-consider-port-activity workspace master-repository)
                          ;; Seal any holes in walls for the csets which just passed through them
                          (dolist (sublist holes-to-close)
                            (destructuring-bind (subsystem satellite-cset-did)
                                sublist
                              (subsystem-seal-hole-in-wall subsystem satellite-cset-did)))
                          ;; Promote the current master cset as a transaction on the PC.
                          ;; In the event the master change is not accompanied by the change-context
                          ;; (which would have attached the description to the resulting change-set
                          ;; object, we do so now.
                          (master-catalog-promote-transaction-cset-to-subsystems-users
                           master-catalog description affected-subsystems)
                          (setq master-cid (txn-context-cid *txn-context*))
                          (setq master-cid-did-string
                                (cid-object-did-string
                                 (cid-object-find-or-create master-cid)))
                          ))
                      ;; Now that the transaction on the master has closed, we want the final timestamp
                      ;; of the master cset just created.
                      (with-cm-master-txn
                          (master-repository
                           master-catalog cm-session-context :read-only
                           "Retrieve change-set time-stamp to update workspace baseline versionref")
                        (multiple-value-bind (unused-reason timestamp)
                            (repository-get-cid-information master-repository master-cid)
                          (declare (ignore unused-reason))
                          (setq master-timestamp timestamp))))

                    ;; Done with the master repository.
                    ;; Clear the change context from the workspace.
                    (when change-context
                      (workspace-delete-change-context workspace))
                    ;; Update workspace VPB context, remove ws-private recordings of added/deleted csets.
                    (workspace-update-for-master-change workspace master-timestamp)
                    )
                  (setq change-completed-p t))))
             ;; unwind-protect cleanup forms for master-change activities
             ;; We don't unlock if master_change failed and they already had the lock, we only unlock
             ;; if it succeeds, or if it fails and they didn't have an outstanding lock (that last clause
             ;; is very important!).
             ;; Note that master_change can fail, but the effect of it can leave an existing master_lock
             ;; upgraded in terms of the set of subsystems it manages, since we're currently performing
             ;; workspace lock manipulation under separate transaction control.  I think this is ok.
             "unlocking master"
             (if *conman-server-log*
                 (conman-server-log-string
                  (cm-session-context-user-name cm-session-context)
                  'master-change
                  (format nil "~:[Performing~;Not performing~] master unlock."
                          (and (eq already-locked-p :already-locked)
                               (not change-completed-p))))
               (debug-message 2 "~:[Performing~;Not performing~] master unlock."
                              (and (eq already-locked-p :already-locked)
                                   (not change-completed-p))))
             (unless (and (eq already-locked-p :already-locked)
                               (not change-completed-p))
               (cmctl-master-unlock cm-session-context :if-none-locked :ignore)))))

    (cmctl-check-in-email-message *cm-cli-command-master-change* mc-cset-name
                                  user-name master-cid-did-string
                                  mc-email-message mc-email-products cset-query-str)

    mc-cset-name))

(defun cmctl-master-lock (cm-session-context description)
  "Lock all subsystems affected by the change in the current workspace.
   Note that 'change in the current workspace' must also reflect the effects
   of change_add and change_remove, which aren't necessarily accompanied by a change context.
   This is because we DO have to lock the subsystems into which we'll be promoting change-sets.

   We represent this lock by two bits of information:
   1) A lock record on the subsystem object(s).
   2) A lock record on the workspace which is locking the subsystems.

   Returns TWO values:
   1) T if successful and locks were not previously held,
      or :already-locked if successful but locks *were* previously held (i.e. was a NOP or upgraded locks),
      otherwise this function signals a terminating condition and lock status remains unchanged
   2) The list of subsystem-dids affected by the session context.
      **NOTE**: this list isn't necessarily suitable for use by logic which is trying to process
      subsystems affected by the change-context (i.e. files altered) and NOT the workspace VPB context
      (i.e. csets added/removed).  It includes subsystems affected by both change context and VPB actions."

  (cm-session-context-require cm-session-context :user-name :repository-name :ws-id)

  ;; Both the master and workspace repositories will be updated if we're successful,
  ;; satellite repositories aren't examined at all.
  (let ((reason (format nil "master lock for workspace ~d"
                        (cm-session-context-ws-id cm-session-context)))
        (return-value t)                        ;if we complete normally, we acquired locks
                                                ; we may modify this result for more specific information
        (affected-subsystem-dids nil)           ;calculated below and returned for efficiency considerations
        (workspace-identifier (cm-session-context-ws-id cm-session-context)))
    (cmctl-call-with-workspace-change-context cm-session-context
      :reason reason
      :txn-mode :read-write
      :require-active-change nil
      :receiver
      (lambda (ws-repository workspace change-context)
          ;; When requesting the workspace, note that we don't require a change, subsystems may still:
          ;; be affected by change_add and change_remove, which don't require an active change context.
          ;;*TBD*: if we decide lock status slots are versioned this must change

          (let* ( (change-context-description (and change-context
                                                   (change-context-cset-description change-context))))

            (unless (or description change-context-description)
              (conman-signal-error *cm-returns-error-missing-change-description*
                                   "A change description is required and was not specified."))

            (let ((locked-subsystem-dids nil)) ;accumulated below

              ;; Note if the user already has locks active
              (when (workspace-locked-subsystems workspace)
                (setq return-value :already-locked))

              (cmctl-call-with-master-repository-txn cm-session-context
                  :reason reason
                  :txn-mode :read-write
                  :make-cset :no-cset
                  :receiver
                  (lambda (master-repository-name master-repository master-catalog)
                      (declare (ignore master-repository-name master-catalog))
                      (with-cm-master-metaversion ((workspace-baseline-timestamp workspace))
                        (let* ((branch (cmctl-guarantee-branch-not-frozen
                                        (workspace-master-pc-branch workspace master-repository)))
                               ;;(pc (branch-owning-project branch))
                               (cc-subsystems (union
                                               ;; Those subsystems affected by the change in progress, if any
                                               (when change-context
                                                 (pc-affected-subsystem-list-from-change-context
                                                  branch change-context))
                                               ;; Those subsystems added-to/removed-from the workspace VPB
                                               (workspace-master-VPB-affected-subsystem-list
                                                workspace master-repository))))
                          ;; Any subsystems to lock?
                          (unless (setq affected-subsystem-dids
                                        (mapcar #'distributed-object-identifier cc-subsystems))
                            (conman-signal-terminating-warning *cm-returns-warning-no-subsystems-to-lock*
                                                               "There were no affected subsystems to lock."))
                          ;; Lock each subsystem, or abort the whole transaction
                          (with-cm-master-metaversion () ;latest for diddling subsystem content
                            ;; check to ensure we have write access to every subsystem
                            (dolist (subsystem cc-subsystems)
                              (unless (subsystem-subscriber-has-mode-p
                                       (subsystem-find-subscription-for-pc-branch subsystem branch)
                                       :write)
                                (conman-signal-error
                                 *cm-returns-error-no-write-permission*
                                 "No write permission to subsystem ~s." (subsystem-name subsystem))))
                            ;; now attempt to acquire the locks
                            (dolist (subsystem cc-subsystems)
                              (unless (subsystem-acquire-lock subsystem workspace-identifier
                                                              (or description change-context-description))
                                (let ((ws-id (subsystem-lock-workspace-id subsystem)))
                                  ;; This ERROR will abort all transactions pending.
                                  (conman-signal-error
                                   *cm-returns-error-subsystem-lock-failed*
                                   "Unable to lock subsystem ~s, it is already locked by workspace ~s ~
                                  [owner ~s] with description ~s."
                                   (subsystem-name subsystem)
                                   ws-id
                                   (workspace-owner-id
                                    (workspace-resolve-spec cm-session-context ws-repository :ws-id ws-id))
                                   (subsystem-lock-description subsystem)  )))
                              ;; Save this, we'll stash it in the workspace for cross reference.
                              (push (distributed-object-identifier subsystem)
                                    locked-subsystem-dids)))))))
              ;; Done with the master repository.  Do stuff here in workspace repository. *FINISH*
              ;; HP question #50: reduced affected subsystem status causes locks to be lost here.  Seems right.
              (workspace-note-locked-subsystems workspace locked-subsystem-dids)))))
    (values return-value affected-subsystem-dids)))

(defun cmctl-master-unlock (cm-session-context &key (if-none-locked :error))
  "Unlock all subsystems locked by the current workspace.  See `cmctl-master-lock' for more details.

   Returns T if successful.

   IF-NONE-LOCKED is :error, signal a terminating condition.
   IF-NONE-LOCKED is :ignore, simply return T to indicate success."

  (cm-session-context-require cm-session-context :user-name :repository-name :ws-id)

  ;; Both the master and workspace repositories will be updated if we're successful,
  ;; satellite repositories aren't examined at all.
  (let ((reason (format nil "master unlock for workspace ~d"
                        (cm-session-context-ws-id cm-session-context))))
    (cmctl-call-with-workspace-change-context cm-session-context
      :reason reason
      :txn-mode :read-write
      :require-active-change nil
      :receiver
      (lambda (ws-repository workspace change-context)
        (declare (ignore ws-repository change-context))
        ;; When requesting the workspace, note that we don't require a change, subsystems may still
        ;; be affected by change_add and change_remove, which don't require an active change context.
        (let ((locked-subsystem-dids (workspace-locked-subsystems workspace)))
          (if (null locked-subsystem-dids)
              (ecase if-none-locked
                ((:error) (conman-signal-terminating-warning *cm-returns-warning-no-subsystems-locked*
                                                             "This workspace has no locked subsystems."))
                ((:ignore) t))

            ;;*TBD*: if we decide lock status slots are versioned this must change
            (progn
              (cmctl-call-with-master-repository-txn cm-session-context
                  :reason reason
                  :txn-mode :read-write
                  :make-cset :no-cset
                  :receiver
                  (lambda (master-repository-name master-repository master-catalog)
                    (declare (ignore master-repository-name master-repository))
                    (with-cm-master-metaversion () ;latest
                      ;; Unlock the subsystems
                      ;; Rather than rely on keeping the slot in the workspace
                      ;; consistent with the info in the master db, we do the
                      ;; slower, but more reliable, map over all subsystems.
                      ;; (mapc #'subsystem-release-lock
                      ;;     (repository-resolve-distributed-identifier-list master-repository locked-subsystem-dids))

                      (master-catalog-map-over-subsystems
                       master-catalog
                       (lambda (subsystem)
                         (let ((locker-id (subsystem-lock-workspace-id subsystem)))
                           (when (and (numberp locker-id)
                                      (cm-session-context-ws-id cm-session-context))
                             (subsystem-release-lock subsystem))))))))

              ;; Done with the master repository.
              ;; Clear out the workspace list.
              (workspace-note-locked-subsystems workspace nil)))))))
  t)

(defun cmctl-resolve-product-branch (cm-session-context master-catalog &key (require-pc t) (require-branch t))
  "Given a session context and master catalog, resolve the pc and branch."
  (let ((pc-name      (cm-session-context-pc-name cm-session-context))
        (branch-name  (or (cm-session-context-release-name cm-session-context) +pc-main-branch-name+)))
    (if (null pc-name)
        (if require-pc
            (conman-signal-error *cm-returns-error-bogus-arguments* "You must specify a product.")
          (values nil nil))
      (let* ((pc (master-catalog-pc-name-lookup master-catalog pc-name :error-if-missing require-pc))
             (branch (pc-lookup-branch pc branch-name :error-if-missing require-branch)))
        ;; return branch first, it is more useful.
        (values branch pc)))))

(defun cmctl-resolve-subsystem-from-session-context (cm-session-context master-catalog
                                                     &key branch
                                                          (error-if-not-supplied t))
  "Given a session context and master-catalog, return the subsystem is specified by either the
   subsystem-name argument, or the pc-name/class-name argument pair.

   If no such subsystem is found, an error is signalled.
   If the subsystem-name argument is given in addition to the class-name argument
   an error is signalled.

   A master transaction should be in effect, and the correct master metaversion
   should be selected prior to calling this function.

   Optional branch argument may be supplied to avoid recomputing it."
  (let ((class-name     (cm-session-context-class-name     cm-session-context))
        (subsystem-name (cm-session-context-subsystem-name cm-session-context)))

    (cond ((null subsystem-name)
           (if (null class-name)
               (when error-if-not-supplied
                 (conman-signal-error *cm-returns-error-bogus-arguments* "You must specify a subsystem or class."))
               (pc-find-subsystem-for-class
                             (or branch (cmctl-resolve-product-branch cm-session-context master-catalog))
                             class-name :error-if-missing t)))

          ((null class-name)
           (or (master-catalog-subsystem-name-lookup master-catalog subsystem-name)
               (conman-signal-error
                *cm-returns-error-pc-subsystem-not-found*       ;; *** This one isn't quite right
                "There is no subsystem named ~a" subsystem-name)))

          (t
           (conman-signal-error
            *cm-returns-error-mutually-exclusive-arguments*
            "~a and ~a are mutually exclusive arguments, ~
             please specify zero or one of these arguments, not both."
            *cm-cli-keyword-class-name-syntax*
            *cm-cli-keyword-subsystem-name-syntax*)))))

(defun cmctl-master-unlock-force (cm-session-context)
  "Unlock the subsystem specified in CM-SESSION-CONTEXT.  This can be done from
   outside the context of a workspace. Email is sent to the user who locked subsys,
   notifying that it has been unlocked."
  ;;*TBD*: if we decide lock status slots are versioned this must change
  (cmctl-call-with-master-repository-txn cm-session-context
      :reason "master unlock force"
      :txn-mode :read-write
      :make-cset :no-cset
      :receiver
      (lambda (master-repository-name master-repository master-catalog)
        (declare (ignore master-repository))
        (let ((subsystem
               (with-cm-master-metaversion ()   ;latest
                 (cmctl-resolve-subsystem-from-session-context cm-session-context master-catalog))))

          ;; The SUBSYSTEM's SUBSYSTEM-LOCK-STATUS is the workspace ID of the workspace.
          ;; We need to remove this subsystem from the workspace's WORKSPACE-LOCKED-SUBSYSTEMS.
          (let ((ws-id (subsystem-lock-workspace-id subsystem)))
            (if (null ws-id)
                (conman-signal-warning *cm-returns-warning-no-subsystems-locked*
                                       "The subsystem is not locked.")
              (progn
                (subsystem-release-lock subsystem)
                (with-open-workspace-repository
                    (ws-repository (conman-workspace-repository-name master-repository-name))
                  (with-workspace-repository-txn (ws-repository :read-write "master unlock force")
                    (let ((workspace (workspace-resolve-spec cm-session-context ws-repository :ws-id ws-id)))
                      (workspace-note-unlocked-subsystem workspace (distributed-object-identifier subsystem))
                      (unless *within-regression-tests*
                        (debug-message 3 "unlock force, sending email server ~s" *conman-smtp-server*)
                        (handler-case (with-input-from-string (message-stream
                                                               (format nil "Your subsystem, ~a, was unlocked by ~a."
                                                                       (subsystem-name subsystem)
                                                                       (cm-session-context-user-name cm-session-context)))
                                        (conman-send-email (cm-session-context-user-name cm-session-context)
                                                           (workspace-owner-id workspace)
                                                           "Subsystem unlocked"
                                                           message-stream))
                          (error ()
                            (conman-signal-warning *cm-returns-warning-notification-failed*
                                                   "Email failed; could not notify lock holder ~s."
                                                   (conman-email-address-lookup (workspace-owner-id workspace)))))))))))))))
  t)

(defun cmctl-product-deactivate (cm-session-context)
  "Clear the 'active' bit in the product.  This spans all branches.  And it works largely by
   side effect, reports and stuff need to check this bit, and commands which care should probably
   warn the user if they're modifying a deactivated product.  It doesn't actually stop product modification,
   just suppresses the product in reports, etc.

   Returns T."
  (cmctl-call-with-master-repository-txn cm-session-context
      :reason (format nil "De-activate product ~s" (cm-session-context-pc-name cm-session-context))
      :txn-mode :read-write
      :make-cset :make-cset             ;the product active bit is versioned
      :receiver
      (lambda (master-repository-name master-repository master-catalog)
          (declare (ignore master-repository-name master-repository))
          (let ((pc (master-catalog-pc-name-lookup master-catalog cm-session-context
                                                   :error-if-missing t)))
            (pc-deactivate pc)
            )))
  t)

(defun cmctl-product-reactivate (cm-session-context)
  "Set the 'active' bit in the product which was previously cleared with cmctl-product-deactivate.
   Returns T."
  (cmctl-call-with-master-repository-txn cm-session-context
      :reason (format nil "Re-Activate product ~s" (cm-session-context-pc-name cm-session-context))
      :txn-mode :read-write
      :make-cset :make-cset                     ;the product active bit is versioned
      :receiver
      (lambda (master-repository-name master-repository master-catalog)
          (declare (ignore master-repository-name master-repository))
          (let ((pc (master-catalog-pc-name-lookup master-catalog cm-session-context
                                                   :error-if-missing t)))
            (pc-activate pc)
            )))
  t)

(defun cmctl-product-rename (cm-session-context new-name)
  "Rename a product from its existing metaversioned name in the session context to 'new-name'
   which should be a string.  Right now we require that the name be unique across all names
   assigned to all products to avoid rename confusion.  However the name may
   refer to a previously used name for the indicated product, in which case we're restoring an old name.

   Returns T."
  (cmctl-call-with-master-repository-txn cm-session-context
      :reason (format nil "Rename product ~s to ~s" (cm-session-context-pc-name cm-session-context) new-name)
      :txn-mode :read-write
      :receiver
      (lambda (master-repository-name master-repository master-catalog)
          (declare (ignore master-repository-name master-repository))
          ;; specified name must be an existing (current) name
          (let ((pc (master-catalog-pc-name-lookup master-catalog cm-session-context ;old-name in context
                                                   :two-phase-search nil ;must be current name
                                                   :search-all-names nil
                                                   :error-if-missing t))
                (conflicting-pc (master-catalog-pc-name-lookup master-catalog new-name
                                                               :error-if-missing nil
                                                               :search-all-names t)))
            (when (and conflicting-pc
                       (neq conflicting-pc pc))
              (conman-signal-error *cm-returns-error-product-rename-another-product-has-name*
                                   "There is another product which currently has, or has had, the name ~s. ~
                                You must select another name for the new name."
                                   new-name))
            (pc-rename pc new-name)
            )))
  t)

(defun cmctl-subsys-rename (cm-session-context new-name)
  "Rename a subsystem from its existing metaversioned name in the session context to 'new-name'
   which should be a string.  Right now we require that the name be unique across all names
   assigned to all subsystems to avoid rename confusion.  However the name may
   refer to a previously used name for the indicated subsystem in which case we're restoring an old name.

   Returns T."
  (cmctl-call-with-master-repository-txn cm-session-context
      :reason (format nil "Rename subsystem ~s to ~s" (cm-session-context-subsystem-name cm-session-context) new-name)
      :txn-mode :read-write
      :receiver
      (lambda (master-repository-name master-repository master-catalog)
          (declare (ignore master-repository-name master-repository))
          ;; specified name must be an existing (current) name
          (let ((subsystem (master-catalog-subsystem-name-lookup master-catalog cm-session-context ;old name
                                                                 :two-phase-search nil ;must be current name
                                                                 :search-all-names nil
                                                                 :error-if-missing t))
                (conflicting-subsystem (master-catalog-subsystem-name-lookup master-catalog new-name
                                                                             :error-if-missing nil
                                                                             :search-all-names t)))
            (when (and conflicting-subsystem
                       (neq conflicting-subsystem subsystem))
              (conman-signal-error *cm-returns-error-subsystem-rename-another-subsystem-has-name*
                                   "There is another subsystem which currently has, or has had, the name ~s. ~
                                You must select another name for the new name."
                                   new-name))
            (subsystem-rename subsystem new-name)
            )))
  t)

(defun cmctl-port-act (cm-session-context)
  "Implements 'port -act'.  The user has already used the reports server 'port'
   page to select changes to be ported to the workspace.  Those changes are
   currently noted in the PORT-ACTIVITY slot of the user's WORKSPACE.

   Now we must take the changes whose status in the WORKSPACE-PORT-ACTIVITY
   of the WORKSPACE is :PORT and do the equivalent of change_adding them to
   the workspace."
  (cm-session-context-require cm-session-context :repository-name :ws-id)
  #||
  ;; For each element of WORKSPACE-PORT-ACTIVITY of the workspace that
  ;; is :PORT, make sure the change has been added or removed, as
  ;; appropriate.
  (let* ((reason (format nil "Port changes: to workspace ~s"
                         (cm-session-context-ws-id cm-session-context))))
    ;; *PERFORMANCE* want to do FS management in MVCC txn, bounded by WS txn, conceptually.
    ;; We need the workspace object to relativize pathnames.  We probably don't want to sit on
    ;; the ws-rep lock while updating the disk, but we do for now.
    (cmctl-call-with-user-txn cm-session-context
      :reason reason
      :txn-mode :read-write
      :require-active-change :prohibit-active-change
      :receiver
      (lambda (workspace-repository workspace change-context
                 master-repository-name master-repository master-catalog)
          (declare (ignore workspace-repository
                           master-repository-name master-catalog))
          (error-if-product-reference-workspace workspace "port")
          (when change-context
            (conman-signal-error
             *cm-returns-error-change-not-permitted*
             "You can not port with a change open."))
          (unless (branch-workspace-up-to-date-p
                   (workspace-master-pc-branch workspace master-repository)
                   workspace)
            (conman-signal-warning
             *cm-returns-warning-workspace-not-up-to-date*
             "The workspace is not up to date.")))
      :update-workspace
      (lambda (workspace-repository workspace change-context)
          (declare (ignore workspace-repository change-context))
          ;; Perform the actual workspace update and return true.
          (workspace-act-on-port-activity workspace)))
    (cmctl-ws-regenerate-immediately cm-session-context nil)
    t)
  ||#
  (cmctl-change-add-or-remove cm-session-context nil nil nil t
                              :merge-port-activity t))

(defun cmctl-commit-change-consider-port-activity (workspace master-repository)
  "This should be called when closing a change to finalize WORKSPACE's
   porting activity.

   Satellite changes which have a status of :REJECT are noted in
   the REJECTED-SATELLITE-CSET-DIDS slot of the relevant subsystem.

   For any changes whose status is either :PORT or :DEFER, these changes are
   removed from the REJECTED-SATELLITE-CSET-DIDS of their relevant subsystems.
   This is necessary because the user has the ability to 'resurrect'
   previously rejected changes."
  (unless (workspace-has-port-activity-p workspace)
    (return-from cmctl-commit-change-consider-port-activity nil))
  (unless (workspace-port-activity-acted-on-p workspace)
    (conman-signal-error
     *cm-returns-error-port-activity-in-progress*
     "You can not commit a change while there is port activity pending for the workspace."))
  (mapc (lambda (entry)
            (destructuring-bind (subsystem-did reject-these unreject-these) entry
              (subsystem-update-rejected-satellite-csets
               (repository-resolve-distributed-identifier master-repository
                                                          subsystem-did)
               reject-these unreject-these)))
        (workspace-port-activity-collect-cset-rejection-by-subsystem workspace))
  (workspace-clear-port-activity workspace)
  nil)

(defun cmctl-description-replace (cm-session-context
                                  description-text
                                  item-type
                                  item-name)
  "replace the description on an object.  See also cm-cli_description_replace."
  (cm-session-context-require cm-session-context :repository-name)

  ;; make sure that the description is a string.  Note that an empty string is treated as NIL
  (check-type description-text (or null string) "description text")
  (if (string= description-text "")
      (setq description-text nil))

  ;; make sure we have a good item-type
  (let ((item-type-keyword (find item-type '(:class :cset :database :label :product :release :subsystem
                                             :transaction :workspace)
                                 :test #'string-equal
                                 :key #'symbol-name))
        (reason (format nil "Replace ~a description for ~s" item-type item-name)))
    ;; The WORKSPACE case is split from the others since we will need to open the workspace
    ;; repository instead of the master repository like the other cases
    ;; disallow DATABASE, LABEL, and TRANSACTION temporarily
    (ecase item-type-keyword
      ((nil)
       (conman-signal-error *cm-returns-error-invalid-description-replace-item-type*
                            "Description_replace item-type (~s) is invalid."
                            item-type))

      ((:workspace)
       (setf (cm-session-context-ws-id cm-session-context) item-name)
       (cmctl-call-with-workspace cm-session-context
         :reason reason
         :txn-mode :read-write
         :receiver (lambda (ws-repository workspace)
                     (declare (ignore ws-repository))
                     (setf (workspace-description workspace) description-text))))

      ((:class :cset :database :label :product :release :subsystem :transaction)
       (cmctl-call-with-master-repository-txn cm-session-context
           :reason reason
           :txn-mode :read-write
           :receiver
           (lambda (master-repository-name master-repository master-catalog)
             (declare (ignore master-repository master-repository-name))
             ;; Figure out what type of object we have and change its description
             (ecase item-type-keyword
               ;; disallow DATABASE, LABEL, and TRANSACTION temporarily
               ((:database :label :transaction)
                (conman-signal-error *cm-returns-error-unimplemented*
                                     "Description_replace item-type (~s) is not yet supported."
                                     item-type))

               ((:class)
                (let* ((satellite-project-ref (master-catalog-lookup-satellite-project-ref master-catalog item-name))
                       (project-did (and satellite-project-ref
                                         (satellite-project-ref-project-did satellite-project-ref))))
                  (unless satellite-project-ref
                    (conman-signal-error *cm-returns-error-project-not-found*
                                         "Description_replace class (~s) not found."
                                         item-name))
                  ;; IMPLEMENTATION NOTE: this is a nested change-set transaction!
                  ;; CAUTION: allocations are made in the satellite database until we exit the
                  ;; with-cm-satellite-repository form.
                  (with-cm-satellite-repository
                      (satellite-repository
                       (dbpath-merge (satellite-project-ref-repository-pathname satellite-project-ref)
                                      (cm-session-context-repository-name cm-session-context)))
                    (with-cm-satellite-txn (satellite-repository
                                            master-catalog :read-write
                                            (format nil "Description query for class ~s." item-name)
                                            :project-var project :cset-type :minor)
                      (setq project (repository-resolve-distributed-identifier satellite-repository project-did))
                      (set-described-object-text project description-text)))))

               ((:cset)
                (let ((cset (master-catalog-resolve-change-set-name master-catalog item-name)))
                  (set-described-object-text cset description-text)))

               ;; ((:database)
               ;;   )

               ;; ((:label)
               ;;   )

               ((:product)
                (let ((pc (master-catalog-pc-name-lookup master-catalog item-name
                                                         :error-if-missing t)))
                  (set-described-object-text pc description-text)))

               ((:release)
                (unless (cm-session-context-pc-name cm-session-context)
                  (conman-signal-error *cm-returns-error-product-name-needed*
                                       "A product name must be given to fully specify a release name"
                                       item-type))
                (let* ((pc-name (cm-session-context-pc-name cm-session-context))
                       (pc (master-catalog-pc-name-lookup master-catalog pc-name
                                                          :error-if-missing t))
                       (release (pc-lookup-branch pc item-name
                                                  :error-if-missing t)))
                  (set-described-object-text release description-text)))

               ((:subsystem)
                (let ((subsystem (master-catalog-subsystem-name-lookup master-catalog item-name
                                                                       :two-phase-search nil ;must be current name
                                                                       :search-all-names nil
                                                                       :error-if-missing t)))
                  (set-described-object-text subsystem description-text)))

               ;; ((:transaction)
               ;;   )

               ;; no error needed here because we checked at the start
               ))))))
  t)


(defun cmctl-description-query-show (cm-session-context
                                     item-type
                                     item-name)
  "query the description on an object.  See also cm-cli_description_query.
   The description is returned as the function value."
  ;; PWW ??? Someday this code should take into account that the description info is versioned
  ;;  for the moment, we will just return the description at the tip
  (cm-session-context-require cm-session-context :repository-name)
  (let ((item-type-keyword (find item-type '(:class :cset :database :label :product :release :subsystem
                                             :transaction :workspace)
                                 :test #'string-equal
                                 :key #'symbol-name))
        (reason (format nil "Query ~s description for ~s" item-type item-name)))
    (unless item-type-keyword
      (conman-signal-error *cm-returns-error-invalid-description-replace-item-type*
                           "Description_query item-type (~s) is invalid."
                           item-type))

    ;; The WORKSPACE case is split from the others since we will need to open the workspace
    ;; repository instead of the master repository like the other cases
    ;; disallow DATABASE, LABEL, TRANSACTION, and WORKSPACE temporarily
    (ecase item-type-keyword
      ((:workspace)
       (setf (cm-session-context-ws-id cm-session-context) item-name)
       (cmctl-call-with-workspace cm-session-context
         :reason reason
         :txn-mode :read-only
         :receiver (lambda (ws-repository workspace)
                     (declare (ignore ws-repository))
                     (workspace-description workspace))))

      ((:class :cset :database :label :product :release :subsystem :transaction)
       (cmctl-call-with-master-repository-txn cm-session-context
        :reason reason
        :txn-mode :read-only
        :receiver (lambda (master-repository-name master-repository master-catalog)
                    (declare (ignore master-repository master-repository-name))
                    ;; Figure out what type of object we have and query its description
                 (ecase item-type-keyword
                   ;; disallow DATABASE, LABEL, and TRANSACTION temporarily
                   ((:database :label :transaction)
                    (conman-signal-error *cm-returns-error-unimplemented*
                                         "Description_query item-type (~s) is not yet supported."
                                         item-type))

                   ((:class)
                    (let* ((satellite-project-ref (master-catalog-lookup-satellite-project-ref master-catalog item-name))
                           (project-did (and satellite-project-ref
                                             (satellite-project-ref-project-did satellite-project-ref))))
                      (unless satellite-project-ref
                        (conman-signal-error *cm-returns-error-project-not-found*
                                             "Description_query class (~s) not found."
                                             item-name))
                      (with-cm-satellite-repository
                          (satellite-repository
                           (dbpath-merge (satellite-project-ref-repository-pathname satellite-project-ref)
                                          (cm-session-context-repository-name cm-session-context)))
                        (with-cm-satellite-txn (satellite-repository
                                                master-catalog :read-only
                                                (format nil "Description query for class ~s." item-name)
                                                :project-var project :cset-type :minor)
                          ;; necessary to make with-cm-satellite-txn happy
                          (setq project (repository-resolve-distributed-identifier satellite-repository project-did))
                          (described-object-text project)))))



                   ((:cset)
                    (described-object-text
                     (master-catalog-resolve-change-set-name master-catalog item-name)))

                   ;; ((:database)
                   ;;   )

                   ;; ((:label)
                   ;;   )

                   ((:product)
                    (described-object-text
                     (master-catalog-pc-name-lookup master-catalog item-name :error-if-missing t)))

                   ((:release)
                    (unless (cm-session-context-pc-name cm-session-context)
                      (conman-signal-error *cm-returns-error-product-name-needed*
                                           "A product name must be given to fully specify a release name"
                                           item-type))
                    (let* ((pc-name (cm-session-context-pc-name cm-session-context))
                           (pc (master-catalog-pc-name-lookup master-catalog pc-name
                                                              :error-if-missing t))
                           (release (pc-lookup-branch pc item-name
                                                      :error-if-missing t)))
                      (described-object-text release)))

                   ((:subsystem)
                    (described-object-text
                     (master-catalog-subsystem-name-lookup master-catalog item-name
                                                           :two-phase-search nil ;must be current name
                                                           :search-all-names nil
                                                           :error-if-missing t)))

                   ;; ((:transaction)
                   ;;   )

                   )))))))

(defun validate-release-name (release)
  "Given an object, RELEASE, determine if it is a string that is a valid release name.
   If not, signal a ChangeSafe error that the release name is invalid."
  (unless (cmctl-valid-name? release)
    (conman-signal-error *cm-returns-error-invalid-release-name*
                         "Release name ~s invalid." release)))

#|
(defun cmctl-label-assign (cm-session-context product release label date-time)
  "Assign a label to a node of a branch."
  (warn "*************NOT YET IMPLEMENTED*******************")

  (validate-date-time date-time)
  (validate-release-name release)
  (validate-label-name label)
  (validate-product-name product)
  (with-master-txn (txn cm-session-context "label_assign")
  (let* ((label-obj (lookup-label txn cm-session-context label))
         (branch-obj (lookup-branch txn cm-session-context product release))
         (node-obj   (lookup-node txn branch-obj date-time)))
         (label-set-node label-obj node-obj))))
|#

(defun cmctl-create-product-release (cm-session-context description release-name)
  "CM-SESSION-CONTEXT
   DESCRIPTION is either nil or a string,
   RELEASE-NAME is the name of the release we are to create"

  (cm-session-context-require cm-session-context :repository-name :pc-name)
  (debug-message 1 "Creating a release ~a.~%" release-name)
  (validate-release-name release-name)
  (check-type description (or null string))

  ;; My interpretation of a vague spec is 1 below.  Its a guess.
  ;;  1. labels name a product-tree node, so time cannot be meaningful with a label.
  ;;  2. (Alternative interpretation) The time is used to look at where the labels WERE THEN,
  ;;     and the node can then be picked from an "old" version of the label's position.
  (let* ((pc-name (cm-session-context-pc-name cm-session-context))
         (double-inheritance (cm-session-context-release-name cm-session-context))
         (from-release-name (or double-inheritance +pc-main-branch-name+))
         (reason (format nil "Create the ~a release of product ~a" release-name pc-name))
         (date-time (cm-session-context-time-spec cm-session-context))
         (label-name (cm-session-context-label-name cm-session-context))
         naha-debug-new-branch
         )
    ;; when from-release was not given, we want the main-trunk
    (when (and date-time label-name)
      (conman-signal-error *cm-returns-error-mutually-exclusive-arguments*
                           "Both label and time specified - semantics unclear.")) ;*FINISH*
    (when label-name
      (conman-signal-error *cm-returns-error-unimplemented*
                           "LABEL specifiers are not yet implemented by this command.")) ;*FINISH*
    (cmctl-call-with-master-repository-txn cm-session-context
        :reason reason
        :txn-mode :read-write
        :receiver
        (lambda (master-repository-name master-repository master-catalog)
          (declare (ignore master-repository))
          ;; verify the product-configuration name already exists (and find it)
          ;; verify that the branch specified via from-release exists (as of the given time) (and find it)
          ;; verify the release does NOT exist CURRENTLY.
          (debug-message 3 "Cid of release creation:~S"
                         (txn-context-cid *txn-context*))
          (with-cm-master-metaversion (date-time)
            (let* ((pc (master-catalog-pc-name-lookup master-catalog pc-name :error-if-missing t))
                   (from-branch (pc-lookup-branch-ok pc from-release-name :error-if-missing t)))
              (with-cm-master-metaversion ()
                (when (pc-lookup-branch-ok pc release-name :error-if-missing nil)
                  (conman-signal-error *cm-returns-error-release-already-exists*
                                       "A release named ~a already exists in product ~a"
                                       release-name pc-name))
                (let ((new-branch (master-catalog-pc-release-create
                                   master-catalog master-repository-name
                                   pc-name pc
                                   from-release-name from-branch
                                   description
                                   release-name date-time
                                   double-inheritance
                                   )))
                  (setq naha-debug-new-branch new-branch)
                  ;; Promote the
                  ;; changes to the PC so we can see them.  Note that in this
                  ;; case, promoting onto the main branch of the created PC is
                  ;; the correct behavior.  We want it to see it's subsystems,
                  ;; after all.
                  (master-catalog-promote-transaction-cset master-catalog reason pc new-branch)))))
          ;; Make some debugging noise to show the composition of
          ;; the new branch.
          (let ((my-noise-level 4))
            (when-debugging my-noise-level
              (progn ;;with-version ((branch-get-most-recent-version naha-debug-new-branch) :include-current t :version-context :pc)
                (debug-message my-noise-level "*** new release composition for ~a[~a]"
                               (pc-name (branch-owning-project naha-debug-new-branch))
                               (branch-name naha-debug-new-branch))
                (pc-map-over-subsystems naha-debug-new-branch
                  (lambda (subsystem)
                      (with-cm-master-metaversion ()
                        (debug-message my-noise-level "    subsystem ~a" (subsystem-name subsystem))
                        (subsystem-map-over-subscribers subsystem
                          (lambda (subscriber)
                              (let ((branch (subsystem-subscriber-pc-branch subscriber)))
                                (debug-message my-noise-level "       ~a[~a] modes: ~s"
                                               (pc-name (branch-owning-project branch))
                                               (branch-name branch)
                                               (subsystem-subscriber-mode-list subscriber)))))))))))
          ))
    t))


(defun cmctl-release-freeze (cm-session-context)
  (cm-session-context-require cm-session-context :repository-name :pc-name :release-name)
  (let* ((pc-name (cm-session-context-pc-name cm-session-context))
         (release-name (cm-session-context-release-name cm-session-context))
         (reason (format nil "Freeze release ~a of product ~a" release-name pc-name)))
    (cmctl-call-with-master-repository-txn cm-session-context
        :reason reason
        :txn-mode :read-write
        :receiver
        (lambda (master-repository-name master-repository master-catalog)
          (declare (ignore master-repository-name master-repository))
          ;; verify the product-configuration name already exists (and find it)
          ;; verify that the branch specified via release-name exists (and find it)
          (with-cm-master-metaversion ()
            (let* ((pc (master-catalog-pc-name-lookup master-catalog pc-name :error-if-missing t))
                   (branch (pc-lookup-branch-ok pc release-name :error-if-missing t)))
              ;; if someone tries to freeze the main branch, we refuse
              (when (eq (project-get-main-branch pc) branch)
                (conman-signal-error *cm-returns-error-invalid-release-name*
                                     "Product main branch cannot be frozen."))
              (let* ((version (branch-get-latest-mutable-version branch)))
                ;; version is nil if the branch is frozen
                (if version
                    (progn ;;with-version (version :version-context :pc)
                      (pc-map-over-subsystems
                          branch
                        (lambda (subsystem)
                            (with-cm-master-metaversion ()
                              ;; if the subsystem is already frozen, we
                              ;; need not have write permission to it,
                              ;; and it stays frozen.
                              ;; Further, since it is frozen, no product
                              ;; can have write permission to it, so we can
                              ;; skip it entirely.
                              (unless (subsystem-frozen? subsystem)
                                (subsystem-map-over-subscribers
                                    subsystem
                                  (lambda (subsystem-subscriber)
                                      (let* ((the-branch (subsystem-subscriber-pc-branch
                                                          subsystem-subscriber))
                                             (the-pc (branch-owning-project the-branch))
                                             (wperm (subsystem-subscriber-has-mode-p
                                                     subsystem-subscriber
                                                     :write)))
                                        (debug-message
                                         4
                                         "*** subscriber subsystem ~a, product ~a[~a], modes ~a"
                                         (subsystem-name subsystem)
                                         (pc-name the-pc) (branch-name the-branch)
                                         (subsystem-subscriber-mode-list subsystem-subscriber))
                                        (cond
                                         ((eq the-branch branch)
                                          (unless wperm
                                            (conman-signal-error
                                             *cm-returns-error-no-write-permission*
                                             "No write permission to subsystem ~s."
                                             (subsystem-name subsystem)))
                                          ;; set the freeze bit on the subsystem
                                          (subsystem-freeze subsystem)
                                          ;; now remove the write permission
                                          (set-subsystem-subscriber-modes
                                           subsystem-subscriber
                                           (remove :write
                                                   (subsystem-subscriber-mode-list
                                                    subsystem-subscriber)))
                                          )
                                         (wperm
                                          (conman-signal-error
                                           *cm-returns-error-has-write-permission*
                                           "Product ~a[~a] has write permission to subsystem ~s."
                                           (pc-name the-pc)
                                           (branch-name the-branch)
                                           (subsystem-name subsystem))))
                                        )))
                                ))))
                      (version-freeze version))
                  (conman-signal-warning *cm-returns-warning-already-frozen*
                                         "The release ~a of product ~a is already frozen."
                                         release-name pc-name)
                  )))))))
  t)


(defun cmctl-release-rename (cm-session-context new-name)
  "Rename a release from its existing metaversioned name in the session context for a given
   product to 'new-name'
   which should be a string.  Right now we require that the name be unique across all names
   assigned to all releases for a given product to avoid rename confusion.  However the name may
   refer to a previously used name for the indicated release, in which case we're restoring an old name.

   Returns T."
  (cm-session-context-require cm-session-context :repository-name :pc-name :release-name)
  (check-type new-name string)
  (cmctl-call-with-master-repository-txn cm-session-context
      :reason (format nil "Rename release ~s to ~s within product ~s"
                      (cm-session-context-release-name cm-session-context)
                      new-name (cm-session-context-pc-name cm-session-context))
      :txn-mode :read-write
      :receiver
      (lambda (master-repository-name master-repository master-catalog)
          (declare (ignore master-repository-name master-repository))
          ;; specified name must be an existing (current) name
          (let* ((pc-name (cm-session-context-pc-name cm-session-context))
                 (pc (master-catalog-pc-name-lookup master-catalog pc-name
                                                    :search-all-names nil
                                                    :two-phase-search nil ;; must be current name
                                                    :error-if-missing t))
                 (release (pc-lookup-branch pc (cm-session-context-release-name cm-session-context)
                                            :search-all-names nil
                                            :two-phase-search nil ;; must be current name
                                            :error-if-missing t))
                 (version (branch-get-latest-mutable-version release))
                 ;; either the most-recent version, or nil if it is frozen
                 (conflicting-release (pc-lookup-branch pc new-name
                                                        :search-all-names t
                                                        :two-phase-search t
                                                        :error-if-missing nil)))
            (when (and conflicting-release
                       (neq conflicting-release release))
              (conman-signal-error *cm-returns-error-release-rename-another-release-has-name*
                                   "There is another release which currently has, or has had, the name ~s ~
                                within the product ~s. You must select another name for the new name."
                                   new-name pc-name))
            (unless version
              (conman-signal-error *cm-returns-error-release-rename-is-frozen*
                                   "The release ~s within product ~s is frozen and may not be renamed"
                                   (cm-session-context-release-name cm-session-context) pc-name))
            (pc-branch-rename pc release new-name)
            )))
  t)

;;; Wall commands

(defun cmctl-validate-wall-arguments (cm-session-context master-catalog)
  "Validate wall_* command arguments and return resolved information for product, branch,
   and optional subsystems specified by the command.  If there are any problems with the arguments, signal
   an error.

   This routine assumes that it has been called in the context of a master transaction
   with the latest metaversion.

   MASTER-CATALOG should be the master-catalog object inside the master-repository.

   Returns three values:
   1) a pointer to the PC matching the product specification.
   2) a pointer to the branch matching the release specification, or the main branch
      if there was no release specification.
   3) a pointer to the subsystem indicated by -CLASS or -SUBSYSTEM arguments, if present, or
      NIL if there was no subsystem/class preference."
  (cm-session-context-require cm-session-context :repository-name :pc-name)
  (multiple-value-bind (branch pc)
      (cmctl-resolve-product-branch cm-session-context master-catalog)
    (values
     pc
     branch
     (cmctl-resolve-subsystem-from-session-context cm-session-context master-catalog
                                                   :branch branch
                                                   :error-if-not-supplied nil))))

(defun cmctl-do-wall-command (cm-session-context command-name cset-name function)
  "This is a helper routine for the CMCTL interfaces to the WALL_* commands,
   since all three commands have similar arguments.

   Execute FUNCTION in the context of the master repository in a NO-CSET transaction
   in order to manipulate walls and wall status.

   COMMAND-NAME should be a string naming the command being carried out by FUNCTION.

   CSET-NAME may be NIL.  If CSET-NAME is NOT nil, function will be supplied with
   the appropriate satellite cset-dids.

   Arguments are validated, the function is invoked on each relevant subsystem
   with the following arguments:

   1) A subsystem to be processed which is either one of all the subsystems in the pc
      if the user didn't specify a subsystem, or the particular subsystem the user did
      specify.

   2) The PC named by the command arguments.

   3) The specific satellite-cset-did if the caller specified a cset-name.
      This argument is not passed if the caller did not specify a cset-name.

   FUNCTION is called in the context of the latest master metaversion.

   No particular value is returned, but an error is signalled if argument validation fails."
  ;; We're validating args in transaction, so reason string will look funny if args fail,
  ;; but so will transaction, so it's fairly moot.
  (cmctl-call-with-master-repository-txn
      cm-session-context
      :reason (format nil "~a on product ~a~@[ (subsystem ~a)~]~@[ (class ~a)~]"
                      command-name
                      (cm-session-context-pc-name cm-session-context)
                      (cm-session-context-subsystem-name cm-session-context)
                      (cm-session-context-class-name cm-session-context))
      :txn-mode :read-write
      ;; NOTE: this command does NOT result in a cset.  The data is NOT under cset control.
      :make-cset :no-cset
      :receiver (lambda (master-repository-name master-repository master-catalog)
                  (declare (ignore master-repository-name master-repository))
                  (multiple-value-bind (pc branch subsystem)
                      (cmctl-validate-wall-arguments cm-session-context master-catalog)
                    (cmctl-guarantee-branch-not-frozen branch)
                    (mapc
                     ;; If caller supplied a cset-name, resolve it to the subsystem cset
                     ;; for each call to function.
                     (if (null cset-name)
                         ;; no cset name, just invoke function on subsystem and pc
                         (lambda (subsystem)
                           (funcall function subsystem pc))
                       (let ((component-cset-dids
                              (master-catalog-list-satellite-cset-dids
                               master-catalog
                               (master-catalog-resolve-change-set-name master-catalog cset-name))))
                         ;; have a cset name, invoke function on subsystem, pc and resolved component cset
                         (lambda (subsystem)
                           (funcall function subsystem pc (subsystem-specific-cset subsystem component-cset-dids)))))
                     (if subsystem
                         (list subsystem)
                       (pc-get-subsystem-list branch)))))))

(defun cmctl-raise-wall (cm-session-context)
  "Implement the WALL_RAISE command, see the command specificiation for details.
   Note that master_change must check wall status to enforce walls."
  (cmctl-do-wall-command
   cm-session-context "wall_raise" nil ;; no particular cset
   (lambda (subsystem pc)
     (unless (subsystem-raise-wall subsystem pc)
       (conman-signal-warning
        *cm-returns-warning-wall-status-unchanged*
        "A wall was already up for subsystem ~a by product ~a"
        (subsystem-name subsystem)
        (cm-session-context-pc-name cm-session-context)))))
  t)

(defun cmctl-lower-wall (cm-session-context)
  "Implement the WALL_LOWER command, see the command specification for details.
   Note that master_change must check wall status to enforce walls."
  (cmctl-do-wall-command
   cm-session-context "wall_lower" nil ;; no particular cset
   (lambda (subsystem pc)
     (unless (subsystem-lower-wall subsystem pc)
       (conman-signal-warning
        *cm-returns-warning-wall-status-unchanged*
        "No wall was up for subsystem ~a by product ~a"
        (subsystem-name subsystem)
        (cm-session-context-pc-name cm-session-context)))))
  t)

(defun cmctl-hole-wall (cm-session-context cset-name)
  "Implement the WALL_HOLE command, see the command specification for details.
   Note that master_change must check wall status to enforce walls."
  (cmctl-do-wall-command
   cm-session-context "wall_hole" cset-name
   (lambda (subsystem pc component-cset-did)
     (when component-cset-did
       (subsystem-hole-wall subsystem pc component-cset-did))))
  t)
||#

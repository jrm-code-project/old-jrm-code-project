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

(eval-when (:load-toplevel :execute)
  (export '(master-catalog

            call-with-master-catalog-transaction

            master-catalog/create-product
            master-catalog/create-class
            master-catalog/create-subscription
            master-catalog/create-subsystem
            master-catalog/install-in-repository
            master-catalog/populate-workspace
            master-catalog/promote-transaction-cset
            master-catalog/resolve-qualified-change-set-name
            master-catalog/scan-products
            master-catalog/scan-classes
            master-catalog/scan-subsystems
            master-catalog/setup-workspace
            master-catalog/find-product
            master-catalog/find-class
            master-catalog/find-subsystem
            master-catalog/update-file-system
#||
            master-catalog-retrieve
            master-catalog-get-pc-list
            master-catalog-map-over-pcs
            master-catalog-get-sorted-pc-list
            master-catalog-get-subsystem-list
            master-catalog-get-sorted-subsystem-list
            master-catalog-map-over-subsystems
            master-catalog-map-over-subsystems-of-class
            master-catalog-create
            master-catalog-note-change-set-name
            master-catalog-resolve-change-set-name ;short cut for many applications of the next two
            master-catalog-resolve-qualified-change-set-name
            master-catalog-resolve-unqualified-change-set-name
            master-catalog-list-satellite-cset-dids
            master-catalog-lookup-cset-relationship ; more efficient form of mc-list-satellite-cset-dids
            master-catalog-find-master-cset-did-for-satellite-cset

            master-catalog-index-cset-relationships-by-cset
            master-catalog-index-cset-relationships-by-cset-cached
            with-master-catalog-cset-relationship-reverse-index-caching

            master-catalog-get-satellite-project-ref-list
            master-catalog-pc-name-lookup
            master-catalog-lookup-satellite-project-ref
            master-catalog-create-satellite-project-ref
            master-catalog-promote-transaction-cset ;use every cmctl operation which updates PC content
            master-catalog-get-satellite-project-ref-for-satellite-cset-did
            master-catalog-get-class-name-for-satellite-cset-did
            master-catalog-satellite-repository-db-files
            master-catalog-workspace-redundant-vpb-changes

            master-catalog-schema-upgrade-to-9-19
||#
            )))

(proclaim (standard-optimizations))

(defclass master-catalog ()
  ;; subsystems and products (PC's) in this repository
  ((products   :initform nil
               :version-technique :composite-set
               :accessor master-catalog/products)

   (subsystems :initform nil
               :version-technique :composite-set
               :accessor master-catalog/subsystems)

   ;; All satellite repositories known to this master.  We don't allow satellite repository names
   ;; to change, though the projects they contain may be renamed.
   ;; **WARNING** BE CAREFUL IF YOU OPERATE in non-latest metaversion views of this list
   ;; or you might try to create a satellite which already exists.  Only update this list using the latest
   ;; metaversion.
   (satellite-repositories :initform nil :version-technique :composite-set)

   ;; Projects (a.k.a. ChangeSafe "classes") contained in satellite repositories.
   ;; The descriptors contain the key mapping from a project name to a satellite-repository-name.
   ;; We could almost make this just a (project-name . satellite-name) mapping, but
   ;; we need to version project-name, and we also want to cache in the master some information
   ;; in the satellites so that we don't always have to examine the satellites for often accessed
   ;; information.
   (classes :accessor master-catalog/classes
            :initform nil
            :version-technique :composite-set)

   ;; Cset-relationship-tuples is conceptually a list of sublists, where each sublist is a tuple.
   ;; For every master cid which results in the creation
   ;; of satellite cids, a tuple is added which enumerates the master cid and the satellite cids which
   ;; it caused to be created.  e.g. '((master.cid.1 satellite-1.cid.1 satellite-2.cid.1))
   ;; Because we want portable references, blah blah blah, we actually reference DIDS of
   ;; CHANGE-SET objects rather than the cids.  We may instead wish to store CID-OBJECT references.
   ;; TBD.

   ;; Right now, this information is maintained only for change transactions which arise from
   ;; WITH-CM-MASTER-TXN and WITH-CM-SATELLITE-TXN.  This is ok, since those are the interesting
   ;; txns which manipulate satellite databases.

   ;; Note that because of the high volume of csets we expect to track, we actually represent
   ;; this information as a vector of vectors to achieve space compaction.
   (cset-relationship-tuples :initform (make-instance 'persistent-vector
                                                           :initial-element nil
                                                           :size 1)
                             :version-technique :nonversioned)

   (cset-rel-tuples-index :initform (make-instance 'persistent-vector
                                                        :initial-element -1
                                                        :size 1)
                          :version-technique :nonversioned)

   ;; BOTH these slots are updated ONLY by vm-txn-note-change-set,
   ;; except for schema upgrading.

   ;; The cset-rel-tuples-index slot is a conceptual hash table into the
   ;; cset-relationship-tuples slot. This is used by
   ;; master-catalog-lookup-cset-relationship
   ;; to avoid an extremely costly linear search of cset-relationship-tuples.
   ;; This is very important for cset_add, cset_remove, and csets_from_file.

   ;; The idea is that the did-string of the master-cid's did is hashed.
   ;; Reducing that hash modulo the number of entries in cset-rel-tuples-index,
   ;; finds a "home" index of cset-rel-tuples-index. Using the sb32 value
   ;; in that element, we either have a -1 (so the entry is not in the
   ;; hash table) or we get an index into cset_relationship_tuples.
   ;; If there is no hash collision, that element of cset_relationship_tuples
   ;; will contain the desired master-cid did we are looking for. If it
   ;; isn't the one we want, we have had a hash collision, and we resolve it
   ;; by linear probing in the next-door (circularly) element of
   ;; cset-rel-tuples-index.
   ;; The number of elements of cset-rel-tuples-index is always a prime number,
   ;; and is also maintained to be more than twice as large as the number of
   ;; entries in cset-relationship-tuples. That is important, to prevent
   ;; clustering and slow searching. So when it grows, cset-rel-tuples-index
   ;; grows by a reasonable factor (about 2) so that it always contains
   ;; at least half "holes", that is, -1.  Further, we want to avoid frequent
   ;; growth, because growing requires computing every entry in the hash table
   ;; again. That makes for a big transaction, as every element of the
   ;; cid-relationship-tuple vector has to be mapped in, and rehashed with
   ;; the new size of cset-rel-tuples-index.
   ;; Space considerations: In Jack's db, there are roughly 40,000 elements
   ;; currently in the cset-relationship-tuples.  Suppose we had 100,000
   ;; elements. In Jack's db, it appears that the tuples are about 2 elements
   ;; each, average. Suppose it were 9. Then the tuples would take 4*(1+9)=40
   ;; bytes each, so 40*100,000 = 4Mb total (plus another 400,000 for the
   ;; cset-relationship-tuples vector itself).  This is large, but not likely
   ;; to be a cause of breakage anytime soon.


   ;; The cset-name-hashtable maps cset names to csets.
   ;; While the HP model of ChangeSafe doesn't allow changing the name of a cset, we allow this
   ;; in general.  So this hash table is keyed by cset name, and valued by all csets which EVER
   ;; bore that name in their versioned name component.  The hash value is therefore a list.
   ;; In the case of system augmented names (by change_create/master_change), there shouldn't
   ;; be any collisions.  We also use this slot to hash unaugmented user names to csets,
   ;; and those are far more likely to have collisions (one key -> multiple csets).  In the case
   ;; of un-augmented names, this is expected. In the case of augmented names, this is an error.
   (cset-name-hashtable :version-technique nil
                        :initform (make-instance 'persistent-hash-table :size 1023)
                        :reader master-catalog/cset-name-hashtable)
   )
  (:documentation "Catalog/hierarchy-root of versioned information maintained in the master repository.")
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defun master-catalog/scan-products (master-catalog)
  (declare (optimizable-series-function))
  (versioned-object/scan-composite-versioned-slot master-catalog 'products))

(defun master-catalog/scan-subsystems (master-catalog)
  (declare (optimizable-series-function))
  (versioned-object/scan-composite-versioned-slot master-catalog 'subsystems))

(defun master-catalog/scan-satellite-repositories (master-catalog)
  (declare (optimizable-series-function))
  (versioned-object/scan-composite-versioned-slot master-catalog 'satellite-repositories))

(defun master-catalog/scan-classes (master-catalog)
  (declare (optimizable-series-function))
  (versioned-object/scan-composite-versioned-slot master-catalog 'classes))

(defun master-catalog/scan-classes-and-subsystems (master-catalog)
  (declare (optimizable-series-function 2))
  (map-fn '(values csf-class subsystem)
          (lambda (subsystem)
            (values (subsystem/csf-class subsystem) subsystem))
          (master-catalog/scan-subsystems master-catalog)))

(defun master-catalog/scan-relevant-subsystems (master-catalog satellite-project-dids)
  (declare (optimizable-series-function))
  (choose-if (lambda (subsystem)
               (member (csf-class/project-did (subsystem/csf-class subsystem)) satellite-project-dids))
             (master-catalog/scan-subsystems master-catalog)))

(defun master-catalog/scan-relevant-classes-and-subsystems (master-catalog satellite-project-dids)
  (declare (optimizable-series-function 2))
  (map-fn '(values csf-class subsystem)
          (lambda (subsystem)
            (values (subsystem/csf-class subsystem) subsystem))
          (master-catalog/scan-relevant-subsystems master-catalog satellite-project-dids)))

(defconstant +master-catalog-root-key+ 'conman-master-catalog
  "A symbol key used for a named repository root, and which has a MASTER-CATALOG
   attached to it.  Created at the time of master-repository creation.")

(defun master-catalog/install-in-repository (master-repository)
  "Create a master catalog for a master repository and stash it in a well known root.
   There should be only one per master repository."
  (assert (eq (repository/type master-repository) :master))
  (repository/add-locally-named-root master-repository
                                     (make-instance 'master-catalog)
                                     +master-catalog-root-key+)
  nil)

(defun master-catalog/retrieve (master-repository)
  "Retrieve the master catalog for a master repository"
  (or (repository/locally-named-root master-repository +master-catalog-root-key+)
      (error "master catalog not found")))


;;; Receiver for this is tricky.  It has to be a thunk that returns a thunk like this:
;;;
;;;  (lambda (cm-transaction)
;;;      ..... make some changes ....
;;;     ;; return two values, a cset-name and this thunk
;;;     (values cset-name
;;;      (lambda (change-set)
;;;           ... promote the change set ....
;;;          )))

(defun call-with-master-catalog-transaction (master-repository-dbpath
                                             user-id-specifier
                                             &key 
                                             ;; Supply these two
                                             master-metaversion
                                             version
                                             aux-master-metaversion
                                             aux-version

                                             reason
                                             transaction-type
                                             receiver)
  "Begin a transaction within the master repository.

   TRANSACTION-TYPE should be :read-only or :read-write.

   Invoke receiver on three arguments, the master-repository-name,
   the master-repository and the master-catalog."
  (assert (member transaction-type '(:read-write :read-only :read-only-compare
                                     :read-only/no-change-set :read-write/no-change-set)))
  (assert version)
  (call-with-master-repository-transaction
   master-repository-dbpath user-id-specifier
   :transaction-type  transaction-type
   :reason            reason
   :master-metaversion master-metaversion
   :version            version
   :aux-master-metaversion  aux-master-metaversion
   :aux-version      aux-version
   :receiver (lambda (master-repository master-transaction)
               (funcall receiver
                        master-repository
                        master-transaction
                        (master-catalog/retrieve master-repository)))))

(defun master-catalog/find-product (master-catalog product-name)
  (let ((products (master-catalog/scan-products master-catalog)))
    (collect-or
     (choose (map-fn 'boolean (lambda (name)
                                    (string= name product-name))
                         (#M named-object/name products))
             products))))

(defun master-catalog/find-class (master-catalog class-name)
  "Look up CLASS-NAME in the master catalog classes (a.k.a. ChangeSafe 'class') list
   under the current cid-set in scope.  Return a CLASS-REF object if we find one matching
   project-name, or NIL if we don't."
  ;; This is expensive, especially when called from within nested report loops, e.g. product history
  ;;(debug-message 4 "Project refs ~s"
  ;;             (mapcar #'satellite-project-ref-name (master-catalog-get-satellite-project-ref-list master-catalog)))
  (let ((classes (master-catalog/scan-classes master-catalog)))
    (collect-or
     (choose (map-fn 'boolean (lambda (name)
                                    (string= name class-name))
                         (#M named-object/name classes))
             classes))))

(defun master-catalog/find-subsystem (master-catalog subsystem-name)
  (let ((subsystems (master-catalog/scan-subsystems master-catalog)))
    (collect-or
     (choose (map-fn 'boolean (lambda (name)
                                    (string= name subsystem-name))
                         (#M named-object/name subsystems))
             subsystems))))
#||
(defun master-catalog/find-product (master-catalog product-name)
  (let ((products (master-catalog/products master-catalog)))
    (or (find product-name products
              :test #'string=
              :key #'named-object/name)
        ;; check for *any* possible name
        (find product-name products
              :test #'string=
              :key #'named-object/historical-names))))
||#

(defun master-catalog/resolve-product-specifier (master-repository master-catalog product-specifier)
  "The product specifier names a product and/or branch in some way, we return
   the product and the associated branch.

   Here are some of the possible things the product-specifier could be:

   A product.
   A branch.
   A string containing the name of a product.
   A distributed-identifier for a product.
   A string containing the distributed-identifier for a product.
   A distributed-identifier for a branch.
   A string containing the distributed-identifier for a branch."
  (etypecase product-specifier
    (product (values product-specifier (product/main-branch product-specifier)))
    (branch  (values (branch/owning-project product-specifier) product-specifier))
    (distributed-identifier (master-catalog/resolve-product-specifier
                             master-repository
                             master-catalog
                             (repository/resolve-distributed-identifier master-repository product-specifier)))
    (string (let ((product-did-prefix (concatenate 'string
                                                   (repository/name master-repository)
                                                   "."
                                                   (symbol-name (class-name (find-class 'project)))
                                                   "."))
                  (branch-did-prefix (concatenate 'string (repository/name master-repository)
                                                  "."
                                                  (symbol-name (class-name (find-class 'project)))
                                                  "."
                                                  )))
              (cond ((or (search product-did-prefix product-specifier)
                         (search branch-did-prefix product-specifier))
                     (master-catalog/resolve-product-specifier
                      master-repository
                      master-catalog
                      (parse-did product-specifier)))
                    ((master-catalog/find-product master-catalog product-specifier)
                     => (lambda (product)
                          (master-catalog/resolve-product-specifier master-repository master-catalog product)))
                    (t (conman-error/no-such-product product-specifier)))))))

(defun master-catalog/resolve-branch-specifier (master-repository master-catalog branch-specifier)
  "The branch specifier names a branch in some way, we return
   the product and the branch associated with the branch specifier.

   Here are some of the possible things the branch-specifier could be:

   A product.
   A branch.
   A string containing the name of a product.
   A distributed-identifier for a product.
   A string containing the distributed-identifier for a product.
   A distributed-identifier for a branch.
   A string containing the distributed-identifier for a branch."
  (if (consp branch-specifier)
      (master-catalog/resolve-branch-specifier-in-product
       master-repository
       master-catalog
       (master-catalog/resolve-product-specifier master-repository master-catalog (car branch-specifier))
       (cdr branch-specifier))
      (master-catalog/resolve-product-specifier master-repository master-catalog branch-specifier)))

(defun master-catalog/resolve-version (master-repository master-catalog branch-specifier version-specifier)
  (multiple-value-bind (product branch)
      (master-catalog/resolve-branch-specifier master-repository master-catalog branch-specifier)
    (values product branch (branch/most-recent-version branch))))

(defun master-catalog/resolve-qualified-change-set-name (master-catalog cset-name)
  "Use CSET-NAME, which should be a system-augmented fully qualified change-set name,
   to do a by-name lookup of a change-set in the master catalog.
   Cset names are versioned in the VM model, but don't change in the CONMAN model, though
   we might like to let them change in other product realizations of the CONMAN package.

   The HP model of ChangeSafe doesn't allow change-set names to change.
   Therefore, if we encounter multiple csets with the same name, we signal an error.
   If we want to allow it, we'll have to resolve which cset with the indicated name is
   the one we want.

   Return a master-repository change-set if we find one.  If we don't find one and
   ERROR-IF-MISSING is true, we signal an error, otherwise we return NIL."
  (let ((change-set-list (persistent-hash-table/gethash
                          (master-catalog/cset-name-hashtable master-catalog)
                          cset-name)))
    (assert (null (cdr change-set-list)))
    (car change-set-list)))

(defun master-catalog/create-product (user-id-specifier reason master-catalog product-name branch-name product-description)
  "Create and return a product-configuration named PC-NAME.

   This routine may result in creation of subsystems, which in turn must open satellite repositories,
   which is why this layer for creating PC's exists in master-catalog (to preserve encapsulation of
   satellite repository opens).

   MASTER-REPOSITORY-NAME is the path specification used to open the master repository, which
   is merged with the pathname of the satellite repository to hopefully locate the satellite repository.

   If this PC is being created from another PC, specify that as PARENT-PC.  When this is
   NON-NIL, One of COPY-ALL or COPY-NONE must be specified to control resulting subsystem
   status in the created PC.  Note that COPY-FILE isn't yet supported *FINISH*.

   DESCRIPTION, if non-nil, should also be a string.

   METAVERSION-TIMESTAMP, if non-nil, should be a time-stamp which specifies the
   metaversion view of the parent-pc and its attendant subsystems.

   COPY-ALL, if true, specifies that the subsystems of the newly created PC will be new
   subsystems derived from the parent-pc (which must be non-nil).  Newly created
   subsystems inherit all from those of the parent-pc.  This option is mutually exclusive
   with the COPY-NONE option.

   COPY-NONE, if true, specifies that the subsystems of the newly created PC will be those
   of the parent PC (which must be non-nil), in a shared fashion.  However the newly
   created PC will NOT have WRITE access to the subsystems. This option is mutually
   exclusive with the COPY-NONE option.

   NOTE: checking for argument compatibility w.r.t. COPY-* and PARENT-PC is assumed done by the caller."
  (let* ((product (make-instance 'product
                                 :name product-name
                                 :description product-description))

         (branch  (make-instance 'csf-branch
                                 :owning-project product
                                 :name branch-name))

         (version (make-instance 'version
                                 :owning-branch branch)))
    (branch/add-version branch version)
    (project/add-main-branch product branch)
    (pushlast product (master-catalog/products master-catalog))
    ;; Promote the changes to the PC so we can
    ;; see them.  Note that in this case,
    ;; promoting onto the main branch of the
    ;; created PC is the correct behavior.  We
    ;; want it to see it's subsystems, after
    ;; all.
    (values (list :name (cmctl/generate-cset-name "create-product" user-id-specifier (named-object/name product))
                  :description reason
                  :satellite-change-set-alist '())
            (lambda (change-set)
              (debug-message 3 "About to promote a change set.")
              (debug-message 3 "Change set is ~s" change-set)
              (when reason
                (setf (described-object/description change-set) reason))
              (product/record-relevant-change-set product change-set)
              (product/promote-master-cset-into-version-scope product branch change-set)
              (values product branch)))))

(defun master-catalog/create-class (master-repository-db-name master-repository master-catalog
                                                              reason user-id-specifier
                                                              class-name description subdirectory)
  "Create a satellite repository, and in that repository create a PROJECT named CLASS-NAME.
   Create a CLASS object in MASTER-REPOSITORY which references the new project, and link it
   into MASTER-CATALOG.

   SUBDIRECTORY names the root directory of the satellite project, which is used
   in forming subdirectories of the master configurations.

   DESCRIPTION is the user's description for this class/project.

   The satellite repository shares the master repository's name as a prefix, and will be created
   in the same directory as the master repository.  This information is derived from MASTER-REPOSITORY-NAME,
   which was used to open the master repository in the first place, and which is minimally a file name,
   and maximally a full path string.

   A change-transaction is assumed active on MASTER-REPOSITORY.

   We signal an error if a class exists which currently bears the indicated
   class name, otherwise we return the class which is created.
   (*TBD*: options here, see code comments).

   We don't bother to name satellite repositories after projects, since projects can be renamed,
   but repositories can't. (Well, they could, but their internal knowledge of their new name
   can't change since it's used in distributed identifiers.  We don't support repository renaming yet.)"

  ;; caller ensures that class does not already exist.
  (multiple-value-bind (satellite-repository-name
                        satellite-repository-path
                        satellite-pathname)
      (conman-satellite-repository-name (repository/name master-repository)
                                        class-name
                                        master-repository-db-name)

    (declare (ignore satellite-repository-name))

    (debug-message 3 "Creating satellite repository. path is ~s" satellite-repository-path)
    (debug-message 3 "Creating satellite repository. relative path is ~s" satellite-pathname)

    (repository/add-satellite-repository master-repository satellite-pathname)

    (multiple-value-bind (satellite-project-did satellite-cset-did)
        ;; Create the class/satellite-project-ref in the satellite
        ;; IMPLEMENTATION NOTE: this is a nested change-set transaction!  whee!
        ;; Wanna have fun?  Try to figure out how to finish the two-phase
        ;; commit between the satellite and the master when you have closed
        ;; the satellite.
        (with-open-repository (satellite-repository
                               satellite-repository-path :update
                               :parent-repository master-repository-db-name
                               :repository-type :satellite
                               :if-exists :error
                               :if-does-not-exist :create)
          ;; Have to use VM transaction directly because
          ;; the satellite repository doesn't have anything
          ;; in it (yet).
          (call-with-vm-transaction
           :change-set-type 'minor-change-set
           :metaversion :latest-metaversion
           :version-specifier :latest-version  ;; use master cid set
           :reason reason
           :repository satellite-repository
           :transaction-type :read-write
           :user-id-specifier user-id-specifier
           :receiver (lambda (vm-transaction)
                       (declare (ignore vm-transaction))
                       (let ((satellite-project
                              (make-instance 'satellite-project
                                             :name class-name
                                             :description description
                                             :root-directory (make-instance 'rfm-directory
                                                                            :element-name " ;" ;; this is the root
                                                                            :modification-date (get-universal-time)
                                                                            )
                                             :filenames-case-sensitive? t)))
                         (values 
                          (list :name (cmctl/generate-cset-name "create-class" user-id-specifier "")
                                :description reason)
                          (lambda (satellite-cset)
                            ;; Promote the class creation cset into the satellite project.
                            (project/add-change-set satellite-project satellite-cset)
                            (values satellite-project satellite-cset)))))))

      (debug-message 1 "Satellite project ~s created, now creating reference to it." satellite-project-did)

      (let ((csf-class (make-instance 'csf-class
                                      :name class-name
                                      :description description
                                      :project-did satellite-project-did
                                      :%project-creation-cset (pstore-list-cons nil nil)
                                      :subsystem-creation-csets '()
                                      :subdirectory subdirectory
                                      :satellite-relative-pathname satellite-pathname)))
        (debug-message 1 "Reference created.")
        (pushlast csf-class (master-catalog/classes master-catalog))
        (values csf-class satellite-cset-did)))))

(defun master-catalog/resolve-subsystem-specifier (master-repository master-catalog branch subsystem-specifier)
  (etypecase subsystem-specifier
    (subsystem (values (subsystem/csf-class subsystem-specifier) subsystem-specifier))
    (string (let ((subsystem-did-prefix (concatenate 'string
                                                     "."
                                                     (symbol-name (class-name (find-class 'subsystem)))
                                                     ".")))
              (cond ((search subsystem-did-prefix subsystem-specifier)
                     (master-catalog/resolve-subsystem-specifier
                      master-repository
                      master-catalog
                      branch
                      (repository/resolve-distributed-identifier master-repository subsystem-specifier)))
                    ((master-catalog/find-subsystem master-catalog subsystem-specifier)
                     => (lambda (subsystem)
                          (master-catalog/resolve-subsystem-specifier master-repository master-catalog branch subsystem)))
                    (t (conman-error/no-such-subsystem subsystem-specifier)))))))

(defun master-catalog/create-subsystem (master-repository-dbpath master-catalog
                                                                  reason user-id-specifier
                                                                  csf-class satellite-metaversion
                                                                  subsystem-name description subdirectory)

  (multiple-value-bind (satellite-project-did satellite-subsystem-did satellite-change-set-did)

      (with-open-repository (satellite-repository (dbpath/merge (csf-class/satellite-relative-pathname csf-class)
                                                                master-repository-dbpath)
                                                  :update
                                                  :repository-type :satellite)
        (call-with-satellite-transaction
         :satellite-metaversion satellite-metaversion
         :satellite-version :latest-version
         :reason reason
         :repository satellite-repository
         :transaction-type :read-write
         :user-id-specifier user-id-specifier
         :receiver (lambda (satellite-transaction)
                     (declare (ignore satellite-transaction))
                     (debug-message 3 "Satellite DID is ~s" (csf-class/project-did csf-class))
                     (let* ((satellite-project (repository/resolve-distributed-identifier
                                                satellite-repository
                                                (csf-class/project-did csf-class)))
                            (satellite-subsystem (make-instance 'satellite-subsystem
                                                                :name subsystem-name
                                                                :description description
                                                                :subdirectory subdirectory
                                                                :owning-project satellite-project))
                            (satellite-version (make-instance 'version
                                                              :owning-branch satellite-subsystem)))
                       (branch/add-version satellite-subsystem satellite-version)
                       (values (list :name (cmctl/generate-cset-name "satellite-create-subsystem"
                                                                     user-id-specifier
                                                                     (named-object/name csf-class))
                                     :description reason)
                               (lambda (change-set)
                                 (satellite-project/add-subsystem satellite-project satellite-subsystem)
                                 (values satellite-project satellite-subsystem 
                                         (distributed-object-identifier change-set))))))))

    (debug-message 3 "Creating subsystem reference for ~s ~s" satellite-project-did satellite-subsystem-did)
    ;; make a reference to the subsystem in the master catalog
    (let ((csf-subsystem (make-instance 'subsystem
                                        :name subsystem-name
                                        :description description
                                        :project-did satellite-project-did
                                        :branch-did  satellite-subsystem-did
                                        :csf-class csf-class
                                        :subdirectory subdirectory)))
      (debug-message 1 "~s created (refers to ~s)." csf-subsystem satellite-subsystem-did)
      (pushlast csf-subsystem (master-catalog/subsystems master-catalog))
      (values csf-subsystem satellite-change-set-did))))

(defun master-catalog/create-subscription (conman-request
                                           master-repository-dbpath
                                           branch-specifier subsystem-specifier subscription-mode)
  (let ((reason (format-in-language (conman-request/client-locale conman-request) nil
                                    :subscribe-to-subsystem-reason branch-specifier subsystem-specifier))
        (userid (conman-request/user-name conman-request)))
    (call-with-master-catalog-transaction
     master-repository-dbpath
     userid
     :master-metaversion :latest-metaversion
     :version            :latest-version
     :reason reason
     :transaction-type :read-write
     :receiver (lambda (master-repository master-transaction master-catalog)
                 (declare (ignore master-transaction))
                 (multiple-value-bind (product branch)
                     (master-catalog/resolve-branch-specifier master-repository master-catalog
                                                              branch-specifier)
                   (multiple-value-bind (class subsystem)
                       (master-catalog/resolve-subsystem-specifier master-repository
                                                                   master-catalog
                                                                   nil
                                                                   subsystem-specifier)
                     (debug-message 3 "Creating subscription ~s, ~s, ~s, ~s" product branch class subsystem)
                     (subsystem/add-subscriber subsystem branch)
                     (csf-branch/add-subsystem branch subsystem)
                     (values (list
                              :name (cmctl/generate-cset-name (concatenate 'string "subscribe-to-" (named-object/name subsystem))
                                                              userid
                                                              (named-object/name product))
                              :description reason
                              :satellite-change-set-alist '())
                             (lambda (change-set)
                               (let ((class-creation-master-change-set (csf-class/project-creation-cset class))
                                     (subsystem-creation-master-change-set (csf-class/subsystem-creation-cset class subsystem)))
                                 (check-type class-creation-master-change-set super-change-set)
                                 (check-type subsystem-creation-master-change-set super-change-set)

                                 (product/record-relevant-change-sets product (list class-creation-master-change-set
                                                                                    subsystem-creation-master-change-set
                                                                                    change-set))
                                 (product/promote-master-csets-into-version-scope product branch (list class-creation-master-change-set
                                                                                                       subsystem-creation-master-change-set
                                                                                                       change-set)) 
                                 nil)))))))))

(defun master-catalog/setup-workspace (master-repository user-id-specifier
                                       master-transaction
                                       master-catalog file-system
                                       reason metaversion-time-stamp
                                       pc branch
                                       populate-files)
  "Calculate & return various values so that a workspace may be created and (later) populated on a
   client file-system workspace representaion using FILE-SYSTEM.  This function does NOT actually
   populate the file-system (see master-catalog-populate-workspace-mvcc below) unless the
   POPULATE-FILES argument is non-nil.
   To do this requires examining each subsystem in the product configuration (PC).

   MASTER-REPOSITORY-NAME is the path specification used to open the master repository, which
   is merged with the pathname of the satellite repository to hopefully locate the satellite repository.

   A transaction on the master repository is implicit in this call, and a nested transaction
   is performed upon the appropriate satellite repositories.  All information is used in read-only fashion.

   Since we want to support read-only use of the versioned repositories, and can't update the
   workspace repository if the versioned repositories were opened in a parent transaction for read-only
   access,

   REASON is passed down from the caller, and should be the reason used for opening the master repository.

   METAVERSION-TIME-STAMP should be a TIME-STAMP object which drives selection of a repository metaversion,
   or NIL, in which case the latest metaversion is used.

   POPULATE-FILES controls whether or not the client file-system workspace representation gets populated.
   A value of NIL means it does NOT get populated.  Anything else cause it to be populated.

   ********************************** NOTE *************************************
   The caller is responsible for creating a workspace with workspace-create in
   the semipersistent workspace repository  using the values returned by this function.

   This function is used for both newly created workspaces as well as repurposing existing workspaces.
   At this level, both actions are viewed as an incremental update.  An incremental update
   on an empty workspace will populate the whole file system...

   We return four values for this purpose, DID's to be used in a call to workspace-create:
   by the caller of this routine (assuming no conditions are signalled):
   1) the DID of the PC to which the workspace applies
   2) the DID of the branch in the pc to which the workspace applies
   3) the DID of the version on the pc-branch which represents the workspace baseline version.
   4) A timestamp which acts as the baseline reference for the workspace.
   5) the master-metaversion-cid-set"

  (unless metaversion-time-stamp
    (setq metaversion-time-stamp (timestamp-allocate)))

  (let ((master-metaversion-cid-set (transaction/cid-set master-transaction))
        (pc-branch branch))

    ;; Populate the client file-system workspace representation?
    (when populate-files
      (master-catalog/populate-workspace master-repository user-id-specifier master-catalog
                                         reason
                                              file-system pc-branch
                                              reason metaversion-time-stamp
                                              master-metaversion-cid-set))

    ;; In the event this function is called for workspace creation, the user will want
    ;; to create a workspace object in the workspace repository.
    ;; This must be accomplished by the caller, so we return information necessary to do this.
    ;;(format *debug-io* "~%Cid-set for pc-lookup-branch: ~s" (txn-context-cid-set *txn-context*))
    (values
            ;; This is the time-reference baseline for the workspace.
            ;; Note that this time-stamp is deliberately unspecific in sub-universal-time granularity
            ;; (or metaversion-time-stamp (time-stamp-create (get-universal-time)))

            ;; Making the time stamp deliberately unspecific is probably a *bad* idea:  if changes come
            ;; in during this interval, (but after we update the master-catalog), the workspace will have
            ;; the wrong timestamp.
            metaversion-time-stamp
            master-metaversion-cid-set
            )))

(defun master-catalog/populate-workspace (master-repository-dbpath user-id-specifier
                                                                   master-repository master-catalog file-system
                                                                   reason
                                                                   branch
                                                                   metaversion-time-stamp
                                                                   master-metaversion-cid-set)

  "Populate a client file-system workspace representaion using FILE-SYSTEM.
   To do this requires examining each subsystem in the product configuration (PC).

   This function is used when master-catalog-setup-workspace-mvcc above is passed a NIL for its
   POPULATE-FILES argument.  This function is then called later in the sequence and will actually
   populate the files.

   MASTER-REPOSITORY-NAME is the path specification used to open the master repository, which
   is merged with the pathname of the satellite repository to hopefully locate the satellite repository.

   A transaction on the master repository is implicit in this call, and a nested transaction
   is performed upon the appropriate satellite repositories.  All information is used in read-only fashion.

   Since we want to support read-only use of the versioned repositories, and can't update the
   workspace repository if the versioned repositories were opened in a parent transaction for read-only
   access,

   REASON is passed down from the caller, and should be the reason used for opening the master repository.

   PC is the Product-Configuration.  PC-VERSION is the appropriate version to use.

   METAVERSION-TIME-STAMP should be a TIME-STAMP object which drives selection of a repository metaversion,
   or NIL, in which case the latest metaversion is used.

   MASTER-METAVERSION-CID-SET is transaction context cid-set.

   ********************************** NOTE *************************************
   The caller is responsible for creating a workspace with workspace-create in
   the semipersistent workspace repository  using the values returned by this function.

   This function could be used for both newly created workspaces as well as repurposing existing workspaces.
   However, at the moment it is only used for creating a workspace.
   At this level, both actions are viewed as an incremental update.  An incremental update
   on an empty workspace will populate the whole file system..."
  (let ((subsystem-count (collect-length (csf-branch/scan-subsystems branch)))
        (progress 0))

    (csf-branch/map-over-branch-satellite-branches
     master-repository-dbpath user-id-specifier branch
     :reason reason
     :satellite-metaversion metaversion-time-stamp
     :satellite-transaction-type :read-only
     :receiver (lambda (csf-class subsystem satellite-repository
                        relative-subdirectory
                        satellite-project satellite-branch satellite-version)
                 (file-system/note-progress file-system nil (ratio->percentage progress subsystem-count))
                 (incf progress)
                 (subsystem-satellite/extract-files-to-disk
                  subsystem satellite-repository file-system
                  satellite-project satellite-branch
                  :when-overwriting-directory #'publish/supersede-if-overwriting
                  :when-overwriting-file #'publish/overwrite-if-changed
                  :clean nil
                  )
                 nil))))

(defun project-filter (satellite-project input-list)
  (collect 'list
           (choose-if (lambda (item)
                        (change-context/applies-to-project? item satellite-project))
                      (scan 'list input-list))))

(defun master-catalog/close-change (master-repository-dbpath master-catalog user-id-specifier reason branch fs
                                                             metaversion-time-stamp
                                                             satellite-cset-name
                                                             &key file-additions
                                                             file-changes
                                                             file-removals
                                                             added-class-cset-tuples
                                                             removed-class-cset-tuples
                                                             promoter)
  "Perform a project-checkin for every subsystem affected by the workspace change-context.

   Assume that an update change transaction is active on the master repository, but do not
   assume that a change transaction is active on the workspace repository.

   REASON is a transaction reason string which the caller has presumably already cobbled together for
   the master repository transaction.

   FINAL_CSET_NAME is the final, fully qualified name to be used for this check-in/promotion/close.  It
   must be unique.

   PROMOTER, should be a procedure that takes the resulting change set and promotes
   it to wherever it ought to be (the workspace or satellite branch).

   FILE-AFFECTED-SUBSYSTEMS, if specified, represents a set of subsystems in which file changes will
   take place.  If non-nil, it should be a list of subsystem objects.

   ALL-AFFECTED-SUBSYSTEMS, if specified, represents a set of subsystems in which either file changes
   will take place,or in which cset promotions will take place.  If non-nil, it should be a list of
   subsystem objects.  If NIL, it defaults to FILE-AFFECTED-SUBSYSTEMS.

   FILE-AFFECTED-SUBSYSTEMS must always be a subset of ALL-AFFECTED-SUBSYSTEMS.

   Returns a list of the affected product names"
  (iterate ((subsystem (csf-branch/scan-relevant-subsystems 
                        branch (union 
                                (map 'list #'change-context/affected-project-did file-additions)
                                (union
                                 (map 'list #'change-context/affected-project-did file-changes)
                                 (map 'list #'change-context/affected-project-did file-removals))))))
    (let* ((csf-class-did (distributed-object-identifier (subsystem/csf-class subsystem)))
           (vpb-cid-dids-to-add (cdr (assoc csf-class-did added-class-cset-tuples)))
           (vpb-cid-dids-to-remove (cdr (assoc csf-class-did removed-class-cset-tuples))))

      (subsystem/call-with-satellite-transaction
       master-repository-dbpath user-id-specifier subsystem
       :reason reason
       :satellite-transaction-type :read-write
       :satellite-metaversion metaversion-time-stamp
       :vpb-cid-dids-to-add vpb-cid-dids-to-add
       :vpb-cid-dids-to-remove vpb-cid-dids-to-remove
       :receiver (lambda (csf-class
                          satellite-repository
                          satellite-transaction
                          satellite-project
                          satellite-branch
                          satellite-version)
                   (master-catalog/checkin-satellite-project
                    satellite-repository
                    satellite-project satellite-branch
                    (logical-file-system/change-directory fs (csf-class/subdirectory csf-class))
                    :file-additions (project-filter satellite-project file-additions)
                    :file-changes   (project-filter satellite-project file-changes)
                    :file-removals  (project-filter satellite-project file-removals))
                   (values (list :name satellite-cset-name
                                 :description reason)
                           (lambda (satellite-change-set)
                             (funcall promoter csf-class satellite-repository satellite-branch satellite-version satellite-change-set))))))))

(defun master-catalog/checkin-satellite-project (satellite-repository satellite-project satellite-branch
                                                                   subdir-file-system
                                                                   &key 
                                                                   file-additions
                                                                   file-changes
                                                                   file-removals)
  (debug-message 4 "We're ready to do project-checkin on ~s" satellite-project)
  (rfm:project-checkin satellite-repository satellite-project satellite-branch subdir-file-system
                       :file-additions file-additions
                       :file-changes   file-changes
                       :file-removals  file-removals))

(defun master-catalog/promote-cset-to-branch-subsystems (master-catalog master-change-set product branch)
  ;; We'll just promote them once, mmkay?
  (let ((branches-seen nil))
    (iterate ((subsystem (csf-branch/scan-subsystems branch)))
      (iterate ((subscribing-branch (#m subscriber/branch (subsystem/scan-subscribers subsystem))))
        (unless (member subscribing-branch branches-seen)
          (push subscribing-branch branches-seen)
          (product/promote-master-cset-into-version-scope product subscribing-branch master-change-set))))))

(defun master-catalog/map-over-project-subsystems (master-repository-dbpath user-id-specifier
                                                                            master-catalog project branch
                                                                            &key satellite-transaction-type
                                                                            satellite-metaversion
                                                                            reason
                                                                            subsystem-list
                                                                            function)
  (check-type project project)
  (check-type branch csf-branch)
  (csf-branch/map-over-branch-satellite-branches
   master-repository-dbpath user-id-specifier branch
   :reason reason
   :satellite-metaversion satellite-metaversion
   :satellite-transaction-type satellite-transaction-type
   :receiver (lambda (csf-class subsystem 
                      satellite-repository
                      relative-subdirectory
                      satellite-project
                      satellite-branch
                      satellite-version)
               (funcall function 
                        csf-class
                        subsystem satellite-repository satellite-project satellite-branch))))

(defun master-catalog/extract-project-files-to-disk (master-repository-dbpath user-id-specifier
                                                                              master-catalog
                                                                              project branch reason timestamp
                                                                              file-system
                                                                              &key 
                                                                              subsystem-list
                                                                              (report-file-system file-system)
                                                                              (clean t)
                                                                              (read-only t)
                                                                              workspace)
  "Populate a directory for a product-configuration with files from the
   current view of the product configuration (as determined by a WITH-VERSION of the appropriate
   master version on the appropriate master branch of a master product configuration).

   To do this we must interate over all subsystems in the product configuration
   and match their contents to those on the disk.  In all cases, FILE-SYSTEM should be a server relative
   file-system, and we use the reference directory spec of the PC in order to determine where
   on the file system to write.

   *TBD*: we will probably want a parameter which controls whether to make the disk match exactly
   the PC file structure, or other options, similar to the RETRIEVE FILE interface in e-zchange.

   REASON is some reason used to open read-only transactions on satellite repositories in order to
   query their contents.

   METAVERSION-TIMESTAMP is used to specify the date-driven metaversion in the satellite repository
   which will govern the state of the subsystem branch and version(s) on that branch which we use.
   It should be a time-stamp object, or NIL indicating that the 'latest' metaversion is desired.

   MASTER-CATALOG is the  master catalog object in the master repository.

   MASTER-REPOSITORY-NAME is used to resolve the file system locations of satellite repositories.

   PC is the product-configuration we're mapping over.  The appropriate versioned content view
   of the PC is to be established by the caller.

   FILE-SYSTEM is a logical file system which is assumed rooted to some meaningful location
   such as the root directory for a PC server-side reference directory, or a client workspace root.

   REPORT-FILE-SYSTEM is a file system to which progress indications will be sent.  It defaults to
   file-system.

   SUBSYSTEM-LIST, if specified, constrains the operation to the indicated list of subsystems.

   SUBSYSTEM-FILE-ALIST, if specified, is an association list whose keys are subsystems, and whose
   values are file DIDS in that subsystem to be extracted.  Subsystem keys which aren't in subsystem-list
   (if specified) are ignored.

   CLEAN, if true, causes target directories to be cleaned out before population.

   WORKSPACE, if present, should be a WORKSPACE object in the WORKSPACE repository. It will be
   used to factor in workspace Virtual Private Branch (VPB) change sets in computing the view
   of file content extracted to disk. It is used only for accessing, not updating, workspace data.

   CHANGE-CONTEXT, if present, should be the transient change context associate with the
   workspace.  It will be used to avoid losing changes in checked out files.

   See also master-catalog-extract-pc-file-names-to-list which was cloned from this function.

   Return value: nil."
  (master-catalog/map-over-project-subsystems 
   master-repository-dbpath user-id-specifier master-catalog project branch
   :satellite-transaction-type :read-only
   :satellite-metaversion timestamp
   :reason reason
   :subsystem-list subsystem-list
   :function (lambda (csf-class subsystem satellite-repository satellite-project satellite-branch)
                 (subsystem-satellite/extract-files-to-disk
                  subsystem satellite-repository file-system
                  satellite-project 
                  satellite-branch
                  ;; deal with overwriting
                  :report-file-system report-file-system
                  :clean clean
                  :when-overwriting-directory #'publish/supersede-if-overwriting
                  :when-overwriting-file #'publish/overwrite-if-changed)
                 nil)))


(defun master-catalog/update-file-system (master-repository-dbpath user-id-specifier
                                                                   master-catalog
                                                                   file-system
                                                                   file-system-name
                                                                   old-timestamp
                                                                   old-product
                                                                   old-branch
                                                                   old-version
                                                                   new-timestamp
                                                                   new-product
                                                                   new-branch
                                                                   new-version
                                                                   &key (report-only nil)
                                                                   (report-file-system file-system)
                                                                   VPB-old-added-satellite-cset-dids
                                                                   VPB-old-removed-satellite-cset-dids
                                                                   VPB-new-added-satellite-cset-dids
                                                                   VPB-new-removed-satellite-cset-dids)
  "Incrementally update a file system from the master catalog.

   It is assumed that the file system in question was published using
   old-time-stamp, vpb-old-added-satellite-cset-dids, and vpb-old-removed-cset-dids
   and we wish to update the file system in question to
   new-time-stamp, vpb-new-added-satellite-cset-dids, and vpb-new-removed-satellite-cset-dids

   FILE-SYSTEM should be rooted at the reference directory or workspace root.
   FILE-SYSTEM-NAME should be a string like \"workspace\" or \"reference directory\"
   which is used for noise.

   REPORT-FILE-SYSTEM, if present, is where the noise goes to."
  (assert (eq old-product new-product))

  (let ((performed-three-way-merge nil)
        (merge-conflict-count       0)
        (performed-binary-merge    nil)
        (old-master-cid-set (repository/master-cid-set *repository* :end-time old-timestamp))
        (new-master-cid-set (repository/master-cid-set *repository* :end-time new-timestamp)))

    (debug-message 3 "old-master-cid-set ~s" old-master-cid-set)
    (debug-message 3 "new-master-cid-set ~s" new-master-cid-set)

    (product/partition-subsystems
     master-repository-dbpath user-id-specifier
     new-product ;; same as old
     old-timestamp old-branch old-version
     new-timestamp new-branch new-version
     :VPB-old-added-satellite-cset-dids   VPB-old-added-satellite-cset-dids
     :VPB-old-removed-satellite-cset-dids VPB-old-removed-satellite-cset-dids
     :VPB-new-added-satellite-cset-dids   VPB-new-added-satellite-cset-dids
     :VPB-new-removed-satellite-cset-dids VPB-new-removed-satellite-cset-dids
     :receiver
     (lambda (subsystems-unchanged subsystems-added subsystems-removed subsystems-changed)
       (debug-message 3 "MASTER-CATALOG-UPDATE-FILE-SYSTEM~
                       ~%subsystems-unchanged ~s~
                       ~%subsystems-added ~s~
                       ~%subsystems-removed ~s~
                       ~%subsystems-changed ~s"
                      subsystems-unchanged subsystems-added subsystems-removed subsystems-changed)

       (file-system/note-progress report-file-system
                                  (format nil "~:[Updating~;Report of work necessary to update~] ~a."
                                          report-only file-system-name)
                                  nil)

       (when subsystems-unchanged
         ;; establish view to get latest names
         (call-with-after-view
          (lambda ()
            (file-system/note-progress report-file-system
                                       (format nil " Subsystem~? ~:[are~;is~] unchanged, no update is necessary."
                                               "~#[~; ~a~;s ~a and ~a~:;s~@{~#[~; and~] ~a~^,~}~]"
                                               (mapcar #'named-object/name subsystems-unchanged)
                                               (null (cdr subsystems-unchanged)))
                                       nil))))
       ;; When a subsystem has been removed, we delete it from the reference area.
       (when subsystems-removed
         ;; establish old view to get name deleted
         (call-with-before-view
          (lambda ()
            (file-system/note-progress report-file-system
                                       (format nil " Subsystem~? ~:[are~;is~] obsolete and will be removed."
                                               "~#[~; ~a~;s ~a and ~a~:;s~@{~#[~; and~] ~a~^,~}~]"
                                               (mapcar #'named-object/name subsystems-removed)
                                               (null (cdr subsystems-unchanged)))
                                       nil)))
         (unless report-only
           (dolist (subsystem-removed subsystems-removed)
             (file-system/delete-directory
              file-system
              (call-with-before-view 
               (lambda ()
                 (subsystem/relative-subdirectory subsystem-removed)))
              :recursive t
              :force t))))

       ;; When a subsystem has been added, we unconditionally publish it to the
       ;; reference area.  There should not be any files in that subsystem there
       ;; currently, so there is no chance of optimizing the publishing.
       (when subsystems-added
         (call-with-after-view
          (lambda ()
            (file-system/note-progress report-file-system
                                       (format nil " Subsystem~? ~:[are~;is~] new."
                                               "~#[~; ~a~;s ~a and ~a~:;s~@{~#[~; and~] ~a~^,~}~]"
                                               (mapcar #'subsystem-name subsystems-added)
                                               (null (cdr subsystems-added)))
                                       nil)
            (unless report-only
              (dolist (added subsystems-added)
                (master-catalog/extract-subsystem-files-to-disk master-repository-dbpath
                                                                master-catalog
                                                                added
                                                                file-system
                                                                "Updating file system directory."
                                                                new-timestamp
                                                                :report-file-system report-file-system
                                                                :clean t))))))
       (when subsystems-changed
         (dolist (subsystem subsystems-changed)
           (call-with-after-view
            (lambda ()
              (file-system/note-progress report-file-system
                                         (format nil " Subsystem ~a in ~a will be updated."
                                                 (named-object/name subsystem)
                                                 file-system-name) nil)))
            (multiple-value-bind (subsystem-three-way-merge
                                  subsystem-conflict-count
                                  subsystem-binary-merge)
                (let ((class-did (distributed-object-identifier (subsystem/csf-class subsystem))))
                  (master-catalog/extract-changed-subsystem-files-to-disk
                   master-repository-dbpath
                   user-id-specifier
                   master-catalog
                   subsystem
                   old-timestamp old-version;; timestamp of old publishing
                   new-timestamp new-version;; timestamp of now
                   file-system
                   "Update file system."
                   :report-only report-only
                   :report-file-system report-file-system
                   :VPB-old-added-satellite-cset-dids
                   (cdr (assoc class-did VPB-old-added-satellite-cset-dids))
                   :VPB-old-removed-satellite-cset-dids
                   (cdr (assoc class-did VPB-old-removed-satellite-cset-dids))
                   :VPB-new-added-satellite-cset-dids
                   (cdr (assoc class-did VPB-new-added-satellite-cset-dids))
                   :VPB-new-removed-satellite-cset-dids
                   (cdr (assoc class-did VPB-new-removed-satellite-cset-dids))
                   ))
              (declare (ignore master-catalog))
              (setq performed-three-way-merge (or performed-three-way-merge subsystem-three-way-merge))
              (incf merge-conflict-count subsystem-conflict-count)
              (setq performed-binary-merge    (or performed-binary-merge subsystem-binary-merge))
              )))))
    (values performed-three-way-merge
            merge-conflict-count
            performed-binary-merge)))

(defun master-catalog/extract-changed-subsystem-files-to-disk 
  (master-repository-dbpath user-id-specifier master-catalog subsystem
                            old-timestamp old-version
                            new-timestamp new-version
                            file-system reason
                            &key 
                            (report-only nil)
                            VPB-old-added-satellite-cset-dids
                            VPB-old-removed-satellite-cset-dids
                            VPB-new-added-satellite-cset-dids
                            VPB-new-removed-satellite-cset-dids
                            (report-file-system file-system))
  "Update a directory for a subsystem with the changes in the files that occurred
   between OLD-TIMESTAMP and NEW-TIMESTAMP.

   *TBD*: we will probably want a parameter which controls whether to make the disk match exactly
   the PC file structure, or other options, similar to the RETRIEVE FILE interface in e-zchange.

   REASON is some reason used to open read-only transactions on satellite repositories in order to
   query their contents.

   OLD-TIMESTAMP and NEW-TIMESTAMP are used to specify the date-driven metaversion in the
   satellite repository.  It should be a timestamp object, or NIL indicating that the 'latest'
   metaversion is desired.

   MASTER-CATALOG is the master catalog object in the master repository.

   MASTER-REPOSITORY-NAME is used to resolve the file system locations of satellite repositories.

   SUBSYSTEM is the subsystem we're extracting.  The appropriate versioned content view
   of the SUBSYSTEM is to be established by the caller.

   FILE-SYSTEM is a logical file system which is assumed rooted to some meaningful location
   such as the root directory for a PC server-side reference directory, or a client workspace root.

   Keyword argument REPORT-FILE-SYSTEM if specified is a file-system to which the publishing noise
   should go.

   Return value: the master-catalog."
  (let* ((subdir (call-with-after-view (lambda () 
                                         (subsystem/subdirectory subsystem))))
         (subdir-file-system (logical-file-system/change-directory file-system subdir)))
    (subsystem/call-with-satellite-repository 
     master-repository-dbpath subsystem
     :readonly
     (lambda (csf-class satellite-repository)
       (subsystem/extract-changed-files-to-disk user-id-specifier subsystem
                                                satellite-repository 
                                                subdir-file-system
                                                old-timestamp :latest-version
                                                new-timestamp :latest-version
                                                :report-only report-only
                                                :report-file-system report-file-system
                                                :VPB-old-added-satellite-cset-dids VPB-old-added-satellite-cset-dids
                                                :VPB-new-added-satellite-cset-dids VPB-new-added-satellite-cset-dids
                                                :VPB-old-removed-satellite-cset-dids VPB-old-removed-satellite-cset-dids
                                                :VPB-new-removed-satellite-cset-dids VPB-new-removed-satellite-cset-dids)))))


#||
(declaim #+allegro (:fbound pc-get-subsystem-list
                  pc-lookup-branch
                  pc-lookup-branch-ok
                  pc-name
                  pc-affected-subsystem-list-from-change-context)
         (special +pc-main-branch-name+))

(defconstant +master-catalog-root-key+ "conman-master-catalog"
  "A string key used for a named repository root, and which has a MASTER-CATALOG
   attached to it.  Created at the time of master-repository creation.")

(define-versioned-class master-catalog
    (:documentation "Catalog/hierarchy-root of versioned information maintained in the master repository.")

  ;; subsystems and products (PC's) in this repository
  (products :initial-value nil :version-technique :composite :collection-type :set)
  (subsystems :initial-value nil :version-technique :composite :collection-type :set)

  ;; All satellite repositories known to this master.  We don't allow satellite repository names
  ;; to change, though the projects they contain may be renamed.
  ;; **WARNING** BE CAREFUL IF YOU OPERATE in non-latest metaversion views of this list
  ;; or you might try to create a satellite which already exists.  Only update this list using the latest
  ;; metaversion.
  (satellite-repositories :initial-value nil :version-technique :composite :collection-type :set)

  ;; Projects (a.k.a. ChangeSafe "classes") contained in satellite repositories.
  ;; The descriptors contain the key mapping from a project name to a satellite-repository-name.
  ;; We could almost make this just a (project-name . satellite-name) mapping, but
  ;; we need to version project-name, and we also want to cache in the master some information
  ;; in the satellites so that we don't always have to examine the satellites for often accessed
  ;; information.
  (satellite-project-refs :initial-value nil :version-technique :composite :collection-type :set)

  ;; Cset-relationship-tuples is conceptually a list of sublists, where each sublist is a tuple.
  ;; For every master cid which results in the creation
  ;; of satellite cids, a tuple is added which enumerates the master cid and the satellite cids which
  ;; it caused to be created.  e.g. '((master.cid.1 satellite-1.cid.1 satellite-2.cid.1))
  ;; Because we want portable references, blah blah blah, we actually reference DIDS of
  ;; CHANGE-SET objects rather than the cids.  We may instead wish to store CID-OBJECT references.
  ;; TBD.

  ;; Right now, this information is maintained only for change transactions which arise from
  ;; WITH-CM-MASTER-TXN and WITH-CM-SATELLITE-TXN.  This is ok, since those are the interesting
  ;; txns which manipulate satellite databases.

  ;; Note that because of the high volume of csets we expect to track, we actually represent
  ;; this information as a vector of vectors to achieve space compaction.
  (cset-relationship-tuples :version-technique nil
                            :initial-value (astore-vector-create
                                            'astore-disk-vector 5000
                                            :initial-element nil))

  (cset-rel-tuples-index :version-technique nil
                         :initial-value (astore-vector-create
                                         'astore-disk-vector-sb32 1
                                         :initial-element -1))

  ;; BOTH these slots are updated ONLY by vm-txn-note-change-set,
  ;; except for schema upgrading.

  ;; The cset-rel-tuples-index slot is a conceptual hash table into the
  ;; cset-relationship-tuples slot. This is used by
  ;; master-catalog-lookup-cset-relationship
  ;; to avoid an extremely costly linear search of cset-relationship-tuples.
  ;; This is very important for cset_add, cset_remove, and csets_from_file.

  ;; The idea is that the did-string of the master-cid's did is hashed.
  ;; Reducing that hash modulo the number of entries in cset-rel-tuples-index,
  ;; finds a "home" index of cset-rel-tuples-index. Using the sb32 value
  ;; in that element, we either have a -1 (so the entry is not in the
  ;; hash table) or we get an index into cset_relationship_tuples.
  ;; If there is no hash collision, that element of cset_relationship_tuples
  ;; will contain the desired master-cid did we are looking for. If it
  ;; isn't the one we want, we have had a hash collision, and we resolve it
  ;; by linear probing in the next-door (circularly) element of
  ;; cset-rel-tuples-index.
  ;; The number of elements of cset-rel-tuples-index is always a prime number,
  ;; and is also maintained to be more than twice as large as the number of
  ;; entries in cset-relationship-tuples. That is important, to prevent
  ;; clustering and slow searching. So when it grows, cset-rel-tuples-index
  ;; grows by a reasonable factor (about 2) so that it always contains
  ;; at least half "holes", that is, -1.  Further, we want to avoid frequent
  ;; growth, because growing requires computing every entry in the hash table
  ;; again. That makes for a big transaction, as every element of the
  ;; cid-relationship-tuple vector has to be mapped in, and rehashed with
  ;; the new size of cset-rel-tuples-index.
  ;; Space considerations: In Jack's db, there are roughly 40,000 elements
  ;; currently in the cset-relationship-tuples.  Suppose we had 100,000
  ;; elements. In Jack's db, it appears that the tuples are about 2 elements
  ;; each, average. Suppose it were 9. Then the tuples would take 4*(1+9)=40
  ;; bytes each, so 40*100,000 = 4Mb total (plus another 400,000 for the
  ;; cset-relationship-tuples vector itself).  This is large, but not likely
  ;; to be a cause of breakage anytime soon.



  ;; The cset-name-hashtable maps cset names to csets.
  ;; While the HP model of ChangeSafe doesn't allow changing the name of a cset, we allow this
  ;; in general.  So this hash table is keyed by cset name, and valued by all csets which EVER
  ;; bore that name in their versioned name component.  The hash value is therefore a list.
  ;; In the case of system augmented names (by change_create/master_change), there shouldn't
  ;; be any collisions.  We also use this slot to hash unaugmented user names to csets,
  ;; and those are far more likely to have collisions (one key -> multiple csets).  In the case
  ;; of un-augmented names, this is expected. In the case of augmented names, this is an error.
  (cset-name-hashtable :version-technique nil
                       :initial-value (repository-hash-table-create))
  )

(defun master-catalog-create (master-repository)
  "Create a master catalog for a master repository and stash it in a well known root.
   There should be only one per master repository."
  (assert (eq (repository-get-repository-type master-repository) :master))
  (repository-add-locally-named-root master-repository
                                     (make-instance 'master-catalog)
                                     +master-catalog-root-key+))

(defun master-catalog-retrieve (master-repository)
  "Retrieve the master catalog for a master repository"
  (assert-error (repository-get-locally-named-root master-repository +master-catalog-root-key+)))

(defun master-catalog-get-pc-list (master-catalog)
  "Return a list of product configuration (PC) objects under the current metaversioned view of the
   master catalotg."
  (vi-stream-as-list (master-catalog-products master-catalog)))

(defun master-catalog-map-over-pcs (master-catalog function)
  "Apply FUNCTION to each product configuration (PC) in the MASTER-CATALOG."
  (vi-stream-for-each function (master-catalog-products master-catalog)))

(defun master-catalog-get-sorted-pc-list (master-catalog)
  "Return a list of product configuration (PC) objects under the current metaversioned view of the
   master catalotg which is sorted by name."
  (sort (master-catalog-get-pc-list master-catalog) #'string-lessp :key #'pc-name))

(defun master-catalog-get-subsystem-list (master-catalog)
  "Return a list of subsystem objects under the current metaversioned view of the master catalog"
  (vi-stream-as-list (master-catalog-subsystems master-catalog)))

(defun master-catalog-get-sorted-subsystem-list (master-catalog)
  "Return a list of subsystem objects under the current metaversioned view of the
   master catalotg which is sorted by name."
  (sort (master-catalog-get-subsystem-list master-catalog) #'string-lessp :key #'subsystem-name))


(defun master-catalog-add-subsystems (master-catalog subsystems)
  "Add a list of subsystems to the master catalog.  Return nil."
  (when subsystems
    (set-master-catalog-subsystems master-catalog
                                   (append subsystems (master-catalog-get-subsystem-list master-catalog))))
  nil)

(defun master-catalog-add-pc (master-catalog pc)
  "Add a pc to the master catalog.  Return nil."
  (set-master-catalog-products master-catalog
                               (cons pc (master-catalog-get-pc-list master-catalog)))
  nil)

(defun master-catalog-map-over-subsystems (master-catalog function)
  "Apply FUNCTION to each subsystem in the metaversioned view of the master catalog."
  (vi-stream-for-each function (master-catalog-subsystems master-catalog)))

(defgeneric master-catalog-map-over-subsystems-of-class (master-catalog class function)
  (:documentation
   "Apply FUNCTION to each subsystem of the specified class in the metaversioned view
    of the master catalog.  Class can be specified either by a SATELLITE-PROJECT-REF or
    a class name string."))

(defmethod master-catalog-map-over-subsystems-of-class ((master-catalog master-catalog)
                                                        (class-name string) function)
  (vi-stream-for-each (lambda (subsystem)
                          (when (string= class-name (subsystem-class-name subsystem))
                            (funcall function subsystem)))
                      (master-catalog-subsystems master-catalog)))

(defmethod master-catalog-map-over-subsystems-of-class ((master-catalog master-catalog)
                                                        (satellite-project-ref satellite-project-ref)
                                                        function)
  (vi-stream-for-each (lambda (subsystem)
                          (when (eq satellite-project-ref (subsystem-satellite-project-ref subsystem))
                            (funcall function subsystem)))
                      (master-catalog-subsystems master-catalog)))


#||
;; Not currently used, but of a utility nature, that could be useful elsewhere.
(defun master-catalog-mult-mod-2**29 (n m)
  "Multiply 2 non-negative fixnums modulo 2**29,
   without any bignum consing. This takes extra time to compute
   in order to save gc time.
   The value returned is (logand (* n m) #x1fffffff), so it is
   as big as possible with Franz's fixnums.
   NOTE: do not change this function without serious retesting,
   as you could unintentionally require a SCHEMA CHANGE."
  ;; NOTE: if you touch this code, you should retest it.
  (assert (<= 0 n #x1fffffff))
  (assert (<= 0 m #x1fffffff))
  (let*(
        (nl (logand n #x03fff)) ;< 2**14
        (ml (logand m #x03fff)) ;< 2**14
        (nh (logand (ash n -14) #x07fff)) ; < 2**15
        (mh (logand (ash m -14) #x07fff)) ; < 2**15
        (p1 (* nl ml))
        ;; (2**15-1) * (2**14-1) = 2**29 - 2**15 - 2**14 + 1
        ;; so p2 and p3 are non-negative fixnums for Franz.
        (p2 (* mh nl))
        (p3 (* nh ml))
        ;; get that last bit of significance
        ;; p4 is part of the 28th bit...
        (p4 (logand mh nh 1))
        (p1-b (logand p1 #x03fff))
        (p1-a (ash p1 -14))
        (p2-b (logand p2 #x07fff))
        (p3-b (logand p3 #x07fff))
        (p4-h (ash p4 14))
        (suma (+ p2-b p3-b))
        (sumb (+ p1-a p4-h))
        (sum (+ suma sumb))
        (ans-a (logand sum #x07fff))
        (ans-h (ash ans-a 14))
        (ans (+ ans-h p1-b))
        )
    (declare (type fixnum nl ml nh mh p1 p2 p3 p4 p1-b p1-a p2-b p3-b
                   p4-h suma sumb sum ans-a ans-h ans n m))
    ans
    ))
||#

;; Not exported, but of a general utility nature, that could be
;; useful elsewhere.
(defun master-catalog-hash-string (string)
  "A general purpose hashing function on a string,
   which returns 28 bits of hash.

   This is important, since sxhash returns only 16 bits of hash,
   so necessarily gets many collisions when dealing with many
   distinct strings.

   If you need to hash MORE than about 2**27 distinct strings,
   you should use a better hashing function than this.

   Note: changing this function is a SCHEMA CHANGE."
  ;; Don't change this function without serious retesting, as it
  ;; is carefully designed to perform BETTER than a totally random
  ;; function of string to fixnum, for almost all applications.

  ;; Testing reveals that this works well at the 200,000 distinct
  ;; string level. Theory says it should work well up to around
  ;; 2**27 (not 2**28) distinct strings, which is about 134 million
  ;; distinct strings.

  ;; This version conses bignums. A version which does no bignum
  ;; consing, but computes the identical values, was written, but
  ;; the extra code required seemed like more of a maintainance
  ;; headache than the minor savings in bignum consing is a nuisance.

  ;; We do left to right here to take advantage of the strong
  ;; tendency of did-strings to vary in the rightmost characters,
  ;; primarily. Of course, performance safety requires that all
  ;; characters of the string participate in the hash.

  ;; Note: the commented out logand saves a bignum cons (per iteration)
  ;; by the following +.  The use of plain * instead of
  ;; master-catalog-mult-mod-2**29 conses a bignum (per iteration)
  ;; but the general product consing is so prevalent that it was
  ;; deemed not worth keeping consing-free.

  ;; Note: The 'magic' numbers do have some theory behind them: they
  ;; constitute a full-period random number generator, when the adding
  ;; in of the char-codes is removed, of high potency. See Knuth.
  ;; Consequently, there are good theoretical reasons to believe that
  ;; earlier characters in the string are not "forgotten" by the
  ;; hashing of the later characters, which would of course lead to
  ;; more collisons in the hash table, if strings that vary only in
  ;; an early character were hashed.
  (do*(
       (len (length string))
       (ans len (logand (+ ;(logand
                            (* ;master-catalog-mult-mod-2**29
                             (+ ans (char-code (char string i)))
                             #x0a8b3695)
                            ;#x0fffffff)
                           #x0b37297)
                        #x0fffffff))
       (i 0 (1+ i))
       )
      ((= i len) ans)
    ))

(defun master-catalog-is-primep (n)
  "If a given integer N, is a prime number, return true,
   else return nil."
  (check-type n integer)
  (cond
   ;; kill negative numbers
   ((< n 2) nil)
   ;; handle easy cases
   ((evenp n) (= n 2))
   ((zerop (mod n 3)) (= n 3))
   ((zerop (mod n 5)) (= n 5))
   ((zerop (mod n 7)) (= n 7))
   ;; ok, its not obvious, prepare to do it the hard way
   (t (let*(
            (iroot (isqrt n))
            (i 11))
        (loop
         (when (> i iroot) (return t))
         (when (zerop (mod n i)) (return nil))
         (incf i 2)
         (when (zerop (mod n i)) (return nil))
         ;; skip multiples of 3
         (incf i 4))))))



(defun master-catalog-next-prime-after (n)
  "Given a integer N, this returns the smallest
   prime number which is greater than N."
  (check-type n integer)
  (if (< n 2) 2
    (progn
      ;; consider only numbers BIGGER than n.
      (incf n)
      ;; No need to try the even numbers
      (if (evenp n) (incf n))
      (loop
       ;; n is odd, and bigger than the original n
       (when (master-catalog-is-primep n)
         (return n))
       ;; bump to next odd number
       (incf n 2)))))


(defun master-catalog-update-cset-rel-tuples-index (master-catalog did index)
  "Given a MASTER-CATALOG, and a DISTRIBUTED-IDENTIFIER
   (for a master repositor change-set), and the INDEX of where
  in the master-catalog-cset-relationship-tuples vector the
  tuple for that change-set is stored,
  update the cset-rel-tuples-index to map to that given INDEX.
  IF necessary, this may have to grow the entire index, but that
  is rare."

  ;; NOTE: this is ONLY called from vm-txn-note-change-set,
  ;; and (during schema upgrade) from master-catalog-schema-upgrade-to-9-19

  ;; NOTE: the hash-prime WILL be 0, when the schema is either brand-new, or
  ;; the schema has not been upgraded yet. This code should work in that case,
  ;; even if index is 0. By WORK, I mean that it will not attempt to handle
  ;; that situation in the "easy" way, but will properly set the capacity
  ;; and contents of the entire cset-rel-tuples-index.
  ;; Consequently, schema upgrade can invoke this code with index of 0,
  ;; if the schema has any entries in cset-relationship-tuples at all.
  ;; If not, the schema need not be diddled during schema upgrade, but
  ;; when this is called in a normal way, because vm-txn-note-change-set
  ;; decided to call it, the easy case will not be done, and it will get
  ;; completely regularly initialized.

  ;; Ensure we don't get a negative index to store, since we use -1 as a flag.
  (assert (<= 0 index))
  (let*(
        (hash-table (master-catalog-cset-rel-tuples-index master-catalog))
        (hash-prime (astore-vector-entries hash-table))
        )
    ;; see if the hash table is not too full...
    ;; (note that if hash-prime is 0, this test will still work,
    ;;  and force us into the hard case)
    (if (< (+ index index) hash-prime)
        ;; easy, typical case. Store new entry and we are done.
        ;; doesn't normally go around this loop much (if any)
        (do* (
              (ht-aref  (astore-vector-get-aref-function hash-table))
              (did-string (did->string-did did))
              (home (mod (master-catalog-hash-string did-string) hash-prime)
                    (1- (if (zerop home) hash-prime home)))
              )
            ((minusp (funcall ht-aref hash-table home))
             (setf (astore-vector-aref hash-table home) index)))


      ;; else unusual case, rarely happens - table too full, so grow it...
      ;; NOTE: we need not add the "new" element, as it is already in
      ;; the cset-relationship-tuple slot (checked with an assert below)
      (do*(
           (tuples (master-catalog-cset-relationship-tuples master-catalog))
           (tuple-aref (astore-vector-get-aref-function tuples))
           ;; we run cur-i backwards, so that recent csets are a LITTLE faster
           ;; to find than old ones.
           (cur-i (1- (astore-vector-entries tuples)) (1- cur-i))
           (ht-aref (progn
                      ;; double-check to be sure the new guy is already
                      ;; where we assume him to be
                      (assert (<= index cur-i))

                      ;; recalulate the hash-prime
                      ;; the 4 is a tunable parameter, that ensures
                      ;; we don't get here very often.
                      ;; If the table is only half-loaded max, then
                      ;; the 4 means that we are quarter-loaded min,
                      ;; so we will have to double the entries before
                      ;; we have to grow the hash table again.

                      ;; The max prevents the capacity from being too small,
                      ;; initially. There is no good reason for the 5000,
                      ;; it just is a reasonably large starting value.
                      (setq hash-prime
                            (master-catalog-next-prime-after
                             (max (* cur-i 4) 5000)))

                      ;; check to see we aren't going to overload the
                      ;; hash table (check here, so the message has
                      ;; a better chance of being easily understood)
                      (assert (< hash-prime array-dimension-limit))

                      ;; set the entries to 0 to avoid needless copying
                      ;; when the capacity is set.
                      (setf (astore-vector-entries hash-table) 0)
                      ;; grow the hash-table's capacity, and set all
                      ;; the elements to -1.
                      (astore-vector-set-capacity hash-table hash-prime -1)
                      ;; set the entries to "all in use", with new size
                      (setf (astore-vector-entries hash-table) hash-prime)
                      ;; finally, get the aref-function
                      (astore-vector-get-aref-function hash-table)))
           (ht-set-aref (astore-vector-get-update-function hash-table)))
          ;; Now put this index, namely i, into the table
          ;; Note this next loop should usually run 0 or only a few times,
          ;; per invocation by the outer loop.
          ((do*(
                (cur-tuple (funcall tuple-aref tuples cur-i))
                (did (svref cur-tuple 0))
                (did-string (did->string-did did))
                (home (mod (master-catalog-hash-string did-string) hash-prime)
                      (1- (if (zerop home) hash-prime home)))
                )
               ;; look for a -1 element
               ((minusp (funcall ht-aref hash-table home))
                ;; found a -1 element, use it
                (funcall ht-set-aref hash-table home cur-i)
                ;; this is the end predicate for the outer loop
                (zerop cur-i))))
        ;; no body for the outer loop, as work is done in the condition...
        ))
    ;; final return value
    nil))

(defun master-catalog-schema-upgrade-to-9-19 (master-catalog)
  "Upgrade the master-catalog for schema 9-19,
   which added the cset-rel-tuples-index slot to master-catalogs,
   if we need to.
   Returns true when an upgrade was done, otherwise nil."

  (let*((tuples (master-catalog-cset-relationship-tuples master-catalog)))
    ;; when there are zero tuples, we can't, and don't need to upgrade.
    ;; when there are non-zero entries in the cset-rel-tuples-index, we
    ;; have already upgraded.
    ;; Otherwise, we can cause an upgrade by inserting the one tuple we
    ;; know we have, and that will insert them all.
    (if (or (zerop (astore-vector-entries tuples))
            (plusp (astore-vector-entries
                    (master-catalog-cset-rel-tuples-index master-catalog))))

        ;; either no tuples, so no upgrade needed,
        ;; or we have already upgraded, so no upgrade needed.
        nil

      ;; else, we need to upgrade, and can do so now.
      ;; Invoking with the 0th entry in the tuples vector is sufficient
      ;; to cause the empty cset-rel-tuples-index to be rebuilt from scratch,
      ;; in the usual way when it is too small, and that rebuild will put all
      ;; the tuples into the rebuilt index.
      (let ((did (svref (astore-vector-aref tuples 0) 0)))
        (master-catalog-update-cset-rel-tuples-index
         master-catalog did 0)
        t)
      )))


(defmethod vm-txn-note-change-set ((master-catalog master-catalog) (change-set change-set))
  "Package up all information information gathered in creation of a super-cset,
   in particular satellite-csets, into the master catalog cset-relationship-tuples.

   We also process promotions/demotions of csets into subsystem mirrors of satellite branches
   at this time.

   CHANGE-SET may be either the master change-set, or a change-set created in a satellite
   repository.  We know that the master change-set call to this method occurs after all satellite
   calls.  If we receive the master change-set, we package up the final result and place it in the
   persistent master catalog."

  (let* ((did (distributed-object-identifier change-set))
         (satellite-ref-count (length *cm-txn-change-sets*))
         (had-satellites-p (plusp satellite-ref-count))
         (master-cset-p (objects-in-same-repository? master-catalog change-set)))
    ;; Strictly speaking, this might be the master too, so the variable isn't completely well named.
    (push did *cm-txn-change-sets*)
    ;; If the change-set is the same database as the master, this is our final master change-set
    ;; update the master catalog.
    (when (and master-cset-p had-satellites-p)
      (let* ((astore-vector (master-catalog-cset-relationship-tuples master-catalog))
             (value-vector (and had-satellites-p
                                (make-array (1+ satellite-ref-count)
                                            :initial-contents *cm-txn-change-sets*)))
             (index
              ;; Push the vector onto the persistent table
              (astore-vector-push-extend astore-vector value-vector nil)))
        ;; Put the new element into the hash table.
        (master-catalog-update-cset-rel-tuples-index
         master-catalog did index)
        ))
    (when master-cset-p
      ;; Associate the master cset with it's name in the master-catalog-cset-name-hashtable.
      ;; We can't do this from change-set-create, since that method doesn't understand our
      ;; down-wind package use of it.
      (unless (master-catalog-note-change-set-name master-catalog change-set)
        (warn "A change-set with name ~s already exists in the master repository"
              (change-set-name change-set)))
      ;; Run any hooks which want to use this final bit of information regarding the created
      ;; change-sets.
      (dolist (hook *cm-txn-master-cset-close-hooks*)
        (funcall hook master-catalog change-set))
      ;; Note that *cm-txn-change-sets* and *cm-txn-master-cset-close-hooks* special variables
      ;; are lexically bound/cleared by WITH-CM-MASTER-TXN
      (when *within-regression-tests*
        (let ((msg-str
               (format
                nil
                ;; Control string should be prepared for 2 args
                (if had-satellites-p
                    "Cset relationship ~s created, reason: ~a"
                  "Master transaction ~s created, reason: ~a")
                *cm-txn-change-sets*
                ;; Ignore unbound-slot error, since we may not have appropriate cid-set here to view
                ;; description.
                (let ((reason
                       (with-unbound-slot-handler ()
                         (described-object-text change-set))))
                  ;; Remove user information since it won't diff in regressions when run by different people
                  (when reason
                    (let* ((tokens (split-string reason))
                           (ntokens (length tokens))
                           (pos (position "user" tokens :test #'string-equal)))
                      (when pos
                        ;; Could consider simply changing the user to "deleted"
                        (setq tokens (append (butlast tokens (- ntokens pos))
                                             (last tokens (- ntokens (+ pos 2)))))
                        (setq reason (join-string-tokens tokens)))))
                  reason))))
          ;; put the string both to debug output and the mst file
          (debug-message 1 msg-str)
          (format t "~%~a" msg-str)))
      ))
  ;; Done.
  (values))                             ;return value ignored

(defun master-catalog-note-change-set-name (master-catalog change-set &optional cset-name)
  "A new master change-set has been created (and implicitly named), or it is being renamed.
   Hash this information for quick cset name resolution *** UNLESS *** there is no name.
   We don't bother to hash NIL-named csets, of which there are many, and which we never
   expect to (indeed can't) resolve by name.

   If the cset-name isn't specified, we use whatever versioned value for the change-set is
   in context.  We assume, for current purposes, that this is the fully qualified (system augmented)
   change-set name provided by the HP ChangeSafe model.

   We also log the unqualified portion of the cset so that we can hash on either the qualified
   or the unqualified name, though the unqualified name is very likely to resolve to multiple
   cset references.

   Return nil if the (fully qualified) change-set has already been noted with the indicated name previously
   (a likely programming error), and true if it has not.
   If the cset name is nil, we return true."
  (unless cset-name
    (setq cset-name (change-set-name change-set)))
  (if cset-name
      (let* ((hashtable (master-catalog-cset-name-hashtable master-catalog))
             (previous-csets-for-qualified-name (repository-hash-table-gethash hashtable cset-name))
             (result t))
        (if (find change-set previous-csets-for-qualified-name)
            (setq result nil)
          (let* ((user-change-name (conman-disassemble-HP-cset-name cset-name))
                 (previous-csets-for-unqualified-name
                  (repository-hash-table-gethash hashtable user-change-name)))
            ;; Update the qualified name entry in the hashtable
            (push change-set previous-csets-for-qualified-name)
            (repository-hash-table-puthash hashtable cset-name previous-csets-for-qualified-name)
            ;; Update the unqualified name entry in the hashtable
            (push change-set previous-csets-for-unqualified-name)
            (repository-hash-table-puthash hashtable user-change-name previous-csets-for-unqualified-name)))
        result)
    t))

(defun master-catalog-queue-cset-hook (master-catalog hook-function)
  "Queue HOOK-FUNCTION for execution as a postlude to the transaction on the master repository
   when all cset-dispositions are known and winding down.  HOOK-FUNCTION must take two arguments,
   a master-catalog and a change-set, which will be the CHANGE-SET object created in the master-repository.

   NOTE: because of the current structure of WITH-VM-TXN, you should not call TXN-CONTEXT-ABORT-IF-NO-CHANGE
   from within hooks.

   Return the master-catalog."
  ;; This function is logically an operation on the master-catalog, but since we don't
  ;; have a transient slot on the master-catalog in which to store hooks and use a special variable,
  ;; we require the master-catalog parameter largely for the logical sense of the operation,
  ;; not the physical.
  (push hook-function *cm-txn-master-cset-close-hooks*)
  master-catalog)

(defun master-catalog-lookup-cset-relationship (master-catalog master-change-set-ref)
  "Return a vector from the cset relationship tuples keyed by the master-change-set reference.
   If there is no relationship tuple, return NIL, which means that the master change-set did not
   have any corresponding satellite change-sets.

   If a vector is returned, the first element of the tuple will be the did of the
   master-change-set-ref, and any other values will be dids of satellite cset refs.

   **** THIS ROUTINE IS CURRENTLY INSENSITIVE TO META-VERSION CONTEXT.  ****
   This is probably good.

   MASTER-CHANGE-SET-REF may be either a change-set object residing in the master repository,
   or a DID of such a change-set.  It may NOT be a change-set name.  If you have a cset-name you wish
   to resolve to a cset, use MASTER-CATALOG-RESOLVE-CSET-NAME."

  ;; check-type below not needed, as etypecase does the job just fine.
  ;;(check-type master-change-set-ref (or change-set distributed-identifier))
  (let*((did (etypecase master-change-set-ref
               (change-set (distributed-object-identifier master-change-set-ref))
               (distributed-identifier master-change-set-ref)))
        (astore-vector (master-catalog-cset-relationship-tuples master-catalog))
        (did-string (did->string-did did))
        (did-string-hash (master-catalog-hash-string did-string))
        (hash-table (master-catalog-cset-rel-tuples-index master-catalog))
        (hash-prime (astore-vector-entries hash-table))
        (home (mod did-string-hash hash-prime))
        index
        value-vector)
    (loop
     ;; This loop is guaranteed to terminate, because there are always
     ;; negative elements (-1) in the hash-table.
     ;; For performance, at least half the elements of the hash-table
     ;; should be -1.
     (setq index (astore-vector-aref hash-table home))
     (when (minusp index)
       ;; Happens only if there is no relationship tuple for the
       ;; given master-change-set-ref, meaning there were no
       ;; corresponding satellite change sets.
       (return nil))
     (setq value-vector (astore-vector-aref astore-vector index))
     (when (eq did (svref value-vector 0))
       (return value-vector))
     ;; should reach this point an average of less than 1/2 times per
     ;; invocation of master-catalog-lookup-cset-relationship.
     (setq home (1- (if (zerop home) hash-prime home)))
     )))

(defun master-catalog-list-satellite-cset-dids (master-catalog change-set)
  "Retrieve a list of satellite csets for a master repository change-set.
   Change-set must be a CHANGE-SET object which must exist in the tuple list,
   note that it will not exist in the tuple list prior to the transaction close which creates CHANGE-SET.

   Return nil if there are no satellite csets, otherwise a list of satellite cset DIDs.

   **** THIS ROUTINE IS CURRENTLY INSENSITIVE TO META-VERSION CONTEXT.  ****
   This is probably good.

   This is a nicer encapsulation of the cset-relationship-tuples management than
   using the raw vector/tuple, however it does cons a bit.  If you have to call it a lot,
   we should use the raw vector interface.  *PERFORMANCE*(GC)"
  (let ((cset-tuple (master-catalog-lookup-cset-relationship master-catalog change-set)))
    (and cset-tuple (loop for x from 1 below (length cset-tuple)
                        collect (svref cset-tuple x)))))

(defun master-catalog-find-master-cset-did-for-satellite-cset (master-catalog satellite-cset-did)
  "Given a satellite CHANGE-SET DID, return the master CHANGE-SET did that hierarchically
   owns that satellite cset and was reponsible for it's creation.  Given the master cset, you can
   get information such as the cset name and creation time of the satellite cset.

   A satellite cset has only ONE owning master cset. It may be manipulated by multiple transactions,
   but the relationship tuple records birth-rights information, not promotion/demotion information.

   *PERFORMANCE*: this is an exceedingly expensive function.  Use with care.
   If you're doing this alot, consider using
   MASTER-CATALOG-INDEX-CSET-RELATIONSHIPS-BY-CSET to build a lookup table."
  (let ((satellite-cset-index (master-catalog-index-cset-relationships-by-cset-cached master-catalog)))
    (if satellite-cset-index  ;; use cache if it is available
        (let ((master-cset-did-vector (gethash satellite-cset-did satellite-cset-index nil)))
          (when master-cset-did-vector
            (return-from master-catalog-find-master-cset-did-for-satellite-cset
              (svref master-cset-did-vector 0))))
      ;; cache is not available, use old slow method
      (loop with astore-vector = (master-catalog-cset-relationship-tuples master-catalog)
            for x from 0 below (astore-vector-entries astore-vector)
            as value-vector = (astore-vector-aref astore-vector x)
            do
            (loop for i from 1 below (length value-vector)
                  for satellite-cset-did-1 = (svref value-vector i)
                  do
                  (when (eq satellite-cset-did satellite-cset-did-1)
                    (return-from master-catalog-find-master-cset-did-for-satellite-cset
                      (svref value-vector 0)))))))
  ;; If we didn't find it, chances are some programmer is very confused.  We shouldn't be asking
  ;; for satellite-cset-dids which aren't in the master repository, at least not until we develop a
  ;; distributed model of components csets built on ChangeSafe.
  (error "Unable to find the master cset corresponding to satellite-cset-did ~s" satellite-cset-did))

(defvar *master-catalog-server-cset-relationship-cache* (make-hash-table :test 'eql)
  "This hash table exists for the life of the server.
   It maps the DIDs of MASTER-CATALOG objectss to a two element list.
   The first element is an integer index indicating the next
   entry of the CSET-RELATIONSHIP-TUPLES of MASTER-CATALOG
   which is to be processed into the cached reverse index.
   The second element of the list the cached reverse index
   implemented as a hash table which maps satellite cset DIDs
   to master cset DIDs.")

(defun master-catalog-index-cset-relationships-by-cset (master-catalog)
  "There are times when we may need to use the CSET-RELATIONSHIP-TUPLES of the
   MASTER-CATALOG to determine the master change set of which a given satellite cset
   is a part.  This routine builds an index for that purpose.
   The value is a hash table which maps a satellite cset DID to the tuple
   containing it from the CSET-RELATIONSHIP-TUPLES of MASTER-CATALOG.
   Since this function accesses the CSET-RELATIONSHIP-TUPLES slot of MASTER-CATALOG
   it should be called within a transaction on the master repository and in
   the tip master metaversion."
  ;;; ****WARNING**** We can only get away with this cacheing because
  ;;; the MASTER-CATALOG-CSET-RELATIONSHIP-TUPLES only grows, the
  ;;; contents doesn't change otherwise.
  (let ((entry (gethash (distributed-object-identifier master-catalog)
                        *master-catalog-server-cset-relationship-cache*))
        (astore-vector (master-catalog-cset-relationship-tuples master-catalog)))
    (unless entry
      (setq entry (list 0 (make-hash-table :test 'eql)))
      (setf (gethash (distributed-object-identifier master-catalog)
                     *master-catalog-server-cset-relationship-cache*)
        entry))
    (destructuring-bind (last-index index-table) entry
      (loop for x from last-index below (astore-vector-entries astore-vector)
          as value-vector = (astore-vector-aref astore-vector x)
          for master-did = (svref value-vector 0)
          do
            (loop for i from 1 below (length value-vector)
                for satellite-cset-ref-did = (svref value-vector i)
                do
                  (let ((already-there (gethash satellite-cset-ref-did index-table)))
                    (when already-there
                      (unless (eq already-there master-did)
                        (error "~s belongs to both ~s and ~s"
                               satellite-cset-ref-did
                               master-did
                               already-there))))
                  (setf (gethash satellite-cset-ref-did index-table) value-vector))
          finally (setf (car entry)
                    (astore-vector-entries astore-vector)))
      index-table)))

#||
(defun master-catalog-index-cset-relationships-by-cset (master-catalog)
  "There are times when we may need to use the CSET-RELATIONSHIP-TUPLES of the
   MASTER-CATALOG to determine the master change set of which a given satellite cset
   is a part.  This routine builds an index for that purpose.
   The value is a hash table which maps a satellite cset DID to the tuple
   containing it from the CSET-RELATIONSHIP-TUPLES of MASTER-CATALOG.
   Since this function accesses the CSET-RELATIONSHIP-TUPLES slot of MASTER-CATALOG
   it should be called within a transaction on the master repository and in
   the tip master metaversion."
  (let ((index (make-hash-table :test 'eql))
        (astore-vector (master-catalog-cset-relationship-tuples master-catalog)))
    (loop for x from 0 below (astore-vector-entries astore-vector)
        as value-vector = (astore-vector-aref astore-vector x)
        for master-did = (svref value-vector 0)
        do
          (loop for i from 1 below (length value-vector)
              for satellite-cset-ref-did = (svref value-vector i)
              do
                (when (gethash satellite-cset-ref-did index)
                  (error "~s belongs to both ~s and ~s"
                         satellite-cset-ref-did
                         master-did
                         (gethash satellite-cset-ref-did index)))
                (setf (gethash satellite-cset-ref-did index) value-vector)))
    index))
||#

(defvar-unbound *cached-master-catalog-cset-relationship-reverse-index*
    "Sometimes we need the reverse index generated by
     MASTER-CATALOG-INDEX-CSET-RELATIONSHIPS-BY-CSET in several different
     functions during the same user transaction.  If this is bound, we cache
     the index here to share its use.")

(defun master-catalog-index-cset-relationships-by-cset-cached (master-catalog)
  "Returns a cset relationship reverse index, as would be generated by
   MASTER-CATALOG-INDEX-CSET-RELATIONSHIPS-BY-CSET, but ensures that the
   index is only generated once within the context of
   WITH-MASTER-CATALOG-CSET-RELATIONSHIP-REVERSE-INDEX-CACHING."
  (if (boundp '*cached-master-catalog-cset-relationship-reverse-index*)
      (if *cached-master-catalog-cset-relationship-reverse-index*
          *cached-master-catalog-cset-relationship-reverse-index*
        (setq *cached-master-catalog-cset-relationship-reverse-index*
              (master-catalog-index-cset-relationships-by-cset master-catalog)))
    ;; If caching isn't emabled, just compute it for them
    (master-catalog-index-cset-relationships-by-cset master-catalog)))

(defmacro with-master-catalog-cset-relationship-reverse-index-caching (&body body)
  "Executes BODY in an environment where the cset relationship reverse index
   as generated by MASTER-CATALOG-INDEX-CSET-RELATIONSHIPS-BY-CSET is cached
   for use throughout the transaction (within the dynamic scope of this macro)."
  `(let ((*cached-master-catalog-cset-relationship-reverse-index* nil))
     ;; If *CACHED-MASTER-CATALOG-CSET-RELATIONSHIP-REVERSE-INDEX* is
     ;; bound, then
     ;; MASTER-CATALOG-INDEX-CSET-RELATIONSHIPS-BY-CSET-CACHED will
     ;; either use the index found there or build the index if needed.
     ,@body))

(defun master-catalog-resolve-change-set-name (master-catalog cset-name)
  "Resolve cset-name.  If it can't be found at all, signal an error.
   If there are multiple csets bearing the same name, signal an error.
   Otherwise return the master change-set object which matches the name.

   This function is a convenient shortcut for many applications of
   master-catalog-resolve-qualified-change-set-name and master-catalog-resolve-unqualified-change-set-name."
  (or (master-catalog-resolve-qualified-change-set-name
       master-catalog cset-name :error-if-missing nil)
      (master-catalog-resolve-unqualified-change-set-name
       master-catalog cset-name :error-if-missing t :error-if-multiple t)))

(defun master-catalog-resolve-qualified-change-set-name (master-catalog cset-name &key (error-if-missing t))
  "Use CSET-NAME, which should be a system-augmented fully qualified change-set name,
   to do a by-name lookup of a change-set in the master catalog.
   Cset names are versioned in the VM model, but don't change in the CONMAN model, though
   we might like to let them change in other product realizations of the CONMAN package.

   The HP model of ChangeSafe doesn't allow change-set names to change.
   Therefore, if we encounter multiple csets with the same name, we signal an error.
   If we want to allow it, we'll have to resolve which cset with the indicated name is
   the one we want.

   Return a master-repository change-set if we find one.  If we don't find one and
   ERROR-IF-MISSING is true, we signal an error, otherwise we return NIL."
  (let ((change-set-list (repository-hash-table-gethash (master-catalog-cset-name-hashtable master-catalog)
                                                        cset-name)))
    (when (> (length change-set-list) 1)
      ;; This will be an 'unclassified' error, it indicates a programming error, since qualified
      ;; cset names are supposed to be unique.
      ;; BTW, it is the HP ChangeSafe model below but we can't say that in an error msg
      (error "The ChangeSafe model doesn't allow multiple change-sets with the same name, and ~
              somehow we've got multiple csets with the name ~s" cset-name))
    (when (and (null change-set-list) error-if-missing)
      (conman-signal-error *cm-returns-error-change-set-name-does-not-exist*
                           "A change-set named ~s does not exist." cset-name))
    (car change-set-list)))             ;return the change-set, or nil if we didn't find one.

(defun master-catalog-resolve-unqualified-change-set-name (master-catalog cset-name
                                                           &key (error-if-missing t)
                                                                (error-if-multiple t))
  "Use CSET-NAME, which is an unaugmented 'raw' user cset-name, to do a by-name lookup
   of a change-set in the master catalog.

   In the case of unqualified cset names there may be multiple csets with the unqualified
   name.

   Similar caveats on versioned cset name changes apply here as in
   MASTER-CATALOG-RESOLVE-QUALIFIED-CHANGE-SET-NAME.

   Return a master-repository change-set if we find a single match for CSET-NAME.
   If we don't find any matches and ERROR-IF-MISSING is true, we signal an error.
   If we find more than one match, and ERROR-IF-MULTIPLE is true, we signal an error.
   Otherwise we return the whole list of csets which were matched."
  (let* ((change-set-list (repository-hash-table-gethash (master-catalog-cset-name-hashtable master-catalog)
                                                         cset-name))
         (n-csets (length change-set-list)))
    (when (and (> n-csets 1) error-if-multiple)
      ;; Unlike MASTER-CATALOG-RESOLVE-QUALIFIED-CHANGE-SET-NAME, this is a model error, not
      ;; a programming error.
      (conman-signal-error
       *cm-returns-error-ambiguous-change-name*
       "The name ~s matches multiple ~ss in the repository, please specify a fully qualified ~a-name."
       cset-name *conman-change-name* *conman-change-name*))
    (when (and (null change-set-list) error-if-missing)
      (conman-signal-error *cm-returns-error-change-set-name-does-not-exist*
                           "A change-set named ~s does not exist." cset-name))
    (if error-if-multiple
        (car change-set-list)           ;return the single cset, they wanted just one
      change-set-list)))                ;return the list, they din't care if there were multiples or none

(defun master-catalog-pc-name-lookup (master-catalog cm-session-context-or-pc-name
                                      &key error-if-missing search-all-names
                                           (two-phase-search t)
                                           (pc-list (master-catalog-get-pc-list master-catalog)))

  "Return a product-configuration object based on the context identified by CM-SESSION-CONTEXT.

   If no product configuration is located matching the named pc in the session context,
   an error is signalled if ERROR-IF-MISSING is T, otherwise the result values are nil.

   Name resolution is currently case sensitive.  And by default, this routine is sensitive
   to, and should be viewed in, the master metaversion context.

   If SEARCH-ALL-NAMES is true, then the metaversion context is ignored and the first product
   encountered which EVER had the indicated name is returned.

   If TWO-PHASE-SEARCH is true (the default), we search 'current' meta-version dictated names for products,
   then initiate a search as if SEARCH-ALL-NAMES had been specified.  This speeds the search since
   searching all names can be expensive.

   PC-LIST, if specified, is both an efficiency mechanism to avoid recalculating the versioned pc list,
   or to prune it down if so desired."
  (let* ((pc-name (or (and (stringp cm-session-context-or-pc-name)
                           cm-session-context-or-pc-name)
                      (cm-session-context-pc-name cm-session-context-or-pc-name)))
         (result
          (loop for pc in pc-list
                when (or (string= (pc-name pc) pc-name)
                         (and search-all-names (named-object-search-all-names pc pc-name :test #'string=)))
                collect pc)))
    (when (> (length result) 1)
      (error "Ambiguous product name for ~a, shouldn't be possible." pc-name))
    (if (null result)
        (if (and two-phase-search (not search-all-names))
            ;; Search all names before giving up
            (master-catalog-pc-name-lookup master-catalog cm-session-context-or-pc-name
                                           :error-if-missing error-if-missing
                                           :search-all-names t
                                           :two-phase-search nil
                                           :pc-list pc-list)
          (when error-if-missing
            (conman-signal-error
             *cm-returns-error-product-not-found*
             (if search-all-names
                 "No product (ever) named ~s exists"
               "No product currently named ~s exists.")
             pc-name)))
      (car result))))

(defun master-catalog-subsystem-name-lookup
    (master-catalog cm-session-context-or-subsystem-name
     &key error-if-missing search-all-names
          (two-phase-search t)
          (subsystem-list (master-catalog-get-subsystem-list master-catalog)))
  "Return a subsystem object based on the context identified by CM-SESSION-CONTEXT.

   If no subsystem is located matching the named pc in the session context,
   an error is signalled if ERROR-IF-MISSING is T, otherwise the result values are nil.

   Name resolution is currently case sensitive.  And by default, this routine is sensitive
   to, and should be viewed in, the master metaversion context.

   If SEARCH-ALL-NAMES is true, then the metaversion context is ignored and the first product
   encountered which EVER had the indicated name is returned.

   If TWO-PHASE-SEARCH is true (the default), we search 'current' meta-version dictated names for subsystems,
   then initiate a search as if SEARCH-ALL-NAMES had been specified.  This speeds the search since
   searching all names can be expensive.

   SUBSYSTEM-LIST, if specified, is both an efficiency mechanism to avoid recalculating the
   versioned subsystem list, or to prune the search space if so desired."
  (let* ((subsystem-name (or (and (stringp cm-session-context-or-subsystem-name)
                                  cm-session-context-or-subsystem-name)
                             (cm-session-context-subsystem-name cm-session-context-or-subsystem-name)))
         (result
          (loop for subsystem in subsystem-list
                when (or (string= (subsystem-name subsystem) subsystem-name)
                         (and search-all-names (named-object-search-all-names
                                                subsystem subsystem-name :test #'string=)))
                collect subsystem)))
    (when (> (length result) 1)
      (error "Ambiguous subsystem name for ~a, shouldn't be possible." subsystem-name))
    (if (null result)
        (if (and two-phase-search (not search-all-names))
            ;; Search all names before giving up
            (master-catalog-subsystem-name-lookup master-catalog cm-session-context-or-subsystem-name
                                           :error-if-missing error-if-missing
                                           :search-all-names t
                                           :two-phase-search nil
                                           :subsystem-list subsystem-list)
          (when error-if-missing
            (conman-signal-error
             *cm-returns-error-subsystem-not-found*
             (if search-all-names
                 "No subsystem (ever) named ~s exists"
               "No subsystem currently named ~s exists.")
             subsystem-name)))
      (car result))))

(defun master-catalog-get-satellite-project-ref-list (master-catalog)
  (vi-stream-as-list (master-catalog-satellite-project-refs master-catalog)))

(defun master-catalog-add-satellite-project-ref (master-catalog satellite-project-ref)
  "Add a satellite-project-ref to the master catalog.  Return the satellite-project-ref object.
   Not exported, used only by master-catalog-create-satellite-project-ref"
  (let ((refs (master-catalog-get-satellite-project-ref-list master-catalog)))
    (push satellite-project-ref refs)
    (set-master-catalog-satellite-project-refs master-catalog refs)
    satellite-project-ref))

(defun master-catalog-lookup-satellite-project-ref
       (master-catalog project-name &optional satellite-project-ref-list-with-names)
  "Look up PROJECT-NAME in the master catalog satellite-project-ref (a.k.a. ChangeSafe 'class') list
   under the current cid-set in scope.  Return a SATELLITE-PROJECT-REF object if we find one matching
   project-name, or NIL if we don't."
  ;; This is expensive, especially when called from within nested report loops, e.g. product history
  ;;(debug-message 4 "Project refs ~s"
  ;;             (mapcar #'satellite-project-ref-name (master-catalog-get-satellite-project-ref-list master-catalog)))
  (if satellite-project-ref-list-with-names
      (dolist (name-proj satellite-project-ref-list-with-names)
        (when (string= (car name-proj) project-name)
          (return (cdr name-proj))))
    (find project-name (master-catalog-get-satellite-project-ref-list master-catalog)
          :key #'satellite-project-ref-name :test #'string=)))

(defun master-catalog-create-satellite-project-ref (master-catalog master-repository master-repository-name
                                                    satellite-project-ref-name subdirectory
                                                    description)
  "Create a satellite repository, and in that repository create a PROJECT named SATELLITE-PROJECT-REF-NAME.
   Create a SATELLITE-PROJECT-REF object in MASTER-REPOSITORY which references the new project, and link it
   into MASTER-CATALOG.

   SUBDIRECTORY names the root directory of the satellite project, which is used
   in forming subdirectories of the master configurations.

   DESCRIPTION is the user's description for this class/project.

   The satellite repository shares the master repository's name as a prefix, and will be created
   in the same directory as the master repository.  This information is derived from MASTER-REPOSITORY-NAME,
   which was used to open the master repository in the first place, and which is minimally a file name,
   and maximally a full path string.

   A change-transaction is assumed active on MASTER-REPOSITORY.

   We signal an error if a satellite-project-ref exists which currently bears the indicated
   satellite-project-ref name, otherwise we return the satellite-project-ref which is created.
   (*TBD*: options here, see code comments).

   We don't bother to name satellite repositories after projects, since projects can be renamed,
   but repositories can't. (Well, they could, but their internal knowledge of their new name
   can't change since it's used in distributed identifiers.  We don't support repository renaming yet.)"

  ;; *TO-DO*: pass a DESCRIPTION for the satellite repository and project

  ;; Verify that the class doesn't exist already.
  ;; Note that we could potentially search the versioned space for class name matches
  ;; of renamed classes, and add a -force or -reuse (make a new one, reuse an existing one)
  ;; switch to control behavior in this case.
  (when (master-catalog-lookup-satellite-project-ref master-catalog satellite-project-ref-name)
    (error "A class named ~s is already active in the current master configuration."
           satellite-project-ref-name))

  ;; Here we create a satellite repository in a nested change transaction within the master change
  ;; transaction.
  (let* ((satellite-repository-name
          ;; Wouldn't it be easier to tell which satelite were which
          ;; if we named them after their associated subsystem.
          (format nil "~a-satellite-~d"
                  (repository-name master-repository)
                  (1+ (length (repository-get-satellite-repository-pathstrings master-repository)))))
         (satellite-repository-path (dbpath/merge (make-pathname :name satellite-repository-name
                                                                  :type *repository-satellite-file-type*
                                                                  :defaults nil)
                                                   master-repository-name))
         ;; Generally don't store the full path of satellites, resolve just the name/type/version
         ;; components relative to master.
         (satellite-pathstring (file-namestring (db-name-pathname satellite-repository-path)))
         (created-project-did nil))

    ;; Chain satellite into master
    (repository-add-satellite master-repository satellite-pathstring)

    ;; Create the class/satellite-project-ref in the satellite
    ;; IMPLEMENTATION NOTE: this is a nested change-set transaction!

    ;; Note that with-cm-satellite-repository performs an implicit with-current-repository
    ;; creating, so we use with-open-repository
    (with-open-repository (satellite-repository satellite-repository-path
                           :update
                           :parent-database-pathstring satellite-pathstring
                           :repository-type :satellite
                           :if-exists :error
                           :if-does-not-exist :create)
      ;; Now create the project in the satellite repository
      (with-cm-satellite-txn (satellite-repository
                              master-catalog :read-write
                              (format nil "Create class/project ~s" satellite-project-ref-name)
                              :project-var project :cset-type :minor)
        (setq project (rfm-project-create satellite-project-ref-name description
                                          (rfm-directory-create subdirectory)
                                          :filenames-case-sensitive? t
                                          ))
        (setq created-project-did (distributed-object-identifier project))))

    ;; Create a satellite-project-ref in the master which references the subproject in the satellite
    (let ((satellite-project-ref (satellite-project-ref-create
                                  satellite-project-ref-name
                                  subdirectory
                                  created-project-did
                                  )))
      ;; Chain the satellite-project-ref into the master catalog. Return the satellite-project-ref.
      (master-catalog-add-satellite-project-ref master-catalog satellite-project-ref)
      )))

(defun master-catalog-call-on-subsystem (master-catalog master-repository-name subsystem
                                         &key satellite-txn-mode reason receiver)
  "Invoke RECEIVER on SUBSYSTEM.

   RECEIVER must take two arguments:
   1) A SUBSYSTEM object
   2) A REPOSITORY object, which is bound to the satellite repository
     , or NIL if it is not specified.

   MASTER-CATALOG is the master catalog object in the master repository.

   MASTER-REPOSITORY-NAME is used to resolve the file system locations of satellite repositories.

   SATELLITE-TXN-MODE is used as the txn mode arguments for satellite transaction.
   If not specified, no attempt is made to begin transactions on the satellite that corresponds
   to the subsystem.

   REASON is some reason used to open a read-only transaction on the satellite repository in order to
   query its contents.

   This routine is often useful with other routines such as SUBSYSTEM-ROOTED-FILE-SYSTEM
   and SUBSYSTEM-SATELLITE-FUNCALL-IN-CONTEXT.

   Return value: NIL"
  ;; TO-DO: this routine should really just be a method on PC if it's needed
  ;; it doesn't really belong here in master-catalog.lsp
  #+allegro (declare (:fbound call-with-subsystem-satellite-repository)) ; forward reference

  (if satellite-txn-mode
      (call-with-subsystem-satellite-repository master-repository-name master-catalog subsystem
                                                :satellite-txn-mode satellite-txn-mode
                                                :reason reason
                                                :receiver receiver)
      (funcall receiver subsystem nil)))


(defun master-catalog-map-pc-subsystems
    (master-catalog master-repository-name pc pc-branch
     &key function satellite-txn-mode reason subsystem-list)
  "Invoke FUNCTION once for every subsystem in PC (a product-configuration).

   FUNCTION must take two arguments:
   1) A SUBSYSTEM object
   2) A REPOSITORY object, which is bound to the satellite repository
      , or NIL if it is not specified.

   MASTER-CATALOG is the  master catalog object in the master repository.

   MASTER-REPOSITORY-NAME is used to resolve the file system locations of satellite repositories.

   PC is the product-configuration we're mapping over.  The appropriate versioned content view
   of the PC is to be established by the caller.

   Is specified, SATELLITE-TXN-MODE is used as the txn mode arguments for satellite transactions.
   If not specified, no attempt is made to begin transactions on satellites which correspond
   to the subsystems.

   REASON is some reason used to open read-only transactions on satellite repositories in order to
   query their contents.  It must be specifed only if SATELLITE-TXN-MODE is specified.

   SUBSYSTEM-LIST, if specified, constrains the operation to the indicated list of subsystems.

   This routine is often useful with other routines such as SUBSYSTEM-ROOTED-FILE-SYSTEM
   and SUBSYSTEM-SATELLITE-FUNCALL-IN-CONTEXT.

   Return value: NIL"
  ;; TO-DO: this routine should really just be a method on PC if it's needed
  ;; it doesn't really belong here in master-catalog.lsp
  #+allegro (declare (:fbound pc-satellite-map-over-subsystem-repositories)) ; forward reference
  (let ((sorted-subsystem-list (sort subsystem-list #'string-lessp :key #'subsystem-name)))

    (if satellite-txn-mode
        (pc-satellite-map-over-subsystem-repositories pc pc-branch master-catalog master-repository-name
          :satellite-txn-mode satellite-txn-mode
          :reason reason
          :subsystem-list sorted-subsystem-list
          :function function)
      ;; Do it without a database transaction
      (dolist (subsystem (or sorted-subsystem-list (pc-get-subsystem-list pc-branch)))
        (funcall function subsystem nil)))
    nil))

(defun master-catalog-extract-subsystem-files-to-disk
    (master-catalog master-repository-name subsystem file-system reason metaversion-timestamp
     &key (report-file-system file-system)
          (clean t)
          workspace)
  "Populate a directory for a subsystem with files from the
   current view of the subsystem (as determined by a WITH-VERSION of the appropriate
   master version on the appropriate master branch of a master product configuration).

   *TBD*: we will probably want a parameter which controls whether to make the disk match exactly
   the PC file structure, or other options, similar to the RETRIEVE FILE interface in e-zchange.

   REASON is some reason used to open read-only transactions on satellite repositories in order to
   query their contents.

   METAVERSION-TIMESTAMP is used to specify the date-driven metaversion in the satellite repository
   which will govern the state of the subsystem branch and version(s) on that branch which we use.
   It should be a time-stamp object, or NIL indicating that the 'latest' metaversion is desired.

   MASTER-CATALOG is the  master catalog object in the master repository.

   MASTER-REPOSITORY-NAME is used to resolve the file system locations of satellite repositories.

   SUBSYSTEM is the subsystem we're extracting.  The appropriate versioned content view
   of the SUBSYSTEM is to be established by the caller.

   FILE-SYSTEM is a logical file system which is assumed rooted to some meaningful location
   such as the root directory for a PC server-side reference directory, or a client workspace root.

   CLEAN, if true, causes target directories to be cleaned out before population.

   Keyword argument REPORT-FILE-SYSTEM if specified is a file-system to which the publishing noise
   should go.

   WORKSPACE, if true, should be a WORKSPACE object in the WORKSPACE repository. It will be
   used to factor in workspace Virtual Private Branch (VPB) change sets in computing the view
   of file content extracted to disk. It is used only for accessing, not updating, workspace data.

   Return value: the master-catalog."

  ;; Save metaversion context for viewing subsystems down in subsystem-satellite routines.
  (let ((master-metaversion-cid-set (with-cm-master-metaversion (metaversion-timestamp)
                                      (txn-context-cid-set *txn-context*))))
    (master-catalog-call-on-subsystem
     master-catalog master-repository-name subsystem
     :satellite-txn-mode :read-only
     :reason reason
     :receiver (lambda (subsystem satellite-repository)
                   (let ((subsystem-did (distributed-object-identifier subsystem)))
                     (subsystem-satellite-extract-files-to-disk
                      subsystem satellite-repository file-system metaversion-timestamp
                      master-metaversion-cid-set
                      ;; deal with overwriting
                      #'publish-supersede-if-overwriting
                      #'publish-overwrite-if-changed
                      :VPB-added-satellite-cset-dids
                      (and workspace
                           (workspace-existing-added-VPB-cset-dids-for-subsystem
                            workspace subsystem-did))
                      :VPB-removed-satellite-cset-dids
                      (and workspace
                           (workspace-existing-removed-VPB-cset-dids-for-subsystem
                            workspace subsystem-did))
                      :report-file-system report-file-system
                      :clean clean)))))
  master-catalog)

(defun master-catalog-extract-changed-subsystem-files-to-disk
    (master-catalog master-repository-name subsystem
     old-timestamp
     new-timestamp
     file-system reason
     &key
     (report-only nil)
     change-context
     VPB-old-added-satellite-cset-dids
     VPB-old-removed-satellite-cset-dids
     VPB-new-added-satellite-cset-dids
     VPB-new-removed-satellite-cset-dids
     (report-file-system file-system))
  "Update a directory for a subsystem with the changes in the files that occurred
   between OLD-TIMESTAMP and NEW-TIMESTAMP.

   *TBD*: we will probably want a parameter which controls whether to make the disk match exactly
   the PC file structure, or other options, similar to the RETRIEVE FILE interface in e-zchange.

   REASON is some reason used to open read-only transactions on satellite repositories in order to
   query their contents.

   OLD-TIMESTAMP and NEW-TIMESTAMP are used to specify the date-driven metaversion in the
   satellite repository.  It should be a timestamp object, or NIL indicating that the 'latest'
   metaversion is desired.

   MASTER-CATALOG is the master catalog object in the master repository.

   MASTER-REPOSITORY-NAME is used to resolve the file system locations of satellite repositories.

   SUBSYSTEM is the subsystem we're extracting.  The appropriate versioned content view
   of the SUBSYSTEM is to be established by the caller.

   FILE-SYSTEM is a logical file system which is assumed rooted to some meaningful location
   such as the root directory for a PC server-side reference directory, or a client workspace root.

   Keyword argument REPORT-FILE-SYSTEM if specified is a file-system to which the publishing noise
   should go.

   Return value: the master-catalog."
  (with-cm-master-metaversion (new-timestamp)
    (let ((subdir (subsystem-relative-subdirectory subsystem))
          (subdir-file-system (subsystem-rooted-file-system subsystem file-system)))
      (unless report-only (file-system-ensure-directory-and-parents file-system subdir))
      (master-catalog-call-on-subsystem
       master-catalog master-repository-name subsystem
       :satellite-txn-mode :read-only :reason reason
       :receiver (lambda (subsystem satellite-repository)
                     (multiple-value-bind (three-way-merge merge-conflict-count binary-merge)
                         (subsystem-satellite-extract-changed-files-to-disk
                          subsystem satellite-repository subdir-file-system
                          old-timestamp
                          new-timestamp
                          :report-only report-only
                          :change-context change-context
                          :VPB-old-added-satellite-cset-dids   VPB-old-added-satellite-cset-dids
                          :VPB-new-added-satellite-cset-dids   VPB-new-added-satellite-cset-dids
                          :VPB-old-removed-satellite-cset-dids VPB-old-removed-satellite-cset-dids
                          :VPB-new-removed-satellite-cset-dids VPB-new-removed-satellite-cset-dids
                          :report-file-system report-file-system
                          )
                       (values master-catalog
                               three-way-merge
                               merge-conflict-count
                               binary-merge)))))))

(defun master-catalog-extract-pc-files-to-disk
    (master-catalog master-repository-name pc pc-branch file-system reason metaversion-timestamp
     &key subsystem-list subsystem-file-alist
          (report-file-system file-system)
          (clean t)
          (read-only t)
          workspace
          change-context)
  "Populate a directory for a product-configuration with files from the
   current view of the product configuration (as determined by a WITH-VERSION of the appropriate
   master version on the appropriate master branch of a master product configuration).

   To do this we must interate over all subsystems in the product configuration
   and match their contents to those on the disk.  In all cases, FILE-SYSTEM should be a server relative
   file-system, and we use the reference directory spec of the PC in order to determine where
   on the file system to write.

   *TBD*: we will probably want a parameter which controls whether to make the disk match exactly
   the PC file structure, or other options, similar to the RETRIEVE FILE interface in e-zchange.

   REASON is some reason used to open read-only transactions on satellite repositories in order to
   query their contents.

   METAVERSION-TIMESTAMP is used to specify the date-driven metaversion in the satellite repository
   which will govern the state of the subsystem branch and version(s) on that branch which we use.
   It should be a timestamp object, or NIL indicating that the 'latest' metaversion is desired.

   MASTER-CATALOG is the  master catalog object in the master repository.

   MASTER-REPOSITORY-NAME is used to resolve the file system locations of satellite repositories.

   PC is the product-configuration we're mapping over.  The appropriate versioned content view
   of the PC is to be established by the caller.

   FILE-SYSTEM is a logical file system which is assumed rooted to some meaningful location
   such as the root directory for a PC server-side reference directory, or a client workspace root.

   REPORT-FILE-SYSTEM is a file system to which progress indications will be sent.  It defaults to
   file-system.

   SUBSYSTEM-LIST, if specified, constrains the operation to the indicated list of subsystems.

   SUBSYSTEM-FILE-ALIST, if specified, is an association list whose keys are subsystems, and whose
   values are file DIDS in that subsystem to be extracted.  Subsystem keys which aren't in subsystem-list
   (if specified) are ignored.

   CLEAN, if true, causes target directories to be cleaned out before population.

   WORKSPACE, if present, should be a WORKSPACE object in the WORKSPACE repository. It will be
   used to factor in workspace Virtual Private Branch (VPB) change sets in computing the view
   of file content extracted to disk. It is used only for accessing, not updating, workspace data.

   CHANGE-CONTEXT, if present, should be the transient change context associate with the
   workspace.  It will be used to avoid losing changes in checked out files.

   See also master-catalog-extract-pc-file-names-to-list which was cloned from this function.

   Return value: the master-catalog."

  ;; Save metaversion context for viewing subsystems down in subsystem-satellite routines.
  (let ((master-metaversion-cid-set (with-cm-master-metaversion (metaversion-timestamp)
                                      (txn-context-cid-set *txn-context*))))
    (master-catalog-map-pc-subsystems master-catalog master-repository-name pc pc-branch
      :satellite-txn-mode :read-only
      :reason reason
      :subsystem-list subsystem-list
      :function (lambda (subsystem satellite-repository)
                    (let ((subsystem-did (distributed-object-identifier subsystem)))
                      (subsystem-satellite-extract-files-to-disk
                       subsystem satellite-repository file-system metaversion-timestamp
                       master-metaversion-cid-set
                       ;; deal with overwriting
                       #'publish-supersede-if-overwriting
                       (if (null change-context)
                           (publish-backup-if-changed)
                         ;; Very tricky overwriting policy:  If the file to be published
                         ;; is currently checked out, *don't* overwrite it or back it up,
                         ;; (cause it has changes in it).  Otherwise, back it up.
                         (lambda (file-system-element file-system file-descriptor)
                             (if (change-context-find-file-changes change-context
                                                                   (distributed-object-identifier file-system-element))
                                 :set-rw
                               ;; indicates that publishing should not proceed,
                               ;; but the file ro attribute should be set to rw
                               (funcall (publish-backup-if-changed) file-system-element file-system file-descriptor))))
                       :subsystem-file-alist subsystem-file-alist
                       :VPB-added-satellite-cset-dids
                       (and workspace
                            (funcall (if nil ;(workspace-in-transition? workspace)
                                         #'workspace-transitional-added-VPB-cset-dids-for-subsystem
                                       #'workspace-existing-added-VPB-cset-dids-for-subsystem)
                                     workspace subsystem-did))
                       :VPB-removed-satellite-cset-dids
                       (and workspace
                            (funcall (if nil ;(workspace-in-transition? workspace)
                                         #'workspace-transitional-removed-VPB-cset-dids-for-subsystem
                                       #'workspace-existing-removed-VPB-cset-dids-for-subsystem)
                                     workspace subsystem-did))
                       :report-file-system report-file-system
                       :read-only read-only
                       :clean clean)))))
  master-catalog)

(defun master-catalog-extract-pc-file-names-to-list
    (master-catalog master-repository-name
     pc pc-branch file-system
     reason metaversion-timestamp
     &key subsystem-list
          subsystem-file-alist
          workspace
          no-directories)
  "Populate a list with the file names for a product-configuration from the current view of the
   product configuration (as determined by a WITH-VERSION of the appropriate master version on
   the appropriate master branch of a master product configuration).

   The list will also contain directories unless NO-DIRECTORIES is non-nil.

   This gives you a list of (full pathname) files known to be part of a product.

   To do this we must interate over all subsystems in the product configuration
   and match their contents to those on the disk.

   REASON is some reason used to open read-only transactions on satellite repositories in order to
   query their contents.

   METAVERSION-TIMESTAMP is used to specify the date-driven metaversion in the satellite repository
   which will govern the state of the subsystem branch and version(s) on that branch which we use.
   It should be a timestamp object, or NIL indicating that the 'latest' metaversion is desired.

   MASTER-CATALOG is the  master catalog object in the master repository.

   MASTER-REPOSITORY-NAME is used to resolve the file system locations of satellite repositories.

   PC is the product-configuration we're mapping over.  The appropriate versioned content view
   of the PC is to be established by the caller.

   FILE-SYSTEM is a logical file system which is assumed rooted to some meaningful location
   such as the root directory for a PC server-side reference directory, or a client workspace root.

   SUBSYSTEM-LIST, if specified, constrains the operation to the indicated list of subsystems. Nil
   means all subsystems.  Nil means use them all.

   SUBSYSTEM-FILE-ALIST, if specified, is an association list whose keys are subsystems, and whose
   values are file DIDS in that subsystem to be extracted.  Subsystem keys which aren't in subsystem-list
   (if specified) are ignored.  Nil means all files

   WORKSPACE, if present, should be a WORKSPACE object in the WORKSPACE repository. It will be
   used to factor in workspace Virtual Private Branch (VPB) change sets in computing the view
   of file content extracted to disk. It is used only for accessing, not updating, workspace data.

   See also master-catalog-extract-pc-files-to-disk from which this function was cloned.

   Return value: the list of filenames."

  ;; Save metaversion context for viewing subsystems down in subsystem-satellite routines.
  (let ((master-metaversion-cid-set (with-cm-master-metaversion (metaversion-timestamp)
                                      (txn-context-cid-set *txn-context*)))
        file-path-list)
    (master-catalog-map-pc-subsystems master-catalog master-repository-name pc pc-branch
      :satellite-txn-mode :read-only :reason reason
      :subsystem-list subsystem-list
      :function
      (lambda (subsystem satellite-repository)
          (let ((subsystem-did (distributed-object-identifier subsystem)))
            (debug-message 5 "Publish F-List Subsystem: ~s~%" subsystem)
            (setq file-path-list
                  (nconc file-path-list
                         (subsystem-satellite-extract-file-names-to-list
                          subsystem satellite-repository file-system metaversion-timestamp
                          master-metaversion-cid-set
                          :subsystem-file-alist subsystem-file-alist
                          :VPB-added-satellite-cset-dids
                          (and workspace
                               (funcall (if nil ;(workspace-in-transition? workspace)
                                            #'workspace-transitional-added-VPB-cset-dids-for-subsystem
                                          #'workspace-existing-added-VPB-cset-dids-for-subsystem)
                                        workspace subsystem-did))
                          :VPB-removed-satellite-cset-dids
                          (and workspace
                               (funcall (if nil ;(workspace-in-transition? workspace)
                                            #'workspace-transitional-removed-VPB-cset-dids-for-subsystem
                                          #'workspace-existing-removed-VPB-cset-dids-for-subsystem)
                                        workspace subsystem-did))
                          :no-directories no-directories))))))
    file-path-list))

(defun master-catalog-create-subsystem (master-catalog master-repository-name
                                        subsystem-name class-name pc-branch
                                        subsystem-description source-subsystem
                                        metaversion-timestamp
                                        subscriber-modes
                                        &key subdirectory
                                             (update-master-catalog t)
                                             (update-pc t))
  "Create a SUBSYSTEM object named SUBSYSTEM-NAME which binds the satellite project named CLASS-NAME
   to PC as a subscriber.  Assumes the latest repository metaversion in effect for this command.

   MASTER-REPOSITORY-NAME is the path specification used to open the master repository, which
   is merged with the pathname of the satellite repository to hopefully locate the satellite repository.

   A branch is created in the satellite project represented by CLASS-NAME.

   An error is signalled if there is no class named CLASS-NAME.

   SUBSYSTEM-DESCRIPTION must be NIL or a string, and is attached to the resulting subsystem.

   SOURCE-SUBSYSTEM must be NIL or a SUBSYSTEM object from which the newly created subsystem
   will import changes and inherit initial change-sets.  The set of initial change-sets depends
   on METAVERSION-TIMESTAMP.

   METAVERSION-TIMESTAMP is used to specify the date-driven metaversion used to select initial
   subsystem content when a base product or base subsystem is indicated (i.e. where on the source
   branch in the satellite we're branching from).  It may be NIL or a TIMESTAMP object.

   SUBSCRIBER-MODES must be NIL, or a list of values from *SUBSYSTEM-SUBSCRIBER-MODES*.

   UPDATE-PC, if nil (true is the default), will suppress addition of the resulting subsystem to
   the indicated PC.  This is typically used as an efficiency measure by product-creation
   code which creates many subsystems.

   SUBDIRECTORY is either a relative pathname of the new subsystem's subdirectory
   (overriding that of the class) or nil.

   A transaction on the master repository is implicit in this call, and a nested transaction
   is performed upon the appropriate satellite repository.

   **** NOTE ****
   This routine does not update the PC reference area to reflect the new subsystem, this
   operation is left to the caller, and will hopefully be done in an MVCC transaction.

   Returns the created subsystem."
  #+allegro (declare (:fbound pc-add-subsystem))
  (unless (cm-txn-latest-master-metaversion-p)
    (error "The master-catalog-create-subsystem assumes the latest-metaversion in effect,
           and an assertion to that effect fails."))
  ;; Verify that the class exists.
  (let* ((pc (branch-owning-project pc-branch))
         (satellite-project-ref (master-catalog-lookup-satellite-project-ref master-catalog class-name))
         (project-did (and satellite-project-ref
                           (satellite-project-ref-project-did satellite-project-ref)))
         (source-branch-did (and source-subsystem (subsystem-satellite-project-branch-did source-subsystem)))
         (branch-did nil)               ;filled in far below
         (subsystem nil)                ;filled in even farther below
         (pc-name (object-user-name pc))
         (subsystem-name (or subsystem-name
                             (conman-synthesize-new-subsystem-name class-name pc-name)))
         (derived-branch-name (concatenate 'string subsystem-name "_" pc-name)))
    (unless satellite-project-ref
      (error "A class named ~s does not exist." class-name))
    ;; IMPLEMENTATION NOTE: this is a nested change-set transaction!
    ;; CAUTION: allocations are made in the satellite database until we exit the
    ;; with-cm-satellite-repository form.
    (with-cm-satellite-repository
        (satellite-repository
         (db-name-merge (satellite-project-ref-repository-pathname satellite-project-ref)
                        master-repository-name))
      ;; Ensure no branch exists with the derived name, and create the branch.
      (with-cm-satellite-txn (satellite-repository
                              master-catalog :read-write
                              (format nil "Create project branch ~s for subsystem ~s."
                                      derived-branch-name subsystem-name)
                              :project-var project :cset-type :minor)
        (setq project (repository-resolve-distributed-identifier satellite-repository project-did))
          ;; Sanity check.
          (when (project-lookup-branch-name project derived-branch-name)
            (error "A branch named ~s already exists in class ~s." derived-branch-name class-name))
        (let* ((source-branch (or (and source-branch-did
                                       (repository-resolve-distributed-identifier satellite-repository
                                                                                  source-branch-did))
                                  (project-get-main-branch project)))
               ;; Create the branch.  Remember its DID for use after we close the satellite repository txn.
               (branch
                (with-cm-satellite-metaversion (metaversion-timestamp)
                  (branch-create
                   derived-branch-name
                   source-branch metaversion-timestamp
                   (format nil "Subsystem branch created for product ~s, class ~s, subsystem name ~s"
                           pc-name class-name subsystem-name)
                   project))))
          (setq branch-did (distributed-object-identifier branch))
          (project-add-branch project branch))))

    ;; Branch created.  Create the subsystem with the relevant information
    (setq subsystem (subsystem-create subsystem-name satellite-project-ref
                                      project-did branch-did
                                      :subsystem-description subsystem-description
                                      :source-subsystem source-subsystem
                                      :subdirectory subdirectory))
    ;; Add the product configuration as a subscriber of the subsystem.
    (subsystem-add-subscriber subsystem pc-branch :subscriber-modes subscriber-modes)
    ;; Add the new subsystem to the master catalog
    (when update-master-catalog
      (master-catalog-add-subsystems master-catalog (list subsystem)))
    ;; Add the subsystem to the product configuration.  Note that the product
    ;; configuration views of the subsystem may differ over time!
    (when update-pc
      (pc-add-subsystem pc-branch subsystem))
    ;; Big picture: we're done with the repository modeling.  We leave it to the caller
    ;; to populate the reference area in a separate, and hopefully MVCC, transaction.
    subsystem))

(defun master-catalog-make-unique-subsystem-name
    (master-catalog
     base-subsys-name                   ; will have a number appended if needed for uniqueness
     )
  "Builds a unique subsystem-name for PRODUCT_CREATE and RELEASE_CREATE by adding a number to
   the base-subsys-name until it is not already in use as a subsystem name.

   It RETURNS the string which is the unique name"
  (let ((new-subsys-name base-subsys-name)
        (name-ctr 0))

    ;; construct a new subsys name until it is unique
    ;; *FINISH* ??? someday we may want to add code to avoid an infinite
    ;; loop by means of a loop limit
    (loop while (master-catalog-subsystem-name-lookup
                 master-catalog new-subsys-name
                 :error-if-missing nil)
          do (progn
               (setq name-ctr (+ name-ctr 1))
               (setq new-subsys-name
                     (concatenate 'string base-subsys-name
                                  "_" (format nil "~s" name-ctr)))
               ))

    #||
    ;; something like this will be needed if we avoid infinite loops
    ;; Make sure that we did not stop because of the counter
    (when (master-catalog-subsystem-name-lookup
           master-catalog new-subsys-name :error-if-missing nil)
      (conman-signal-error *cm-returns-error-subsystem-already-exists*
                           "A subsystem named ~S already exists."
                           new-subsys-name))
    ||#

    new-subsys-name))

(defun master-catalog-pc-create
    (master-catalog
     master-repository-name             ;name of the master repository for satellite repository derivation
     pc-name                            ;name of this PC (named-object value)
     parent-pc                          ;parent PC object, or NIL
     parent-pc-branch-name              ;parent pc-branch-name, or NIL
     pc-description                     ;string, or NIL
     metaversion-timestamp              ;timestamp or NIL
     copy-all copy-none                 ;true or nil
     copy-file)                         ;list of subsys names: these will be copied; rest are shared
  "Create and return a product-configuration named PC-NAME.

   This routine may result in creation of subsystems, which in turn must open satellite repositories,
   which is why this layer for creating PC's exists in master-catalog (to preserve encapsulation of
   satellite repository opens).

   MASTER-REPOSITORY-NAME is the path specification used to open the master repository, which
   is merged with the pathname of the satellite repository to hopefully locate the satellite repository.

   If this PC is being created from another PC, specify that as PARENT-PC.  When this is
   NON-NIL, One of COPY-ALL or COPY-NONE must be specified to control resulting subsystem
   status in the created PC.  Note that COPY-FILE isn't yet supported *FINISH*.

   DESCRIPTION, if non-nil, should also be a string.

   METAVERSION-TIMESTAMP, if non-nil, should be a timestamp which specifies the
   metaversion view of the parent-pc and its attendant subsystems.

   COPY-ALL, if true, specifies that the subsystems of the newly created PC will be new
   subsystems derived from the parent-pc (which must be non-nil).  Newly created
   subsystems inherit all from those of the parent-pc.  This option is mutually exclusive
   with the COPY-NONE option.

   COPY-NONE, if true, specifies that the subsystems of the newly created PC will be those
   of the parent PC (which must be non-nil), in a shared fashion.  However the newly
   created PC will NOT have WRITE access to the subsystems. This option is mutually
   exclusive with the COPY-NONE option.

   NOTE: checking for argument compatibility w.r.t. COPY-* and PARENT-PC is assumed done by the caller."
  #+allegro (declare (:fbound set-pc-subsystems pc-create)) ;forward reference
  (unless (cm-txn-latest-master-metaversion-p)
    (error "Master-catalog-pc-create called without latest metaversion. ~
            Do not pass go.  Do not collect $200."))
  (let* ((parent-branch-or-nil
          (when parent-pc
            (pc-lookup-branch parent-pc parent-pc-branch-name
                              :error-if-missing t)))
         (pc (pc-create pc-name                 ;name of this PC (named-object value)
                        parent-branch-or-nil
                        (when parent-branch-or-nil metaversion-timestamp)
                        pc-description))        ;string, or NIL
         (pc-branch (project-lookup-branch-name pc +pc-main-branch-name+))
         (metaversion-cidset nil)               ;cached for performance
         (source-subsystem-list nil))   ;filled in below

    ;; Calculate source subsystems, which must be done under appropriate metaversion.
    ;; Note that we can't create new target subsystems until we have the pc context for them.
    (when parent-pc
      (with-cm-master-metaversion (metaversion-timestamp)
        (setq metaversion-cidset (txn-context-cid-set *txn-context*))
        ;; *FINISH*: need to observe label specs.
        (progn ;;with-version ((branch-get-most-recent-version (pc-lookup-branch-ok parent-pc parent-pc-branch-name)))
          (setq source-subsystem-list (pc-get-subsystem-list
                                       (pc-lookup-branch-ok parent-pc parent-pc-branch-name)))))
      ;; Manage subsystems to be associated with the PC if a parent PC is present
      ;; alter the subscriber mode of shared subsystems.
      (flet ((copy-subsystem (source-subsystem)
               (let (class-name description sub-dir)
                 ;; Do the binding in advance of the subsys-create call, since it
                 ;; should occur in the latest metaversion.
                 (with-cm-master-metaversion (metaversion-cidset)
                   (setq class-name (subsystem-class-name source-subsystem))
                   ;; *PERFORMANCE* (Space)
                   ;; This is wasteful, would be nice to have interned strings here...
                   (setq description (subsystem-description source-subsystem))
                   (setq sub-dir (subsystem-relative-subdirectory source-subsystem)))

                 ;; Construct a subsystem-name, if the constructed name already exists,
                 ;; reconstruct it (by adding a number) until it is new
                 (let ((new-subsys-name
                        (conman-synthesize-new-subsystem-name class-name pc-name))) ; new subsystem-name

                   (setq new-subsys-name
                         (master-catalog-make-unique-subsystem-name master-catalog new-subsys-name))

                   ;; Create the subsystem
                   (master-catalog-create-subsystem
                    master-catalog master-repository-name
                    new-subsys-name
                    class-name pc-branch ;;the one we just made
                    description source-subsystem metaversion-timestamp
                    '(:WRITE) ;; Created subsystem initially has write access
                    :update-pc nil
                    :update-master-catalog nil  ;; don't push this subsys to pc, done en masse below
                    :subdirectory sub-dir))))

             (share-subsystem (subsystem)
               (subsystem-add-subscriber subsystem pc-branch :subscriber-modes nil) ;; NIL --> read only
               subsystem)

             (zap-subsystem-slot (new-subsystems)
               ;; Update the pc subsystem list in the correct branch versioned context.
               ;; main branch is ok, this is product_create with its main branch
               (progn
                 ;;with-version ((branch-get-most-recent-version (pc-lookup-branch-ok pc +pc-main-branch-name+)))
                 (set-pc-subsystems (pc-lookup-branch-ok pc +pc-main-branch-name+)
                                    (sort new-subsystems #'string-lessp :key #'subsystem-name))))
             )
        (flet ((zap-both-slots (new-subsystems)
                 (master-catalog-add-subsystems master-catalog new-subsystems)
                 (zap-subsystem-slot new-subsystems))

               )
          ;; We're in the metaversion.  Note that the pc-subsystem slot updates must be in the
          ;; branch-based version scope, but the copy-subsystem calls must be in the metaversion
          (cond (copy-all
                 ;; Create new subsystems with new pc as parent
                 (zap-both-slots (mapcar #'copy-subsystem  source-subsystem-list)))

                (copy-none
                 ;; Share the base product's subsystems but only in read-only mode.
                 ;; Since the product is newly created, the master metaversion version context
                 ;; is the same version context as the latest version of the product.
                 (zap-subsystem-slot (mapcar #'share-subsystem source-subsystem-list)))

                (copy-file
                 ;; Copy the subsystems named in copy-file, and share the others
                 (let* ((invalids ())
                        (copies (mapcan
                                 (lambda (name &aux (sys (find name source-subsystem-list
                                                                 :key #'subsystem-name
                                                                 :test #'string=)))
                                     (if sys (list sys)
                                       (prog1 nil (push name invalids))))
                                 copy-file))
                        (shares (set-difference source-subsystem-list copies)))
                   (when invalids
                     (conman-signal-error *cm-returns-error-subsystem-not-found*
                                          "Invalid subsystem names: ~S" invalids))
                   (let ((new-subsystems (mapcar #'copy-subsystem  copies)))
                     (master-catalog-add-subsystems master-catalog new-subsystems)
                     (zap-subsystem-slot (nconc new-subsystems
                                                (mapcar #'share-subsystem shares))))))

                (t (error "master-catalog-pc-create: not-implemented"))))))
    ;; Catalog the resulting pc
    (master-catalog-add-pc master-catalog pc)
    pc))

(defun master-catalog-pc-release-create
    (master-catalog
     master-repository-name             ;name of the master repository for satellite repository derivation
     pc-name                            ;name of this PC (named-object value)
     pc                                 ; the project we are from
     from-release-name                  ;
     from-release-obj                   ; the branch we are from (may be the main branch)
     description                        ;string, or NIL
     release-name                       ;validated relase-name
     metaversion-timestamp              ;timestamp or NIL
     double-inheritance                 ;true when we need to also inherit from main-trunk
     )

  "MASTER-CATALOG
   MASTER-REPOSITORY-NAME
   PC-NAME a string of the name of the product
   PC - the project we are from
   FROM-RELEASE-NAME - the name of the release we are created from (never nil, always a string)
   FROM-RELEASE-OBJ - the branch we are from
   DESCRIPTION - a string or nil
   RELEASE-NAME - a string that names the release we are creating
   METAVERSION-TIMESTAMP - the timestamp we were given to find the node on the product tree
   DOUBLE-INHERITANCE - a boolean flag (nil or true) that is true when we are to also inherit
    (selectively) from the main-trunk, rather than solely from the FROM-RELEASE-OBJ"
  #+allegro (declare (:fbound pc-create-branch conman-branch-get-mutable-tip))
  (unless (cm-txn-latest-master-metaversion-p)
    (error "Master-catalog-pc-release-create called without latest metaversion. ~
            Do not pass go.  Do not collect $200."))
  (let ((metaversion-cidset nil)        ;cached, could be expensive to compute
        (source-subsystem-list nil)     ;filled in below
        (class-to-subsystem-map nil)
        (pc-branch
         ;; Create the VM::BRANCH object which is scoped by the PC on which we're creating a release.
         ;; Do this through an encapsulation on the PC, in the context of the latest metaversion.
         (pc-create-branch pc release-name from-release-obj
                           (when from-release-obj metaversion-timestamp)
                           (or description
                               (format nil
                                       "ChangeSafe branch/release ~a for product ~a"
                                       release-name pc-name)))))
    (flet ((copy-subsystem (source-subsystem)
             (let (class-name description subsystem sub-dir)
               ;; Do the binding in advance of the subsys-create call, since it
               ;; should occur in the latest metaversion.
               (with-cm-master-metaversion (metaversion-cidset)
                 (setq class-name (subsystem-class-name source-subsystem))
                 ;; *PERFORMANCE* (Space)
                 ;; This is wasteful, would be nice to have interned strings here...
                 (setq description (subsystem-description source-subsystem))
                 (setq sub-dir (subsystem-relative-subdirectory source-subsystem)))
               (setq subsystem
                     (master-catalog-create-subsystem
                      master-catalog master-repository-name
                      ;; New subsystems created for new releases have name class_product_release
                      (master-catalog-make-unique-subsystem-name
                       master-catalog
                       (concatenate 'string class-name "_" pc-name "_" release-name)) ; new subsystem-name
                      class-name pc-branch
                      description source-subsystem metaversion-timestamp
                      '(:WRITE) ;; Created subsystem initially has write access
                      :update-pc nil ;; done en masse below in set-pc-subsytems
                      :update-master-catalog nil  ;; done en masse below in master-catalog-add-subsytems
                      :subdirectory sub-dir)) ;; always use source-sub-dir, bug
               (subsystem-set-inheritance-mode subsystem source-subsystem :select)
               ;; need to handle inheritance from main-trunk, sometimes
               (when double-inheritance
                 (let ((src-subsys (gethash class-name class-to-subsystem-map nil)))
                   (when (and src-subsys (not (eq src-subsys source-subsystem)))
                     (subsystem-set-inheritance-mode subsystem src-subsys :select))))
               subsystem
               )))
      ;; Calculate source subsystems, which must be done under appropriate metaversion.
      ;; Note that we can't create new target subsystems until we have the pc context for them.
      (with-cm-master-metaversion (metaversion-timestamp)
        (setq metaversion-cidset (txn-context-cid-set *txn-context*))
        (when double-inheritance
          ;; We really mean this, we're searching for the main branch of the product from which this
          ;; new release is ultimately derived.
          (progn ;;with-version ((branch-get-most-recent-version (pc-lookup-branch-ok pc +pc-main-branch-name+)) :version-context :pc)

            (let ((main-subsystems (pc-get-subsystem-list (pc-lookup-branch-ok pc +pc-main-branch-name+))))
              (setq class-to-subsystem-map
                    (make-hash-table :test #'equal
                                     :size (* 2 (length main-subsystems))))
              (dolist (item main-subsystems)
                (setf (gethash (subsystem-class-name item) class-to-subsystem-map) item)))))

        ;; *FINISH*: need to observe label specs.
        (progn ;;with-version ((branch-get-most-recent-version (pc-lookup-branch-ok pc from-release-name)) :version-context :pc)
          (debug-message 4 "CID SET for getting subsyslist: ~S"
                         (txn-context-cid-set *txn-context*))
          (setq source-subsystem-list (pc-get-subsystem-list (pc-lookup-branch-ok pc from-release-name))))

        (with-cm-master-metaversion ()
          (progn
            (let ((new-subsystems (mapcar #'copy-subsystem source-subsystem-list)))
              (master-catalog-add-subsystems master-catalog new-subsystems)
              (setq source-subsystem-list
                    (sort new-subsystems #'string-lessp :key #'subsystem-name)))

            ;;with-version ((conman-branch-get-mutable-tip pc-branch) :version-context :pc)
            ;; above version unneeded, but call may be needed to trap frozen branches
            (conman-branch-get-mutable-tip pc-branch)
            ;; Manage subsystems to be associated with the PC if a parent PC is present
            ;; alter the subscriber mode of shared subsystems.
            ;; Create new subsystems with new pc as parent
            (set-pc-subsystems pc-branch source-subsystem-list)))))
    pc-branch))

(defun master-catalog-setup-workspace-mvcc (master-repository-name master-catalog
                                            file-system pc branch
                                            reason metaversion-timestamp
                                            populate-files)
  "Calculate & return various values so that a workspace may be created and (later) populated on a
   client file-system workspace representaion using FILE-SYSTEM.  This function does NOT actually
   populate the file-system (see master-catalog-populate-workspace-mvcc below) unless the
   POPULATE-FILES argument is non-nil.
   To do this requires examining each subsystem in the product configuration (PC).

   MASTER-REPOSITORY-NAME is the path specification used to open the master repository, which
   is merged with the pathname of the satellite repository to hopefully locate the satellite repository.

   A transaction on the master repository is implicit in this call, and a nested transaction
   is performed upon the appropriate satellite repositories.  All information is used in read-only fashion.

   Since we want to support read-only use of the versioned repositories, and can't update the
   workspace repository if the versioned repositories were opened in a parent transaction for read-only
   access,

   REASON is passed down from the caller, and should be the reason used for opening the master repository.

   METAVERSION-TIMESTAMP should be a TIMESTAMP object which drives selection of a repository metaversion,
   or NIL, in which case the latest metaversion is used.

   POPULATE-FILES controls whether or not the client file-system workspace representation gets populated.
   A value of NIL means it does NOT get populated.  Anything else cause it to be populated.

   ********************************** NOTE *************************************
   The caller is responsible for creating a workspace with workspace-create in
   the semipersistent workspace repository  using the values returned by this function.

   This function is used for both newly created workspaces as well as repurposing existing workspaces.
   At this level, both actions are viewed as an incremental update.  An incremental update
   on an empty workspace will populate the whole file system...

   We return four values for this purpose, DID's to be used in a call to workspace-create:
   by the caller of this routine (assuming no conditions are signalled):
   1) the DID of the PC to which the workspace applies
   2) the DID of the branch in the pc to which the workspace applies
   3) the DID of the version on the pc-branch which represents the workspace baseline version.
   4) A timestamp which acts as the baseline reference for the workspace.
   5) the master-metaversion-cid-set"

  (unless metaversion-timestamp
    (setq metaversion-timestamp (timestamp-allocate)))

  (with-cm-master-metaversion (metaversion-timestamp)
    (let* ((master-metaversion-cid-set (txn-context-cid-set *txn-context*))
           (pc-branch branch)
           (pc-version (branch-get-most-recent-version pc-branch))
           )

      ;; Populate the client file-system workspace representation?
      (when populate-files
        (master-catalog/populate-workspace master-repository-name master-catalog
                                                     file-system reason pc-branch
                                                     metaversion-timestamp
                                                     master-metaversion-cid-set))

      ;; In the event this function is called for workspace creation, the user will want
      ;; to create a workspace object in the workspace repository.
      ;; This must be accomplished by the caller, so we return information necessary to do this.
      ;;(format *debug-io* "~%Cid-set for pc-lookup-branch: ~s" (txn-context-cid-set *txn-context*))
      (values pc pc-branch pc-version
              ;; This is the time-reference baseline for the workspace.
              ;; Note that this timestamp is deliberately unspecific in sub-universal-time granularity
              ;; (or metaversion-timestamp (timestamp-create (get-universal-time)))

              ;; Making the time stamp deliberately unspecific is probably a *bad* idea:  if changes come
              ;; in during this interval, (but after we update the master-catalog), the workspace will have
              ;; the wrong timestamp.
              metaversion-timestamp
              master-metaversion-cid-set
              ))))

(defun master-catalog-just-populate-workspace-mvcc (master-repository-name master-catalog
                                                    file-system pc pc-branch
                                                    reason metaversion-timestamp
                                                    master-metaversion-cid-set)
  "Populate a client file-system workspace representaion using FILE-SYSTEM.
   To do this requires examining each subsystem in the product configuration (PC).

   MASTER-REPOSITORY-NAME is the path specification used to open the master repository, which
   is merged with the pathname of the satellite repository to hopefully locate the satellite repository.

   A transaction on the master repository is implicit in this call, and a nested transaction
   is performed upon the appropriate satellite repositories.  All information is used in read-only fashion.

   We also assume that the caller has done a 'with-cm-master-metaversion' to finish setting up the
   environment.

   Since we want to support read-only use of the versioned repositories, and can't update the
   workspace repository if the versioned repositories were opened in a parent transaction for read-only
   access,

   PC is the Product-Configuration.  PC-VERSION is the appropriate version to use.

   REASON is passed down from the caller, and should be the reason used for opening the master repository.

   METAVERSION-TIMESTAMP should be a TIMESTAMP object which drives selection of a repository metaversion,
   or NIL, in which case the latest metaversion is used.

   MASTER-METAVERSION-CID-SET is transaction context cid-set.

   ********************************** NOTE *************************************
   The caller is responsible for creating a workspace with workspace-create in
   the semipersistent workspace repository  using the values returned by this function.

   This function is used for both newly created workspaces as well as repurposing existing workspaces.
   At this level, both actions are viewed as an incremental update.  An incremental update
   on an empty workspace will populate the whole file system..."

  (progn ;;with-version (pc-version)
    (master-catalog-map-pc-subsystems   ;pc version context responsibility of THIS calling routine
     master-catalog master-repository-name pc pc-branch
     :reason reason
     :satellite-txn-mode :read-only
     :function (lambda (subsystem satellite-repository)
                   (subsystem-satellite-extract-files-to-disk
                    subsystem
                    satellite-repository
                    file-system
                    metaversion-timestamp
                    master-metaversion-cid-set
                    ;; In theory we could optimize these.
                    #'publish-supersede-if-overwriting ; directory
                    #'publish-overwrite-if-changed ; file
                    ;; don't trash what's there if we are doing
                    ;; ws_set
                    :clean nil
                    )))))

(defun master-catalog-populate-workspace-mvcc (master-repository-name master-catalog
                                               file-system pc pc-branch
                                               reason master-metaversion-cid-set
                                               metaversion-timestamp)
  "Populate a client file-system workspace representaion using FILE-SYSTEM.
   To do this requires examining each subsystem in the product configuration (PC).

   This function is used when master-catalog-setup-workspace-mvcc above is passed a NIL for its
   POPULATE-FILES argument.  This function is then called later in the sequence and will actually
   populate the files.

   MASTER-REPOSITORY-NAME is the path specification used to open the master repository, which
   is merged with the pathname of the satellite repository to hopefully locate the satellite repository.

   A transaction on the master repository is implicit in this call, and a nested transaction
   is performed upon the appropriate satellite repositories.  All information is used in read-only fashion.

   Since we want to support read-only use of the versioned repositories, and can't update the
   workspace repository if the versioned repositories were opened in a parent transaction for read-only
   access,

   REASON is passed down from the caller, and should be the reason used for opening the master repository.

   PC is the Product-Configuration.  PC-VERSION is the appropriate version to use.

   METAVERSION-TIMESTAMP should be a TIMESTAMP object which drives selection of a repository metaversion,
   or NIL, in which case the latest metaversion is used.

   MASTER-METAVERSION-CID-SET is transaction context cid-set.

   ********************************** NOTE *************************************
   The caller is responsible for creating a workspace with workspace-create in
   the semipersistent workspace repository  using the values returned by this function.

   This function could be used for both newly created workspaces as well as repurposing existing workspaces.
   However, at the moment it is only used for creating a workspace.
   At this level, both actions are viewed as an incremental update.  An incremental update
   on an empty workspace will populate the whole file system..."

  (with-cm-master-metaversion (metaversion-timestamp)

    ;; Populate the client file-system workspace representation?
    (master-catalog-just-populate-workspace-mvcc master-repository-name master-catalog
                                                 file-system pc pc-branch
                                                 reason metaversion-timestamp
                                                 master-metaversion-cid-set)
    ))
#||
;; UNUSED and stale (assumes subsystem list on PC, instead of BRANCH)
(defun master-catalog-resolve-disk-file-name (pc file-name)
  "Determine what subsystem corresponds to the filename."
  (loop for subsystem in (pc-get-subsystem-list pc)
      as satellite-project-ref = (subsystem-satellite-project-ref subsystem)
      as satellite-project-logical-subdirectory = (satellite-project-ref-logical-subdirectory
                                                   satellite-project-ref)
  ;; don't need this one
  ;; as satellite-project-string-did = (satellite-project-ref-project-did-string satellite-project-ref)
      do
        (when (directory-is-prefix? satellite-project-logical-subdirectory file-name)
          (return-from master-catalog-resolve-disk-file-name
            (list (enough-pathname file-name satellite-project-logical-subdirectory)
                  ;; the following should be `friendly'
                  (enough-namestring file-name satellite-project-logical-subdirectory)
                  satellite-project-ref)))))
;; UNUSED and stale (cf. comment preceding prior function)
(defun master-catalog-resolve-disk-file-names (pc file-names)
  "For each file name in FILE-NAMES (which may be the empty list),
determine that the file exists, and find the subsystem that corresponds to
the filename."
  (mapcar (lambda (filename)
              (master-catalog-resolve-disk-file-name pc filename))
          file-names))

;; UNUSED and stale (doesn't have pc-branch)
(defun master-catalog-resolve-file-names (master-catalog master-repository-pathstring
                                          pc file-names)
  "For each file name in FILE-NAMES (which may be the empty list),
   find the corresponding file in product configuration
   subsystems by determining the associated subsystem via workspace-relative file location.

   Return an alist of (file-did . file-namestring) pairs compatable with use in a CHANGE-CONTEXT
   object via CHANGE-CONTEXT-CREATE for every file named in FILE-NAMES.  Note that the
   file-namestring in the returned alist may not match the file-name in FILE-NAMES because it
   may be canonicalized (TBD).

   WARNING:  The elements in the returned alist are not in the same order as the
   corresponding elements in FILE-NAMES.

   Signal an error if a filename in file-names cannot be resolved.

   This will be a moderately tricky piece of logic, since file names need to be
   matched to known locations in a workspace and resolved to subsystem references.
   Thus the workspace must be passed as part of known context.

   MASTER-REPOSITORY-PATHSTRING is used to derive REPOSITORY-OPEN paths for satellite repositories. "
  ;; JDT: hopefully fixed bug here, we *were* returning the satellite-project-did and file-did,
  ;; not the file-namestring and file-did
  (master-catalog-file-names-resolver master-catalog master-repository-pathstring
                                      pc branch file-names
    (lambda (file-name file-did satellite-project-did
               subsystem file-namestring)
        (declare (ignore file-name file-name file-namestring))
        (list subsystem satellite-project-did file-did))))
||#
(defun master-catalog-file-names-resolver (master-catalog master-repository-pathstring
                                           pc pc-branch file-pathnames collector-function
                                           &key (errorp t)
                                                workspace
                                                (pathname-key #'identity))
  "For each file name in FILE-NAMES (which may be the empty list),
   find the corresponding file in product configuration
   subsystems by determining the associated subsystem via workspace-relative file location.

   COLLECTOR-FUNCTION is called on
       PATHNAME (which is EQ to some element of FILE-NAMES), a workspace relative pathname
       FILE-DID,
       SATELLITE-PROJECT-DID
       SUBSYSTEM (from the master reposity which maps to a branch in satellite-project-did)
       SUBSYS-RELATIVE-PATHNAME, PATHNAME expressed as a relative pathname with
                                 respect to the satellite project subsystem's root.

   WARNING:  The elements are not necessarily acted on in the order they
   appear in FILE-NAMES.

   You probably shouldn't do anything complicated in the COLLECTOR-FUNCTION
   since it is called in a different repository and transaction context than
   you might expect.

   FILE-PATHNAMES is a list of pathnames or of objects for which the PATHNAME-KEY
   function will return a pathname.

   If ERRORP is true, Signal an error if a filename in FILE-PATHNAMES cannot be resolved.

   WORKSPACE, if true, should be a WORKSPACE object in the WORKSPACE repository. It will be
   used to factor in workspace Virtual Private Branch (VPB) change sets and metaversion time stamps
   while computing the view of repository file system structure.

   This will be a moderately tricky piece of logic, since file names need to be
   matched to known locations in a workspace and resolved to subsystem references.
   Thus the workspace must be passed as part of known context.

   MASTER-REPOSITORY-PATHSTRING is used to derive REPOSITORY-OPEN paths for satellite repositories. "
  (declare (ignore pc))

  (loop for fp-thing in file-pathnames
        for pn = (funcall pathname-key fp-thing)
        do (guarantee-relative-file-pathname pn))

  (let ((metaversion-timestamp (and workspace (workspace-baseline-timestamp workspace))))
    (mapcan (lambda (subsystem)
                (let* ((satellite-project-ref (subsystem-satellite-project-ref subsystem))
                      (logical-subdirectory (with-cm-master-metaversion (metaversion-timestamp)
                                              (subsystem-relative-subdirectory subsystem)))
                      (satellite-project-did (satellite-project-ref-project-did satellite-project-ref)))
                  ;; Do any of the files fall into this subsystem?  Find the
                  ;; first one that does, start a transaction, and then find all
                  ;; the ones that do.  This logic is a bit contorted so that we
                  ;; can do all of the files of this satellite in a single
                  ;; transaction and so that we don't start a transaction if
                  ;; there are no files of that satellite.
                  (flet ((file-in-satellite-p (fp-thing)
                           (directory-is-prefix? logical-subdirectory
                                                 (funcall pathname-key fp-thing))))
                    (let ((subsystem-files-to-consider
                           ;; These might or might not be part of the subsystem.
                           ;; At this point we're just seeing if there's at
                           ;; least one file in the subsystem since if there
                           ;; isn't we needn't open the repository.
                           (member-if #'file-in-satellite-p file-pathnames)))
                      ;; *** If the file isn't in a satellite then the
                      ;; CMCTL-PATHNAME-WORK object (or whatever FP-THING is)
                      ;; will be left with an unbound FILE-DID slot.  We need to
                      ;; do something useful here when FP-THING isn't in a
                      ;; satellite.  Either that, or our callers need to be wary
                      ;; of that.
                      (when subsystem-files-to-consider
                        ;; Resolve them all in a single transaction
                        (with-cm-satellite-repository
                            (satellite-repository
                             (db-name-merge (satellite-project-ref-repository-pathname satellite-project-ref)
                                            master-repository-pathstring))
                          (with-cm-satellite-txn
                              (satellite-repository master-catalog :read-only "resolve file names")
                            (with-cm-satellite-metaversion (metaversion-timestamp)
                              (let* ((project (repository-resolve-distributed-identifier
                                               satellite-repository satellite-project-did))
                                     ;; *FINISH*: we need to use the correct satellite branch & version view of
                                     ;; content.
                                     (branch (repository-resolve-distributed-identifier
                                              satellite-repository (subsystem-satellite-project-branch-did subsystem)))
                                     (version (branch-get-most-recent-version branch))
                                     (vpb-added
                                      (when workspace
                                        (workspace-existing-added-VPB-cset-dids-for-subsystem
                                         workspace
                                         (distributed-object-identifier subsystem))))
                                     (vpb-removed
                                      (when workspace
                                        (workspace-existing-removed-VPB-cset-dids-for-subsystem
                                         workspace
                                         (distributed-object-identifier subsystem)))))
                                (with-version (version
                                               :cset-dids-to-add vpb-added
                                               :cset-dids-to-remove vpb-removed)

                                  ;; Sanity check. The satellite-project-ref should
                                  ;; name the same project entity as the subsystem.
                                  (assert (eq satellite-project-did
                                              (subsystem-satellite-project-did subsystem)))
                                  (mapcar (lambda (fp-thing)
                                              ;; We relly do need this test here since
                                              ;; SUBSYSTEM-FILES-TO-CONSIDER, though it starts
                                              ;; with a file frm this subsystem might also
                                              ;; contain those of other subsystems.
                                              (when (file-in-satellite-p fp-thing)
                                                (let* ((pn (funcall pathname-key fp-thing))
                                                       (subsystem-relative-pathname
                                                        ;; If this were computed from the
                                                        ;; pathname of the FILE object
                                                        ;; rather than the pathname as
                                                        ;; specified by the user, then that
                                                        ;; would confuse things if there
                                                        ;; were a pending rename of the
                                                        ;; file.  We assume that the
                                                        ;; filename that the user specified
                                                        ;; is the new name, not the old one.
                                                        (enough-pathname
                                                         pn logical-subdirectory))
                                                       ;; FP-THING's FILE object might be in
                                                       ;; some subdirectory of
                                                       ;; LOGICAL-SUBDIRECTORY.
                                                       (dir (rfm-directory-find-subdir
                                                             (pathname-directory pn)
                                                             (length (pathname-directory
                                                                      logical-subdirectory))
                                                             (rfm-project-root-directory project)))
                                                       (dir-content (when dir
                                                                      (rfm-directory-content-list dir)))
                                                       (fse-probe (when dir-content
                                                                    (find (file-namestring pn)
                                                                          dir-content
                                                                          :test #'string=
                                                                          :key #'file-system-element-name))))
                                                  (if fse-probe
                                                      (funcall collector-function fp-thing
                                                               (distributed-object-identifier fse-probe)
                                                               satellite-project-did subsystem
                                                               subsystem-relative-pathname)
                                                    (if errorp
                                                        (error "No such file ~s in project ~s" pn
                                                               (object-user-name project))
                                                      (funcall collector-function fp-thing nil
                                                               ;; In this case, the file isn't in the project, yet.
                                                               satellite-project-did subsystem
                                                               subsystem-relative-pathname))))))
                                          ;; What we are mapcar'ing over.
                                          subsystem-files-to-consider)))))))))))
            (pc-get-subsystem-list pc-branch))))

(defun master-catalog-revert-change-context (master-catalog cm-session-context workspace
                                             change-context pc branch file-system reason
                                             &key (deleting-workspace-p nil))
  "Undo the file system operations specified in the change context.
   Assumes appropriate master metaversion is active!

   If EXTRACT-TO-DISK-P is NIL, the user's on-disk workspace hierarchy will not
   be updated from the repository.  This is useful for ws_delete since all the
   workspace files that exist in the repositoy are to be removed from the workspace
   anyway."
  ;; Date-driven master metaversion in place
  (let* ((master-metaversion-cid-set (txn-context-cid-set *txn-context*))
         (master-repository-pathstring (cm-session-context-repository-name
                                        cm-session-context)))
    ;; Examine product content under product version, and product branch/version under above
    ;; master metaversion
    (progn ;;with-version ((branch-get-most-recent-version branch))
      (let ((affected-subsystems (if deleting-workspace-p
                                     (pc-get-subsystem-list branch)
                                   (pc-affected-subsystem-list-from-change-context branch change-context))))
        (when affected-subsystems
          ;; note when there are no affected-subsystems,
          ;; pc-satellite-map-over-subsystem-repositories THINKS it should
          ;; map over all the subsystems, but instead here we need do nothing.
          ;; This when is important for performance of cset_uncreate.
        (pc-satellite-map-over-subsystem-repositories pc branch master-catalog master-repository-pathstring

          :reason reason
          :subsystem-list affected-subsystems
          :function (lambda (subsystem satellite-repository)
                        (with-cm-satellite-metaversion ((workspace-baseline-timestamp workspace))
                          (let* ((satellite-project-did (subsystem-satellite-project-did subsystem))
                                 (subdir-file-system
                                  (with-cm-master-metaversion (master-metaversion-cid-set)
                                    (subsystem-rooted-file-system subsystem file-system)))
                                 (satellite-project-branch
                                  (repository-resolve-distributed-identifier
                                   satellite-repository (subsystem-satellite-project-branch-did subsystem)))
                                 ;; Calculate any csets affecting VPB by virtue of change_add/change_remove or prior
                                 ;; change_close operations which weren't promoted via master_change.
                                 (satellite-project-version (branch-get-most-recent-version satellite-project-branch))
                                 (subsystem-did (distributed-object-identifier subsystem))
                                 (added-VPB-dids (workspace-existing-added-VPB-cset-dids-for-subsystem
                                                  workspace subsystem-did))
                                 (removed-VPB-dids (workspace-existing-removed-VPB-cset-dids-for-subsystem
                                                    workspace subsystem-did))

                                 ;; These files should be preserved.
                                 (files-added (remove-if (complement (lambda (addition)
                                                                         (rfm::cc-file-base-change-applies-to-project-p
                                                                          addition satellite-project-did)))
                                                         (when change-context
                                                           (change-context-file-additions change-context))))
                                 (files-renamed (remove-if (complement (lambda (rename)
                                                                           (rfm::cc-file-base-change-applies-to-project-p
                                                                            rename satellite-project-did)))
                                                           (when change-context
                                                             (change-context-file-renames change-context)))))

                            (debug-message 3 "Files added are ~s" files-added)

                            ;; In order to undo the file renames, we should do a `topological sort'.
                            ;; But this is hard, and ultimately unnecessary.  Instead, we just chuck out
                            ;; the renamed files and reconstitute them with the old name.
                            (debug-message 3 "Files renamed are ~s" files-renamed)
                            (dolist (cc-filerename files-renamed)
                              (when (file-system-probe-file subdir-file-system (cc-filerename-new-pathname cc-filerename))
                                (file-system-delete-file subdir-file-system (cc-filerename-new-pathname cc-filerename))))

                            ;; Changes should be discarded, so we will simply delete the
                            ;; files that are `checked out'.  If the file was renamed as well, it
                            ;; should already be deleted.
                            (dolist (cc-filechange (remove-if (complement (lambda (change)
                                                                              (rfm::cc-file-base-change-applies-to-project-p
                                                                               change satellite-project-did)))
                                                              (when change-context
                                                                (change-context-file-changes change-context))))
                              (debug-message 3 "Reverting ~s" cc-filechange)
                              (let* ((did (cc-filechange-file-did cc-filechange))
                                     (rfmfile (repository-resolve-distributed-identifier satellite-repository did)))
                                (unless (find did files-renamed :key #'cc-filerename-file-did)
                                  (with-version (satellite-project-version
                                                 :include-current t
                                                 :cset-dids-to-add added-VPB-dids
                                                 :cset-dids-to-remove removed-VPB-dids)
                                    (when (file-system-probe-file subdir-file-system (rfm::file-system-element-relative-path
                                                                    rfmfile))
                                      (debug-message 3 "Deleting ~s" (rfm::file-system-element-relative-path
                                                                      rfmfile))
                                      (file-system-delete-file subdir-file-system (rfm::file-system-element-relative-path
                                                                                   rfmfile)))))))
                            (if deleting-workspace-p
                                ;; Deleting the workspace.  Remove any repository based files:
                                (subsystem-satellite-remove-files-from-disk
                                 subsystem satellite-repository file-system
                                 (workspace-baseline-timestamp workspace)
                                 master-metaversion-cid-set
                                 :VPB-added-satellite-cset-dids added-VPB-dids
                                 :VPB-removed-satellite-cset-dids removed-VPB-dids)
                              ;; extracting to the user's disk:
                              (subsystem-satellite-extract-files-to-disk
                               subsystem satellite-repository file-system
                               (workspace-baseline-timestamp workspace)
                               master-metaversion-cid-set
                               (publish-error-if-overwriting "Cannot handle reverting a file to directory.")
                               ;; Jrm sez:  since we are reverting the change context, the files
                               ;; that are unchanged in the workspace are the ones that we never
                               ;; checked out at all.  We don't want to back them up, too much work!
                               (publish-backup-if-changed)
                               :VPB-added-satellite-cset-dids added-VPB-dids
                               :VPB-removed-satellite-cset-dids removed-VPB-dids
                               :clean nil))
                            )))
          ))))))

(defun master-catalog-close-change (master-catalog cm-session-context workspace
                                    change-context pc branch file-system reason
                                    final-cset-name
                                    &key promote
                                         file-affected-subsystems
                                         all-affected-subsystems)
  "Perform a project-checkin for every subsystem affected by the workspace change-context.

   Assume that an update change transaction is active on the master repository, but to not
   assume that a change transaction is active on the workspace repository.

   REASON is a transaction reason string which the caller has presumably already cobbled together for
   the master repository transaction.

   FINAL_CSET_NAME is the final, fully qualified name to be used for this check-in/promotion/close.  It
   must be unique.

   PROMOTE, if true, will cause promotion of satellite cids into appropriate satellite subsystem branches
   and master repository subsystem branch mirrors.
   This flag should probably be true only if a master_change command is being performed.

   FILE-AFFECTED-SUBSYSTEMS, if specified, represents a set of subsystems in which file changes will
   take place.  If non-nil, it should be a list of subsystem objects.

   ALL-AFFECTED-SUBSYSTEMS, if specified, represents a set of subsystems in which either file changes
   will take place,or in which cset promotions will take place.  If non-nil, it should be a list of
   subsystem objects.  If NIL, it defaults to FILE-AFFECTED-SUBSYSTEMS.

   FILE-AFFECTED-SUBSYSTEMS must always be a subset of ALL-AFFECTED-SUBSYSTEMS.

   Returns a list of the affected product names"
  ;; Assume master repository metaversion is correct.
  (let* (;;(version (branch-get-most-recent-version branch))
         (read-onlys nil)               ;list of (file-system . pathname) pairs needing r/o bit set
         product-name-list
         (master-repository-pathstring (cm-session-context-repository-name cm-session-context))
         (master-metaversion-cid-set (txn-context-cid-set *txn-context*)))
    (progn ;;with-version (version)             ;bind correct scope for viewing product slots
      (if (master-catalog-resolve-qualified-change-set-name master-catalog
                                                                        final-cset-name
                                                                        :error-if-missing nil)
                      (conman-signal-error *cm-returns-error-duplicate-change-name*
                                             "The fully qualified cset name (~s) is already in use; choose another name."
                                             final-cset-name))
      (unless file-affected-subsystems
        (setq file-affected-subsystems (pc-affected-subsystem-list-from-change-context branch change-context)))
      (unless all-affected-subsystems
        (setq all-affected-subsystems file-affected-subsystems))
      ;; Make sure all elements in file-affected-subsystems are in all-affected-subsystems
      (assert (null (set-difference file-affected-subsystems all-affected-subsystems)))

      (pc-satellite-map-over-subsystem-repositories
       pc branch master-catalog master-repository-pathstring
       :satellite-txn-mode :read-write
       :reason reason :subsystem-list all-affected-subsystems
       :function
       (lambda (subsystem satellite-repository)
           ;; We declare these two things because we want to use themoutside the
           ;; satellite metaversion scope we establish below
           (let ((satellite-cid-set-basis nil) ;cid-set-basis of satellite change-set
                 (satellite-project nil))
             ;; Okay, right now we're in the default metaverion of the satellite.  Alter as appropriate.
             (with-cm-master-metaversion (master-metaversion-cid-set)

               ;; get the list of product names
               (dolist (subsys-scrib (vi-stream-as-list (subsystem-subscribers subsystem)))
                 (push (object-user-name (branch-owning-project
                                          (subsystem-subscriber-pc-branch subsys-scrib)))
                       product-name-list))

               (let ((satellite-project-did (subsystem-satellite-project-did subsystem))
                     ;; subdir-file-system is the only thing that *really cares about the master metaversion,
                     ;; the rest is on general principle (accessing a subsystem's information which is
                     ;; in general master-metaversioned
                     (subdir-file-system (subsystem-rooted-file-system subsystem file-system))
                     (subsystem-did (distributed-object-identifier subsystem)))
                 (with-cm-satellite-metaversion ((workspace-baseline-timestamp workspace))
                   (setq satellite-project (repository-resolve-distributed-identifier
                                            satellite-repository satellite-project-did))
                   (let* ((satellite-project-branch
                           (repository-resolve-distributed-identifier
                            satellite-repository (subsystem-satellite-project-branch-did subsystem)))
                          ;; Calculate any csets affecting VPB by virtue of change_add/change_remove or prior
                          ;; change_close operations which weren't promoted via master_change.
                          (satellite-project-version (branch-get-most-recent-version satellite-project-branch))
                          (added-VPB-dids (workspace-existing-added-VPB-cset-dids-for-subsystem
                                           workspace subsystem-did))
                          (added-VPB-csets (repository-resolve-distributed-identifier-list satellite-repository
                                                                                           added-VPB-dids))
                          (removed-VPB-dids (workspace-existing-removed-VPB-cset-dids-for-subsystem
                                             workspace subsystem-did))
                          (removed-VPB-csets (repository-resolve-distributed-identifier-list
                                              satellite-repository removed-VPB-dids)))
                     ;; Do the checkin of file changes, if the subsystem contains file changes.
                     (if (find subsystem file-affected-subsystems)
                         ;; continue to defer the setting of the files to read-only
                         ;; in case an error occurs later.
                         (multiple-value-bind (current-read-onlys basis)
                             (master-catalog-checkin-satellite-project
                              satellite-project-did satellite-project satellite-project-version subdir-file-system
                              change-context added-VPB-dids removed-VPB-dids satellite-project-branch)
                           (setq read-onlys (nconc current-read-onlys read-onlys)
                                 satellite-cid-set-basis basis))
                       ;; This isn't a 'cset', no file actions apply to this subsystem
                       ;; so we're just adding and/or removing other csets.
                       ;; We'll use the metaversion for the subsystem as the change-set cid-set-basis
                       (setq satellite-cid-set-basis nil))
                     ;; We always need to promote change being closed into workspace view.
                     ;; However if the change is being promoted in the master,
                     ;; we need only update the versionref
                     (if promote
                         (progn
                           ;; Sanity check, shouldn't promote unless this is master-change at tip
                           (unless (cid-set-equal? *cm-txn-master-repository-latest-cid-set*
                                                   master-metaversion-cid-set)
                             (error "Attempt to promote current change-set during a cset-close or ~
                                  master_change operation in a non-latest metaversion."))
                           ;; Promote into subsystems visible in master.  We let the caller
                           ;; handle workspace promotion so that we'll update its versionref.
                           (subsystem-satellite-promote-csets
                            subsystem satellite-repository
                            ;; Add/remove any workspace cset changes which are to be promoted.
                            ;; NOTE: if PROMOTE isn't set, this has to be done by other subsystem-mapping
                            ;; logic as part of master_change.  Doing this here saves having to re-map
                            ;; satellites, but it's doing the same work.  Also note that we're not
                            ;; altering the subsystem mirror of csets here, just satellite repositories.
                            added-VPB-csets removed-VPB-csets
                            :promote-current-cid t :satellite-subsystem-version satellite-project-version))
                       ;; If the change is NOT being promoted in the master, we must promote
                       ;; satellite cids into the workspace object here.
                       (workspace-satellite-queue-current-change-promotion-via-localref
                        workspace subsystem satellite-repository))))))
             ;; Now, here, in the satellite transaction context, get *that* context to know
             ;; the desired basis that was used in formulating it's 'cset'.
             ;; However if the subsystem we're iterating over doesn't have a cset,
             ;; and is instead just promoting/demoting other csets, then satellite-cid-set-basis will
             ;; be nil, and we won't call this routine, which means that the default repository
             ;; transaction logic will use the metaversion as the change-set basis.  That's ok.
             ;; Perhaps we should optimize it away to a zero length cid-set (**PERFORMANCE**),
             ;; but not without some serious thought about it first.
             (when satellite-cid-set-basis
               (txn-context-note-cid-set-basis *txn-context* satellite-cid-set-basis))
             ;; Return the satellite project, the dispatcher of this lambda wants it as the return value.
             satellite-project)))
      ;; Finally, zap the read-only bits of files that were checked in.
      ;; Presumably if we reached this point we didn't abort the transaction.
      (dolist (item read-onlys)
        (file-system-set-read-only (car item) (cdr item) t))
      )
    (debug-message 4 "master-catalog-close-change products: ~a" product-name-list)
    (remove-duplicates product-name-list :test #'string=)))

(defun master-catalog-checkin-satellite-project
    (satellite-project-did satellite-project satellite-project-version subdir-file-system
     change-context added-VPB-dids removed-VPB-dids satellite-project-branch
     &aux no-change-conditions no-change-entity-names read-onlys cid-set-basis
          unused-satellite-version)
  "A helper routine which wraps up the calls to satellite RFM project-checkin logic with
   lots of ugly condition translations and semantic interpretations.
   This routine should NOT be exported, it is used by MASTER-CATALOG-CLOSE-CHANGE only,
   and the arguments are myriad things selected from the guts of that context.
   Note that this routine can signal errors which will abort
   the satellite and master transaction scope which is assumed active at the time of this call.

   This function returns two values:
   1) A list of conses of file-system and file-pathname to represent deferred
      calls to file-system-set-read-only. They are deferred because they should not be done if an
      error is encountered.
   2) A CID-SET that is meant to represent the cid-set-basis against which changes were made
      in the satellite subsystem branch.  Some hocus-pocus may be necessary to ensure that this
      basis is registered as that for the satellite cid-set since we're zapping all kinds of cid-sets
      around in all kinds of transaction scopes.  Caller should make sure it ends up as the cid-set
      basis.  It reflects the VPB adds/removes and current-txn-cid.
   "
  (declare (ignorable unused-satellite-version))

  ;; NOTE: clueless readers of this inscrutable code should talk to Dave if they have questions.

  ;; WITH-VERSION (satellite-project-version) not used, becuase project-checkin will do it
  ;;(with-version (satellite-project-version)
  (debug-message
   4 "We're ready to do project-checkin on ~s, project ~s, version ~s, directory ~s"
   satellite-project-did
   (distributed-object-identifier satellite-project)
   (distributed-object-identifier satellite-project-version)
   (rfm::file-system-element-relative-path
    (rfm-project-root-directory satellite-project)))
  ;; Check in satellite changes.
  ;; If there are no useful changes in a subsystem/satellite, we abort the satellite
  ;; cset and the entire parent master cset (since we don't have true nested transactions).
  ;; However a checked-out file should have no changes, but there are other changes in the
  ;; cset, we do not abort the transaction.

  ;; ****************** NOTE *****************
  ;; There may be no-change conditions signalled, such as file modification time,
  ;; which really shouldn't occur.  RFM should check to see if they're different before
  ;; changing them.  We have several moderately spurious bugs in this regard.  Fix them,
  ;; don't code around them.
  (handler-bind
      ;; Convert the rfm warning to one of our own
      ((vi-no-change-condition
        (lambda (condition)
            (let* ((slot-name (vi-no-change-condition-vo-slot-name condition))
                   (versioned-object (vi-no-change-condition-vo-being-changed condition))
                   (object-name (ignore-errors (object-user-name versioned-object)))
                   (type (type-of versioned-object)))
              ;; We collect this stuff here instead of just collecting
              ;; the condition object because the handler will be
              ;; invoked in the dynamic context which has the correct
              ;; version scope.
              (push (list condition object-name type slot-name)
                    no-change-conditions))
            ;; Don't print the default CORE package warning, we'll translate for
            ;; the user further down in this routine.  It's rather undesirable to know
            ;; here that the condition was signalled with WARN and that the muffle-warning restart
            ;; can be used.  The no-change condition handling facilities definitely need some work.
            ;; (Please talk to Dave before you attempt this work...)
            (muffle-warning))))

    (multiple-value-setq
        (unused-satellite-version read-onlys cid-set-basis)
      (rfm:project-checkin
       satellite-project
       subdir-file-system
       change-context
       :mark-read-only t
       ;; The following two args ensure that workspace VPB is reflected during
       ;; checkin activity, since project-checking performs a WITH-VERSION.
       :cset-dids-to-add added-VPB-dids
       :cset-dids-to-remove removed-VPB-dids
       :branch satellite-project-branch
       :version satellite-project-version)))

  ;; Translate the queued/muffled conditions to something interpreted by
  ;; a ChangeSafe warning code.

  ;; ** NOTE ** NOTE ** NOTE ** NOTE ** NOTE ** NOTE ** NOTE ** NOTE ** NOTE ** NOTE **
  ;; The following 'conman-signal-warning' conditions aren't displayed if the whole transaction
  ;; is aborted, only if the transaction proceeds (because the current conman implementation
  ;; only reports the most severe message, not all messages).  When we log all messages
  ;; to the system console and under verbosity control, then we'll be a bit more verbose
  ;; client-side.
  (dolist (condition-stuff no-change-conditions)
    (destructuring-bind (condition name type slot-name) condition-stuff
      (cond
       (name
        (push name no-change-entity-names)
        (conman-signal-warning
         *cm-returns-warning-no-changes-found*
         "There were no changes in the ~a attribute of ~a ~a."
         slot-name                              ; e.g. 'text-contents
         type                                   ; e.g. file
         name))                                 ; e.g. "foo.c"
       (t
        (conman-signal-warning
         *cm-returns-warning-no-changes-found*
         "An entity which was supposed to change did not actually change. Condition:~%~a"
         condition)))))
  ;; Now, abort the whole satellite transaction, and the master txn, if nothing changed.
  ;; Sigh.  The 'abort-if-no-changes' thing uses catch/throw.  Probably was the wrong
  ;; thing to do.  JDT: talk to the lispheads to consider alternatives and learn lisp.
  (with-repository-txn-abort-handler (aborted-p abort-reason) ; catches no-change abort
    (unwind-protect
        (txn-context-abort-if-no-change *txn-context*)
      "resignal abort on no changes"
      (when aborted-p
        (conman-signal-error            ;resignal error and terminate/abort change transaction
         *cm-returns-error-no-changes-found*
         "There were no changes in the component cset for class ~a, transaction processing will be ~
         abandoned and no change-sets will be created.~
         ~@[~%Named entities observed to be without change: ~s~]"
         (or (ignore-errors (object-user-name satellite-project)) "<name unavailable>")
         no-change-entity-names
         ))))

    (values read-onlys cid-set-basis))


(defun master-catalog-process-subsystem-cset-updates-for-workspace
    (master-catalog master-repository-pathstring pc branch affected-subsystems workspace)
  "Perform promotion/demotion of cset alterations recorded in workspace VPB.
   This means we must alter satellite repository subsystem branches,
   and subsystem mirrors in the master repository.

   This logic is redundant with a subset of logic performed by master-catalog-close-change,
   however this routine is called when master-catalog-close-change is NOT called, because a master_change
   is being performed in a case where there was no active change context (but there were
   changes, only they were restricted to cset status changes, not file content changes).
   (It also doesn't have to promot the current 'transaction' cid, and it isn't doing project-checkin stuff)

   Return value: N/A"
  (pc-satellite-map-over-subsystem-repositories pc branch master-catalog master-repository-pathstring
    :satellite-txn-mode :read-write
    :reason "Add/remove csets in workspace to subsystem, i.e. master_change without an active change."
    :subsystem-list affected-subsystems
    :function (lambda (subsystem satellite-repository)
                  (let* ((satellite-project-did (subsystem-satellite-project-did subsystem))
                         (satellite-project (repository-resolve-distributed-identifier
                                             satellite-repository satellite-project-did))
                         (satellite-project-branch
                          (repository-resolve-distributed-identifier
                           satellite-repository (subsystem-satellite-project-branch-did subsystem)))
                         (satellite-project-version (branch-get-most-recent-version satellite-project-branch))
                         (subsystem-did (distributed-object-identifier subsystem)))
                    (subsystem-satellite-promote-csets
                     subsystem satellite-repository
                     ;; Add/remove any workspace cset changes which are to be promoted.
                     ;; NOTE: if PROMOTE isn't set, this has to be done by other subsystem-mapping
                     ;; logic as part of master_change.  Doing this here saves having to re-map
                     ;; satellites, but it's doing the same work.  Also note that we're not
                     ;; altering the subsystem mirror of csets here, just satellite repositories.

                     ;; Added change-sets
                     (repository-resolve-distributed-identifier-list
                      satellite-repository (workspace-existing-added-VPB-cset-dids-for-subsystem
                                            workspace subsystem-did))

                     ;; Removed change-sets
                     (repository-resolve-distributed-identifier-list
                      satellite-repository (workspace-existing-removed-VPB-cset-dids-for-subsystem
                                            workspace subsystem-did))

                     :satellite-subsystem-version satellite-project-version)
                    satellite-project))
    ))


;;;
;;; And so we revisit the eternally annoying method of decorating change-set objects.
;;; Doing it in a hook after the main body of WITH-VM-TXN is very inconvenient, and makes
;;; the notion of binding a variable like *change-set* for the duration of its body and fixing
;;; the abort handler scenarios.
;;;
;;; However one good thing about deferring the logic until the post-body hook is that we're
;;; fairly certain we're done creating satellite csets, and that the master<->satellite
;;; cset relationship information is fairly complete.  This is good (though it assumes
;;; nobody creates a satellite cset in the post-txn hook).
;;;
;;; So for today, this model continues to live, even though I don't like it.  Expedience wins.
;;;

(defun master-catalog-promote-transaction-cset (master-catalog description pc pc-branch)
  "The current master cset is a 'transaction' which is causing alteration to PC content.
   Reflect this 'transaction' by promoting the current master cset into
   the PC appropriate version.  This particular logic must be performed in the transaction postlude
   since the cset is still being created.

   This function should be called by every routine in CMCTL which alters the content of a PC.

   **** WARNING **** this routine works via change transaction postlude hooks.
   Effects are not observed until change-set creation is complete for the current master change-set
   in progress.

   Ensure that DESCRIPTION (and later abstract, *FINISH*) are put into the change-set being created."

  ;; Manage the affected satellite repositories

  ;; Whether or not a change-close was done, it is *this* master repository change-set (in progress)
  ;; which is of interest, and that we wish to add to our various lists of active csets in the
  ;; versioned pc contents.  Other csets done via change_add, etc.., modify the pc cset content lists
  ;; in their own routines.
  (master-catalog-queue-cset-hook master-catalog
    ;; It'd be nice to just have this be a statically compiled function, but we're using the closure
    ;; to contain key information passed to the calling function.
    (lambda (master-catalog change-set)
        (master-catalog-promote-pc-changes-aux master-catalog change-set description pc pc-branch)))
  master-catalog)

(defun master-catalog-promote-transaction-cset-to-subsystems-users (master-catalog
                                                                    description
                                                                    subsystems)
  "For each subsystem in SUBSYSTEMS, call MASTER-CATALOG-PROMOTE-TRANSACTION-CSET
   for all of the PC BRANCHes which use the subsystem."
  (let ((branches-to-hack nil))
    (dolist (subsystem subsystems)
      (subsystem-map-over-subscribers subsystem
        (lambda (subsystem-subscriber)
            (pushnew (subsystem-subscriber-pc-branch subsystem-subscriber)
                     branches-to-hack))))
    (dolist (branch branches-to-hack)
      (master-catalog-promote-transaction-cset master-catalog description
                                               (branch-owning-project branch)
                                               branch))))

(defun master-catalog-promote-pc-changes-aux (master-catalog change-set description pc pc-branch)
  "This function is called by a hook during the master repository transaction postlude.
   We decorate CHANGE-SET, which is a master repository change-set, with descriptive information
   such as the change DESCRIPTION, and promote the change-set into the product configuration branch
   tip described by PC-BRANCH.

   NOTE: because of the current structure of WITH-VM-TXN, you should not call TXN-CONTEXT-ABORT-IF-NO-CHANGE
   from within hooks.

   Return master-catalog."
  #+allegro (declare (:fbound pc-promote-master-cset-into-version-scope pc-promote-master-cset-as-content))
  (when description
    (set-described-object-text change-set description))
  ;; Promote CHANGE-SET into the PC-BRANCH to view contents (representing *our* versioned view of user
  ;; versioned view.
  ;; We used to promote the master cset into a master-cset list in the PC, but we don't do that any more.
  (pc-promote-master-cset-into-version-scope pc pc-branch change-set)
  (debug-message 3 "master-catalog-promote-pc-changes-aux: Current pc-branch version contents: ~s"
                 (vm::version-get-active-resident-cids (branch-get-most-recent-version pc-branch)))
  master-catalog)


#||
(defun master-catalog-get-class-for-satellite-cset-did (master-catalog satellite-cset-did)
  "Return the class for a given satellite cset did."
  (loop for satellite-project-ref in (master-catalog-satellite-project-refs master-catalog)
        do
        (when (satellite-project-ref-contains-cset-did satellite-project-ref
                                                       satellite-cset-did)
          (return-from master-catalog-get-class-for-satellite-cset-did
            (satellite-project-ref-class-name satellite-preject-ref)))))
||#

(defun master-catalog-update-file-system (master-catalog master-repository-name
                                          file-system
                                          file-system-name
                                          pc branch
                                          old-timestamp
                                          new-timestamp
                                          &key (report-only nil)
                                               (report-file-system file-system)
                                               change-context
                                               VPB-old-added-satellite-cset-dids
                                               VPB-old-removed-satellite-cset-dids
                                               VPB-new-added-satellite-cset-dids
                                               VPB-new-removed-satellite-cset-dids)
  "Incrementally update a file system from the master catalog.

   It is assumed that the file system in question was published using
   old-timestamp, vpb-old-added-satellite-cset-dids, and vpb-old-removed-cset-dids
   and we wish to update the file system in question to
   new-timestamp, vpb-new-added-satellite-cset-dids, and vpb-new-removed-satellite-cset-dids

   FILE-SYSTEM should be rooted at the reference directory or workspace root.
   FILE-SYSTEM-NAME should be a string like \"workspace\" or \"reference directory\"
   which is used for noise.

   REPORT-FILE-SYSTEM, if present, is where the noise goes to."
  #+allegro (declare (:fbound pc-partition-subsystems))
  (let ((performed-three-way-merge nil)
        (merge-conflict-count       0)
        (performed-binary-merge    nil))

    (pc-partition-subsystems
     pc branch old-timestamp new-timestamp
     :VPB-old-added-satellite-cset-dids VPB-old-added-satellite-cset-dids
     :VPB-old-removed-satellite-cset-dids VPB-old-removed-satellite-cset-dids
     :VPB-new-added-satellite-cset-dids VPB-new-added-satellite-cset-dids
     :VPB-new-removed-satellite-cset-dids VPB-new-removed-satellite-cset-dids
     :receiver
     (lambda (subsystems-unchanged subsystems-added subsystems-removed subsystems-changed)
         (debug-message 3 "MASTER-CATALOG-UPDATE-FILE-SYSTEM~
                       ~%subsystems-unchanged ~s~
                       ~%subsystems-added ~s~
                       ~%subsystems-removed ~s~
                       ~%subsystems-changed ~s"
                        subsystems-unchanged subsystems-added subsystems-removed subsystems-changed)


         (file-system-note-progress report-file-system
                                    (format nil "~:[Updating~;Report of work necessary to update~] ~a."
                                            report-only file-system-name)
                                    nil)

         (when subsystems-unchanged
           (with-cm-master-metaversion (new-timestamp)
             (file-system-note-progress report-file-system
                                        (format nil " Subsystem~? ~:[are~;is~] unchanged, no update is necessary."
                                                "~#[~; ~a~;s ~a and ~a~:;s~@{~#[~; and~] ~a~^,~}~]"
                                                (mapcar #'subsystem-name subsystems-unchanged)
                                                (= (length subsystems-unchanged) 1)) nil)))

         ;; When a subsystem has been removed, we delete it from the reference area.
         (when subsystems-removed
           (with-cm-master-metaversion (old-timestamp)
             (file-system-note-progress report-file-system
                                        (format nil " Subsystem~? ~:[are~;is~] obsolete and will be removed."
                                                "~#[~; ~a~;s ~a and ~a~:;s~@{~#[~; and~] ~a~^,~}~]"
                                                (mapcar #'subsystem-name subsystems-removed)
                                                (= (length subsystems-removed) 1)) nil))
           (unless report-only
             (dolist (subsystem-removed subsystems-removed)
               (file-system-delete-directory
                file-system
                (with-cm-master-metaversion (old-timestamp)
                  (subsystem-relative-subdirectory subsystem-removed))
                :recursive t
                :force t))))

         ;; When a subsystem has been added, we unconditionally publish it to the
         ;; reference area.  There should not be any files in that subsystem there
         ;; currently, so there is no chance of optimizing the publishing.
         (when subsystems-added
           (with-cm-master-metaversion (new-timestamp)
             (file-system-note-progress report-file-system
                                        (format nil " Subsystem~? ~:[are~;is~] new."
                                                "~#[~; ~a~;s ~a and ~a~:;s~@{~#[~; and~] ~a~^,~}~]"
                                                (mapcar #'subsystem-name subsystems-added)
                                                (= (length subsystems-added) 1))
                                        nil)
             (unless report-only
               (dolist (added subsystems-added)
                 (master-catalog-extract-subsystem-files-to-disk master-catalog
                                                                 master-repository-name
                                                                 added
                                                                 file-system
                                                                 "Updating file system directory."
                                                                 new-timestamp
                                                                 :report-file-system report-file-system
                                                                 :clean t)))))

         (when subsystems-changed
           (dolist (subsystem subsystems-changed)
             (with-cm-master-metaversion (new-timestamp)
               (file-system-note-progress report-file-system
                                          (format nil " Subsystem ~a in ~a will be updated."
                                                  (subsystem-name subsystem)
                                                  file-system-name) nil)
               (multiple-value-bind (master-catalog
                                     subsystem-three-way-merge
                                     subsystem-conflict-count
                                     subsystem-binary-merge)
                   (let ((subsystem-did (distributed-object-identifier subsystem)))
                     (master-catalog-extract-changed-subsystem-files-to-disk
                      master-catalog
                      master-repository-name
                      subsystem
                      old-timestamp ;; timestamp of old publishing
                      new-timestamp       ;; timestamp of now
                      file-system
                      "Update file system."
                      :report-only report-only
                      :report-file-system report-file-system
                      :change-context change-context
                      :VPB-old-added-satellite-cset-dids
                      (cdr (assoc subsystem-did VPB-old-added-satellite-cset-dids))
                      :VPB-old-removed-satellite-cset-dids
                      (cdr (assoc subsystem-did VPB-old-removed-satellite-cset-dids))
                      :VPB-new-added-satellite-cset-dids
                      (cdr (assoc subsystem-did VPB-new-added-satellite-cset-dids))
                      :VPB-new-removed-satellite-cset-dids
                      (cdr (assoc subsystem-did VPB-new-removed-satellite-cset-dids))
                      ))
                 (declare (ignore master-catalog))
                 (setq performed-three-way-merge (or performed-three-way-merge subsystem-three-way-merge))
                 (incf merge-conflict-count subsystem-conflict-count)
                 (setq performed-binary-merge    (or performed-binary-merge subsystem-binary-merge))
                 ))))))
    (values performed-three-way-merge
            merge-conflict-count
            performed-binary-merge)))

(defun master-catalog-get-satellite-project-ref-for-satellite-cset-did
       (master-catalog satellite-cset-did &optional project-refs)
  "Return the SATELLITE-PROJECT-REF for a given satellite cset did."
  (let* ((repos (did/repository satellite-cset-did))
         (test (lambda (satellite-project-ref)
                   (equal repos (satellite-project-ref-repository-id satellite-project-ref))))
         (project-refs (or project-refs (master-catalog-satellite-project-refs master-catalog))))
    (if (consp project-refs)
        (dolist (repos-proj project-refs)
          (when (string= (car repos-proj) repos)
            (return (cdr repos-proj))))
      (vi-stream-find-if test project-refs))))

(defun master-catalog-get-class-name-for-satellite-cset-did
       (master-catalog satellite-cset-did &optional project-refs)
  "Return the class name for the class associated with a satellite cset DID."
  (satellite-project-ref-name
   (master-catalog-get-satellite-project-ref-for-satellite-cset-did master-catalog satellite-cset-did
                                                                    project-refs)))

(defun master-catalog-satellite-repository-db-files (master-catalog)
  "Returns the pathnames of all satellite repository files known about by the MASTER-CATALOG."
  (let ((collection nil))
    (mapc (lambda (satellite-project-ref)
              (pushnew (satellite-project-ref-repository-pathname satellite-project-ref)
                       collection
                       :test #'equal))
          (master-catalog-get-satellite-project-ref-list master-catalog))
    collection))

;;; I think this should have gone in workspace-master.lsp but certain
;;; macros it needs arn't defined at that point.

(defun master-catalog-workspace-redundant-vpb-changes (workspace master-repository)
  "After ws_update or ws_set, certain changes might have been added or removed
   from WORKSPACE's baseline.  These changes might also be added or removed from
   the workspace's virtual private branch.  This function identifies That activity
   in the VPB which has become redundant.

   Two values are returned the redundant added changes and teh redundant
   removed changes.  Both of these are alists similar instructore to those in
   the WORKSPACE's added and removed subsystem cset tuples slots."
  (declare (values irrelevant-adds irrelevant-removes))
  (let* ((baseline (workspace-baseline-timestamp workspace))
         (added (workspace-added-subsystem-cset-tuples workspace))
         (removed (workspace-removed-subsystem-cset-tuples workspace))
         (subsystem-dids (union (mapcar #'car added) (mapcar #'car removed)))
         (redundant-adds nil)
         (redundant-removes nil))
    (with-cm-master-metaversion (baseline)
      (loop for subsystem-did in subsystem-dids
            for subsystem = (repository-resolve-distributed-identifier master-repository
                                                                       subsystem-did)
            for subsystem-changes = (subsystem-get-satellite-cset-did-list subsystem)
            for added-satellite-cset-dids = (cdr (assoc subsystem-did added))
            for removed-satellite-cset-dids = (cdr (assoc subsystem-did removed))
            do
            (push (cons subsystem-did
                        (set-difference added-satellite-cset-dids
                                        (intersection added-satellite-cset-dids
                                                      subsystem-changes)))
                  redundant-adds)
            (push (cons subsystem-did
                        (intersection removed-satellite-cset-dids subsystem-changes))
                  redundant-removes)))
    (values redundant-adds redundant-removes)))
||#

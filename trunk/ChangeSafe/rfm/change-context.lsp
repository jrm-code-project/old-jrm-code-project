;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright (c) 1999 Content Integrity, Inc.
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
;;;; File Name:     change-context.lsp
;;;; Author:        Dave Tenny
;;;;
;;;; Module Description:
;;;;
;;;; A CHANGE-CONTEXT describes the current state of a change in progress.
;;;; A change in progress isn't necessarily in an active change transaction
;;;; as per core semantics.  A change often begins when files are checked out.
;;;; In some cases, no formal checkout process is used, and a change context
;;;; doesn't have a lifetime much beyond that of the change-transaction.
;;;;
;;;; A change context describes the current state an intended change,
;;;; including the eventual change-set name and description, though these
;;;; items may not be known until the last moment during change-set creation.
;;;;
;;;; The change-context describes the operations which are to be
;;;; performed on the system.  This is essentially an advance
;;;; declaration of change intent.  Additional examples include file
;;;; additions, renames, changes, and deletions.  Over time, we may
;;;; add many other change operations, such as change-set addition and
;;;; removal to versions/branches, user/security modifications,
;;;; etc.
;;;;
;;;; There are several ways to build up and modify change-contexts.  In the
;;;; standard developer mode you would build up a change-context through
;;;; successive operations such as CHECKOUT, branch merge and split
;;;; operations, etc.  In other modes the change-context is built pretty
;;;; much automatically by examining the file system, detecting changes,
;;;; and doing a "snapshot" style checkin.
;;;;
;;;; TBD: checkout-record semantics...  Theoretically each operation which
;;;; mutates a change-context results in a CHECKOUT-RECORD which describes
;;;; the parameters of the requested operation.  Not implemented now.
;;;;
;;;; As the change-context evolves, at some point you are ready to
;;;; create (and perhaps promote) a change-set.  You do this by doing
;;;; a some for of 'checkin' operation on the change-context.
;;;;
;;;; A change-context also specifies the view of a project to which
;;;; you are making changes (thus the 'context' of change-context).
;;;;
;;;; CHANGE-CONTEXTS/CHECKOUT-RECORDS DO NOT STORE ALL CHANGE TO BE
;;;; APPLIED.  For instance, when you checkout a file, your changes to
;;;; the file are on disk until you check in the change-context.  When
;;;; you rename a file, the file name remains unchanged in the
;;;; repository until you checkin the change-context.  However on
;;;; disk, the file will have the new name.
;;;;
;;;; We will probably eventually add the ability to cache materials used
;;;; for change on the server (either briefly on disk, or less briefly in
;;;; special 'cache repositories', never production repositories).  This is
;;;; useful when you have work in progress, want to store it centrally, and
;;;; work on it until the point where you say 'check in the client or
;;;; central image of this work'.  It would be an essential capability for
;;;; making WEBDAV compatible changes from things like palm pc devices.
;;;; For now, these capabilities don't exist.
;;;;
;;;; Change-context objects are structured so that you can store them in
;;;; any repository and use them transiently, outside of a DB transaction.
;;;; This means that references to versioned entities are by DID, typically
;;;; in string form, and not direct database references.
;;;;
;;;; CHANGE-CONTEXT INFORMATION IS NOT VERSIONED, AND CHANGES TO
;;;; CHANGE-CONTEXTS SHOULD NOT BE IMPORTED/EXPORTED, OR CAUSE CREATION OF
;;;; CHANGE-SETS.  (There are subtle implications here for any approval
;;;; processes which may be associated with change-contexts in advance
;;;; of change-set creation, avoid change-context-level approval
;;;; modes where possible.  It's not too bad however, and can be done,
;;;; it just isn't meant to be a distributed or versioned approval process.)
;;;;
;;;; Astore's limited and completely stupid support for allowing objects
;;;; to be persistent or transient comes into consideration here. For now,
;;;; these objects are primarily oriented to transient use, and stored by
;;;; using astore object encoding/decoding support for references and
;;;; a proxy persistent-change-context object.
;;;;
;;;; Where they're stored will evolve.  For now, they're stored in the server
;;;; session database, later we may choose to store them in the repository
;;;; where the intended change is to take place.  That's all TBD and highly
;;;; likely to change.
;;;;
;;;; Note that a CHANGE-CONTEXT is different from an RFM-SESSION-CONTEXT.
;;;; The former identifies parameters for creation of a change-set.  The latter
;;;; identifies parameters for meaningful interaction of user interfaces with
;;;; particular projects/repositories, etc..  Finally, both are different
;;;; from a SERVER::SESSION object, which is strictly in the business of
;;;; maintaining logical session information which span client physical
;;;; connections to the server over time.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CSF/REPOSITORY-FILE-MANAGEMENT")

(proclaim (standard-optimizations))

;;; API's exported from this module.
(eval-when (:load-toplevel :execute)
  (export '(
            change-context
            persistent-change-context
            change-context/create-restore

            change-context-fileadd
            change-context-filechange
            change-context-fileremove
            change-context/pathname

            change-context/add-file
            change-context/add-file-no-log
            change-context/add-file-undo
            change-context/add-file-undo-no-log
            change-context/add-log-entry
            change-context/adds-file?
            change-context/affected-project-did
            change-context/affected-project-dids
            change-context/applies-to-project?
            change-context/branch-did
            change-context/change-file
            change-context/change-file-undo
            change-context/change-in-progress?
            change-context/changes-file?
            change-context/client-directory-pathname
            change-context/client-file-system-platform
            change-context/cset-abstract
            change-context/cset-description
            change-context/cset-name
            change-context/file-additions
            change-context/file-additions-push-count
            change-context/file-additions-undone?
            change-context/file-changes
            change-context/file-did
            change-context/file-removals
            change-context/file-renames
            change-context/find-file-additions
            change-context/find-file-changes
            change-context/find-file-removals
            change-context/find-file-renamed-to
            change-context/find-file-renames
            change-context/log
            change-context/persistent-change-context
            change-context/project-did
            change-context/remove-file
            change-context/remove-file-undo
            change-context/rename-file
            change-context/rename-file-undo
            change-context/repository-id
            change-context/version-did-string

            change-context->persistent-change-context

            #||                         ;

            ;;
            cc-file-base-change-applies-to-project-p
            cc-file-base-change-affected-project-did
            cc-file-base-change-other-stuff

            ;; CC-FILEADD
            cc-fileadd
            cc-fileadd-create
            cc-fileadd-pathname         ;path-spec, string or pathname
            cc-fileadd-content-type
            ;; CC-FILEREMOVE
            cc-fileremove
            cc-fileremove-create
            cc-fileremove-file-did
            ;; CC-NEW-FILEREMOVE
            cc-new-fileremove
            cc-new-fileremove-create
            cc-new-fileremove-pathname
            ;; CC-FILERENAME
            cc-filerename
            cc-filerename-create
            cc-filerename-file-did
            cc-filerename-new-pathname
            ;; CC-NEW-FILERENAME
            cc-new-filerename
            cc-new-filerename-create
            cc-new-filerename-old-pathname
            cc-new-filerename-new-pathname
            ;; CC-FILECHANGE
            cc-filechange
            cc-filechange-create
            cc-filechange-file-did

            ;; CHANGE-CONTEXT
            change-context
            change-context-create
            change-context-create-restore ;create from a persistent-change-context
            change-context-change-in-progress?
            change-context-set-description ;update the description
            change-context-cset-name    ;read the intended name of the change-set to be created
            change-context-cset-description ;read the description value
            change-context-cset-abstract ;read the abstract value
            change-context-change-file  ;note intent to change a existing repository file
            change-context-change-file-undo
            change-context-add-log-entry
            change-context-add-file     ;note intent to add a new file to the reposiotry upon checkin
            change-context-add-file-no-log ;note intent to add a new file to the reposiotry upon checkin
            change-context-add-file-undo
            change-context-add-file-undo-no-log
            change-context-remove-file  ;note intent to remove an existing file from the reposiotry upon checkin
            change-context-remove-file-undo
            change-context-rename-new-file
            change-context-rename-file  ;note intent to rename a file in the reposiotry upon checkin
            change-context-rename-file-undo
            change-context-file-additions ; use only as an accessor outside this module
            change-context-file-removals ; use only as an accessor outside this module
            change-context-file-renames ; use only as an accessor outside this module
            change-context-file-changes ; use only as an accessor outside this module
            change-context-find-file-additions
            change-context-find-file-changes
            change-context-find-file-removals
            change-context-find-file-renames
            change-context-find-file-renamed-to
            change-context-file-additions-push-count
            change-context-file-additions-undone?
            change-context-affected-project-dids
            ;; persistent-change-context
            persistent-change-context
            persistent-change-context-create ;create from a transient change-context
            persistent-change-context-update ;update existing instance with content from transient instance
            ||#

            )))

(defclass change-context-base-change ()
  ;; The DID of the subsystem project which is affected by this change
  ((affected-project-did :initarg :affected-project-did
                         :initarg :project-did
                         :initform (error "Required initarg :affected-project-did omitted.")
                         :reader change-context/affected-project-did
                         :type distributed-identifier)
   ;; The OTHER-STUFF slot is provided so that other applications which
   ;; use RFM:CHANGE-CONTEXTs, etc. can store information specific to
   ;; their application.  One example of this is storing the CONMAN
   ;; SUBSYSTEM which contains the file being changed.
   (other-stuff :initarg :other-stuff
                :initform (error "Required initarg :other-stuff omitted.")
                :accessor change-context/other-stuff))
  ;; REMEMBER that when you add slots to here, you need to augmnent
  ;; the methods for ASTORE::ENCODE-IN-DATABASE and
  ;; ASTORE::DECODE-FROM-DATABASE for every subclass.  This sucks.

  (:metaclass persistent-standard-class)
  (:schema-version 0)
  )

(defmethod change-context/applies-to-project? ((cc change-context-base-change)
                                               (project-did distributed-identifier))
  (let ((my-project (change-context/affected-project-did cc)))
    (or (null my-project)
        (eq my-project project-did))))

(defmethod change-context/applies-to-project? ((cc change-context-base-change) (project rfm-project))
  (change-context/applies-to-project? cc (distributed-object-identifier project)))

(defmethod objects-equalp ((left change-context-base-change) (right change-context-base-change))
  "This is the default case.  If the types aren't the same, they can't be object-equalp."
  nil)

(defclass change-context-filechange (change-context-base-change)
  ((file-did :initarg :file-did
             :initform (error "Required initarg :file-did omitted.")
             :reader change-context/file-did
             :type distributed-identifier))
  (:documentation
   "A change-context representation for a file modification.
      This class represents both the client pathname (in some form compatible with PROJECT-CHECKIN)
      and a did of the project to which the file is to be added.
      This entity may be used both persistently and transiently, and relies upon
      astore::encode-in-database to do the work.")
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defmethod print-object ((change-context-filechange change-context-filechange) stream)
  (print-unreadable-object (change-context-filechange stream :type t)
    (format stream "~a ~a"
            (change-context/affected-project-did change-context-filechange)
            (change-context/file-did change-context-filechange))))

(defmethod objects-equalp ((left change-context-filechange) (right change-context-filechange))
  (and (eq (change-context/file-did left) (change-context/file-did right))
       (eq (change-context/affected-project-did left)
           (change-context/affected-project-did right))
       (objects-equalp
        (change-context/other-stuff left)
        (change-context/other-stuff right))))

(defmethod change-context/changes-file? ((change-context-filechange change-context-filechange) file-did)
  (eq file-did (change-context/file-did change-context-filechange)))

(defmethod serialize ((object change-context-filechange) stream symbol-table)
  (write-byte pstore::serialization-code/change-context-filechange stream)
  (write-fixnum (symbol-table/intern-did symbol-table (change-context/affected-project-did object)) stream)
  (serialize (change-context/other-stuff object) stream symbol-table)
  (write-fixnum (symbol-table/intern-did symbol-table (change-context/file-did object)) stream))

(defmethod deserialize-dispatch ((code (eql serialization-code/change-context-filechange)) stream symbol-table)
  (let ((project-did (symbol-table/resolve-did symbol-table (read-fixnum stream)))
        (other-stuff (deserialize-dispatch (read-byte stream) stream symbol-table))
        (file-did    (symbol-table/resolve-did symbol-table (read-fixnum stream))))
    (make-instance 'change-context-filechange
                   :affected-project-did project-did
                   :other-stuff other-stuff
                   :file-did file-did)))

(defclass change-context-fileadd (change-context-base-change)
                                        ;canonicalized pathname representing client path with file to add.
                                        ; pathname is relative to the project root.

  ((pathname :initarg :pathname
             :initform (error "Required initarg :pathname omitted.")
             :reader change-context/pathname
             :type pathname)
                                        ;any file content-type explicitly declared by user: :text or :binary
   (content-type :initarg :content-type
                 :initform (error "Required initarg :content-type omitted.")
                 :reader change-context/content-type))
  (:documentation
   "A change-context representation for a file addition.
    This class represents both the client pathname (in some form compatible with PROJECT-CHECKIN)
    and a did of the project to which the file is to be added.
    This entity may be used both persistently and transiently, and relies upon
    astore::encode-in-database to do the work.

    ****NOTE**** this needs to be encapsulated in the CORE package, or worked around.")
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defmethod objects-equalp ((left change-context-fileadd) (right change-context-fileadd))
  (and (equal (change-context/pathname left) (change-context/pathname right))
       (eq (change-context/affected-project-did left)
           (change-context/affected-project-did right))
       (objects-equalp
        (change-context/other-stuff left)
        (change-context/other-stuff right))))

(defmethod print-object ((change-context-fileadd change-context-fileadd) stream)
  (print-unreadable-object (change-context-fileadd stream :type t)
    (format stream "~a ~s"
            (change-context/affected-project-did change-context-fileadd)
            (change-context/pathname change-context-fileadd))))

(defmethod change-context/adds-file? ((change-context-fileadd change-context-fileadd) file-name project-did)
  (let ((answer (and (eq project-did (change-context/affected-project-did change-context-fileadd))
                     (equal file-name (change-context/pathname change-context-fileadd)))))
    (debug-message 5 "file-name host ~s" (pathname-host file-name))
    (debug-message 5 "file-name device ~s" (pathname-device file-name))
    (debug-message 5 "file-name directory ~s" (pathname-directory file-name))
    (debug-message 5 "file-name name ~s" (pathname-name file-name))
    (debug-message 5 "file-name type ~s" (pathname-type file-name))
    (debug-message 5 "file-name version ~s" (pathname-version file-name))
    (debug-message 5 "cc host ~s" (pathname-host (change-context/pathname change-context-fileadd)))
    (debug-message 5 "cc device ~s" (pathname-device (change-context/pathname change-context-fileadd)))
    (debug-message 5 "cc directory ~s" (pathname-directory (change-context/pathname change-context-fileadd)))
    (debug-message 5 "cc name ~s" (pathname-name (change-context/pathname change-context-fileadd)))
    (debug-message 5 "cc type ~s" (pathname-type (change-context/pathname change-context-fileadd)))
    (debug-message 5 "cc version ~s" (pathname-version (change-context/pathname change-context-fileadd)))
    answer))

;;; ***HACK***: ENCAPSULATE ASTORE:: ENCODING INTERFACES IN THE CORE PACKAGE.
;;; PLEASE CHECK WITH DAVE BEFORE OVERLOADING ENCODE-IN-DATABASE...
;;; (After lecturing people about using astore outside CORE, here I am doing it,
;;; though I have provided encapsulations for those other traditional and bogus references to astore
;;; at this point, this particular thing still poses a problem)

(defmethod serialize ((object change-context-fileadd) stream symbol-table)
  (write-byte pstore::serialization-code/change-context-fileadd stream)
  (write-fixnum (symbol-table/intern-did symbol-table (change-context/affected-project-did object)) stream)
  (serialize (change-context/other-stuff object) stream symbol-table)
  (serialize (change-context/pathname object) stream symbol-table)
  (serialize (change-context/content-type object) stream symbol-table))

(defmethod deserialize-dispatch ((code (eql pstore::serialization-code/change-context-fileadd)) stream symbol-table)
  "Make a transient cc-fileadd object given it's representation as returned by the second value
   of astore::encode-in-database on a cc-fileadd object."
  (let ((project-did (symbol-table/resolve-did symbol-table (read-fixnum stream)))
        (other-stuff (deserialize-dispatch (read-byte stream) stream symbol-table))
        (pathname    (deserialize-dispatch (read-byte stream) stream symbol-table))
        (content-type (deserialize-dispatch (read-byte stream) stream symbol-table)))
    (make-instance 'change-context-fileadd
                   :affected-project-did project-did
                   :other-stuff other-stuff
                   :pathname pathname
                   :content-type content-type)))


(defclass change-context-fileremove (change-context-base-change)
  ((file-did :initarg :file-did
             :initform (error "Required initarg :file-did omitted.")
             :reader change-context/file-did
             :type distributed-identifier))
  (:documentation
   "A change-context representation for the removal of a  file.
    This class represents both the client pathname (in some form compatible with PROJECT-CHECKIN)
    and a did of the project to which the file is to be removed.
    This entity may be used both persistently and transiently, and relies upon
    astore::encode-in-database to do the work.

    ****NOTE**** this needs to be encapsulated in the CORE package, or worked around.")
  (:metaclass persistent-standard-class)
  (:schema-version 0)
)

(defmethod print-object ((change-context-fileremove change-context-fileremove) stream)
  (print-unreadable-object (change-context-fileremove stream :type t)
    (format stream "~a ~a"
            (change-context/affected-project-did change-context-fileremove)
            (change-context/file-did change-context-fileremove))))

(defmethod objects-equalp ((left change-context-fileremove) (right change-context-fileremove))
  (and (eq (change-context/file-did left) (change-context/file-did right))
       (eq (change-context/affected-project-did left)
           (change-context/affected-project-did right))
       (objects-equalp
        (change-context/other-stuff left)
        (change-context/other-stuff right))))

(defmethod change-context/removes-file-p ((change-context-fileremove change-context-fileremove) file-did)
  (eq file-did (change-context/file-did change-context-fileremove)))

(defmethod serialize ((object change-context-fileremove) stream symbol-table)
  (write-byte pstore::serialization-code/change-context-fileremove stream)
  (write-fixnum (symbol-table/intern-did symbol-table (change-context/affected-project-did object)) stream)
  (serialize (change-context/other-stuff object) stream symbol-table)
  (write-fixnum (symbol-table/intern-did symbol-table (change-context/file-did object)) stream))

(defmethod deserialize-dispatch ((code (eql serialization-code/change-context-fileremove)) stream symbol-table)
  (let ((project-did (symbol-table/resolve-did symbol-table (read-fixnum stream)))
        (other-stuff (deserialize-dispatch (read-byte stream) stream symbol-table))
        (file-did    (symbol-table/resolve-did symbol-table (read-fixnum stream))))
    (make-instance 'change-context-fileremove
                   :affected-project-did project-did
                   :other-stuff other-stuff
                   :file-did file-did)))


(defclass change-context-new-fileremove (change-context-base-change)
  ((pathname :initarg :pathname
             :initform (error "Required initarg :pathname omitted.")
             :reader change-context/pathname
             :type pathname))
  (:documentation
   "A change-context representation for the removal of a fileadded file.
      This entity may be used both persistently and transiently, and relies upon
      astore::encode-in-database to do the work.

      ****NOTE**** this needs to be encapsulated in the CORE package, or worked around.")
  (:metaclass persistent-standard-class)
  (:schema-version 0)
)

(defmethod print-object ((change-context-new-fileremove change-context-new-fileremove) stream)
  (print-unreadable-object (change-context-new-fileremove stream :type t)
    (format stream "~a ~a"
            (change-context/affected-project-did change-context-new-fileremove)
            (change-context/pathname change-context-new-fileremove))))

(defmethod objects-equalp ((left change-context-new-fileremove) (right change-context-new-fileremove))
  (and (equal (change-context/pathname left) (change-context/pathname right))
       (eq (change-context/affected-project-did left)
           (change-context/affected-project-did right))
       (objects-equalp
        (change-context/other-stuff left)
        (change-context/other-stuff right))))

(defmethod serialize ((object change-context-new-fileremove) stream symbol-table)
  (write-byte pstore::serialization-code/change-context-new-fileremove stream)
  (write-fixnum (symbol-table/intern-did symbol-table (change-context/affected-project-did object)) stream)
  (serialize (change-context/other-stuff object) stream symbol-table)
  (serialize (change-context/pathname object) stream symbol-table))

(defmethod deserialize-dispatch ((code (eql pstore::serialization-code/change-context-new-fileremove)) stream symbol-table)
  "Make a transient cc-new-fileremove object given it's representation as returned by the second value
   of astore::encode-in-database on a cc-new-fileremove object."
  (let ((project-did (symbol-table/resolve-did symbol-table (read-fixnum stream)))
        (other-stuff (deserialize-dispatch (read-byte stream) stream symbol-table))
        (pathname    (deserialize-dispatch (read-byte stream) stream symbol-table)))
    (make-instance 'change-context-new-fileremove
                   :affected-project-did project-did
                   :other-stuff other-stuff
                   :pathname pathname)))


(defclass change-context-filerename (change-context-base-change)
  ((file-did :initarg :file-did
             :initform (error "Required initarg :file-did omitted.")
             :reader change-context/file-did
             :type distributed-identifier)
   (new-pathname :initarg :new-pathname
             :initform (error "Required initarg :pathname omitted.")
             :reader change-context/new-pathname
             :type pathname))
  (:documentation
   "A change-context representation for the renaming of a file.
      This class represents both the new namestring (in some form compatible with PROJECT-CHECKIN)
      and the did of the project containing the file is to be renamed.
      This entity may be used both persistently and transiently, and relies upon
      astore::encode-in-database to do the work.

      ****NOTE**** this needs to be encapsulated in the CORE package, or worked around.")
  (:metaclass persistent-standard-class)
  (:schema-version 0)
)

(defmethod print-object ((change-context-filerename change-context-filerename) stream)
  (print-unreadable-object (change-context-filerename stream :type t)
    (format stream "~a ~a -> ~a"
            (change-context/affected-project-did change-context-filerename)
            (change-context/file-did change-context-filerename)
            (change-context/new-pathname change-context-filerename))))

(defmethod objects-equalp ((left change-context-filerename) (right change-context-filerename))
  (and (eq (change-context/file-did left) (change-context/file-did right))
       (equal (change-context/new-pathname left) (change-context/new-pathname right))
       (eq (change-context/affected-project-did left)
           (change-context/affected-project-did right))
       (objects-equalp
        (change-context/other-stuff left)
        (change-context/other-stuff right))))

(defmethod change-context/renames-file-p ((change-context-filerename change-context-filerename) file-did)
  (eq file-did (change-context/file-did change-context-filerename)))

(defmethod change-context/renames-file-to-p ((change-context-filerename change-context-filerename)
                                            new-file-name project-did)
  (and (eq project-did (change-context/affected-project-did change-context-filerename))
       (equal new-file-name (change-context/new-pathname change-context-filerename))))

(defmethod serialize ((object change-context-filerename) stream symbol-table)
  (write-byte pstore::serialization-code/change-context-filerename stream)
  (write-fixnum (symbol-table/intern-did symbol-table (change-context/affected-project-did object)) stream)
  (serialize (change-context/other-stuff object) stream symbol-table)
  (write-fixnum (symbol-table/intern-did symbol-table (change-context/file-did object)) stream)
  (serialize (change-context/new-pathname object) stream symbol-table))

(defmethod deserialize-dispatch ((code (eql pstore::serialization-code/change-context-filerename)) stream symbol-table)
  (let ((project-did (symbol-table/resolve-did symbol-table (read-fixnum stream)))
        (other-stuff (deserialize-dispatch (read-byte stream) stream symbol-table))
        (file-did    (symbol-table/resolve-did symbol-table (read-fixnum stream)))
        (pathname    (deserialize-dispatch (read-byte stream) stream symbol-table)))
    (make-instance 'change-context-filerename
                   :affected-project-did project-did
                   :other-stuff other-stuff
                   :file-did file-did
                   :new-pathname pathname)))

(defclass change-context-new-filerename (change-context-base-change)
  ((old-pathname :initarg :old-pathname
                 :initform (error "Required initarg :old-pathname omitted.")
                 :reader change-context/old-pathname
                 :type pathname)
   (new-pathname :initarg :new-pathname
                 :initform (error "Required initarg :pathname omitted.")
                 :reader change-context/new-pathname
                 :type pathname))
  (:documentation
   "A change-context representation for the renaming of a file to be added.
    This class is used in the change-context-log to record a rename operation on
    a file that has been file-added, but not committed to the database.

    This class represents both the old namestring and the new namestring
    and the did of the project containing the file is to be renamed.
    This entity may be used both persistently and transiently, and relies upon
    astore::encode-in-database to do the work.

    ****NOTE**** this needs to be encapsulated in the CORE package, or worked around.")
  (:metaclass persistent-standard-class)
  (:schema-version 0)
)

(defmethod print-object ((change-context-new-filerename change-context-new-filerename) stream)
  (print-unreadable-object (change-context-new-filerename stream :type t)
    (format stream "~a ~a -> ~a"
            (change-context/affected-project-did change-context-new-filerename)
            (change-context/old-pathname change-context-new-filerename)
            (change-context/new-pathname change-context-new-filerename))))

(defmethod objects-equalp ((left change-context-new-filerename) (right change-context-new-filerename))
  (and (equal (change-context/old-pathname left) (change-context/old-pathname right))
       (equal (change-context/new-pathname left) (change-context/new-pathname right))
       (eq (change-context/affected-project-did left)
           (change-context/affected-project-did right))
       (objects-equalp
        (change-context/other-stuff left)
        (change-context/other-stuff right))))

(defmethod serialize ((object change-context-new-filerename) stream symbol-table)
  (write-byte pstore::serialization-code/change-context-new-filerename stream)
  (write-fixnum (symbol-table/intern-did symbol-table (change-context/affected-project-did object)) stream)
  (serialize (change-context/other-stuff object) stream symbol-table)
  (serialize (change-context/old-pathname object) stream symbol-table)
  (serialize (change-context/new-pathname object) stream symbol-table))

(defmethod deserialize-dispatch ((code (eql pstore::serialization-code/change-context-new-filerename)) stream symbol-table)
  (let ((project-did  (symbol-table/resolve-did symbol-table (read-fixnum stream)))
        (other-stuff  (deserialize-dispatch (read-byte stream) stream symbol-table))
        (old-pathname (deserialize-dispatch (read-byte stream) stream symbol-table))
        (new-pathname (deserialize-dispatch (read-byte stream) stream symbol-table)))
    (make-instance 'change-context-new-filerename
                   :affected-project-did project-did
                   :other-stuff other-stuff
                   :old-pathname old-pathname
                   :new-pathname new-pathname)))


(defclass change-context ()
  ((persistent-change-context
    :initarg :persistent-change-context
    :type (or nil persistent-change-context)
    :reader change-context/persistent-change-context)
   (cset-name :initarg :cset-name
              :initform (error "Required initarg :cset-name omitted.")
              :type string
              :reader change-context/cset-name)
   (cset-description :initarg :cset-description
                     :initform (error "Required initarg :cset-description omitted.")
                     :type string
                     :reader change-context/cset-description)

   (log :initform nil
        :accessor change-context/log)

                ; count of added files waiting to be pushed to persistent copy of self

   (file-additions :initform nil
                   :accessor change-context/file-additions)

   (addition-push-count
    :initform 0
    :accessor change-context/file-additions-push-count)

   )
  (:documentation
   "A CHANGE-CONTEXT describes the intended effects of changes in progress,
    for which a CHANGE-SET will ultimately be created when the change-context is
    checked in.  See module documentation for more details."))

#||

(define-tenn-class change-context
    (:documentation
     "A CHANGE-CONTEXT describes the intended effects of changes in progress,
      for which a CHANGE-SET will ultimately be created when the change-context is
      checked in.  See module documentation for more details."
     :no-writer t)                      ;for print-object method.

  ;; At various points in time, all attributes of this class may be
  ;; nil.  And changes to most components may require semantic checks
  ;; (like "is a file locked"), as our RFM package capability model
  ;; matures.
  (persistent-change-context nil)       ;persistent-change-context this represents, if any.

  (repository-id nil)                   ;repository namestring compatible with
                                        ; RFM-SERVER-FIND-REPOSITORY-PATHNAME
  (project-did nil)                     ;DID of project being updated
  (branch-did nil)                      ;DID of branch being served in project, if known
  (client-directory-pathname nil)       ;pathname of root directory for snapshots, etc..
  (client-file-system-platform nil)     ;one of *PLATFORMS*, retval of (file-system-platform fs)
  ;; Enforcement of cset descriptive information is left to UI's and server model functions.
  ;; We're just a holding place for it now, if it's known.
  (cset-name nil)                       ;name of change set to be created, if any
  (cset-description nil)                ;description of change set to be created, if any
  (cset-abstract nil)                   ;abstract of change set to be created, if any

  ;; TBD: how to manage version of version information/error checking
  ;; (version-did-string nil)           ; always LATEST, but could mutate out from under context owner

  ;; Namestrings must always be usable for the purpose of reading/querying the file via a
  ;; FILE-SYSTEM object.

  (log nil)                             ; list of change-context-fileadd, change-context-fileremone, change-context-filerename
                                        ; and change-context-filechange objects in the reverse order of creation.

  ;; It's desirable to use an intermediate structure for rename information.
  (file-additions nil)                  ;list of change-context-fileadd objects.
  (file-removals nil)                   ;list of change-context-fileremove objects
  (file-renames nil)                    ;list of change-context-filerename objects
  (file-changes nil)                    ;list of change-context-filechange objects

  (file-additions-push-count 0)         ; count of added files waiting to be pushed to persistent copy of self
  (file-additions-undone? nil)          ; true if any file additions were undone (so must mutate persistent copy)
  )

(defun change-context-create (&key persistent-change-context
                                   repository-id project-did branch-did
                                   client-directory-pathname cset-name cset-description cset-abstract
                                   client-file-system-platform
                                   log
                                   file-additions file-removals file-renames file-changes)
  "See also: CHANGE-CONTEXT-RESTORE, which creates a transient change-context from a persistent one.

   FILE-ADDITIONS is a list of file namestrings (full path spec!).
   FILE-REMOVALS is a an association list whose element keys are the FSE DID and whose
                  element values are the client file namestring of the file being deleted.
                  (currently just the filename portion, and not a full namestring, and repository-relative,
                  not client relative, since the file is no longer present on the client!)
   FILE-RENAMES is a list of 2 or 3-tuples.  In all cases the first two elements of each
       sublist must be the FSE DID, the new client file namestring,
       and an optional string which represents the old versioned FSE name, typically used for client
       display and no real internal semantic purpose.
   FILE-CHANGES is an assocation list whose key is the FSE DID and whose cdr is the
       client file namestring.

   List arguments which have conversions are not altered, though their list structure may be shared."

  ;; No more FD conversion!

  (when client-directory-pathname (check-type client-directory-pathname pathname))
  (when project-did   (check-type project-did    distributed-identifier))
  (when branch-did    (check-type branch-did     distributed-identifier))
  (when log (check-type log astore-list))
  (dolist (addition file-additions) (assert (typep addition 'change-context-fileadd)))
  (dolist (remove   file-removals)  (assert (typep remove 'change-context-fileremove)))
  (dolist (change   file-changes)   (assert (typep change 'change-context-filechange)))
  (dolist (rename   file-renames)   (assert (typep rename 'change-context-filerename)))

  (make-instance 'change-context
    :change-context-persistent-change-context persistent-change-context
    :change-context-repository-id repository-id
    :change-context-project-did project-did
    :change-context-branch-did branch-did
    :change-context-client-directory-pathname client-directory-pathname
    :change-context-cset-name        cset-name
    :change-context-cset-description cset-description
    :change-context-cset-abstract    cset-abstract
    :change-context-client-file-system-platform (when client-file-system-platform
                                                  (guarantee-platform client-file-system-platform))
    :change-context-log log
    :change-context-file-additions file-additions
    :change-context-file-removals  file-removals
    :change-context-file-renames   file-renames
    :change-context-file-changes   file-changes)
  )

||#
(defmethod change-context/add-log-entry ((change-context change-context) entry)
  (when (change-context/persistent-change-context change-context)
    (pstore-list-push entry (change-context/log change-context))))

#||
(defmethod change-context-set-description ((change-context change-context) description)
  "Update the description associated with a change-context.  This corresponds to the cset-description slot.
   Return the change-context."
  (setf (change-context-cset-description change-context) description)
  change-context)

(defmethod change-context-change-file ((change-context change-context) (change-context-filechange change-context-filechange))
  "Add a record of intent to change the indicated file to this change context."
  (change-context-add-log-entry change-context change-context-filechange)
  (push change-context-filechange (change-context-file-changes change-context)))

(defmethod change-context-change-file-undo ((change-context change-context) (change-context-filechange change-context-filechange))
  (deletef change-context-filechange (change-context-file-changes change-context))
  )

||#

(defmethod change-context/add-file-no-log ((change-context change-context) (change-context-fileadd change-context-fileadd))
  "Record the intent in this change-context to add a new file to the repository.
   CHANGE-CONTEXT-FILEADD is a CHANGE-CONTEXT-FILEADD object which encodes the necessary state to retrieve the file
   when we attempt to create the change-set.
   Return the change-context."
  (push change-context-fileadd (change-context/file-additions change-context))
  (incf (change-context/file-additions-push-count change-context))
  change-context)

(defmethod change-context/add-file ((change-context change-context) (change-context-fileadd change-context-fileadd))
  "Record the intent in this change-context to add a new file to the repository.
   CHANGE-CONTEXT-FILEADD is a CHANGE-CONTEXT-FILEADD object which encodes the necessary state to retrieve the file
   when we attempt to create the change-set.
   Return the change-context."
  (change-context/add-log-entry change-context change-context-fileadd)
  (change-context/add-file-no-log change-context change-context-fileadd))

#||
(defmethod change-context-add-file-undo-no-log ((change-context change-context) (change-context-fileadd change-context-fileadd))
  (setf (change-context-file-additions-undone? change-context) t)
  (deletef change-context-fileadd (change-context-file-additions change-context)))

(defmethod change-context-add-file-undo ((change-context change-context) (change-context-fileadd change-context-fileadd))
  "Cancels the effect of a previous CHANGE-CONTEXT-ADD-FILE."
  (change-context-add-log-entry change-context
                                (change-context-new-fileremove-create
                                 (change-context-fileadd-pathname change-context-fileadd)
                                 :project-did (change-context/affected-project-did change-context-fileadd)
                                 :other-stuff (change-context/other-stuff change-context-fileadd)))
  (change-context-add-file-undo-no-log change-context change-context-fileadd))

(defmethod change-context-remove-file ((change-context change-context) (change-context-fileremove change-context-fileremove))
  "Record the intent in this change-context to remove a file from the repository.
   CHANGE-CONTEXT-FILEREMOVE is a CHANGE-CONTEXT-FILREMOVE object which encodes the necessary state to retrieve the file
   when we attempt to create the change-set.
   Return the change-context."
  (change-context-add-log-entry change-context change-context-fileremove)
  (push change-context-fileremove (change-context-file-removals change-context))
  change-context)

(defmethod change-context-remove-file-undo ((change-context change-context) (change-context-fileremove change-context-fileremove))
  "Cancels the effect of a previous CHANGE-CONTEXT-REMOVE-FILE."
  (deletef change-context-fileremove (change-context-file-removals change-context)))

(defmethod change-context-rename-new-file ((change-context change-context) (change-context-new-filerename change-context-new-filerename))
  (change-context-add-log-entry change-context change-context-new-filerename))

(defmethod change-context-rename-file ((change-context change-context) (change-context-filerename change-context-filerename))
  "Record the intent in this change-context to rename a file in the repository.
   CHANGE-CONTEXT-FILERENAME is a CHANGE-CONTEXT-FILERENAME object which encodes the necessary state to retrieve the file
   when we attempt to create the change-set.
   Return the change-context."
  (change-context-add-log-entry change-context change-context-filerename)
  (push change-context-filerename (change-context-file-renames change-context))
  change-context)

(defmethod change-context-rename-file-undo ((change-context change-context) (change-context-filerename change-context-filerename))
  "Cancels the effect of a previous CHANGE-CONTEXT-RENAME-FILE."
  (deletef change-context-filerename (change-context-file-renames change-context)))

(defmethod print-object ((change-context change-context) stream)
  ;; Note the use of :NO-READER and :NO-WRITER on class definition to avoid a ACL compilation warning
  ;; about defining PRINT-OBJECT twice in this module.

  ;; THIS METHOD SHOULD MATCH THAT FOR PERSISTENT-CHANGE-CONTEXT's PRINT-OBJECT METHOD
  (print-unreadable-object (change-context stream :type t) ; print class name, but not address id
    (princ (or (change-context-cset-name change-context)
               "<unknown change name>")
           stream)))

(defmethod change-context-affected-project-dids ((change-context change-context))
  "Returns a list of satelite project DIDs of all projects affected by CHANGE-CONTEXT."
  (let ((result nil))
    (flet ((collect-it (base-change)
             (pushnew (change-context/affected-project-did base-change)
                      result)))
      (mapc #'collect-it (change-context-file-additions change-context))
      (mapc #'collect-it (change-context-file-changes   change-context))
      (mapc #'collect-it (change-context-file-renames   change-context))
      (mapc #'collect-it (change-context-file-removals  change-context)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finding the state of a file in the change context

;;; A file could be:
;;;   (1)  in the repository
;;;   (2)  the subject of a file change (did only)
;;;   (3)  the subject of a fileadd (pathname only)
;;;   (4)  the subject of a fileremove (did only)
;;;   (5)  the subject (did only) or target (pathname only) of a
;;;        filerename.
;;; We're not concerned about whether the file is present on the
;;; user's disk at this level.

(defgeneric change-context-find-file-additions (change-context new-file-name project-did)
  (:documentation
   "Look in the FILE-ADDITIONS of CHANGE-CONTEXT for an entry which
will add the file named NEW-FILE-NAME to the project specified by PROJECT-DID."))

(defgeneric change-context-find-file-changes (change-context file-did)
  (:documentation
   "Look in the FILE-CHANGES of CHANGE-CONTEXT for an entry which affects
the file idenfified by FILE-DID."))

(defgeneric change-context-find-file-removals (change-context file-did)
  (:documentation
   "Look in the FILE-REMOVALS of CHANGE-CONTEXT for an entry which will remove
the file idenfified by FILE-DID."))

(defgeneric change-context-find-file-renames (change-context file-did)
  (:documentation
   "Look in the FILE-RENAMES of CHANGE-CONTEXT for an entry which will rename
the file idenfified by FILE-DID."))

(defgeneric change-context-find-file-renamed-to (change-context new-file-name project-did)
  (:documentation
   "Look in the FILE-RENAMES of CHANGE-CONTEXT for an entry which will rename
some file to NEW-FILE-NAME."))

;;; These methods are defined on T because there isn't a sensible
;;; ancestor that CHANGE-CONTEXT and PERSISTENT-CHANGE-CONTEXT both
;;; inherit from and apparently AllegroStore prevents there from being
;;; one.

(defmethod change-context-find-file-additions ((change-context t) file-name project-did)
  (loop for add in (change-context-file-additions change-context)
      when (change-context-fileadd-adds-file-p add file-name project-did)
      collect add))

(defmethod change-context-find-file-changes ((change-context t) file-did)
  (loop for change-context-filechange in (change-context-file-changes change-context)
      when (change-context-filechange-changes-file-p change-context-filechange file-did)
      collect change-context-filechange))

(defmethod change-context-find-file-removals ((change-context t) file-did)
  (loop for deletion in (change-context-file-removals change-context)
      when (change-context-fileremove-removes-file-p deletion file-did)
      collect deletion))

(defmethod change-context-find-file-renames ((change-context t) file-did)
  (loop for rename in (change-context-file-renames change-context)
      when (change-context-filerename-renames-file-p rename file-did)
      collect rename))

(defmethod change-context-find-file-renamed-to ((change-context t) new-file-name project-did)
  (loop for rename in (change-context-file-renames change-context)
      when (change-context-filerename-renames-file-to-p rename new-file-name project-did)
      collect rename))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod change-context-file-base-change-applies-to-project-p ((cc change-context-file-base-change)
                                                     project-did)
  (let ((my-project (change-context/affected-project-did cc)))
    (cond ((null my-project) t)         ; special case which RFM is assumed to depend on
          ((eq my-project project-did) t)
          (t nil))))

||#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Persistent change contexts.  Just a transient<->persistent relay, all interesting
;;; change-context manipulations should occur on the transient CHANGE-CONTEXT class.

;;; Implementation note: we could use the Astore encode/decode primitives and turn
;;; the transient representation into a persistent vector/list, thereby eliminating
;;; a schema element.  This might be the right thing to do, I'm not sure.

;;; Definitely the wrong thing to do.

(defclass persistent-change-context ()
  ((cset-name :initarg :cset-name
              :initform (error "Required initarg :cset-name omitted.")
              :type string
              :reader change-context/cset-name)
   (cset-description :initarg :cset-description
                     :initform (error "Required initarg :cset-description omitted.")
                     :type string
                     :reader change-context/cset-description)

   (file-additions :initform nil
                   :initarg :file-additions
                   :accessor persistent-change-context/file-additions))
  (:documentation "A persistent representation of CHANGE-CONTEXT information.")
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defun change-context/create-restore (persistent-change-context)
  "Create a CHANGE-CONTEXT from it's persistent clone."
  (make-instance 'change-context
    :persistent-change-context persistent-change-context
    :cset-name (change-context/cset-name persistent-change-context)
    :cset-description (change-context/cset-description persistent-change-context)))

(defun change-context->persistent-change-context (change-context)
  "Create and return a persistent representation of <change-context>."
  (let ((persistent-change-context
         (make-instance 'persistent-change-context
                        :cset-name (change-context/cset-name change-context)
                        :cset-description (change-context/cset-description change-context))))
    persistent-change-context))

(defmethod change-context/add-file ((change-context persistent-change-context) (fileadd change-context-fileadd))
  (persistent-vector-push (persistent-change-context/file-additions change-context) fileadd))

#||
(define-tenn-class persistent-change-context
    (:documentation "A persistent representation of CHANGE-CONTEXT information."
     :astore-persistent t :no-writer t)
  (repository-id nil)
  (project-did nil)
  (branch-did nil)
  (client-directory-pathname nil)
  (client-file-system-platform nil)
  (cset-name nil)
  (cset-description nil)
  (cset-abstract nil)
  (log nil)                             ;list of operations that affect the following four slots
  (file-additions nil)
  (file-removals  nil)
  (file-renames   nil)
  (file-changes   nil)
  )

(defvar *change-context-use-astore-lists* t
  "If true persistent-change-context-file-additions is represented as an astore-list vs. lisp list,
   which avoids copying the entire list when simply pushing new entries at front of list.")

(defun persistent-change-context-update (persistent-change-context change-context)
  "Update and return a persistent change-context with contents of a transient change-context.
   Useful if you've been performing operations on the transient instance and want update a
   persistent instance to which it corresponds."
  (macrolet ((doit (slot)
               (let ((pcc-slot-name (symbol-append 'persistent-change-context- slot))
                     (change-context-slot-name  (symbol-append 'change-context- slot)))
                 `(LET ((OLD-VALUE (,pcc-slot-name PERSISTENT-CHANGE-CONTEXT))
                        (NEW-VALUE (,change-context-slot-name  CHANGE-CONTEXT)))
                    ;; Avoid setting slots that are the same.
                    ;; Why can't we use EQ ?  Because the objects remain
                    ;; EQ, but their contents are modified!  This means that if
                    ;; we were to remove an element from the middle of a list, the
                    ;; list would not get remarshalled out to the database and the
                    ;; change would be lost.
                    (UNLESS (OR
                             (AND (DISTRIBUTED-IDENTIFIER? OLD-VALUE)
                                  (DISTRIBUTED-IDENTIFIER? NEW-VALUE)
                                  (EQ OLD-VALUE  NEW-VALUE))
                             (AND (SYMBOLP OLD-VALUE)
                                  (SYMBOLP NEW-VALUE)
                                  (EQ OLD-VALUE NEW-VALUE))
                             ;; But we *can* use EQ here!  The ASTORE-LISTS are consed
                             ;; in persistant space, so changes to the middle of the list
                             ;; ARE seen in the database when we flush the list.
                             (AND (TYPEP OLD-VALUE 'ASTORE-LIST)
                                  (TYPEP NEW-VALUE 'ASTORE-LIST)
                                  (EQ OLD-VALUE NEW-VALUE))
                             (AND (STRINGP OLD-VALUE)
                                  (STRINGP NEW-VALUE)
                                  (STRING= OLD-VALUE  NEW-VALUE))
                             )
                      (SETF (,pcc-slot-name PERSISTENT-CHANGE-CONTEXT)
                            NEW-VALUE)
                      ;; (astore-flush-changes :verbose t) ;; write out new slot contents
                      )))))
    (doit repository-id)
    (doit project-did)
    (doit branch-did)
    (doit client-directory-pathname)
    (doit client-file-system-platform)
    (doit cset-name)
    (doit cset-description)
    (doit cset-abstract)
    (doit log)
    (doit file-removals)
    (doit file-renames)
    (doit file-changes)
    ;; Finally update file-additions, converting to an astore-list if desired
    (cond ((not *change-context-use-astore-lists*) (doit file-additions))
          ;; If some file-adds were undone then this may require mutation of file-adds in our persistent copy.
          ;; For now we are stupid and just replace the entire file-adds list. Todo: be smarter and mutate it.
          ((change-context-file-additions-undone? change-context)
           (astore-list-reclaim (persistent-change-context-file-additions persistent-change-context))
           (setf (persistent-change-context-file-additions persistent-change-context)
                 (lisp-list-to-astore-list
                   (change-context-file-additions change-context))))
          ((< 0 (change-context-file-additions-push-count change-context))
           (labels ((a-push-n (lisp-list count)
                      (astore-list-cons (car lisp-list)
                                        (if (<= count 1)
                                            (persistent-change-context-file-additions persistent-change-context)
                                          (a-push-n (cdr lisp-list) (1- count))))))
             (setf (persistent-change-context-file-additions persistent-change-context)
                   (a-push-n (change-context-file-additions change-context)
                             (change-context-file-additions-push-count change-context))))))
    ;; (astore-flush-changes :verbose t)
    persistent-change-context
    ))

(defmethod persistent-change-context-create ((change-context change-context))
  "Create a persistent change-context proxy for the indicated change-context.
   A R/W database transaction must be active."
  (persistent-change-context-update (make-instance 'persistent-change-context) change-context))

(defun change-context-create-restore (persistent-change-context)
  "Create a CHANGE-CONTEXT from it's persistent clone."
  (make-instance 'change-context
    :change-context-persistent-change-context persistent-change-context
    :change-context-repository-id    (persistent-change-context-repository-id    persistent-change-context)
    :change-context-project-did      (persistent-change-context-project-did      persistent-change-context)
    :change-context-branch-did       (persistent-change-context-branch-did       persistent-change-context)
    :change-context-client-directory-pathname   (persistent-change-context-client-directory-pathname
                                                  persistent-change-context)
    :change-context-client-file-system-platform (persistent-change-context-client-file-system-platform
                                                  persistent-change-context)
    :change-context-cset-name        (persistent-change-context-cset-name        persistent-change-context)
    :change-context-cset-description (persistent-change-context-cset-description persistent-change-context)
    :change-context-cset-abstract    (persistent-change-context-cset-abstract    persistent-change-context)
    :change-context-log              (persistent-change-context-log              persistent-change-context)
    :change-context-file-additions (let ((ads (persistent-change-context-file-additions persistent-change-context)))
                                     (if (listp ads) ads
                                       (astore-list-to-lisp-list ads)))
    :change-context-file-removals    (persistent-change-context-file-removals    persistent-change-context)
    :change-context-file-renames     (persistent-change-context-file-renames     persistent-change-context)
    :change-context-file-changes     (persistent-change-context-file-changes     persistent-change-context)))

(defmethod print-object ((persistent-change-context persistent-change-context) stream)
  ;; Note the use of :NO-READER and :NO-WRITER on class definition to avoid a ACL compilation warning
  ;; about defining PRINT-OBJECT twice in this module.
  (print-unreadable-object (persistent-change-context stream :type t) ; print class name, but not address id
    (princ (or (persistent-change-context-cset-name persistent-change-context)
               "<unknown change name>")
           stream)))

(defmethod change-context-change-in-progress? ((change-context change-context))
  "Returns two values, the first is T if there are outstanding additions, removals,
   renames or changes,  the second is T iff there are outstanding changes.

   Having no change in progress is equivalent to having no change-context in certain
   situations."
  (values (or (change-context-file-additions change-context)
              (change-context-file-removals  change-context)
              (change-context-file-renames   change-context)
              (change-context-file-changes   change-context))
          (change-context-file-changes   change-context)))

;;; These accessors should have just been given generic names to begin
;;; with, but DEFINE-TENN-CLASS imposes bogus restrictions about
;;; accessor names.

(defmethod change-context-file-additions ((cc persistent-change-context))
  (let ((ads (persistent-change-context-file-additions cc)))
    (if (listp ads) ads
      (astore-list-to-lisp-list ads))))

(defmethod change-context-file-changes ((cc persistent-change-context))
  (persistent-change-context-file-changes cc))

(defmethod change-context-file-removals ((cc persistent-change-context))
  (persistent-change-context-file-removals cc))

(defmethod change-context-file-renames ((cc persistent-change-context))
  (persistent-change-context-file-renames cc))

(defmethod change-context-cset-name ((cc persistent-change-context))
  (persistent-change-context-cset-name cc))

(defmethod change-context-cset-description ((cc persistent-change-context))
  (persistent-change-context-cset-description cc))

(defmethod change-context-cset-abstract ((cc persistent-change-context))
  (persistent-change-context-cset-abstract cc))

||#

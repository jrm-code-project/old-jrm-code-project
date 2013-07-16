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
;;;; File Name:     rfm-project.lsp
;;;; Author:        Dave Tenny
;;;;
;;;; Module Description:
;;;;
;;;; Subtype of VM::PROJECT designed to manage a repository file system,
;;;; disk files, and websites currently provided for in the RFM package.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CSF/REPOSITORY-FILE-MANAGEMENT")

(proclaim (standard-optimizations))

(eval-when (:load-toplevel :execute)
  (export '(rfm-project
            rfm-project/publish-to-file-system
            rfm-project/publish-to-file-list
            rfm-project/root-directory
            rfm-project/scan-files
#||
            rfm-project-create
            rfm-project-initialize
            rfm-project-website-list
            rfm-project-root-directory
||#
            )))

(defclass rfm-project (project)
  ((root-directory :initarg  :root-directory
                   :initform (error "Required initarg :root-directory omitted.")
                   :reader rfm-project/root-directory
                   :version-technique :nonversioned))
  (:documentation "Subtype of VM::PROJECT which manages versioned elements for the RFM model.")
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defun rfm-project/scan-files (project)
  (declare (optimizable-series-function))
  (transitive-scan (rfm-project/root-directory project)))

(defun rfm-project/publish-to-file-system (project branch file-system
                                                   &key create?
                                                   publish-read-only?
                                                   report-file-system
                                                   unpublish?
                                                   when-overwriting-directory
                                                   when-overwriting-file)
  (rfm-directory/publish-to-file-system
   project branch file-system
   (rfm-project/root-directory project)
   :create? create?
   :publish-read-only? publish-read-only?
   :report-file-system report-file-system
   :unpublish? unpublish?
   :when-overwriting-directory when-overwriting-directory
   :when-overwriting-file when-overwriting-file))

(defun rfm-project/publish-to-file-list (project)
  (traverse-rfm-file-system (rfm-project/root-directory project)
                            (lambda (element rest)
                              (cons (file-system-element/relative-path element)
                                    rest))
                            nil))

#||
(defclass rfm-project (project)
  ;; This shouldn't change.  Also, by making it non-versioned, we don't need to promote it's
  ;; creation cid into every branch that wants to view it.  Conman::test-5a will be unhappy if you
  ;; require the creation cid...
                                        ; RFM-DIRECTORY, write-once slot
  ((root-directory :initarg :directory
                   :initform (error "Required initarg :directory missing."))

   ;; This inverse relationship between project websites and website projects is inconvenient.
   ;; It'd be nice to do away with it, at least in the interface that maintains it, if not totally.
   (websites :initform nil
             :version-technique :composite-set) ; zero or more websites associated with project

   ;; If true, then filenames that differ only in case will be treated as
   ;; different (unix-like semantics), if false, then filenames that differ
   ;; in case will be treated the same (NT semantics).  Filename case is
   ;; always preserved.  Note that with NT semantics, it is possible for a
   ;; file to appear twice in a checkin list (if checked in from a UNIX box).
   (filenames-case-sensitive?
    :initarg :filenames-case-sensitive?
    :initform nil
    :version-technique :scalar))

  (:documentation "Subtype of VM::PROJECT which manages versioned elements for the RFM model.")
  (:metaclass versioned-class))

(defmethod initialize-instance :after ((rfm-project rfm-project) &rest initargs
                                       &key websites directory)
  (assert (null (containee/containers directory)))
  (setf (containee-%containers directory) (list rfm-project))
  (when websites
    (rfm-project/change-websites rfm-project websites)))
||#
#||
(defun rfm-project-initialize (rfm-project project-name project-description root-rfm-directory
                               &key (initial-branch-name "main") websites filenames-case-sensitive?)
  "Initialize a RFM-PROJECT.  A change-transaction must already be active.
   RFM-PROJECT is the newly created rfm-project instance (or subtype thereof).
   PROJECT-NAME is a string naming the project.
   PROJECT-DESCRIPTION is a string describing the project.

   ROOT-RFM-DIRECTORY is an RFM-DIRECTORY object which will serve as the root for the repository file
   system representation.

   INITIAL-BRANCH-NAME is a string naming the initial project branch.
   WEBSITES, if specified, should be a list of websites associated with the project."
  (check-type root-rfm-directory rfm-directory)
  (project-initialize rfm-project project-name project-description
                      :initial-branch-name initial-branch-name)
  (set-rfm-project-root-directory rfm-project root-rfm-directory)
  (set-rfm-project-filenames-case-sensitive? rfm-project filenames-case-sensitive?)
  ;; revisit this if the root directory for a project can be owned
  ;; in several projects
  (assert (null (containee-container-list root-rfm-directory)))
  (set-containee-%containers root-rfm-directory (list rfm-project))
  (when websites
    (rfm-project-change-websites rfm-project websites))
  rfm-project)

(defsubst rfm-project-create (&rest rfm-project-initialize-args)
  "Create and return a RFM-PROJECT instance.  See RFM-PROJECT-INITIALIZE for argument details.
   Example: (rfm-project-create project-name project-desc root-rfm-dir)"
  (apply #'rfm-project-initialize (make-instance 'rfm-project) rfm-project-initialize-args))

(defmethod object-change-interesting-p or ((rfm-project rfm-project) (slot-name symbol) birth-cid-p)
  (and (not birth-cid-p)
       (eq slot-name 'websites)))

(defmethod object-change-user-semantics or ((rfm-project rfm-project) (slot-name symbol)
                                            change-type valid-p new-or-old old)
  (declare (ignore old))
  (and (eq slot-name 'websites)
       (format nil "~(~a~) web site ~s" ; convert keyword to lower cae
               change-type
               ;; Should never see :CHANGE to websites in list, just :ADD or :DELETE from list
               (or (and valid-p
                        ;; Name of website, but may not correctly reflect project version scope for
                        ;; website name.
                        (object-user-name new-or-old))
                   "<not imported into repository, web site name unavailable>"))))

(defun rfm-project-default-file (rfm-project rfm-directory)
  "Pick a file in RFM-DIRECTORY which is a file of some significance for a project.
   Typically rfm-directory is the root directory.

   When we have a real protocol for specifying files of interest, like 'index.html',
   we'll either  want to do away with the RFM-DIRECTORY argument, or allow bindings on a
   per-rfm-directory basis."
  (declare (ignore rfm-project)         ;anticipating this as a required arg in the future
           #+allegro (:fbound file-system-element-name rfm-directory-fse-list))
  ;; *FINISH*: implement protocol support.
  ;; This implementation caches versioned slot values for performance reasons
  (let* ((fse-list (rfm-directory-fse-list rfm-directory))
         (fse-names (mapcar #'file-system-element-name fse-list))
         (interesting-names (mapcar #'file-namestring
                                    (list (make-logical-pathname :name "index" :type "html")
                                          (make-logical-pathname :name "index" :type "htm")
                                          (make-logical-pathname :name "default" :type "html")
                                          (make-logical-pathname :name "default" :type "htm")
                                          (make-logical-pathname :name "home" :type "html")
                                          (make-logical-pathname :name "home" :type "htm")))))
    (loop for fse in fse-list
        for fse-name in fse-names
        do
          (loop for candidate-name in interesting-names
              when (string= candidate-name fse-name)
              do (return-from rfm-project-default-file fse)))
    (first fse-list)))



;;;
;;; Website list maintenance.  Inverse relationship management a big pain.
;;;

;;; The following four routines for string-did transformations and sorting should disappear
;;; when we have a 'perfect diff'.  Though the string-did-transform and resolve-string-dids
;;; methods may be useful.  They're all working with distributed objects in active transaction contexts.

(defsubst string-did-transform (object-list)
  "Return a list of string-dids which reflects the distributed objects in OBJECT-LIST,
  preserving order of the objects referenced by the respective handles in the source and result lists."
  (mapcar #'did->string-did
          (mapcar #'distributed-object-identifier object-list)))

(defun resolve-string-dids (string-did-list)
  "Resolve every string-did in STRING-DID-LIST and return a list of resolved distributed objects
   corresponding to and in the same order as those handles in STRING-DID-LIST."
  (mapcar (lambda (string-did)
              (repository-resolve-distributed-identifier
               *repository* (parse-did string-did)))
          (sort string-did-list #'string<)))

(defsubst sort-string-dids (string-did-list)
  "Sort the string-dids in STRING-DID-LIST and return them.
   Currently used to maintain versioned object lists in absence of a hash-based 'perfect diff' algorithm."
  (sort string-did-list #'string<))

(defsubst sort-dids (did-list)
  "Sort the dids in DID-LIST and return them.
   Currently used to maintain versioned object lists in absence of a hash-based 'perfect diff' algorithm."
  (sort did-list #'did<))

(defsubst sort-objects-by-did (object-list)
  "Sort distributed objects in OBJECT-LIST in ascending lexicographic order of their distributed
   identifier string forms.  This is used only until such time as we have a 'perfect diff' for
   objects.  It is used in places outside this module as well.

   A sorted list of objects in OBJECT-LIST is returned.  OBJECT-LIST may be modified."
  (sort object-list #'did< :key #'distributed-object-identifier))

;;; End of perfect-diff workaround utility routines (but not the hacks which use these routines)

(defsubst rfm-project-website-list (rfm-project)
  "Retrieve the possibly empty list of websites associated with an RFM-PROJECT."
  (vi-stream-as-list (rfm-project-websites rfm-project)))

(defun rfm-project-add-website (rfm-project website)
  "Add a website to the project.  This is called by the website, it is NOT meant to be called
   by other agencies.  It is an error if WEBSITE is already in the project website list."
  (set-rfm-project-websites rfm-project
                            (sort-objects-by-did (cons website (rfm-project-website-list rfm-project)))))

(defun rfm-project-remove-website (rfm-project website)
  "Remove a website from the project.  This is called by the website, it is NOT meant to be called
   by other agencies.  It is an error if WEBSITE is not already in the project website list."
  (set-rfm-project-websites rfm-project
                            ;; No sorting necessary for deletion
                            (delete website (rfm-project-website-list rfm-project))))

(defun rfm-project-change-websites (rfm-project website-objects)
  ;; WARNING: CODE CLONE ALERT: SEE WEBSITE-CHANGE-PROJECTS
  "Change the set of websites associated with a rfm-project under the current versioned view.

   Specifiy the websites as WEBSITE-STRING-DIDS if possible, otherwise as WEBSITE-OBJECTS (both lists)
   Once we have perfect diff, we don't need the string dids, and project-objects will be preferred.

   In order to minimize diffs, we are careful to sort projects according to string did,
   since we don't have a perfect diff algorithm right now.
   Return value: N/A"
  #+allegro (declare (:fbound website-add-project website-remove-project)) ;forward references
  ;; *FINISH* - use the perfect diff, don't do this hideous sort
  (let ((sorted-new-website-objects (sort-objects-by-did website-objects))
        (old-website-objects (rfm-project-website-list rfm-project)))
    (set-rfm-project-websites rfm-project sorted-new-website-objects)

    ;; All websites which were removed from association or added to association need to
    ;; be notified of the change, and thus subscribe to the change-set (primarily they can notify
    ;; their other interested projects). We also need to update
    ;; their inverse relationship lists of websites.

    ;; Notify of change-set
    (mapc #'vm-txn-subscribe-to-change-set
          (union old-website-objects sorted-new-website-objects))

    ;; Add or remove the rfm-project from the website lists
    (mapc (lambda (website)
              (website-remove-project website rfm-project))
          (set-difference old-website-objects sorted-new-website-objects))
    (mapc (lambda (website)
              (website-add-project website rfm-project))
          (set-difference sorted-new-website-objects old-website-objects))
    ))
||#

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
;;;;
;;;; File Name:     repository-file-system.lsp
;;;; Author:        Dave Tenny
;;;;
;;;; Module Description:
;;;;
;;;; This file contains all classes which represent file systems internal
;;;; to the repository and the basic services model.  This module does NOT
;;;; represent aspects of the physical operating system file system,
;;;; support for which mostly resides in other modules and standard lisp
;;;; functions.  The preceding statement notwithstanding, there may be
;;;; some routines which bridge repository file system representations
;;;; and operating system representations for activities such as reference
;;;; area maintenance, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CSF/REPOSITORY-FILE-MANAGEMENT")

(proclaim (standard-optimizations))

(eval-when (:load-toplevel :execute)
  (export '(csf/config::*file-content-changed-criteria*
            )
          "CSF/CONFIG")
  (export '(rfm-directory
            rfm-file
            scan-rfm-directory
            file-system-element/relative-path
            file-system-element/descendants
            )))

(defclass file-system-element (described-object distributed-object containee)
  (
   ;; File name, type, and if appropriate, version.  Not a pathname, and not path-qualified,
   ;; the path of an FSE is determined by its container.

   ;; **** NOTE **** The NAME slot always reflects an ENCODED pathname.  Use
   ;; (decode-pathname (file-system-element-relative-path fse)) to obtain a human readable form.
   ;; Better, use (file-system-friendly-name (file-system-element-relative-path fse) file-system)
   ;; if you have a file-system handy in order to resolve the path accordint to various file system
   ;; conventions.
   (name :initarg :element-name
         :type string
         :version-technique :scalar
         :reader file-system-element/name)
   ;; Modification date is lisp universal-time.  We'll probably need with better granularity later.
   ;; WARNING: you can essentially 'modify' a file system element by specifying alternate CID-SET views.
   ;; As such, this is a best guess as to actual FSE-specific checkin activity given a view.
   ;; We could potentially cache size too, it has the same problem.

   ;; We should really be checking for the last modification time
   ;; in the current branch rather than depending on this element.
   (modification-date :initarg :modification-date
                      :initform (error "Required initarg :modification-date omitted.")
                      :reader file-system-element/modification-date
                      :type integer
                      :version-technique :scalar
                      :documentation
                      "Date of last meaningful content modification, interpreted by subtypes.
    The NAME attribute of an FSE is typically not related to the modification date.
    File and directory contents are related to modification date."
                      ))
  (:documentation
   "Abstract base class for any object which will be extracted to a file system (disk).
    Subtypes of file-system-element are capable of yielding creation or modification timestamps,
    size of the element in bytes, and potentially other useful tidbits.")
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defmethod print-object ((object file-system-element) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s" (file-system-element/name object))))

(defgeneric file-system-element/relative-path (fse)
  (:documentation
   "Return a logical pathname that represents the path to this file."))



;;; RFM-FILE
(defclass rfm-file (file-system-element)
  ((content-type :initarg :content-type
                 :initform (error "Required initarg content-type omitted.")
                 :reader rfm-file/content-type
                 :version-technique :scalar)
   (binary-content   :initarg :binary-content
                     :reader rfm-file/binary-content
                     :version-technique :scalar)
;;  (binary-size :version-technique :scalar) ;size of binary contents (minus padding), kept in sync with binary contents
;;  (binary-crc  :version-technique :scalar) ; crc of binary contents, kept in sync with binary contents
   (content-encoding :initarg :content-encoding
                     :initform (error "Required initarg content-encoding omitted.")
                     :reader rfm-file/content-encoding
                     :version-technique :scalar)
   (record-separator :initarg :record-separator
                     :initform (error "Required initarg record-separator omitted.")
                     :reader rfm-file/record-separator
                     :version-technique :scalar)
   (content          :initform nil
                     :accessor rfm-file/content
                     :version-technique :composite-file))
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defun rfm-file/scan-records (file)
  (declare (optimizable-series-function))
  (versioned-object/scan-composite-versioned-slot file 'content))

(defun rfm-file/binary-size (file)
  ;; We'll have to do this better later.
  (length (rfm-file/binary-content file)))

(defun rfm-file/binary-crc (file)
  (let ((content (rfm-file/binary-content file)))
    (byte-array-crc *crc-seed* content)))

(defmethod graph-node-lessp ((left file-system-element) (right file-system-element))
  (string-lessp (namestring (file-system-element/relative-path left))
                (namestring (file-system-element/relative-path right))))

(defmethod graph-node-eq ((left file-system-element) (right file-system-element))
  nil)

(defmethod graph-node-children ((node rfm-file)) '())

(defsubst resolve-record-terminator (file platform-record-terminator)
  (if (eq (rfm-file/record-separator file) :PLATFORM)
      platform-record-terminator
    (rfm-file/record-separator file)))

(defun file-system-element/crc (file)
  "Retrieve the CRC of either the BINARY-CONTENTS slot or the TEXT-CONTENTS
slot by interpreting the MIME-CONTENT information."
  (if (media-type/binary? (rfm-file/content-type file))
      (rfm-file/binary-crc file)
      (collect-crc (rfm-file/scan-records file))))

(defmethod file-system-element/relative-path ((fse rfm-file))
  (let* ((raw-name (file-system-element/name fse))
         ;; Instead of calling the pathname parser, we just
         ;; look for the dot.  Much faster.
         (dot (position #\. raw-name))
         (name (if dot (subseq raw-name 0 dot) raw-name))
         (type (if dot (subseq raw-name (1+ dot) (length raw-name)) nil)))
    (rfm-directory/path-elements (file-system-element/directory fse)
                                 (lambda (dir)
                                   (make-pathname :host "REPOSITORY"
                                                  :directory dir
                                                  :name name
                                                  :type type
                                                  :version nil))
                                 (constantly nil))))

(defgeneric file-system-element/size (fse &rest stuff &key &allow-other-keys)
  (:documentation
   "Return some subtype dependent interpretation of the size of a FSE.
    For files, this is probably the size of the file in bytes for a given view.
    However such a bit of information depends on the view used to view the file,
    and so may be derived since a static value may be meaningless.
    For directories, this generic function might return something like the number
    of directory entries.  And so on.")
  (:method ((fse file-system-element) &key &allow-other-keys)
    "Without subtype specialization, file-system-element size is zero."
    0))

(defgeneric file-content/equal? (repository-file alternate-file-source)
  (:documentation
   "Return true if the content of the REPOSITORY-FILE and ALTERNATE-FILE-SOURCE
    are equal, NIL otherwise.")

  (:method ((repository-file rfm-file) (disk-file pathname))
    (error "Manipulation of server-local files should occur through the FILE-SYSTEM abstraction")))



;;;
;;; DIRECTORY
;;;

(defclass rfm-directory (file-system-element container)
    ;; Note:  CONTAINEE used to be a direct superclass, but all file-system-elements are
    ;; containees, so it is now an indirect superclass.  rfm-directories are still
    ;; containers, however.
  ()
  (:documentation
     "Analogous to a file system directory, children are managed via CONTAINER behavior.
    All children must be of type FILE-SYSTEM-ELEMENT, though that opens the way for
    children to exist which aren't necessarily defined in this module, such as (potentially)
    PROJECT references.  Within any view of the rfm-directory, the set of accessible
    FILE-SYSTEM-ELEMENT names must be unique.  Class RFM-DIRECTORY enforces this constraint.

    The FSE modification-date of a rfm-directory is changed only when directory contents change
    by addition/deletion of containees, not by rename or other actions on the contained objects. ")
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defun rfm-directory/content-list (rfm-directory)
  (container/containee-list rfm-directory))

;;; Iterate over the directory
(defun scan-rfm-directory (rfm-directory)
  (declare (optimizable-series-function))
  (scan-container rfm-directory))

;;; If the file system elements form a tree, we can do a *lot* less work in traversing
;;; the repository.  Right now, I'm going to assume that they *are* a tree, and when
;;; we add symbolic links, I'll cope then.
(defun file-system-element/descendants (fse)
  (cdr
   (named-let luup ((fse fse))
              (etypecase fse
                (rfm-file (list fse))
                (rfm-directory
                 (cons fse (collect-nconc
                            (#M luup (scan-rfm-directory fse)))))))))

(defun file-system-element/directory (fse)
  ;; This should be thought out some more.  There may be more than one directory holding this
  ;; file system element.  For now, assume that the first container is canonical.
  "Return the primary (canonical) directory that holds the element."
    (let ((containers (containee/container-list fse)))
      (unless (or (null containers)
                  (null (cdr containers)))
        (warn "Object has multiple directories which require resolution to a single parent"))
      (find-if (lambda (thing)
                 (typep thing 'rfm-directory))
               containers)))

;;; This has become a little hairier because we used to rely on a bug
;;; that allowed us to find the `parent directory' of a deleted file
;;; or directory.  Since we can become detached from the project at
;;; any level, we use CPS to get us out of the computation as soon
;;; as we notice that we are detached.

(defun rfm-directory/path-elements (rfm-directory receiver if-detached)
  "Cons up a list which is suitable for specifying pathname directory components
   by traversing the upward directory links visible from rfm-directory.

   If the file is accessible from the root, RECEIVER (a function of one argument)
   is called with the result.

   If the file is not accessible from the root (it has been deleted),
   IF-DETACHED (a function of no arguments) is called."
  (labels ((luup (superdir receiver)
                 (if (null superdir)
                     (funcall if-detached)
                     (progn
                       (assert (typep superdir 'rfm-directory))
                       (let ((parent (file-system-element/directory superdir)))
                         (if (typep parent 'rfm-directory)
                             ;; it is not a root directory
                             (luup parent
                                   (lambda (partial-result)
                                     (funcall receiver
                                              (cons (file-system-element/name superdir)
                                                    partial-result))))
                             (funcall receiver (list :relative))))))))

          (luup rfm-directory
                (lambda (backward-name)
                  (funcall receiver
                           (canonicalize-pathname-directory
                            (reverse backward-name)))))))

(defmethod file-system-element/relative-path ((directory rfm-directory))
  (rfm-directory/path-elements directory
                               (lambda (dir)
                                 (make-pathname :host "REPOSITORY"
                                                :directory dir))
                               (constantly nil)))


(defun traverse-rfm-file-system (fse combiner initial-value)
  ;; slow way
  ;; (transitive-closure fse combiner initial-value))
  (named-let reduction-loop ((elements (file-system-element/descendants fse))
                             (accumulator initial-value))
                  (if (null elements)
                      accumulator
                    (reduction-loop (cdr elements)
                                    (funcall combiner (car elements) accumulator)))))

(defparameter *file-content-changed-criteria* :crc-only
  ;; Currently, I recommend :crc-only,
  ;; but HP wants a cheesier one for checkout.

  ;; Read function below for list of valid values.
  "Selects the amount of work that file-content-changed? goes through in order
   to decide whether the content on the user's disk is different from that in
   the repository.  One end of the spectrum would be to read in the content from
   the disk and compare it, the other end, might just look to see that the file size
   is the same.")

(defgeneric file-content-changed? (repository-file file-system file-descriptor)
  (:documentation
   "Predicate to determine if a file has changed, incorporating cheap tests such as checksum
    comparison when possible (unlike FILE-CONTENT-EQUAL? which does the expensive test).

    NOTE: if it's your intent to check in the file if it's different and you can do it in the
    transaction which would call this method, then call FILE-MAYBE-CHANGE-CONTENT, which does the
    cheap tests and comparison with simultaneous checkin if they're different.  Using this method
    and subsequent checkin would cause two comparisons of file content, which isn't necessarily a cheap
    operation.

    Return true if the file content in the file system is different than it is in the repository.

    RECORD-TERMINATOR is as for FILE-SYSTEM-ELEMENT-SIZE.")
  (:method ((file rfm-file) (fs file-system) (fd file-descriptor))
    ;;   (unless repository-file-size
    ;;       (assert record-terminator))             ;must be present if file-size isn't present.
    ;; TO-DO:  implement timestamp checks as an option since it isn't
    ;; necessarily reliable.
    (flet ((size-differs? ()
             (/= (file-system-element/size file
                                           :platform-record-separator
                                           (file-system/record-separator
                                            (file-descriptor/file-system
                                             fd)))
                 (file-descriptor/size fd)))

           (file-permissions-allow-change? ()
             (not (file-descriptor/read-only? fd)))

           (date-differs? ()
             ;; Jrm sez:
             ;; Ugh.  When we are maintaining versioned content in a directory, and we let
             ;; changesafe manipulate the timestamps, we really can't know if the content is
             ;; the same.  (For instance, we could have imported a change that modified the
             ;; the file contents, but not the timestamps).
             (not (file-system-element/timestamp-equal file fs fd)))

           (crc-differs?  ()
             ;; Magic!
             ;; When possible, we start the file system agent computing the CRC
             ;; while we grovel around in the database.
             (file-descriptor/crc-background
              fd (rfm-file/content-type file)
              (lambda (get-crc)
                (debug-message 5 "Computing File CRC")
                  (let ((fse-crc (file-system-element/crc file))
                        (ignore  (debug-message 5 "Fetching descriptor CRC"))
                        (fd-crc  (funcall get-crc)))
                    (debug-message 5 "File CRC: ~x, FD CRC: ~x" fse-crc fd-crc)
                    (/= fse-crc fd-crc)))))

           (content-differs? ()
             (not (file-content/equal? file fd))))

      (ecase *file-content-changed-criteria*
        (:crc-only (crc-differs?))

        (:permissions-size-crc  (if (file-permissions-allow-change?)
                                    ;; User *could* have changed it, check the crc.
                                    (crc-differs?)
                                  (size-differs?)))

        (:permissions-crc (if (file-permissions-allow-change?)
                              (crc-differs?)
                            nil))

        (:content-only (content-differs?))))))

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
;;;; File Name: publish.lsp
;;;; Author:        jrm
;;;;
;;;; Module Description: Web Publishing API
;;;;
;;;; JDT: this module should be considered an extension of RFM-SERVER.LSP
;;;; and should potentially be integrated with that module.  It uses RFM-SERVER
;;;; primitives, and establishes transaction boundaries on data model access.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CSF/REPOSITORY-FILE-MANAGEMENT")

;;; API's exported from this module.
(eval-when (:load-toplevel :execute)
  (export '(rfm-file/publish
            publish-unpublish-file
            ;; rfm-server-publish-project-helper
            publish-rfm-server-publish-directory-to-file-system
            publish-rfm-server-publish-directory-to-list
            publish/error-if-overwriting
            publish/supersede-if-overwriting
            publish/punt-if-overwriting
            publish/backup-if-overwriting
            publish/backup-if-changed
            publish/overwrite-if-changed
            )))

;;; Dealing with OVERWRITE
;;;
;;; When writing files to the disk, there is the possibility that a file
;;; of the same name already exists.  What to do about this is controlled
;;; by the value of the OVERWRITE function.
;;;
;;; The overwrite function is a function of three arguments that the
;;; caller provides to handle overwrites.  The first argument will be the
;;; file-system-element that we intended to publish, the second
;;; will be the file-system that we are using, and the third will be
;;; a file-descriptor that represents the existing file on the disk.
;;;
;;; If the overwrite function returns (it is allowed to throw), it
;;; may return any of the following kinds of values:
;;;
;;; NIL means return from publish-file without publishing content,
;;;  but set the time-stamp, execute bit, and read-only bit.
;;;
;;; :set-rw means return from publish-file without publishing content,
;;;  and without altering the time-stamp or executable bit, but set
;;;  the read-only bit off.  (Used for regenerating checked out files).
;;;
;;; T means proceed with writing the contents of the file-system-element.
;;;
;;; A pathname/string means to call publish-file again with the new pathname/string
;;; as the target file.  (used for conflict resolution)
;;;
;;; A vi-record-stream means to write a record stream to the file
;;; rather than the contents of the file-system-element.  (used in merge)
;;;
;;; Example of things the overwrite function might do:
;;;
;;; Throw an error.  If the file system should be `clean', the existence of
;;;     a file is an error.
;;;
;;; Rename the file on disk.  If the file on disk is to be retained, but
;;;     should not `collide' with the file being written, it may be renamed.
;;;
;;; Return a new pathname.  If the file on disk is to be retained and must not
;;;     be renamed, the new pathname is used for the publishing.
;;;
;;; Delete the file on disk.  If the file is to be `reverted'.
;;;
;;; Return a merged record stream.  If the file is to be merged with what
;;;     is on the disk.
;;;
;;; Optionally choose one of these alternatives.  The overwrite function could
;;;     inspect the timestamp and checksum of the disk file to determine if
;;;     one version is preferred over the other.

(defun rfm-file/publish (fse fsa
                             &key
                             (filename (file-system-element/relative-path fse))
                             (publish-read-only? t)
                             when-overwriting-file)
  "Stuff a file system element down the throat of the FSA.
  Note that the read-only argument may be T, nil, or a function of
  the fse and fsa, that determines whether the read-only bit should be
  set on (T) or off (nil). This is used when regenerating a file that
  is missing, to leave checked-out files rw, and other files ro."
  (let ((probe (file-system/probe-file fsa filename))
        (content-override nil))
    (when probe
      ;; trying to overwrite
      (let ((thing (funcall when-overwriting-file fse fsa probe)))
        (cond ((null thing)             ; indicates that the file on disk should not be touched.
               ;; make file rw so attributes can be changed
               (file-system/set-read-only fsa filename nil)
               ;; Set the time stamp, if possible (and read-only, executable properties)
               ;; This is currently a no-op for those FSA's that cannot support it.
               (file-system/touch-file fsa filename
                                       (file-system-element/modification-date fse)
                                       :if-does-not-exist :create
                                       :set-read-only
                                       (if (functionp publish-read-only?)
                                           (funcall publish-read-only? fse fsa)
                                           publish-read-only?))
               (return-from rfm-file/publish nil))
              ((eq thing t)) ; indicates normal processing
              ((eq thing :set-rw) ; indicates just toggle the r-o bit
               (file-system/set-read-only fsa filename nil)
               (return-from rfm-file/publish nil))
              ((or (pathnamep thing)
                   (stringp thing)) ; indicates publish to a different name
               (rfm-file/publish fse fsa 
                                 :filename thing
                                 :publish-read-only? publish-read-only?
                                 :when-overwriting-file when-overwriting-file)
               (return-from rfm-file/publish nil))
              (t (setq content-override thing))))) ; indicates replacement content

    ;; We are copacetic (if the override function did it's job)
    (ecase (rfm-file/content-type fse)
      ;; Just fall through (and touch the file).
      (:zero (file-system/note-progress fsa (format nil "Touching ~s" fse) nil))
      (:text
       (with-file-system-stream
        (file-stream fsa
                     filename
                     :direction :output
                     :element-type 'character
                     :record-separator (resolve-record-terminator fse
                                                                  (file-system/record-separator fsa))
                     ;; overwrite function should have deleted
                     ;; or rename existing file.
                     :if-exists :error
                     :if-does-not-exist :create)
        (iterate ((line (scan 'list (or content-override
                                        (rfm-file/content fse)))))
          (file-system/write-line fsa file-stream line))))
      (:binary
       (with-file-system-stream (file-stream fsa
                                             filename
                                             :direction :output
                                             :record-separator :NONE
                                             #-(and :allegro-version>= (:version>= 6 0)) :element-type
                                             #-(and :allegro-version>= (:version>= 6 0)) '(unsigned-byte 8)
                                             :if-exists :error
                                             :if-does-not-exist :create)
       (file-system/write-bytes fsa file-stream
                                (or content-override (rfm-file/binary-content fse))
                                :end (rfm-file/binary-size fse))))
      )
    ;; Set the time stamp, if possible (and read-only, executable properties)
    ;; This is currently a no-op for those FSA's that cannot support it.
    (file-system/touch-file fsa filename
                            (file-system-element/modification-date fse)
                            :if-does-not-exist :create
                            :set-read-only
                            (if (functionp publish-read-only?)
                                (funcall publish-read-only? fse fsa)
                              publish-read-only?))))

#||
(defun publish-file (fse fsa overwrite-function &key (filename (file-system-element-relative-path fse))
                                                     (read-only t))
  "Stuff a file system element down the throat of the FSA.
  Note that the read-only argument may be T, nil, or a function of
  the fse and fsa, that determines whether the read-only bit should be
  set on (T) or off (nil). This is used when regenerating a file that
  is missing, to leave checked-out files rw, and other files ro."
  (let ((probe (file-system-probe-file fsa filename))
        (vi-stream-override nil))
    (when probe
      ;; trying to overwrite
      (let ((thing (funcall overwrite-function fse fsa probe)))
        (cond ((null thing)
               ;; make file rw so attributes can be changed
               (file-system-set-read-only fsa filename nil)
               ;; Set the time stamp, if possible (and read-only, executable properties)
               ;; This is currently a no-op for those FSA's that cannot support it.
               (file-system-touch-file fsa filename
                                       (file-system-element-modification-date fse)
                                       :if-does-not-exist :create
                                       :set-read-only
                                       (if (functionp read-only)
                                           (funcall read-only fse fsa)
                                           read-only)
                                       :set-executable (file-executable? fse))
               ;; Set the read-only bits, if necessary and possible.
               ;; Now done by touch-file, above.
               ;;(file-system-set-read-only fsa filename read-only)
               (return-from publish-file nil))
              ((eq thing t))
              ((eq thing :set-rw)
               (file-system-set-read-only fsa filename nil)
               (return-from publish-file nil))
              ((or (pathnamep thing)
                   (stringp thing))
               (publish-file fse fsa overwrite-function :filename thing :read-only read-only)
               (return-from publish-file nil))
              ((vi-stream? thing) (setq vi-stream-override thing))
              (t (error "The override function returned an illegal value: ~s" thing)))))
    ;; We are copacetic (if the override function did it's job)

    (ecase (file-system-element-get-content-type fse)
      ;; Just fall through (and touch the file).
      (:zero (file-system-note-progress fsa (format nil "Touching ~s" fse) nil))
      (:text
       (with-file-system-stream
           (file-stream fsa
            filename
            :direction :output
            :element-type 'character
            :record-terminator (resolve-record-terminator fse
                                                          (file-system-record-terminator fsa))
            ;; overwrite function should have deleted
            ;; or rename existing file.
            :if-exists :error
            :if-does-not-exist :create)
         (loop
             with vi-record-stream = (or vi-stream-override (file-get-content fse))
             while (vi-stream-ready? vi-record-stream)
             do
               (file-system-write-line
                fsa file-stream
                (vi-stream-get-value vi-record-stream)
                :suppress-termination
                ;; If it is the last record, and it is not terminated,
                ;; suppress the termination.
                (and (not (vi-stream-ready? vi-record-stream))
                     (not (vi-stream-terminated? vi-record-stream))))
               )))
      (:binary
       (with-file-system-stream (file-stream fsa
                                 filename
                                 :direction :output
                                 :record-terminator :NONE
                                 #-(and :allegro-version>= (:version>= 6 0)) :element-type
                                 #-(and :allegro-version>= (:version>= 6 0)) '(unsigned-byte 8)
                                 :if-exists :error
                                 :if-does-not-exist :create)
         (file-system-write-bytes fsa file-stream
                                  (or vi-stream-override (file-binary-contents fse))
                                  :end (file-binary-size fse))))
      )
    ;; Set the time stamp, if possible (and read-only, executable properties)
    ;; This is currently a no-op for those FSA's that cannot support it.
    (file-system-touch-file fsa filename
                            (file-system-element-modification-date fse)
                            :if-does-not-exist :create
                            :set-read-only
                            (if (functionp read-only)
                                (funcall read-only fse fsa)
                              read-only)
                            :set-executable (file-executable? fse))
    ;; Set the read-only bits, if necessary and possible.
    ;; Now done by touch-file, above.
    ;; (file-system-set-read-only fsa filename read-only)
    ))
||#

(defun publish/backup-if-overwriting (&optional (pattern "bak~@[~d~]"))
  (lambda (fse fsa probe)
    (declare (ignore fse))
    (debug-message 3 "publish-backup-if-overwriting, backing up ~s" (file-descriptor/path probe))
    (file-system/backup fsa (file-descriptor/path probe) :pattern pattern)
    t))                                 ;proceed with overwrite

(defun publish/backup-if-changed (&optional (pattern "bak~@[~d~]"))
  "A possible value for the overwrite function.  If the file is unchanged,
  nothing happens.  If there is a change, the file-descriptor is backed up."
  (lambda (fse fsa file-descriptor)
    (if (file-content-changed? fse fsa file-descriptor)
        (progn
          (debug-message 3 "publish-backup-if-changed, backing up ~s" (file-descriptor/path file-descriptor))
          (file-system/backup fsa (file-descriptor/path file-descriptor) :pattern pattern)
          t)                            ; proceed with overwrite
        (progn
          (debug-message 3 "publish-backup-if-changed, file unchanged ~s" (file-descriptor/path file-descriptor))
          nil))))                       ; punt

(defun publish/error-if-overwriting (format-string &rest format-args)
  (lambda (fse fsa probe)
    (declare (ignore fse fsa probe))
    (loop (error "~?" format-string format-args))))

(defun publish/supersede-if-overwriting (fse fsa file-descriptor)
  "A possible value for the overwrite function.  Will simply delete the file-descriptor."
  (declare (ignore fse))
  (do ()
      ((null (file-system/probe-file fsa (file-descriptor/path file-descriptor)))
       (debug-message 3 "publish-supersede-if-overwriting, deleting original file ~s."
                      (file-descriptor/path file-descriptor)))
    (file-system/delete-file fsa file-descriptor :force t))
  t)                                    ;always overwrite

(defun publish/punt-if-overwriting (fse fsa file-descriptor)
  (declare (ignore fse fsa))
  (debug-message 3 "Publish-punt-if-overwriting, skipping publishing ~s." (file-descriptor/path file-descriptor))
  nil)

(defun publish/overwrite-if-changed (fse fsa file-descriptor)
  "A possible value for the overwrite function.  If the file is unchanged,
nothing happens.  If there is a change, the file-descriptor is deleted."
  (if (file-content-changed? fse fsa file-descriptor)
      (do ()
          ((null (file-system/probe-file fsa (file-descriptor/path file-descriptor)))
           (debug-message 3 "Publish-overwrite-if-changed, deleting original file ~s."
                          (file-descriptor/path file-descriptor))
           t)                           ; overwrite
        (file-system/delete-file fsa file-descriptor :force t))
      (progn
        (debug-message 3 "Publish-overwrite-if-changed, file ~s is unchanged" (file-descriptor/path file-descriptor))
        nil)                            ;don't overwrite
      ))

(defun rfm-directory/publish-to-file-system (project branch file-system rfm-directory
                                                           &key create?
                                                           clean?
                                                           unpublish?
                                                           (publish-read-only? t)
                                                           (when-overwriting-file #'publish/error-if-overwriting)
                                                           (when-overwriting-directory #'publish/error-if-overwriting)
                                                           (report-file-system file-system))
  "Publish all directory contents to a file system, including subdirectories.

   If FILE-LIST is specified, it should be a list of file objects which are to be published,
   in other words it constrains set the files which are actually written to disk.
   This is an opportunistic hack to let me extract select files without writing the other logic
   to grovel over all the directories.  However it is currently limited in that it won't let you
   selectively publish a file to one subdirectory of rfm-directory but not another, for that
   we need to specify pathnames or rfm-directory keys in some alist format.

   *FINISH*: need another interface for FILE-LIST

   If UNPUBLISH? is true then we are removing the files rather than publishing them.

   If REPORT-FILE-SYSTEM is supplied, the progress is noted to that file system instead of the one
   that is being published to.

   See also publish-rfm-server-publish-directory-to-list which was cloned from this function.

   Returns NIL."
  (declare (ignore project branch))

  ;; Step one.  If the user wants, create the directory that the files
  ;; are going into.
  (when (and create? (not unpublish?))
    ;; This turns out to be a little tricky.  We are deliberately
    ;; stepping outside the rooted file system abstraction in order
    ;; ensure that the root exists.
    (assert (typep file-system 'logical-file-system))
    (file-system/note-progress report-file-system "Ensuring root directory exists." nil)
    (let ((rfsa (delegate-file-system/underlying-file-system file-system))
          (root (logical-file-system/root file-system)))
      ;; Walk up the directory tree.  When we get to the top, we
      ;; walk back down creating subdirectories.
      (file-system/ensure-directory-and-parents rfsa root)))

  (file-system/note-progress report-file-system "Composing view of versioned directory structure." nil)
  (let* ((all-files (file-system-element/descendants rfm-directory))
         ;; If we are going to show progress, we need a pass to figure out how
         ;; much work needs to be done.  This is a rather rough estimate because
         ;; we don't examine the remote file system to determine whether we actually
         ;; will send files to it. (It might already be updated, for instance.)

         ;; Start total-work and current-work at 1 to avoid divide-by-zero error.
         (total-work (+ (length all-files) 1))
         (current-work 1)
         (last-work-factor 0))
    (labels ((work-factor ()
               (* 5 (floor (* (/ current-work total-work) 20.))))

             (work-tick ()
               (incf current-work)
               (unless (= (work-factor) last-work-factor)
                 (setq last-work-factor (work-factor))
                 (file-system/note-progress report-file-system nil (work-factor)))))

      (unless unpublish?
        ;; Create the directory structure.
        (file-system/note-progress report-file-system "Creating subdirectories." (work-factor))
        (dolist (fse all-files)
          (when (typep fse 'rfm-directory)
            (let* ((path (file-system-element/relative-path fse))
                   (probe (file-system/probe-file file-system path)))
              (cond ((null probe)
                     ;;(file-system-create-directory  file-system path) ;doesn't create intermediate subdirs.
                     (file-system/ensure-directory-and-parents file-system path))
                    ((file-descriptor/is-directory? probe))
                    (t (funcall when-overwriting-directory fse file-system probe))))
            (work-tick))))

      ;; Clean the directory structure.  If we are to remove stuff
      ;; from the file system that does not exist in the repository,
      ;; we should do so now.  This code is similar to the change
      ;; detection code, but there are some differences that mean that
      ;; we can't just call the change detection code at this point.

      (when clean? ;; never clean directory when publish individual files.
        (file-system/note-progress report-file-system "Cleaning target directory." (work-factor))
        (let ((rfm-tree (make-tree #'string-equal #'string-lessp)))
          (debug-message 4 "Database files.")
          ;; Walk the repository to find out what is there.
          (dolist (node all-files)
            (debug-message 5 "~s" (namestring
                                   (file-system-element/relative-path node)))
            (rb-tree/insert! rfm-tree
                             (namestring
                              (file-system-element/relative-path node))
                             node))

          ;; Walk the file system.  If it isn't in the repository,
          ;; nuke it.  The difficulty here is that some systems don't
          ;; let you nuke a directory unless you nuke everything in it
          ;; first.
          (transitive-closure
           file-system
           (lambda (node accumulate)
             (declare (ignore accumulate))
             ;; Punt on directories for now...
             (unless (file-descriptor/is-directory? node)
               (debug-message 5 "~s" (namestring (file-descriptor/path node)))
               (unless (rb-tree/lookup rfm-tree (namestring (file-descriptor/path node)) nil)
                 (file-system/delete-file file-system node :force t))))
           nil)))

      ;; Now we are ready to install the files.

      (file-system/note-progress report-file-system
                                 (if unpublish?
                                     "Unpublishing files."
                                     "Publishing files.")
                                 (work-factor))

      ;; Publish the files.
      (dolist (fse all-files)
        (when (typep fse 'rfm-directory)
          (file-system/note-progress report-file-system
                                     (file-system/friendly-name
                                      (file-system-element/relative-path fse)
                                      file-system)
                                     (work-factor)))
        (when (typep fse 'rfm-file)
          (if unpublish?
              (rfm-file/unpublish fse file-system)
              (rfm-file/publish fse file-system
                                :when-overwriting-file when-overwriting-file
                                :publish-read-only? publish-read-only?))
          (work-tick))))))


(defun rfm-file/unpublish (fse fsa &key (filename (file-system-element/relative-path fse)))
  "Use the FSA to remove the file.
   FSE is a FILE-SYSTEM-ELEMENT."
  (debug-message 3 "Unpublishing ~s" filename)
  (let ((probe (file-system/probe-file fsa filename)))
    (when probe
      (file-system/delete-file fsa probe :force t))))

#||
(defun publish-rfm-server-publish-project-to-file-system (rfm-session-context fsa
                                                  file-overwrite-function
                                                  directory-overwrite-function
                                                  &key create? clean? unpublish?)

  ;; This isn't quite what I want.  The decision of whether to use an
  ;; FSA (or whether one is needed) should be deferred until here, but
  ;; the logic of the server dictates that it must be created prior to
  ;; calling this function. *sigh*

  "Arranges for the files in a project to be deployed to a file system.

   FSA denotes a logical-file-system to which we'll deploy files (and which implicitly contains
   any target directory specification).

   RFM-SESSION-CONTEXT supplies the project, version, and repository directory to be published.

   In the session context, at the very least a project or version identifier must be specified.

   If the project is specified without a version, the Latest version of the main branch is used.
   If the version is specified, that version is used to view content. (Versions currently always
   uniquely belong to projects and we can trace this backlink.)

   If project is missing and version is specified, we determine project from version backpointers.
   If project and version are specified, we assume that version is used to view project regardless
   of whether or not version resides in the project.

   If UNPUBLISH? is true, then we remove the files from the file system rather than writing
   them there."

  (with-rfm-bound-session-context (bound-context rfm-session-context "Publish project files")
    (rfm-server-infer-bound-session-context-info bound-context rfm-session-context :get-directory t)
    (let ((version (rfm-bound-session-context-version bound-context))
          (directory (rfm-bound-session-context-fse bound-context)))
      (rfm-server-publish-project-helper directory version fsa file-overwrite-function
                                         directory-overwrite-function create? clean? unpublish?))))

(defun rfm-server-publish-project-helper (directory version fsa
                                          file-overwrite-function
                                          directory-overwrite-function
                                          create? clean? unpublish?)
  (check-type directory rfm-directory)
  (with-version (version)
    (publish-rfm-server-publish-directory-to-file-system directory fsa
                                                 file-overwrite-function
                                                 directory-overwrite-function create? clean?
                                                 :unpublish? unpublish?)))

(defun publish-rfm-server-publish-directory-to-file-system (rfm-directory file-system
                                                            file-overwrite-function
                                                            directory-overwrite-function
                                                            create? clean?
                                                            &key file-list
                                                                 (read-only t)
                                                                 (unpublish? nil)
                                                                 (report-file-system file-system)
                                                                 )
  "Publish all directory contents to a file system, including subdirectories.

   If FILE-LIST is specified, it should be a list of file objects which are to be published,
   in other words it constrains set the files which are actually written to disk.
   This is an opportunistic hack to let me extract select files without writing the other logic
   to grovel over all the directories.  However it is currently limited in that it won't let you
   selectively publish a file to one subdirectory of rfm-directory but not another, for that
   we need to specify pathnames or rfm-directory keys in some alist format.

   *FINISH*: need another interface for FILE-LIST

   If UNPUBLISH? is true then we are removing the files rather than publishing them.

   If REPORT-FILE-SYSTEM is supplied, the progress is noted to that file system instead of the one
   that is being published to.

   See also publish-rfm-server-publish-directory-to-list which was cloned from this function.

   Returns NIL."

  ;; We are going to be groveling around in the RFM structures,
  ;; so we cache the results.
  (call-with-rfm-caching
    (lambda ()
        ;; (file-system-note-progress file-system (format nil "Create is ~s" create?) nil)
        ;; (file-system-note-progress file-system (format nil "Clean is ~s" clean?) nil)
        ;; (file-system-note-progress file-system (format nil "Overwrite is ~s" overwrite) nil)

        ;; Step one.  If the user wants, create the directory that the files
        ;; are going into.
        (when (and create? (not unpublish?))
          ;; This turns out to be a little tricky.  We are deliberately
          ;; stepping outside the rooted file system abstraction in order
          ;; ensure that the root exists.
          (assert (typep file-system 'logical-file-system))
          (file-system-note-progress report-file-system "Ensuring root directory exists." nil)
          (let ((rfsa (delegate-file-system-underlying-file-system file-system))
                (root (logical-file-system-root file-system)))
            ;; Walk up the directory tree.  When we get to the top, we
            ;; walk back down creating subdirectories.
            (file-system-ensure-directory-and-parents rfsa root)))

        (file-system-note-progress report-file-system "Composing view of versioned directory structure." nil)
        (let* ((all-files (or file-list
                              ;;(transitive-closure rfm-directory #'cons '())
                              (file-system-element-descendants rfm-directory)
                              ))

               ;; If we are going to show progress, we need a pass to figure out how
               ;; much work needs to be done.  This is a rather rough estimate because
               ;; we don't examine the remote file system to determine whether we actually
               ;; will send files to it. (It might already be updated, for instance.)

               ;; Start total-work and current-work at 1 to avoid divide-by-zero error.
               (total-work (+ (length all-files) 1))
               (current-work 1)
               (last-work-factor 0))
          (tail-labels ((work-factor ()
                          (* 5 (floor (* (/ current-work total-work) 20.))))

                        (work-tick ()
                          (incf current-work)
                          (unless (= (work-factor) last-work-factor)
                            (setq last-work-factor (work-factor))
                            (file-system-note-progress report-file-system nil (work-factor)))))

            (unless unpublish?
              ;; Create the directory structure.
              (file-system-note-progress report-file-system "Creating subdirectories." (work-factor))
              (dolist (fse all-files)
                (when (typep fse 'rfm-directory)
                  (let* ((path (file-system-element-relative-path fse))
                         (probe (file-system-probe-file file-system path)))
                    (cond ((null probe)
                           ;;(file-system-create-directory  file-system path) ;doesn't create intermediate subdirs.
                           (file-system-ensure-directory-and-parents file-system path)
                           )
                          ((file-descriptor-is-directory? probe))
                          (t (funcall directory-overwrite-function fse file-system probe))))
                  (work-tick))))

            ;; Clean the directory structure.
            ;; If we are to remove stuff from the file system that does
            ;; not exist in the repository, we should do so now.
            ;; This code is similar to the change detection code, but
            ;; there are some differences that mean that we can't just
            ;; call the change detection code at this point.
            (when (and clean? (not file-list)) ;; never clean directory when publish individual files.
              (file-system-note-progress report-file-system "Cleaning target directory." (work-factor))
              (let ((rfm-tree (make-tree #'string-equal #'string-lessp)))
                (debug-message 4 "Database files.")
                ;; Walk the repository to find out what is there.
                (dolist (node all-files)
                  (debug-message 5 "~s" (namestring
                                         (file-system-element-relative-path node)))
                  (rb-tree/insert! rfm-tree
                                   (namestring
                                    (file-system-element-relative-path node))
                                   node))

                ;; Walk the file system.  If it isn't in the repository,
                ;; nuke it.  The difficulty here is that some systems don't
                ;; let you nuke a directory unless you nuke everything in it
                ;; first.
                (transitive-closure
                    file-system
                  (lambda (node accumulate)
                      (declare (ignore accumulate))
                      ;; Punt on directories for now...
                      (unless (file-descriptor-is-directory? node)
                        (debug-message 5 "~s" (namestring (file-descriptor-path node)))
                        (unless (rb-tree/lookup rfm-tree (namestring (file-descriptor-path node)) nil)
                          (file-system-delete-file file-system node :force t))))
                  nil)))

            ;; Now we are ready to install the files.

            (file-system-note-progress report-file-system
                                       (if unpublish?
                                           "Unpublishing files."
                                         "Publishing files.")
                                       (work-factor))
            ;; Publish the files.
            (dolist (fse all-files)
              (when (typep fse 'rfm-directory)
                (file-system-note-progress report-file-system
                                           (file-system-friendly-name
                                            (file-system-element-relative-path fse)
                                            file-system)
                                           (work-factor)))
              (when (typep fse 'file)
                (if unpublish?
                    (publish-unpublish-file fse file-system)
                  (publish-file fse file-system file-overwrite-function :read-only read-only))
                (work-tick))))))))

(defun publish-rfm-server-publish-directory-to-list
    (rfm-directory file-system
     &key file-list
          )
  "Publish all directory contents to a list as filepath names.

   If FILE-LIST is specified, it should be a list of file objects which are to be published,
   in other words it constrains set the files which are actually written to disk.
   This is an opportunistic hack to let me extract select files without writing the other logic
   to grovel over all the directories.  However it is currently limited in that it won't let you
   selectively publish a file to one subdirectory of rfm-directory but not another, for that
   we need to specify pathnames or rfm-directory keys in some alist format. Nil means do all the
   files.

   *FINISH*: need another interface for FILE-LIST

   Returns the list of files (with relative path names) and the directories (also relative).

   See also publish-rfm-server-publish-directory-to-file-system (from which this was cloned)."

  ;; We are going to be groveling around in the RFM structures,
  ;; so we cache the results.
  (declare (ignore file-system))
  (let ((result-file-list nil))
    (call-with-rfm-caching
     (lambda ()

         ;; Step one. Make up the list to work on
         (let* ((all-files (or file-list
                               ;;(transitive-closure rfm-directory #'cons '())
                               (file-system-element-descendants rfm-directory)
                               )))

           ;; Publish the files.
           (dolist (fse all-files)
             (let ((file-name (file-system-element-relative-path fse)))
               (debug-message 5 "Publish F-List (~s) ~s~%"
                              (if (typep fse 'file) "file"
                                (if (typep fse 'rfm-directory) "rdir"
                                  "unkn"))
                              file-name)
               (when (typep fse 'file)
                   (push file-name
                         result-file-list))
               ))

           )))
    result-file-list))


(defun rfm-server-publish-project-to-website (rfm-session-context sfs)
  "Publish to a web site, which is called here a POST.
   This attempts to use Microsoft or other vendor specific interfaces,
   but which we may later wish to have use simple FSA capabilities.

   RFM-SESSION-CONTEXT must name a repository and project.
   Project directory and version information is currently ignored.  Of perhaps more interest
   would be a meta-version specification for viewing the project website information, but we don't have that
   implemented yet.

   Return value: the web site home page url (a string).:"
  (assert (rfm-session-context-repository-id rfm-session-context))
  (assert (rfm-session-context-project-did rfm-session-context))

  (file-system-note-progress sfs "Publishing...." nil)

  (with-rfm-bound-session-context (bound-context rfm-session-context "Post project files")
    (rfm-server-infer-bound-session-context-info bound-context rfm-session-context :get-directory t)

    (let* ((project (rfm-bound-session-context-project bound-context))
           (userinfo (rfm-session-context-user-info rfm-session-context))
           (password (rfm-session-context-password rfm-session-context))
           (websites (rfm-project-website-list project)))
      ;; Publish files from temp directory to web server.
      (cond ((null websites) (error "No websites associated with project"))
            ((cdr websites) (error "Multiple websites associated with project"))
            (t (let* ((website (car websites))
                      (web-url (website-home-page-url website))
                      (webservers (website-webserver-list website)))
                 (cond ((null webservers) (error "No webservers associated with website."))
                       ((cdr webservers) (error "Multiple webservers associated with website."))
                       (t (let* ((webserver (car webservers))
                                 (content-server (rweb-server-create
                                                  (webserver-url webserver)
                                                  (webserver-os-type webserver)
                                                  (webserver-software-type webserver))))
                            (declare (ignore content-server)) ; for now

                            (let ((version (rfm-bound-session-context-version bound-context))
                                  (directory (rfm-bound-session-context-fse bound-context))
                                  (create? t)
                                  (clean? t)
                                  (directory-overwrite-function #'supersede-if-overwriting)
                                  (file-overwrite #'supersede-if-overwriting))

                              (call-with-frontpage-file-system web-url userinfo password
                                (lambda (string)
                                    (file-system-note-progress sfs string nil))
                                (lambda ()
                                    (file-system-note-progress sfs "Login incorrect." nil)
                                    (return-from rfm-server-publish-project-to-website nil)
                                    )


                                (lambda (frontpage-fsa)
                                    (let ((fsa (url-file-system-create web-url frontpage-fsa)))
                                      (file-system-note-progress sfs "Server has been contacted, computing work." nil)

                                      (with-version (version)
                                        ;; We are going to be groveling around in the RFM structures,
                                        ;; so we cache the results.
                                        (call-with-rfm-caching
                                          (lambda ()
                                              ;; (file-system-note-progress fsa (format nil "Create is ~s" create?) nil)
                                              ;; (file-system-note-progress fsa (format nil "Clean is ~s" clean?) nil)
                                              ;; (file-system-note-progress fsa (format nil "Overwrite is ~s" overwrite) nil)

                                              ;; Step one.  If the user wants, create the directory that the files
                                              ;; are going into.

                                              ;; No need to create file system on frontpage web.  It will happen for us
                                              ;; (I think).
                                              (when create? nil)

                                              ;; If we are going to show progress, we need a pass to figure out how
                                              ;; much work needs to be done.  This is a rather rough estimate because
                                              ;; we don't examine the remote file system to determine whether we actually
                                              ;; will send files to it. (It might already be updated, for instance.)

                                              ;; Start total-work and current-work at 1 to avoid divide-by-zero error.
                                              (let ((total-work (transitive-closure directory
                                                                  (lambda (fse work)
                                                                      (declare (ignore fse))
                                                                      (1+ work))
                                                                  1))
                                                    (current-work 1))
                                                (flet ((work-factor ()
                                                         (floor (* (/ current-work total-work) 100.))))


                                                  ;; Create the directory structure.
                                                  (file-system-note-progress sfs "Creating subdirectories." (work-factor))
                                                  (transitive-closure directory
                                                    (lambda (fse accumulate)
                                                        (declare (ignore accumulate))
                                                        (when (typep fse 'rfm-directory)
                                                          (let* ((path (file-system-element-relative-path fse))
                                                                 (probe (file-system-probe-file fsa path)))
                                                            (cond ((null probe)
                                                                   (file-system-create-directory  fsa path))
                                                                  ((file-descriptor-is-directory? probe))
                                                                  (t (funcall directory-overwrite-function fse fsa probe))))
                                                          (incf current-work)
                                                          (file-system-note-progress sfs nil (work-factor))))
                                                    nil)

                                                  ;; Clean the directory structure.
                                                  ;; If we are to remove stuff from the file system that does
                                                  ;; not exist in the repository, we should do so now.
                                                  ;; This code is similar to the change detection code, but
                                                  ;; there are some differences that mean that we can't just
                                                  ;; call the change detection code at this point.
                                                  (when clean?
                                                    (file-system-note-progress sfs "Cleaning target directory." (work-factor))
                                                    (let ((rfm-tree (make-tree #'string-equal #'string-lessp)))
                                                      (debug-message 4 "Database files.")
                                                      ;; Walk the repository to find out what is there.
                                                      (transitive-closure directory
                                                        (lambda (node accumulate)
                                                            (declare (ignore accumulate))
                                                            (debug-message 5 "~s" (namestring
                                                                                   (file-system-element-relative-path node)))
                                                            (rb-tree/insert! rfm-tree
                                                                             (namestring
                                                                              (file-system-element-relative-path node))
                                                                             node))
                                                        nil)
                                                      ;; Walk the file system.  If it isn't in the repository,
                                                      ;; nuke it.  The difficulty here is that some systems don't
                                                      ;; let you nuke a directory unless you nuke everything in it
                                                      ;; first.
                                                      (transitive-closure fsa
                                                        (lambda (node accumulate)
                                                            (declare (ignore accumulate))
                                                            ;; Punt on directories for now...
                                                            (unless (file-descriptor-is-directory? node)
                                                              (debug-message 5 "~s" (namestring (file-descriptor-path node)))
                                                              (unless (rb-tree/lookup rfm-tree (namestring (file-descriptor-path node)) nil)
                                                                (file-system-delete-file fsa node :force t))))
                                                        nil)))

                                                  ;; Now we are ready to install the files.

                                                  (file-system-note-progress sfs "Publishing files." (work-factor))
                                                  ;; Publish the files.
                                                  (transitive-closure directory
                                                    (lambda (fse accumulate)
                                                        (declare (ignore accumulate))
                                                        (when (typep fse 'rfm-directory)
                                                          (file-system-note-progress fsa
                                                                                     (file-system-friendly-name
                                                                                      (file-system-element-relative-path fse)
                                                                                      fsa)
                                                                                     (work-factor)))

                                                        (when (typep fse 'file)
                                                          (publish-file fse fsa file-overwrite)
                                                          (incf current-work)
                                                          (file-system-note-progress sfs nil (work-factor))
                                                          ))
                                                    nil))))))
                                      (file-system-note-progress sfs "Updating server information." 100)
                                      ))))

                            (website-home-page-url website)
                            )))))))))
||#

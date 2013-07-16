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
;;;; Module Description: eXtensions to the VM package.
;;;; Namely, a couple of routines which work to derive information from
;;;; Project and Version objects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CSF/REPOSITORY-FILE-MANAGEMENT")

;;; API's exported from this module.
(eval-when (:load-toplevel :execute)
  (export '(project-checkin
	    ;; version-list-fse-changes
            )))


;;; This PROJECT method is defined after version methods because it relies
;;; on the WITH-VERSION macroexpansion.

(defun project-checkin (repository rfm-project rfm-branch file-system
                                   &key 
                                   file-additions
                                   file-changes
                                   file-removals)
  (declare (ignore rfm-branch))
  (let* ((root-directory (rfm-project/root-directory rfm-project))
         (directory-contents-cache (make-hash-table))

         (project-filter (complement
                          (lambda (file-change)
                            (change-context/applies-to-project? file-change rfm-project))))

         (fileadds-for-project
          (remove-if project-filter file-additions))

         (filechanges-for-project
          (remove-if project-filter file-changes))

         (fileremovals-for-project
          (remove-if project-filter file-removals))

         ;; For progress meter.
         ;; Start total-work and current-work at 1 to avoid divide-by-zero error.
         (total-work (+
                      (length fileadds-for-project)
                      (length filechanges-for-project)
                                        ;(length filerenames-for-project)
                      (length fileremovals-for-project)
                      1))
         (current-work 1)
         (last-work-factor 0))

    (labels ((write-directory-contents-cache ()
               (unless (zerop (hash-table-count directory-contents-cache))
                 (maphash (lambda (directory contents)
                            (debug-message 4 "Storing ~s ~s" directory contents)
                            (setf (container/containee-list directory) contents))
                          directory-contents-cache)))

             (get-directory-contents (directory)
               (let ((result
                      (or (gethash directory directory-contents-cache)
                          (let ((contents (rfm-directory/content-list directory)))
                            (debug-message 4 "directory contents are not cached")
                            (setf (gethash directory directory-contents-cache) contents)
                            contents))))
                 (debug-message 4 "get-directory-contents ~s ~s" directory result)
                 result))

             (push-directory-contents (directory new-value)
               (debug-message 4 "push-directory-contents ~s ~s" directory new-value)
               (let ((contents (get-directory-contents directory)))
                 (assert (not (member new-value contents))))
               (setf (gethash directory directory-contents-cache)
                     (cons new-value (get-directory-contents directory))))

             (delete-directory-item (directory value)
               (debug-message 4 "delete-directory-contents ~s ~s" directory value)
               (setf (gethash directory directory-contents-cache)
                     (delete value (get-directory-contents directory))))

             (fse-find-child (parent child-name)
               (find child-name (get-directory-contents parent)
                     :key #'file-system-element/name
                     :test #'string-equal))

             (find-or-create-subdirectory (parent path)
               "Walk down the directory hierarchy, creating subdirs if necessary."

               ;; NOTE:  You could run into the case of looking for subdirectory FOO and
               ;; finding file FOO in its place.  This is tricky and we are punting for now by
               ;; simply asserting that it doesn't happen.  ~jrm
               (assert (typep parent 'rfm-directory))

               (if (null path)
                   parent               ; found it!
                   (find-or-create-subdirectory
                    (or (fse-find-child parent (car path))
                        (let ((new-dir (make-instance 'rfm-directory
                                                      :element-name (car path)
                                                      :modification-date (get-universal-time))))
                          (push-directory-contents parent new-dir)
                          new-dir))
                    (cdr path))))

             (work-factor ()
               (* 5 (floor (* (/ current-work total-work) 20.))))

             (work-tick ()
               (incf current-work)
               (unless (= (work-factor) last-work-factor)
                 (setq last-work-factor (work-factor))
                 (file-system/note-progress file-system nil (work-factor))))

             (checkin-phase (name noise elements process-one-element)
               (debug-message 3 "Checkin phase ~a" name)
               (file-system/note-progress file-system (format nil "~@?" noise (length elements)) nil)
               (dolist (element elements)
                 (debug-message 4 "Processing ~s" element)
                 (funcall process-one-element element)
                 (work-tick)))

             )

      ;; First, process directory addition.
      (let ((directories (collect 'list 
                                  (choose-if #'directory-pathname? 
                                             (#M change-context/pathname
                                                 (scan 'list file-additions))))))
        (when directories
          (checkin-phase
           "adding directories."
           "Adding ~d director~@:p"
           directories
           (lambda (dirpath)
             (find-or-create-subdirectory root-directory
                                          (cdr (pathname-directory dirpath)))))))

      (checkin-phase
       "adding files."
       "Adding ~d file~:p"
       (collect 'list (let ((additions (scan 'list file-additions)))
                        (choose (#M file-pathname?
                                    (#M change-context/pathname
                                        additions))
                                additions)))
       (lambda (fileadd)
         (let* ((filepath (change-context/pathname fileadd))
                (directory (find-or-create-subdirectory root-directory
                                                        (cdr (pathname-directory filepath))))
                (descriptor (file-system/probe-file file-system filepath)))
           (debug-message 4 "Adding ~s ~s" directory filepath)
           (unless descriptor
             (error "The file named ~s which was to be added as part of the change in progress ~
                          was not found on disk at the time project checkin occurred.  Change processing ~
                          will be abandoned."
                    (namestring filepath)))
           (let* ((content-type     (file-descriptor/content-type descriptor))
                  (binary? (media-type/binary? content-type))
                  (content-encoding (file-descriptor/content-encoding descriptor)))
             (push-directory-contents
              directory
              (call-with-file-descriptor-content 
               descriptor (file-descriptor/record-separator descriptor)
               (lambda (content)
                 (let ((file (make-instance 'rfm-file
                                            :element-name      (file-descriptor/namestring descriptor)
                                            :modification-date (file-descriptor/modification-date descriptor)

                                            :content-type      content-type
                                            :binary-content    (when binary? content)
                                            :content-encoding  content-encoding
                                            :record-separator  (unless binary?
                                                                 (file-descriptor/record-separator descriptor)))))
                   (debug-message 5 "Checked in file ~s" file)
                   ;; (break)
                   (unless binary?
                     (setf (rfm-file/content file) content))
                   file))))))))

      (checkin-phase 
       "process changes."
       "Changing ~d file~:p" 
       filechanges-for-project
       (lambda (filechange)
         (debug-message 4 "Changing ~s" filechange)
         (let* ((file (repository/resolve-distributed-identifier repository (change-context/file-did filechange)))
                (filepath (file-system-element/relative-path file))
                (descriptor (file-system/probe-file file-system filepath)))
           (unless descriptor
             (error "The file named ~s which was to be altered as part of the change in progress ~
                      was not found on disk at the time project checkin occurred.  Change processing ~
                      will be abandoned."
                    (decode-pathname (namestring filepath))))
           (call-with-file-descriptor-content
            descriptor (file-descriptor/record-separator descriptor)
            (lambda (contents)
              (setf (rfm-file/content file) contents)
              )))))

      (checkin-phase
       "process deletions."
       "Deleting ~d file~:p"
       fileremovals-for-project
       (lambda (file-removal)
         (let* ((file (repository/resolve-distributed-identifier
                       repository
                       (change-context/file-did file-removal)))
                (directory (file-system-element/directory file)))
           (delete-directory-item directory file))))

      (write-directory-contents-cache))))

;(defun project-checkin (project file-system change-context
;			&key
;			branch	 ;;defaulted below to (project-get-main-branch project)
;			version	 ;;defaulted below to (branch-get-latest-version branch)
;			(root-directory (or (rfm-project-root-directory project)
;					    (set-rfm-project-root-directory
;					     project (rfm-directory-create ""))))
;			cset-dids-to-add cset-dids-to-remove
;			(mark-read-only nil))
;  "Effect changes described in CHANGE-CONTEXT and accessible through FILE-SYSTEM.

;   Branch, if unspecified, defaults to the main project branch.

;   Version, if unspecified, defaults to the latest version on the branch
;   (not sure another default will ever make sense for that matter).

;   Root-Directory, if unspecified, defaults to the project root directory.

;   Note that branch and version information which may exist in the change-context as DID strings
;   is not used at this point, we assume the caller uses that information to pass the appropriate
;   resolved repository entities to this routine.

;   CSET-DIDS-TO-{ADD,REMOVE} are (possibly empty) lists of change-set DIDs which are to be
;   added to or removed from version used to view content (against which we'll apply changes).
;   These modifiers are typically derived from workspace virtual private branches.
;   These arguments must be CHANGE-SET DIDS, not cid-object dids, or cids.

;   Returns 3 values,
;   1) The version which we modify to derive a view of file content
;   2) A list of conses containing file-system and file-pathname, presumably to use later
;      in setting read-only bits of files that were checked in.
;   3) The cid-set that we ultimately used as the basis against which change was made.  It will include
;      the current cid making this change transaction.

;   If MARK-READ-ONLY is nil, the default, the second return value will be an empty list.
;   Otherwise the second return value represents the deferred file-system-set-read-only calls
;   needed to set the files read only.  They are deferred precisely because we do not want
;   to do them if any errors occur in any subsystem, so they are done by the caller after
;   all the main error producing work has been done.

;   All R/W transaction, abort-if-no-change, cid promotion and checkpointing logic
;   should be handled by the caller."

;  ;; We initialize here so that if the branch keyword has a supplied value of nil, we will
;  ;; still default things correctly
;  (unless branch (setq branch (project-get-main-branch project)))
;  (unless version (setq version (branch-get-latest-mutable-version branch)))
;  (unless version
;    (error "branch is frozen."))
;  (when (not (eq version (branch-get-most-recent-version branch)))
;    ;; for now, anything else requires semantic considerations
;    (error "Checking files into a version which isn't LATEST!"))
;  (check-type version version)

;  (debug-message 2 "Project checkin.")
;  (with-version (version
;		 :include-current t		; view directory contents in appropriate cid-set context
;		 :cset-dids-to-add    cset-dids-to-add
;		 :cset-dids-to-remove cset-dids-to-remove)

;    ;; At this point, we are ready to apply the changes to the repository.

;    (let* ((directory-contents-cache (make-hash-table))
;	   (read-onlys nil)
;	   (file-system (if (change-context-client-directory-pathname change-context)
;			    (logical-file-system-change-directory
;			     file-system
;			     (change-context-client-directory-pathname change-context))
;			  file-system))
;	   (project-did (distributed-object-identifier project))
;	   (project-filter (complement
;			    (lambda (cc-filething)
;				(cc-file-base-change-applies-to-project-p cc-filething
;									  project-did))))
;	   (fileadds-for-project
;	    (remove-if project-filter
;		       (change-context-file-additions change-context)))
;	   (filechanges-for-project
;	    (remove-if project-filter
;		       (change-context-file-changes change-context)))
;	   (fileremovals-for-project
;	    (remove-if project-filter
;		       (change-context-file-removals change-context)))
;	   (filerenames-for-project
;	    (remove-if project-filter
;		       (change-context-file-renames change-context)))
;	   ;; For progress meter.
;	   ;; Start total-work and current-work at 1 to avoid divide-by-zero error.
;	   (total-work (+
;			(length fileadds-for-project)
;			(length filechanges-for-project)
;			(length filerenames-for-project)
;			(length fileremovals-for-project)
;			1))
;	   (current-work 1)
;	   (last-work-factor 0)
;	   )

;      ;; Every time we call SET-CONTAINER-CONTAINEES on a directory,
;      ;; we are doing an expensive operation.  We defer that operation
;      ;; until we have all the files installed.
;      (tail-labels ((write-directory-contents-cache ()
;		      (unless (zerop (hash-table-count directory-contents-cache))
;			(maphash (lambda (directory contents)
;				   (debug-message 4 "Storing ~s ~s" directory contents)
;				   (set-container-containees directory contents)
;				   ;; (astore-flush-changes-for-object directory)
;				   )
;				 directory-contents-cache)))

;		    (get-directory-contents (directory)
;		      (let ((result
;			     (or (gethash directory directory-contents-cache)
;				 (let ((contents (rfm-directory-content-list directory)))
;				   (debug-message 4 "directory contents are not cached")
;				   (setf (gethash directory directory-contents-cache) contents)
;				   contents))))
;			(debug-message 4 "get-directory-contents ~s ~s" directory result)
;			result))

;		    (push-directory-contents (directory new-value)
;		      (debug-message 4 "push-directory-contents ~s ~s" directory new-value)
;		      (let ((contents (get-directory-contents directory)))
;			(assert (not (member new-value contents))))
;		      (setf (gethash directory directory-contents-cache)
;			    (cons new-value (get-directory-contents directory))))

;		    (delete-directory-item (directory value)
;		      (debug-message 4 "delete-directory-contents ~s ~s" directory value)
;		      (setf (gethash directory directory-contents-cache)
;			    (delete value (get-directory-contents directory))))

;		    (fse-find-child (parent child-name)
;		      (find child-name (get-directory-contents parent)
;			    :key #'file-system-element-name
;			    :test #'string-equal))

;		    (find-or-create-subdirectory (parent path)
;		      "Walk down the directory hierarchy, creating subdirs if necessary."

;		      ;; NOTE:  You could run into the case of looking for subdirectory FOO and
;		      ;; finding file FOO in its place.  This is tricky and we are punting for now by
;		      ;; simply asserting that it doesn't happen.  ~jrm
;		      (assert (typep parent 'rfm-directory))

;		      (if (null path)
;			  parent		; found it!
;			(find-or-create-subdirectory
;			 (or (fse-find-child parent (car path))
;			     (let ((new-dir (rfm-directory-create (car path))))
;			       (push-directory-contents parent new-dir)
;			       new-dir))
;			 (cdr path))))

;		    (work-factor ()
;		      (* 5 (floor (* (/ current-work total-work) 20.))))

;		    (work-tick ()
;		      (incf current-work)
;		      (unless (= (work-factor) last-work-factor)
;			(setq last-work-factor (work-factor))
;			(file-system-note-progress file-system nil (work-factor))))

;		    (maybe-mark-read-only (pathname)
;		      (when mark-read-only
;			(push (cons file-system pathname) read-onlys)))

;		    (checkin-phase (name noise elements process-one-element)
;		      (debug-message 3 "Checkin phase ~a" name)
;		      (file-system-note-progress file-system (format nil "~@?" noise (length elements)) nil)
;		      (dolist (element elements)
;			(debug-message 4 "Processing ~s" element)
;			(funcall process-one-element element)
;			(work-tick)))
;		    )

;	;; First, process directory addition.
;	(let ((directories
;	       (remove-if (complement #'directory-pathname?)
;			  (mapcar #'cc-fileadd-pathname fileadds-for-project))))
;	  (when directories
;	    (checkin-phase
;	     "adding directories."
;	     "Adding ~d director~@:p"
;	     directories
;	     (lambda (dirpath)
;	       (find-or-create-subdirectory root-directory (cdr (pathname-directory dirpath)))))))

;	;; Second, process file additions.
;	;; Filter files by qualifying subsystem, since there may be cc-fileadd
;	;; records which are not intended for this PROJECT instance.
;	(checkin-phase
;	 "adding files."
;	 "Adding ~d file~:p"
;	 (loop for add in fileadds-for-project
;	       for pn = (cc-fileadd-pathname add)
;	       unless (directory-pathname? pn)
;	       collect (cons pn (cc-fileadd-content-type add))) ; destructured below
;	 (lambda (fileadd-info)
;	     (let ((filepath              (car fileadd-info))
;		   (content-type-declared (cdr fileadd-info)))
;	       (check-type filepath logical-pathname)

;	       ;; By calling find-or-create-subdirectory here, we will automatically create
;	       ;; subdirectories even if the user deselected the subdir in the option list.
;	       ;; This may not be the right thing to do.  Other options include not allowing
;	       ;; the user to add files without the subdir by throwing an error at this point,
;	       ;; or by making the browser automatically synchronize the selection of file
;	       ;; creation and subdir creation.
;	       (let ((directory (find-or-create-subdirectory root-directory
;							     (cdr (pathname-directory filepath))))
;		     (fd (file-system-probe-file file-system filepath)))
;		 (debug-message 4 "Adding ~s ~s" directory filepath)
;		 (unless fd
;		   (error "The file named ~s which was to be added as part of the change in progress ~
;                          was not found on disk at the time project checkin occurred.  Change processing ~
;                          will be abandoned."
;			  (namestring filepath)))
;		 (maybe-mark-read-only fd)
;		 (push-directory-contents
;		  directory
;		  (let* ((mime-type
;			  (multiple-value-call #'mime-content-create
;			    (file-descriptor-mime-type fd content-type-declared)))
;			 (record-terminator
;			  (when (eq (mime-content-file-mode mime-type) :TEXT)
;			    (if (eq (file-descriptor-record-terminator fd)
;				    (file-system-record-terminator
;				     (file-descriptor-file-system fd)))
;				:PLATFORM
;			      (file-descriptor-record-terminator fd))))
;			 )
;		    (call-with-file-descriptor-content fd (file-descriptor-record-terminator fd)
;		      (lambda (content)
;			  (file-create (file-descriptor-modification-date fd)
;				       (file-descriptor-file-namestring fd)
;				       mime-type
;				       content
;				       record-terminator
;				       ;; capture any FS executable bit
;				       (and (platform-has-file-executable-bit?
;					     (file-system-platform file-system))
;					    (file-descriptor-executable? fd))
;				       )
;			  )
;		      :content-type content-type-declared)))
;		 ;; (astore-flush-changes :verbose t)  ;; write out file contents for each file
;		 ))))

;	;; Process renames. Must occur BEFORE changes so they access filesys using the new name.
;	(checkin-phase
;	 "process renames."
;	 "Renaming ~d file~:p"
;	 filerenames-for-project
;	 (lambda (cc-filerename)
;	     (debug-message 4 "Renaming ~s to ~s"
;			    (cc-filerename-file-did cc-filerename)
;			    (cc-filerename-new-pathname cc-filerename))
;	     (file-rename (repository-resolve-distributed-identifier
;			   *repository* (cc-filerename-file-did cc-filerename))
;			  (file-system-file-namestring
;			   file-system (cc-filerename-new-pathname cc-filerename)))
;	     (maybe-mark-read-only (cc-filerename-new-pathname cc-filerename))))

;	;; (astore-flush-changes :verbose t) ;; write out new file names

;	;; Process changes. Must occur AFTER renames so that we access filesys with new name.
;	(checkin-phase
;	 "process changes."
;	 "Changing ~d file~:p"
;	 filechanges-for-project
;	 (lambda (filechange)
;	     (debug-message 4 "Changing ~s" filechange)
;	     (let* ((file (repository-resolve-distributed-identifier
;			   *repository*
;			   (cc-filechange-file-did filechange)))
;		    (filepath (file-system-element-relative-path file))
;		    (fd (file-system-probe-file file-system filepath)))
;	       (unless fd
;		 (error "The file named ~s which was to be altered as part of the change in progress ~
;                      was not found on disk at the time project checkin occurred.  Change processing ~
;                      will be abandoned."
;			(decode-pathname (namestring filepath))))
;	       (maybe-mark-read-only fd)
;	       (file-set-content file fd))
;	     ;; (astore-flush-changes :verbose t) ;; write out new contents
;	     ))

;	;; Process deletions
;	(checkin-phase
;	 "process deletions."
;	 "Deleting ~d file~:p"
;	 fileremovals-for-project
;	 (lambda (deletion-pair)
;	     (let* ((file (repository-resolve-distributed-identifier
;			   *repository*
;			   (cc-fileremove-file-did deletion-pair)))
;		    (directory (file-system-element-directory file)))
;	       (delete-directory-item directory file))))

;	(write-directory-contents-cache)
;	;; (astore-flush-changes :verbose t) ;; save out directory contents

;	#||
;	;; Remove those changes which have been processed from the change context
;	(setf (change-context-file-additions change-context)
;	      (set-difference (change-context-file-additions change-context)
;			      fileadds-for-project))
;	(setf (change-context-file-changes change-context)
;	      (set-difference (change-context-file-changes change-context)
;			      filechanges-for-project))
;	(setf (change-context-file-removals change-context)
;	      (set-difference (change-context-file-removals change-context)
;			      fileremovals-for-project))
;	(setf (change-context-file-renames change-context)
;	      (set-difference (change-context-file-renames change-context)
;			      filerenames-for-project))
;	||#

;	(debug-message 3 "Checkin finished.")
;	;; Done!  Return the version we modified, though it's callers responsibility to promote cid
;	;; into version...
;	(values version read-onlys (txn-context-cid-set *txn-context*))))))

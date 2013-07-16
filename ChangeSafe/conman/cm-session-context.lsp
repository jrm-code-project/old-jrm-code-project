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
;;;; File Name:     cm-session-context.lsp
;;;; Author:        Dave Tenny
;;;; Creation Date: September 1999 - Derived from rfm-session-context.lsp
;;;;
;;;; Module Description: Session context information for accessing CM
;;;; capabilities.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CONMAN")

#||
(eval-when (:load-toplevel :execute)
  (export '(;; cm-session-context-*
	    cm-session-context-add
	    cm-session-context-create
	    ;; cm-session-context-create-from-http-connection
	    cm-session-context-http-connection
	    cm-session-context-allow-busy-redirect
	    cm-session-context-repository-name ;master repository
	    cm-session-context-user-name
	    cm-session-context-user-home-directory
	    cm-session-context-rc-path	;client path of .csf used to supply information to context
	    cm-session-context-product-family-name
	    cm-session-context-pc-name
	    cm-session-context-class-name
	    cm-session-context-subsystem-name
	    cm-session-context-ws-id	;an integer, or a string naming a client workspace path
	    cm-session-context-workspace-root
	    cm-session-conetxt-return-code
	    cm-session-context-time-spec ;a TIME-STAMP object, or NIL, typically for metaversion computation
	    cm-session-context-verbosity
	    cm-session-context-file-system-agent
	    cm-session-context-workspace-rooted-file-system
	    )))

;; All 'name' slots are strings, pathnames, integers, or nil.

(define-tenn-class cm-session-context
    (:documentation "State of known context for ChangeSafe server activity on behalf of user session.")
  (http-connection nil)                 ;connection that the user is coming in on
  (allow-busy-redirect T)               ; if NIL, we must handle the request if possible, if T, we can redirect.
  (repository-name nil)			;full pathname of a repository
  (user-name nil)			;OS or otherwise supplied user name
  (client-platform nil)			;Keyword describing the client's OS.  Used to parse pathnames.
  (client-timezone nil)			;offset in minutes from GMT of client system
  ;; May not be necessary?  Should be able to resolve relative names in the client,
  ;; but if we do server relative, we need this.
  (current-directory nil)		;OS or otherwise supplied current directory
  (user-home-directory nil)		;OS or otherwise supplied home directory of user

  (rc-path nil)				;client path of .csf file, or NIL if there wasn't one used.
  (pc-name nil)				;Product-configuration (PC) name
  (class-name nil)			;Satellite project name, may need to suppport lists of them
  (subsystem-name nil)			;subsystem name.
  (release-name nil)			;release specification, typically names a branch on a PC
  (time-spec nil)			;time specification, TIME-STAMP object, or nil.
  (label-name nil)			;label name, identifying a label on a branch(??), *TBD*
  (ws-id nil)				;nil, integer, or file namestring identifying a workspace
  (verbosity nil)				;will be set to 0, 1 or 2 based on highest switch -v0, -v1, -v2
  (file-system-agent nil)               ;nil, or the file-system-agent currently working on the user's behalf
  )

(defun cm-session-context-create (http-connection &key
						  allow-busy-redirect
						  user-name client-platform current-directory rc-path
				       repository-name pc-name class-name
				       subsystem-name release-name time-spec label-name ws-id)
  "Create an CM-SESSION-CONTEXT object.
   All arguments should be pathnames or strings except WS-ID, which may be a string or an integer.
   If it's a string which contains an integer representation, it is converted to an integer."
  (check-type repository-name (or null pathname))
  (check-type rc-path (or null pathname))
  (check-type current-directory (or null pathname))
  (check-type allow-busy-redirect (or null boolean))
  (flet ((check-string (val name)
	   (when (and val (empty-string? val))
	     (error "~a must not be an empty string." name))))
					;(check-string repository-name "Repository name")
    (check-string user-name "User name")
    (check-string client-platform "Client platform")
					;(check-string current-directory "Current directory")
					;(check-string rc-path "Resource configuration path, i.e. .csf path")
    (check-string pc-name "Product configuration name")
    (check-string class-name "Class name")
    (check-string subsystem-name "Subsystem name")
    (check-string release-name "Release name")
    (check-string time-spec "Time specification")
    (check-string label-name "Label name")
    (check-string ws-id "Workspace identifier")
    )

  ;; *FINISH*: further syntax check time specifications for validity

  (make-instance 'cm-session-context
    :cm-session-context-http-connection http-connection
    :cm-session-context-allow-busy-redirect allow-busy-redirect
    :cm-session-context-repository-name (when repository-name
					  (guarantee-absolute-pathname repository-name))
    :cm-session-context-user-name user-name
    :cm-session-context-client-platform client-platform
    :cm-session-context-current-directory (when current-directory
					    (guarantee-absolute-pathname current-directory))
    :cm-session-context-rc-path (when rc-path
				  (guarantee-absolute-pathname rc-path))
    :cm-session-context-pc-name pc-name
    :cm-session-context-class-name class-name
    :cm-session-context-subsystem-name subsystem-name
    :cm-session-context-release-name release-name
    :cm-session-context-time-spec time-spec
    :cm-session-context-label-name label-name
    :cm-session-context-ws-id (or (and (stringp ws-id)
				       (parse-integer ws-id :junk-allowed t))
				  ws-id)
    ))

(defun cm-session-context-add (cm-session-context
			       &key repository-name pc-name class-name
				    subsystem-name release-name time-spec
				    label-name ws-id rc-path user-name
				    file-system-agent verbosity)
  "Add data to a session context which is being initialized, perform basic syntactic checking.
   All arguments should be strings or pathnames except WS-ID, which may be a string or an integer.
   If it's a string which contains an integer representation, it is converted to an integer.
   Note that rc-path and user-name will only rarely be used.
   Return the session context."
  #+allegro (declare (:fbound cm-cli-parse-conman-time-spec
		    cm-cli-parse-repository-name))
  (check-type repository-name (or null string db-name))
  (flet ((doit (val name)
	   (when (empty-string? val)
	     (error "~a must not be an empty string." name))))
    (when repository-name
      ;(doit repository-name "Repository name")
      (setf (cm-session-context-repository-name cm-session-context)
	(cm-cli-parse-repository-name repository-name)))
    (when pc-name
      (doit pc-name "Product configuration name")
      (setf (cm-session-context-pc-name cm-session-context) pc-name))
    (when class-name
      (doit class-name "Class name")
      (setf (cm-session-context-class-name cm-session-context) class-name))
    (when subsystem-name
      (doit subsystem-name "Subsystem name")
      (setf (cm-session-context-subsystem-name cm-session-context) subsystem-name))
    (when release-name
      (doit release-name "Release name")
      (setf (cm-session-context-release-name cm-session-context) release-name))
    (when time-spec
      (doit time-spec "Time specification")
      (let ((ts (cm-cli-parse-conman-time-spec time-spec :upper)))
	(unless ts
	  (error "~s is not a valid time specification" time-spec))
	(setf (cm-session-context-time-spec cm-session-context) ts)))
    (when label-name
      (doit label-name "Label name")
      (setf (cm-session-context-label-name cm-session-context) label-name))
    (when ws-id
      (unless (integerp ws-id)
	(when (empty-string? ws-id)
	  (error "Workspace identifier must be an integer key or a file name string."))
	(let ((integer (parse-integer ws-id :junk-allowed t)))
	  (when integer
	    (setq ws-id integer))))
      (setf (cm-session-context-ws-id cm-session-context) ws-id))
    (when rc-path
      (unless (pathnamep rc-path)
	(error "RC-Path must be a pathname."))
      (setf (cm-session-context-rc-path cm-session-context) (guarantee-absolute-pathname rc-path)))
    (when user-name
      (doit user-name "User name")
      (setf (cm-session-context-user-name cm-session-context) user-name))
    (when file-system-agent
      (setf (cm-session-context-file-system-agent cm-session-context) file-system-agent))
    (when verbosity
      (unless (and (integerp verbosity)
		   (or (= 0 verbosity)
		       (= 1 verbosity)
		       (= 2 verbosity)))
	(error "VERBOSITY must be 0, 1, or 2"))
      (setf (cm-session-context-verbosity cm-session-context) verbosity))
    cm-session-context))

(defun cm-session-context-require (cm-session-context &rest keys)
  "Low level assertion that required cm-session-context slots have values.

   KEYS may be any set from:

   (:USER-NAME :REPOSITORY-NAME, :PC-NAME, :CLASS-NAME, :SUBSYSTEM-NAME,
    :RELEASE-NAME :TIME-SPEC :LABEL-NAME :WS-ID)

   Returns the cm-session-context."
  (loop for key in keys
      do (ecase key
	   (:user-name (assert (cm-session-context-user-name cm-session-context)))
	   (:client-platform (assert (cm-session-context-client-platform cm-session-context)))
	   (:current-directory (assert (cm-session-context-current-directory cm-session-context)))
	   (:user-home-directory (assert (cm-session-context-user-home-directory cm-session-context)))
	   (:repository-name (assert (cm-session-context-repository-name cm-session-context)))
	   (:pc-name (assert (cm-session-context-pc-name cm-session-context)))
	   (:class-name (assert (cm-session-context-class-name cm-session-context)))
	   (:subsystem-name (assert (cm-session-context-subsystem-name cm-session-context)))
	   (:release-name (assert (cm-session-context-release-name cm-session-context)))
	   (:time-spec (assert (cm-session-context-time-spec cm-session-context)))
	   (:label-name (assert (cm-session-context-label-name cm-session-context)))
	   (:ws-id (assert (cm-session-context-ws-id cm-session-context)))
	   (:file-system-agent (assert (cm-session-context-file-system-agent cm-session-context)))
	   ))
  cm-session-context)

(defun cm-session-context-workspace-root (cm-session-context)
  "Return the pathname that names the root of the `current workspace'.  Right now,
   we infer this from the location of the .csf file."
  ;; **** This is bogus, there is no guarantee that the .csf "rc" file
  ;; will be at a workspace root.  It could be a stray .csf file
  ;; that's not in any workspace.

  ;; Ah, but then it wouldn't have a ws-id.
  (let ((rc-path (cm-session-context-rc-path cm-session-context))
	(ws-id   (cm-session-context-ws-id cm-session-context)))
    (when (and rc-path ws-id)
       (make-pathname
	:name nil
	:type nil
	:version nil
	:defaults
	(cm-session-context-rc-path cm-session-context)))))

(defun cm-session-context-workspace-rooted-file-system (cm-session-context &optional server-relative workspace)
  "Return a logical file system rooted at the user's current workspace as determined by the
   session context.  If server-relative is supplied, we use the workspace root as taken from the
   workspace object and pray that it is correct."
  #+allegro (declare (:fbound workspace-path))
  (cm-session-context-require cm-session-context :file-system-agent)
  (logical-file-system-create
   (if server-relative
       (workspace-path workspace)
     (guarantee-absolute-directory-pathname
      (cm-session-context-workspace-root cm-session-context)))
   (cm-session-context-file-system-agent cm-session-context)))
||#

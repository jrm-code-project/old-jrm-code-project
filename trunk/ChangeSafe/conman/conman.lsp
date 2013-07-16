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
;;;;
;;;; Module Description:  Package definition for CONMAN capabilities.
;;;; ConMan is the product configuration capability/product
;;;; implemented for Hewlett Packard.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CONMAN")

(proclaim (standard-optimizations))

(defconstant +workspace-repository-suffix+ "-workspaces")

(defun conman-workspace-repository-dbpath (master-repository-dbpath)
  "Derive the name of the workspace repository given the name of the master repository."
  (dbpath/merge (make-pathname :name (concatenate 'string (pathname-name (dbpath/pathname master-repository-dbpath))
                                                   +workspace-repository-suffix+)
                                :type +repository/workspace-file-type+
                                :defaults "")
                 master-repository-dbpath))

(defun conman-satellite-repository-name (master-repository-name class-name master-repository-dbpath)
  (let* ((satellite-repository-name
          (format nil "~(~a~)-satellite-~(~a~)" 
                  (encode-namestring master-repository-name) 
                  (encode-namestring class-name)))
         (satellite-repository-path (dbpath/merge (make-pathname :name satellite-repository-name
                                                                  :type +repository/satellite-file-type+
                                                                  :defaults "")
                                                   master-repository-dbpath))
         ;; Generally don't store the full path of satellites, resolve just the name/type/version
         ;; components relative to master.
         (satellite-relative-pathname (enough-pathname
                                       (dbpath/pathname satellite-repository-path)
                                       (dbpath/pathname master-repository-dbpath))))
    (values satellite-repository-name
            satellite-repository-path
            satellite-relative-pathname)))

#||
(defpackage :conman
  (:use :common-lisp
	:repository-file-management
	:version-management
	:server
	:web
	:core
	:utility)
  #+allegro (:shadowing-import-from :utility
				    "IGNORE-ERRORS"
				    "UNWIND-PROTECT"
				    "WITH-OPEN-FILE"
				    "WITH-OPEN-STREAM"

				    "DELETE-DIRECTORY"
				    )
  ;; DO NOT IMPORT SYMBOLS FROM THE USER PACKAGE!
  )

(in-package :conman)

;;; API's exported from this module.
(eval-when (:load-toplevel :execute)
  (export '(*conman-product-name*
	    *conman-change-name*

	    *all-changesafe-server-uri-objects*
	    this-changesafe-server-uri
	    alternative-command-line-server-uri-strings

	    +conman-test-dot-conman-file-name+
	    conman-workspace-repository-name
	    conman-generate-HP-cset-name
	    conman-java
	    conman-object-string-did
	    conman-parse-string-did
	    conman-branch-get-mutable-tip
	    configure-email-smtp-server
	    conman-email-register-address
	    conman-send-email
	    *conman-check-in-email-product-distribution-lists*
	    conman-configure-email-for-check-ins
	    conman-send-email-for-check-ins
	    set-changesafe-build-stamp

	    conman-set-server-operating-mode
	    conman-check-server-operating-mode
	    conman-repository-open-mode-from-operating-mode
	    conman-wrong-server-operating-mode
	    )))

(defconstant *conman-product-name* "ChangeSafe"
  "Product name for the product configuration management software developed for HP.")

(defconstant *conman-change-name* "delta"
  "Normally CHANGE, but HP prefers delta.  Subsystems consist of deltas.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *all-changesafe-server-uri-objects* nil
  "A list of URI objects, one for every ChangeSafe server we know about.
   This will eventually  replace *ALTERNATIVE-COMMAND-LINE-SERVER-URIS*
   since it is a more canonical form.
   *** Must be kept in sync with *ALTERNATIVE-COMMAND-LINE-SERVER-URIS*.
   *** Never modify this directly.  Use NOTE-ALTERNATE-SERVER-URI.")

(defun this-changesafe-server-uri ()
  "Returns a URI object identifying this ChangeSafe server."
  (declare (special *conman-server-log-start-port*))
  (intern-uri (format nil "http://~a:~d"
		      (utility::canonical-local-host-name)
		      *conman-server-log-start-port*)))

(defun alternative-command-line-server-uri-strings ()
  "Returns a list of strings representing the server URIs in
   *ALL-CHANGESAFE-SERVER-URI-OBJECTS*."
  ;; Various things (like the CLI client) expect these to have
  ;; trailing slashes, so make sure they do.
  (let ((results
	 (mapcar (lambda (uri)
		     (let ((u (net.uri::uri-string uri)))
		       (if (char= #\/ (char u (1- (length u))))
			   u
			 (concatenate 'string u "/"))))
		 *all-changesafe-server-uri-objects*)))
    ;; (format *error-output* "~&ALTERNATIVE-COMMAND-LINE-SERVER-URI-STRINGS ~s" results)
    results))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +conman-test-dot-conman-file-name+ ".test-conman"
  "The NON-repository relative name of the .csf file as used by the conman test system.
   This value is used to replace the value in *cmctl-dot-conman-file-name* (after
   conversion to a repository relative name.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The following two special variables are a hack put in for HP to allow them to get their
;;; 17k change-sets into the system during initial load up with the proper dates

(defvar *conman-enable-secret-cset-zapping* nil
  "Enable the -secret-userid and -secret-date-time keywords on the
   CSET_CLOSE and MASTER_CHANGE commands to allow these values to
   be supplied by the user when the command is executed.")

;;;
;;; The following two functions are co-dependent
;;;
(
 defun conman-generate-HP-cset-name (user-change-name user-name pc-name)
  "Generate a change name according to HP desires...  All arguments are strings.
   USER-CHANGE-NAME is the cset name as specified by the user.
   USER-NAME is string userid information, not currently a canonical core-user object.
   PC-NAME is a product-configuration name."
  (concatenate 'string user-name "_"
	       (numeric-date-string) "_"
	       pc-name ":"
	       user-change-name))

(defun conman-disassemble-HP-cset-name (augmented-cset-name)
  "Given a name returned by CONMAN-GENERATE-HP-CSET-NAME, return its constituent components as
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


(defun conman-synthesize-new-subsystem-name (class-name pc-name)
  "In the cases where the user does not explicitly name a new subsystem, the name
   should be synthesized from the class name and the product name.

   Given the CLASS-NAME and PC-NAME (as strings) return the name for the new
   subsystem as a string."
  ;; New subsystems created for new products have name class_product
  (concatenate 'string class-name "_" pc-name))

;;; this section has been moved to java\auxiliary\makejava.lsp

#||
(defparameter *conman-java-server-package-files*
    '("FileSystemAgent"
      "FileSystemAgentException"
      "FileSystemAgentProtocol"
      "FSAProgressIndicator"
      "OSServicesInterface"
      "OSServices"
      "MSServices"
      "SunServices"
      "HttpImpl"
      "crc"
      )
  "Java .CLASS files which are used in the CONMAN package and derived from the SERVER package.")

(defun conman-java (&key clean verbose)
  "Build, copy, or otherwise obtain the necessary .class files for java components of the CONMAN
   Package.  Note that many components are copied from the SERVER package."
  (when verbose
    (format t "~%; Copying required .class files from the SERVER directory to the CONMAN directory."))
  (dolist (filename *conman-java-server-package-files*)
    (let ((source (user::ts-source-directory-pathname (make-pathname :directory '(:relative "server")
								     :name filename :type "class"
								     :defaults nil)))
	  (dest	  (user::ts-source-directory-pathname (make-pathname :directory '(:relative "conman")
								     :name filename :type "class"
								     :defaults nil))))
      (if (probe-file source)
	  (tcopy-file source dest :overwrite t :verbose verbose)
	(format t "~%; Skipping copying ~s because it isn't there, I hope that's ok..." source))))
  ;; Compile after copying the necessary .class files.
  (let ((conman-directory (user::ts-source-directory-pathname
			   (make-pathname :directory '(:relative "conman") :defaults nil))))
    (with-new-current-directory conman-directory
      (java-tools:java-compile conman-directory :clean clean :debug nil :verbose verbose))))

||#

(defparameter +cm-use-client-from-distribution+ nil
  "*** Not yet used ***
   Whether, when invoking the Conman client, the CM function should use
   the client in the distribution hierarchy (T) or the development one (NIL).")

(defun conman-distribution-client-directory-pathname ()
  (error "not yet implemented")
  ;; TS50:ts50;distribution-conman; WHAT PLATFORM
  )

(defun invoke-conman-from-lisp (cm-command-and-args-string &key properties)
  "Invoke a conman command from lisp via the CM java client.
   CM-COMMAND-AND-ARGS-STRING is a string containing the CONMAN
   command and arguments, for example \"help file_add\".
   The appropriate Java drivel will be prepended and the result SHELL-EXECUTEd."
  ;; Start the java agent from lisp as if from the command line.
  (run-subprocess (java-tools:java-command-line
		   (if +cm-use-client-from-distribution+
		       (translate-logical-pathname
			(make-pathname :name "CM"
				       :defaults (conman-distribution-client-directory-pathname)))
		     (translate-logical-pathname "TS50:JAVA;cm;CM"))
		   cm-command-and-args-string
		   :classpath-additions (list (translate-logical-pathname #p"TS50:JAVA;fsa;"))
		   :properties properties)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DID <--> STRING DID

(defun conman-object-string-did (distributed-object)
  "Safe way to get the DID string of DISTRIBUTED-OBJECT.
   This just calls DISTRIBUTED-OBJECT-REFERENCE-AS-STRING-DID,
   but does so in a context where *PACKAGE* is bound to something
   consistent."
  ;; Tiptoe through the package kludgery that surrounds distributed
  ;; object string representations.
  (let ((*package* (find-package "CONMAN")))
    (->string-did distributed-object)))

(defun conman-parse-string-did (string-did)
  "Safe way to get a DID given its string representation."
  (check-type string-did string)
  (parse-did string-did))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *conman-omit-master-change-up-to-date-check-for-regressions* nil
  "DO NOT USE THIS VARIABLE IN NEW CODE.  IT IS HERE ONLY TO ALLOW CONTINUED WORKING OF
   EXISTING REGRESSION TESTS.  Bound to true if we're skipping the workspace-up-to-date check on
   a master_change invocation.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ChangeSafe can access ObjectStore servers running on different
;;; machines from itself.

;;; ***FINISH*** WE STILL NEED TO INTEGRATE THIS WITH THE MECHANISM
;;; WHICH THE HTTP SERVER USES TO FIND MASTER REPOSITORIES.

(defparameter +conman-database-server-hosts+ nil
  "This parameter contains a list of entries describing each ObjectStore
   database server which the CHangeSafe server can access.

   Each entry is a list of the form (HOST-NAME PLATFORM-TYPE . MASTER-DATABASES)
   where
   HOST-NAME is a sting naming a host which is running an ObjectStore server.
   PLATFORM-TYPE is the platform type for that hoes, e.g. :DOS, :UNIX, as might
   be returned by SERVER-PLATFORM and friends.  It determines how pathname
   namestrings for this host will be parsed.
   MASTER-DATABASES is a list of the absolute pathname namestrings of master
   databases on that ObjectStore server is is provided so that the HTTP server
   will know what databases are available since it has no way of knowing
   otherwise.")

(defun conman-parse-database-name (database-namestring)
  "Parse DATABASE-NAMESTRING returning a pathname object.
   If DATABASE-NAMESTRING has a known database server host prefix
   matching that of an entry in +CONMAN-DATABASE-SERVER-HOSTS+ then
   it is parsed according to that host's syntax and the PATHNAME-HOST
   of the resulting pathname is filled in accordingly.
   A second return value will be a list of the host name and its
   platform type in this case.
   If DATABASE-NAMESTRING does not have a known database server host
   as a host prefix, then it is assumed to name a database file on the
   ChageSafe server hots (that's us) and the pathname is parsed for
   for the local host.  In this case, the second value is NIL."
  ;; hostname ':' pathname-in-host-syntax
  (declare (values pathname host-info))
  (block done
    (loop
	for (host-name platform-type . rest) in +conman-database-server-hosts+
	for host-name-length = (length host-name)
	do
	  (progn rest)			; ignore
	  (when (and (< host-name-length (length database-namestring))
		     (string-equal host-name database-namestring
				   :end2 host-name-length)
		     (char= #\: (char database-namestring host-name-length)))
	    (return-from done
	      (values
	       (make-pathname :host host-name
			      :defaults (parse-client-namestring
					 database-namestring platform-type
					 :start (1+ host-name-length)))
	       (list host-name platform-type)))))
    (values
     (parse-client-namestring database-namestring (server-platform))
     nil)))

(defun remote-master-repository-database-paths ()
  "Returns a list of master database pathname strings for the master databases
   on remote OnjectStore servers."
  ;; We need this, and the MASTER-DATABASES portion of the entries in
  ;; +DATABASE-SERVER-HOSTS+ because we otherwise have no way of
  ;; knowing what master repositories are available on remote
  ;; ObjectStore servers.  We would need some form of file system
  ;; access to use the *KNOWN-MASTER-REPOSITORY-DIRECTORIES* mechanisn
  ;; that we use for the local host.
  (loop
      with remote = nil
      for (host platform-type . master-db-paths) in +conman-database-server-hosts+
      do (progn platform-type)	; Ignore
	 (dolist (db master-db-paths)
	   (push (concatenate 'string host ":" db) remote))
	 finally (return remote)))


(defparameter *conman-smtp-server* nil
  "The SMTP server that ChangeSafe uses to send email notifications.")

(defun configure-email-smtp-server (server)
  "Configure SMTP server for email notifications."
  (check-type server string)
  (setf *conman-smtp-server* server))

;; Define hashtable to hold mapping of userids to email adresses.
(defvar *conman-email-hashtable* (make-hash-table)
  "A hash table which maps CHangeSafe users to their email addresses.
   Don't access this directly.  Instead use CONMAN-EMAIL-ADDRESS-LOOKUP
   and CONMAN-EMAIL-REGISTER-ADDRESS.")

(defun conman-email-register-address (userid email-address)
  "Assign supplied email address to userid. Store in hashtable
   for subsequent query via conman-email-address-lookup."
  (check-type userid string)
  (check-type email-address string)
  (setf (gethash userid *conman-email-hashtable*) email-address))

(defun conman-email-address-lookup (userid)
  "Returns email address corresponding to the supplied userid.
   Defaults to userid at *CONMAN-SMTP-SERVER*."
  (or (gethash userid *conman-email-hashtable*)
      (concatenate 'string userid "@" *conman-smtp-server*)))

(defun conman-send-email (from-user recipients subject string-stream)
  "Send email to the specified users using the configured SMTP server."
  (declare (special *cm-returns-warning-no-smtp-server-configured*)
	   #+allegro (:fbound conman-signal-warning))
  (if *conman-smtp-server*
      (let ((sender-email-address (conman-email-address-lookup from-user))
	    (recipient-addresses (if (listp recipients)
				     (mapcar #'conman-email-address-lookup
					     recipients)
				   (list (conman-email-address-lookup recipients)))))
	(smtp-send-mail string-stream
			recipient-addresses
			:subject subject
			:from sender-email-address
			:smtp-server-hostname *conman-smtp-server*
			:smtp-reply-address sender-email-address)
	t)
    (progn
      (conman-signal-warning
       *cm-returns-warning-no-smtp-server-configured*
       "ChangeSafe is not configured to send email.")
      nil)))

(defparameter *conman-check-in-email-product-distribution-lists* nil
  "list of lists (an assoc list) of strs to send master_change info to.  Each sublist starts
   with a product name and is followed by one or more user-ids (uses *conman-email-hashtable*
   to get email addrs).  A product name of :all matches any product and is used as
   a sort of universal clause.  Names in the :all section are always on the resulting
   email list.  A product name of :else is used when nothing else matches
   (besides the :all universal product).")

(defun conman-configure-email-for-check-ins (list-of-lists)
  "takes a possible value for *conman-check-in-email-product-distribution-lists*
   and confirms that it is structurally ok.  If so, it assigns it to
   *conman-check-in-email-product-distribution-lists*.  The value is expected to be
   either NIL (turn-off the emailing) or a list of lists where the sub-lists consist
   of a set of strings, the first string is a product name, all the rest are user-ids.
   The value could also be a string which evaluates to a list of lists."
  (declare (special *cm-returns-error-improper-email-list-for-check-ins*)
	   #+allegro (:fbound conman-signal-error))
  ;; convert from string to list of lists, if needed
  (let ((l-of-l list-of-lists)) ;; so we don't modify our input
    (when (stringp l-of-l)

      ;; implement a finite-state parser to pull apart the string into a
      ;; list of lists of strings.  The states are nil (at beginning or after
      ;; end), :outer-list (inside outer parens), :inner-list (inside inner
      ;; parens), :inner-str-dq (inside a double-quoted str), or :inner-token
      ;; (inside a token).  White space, a closing double quote or a right
      ;; paren will stop a token.

      (let ((more t)
	    state ;; :outer-list, :inner-list, :inner-str-dq, :inner-token
	    token
	    in-list
	    char
	    (char-idx 0)
	    result)
	(with-input-from-string (str-stream l-of-l)
	  (do () ;; loop over all the possible items in the list-of-lists string
	      ((not more))
	    (setq char (read-char str-stream nil nil))
	    (setq more char)
	    (when more
	      (incf char-idx)
	      (cond ((eq char #\() ;; start a list (inner or outer)
		     (if (eq state :inner-str-dq)
			 (setq token (concatenate 'string token "("))
		       (if (eq state :outer-list) ;; start inner list
			   (setq state :inner-list
				 in-list nil)
			 (if (null state) ;; start outer list
			     (progn
			       (when result ;; already had stuff
				 (conman-signal-error
				  *cm-returns-error-improper-email-list-for-check-ins*
				  "Too many outer ('(') lists in the email-for-check-ins list at character ~d"
				  char-idx))
			       (setq state :outer-list
				     in-list nil))
			   (conman-signal-error
			    *cm-returns-error-improper-email-list-for-check-ins*
			    "Too many inner ('(') lists in the email-for-check-ins list at character ~d"
			    char-idx)))))
		    ((eq char #\)) ;; end a list (inner or outer)
		     (if (eq state :inner-str-dq)
			 (setq token (concatenate 'string token ")"))
		       (if (eq state :outer-list) ;; finish whole thing
			   (setq state nil)
			 (if (or (eq state :inner-list) ;; finish an inner list
				 (eq state :inner-token))
			     (progn
			       (when token
				 (push token in-list))
			       (setq in-list (reverse in-list))
			       (when (string= ":all" (car in-list))
				 (setq in-list (cons :all (cdr in-list))))
			       (when (string= ":else" (car in-list))
				 (setq in-list (cons :else (cdr in-list))))
			       (push in-list result)
			       (setq state :outer-list
				     token nil
				     in-list nil))
			   (conman-signal-error
			    *cm-returns-error-improper-email-list-for-check-ins*
			    "Too many closed (')') lists in the email-for-check-ins list at character ~d"
			    char-idx)))))
		    ((eq char #\") ;; start or end a str, or add a \" to a token
		     (if (eq state :inner-str-dq) ;; close a token
			 (progn
			   (when token
			     (push token in-list))
			   (setq state :inner-list
				 token nil))
		       (if (eq state :inner-token) ;; close a token
			   (progn
			     (when token
			       (push token in-list))
			     (setq state :inner-str-dq
				   token nil))
			 (if (eq state :inner-list) ;; start a token
			     (setq state :inner-str-dq)
			   (conman-signal-error
			    *cm-returns-error-improper-email-list-for-check-ins*
			    "Unexpected char ('\"') in the email-for-check-ins list at character ~d"
			    char-idx)))))
		    (t ;; handle any other char
		     (if (eq state :inner-str-dq)
			 (setq token (concatenate 'string token
						  (make-string 1 :initial-element char)))
		       (if (or (eq char #\Space) ;; close token and skip whitespace
			       (eq char #\newline)
			       (eq char #\tab)
			       (eq char #\linefeed)
			       (eq char #\return))
			   (progn
			     (when token
			       (push token in-list))
			     (setq state (if (eq state :outer-list) :outer-list :inner-list)
				   token nil))
			 (if (eq state :inner-list)
			     (setq token (make-string 1 :initial-element char)
				   state :inner-token)
			   (if (eq state :inner-token)
				 (setq token (concatenate 'string token
							  (make-string 1 :initial-element char)))
			     (conman-signal-error
			      *cm-returns-error-improper-email-list-for-check-ins*
			      "Unexpected char ('~a') in the email-for-check-ins list at character ~d"
			      char char-idx))))))
		    ))))
	(when state ;; non-nil means we are in the middle of something
	  (conman-signal-error
	   *cm-returns-error-improper-email-list-for-check-ins*
	   "Improperly formed email-for-check-ins list (missing close parentheses?): ~a"
	   l-of-l))
	(debug-message 4 "conman-configure-email-for-check-ins converted: ~a" result)
	(setq l-of-l result)))

    (debug-message 0 "conman-configure-email-for-check-ins converted: ~a" l-of-l)

    (when l-of-l ;; avoid all this processing if nil
      (unless (listp l-of-l)
	(conman-signal-error
	 *cm-returns-error-improper-email-list-for-check-ins*
	 "The value (~a) for the email-for-check-ins list must be a list of lists, you gave a ~a"
	 l-of-l (type-of l-of-l)))
      (when (< (length l-of-l) 1)
	(conman-signal-error
	 *cm-returns-error-improper-email-list-for-check-ins*
	 "The length (~a) of the email-for-check-ins list must be at least 1"
	 (length l-of-l)))

      (dolist (val l-of-l)
	(unless (listp val)
	  (conman-signal-error
	   *cm-returns-error-improper-email-list-for-check-ins*
	   "The sub-value (~a) for the email-for-check-ins list must be a list of consisting of ~
             a product-name followed by user-ids, you gave a ~a"
	   val (type-of val)))
	(when (< (length val) 2)
	  (conman-signal-error
	   *cm-returns-error-improper-email-list-for-check-ins*
	   "The length (~a) of the email-for-check-ins list must be at least 2 (consisting of a ~
             product-name and one or more user-ids)"
	   (length val)))

	(let ((list-idx 0))
	  (dolist (subval val)
	    (incf list-idx)
	    (unless (or (stringp subval)
			(and (= 1 list-idx)
			     (symbolp subval)
			     (or (equal :all subval)
				 (equal :else subval))))
	      (conman-signal-error
	       *cm-returns-error-improper-email-list-for-check-ins*
	       "The sub-value parts (~a) of the email-for-check-ins list must be strings, you gave a ~a"
	       subval (type-of subval)))))))

    ;; everything looks ok, so save it
    (setq *conman-check-in-email-product-distribution-lists*
	  (sort l-of-l #'string< :key #'car))
    (debug-message 3 "conman-configure-email-for-check-ins: ~a"
		   *conman-check-in-email-product-distribution-lists*)
    *conman-check-in-email-product-distribution-lists*
    ))

(defun conman-send-email-for-check-ins ()
  "returns T if we need to send an email for a check-in (master_change or cset_close)"
  (declare (special conman::*cm-cli-doing-performance-run*))
  (or (and *within-regression-tests*
	   (not conman::*cm-cli-doing-performance-run*))
      (and *conman-check-in-email-product-distribution-lists*
	   *conman-smtp-server*)))

(defun conman-check-in-email-recipients (product-name-list)
  "returns a sorted list of email addresses suitable for conman-send-mail for who should
   get email about a check-in (cset_close or master_change)"

  (let (result
	found-one)

    ;; check his list for matches
    (dolist (pn product-name-list)
      (dolist (addr (cdr (assoc pn *conman-check-in-email-product-distribution-lists*
			       :test #'string=)))
	(setq found-one t)
	(push addr result)))

    ;; now add the universal set (if any)
    (dolist (addr (cdr (assoc :all *conman-check-in-email-product-distribution-lists*
			      :test #'equal)))
      (push addr result))

    ;; now add the *else* set (if needed)
    (unless found-one
      (dolist (addr (cdr (assoc :else *conman-check-in-email-product-distribution-lists*
				:test #'equal)))
	(push addr result)))

    (setq result (remove-duplicates result :test #'string=))
    (debug-message 4 "conman-check-in-email-recipients: ~a" result)
    (sort result #'string<))) ;; for now

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *changesafe-build-stamp* nil
  "When we build a delivered image, this variable is set (by calling
   SET-CHANGESAFE-BUILD-STAMP) to note the user, machine and time of
   the build.")

(defun set-changesafe-build-stamp ()
  "Called automatically when building a delivered image (invoked from
   a form in ts50/dload.lsp) to note when the image was build and by
   whoom."
  (setq *changesafe-build-stamp*
	(list :user (sys:user-name)
	      :host (local-hostname)
	      :time (get-universal-time))))

(defun changesafe-delivery-build-stamp-hook-function (&key delivery-load
						      &allow-other-keys)
  (when delivery-load
    (set-changesafe-build-stamp)))

(eval-when (:load-toplevel :execute)
  (user::add-hook user::*ts-load-finish-hooks*
		  changesafe-delivery-build-stamp-hook-function))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric conman-user-name (user-id-or-object)
  (:documentation
   "Return the user name string for the user identified by USER-ID-OR-OBJECT."))

(defmethod conman-user-name ((user-name string))
  ;; If they already have the name, let them keep it.
  user-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Distinguishing between read-only and read/write servers

(defparameter *conman-server-operating-mode* nil
  "The mode that the ChangeSafe server is operating in.  Should be one
   of :READ-ONLY or :READ/WRITE.
   *** For purposes of modularity and abstraction, never read this directly,
   use CONMAN-CHECK-SERVER-OPERATING-MODE.  This houls never be bound.")

(defun conman-set-server-operating-mode (mode)
  "Sets the operating mode of the CHangeSAfe server.
   *** This should only be called by the configuration file or by
   the ChangeSafe startup code."
  (assert (member mode '(:read-only :read/write)))
  (setq *conman-server-operating-mode* mode))

#+(and :allegro-version>= (:version>= 6 0))
(declaim (:fbound conman-error-required-mode
		  conman-error-current-mode))

(define-condition conman-wrong-server-operating-mode (error)
  ((required-mode :initarg :required-mode
		  :reader conman-error-required-mode)
   (current-mode  :initarg :current-mode
		  :reader conman-error-current-mode))
  (:documentation
   "This condition is signalled by CONMAN-CHECK-SERVER-OPERATING-MODE
    when the ChangeSafe server operating mode is does not match that
    required by its caller.")
  (:report conman-wrong-server-operating-mode-report))

(defun conman-wrong-server-operating-mode-report (condition stream)
  (check-type condition conman-wrong-server-operating-mode)
  (format stream "The ChangeSafe server's operating mode is ~a but the expected mode was ~a."
	  (conman-error-current-mode condition)
	  (conman-error-required-mode condition)))

(defun conman-check-server-operating-mode (&optional
					   (required-operating-mode nil romp))
  "Return the operating mode of the currently running server.
   If REQUIRED-OPERATING-MODE is specified, signal an error
   of type CONMAN-WRONG-SERVER-OPERATING-MODE is the operating
   mode does not match it."
  ;; HTTP-CONNECTION is an argument because some day we might
  ;; encapsulate all of the information about a guven server into a
  ;; server object which will be used to start/run the server and
  ;; which might be noted in the HTTP-CONNECTION so that response
  ;; functions can access it.
  (when romp
    (unless (eq *conman-server-operating-mode*
		required-operating-mode)
      (error 'conman-wrong-server-operating-mode
	     :required-mode required-operating-mode
	     :current-mode *conman-server-operating-mode*)))
  *conman-server-operating-mode*)

(defun conman-repository-open-mode-from-operating-mode ()
  "Return the mode in which repositories should be opened based on
   what the server's operating mode is.  This allows for operations
   which don't care (because they are only doing MVCC reads) to
   specify a mode which is consistent with all other REPOSITORY-OPENs
   which might be performed when operating under the given mode, so that
   the repository open cache's sensitive feelings won't be hurt."
  (ecase *conman-server-operating-mode*
    (:read-only :mvcc)
    (:read/write :update)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

||#

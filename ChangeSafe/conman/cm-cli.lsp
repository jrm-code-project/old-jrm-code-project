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
;;;; File Name:     cm-cli.lsp
;;;; Author:        Dave Tenny
;;;; Creation Date: September 1999
;;;;
;;;; Module Description: ConMan CLI (Command Line Interface) module.
;;;;
;;;; This module contains handlers for all server entries to ConMan
;;;; capabilities which occur via the command line.  It assumes clients
;;;; connect using the CM.Java module and its associated "CLI/RESPONSE"
;;;; protocol, or equivalent module supporting the same protocol.
;;;;
;;;; Explicit user arguments passed on the command line are matched
;;;; and bound to argument descriptors defined for each CLI command.
;;;; Implicit environment context, or context which is saved
;;;; client-side such that it isn't necessary on the command line, is
;;;; bound in cm-session-context objects which are also passed to each
;;;; CLI command.
;;;;
;;;; The method of encoding the respective types of information
;;;; are determined by a combination of ServerRequest.java, CLI-REQUEST.LSP,
;;;; CM.java, and a bit of cm-session-context.lsp.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CONMAN")

(proclaim (standard-optimizations))

;(defconstant *cm-cli-content-type* "cli/response"
;  "HTTP content type which is recognized by the CM.java client module,
;   and specifies the protocol used by many of the command processors in this module.")

(defvar *cm-cli-testing-shutdown* nil
  "Bound to true if we're testing the cli in the regressions, and the listener should
   quit after every command.")

(defparameter *cm-cli-command/uri-prefix* "/csf/command/"
  "This prefix at the beginning of a URL indicates that the HTTP request is
   for the server to perform a ChangeSafe command.")

#||

(defvar *cm-cli-debug-server-info* nil
  "when T, causes various data including the port number and time of day to be passed
   back to the client (via cm-cli-response).")

(defvar *cm-cli-command-times-show* nil
  "when T, causes per command and cummulative times (in seconds/milliseconds) to be
   printed at the client (via cm-cli-response).")

(defvar *cm-cli-command-times-so-far* 0.0
  "This the accumulated time of the commands run so far (in seconds (out to milliseconds)).
   This will be reset every time *cm-cli-command-times-show* is set.")

(defun note-alternate-server-uri (uri)
  "This is the fuinction that is used to tell this ChangeSafe server about all
   of the available ChangeSafe servers (including itself).
   It is typically called from the ChangeSafe server configuration file."
  (let* ((parsed (web:url-parse uri))
	 (canonical-host-name
	  (let ((uri-host (net.uri:uri-host parsed)))
	    (if *within-regression-tests*
		(handler-case
		    (canonicalize-host-name uri-host)
		  (error () uri-host))
	      (canonicalize-host-name uri-host))))
	 (canonicalized (intern-uri (web::url-create :host canonical-host-name
						     :path nil
						     :query-args nil
						     :defaults parsed))))
    (pushnew canonicalized *all-changesafe-server-uri-objects*)))

(defun filter-alternative-servers-by-host (host)
  (let* (
	 (ans nil)
	 (short (concatenate 'string host
			 (if (find #\. host) ":" ".")))
	 (long (concatenate 'string "http://" short))
	 (ends (length short))
	 (endl (length long))
	 )
    (dolist (it (alternative-command-line-server-uri-strings))
      (let ((lit (length it)))
	(when (or  (string-equal it long :end1 (min endl lit))
		   (string-equal it short :end1 (min ends lit)))
	  (push it ans)
	  )))
     ans))


(defun cm-cli-response (type value stream &key (escape-it t))
  "Generate a cli/response packet for CM.java to interprets.

   This routine is useful for 'conversational' things.  To invoke the FSA using
   the cli/response STARTFSA protocol, use WITH-CM-CLI-FSA.

   Valid TYPE values for this function are :DISPLAY, :RETURN.
   VALUE must be a datatype appropriate to TYPE.  Typically a number, sometimes a string.

   If ESCAPE-IT is true, print strings with escapes, otherwise assume they've been escaped."
  (debug-message 4 "cm-cli-response: ~a ~:[~s~;~a~] ~:[(escaped)~;~]" type escape-it value escape-it)
  (flet ((output-redirect (alternates redirect-type &rest strings)
	   ;; We permute the alternate servers so that the clients will
	   ;; be redirected in a pseudo-random order.  This will distribute
	   ;; the load among the several servers.
	   (let ((permuted-alternates
		  (if *within-regression-tests*
		      alternates
		    (permute alternates))))
	     ;; magic format string.  Each server on separate line, indented.
	     (debug-message 4 "alternate-servers: ~{~#[<none>~;~35t~a~;~35t~a~&~35t~a~:;~35t~a~@{~&~35T~a~^~}~]~}" permuted-alternates)
	     (format stream "REDIRECT ~s ~{~a~}~{ ~a~}" redirect-type strings permuted-alternates))))

    (ecase type

      (:CHANGE-DIRECTORY
       (conman-server-log-string nil "CLI-RESPONSE" (format nil "Change-Directory ~a" value))
       (check-type value string)
       (fast-write-string "CD " stream)
       (fast-write-string value stream))

      (:DISPLAY
       ;; don't bother to log :DISPLAY cm-cli-responses
       (check-type value string)
       (cond (escape-it
	      (fast-write-string "DISPLAY " stream)
	      (prin1 value stream))
	     (t
	      (fast-write-string "DISPLAY \"" stream)
	      (fast-write-string value stream)
	      (fast-write-char #\" stream))))

      (:BUSY-REDIRECT
       ;; Ignoring the value for now.
       (conman-server-log-string nil "CLI-RESPONSE" "Busy-Redirect")
       (output-redirect
	(alternative-command-line-server-uri-strings)
	:BUSY))

      (:PREFER-READ-ONLY-REDIRECT
       ;; Ignoring the value for now.
       (conman-server-log-string nil "CLI-RESPONSE" "Prefer-Read-Only-Redirect")
       (output-redirect
	(alternative-command-line-server-uri-strings)
	:READ-ONLY))

      (:NO-SERVICE-REDIRECT
       (conman-server-log-string nil "CLI-RESPONSE" "No-Service-Redirect")
       (output-redirect
	(alternative-command-line-server-uri-strings)
	:NO-SERVICE))

      (:SPECIFIC-HOST-REDIRECT
       (conman-server-log-string nil "CLI-RESPONSE" "Specific-Host-Redirect")
       ;; value is the list of alternative server uris with the right host
       (output-redirect
	value
	:NO-SERVICE))

      (:CONTINUE-REDIRECT
       (conman-server-log-string nil "CLI-RESPONSE" "Continue-Redirect")
       (check-type value string)
       (output-redirect
	(alternative-command-line-server-uri-strings)
	:CONTINUE (subseq *cm-cli-command/uri-prefix* 1) value))

      (:RETURN
	(conman-server-log-string nil "CLI-RESPONSE" (format nil "Return ~a" value))
	(check-type value integer)
	(fast-write-string "RETURN " stream)
	(princ value stream))

      (:TRACE
       (conman-server-log-string nil "CLI-RESPONSE" (format nil "Trace ~a" value))
       (check-type value integer)
       (fast-write-string "TRACE " stream)
       (princ value stream)))

    (terpri stream)))

(defun with-cm-cli-fsa-function (cm-session-context server-relative body-function)
  "See WITH-CM-CLI-FSA"
  (let ((completed-p nil)
	(condition nil)
	(stream (http-connection-stream (cm-session-context-http-connection cm-session-context)))
	file-system)
    (tail-labels ((shutdown-fsa-and-resignal-condition (condition)
		    (redirect-noise "busy redirect, shutdown fsa")
		    (file-system-shutdown file-system :success t)
		    ;; so we don't close it twice.
		    (setq file-system nil)
		    ;; now we just exit, and let the CM-CLI controller
		    ;; take over.

		    ;; We do an explicit signal here rather than just
		    ;; declining, because that allows us to bypass other
		    ;; handlers in this HANDLER-BIND.  Were this SIGNAL to
		    ;; find no handlers, we would then attempt to handle the
		    ;; previous signalling of the condition in this dynamic
		    ;; context.
		    (signal condition))

		  (redirect-noise (message)
		    (debug-message 3 "Process ~s ~s.~&~s" *current-process* message core::*txn-mutex-queue*)
		    #||
		    (when *conman-server-log*
			(conman-server-log-string
			 (cm-session-context-user-name cm-session-context)
			 'busy-redirect
			 (format nil "Process ~s ~s.~&~s" *current-process* message core::*txn-mutex-queue*))) ||#)
		  )

      (unwind-suggest
	  (handler-bind ((awaiting-txn-mutex
			  (lambda (condition)
			    (redirect-noise "notifying user of wait")
			    (file-system-note-progress
			     file-system
			     "The server is busy servicing another request."
			     nil)
			    (file-system-note-progress
			     file-system
			     (format nil
				     "Your request will be enqueued, there ~
                                          ~[are~;is~:;are~] ~:*~R ~
                                          request~:P before yours."
				     (awaiting-txn-mutex-condition/number condition))
			     nil)
			    (continue)))

			 (granted-txn-mutex
			  (lambda (condition)
			    (declare (ignore condition))
			    (redirect-noise "notifying user that wait has ended")
			    (file-system-note-progress
			     file-system
			     (format nil "Now serving your request.")
			     nil)
			    (continue)))

			 (busy-redirect #'shutdown-fsa-and-resignal-condition)

			 (conman-wrong-server-operating-mode
			  #'shutdown-fsa-and-resignal-condition)

			 ;; These are not errors that we want to report to the user.
			 (excl:interrupt-signal #'signal)

			 ;; Internal errors should halt things.
			 (type-error #'signal)

			 ;; This handler doesn't do anything other than to remember the
			 ;; condition and decline to handle it.
			 (condition (lambda (condition1)
					(setq condition
					      (with-output-to-string (stream)
						(write condition1 :escape nil :stream stream)))
					(debug-note-condition "caught in with-cm-cli-fsa-function" condition1)))
			 )
	    (setq file-system (if server-relative
				  (lisp-file-system-create)
				(progn
				  (fast-write-string "STARTFSA" stream)
				  (terpri stream)
				  (finish-output stream)
				  (let ((sfs (socket-file-system-create stream)))
				    (unless *within-regression-tests*
				      (when-debugging 4
					(server::socket-file-system-debug sfs t)))
				    sfs))))
	    (multiple-value-prog1
		(unwind-protect
		    (progn (cm-session-context-add cm-session-context :file-system-agent file-system)
			   (funcall body-function))
		  "unsetting file-system-agent"
		  (cm-session-context-add cm-session-context :file-system-agent nil))
	      (setq completed-p t)))
	(unwind-suggest
	    (when file-system
	      (file-system-shutdown file-system :success completed-p))
	  (when condition
	    (if nil				; *within-regression-tests*
		(cm-cli-response
		 :display
		 (concatenate 'string "A condition was noted during file system operation, "
			      "but was not printed in order that regression test output be consistent across runs.")
		 stream)
	      (cm-cli-response :display condition stream))))))))

(defmacro with-cm-cli-fsa ((cm-session-context &key server-relative) &body body)
  "Execute BODY as an implicit PROGN with CM-SESSION-CONTEXT-FILE-SYSTEM set to a FILE-SYSTEM object.
   When SERVER-RELATIVE is true, the file system is bound to a server-relative file system.
   When it is false (the default), it is bound to a file-system subtype which communicates
   with a FileSystemAgent running in the client context which generated the connection to the server.

   ****** WARNING ****** ****** WARNING ****** ****** WARNING ****** ****** WARNING ******

   It is imperative that BODY does not read from or write to the http connection stream
   within the lexical scope of this macro.  It would be regarded poorly by the client
   file system agent.

   ****** WARNING ****** ****** WARNING ****** ****** WARNING ****** ****** WARNING ******

   If a some non-local exit should occur and a condition was intercepted, we try to print
   the condition to the CLI connection. Otherwise we do nothing except what BODY tells us to do."
  (let ((with-cm-cli-fsa-body-name (gensym (symbol-name :with-cm-cli-fsa-body-))))
    `(flet ((,with-cm-cli-fsa-body-name () ,@body))
       (declare (dynamic-extent #',with-cm-cli-fsa-body-name))
       (let ((cm-session-context ,cm-session-context)
	     (server-relative ,server-relative))
	 (with-cm-cli-fsa-function cm-session-context server-relative
				   #',with-cm-cli-fsa-body-name)))))

(defun cm-cli-http-response (error-code http-connection format-control-string &rest format-args)
  "Generate a CLI/RESPONSE protocol message indicating a problem exists, and that the client command
   failed.

   ERROR-CODE should be an integer selected from cm-returns.

   FORMAT-CONTROL-STRING must be specified and should describe the nature of the error.

   FORMAT-ARGS are any optional args required by the format control string."
  ;; *TBD* when we have clients which specify the Accept: header on the HTTP request,
  ;; we can decide to reply with text/HTML here, or CLI/RESPONSE.  For now, it's the latter.
  (let ((*print-pretty* nil)
	(*print-escape* t)
	(stream (http-connection-stream http-connection)))
    (http-response http-connection *http-status-okay* :content-type *cm-cli-content-type*)
    ;; Ensure that we escape quotes in resulting string
    ;; ***** Shouldn't we be calling CM-CLI-RESPONSE?
    (format stream "DISPLAY ~s~%"
	    (apply #'format nil format-control-string format-args))
    (format stream "RETURN ~d~%" error-code))
  nil)

(defparameter *cm-cli-command-list* NIL
  "This list is used to match URI <abs_path> specifications to functions which will process the request.

   This list is automatically maintained through the use of DEFINE-CM-CLI-COMMAND.

   The list contains sublists.  Each sublist has as its elements:
   1) A string describing the token in the URI <abs_path> after the leading '/', e.g.
      \"cm_class_create\".
   2) A symbol naming the function which will process that command.
   3) Parameter descriptions for the request.  These argument descriptors are as per
      CLI-REQUEST-PARSE-URI-ARGS.
   ")

(defun cm-cli-find-command-description (how command-key)
  "Find a CM command description in *CM-CLI-COMMAND-LIST*.
If HOW is :NAME then COMMAND-KEY is the name of a command (a string).
If HOW is :FUNCTION then COMMAND-KEY is a symbol naming a command function."
  (ecase how
    (:name	(find command-key *cm-cli-command-list* :key #'car :test #'string=))
    (:function	(find command-key *cm-cli-command-list* :key #'second))))

(defun arg-descriptor-name (argument-descriptor)
  (first argument-descriptor))

(defun arg-descriptor-syntax (argument-descriptor)
  (getf (cdr argument-descriptor) :syntax))

(defun arg-descriptor-type (argument-descriptor)
  (getf (cdr argument-descriptor) :type))

(defun arg-descriptor-required (argument-descriptor)
  (getf (cdr argument-descriptor) :required))

(defun arg-descriptor-default (argument-descriptor &optional not-present-value)
  (getf (cdr argument-descriptor) :default not-present-value))

(defparameter *cm-cli-debug* nil "True if you want a trace of cm-cli command dispatches")

;; Set to a file-stream by admin-database-error-log to enable logging
;; Set to nil to disable logging
(defvar *cm-database-error-log* nil "Stream for logging server commands")
(defvar *cm-database-error-log-name* nil
  "Name of the file for the error log")

(defun cm-cli-close-error-log-stream ()
  "Close the error logging stream.

   If we are unable to close the stream, return the suppressed condition which was signalled in closing
   the stream, or T if the condition is unknown.  Otherwise return NIL (indicating success)."
  ;; We don't signal an error here, because the caller may be sensitive to the way errors are handled
  ;; and reported.
  (when *cm-database-error-log*
    (multiple-value-bind (result condition)
	(ignore-errors (close *cm-database-error-log*))
      (setq *cm-database-error-log* nil
	    *cm-database-error-log-name* nil)
      (and (null result) condition))))

(defun cm-cli-database-error-log-string (user-name what message)
  "Log the MESSAGE in the *cm-database-error-log* along with the time of day.
   The HTTP-CONNECTION arg lets us get the user-id.
   The WHAT is a label (string) like ERROR or COMMAND to identify the kind of message."
  (when *cm-database-error-log*
    (format *cm-database-error-log* "~%~A, WHO: ~a~%~s: ~A~%"
	    (time-string (get-universal-time) :no-days t)
	    user-name what message)
    (force-output *cm-database-error-log*))

  ;; Now try logging this as a server message
  (conman-server-log-string user-name what message)
  )

;; Set to a file-stream by admin-database-performance-log if performance logging is enabled
(defvar *cm-cli-server-performance-log-stream* nil
  "Stream for logging performance monitoring data")
(defvar *cm-cli-server-performance-log-name* nil
  "Name of the file to which performance monitoring data is being logged")

(defvar *cm-cli-doing-performance-run* nil
  "Allows us to generate certain dynamic output during performance runs")

(defun cm-cli-close-performance-log-stream ()
  "Close the performance logging stream.

   If we are unable to close the stream, return the suppressed condition which was signalled in closing
   the stream, or T if the condition is unknown.  Otherwise return NIL (indicating success)."
  ;; We don't signal an error here, because the caller may be sensitive to the way errors are handled
  ;; and reported.
  (when *cm-cli-server-performance-log-stream*
    (multiple-value-bind (result condition)
	(ignore-errors (close *cm-cli-server-performance-log-stream*))
      (setq *cm-cli-server-performance-log-stream* nil
	    *cm-cli-server-performance-log-name* nil)
      (and (null result) condition))))

(defvar *cm-file-add-time* nil "Cumulative time for cm_file_add commands")

||#

(defun cm-cli-http-server-get (req ent)
  "HTTP GET-DISPATCHER CLI-based GET requests.

   This routine (and those that it calls) assume that requests were packaged by the
   ServerRequest.java module, and that command line arguments have a known form based on that assumption.
   If further assumes clients supporting the CLI/RESPONSE protocol documented in CM.JAVA."

  ;; **** NOTE ****
  ;; If you change this routine, you must restart CHANGESAFE-SERVER-START to observe its effects.

  (let* ((uri (net.aserve:request-uri req))
         (uri-path (uri/raw-path uri))
	 (command-start-time (get-internal-real-time))
	 (return-val (if *cm-cli-testing-shutdown* :quit nil))
	 (raw-command (if (> (length uri-path) (length *cm-cli-command/uri-prefix*))
			  (subseq uri-path (length *cm-cli-command/uri-prefix*))
			(subseq uri-path (min 1 (1- (length uri-path))))))
	 command-name)
    (debug-message 0 "~15@a:  starting" raw-command)
    (unwind-suggest
	;; Attempt to do all the command boilerplate.  This includes resolving the command,
	;; validating args, etc.  If we succeed, call the appropriate function with the http connection
	;; and bindings for the arguments.
	(with-simple-restart (abort-current-web-transaction
			      "Abort this web transaction")

	  (let ((cm-session-context nil))
	    (when (> (length uri-path) (length *cm-cli-command/uri-prefix*))
	      (setq cm-session-context (cm-cli-create-cm-session-context http-connection)))

	    ;; log the command
	    (cm-cli-database-error-log-string (and cm-session-context
						   (cm-session-context-user-name cm-session-context))
					      "CLI-COMMAND"
					      (http-connection-uri-string http-connection))

	    (when (or *cm-cli-debug* (debug-level-meets-or-exceeds? 4))
	      (format *debug-io* "~%CM-CLI connect: ~a~%" (http-connection-uri-string http-connection)))
	    (when (> (length uri-path) (length *cm-cli-command/uri-prefix*))
	      (let* ((command (subseq uri-path  ;skip over leading prefix portion of path
				      (length *cm-cli-command/uri-prefix*)))
		     (command-info (cm-cli-find-command-description :name command)))
		(setq command-name command)
		(when command-info
		  ;; Bundle tagged argument bindings into a session context.
		  ;; Removes 'default context' args, which are any args whose keys are not
		  ;; string representations of integers.        (destructively updates uri arg list...)

		  ;; Now bind the remaining args according to argument descriptions
		  (multiple-value-bind (argument-bindings unmatched-descriptions unmatched-args)
		      ;; Get .conmanrc which need to be added and fold into explicit user-supplied args.
		      (cm-cli-parse-args http-connection (third command-info)
					 (cm-cli-intercept-rc-arguments http-connection)
					 cm-session-context)
		    ;; Normally, for error conditions, we'd return some appropriate http response code
		    ;; and some html to display the response.  However, since this handler is for
		    ;; "cli/response" clients, instead of calling HTTP-ERROR-RESPONSE, we call a
		    ;; CM-CLI-ERROR-RESPONSE equivalent which will cause the client to print something nice.
		    (when (or *cm-cli-debug* (debug-level-meets-or-exceeds? 4))
		      (format *debug-io* "Bindings: ~s~%" argument-bindings)
		      (format *debug-io* "Missing args: ~s~%" unmatched-descriptions)
		      (format *debug-io* "Excess args: ~s~%" unmatched-args)
		      (format *debug-io* "Context: ~s~%" cm-session-context))
		    (when unmatched-descriptions
		      (cm-cli-http-response
		       *cm-returns-error-too-few-arguments*
		       http-connection "Required command arguments were missing: ~%~a"
		       (with-output-to-string (stream)
			 (mapc (lambda (arg-description)
				   (write-string "     " stream)
				   (pretty-arg-description arg-description stream)
				   (terpri stream))
			       unmatched-descriptions)))
		      (return-from cm-cli-http-server-get return-val))
		    (when unmatched-args
		      (cm-cli-http-response
		       *cm-returns-error-too-many-arguments*
		       http-connection "Too many command arguments were specified: ~s" unmatched-args)
		      (return-from cm-cli-http-server-get return-val))
		    ;; Args ok as far as we can tell here.  Dispatch the command with the cli/response
		    ;; protocol enabled.
		    (http-response http-connection *http-status-okay* :content-type *cm-cli-content-type*)
		    (let* ((result nil)
			   (perflog
			    (with-perflog (:command ;classification of thing we're logging
					   :comment (first command-info)) ; the command name
			      (setq result (funcall (second command-info)
						    http-connection cm-session-context argument-bindings))
			      *perflog*)))
		      ;; Print the perflog entry if we're logging performance, and return the command
		      ;; result.
		      (when *cm-cli-server-performance-log-stream*
			(perflog-note perflog "Args: ~s" argument-bindings)
			(unless (ignore-errors (print perflog *cm-cli-server-performance-log-stream*))
			  ;; This message should itself be logged to the error log, need additional
			  ;; error logging primitives to do this.  *FINISH*
			  (warn "Error writing to performance logging stream, shutting down the stream")
			  (cm-cli-close-performance-log-stream)))

		      (return-from cm-cli-http-server-get
			(if *cm-cli-testing-shutdown* :quit result))))))))

	  ;; Unrecognized request
	  (cm-cli-http-response
	   *cm-returns-error-unrecognized-command*
	   http-connection "Command ~a is not supported." raw-command))

      (when-debugging 0
	(let* ((command-end-time (get-internal-real-time))
	       (command-time (/ (- command-end-time command-start-time)
				(/ internal-time-units-per-second 1.0))))
	  (when (and *cm-file-add-time* (equal command-name "file_add" #||"cm_file_add"||#))
	    (format *trace-output* "~&acculumating file_add time")
	    (incf *cm-file-add-time* command-time))
	  (debug-message 0 "~15@a:  finished in ~d seconds (return value: ~a)"
			 raw-command command-time return-val))))

    ;normally nil so server doesn't exit
    return-val))			

#||

(defparameter *cm-cli-debug-arg-parsing* nil "True to print arg processing diagnostics")

(defun cm-cli-parse-args (http-connection arg-descriptors rc-args cm-session-context)
  "Return the same values as CLI-REQUEST-PARSE-HTTP-URI-ARGS, except that we require an additional
   parameter which serves as a list of arguments supplied as if they were a second command line
   of 'default' values.  Typically these resource configuration arguments are found in a client '.csf'
   file or equivalent.

   We validate and bind arguments as per those supplied by the user in the HTTP-CONNECTION uri args
   against the descriptions in ARG-DESCRIPTORS.  For any arguments in arg-descriptors which are not
   satisfied by the http connection uri args we then attempt to satisfy them against RC-ARGS. If any
   of the switches -v0, -v1 or -v2 was specified by the http connection, don't look in RC-ARGS. If none
   of the verbosity switches are set, default cm-session-context-verbosity to 1 (meaning -v1).

   RC-ARGS should be a (possibly empty) list of tokenized command strings in the correct
   (user assigned) order.

   We return three values as per CLI-REQUEST-PARSE-HTTP-URI-ARGS:
   1) argument bindings
   2) unmatched descriptions
   3) unmatched arguments"
  ;; Implementation note, this two-phased scheme means that the first call to match descriptors
  ;; against the http-connection uri args (i.e. *explicit* user supplied args) may yield
  ;; excess descriptors.  It also means that in a subsequent call to match rc-args, we may have
  ;; extra arguments, which are those for which overriding values were supplied in the first call
  ;; to parsing args.
  (multiple-value-bind (arg-bindings unmatched-descriptions unmatched-args)
      (cli-request-parse-http-uri-args http-connection arg-descriptors)

    (when (or *cm-cli-debug-arg-parsing* (debug-level-meets-or-exceeds? 4))
      (format *debug-io* "~%HTTP Argument Bindings: ~s~%" arg-bindings)
      (format *debug-io* "Unmactched Descriptors: ~s~%" unmatched-descriptions)
      (format *debug-io* "Unmatched Arguments: ~s~%" unmatched-args)
      (format *debug-io* "RC Arguments: ~s~%" rc-args))

    ;; Was there a verbosity switch set on the command line (i.e. specified by the http connection)?
    ;; if so, make cm-session-context-verbosity the highest number for which the switch is on
    (when (second (assoc 'v0 arg-bindings)) (cm-session-context-add cm-session-context :verbosity 0))
    (when (second (assoc 'v1 arg-bindings)) (cm-session-context-add cm-session-context :verbosity 1))
    (when (second (assoc 'v2 arg-bindings)) (cm-session-context-add cm-session-context :verbosity 2))

    ;; If there were excess descriptions, attempt to match them against arg descriptors.
    ;; Because the arg parser's weeding out of :REST args descriptors in unmatched descriptors
    ;; is a feature, we reinstate it here if there was such a beast.
    ;; [2000-04-24 naha] 10 bonus points go to anyone who has a clue what the
    ;; above comment means.
    (when ;; (and unmatched-descriptions rc-args)
	rc-args
      (multiple-value-bind (arg-bindings-2 unmatched-descriptions-2 #||unmatched-args-2||#)
	  (cli-request-parse-ordered-args
	   rc-args				; (append rc-args unmatched-args)
	   ;; Only switch and keyword arguments may appear in the
	   ;; ChangeSafe RC file.
	   (remove-if-not (lambda (desc)
			      (and (member (getf (cdr desc) :type)
					   '(:keyword :switch))
				   ;; elminimate descriptors of
				   ;; arguments we already have values
				   ;; for.
				   (not (find (car desc) arg-bindings
					      :key #'car))))
			  ;; Can't use UNMATCHED-DESCRIPTIONS here
			  ;; because it only includes descriptions of
			  ;; :REQUIRED arguments, so only required
			  ;; arguments would be gotten from the RC
			  ;; file.  Certain commands, like "ws_query",
			  ;; take an optional "-ws" argument from the
			  ;; .csf file.
			  arg-descriptors))

	(when (or *cm-cli-debug-arg-parsing* (debug-level-meets-or-exceeds? 4))
	  (format *debug-io* "RC Argument Bindings: ~s~%" arg-bindings-2)
	  (format *debug-io* "Unmatched Descriptors: ~s~%" unmatched-descriptions-2))

	;; If cm-session-context-verbosity is still nil, see if any verbosity switches were
	;; set by the second binding (presumably from .csf)
	(unless (cm-session-context-verbosity cm-session-context)
	  (when (second (assoc 'v0 arg-bindings-2)) (cm-session-context-add cm-session-context :verbosity 0))
	  (when (second (assoc 'v1 arg-bindings-2)) (cm-session-context-add cm-session-context :verbosity 1))
	  (when (second (assoc 'v2 arg-bindings-2)) (cm-session-context-add cm-session-context :verbosity 2)))

	;; If there are unmatched descriptions, they are the ones we care about.
	;; Unmatched args on the other hand are of no concern for these implicit default args.
	;; Merge the arg bindings, and supplant the unmatched descriptions,
	;; don't do anything with the nested unmatched args except ignore them.
	(setq arg-bindings (nconc arg-bindings arg-bindings-2))
	(setq unmatched-descriptions unmatched-descriptions-2))

      ;; If there were excess arguments, make one last ditch attempt at adding
      ;; a previously unfulfilled :REST descriptor to match remaining arguments
      ;; We really need a good argument merging fuction, but this was written under extreme time pressure.
      (let ((rest-desc (find-if (lambda (desc)
				    (eq (getf (cdr desc) :type) :rest))
				arg-descriptors)))
	(when (and rest-desc
		   (not (find (car rest-desc) arg-bindings :key #'car))
		   unmatched-args)
	  ;; :REST was specified, and not previously matched...
	  (let ((arg-bindings-3 (cli-request-parse-ordered-args unmatched-args
								(append unmatched-descriptions
									(list rest-desc)))))
	    ;; Don't need to report unmatched :REST desc if it wasn't matched.  If it was,
	    ;; merge the binding into the returned arg-bindings.
	    (when arg-bindings-3
	      ;; Remove the arguments which were matched by :REST
	      ;; Note we may(?) match switches and other things which, if we matched here, we're ignoring.
	      (setq unmatched-args (set-difference unmatched-args
						   ;; Skip the name for the binding,
						   ;; the result of the binding is a list of strings
						   (second
						    ;; Find the right binding, if there were multiples
						    (find (car rest-desc)
							  arg-bindings-3 :key #'car))
						   :test #'string=))
	      ;; Remove the description from unmatched descriptions
	      (setq unmatched-descriptions (remove rest-desc unmatched-descriptions))
	      ;; Add the bindings to our result
	      (setq arg-bindings (append arg-bindings arg-bindings-3)))))))
    ;; If cm-session-context-verbosity is still nil, default it to 1
    (unless (cm-session-context-verbosity cm-session-context)
      (cm-session-context-add cm-session-context :verbosity 1))
    (values arg-bindings unmatched-descriptions unmatched-args)))

(defconstant *cm-cli-conmanrc* "CHANGESAFERC" "URI arg key string used to convey .csf client contents.")

(defun cm-cli-intercept-rc-arguments (http-connection)
  "If there is a CONMANRC uri arg key in the uri args for the http-connection,
   strip it out, parse the string into command line tokens, and add it to the arguments
   as if the user typed them in http-connection.  We destructively modify the
   decoded-uri-args of the http-connection.  Return-value: n/a."
  (let ((rc-args (http-connection-get-uri-arg http-connection *cm-cli-conmanrc*)))
    (when rc-args
      ;; Zap the input
      (setf (http-connection-decoded-uri-args-alist http-connection)
	(delete *cm-cli-conmanrc* (http-connection-decoded-uri-args-alist http-connection)
		:key #'car :test #'string=))
      ;; Parse the args into command line token format, only tricky if there are quoted substrings
      (tokenize-string-cli rc-args))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +cm-cli-command-function-prefix+ "CM-CLI-"
    "This string is prepended to ChangeSafe command name strings to form
   the name of the command name symbol."))

(defmacro define-cm-cli-command (&environment env
					      command-name (connection-var cm-session-context-var
							    &rest arg-descriptions)
				 &body body)
  "Define a function which will handle a given CM command whose URI abs_path is /command-name.
   Within the body of the function, CONNECTION-VAR is bound to an HTTP-CONNECTION object,
   ARG-DESCRIPTIONS should describe arguments compatible with
   CLI-REQUEST-PARSE-URI-ARGS.  Runtime results will be bound to lexical variables matching
   those named in arg-descriptions.

   CM-SESSION-CONTEXT-VAR should describe a variable which is bound to a CM-SESSION-CONTEXT object,
   which receives default contextual information generated independently from those arguments
   described by ARG-DESCRIPTIONS.

   *TBD* add supplied-p capability for argument detection.

   BODY is the body of the function which may include a docstring.

   By using this macro, the command is validated as a legitimate point of entry
   to the http server, and it is called only if the arguments supplied are compatible with those
   in the arg-descriptions-list."
  ;; Make sure the :syntax elements of the ARG-DESCRIPTIONs are all
  ;; strings.  I spent a day trying to figure out wht a command
  ;; wouldn't run and the problem was that I left the stupid #. off
  ;; of the value of the :syntax property.
  (loop for (arg-name . arg-props) in arg-descriptions
      for syntax = (getf arg-props :syntax nil)
      for type = (getf arg-props :type nil)
      do
	(if type
	    (unless (member type '(:positional :keyword :switch :rest))
	      (warn "The :TYPE for argument ~s, ~s in not valid."
		    arg-name type))
	  (warn "No :type specified for argument ~s" arg-name))
	(when syntax
	  (unless (stringp syntax)
	    (warn "The :SYNTAX for argument ~s is ~s, which is not a string."
		  arg-name syntax))))
  (let* ((command-name (typecase command-name
			 (string command-name)
			 (symbol (if (constantp command-name env)
				     (symbol-value command-name)
				   (error "Can't determine command name from ~s at compile time."
					  command-name)))))
	 (function-name (intern (concatenate 'string +cm-cli-command-function-prefix+
					     ;; This obviously expects cm_ to be part of things
					     #||
					     (nstring-substitute-chars
					      (subseq (string-upcase command-name) 3)
					      '((#\_ . #\-)))
					     ||#
					     command-name)))
	 (arg-descriptions
	  ;; HTC 2/2/2000 added verbosity switches to every command
	  (append arg-descriptions
		  '((v0 :type :switch :syntax #.*cm-cli-switch-v0*)
		    (v1 :type :switch :syntax #.*cm-cli-switch-v1*)
		    (v2 :type :switch :syntax #.*cm-cli-switch-v2*))))
	 (arg-bindings-var '#:arg-bindings)
	 (docstring nil)
	 (declarations nil)
	 (conection-param '#:http-connection)
	 (session-param '#:cm-session-context)
	 (bindings (mapcar (lambda (desc)
			       (list (car desc)
				     `(cli-request-argument-value ',(car desc) ,arg-bindings-var)))
			   arg-descriptions))
	 ;; (doc-and-decls nil)
	 )
    (when (stringp (car body))
      (setf docstring (car body))
      (setf body (cdr body)))
    (unless bindings
      (if declarations
	  (setq declarations (append declarations `((ignore ,arg-bindings-var))))
	(setq declarations `(declare (ignore ,arg-bindings-var)))))
    `(progn
       (when (cm-cli-find-command-description :function ',function-name)
	 (setq *cm-cli-command-list* (delete ',function-name *cm-cli-command-list* :key #'second)))
       (push (list ,command-name ',function-name ',arg-descriptions) *cm-cli-command-list*)
       (defun ,function-name (,conection-param ,session-param ,arg-bindings-var)
	 ,docstring
	 (declare (ignorable ,arg-bindings-var))
	 (let ((,connection-var ,conection-param)
	       (,cm-session-context-var ,session-param)
	       ,@bindings)
	   (declare (ignorable v0 v1 v2))
	   ,@body)))))

(defmacro with-cm-cli-control ((http-connection cm-session-context activity-description) &body activity)
  "Execute ACTIVITY in with errors ignored, so that we can report any errors in executing the activity.
   This is simple boilerplate for responding to client requests after performing the expected activity.

   The result of ACTIVITY is discarded.

   ACTIVITY-DESCRIPTION should typically describe the activity being performed, and  should be a string.
   It will be embedded in various success or failure response messages."
  `(flet ((body () ,@activity))
     (declare (dynamic-extent #'body))
     (with-cm-cli-control-function ,http-connection ,cm-session-context ,activity-description #'body)))

(defun with-cm-cli-control-function (http-connection cm-session-context
				     activity-description activity-function)
  "Helper routine for the with-cm-cli-control macro."
  (let ((stream (http-connection-stream http-connection))
	(return-code *cm-returns-success*)
	(command-start-time nil)
	(warnings-text nil))

    (when *cm-cli-command-times-show*
      (setq command-start-time (get-internal-real-time)))

    ;; print (maybe) some info for each command - generally for testing purposes
    (when *cm-cli-debug-server-info*
      (let* ((thread-info (format nil "~a"  sys:*current-process*))
	     (tid-start (+ 8 (or (search " thread " thread-info) 0)))
	     (tid-end (search " for " thread-info))
	     (thread-id (and tid-end (parse-integer thread-info :junk-allowed t
				       :start tid-start
				       :end tid-end)))
	     (dont-show-dynamic-text (and *within-regression-tests*
					  (not *cm-cli-doing-performance-run*))))
	;; if *within-regression-tests*, don't show the stuff that creates diffs
	;; unless we are debugging or are doing a performance run
	(cm-cli-response :display
			 (format nil "~&;;;;CSF ~a, Server: ~a, Port: ~a, ~a, PID: ~a, Command: ~a~%"
				 (if dont-show-dynamic-text
				     "<{Date & Time}>"
				   (time-string (get-universal-time) :no-days t))
				 (if dont-show-dynamic-text
				     "<{Server}>"
				   (long-site-name))
				 *conman-server-log-start-port*
				 (ecase (conman-check-server-operating-mode)
				   (:read-only "R/O")
				   (:read/write "R/W"))
				 (if dont-show-dynamic-text
				     "<{PID/T}>"
				   (format nil "~a/T~a" (excl::getpid) ;; Franz SPR 22543
					   (or thread-id "-")))
				 activity-description)
			 stream :escape-it t)))

    (unless *within-regression-tests*
      (when-debugging 4
	(cm-cli-response :trace 1 stream :escape-it nil)))

    (tail-labels ((update-return-code (new-code)
		    ;; We keep the "most severe" return code, discarding any
		    ;; others.  Error return codes are most severe.  Warnings
		    ;; are more severe than *CM-RETURNS-SUCCESS*.
		    ;; *CM-RETURNS-WARNING-MULTIPLE-WARNINGS-ISSUED* is more
		    ;; severe than any other warning.
		    (cond ((= return-code *cm-returns-success*)
			   ;; This is the first warning.  Note it's return code
			   (setq return-code new-code))
			  ((cm-returns-error-p return-code)
			   ;; NEW-CODE might also be an error code, but in
			   ;; theory this shouldn't happen since errors
			   ;; terminate operation.  Only if another error
			   ;; occurred during error handling or responding
			   ;; would we get another error.  Do nothing.  Maybe
			   ;; someday we'll think of something better.
			   )
			  ((cm-returns-error-p new-code)
			   ;; Errors are more severe than anything but other errors.
			   (setq return-code new-code))
			  ((= return-code new-code)
			   ;; Same kind of warning as the previous one, ignore it.
			   )
			  (t ;; a new warning with a different return code
			   (setq return-code *cm-returns-warning-multiple-warnings-issued*))))

		  (log-condition (condition)
		    (when *cm-database-error-log*
		      (cm-cli-database-error-log-string (cm-session-context-user-name cm-session-context)
							"CLI-ERROR-MESSAGE"
							(format nil "~a" condition))
		      (when (typep condition 'conman-condition)
			(cm-cli-database-error-log-string (cm-session-context-user-name cm-session-context)
							  "CLI-ERROR-NUMBER"
							  (format nil "~a" (conman-condition-return-code condition))))
		      ))

		  (display-condition (condition)
		    (setq warnings-text
			  (nconc warnings-text (list (format nil "~a" condition)))))


		  (redirect-noise (message)
		    (debug-message 3 "Process ~s ~s.~&~s" *current-process* message core::*txn-mutex-queue*)
		    #||
		    (when *conman-server-log*
		      (conman-server-log-string
		       (cm-session-context-user-name cm-session-context)
		       'busy-redirect
		       (format nil "Process ~s ~s.~&~s" *current-process* message core::*txn-mutex-queue*))) ||#))

      (multiple-value-bind (result condition)
	  (with-conman-signal-handling
	      ;; *FINISH*: when we have meaningful error codes or conditions defined,
	      ;; need to stash them in RETURN-CODE.  Right now we ignore the return-code
	      ;; values
	      (:non-terminating-warning-hook ;has a return code
	       (lambda (condition)
		   ;; *FINISH* What should we do if there are multiple
		   ;; warnings during the course of a single command?
		   ;; We can only return one status code.
		   (log-condition condition)
		   (update-return-code (conman-condition-return-code condition))
		   (display-condition condition)
		   (muffle-warning))
	       :terminating-warning-hook ;has a return code
	       (lambda (condition)
		   ;; *FINISH* What should we do if there are multiple
		   ;; warnings during the course of a single command?
		   ;; We can only return one status code.
		   (log-condition condition)
		   (update-return-code (conman-condition-return-code condition))
		   (display-condition condition))
	       :conman-error-hook	;has a return code
	       (lambda (condition)
		   (log-condition condition)
		   (setq return-code (conman-condition-return-code condition)))
	       :generic-warning-hook	;does not have a return code
	       ;; NOTE! Called for non-terminating conman warnings, only case where a hook is called twice.
	       (lambda (condition)
		   (log-condition condition)
		   (unless (or (cm-returns-error-p return-code)
			       (cm-returns-warning-p return-code))
		     ;; Don't let generic warning override a specific code if one has been set.
		     (setq return-code *cm-returns-warning-default*) ;unknown warning code
		     )
		   (display-condition condition)
		   (muffle-warning))
	       :generic-error-hook	;does not have a return code
	       (lambda (condition)
		   (log-condition condition)
		   ;; Don't let generic error override a specific error code if one has been set.
		   (unless (cm-returns-error-p return-code)
		     ;;unknown failure code
		     (setq return-code *cm-returns-error-default*))))
	    (handler-bind ((awaiting-txn-mutex
			    (lambda (condition)
				(redirect-noise "cm-cli-control notifying user awaiting mutex")
				(cm-cli-response :display (format nil "~a" condition) stream
						 :escape-it nil)
				(continue)))
			   (granted-txn-mutex
			    (lambda (condition)
				(redirect-noise "cm-cli-control notifying user granted mutex")
				(cm-cli-response :display (format nil "~a" condition) stream
						 :escape-it nil)
				(continue)))

			   (busy-redirect
			    (lambda (condition)
				(redirect-noise "cm-cli-control issuing busy-redirect")
				(cm-cli-response :busy-redirect (busy-redirect/queue-length condition) stream)
				;; And we're out of here!
				(return-from with-cm-cli-control-function (values))))

			   (conman-wrong-server-operating-mode
			    (lambda (condition)
				(declare (ignore condition))
				(redirect-noise "cm-cli-control issuing no-service-redirect")
				(cm-cli-response :no-service-redirect "" stream)
				(return-from with-cm-cli-control-function (values))))
			   )
	      ;; The actual work being done under handler control
	      (funcall activity-function)))

	;; show the elapsed time?
	(when *cm-cli-command-times-show*
	  (let* ((command-end-time (get-internal-real-time))
		 (command-time (/ (- command-end-time command-start-time)
				  (/ internal-time-units-per-second 1.0))))
	    (incf *cm-cli-command-times-so-far* command-time)
	    (cm-cli-response :display
			     (format nil ";;;;CSF ~a: Finished in ~d seconds, total: ~d seconds"
				     activity-description command-time
				     *cm-cli-command-times-so-far*)
			     stream :escape-it t)))

	;; If the return value is :to-be-continued, then the CM has already been redirected.
	;; We remain silent.
	(unless (eq result :to-be-continued)
	  ;; Print any collected diagnostic messages:
	  (dolist (w warnings-text)
	    (cm-cli-response :display w stream :escape-it t))
	  ;; Now interpret return codes for the CLI.
	  (cond (result			;things went ok, but check for warnings
		 ;; HTC 2/3/2000 do not put out completed message if verbosity is 0 (-v0)
		 (unless (= (cm-session-context-verbosity cm-session-context) 0)
		   (cm-cli-response
		    ;; ~@( is upper case conversion on first letter. )
		    :display (format nil "~@(~a~) completed ~a."
				     activity-description
				     (if (= return-code *cm-returns-success*)
					 "successfully"
				       "with warnings"))
		    stream))
		 (cm-cli-response :return return-code stream))
		(condition		;things did not go ok
		 (cm-cli-response :display
				  (format nil "An error occurred while attempting ~a:~%~a"
					  activity-description condition)
				  stream :escape-it t)
		 (cm-cli-response
		  ;; Assume the current condition is an error, and if there wasn't a failure code recorded,
		  ;; report the generic one now
		  :return (if (cm-returns-error-p return-code)
			      return-code
			    *cm-returns-error-default*)
		  stream))
		(t			;if result is nil and condition is nil, we've no clue what happened
		 (cm-cli-response :display
				  (format nil "An unknown error occurred while attempting ~a."
					  activity-description)
				  stream)
		 (cm-cli-response :return *cm-returns-error-default* stream)))))))
  nil)

(defun prefer-read-only-server (http-connection cm-session-context thunk)
  "If we are operating on a read-only server, simply funcall the thunk.  If not,
   then issue a fake busy redirect in order to shuffle the load off the primary
   server."
  ;; If we are a read/write server, we fake a busy signal for this command
  ;; if we are allowed to.  This will have the effect of pushing
  ;; commands off to read-only servers.
  (if (and (eq (conman-check-server-operating-mode) :read/write)
	   (cm-session-context-allow-busy-redirect cm-session-context)
	   ;; Technically, this could be null and the client might know of
	   ;; other servers, but this will avoid sending busy redirects in
	   ;; the more common case that we are the only server.
	   (alternative-command-line-server-uri-strings)
	   ;; Don't bother if we are regression testing, there is no server there.
	   ;;   unless we are running HWP servers (which need to test the redirection)
	   (or (not *within-regression-tests*)
	       *within-regression-tests-hwp-servers*))
	(progn
	  (if (null (svref core::*txn-mutex-queue* 0))
	      ;; perform a prefer-read-only redirect.
	      (cm-cli-response :prefer-read-only-redirect 0 (http-connection-stream http-connection))
	    ;; we are read-write, but we actually seem to be busy.
	    (cm-cli-response :busy-redirect 0 (http-connection-stream http-connection)))
	  :to-be-continued)
      (funcall thunk)))

(defun conman-require-host (host http-connection cm-session-context thunk)
  (declare (ignore cm-session-context))
  (let* (
	 (hn (machine-instance))
	 (lhn (length hn))
	 (lhost (length host))
	 )
    (if (or (and (= lhn lhost)
		 (string-equal host hn))
	    (and (> lhost lhn)
		 (string-equal host hn :end1 lhn)
		 (char= (char host lhn) #\.))
	    (and (< lhost lhn)
		 (char= (char hn lhost) #\.)
		 (string-equal host hn :end2 lhost)))
	(funcall thunk)
      (let* ((hosts (filter-alternative-servers-by-host host)))
	(if hosts
	    (progn
	      (cm-cli-response
	       :specific-host-redirect hosts
	       (http-connection-stream http-connection))
	      :to-be-continued)
	  (conman-signal-error *cm-returns-error-host-not-available*
			       "Specified host ~a is not available." host)
	  )))))



(defun cm-cli-create-cm-session-context (http-connection)
  "Create a CM-SESSION-CONTEXT from the uri-arg-list associated with HTTP-CONNECTION,
   for any keys named by *cm-cli-uri-key-xxx* constants.

   Any URI args which are not keyed by integer representations and which match the *CM-CLI-URI-KEY-XXX
   variables above will be stripped from the http connection, and will be encoded in the context.
   If no variables are found, a CM-SESSION-CONTEXT is still returned.

   URI <query-arg> keys which aren't strings matching the above keys are ignored.

   We return a CM-SESSION-CONTEXT object which has zero or more slots filled in.
   Note that basic syntactic validation of supplied values is performed, but no semantic validation."
  (let ((session (cm-session-context-create http-connection))
	(matched-pairs nil))		;acons cells in http connection args alist which we "ate"
    ;; Here we parse keys which are found in the uri args
    (flet ((process-key (key value)	;add key if appropriate
	     (unless (parse-integer key :junk-allowed t)
	       ;; Not an integer key, see if it matches known key values
	       (macrolet ((set-it (slot) `(SETF (,slot SESSION) VALUE))
			  (set-bool (slot) `(SETF (,slot SESSION) (STRING-EQUAL VALUE "T")))
			  (set-did (slot) `(SETF (,slot SESSION)
					     (PARSE-DID VALUE)))
			  (set-platform (slot)
			    `(SETF (,slot SESSION)
			       (GUARANTEE-PLATFORM
				(PLATFORM-NAME->PLATFORM VALUE))))
			  (set-pathname (slot)
			    `(SETF (,slot SESSION)
			       (PARSE-CLIENT-NAMESTRING VALUE
							(CM-SESSION-CONTEXT-CLIENT-PLATFORM
							 SESSION))))
			  (set-directory (slot)
			    `(SETF (,slot SESSION)
			       (PARSE-CLIENT-DIRECTORY-NAMESTRING VALUE
								  (CM-SESSION-CONTEXT-CLIENT-PLATFORM
								   SESSION))))
			  )
		 ;; PERFORMANCE: these string comparisons are not good.  Optimize
		 ;; Return T if we ate the arg, NIL if we didn't

		 (cond ((string= key *cm-cli-uri-key-user-name*)
			(set-it cm-session-context-user-name) t)
		       ((string= key *cm-cli-uri-key-client-platform*)
			(set-platform cm-session-context-client-platform) t)
		       ((string= key *cm-cli-uri-key-current-directory*)
			(set-directory cm-session-context-current-directory) t)
		       ((string= key *cm-cli-uri-key-user-home-directory*)
			(set-directory cm-session-context-user-home-directory) t)
		       ((string= key *cm-cli-uri-key-rcpath*)
			(set-pathname cm-session-context-rc-path) t)
		       ((string= key *cm-cli-uri-key-timezone*)
			(set-it cm-session-context-client-timezone))
		       ((string= key *cm-cli-uri-key-allow-busy-redirect*)
			(set-bool cm-session-context-allow-busy-redirect) t)
		       (t nil))))))
      (let* ((decoded-alist (http-connection-decoded-uri-args-alist http-connection))

	     ;; The *cm-cli-uri-key-client-platform* must be
	     ;; present and processed before any pathnames are.
	     (platform-pair (assoc *cm-cli-uri-key-client-platform* decoded-alist
				   :test #'string=)))
	(when platform-pair
	  (process-key (car platform-pair) (cdr platform-pair)))

	(loop for acons in decoded-alist
	    do (when (process-key (car acons) (cdr acons))
		 (push acons matched-pairs))))
      ;; Strip matched pairs from http-connection
      ;; If this is not desired, we should return it as a second value so the caller knows.
      (setf (http-connection-decoded-uri-args-alist http-connection)
	(set-difference (http-connection-decoded-uri-args-alist http-connection)
			matched-pairs)))
    session))


;;;
;;; Pathname parsing for cm-cli.
;;;

(defun cm-cli-parse-namestring-in-server-syntax (namestring)
  "Given NAMESTRING in the format used by the SERVER, return a pathname.
   Trailing directory separator must be present to indicate a directory."

  ;; THIS MUST ASK THE CLIENT TO CANONICALIZE (SNAP SYMLINKS)
  ;; (except the problem is that the java client can't do this,
  ;; thank you, Sun)
  (parse-client-namestring namestring (server-platform)))

(defun cm-cli-parse-directory-in-server-syntax (namestring)
  "Given NAMESTRING which is expected to name a directory (relative or absolute)
   in the format used by the SERVER, return a pathname.  Trailing directory
   separator is optional as NAMESTRING will never be interpreted as a file."
  (parse-client-directory-namestring namestring (server-platform)))

(defun cm-cli-parse-absolute-directory-in-server-syntax (namestring)
  "Given NAMESTRING which is expected to name an absolute directory in the format
   used by the SERVER, return a pathname.  Trailing directory separator is optional
   as NAMESTRING will never be interpreted as a file."
  (guarantee-absolute-directory-pathname
   (cm-cli-parse-directory-in-server-syntax namestring)))

(defun cm-cli-parse-relative-directory-in-client-syntax (namestring cm-session-context)
  "Given NAMESTRING which is expected to name a relative directory in the format
   used by the client, return a pathname.  Trailing directory separator is optional
   as NAMESTRING will never be interpeted as a file."
  (guarantee-relative-directory-pathname
   (parse-client-directory-namestring namestring
				      (cm-session-context-client-platform cm-session-context))))

(defun cm-cli-parse-absolute-directory-specifier (cm-session-context namestring server-relative)
  "Given NAMESTRING, which is expected to name a directory, parse it
   according to the appropriate switches and return the pathname.  If SERVER-RELATIVE,
   NAMESTRING must name an absolute path in server syntax.  If not SERVER-RELATIVE,
   NAMESTRING can be an absolute path in client syntax, or it can be a relative
   path, in which case it is merged with the clients current-directory.  Trailing
   directory separator is optional as NAMESTRING will never be interpreted as
   a file.

   The result of this function is always an absolute directory pathname."
  (unless (stringp namestring)
    (conman-signal-error *cm-returns-error-invalid-database-directory*
			  "A directory name is needed. You gave: ~s"
			  namestring))

  (guarantee-absolute-directory-pathname
   (if server-relative
       ;; If server-relative, must be in server-platform syntax
       ;; and must be an absolute pathname.
       (cm-cli-parse-absolute-directory-in-server-syntax namestring)
     ;; If not server-relative, must be in client-platform syntax
     ;; and may be a pathname relative to the current directory
     (merge-pathnames
      (parse-client-directory-namestring namestring
					 (file-system-platform
					  (cm-session-context-file-system-agent cm-session-context)))
      (cm-session-context-current-directory cm-session-context)))))


(defun cm-cli-parse-filename (cm-session-context namestring server-relative)
  "Given NAMESTRING, parse it according to the appropriate switches and return
   the pathname.  If SERVER-RELATIVE, NAMESTRING should name an absolute path in
   server syntax.  If not SERVER-RELATIVE, NAMESTRING can be an absolute path in
   client syntax, or it can be a relative path, in which case it is merged with
   the clients current-directory.

   Note:  If NAMESTRING names a relative path, but SERVER-RELATIVE is specified,
   the resultant path will be merged with current client directory.  This is
   unlikely to be meaningful unless the server and client run on the same machine
   or have the same mount points.

   The result of this function is always an absolute directory pathname."
  (guarantee-absolute-pathname
   (if server-relative
       ;; What do we do here?
       ;; This idea of parsing relative to the client current directory is
       ;; completely bogus.
       (merge-pathnames
	(parse-client-namestring namestring (server-platform))
	(cm-session-context-current-directory cm-session-context))

     (merge-pathnames
      (parse-client-namestring namestring
			       (file-system-platform (cm-session-context-file-system-agent cm-session-context)))
      (cm-session-context-current-directory cm-session-context)))))

(defun default-repository-directory ()
  "This is where the ChangeSafe repositories will go if you do not specify a
   fully qualified pathname to -master-repository-name."
  ;; I expect that this is the better choice, but the current-directory is
  ;; where test-4a expects it to be!
  ;; (translate-logical-pathname "TS50:REPOSITORIES;")
  ;; Ouch!
  (cd)
  )

(defun cm-cli-parse-repository-name (master-repository-name)
  "Given MASTER-REPOSITORY-NAME as a fully qualified namestring, parse it
   according to server syntax and return the result.  If a relative pathname
   is given (or even just a file name), merge the path with
   (DEFAULT-REPOSITORY-DIRECTORY)."
  (etypecase master-repository-name
    (string
     (let ((db-name (db-name-parse master-repository-name)))
       (if (db-name-local-p db-name)
	   (let ((merged (db-name-merge db-name (default-repository-directory))))
	     (guarantee-absolute-pathname (db-name-pathname merged))
	     merged)
	 db-name)))
    (db-name master-repository-name)
    (pathname master-repository-name)))




;;;
;;; Date and time parsing
;;;

(defun cm-cli-parse-conman-time-spec (string interval-end)
  "Parse a ChangeSafe date/time specification.  Since the user might be vague
   about the time (omitting seconds, or omitting time of day entirely) we
   need some clue about whether the resulting time should be at the upper
   end or the lower end of the interval of times that fall into the user's
   vaguely specified range.  That's what INTERVAL-END is for.
   If the user just enters \"1999-12-07\", then that will be interpreted as
   the time 00:00 on that date if INTERVAL-END is :LOWER and 23:59 if
   INTERVAL-END is :UPPER.

   Return a TIME-STAMP object with the result.  One might expect that a universal time would
   be preferred, but ChangeSafe uses all the time-specs to drive metaversion use, and metaversions
   are specified using time-stamp objects."
  (time-stamp-create
   (multiple-value-bind (ut precision)
       (iso-date-time-string->universal-time string)
     (ecase interval-end
       (:lower ut)
       (:upper (+ ut precision -1))))))

;;;
;;;  Other tests used below.
;;;

(defun current-directory-in-workspace-p (cm-session-context)
  ;; Detect whether the current directory in session context is at or
  ;; below the directory associated with the workspace.
  (let ((current-directory (cm-session-context-current-directory cm-session-context))
	(workspace-root (cm-session-context-workspace-root cm-session-context)))
    (and workspace-root
	 (or (funcall (platform-pathname-match-p (cm-session-context-client-platform cm-session-context))
		      current-directory (make-wild-pathname workspace-root))
	     ;; *FINISH*: gross hack.  We need z:\TEMP and Z:\temp to compare equal on NT
	     ;; This makes is so I can at least run test-8a on my machine.  JDT.

	     ;; Yuck.  Write a platform independent pathname-match-p in pathname-hacks.
	     ;; ~ jrm

	     (and (stringp (pathname-device current-directory)) ; assume we're comparing windows pathname
		  (equal (search (namestring workspace-root)
				 (namestring current-directory)
				 :test #'char-equal) ;insensitive file case,
			 0))))))

(defun guarantee-cd-in-workspace (cm-session-context server-relative-p
				  &optional (cm-error-code *cm-returns-error-default*))
  "Checks to ensure that the current directory is at or below the current
   workspace.

   SERVER-RELATIVE-P should be true if the operation is being performed
   as a server-relative operation, and NIL otherwise."
  ;; You simply can't be CD'd to a directory on another machine unless
  ;; there is some sort of mapping.  So we punt.
  (unless (cm-session-context-ws-id cm-session-context)
    (conman-signal-computed-error
     cm-error-code
     "The working directory ~a is not in a workspace."
     (cm-session-context-current-directory cm-session-context)
     ))

  (unless server-relative-p
    (guarantee-absolute-directory-pathname (cm-session-context-workspace-root cm-session-context))
    (unless (current-directory-in-workspace-p cm-session-context)
      (conman-signal-computed-error
       cm-error-code
       "The working directory ~a is not in the workspace ~a"
       (cm-session-context-current-directory cm-session-context)
       (cm-session-context-workspace-root cm-session-context)))))

(defun guarantee-cd-not-in-workspace (cm-session-context server-relative
				      &optional (cm-error-code *cm-returns-error-default*))
  "Checks to ensure that the current directory is not at or below the current
   workspace."
  ;; You simply can't be CD'd to a directory on another machine unless
  ;; there is some sort of mapping.  So we punt.
  (unless server-relative
    (when (and (cm-session-context-ws-id cm-session-context)
	       (cm-session-context-workspace-root cm-session-context))
      (guarantee-absolute-directory-pathname (cm-session-context-workspace-root cm-session-context))
      (when (current-directory-in-workspace-p cm-session-context)
	;; The workspace root pathname needs to be output with ~S
	;; rather than ~A because this error message can appear in
	;; test output and the period at the end of the message might
	;; be interpreted as part of the pathname by the test
	;; harness's pathname filter.
	(conman-signal-computed-error
	 cm-error-code
	 "The working directory ~s appears to be in the workspace ~s."
	 (cm-session-context-current-directory cm-session-context)
	 (cm-session-context-workspace-root cm-session-context))))))


;;; The following definitions all respond to CLI commands via HTTP GET requests.
;;; The "cli/response" procotol used here is defined in CM.Java.
;;;
;;; ATTENTION!!
;;;
;;; Docstrings for these commands are used for on-line help, and as the potential
;;; basis for hardcopy documentation.  Please provide useful and complete descriptions of
;;; command behavior from a user perspective in these commands.

(define-cm-cli-command "test_dummy" (http-connection cm-session-context (args :type :rest))
  "This command is purely used for regression testing, it is not a CM command for end-users."
  (let ((stream (http-connection-stream http-connection)))
    (cm-cli-response :display "Regression Interface Invoked" stream)
    (format *debug-io* "~%CM-CLI: User: ~a" (cm-session-context-user-name cm-session-context))
    (format *debug-io* "~%CM-CLI: Home: ~a" (cm-session-context-user-home-directory cm-session-context))
    (format *debug-io* "~%CM-CLI: Client Platform: ~a"
	    (cm-session-context-client-platform cm-session-context))
    (format *debug-io* "~%CM-CLI: Current Directory: ~a"
	    (cm-session-context-current-directory cm-session-context))
    (format *debug-io* "~%CM-CLI: RCPATH: ~a" (cm-session-context-rc-path cm-session-context))
    (format t "~%User: ~a"
	    (if (cm-session-context-user-name cm-session-context)
		"<Present, but not displayed to suppress regression diffs based on user running tests.>"
	      "Not present"))
    (format t "~%Home: ~a"
	    (if (cm-session-context-user-home-directory cm-session-context)
		"<Present, but not displayed to suppress regression diffs based on user running tests.>"
	      "Not present"))
    (format t "~%Client Platform: ~a"
	    (if (cm-session-context-client-platform cm-session-context)
		"<Present, but not displayed to suppress regression diffs based on platform.>"
	      "Not present"))
    (format t "~%Client timezone offset: ~a"
	    (cm-session-context-client-timezone cm-session-context))
    (let ((rc-path (cm-session-context-rc-path cm-session-context)))
      (if rc-path
	  (format t "~%Rc-path: ~a"
		  ;; This probably only works without regression diffs because of Franz support
		  ;; for paths with unix and dos component separators.
		  ;; Assume pathname-directory returns something like
		  ;; => (ABSOLUTE ts50 conman root-tempdir client-tempdir)
		  (last (pathname-directory rc-path) 2))
	(format t "~%RC-PATH was NIL."))
      (when rc-path
	(format t "~%Current Directory: ~a"
		(last (pathname-directory rc-path) 2))))
    (format t "~%Args: ~a~%" args)
    (with-cm-cli-fsa (cm-session-context)
      (print (file-system-platform (cm-session-context-file-system-agent cm-session-context)) *debug-io*)
      (print (file-system-get-parent-directory (cm-session-context-file-system-agent cm-session-context) ".") *debug-io*))
    (cm-cli-response :display "Test_dummy appears to have completed successfully." stream)
    (cm-cli-response :return *cm-returns-success* stream))
  )

(define-cm-cli-command "test_dummy2" (http-connection cm-session-context)
  "This command is purely used for regression testing, it is not a CM command for end-users.
   It is similar to test_dummy, but is used for testing of the warnings reporting mechanism."
  (with-cm-cli-control (http-connection cm-session-context "test warnings")
    (conman-signal-warning
     *cm-returns-warning-default*
     "This is a warning message")
    t))

(defvar *test-dummy-waiting-semaphore*
    (make-semaphore "Test dummy.")
  "A semaphore for synchronizing test dummy3.")

(define-cm-cli-command "test_dummy3"
    (http-connection
     cm-session-context
     (switch-a :type :switch :syntax "-option-a")
     (switch-b :type :switch :syntax "-option-b"))
  "This command is purely used for regression testing, it is not a CM command for end-users.
   It is similar to test_dummy, but is used for testing load balancing and redirection."
  (tail-labels ((pretend-to-work ()
		  (call-with-txn-mutex
		   (lambda () (sleep .5) t) ;; pretend to do work.
		   nil ;; wait forever
		   (lambda () (error "Timeout function should not be called."))))


		(work-dispatch ()
		  (handler-bind ((awaiting-txn-mutex
				  (lambda (condition)
				    (declare (ignore condition))
				    (signal-semaphore *test-dummy-waiting-semaphore*)
				    ;; decline it.
				    nil)))

		    (if switch-b
			(cmctl-pretend-to-work cm-session-context #'report)
		      (pretend-to-work))))

		(report ()
		  (if (null switch-a)
		      (cm-cli-response :display "Finally processed request."
				       (http-connection-stream http-connection))
		    (file-system-note-progress
		     (cm-session-context-file-system-agent cm-session-context)
		     "Finally processed request."
		     nil))
		  t))

    (with-cm-cli-control (http-connection cm-session-context "redirection testing")
      (if (null switch-a)
	  (work-dispatch)
	(with-cm-cli-fsa (cm-session-context)
	  (work-dispatch))))))

(define-cm-cli-command "client_self_test" (http-connection cm-session-context)
  "This command causes the ChangeSafe client to execute a self-test and display the results.
   It is useful if you are having difficulty with your .csf-preferences files or with any
   shell scripts that invoke the ChangeSafe client."
  (declare (ignore http-connection cm-session-context))
  ;; This function is actually never invoked!
  ;; The client intercepts the `client_self_test' command and does its own thing.
  ;; We put a dummy command here for two reasons:
  ;;
  ;;   1.  To reserve the command name so we don't wonder why this magic command
  ;;       cannot be sent by the client.
  ;;
  ;;   2.  So the documentation string prints out when the user types help.
  ;;
  nil)

(define-cm-cli-command "shutdown"
    (http-connection
     cm-session-context
     (port-number :type :keyword :syntax "-port" :required t)
     (pid :type :keyword :syntax "-pid" :required t)  ;; bug 001214-0001
     )
  "Shut down this application server.  The PORT must be the number of the port the server
   is listening on.  The PID is the process-id of the server.  These are given to verify
   that you are shutting down the correct server.  Both values are available via the
   server_version command.

   *FINISH*: must have careful controls on this command,
   and potentially shut down all application servers for a given repository hierarchy."
  ;; validate user?   *TBD*
  ;; *TBD*: optional argument that kills the process too.  Mostly for use in development.
  (let ((stream (http-connection-stream http-connection))
	(do-shutdown nil))
    (with-cm-cli-control (http-connection cm-session-context "server shutdown")
      ;; the with-cm-cli-control is local for the error handling.  Shutdown(:quit)
      ;; does not work if it applies to the whole command

      (let ((port-value (parse-integer port-number :junk-allowed t))
	    (pid-value (parse-integer pid :junk-allowed t)))
	(if (null port-value)
	    (conman-signal-error
	     *cm-returns-error-bogus-arguments*
	     "Can not shutdown the server unless your port number (~a) matches the server's (~d)."
	     port-number *conman-server-log-start-port*))
	(if (or (null pid-value)
		(/= pid-value (excl::getpid))) ;; Franz SPR 22543
	    (unless *within-regression-tests*  ;; to allow hwp tests (50a & test-many-cos) to work
	      (conman-signal-error
	       *cm-returns-error-bogus-arguments*
	       "Can not shutdown the server unless your PID number (~a) matches the server's (~d)."
	       pid (excl::getpid)))) ;; Franz SPR 22543
	(if (or (null *conman-server-log-start-port*)
		(/= port-value *conman-server-log-start-port*))
	    (if *conman-server-log-start-port*
		(conman-signal-error
		 *cm-returns-error-bogus-arguments*
		 "Can not shutdown the server unless your port number (~a) matches the server's (~d)."
		 port-number *conman-server-log-start-port*)
	      (progn
		(setq do-shutdown t) ;; shut down anyway
		;; a nil value can happen when in the regressions tests (conman/test-3a)
		(conman-signal-warning
		 *cm-returns-warning-bogus-arguments*
		 "Shutting down the server even though your port number (~a) does not match the server's (~d)."
		 port-number *conman-server-log-start-port*))
	      )
	  (setq do-shutdown t))))

    (when do-shutdown
      ;; Close server log streams
      (when *cm-cli-server-performance-log-stream*
	(cm-cli-response :display (format nil "Closing performance logging stream to ~a"
					  *cm-cli-server-performance-log-name*)
			 stream)
	(cm-cli-close-performance-log-stream)) ;will suppress errors during close
      ;; ******** If there is another thread which is performing a
      ;; transaction we can't close the error log.
      (when *cm-database-error-log*
	(cm-cli-response :display (format nil "Closing error logging stream to ~a"
					  *cm-database-error-log-name*)
			 stream)
	(cm-cli-close-error-log-stream)) ;will suppress errors during close

      (when *conman-server-log*
	(cm-cli-response :display (format nil "Closing logging stream to ~a"
					  *conman-server-log-name*)
			 stream)
	(conman-server-log-close-stream (cm-session-context-user-name cm-session-context)))
					;will suppress errors during close

      (cm-cli-response :display "ChangeSafe server is history..." stream)
      (cm-cli-response :return *cm-returns-success* stream)

      :quit)))

(define-cm-cli-command "admin_database_create"
    (http-connection cm-session-context
     (dir-spec :type :keyword :syntax #.*cm-cli-keyword-db-dirspec-syntax* :required t)
     (master-name :type :positional :required t))
  "Create a master repository area for organizing families of products.
   DIR-SPEC should name a non-existant directory, accessible to the CM server for creation.

   MASTER-NAME should be a name which identifies the general nature of the master and satellites
   which will be created.  Note that this name is used as an embedded token in repository names,
   and isn't a complete name unto itself.  (E.g. \"jo\")."

  (with-cm-cli-control (http-connection cm-session-context "master repository creation")
    (conman-check-server-operating-mode :read/write)
    (cmctl-create-master cm-session-context
			 (db-name-parse dir-spec :directory? t)
			 master-name)))

(define-cm-cli-command "admin_database_backup"
    (http-connection
     cm-session-context
     (master-repository-name
      :type :keyword
      :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (image-file
      :type :keyword
      :syntax #.*cm-cli-keyword-image-file-syntax* :required t)
     )
  "Backup all databases associated with a given master repository.

   -database names a master repository.
   -image-file names the target host and file to back all the associated
    databases into.  The host must be specified.

   A complete, level 0 backup is done, on the specified host, of all
   databases associated with that master repository.
   "

  (with-cm-cli-control (http-connection cm-session-context
					"database dump")
    (let* ((where (position #\: image-file)))
      (unless where
	(conman-signal-error *cm-returns-error-host-not-specified*
	    "No host was specified in the image-file."
	    ))
      (unless (< (1+ where) (length image-file))
	(conman-signal-error *cm-returns-error-image-file-missing*
	    "The image file was not specified."))
      (cm-session-context-add
       cm-session-context
       :repository-name
       master-repository-name)

      (conman-require-host (subseq image-file 0 where)
			   http-connection
			   cm-session-context
	  (lambda ()

	      (cmctl-database-backup cm-session-context
				   (subseq image-file (1+ where)))
	      )
	  )
    )
  ))


(defun cm-cli-error-logging-param-mutual-exclusion-check (log-to stop-log show-param)
  "Helper routine to verify mutual exclusion constraints on
   certain arguments to admin_database_xxx_log commands.
   Return value: N/A, an error is signalled if there is a problem."
  (unless (= 2 (count nil (list log-to stop-log show-param)))
    (conman-signal-error *cm-returns-error-mutually-exclusive-arguments*
			 "Exactly one of ~A, ~A, or ~A must be specified, they are mutually exclusive."
			 *cm-cli-keyword-logto-syntax*
			 *cm-cli-switch-stoplog-syntax*
			 *cm-cli-switch-showparam-syntax*)))


(define-cm-cli-command "admin_database_error_log"
    (http-connection
     cm-session-context
     (log-to :type :keyword :syntax #.*cm-cli-keyword-logto-syntax*)
     (stop-log :type :switch :syntax #.*cm-cli-switch-stoplog-syntax*)
     (show-param :type :switch :syntax #.*cm-cli-switch-showparam-syntax*)
     )
  "Setup server logging of database error information.

   -SHOWPARAM prints out the current log settings for the server.

   -STOPLOG turns off logging for the server.

   -LOGTO <file> specifies where to write the log.  This must be a server-accessible pathname.
   If the file does not exist, it is created; if it exists, the log is appended.

   -SHOWPARAM, -STOPLOG, and -LOGTO are mutually exclusive arguments.

   Note that this command is incomplete with respect to the command specification, and modification to
   its semantics and specification are still in progress."

  (with-cm-cli-control (http-connection cm-session-context "error logging changes")
    (cm-cli-error-logging-param-mutual-exclusion-check log-to stop-log show-param)
    (cond
     (show-param
      (cm-cli-response :display
		       (if *cm-database-error-log*
			   (format nil "~A ~S" *cm-cli-keyword-logto-syntax*
				   *cm-database-error-log-name*)
			 (format nil "~A" *cm-cli-switch-stoplog-syntax*)
			 )
		       (http-connection-stream http-connection)
		       :escape-it t)
      )
     (t
	 ;; close existing log, if any
	 (when *cm-database-error-log*
	   (let ((condition (cm-cli-close-error-log-stream)))
	     (when condition
	       (conman-signal-warning
		*cm-returns-warning-error-closing-log-stream*
		"A condition was raised while attempting to close the error log stream: ~a"
		condition))))

	 (when log-to
	   ;; attempt to open new log
	   (let ((x (open
		     log-to
		     :direction :output
		     :if-exists :append
		     :if-does-not-exist :create
		     )))
	   (when (open-stream-p x)
	     (setq
		   *cm-database-error-log-name* log-to
		   *cm-database-error-log* x)
	     )
	   ))
      )
     )
    t
   ))

(define-cm-cli-command "admin_server_log"
    (http-connection
     cm-session-context
     (log-to :type :keyword :syntax #.*cm-cli-keyword-logto-syntax*)
     (stop-log :type :switch :syntax #.*cm-cli-switch-stoplog-syntax*)
     (show-param :type :switch :syntax #.*cm-cli-switch-showparam-syntax*)
     )
  "Setup server logging of information.

   -SHOWPARAM prints out the current log settings for the server.

   -STOPLOG turns off logging for the server.

   -LOGTO <file> specifies where to write the log.  This must be a server-accessible pathname.
   If the file does not exist, it is created; if it exists, the log is appended.

   -SHOWPARAM, -STOPLOG, and -LOGTO are mutually exclusive arguments.
   "
  (with-cm-cli-control (http-connection cm-session-context "server logging changes")
    (cm-cli-error-logging-param-mutual-exclusion-check log-to stop-log show-param)
    (if show-param
	(cm-cli-response :display
			 (if *conman-server-log*
			     (format nil "~A ~S" *cm-cli-keyword-logto-syntax*
				     *conman-server-log-name*)
			   (format nil "~A" *cm-cli-switch-stoplog-syntax*))
			 (http-connection-stream http-connection)
			 :escape-it t)
      (conman-server-log-open-stream log-to (cm-session-context-user-name cm-session-context)))
    t))

(define-cm-cli-command "admin_server_statistics"
    (http-connection
     cm-session-context
     (repository-path :type :keyword :syntax "-repository-path")
     (diagnose-class :type :keyword :syntax "-diagnose-class")
     (all-dbs :type :switch :syntax "-all-dbs")
     (deep-counts :type :switch :syntax "-deep-counts")
     (debug-integrity :type :switch :syntax "-debug-integrity")
     (inspect-danglers :type :switch :syntax "-inspect-danglers")
     (summarize-cids-path :type :keyword :syntax "-summarize-cids-path")
     (version-cid-set-cache-report :type :switch :syntax "-version-cid-set-cache-report")
     (performance-log :type :keyword :syntax "-performance-log")
     (modulus :type :keyword :syntax "-modulus")
     ;; (show-graph :type :switch :syntax "-show-graph")
     (secret-arg :type :keyword :secret t :syntax "-secret-arg")
     (secret-arg-password :type :keyword :secret t :syntax "-secret-arg-password")
     )
  "Extract Server Statistics of various sorts.  Generally for CII use only.

   Use -REPOSITORY-PATH to specify which repository to get object count statistics from.
   The three parameters -DIAGNOSE-CLASS, -DEBUG-INTEGRITY, and -INSPECT-DANGLERS go
   with the -REPOSITORY-PATH choice.  If ALL-DBS is set, REPOSITORY-PATH is assumed to
   be the master database and all of the databases in the set will be done.  -DEEP-COUNTS
   causes deeper analysis and object counts to be done.

   Use -SUMMARIZE-CIDS-PATH to get a list of CIDs from the database.  ALL-DBS also applies.

   Use -VERSION-CID-SET-CACHE-REPORT to produce a report of the
   state of the version-cid-set-cache.  -V2 will produce more output.

   Use -PERFORMANCE-LOG (followed by the name of the log file) to get the output of the
   admin_database_performance_log command analyzed.  The log file must be on the server machine.
   The -MODULUS is an integer from 1 to N and controls how much detail is shown.  The
   default value is 50.  -V0 will produce less output.
   "
  ;; -SHOW-GRAPH (T or F) will cause a simple character graph to be
  ;; printed.  The default is F.

  ;; -SECRET-ARG does an eval of its value with the output maybe going back to the client
  ;; -SECRET-ARG-PASSWORD must be given to use -SECRET-ARG

  (labels ((printit (string)
	     (cm-cli-response :display string
			      (http-connection-stream http-connection)
			      :escape-it t)))

    (with-cm-cli-control (http-connection cm-session-context "server statistics")

      (when repository-path
	(if all-dbs
	    (progn
	      (cm-session-context-add cm-session-context
				      :repository-name  repository-path)
	      ;; This should be in cmctl.lsp
	      (cmctl-call-with-master-repository-txn cm-session-context
		  :reason "admin_server_statistics repository-statistics"
		  :txn-mode :read-only
		  :receiver
		  (lambda (master-repository-name master-repository master-catalog)
		    (declare (ignore master-catalog))
		    (repository-statistics master-repository
					      :string-out #'printit
					      :deep-counts deep-counts
					      :diagnose-class diagnose-class
					      :debug-integrity debug-integrity
					      :inspect-danglers inspect-danglers)
		    (cmctl-map-over-every-dbname-for-master
		     (lambda (dbname)
		       (repository-statistics dbname
					      :string-out #'printit
					      :open-mode (conman-repository-open-mode-from-operating-mode)
					      :deep-counts deep-counts
					      :diagnose-class diagnose-class
					      :debug-integrity debug-integrity
					      :inspect-danglers inspect-danglers))
		     (merge-pathnames
		      ;; Discard the host component.
		      (db-name-pathname master-repository-name)
		      (make-pathname :type
				     core:*repository-master-file-type*))
		     master-repository
		     :add-mdb nil))))
	  (repository-statistics repository-path
				 :string-out #'printit
				 :open-mode (conman-repository-open-mode-from-operating-mode)
				 :deep-counts deep-counts
				 :diagnose-class diagnose-class
				 :debug-integrity debug-integrity
				 :inspect-danglers inspect-danglers)))

      (when summarize-cids-path
	(if all-dbs
	    (progn
	      (cm-session-context-add cm-session-context
				      :repository-name  summarize-cids-path)
	      ;; This should be in cmctl.lsp
	      (cmctl-call-with-master-repository-txn cm-session-context
		  :reason "admin_server_statistics summarize-contents"
		  :txn-mode :read-only
		  :receiver
		  (lambda (master-repository-name master-repository master-catalog)
		    (declare (ignore master-catalog))
		    (summarize-contents master-repository
					:string-out #'printit
					:verbose t)
		    (cmctl-map-over-every-dbname-for-master
		     (lambda (dbname)
		       (summarize-contents dbname
					   :string-out #'printit
					   :open-mode (conman-repository-open-mode-from-operating-mode)
					   :verbose t))
		     (merge-pathnames
		      ;; Discard the host component.
		      (db-name-pathname master-repository-name)
		      (make-pathname :type
				     core:*repository-master-file-type*))
		     master-repository
		     :add-mdb nil))))
	  (summarize-contents summarize-cids-path
			      :string-out #'printit
			      :open-mode (conman-repository-open-mode-from-operating-mode)
			      :verbose t)))

      ;; show version-cid-set-cache report
      (when version-cid-set-cache-report
	(call-with-busy-redirection
	    cm-session-context "admin_server_statistics version-cid-set-cache-report" nil
	    (lambda ()
		(utility::cache-mgr-report vm::*with-version-cid-set-cache*
					   :string-out #'printit
					   :verbose (> (cm-session-context-verbosity
							cm-session-context) 1)))))

      ;; analyze a performance log
      (when performance-log
	(call-with-busy-redirection
	    cm-session-context "admin_server_statistics performance-log analyze" nil
	    (lambda ()
		(setq modulus (or (parse-integer modulus :junk-allowed t)
				  *conman-perflog-default-modulus*))
		(when (< modulus 1)
		  (setq modulus *conman-perflog-default-modulus*))
		(perflog-analyze-read-perf-log performance-log
					       :modulus modulus
					       :verbose (> (cm-session-context-verbosity
							    cm-session-context) 0)
					       :string-out #'printit))))

      ;; eval the given string
      (when secret-arg
	(unless
	    (string-equal secret-arg-password
			  "Unauthorized use voids your CII warranty")
	  (conman-signal-error *cm-returns-error-too-many-arguments*
			       "Too many command arguments were specified: ~s"
			       secret-arg))
	(with-input-from-string (in-stream secret-arg)
	  (let* (out-value-list
		 (idx 0)
		 (out-string
		  (with-output-to-string (out-stream )
		    (let ((*standard-output* out-stream)
			  (*error-output* out-stream)
			  (*trace-output* out-stream)
			  ;; the following are deliberately left out of the bindings
			  ;; since they are I/O streams and we have only an output
			  ;; stream to work with.  If your eval string sends output
			  ;; to one of these places, the output will go there (probably
			  ;; the server console log) or you can try doing the binding
			  ;; in your eval string
			  ;; (*debug-io* out-stream)
			  ;; (*terminal-io* out-stream)
			  ;; (*query-io* out-stream)
			  )
		      (let ((code (read in-stream)))
			(setq out-value-list
			      (multiple-value-list
			       (eval code)))
			)))))
	    (printit (format nil "secret output: (~%~a~%)~%" out-string))
	    (dolist (val out-value-list)
	      (incf idx)
	      (printit (format nil "secret value ~d: ~s~%" idx val))))
	  ))

      t ;; return value
      )))

(define-cm-cli-command "admin_server_utilities"
    (http-connection
     cm-session-context
     (alternative-server-uri :type :keyword :syntax "-alternative-server-uri")
     (configure-email-for-check-ins :type :keyword :syntax "-configure-email-for-check-ins")
     (configure-email-smtp-server :type :keyword :syntax "-configure-email-smtp-server")
     (debug-noise-level :type :keyword :syntax "-debug-noise-level")
     (debug-server-info :type :keyword :syntax "-debug-server-info")
     (force-console-output :type :switch :syntax "-force-console-output")
     (referential-integrity :type :keyword :secret t :syntax "-referential-integrity")
     (show-command-times :type :keyword :syntax "-show-command-times")
     (version-cid-set-cache-sanity-check :type :keyword :syntax "-version-cid-set-cache-sanity-check")
     (version-cid-set-cache-size :type :keyword :syntax "-version-cid-set-cache-size")
     (version-cid-set-cache-stickiness :type :keyword
				       :syntax "-version-cid-set-cache-stickiness-reduction-factor")
     (within-regression-tests :type :keyword :secret t :syntax "-within-regression-tests")
     )
  "Server utility functions.  Generally for CII use only.

   Use -ALTERNATIVE_SERVER_URI to add an additional server uri of the form:
       \"http://localhost:7999/\".

   Use -CONFIGURE-EMAIL-FOR-CHECK-INS to set up who should get email about check-ins
   (master_change or cset_close) against which products.

   Use -CONFIGURE-EMAIL-SMTP-SERVER to dynamically set the email server.

   Use -DEBUG-NOISE-LEVEL to cause (or not) various levels of debug info to be printed.

   Use -DEBUG-SERVER-INFO (T or F) to cause info like the port number and the time of day to be
   passed back to the client.

   Use -FORCE-CONSOLE-OUTPUT to flush any pending console output.  Usually only useful for batch tests.

   Use -SHOW-COMMAND-TIMES to cause per command and cummulative times (in milliseconds) to be
   printed at the client for each command.

   Use -VERSION-CID-SET-CACHE-SIZE followed by a positive integer to set a new size for the
   version-cid-set-cache.  -VERSION-CID-SET-CACHE-STICKINESS controls how fast things age in
   the cache.
   "
  (let ((negatives '("F" "False" "N" "Nil" "No" "0" ; zero
		     "Off")))
    (with-cm-cli-control (http-connection cm-session-context "server utilities")

      ;; used by conman/tests.lsp at the least
      (when alternative-server-uri
	(note-alternate-server-uri alternative-server-uri))

      ;; Set up the email-for-check-ins dynamically
      (when configure-email-for-check-ins
	(conman-configure-email-for-check-ins configure-email-for-check-ins))

      ;; Set up the email server dynamically
      (when configure-email-smtp-server
	(configure-email-smtp-server configure-email-smtp-server))

      ;; turn *debug-noise-level* on and off
      (when debug-noise-level
	(setq *debug-noise-level* (parse-integer debug-noise-level :junk-allowed t)))

      ;; show that this server is servicing which commands
      (when debug-server-info
	(setq *cm-cli-debug-server-info*
	      (not (member debug-server-info negatives :test #'string-equal))))

      ;; force console output
      (when force-console-output
	(finish-output *standard-output*))

      ;; Set Referential Integrity Flag, can only be done before openning DB files
      (when referential-integrity
	(setq *repository-run-with-allegrostore-referential-integrity*
	      (not (member referential-integrity negatives :test #'string-equal)))
	(repository-set-referential-integrity))

      ;; start/stop timing commands
      (when show-command-times
	(setq *cm-cli-command-times-show*
	      (not (member show-command-times negatives :test #'string-equal)))
	(setq *cm-cli-command-times-so-far* 0.0))

      ;; start/stop version-cid-set-cache sanity checks
      (when version-cid-set-cache-sanity-check
	(setq vm::*version-cid-set-cache-sanity-check*
	      (not (member show-command-times negatives :test #'string-equal))))

      ;; change the version-cid-set-cache size
      (when (or version-cid-set-cache-size
		version-cid-set-cache-stickiness)
	(setq version-cid-set-cache-size (parse-integer version-cid-set-cache-size :junk-allowed t))
	(setq version-cid-set-cache-stickiness
	      (max 5 (or (parse-integer version-cid-set-cache-stickiness :junk-allowed t)
			 0))) ;; really 5

	(call-with-busy-redirection
	 cm-session-context "admin_server_utilities version-cid-set-cache-size" nil
	 (lambda ()
	     (vm::with-version-start-new-cid-set-cache
		 version-cid-set-cache-size
	       version-cid-set-cache-stickiness))))

       ;; set up server for regression tests (reduce variability in the output mostly)
      (when within-regression-tests
	(if (member within-regression-tests negatives :test #'string-equal)
	    (progn
	      (unless *cm-cli-server-performance-log-stream*
		(setq *cm-cli-doing-performance-run* nil)
		(setq *with-version-performance-tests* nil))
	      (setq *within-regression-tests* nil)
	      (setq *within-regression-tests-hwp-servers* nil)
	      (setq *busy-timeout* +cmctl-busy-timeout-default+)
	      (setq *cmctl-dot-conman-file-name* +cmctl-dot-conman-file-name-default+)
	      )
	  (progn
	    (when (string-equal within-regression-tests "PerformanceTest")
	      (setq *cm-cli-doing-performance-run* t)
	      (setq *with-version-performance-tests* t))
	    (setq *within-regression-tests* t)
	    (setq *within-regression-tests-hwp-servers* t)
	    (setq *busy-timeout* 1) ;; speed up regressions
	    (setq *cmctl-dot-conman-file-name* (make-logical-pathname
						:name +conman-test-dot-conman-file-name+))
	    )))

      t
      )))

(define-cm-cli-command "admin_database_upgrade"
    (http-connection
     cm-session-context
     (password :type :keyword :syntax "-upgrade-password" :required t)
     (repository-path :type :keyword :syntax "-repository-path" :required t)
     (class-block-count :type :keyword :syntax "-class-block-count")
     )
  "Upgrade the Database Schema to match the executable Product Schema.

   Use -REPOSITORY-PATH to specify which master repository to upgrade.  It is expected
   that the database file is otherwise not in use.  This is a READ/WRITE operation.

   Because of the sensitive nature of this command, the -UPGRADE-PASSWORD must be
   given.

   CLASS-BLOCK-COUNT controls how much memory is used at any one time to upgrade
   inidividual objects.  The default value is 10000.  The larger the number, the
   faster the upgrade will proceed.  The smaller the value, the less memory is
   used.
   "
  (with-cm-cli-control (http-connection cm-session-context "database schema upgrade")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name  repository-path)

    (unless (string-equal password "ms666")
      (error "Incorrect password to admin_database_upgrade"))

    ;; set up defaults for parameters

    (setq class-block-count (or (parse-integer class-block-count :junk-allowed t)
				*repository-upgrade-schema-class-block-count*))

    (labels ((printit (string)
	       (cm-cli-database-error-log-string (and cm-session-context
						      (cm-session-context-user-name cm-session-context))
						 "ADMIN_DATABASE_UPGRADE" string)
	       (cm-cli-response :display string
				(http-connection-stream http-connection)
				:escape-it t)))

      ;; Note that upgrade funcs are ONLY allowed to work on a single file at a time
      ;; and we are responsible for starting a transaction

      (call-with-busy-redirection
       cm-session-context "database schema upgrade" nil
       (lambda ()
	   (repository-ensure-db-files-closed :error-if-open t) ;; SPR 22996 - no open DBs when we upgrade

	   ;; do the master file first so we can then get the info about the other DBs
	   ;; this must be done because we can only do these one at a time and we need
	   ;; to upgrade the master DB in order to read it in the normal fashion.
	   (printit (format nil "~%Upgrading ~a~%" repository-path))
	   (repository-upgrade-schema repository-path
				     :string-out #'printit
				     :master-db t
				     :conman-specific-upgrade-class-9-14 'subsystem
				     :conman-specific-upgrade-func-9-14
				     (lambda (repository master-db subsystem-count)
					 (assert master-db) ;; upgrade only applies if it is the master-db
					 ;; upgrade schema-9-14 eliminates subsystem-satellite-cset-dids mirroring
					 (subsystem-upgrade-subsystem-satellite-cset-dids-to-9-14
					  repository subsystem-count class-block-count #'printit))
				     :conman-specific-upgrade-class-9-19 'master-catalog
				     :conman-specific-upgrade-func-9-19
				     #'master-catalog-schema-upgrade-to-9-19
				     :verbose (> (cm-session-context-verbosity cm-session-context) 0)
				     :class-block-count class-block-count)
	   (repository-ensure-db-files-closed :error-if-open t) ;; SPR 22996 - no open DBs when we upgrade

	   (let ((dblist nil)) ;; SPR 22996 - no open DBs when we upgrade
	     ;; loop over the entire set of DBs (remembering to drop the master)
	      (cmctl-call-with-master-repository-txn cm-session-context
		  :reason "database schema upgrade"
		  :txn-mode :read-only
		  :receiver
		  (lambda (master-repository-name master-repository master-catalog)
		    (declare (ignore master-catalog))
		    (cmctl-map-over-every-dbname-for-master
		     (lambda (dbname) (push  dbname dblist))
		     master-repository-name master-repository
		     :add-mdb nil)))

	     ;; make sure that the DB cache is empty, Franz 22996 -- no open DBs when we upgrade
	     (repository-ensure-db-files-closed) ;; master file will be open so close it

	     ;; upgrade the rest of the DBs
	     (dolist (dbname dblist)
	       (printit (format nil "~%Upgrading ~a~%" dbname))
	       (repository-upgrade-schema dbname
					 :string-out #'printit
					 :master-db nil
					 ;; no upgrade func since it is not the master-db
					 :verbose (> (cm-session-context-verbosity cm-session-context) 0)
					 :class-block-count class-block-count)
	       (repository-ensure-db-files-closed :error-if-open t) ;; SPR 22996 no open DBs when we upgrade
	       )))
       ))

    t))

(define-cm-cli-command "admin_database_performance_log"
    (http-connection
     cm-session-context
     (log-to :type :keyword :syntax #.*cm-cli-keyword-logto-syntax*)
     (log-comment :type :keyword :syntax "-log-comment")
     (stop-log :type :switch :syntax #.*cm-cli-switch-stoplog-syntax*)
     (show-param :type :switch :syntax #.*cm-cli-switch-showparam-syntax*)
     )

  "Setup server logging of performance data.

   -SHOWPARAM prints out the current log settings for the server.

   -STOPLOG turns off logging for the server.

   -LOGTO <file> specifies where to write the log.  This must be a server-accessible pathname.
   If the file does not exist, it is created; if it exists, the log is appended.  If starting
   a log, the -LOG-COMMENT will be added to the output stream.

   -SHOWPARAM, -STOPLOG, and -LOGTO are mutually exclusive arguments.

   Note that this command is incomplete with respect to the command specification, and modification to
   its semantics and specification are still in progress."

  (with-cm-cli-control (http-connection cm-session-context "performance logging changes")
    (cm-cli-error-logging-param-mutual-exclusion-check log-to stop-log show-param)
    (cond
     (show-param
      ;; Print logging status
      (cm-cli-response :display
		       (if *cm-cli-server-performance-log-name*
			   (format nil "~A ~S" *cm-cli-keyword-logto-syntax*
				   *cm-cli-server-performance-log-name*)
			 *cm-cli-switch-stoplog-syntax*)
		       (http-connection-stream http-connection)
		       :escape-it t))
     (stop-log
      ;; Close existing log, if any.
      (if *cm-cli-server-performance-log-stream*
	  (let ((condition (cm-cli-close-performance-log-stream)))
	    (setq *cm-cli-doing-performance-run* nil)
	    (when condition
	      (conman-signal-warning
	       *cm-returns-warning-error-closing-log-stream*
	       "A condition was raised while attempting to close the performance log stream: ~a"
	       condition)))
	(conman-signal-warning *cm-returns-warning-no-state-change* "Performance logging is not active.")))

     (log-to
      ;; Start or redirect performance data destination
      ;; Hmmm: here I leave the old stream open in case I fail to open the new stream,
      ;; and close it after opening the new stream. Not clear that this is worthwhile.
      (let ((x (open log-to :direction :output :if-exists :append :if-does-not-exist :create)))
	(unwind-protect			;ensure we set the special variables to new stream
	    (when *cm-cli-server-performance-log-stream*
	      ;; Close the old stream now that we've opened the new stream
	      (let ((condition (cm-cli-close-performance-log-stream)))
		(when condition
		  (conman-signal-warning
		   *cm-returns-warning-error-closing-log-stream*
		   "A condition was raised while attempting to close the performance log stream: ~a"
		   condition)))
	      (conman-signal-warning *cm-returns-warning-log-file-closed*
				     "Performance log file ~a closed, new log file opened."
				     *cm-cli-server-performance-log-name*))
	  "restoring performance log special variables."
	  (setq *cm-cli-server-performance-log-name* log-to
		*cm-cli-server-performance-log-stream* x))

	(setq *cm-cli-doing-performance-run* t)

	;; Write header info to the stream
	(format *cm-cli-server-performance-log-stream*
		"~%~%~a~a by ~a~%~%"
		*conman-perflog-header-starts*
		(time-string) (sys:user-name))
	(format *cm-cli-server-performance-log-stream*
		"~a~s~%" *conman-perflog-header-comment-prefix*	log-comment)
	(format *cm-cli-server-performance-log-stream*
		"~aSoftware: ~d.~d, Schema: ~d.~d~%"
		*conman-perflog-header-versions-prefix*
		user:*major-software-version* user:*minor-software-version*
		user:*major-schema-version*   user:*minor-schema-version*)
	(format *cm-cli-server-performance-log-stream*
		"~aMutli-threaded: ~a, Memory-Footprint: ~a~%~%"
		*conman-perflog-header-switches-prefix*
		web::*http-server-default-multithreaded*
		utility::*changesafe-memory-footprint*)
	(conman-server-log-start-info *cm-cli-server-performance-log-stream*
				      "Start Performance Log"
				      (sys:user-name))
	(format *cm-cli-server-performance-log-stream*
		"~%~a~s~%~%" *conman-perflog-header-comment-prefix* log-comment)
	(format *cm-cli-server-performance-log-stream*
		"~%~%~a~%" *conman-perflog-header-ends*)
	))
     (t
      ;; They didn't specify any of the parameters
      (conman-signal-warning *cm-returns-warning-missing-arguments-no-op*
			     "No arguments were specified, no actions were taken.")))


    ;; Instruct the command dispatcher that this is NOT a "server exit" return code.
    ;; Failure to return non-nil will also cause with-cm-cli-control to say "an unknown error occured."
    t))

(define-cm-cli-command *cm-cli-command-product-create*
    (http-connection cm-session-context
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     ;; One of these switches must be present if a base product is specified
     (copy-none :type :switch  :syntax #.*cm-cli-switch-base-pc-copy-none-syntax*)
     (copy-all  :type :switch  :syntax #.*cm-cli-switch-base-pc-copy-all-syntax*)
     (copy-file :type :keyword :syntax #.*cm-cli-keyword-base-pc-copy-file-syntax*)
     ;; N.B. The following parameters have an equivalent set in cm_subsys_create
     (base-product-name :type :keyword :syntax #.*cm-cli-keyword-base-pc-name-syntax*)
     (label-name :type :keyword :syntax #.*cm-cli-keyword-label-syntax*)
     (time-specification :type :keyword :syntax #.*cm-cli-keyword-time-syntax*)
     (release-name :type :keyword :syntax #.*cm-cli-keyword-release-name-syntax*)
     (description :type :keyword :syntax #.*cm-cli-keyword-description-syntax*)
     ;; product-create specific
     (product-name :type :positional :required t)
     )
  "Create a product.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   PRODUCT-NAME is the name to be assigned to the newly created product.
   An error is signalled if a product with this name currently exists in the master repository.

   BASE-PRODUCT-NAME, if specified, names another product that
   is considered the ancestor of the new product to be
   created. It is often used with modifying views via label, time, and release
   specifications.  Most products are created based on already pre-existing
   products, it is rare that products are created with no base product.

   When a base product is used, the degree of sharing of base product
   subsystems is dictated by one of the COPY-XXXX switches or keywords.  One of COPY-ALL,
   COPY-NONE, or COPY-FILE must be specified if BASE-PRODUCT-NAME is specified.

   COPY-NONE is a switch that says to copy none of the subsystems to the new
   product, in which case the user presumably adds subsystems to the product via
   SUBSYS_CREATE or related commands.  It is valid only if BASE-PRODUCT-NAME is specified.

   COPY-ALL is a switch that says to copy all of the subsystems to the new product.  It is valid only
   if BASE-PRODUCT-NAME is specified.

   COPY-FILE is a keyword followed by a file specification whose contents lists those subsystems to be
   added to the product.

   TIME-SPECIFICATION specifies the time of the last transaction which should be used
   off a product branch to create the new product.  It must be accompanied by
   a BASE-PRODUCT-NAME.

   Time should be specified in the following near-ISO format: yyyy-mm-dd[-hh:mm[:ss]].
   Hours are specified as 24 hour time.  The seconds, or the entire time of day, are
   optional.

   RELEASE-NAME, if specified, names the release to use for the base to get the
   initial product contents.

   DESCRIPTION, if specified, is a string which is attached to the created product."
  (with-cm-cli-control (http-connection cm-session-context "product creation")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :pc-name product-name
			    :label-name label-name
			    :time-spec time-specification
			    :release-name release-name)
    (with-cm-cli-fsa (cm-session-context)
      (cmctl-create-product-configuration
       cm-session-context
       base-product-name
       copy-none copy-all copy-file
       description))))

(define-cm-cli-command "class_create"
    (http-connection
     cm-session-context
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     ;; "-PATH"?  "-RELPATH"?
     (subdirectory-name :type :keyword :syntax #.*cm-cli-keyword-class-dirspec-syntax* :required t)
     (class-name :type :positional :required t)
     (description :type :keyword :syntax #.*cm-cli-keyword-description-syntax*))
  "Create a ChangeSafe class and an associated server repository which will contain class files.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   SUBDIRECTORY-NAME names a directory which is relative to master reference directory and
   client workspace roots, and which is used as the root for elements contained in a given class.
   It should be in client-oriented syntax.

   CLASS-NAME names the class to be created.  Note that the repository which
   is created by the server bears no resemblance to the class name, and you should never need
   to specify the name of the resulting satellite repository, always use the master.

   DESCRIPTION, if specified, is a string which is attached to the created class."
  ;; Print it so we can see it work.  Printing it into regression output wouldn't work
  ;; when multiple people run regressions however.
  (with-cm-cli-control (http-connection cm-session-context "class creation")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :class-name class-name)
    (cmctl-class-create cm-session-context
			(cm-cli-parse-relative-directory-in-client-syntax
			 subdirectory-name
			 cm-session-context)
			description)))

(define-cm-cli-command "subsys_create"
    (http-connection
     cm-session-context
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (product-name :type :keyword :syntax #.*cm-cli-keyword-subscriber-pc-name-syntax* :required t)
     (class-name :type :keyword :syntax #.*cm-cli-keyword-class-name-syntax* :required t)
     ;; N.B. The following parameters have an equivalent set in cm_product_create
     (base-product-name :type :keyword :syntax #.*cm-cli-keyword-base-pc-name-syntax*)
     (base-subsystem-name :type :keyword :syntax #.*cm-cli-keyword-base-subsystem-name-syntax*)
     (label-name :type :keyword :syntax #.*cm-cli-keyword-label-syntax*)
     (time-specification :type :keyword :syntax #.*cm-cli-keyword-time-syntax*)
     (release-name :type :keyword :syntax #.*cm-cli-keyword-release-name-syntax*)
     (description :type :keyword :syntax #.*cm-cli-keyword-description-syntax*)
     ;; subsys_create specific
     (subdirectory :type :keyword :syntax #.*cm-cli-keyword-subsys-dirspec-syntax*)
     (subsystem-name :type :positional :required nil))

  "Create a subsystem.  A subsystem binds a (possibly shared) class view to a product.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   PRODUCT-NAME names the product for which a subsystem is created.
   The subsystem which results from this command is immediately bound to the product.

   CLASS-NAME names the class which is to be represented by the created subsystem.
   An error is signalled if CLASS-NAME names a class for which a subsystem already exists in the product
   configuration, or if the class does not exist.

   SUBSYSTEM-NAME names the created subsystem.  An error is signalled if SUBSYSTEM-NAME
   names a subsystem which has (currently, or ever in the past) the same name.

   BASE-SUBSYSTEM-NAME is the name of a subsystem from which the new subsystem is to inherit.

   TIME-SPECIFICATION, if given, specifies the time at which to get the contents
   of the BASE-SUBSYSTEM.  The time is given in the normal iso form of
   yyyy-mm-ddThh:mm[:ss], possibly with a time-zone appended.

   RELEASE-NAME, if given, names the release to inherit from.

   DESCRIPTION, if specified, is a string which is attached to the created subsystem.

   Note that currently there is no way to specify what release/branch of
   the product the new subsystem should be added to."

  (with-cm-cli-control (http-connection cm-session-context "subsys creation")
    (conman-check-server-operating-mode :read/write)
    (when (and base-product-name base-subsystem-name)
      (conman-signal-error
       *cm-returns-error-bogus-arguments*
       "You may only specify one of ~a or ~a."
       *cm-cli-keyword-base-subsystem-name-syntax*
       *cm-cli-keyword-base-pc-name-syntax*))
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :pc-name product-name
			    :class-name class-name
			    :subsystem-name (or subsystem-name
						(conman-synthesize-new-subsystem-name
						 class-name product-name))
			    :label-name label-name
			    :time-spec time-specification
			    :release-name release-name)
    (with-cm-cli-fsa (cm-session-context)
      ;; ***** Since there is currently no way for the user to specify
      ;; what branch of PRODUCT-NAME the new subsystem should be added
      ;; to, CMCTL-SUBSYS-CREATE will assume that it is the "Main"
      ;; branch that should be affected.  We can fix that once we
      ;; figure out how the user will tell us what relase the want to
      ;; affect and how we should pass that value in to
      ;; CMCTL-SUBSYS-CREATE.
      (cmctl-subsys-create cm-session-context
			   (lisp-file-system-create)
			   base-product-name
			   base-subsystem-name
			   description
			   (when subdirectory
			     (cm-cli-parse-relative-directory-in-client-syntax
			      subdirectory cm-session-context))))))

(define-cm-cli-command *cm-cli-command-subsys-subscriber-list*
    (http-connection
     cm-session-context
     (master-repository-name :type :keyword
			     :syntax #.*cm-cli-keyword-master-syntax*
			     :required t)
     ;; Can specify either SUBSYSTEM or BASE-PRODUCT and CLASS
     (subsystem-name :type :keyword
		     :syntax #.*cm-cli-keyword-subsystem-name-syntax*
		     :required nil)
     (base-product-name :type :keyword
			:syntax #.*cm-cli-keyword-base-pc-name-syntax*
			:required nil)
     (class-name :type :keyword
		 :syntax #.*cm-cli-keyword-class-name-syntax*
		 :required nil)
     ;; Exactly one of these
     (mode-write     :type :switch
		     :syntax #.*cm-cli-switch-subsys-subscriber-write-syntax*
		     :default nil)
     (mode-no-write  :type :switch
		     :syntax #.*cm-cli-switch-subsys-subscriber-readonly-syntax*
		     :default nil)
     (mode-delete    :type :switch
		     :syntax #.*cm-cli-switch-subsys-subscriber-delete-syntax*
		     :default nil)
     ;; optional release name
     (release-name :type :keyword
		   :syntax #.*cm-cli-keyword-release-name-syntax*
		   :default #.+pc-main-branch-name+)
     ;; product
     (product-name :type :positional :required t)
     )
  "Add or delete a product from a subsystem's subscriber list or change
   the product's write permission for that subsystem.

   A subsystem may have any number of subscribers and any of them may have
   write permission.

   If -write or -nowrite is specified, the product is added to the subscriber list.
   If it is already there, its permission is set to the specified value.

   If the product is added to the list, it is automatically removed from the
   list of any other subsys in the same class.  A product can only use one
   member of a class.

   A product is not allowed to have two subsystems sharing the same
   subdirectory path.

   By default, the Main branch of the product is the one affected.  Once
   can specify a different release branch with the -release argument.

   If -delete is specified, the product is removed from this subsystem's subscriber list."
  ;; *** NOTE that there is currently no way to specify the
  ;; branch/release of the base product.
  (with-cm-cli-control (http-connection cm-session-context "subsys change subscribers list")
    (conman-check-server-operating-mode :read/write)
    (unless (only-one-thing mode-write mode-no-write mode-delete)
      (conman-signal-error
       *cm-returns-error-bogus-arguments*
       "You must specify exactly one of ~@{~a~^, ~}."
       *cm-cli-switch-subsys-subscriber-write-syntax*
       *cm-cli-switch-subsys-subscriber-readonly-syntax*
       *cm-cli-switch-subsys-subscriber-delete-syntax*))
    (cond (subsystem-name
	   (when (or base-product-name class-name)
	     (conman-signal-error
	      *cm-returns-error-bogus-arguments*
	      "If you specify SUBSYSTEM-NAME you may not specify CLASS-NAME or BASE-PRODUCT-NAME.")))
	  (class-name
	   (cond (mode-delete
		  ;; If we're dropping the subsystem then the class
		  ;; name should be sufficient.
		  (when base-product-name
		    (conman-signal-error
		     *cm-returns-error-bogus-arguments*
		     "When removing a subsystem, do not specify a base product")))
		 (base-product-name)
		 (t (conman-signal-error
		     *cm-returns-error-bogus-arguments*
		     "If you specify one of CLASS-NAME and BASE-PRODUCT-NAME you must specify them both ~
                      unless you are doing ~a."
		     *cm-cli-switch-subsys-subscriber-delete-syntax*)))))
    (cm-session-context-add
     cm-session-context
     :repository-name master-repository-name
     :pc-name product-name
     :release-name release-name)
    (with-cm-cli-fsa (cm-session-context)
      (cmctl-subsys-users-list cm-session-context
			       product-name release-name
			       ;; usage mode
			       (cond (mode-write :write)
				     (mode-no-write :read)
				     (mode-delete nil))
			       ;; what are we using:
			       (if subsystem-name
				   subsystem-name
				 (list base-product-name class-name))))))

(define-cm-cli-command "subsys_inherit_from"
    (http-connection
     cm-session-context
     (master-repository-name :type :keyword
			     :syntax #.*cm-cli-keyword-master-syntax*
			     :required t)
     ;; Either a subsystem or product must me specified.  If a product
     ;; is specified, the class can be inferred from the inheriting
     ;; subsystem.
     (from-subsystem-name :type :keyword
			  :syntax #.*cm-cli-keyword-subsys-inherit-from-subsys-syntax*
			  ;; For now it's required since we don't yet let
			  ;; the user specify it by product.
			  :required t
			  :default nil)
     (from-product-name :type :keyword
			:syntax #.*cm-cli-keyword-subsys-inherit-from-product-syntax*
			:default nil)
     ;; Exactly one mode must be specified
     (mode-all	   :type :switch
		   :syntax #.*cm-cli-switch-subsys-inherit-all-changes-syntax*
		   :default nil)
     (mode-select  :type :switch
		   :syntax #.*cm-cli-switch-subsys-inherit-select-changes-syntax*
		   :default nil)
     (mode-delete  :type :switch
		   :syntax #.*cm-cli-switch-subsys-dont-inherit-syntax*
		   :default nil)
     ;; either a subsystem or a product and class must be specified
     (subsystem-or-product-name :type :positional :required t)
     (class-name :type :positional :required nil))
  "For the given subsystem, change the inherit_from list.

   The inherit_from list determines how csets are pulled into a
   product when the cm port command is used.

   The subsys or product + class specifies the subsystem that is
   receiving csets.  If only subsystem-or-product-name is specified, then
   it is assumed to be a subsystem name.  If class-name is also specified then
   subsystem-or-product-name is taken as a product name, which in association
   with class-name identifies a subsystem.

   The from_subsystem or from_product names the subsystem that is supplying
   csets. It is not necessary to specify a \"from_class\" because the class
   must be the same for both the supplier and receiver.

   The switch \"-delete\" means take the from_subsystem off of the inherit
   list. Don't inherit from this one any more. The switch \"-all\" means
   I want to inherit all csets. The switch \"-select\" means I want to
   look at csets and decide which to take.  Exactly one of the three switches
   should be specified.

   A subsys may inherit from multiple sources, or from none. However, the most
   usual case is to inherit from a single subsys."
  (with-cm-cli-control (http-connection cm-session-context "subsys change inheritance")
    (conman-check-server-operating-mode :read/write)
    (unless (only-one-thing mode-all mode-select mode-delete)
      (conman-signal-error
       *cm-returns-error-bogus-arguments*
       "You must specify exactly one of ~@{~a~^, ~}."
       *cm-cli-switch-subsys-inherit-all-changes-syntax*
       *cm-cli-switch-subsys-inherit-select-changes-syntax*
       *cm-cli-switch-subsys-dont-inherit-syntax*))
    (unless (only-one-thing from-subsystem-name from-product-name)
      (conman-signal-error
       *cm-returns-error-bogus-arguments*
       "You must specify exactly one of ~@{~a~^, ~}."
       *cm-cli-keyword-subsys-inherit-from-subsys-syntax*
       *cm-cli-keyword-subsys-inherit-from-product-syntax*))
    (cm-session-context-add
     cm-session-context :repository-name master-repository-name)
    (if class-name
	(progn
	  (cm-session-context-add cm-session-context :pc-name subsystem-or-product-name)
	  (cm-session-context-add cm-session-context :class-name class-name))
      (cm-session-context-add cm-session-context
			      :subsystem-name subsystem-or-product-name))
    (with-cm-cli-fsa (cm-session-context)
      (cmctl-subsys-inherit-from cm-session-context
				 ;; The inheriting from subsystem is
				 ;; specified by a subsystem name or by
				 ;; a product and class name.
				 subsystem-or-product-name
				 class-name
				 ;; The inherited from subsystem is
				 ;; identified by either a subsystem
				 ;; name or a product name
				 (cond (from-subsystem-name :subsystem)
				       (from-product-name :product))
				 (or from-subsystem-name from-product-name)
				 ;; The inheritance mode
				 (cond (mode-all :all)
				       (mode-select :select)
				       (mode-delete nil))))))

(define-cm-cli-command "ws_sync"
    (http-connection
     cm-session-context
     (server-relative :type :switch :syntax #. *cm-cli-switch-server-relative*)
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required t)
     (args :type :rest))
  "Resynchronize workspace area on client disk with workspace object in master repository.
   This command is not a user command; rather it is used as an intermediate command for
   load balancing."
  (declare (ignore args))
  (with-cm-cli-control (http-connection cm-session-context "workspace sync")
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id)
    (prefer-read-only-server http-connection cm-session-context
     (lambda ()
	 (guarantee-cd-in-workspace cm-session-context server-relative)
	 (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
	   (cmctl-ws-sync cm-session-context server-relative))
	 (cm-cli-response :continue-redirect "ws_sync_commit" (http-connection-stream http-connection))
	 :to-be-continued))))

(define-cm-cli-command "ws_sync_commit"
    (http-connection
     cm-session-context
     (server-relative :type :switch :syntax #. *cm-cli-switch-server-relative*)
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required t)
     (args :type :rest))
  "Mark master repository workspace object to indicate that the workspace area on the client disk
   is synchronized with it.  This is not a user command."
  (declare (ignore args))
  (with-cm-cli-control (http-connection cm-session-context "workspace modification commit")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id)
    (guarantee-cd-in-workspace cm-session-context server-relative)
    (cmctl-ws-sync-commit cm-session-context)))

(define-cm-cli-command "ws_sync_abort"
    (http-connection
     cm-session-context
     (server-relative :type :switch :syntax #. *cm-cli-switch-server-relative*)
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required t))
  "Mark master repository workspace object to indicate that the workspace area on the client disk
   cannot be synchronized with it.  This user command is used in extreme situations where
   no workspace commands can proceed because ws_sync is not functioning."
  (with-cm-cli-control (http-connection cm-session-context "workspace modification abort")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id)
    (guarantee-cd-in-workspace cm-session-context server-relative)
    (cmctl-ws-sync-abort cm-session-context)))

(define-cm-cli-command "ws_create"
    (http-connection
     cm-session-context
     (ws-id :type :keyword
	    :syntax #.*cm-cli-keyword-ws-id-syntax*
	    :required nil
	    :secret t)
     (server-relative :type :switch :syntax #. *cm-cli-switch-server-relative*)
     (xeno :type :switch :syntax #.*cm-cli-switch-xeno*)
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (product-name :type :keyword :syntax #.*cm-cli-keyword-pc-name-syntax* :required t)
     (directory-spec :type :positional :required t)
     (product-directory :type :switch :syntax #.*cm-cli-switch-pc-product-directory-syntax*)
     ;; N.B. The following parameters have an equivalent set in many commands.
     (label-name :type :keyword :syntax #.*cm-cli-keyword-label-syntax*)
     (time-specification :type :keyword :syntax #.*cm-cli-keyword-time-syntax*)
     (release-name :type :keyword :syntax #.*cm-cli-keyword-release-name-syntax*)
     (description :type :keyword :syntax #.*cm-cli-keyword-description-syntax*))

  "Create a workspace area on the client disk, and a workspace object representation under control
   of the master repository. (Repository-side workspaces representations do not reside in the
   master repository, but in a 'semipersistent' database used for managing short-lived but persistent
   information).

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   PRODUCT-NAME names the product which is to be managed in
   the user disk area.  The client disk space will be populated with files from every
   subsystem in the product.

   DIRECTORY-SPEC is a directory pathname specification which names the area of the file
   system where the workspace will be created.  The directory must not exist when the
   command is executed.

   TIME-SPECIFICATION specifies a point in time that is to be reflected in the resulting
   workspace view of the product branch.

   Time should be specified in the following ISO format, yyyy-mm-dd[Thh:mm[:ss]].
   Hours are specified as 24 hour time.  The seconds, or the entire time of day, are
   optional. A time-zone may also be appended.


   RELEASE-NAME, if given, specifies that the workspace is to be based on the
   given release, rather than the main branch.

   DESCRIPTION, if specified, is a string which is attached to the created workspace representation
   in the master semipersistent repository.  It is displayed in various workspace reports.

   PRODUCT-DIRECTORY flags the workspace as the product reference workspace.
   *FINISH* enforcement of read-only semantics for product-directory is not yet implemented."

  #|
   If SERVER-RELATIVE is specified, then DIRECTORY-SPEC must name an absolute directory pathname.
   This is to prevent accidental use of server disk areas.

   If SERVER-RELATIVE is not specified, then DIRECTORY-SPEC may name either an absolute
   directory pathname or a relative directory pathname.  Relative directories are
   interpreted relative to the user's current working directory.

   *FINISH* ???: allow administrators to optionally require that disk areas are under the
   user's HOME directory, and enforce that constraint if selected.

|#
  (with-cm-cli-control (http-connection cm-session-context "workspace creation")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :ws-id ws-id
			    :repository-name master-repository-name
			    :pc-name product-name
			    :label-name label-name
			    :time-spec time-specification
			    :release-name release-name)
    (guarantee-cd-not-in-workspace cm-session-context server-relative)
    (let (workspace-directory)
      (when (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
	      (setq workspace-directory (cm-cli-parse-absolute-directory-specifier
					 cm-session-context
					 directory-spec
					 server-relative))
	      (let ((home-directory (cm-session-context-user-home-directory cm-session-context)))
		;; If the workspace being created is not
		;; under the user's home directory, they must specify the additional control
		;; switch `xeno'
		(unless (or server-relative
			    xeno
			    (and home-directory
				 (funcall
				  (platform-pathname-match-p (cm-session-context-client-platform cm-session-context))
				  workspace-directory (make-wild-pathname home-directory))))
		  (conman-error-workspace-not-under-homedir workspace-directory home-directory))
		(cmctl-ws-create cm-session-context workspace-directory description
				 (not (null product-directory)))))
	;; Now, cd to the workspace directory and sync.
	(cm-cli-response :change-directory (pathname->platform-namestring
					    workspace-directory
					    (cm-session-context-client-platform cm-session-context))
			 (http-connection-stream http-connection))
	(cm-cli-response :continue-redirect "ws_sync" (http-connection-stream http-connection))
	:to-be-continued))))

(define-cm-cli-command "ws_set"
    (http-connection
     cm-session-context
     (server-relative :type :switch :syntax #. *cm-cli-switch-server-relative*)
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (product-name :type :keyword :syntax #.*cm-cli-keyword-pc-name-syntax* :required nil)
     (ws-id :type :keyword
	    :syntax #.*cm-cli-keyword-ws-id-syntax*
	    :required t
	    :secret t)
     (force :type :switch :syntax #.*cm-cli-switch-force-syntax* :required nil)
     ;; N.B. The following parameters have an equivalent set in many commands.
     (label-name :type :keyword :syntax #.*cm-cli-keyword-label-syntax*)
     (time-specification :type :keyword :syntax #.*cm-cli-keyword-time-syntax*)
     (release-name :type :keyword :syntax #.*cm-cli-keyword-release-name-syntax*)
     (description :type :keyword :syntax #.*cm-cli-keyword-description-syntax*))

  "Reset a workspace area on the client disk, and the workspace object representation under control
   of the master repository. (Repository-side workspaces representations do not reside in the
   master repository, but in a 'semipersistent' database used for managing short-lived but persistent
   information).

   If -force set, set workspace even though there are differences with workspace baseline.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   PRODUCT-NAME names the product which is to be managed in
   the user disk area.  The client disk space will be populated with files from every
   subsystem in the product.  If the product is different from what originally used to
   create or populate the workspace, the original product's files will be removed first.

   TIME-SPECIFICATION specifies a point in time that is to be reflected in the resulting
   workspace view of the product branch.

   Time should be specified in the following near-ISO format, yyyy-mm-dd[-hh:mm[:ss]].
   Hours are specified as 24 hour time.  The seconds, or the entire time of day, are
   optional. A time-zone specification may be appended, if desired.

   RELEASE-NAME, if given, specifies what release to use to set the workspace from,
   rather than the default main branch.

   DESCRIPTION, if specified, is a string which is attached to the created workspace representation
   in the master semipersistent repository.   It is displayed in various workspace reports."
  ;; *FINISH* ????: allow administrators to optionally require that disk areas are under the
  ;;  user's HOME directory, and enforce that constraint if selected.
  (with-cm-cli-control (http-connection cm-session-context "workspace set")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id
			    :pc-name product-name
			    :label-name label-name
			    :time-spec time-specification
			    :release-name release-name)
    (guarantee-cd-in-workspace cm-session-context server-relative)
;    (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
;      (cmctl-ws-set cm-session-context
;                   (cm-session-context-workspace-root cm-session-context)
;                   description force server-relative))

    (when (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
	    (cmctl-ws-set cm-session-context
			  (cm-session-context-workspace-root cm-session-context)
			  description force server-relative))
      ;; Now CD to the workspace directory and sync
      (cm-cli-response :change-directory (pathname->platform-namestring
					  (cm-session-context-workspace-root cm-session-context)
					  (cm-session-context-client-platform cm-session-context))
		       (http-connection-stream http-connection))
      (cm-cli-response :continue-redirect "ws_sync" (http-connection-stream http-connection))
      :to-be-continued)))

(define-cm-cli-command "ws_update"
    (http-connection
     cm-session-context
     (server-relative :type :switch :syntax #. *cm-cli-switch-server-relative*)
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required t)
     (report :type :switch :syntax #.*cm-cli-switch-update-report-syntax*)
     ;; N.B. The following parameters have an equivalent set in many commands.
     (label-name :type :keyword :syntax #.*cm-cli-keyword-label-syntax*)
     (time-specification :type :keyword :syntax #.*cm-cli-keyword-time-syntax*))
  "Bring in csets that have occurred in master since last update or set.

   If workspace has been set to a release branch, bring in csets that have
   occurred on release branch.

   If a report only is requested, ChangeSafe lists the csets that would be brought in,
   without bringing them in. Shows any conflicts in files that have been changed.

   ws_update brings csets into the workspace and moves the reference version
   to be the master_tip (or the release_tip).

   If a label is specified, update to label rather than tip. Label must be on the
   same branch as the reference_version, or on the product trunk if the
   reference_version is on the trunk.

   TIME-SPECIFICATION, if specified, must be forward in time from the workspace baseline
   time on the product branch, and will be used to limit the update of the workspace to
   the indicated time. If omitted, the workspace is brough up to date with respect to the
   branch tip.

   Time should be specified in the following near-ISO format, yyyy-mm-dd[-hh:mm[:ss]].
   Hours are specified as 24 hour time.  The seconds, or the entire time of day, are
   optional.

   A workspace cannot be updated to a time earlier than the reference version."
  (with-cm-cli-control (http-connection cm-session-context "workspace update")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :label-name label-name
			    :time-spec time-specification
			    :ws-id ws-id)
    (guarantee-cd-in-workspace cm-session-context server-relative)
;    (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
;      (cmctl-ws-update cm-session-context
;                      server-relative
;                      report))


    (if report
	(prefer-read-only-server http-connection cm-session-context
	 (lambda ()
	     (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
	       (cmctl-ws-update cm-session-context
				server-relative
				report))))
      (when
	  (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
	       (cmctl-ws-update cm-session-context
				server-relative
				report))
	;; Now CD to the workspace directory and sync
	(cm-cli-response :change-directory (pathname->platform-namestring
					    (cm-session-context-workspace-root cm-session-context)
					    (cm-session-context-client-platform cm-session-context))
			 (http-connection-stream http-connection))
	(cm-cli-response :continue-redirect "ws_sync" (http-connection-stream http-connection))
	:to-be-continued))))

(define-cm-cli-command "ws_delete"
    (http-connection
     cm-session-context
     (server-relative :type :switch :syntax #. *cm-cli-switch-server-relative*)
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required nil)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required nil :secret t)
     ;; regular arguments
     (force :type :switch :syntax #.*cm-cli-switch-force-syntax* :required nil)
     (directory-spec :type :positional :required t)
     (keep-all-files  :type :switch :syntax "-keep-all-files"  :required nil)
     (keep-wild-files :type :switch :syntax "-keep-wild-files" :required nil))
  ;; get ws-id from the given directory-spec (via the csf file).
  ;; we delete the workspace, but if we are cd'd to the
  ;; workspace, we cannot delete the workspace directory!
  ;; The directory must be specified.  If we are 'in' the directory specified, we
  ;; are deleting the current workspace.  If we aren't 'in' it, we
  ;; assume we are deleting a different workspace.

  "Delete an old workspace that isn't being used any more.  If the workspace has an
   open cset, then -force is required. It will do an uncreate of the open cset.

   The directory and all its contents will be deleted except when your current
   working directory is 'in' the workspace.   In this case, some directories may be
   left on disk.  The workspace is removed from the response to
   \"what are my workspaces?\"

   The user does not have to be the owner of a workspace to delete it from the
   repository.  If the user does not have rights to delete the files, they will
   not be removed.

   The workspace must exist on disk with a valid .csf file for the command to
   proceed.

   If the switches -keep-all-files or -keep-wild-files are supplied, then the workspace
   is deleted from repository only, but all (or all wild) files are left on disk."
  ;; This was in the documentation:
  ;;   If directory has already been deleted from file system, this command
  ;;   is still valid. ChangeSafe remembers where the workspace was.
  ;;
  ;; I can't see how to implement that in any reasonable way.
  ;;
  ;; Also, the statement `User does not have to be the owner of a workspace
  ;; to delete it'  is only partly true.  It will be removed from the
  ;; repository, but it cannot be removed from the disk unless the
  ;; user has write permission.
  (declare (ignore master-repository-name))

  (with-cm-cli-control (http-connection cm-session-context "workspace deletion")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context :ws-id ws-id)

    (with-cm-cli-fsa (cm-session-context :server-relative server-relative)

      (let* ((workspace-directory-to-delete
	      (cm-cli-parse-absolute-directory-specifier cm-session-context directory-spec server-relative))

	     (currently-in-a-workspace (current-directory-in-workspace-p cm-session-context))
	     (current-workspace-root   (when currently-in-a-workspace
					 (cm-session-context-workspace-root cm-session-context)))
	     (current-ws-id            (when currently-in-a-workspace
					 (cm-session-context-ws-id cm-session-context)))
	     (keep-files (cond (keep-all-files  :all)
			       (keep-wild-files :wild))))

	;; read the .csf file and make sure we have the proper stuff (ws-id & dbname)
	;; Unfortunately, this side effects the session context!!!
	;;   --- insert usual rant about side effects ---
	;; so we have to capture certain info before we do this.
	(cmctl-read-dot-csf-file cm-session-context
				 workspace-directory-to-delete
				 cm-session-context)

	;; We have three cases:
	;; 1)  We are not cd'd to a workspace.
	;; 2)  We are cd'd to a workspace and we wish to delete it.
	;; 3)  We are cd'd to a workspace, but we wish to delete a different workspace.
	;;
	;; Option 3 is an error because it is confusing.
	;;
	;; However, note that for option 2, we will not likely be able to actually delete
	;; all of the directories as one or more of them will be "in use" since we have
	;; them as our current directory

	(if currently-in-a-workspace
	    ;; We *are* cd'd to a workspace.  Verify that the directory we specified to delete
	    ;; is the *same* directory as the workspace that we are in.
	    (if (= current-ws-id (cm-session-context-ws-id cm-session-context))
		(cmctl-ws-delete-directory
		  cm-session-context current-workspace-root server-relative force keep-files);; option 2
	      (conman-error-workspace-mismatch "delete" workspace-directory-to-delete current-workspace-root)) ;; option 3

	  ;; Not cd'd to a workspace -- option 1
	  (progn
	    (cm-session-context-add cm-session-context :rc-path workspace-directory-to-delete)
	    (cmctl-ws-delete-directory
	      cm-session-context workspace-directory-to-delete server-relative force keep-files)))))))

(define-cm-cli-command "ws_move"
    (http-connection
     cm-session-context
     (ws-id :type :keyword
	    :syntax #.*cm-cli-keyword-ws-id-syntax*
	    :required nil
	    :secret t)
     (xeno :type :switch :syntax #.*cm-cli-switch-xeno*)
     ;; PWW ??? Do we need server-relative ???
     (server-relative :type :switch :syntax #. *cm-cli-switch-server-relative*)
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (old-directory-spec :type :positional :required t)
     (new-directory-spec :type :positional :required t))

  "Rename a workspace to move it on the client machine; database changes are committed before the
   disk rename occurs; if errors occur during the disk rename, the user needs to finish this part
   by hand.

   The workspace object representation remains under control of the master repository.
   (Repository-side workspaces representations do not reside in the master repository, but in
   a 'semipersistent' database used for managing short-lived but persistent information).
   Other than remembering the new location of the workspace, all of the effort happens on
   the client side computer.  With this exception (the database remembers the new location),
   the semantic are rather like the UNIX \"mv\" command.  The directories can be specified
   with either an absolute or relative path.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative
   pathname.

   OLD-DIRECTORY-SPEC is a directory pathname specification which names the current location
   of the file system where the workspace is.  This directory must exist when the command is
   executed.

   NEW-DIRECTORY-SPEC is a directory pathname specification which names the new location of the
   file system where the workspace will be moved to.  This directory must not exist when the
   command is executed.

   XENO controls whether or not the new workspace location may be anywhere on the client machine.
   If not given, the new workspace must be under the user's home directory."

  #|
  If SERVER-RELATIVE is specified, then DIRECTORY-SPEC must name an absolute directory pathname.
  This is to prevent accidental use of server disk areas.

  If SERVER-RELATIVE is not specified, then DIRECTORY-SPEC may name either an absolute
  directory pathname or a relative directory pathname.  Relative directories are
  interpreted relative to the user's current working directory.

  *FINISH* ???: allow administrators to optionally require that disk areas are under the
  user's HOME directory, and enforce that constraint if selected.

  |#

  (with-cm-cli-control (http-connection cm-session-context "workspace move")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id)
    (guarantee-cd-not-in-workspace cm-session-context server-relative
				   *cm-returns-error-ws-move-current-dir-in-old-ws*)

    (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
      (let ((old-workspace-directory (cm-cli-parse-absolute-directory-specifier
				      cm-session-context
				      old-directory-spec
				      server-relative))
	    (new-workspace-directory (cm-cli-parse-absolute-directory-specifier
				      cm-session-context
				      new-directory-spec
				      server-relative))
	    (home-directory (cm-session-context-user-home-directory cm-session-context)))
	;; If the new workspace being created is not
	;; under the user's home directory, they must specify the additional control
	;; switch `xeno'
	(unless (or server-relative
		    xeno
		    (and home-directory
			 (funcall
			  (platform-pathname-match-p (cm-session-context-client-platform cm-session-context))
			  new-workspace-directory (make-wild-pathname home-directory))))
	  (conman-error-workspace-not-under-homedir new-workspace-directory home-directory))

	;; This chunk of code makes no sense to me.
	;; Up above, we call guarantee-cd-not-in-workspace, so we *know* that
	;; the conditional will always be false.
	;; Furthermore, the inner conditional only has one arm!

	#||
	;; We do not allow you to be "in" the old workspace as a current directory as this would
	;; preclude us from moving some of the directories on some platforms.
	(if (current-directory-in-workspace-p cm-session-context)
	    ;; We *are* cd'd to a workspace.  Verify that the directory we specified to move
	    ;; is the *same* directory as the workspace that we are in.
	    (let ((root-dir (cm-session-context-workspace-root cm-session-context)))
	      (if (funcall (platform-pathname-match-p (cm-session-context-client-platform cm-session-context))
			   old-workspace-directory
			   root-dir)
		  (conman-error-workspace-mismatch "move" old-workspace-directory root-dir))))
	||#

	(cmctl-ws-move cm-session-context old-workspace-directory new-workspace-directory)))))

;;;; ws_query
(define-cm-cli-command "ws_query"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword
	    :syntax #.*cm-cli-keyword-ws-id-syntax*
	    :required nil
	    :secret t)
     ;; Arguments specific to this command
     (show-work :type :switch :syntax #.*cm-cli-switch-show-work*)
     (list-user-ws :type :switch :syntax #.*cm-cli-switch-list-user-workspaces*)
     (list-all-ws :type :switch :syntax #.*cm-cli-switch-list-all-workspaces*)
     (list-workspaces-of-user :type :positional :required nil)
     )
  "Show what work the user has done in this workspace, or list this user's workspaces.

   Work in this workspace includes cset adds and removes, file adds and removes,
   file checkouts, and renames.
   \"show-work\" also tells what is the workspace baseline for this workspace.
   List of user workspaces includes all workspaces that the user has created
   and not deleted.  Workspaces do not have names,
   so the file system location of each workspace is given.
   \"-all\" option lists all workspaces active for the entire database."
  (with-cm-cli-control (http-connection cm-session-context "ws_query")
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id)
    (prefer-read-only-server http-connection cm-session-context
     (lambda ()
	 (unless (only-one-thing show-work list-user-ws list-all-ws)
	   (conman-signal-error
	    *cm-returns-error-bogus-arguments*
	    "You must specify exactly one of ~@{~a~^, ~}."
	    *cm-cli-switch-show-work*
	    *cm-cli-switch-list-user-workspaces*
	    *cm-cli-switch-list-all-workspaces*))
	 (cond (show-work
		(when list-workspaces-of-user
		  (conman-signal-error
		   *cm-returns-error-bogus-arguments*
		   "~a doesn't take a user argument." *cm-cli-switch-show-work*))
		(unless ws-id
		  (conman-signal-error
		   *cm-returns-error-no-such-workspace*
		   "Your working directory does not correspond to any known workspace."))
		(apply #'cm-cli-ws-query-show-display
		       http-connection
		       (with-cm-cli-fsa (cm-session-context)
			 (cmctl-ro-workspace-info-xact
			  (cm-session-context-repository-name cm-session-context)
			  (cm-session-context-ws-id cm-session-context))))
		t)
	       (list-all-ws
		(when list-workspaces-of-user
		  (conman-signal-error
		   *cm-returns-error-bogus-arguments*
		   "~a doesn't take a user argument." *cm-cli-switch-list-all-workspaces*))
		(cm-cli-ws-query-list-display
		 http-connection
		 (cmctl-ws-query-list cm-session-context :all))
		t)
	       (list-user-ws
		;; If user isn't specified it defaults to the current user
		(cm-cli-ws-query-list-display
		 http-connection
		 (cmctl-ws-query-list cm-session-context
				      (or list-workspaces-of-user
					  (cm-session-context-user-name cm-session-context))))
		t))))))

(defun cm-cli-ws-query-list-display (http-connection workspace-collection)
  "Output the list of workspaces for ws_query."
  (let ((stream (http-connection-stream http-connection)))
    (flet ((do-workspace-entry (ws-info)
	     (destructuring-bind (&key id user-name path product-refdir-p
				  &allow-other-keys) ws-info
	       (cm-cli-response :display
				(format nil "~&  ~3,' d ~10a ~a"
					id
					(if product-refdir-p "" user-name)
					path)
				stream
				:escape-it t))))
      (declare (dynamic-extent #'do-workspace-entry))
      (workspace-collection-map #'do-workspace-entry workspace-collection))))

(defun cm-cli-ws-query-show-display (http-connection
				     &key
				     ((:ws-id workspace-id))
				     up-to-date-p ws-timestamp branch-timestamp
				     ((:cset-name current-cset-name))
				     ((:cset-description current-cset-description))
				     ((:current-changes file-operations-by-subsystem))
				     ((:current-changes-info presentation-table))
				     ((:added-and-removed-csets
				       added-and-removed-changes-by-subsystem))
				     product-directory-p
				     product-name branch-name
				     pending-port-activity-p
				     port-adds port-deletes port-rejects
				     port-supplementary-info
				     return-a-string
				     &allow-other-keys)
  "Output the information for ws_query.
   FILE-OPERATIONS-BY-SUBSYSTEM and PRESENTATION-TABLE are as would be
   returned by CMCTL-WS-QUERY-SHOW.

   If RETURN-A-STRING is non-nil, no output is produced.  Instead a string containing
   the data is returned."
  ;; We need to sort the output so that it will always be in the same
  ;; order during the regression tests.
  (declare (ignore current-cset-description))
  (let (result-strs ;; possible result (but in reverse order and as a list)
	result-string) ;; possible result (in proper order as a string)
    (labels ((subsystem-dids-sorted-by-name ()
	       (let ((subsystem-dids nil))
		 (maphash (lambda (key value)
			      (declare (ignore value))
			      (push key subsystem-dids))
			  file-operations-by-subsystem)
		 (sort subsystem-dids #'string<
		       :key (lambda (subsystem-did)
				(gethash subsystem-did presentation-table)))))
	     (output-string (string)
	       (if return-a-string
		   (push (format nil "~a~%" string) result-strs)
		 (cm-cli-response :display string
				  (http-connection-stream http-connection)
				  :escape-it t)))
	     (do-sorted-output (collection pretty-string &rest accessors)
	       (mapc #'output-string
		     (sort (loop for thing in collection
				 for values = (mapcar (lambda (accessor)
							  (gethash (funcall accessor thing)
								   presentation-table))
						      accessors)
				 collect (format nil "~a~18t~{~a~^ ~}" pretty-string values))
			   #'string-lessp)))
	     (show-deltas (verb deltas)
	       (loop for (cset-did cset-delta-name) in deltas
		     do
		     (progn cset-did)	; ignore
		     (output-string (format nil "    ~a change ~a"
					    verb cset-delta-name)))))
      (output-string (format nil "~:[Workspace~;Reference workspace~] of product ~a, branch ~a."
			     product-directory-p product-name branch-name))
      (output-string (format nil "The unique id of this workspace is ~d." workspace-id))
      (output-string
       (if up-to-date-p
	   (format nil "It is up to date with the branch tip, last modified at ~a."
		   (universal-time->iso-date-time-string
		    (time-stamp-as-universal-time branch-timestamp)))
	 (format nil "Workspace is NOT up to date.~
                    ~&  Workspace timestamp ~a~
                    ~&  Branch timestamp    ~a"
		 (universal-time->iso-date-time-string
		  (time-stamp-as-universal-time ws-timestamp))
		 (universal-time->iso-date-time-string
		  (time-stamp-as-universal-time branch-timestamp)))))
      (if current-cset-name
	  (progn
	    (output-string (format nil "Current change \"~a\":" current-cset-name))
	    (when file-operations-by-subsystem
	      (loop for subsystem-did in (subsystem-dids-sorted-by-name)
		    for stuff = (gethash subsystem-did file-operations-by-subsystem)
		    do
		    (destructuring-bind (fileadds filechanges filerenames fileremoves) stuff
		      (do-sorted-output fileadds "  Add file"
					#'cc-fileadd-pathname)
		      (do-sorted-output filechanges "  Checkout file"
					#'cc-filechange-file-did)
		      (do-sorted-output filerenames "  Rename file"
					#'cc-filerename-file-did
					#'cc-filerename-new-pathname)
		      (do-sorted-output fileremoves "  Remove file"
					#'cc-fileremove-file-did)))))
	(output-string "There is no change in progress for this workspace."))
      (if added-and-removed-changes-by-subsystem
	  (loop initially (output-string "Added and removed changes:")
		for ((subsystem-did subsystem-name) added removed)
		in added-and-removed-changes-by-subsystem
		do
		(progn subsystem-did)	; ignore
		(output-string (format nil "  Subsystem ~a:" subsystem-name))
		(show-deltas "Added" added)
		(show-deltas "Removed" removed))
	(output-string "No csets have been added or removed for this workspace."))
      (if (or port-adds port-deletes port-rejects)
	  (let ((sorted-subsystem-dids (make-tree #'string= #'string<)))
	    (maphash (lambda (did plist)
			 ;; If there's a :SUBSYSTEM-NAME then DID is a subsystem DID
			 (when (getf plist :subsystem-name)
			   (rb-tree/insert! sorted-subsystem-dids
					    (getf plist :class-name)
					    did)))
		     port-supplementary-info)
	    (output-string "Port activity:")
	    (flet ((show-cset-did (description satellite-cset-did)
		     (output-string
		      (format nil "    ~a~15t~a"
			      description
			      (getf (gethash (getf (gethash satellite-cset-did
							    port-supplementary-info)
						   :delta-cset-did)
					     port-supplementary-info)
				    :cset-name)))))
	      (rb-tree/foreach
	       sorted-subsystem-dids
	       (lambda (class-name subsystem-did)
		   (output-string (format nil "  class ~a" class-name))
		   ;; ***TBD*** Shouldn't list the ones that have
		   ;; already shown up above as having already been
		   ;; added or removed in the workspace VPB.
		   (let ((omit-from-unreject nil))
		     (let ((adds (cdr (find subsystem-did port-adds :key #'car))))
		       (setq omit-from-unreject (append adds omit-from-unreject))
		       (mapc (lambda (cset) (show-cset-did "Add" cset)) adds))
		     (let ((removes (cdr (find subsystem-did port-deletes :key #'car))))
		       (setq omit-from-unreject (append removes omit-from-unreject))
		       (mapc (lambda (cset) (show-cset-did "Remove" cset)) removes))
		     (destructuring-bind (to-reject to-unreject)
			 (cdr (find subsystem-did port-rejects :key #'car))
		       (mapc (lambda (cset) (show-cset-did "Unreject" cset))
			     (set-difference to-unreject omit-from-unreject))
		       (mapc (lambda (cset) (show-cset-did "Reject" cset))
			     to-reject))))))
	    (when pending-port-activity-p
	      (output-string "There has been port activity since the last port -act")))
	(output-string "There is no port activity for this workspace.")))

    ;; fix up the string to return (if needed)
    (when return-a-string
      (setq result-string (apply #' concatenate 'string (reverse result-strs))))
    result-string))

;;;; cset_create
(define-cm-cli-command *cm-cli-command-change-create*
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required t)
     (server-relative :type :switch :syntax #.*cm-cli-switch-server-relative*)
     ;; The following arguments are NOT to be defaulted.
     (cset-name :type :keyword :syntax #.*cm-cli-keyword-change-name-syntax* :required t)
     (description :type :keyword :syntax #.*cm-cli-keyword-description-syntax*)
     (abstract-file :type :keyword :syntax #.*cm-cli-keyword-abstract-name-syntax*)
     (no-update? :type :switch :syntax #.*cm-cli-switch-no-update-syntax*)
     (file-names :type :rest))
  "Create a cset in which files may be added, altered, and removed from a workspace
   for a product.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   CSET-NAME is a user-specified name for the cset which is used as a suffix to a more fully
   qualified name in which the prefix is generated by the system.

   DESCRIPTION, if specified, is a string which is attached to the created cset representation
   in the master semipersistent repository.   It is displayed in various cset reports.
   Unless superseded during cset_close or master_change, this cset will be used
   to describe the resulting master change-set (a.k.a. 'super-cset').

   NO-UPDATE?, if specified, specifies that the workspace should not be ensured as up-to-date
   as with by cm_ws_update. *FINISH*: clarify, maybe eliminate in favor of workspace context.

   FILE-NAMES, if specified, represent zero or more file specifications
   for files which should be checked out as with cm_co into the workspace.
   If the file names are absolute path specifications, the path must map to a valid workspace
   directory.  If they are relative path specifications, they are merged with the client current working
   directory, and this in turn must map to a valid workspace directory.

   The FILE-NAMES, if specified, will be checked out at nearly the end of the cset_create command
   by means of calling the CO (checkout) command.

   ABSTRACT-FILE, if specified, should be the pathname of a lengthy description of the change,
   elaborating on the briefer text provided by the description argument. It may be altered
   later by the commands master_change or cset_close, at which time an abstract must've been
   supplied here or there."

  #|
   WS-ID identifies a workspace accessible to the client to which this cset applies.
   Csets are always bound to specific workspace instances, (though a workspace may exist
   without an active cset).  It must be either a unique workspace identifier, or a pathname
   which refers to the root directory of a workspace and matches the value stored in ChangeSafe's
   workspace-tracking repository.

   SERVER-RELATIVE, if specified, specifies that the workspace should be managed using
   the file system services which accessible to the master repository server.  This option
   can significantly reduce file management operation time in some NFS-centric
   environments.  By default, file system operations are performed using the file system
   services of the client desktop (user's machine).  An error is signalled if the server cannot locate the
   workspace when this option is specified.

   Your system administrator may disallow use of the SERVER-RELATIVE option.
   |#
  (declare (ignore product-name))
  (with-cm-cli-control (http-connection cm-session-context
			#.(format nil "~a creation" *cm-cli-command-change*))
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id)
    (guarantee-cd-in-workspace cm-session-context server-relative)
    (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
      (cmctl-change-create cm-session-context cset-name description
			   (when abstract-file
			     (guarantee-absolute-file-pathname
			      (cm-cli-parse-filename cm-session-context abstract-file server-relative)))
			   no-update?
			   ;; See JRM comments on relativizing
			   ;; filenames in file_rename
			   (mapcar (lambda (file-name)
				       (cm-cli-parse-filename
					cm-session-context file-name server-relative))
				   file-names))
      )))


;;;; cset_uncreate
(define-cm-cli-command *cm-cli-command-change-uncreate*
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required t)
     (server-relative :type :switch :syntax #.*cm-cli-switch-server-relative*)
     ;; The following arguments are NOT to be defaulted.
     ;; This command has no regular arguments.  The currently open cset is the one
     ;; that will be uncreated.
     )
  "Uncreate/abort the current cset.
   Currently, this command will only work if the cset has no changes in it."
  (declare (ignore product-name))
  (with-cm-cli-control (http-connection cm-session-context
			#.(format nil "~a uncreation" *cm-cli-command-change*))
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id)
    (guarantee-cd-in-workspace cm-session-context server-relative)
    (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
      (cmctl-change-uncreate cm-session-context))))

(define-cm-cli-command *cm-cli-command-change-add*   ;; cset_add
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required t)
     (server-relative :type :switch :syntax #.*cm-cli-switch-server-relative*)
     (suppress-workspace-update :type :switch :syntax "-punt-workspace-update"
				:required nil
				:secret t)
     ;; The following arguments are NOT to be defaulted.
     (class-name :type :keyword :syntax #.*cm-cli-keyword-class-name-syntax*)
     (cset-name :type :positional :required t))
  ;; *TBD* HP questions #54, #55, and #56
  "Add the named cset to the current workspace context.  Files in the workspace are updated.
   Any affected files that have been checked out in this workspace are copied to a backup file,
   i.e. filename.bak.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   CSET-NAME is a system-augmented name as generated by cset_create, and must uniquely
   identify the cset to add.

   CLASS-NAME, if specified, names a class.  In this case, subsystem csets for subsystems
   which map to the indicated class are added to the workspace, and not the entire
   master repository cset (which might span many classes/subsystems)."
  (declare (ignore product-name))
  (with-cm-cli-control (http-connection cm-session-context
			#.(format nil "~a addition" *cm-cli-command-change*))
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id)
    (guarantee-cd-in-workspace cm-session-context server-relative)
    (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
      (cmctl-change-add-or-remove cm-session-context (list cset-name) nil class-name (not suppress-workspace-update)))))



;    (when
;    (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
;      (cmctl-change-add-or-remove cm-session-context (list cset-name) nil class-name))))
;      (cm-cli-response :change-directory (pathname->platform-namestring
;                                         (cm-session-context-workspace-root cm-session-context)
;                                         (cm-session-context-client-platform cm-session-context))
;                      (http-connection-stream http-connection))
;      (cm-cli-response :continue-redirect "ws_sync" (http-connection-stream http-connection))
;      :to-be-continued)))

(define-cm-cli-command *cm-cli-command-change-remove*   ;; cset_remove
    ;; Identical to cm_cset_add, a shame to clone the code.  We do it only so that define-cm-cli-command
    ;; can register the appropriate command name and doc string.
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required t)
     (server-relative :type :switch :syntax #.*cm-cli-switch-server-relative*)
     (suppress-workspace-update :type :switch :syntax "-punt-workspace-update"
				:required nil
				:secret t)
     ;; The following arguments are NOT to be defaulted.
     (class-name :type :keyword :syntax #.*cm-cli-keyword-class-name-syntax*)
     (cset-name :type :positional :required t))
  ;; *TBD* HP questions #54, #55, and #56
  "Remove the named cset from the current workspace context.  Files in the workspace are updated.
   Any affected files that have been checked out in this workspace are copied to a backup file,
   i.e. filename.bak.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   CSET-NAME is a system-augmented name as generated by cset_create, and must uniquely
   identify the cset to remove.

   CLASS-NAME, if specified, names a class.  In this case, subsystem csets for subsystems
   which map to the indicated class are added to the workspace, and not the entire
   master repository cset (which might span many classes/subsystems)."
  (declare (ignore product-name))
  (with-cm-cli-control (http-connection cm-session-context
			#.(format nil "~a removal" *cm-cli-command-change*))
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id)
    (guarantee-cd-in-workspace cm-session-context server-relative)
    (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
      (cmctl-change-add-or-remove cm-session-context nil (list cset-name) class-name (not suppress-workspace-update)))))

;    (when
;       (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
;         (cmctl-change-add-or-remove cm-session-context nil (list cset-name) class-name))
;      (cm-cli-response :change-directory (pathname->platform-namestring
;                                         (cm-session-context-workspace-root cm-session-context)
;                                         (cm-session-context-client-platform cm-session-context))
;                      (http-connection-stream http-connection))
;      (cm-cli-response :continue-redirect "ws_sync" (http-connection-stream http-connection))
;      :to-be-continued)))

(define-cm-cli-command "csets_from_file"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required t)
     (server-relative :type :switch :syntax #.*cm-cli-switch-server-relative*)
     (suppress-workspace-update :type :switch :syntax "-punt-workspace-update"
				:required nil
				:secret t)
     ;; The following arguments are NOT to be defaulted.
     (class-name :type :keyword :syntax #.*cm-cli-keyword-class-name-syntax*)
     (file-name :type :positional :required t))
  ;; PWW ??? *TBD* HP questions #54, #55, and #56
  "Insert or remove a number of csets, all together.

   The file given contains a list of cset names, each prepended by a \"+\" or \"-\", indicating
   to add the csets to or remove the csets from the workspace.

   The cset names are separated by white space and/or newlines.  There is no other information
   in the file.

   This command works like cnm cset_add and cnm cset_remove, except that you can move several
   csets in a single operation.

   Those csets which are to be added (+) will be added to the current workspace context.  Files
   in the workspace are updated.  Any affected files that have been checked out in this workspace
   are copied to a backup file, i.e. filename.bak.

   Those csets which are to be removed (-) will be removed from the current workspace context.
   Files in the workspace are updated.  Any affected files that have been checked out in this
   workspace are copied to a backup file, i.e. filename.bak.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative
   pathname.

   FILE-NAME is the name of the file containing the cset names to be added or removed.

   CLASS-NAME, if specified, names a class.  In this case, subsystem csets for subsystems
   which map to the indicated class are added to the workspace, and not the entire
   master repository cset (which might span many classes/subsystems).

   The cset names within the file are the system-augmented names as generated by cset_create,
   and must uniquely identify the cset to add."
  (with-cm-cli-control (http-connection cm-session-context "csets from file added or removed")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id)
    (guarantee-cd-in-workspace cm-session-context server-relative)
    (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
      (cmctl-csets-from-file cm-session-context
			     (guarantee-absolute-file-pathname
			      (cm-cli-parse-filename
			       cm-session-context file-name server-relative))
			     class-name
			     (not suppress-workspace-update)))))

;    (when
;       (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
;         (cmctl-csets-from-file cm-session-context
;                                (guarantee-absolute-file-pathname
;                                 (cm-cli-parse-filename
;                                  cm-session-context file-name server-relative))
;                                class-name))
;      (cm-cli-response :change-directory (pathname->platform-namestring
;                                         (cm-session-context-workspace-root cm-session-context)
;                                         (cm-session-context-client-platform cm-session-context))
;                      (http-connection-stream http-connection))
;      (cm-cli-response :continue-redirect "ws_sync" (http-connection-stream http-connection))
;      :to-be-continued)))

(define-cm-cli-command *cm-cli-command-file-add* ; "file_add"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required t)
     (server-relative :type :switch :syntax #.*cm-cli-switch-server-relative*)
     (text-content    :type :switch :syntax "-text")
     (binary-content  :type :switch :syntax "-binary")
     ;; The following arguments are NOT to be defaulted.
     (description :type :keyword :syntax #.*cm-cli-keyword-description-syntax*)
     (file-names :type :rest))
  "Declare the intent to add specified files to a cset context which is active in a given
   workspace.  A cset must be active in the workspace when this command is issued.
   Note that the effect of this command is a declaration of intent, not an actual effect
   upon the central repository.  Files aren't added to the repository until the cset is committed
   via cset_close or master_change commands.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   DESCRIPTION, if specified, is a string which is attached to the files which are created
   when the cset is committed.   It is displayed in various file-oriented reports.
   It is of questionable value to attach the same description to many files, this should be left
   to the cset description.  If descriptions are provided for files, the description should
   relate to the purpose, use, caveats, and other critical information which should be seen
   by others who will work with the file.

   If you wish to attach a unique description to each file, you must use a separate file_add command for
   each file.

   FILE-NAMES, if specified, represent zero or more file specifications
   for files which are to be added to the change repository when a cset is committed.
   If the file names are absolute path specifications, the path must map to a valid workspace
   directory.  If they are relative path specifications, they are merged with the client current working
   directory, and this in turn must map to a valid workspace directory.

   Each file named in FILE-NAMES must exist under a subsystem-rooted directory in the workspace.
   If the file exists in the workspace in a directory which is not a (or parented by a) subsystem
   directory, an error is signalled.

   -BINARY and -TEXT switches declare the file content type of all files listed in FILE-NAMES.
   In absence of such declarations the content type is auto-detected via built-in heuristics."
  (declare (ignore product-name))
  (with-cm-cli-control (http-connection cm-session-context "file addition")
    (conman-check-server-operating-mode :read/write)
    (when (and text-content binary-content)
      (conman-signal-error *cm-returns-error-bogus-arguments*
			   "You can't specify both '-TEXT' and '-BINARY' file content type"))
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id)
    (guarantee-cd-in-workspace cm-session-context server-relative)
    (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
      (cmctl-file-add cm-session-context description
		      ;; See JRM comments on relativizing filenames in file_rename
		      (mapcar (lambda (file-name)
				  (cm-cli-parse-filename
				   cm-session-context file-name server-relative))
			      file-names)
		      (cond (binary-content :binary)
			    (text-content   :text))))))

(define-cm-cli-command "undo_file_add"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted by .csf file or similar mechanism
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required t)
     (server-relative :type :switch :syntax #.*cm-cli-switch-server-relative*)
     ;; regular command line arguments
     (file-names :type :rest))
  "Undo a file_add operation."
  (declare (ignore product-name))
  (with-cm-cli-control (http-connection cm-session-context "undo file addition")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id)
    (guarantee-cd-in-workspace cm-session-context server-relative)
    (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
      (cmctl-undo-file-add cm-session-context
			   (mapcar (lambda (file-name)
				       (cm-cli-parse-filename
					cm-session-context file-name server-relative))
				   file-names)
			   server-relative))))

(define-cm-cli-command "undo"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted by .csf file or similar mechanism
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required t)
     (server-relative :type :switch :syntax #.*cm-cli-switch-server-relative*)
     ;; regular command line arguments
     (command-to-undo :type :positional :required t)
     (file-names :type :rest))
  "Undo a file_add, file_remove, file_change, or file_delete command."
  (with-cm-cli-control (http-connection cm-session-context "undo")
    (conman-check-server-operating-mode :read/write)
    (flet ((use-command (suggested-replacement)
	     (conman-error-inappropriate-undo command-to-undo suggested-replacement))

	   (written-in-stone ()
	     (conman-error-prohibited-undo command-to-undo))

	   (cant-undo ()
	     (conman-error-cant-undo command-to-undo)))

      (let ((undoer
	     (cond ((string-equal command-to-undo "file_add")	 #'cmctl-undo-file-add)
		   ((string-equal command-to-undo "file_remove") #'cmctl-undo-file-remove)
		   ((string-equal command-to-undo "file_rename") #'cmctl-undo-file-rename)
		   ((string-equal command-to-undo "co")	       (use-command "unco"))
		   ((string-equal command-to-undo *cm-cli-command-change-add*)
		    (use-command *cm-cli-command-change-remove*))
		   ((string-equal command-to-undo *cm-cli-command-change-remove*)
		    (use-command *cm-cli-command-change-add*))
		   ((string-equal command-to-undo "master_change") (written-in-stone))
		   ((string-equal command-to-undo *cm-cli-command-change-close*)
		    (written-in-stone))
		   (t (cant-undo)))))
	(cm-session-context-add cm-session-context
				:repository-name master-repository-name
				:ws-id ws-id)
	(guarantee-cd-in-workspace cm-session-context server-relative)
	(with-cm-cli-fsa (cm-session-context :server-relative server-relative)
	  (funcall undoer
		   cm-session-context
		   (mapcar (lambda (file-name)
			       (cm-cli-parse-filename
				cm-session-context file-name server-relative))
			   file-names)
		   server-relative))))))

;;;; *FINISH* "CO" should deal with the notification of the other users, etc. etc.
;;;; co aka checkout
(define-cm-cli-command "co"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required t)
     (server-relative :type :switch :syntax #.*cm-cli-switch-server-relative*)
     ;; The following arguments are NOT to be defaulted.
     (file-names :type :rest))
  "Check-out a file for modification in the current cset in progress in a workspace.

   A cset must be active in the workspace when this command is issued.
   Note that the effect of this command is a declaration of intent, not an actual effect
   upon the central repository.  Files aren't modified in the repository until the cset is committed
   via cset_close or master_change commands.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   While the files aren't actually read into the change management repository by this command,
   they are probed for existence and other compatibility issues via file system operations.

   FILE-NAMES, if specified, represent zero or more file specifications
   for files which are to be added to the change repository when a cset is committed.
   If the file names are absolute path specifications, the path must map to a valid workspace
   directory.  If they are relative path specifications, they are merged with the client current working
   directory, and this in turn must map to a valid workspace directory.

   Each file named in FILE-NAMES must exist under a subsystem-rooted directory in the workspace.
   If the file exists in the workspace in a directory which is not a (or parented by a) subsystem
   directory, an error is signalled."
  (with-cm-cli-control (http-connection cm-session-context "file checkout")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id)
    (guarantee-cd-in-workspace cm-session-context server-relative)
    (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
      (cmctl-file-checkout cm-session-context
			   ;; See JRM comments on relativizing filenames in file_rename
			   (mapcar (lambda (file-name)
				       (cm-cli-parse-filename
					cm-session-context file-name server-relative))
				   file-names))
      )))

;;;; unco
(define-cm-cli-command "unco"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required t)
     (server-relative :type :switch :syntax #.*cm-cli-switch-server-relative*)
     ;; The following arguments are NOT to be defaulted.
     (no-copy :type :switch :syntax #.*cm-cli-switch-nocopy-syntax*)
     (all :type :switch :syntax #.*cm-cli-switch-all-files-syntax*)
     (class :type :keyword :syntax #.*cm-cli-keyword-class-name-syntax*)
     (subsystem :type :keyword :syntax #.*cm-cli-keyword-subsystem-name-syntax*)
     (file-names :type :rest))
  "Uncheckout. Cancel results of one or more cm co commands.

   User can uncheckout one or more individual files, all the files in a given
   subsys, or all the files checked out for this cset.

   This command does not do a cm cset_uncreate. The cset is still active,
   even if all files have been unco'ed.

   Files that are uncheckedout will be copied to backup (.bak) files, and be
   replaced with read-only files generated from the reference version. If
   the -nocopy switch is present, no backup files will be created."
  (with-cm-cli-control (http-connection cm-session-context "unco")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id
			    :subsystem-name subsystem
			    :class-name class)
    (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
      (let ((options-specified
	     ;; Counts number of Non-nil items!
	     (count nil (list class subsystem all file-names) :key #'null)))
	(cond ((zerop options-specified)
	       (conman-condition-no-args-to-unco))
	      ((> options-specified 1)
	       (conman-condition-too-many-args-to-unco
		(append (when all   (list *cm-cli-switch-all-files-syntax*))
			(when class (list *cm-cli-keyword-class-name-syntax*))
			(when subsystem (list *cm-cli-keyword-subsystem-name-syntax*)))
		file-names))
	      (t
	       (guarantee-cd-in-workspace cm-session-context server-relative)
	       (cmctl-uncheckout cm-session-context
				 all
				 class subsystem
				 (mapcar (lambda (file-name)
					     (cm-cli-parse-filename
					      cm-session-context file-name server-relative))
					 file-names)
				 server-relative
				 no-copy
				 )))))))

(define-cm-cli-command *cm-cli-command-file-remove* ; "file_remove"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required t)
     (server-relative :type :switch :syntax #.*cm-cli-switch-server-relative*)
     ;; The following arguments are NOT to be defaulted.
     (file-names :type :rest))
  "Declare the intent to remove specified files in a cset context which is active in a given
   workspace.  A cset must be active in the workspace when this command is issued.
   Note that the effect of this command is a declaration of intent, not an actual effect
   upon the central repository.  Files aren't removed from the repository until the cset is committed
   via cset_close or master_change commands.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   While the files aren't actually read into the change management repository by this command,
   they are probed for existence and other compatibility issues via file system operations.

   FILE-NAMES, if specified, represent zero or more file specifications
   for files which are to be added to the change repository when a cset is committed.
   If the file names are absolute path specifications, the path must map to a valid workspace
   directory.  If they are relative path specifications, they are merged with the client current working
   directory, and this in turn must map to a valid workspace directory.

   Each file named in FILE-NAMES must exist under a subsystem-rooted directory in the workspace.
   If the file exists in the workspace in a directory which is not a (or parented by a) subsystem
   directory, an error is signalled."
  (with-cm-cli-control (http-connection cm-session-context "file remove")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id)
    (guarantee-cd-in-workspace cm-session-context server-relative)
    (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
      (cmctl-file-remove cm-session-context
			 ;; See JRM comments on relativizing filenames in file_rename
			 (mapcar (lambda (file-name)
				     (cm-cli-parse-filename
				      cm-session-context file-name server-relative))
				 file-names)))))

(define-cm-cli-command *cm-cli-command-file-rename* ; "file_rename"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required t)
     (server-relative :type :switch :syntax #.*cm-cli-switch-server-relative*)
     ;; The following arguments are NOT to be defaulted.
     (src-file-name :type :positional :required t)
     (dst-file-name :type :positional :required t))

  "Rename a file in the workspace which is also represented in the repository.

   A cset must be active in the workspace when this command is issued.
   Note that the effect of this command is a declaration of intent, not an actual effect
   upon the central repository.  The file isn't renamed in the repository until the cset is committed
   via cset_close or master_change commands.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   SRC-FILE-NAME is the 'old' name of the file, and should name the file as it currently exists in
   the workspace.

   DST-FILE-NAME is the 'new' name of the file.

   SERVER-RELATIVE, if specified, specifies that the workspace should be managed using
   the file system services which accessible to the master repository server.  This option
   can significantly reduce file management operation time in some NFS-centric
   environments.  By default, file system operations are performed using the file system
   services of the client desktop (user's machine).  An error is signalled if the server cannot locate the
   workspace when this option is specified.

   Your system administrator may disallow use of the SERVER-RELATIVE option.

   While the files aren't actually read into the change management repository by this command,
   they are probed for existence and other compatibility issues via file system operations."
  (with-cm-cli-control (http-connection cm-session-context "file rename")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id)
    (guarantee-cd-in-workspace cm-session-context server-relative)
    (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
      (cmctl-file-rename cm-session-context
			 ;; JRM: I'd like to relativize the filenames before
			 ;; passing them in, but if they are server relative,
			 ;; I have no idea what the workspace root is without
			 ;; opening the workspace repository.  So instead,
			 ;; I convert to absolute pathnames and relativize
			 ;; them at the earliest convenience.
			 (cm-cli-parse-filename
			  cm-session-context src-file-name server-relative)
			 (cm-cli-parse-filename
			  cm-session-context dst-file-name server-relative)))))

(defun cm-cli-cset-check-secret-args (cm-session-context secret-username secret-cset-name cset-name)
  "Checks for whether or not CSET_CLOSE or MASTER_CHANGE secret args are allowed to be
   used.  Also does some validity tests"
  (when secret-username
    (unless *conman-enable-secret-cset-zapping*
      ;; give the same error as when a completely random option is used
      (conman-signal-error *cm-returns-error-too-many-arguments*
			   "Too many command arguments were specified: ~s"
			   secret-username))
    (when (or secret-cset-name
	      (not (stringp secret-username)))
      ;; only one of these may be used at a time, so give an error
      ;; also username must be a string
      (conman-signal-error *cm-returns-error-too-many-arguments*
			   "Too many command arguments were specified: ~s"
			   secret-username))

    ;; assign the secret-userid into the cm-session-context slot and just use it thru-out
    ;; this just throws away whatever user-name he actually has for the duration of the
    ;; command.
    (check-type secret-username string)
    (cm-session-context-add cm-session-context :user-name secret-username))

  (when secret-cset-name
    (unless *conman-enable-secret-cset-zapping*
      ;; give the same error as when a completely random option is used
      (conman-signal-error *cm-returns-error-too-many-arguments*
			   "Too many command arguments were specified: ~s"
			   secret-cset-name))

    (unless cset-name
      ;; the cset-name is required when the -secret-cset-name arg is used
      (conman-signal-error *cm-returns-error-too-few-arguments*
			   "Not enough command arguments were specified: -name"))

    ;; check that the csetname is indeed fully qualified
    (multiple-value-bind (user-change-name user-name pc-name date-string)
	(conman-disassemble-HP-cset-name cset-name)

      (unless (and user-change-name user-name pc-name date-string)
	(conman-signal-error *cm-returns-error-too-many-arguments*
			     "Too many command arguments were specified: ~s"
			     secret-cset-name))
      ;; assign the user-name from the fully qualified cset-name into the
      ;; cm-session-context slot and just use it thru-out
      ;; this just throws away whatever user-name he actually has for the duration of the
      ;; command.
      (check-type user-name string)
      (cm-session-context-add cm-session-context :user-name user-name)

      ;; make sure that the user cset name portion is syntactically valid
      (unless (cmctl-valid-name? user-change-name)
	(conman-signal-error *cm-returns-error-invalid-change-name*
			     "The cset name, ~S, specified was invalid."
			     user-change-name))

      ;; make sure we have a legal date
      (iso-date-time-string->universal-time date-string)
      ))
  )

;; aka cset_close
(define-cm-cli-command *cm-cli-command-change-close*
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required t)
     (server-relative :type :switch :syntax #.*cm-cli-switch-server-relative*)
     ;; The following arguments are NOT to be defaulted.
     (cset-name :type :keyword :syntax #.*cm-cli-keyword-change-name-syntax*)
     (abstract-file :type :keyword :syntax #.*cm-cli-keyword-abstract-name-syntax*)
     (description :type :keyword :syntax #.*cm-cli-keyword-description-syntax*)
     (adc-import :type :switch :syntax "-ADCimport")
     (secret-userid :type :keyword :secret t :syntax "-secret-userid")
     (secret-cset-name :type :switch :secret t :syntax "-secret-cset-name")
     )
  "Create a change-set in the master repository which reflects a check-in of all file changes
   in the workspace, without actually promoting the resulting change-set on the product branch.
   (This latter step is achieved by master_change, or by cset_add).

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   CSET-NAME, if specified, is a user-specified name for the cset which is used as a
   suffix to a more fully qualified name in which the prefix is generated by the system.
   If specified for this command, the user is effectively changing the name which was
   specified on cset_create.

   DESCRIPTION, if specified, is a string which is attached to the resulting change-set in the
   master repository.   It is displayed in various change-set reports.  This argument may be
   omitted if a description was specified on the cset_create command, otherwise it must be specified.

   ABSTRACT-FILE, if specified, should be the pathname of a file that will override the abstract-file
   argument which was specified for the cset_create command, and identifies the nature of the change
   being achieved with this call to master_change.  ABSTRACT-FILE contents are typically lengthy descriptions of
   the change elaborating the somewhat briefer text provided by DESCRIPTION arguments.

   If a cset is open then an abstract must be supplied either by this command or cset_create;
   otherwise abstract is optional."
  ;; SECRET-USERID and SECRET-CSET-NAME may only be used when *conman-enable-secret-cset-zapping* is non-nil.
  ;; Since they are secret, we do not include tham as part of the regular doc string (above).  They allow
  ;; the user to reassign these attributes of a cset.
  (declare (ignore product-name))
  (let ((rfm:*file-text-contents-are-diffs* adc-import))
    (with-cm-cli-control (http-connection cm-session-context
					  #.(format nil "~a close" *cm-cli-command-change*))
      (conman-check-server-operating-mode :read/write)
      (cm-cli-cset-check-secret-args cm-session-context secret-userid secret-cset-name cset-name)
      (cm-session-context-add cm-session-context
			      :repository-name master-repository-name
			      :ws-id ws-id)
      (guarantee-cd-in-workspace cm-session-context server-relative)
      (let ((cset-query-str ;; for the email, only doit if we will have email
	     (and (conman-send-email-for-check-ins)
		  (apply #'cm-cli-ws-query-show-display
			 http-connection
			 :return-a-string t
			 (with-cm-cli-fsa (cm-session-context)
			   (cmctl-ro-workspace-info-xact
			    (cm-session-context-repository-name cm-session-context)
			    (cm-session-context-ws-id cm-session-context))))))
	    final-cset-name)
	(with-cm-cli-fsa (cm-session-context :server-relative server-relative)
	  (setq final-cset-name
		(cmctl-change-close
		 cm-session-context cset-name
		 (when abstract-file
		   (guarantee-absolute-file-pathname
		    (cm-cli-parse-filename cm-session-context abstract-file server-relative)))
		 description secret-cset-name cset-query-str))
	  (if final-cset-name t nil)) ;; since with-cm-cli-fsa wants a t or nil

	(when (not (eq t final-cset-name)) ;; bug 001019-0000
	  (cm-cli-response
	   :display (format nil "~%~a CSET Name: ~a~%~%" *cm-cli-command-change-close* final-cset-name)
	   (http-connection-stream http-connection)
	   :escape-it t))
	(if final-cset-name t nil) ;; since our caller wants a T or NIL
	))))

(define-cm-cli-command *cm-cli-command-workspace-regenerate*
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required t)
     (server-relative :type :switch :syntax #.*cm-cli-switch-server-relative*))
  "Regenerate a workspace that has been damaged.

   Damage can occur from users deleting or overwriting read-only files, or from the user deleting the
   entire workspace. This command goes through all the csets of the workspace, to regenerate all the files.

   This command should be called from within the workspace to be regenerated. If the workspace was deleted,
   recreate the directory on the file system and call this command.

   This command does the following:

      -    Regenerates all files as read-only.

      -    Leaves wild files where they are.

      -    If there is a wild file where a read-only file should be, makes a backup copy of the wild file,
           and replaces it with the correct read-only file.

      -    Applies whatever csets have been brought into the workspace.

      -    Leaves checked out files unchanged. If a checked out file is missing from the file system,
           checks out a fresh copy."
  (with-cm-cli-control (http-connection cm-session-context "ws_regenerate")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id)
    (guarantee-cd-in-workspace cm-session-context server-relative)
;    (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
;      (cmctl-ws-regenerate-immediately cm-session-context server-relative))

    (when (cmctl-ws-regenerate cm-session-context server-relative)
      (cm-cli-response :change-directory (pathname->platform-namestring
					  (cm-session-context-workspace-root cm-session-context)
					  (cm-session-context-client-platform cm-session-context))
		       (http-connection-stream http-connection))
      (cm-cli-response :continue-redirect "ws_sync" (http-connection-stream http-connection))
      :to-be-continued)))

(define-cm-cli-command "help"
    (http-connection
     cm-session-context
     (command-name :type :positional :required nil :default nil))
  "Describe a cm command in various levels of detail depending on the verbosity level.
   If no command is specified, give descriptions of all commands (also varying in detail with
   verbosity level)."
  (with-cm-cli-control (http-connection cm-session-context "help")
    (cm-cli-response :display
		     (with-output-to-string (stream)
		       (if command-name
			   (let ((command-description
				  (cm-cli-find-command-description :name command-name)))
			     (if command-description
				 (progn
				   (when (= (cm-session-context-verbosity cm-session-context) 0)
				     (document-cm-command command-description
							  :stream stream
							  :show-args t
							  :arg-separator :mandatory
							  :show-doc nil))
				   (when (= (cm-session-context-verbosity cm-session-context) 1)
				     (document-cm-command command-description
							  :stream stream
							  :show-args t
							  :arg-separator :mandatory
							  :show-doc :ONE-LINER))
				   (when (= (cm-session-context-verbosity cm-session-context) 2)
				     (document-cm-command command-description
							  :stream stream
							  :show-args t
							  :arg-separator :mandatory
							  :show-doc t)))
			       (format stream "There is no ~s command." command-name)))
			 (list-cm-commands cm-session-context :stream stream)))
		     (http-connection-stream http-connection)
		     :escape-it t)
    t))

(define-cm-cli-command "ci" (http-connection cm-session-context
			     (ignore :type :rest :required nil))
  "There is no ci command.  To do a checkin use either cset_close or master_change, as appropriate."
  (declare (ignore cm-session-context ignore))
  (cm-cli-response
   :display
   "There is no ci command.  To do a checkin use either cset_close or master_change, as appropriate."
   (http-connection-stream http-connection)))

;; aka master_change
(define-cm-cli-command *cm-cli-command-master-change*
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted by .csf file or similar mechanism
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax*
	    :required t :secret t)
     ;; release should be required, but main is not yet working that way yet
     (server-relative :type :switch :syntax #.*cm-cli-switch-server-relative*)
     ;; The following arguments are NOT to be defaulted.
     (cset-name :type :keyword :syntax #.*cm-cli-keyword-change-name-syntax*)
     (abstract-file :type :keyword :syntax #.*cm-cli-keyword-abstract-name-syntax*)
     (description :type :keyword :syntax #.*cm-cli-keyword-description-syntax*)
     (ws-update  :type :switch :syntax "-update")
     (adc-import :type :switch :syntax "-ADCimport")
     (secret-userid :type :keyword :secret t :syntax "-secret-userid")
     (secret-cset-name :type :switch :secret t :syntax "-secret-cset-name")
     )
  "Ensure that all changes present in the workspace are checked in and promoted into the
   master product.  A cset context, if active via cset_create, will be
   closed as with cset_close, and promoted into the master branch tip.  If no cset is
   active, but change-sets have been added to or removed from the workspace via
   cset_add, cset_remove, or the 'port' command, these csets will be be promoted
   into the master tip.

  MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   CSET-NAME, if specified, overrides the cset-name which was specified for the cset_create
   command.  If there is no active cset (as by cset_create), then CSET-NAME must be specified.

   DESCRIPTION, if specified, overrides the description which was specified for the cset_create
   command, and identifies the nature of the cset being achieved with this call to master_change.
   If there is no active cset (as by cset_create), then DESCRIPTION must be specified.

   ABSTRACT-FILE, if specified, should be the pathname of a file that will override the abstract-file
   argument which was specified for the cset_create command, and identifies the nature of the change
   being achieved with this call to master_change.  ABSTRACT-FILE contents are typically lengthy descriptions of
   the change elaborating the somewhat briefer text provided by DESCRIPTION arguments.
   If a cset is open then an abstract must be supplied either by this command or cset_create;
   otherwise abstract is optional.

   UPDATE, if specified, automatically updates the workspace before performing the master_change.
   If updating results in a merge the command aborts after the update and before the master_change."

  ;; SECRET-USERID and SECRET-CSET-NAME may only be used when *conman-enable-secret-cset-zapping* is non-nil.
  ;; Since they are secret, we do not include tham as part of the regular doc string (above).  They allow
  ;; the user to reassign these attributes of a cset.
  (let ((rfm:*file-text-contents-are-diffs* adc-import))
    (with-cm-cli-control (http-connection cm-session-context "master change")
      (conman-check-server-operating-mode :read/write)
      (cm-cli-cset-check-secret-args cm-session-context secret-userid secret-cset-name cset-name)
      (cm-session-context-add cm-session-context
			      :repository-name
			      master-repository-name
			      :ws-id ws-id)
      (guarantee-cd-in-workspace cm-session-context server-relative)
      (let ((cset-query-str ;; for the email, only doit if we will have email
	     (and (conman-send-email-for-check-ins)
		  (apply #'cm-cli-ws-query-show-display
			 http-connection
			 :return-a-string t
			 (with-cm-cli-fsa (cm-session-context)
			   (cmctl-ro-workspace-info-xact
			    (cm-session-context-repository-name cm-session-context)
			    (cm-session-context-ws-id cm-session-context))))))
	    final-cset-name)
	(with-cm-cli-fsa (cm-session-context)
	  ;; **** WARNING **** **** WARNING **** **** WARNING **** **** WARNING **** **** WARNING ****
	  ;;
	  ;;    Use of http-connection-stream within the scope of WITH-CM-CLI-FSA is PROHIBITED.
	  ;;    It is used by the file-system which is bound below.
	  ;;
	  ;; **** WARNING **** **** WARNING **** **** WARNING **** **** WARNING **** **** WARNING ****
	  (setq final-cset-name
		(cmctl-master-change
		 cm-session-context cset-name
		 (when abstract-file
		   (guarantee-absolute-file-pathname
		    (cm-cli-parse-filename cm-session-context abstract-file server-relative)))
		 description secret-cset-name ws-update cset-query-str))
	  (if final-cset-name t nil)) ;; since with-cm-cli-fsa wants a t or nil

	(when (not (eq t final-cset-name)) ;; bug 001019-0000
	  (cm-cli-response
	   :display (format nil "~%~a CSET Name: ~a~%~%" *cm-cli-command-master-change* final-cset-name)
	   (http-connection-stream http-connection)
	   :escape-it t))
	(if final-cset-name t nil) ;; since our caller wants a T or NIL
	))))

(define-cm-cli-command "master_lock"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted by .csf file or similar mechanism
     (master-repository-name :type :keyword
			     :syntax #.*cm-cli-keyword-master-syntax*
			     :required t)
     (ws-id :type :keyword
	    :syntax #.*cm-cli-keyword-ws-id-syntax*
	    :required t)
     ;; There are no command arguments other than -q and -v, which aren't implemented yet for any command.
     ;; We may wish to support a specific lock description (-desc) argument describing why
     ;; the user felt compelled to perform the lock.  *TBD*, *FINISH*: status approved
     (description :type :keyword
		  :syntax #.*cm-cli-keyword-description-syntax*
		  :required t)
     )
  "Lock all subsystems which have been changed in the current workspace.  A workspace context is required.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.
   This argument is typically inferred from the client environment, rather than specified on the
   command line."
  (with-cm-cli-control (http-connection cm-session-context "master lock")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id)	;not server relative
    (guarantee-cd-in-workspace cm-session-context nil)
    ;; This is one of the rarer commands which doesn't require a file-system object.
    (cmctl-master-lock cm-session-context description)))

(define-cm-cli-command "master_unlock"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted by .csf file or similar mechanism
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required t)
     ;; There are no command arguments other than -q and -v, which aren't implemented yet for any command.
     )
  "Unlock all subsystems locked via master_lock.  The invoker of this command must be the owner of the
   workspace.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.
   This argument is typically inferred from the client environment, rather than specified on the
   command line."
  (with-cm-cli-control (http-connection cm-session-context "master unlock")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id)	;not server relative
    (guarantee-cd-in-workspace cm-session-context nil)
    ;; This is one of the rarer commands which doesn't require a file-system object.
    (cmctl-master-unlock cm-session-context)))

(define-cm-cli-command "master_unlock_force"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted by .csf file or similar mechanism
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     ;; regular command line arguments
     (subsystem :type :keyword :syntax #.*cm-cli-keyword-subsystem-name-syntax* :required nil)
     (product :type :keyword :syntax #.*cm-cli-keyword-pc-name-syntax* :required nil)
     (class :type :keyword :syntax #.*cm-cli-keyword-class-name-syntax* :required nil))
  "Unlock a subsystem that has been unreasonably locked.

   This command is to be used infrequently, for example when somebody
   locked the master and went on vacation.

   Command can only be used to unlock a single subsystem.

   This command may be called outside the context of a workspace. It must
   have at least one argument.

   Email is sent to the user who locked subsys, notifying that it has been unlocked.

   This command can fail if ChangeSafe is in the middle of an operation, such as master_change.
   "
  (with-cm-cli-control (http-connection cm-session-context "master unlock force")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :subsystem-name subsystem
			    :pc-name product
			    :class-name class)
    (cmctl-master-unlock-force cm-session-context)))

(define-cm-cli-command "product_deactivate"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     ;; This typically defaulted argument is optional unless product-name is ambiguous
     (product-name :type :positional :required t))
  "Deactivate a product so that it no longer appears in reports and general day-to-day activities.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   PRODUCT-NAME names the product to be deactivated."

  (with-cm-cli-control (http-connection cm-session-context "product deactivation")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :pc-name product-name)
    (cmctl-product-deactivate cm-session-context)))

(define-cm-cli-command "product_reactivate"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     ;; The following information may not be defaulted.
     (product-name :type :positional :required t))
  "Activate a product previously deactivated via product_deactivate.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   PRODUCT-NAME names the product to be re-activated."

  (with-cm-cli-control (http-connection cm-session-context "product reactivation")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :pc-name product-name)
    (cmctl-product-reactivate cm-session-context)))

(define-cm-cli-command "product_rename"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     ;; The following information may not be defaulted.
     (old-name :type :positional :required t)
     (new-name :type :positional :required t))
  "Rename a product.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   OLD-NAME names the product to be renamed.
   NEW-NAME is the new name for the product.

   In order to avoid confusion over product names and ensure that a references to a given product name
   always yield the same product, new product names must be unique.  This
   is true FOR ALL TIME.  What this means is that if a there was ever a product named 'X'
   there will never be another product named 'X' in the same repository.  So name your products carefully.

   When referencing a product, you can use any of the names that the product
   has ever had.  However when doing back-dated work or reporting, the product name will appear in all
   generated materials as it was at that time.

   Note that product_rename does NOT change the name of the reference directory associated with the product,
   you must use the product_directory_move command to perform that task."

  (with-cm-cli-control (http-connection cm-session-context "product name change")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :pc-name old-name)
    (cmctl-product-rename cm-session-context new-name)))

(define-cm-cli-command "subsys_rename"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     ;; The following information may not be defaulted.
     (old-name :type :positional :required t)
     (new-name :type :positional :required t))
  "Rename a subsystem.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   OLD-NAME is the current name of the subsystem to be renamed.
   NEW-NAME is the new name for the subsystem.

   In order to avoid confusion over names and ensure that a references to a given subsystem name
   always yield the same product, new subsystem names must be unique. This
   is true FOR ALL TIME.  What this means is that if a there was ever a subsystem named 'X'
   there will never be another subsystem named 'X' in the same repository.
   So name your subsystems carefully.

   When referencing a subsystem you can use any of the names that the subsystem
   has ever had.  However when doing back-dated work or reporting, the product name will appear in all
   generated materials as it was at that time."

  (with-cm-cli-control (http-connection cm-session-context "subsystem name change")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :subsystem-name old-name)
    (cmctl-subsys-rename cm-session-context new-name)))

(define-cm-cli-command "port"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax* :required t)
     (gui-switch :type :switch :syntax #.*cm-cli-switch-port-gui*)
     (act-switch :type :switch :syntax #.*cm-cli-switch-port-act*))
  "Port changes from inherited subsystems into this workspace.
   -gui is currently not yet supported.  Instead, use a web browser to access
   the ports page on the reports server.
   -act will bring the changes selectfrm the port report page into the workspace."
  (declare (ignore product-name))
  (with-cm-cli-control (http-connection cm-session-context "port")
    (conman-check-server-operating-mode :read/write)
    (unless (only-one-thing gui-switch act-switch)
      (conman-signal-error
       *cm-returns-error-bogus-arguments*
       "You must specify exactly one of ~@{~a~^, ~}."
       *cm-cli-switch-port-gui*
       *cm-cli-switch-port-act*))
    ;; Do we really care what his current working directory is?
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :ws-id ws-id)
    (guarantee-cd-in-workspace cm-session-context nil)
    (with-cm-cli-fsa (cm-session-context :server-relative nil)
      (cond (gui-switch
	     (conman-signal-error
	      *cm-returns-error-default*
	      "The ~a switch to the port command is not yet supported.  Use a web browser to ~
               access the port page for your workspace on the reports server."
	      *cm-cli-switch-port-gui*))
	    (act-switch
	     (cmctl-port-act cm-session-context))))))

(define-cm-cli-command "description_replace"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (server-relative :type :switch :syntax #. *cm-cli-switch-server-relative*)
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (product-name :type :keyword :syntax #.*cm-cli-keyword-pc-name-syntax* :required nil)
;;;; product-name is only needed for dealing with "release" descriptions
     ;; The rest of these args are not defaulted
     (description-text :type :keyword :syntax #.*cm-cli-keyword-description-syntax* :required t)
     (item-type :type :positional :required t)
     (item-name :type :positional :required t))
  "Replace the description previously given for something.

   Item being described can be anything that was created by user and had or could have
   had a description at time of creation.
   If the item had an abstract (a cset_close or master_change), the abstract may be replaced
   or left intact.

   The item types that can have their descriptions replaced are:  class, cset, database,
   label, product, release, subsystem, transaction, and workspace.  (A transaction is what
   gets created when you do master_change.)

   If the item type is workspace, then the item name is really the directory pathname where
   the workspaces resides."

  (with-cm-cli-control (http-connection cm-session-context "description replacement")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :pc-name product-name)
    (if (string-equal item-type "workspace")
	(with-cm-cli-fsa (cm-session-context :server-relative server-relative)
	  (let ((workspace-dir (cm-cli-parse-absolute-directory-specifier
				cm-session-context item-name server-relative)))
	    (cmctl-description-replace cm-session-context
				       description-text
				       item-type
				       workspace-dir)))
      (cmctl-description-replace cm-session-context
				 description-text
				 item-type
				 item-name))))

(define-cm-cli-command "description_query"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (server-relative :type :switch :syntax #. *cm-cli-switch-server-relative*)
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (product-name :type :keyword :syntax #.*cm-cli-keyword-pc-name-syntax* :required nil)
     ;; product-name is only needed for dealing with "release" descriptions
     ;; The rest of these args are not defaulted
     (item-type :type :positional :required t)
     (item-name :type :positional :required t))
  "Query the description previously given for something.

   The item being queried can be anything that was created by user and had or could have
   had a description at time of creation.

   The item types that can have their descriptions replaced are:  class, cset, database,
   label, product, release, subsystem, transaction, and workspace.  (A transaction is what
   gets created when you do master_close.)

   If the item type is workspace, then the item name is really the directory pathname where
   the workspaces resides."

  (with-cm-cli-control (http-connection cm-session-context "description query")
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :pc-name product-name)
    (if (string-equal item-type "workspace")
	(let ((item-descr))
	  (with-cm-cli-fsa (cm-session-context :server-relative server-relative)
	    (let ((workspace-dir (cm-cli-parse-absolute-directory-specifier
				  cm-session-context item-name server-relative)))
	      (setq item-descr (cmctl-description-query-show cm-session-context
							     item-type
							     workspace-dir))))

	  (cm-cli-description-query-show-display
	   http-connection item-type item-name item-descr))

      (cm-cli-description-query-show-display
       http-connection item-type item-name
       (cmctl-description-query-show cm-session-context
				     item-type
				     item-name)))))

(defun cm-cli-description-query-show-display (http-connection
					      item-type
					      item-name
					      item-description)
  "Output the information for description_query.
   Three values are input: the item type (e.g. cset), the item name, and
   the description of the item (note that the description may be nil)"
  (labels ((output-string (string)
	     (cm-cli-response :display string
			      (http-connection-stream http-connection)
			      :escape-it t))
	   )
    (output-string (format nil "The description of the ~a object named ~a is:" item-type item-name))
    (if item-description
	(output-string (format nil "~4t~s" item-description))
      (output-string (format nil "~4t\"\"")))
    )
  t)

(define-cm-cli-command "release_create"
    (http-connection
     cm-session-context
     (master-repository-name
      :type :keyword :syntax #.*cm-cli-keyword-master-syntax*         :required t)
     (ws-id :type :keyword :syntax #.*cm-cli-keyword-ws-id-syntax*    :required nil :secret t)
     (product      :type :keyword :syntax #.*cm-cli-keyword-pc-name-syntax*        :required t)
     (release      :type :keyword :syntax #.*cm-cli-keyword-release-name-syntax*   :required t)
     (description  :type :keyword :syntax #.*cm-cli-keyword-description-syntax*    :required nil)
     (date-time    :type :keyword :syntax #.*cm-cli-keyword-time-syntax*           :required nil)
     (label        :type :keyword :syntax #.*cm-cli-keyword-label-syntax*          :required nil)
     (from-release :type :keyword :syntax #.*cm-cli-keyword-from-release-syntax*
		   :required nil)
     )
  "Create a new branch named RELEASE of the PRODUCT, either from the main trunk or another branch
   (named FROM-RELEASE) of the product.

   Normally, the new branch branches from the tip of the specified branch, but that may be overridden
   by specification of the DATE-TIME or LABEL.
   Currently, LABEL is not yet supported.
  "
  (with-cm-cli-control (http-connection cm-session-context "release creation")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :ws-id ws-id
			    :repository-name master-repository-name
			    :pc-name product
			    :label-name label
			    :time-spec date-time
			    :release-name from-release)
    (guarantee-cd-not-in-workspace cm-session-context nil)
    (with-cm-cli-fsa (cm-session-context)
      (cmctl-create-product-release
       cm-session-context
       description
       release))))

(define-cm-cli-command "release_rename"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     (product  :type :keyword :syntax #.*cm-cli-keyword-pc-name-syntax* :required t)
     ;; The following information may not be defaulted.
     (old-name :type :positional :required t)
     (new-name :type :positional :required t))
  "Change the name of a release.

   Only fluid releases can be renamed. Once a release is frozen, its name is also frozen.
   Two releases for the same product cannot have the same name.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   OLD-NAME names the product to be renamed.
   NEW-NAME is the new name for the product.

   In order to avoid confusion over release names and ensure that a references to a given release name
   always yield the same release, new product names must be unique within the given product.  This
   is true FOR ALL TIME.  What this means is that if a there was ever a release named 'X'
   there will never be another release named 'X' for the same product.  So name your releases carefully.

   When referencing a release, you can use any of the names that the release
   has ever had.  However when doing back-dated work or reporting, the release name will appear in all
   generated materials as it was at that time."
  (with-cm-cli-control (http-connection cm-session-context "release name change")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :pc-name product
			    :release-name old-name)
    (cmctl-release-rename cm-session-context new-name)))

(define-cm-cli-command "server_version"
    (http-connection cm-session-context (args :type :rest))
  "Show information about the version of the ChangeSafe server."
  (declare (ignore args))
  (with-cm-cli-control (http-connection cm-session-context "server_version")
    (let ((dont-show-dynamic-text (and *within-regression-tests*
				       (not *cm-cli-doing-performance-run*))))
      (flet ((output (string)
	       (cm-cli-response :display string
				(http-connection-stream http-connection)
				:escape-it t)))
	(output "")
	(output
	 (if *changesafe-build-stamp*
	     (destructuring-bind (&key user host time) *changesafe-build-stamp*
	       (format nil  "ChangeSafe built:  ~a by ~a at ~a"
		       host user
		       (universal-time->iso-date-time-string time)))
	   (format nil "This is a development image of ChangeSafe." )))
	(output "     Copyright (c) 1999-2001 Content Integrity, Inc.  ALL RIGHTS RESERVED.")
	(output (format nil "Software version:          ~a"
			(if dont-show-dynamic-text
			    "<{Version}>"
			  (format nil "~d.~d"
				  user:*major-software-version* user:*minor-software-version*))))
	(output (format nil "Schema version:            ~d.~d"
			user:*major-schema-version* user:*minor-schema-version*))
	(output (format nil "File System Agent version: ~d"
			*socket-file-system-protocol-version*))
	(output "")
	(output (format nil "Server machine:    ~a"
			(if dont-show-dynamic-text
			    "<{Server}>"
			  (long-site-name))))
	(output (format nil "Server port:       ~a" *conman-server-log-start-port*))
	(output (format nil "Server process-id: ~a"
			(if dont-show-dynamic-text
			    "<{PID}>"
			  (excl::getpid)))) ;; Franz SPR 22543
	(output (format nil "Server log:        ~a"
			(if dont-show-dynamic-text
			    "<{Server Log Pathname}>"
			    *conman-server-log-name*)))
	(output (format nil "Server started:    ~a by ~a"
			(if dont-show-dynamic-text
			    "<{Date & Time}>"
			  (time-string *conman-server-log-start-time*))
			(sys:user-name)))
	(output (format nil "Server mode:       ~a" *conman-server-log-start-server-type*))
	(output (format nil "SMTP Server:       ~a" *conman-smtp-server*))
	(when (> (cm-session-context-verbosity
		  cm-session-context) 1)
	  (if (not *conman-check-in-email-product-distribution-lists*)
	      (output (format nil "Check-in Email lists: nil"))
	    (let ((*print-length* nil)) ;; show full length
	      (output (format nil "Check-in Email lists:"))
	      (dolist (val *conman-check-in-email-product-distribution-lists*)
		(output (format nil "                   ~a" val)))))
	  (output (format nil "Server ACL Version: ~a (~a)"
			  allegrostore::*allegrostore-version*
			  #+allegro-v6.0 "6.0"
			  #+allegro-v5.0.1 "5.0.1"
			  #-(or allegro-v6.0 allegro-v5.0.1) "?"))
	  (output (format nil "Server Threading:  ~a"
			  (if web::*http-server-default-multithreaded*
			      "Multi Threaded" "Single Threaded")))
	  (output (format nil "Memory Footprint:  ~a" utility::*changesafe-memory-footprint*))
	  (output (format nil "Server Cache Size: ~a" vm::*with-version-cid-set-cache-size*))
	  )
	(output (format nil "Server R/I:        ~a" (if (repository-referential-integrity) "On" "Off")))
	(output (format nil "Server Caching:    ~a" (if *with-version-cid-set-cache*
							(if dont-show-dynamic-text
							    "On, <{Cache Hit Counts}>"
							  (format
							   nil "On, ~d hits, ~d edits, ~d tossed, ~d misses"
							   (+ (utility::cache-mgr-object-num-hits-primary
							       *with-version-cid-set-cache*)
							      (utility::cache-mgr-object-num-hits-secondary-wd
							       *with-version-cid-set-cache*))
							   (utility::cache-mgr-object-num-hits-edits
							    *with-version-cid-set-cache*)
							   (utility::cache-mgr-object-num-hits-tosses
							    *with-version-cid-set-cache*)
							   (utility::cache-mgr-object-num-misses
							    *with-version-cid-set-cache*)))
						      "Off")))
	(output "")
	(output (format nil "Alternative Server URIs:"))
	(if (null (alternative-command-line-server-uri-strings))
	    (output (format nil "   <none>~%"))
	  (loop for alt-server in (alternative-command-line-server-uri-strings)
		do (output (format nil "   ~a" alt-server))))
	(output ""))
      t)))

(define-cm-cli-command "release_freeze"
    (http-connection
     cm-session-context
     (master-repository-name :type :keyword
			     :syntax #.*cm-cli-keyword-master-syntax*
			     :required t)
     (product :type :keyword
	      :syntax #.*cm-cli-keyword-pc-name-syntax*      :required t)
     (release :type :keyword
	      :syntax #.*cm-cli-keyword-release-name-syntax* :required t)
     )
  "Freeze a release of a product so no further changes may be made in that branch/release"
  (with-cm-cli-control (http-connection cm-session-context "release freeze")
    (conman-check-server-operating-mode :read/write)
    (cmctl-release-freeze (cm-session-context-add
			   cm-session-context
			   :repository-name master-repository-name
			   :pc-name product
			   :release-name release))))

(define-cm-cli-command "wall_raise"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     ;; We would rather the all remaining arguments were not defaulted.
     (product   :type :keyword :syntax #.*cm-cli-keyword-pc-name-syntax*        :required t)
     (release   :type :keyword :syntax #.*cm-cli-keyword-release-name-syntax*   :required nil)
     ;; The following arguments are mutually exclusive.
     (class     :type :keyword :syntax #.*cm-cli-keyword-class-name-syntax*     :required nil)
     (subsystem :type :keyword :syntax #.*cm-cli-keyword-subsystem-name-syntax* :required nil)
     )
  "Raise a \"wall\" that acts as a barrier to entry of csets into one or more subsystems in a given product.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   PRODUCT names the product whose subsystems will acquire wall-raised status.

   CLASS names a specific subsystem in the product for which a wall is to be raised.
   If this option is specified, subsystems corresponding to other classes in the product remain unaffected
   with respect to wall status.  This argument is mutually exclusive with the SUBSYSTEM argument.

   SUBSYSTEM names a specific subsystem in the product for which a wall is to be raised.
   If this option is specified, other product subsystems remain unaffected
   with respect to wall status.  This argument is mutually exclusive with the CLASS argument."
  (with-cm-cli-control (http-connection cm-session-context "wall raise")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :pc-name product
			    :release-name release
			    :subsystem-name subsystem
			    :class-name class)
    (cmctl-raise-wall cm-session-context)))

(define-cm-cli-command "wall_lower"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     ;; We would rather the all remaining arguments were not defaulted.
     (product   :type :keyword :syntax #.*cm-cli-keyword-pc-name-syntax*        :required t)
     (release   :type :keyword :syntax #.*cm-cli-keyword-release-name-syntax*   :required nil)
     ;; The following arguments are mutually exclusive.
     (class     :type :keyword :syntax #.*cm-cli-keyword-class-name-syntax*     :required nil)
     (subsystem :type :keyword :syntax #.*cm-cli-keyword-subsystem-name-syntax* :required nil)
     )
  "Lower a \"wall\" that acts as a barrier to entry of csets into one or more subsystems in a given product,
   and that was raised by wall_raise.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   PRODUCT names the product whose subsystem(s) already possess wall-raised status.

   CLASS names a specific subsystem in the product for which a wall is to be lowered.
   If this option is specified, subsystems corresponding to other classes in the product remain unaffected
   with respect to wall status.  This argument is mutually exclusive with the SUBSYSTEM argument.

   SUBSYSTEM names a specific subsystem in the product for which a wall is to be lowered.
   If this option is specified, other product subsystems remain unaffected
   with respect to wall status.  This argument is mutually exclusive with the CLASS argument."
  (with-cm-cli-control (http-connection cm-session-context "wall lower")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :pc-name product
			    :release-name release
			    :subsystem-name subsystem
			    :class-name class)
    (cmctl-lower-wall cm-session-context)))

(define-cm-cli-command "wall_hole"
    (http-connection
     cm-session-context
     ;; The following arguments are typically defaulted.
     (master-repository-name :type :keyword :syntax #.*cm-cli-keyword-master-syntax* :required t)
     ;; We would rather the all remaining arguments were not defaulted.
     (product   :type :keyword :syntax #.*cm-cli-keyword-pc-name-syntax*        :required t)
     (release   :type :keyword :syntax #.*cm-cli-keyword-release-name-syntax*   :required nil)
     ;; The following arguments are mutually exclusive.
     (class     :type :keyword :syntax #.*cm-cli-keyword-class-name-syntax*     :required nil)
     (subsystem :type :keyword :syntax #.*cm-cli-keyword-subsystem-name-syntax* :required nil)
     ;; Cset to be allowed passage
     (cset-name :type :positional :required t)
     )
  "Poke a hole through specified subsystems that will allow passage of the indicated cset in reponse to
   MASTER_CHANGE cset promotions and/or demotions.  Note that if multiple walls exist on a subsystem by
   multiple products, then all walls must have holes in them for the named cset, or the cset may not be
   promoted into or demoted out of the subsystem.

   MASTER-REPOSITORY-NAME names the master repository.  It should minimally be the file name
   component of a pathname like \"Jo\" in \"Jo.mdb\", and may also be a full server-relative pathname.

   PRODUCT names the product whose subsystem(s) already possess wall-raised status.

   CLASS names a specific subsystem in the product for which a wall is to be lowered.
   If this option is specified, subsystems corresponding to other classes in the product remain unaffected
   with respect to wall status.  This argument is mutually exclusive with the SUBSYSTEM argument.

   SUBSYSTEM names a specific subsystem in the product for which a wall is to be lowered.
   If this option is specified, other product subsystems remain unaffected
   with respect to wall status.  This argument is mutually exclusive with the CLASS argument."
  (with-cm-cli-control (http-connection cm-session-context "wall hole")
    (conman-check-server-operating-mode :read/write)
    (cm-session-context-add cm-session-context
			    :repository-name master-repository-name
			    :pc-name product
			    :release-name release
			    :subsystem-name subsystem
			    :class-name class)
    (cmctl-hole-wall cm-session-context cset-name)))

(defun convert-alias-type (type)
  "Given an alias TYPE, convert it to a keyword form.
   If the type is invalid, signal improper type given.
  "
  (let ((ans (cdr (assoc type '(("parameter" . :parameter)
				("command" . :command)
				("database" . :database)
				("email" . :email))
			    :test #'string=))))
    (unless ans
      (conman-signal-error *cm-returns-error-bogus-arguments*
			   "Improper type given."))
    ans))

(define-cm-cli-command "admin_alias_create"
    (http-connection
     cm-session-context
     (type :type :keyword
	   :syntax #.*cm-cli-keyword-type-syntax*
	   :required t)
     (alias-name :type :positional :required t)
     (alias-value :type :positional :required t)
     )
  "Create an alias- that is, an alternate name - for something.

  The TYPE argument, which must be one of parameter, database, command, or email,
  determines the kind of name that this alias will be a substitution for.

  The ALIAS-NAME argument is the new name, and the ALIAS-VALUE argument is the
  standard name (not itself an alias) the alias will be an additional name for.
 "
  (with-cm-cli-control (http-connection cm-session-context "alias create")

    (unless
	(cmctl-valid-name? alias-name)
      (conman-signal-error *cm-returns-error-invalid-change-name*
			   "Alias doesn't follow naming rules."))

    (case (convert-alias-type type)
      ((:parameter)
       (let ((d-name (concatenate 'string "-" alias-name))
	     (d-value (concatenate 'string "-" alias-value)))

	 (unless d-value
	   (conman-signal-error *cm-returns-error-too-few-arguments*
	      "Too few arguments supplied."))
	 (when (parameter-name-p d-name)
	   (conman-signal-error *cm-returns-error-bogus-arguments*
				"Alias is a reserved word."))

	 (unless (parameter-name-p d-value)
	   (conman-signal-error *cm-returns-error-bogus-arguments*
				"Expansion is not a legal parameter."))
	 (create-parameter-alias d-name d-value)))
       ((:command :database :email)
	 (conman-signal-error *cm-returns-error-unimplemented*
			      "Alias type not currently supported.")
       ))))

(defun do-param-alias-create (alias expansion)
  "Hook function to be called from changesafe-server-config.lsp.
  Creates a parameter alias named -ALIAS with expansion -EXPANSION.
  Both arguments should be strings.
  "
  (if (cmctl-valid-name? alias)
      (let ((d-name  (concatenate 'string "-" alias))
	    (d-value (concatenate 'string "-" expansion)))
	(cond
	 ((parameter-name-p d-name)
	  (format t "Alias (~a) is a reserved word.~%") alias)
	 ((not (parameter-name-p d-value))
	  (format t "Expansion (~a) is not a legal parameter.~%") expansion)
	 (t (create-parameter-alias d-name d-value)
	  (format t "Alias ~a for ~a created.~%" d-name d-value)
	 )
	))
    (format t "Alias (~a) doesn't follow naming rules.~%" alias)
    )
  (values))

(define-cm-cli-command "admin_alias_uncreate"
    (http-connection
     cm-session-context
     (type :type :keyword
	   :syntax #.*cm-cli-keyword-type-syntax*
	   :required t)
     (alias-name :type :positional :required t)
     )
  "Given an alias TYPE and ALIAS-NAME, delete that alias.
   For example, the command:
     admin_alias_uncreate -type parameter n
   will delete a parameter alias with the name n.
   Parameter aliases always have an understood - before them, so
   the effect is that a keyword of -n would no longer be acceptable
   after the above command (until a admin_alias_create reinstates
   the alias).
  "

  (with-cm-cli-control (http-connection cm-session-context "alias uncreate")
    (case (convert-alias-type type)
      ((:parameter)
       (let ((d-alias (concatenate 'string "-" alias-name)))
	 (unless (find-parameter-alias d-alias)
	   (conman-signal-error *cm-returns-error-no-such-alias*
				"No such alias is active."))
	 (delete-parameter-alias d-alias)))
      ((:command :database :email)
       (conman-signal-error *cm-returns-error-unimplemented*
			    "Alias type not currently supported.")
       ))))

(define-cm-cli-command "admin_alias_query"
    (http-connection
     cm-session-context
     (type :type :keyword
	   :syntax #.*cm-cli-keyword-type-syntax*
	   :required t)
     )
  "Given a TYPE, print out the sorted set of aliases of that type.
   For example, the command:
      admin_alias_query -type parameter
   will print out the entire current set of parameter aliases and
   their expansions.
  "
  (with-cm-cli-control (http-connection cm-session-context "alias query")
    (let (
	  (aliases ())
	  (width 5) ;; 5 is (length "alias")
	  (stream (http-connection-stream http-connection))
	  )
      (declare (fixnum width))

      (flet ((output (alias expansion)
		 (declare (string alias expansion))
		 ;; determine if 1 line is ok
		 (if (and (<= (length alias) width)
			  (<= (+ width (length expansion)) 78))
		     (cm-cli-response :display
				      (format nil "~VA ~A" width alias expansion)
				      stream
				      :escape-it t)
		   (progn
		     (cm-cli-response :display alias stream :escape-it t)
		     (cm-cli-response :display (format nil "    ~A" expansion)
				      stream :escape-it t)))))


	(funcall
	 (case (convert-alias-type type)
	   ((:parameter) #'map-parameter-aliases)
	   ((:command :database :email)
	    (conman-signal-error *cm-returns-error-unimplemented*
				 "Alias type not currently supported.")
	    ))
	 (lambda (alias expansion)
	     (push (cons alias expansion) aliases)
	     (setq width (max (length alias) width))
	     ))
	(setq aliases (sort aliases #'string< :key #'car))
	(setq width (min width 30))
	(output "Alias" "Expansion")
	(dolist (i aliases)
	  (output (car i) (cdr i))
	  )))
  t))
||#

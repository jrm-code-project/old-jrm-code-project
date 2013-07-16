;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2000-2005 ChangeSafe, LLC
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
;;;; File Name:     server-log.lsp
;;;; Author:        Peter White
;;;; Creation Date: August 2000
;;;;
;;;; Module Description:
;;;;
;;;; Data logging for ChangeSafe server
;;;; This module provides some variables and functions for gathering server
;;;; data.  It's a minimal implementation.
;;;;
;;;; Mode of operation:
;;;;
;;;; If logging is enabled, *conman-server-log* will be bound to a file
;;;; stream and the functions below can be used to log strings to the
;;;; stream.  There is a header that is added whenever a new log file
;;;; is started (see conman-server-log-start-info).
;;;;
;;;; Each message is timestamped and has a userid.
;;;;
;;;; See also conman.lsp for other special variables.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CONMAN")

(eval-when (:load-toplevel :execute)
  (export '(
	    conman-server-log-start-globals
            )))

(proclaim (standard-optimizations))

(defvar *conman-server-log-start-number* 0
  "the number of logs opened since the server was started")
(defvar *conman-server-log-start-port* nil
  "Port the server was started on")
(defvar *conman-server-log-start-server-state-file* nil
  "Server State File name when server was started")
(defvar *conman-server-log-start-server-type* nil
  "Type of server started (http or command-line)")
(defvar *conman-server-log-start-time* nil
  "Universal time when server was started")

(defun conman-server-log-start-globals (port server-type server-state-file)
  "sets the conman-server-log special variables so we have them when we
   print the log-start-info."
  (setq *conman-server-log-start-number* 0)
  (setq *conman-server-log-start-port* port)
  (setq *conman-server-log-start-server-state-file* server-state-file)
  (setq *conman-server-log-start-server-type* server-type)
  (setq *conman-server-log-start-time* (get-universal-time))
  )

#||

;;; API's exported from this module.
(eval-when (:load-toplevel :execute)
  (export '(conman-server-log-start-info
	    *conman-server-log-start-time*
	    *conman-server-log-start-number*
	    *conman-server-log-start-port*
	    *conman-server-log-start-server-type*
	    *conman-server-log-start*
	    *conman-server-log*
	    *conman-server-log-name*
	    *conman-server-log-memory-minimum-lisp*
	    *conman-server-log-memory-minimum-c*
	    *conman-server-log-memory-minimum-odi*
	    *conman-server-log-memory-errors*
	    *conman-server-log-show-memory-usage*
	    conman-server-log-close-stream
	    conman-server-log-open-stream
	    conman-server-log-string

	    ;; other things needed to be declared earlier
	    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; General Conman Server logging functions

;;;; You get a server log when the server starts regardless of what kind of server
;;;; it is (cli, http, whatever).  The log is a disk file with a name like:

;;;;    log-<cli or http>-server-<date e.g. 2000-08-16>

;;;; If the file does not exist, it will be created.  If it does exist, it will be
;;;; appended to.  At the moment, info about all commands, warnings, and errors
;;;; goes into the log (w/ timestamps).  We may add switches in the future to
;;;; control this more finely.

;;;; While each server gets a log started for it, there is no attempt
;;;; to handle having more than one process updating the log file
;;;; simultaneously.  This is likely to cause failures and loss of
;;;; information and is to be avoided.
;;;; *** SO WHY NOT USE THE NOTIFICATIONS MECHANISM DEFINED IN
;;;; utility\notification.lsp?

(defvar *conman-server-log* nil
  "The file Stream for logging server events")
(defvar *conman-server-log-name* nil
  "The name of the file for the server error log")

(defvar *conman-server-log-start* nil
  "controls whether or not the server log is started automatically when the server starts")


(defvar *conman-server-log-memory-minimum-lisp* #x04000000 ;; 64mb
  "minimum size of the lisp heap (lisp-heap-size)")
(defvar *conman-server-log-memory-minimum-c* #x01000000 ;; 16mb
  "minimum size of the c heap (c-heap-size)")
(defvar *conman-server-log-memory-minimum-odi* #x08000000 ;; 128mb, default differs by machine, this is smallest
  "minimum size of the ObjectStore data (OS_AS_SIZE)")
(defvar *conman-server-log-memory-errors* t
  "memory problems cause errors (not warnings)")

(defun conman-server-log-start-info (stream reason log-user)
  "Print standard stuff into the STREAM at server startup time.
   The PORT is the port number this server is listening to.
   The REASON is a string (typically either Server-Start-UP or New-Log-File).
   The SERVER-TYPE is either :http or :command-line.
   The SERVER-STATE-FILE is the name of the file containing the previous serevr state or nil.

   This can also be called at log start up time.
   "

  (let ((production-version nil))
    ;; Delineate this log start up
    (format stream "~&~%-----------------------------------------------------------------------------~%~%")

    ;; Show product version info
    (format stream "This Server is using ChangeSafe Product version ~d.~d~%"
	    user:*major-software-version* user:*minor-software-version*)
    (format stream "     and Schema version ~d.~d~%"
	    user:*major-schema-version* user:*minor-schema-version*)
    (format stream "     and File System Agent version: ~d~%"
	    *socket-file-system-protocol-version*)
    (if *changesafe-build-stamp*
	(progn
	  (destructuring-bind (&key user host time) *changesafe-build-stamp*
	    (format stream "     Built on ~a by ~a at ~a~%"
		    host user
		    (universal-time->iso-date-time-string time)))
	  (setq production-version t))
      (format stream "     This is a development image~%" ))

    ;; Show Machine info
    (format stream "~%The Server is:       ~a~%" (long-site-name))
    (format stream "    Also known as:   ~a~%" (machine-instance))
    (format stream "The machine type is: ~a~%" (machine-type))
    (format stream "    running:         ~a~%" (software-type))

    ;; show stuff that can vary with each invocation
    (format stream "~%The Server State File is: ~s~%" *conman-server-log-start-server-state-file*)
    (format stream "The Port is: ~d~%" *conman-server-log-start-port*)
    (format stream "The Process-ID is: ~a~%" (excl::getpid)) ;; Franz SPR 22543
    (format stream "The Server Log is: ~d~%" *conman-server-log-name*)
    (when *conman-server-log-name*
      (format stream "    and it is log #~d since the server was started~%"
	      *conman-server-log-start-number*)
      (format stream "    and was started by ~a~%" log-user))

    ;; Show today's date & time and who started the server
    (format stream "~%Server type: ~a~%"
	    *conman-server-log-start-server-type*)
    (format stream "Started:     ~a by ~a~%"
	    (time-string *conman-server-log-start-time*)
	    (sys:user-name))
    (format stream "Reason:      ~a~%~%" reason)

    ;; Show associated servers
    (format stream "~%Alternative Command Line Servers:~%")
    (let ((servers (alternative-command-line-server-uri-strings)))
      (if (null servers)
	  (format stream "   <none>~%")
	(loop for alt-server in servers
	      do (format stream "   ~a~%" alt-server))))

    ;; Show environment variables
    (main-report-environment :stream stream)

    ;; show lisp patches
    (format stream "~%~%AllegroStore Lisp Patches:~%")
    (if (null sys:*patches*)
	(format stream "   <none>~%")
      (if (listp sys:*patches*)
	  (loop for one-patch in sys:*patches*
		do
		(progn
		  (if (listp one-patch)
		      (progn
			(format stream "   ~a~%" (car one-patch))
			(loop for partial-patch in (cdr one-patch)
			      do (format stream "      ~a~%" partial-patch)))
		    (format stream "   ~a~%" one-patch))))
	(format stream "   ~a~%" sys:*patches*)))

    ;; Show dribble output
    (format stream "~%AllegroStore Dribble Data:~%")
    (format stream "~%AStore Referential Integrity is: ~a~%~%"
	    (if (repository-referential-integrity) "On" "Off"))
    (excl::print-dribble-bug-info stream) ;; Franz spr22781

    ;; Show memory usage (at this time)
    (conman-server-log-show-memory-usage stream production-version)

    ;; show various switch settings
    (conman-server-log-show-switch-settings stream production-version)

    ;; show garbage collection info
    (let ((*standard-output* (if (eq stream t) *standard-output* stream)))
      (format stream "~%~%Garbage Collection Switch Settings:~%~%")
      (sys:gsgc-parameters))

    (format stream "~%"))
  )

(defun conman-server-log-show-switch-settings (stream production-version)
  "show current settings of various switches"
  (declare (ignore production-version)
	   #+allegro (:fbound running-development-changesafe-server-p))
  (format stream "~%~%ChangeSafe Switch Settings (at ~a)~%~%" (time-string))
  (macrolet ((log-arg-val (arg)
		     `(format stream "~40a ~a~%"
			      ,(symbol-name arg) ,arg)))
    ;; please do these variables alphabetically within each section

    ;; the following are kind of iffy because the values now have no
    ;; particular relationship to the values in use when the FASL files
    ;; were built but we print them anyway in hopes they will help more
    ;; than hinder
    (log-arg-val utility::*debug-declaration*)
    (log-arg-val utility::*default-debug-declaration*)
    (log-arg-val utility::*default-safety-declaration*)
    (log-arg-val utility::*default-space-declaration*)
    (log-arg-val utility::*default-speed-declaration*)
    (log-arg-val utility::*safety-declaration*)
    (log-arg-val utility::*space-declaration*)
    (log-arg-val utility::*speed-declaration*)
    (format stream "~%")

    (let ((*standard-output* (if (eq stream t) *standard-output* stream))
	  (*error-output* (if (eq stream t) *standard-output* stream))
	  (*debug-io* (if (eq stream t) *standard-output* stream))
	  (*trace-output* (if (eq stream t) *standard-output* stream)))
      (when (running-development-changesafe-server-p)
	(excl::explain-compiler-settings)))
    (format stream "~%")
    (log-arg-val compiler::compile-format-strings-switch)
    (log-arg-val compiler::declared-fixnums-remain-fixnums-switch)
    (log-arg-val compiler::generate-inline-call-tests-switch)
    (log-arg-val compiler::generate-interrupt-checks-switch)
    (log-arg-val compiler::internal-optimize-switch)
    (log-arg-val compiler::peephole-optimize-switch)
    (log-arg-val compiler::save-arglist-switch)
    (log-arg-val compiler::save-local-names-switch)
    (log-arg-val compiler::save-local-scopes-switch)
    (log-arg-val compiler::tail-call-non-self-merge-switch)
    (log-arg-val compiler::tail-call-self-merge-switch)
    (log-arg-val compiler::trust-declarations-switch)
    (log-arg-val compiler::trust-dynamic-extent-declarations-switch)
    (log-arg-val compiler::verify-argument-count-switch)
    (log-arg-val compiler::verify-car-cdr-switch)
    (log-arg-val compiler::verify-non-generic-switch)
    (log-arg-val compiler::verify-symbol-value-is-bound-switch)

    (format stream "~%")
    )
  )

(defun conman-server-log-show-memory-usage (stream production-version)
  "show current memory usage and check for overlaps of our three main regions: the lisp-heap,
   the c-heap, and the ODI ObjectStore.  Our ability to check the overlap depends on Franz
   specific functions described in SPR 22555.  If this stops working in the future, ask Franz
   about that spr."
  ;; show the usage info (from room)
  (format stream "~%~%ChangeSafe Memory Usage (at ~a)~%~%" (time-string))
  (let ((*standard-output* (if (eq stream t) *standard-output* stream)))
    (room t))

  ;; Check for overlaps (uses Franz specific functions described in SPR 22555)
  (let* ((map (sys::gsgc-map nil))
	 (lisp-heap-end (aref map 6))
	 (lisp-heap-start (aref map 7))
	 (lisp-heap-size (- lisp-heap-end lisp-heap-start))
	 (os-as-start-str (sys:getenv "OS_AS_START"))
	 (os-as-size-str (sys:getenv "OS_AS_SIZE"))
	 os-as-start os-as-size os-as-end
	 (c-heap-start (aref map 13))
	 (c-heap-sbrk (aref map 14))
	 (c-heap-end (aref map 15))
	 (c-heap-size (- c-heap-end c-heap-start))
	 (mb (* 1024 1024))
	 had-overlap)
    (macrolet ((print-value (val)
		 `(format stream "~20a 0x~8,'0x (~a mb)~%" ,(symbol-name val)
			  (if ,val ,val 0) (and ,val (floor ,val mb)))))


      (format stream "~%~%ChangeSafe Memory Regions~%~%")
      (print-value lisp-heap-start)
      (print-value lisp-heap-end)
      (print-value lisp-heap-size)

      (when os-as-start-str
	(if (or (search "x" os-as-start-str) (search "X" os-as-start-str))
	    (setq os-as-start (parse-integer os-as-start-str :radix 16
					     :start (+ 1 (or (search "x" os-as-start-str)
							     (search "X" os-as-start-str)))))
	  (setq os-as-start (parse-integer os-as-start-str))))
      (when os-as-size-str
	(if  (or (search "x" os-as-size-str) (search "X" os-as-size-str))
	    (setq os-as-size (parse-integer os-as-size-str :radix 16
					    :start (+ 1 (or (search "x" os-as-size-str)
							    (search "X" os-as-size-str)))))
	  (setq os-as-size (parse-integer os-as-size-str))))
      (when (and os-as-start os-as-size)
	(setq os-as-end (+ os-as-start os-as-size)))
      (print-value os-as-start)
      (print-value os-as-end)
      (print-value os-as-size)

      (print-value c-heap-start)
      (print-value c-heap-end)
      (print-value c-heap-sbrk)
      (print-value c-heap-size)
      )

    ;; See if there are any overlaps with the 3 regions
    (flet ((inside (low-end x hi-end) (and (< low-end x) (< x hi-end))))
      (flet ((check-lisp (addr region-name)
	       (if (inside lisp-heap-start addr lisp-heap-end)
		   (progn (format stream "~%*** The lisp-heap overlaps with ~a ***~%~%" region-name)
			  (setq had-overlap t))))
	     (check-c (addr region-name)
	       (if (inside c-heap-start addr c-heap-end)
		   (progn (format stream "~%*** The c-heap overlaps with ~a ***~%~%" region-name)
			  (setq had-overlap t))))
	     (check-os (addr region-name)
	       (if (inside os-as-start addr os-as-end)
		   (progn (format stream "~%*** The OS_AS_Store overlaps with ~a ***~%~%" region-name)
			  (setq had-overlap t))))
	     )
	(check-lisp c-heap-start "c-heap-start")
	(check-lisp c-heap-sbrk "c-heap-sbrk")
	(check-lisp c-heap-end "c-heap-end")
	(when os-as-start
	  (check-lisp os-as-start "os-as-start"))
	(when os-as-end
	  (check-lisp os-as-end "os-as-end"))

	(check-c lisp-heap-start "lisp-heap-start")
	(check-c lisp-heap-end "lisp-heap-end")
	(when os-as-start
	  (check-c os-as-start "os-as-start"))
	(when os-as-end
	  (check-c os-as-end "os-as-end"))

	(when (and os-as-start os-as-end)
	  (check-os lisp-heap-start "lisp-heap-start")
	  (check-os lisp-heap-end "lisp-heap-end")
	  (check-os c-heap-start "c-heap-start")
	  (check-os c-heap-sbrk "c-heap-sbrk")
	  (check-os c-heap-end "c-heap-end"))
	))

    (when (< lisp-heap-size *conman-server-log-memory-minimum-lisp*)
      (format stream "~%*** The lisp-heap-size (~a) is too small, the minimum is ~a bytes ***~%~%"
	      lisp-heap-size *conman-server-log-memory-minimum-lisp*)
      (setq had-overlap t))
    (when (< c-heap-size *conman-server-log-memory-minimum-c*)
      (format stream "~%*** The c-heap-size (~a) is too small, the minimum is ~a bytes ***~%~%"
	      c-heap-size *conman-server-log-memory-minimum-c*)
      (setq had-overlap t))
    (when (and os-as-size (< os-as-size *conman-server-log-memory-minimum-odi*))
      (format stream "~%*** The OS_AS_Store (~a) is too small, the minimum is ~a bytes ***~%~%"
	      os-as-size *conman-server-log-memory-minimum-odi*)
      (setq had-overlap t))


    (format stream "~%")
    (finish-output stream)
    (when had-overlap
      (if (and *conman-server-log-memory-errors* production-version)
	  (error "Memory region overlap or size error: Check the log (~a) for details"
		 (if *conman-server-log-name* *conman-server-log-name* "console"))
	(warn "Memory region overlap or size error: Check the log (~a) for details"
	      (if *conman-server-log-name* *conman-server-log-name* "console"))))
    ))

(defun conman-server-log-open-stream (log-name user-name &key (reason "New Log File"))
  "Open the server logging stream (after closing the old one if needed).
  REASON is either Server-Start-Up or New-Log-File (as a string).
  "
  ;; close existing log, if any
  (when *conman-server-log*
    (let ((condition (conman-server-log-close-stream user-name)))
      (when condition
	(conman-signal-warning
	 *cm-returns-warning-error-closing-log-stream*
	 "A condition was raised while attempting to close the server log stream: ~a"
	 condition))))

  (when log-name
    ;; attempt to open new log
    (let* ((log-path (merge-pathnames log-name (excl:current-directory)))
	   (log-file (open
		      log-path
		      :direction :output
		      :if-exists :append
		      :if-does-not-exist :create
		      )))
      (when (open-stream-p log-file)
	(setq
	 *conman-server-log-name* (namestring log-path)
	 *conman-server-log* log-file
	 *conman-server-log-start-number* (+ *conman-server-log-start-number* 1)
	 *core-server-log-function* #'conman-server-log-string
	 *utility-server-log-function* #'conman-server-log-string)

	;; put in the header info
	(conman-server-log-start-info *conman-server-log* reason user-name)
	(finish-output *conman-server-log*)
	)))
  )

(defun conman-server-log-close-stream (user-name)
  "Close the server logging stream.

   If we are unable to close the stream, return the suppressed condition which was signalled in closing
   the stream, or T if the condition is unknown.  Otherwise return NIL (indicating success)."
  ;; We don't signal an error here, because the caller may be sensitive to the way errors are handled
  ;; and reported.
  (when *conman-server-log*
    (conman-server-log-show-memory-usage *conman-server-log* nil)
    (conman-server-log-string user-name "Shutdown" "---------- By Command ----------")
    (multiple-value-bind (result condition)
	(ignore-errors (close *conman-server-log*))
      (setq *conman-server-log* nil
	    *conman-server-log-name* nil
	    *core-server-log-function* nil
	    *utility-server-log-function* nil)
      (and (null result) condition))))

(defun conman-server-log-string (user-name what message)
  "Log the MESSAGE in the *conman-server-log* along with the time of day.
   The user-name gives us the userid.
   The WHAT is a label (string) like ERROR or COMMAND to identify the kind of message."
  #||
  (debug-message 0 "PWWDB: server-log: ~a, ~a" what message)
  (debug-message 0 "PWWDB: PID: ~a Port: ~a tod: ~a"
		 (excl::getpid) ;; Franz SPR 22543
		 *conman-server-log-start-port*
		 (time-string (get-universal-time) :no-days t))
  ||#
  (when *conman-server-log*
    (multiple-value-bind (result condition)
	(ignore-errors
	 (let* ((user-string (if user-name
				 (format nil ", WHO: ~a" user-name) "")))
	   (format *conman-server-log* "~%~A~a~%~a: ~A~%"
		   (time-string (get-universal-time) :no-days t) user-string
		   what message))
	 (finish-output *conman-server-log*))
      (declare (ignore condition))
      ;; What should we do here?  We couldn't write the log.
      result))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Report on the execution environment of the server

(defparameter *main-interesting-environment-variables*
    '(
      "AS_CONFIG_PATH"
      "LD_LIBRARY_PATH"
      "OS_ACTION_SET_INITIAL_SIZE"
      "OS_AS_SIZE"
      "OS_AS_START"
      "OS_CACHE_DIR"
      "OS_CACHE_SIZE"
      "OS_COMMSEG_DIR"
      "OS_COMMSEG_RESERVE_SIZE"
      "OS_COMMSEG_SIZE"
      "OS_PROTECT_INFO_SEGMENT"
      "OS_RELOPT_THRESH"
      "OS_RESERVE_AS"
      "OS_ROOTDIR"
      "PATH"
      "SHLIB_PATH"
      )
  "System environment variables which should be reported on in the server
   log output.")

(defun main-report-environment (&key (stream t))
  "Dump values of interesting environment variables."
  (format stream "~&~%Environment variables:")
  (loop for env-name in *main-interesting-environment-variables*
	for val = (sys:getenv env-name)
	do (format stream "~&  ~a~30,5t ~:[unassigned~;~s~]" env-name val val))
  (format stream "~%"))
||#

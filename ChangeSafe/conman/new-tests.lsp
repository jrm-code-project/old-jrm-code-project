;;;; -*- Mode: LISP; coding: iso-8859-1; Syntax: COMMON-LISP; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2005 ChangeSafe, LLC
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

(in-package "CONMAN")

(proclaim (standard-optimizations))

(defvar *conman-test-listener-process* nil
  "Dynamically bound to the current CONMAN test listener process.")

(defparameter +conman-test-port-base+ 7001
  "The port number that will be used by the conman test suite.")

(defparameter +conman-secondary-test-port-base+ 9501
  "An alternative port number that will be used by the conman test suite.")

(defun conman-test-port ()
  "Returns the test port for running the conman tests.
   On Windows NT, this is the +conman-test-port-base+, but on HP-UX,
   the current user-id is added to the test-port-base in order to
   ensure that multiple users can simultaneously run tests."
  (+ #+(or mswindows :win32)            0
     #+(and unix allegro) (excl::getuid)
     +conman-test-port-base+))

(defun test-changesafe-server-uri ()
  "The URI for contancting the ChangeSafe server in the regression tests."
  (format nil "http://localhost:~d/" (conman-test-port)))

(defun test-changesafe-server-uri-alternative ()
  "The URI for contancting the ChangeSafe server in the regression tests."
  (format nil "http://localhost:~d/" (conman-secondary-test-port)))

(eval-when (:load-toplevel)
  (make-instance 'regression-test-suite
                 :name "CONMAN"
                 :directory (translate-logical-pathname "CSF:CONMAN;")))

(defconstant +conman-test-dot-conman-file-name+ ".test-conman"
  "The NON-repository relative name of the .csf file as used by the conman test system.
   This value is used to replace the value in *cmctl-dot-conman-file-name* (after
   conversion to a repository relative name.")

(defparameter +test-java-client-relax-interval+
    #+(or mswindows :win32) nil
    #+hpux nil #| 60 |#)

(defvar *test-java-client-last-run* nil)

(defun test-java-client (class command-line-args listener-function
                         &key (directory (get-temporary-directory))
                              expected-status
                              (changesafe-server-uri (test-changesafe-server-uri))
                              (java-out-file (translate-logical-pathname #p"CSF:CONMAN;test-output;java.out"))
                              (java-err-file (translate-logical-pathname #p"CSF:CONMAN;test-output;java.err")))
  "Invoke a JVM on CLASS with COMMAND-LINE-ARGS.
   Capture the command output and display it to *standard-output*.
   Returns the exits status of the command.

   If DIRECTORY is passed, is should be a path specification which names a directory.
   The client command will be invoked in the context of this directory.

   Assume the java program will establish a connection to the lisp server, and invoke
   LISTENER-FUNCTION to handle the request.  LISTENER-FUNCTION takes no arguments.
   However any output it makes to standard/error/output is of interest, since it is typically
   captured by the regression tests.  So if we fire this up on another thread, we want to
   ensure that we still redirect its output to the same streams as the parent (this) function.
   If LISTENER-FUNCTION is NIL, then we assume that a server was started via some other means.

   EXPECTED-STATUS can be a numeric status code or a list of same.  If EXPECTED-STATUS is
   specified and the resulting client operation does not result in one of the expected status
   codes then a breakpoint is enterred."

  ;; Under HP-UX, there's a time lag between when the server closes
  ;; it's socket and when the port is available to be opened for a new
  ;; socket.  If the next server invocation tries to open a port
  ;; before the port is free, it gets an error of type
  ;; EXCL:SOCKET-ERROR (with a EXCL::SOCKET-ERROR-IDENTIFIER of
  ;; :ADDRESS-IN-USE).  We introduce a time delay here to be sure the
  ;; OS has had a chance to free the port.
  ;; This is presumably remedied by the :REUSE-ADDRESS argument to
  ;; SOCKET:MAKE-SOCKET.
  (when (and +test-java-client-relax-interval+
             *test-java-client-last-run*)
    (let ((delta (- (+ *test-java-client-last-run*
                       +test-java-client-relax-interval+)
                    (get-universal-time))))
      (when (> delta 0)
        (sleep delta))))

  ;; Make sure the java code has been compiled
  ;; (java-tools:java-compile ".")              ; must be called "outside" the test harness.
  ;; Start the java agent in the background.

  ;; **HACK** *FINISH* USE MULTI THREADING IN LISP
  ;; This test relies on the fact that java takes longer to start than it takes us to
  ;; get to the listener after starting it.  Timing error likely.   Bad bad bad.
  ;; The multi-threaded approach needs to observe stream variables in this test context
  ;; for this to work.
  (ensure-directories-exist directory)
  (let* ((server-directory (os-get-current-directory))
         (conman-error-code
          (call-with-java-agent
           :java-class               class
           :java-argument-string     command-line-args
           :java-properties          (test-cm-command-java-properties
                                      :conman-server-uri changesafe-server-uri
                                      :conman-rc-file-name +conman-test-dot-conman-file-name+)
           :java-classpath-additions (list (translate-logical-pathname #p"CSF:JAVA;fsa;"))
           :java-output-pathname     (translate-logical-pathname java-out-file)
           :java-error-pathname      (translate-logical-pathname java-err-file)
           :java-current-directory   directory
           :grab-conman-error-code   t
           :receiver (lambda (start-java-agent)
                       (signal-semaphore start-java-agent)

                       (if (or (null listener-function)
                               *conman-test-listener-process*)
                           (signal-semaphore start-java-agent)
                           (let (
                                        ; (web:*http-server-semaphore* start-java-agent)
                                        ; (excl::*print-autoload* nil) ;suppress autoloader messages, SPR18868
                                 )
                             (with-new-current-directory server-directory
                               (funcall listener-function))))))))

    (setq *test-java-client-last-run* (get-universal-time))
    (when expected-status
      (unless (if (listp expected-status)
                  (member conman-error-code expected-status)
                (eql conman-error-code expected-status))
        (break "While testing ~s, got return code ~d when ~a was expected."
               command-line-args
               conman-error-code expected-status)))
    conman-error-code))

(defun test-cm-command-java-properties (&key (conman-server-uri
                                              (test-changesafe-server-uri))
                                             (conman-rc-file-name +conman-test-dot-conman-file-name+))
  "Return a value to be used for the :PROPERTIES argument of
   JAVA-TOOLS:JAVA-COMMAND-LINE which are appropriate for testing CHangeSafe."
    `((CHANGESAFE_SERVER_URI ,conman-server-uri)
      (RCFILENAME ,conman-rc-file-name)
      (SET_FILE_READ_ONLY ,(format nil "~s" (os-read-only-command)))
      (SET_FILE_READ_WRITE ,(format nil "~s" (os-read-write-command)))
    #+unix ,@(when (os-test-executable-command)
               `((GET_FILE_EXECUTABLE
                  ,(format nil "~s" (os-test-executable-command)))))
    #+unix ,@(when (os-executable-command)
               `((SET_FILE_EXECUTABLE
                  ,(format nil "~s" (os-executable-command)))))
    #+unix ,@(when (os-not-executable-command)
               `((SET_FILE_NOT_EXECUTABLE
                  ,(format nil "~s" (os-not-executable-command)))))
      ,@(when (os-set-file-last-modified-command)
          `((SET_FILE_LAST_MODIFIED
             ,(format nil "\"~{~a ~a ~a~}\""
                      (os-set-file-last-modified-command)))))))

(defun test-1a-listener-get-method (req ent)
  "The GET method function used by TEST-1A-LISTENER."
  (debug-message 5 "test-1a-listener-get-method ~s ~s" req ent)
  (net.aserve:with-http-response (req ent :content-type "text/plain")
    (net.aserve:with-http-body (req ent)
      (format net.aserve::*html-stream* "Hello World!~c~c" #\return #\linefeed)
      ;; Avoid printing URL 'cause it has a port number in it.
      ;; So we just print the elements.
      (dolist (acons (uri/query (net.aserve:request-uri req)))
        (format net.aserve::*html-stream* "Uri arg key: ~s, uri arg val: ~s~c~c" 
                (car acons) (cdr acons) #\return #\linefeed))
    ))
  (throw 'exit-server nil))

(defun http-server-start (publisher &key port reuse-port-p)
  (let ((http-server (make-instance 'net.aserve:wserver)))
    (let ((net.aserve:*wserver* http-server))
      (net.aserve:unpublish :all t)

      (funcall publisher)

      (catch 'exit-server
        (net.aserve:start
         :listeners 0
         :port port))
      (debug-message 3 "Server exited.")
      nil)))

(defun test-http-server-start (publisher &key verbose)
  "Wrapper function so that various globals get set properly for the conman tests
   when starting the CSF server."
  (when verbose
    (format t "~%Starting ConMan CLI HTTP server on port ~a ..." (conman-test-port)))

  ;; Make sure that various globals have appropriate values so that the server will run properly
  (conman-server-log-start-globals (conman-test-port) :command-line nil)

  ;; server must be run with caching
  (if csf/core::*repository-cache-active*  ;; avoid recursion
      (http-server-start publisher :port (conman-test-port)
                         :reuse-port-p t
                         )
    (with-cached-open-repositories ()
      (debug-message 3 "Starting http-server")
      (http-server-start publisher :port (conman-test-port)
                         :reuse-port-p t
                         ))
    ))

(defun test-1a-listener ()
  "A port listener for TEST-1A."
  (test-http-server-start
   (lambda () 
     (net.aserve:publish
      :function #'test-1a-listener-get-method 
      :path "/"))
   :verbose t
   )
  )

(define-regression-test test-1a ()
  "Temporary test of ServerRequest.java, as a CLI request shell built on HTTP protocols"
  (test-java-client (make-pathname :name "ServerRequest" 
                                   :defaults (translate-logical-pathname #p"CSF:JAVA;cm;"))
                    (format nil "http://localhost:~d/ hi" (conman-test-port))
                    #'test-1a-listener))

(define-regression-test test-2a ()
  "Test the CLI-REQUEST module."        ;this test belongs in whatever package CLI-REQUEST lives in.
  (flet ((doit (args descriptors)
           (format t "~%Testing args ~s and descriptors ~s" args descriptors)
           (multiple-value-bind (bindings unmatched-descriptions unmatched-args)
               (cli-request/parse-ordered-args args descriptors)
             (format t "~%Result bindings: ~s" bindings)
             (format t "~%Unmatched descriptors: ~s" unmatched-descriptions)
             (format t "~%Unmatched arguments: ~s" unmatched-args))
           (terpri)))
    (multiple-value-bind (result condition)
        (ignore-errors (doit '("-f" "foo")
                             '((force-p :type :switch :syntax "-f")
                               (unforce-p :type :switch :syntax "-f")))) ;catch duplicate syntax
      (assert (null result))
      (format t "~%Expected syntax error: ~a" condition))
    ;; Technically speaking, these pathnames do not have to be underscored.
    ;; But we might as well be consistent.
    ;; NO!  Since they are not real pathnames, the pathname canonicalization code
    ;; in core/test-substrate.lisp won't be able to canonicalize them properly.
    ;; Since, from the point of view of this test, they are just strings, don't
    ;; put in underscores so that the pathname canonicalization will leave them alone.
    (doit '() '((ignore :type :rest :required nil)))
    (doit '("-mdb" "e:\\ts50\\conman\\root-tempdir\\server-tempdir\\jo" "-prod" "akron" "-ws" "1")
          '((MASTER-REPOSITORY-NAME :TYPE :KEYWORD :SYNTAX "-mdb" :REQUIRED T)
            (WS-ID :TYPE :KEYWORD :SYNTAX "-ws" :REQUIRED T)))
    (doit '("-mdb" "e:\\ts50\\conman\\root-tempdir\\server-tempdir\\jo" "-ws" "1")
          '((MASTER-REPOSITORY-NAME :TYPE :KEYWORD :SYNTAX "-mdb" :REQUIRED T)
            (WS-ID :TYPE :KEYWORD :SYNTAX "-ws" :REQUIRED T)))
    (doit '("pcl" "-dir" "pcl\\src" "-mdb" "foo.mdb" "-desc" "blah blah blah")
          '((master-repository-name :type :keyword :syntax "-mdb" :required t)
            (subdirectory-name :type :keyword :syntax "-dir" :required t)
            (cm-class-name :type :positional :required t)
            (description :type :keyword :syntax "-desc")))
    (doit '("-f" "-dir" "test-4a-temp\\dbdir\\" "-mdb" "jo")
          '((force-p :type :switch :syntax "-f")
            (dir-spec :type :keyword :syntax "-dir" :required t)
            (master-name :type :keyword :syntax "-mdb" :required t)))
    (doit '("-dir" "test-4a-temp\\dbdir\\" "jo")
          '((FORCE-P :TYPE :SWITCH :SYNTAX "-f")
            (DIR-SPEC :TYPE :KEYWORD :SYNTAX "-dir" :REQUIRED T)
            (MASTER-NAME :TYPE :KEYWORD :SYNTAX "-mdb" :REQUIRED T)))
    (doit '("-b" "bar")
          '((foo-var :type :keyword :required t :syntax "-f")
            (bar-var :type :keyword :required t :syntax "-b")
            (baz-var :type :switch :syntax "-baz")))
    (doit '("-q" "foo")
          '((foo-var :type :keyword :required t :syntax "-q")
            (bar-var :type :switch :syntax "-b" :required t)))
    (doit '("hello" "world")
          '((hello-var :type :positional)
            (world-var :type :positional)))
    (doit '("hello" "world")
          '((hello-var :type :positional)
            (world-var :type :positional :required nil)))
    (doit '("hello")
          '((hello-var :type :positional)
            (world-var :type :positional :required nil)))
    (doit '("hello")
          '((hello-var :type :positional)
            (world-var :type :positional :required t)))
    (doit '("hello" "world")
          '((hello-var :type :positional)))
    (doit '("-force" "-reason" "because I want to" "filea" "fileb" "filec")
          '((force-var :type :switch :syntax "-force")
            (reason-var :type :keyword :syntax "-reason")
            (file-var :type :positional :required t)
            (other-files-var :type :rest)))
    (doit '("-v0" "-v1" "-v2" "co")
          '((v0 :type :switch :syntax "-v0")
            (v1 :type :switch :syntax "-v1")
            (v2 :type :switch :syntax "-v2")
            (command-name :type :positional :required nil)))
    ))

(defun test-3a ()
  "Test the CM.java module and attendant protocol with some basic and innocuous CM CLI commands."
  ;; NOTE: this test restarts the lisp listener for each command we run, since we currently
  ;; don't run the lisp listener on a separate thread.

  ;; Unlike many java-client class we make from lisp tests, this particular client
  ;; is responsible for deducing its lisp server to which it connects from environment
  ;; or other install-time variables for the conman project.  We're just simulating a user
  ;; invocation here.
  (let ((cm-class (make-pathname :name "CM" :defaults (translate-logical-pathname "CSF:JAVA;cm;"))))
    (flet ((CM (command-string expected-status)
             (format t "~%Testing CM ~a~&" command-string)
             (test-java-client cm-class
                               command-string
                               #'test-3-listener
                               :expected-status expected-status)))
      (CM (format nil "product_create ~a my-description fred" +cm-cli-keyword/description+)
          *cm-returns-error-too-few-arguments*)
      (CM "class_create" *cm-returns-error-too-few-arguments*)

      ;; since the PID is inherently different (pretty much) everytime
      ;; and that would screw up the regression comparisons, take advantage
      ;; of the escape that shutdown will ignore the need for a pid if we
      ;; *within-regression-tests* and just pass in anything-is-ok

      ;; This one has too many args, though the server will quit anyway because of
      ;; binding *cm-cli-testing-shutdown*
        (CM "shutdown -port 123 -pid anything-is-ok please" *cm-returns-error-too-many-arguments*)
        (CM "shutdown please" *cm-returns-error-too-few-arguments*)
        (CM "shutdown -port 1 -pid anything-is-ok" *cm-returns-error-bogus-arguments*)
        (CM (format nil "shutdown -port ~a -pid anything-is-ok" (conman-test-port))
            *cm-returns-success*))))

(defun test-3-listener (&optional (shutdown-p t))
  "A port listener for TEST-3A.  This one runs the production CM-CLI dispatcher."
  (let ((*cm-cli-testing-shutdown* shutdown-p))

    ;; Make sure that various globals have appropriate values so that the server will run properly
    (conman-server-log-start-globals (conman-test-port) :command-line nil)

    ;; (changesafe:server-top-level)

    ;;(changesafe-server-start :operating-mode :read/write
    ;;                       :port (conman-test-port)
    ;;                       :reuse-port-p t)
    (test-http-server-start
     (lambda ()
       (net.aserve:publish
        :function  #'cm-cli-http-server-get
        :path "/csf/command/product_create"))
     :verbose t)
    ))

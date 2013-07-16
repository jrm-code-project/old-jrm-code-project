;;;; -*- Mode: LISP; coding: iso-8859-1; Syntax: COMMON-LISP; Base: 10; -*-
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
;;;; Module Description:
;;;; This file contains the startup routine and other facilities for the
;;;; ChangeSafe server application.  Eventually this file will include routines
;;;; to process Lisp command line arguments, maintain server logs, etc.
;;;;
;;;; Author:        Mark Nahabedian
;;;; Creation Date: 1999-12-30
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CHANGESAFE")

;;; API's exported from this module.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(csf/config::*http-server-port*
            csf/config::*http-server-worker-threads*
            csf/config::*http-server-multithreaded*
            csf/config::*log-directory*
            csf/config::*http-log-file*
            )
          "CSF/CONFIG")
  (export '(
            SERVER-TOP-LEVEL
            QUIT
            ;; start-server
            get-server-install-directory
            get-server-launch-directory
            )))

(proclaim (standard-optimizations))

(defparameter *server-install-directory* nil
  "Absolute directory where the server executable is installed.
   Do not use this.  Call (get-server-install-directory) instead.")

(defparameter *server-launch-directory* nil
  "Directory that was `current' when the server was launched.
   Do not use this.  Call (get-server-launch-directory) instead.")

(defvar *http-server-multithreaded* nil
  "T if using worker threads to handle requests.")

(defvar *http-server-port* 8000
  "TCP port monitored by ChangeSafe server.")

(defvar *http-server-worker-threads* 5
  "Maximum number of worker threads serving changesafe requests.")

(defvar *log-directory* (make-pathname :directory '(:relative "log")
                                       :defaults "")
  "The subdirectory in which to put the log files.")

(defvar *http-log-file* (make-pathname :name "http"
                                       :type "log"
                                       :defaults "")
  "The subdirectory in which to put the log files.")

(defparameter +changesafe-server-config-file-name+
    "changesafe-server-config.lsp"
  "Name of file (in the directory where the ChangeSafe server software is installed)
   from which configuration parameters can be read.")

(defparameter +changesafe-debug-config-file-name+
    "changesafe-debug-config.lsp"
  "Name of file (in the directory where the ChangeSafe server software is installed)
   from which debug configuration parameters can be read.")  ;; development only

(defparameter +changesafe-server-config-command-port-number+ 7999
  "Port number to be used for debug command line client requests.")

(defparameter +changesafe-debug-config-command-port-number+ 6666
  "Port number to be used for debug command line client requests.")

(defparameter +changesafe-server-config-browser-port-number+ 8000
  "Port number to be used for browser/report client requests.")

(defparameter +changesafe-debug-config-browser-port-number+ 6667
  "Port number to be used for browser/report client requests.")

(defvar *allow-shutdown* nil
  "When T, shutdown page causes the server to exit.")

(defparameter +command-line-options+
  (list (make-instance 'keyword-flag-command-line-argument
                       :key :debug
                       :type 'boolean
                       :compute-default-value (lambda () nil)
                       :description-key :debug-command-line-option)
        (make-instance 'keyword-single-command-line-argument
                       :key :log-directory
                       :type 'absoulte-directory
                       :compute-default-value (lambda () nil)
                       :description-key :log-directory-command-line-option))
  "A list of command line argument specifiers for the ChangeSafe server.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Here is the function which starts the server.

;;; This is launched with no arguments from top level in the
;;; delivered process.  You can also launch it within the current
;;; process by calling it with an appropriate set of command-line arguments.

(defun server-top-level (&key (operating-mode :standalone)
                              (command-line-args (sys:command-line-arguments)))
  "This is the entry point for starting the ChangeSafe server.
   It takes care of any initial setup and then starts the server.
   COMMAND-LINE-ARGS is the list of strings found on the command line.
   (In Allegro, these would be found after the '--' argument on the command line)."
  (pstore::close-all-open-persistent-stores);; sometimes happens during debugging
  (incf *template-cache-tick*)
  (get-server-launch-directory)
  ;; Establish the pathname translations for the server.
  (server-path-translations (get-server-install-directory))
  (csf/server::setup-repository-logical-host)

  (flet ((find-command-line-arg (key-string)
           (member key-string command-line-args
                   :test (lambda (key cmd)
                           (and (> (string-length cmd) 1)
                                (or (char-equal (char cmd 0) #\-)
                                    #+:win32 (char-equal (char cmd 0) #\\))
                                (string-equal (symbol-name key) (subseq cmd 1)))))))

    (let ((debug (find-command-line-arg :debug)))
      (when (load-server-configuration-file
             (or (second (find-command-line-arg :config-file))
                 (if debug
                     +changesafe-debug-config-file-name+
                     +changesafe-server-config-file-name+)))
        (apply #'process-server-command-line
               (command-line->argument-list
                (server-platform)
                (sys:command-line-arguments)
                +command-line-options+))))))

(defun prerelease-warning ()
  (format *terminal-io*
          "ChangeSafe Server ~d.~d PRERELEASE"
          (and (boundp '*major-software-version*) *major-software-version*)
          (and (boundp '*minor-software-version*) *minor-software-version*))
  (mapc (lambda (line)
          (terpri *terminal-io*)
          (write line :escape nil :stream *terminal-io*))
        '(
          "This software is a prerelease version and is not fully functional."
          "It has been provided for evaluation purposes only."
          "The final product may differ substantially from this version.")))

(defun get-server-install-directory ()
  "Returns the pathname of the directory of the executable image of the server.
   When developing, this is always the workspace directory."
  (or *server-install-directory*
      (setq *server-install-directory*
            (if (running-development-changesafe-server-p)
                (changesafe-source-pathname)
                (merge-pathnames
                 (make-pathname :name nil
                                :type nil
                                :defaults (parse-namestring (car (sys:command-line-arguments))))
                 (get-server-launch-directory))))))

(defun get-server-launch-directory ()
  "Returns the pathname of the directory current when the server was launched."
  (or *server-launch-directory*
      (setq *server-launch-directory*
            (csf/utility:os-get-current-directory))))

(defun server-path-translations (server-directory)
  "Set the logical path translations for the server."
  ;; Don't reset them if they are already set.
  (when (null (ignore-errors (logical-pathname-translations "CSF")))
    (labels ((relative-subdirectory (&rest elements)
               (merge-pathnames
                (make-pathname :directory `(:relative ,@elements :wild-inferiors)
                              :name :wild
                              :type :wild
                              :defaults "")
                server-directory))

             (wild-suffix (prefix)
               (concatenate 'string (string-upcase prefix) ";**;*.*"))

             (standard-translation (subdir)
               (list (wild-suffix subdir) (relative-subdirectory subdir))))

      (setf (logical-pathname-translations "CSF")
            (list (standard-translation "web-content")
                  (list ";" server-directory)
                  (list "**;*.*" (relative-subdirectory)))))))

(defun process-server-command-line (&key debug log-directory)
  ;; (format t "~&Debug is: ~s~&Log-directory is: ~s" debug log-directory)
  ;; wrong, but for demo
  (declare (ignore debug log-directory))
  (start-server))

(defun changesafe-server-directory-pathname (&optional pathname)
  "With no argument, return the pathname of the directory containing the
   ChangeSafe server software.  If PATHNAME is provided, the result of merging
   it with the ChangeSafe server directory is returned."
  (let ((dir (if (running-development-changesafe-server-p)
                 ;; Running ChangeSafe in a development lisp image.
                 ;; test-config is a directory where we can put the
                 ;; configuration files for a server being run from a
                 ;; development lisp.
                 (let ((dir (translate-logical-pathname
                             "CSF:SEMITEMP;distribution-changesafe;test-config;")))
                   (ensure-directories-exist dir)
                   dir)
               ;; Running a delivered ChangeSafe image
               (translate-logical-pathname "CSF:"))))
    (if pathname
        (merge-pathnames pathname dir)
      dir)))

(defun load-server-configuration-file (filename)
  "The ChangeSafe server has a configuration file which is loaded by this
   function at server startup time.  Returns T on success, NIL on failure."
  (let ((config-file-pathname
         (changesafe-server-directory-pathname filename)))
    (if (probe-file config-file-pathname)
        (handler-case (progn (load config-file-pathname :verbose nil :print nil) t)
          (error (condition)
                 (format *error-output* "~&Error loading configuration file ~s" config-file-pathname)
                 (format *error-output* "~&~s" condition)
                 nil))
        (progn
          (format *error-output* "~&Configuration file ~s not found." config-file-pathname)
          ;; If were developing, punt on the config file.
          (running-development-changesafe-server-p)))))

(defun running-development-changesafe-server-p ()
  "Returns true if we're running in a development image, false for a delivered image."
  (not (and (boundp '*changesafe-build-stamp*)
            (symbol-value '*changesafe-build-stamp*))))

(defun call-with-possible-output-file (generate-file-name receiver)
  (let ((output-file (funcall generate-file-name)))
    (if (null output-file)
        (funcall receiver '())
        (progn
          (ensure-directories-exist
           (pathname-syntactic-parent output-file))
          (with-open-file (stream output-file
                                  :direction :output
                                  :if-exists :append
                                  :if-does-not-exist :create)
            (unwind-protect
                (funcall receiver stream)
              (finish-output stream)
              (close stream)))))))

(defun get-http-log-file ()
  (changesafe-server-directory-pathname
   (make-pathname :directory (list :relative "log")
                  :name "http"
                  :type "log"
                  :defaults "")))

(defun get-http-debug-file ()
  (changesafe-server-directory-pathname
   (make-pathname :directory (list :relative "log")
                  :name "http-debug"
                  :type "log"
                  :defaults "")))

(defun call-with-http-logging (receiver)
  (call-with-possible-output-file #'get-http-log-file receiver))

(defun call-with-http-debugging (receiver)
  (call-with-possible-output-file
   #'get-http-debug-file
   (lambda (debug-stream)
     (when debug-stream
       (net.aserve::debug-on :info))
     (funcall receiver debug-stream))))

;; wserver-log-function not used.

;; wserver-log-stream  (initially to *standard-output*)
;; wserver-terminal-io

;;; logmess outputs to this:  *aserve-debug-stream*
;;; *debug-stream*
;;; *enable-logging*

;;; start takes a debug-stream, then sets *aserve-debug-stream*
;;; to that value

;;; log-request write the standard http log message
;;; to wserver-log-stream

(defun start-server ()
  (call-with-http-logging
   (lambda (http-log-stream)
     (debug-message 3 "http-log-stream is ~s" http-log-stream)
     (call-with-http-debugging
      (lambda (http-debug-stream)
        (debug-message 3 "http-debug-stream is ~s" http-debug-stream)
        (let ((http-server (make-instance 'net.aserve:wserver
                                          :log-stream http-log-stream
                                          )))

          ;; Bind server variables so things can find it.
          (let ((net.aserve:*wserver* http-server)
                (net.aserve::*debug-stream* http-debug-stream))

            (csf/core:with-cached-open-repositories ()

              ;; Install the commands, images, etc.
              (publish-changesafe http-server)

              ;; Start the server.
              ;; If the server is in this thread, then this call won't
              ;; return until the server shuts down.
              (handler-case (net.aserve:start
                             :debug-stream http-debug-stream
                             :listeners (if *http-server-multithreaded*
                                            *http-server-worker-threads*
                                            0)
                             :port *http-server-port*)
                ;; The server will throw an stream closed error when
                ;; it shuts down.
                (changesafe-stream-closed-error (condition)
                                                (declare (ignore condition))
                                                (debug-message 1 "Main socket closed."))))

            (let ((accept-thread (net.aserve::wserver-accept-thread http-server)))
              (when accept-thread
                ;; Sleep until the server quits.
                (mp:process-wait "Awaiting web server shutdown."
                                 (complement #'mp:process-alive-p) accept-thread))))))))))

;;; Index page redirect.
(defun index-redirect-function (req ent)
  "Redirect requests from the root url to index.lsp"
  (net.aserve:with-http-response (req ent
                                      :response net.aserve:*response-moved-permanently*)
    (setf (net.aserve:reply-header-slot-value req "location") "index.htm")
    (net.aserve:with-http-body (req ent)
      ;; this is most likely unnecessary since most
      ;; browsers understand the redirect response
      (net.aserve::html
       (:html
	(:head (:title "Object Moved"))
	(:body
	 (:h1 "Object Moved")
	 "The page you're looking for is now at "
	 ((:a :href "index.htm") "this location")
	 "."))))))

(defun parse-accept-header (header)
  (and header
       (map 'list (lambda (option)
                    (let ((split (split-string option #\;)))
                      (cons (string-trim " " (car split))
                            (map 'list (lambda (param)
                                         (split-string param #\=))
                                 (cdr split)))))
            (split-string header #\,))))

(defun get-accept-header (req header)
  (parse-accept-header
   (net.aserve::header-slot-value req header)))

(defun get-locale (req)
  (cond ((get-accept-header req :accept-language)
         => (lambda (x)
              (parse-locale (caar x))))
        (t nil)))

(defun content-acceptable? (header-info content)
  "Check that header info allows us to sent content."
  (let ((probe1 (assoc content header-info :test #'string-equal)))
    (when probe1
      ;; should check Q factor
      t)))


(defun charset-acceptable? (header-info charset)
  "Check that header info allows us to sent content."
  (let ((probe1 (assoc charset header-info :test #'string-equal)))
    (when probe1
      ;; should check Q factor
      t)))

(defun ensure-acceptable-client (receiver)
  "Make sure that the client accepts text/html and UTF-8.
   If so, invoke receiver after establishing the output stream.
   If not, return an Error 406 to the client and do not invoke receiver."
  (lambda (req ent)
    (labels ((unacceptable-client-response ()
               (net.aserve:with-http-response (req ent :response (net.aserve::make-resp 406 "Not acceptable"))
                 (net.aserve:with-http-body (req ent)
                   (net.aserve::html
                    (:html (:title "Error 406 - Not acceptable")
                           (:body "Error 406 - Not acceptable"))))))

           (ensure-accept-format (receiver)
             (let ((accept (get-accept-header req :accept)))
               (if (or (null accept)    ; no accept header means anything goes
                       (content-acceptable? accept "text/html")
                       (content-acceptable? accept "text/*")
                       (content-acceptable? accept "*/*"))
                   (funcall receiver)
                   (unacceptable-client-response))))

           (ensure-accept-charset (receiver)
             (let ((charset (get-accept-header req :accept-charset)))
               (if (or (null charset)
                       (charset-acceptable? charset "utf-8"))
                   (funcall receiver)
                   (unacceptable-client-response)))))
      (debug-message 1 "Request for ~s" (net.aserve:request-uri req))
      (ensure-accept-format
       (lambda ()
         (ensure-accept-charset
          (lambda ()
            (net.aserve:with-http-response (req ent)
              (setf (net.aserve:reply-header-slot-value req "Expires") 
                    (net.aserve::universal-time-to-date (+ (get-universal-time) 0)))
              (net.aserve:with-http-body (req ent :external-format :utf-8)
                ;; replace the output stream with a UTF-8 converter
                (setf (net.aserve:request-reply-stream req)
                      (make-instance 'utf-8-output-stream
                                     :underlying-stream (net.aserve:request-reply-stream req)))
                (let ((net.aserve::*html-stream* (net.aserve:request-reply-stream req)))
                  ;; emit BOM so client knows
                  (write-char #\U+FEFF (net.aserve:request-reply-stream req))
                  (funcall receiver req ent)))))))))))

(defun emit-xhtml-header (stream)
  "Write to <stream> the magic characters that are supposed to be at the start
   of every XHTML file."
  (write-string "<?xml version=\"1.0\" encoding=\"utf-8\"?>" stream)
  (terpri stream)
  (write-string "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" stream)
  (terpri stream)
  (write-string "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">" stream)
  (terpri stream))

(defun emit-xhtml-trailer (stream)
  "Write to <stream> the magic characters that are supposed to be at the end
   of every XHTML file.  This balances any tags opened by EMIT-XHTML-HEADER."
  (write-string "</html>" stream)
  (terpri stream))

(defun html-script (&rest script-lines)
  (net.aserve::html
   (:newline)
   ((:script :type "text/javascript" :language "javascript")
    (:newline)
    (mapc (lambda (line)
            (net.aserve::html
             (:princ line)
             (:newline)))
          (cons "//<![CDATA[" script-lines))
    "//]]>"
    (:newline))))

;;; Cheesy, but damn effective!
(defun disable-back-button ()
  (html-script
   "  history.forward();"))

(defun divbreak ()
  (net.aserve::html
   ((:div style "clear: left; font-size: 1px;") "&nbsp;")))

(defmacro xhtml (req &body body)
  (with-unique-names (command ticket comment success-page fail-page)
   `(PROGN
      (EMIT-XHTML-HEADER (NET.ASERVE:REQUEST-REPLY-STREAM ,req))
      (MACROLET ((FILE-SYSTEM-AGENT (,COMMAND ,TICKET ,COMMENT ,SUCCESS-PAGE ,FAIL-PAGE)
       `(NET.ASERVE::HTML
         ((:div :id "fsa")
          (:P ,,comment)
          ((:object
            :archive "FSTestApplet.jar"
            :code "FSTestApplet.class"
            :id "file-system-agent"
            :width  "100%"
            :height "300"
            ;; :standby "Loading file system agent..."
            :type "application/x-java-applet"
            :src "FSTestApplet.class"
            )
           ((:param :name "URL"
                    :value (render-uri (extend-uri-query
                                        (net.aserve:request-uri req)
                                        '(:absolute "file-system-agent.htm")
                                        `((:command . ,',,command)
                                          (:hatcheck-ticket . ,,,ticket)))
                                       nil)))
           ((:param name "cabbase"    value "FSTestApplet.cab"))
           ((:param name "SUCCESSURL" value (render-uri
                                             (extend-uri-query
                                              (net.aserve:request-uri req)
                                              `(:absolute ,',,success-page)
                                              `((:hatcheck-ticket . ,,,ticket)))
                                             nil)))
           ((:param name "FAILURL"    value (render-uri
                                             (extend-uri-query
                                              (net.aserve:request-uri req)
                                              `(:absolute ,',,fail-page)
                                              `((:hatcheck-ticket . ,,,ticket)))
                                             nil)))
           ((:param name "DEBUG"      value "DEBUG"))
           ;; Ensure that the FSA can manipulate the browser.
           ((:param name "MAYSCRIPT"  value "true"))
           (:p "Alternate applet text"))
          (divbreak)))))
        (NET.ASERVE::HTML ,@body))
      (EMIT-XHTML-TRAILER (NET.ASERVE:REQUEST-REPLY-STREAM ,req)))))

(defun standard-page-title (title locale)
  (net.aserve::html
   (:princ (localized-string :changesafe-product-name locale))
   " &#8212; "
   (:princ (localized-string title locale))))

(defun standard-html-head (title locale)
  (net.aserve::html
   (:head
    (:title (standard-page-title title locale))
    (disable-back-button)
    (:newline)
    ;; External style sheet
    ((:link rel "STYLESHEET" type "text/css" href "/style.css"))
    (:newline)
    ;; External script
    ((:script charset "ISO 8859-1" :language "Javascript" :src "/changesafe.js"))
    (:newline)
    ((:meta http-equiv "Content-Script-Type" content "text/javascript"))
    (:newline)
    ((:meta http-equiv "Pragma" content "no-cache"))
    (:newline)
    ((:meta http-equiv "Cache-control" content "no-cache"))
    (:newline)
    ((:meta http-equiv "Expires" content "0"))
    (:newline))))

(defun standard-page-header (title locale)
  (net.aserve::html
   ((:div id "pageheader")
    ((:div id "banner") (standard-page-title title locale))
    (divbreak))))

(defun standard-page-footer (locale)
  (net.aserve::html
   ((:div id "footer")
    ((:div id "copyright")
     (:princ (localized-string :copyright locale)) " "
     (:princ (localized-string :vendor locale)) " "
     (:princ (localized-string :all-rights-reserved locale)))
    ((:div id "versioninfo")
     (:princ (localized-string :changesafe-product-name locale)) " "
     " Version" (:princ 1) "." (:princ 9) " Build ####")
    (divbreak))))
   

(defun missing-template (req ent locale)
  (declare (ignore ent))
  (XHTML REQ
         (standard-html-head :missing-template locale)
         (:body
          (standard-page-header :missing-template locale)

          ((:div id "pagebody")
           (:princ-safe (format-in-language nil
                                       :template-is-missing
                                       locale
                                       (net.aserve:request-uri req))))
          (standard-page-footer locale))))


(defun register-master-checkbox (master-id)
  (html-script
   (format nil "  register_master_checkbox (document.getElementById (\"~a\"));" 
           master-id)))

(defun register-slave-checkbox (master-id slave-id)
  (html-script
   (format nil "  register_slave_checkbox (document.getElementById (\"~a\"), document.getElementById (\"~a\"));"
           master-id 
           slave-id)))

(defun register-form (form-id)
  (html-script
   (format nil "  register_form (\"~a\");" form-id)))

(defun compile-template (file)
  (debug-message 3 "Compiling GUI template ~s" file)
  (with-open-file (stream file :direction :input
                          :element-type 'character
                          :if-does-not-exist :error)
    (handler-case
        (compile
         nil
         `(LAMBDA (REQ ENT LOCALE QUERY)
            (DECLARE (IGNORABLE ENT LOCALE QUERY))
            (MACROLET ((DEFINE-PAGE (NAME &BODY BODY)
                         `(XHTML REQ
                                 (STANDARD-HTML-HEAD ,name LOCALE)

                                 (:BODY
                                  (STANDARD-PAGE-HEADER ,name LOCALE)

                                   ((:DIV :ID "pagebody")
                                    (:H1 (:PRINC (LOCALIZED-STRING ,name LOCALE)))
                                    ,@body
                                    (divbreak)
                                    (when-debugging 3
                                        (let ((query (uri/query (net.aserve:REQUEST-URI REQ))))
                                          (NET.ASERVE::HTML (:h4 "Debugging info follows"))
                                          (when (and query
                                                     (not (eq query :unspecific)))
                                            (NET.ASERVE::HTML
                                             (:TABLE
                                              (:TR ((:TH) "Query Key")
                                                   ((:TH) "Query Value"))
                                              (ITERATE (((key value) (scan-query query)))
                                                (NET.ASERVE::HTML
                                                 (:TR ((:TD) (:PRINC-SAFE key))
                                                      ((:TD) (:PRINC-SAFE value)))))))
                                            (let ((ticket (uri-query/lookup query :hatcheck-ticket)))
                                              (when ticket
                                                (let ((hatcheck (csf/utility::hatcheck/%peek ticket)))
                                                  (net.aserve::html
                                                   (:table
                                                    (:tr ((:TH) "Hatcheck Key")
                                                         ((:TH) "Hatcheck Value"))
                                                    (iterate (((key value) (scan-query hatcheck)))
                                                      (net.aserve::html
                                                       (:tr ((:td) (:princ-safe key))
                                                            ((:td) (:princ-safe value)))))))))))
                                          (net.aserve::html 
                                           (:h4 "End of debugging info")
                                           (divbreak)))))

                                   (STANDARD-PAGE-FOOTER LOCALE)))))

            ,@(let ((*package* (find-package "CHANGESAFE")))
                (do* ((form (read stream nil :eof) (read stream nil :eof))
                      (result (list form) (cons form result)))
                    ((eq form :eof) (nreverse (cdr result))))))))
      (error (condition)
        (compile-template-error file condition)))))

(defun compile-template-error (file condition)
  (lambda (req ent locale)
    (declare (ignore ent))
    (XHTML REQ
           (standard-html-head :template-error locale)

           (:body
            (standard-page-header :template-error locale)

            ((:div id "pagebody")
             "Error while compiling template."
             (:princ-safe file)
             (:princ-safe condition))

            (standard-page-footer locale)))))

(defvar *template-cache-tick* 0
  "Increment this to flush the template cache.")

(defun render-template (template-path post?)
  (check-type template-path absolute-pathname)
  (let ((template-file (merge-pathnames (translate-logical-pathname #p"CSF:WEB-CONTENT;TEMPLATES;") template-path))
        (template-cach-tick *template-cache-tick*)
        (template-cache     #'missing-template)
        (template-timestamp nil))
    (labels ((get-arguments (req)
              (if post?
                  (let ((query (parse-uri-query (net.aserve::get-request-body req))))
                    (debug-message 5 "Getting query from body: ~s" query)
                    query)
                  (let ((query (uri/query (net.aserve::request-uri req))))
                    (debug-message 5 "Getting query from uri: ~s" query)
                    query)))

             (render (template req ent)
               (funcall template req ent (get-locale req) (get-arguments req)))

             (render-from-cache (req ent)
               (debug-message 3 "Rendering ~s from cache." template-file)
               (render template-cache req ent)))

      (ensure-acceptable-client
       (lambda (req ent)
         (debug-message 2 "Rendering template for ~s" (net.aserve:request-uri req))
         (if (and (= template-cach-tick *template-cache-tick*)
                  (numberp template-timestamp)
                  (= template-timestamp (os-get-timestamp template-file)))
             (render-from-cache req ent)
             (let ((new-timestamp (os-get-timestamp template-file))
                   (template (compile-template template-file)))
               (if (null template)
                   (render-from-cache req ent)
                   (progn (setq template-cache template)
                          (setq template-timestamp new-timestamp)
                          (render template req ent))))))))))

(defvar *fsa-commands*
  ;; should compute this, but for now...
  '(
    (:add-changes      . conman::cmctl/add-and-remove-changes)
    (:change-create    . conman::cmctl/change-create)
    (:change-files     . conman::cmctl/change-files)
    (:change-selected  . conman::cmctl/change-selected)
    (:create-workspace . conman::cmctl/create-workspace)
    (:delete-change    . conman::cmctl/change-delete)
    (:delete-workspace . conman::cmctl/delete-workspace)
    (:master-change    . conman::cmctl/master-change)
    (:update-workspace . conman::cmctl/update-workspace)
    (:sync-workspace   . conman::cmctl/sync-workspace)
    )
  "A table of commands that can be run with an FSA.")

(defun process-fsa-request (req ent)
  (net.aserve:with-http-response (req ent :timeout (* 60 60))
    (net.aserve:with-http-body (req ent)
      (debug-message 5 "Process fsa-request ~s ~s" req ent)
      (when (socket::socket/output-chunking? (net.aserve:request-reply-stream req))
        ;; turn of chunking and fool allegroserve into not sending a final chunk
        (socket::socket-control (net.aserve:request-reply-stream req)
                                :input-chunking nil
                                :output-chunking nil
                                :output-chunking-eof t)
        (setf (net.aserve::request-reply-strategy req)
              (delete :chunked (net.aserve::request-reply-strategy req) :test #'eq)))
      (let ((fsa     nil)
            (success nil))
        (unwind-protect
            (progn
              (setq fsa (make-instance 'fsa-file-system
                                       :stream (net.aserve:request-reply-stream req)))
              (let* ((query           (uri/query (net.aserve:request-uri req)))
                     (command         (uri-query/lookup query :command))
                     (hatcheck-ticket (uri-query/lookup query :hatcheck-ticket))
                     (hatcheck-value  (and hatcheck-ticket (hatcheck/retrieve hatcheck-ticket))))
                (debug-message 5 "uri query is ~s" (uri/query (net.aserve:request-uri req)))
                (debug-message 5 "hatcheck-value is ~s" hatcheck-value)
                (if (null command)
                    (error "No command.")
                    (let ((handler (assoc command *fsa-commands*)))
                      (if (null handler)
                          (error "No handler for command ~s" command)
                          (setq success
                                (funcall (cdr handler)
                                         (dbpath/parse
                                          (namestring
                                           (translate-logical-pathname
                                            #p"CSF:REPOSITORY;repository.ydm")))
                                         fsa
                                         [repository.core-user.1]
                                         query
                                         :hatcheck-ticket hatcheck-ticket
                                         :hatcheck-value hatcheck-value)))))))
          "Shutdown FSA."
          (when fsa
            (file-system/shutdown fsa :success success)))))))

(defun publish-dtds ()
  ;; Publish the DTD in case anyone asks.
  (net.aserve:publish-file
   :content-type "text/plain"
   :file #p"CSF:WEB-CONTENT;STATIC;xhtml1-strict.dtd"
   :path "/DTD/xhtml1-strict.dtd")

  (net.aserve:publish-file
   :content-type "text/plain"
   :file #p"CSF:WEB-CONTENT;STATIC;xhtml-lat1.ent"
   :path "/DTD/xhtml-lat1.ent")

  (net.aserve:publish-file
   :content-type "text/plain"
   :file #p"CSF:WEB-CONTENT;STATIC;xhtml-special.ent"
   :path "/DTD/xhtml-special.ent")

  (net.aserve:publish-file
   :content-type "text/plain"
   :file #p"CSF:WEB-CONTENT;STATIC;xhtml-symbol.ent"
   :path "/DTD/xhtml-symbol.ent"))

(defun publish-certificates ()
  (net.aserve:publish-file
   :content-type "application/x-x509-ca-cert"
   :file #p"CSF:WEB-CONTENT;STATIC;x509.cacert"
   :path "/certificate.cacert"))

(defun publish-changesafe (http-server)
  "Install the changesafe command set."
  (declare (ignore http-server))

  ;; Clear all the old pages
  (net.aserve:unpublish :all t)

  (net.aserve:publish-file
   :content-type "text/css"
   :file #p"CSF:WEB-CONTENT;STATIC;style.css"
   :path "/style.css")

  (net.aserve:publish-file
   :content-type "application/x-javascript"
   ;; :content-type "text/javascript" ;; not valid, but de-facto standard  sigh.
   :file #p"CSF:WEB-CONTENT;STATIC;JS;changesafe.js"
   :path "/changesafe.js")

  (net.aserve:publish-file
   :content-type "image/x-icon"
   :file #p"CSF:WEB-CONTENT;STATIC;ci402.ico"
   :path "/favicon.ico")

  (publish-dtds)
  (publish-certificates)

  (net.aserve:publish-file
   :content-type "application/octet-stream"
   :file #p"CSF:WEB-CONTENT;APPLETS;FSTestApplet.cab"
   :path "/FSTestApplet.cab")

  (net.aserve:publish-file
   :content-type "application/x-java-vm"
   :file #p"CSF:WEB-CONTENT;APPLETS;FSTestApplet.jar"
   :path "/FSTestApplet.jar")

;;  ;; Publish class files
;;  (mapc (lambda (entry)
;;          (destructuring-bind (uri file) entry
;;            (net.aserve:publish-file
;;             :content-type "application/x-java-vm"
;;             :file (make-pathname :name file
;;                                  :type "class"
;;                                  :defaults (translate-logical-pathname
;;                                             #p"CSF:WEB-CONTENT;APPLETS;"))
;;             :path uri)))
;;        '(
;;          ("/FSAAppletThread.class"            "FSAAppletThread")
;;          ("/FSAProgressIndicator.class"       "FSAProgressIndicator")
;;          ("/FSAProgressIndicatorApplet.class" "FSAProgressIndicatorApplet")
;;          ("/FSAProgressIndicatorPanel.class"  "FSAProgressIndicatorPanel")
;;          ("/FSTestApplet.class"               "FSTestApplet")
;;          ("/FileSystemAgent.class"            "FileSystemAgent")
;;          ("/FileSystemAgentException.class"   "FileSystemAgentException")
;;          ("/FileSystemAgentProtocol.class"    "FileSystemAgentProtocol")
;;          ("/HttpFileSystemAgent.class"        "HttpFileSystemAgent")
;;          ("/HttpImpl.class"                   "HttpImpl")
;;          ("/OSServices.class"                 "OSServices")
;;          ("/OSServicesInterface.class"        "OSServicesInterface")
;;          ("/Queue.class"                      "Queue")
;;          ("/SunServices.class"                "SunServices")
;;          ("/crc.class"                        "crc")
;;          ))


  (net.aserve:publish
   :function (lambda (req ent)
               (process-fsa-request req ent))
   :path "/file-system-agent.htm")

  ;; redirect "/" -> "/index.htm"
  (net.aserve:publish
   :content-type "application/xhtml+xml"
   :function #'index-redirect-function
   :path "/")

  ;; GUI related redirects
  (net.aserve:publish
   :content-type "application/xhtml+xml"
   :path "/regenerate-workspace.htm"
   :function (lambda (req ent)
               (debug-message 4 "Workspace-sync-commit request ~s" (net.aserve:request-uri req))
               (net.aserve:with-http-response (req ent
                                                   :response net.aserve:*response-moved-permanently*)
                 (let* ((query           (uri/query (net.aserve:request-uri req)))
                        (workspace-id    (uri-query/lookup query :workspace-id))
                        (new-location    (render-uri (extend-uri-query
                                                      (net.aserve:request-uri req)
                                                      '(:absolute "workspace-sync.htm")
                                                      `())
                                                     nil)))
                   ;; mark workspace for regeneration
                   (conman::cmctl/regenerate-workspace (dbpath/parse
                                                (namestring
                                                 (translate-logical-pathname
                                                  #p"CSF:REPOSITORY;repository.ydm")))
                                         [repository.core-user.1]
                                         query)
                   (debug-message 4 "Redirecting to ~s" new-location)
                   (setf (net.aserve:reply-header-slot-value req "location") new-location)
                   ;;
                   (net.aserve:with-http-body (req ent)
                     ;; this is most likely unnecessary since most
                     ;; browsers understand the redirect response
                     (net.aserve::html
                      (:html
                       (:head (:title "Redirect to workspace sync."))
                       (:body (:h1 "Redirect to workspace sync.")
                              ((:a :href "/workspace-sync.htm") "Click me.")))))))))

  (net.aserve:publish
   :content-type "application/xhtml+xml"
   :path "/workspace-sync-commit.htm"
   :function (lambda (req ent)
               (debug-message 4 "Workspace-sync-commit request ~s" (net.aserve:request-uri req))
               (net.aserve:with-http-response (req ent
                                                   :response net.aserve:*response-moved-permanently*)
                 (let* ((query           (uri/query (net.aserve:request-uri req)))
                        (hatcheck-ticket (uri-query/lookup query :hatcheck-ticket))
                        (hatcheck-value  (and hatcheck-ticket (hatcheck/retrieve hatcheck-ticket)))
                        (workspace-id    (and hatcheck-value (uri-query/lookup hatcheck-value :workspace-id)))
                        (new-location (render-uri (extend-uri-query
                                                   (net.aserve:request-uri req)
                                                   '(:absolute "workspace-info.htm")
                                                   `((:workspace-id . ,workspace-id)))
                                                  nil)))

                   (conman::cmctl/sync-commit
                    (dbpath/parse
                     (namestring
                      (translate-logical-pathname
                       #p"CSF:REPOSITORY;repository.ydm")))
                    [repository.core-user.1]
                    query
                    :hatcheck-ticket hatcheck-ticket
                    :hatcheck-value hatcheck-value)

                   (debug-message 4 "Redirecting to ~s" new-location)
                   (setf (net.aserve:reply-header-slot-value req "location") new-location)

                   (net.aserve:with-http-body (req ent)
                     ;; this is most likely unnecessary since most
                     ;; browsers understand the redirect response
                     (net.aserve::html
                      (:html
                       (:head (:title "Redirect to workspace info."))
                       (:body (:h1 "Redirect to workspace info.")
                              ((:a :href "/workspace-info.htm") "Click me.")))))))))

  (net.aserve:publish
   :content-type "application/xhtml+xml"
   :path "/workspace-sync-abort.htm"
   :function (lambda (req ent)
               (net.aserve:with-http-response (req ent
                                                   :response net.aserve:*response-see-other*)
                 (setf (net.aserve:reply-header-slot-value req "location")
                       "/workspace-info.htm")
                 (net.aserve:with-http-body (req ent)
                   ;; this is most likely unnecessary since most
                   ;; browsers understand the redirect response
                   (net.aserve::html
                    (:html
                     (:head (:title "Redirect to workspace info."))
                     (:body (:h1 "Redirect to workspace info.")
                            ((:a :href "/workspace-info.htm") "Click me."))))))))


  ;; Publish the GUI pages
  (mapc (lambda (entry)
          (destructuring-bind (uri file post-method?) entry
            (debug-message 4 "Publishing ~s" uri)
            (net.aserve:publish
             ;; :authorizer authorizer
             :content-type "application/xhtml+xml"
             :function (render-template
                        (make-pathname
                         :name file
                         :type "lsp"
                         :defaults (translate-logical-pathname
                                    #p"CSF:WEB-CONTENT;TEMPLATES;"))
                        post-method?)
             :path uri)))
        '(("/index.htm"             "index" nil)

          ("/administration.htm"    "administration" nil)
          ("/about.htm"             "about" nil)
          ("/browser-headers.htm"   "browser-headers" nil)
          ("/server-config.htm"     "server-config" nil)
          ("/shutdown.htm"          "shutdown" nil)
          ("/unicode-test.htm"      "unicode-test" nil)
          ("/xml-test.htm"          "xml-test" nil)

          ("/add-remove-changesets-fsa.htm" "add-remove-changesets-fsa" t)

          ("/branch-info.htm"        "branch-info" nil)

          ("/change-create-fsa.htm" "change-create-fsa" nil)
          ("/change-files-fsa.htm"  "change-files-fsa" nil)
          ("/change-files.htm"       "change-files" nil)
          ("/change-set-browser.htm" "change-set-browser" nil)
          ("/change-selected-fsa.htm" "change-selected-fsa" t)

          ("/class-create.htm"      "class-create" nil)
          ("/class-editor.htm"      "class-editor" nil)
          ("/class-browser.htm"     "class-browser" nil)
          ("/class-info.htm"        "class-info" nil)

          ("/master-change-fsa.htm"     "master-change-fsa" nil)

          ("/product-create.htm"    "product-create" nil)
          ("/product-browser.htm"   "product-browser" nil)
          ("/product-editor.htm"    "product-editor" nil)
          ("/product-info.htm"      "product-info" nil)

          ("/select-changes.htm"    "select-changes" nil)

          ("/subsystem-create.htm"  "subsystem-create" nil)
          ("/subsystem-browser.htm" "subsystem-browser" nil)
          ("/subsystem-editor.htm"  "subsystem-editor" nil)
          ("/subsystem-info.htm"    "subsystem-info" nil)

          ("/reports.htm"           "reports" nil)

          ("/version-info.htm"      "version-info" nil)

          ("/workspace-create.htm"       "workspace-create" nil)
          ("/workspace-create-fsa.htm"   "workspace-create-fsa" nil)
          ("/workspace-create-fail.htm"  "workspace-create-fail" nil)
          ("/workspace-update-fsa.htm"   "workspace-update-fsa" nil)
          ("/workspace-sync.htm"         "workspace-sync" nil)
          ("/workspace-sync-fail.htm"    "workspace-sync-fail" nil)

          ("/workspace-browser.htm" "workspace-browser" nil)
          ("/workspace-editor.htm"  "workspace-editor" nil)
          ("/workspace-info.htm"    "workspace-info" nil)

          ))

;  (let ((authorizer (make-instance 'net.aserve:password-authorizer
;                                   :allowed '(("jrm" . "foobar")
;                                              ("chris" . "chris")
;                                              ("guest" . ""))
;                                   :realm "ChangeSafe")))


;    ;; Publish the commands
;    (maphash (lambda (command-name command)
;               (declare (ignore command-name))
;               ;; Sigh, can't easily publish a `prefix' that points to a function.
;               (net.aserve:publish
;                :authorizer authorizer
;                :path (changesafe-command/uri command)
;                :function (process-changesafe-command command)))
;             *changesafe-commands*)



;       (net.aserve:with-http-response (req ent :response
;                                           (apply #'net.aserve::make-resp info))
;         (net.aserve:with-http-body (req ent)
;           (net.aserve::html
;            (:html (:title (format nil "Error ~d" (car info)))
;                   (:body (format nil "Error ~d - ~a" (car info) (cadr info)))))))))))
;        '((400 "Bad Request")
;          (401 "Unauthorized")
;          (402 "Payment Required")
;          (407 "Proxy Authentication Request")
;          (411 "Length Required")
;          (412 "Precondition Failed")
;          (413 "Request Entity too large")
;          (414 "Request URI too large")
;          (415 "Unsupported media type")
;          (416 "Request range not satisfiable")
;          (417 "Expectation Failed")
;          (501 "not implemented")
;          (502 "bad gateway")
;          (503 "service unavailable")
;          (504 "gateway time-out")

;          ;; Recognized by IE
;          (403 "Forbidden")
;          (404 "Not Found")
;          (405 "Method Not Allowed")
;          (406 "Not acceptable")
;          (408 "Request Timeout")
;          (409 "Conflict")
;          (410 "Gone")

;          (500 "Internal server error")
;          (505 "http version not supported")))


;         (net.aserve:with-http-body (req ent)
;           (net.aserve::html
;            (:html
;             (:title "Not Acceptable")
;             (:body
;              "This was a not acceptable response."))))))))

  ;; (publisher:publish-site (translate-logical-pathname "CSF:WEB-CONTENT;TEMPLATES;"))

  (net.aserve:publish
   :path "/headers.htm"
   :content-type "application/xhtml+xml"
   :function
   (lambda (req ent)
     (net.aserve:with-http-response (req ent)
       (net.aserve:with-http-body (req ent)
         (net.aserve::html
          (:html
           (:title "Headers")
           (:body
            (:table (:tr (:th "Header") (:th "Value"))
                    (map 'list (lambda (header-name header-key)
                                 (net.aserve::html
                                  (:tr (:td (:princ header-name))
                                       (:td (:prin1-safe
                                             (let ((val (net.aserve::header-slot-value
                                                         req header-key)))
                                               (debug-message 4 "~s:  ~s" header-key val)
                                               (and (stringp val)
                                                    (map 'list
                                                         (lambda (value)
                                                           (let ((stuff (split-string value #\;)))
                                                             (cons (car stuff)
                                                                   (map 'list
                                                                        (lambda (p)
                                                                          (split-string p #\=))
                                                                        (cdr stuff)))))
                                                         (split-string val #\,)))))))))
                         net.aserve::*header-name-array*
                         net.aserve::*header-keyword-array*)))))))))

  (net.aserve:publish
   :path "/configuration.htm"
   :content-type "application/xhtml+xml"
   :function (lambda (req ent)
               (net.aserve:with-http-response (req ent)
                 (net.aserve:with-http-body (req ent)
                   (net.aserve::html
                    (:html (:title "Configuration")
                           (:body
                            (:table (:tr (:th "Variable") (:th "Value"))
                                    (map 'list (lambda (var-val)
                                                 (net.aserve::html
                                                  (:tr (:td (:princ (car var-val))
                                                            (:td (:princ-safe (cadr var-val)))))))
                                         `((lw:*lispworks-directory* ,lw:*lispworks-directory*)
                                           (*log-directory* ,*log-directory*)
                                           (*default-pathname-defaults* ,*default-pathname-defaults*)
                                           (current-directory ,(os-get-current-directory))
                                           (cmd-line ,sys:*line-arguments-list*)
                                           ("CSF:;" ,(translate-logical-pathname "CSF:;"))
                                           ("CSF:WEB-CONTENT;" ,(translate-logical-pathname "CSF:WEB-CONTENT;"))
                                           ("CSF:WEB-CONTENT;index.htm" ,(translate-logical-pathname "CSF:WEB-CONTENT;index.htm"))
                                           ))))))))))

  (net.aserve:publish
   :path "/utf-8.htm"
   :content-type "application/xhtml+xml; charset=\"utf-8\""
   :function (lambda (req ent)
               (net.aserve:with-http-response (req ent)
                 (net.aserve:with-http-body (req ent
                                                 :external-format :utf-8)
                   (setf (net.aserve:request-reply-stream req)
                         (make-instance 'utf-8-output-stream :underlying-stream (net.aserve:request-reply-stream req)))
                   (debug-message 3 "Stream is ~s" (net.aserve:request-reply-stream req))
                   (let ((net.aserve::*html-stream* (net.aserve:request-reply-stream req)))
                     ;; emit BOM
                     (write-char #\U+FEFF (net.aserve:request-reply-stream req))
                     (net.aserve::html
                      (:html (:title "UTF-8 Test")
                             (:body
                              (:h1 "UTF-8 Test")
                              (:p (:princ-safe (csf/utility::utf-8-sample)))))))))))



  )

(defconstant +branch-name/maximum-length+ 64
  "The maximum number of characters in the branch name.
   Make this bigger if necessary.")

(defconstant +branch-name/suggested-length+ 32
  "The suggested number of characters in the branch name.
   Field in form will be this long.")

(defconstant +product-description/maximum-length+ 1024
  "The maximum number of characters in the product description.
   Make this bigger if necessary.")

(defconstant +product-description/suggested-length+ 128
  "The suggested number of characters in the product description.
   Field in form will be this long.")

(defconstant +product-name/maximum-length+ 64
  "The maximum number of characters in the product name.
   Make this bigger if necessary.")

(defconstant +product-name/suggested-length+ 32
  "The suggested number of characters in the product name.
   Field in form will be this long.")

#||

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A hack to find where the master repositories are.  It's here
;;; because it happens at initialization time and because I couldn't
;;; think of where else to put it.

(defun note-conman-master-repository-directory (master-repository-directory-pathname)
  "Can be called by the test suite, for example, so that the master repositories
   it is currently working with will be known to the HTTP server."
  (declare (special *known-master-repository-directories*))
  (let ((dir-pathname (make-pathname :name :wild
                                     :type *repository-master-file-type*
                                     :defaults master-repository-directory-pathname)))
    (pushnew dir-pathname *known-master-repository-directories*
             :test #'equal)
    (conman-server-log-string nil "Master-Repository-Local"
                              *known-master-repository-directories*)
    *known-master-repository-directories*))

(defconstant +conman-db-host-plist-remote-repository-indicator+ 'conman-remote-repositories
  "The indicator in the DB-HOST-PLIST of a DB-HOST where we find the host's list of
   known master repositories.")

(defun note-conman-remote-master-repository (master-repository-full-name)
  "MASTER-REPOSITORY-FULL-NAME is the full name (including host name and colon prefix)
   of a master repository on some remote host (which must be a defined DB-HOST).
   This function adds this repository name to the DB-HOST's list of known repositories.
   This is needed because we have no way of seeing what files are in a remote directory."
  (let* ((db-name (db-name-parse master-repository-full-name))
         (db-host (db-name-host db-name)))
    (if (string= *repository-master-file-type*
                 (pathname-type (db-name-pathname db-name)))
        (progn
          (conman-server-log-string nil "Master-Repository-Remote"
                                    master-repository-full-name)
          (pushnew db-name
                   (getf (db-host-plist db-host)
                         +conman-db-host-plist-remote-repository-indicator+)
                   :test #'objects-equalp))
      (warn "Ignoring ~s which is not the name of a master repository file."
            master-repository-full-name))))

(defparameter *known-master-repository-directories* nil
  "A list of wildcard pathnames of master repository database files
   known about by this server.")

(defun available-conman-master-repositories (&key purge-boring-directories)
  "Return a list of the pathnames of master repositories which have
   been accessed by this server."
  ;; If there were databases created by the test suite and it has
  ;; finished running, the databases will be gone, so forget them
  (loop with local-db-host = (db-host-lookup nil)
        for master-dir in *known-master-repository-directories*
        for found-masters = (directory (make-pathname :type *repository-master-file-type*
                                                    :defaults master-dir))
        with gone = nil
        append (mapcar (lambda (pathname)
                           (db-name-create local-db-host pathname))
                       found-masters)
        into all-found-masters
      do (when (and purge-boring-directories
                    (null found-masters))
           (format *trace-output* "~&No master repositories matching ~s."
                   master-dir)
           (push master-dir gone))
      finally (when gone
                (setq *known-master-repository-directories*
                  (set-difference *known-master-repository-directories* gone)))
              (return (nconc all-found-masters
                             (loop for db-host in *database-server-hosts*
                                   append (getf (db-host-plist db-host)
                                                +conman-db-host-plist-remote-repository-indicator+))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +conman-server-config-file-name+
    "changesafe-server-config.lsp"
  "Name of file (in the directory where the ChangeSafe server software is installed)
   from which configuration parameters can be read.")

(defparameter +conman-debug-config-file-name+
    "changesafe-debug-config.lsp"
  "Name of file (in the directory where the ChangeSafe server software is installed)
   from which debug configuration parameters can be read.")  ;; development only

(defparameter +conman-server-state-file-name+
    "conman-server-state.lsp"
  "Name of file (in the directory where the ChangeSafe server software is installed)
   in which state information about the server is saved to and read from.
   This information might include the list of master repositories which have been
   accessed by this server, for example.")

(defparameter +conman-debug-state-file-name+
    "conman-debug-state.lsp"
  "Name of file (in the directory where the ChangeSafe server software is installed)
   in which state information about the server is saved to and read from.
   This information might include the list of master repositories which have been
   accessed by this server, for example.") ;; development only

(defparameter *conman-server-config-command-port-number* 7999
  "Port number to be used for debug command line client requests.")

(defparameter *conman-debug-config-command-port-number* 6666
  "Port number to be used for debug command line client requests.")

(defparameter *conman-server-config-browser-port-number* 8000
  "Port number to be used for browser/report client requests.")

(defparameter *conman-debug-config-browser-port-number* 6667
  "Port number to be used for browser/report client requests.")

(defun conman-server-directory-pathname (&optional pathname)
  "With no argument, return the pathname of the directory containing the
   ChangeSafe server software.  If PATHNAME is provided, the result of merging
   it with the ChangeSafe server directory is returned."
  (let ((dir (if (running-development-changesafe-server-p)
                 ;; Running ChangeSafe in a development lisp image.
                 ;; test-config is a directory where we can put the
                 ;; configuration files for a server being run from a
                 ;; development lisp.
                 (let ((dir (translate-logical-pathname
                             "TS50:SEMITEMP;distribution-conman;test-config;")))
                   (ensure-directories-exist dir)
                   dir)
               ;; Running a delivered ChangeSafe image
               (translate-logical-pathname "SYS:"))))
    (if pathname
        (merge-pathnames pathname dir)
      dir)))

(defun read-conman-server-file (filename)
  "The ChangeSafe server has a configuration file which is loaded by this
   function at server startup time."
  (let ((config-file-pathname
         (conman-server-directory-pathname filename)))
    (when (probe-file config-file-pathname)
      (load config-file-pathname)
      (format t "~&Finished loading ~a~%" config-file-pathname))))

(defun dump-conman-server-state-file ()
  (let ((state-file-pathname
         (conman-server-directory-pathname +conman-server-state-file-name+))
        (variables-to-dump '()))
    (when variables-to-dump
      (with-open-file (stream state-file-pathname
                       :direction :output
                       :if-exists :supersede)
        (let ((*package* (find-package :keyword))) ; force printing of package prefixes
          (dolist (v variables-to-dump)
            (terpri stream)
            (print `(setq ,v ,(symbol-value v)) stream)
            (fresh-line stream))))
      state-file-pathname)))

(defun running-development-changesafe-server-p ()
  "Returns true if we're running in a development image, false for a delivered image."
  (not (and (boundp '*changesafe-build-stamp*)
            *changesafe-build-stamp*)))

;;;; make sure that cmctl-open-all-repositories gets changed if
;;;; the list of known servers expands.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Here is the function which starts the server.

(defun changesafe-server-top-level (&key (suppress-noise *within-regression-tests*)
                                         (command-line-args
                                          (changesafe-command-line-arguments)))
  "This is the entry point for starting the ChangeSafe server.
   It takes care of any initial setup and then starts the server.
   COMMAND-LINE-ARGS is a list of strings as would be found after the
   '--' argument on the command line."
  (flet ((find-command-line-arg (key-string)
           (member key-string command-line-args
                   :test #'string-equal)))
    (let* ((running-development-server-p (running-development-changesafe-server-p))
           (debug (find-command-line-arg "-debug"))
           (config-file (or (second (find-command-line-arg "-config-file"))
                            (if debug
                                +conman-debug-config-file-name+
                              +conman-server-config-file-name+)))
           (server-state-file (conman-server-directory-pathname +conman-server-state-file-name+))
           (port (or (let ((port-string (second (find-command-line-arg "-port"))))
                       (when port-string
                         (parse-integer port-string)))
                     (if debug
                         *conman-debug-config-command-port-number*
                       *conman-server-config-command-port-number*)))
           (no-noise suppress-noise)
           result)
      (let ((ro-p (find-command-line-arg "-read-only"))
            (rw-p (find-command-line-arg "-read-write")))
        (cond ((and ro-p rw-p)
               (error "The -read-only and -read-write arguments are mutually exclusive."))
              (ro-p (conman-set-server-operating-mode :read-only))
              (rw-p (conman-set-server-operating-mode :read/write))
              (t (error "You must specify a server type: either -read-only or -read-write."))))

      (mp:start-scheduler)  ; so that we can multiprocess

      ;; No longer necessary.
      ;; allow processing of deferred interrupts
      ;; (start-deferred-interrupt-delivery-process)

      ;; This is necessary so that ACL recognizes "REPOSITORY:" as a
      ;; logical pathname host.
      (setup-repository-logical-host)

      ;; When running a delivered image, AllegroCL should load the
      ;; logical translations for TS50:TS50; so that various places in
      ;; the code which refer to that will find something.  We provide a
      ;; hosts.cl file for this.

      ;; Show product version info
      (unless no-noise
        (format t "~&This is ChangeSafe version ~d.~d~%"
                user:*major-software-version* user:*minor-software-version*)
        (when running-development-server-p
          (format t "~&Running ChangeSafe server in development mode.~%")))

      ;; (lisp-load-patches)  ; gets errors
      (csf-load-patches)

      ;; setup the local host
      (db-host-create nil (server-platform))

      (db-host-create nil (server-platform))

      (read-conman-server-file config-file)

      ;; If there's a server state file then load it.
      (when server-state-file
        (read-conman-server-file server-state-file))

      ;; set referential integrity for Allegrostore
      (repository-set-referential-integrity)

      ;; set up global info for logging
      (conman-server-log-start-globals port
                                       (conman-check-server-operating-mode)
                                       server-state-file)

      ;; Set up & Show product version info in the log
      (when *conman-server-log-start*
        (let ((log-name (conman-server-directory-pathname
                         (merge-pathnames
                          (format nil "CSF-~a~d-server-~a.log"
                                  (ecase (conman-check-server-operating-mode)
                                    (:read-only "ro")
                                    (:read/write "rw"))
                                  port
                                  (universal-time->iso-date-string
                                   (get-universal-time)))
                          (make-pathname :directory '(:relative "logs"))))))
          ;; I havn't a clue what this is doing hewre.  It assumes we're
          ;; running within the context of WITH-CM-CLI-CONTROL, which we
          ;; are clearly not.
          (ensure-directory-exists (make-pathname :name nil
                                                  :type nil
                                                  :defaults log-name))
          (conman-server-log-open-stream log-name (sys:user-name)
                                         :reason "Server Start Up")))
      (unless no-noise
        (conman-server-log-start-info t "Server Start Up" (sys:user-name)))

      ;; Also report how the memory is layed out.
      (unless no-noise
        (room t))

      (setq result
            ;; This catch is a quick hack so TEST-ALL can exit server-top-level and run tests
            (catch :exit-conman-server-top-level
              ;; Note: this binding is here since it must be disestablished after a throw
              ;; so that any successive TEST-ALLs do not still see it.

              ;; start the server
              (unwind-protect
                  ;; We need to make sure *PACKAGE* is bound to the conman package or
                  ;; the distributed identifier resolution code won't work properly.
                  (changesafe-server-start :port port)
                ;; Preserve some state for the next server session.
                (when server-state-file
                  (dump-conman-server-state-file)))

              (unless running-development-server-p
                (excl:exit))))

      (if (functionp result)
          ;; Run any continuation (used by TEST-ALL to run tests from a server breakpoint)
          (funcall result)
        result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

||#

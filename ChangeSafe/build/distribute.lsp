;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2003 ChangeSafe, LLC
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
;;;; File Name:     distribute.lsp
;;;; Author:        Mark Nahabedian
;;;; Creation Date: 1999-12-23
;;;;
;;;; Module Description:
;;;;
;;;; Collect all of the files needed for a distribution into a single
;;;; distribution folder for each product.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CSF/UTILITY")

;;; API's exported from this module.
(eval-when (:load-toplevel :execute)
  (export '(
            distribute-changesafe
            distribute-files
            )))

;;; This file requires that delivery.lsp and was previously loaded.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Our copyright notice

(defparameter +copyright-notice+
    "<p>Copyright © 2003 ChangeSafe, LLC<br>ALL RIGHTS RESERVED.<br>

<pre>
ChangeSafe, LLC
State Park Road
Hull, MA 02045
</pre>

<p>
 This software and information comprise valuable intellectual property
 and trade secrets of ChangeSafe, LLC, developed at substantial
 expense by ChangeSafe, which ChangeSafe intends to
 preserve as trade secrets.  This software is furnished pursuant to a
 written license agreement and may be used, copied, transmitted, and
 stored only in accordance with the terms of such license and with the
 inclusion of the above copyright notice.  This software and
 information or any other copies thereof may not be provided or
 otherwise made available to any other person.  NO title to or
 ownership of this software and information is hereby transferred.
 ChangeSafe assumes no responsibility for the use or reliability
 of this software.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Distribution file lists

;;; A distribution file list describes how to assemble the files that
;;; are necessary for a given distribution.

;;; Each element of the distribution file list is a list of the form

;;;    (SOURCE-PATHNAME DISTRIBUTION-PATHNAME &optional DESCRIPTION-STRING)

;;; The SOURCE-PATHNAME is merged against CSF:; to determine the
;;; location of the source file.  The source pathname can contain
;;; wildcards to represent a set of files.  The "SOURCE-PATHNAME" can
;;; also be a symbol naming a function or a list whose CAR is a symbol
;;; naming a function.  The function's first argument will be an
;;; output stream to which is should write the content of the file.
;;; In the list case, the rest of the list will be passed as
;;; additional arguments to the function.

;;; The destination pathname, if present is used as the target to copy
;;; to.  It is merged against a specified distribution directory (the
;;; one being created for the distribution).

;;; If DESCRIPTION-STRING is present, it will be included in a
;;; "Files.html" file which describes the files in the distribution.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General code for making a distribution

(defun changesafe-application-target ()
  "Returns the namestring of the application and the directory
   to which the application and other associated files should be written."
  ;; We base the name of the target directory on the platform we are
  ;; building for.  That way they don't get mixed up if the ChangeSafe
  ;; hierarchy is on a shared server.
  (let* ((target-namestring (cadr (member "-image" sys:*line-arguments-list* :test #'string-equal)))
         (target-directory (pathname-syntactic-parent
                            (make-pathname :name nil
                                           :type nil
                                           :version nil
                                           :defaults (parse-namestring target-namestring)))))
    (values target-namestring target-directory)))

(defun distribute-files (files-to-distribute distribution-directory)
  ;; FILES-TO-DISTRIBUTE is a distribution file list.

  ;; To deal with the CRLF v. Newline issue in text files, we should
  ;; maintain completely separate hierarchies for the HP/UX and
  ;; MSWindows versions.
;  (setq distribution-directory
;    (make-pathname :defaults distribution-directory
;                  :directory (append (pathname-directory distribution-directory)
;                                     (list #+hpux "hpux"
;                                           #+MSWindows "MSWindows"))))
  (let ((files-and-descriptions nil))
    (dolist (f files-to-distribute)
      (destructuring-bind (source &optional destination description) f
        (format t "~&;; ~a" description)
        (let ((destination (if destination
                               (merge-pathnames destination distribution-directory)
                             distribution-directory)))
          ;; Make sure the target directory is there
          (ensure-directories-exist (make-pathname :defaults destination
                                                   :name nil :type nil))
          (cond ((or (symbolp source)
                     (and (listp source)
                          (symbolp (first source))
                          (fboundp (first source))))
                 (with-simple-restart (skip-writing-file
                                       "Skip writing ~s" destination)
                   (format t "~&;; Writing ~s" destination)
                   (with-open-file (stream destination
                                    :direction :output
                                    :element-type 'character)
                     (if (listp source)
                         (apply (first source) stream (rest source))
                       (funcall source stream))))
                 (push (list (list (enough-namestring destination distribution-directory))
                             description)
                       files-and-descriptions))
                (t
                 (let ((source (merge-pathnames source (translate-logical-pathname "CSF:;")))
                       (destination
                        (if (wild-pathname-p source)
                            (make-pathname :name :wild
                                           :type :wild
                                           :defaults destination)
                          destination))
                       (these-files nil))
                   (format t "~&;; ~s -> ~s" source destination)
                   (dolist (file (directory source))
                     (unless (lw:file-directory-p file)
                       (let ((dest-file (translate-pathname file source destination))
                             (transfer-mode (copy-mode-for-file file)))
                         ;; At some point we need to actually copy them.

                         ;; Should we execute some program on the platform to do
                         ;; that, or write our own copier in Lisp.  We could use
                         ;; the FTP-FILES stuff if we're putting it on a machine
                         ;; with an FTP server.
                         (ensure-directories-exist (make-pathname :defaults dest-file
                                                                  :name nil :type nil))
                         (if transfer-mode
                             (with-simple-restart (skip-copying-file
                                                   "Skip copying ~s" file)
                               (unless (eq :no-copy transfer-mode)
                                 (format t "~&Copying ~s to ~s" file dest-file)
                                 (os-copy-file file dest-file
                                               :element-type (ecase transfer-mode
                                                               (:text 'character)
                                                               (:binary '(unsigned-byte 8))))))
                           (warn "Don't know how to copy ~s.  It will not be copied." file))
                         (push (enough-namestring dest-file distribution-directory)
                               these-files))))
                   (unless these-files
                     (warn "No files for ~s ~s" source destination))
                   (push (list (sort these-files #'string-lessp)
                               (or description ""))
                         files-and-descriptions)))))))
    (with-open-file (stream (merge-pathnames "Files.html"
                                             distribution-directory)
                     :direction :output
                     :if-exists :supersede)
      (write-distribution-file-descriptions files-and-descriptions stream)))
  distribution-directory)

(defun write-distribution-file-descriptions (files-and-descriptions to-stream)
  (format to-stream "<h2>Description of files in the ChangeSafe distribution for platform ~a</h2>"
          #+HPUX "HP/UX"
          #+MSWindows "WindowsNT")
  (terpri to-stream)
  (progn
    (format to-stream "~&~%<table border=1>")
    (loop for (files description) in files-and-descriptions
        do
          (format to-stream "~&  <tr>")
          (format to-stream "~&    <td>")
          (dolist (f files)
            (format to-stream "~&      ~a<br>" f))
          (format to-stream "~&    </td>~&    <td>~a~&    </td></tr>"
                  description))
    (format to-stream "~&</table>~%"))
  (progn
    (format to-stream "~&~%<p>This distribution was built on ")
    (multiple-value-bind (ignore1 ignore2 ignore3 day month year)
        (decode-universal-time (get-universal-time))
      (declare (ignore ignore1 ignore2 ignore3))
      (utility::iso-date-string year month day :to-stream to-stream))
    (format to-stream " based on ChangeSafe version ~d.~d"
            *major-software-version* *minor-software-version*))
  (format to-stream "~&~%<p>~&~a" +copyright-notice+)
  )

(defun lisp-installation-directory (&optional subdir-pathname)
  (let ((acl-root (translate-logical-pathname "SYS:")))
    (if subdir-pathname
        (merge-pathnames subdir-pathname acl-root)
      acl-root)))

(defun write-file-text (stream &rest text)
  (loop for item in text
        do (cond ((stringp item) (write-string item stream))
                 ((symbolp item)
                  (ecase item
                    (:newline (terpri stream))))
                 (t (error "Bad item ~s" item)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; figuring out how to copy a file of a given type

(defparameter +file-copy-modes+
    '(("lsp" :text) ("cl" :text)
      ("config" :text)
      ("dtd" :text) ("ent" :text) ("css" :text)
      ("exe" :binary) ("dll" :binary) ("dxl" :binary) ("adb" :binary)
      ("lib" :binary) ("sl" :binary)
      ("class" :binary) ("cab" :binary) ("ini" :binary) ("jar" :binary)
      ("gif" :binary) ("jpg" :binary) ("bmp" :binary)
      ("zip" :binary) ("gz" :binary) ("Z" :binary)
      ("txt" :text) ("text" :text) ("TXT" :text)
      ("msg" :text)
      ("html" :text) ("htm" :text)
      ("info" :text)
      ("sh" :text) ("bat" :text)
      ("ico" :binary) ("ins" :binary) ("ex_" :binary)
      (nil :text) ;; <-- catches .csf-prefs
      ("csf-prefs" :text)
      ("pdf" :binary)
      ;; Random crap that should not be copied
      ("js" :no-copy) ;; changesafe 2 stuff
      ("eps" :no-copy) ("tif" :no-copy)
      ;; Byproducts of the build which should not be copied
      ("out" :no-copy) ("log" :no-copy)
      )
  "An alist for determining how a file should be copied based on its file type.")

(defun emacs-backup-file? (type)
  (and (stringp type)
       (let ((length (string-length type)))
         (and (>= length 3)
              (char= (char type 0) #\~)
              (char= (char type (- length 1)) #\~)
              (null (position-if-not #'digit-char-p type :start 1 :end (- length 1)))))))

(defun copy-mode-for-file (pathname)
  (let ((type (pathname-type pathname)))
    (cond ((second (assoc type +file-copy-modes+ :test #'equal)))
          ((emacs-backup-file? type) :no-copy)
          (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The CHANGESAFE distribution

(defparameter +changesafe-distribution-files+
    ;; See the description of distribution file lists above.

    ;; NOTE: To make it easier to review merge conflicts, this list
    ;; should only contain one entry per line.
    `(
      ;; A README file describing how to install and test
      ;; Release notes, limitatins, caveats, etc.
      ;; Sun Java JRE 1.1.8

      ;; Release notes
      #+hpux
      (,(merge-pathnames
         (make-pathname :directory '(:relative "hpux")
                         :name "hpux-install-notes"
                         :type "txt")
         (translate-logical-pathname "CSF:README;"))
       ""
       "The installation notes for this version of ChangeSafe.")

      ;; Lisp executable and Lisp image file
      ;; This is created by the delivery call below.
;      (,(make-wild-pathname (nth-value 1 (changesafe-application-target))) ; defined at end of lwdeliver.lsp
;       "server/"
;       "These are the files which implement the ChangeSafe server.")

      (,(translate-logical-pathname #p"CSF:MISC;sh;ChangeSafe-server-startup.sh")
       "server/"
       "This is an example UNIX shell script for starting the command line and reports ChangeSafe servers.")

;;; These won't work as is. 
;      (write-changesafe-server-start-script
;       "server/start-changesafe-read-write-server"
;       "This file is a command script which will start a ChangeSafe read-write server.")
;      (write-changesafe-http-server-start-script
;       "server/start-changesafe-read-only-server"
;       "This file is a command script which will start a ChangeSafe read-only server.")

      ;; Under HP/UX we need aslisp, rather than what
      ;; GENERATE-APPLICATION wrote.  No we don't.
      ;; GENERATE-APPLICATION was smart enough to copy the right one.

      (,(translate-logical-pathname #p"CSF:JAVA;fsa;*.class")
       "client/"
       "These are the Java class files which implement the CHANGESAFE client.")

;;; Hold off on distributing the client files for now.
#||
      ;; *** Files needed on the client side.
      ;; Java class files:
      (,(translate-logical-pathname #p"CSF:JAVA;cm;*.class")
       "client/"
       "These are the Java class files which implement the CHANGESAFE client.")

      ;; The sample .csf-prefs file should go in the same directory
      ;; as the class files since CM.class will try to look for the
      ;; site-wide one there.  The user will need to customize this
      ;; file based on the host name of their changesafe server.
      (,(make-pathname :name ""
                       :type "csf-prefs"
                       :defaults (translate-logical-pathname #p"CSF:JAVA;cm;"))
       "client/"
       "This file contains configuration information for your ChangeSafe installation.
        This is where you tell the ChangeSafe client which host is running the ChangeSafe
        server software.  A user can have their own copy of this file in their home
        directory if they wish to override these settings.")

      ;; A shell script for writing the "cm" shell script based on
      ;; where Java is installed:
      (,(translate-logical-pathname #p"CSF:MISC;sh;write-cm-shell-script.sh")
       "client/"
       "Invoking the Java runtime on CM.class can be quite cumbersome.  This file is a
        UNIX shell script which will search for your Java installation and write a shell
        script named <tt>csf</tt> which will invoke the Java runtime with the appropriate
        classpath and arguments.")

      ;; MSWindows batch file for invoking the ChangeSafe client
      (,(translate-logical-pathname #p"CSF:MISC;bat;csf.bat")
       "client/"
       "An example MSWindows batch file for invoking the ChangeSafe client.")
||#

      ;; An example server configuration file
      (,(translate-logical-pathname #p"CSF:CONMAN;changesafe-server-config.lsp")
       "server/"
       "This file is loaded by the ChangeSafe server when it starts up and before it starts
        serving requests.  It can contain various configuration parameters.")

;; Don't think we need this.
;      (,(translate-logical-pathname #p"CSF:BUILD;hosts.cl")
;       "server/hosts.cl"
;       "This file is needed to help the server find its static files.  Don't edit it.")

      ;; Patch directories.
      ;; We currently don't have a mechanism for populating the patch
      ;; directories from the distribution script.  We can at least
      ;; ensure that empty patch directories are created in the right
      ;; place.
      ((write-file-text
        "This directory contains patches provided by Xanalys Inc."
        :newline
        "which will be loaded each time ChangeSafe starts up."
        :newline)
       "server/update/README.txt"
       "This is a README file for the Lisp patch directory.")
      ((write-file-text
        "This directory contains patches provided by ChangeSafe LLC."
        :newline
        "which will be loaded each time ChangeSafe starts up."
        :newline)
       "server/csf-update/README.txt"
       "This is a README file for the directory containing patches to ChangeSafe.")

      (,(translate-logical-pathname #p"CSF:WEB-CONTENT;static;images;logo.bmp")
       "server/web-content/static/images/ChangeSafe logo 8-14-03.bmp"
       "Our beloved logo.")

      (,(translate-logical-pathname #p"CSF:WEB-CONTENT;static;style.css")
       "server/web-content/static/style.css"
       "This style sheet governs the look and feel of ChangeSafe.")

      (,(translate-logical-pathname #p"CSF:WEB-CONTENT;static;*.dtd")
       "server/web-content/static/"
       "DTDs for XHTML.")

      (,(translate-logical-pathname #p"CSF:WEB-CONTENT;static;*.ent")
       "server/web-content/static/"
       "Entity definitions for XHTML.")

;;; Template files follow.  This should turn into a wildcard at some point,
;;; but for now, I'm doing it one by one.

      (,(translate-logical-pathname #p"CSF:WEB-CONTENT;templates;*.lsp")
       "server/web-content/templates/"
       "Template definitions GUI pages.")

      (,(translate-logical-pathname #p"CSF:WEB-CONTENT;applets;FSTestApplet.cab")
       "server/web-content/applets/"
       "CAB file for the FSA.")

;      ;; Static content for the HTTP server
;      ("web-content/**/*.*"                     ; was "web-content/static/**/*.*"
;       "server/web-content/"                    ; was "server/web-content/static/"
;       "This directory contains various files needed by the HTTP server.  They include
;        various static web content like icons, etc.
;        Also under here are the applet files needed for the ChangeSafe client.")

      ;; Some documentation
;      (changesafe:generate-return-code-documentation-html
;       "documentation/return-codes.html"
;       "This is an HTML file which gives a description of each of the numeric
;        status codes that can be returned by each command.")

      )
  "A list of files required to install ChangeSafe at a customer site.
   This is the 'packing list' from which the archive to be sent
   to HP will be generated.")

(defun distribute-changesafe (&key (to-where (translate-logical-pathname "CSF:Delivery;")))
  (distribute-files +changesafe-distribution-files+ to-where))

(defun write-changesafe-server-start-script (stream)
  "Writes a platform specific script for starting the changesafe server."
  #+hpux      (format stream "~&~%changesafe.exe -I changesafe.dxl -- -read-write -port 7999~%~%")
  #+MSWindows (format stream "~&~%changesafe.exe -I changesafe.dxl -as ashook.dll -- -read-write -port 7999~%~%"))

(defun write-changesafe-http-server-start-script (stream)
  "Writes a platform specific script for starting the changesafe server."
  ;; The -- argument is to prevent ACL frm complaining that it doesn't
  ;; recognize the -http argument.
  #+hpux      (format stream "~&~%changesafe.exe -I changesafe.dxl -- -read-only -port 8000~%~%")
  #+MSWindows (format stream "~&~%changesafe.exe -I changesafe.dxl -as ashook.dll -- -read-only -port 8000~%~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

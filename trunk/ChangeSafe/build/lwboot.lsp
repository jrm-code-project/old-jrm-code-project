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


;;; Bootstrap the development environment.
;;; This sets up the logical pathname translations and load the
;;; `system' file.

(in-package "CHANGESAFE")

#+lispworks
(eval-when (:load-toplevel :execute)

  ;; In violation of the standard, the logical pathname translations
  ;; of SYS aren't defined by default.  Set it up now.
  (when (null (ignore-errors (logical-pathname-translations "SYS")))
    (let* ((executable-path (parse-namestring (car sys:*line-arguments-list*)))
           (executable-directory (make-pathname :name nil
                                                :type nil
                                                :defaults executable-path)))
      (setf (logical-pathname-translations "SYS")
            (list (list "**;*.*"
                            (merge-pathnames 
                             (make-pathname :directory '(:relative :wild-inferiors)
                                            :name :wild
                                            :type :wild
                                            :defaults "")
                             (merge-pathnames executable-directory (hcl:get-working-directory)))))))))

;;; Set everything up so we are running in the users workspace.
(defparameter *changesafe-source-directory*
  (let ((source-directory
         (and *load-pathname*
              (merge-pathnames
               (make-pathname :directory '(:relative :back)
                              :defaults "")
               (make-pathname :name nil
                              :type nil
                              :version nil
                              :defaults *load-pathname*)))))
    (setq *default-pathname-defaults* source-directory)
    (hcl:change-directory *default-pathname-defaults*)
    source-directory))

(defun changesafe-source-pathname (&optional (filename ""))
  (let ((pathname (if (stringp filename)
                      (parse-namestring filename)
                    filename)))
    (merge-pathnames pathname *changesafe-source-directory*)))

#+lispworks
(eval-when (:load-toplevel :execute)
  (setq system::*PATHNAME-CURRENT-USER-HOMEDIR-PATHNAME-FORMAT-STRING-DEFAULT* "C:\\Home\\~@(~a~)\\"))

(defun csf/utility::get-user-name ()
  #+allegro   (sys:user-name)
  #+lispworks (sys:get-user-name))

(defun standard-path-translations ()
  "Set the logical path translations for CSF.  This is a function so that it can
   be re-evaluated after the pathname-hacks file is loaded."
  (let ((temporary-directory

         #+allegro (sys:temporary-directory)

         ;; For lispworks, create a temp file and find out where the system put it!
         #+lispworks
         (let ((temp-file nil))
           (unwind-protect
               (progn (setq temp-file (hcl:make-temp-file))
                      (make-pathname :host      (pathname-host temp-file)
                                     :device    (pathname-device temp-file)
                                     :directory (pathname-directory temp-file)
                                     :name nil
                                     :type nil
                                     :version nil))
             (when temp-file (delete-file temp-file))))))

    (labels ((relative-subdirectory (&rest elements)
               (changesafe-source-pathname
                (make-pathname :directory `(:relative ,@elements :wild-inferiors)
                               :name :wild
                               :type :wild
                               :defaults "")))

             (wild-suffix (prefix)
               (concatenate 'string (string-upcase prefix) ";**;*.*"))

             (standard-translation (subdir)
               (list (wild-suffix subdir) (relative-subdirectory subdir))))

      (setf (logical-pathname-translations "CSF")
            (list
             (list "**;*.*" (merge-pathnames 
                             (make-pathname :directory '(:relative :wild-inferiors)
                                            :name :wild
                                            :type :wild
                                            :defaults "")
                             *changesafe-source-directory*))
             (standard-translation "ansi-series")
             (standard-translation "allegroserve")
             (standard-translation "build")
             (standard-translation "conman")
             (standard-translation "core")
             (standard-translation "defsystem")
             (list "HTMLGEN;**;*.*"
                   (relative-subdirectory "allegroserve" "htmlgen"))
;             (list "HTML-PARSER;**;*.*"
;                   (relative-subdirectory "lisp-server-pages" "html-parser"))
;             (list "LSP;**;*.*"
;                   (relative-subdirectory "lisp-server-pages" "lsp"))
;             (list "PLIST;**;*.*"
;                   (relative-subdirectory "lisp-server-pages" "plist"))
;             (list "PUBLISHER;**;*.*"
;                   (relative-subdirectory "lisp-server-pages" "publisher"))
;             (list "TOKENIZER;**;*.*"
;                   (relative-subdirectory "lisp-server-pages" "tokenizer"))
             (standard-translation "java")
             (standard-translation "java;auxiliary")
             (standard-translation "java;fsa")
             (standard-translation "misc")
             (standard-translation "pstore")
             ;; A `temporary' directory for testing
             (list "REPOSITORIES;**;*.*"
                   (merge-pathnames
                    (make-pathname :directory `(:relative
                                                #+unix ,(csf/utility::get-user-name)
                                                "ChangeSafe-temp" "repositories")
                                   :defaults "")
                    temporary-directory))
             (standard-translation "readme")
             (standard-translation "rfm")
             (standard-translation "server")
             ;; A `temporary' directory for testing
             (list "SEMITEMP;**;*.*"
                   (merge-pathnames
                    (make-pathname :directory `(:relative
                                                #+unix ,(csf/utility::get-user-name)
                                                "ChangeSafe-temp" "semitemp")
                                   :defaults "")
                    temporary-directory))
             (list ";" (changesafe-source-pathname))
             (list "CSF-PACKAGES;**;*.*"
                   (merge-pathnames
                    (make-pathname :directory `(:relative
                                                #+unix ,(csf/utility::get-user-name)
                                                "ChangeSafe-temp" "packages")
                                   :defaults "")
                    temporary-directory))
             (list "CSF-SERVER;**;*.*"
                   (merge-pathnames
                    (make-pathname :directory `(:relative
                                                #+unix ,(csf/utility::get-user-name)
                                                "ChangeSafe-temp" "server")
                                   :defaults "")
                    temporary-directory))
             (standard-translation "source-compare")
             (standard-translation "test-data")
             (standard-translation "utility")
             (standard-translation "vm")
             (standard-translation "web")
             (standard-translation "web-content")
             (standard-translation "win32")

             (list "PLATFORM-PATCHES;**;*.*"
                   (relative-subdirectory #+allegro "acl-patches"
                                          #+allegro-v6.0 "6.0"
                                          #+allegro-v5.0.1 "5.0.1"
                                          #+lispworks "lw-patches"
                                          #+hpux "hpux"
                                          #+MSWindows "win32")))))))

(eval-when (:load-toplevel :execute)
  (standard-path-translations))

;; For LSP
(eval-when (:load-toplevel :execute)
  (push :aserve *features*))

(eval-when (:load-toplevel :execute)
  (load #p"CSF:BUILD;lwmksystem.lsp"))



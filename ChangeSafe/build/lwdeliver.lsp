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

;;; This file builds and dumps a ChangeSafe executable.

(in-package "CL-USER")

(load-all-patches)

(with-open-file (delivery-log
                 (merge-pathnames
                  (make-pathname :directory `(:relative :back)
                                :name "delivery"
                                :type "log"
                                :version nil
                                :defaults "")
                  (parse-namestring
                   (cadr (member "-image" sys:*line-arguments-list* :test #'string-equal))))
                 :direction :output
                 :if-exists :supersede
                 :if-does-not-exist :create)
  (multiple-value-bind (sec min hr day month year) (decode-universal-time (get-universal-time))
    (format delivery-log "; ChangeSafe delivery started ~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month day hr min))
  (dotimes (i 3)
    (write-string ";" delivery-log) (terpri delivery-log))
  ;; It'd be nice if it weren't truncated, but I can't seem to make
  ;; the delivery script happy.
  (write-string ";  IT IS NORMAL FOR THIS FILE TO BE TRUNCATED" delivery-log)
  (terpri delivery-log)
  (dotimes (i 3)
    (write-string ";" delivery-log) (terpri delivery-log))

  (let ((*standard-output* delivery-log))
    (finish-output delivery-log)

(let ((*default-pathname-defaults*
       (merge-pathnames
        (make-pathname :name nil :type nil :version nil
                       :defaults (first sys:*line-arguments-list*))
        (hcl:get-working-directory))))
  (load "lib\\4-2-0-0\\config\\configure.lisp"))

;; Need these things
(require "arraymac")
(require "comm")
(require "formatter")
(require "specmac")

;;; A MUST-HAVE utility
(defun package-lossage-funcall (package-name symbol-name &rest arguments)
  "Allow this file to invoke functions that will be loaded without having
 to deal with the fact that the packages have not been loaded when this file
 is read."
  (check-type package-name keyword)
  (check-type symbol-name keyword)
  (with-simple-restart (skip-calling
                        "Skip calling (~a::~a~{ ~s~})"
                        package-name symbol-name arguments)
    (let ((package (find-package (symbol-name package-name))))
      (unless package
        (error "Cannot call (~a::~a~{ ~s~}) because package ~s doesn't exist."
               package-name symbol-name arguments package-name))
      (let ((symbol (find-symbol (symbol-name symbol-name) package)))
        (unless symbol
          (error "Cannot call (~a::~a~{ ~s~}) because symbol ~s doesn't exist in ~s."
                 package-name symbol-name arguments symbol-name package))
        (unless (fboundp symbol)
          (error "Cannot call (~a::~a~{ ~s~}) because symbol ~s isn't fbound."
                 package-name symbol-name arguments symbol))
        (apply symbol arguments)))))

(eval-when (:load-toplevel :execute)
  (compile 'package-lossage-funcall))

(defpackage "CSF/CONFIG"
  (:documentation "Configuration variables.")
  (:use "COMMON-LISP")
  (:export "*MAJOR-SOFTWARE-VERSION*"
           "*MINOR-SOFTWARE-VERSION*"))

;;; Create the utility package so we can load our package defs into it.
(defpackage "CSF/UTILITY"
  (:nicknames "UTILITY" "UTILS")
  (:documentation "The basic utility package.")
  (:use "COMMON-LISP" "CSF/CONFIG"))

(let* ((source-directory (and *load-pathname*
                              (merge-pathnames
                               (make-pathname :directory '(:relative :back))
                               (make-pathname :name nil
                                              :type nil
                                              :version nil
                                              :defaults *load-pathname*))))
       (*default-pathname-defaults* source-directory))
  (labels ((source-file (subdirectory name &optional (type "lsp"))
             (merge-pathnames
              (make-pathname :directory (if (consp subdirectory)
                                            (cons ':relative subdirectory)
                                            (list ':relative subdirectory))
                             :name name
                             :type type)
              source-directory))

           (load-third-party (lisp-file)
             (let ((fasl-file (compile-file-pathname lisp-file)))
               (when (and (file-write-date lisp-file)
                          (or (null (file-write-date fasl-file))
                              (> (file-write-date lisp-file)
                                 (file-write-date fasl-file))))
                 (compile-file lisp-file))
               (when (probe-file fasl-file)
                 (load fasl-file)))))

    ;; Setup the package system
    ;; Some third-party add-ons have their own package files.
    (load (source-file "ansi-series" "s-package" "lisp"))
    ;; (load (source-file '("lisp-server-pages" "plist") "package" "lisp"))
    ;; (load (source-file '("lisp-server-pages" "htmlgen") "package" "cl"))

    ;; Move the existing `make' package out of the way.
    (when (and (find-package "MAKE")
             (not (member :mk-defsystem *features*)))
      (rename-package (find-package "MAKE") "ORIGINAL-MAKE"))

    ;; Fake up the `foreign' package for new make.
    (rename-package (find-package "SYS")
                  (package-name (find-package "SYS"))
                  (cons "FOREIGN" (package-nicknames (find-package "SYS"))))

    (unless (fboundp 'sys::lispworks-version)
      (setf (symbol-function 'sys::lispworks-version)
          (lambda ()
            (values sys::*major-version-number*
                    sys::*minor-version-number*))))

    ;; Load up our package system.
    (load (source-file "build" "packages"))

    (let ((new-packages (package-lossage-funcall
                         :utility
                         :setup-package-system :verbose t)))

      ;; Install fixes to CL macros.  Must be first!
      ;; Series code needs fixes to with-open-file/stream macros.
      (load (source-file "utility" "replacement-macros" "lsp"))
      ;; Ensure this is bound before someone uses it.
      (set (intern "*DEFER-INTERRUPTS*" (find-package :utility)) nil)

      ;; Install basic third-party code.
      (load-third-party (source-file "ansi-series" "s-code" "lisp"))
      (load-third-party (source-file "source-compare" "sc" "lsp"))
      (load-third-party (source-file "defsystem" "defsystem" "lisp"))

      ;; Install series code in our packages.
      (mapc (lambda (package)
              (package-lossage-funcall :series :install :pkg package))
            (list* (find-package "CL-USER")
                   (find-package "CSF/JAVA-TOOLS")
                   new-packages))

      ;; Prepare reader for GUIDs
      (set-dispatch-macro-character #\# #\{ 
                                    (intern "#{-reader" (find-package "CSF/UTILITY")))
      ;; Prepare reader for distributed identifiers
      (set-macro-character #\[ (intern "[-reader" (find-package "CSF/PERSISTENT-STORE")))

      ;; Skip BOM mark in Unicode files
      (set-syntax-from-char (code-char #xFEFF) #\Space)
      (setq sys:*text-file-type* "lsp")
      (setq lw:*default-character-element-type* 'lw:simple-char)
      (setq sys:*extended-spaces* t)

      ;; makes directory reading much faster
      #+:win32 (setq sys::*directory-link-transparency* nil)
      (format delivery-log "~&; *default-pathname-defaults* is now ~s" *default-pathname-defaults*)
      (load (source-file "build" "lwboot"))
      (format delivery-log "~&; *default-pathname-defaults* is now ~s" *default-pathname-defaults*)
      (dolist (system '(:csf-foundation
                        :csf-utility
                        :allegroserve
                        :changesafe))
        (package-lossage-funcall :make :operate-on-system
                                 system :load
                                 :bother-user-if-no-binary t
                                 :compile-during-load nil
                                 :verbose t))
      (load (source-file "build" "version"))
      (load (source-file "build" "distribute"))
      (setf (symbol-value (intern "*CHANGESAFE-BUILD-STAMP*" (find-package "CHANGESAFE")))
            (get-universal-time))
      )))

(defvar *process-lock-count* 0)

(defadvice (mp::process-lock check-scheduler-deadlock :before) (lock &optional whostate timeout)
  (incf *process-lock-count*)
  (unless (eq (mp:lock-owner lock) mp:*current-process*)
    (assert (null mp:*inhibit-scheduling-flag*))
    (assert (null system::*in-no-interrupts*))))

;;; Now dump it.
(let* ((source-directory (and *load-pathname*
                              (merge-pathnames
                               (make-pathname :directory '(:relative :back))
                               (make-pathname :name nil
                                              :type nil
                                              :version nil
                                              :defaults *load-pathname*))))
       (*default-pathname-defaults* source-directory))
  (multiple-value-bind (target-namestring target-directory) 
      (package-lossage-funcall :utility :changesafe-application-target)

    ;; Copy relevant files.
    (package-lossage-funcall :utility :distribute-files 
                             (symbol-value (find-symbol "+CHANGESAFE-DISTRIBUTION-FILES+"
                                                        (find-package "CSF/UTILITY")))
                             target-directory)

    ;; clean up for delivery
    (maphash (lambda (key value)
               (declare (ignore value))
               (remhash key system::*logical-pathname-translations*))
             system::*logical-pathname-translations*)
    (setq system::*auto-mount-reverse-translations* nil)
    (setq *default-pathname-defaults* (make-pathname :defaults ""))
    (finish-output delivery-log)
    (deliver (intern "SERVER-TOP-LEVEL" (find-package "CHANGESAFE"))
             target-namestring
             0
             :console t
             :exit-after-delivery t
             :functions-to-remove (list (intern "SETUP-PACKAGE-SYSTEM" (find-package "CSF/UTILITY")))
         
             :icon-file (merge-pathnames
                         (make-pathname :directory '(:relative "build")
                                        :name "ChangeSafe"
                                        :type "ico"
                                        :version nil
                                        :defaults "")
                         source-directory)
             :format t
             :keep-lisp-reader t
             ;; Needed for fancy format strings.
             :keep-pretty-printer t
             :multiprocessing t
             :startup-bitmap-file (merge-pathnames
                                   (make-pathname :directory '(:relative "build")
                                                  :name "Splash"
                                                  :type "bmp"
                                                  :version nil
                                                  :defaults "")
                                   source-directory)
             :versioninfo '(:binary-version #x0001000100000100
                                            :company-name "ChangeSafe, LLC"
                                            :comments "Prerelease version not for distribution."
                                            :debugp t
                                            :file-description "Server Executable"
                                            :language :us-english
                                            :legal-copyright "Copyright © 2003 ChangeSafe, LLC  ALL RIGHTS RESERVED."
                                            :prereleasep t
                                            :product-name "ChangeSafe Server"
                                            :version-string "Prerelease"
                                            )
             )))))



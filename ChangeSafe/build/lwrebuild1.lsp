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

(in-package "CL-USER")

(load-all-patches)

(let ((*default-pathname-defaults*
       (make-pathname :name nil :type nil :version nil
                      :defaults (first sys:*line-arguments-list*))))
  (load "lib\\4-2-0-0\\config\\configure.lisp"))

;;; We need to ensure that tail recursion is on no matter what.
(setq compiler::*debug-do-tail-merge-policy* 'true
      compiler::*eliminate-tail-recursion-policy* 'true)
(compiler::update-policy-switches)

;; Need sockets
(require "comm")

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

(defpackage "CSF/CONFIG"
  (:documentation "Configuration variables.")
  (:use "COMMON-LISP"))

;;; Create the utility package so we can load our package defs into it.
(defpackage "CSF/UTILITY"
  (:nicknames "UTILITY" "UTILS")
  (:documentation "The basic utility package.")
  (:use "COMMON-LISP" "CSF/CONFIG"))

;;; Create the ilisp package
(if (member "-console" sys:*line-arguments-list* :test #'string-equal)
(defpackage "ILISP"
  (:documentation "Package for emacs communication.")
  (:use "COMMON-LISP")))

(let ((source-directory (and *load-pathname*
                             (merge-pathnames
                              (make-pathname :directory '(:relative :back))
                              (make-pathname :name nil
                                             :type nil
                                             :version nil
                                             :defaults *load-pathname*)))))
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
    (load (source-file '("lisp-server-pages" "plist") "package" "lisp"))
    ;; (load (source-file '("lisp-server-pages" "htmlgen") "package" "cl"))

;;    ;; Move the existing `make' package out of the way.
;;    (when (and (find-package "MAKE")
;;             (not (member :mk-defsystem *features*)))
;;      (rename-package (find-package "MAKE") "ORIGINAL-MAKE"))

;;    ;; Fake up the `foreign' package for new make.
;;    (rename-package (find-package "SYS")
;;                  (package-name (find-package "SYS"))
;;                  (cons "FOREIGN" (package-nicknames (find-package "SYS"))))

;;    (unless (fboundp 'sys::lispworks-version)
;;      (setf (symbol-function 'sys::lispworks-version)
;;          (lambda ()
;;            (values sys::*major-version-number*
;;                    sys::*minor-version-number*))))

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
      ;; (load-third-party (source-file "defsystem" "defsystem" "lisp"))

      ;; Install series code in our packages.
      (mapc (lambda (package)
              (package-lossage-funcall :series :install :pkg package))
            (list* (find-package "CL-USER")
                   (find-package "CSF/JAVA-TOOLS")
                   new-packages))

      ;; Prepare reader for GUIDs
      (set-dispatch-macro-character #\# #\{ 'CSF/UTILITY::|#{-reader|)
      )))

(setq sys:*text-file-type* "lsp")
(setq lw:*default-character-element-type* 'lw:simple-char)
(setf sys:*extended-spaces* t)

(defvar *process-lock-count* 0)

(defvar *allow-bogus-locking* nil)

(defadvice (mp::process-lock check-scheduler-deadlock :before) (lock &optional whostate timeout)
  (incf *process-lock-count*)
  (unless (or (eq (mp:lock-owner lock) mp:*current-process*)
              *allow-bogus-locking*)
    (assert (null mp:*inhibit-scheduling-flag*))
    (assert (null system::*in-no-interrupts*))))

(defadvice (capi::install-editor-pane-font grab-lock :around) (self)
  (let ((*allow-bogus-locking* t))
    (call-next-advice self)))

;  (win32::with-dc-lock ((win32::r-device-memory-dc
;                         (win32::logical-font-device
;                          (graphics-ports::%font-device-font
;                           (capi::simple-pane-font self)))))


;; Dump the image
(if (member "-console" sys:*line-arguments-list* :test #'string-equal)
    (save-image (cadr (member "-image" sys:*line-arguments-list* :test #'string-equal))
                :console t
                :environment nil
                :restart-function 'mp:initialize-multiprocessing)
    (save-image (cadr (member "-image" sys:*line-arguments-list* :test #'string-equal))))

(quit)

;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002 - 2005 ChangeSafe, LLC
;;;;
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
;;;; File Name:     build\packages.lsp
;;;; Author:        jrm
;;;;
;;;; Set up the package system.
;;;;
;;;; "The important point is that the packages setup when you compile
;;;; a file must be identical to the packages setup when you load the
;;;; file....  What will help, 100%, is to have a file which issues
;;;; all the necessary `defpackage' forms, and make sure this file is
;;;; loaded before anything else and before any `compile-file'.
;;;; -- Bruno Haible
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CSF/UTILITY")

(defun setup-package-system (&key verbose)
  (let ((broken-symbols
         #+lispworks
         '(cl:cond  ;; adding some functionality to cond
           cl:directory
           cl:enough-namestring
           cl:nconc     ;; actually an error!
           cl:probe-file
           cl:subtypep
           ;; cl:the       ;; again, paranoid
           cl:unwind-protect
           cl:with-open-file
           cl:with-open-stream)

         #+allegro
         '(cl:delete-file
           cl:directory
           cl:enough-namestring                 ; technically not broken, but hazardous
           ;; They moved this symbol and broke it.
           #+(and :allegro-version>= (:version>= 6 1)) excl:enough-pathname
           cl:ignore-errors
           cl:probe-file
           cl:unwind-protect
           cl:with-open-file
           cl:with-open-stream))

        (utility-package (find-package "CSF/UTILITY")))

    ;; The utility package will contain replacements for the broken symbols
    #+allegro
    (mapc (lambda (sym) (shadow sym utility-package))
          (if (member :dont-fix-franz *features*)
              (list 'excl::directory 'excl::delete-directory)
              (list*
               'excl::without-interrupts
               'excl::delete-directory
               broken-symbols)))

    #+lispworks
    (mapc (lambda (sym) (shadow sym utility-package))
          (list*
           'cl:error              ;; shadowed for error reporting
           'cl:warn               ;; shadowed for error reporting
           'cl:check-type         ;; shadowed for error reporting
           broken-symbols))

    #+lispworks
    (flet ((install-utility-symbol (symbol-name built-in-package)
             (let* ((symbol (intern symbol-name utility-package)))
               (import symbol built-in-package)
               (export symbol built-in-package))))

      (let ((sys-package (find-package "SYS")))
        (install-utility-symbol "COMMAND-LINE-ARGUMENTS" sys-package)
        (install-utility-symbol "COMMAND-LINE-ARGUMENT" sys-package)
        (install-utility-symbol "REAP-OS-SUBPROCESS" sys-package)
        (install-utility-symbol "*TILDE-EXPAND-NAMESTRINGS*" sys-package)
        (import 'mp::*current-process* sys-package)
        (export '(mp::*current-process*) sys-package))

      (let ((mp-package (find-package "MP")))
        (install-utility-symbol "MAKE-PROCESS-LOCK" mp-package)
        (install-utility-symbol "PROCESS-ADD-RUN-REASON" mp-package)
        (install-utility-symbol "PROCESS-ALLOW-SCHEDULE" mp-package)
        (install-utility-symbol "PROCESS-PROPERTY-LIST" mp-package)
        (install-utility-symbol "PROCESS-REVOKE-RUN-REASON" mp-package)
        (install-utility-symbol "WITH-PROCESS-LOCK" mp-package)
        (install-utility-symbol "WITH-TIMEOUT" mp-package)
        (install-utility-symbol "WITHOUT-SCHEDULING" mp-package)
        (export '(mp::make-process) mp-package)))

    ;; patch-up the third-party packages
    (mapc (lambda (package-name)
            (let ((package (find-package package-name)))
              (when package
                (mapc (lambda (sym)
                        (shadowing-import
                         (intern (symbol-name sym) utility-package) package))
                      broken-symbols))))
          '(
            "MAKE"
            ;; "HTML-PARSER"
            ;; "LSP"
            ;; "LSP-USER"
            ;; "PLIST"
            ;; "PUBLISHER"
            "SERIES"
            ;; "TOKENIZER"
            ))

    ;; If allegrostore isn't loaded, we fake up the package
    ;; so we can load ChangeSafe.

    #+allegro
    (or (member :allegrostore *features*)
        (find-package "ALLEGROSTORE")
        (make-package "ALLEGROSTORE" :nicknames '("ASTORE") :use '("COMMON-LISP")))

    (let* (
           ;; MKUTILS packages
           (make-package               (or (find-package "MAKE")
                                           (make-package "MAKE"
                                                         :nicknames '("MK")
                                                         :use '("COMMON-LISP"))))

           (source-compare-package     (or (find-package "SOURCE-COMPARE")
                                           (make-package "SOURCE-COMPARE"
                                                         :nicknames '("SC")
                                                         :use '("COMMON-LISP"))))

           ;; (regression-test-package    (or (find-package "RT")
           ;;                                 (make-package "RT"
           ;;                                    :use '("COMMON-LISP"))))

           ;; AllegroServe packages
           #-allegro
           (meta-package               (or (find-package "META")
                                           (make-package "META"
                                                         :use '("COMMON-LISP"
                                                                "CSF/CONFIG"
                                                                "CSF/UTILITY"))))

           #-allegro
           (fake-excl-package          (or (find-package "FAKE-EXCL")
                                           (make-package "FAKE-EXCL"
                                                         :nicknames '("EXCL")
                                                         :use '("COMMON-LISP"
                                                                "CSF/CONFIG"
                                                                "CSF/UTILITY"))))
           #-allegro
           (socket-package             (or (find-package "SOCKET")
                                           (make-package "SOCKET"
                                                         :use '("COMMON-LISP"
                                                                #+lispworks "COMM"
                                                                "CSF/CONFIG"
                                                                "CSF/UTILITY"))))

           (net.uri-package            (or (find-package "NET.URI")
                                           (make-package "NET.URI"
                                                         :use '("COMMON-LISP"
                                                                #+allegro "EXCL"
                                                                #-allegro "FAKE-EXCL"
                                                                ))))

           (net.html.generator-package (or (find-package "NET.HTML.GENERATOR")
                                           (make-package "NET.HTML.GENERATOR"
                                                         :nicknames '("HTMLGEN")
                                                         :use '("COMMON-LISP"
                                                                "FAKE-EXCL"))))
           #-allegro
           (net.aserve-package         (or (find-package "NET.ASERVE")
                                           (make-package "NET.ASERVE"
                                                         :use '("COMMON-LISP"
                                                                "FAKE-EXCL"
                                                                "NET.HTML.GENERATOR"
                                                                "NET.URI"))))

;           ;; ;; Lisp Server Pages Packages
;           (lsp-publisher-package      (or (find-package "PUBLISHER")
;                                           (make-package "PUBLISHER"
;                                                         :use '("COMMON-LISP"))))

;           (tokenizer-package          (or (find-package "TOKENIZER")
;                                           (make-package "TOKENIZER"
;                                                         :nicknames '("TK")
;                                                         :use '("COMMON-LISP"))))

;           (property-list-package      (or (find-package "PROPERTY-LIST")
;                                           (make-package "PROPERTY-LIST"
;                                                         :nicknames '("PLIST")
;                                                         :use '("COMMON-LISP"))))

;           (html-parser-package        (or (find-package "HTML-PARSER")
;                                           (make-package "HTML-PARSER"
;                                                         :use '("COMMON-LISP"
;                                                                "TOKENIZER"
;                                                                "PROPERTY-LIST"))))

;           (lsp-package                (or (find-package "LSP")
;                                           (make-package "LSP"
;                                                         :use '("COMMON-LISP"
;                                                                "HTML-PARSER"))))

;           (lsp-user-package           (or (find-package "LSP-USER")
;                                           (make-package "LSP-USER"
;                                                         :use '("COMMON-LISP"
;                                                                "LSP"))))

           ;; ;; ChangeSafe core packages

           (fixnum-package        (or (find-package "FIXNUM-MATH")
                                      (let ((pkg (make-package "FIXNUM-MATH"
                                                               :nicknames '("FIX")
                                                               :use '())))
                                        (shadow (list "=" "/=" "<" ">" "<=" ">="
                                                      "MAX" "MIN"
                                                      "MINUSP" "PLUSP"
                                                      "FLOOR" "CEILING"
                                                      "*" "+" "-" "/" "1+" "1-"
                                                      "ABS" "EVENP" "ODDP"
                                                      "GCD" "INCF" "DECF"
                                                      "LCM" "ASH"
                                                      "LOGAND" "LOGANDC1" "LOGANDC2"
                                                      "LOGEQV" "LOGIOR" "LOGNAND"
                                                      "LOGNOR" "LOGNOT" "LOGORC1" "LOGORC2"
                                                      "LOGXOR" "LOGBITP" "LOGCOUNT" "LOGTEST"
                                                      "DEPOSIT-FIELD" "DPB" "LDB" "LDB-TEST"
                                                      "MASK-FIELD") pkg)
                                        (use-package (list (find-package "COMMON-LISP")
                                                           (find-package "CSF/CONFIG"))
                                                     pkg)
                                        pkg)))

           (index-package        (or (find-package "INDEX-MATH")
                                      (let ((pkg (make-package "INDEX-MATH"
                                                               :nicknames '("INDEX")
                                                               :use '())))
                                        (shadow (list "=" "/=" "<" ">" "<=" ">="
                                                      "MAX" "MIN"
                                                      "MINUSP" "PLUSP"
                                                      "FLOOR" "CEILING"
                                                      "*" "+" "-" "/" "1+" "1-"
                                                      "ABS" "EVENP" "ODDP"
                                                      "GCD" "INCF" "DECF"
                                                      "LCM" "ASH"
                                                      "LOGAND" "LOGANDC1" "LOGANDC2"
                                                      "LOGEQV" "LOGIOR" "LOGNAND"
                                                      "LOGNOR" "LOGNOT" "LOGORC1" "LOGORC2"
                                                      "LOGXOR" "LOGBITP" "LOGCOUNT" "LOGTEST"
                                                      "DEPOSIT-FIELD" "DPB" "LDB" "LDB-TEST"
                                                      "MASK-FIELD") pkg)
                                        (use-package (list (find-package "COMMON-LISP")
                                                           (find-package "CSF/CONFIG"))
                                                     pkg)
                                        pkg)))

;          (omnicode-package          (or (find-package "CSF/OMNICODE")
;                                        (make-package "CSF/OMNICODE"
;                                                      :nicknames '("OMNICODE")
;                                                      :use '("COMMON-LISP"
;                                                             "CSF/UTILITY"
;                                                             "CSF/CONFIG"))))

           (persistent-store-package (or (find-package "CSF/PERSISTENT-STORE")
                                         (make-package "CSF/PERSISTENT-STORE"
                                                       :nicknames '("CSF/PSTORE" "PSTORE")
                                                       :use '("COMMON-LISP"
                                                              "CSF/UTILITY"
                                                              "CSF/CONFIG"))))

           (versioned-file-package (or (find-package "CSF/VERSIONED-FILE")
                                       (make-package "CSF/VERSIONED-FILE"
                                                     :nicknames '("CSF/VFILE" "VFILE")
                                                     :use '("COMMON-LISP"
                                                            "CSF/UTILITY"
                                                            "CSF/CONFIG"))))


           (java-tools-package     (or (find-package "CSF/JAVA-TOOLS")
                                       (make-package "CSF/JAVA-TOOLS"
                                                     :nicknames '("JAVA")
                                                     :use '("COMMON-LISP"
                                                            "CSF/UTILITY"
                                                            "CSF/CONFIG"))))

           (core-package               (or (find-package "CSF/CORE")
                                           (make-package "CSF/CORE"
                                                         :use '("COMMON-LISP"
                                                                "CSF/UTILITY"
                                                                "CSF/CONFIG"
                                                                "CSF/PERSISTENT-STORE"
                                                                "CSF/VERSIONED-FILE"
                                                                #+aclpc "ALLEGRO"
                                                                ))))
           (web-package                (or (find-package "CSF/WEB")
                                           (make-package "CSF/WEB"
                                                         :use '("COMMON-LISP"
                                                                "CSF/UTILITY"
                                                                "CSF/CONFIG"
                                                                ))))

           (vm-package                 (or (find-package "CSF/VERSION-MANAGEMENT")
                                           (make-package "CSF/VERSION-MANAGEMENT"
                                                         :nicknames '("VM")
                                                         :use '("COMMON-LISP"
                                                                "CSF/CORE"
                                                                "CSF/PERSISTENT-STORE"
                                                                "CSF/UTILITY"
                                                                "CSF/CONFIG"
                                                                ))))

           (server-package             (or (find-package "CSF/SERVER")
                                           (make-package "CSF/SERVER"
                                                         :use '("COMMON-LISP"
                                                                "CSF/CORE"
                                                                "CSF/UTILITY"
                                                                "CSF/CONFIG"
                                                                "CSF/WEB"
                                                                ))))

           (rfm-package                (or (find-package "CSF/REPOSITORY-FILE-MANAGEMENT")
                                           (make-package "CSF/REPOSITORY-FILE-MANAGEMENT"
                                                         :nicknames '("RFM")
                                                         :use '("COMMON-LISP"
                                                                "CSF/CONFIG"
                                                                "CSF/CORE"
                                                                "CSF/PERSISTENT-STORE"
                                                                "CSF/SERVER"
                                                                "CSF/UTILITY"
                                                                "CSF/VERSION-MANAGEMENT"
                                                                "CSF/WEB"
                                                                ))))

           ;; ChangeSafe product packages
           (conman-package                 (or (find-package "CONMAN")
                                               (make-package "CONMAN"
                                                             :use '("COMMON-LISP"
                                                                    "CSF/CONFIG"
                                                                    "CSF/CORE"
                                                                    "CSF/PERSISTENT-STORE"
                                                                    "CSF/REPOSITORY-FILE-MANAGEMENT"
                                                                    "CSF/SERVER"
                                                                    "CSF/UTILITY"
                                                                    "CSF/VERSION-MANAGEMENT"
                                                                    "CSF/WEB"
                                                                    ))))
           ;; ChangeSafe gui packages
           (changesafe-package         (or (find-package "CHANGESAFE")
                                           (make-package "CHANGESAFE"
                                                         :USE '("COMMON-LISP"
                                                                "CSF/UTILITY"
                                                                "CSF/CONFIG"
                                                                "CONMAN"
                                                                "CSF/CORE"
                                                                "CSF/SERVER"
                                                                "CSF/PSTORE"
                                                                "NET.ASERVE"
                                                                "NET.HTML.GENERATOR"
                                                                ;; "PUBLISHER"
                                                                ;; "LSP-USER"
                                                                ))))
           )

      ;; These stub definitions must be set before we proceed.
      (unless (member :dont-fix-franz *features*)
        (mapc (lambda (original)
                (let ((sym (find-symbol (symbol-name original) utility-package))
                      (fun (symbol-function original)))
                  (when (functionp fun)
                    (when verbose
                      (let ((*package* (find-package :keyword)))
                        (format *trace-output* "~&; Installing stub for ~s" sym)))
                    (setf (symbol-function sym) fun))))
              broken-symbols))

      ;; Special stub definitions
      (setf (symbol-function (intern "CALL-WITH-DEFERRED-INTERRUPTS" utility-package))
            (lambda (reason thunk)
              (declare (ignore reason))
              (cl:funcall thunk #'cl:funcall)))

      ;; Install more verbose error handler
      (mapc (lambda (package)
              (when package
                (shadowing-import (intern "CHECK-TYPE" utility-package) package)
                (shadowing-import (intern "ERROR" utility-package) package)
                (shadowing-import (intern "WARN" utility-package) package)))
            (list
             changesafe-package
             conman-package
             core-package
             fake-excl-package
             fixnum-package
             ; html-parser-package
             index-package
             java-tools-package
             ; lsp-package
             ; lsp-publisher-package
             ; lsp-user-package
             meta-package
             net.aserve-package
             net.html.generator-package
             net.uri-package
             persistent-store-package
             ; property-list-package
             ; omnicode-package
             rfm-package
             server-package
             socket-package
             ; tokenizer-package
             versioned-file-package
             vm-package
             web-package))
      (setf (macro-function (intern "COND" utility-package))
            (lambda (form env) (declare (ignore env)) `(CL:COND ,@(cdr form))))
      (setf (macro-function (intern "CHECK-TYPE" utility-package))
            (lambda (form env) (declare (ignore env)) `(CL:CHECK-TYPE ,@(cdr form))))
      (setf (macro-function (intern "ERROR" utility-package))
            (lambda (form env) (declare (ignore env)) `(CL:ERROR ,@(cdr form))))
      ;; (setf (macro-function (intern "THE" utility-package))
      ;;       (lambda (form env) (declare (ignore env)) `(CL:THE ,@(cdr form))))
      (setf (macro-function (intern "WARN" utility-package))
            (lambda (form env) (declare (ignore env)) `(CL:WARN ,@(cdr form))))

      (unless (member :dont-fix-franz *features*)
        (mapc (lambda (package)
                (when package
                  (mapc (lambda (sym)
                          (shadowing-import
                           (intern (symbol-name sym) utility-package) package))
                        broken-symbols)))
              (list
               changesafe-package
               conman-package
               core-package
               fake-excl-package
               fixnum-package
               index-package
               java-tools-package
               ; lsp-package
               ; lsp-publisher-package
               ; lsp-user-package
               #-allegro meta-package
               make-package
               net.aserve-package
               net.html.generator-package
               net.uri-package
               ; omnicode-package
               persistent-store-package
               ; property-list-package
               rfm-package
               server-package
               socket-package
               ; tokenizer-package
               utility-package
               versioned-file-package
               vm-package
               web-package)))

      (set (intern "*CHANGESAFE-PACKAGES*" changesafe-package)
           (list changesafe-package
                 conman-package
                 (find-package "CSF/CONFIG")
                 core-package
                 fixnum-package
                 index-package
                 ; omnicode-package
                 persistent-store-package
                 rfm-package
                 server-package
                 utility-package
                 versioned-file-package
                 vm-package
                 web-package))
      )))

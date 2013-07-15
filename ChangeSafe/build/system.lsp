;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright (c) 2000 Content Integrity, Inc.
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
;;;; File Name:     system.lsp
;;;; Author:        jrm
;;;;
;;;; Module Description:  Master load file for ChangeSafe.
;;;;
;;;; When adding files to system definitions, only put one file per
;;;; line so that source-compare and merging utilities can do their
;;;; jobs without requiring the user to resolve conflicts.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "USER")

(mk:defsystem :ansi-series
    :source-pathname #.(logical-pathname "TS50:ANSI-SERIES;")
    :source-extension "lisp"
    :components ((:file "s-package")
		 (:file "s-code")
		 (:file "s-install"
			:load-only t)))

(mk:defsystem :ansi-series-tests
    :source-pathname #.(logical-pathname "TS50:ANSI-SERIES;")
    :depends-on (ansi-series)
    :components ((:file "s-test")))

(mk:defsystem :ts
    :source-pathname #.(logical-pathname "TS50:;")
    :source-extension "lsp"
    :language :lisp
    :components
    (
     ;; No code in here, just constants.
     (:module version
	      :source-pathname "build"
	      :components ("version"))

     ;; File "declarations" controls some compilation settings.  It should
     ;; compiled and loaded first.
     (:module utility
	      :source-pathname "utility"
	      :components (
			   ;; NO FILES BEFORE DECLARATIONS
			   "declarations"
			   "utility-macros"	; macros used by utility functions
			   ;; "process-fix"        ; fix franz brain damage (race conditions)
			   "promise"
			   "protect"
			   "fixnum" ; fixnum math
			   "ascii"
			   "date-time"
			   "platform"
			   #+microsoft-32 "homedir-fix" ; fix franz brain damage (home directory not obeyed)
			   #+unix "pathname-fix" ; fix franz brain damage (pathnames)
			   "pathname-hacks"
			   "replacement-functions"
			   "queue"
			   "multiprocessing"
			   "notification"
			   #+allegro "subprocess" ; real subprocess management
			   "utils"
			   ;; "utils2"
			   "caterpillar"
			   "blob-sb32vec"
			   "astore"
			   "db-name"
			   "yarrow"
			   "wttree"
			   "rbtree"
			   "oset"
			   "graph"
			   "crc"
			   "base64"
			   "formatting"
			   "validate"
			   "self"
			   "random"
			   "cache-mgr"
			   "serve"
			   "file-utils"
			   "psbit"
			   ))
     (:module source-compare
	      :source-pathname "source-compare"
	      :components ("sc"		;SC in package SOURCE-COMPARE AKA SC.  Used for redundant
					;diff testing, not intrinsic part of product
			   ))

     ;; Win32 API
     #+microsoft-32
     (:module win32
	      :source-pathname "win32"
	      :components ("win32"
			   "types"
			   "errors"
			   "functions"
			   "ole"))
     (:module java-components
	      :source-pathname "java/auxiliary"
	      :components ("java-tools"
			   "makejava")
	      :depends-on (utility))
     (:module clos-finalize
	      :source-pathname "build"
	      :components ("clos-finalize")
	      :depends-on ())
     (:module core
	      :source-pathname "core"
	      :components ("core"
			   "test-substrate"
			   "generic"
			   "canonical"
			   "distributed"
			   "cid-set"
			   "txn-context"
			   "txn-mutex"
			   "repository"
			   "vi"
			   "rsdiff"
			   "vi-eviscerated"
			   "cvi"
			   "cvi-report-stream"
			   "reports"
			   "core-user"
			   "tests"
			   "package-lock"
			   "schema-compare")
	      :depends-on (utility))
     (:module web
	      :source-pathname "web"
	      :components ("web"
			   "inet-headers"
			   "smtp"
			   ;; Use the Franz URI code (with some of our extensions)
			   #-allegro "url"
			   #+allegro "uri-plus" ;; extensions to Franz URI code
			   "images"
			   "html-request"
			   "http-connection"
			   "html-response"
			   "js-tooltip"
			   "http-server"
			   "http-authentication"
			   "http-client"
			   "html-link-scanner"
			   "web-walker"
			   "testing"
			   )
	      :depends-on (utility))
     ;;     #+microsoft-32
     ;;     (:module frontpage
     ;;       :source-pathname "frontpage"
     ;;       :components ("frontpage")
     ;;       :depends-on (win32))      ; Could use utility if we want to...
     (:module server
	      :source-pathname "server"
	      :components ("server"
			   "session"
			   "fs-record-stream"
			   "file-system"
			   ;;#+microsoft-32 "frontpage"
			   ;;#-microsoft-32 "nofrontpage"
			   "nofrontpage"
			   "content-server"
			   "rweb-server"
			   "iis-rweb-server"
			   "tests")
	      :depends-on (web core ;; #+microsoft-32 frontpage
			       ))
     (:module version-management
	      :source-pathname "vm"
	      :components ("vm"
			   "generic"
			   "named-object"
			   "described-object"
			   "change-set"
			   "vm-txn"
			   "container"
			   "version"
			   "branch"
			   "project"
			   "project-context"
			   "versionref"	;semipersistent non-versioned equivalent of project-context
			   ;; "vm-session-context" - for those parts of context in VM package
			   ;; then have scm-session-context inherit/user it?
			   )
	      :depends-on (core))
     (:module repository-file-management
	      :source-pathname "rfm"
	      :components ("rfm"
			   "rfm-project"
			   "mime-content" ; add "type-registry" at some point
			   "change-context"
			   "go-menu"
			   "repository-file-system"
			   "website"
			   "webserver"
			   "rfm-user"
			   ;; This is moribund and should go away altogether.
			   #+allegrostore "rfm-workspace"
			   "x-vm"
			   "rfm-session-context"
			   "rfm-server"
			   "rfm-http-server"
			   "rfm-http-tech"
			   "publish"
			   "merge"
			   "rfm-http-ui-tools"
			   "rfm-http-ui-user"
			   "rfm-http-ui"
			   "tests"
			   )
	      :depends-on (version-management server))
     (:module conman
	      :source-pathname "conman"
	      :components ("conman"
			   "cm-cli-command-syntax-constants"
			   "cm-returns"	;return codes
			   "conditions"	;condition handlers and signallers
			   ;; Debugging and data monitoring
			   "server-log"
			   ;; Performance monitoring
			   "perflog"
			   ;; Session
			   "cm-session-context"
			   ;; intermediary between CONMAN and CHANGE-CONTEXTs
			   "conman-change-context"
			   ;; Semi-persistent model
			   "workspace"
			   "workspace-master"
			   "workspace-satellite"
			   ;; Versioned persistent Model
			   "cm-txn"
			   "satellite-project-ref"
			   "wall"	;walls in subsystems
			   "subsystem"	;SUBSYSTEM and SUBSYSTEM-SUBSCRIBER objects
			   "subsystem-satellite" ;subsystem objects in satellite txn/version contexts
			   ;;"merge"            ; merge functionality
			   "master-catalog"
			   "cm-branch"
			   "pc"
			   "pc-satellite"
			   ;; other substrate
			   "workspace-collection"
			   "class-name-collection"
			   "pinfo"
			   ;; Controller
			   "cmctl"
			   "cmctl-ro"
			   "update-db"
			   ;; UI glue layer
			   "gui-glue"
			   ;; report server
			   "cm-reports"
			   "cm-port"
			   ;; View
			   "alias"
			   "cli-request"
			   "help-doc"
			   "cm-cli"
			   "flux"
			   "cm-http"
			   "csf-patch"
			   "changesafe-server"
			   "main"
			   ;; Regressions
			   "tests"
			   "test-db-gen"
			   "bulktests"
			   ;; utility
			   "self-load"
			   )
	      :depends-on (repository-file-management))
     ))

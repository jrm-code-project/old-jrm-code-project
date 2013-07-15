;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;	      Copyright (c) 1999 Content Integrity, Inc.
;;;;	      ALL RIGHTS RESERVED.
;;;;
;;;;	      Content Integrity, Inc. CONFIDENTIAL and PROPRIETARY material.
;;;;
;;;;	      Content Integrity, Inc
;;;;	      Braintree Executive Office Park
;;;;          P.O. Box 850942
;;;;	      Braintree, MA 02185-0942
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
;;;; File Name:	    delivery.lsp
;;;; Author:        Dave Bakhash
;;;; Creation Date: 29 Nov 1999
;;;;
;;;; Module Description:
;;;;
;;;; Code for delivering Conman as a packaged lisp application.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :user)

;; Create the executable
(defun deliver-conman (&key runtime (recompile-p t) debug-p suppress-regressions verbose)
  "Package up ChangeSafe as a lisp application, delivering necessary files
   into the build\delivery directory.

   ** Don't run this yourself.  It's only to be run from TS-LOAD.

   RUNTIME is simply passed on to generate-application.
   It should be :standard to generate a legal runtime environment - which excludes
   the compiler and other components not legally redistributible.

   RECOMPILE-P should normally be true for production system delivery,
   it causes all lisp files to be recompiled.
   (*TBD*: should probably recompile java too)

   DEBUG-P should normally be false for production system delivery.  If true
   it generates a lisp application in which some debugging tools are enabled.
   The image will be substantially larger and may incur performance penalties.

   SUPPRESS-REGRESSIONS should normally be nil.  If true, we will not run the regression tests
   to train the CLOS caches. This is sometimes useful if you're debugging the system build process,
   since regression tests can take a long time to run."

  (when (and (find-package "UTILITY")
	     ;; Utility package could exist, but have no symbols in it.
	     ;; Assume that if *disable-debug-messages* is interned, that
	     ;; the rest are, too.
	     (find-symbol "*DISABLE-DEBUG-MESSAGES*" (find-package "UTILITY")))
    ;; Set up flags for delivery.
    (set (find-symbol "*DISABLE-DEBUG-MESSAGES*"     (find-package "UTILITY")) t)
    (set (find-symbol "*DEFAULT-DEBUG-DECLARATION*"  (find-package "UTILITY")) 1)
    (set (find-symbol "*DEFAULT-SAFETY-DECLARATION*" (find-package "UTILITY")) 3)
    (set (find-symbol "*DEFAULT-SPACE-DECLARATION*"  (find-package "UTILITY")) 0)
    (set (find-symbol "*DEFAULT-SPEED-DECLARATION*"  (find-package "UTILITY")) 1)
    )
  (when (or recompile-p (not (find-package :conman)))
    (ts-load :clean-lisp recompile-p :make-java nil :verbose verbose)) ; load the packages

  (unless suppress-regressions
    ;; Invoke the regression system to 'train' the clos caches
    (funcall (find-symbol "TEST-ALL"
			  (find-package :core))
	     1
	     :test-suites :all
	     :interactive nil
	     :update-mst-files nil))
  (compile-file (translate-logical-pathname #p"TS50:BUILD;closopt.lsp"))			; create `closopt.fasl'
  ;; Compute parameters for heap layout.
  (let* ((megabytes (expt 2 20))

	 ;; These values were empirically deteremined.
	 ;; I started with the default values that ObjectStore
	 ;; uses (as per the ObjectStore Management manual),
	 ;; because I assume that the default values should
	 ;; be a better choice than any other random value.
	 ;; But since the default persistent storage region on
	 ;; HP is only about 300MB, I increased the OS_AS_SIZE
	 ;; and moved the lower boundary down to accomodate
	 ;; more storage.
	 #+hpux (recommended-os-as-start #x280000000)
	 #+hpux (recommended-os-as-size  (* 2048 megabytes))

;	 #+hpux (recommended-os-as-start (* (floor
;					     (- #x68FA8000 ;; default ending address
;						recommended-os-as-size)
;					     #x10000) #x10000)) ;; must align

	 ;; Now the standard location of the C heap in Allegro is smack
	 ;; in the middle of the ObjectStore region.  Boneheads.
	 ;; Moving it below the ObjectStore region doesn't seem to work,
	 ;; so we move it above.  (I think that SBRK is set after
	 ;; the C heap, so that allocations made after we boot were
	 ;; crawling up into the ObjectStore region.  HP says that SBRK
	 ;; and MMAP don't play well together, so we'll try it this way.

	 ;; Since we don't really use that much C heap, the default
	 ;; size (16mb) is ok.

	 ;; As far as I know, there is no need to allow any extra
	 ;; room beyond it, but what do I know?  There's some room
	 ;; after it in any case.
	 #+hpux (c-heap-start #x70000000)
	 #+hpux (c-heap-size  (* 16 megabytes))

	 ;; The Lisp heap ought to be resized to keep it from running into
	 ;; the ObjectStore region.  However, if you specify the correct
	 ;; size the build will fail.  So we just set it to 64MB.
	 #+hpux (lisp-heap-start #x10000000)
	 #+hpux (lisp-heap-size  (* 64 megabytes))
	 )
    (declare (ignorable megabytes))

    (excl:generate-application
     "changesafe.exe"				; application name
     ;; Destination directory, ts50\bin
     (ts50-application-target-directory)
     ;; don't ask why I have this rediculous `dload.lsp' file.  It only
     ;; contains one form: (ts-load).  I tried handing that initializer
     ;; to the `:restart-init-function', but to no avail.  I definately
     ;; didn't want to add this form to the end of load.lsp, so this is
     ;; my compromise, until I figure it out.
     ;;
     `(;; ORDER MATTERS HERE, SO BE CAREFUL
       :defftype				; init files
       :process
       :uri
       :allegrostore
       :fileutil
       ,(translate-logical-pathname #p"TS50:BUILD;load.lsp")
       ,(translate-logical-pathname #p"TS50:BUILD;dload.lsp")
       ,(translate-logical-pathname #p"TS50:BUILD;closopt.fasl") 	;generated by the training phase of the build
       )

     :runtime runtime				; nil, :standard, or :dynamic

     :include-devel-env (if runtime nil debug-p)

     :build-executable
     ;; Do NAMESTRING of this since on Windows, ACL bombs out trying to
     ;; do LENGTH of this.
     (namestring
      #+MSWindows (translate-logical-pathname "SYS:lisp.exe")
      #+hpux (translate-logical-pathname "SYS:aslisp"))

     ;; Additional command line arguments to aslisp
     :additional-arguments nil

     :wait t					; On Windows, so the window of the process that's
						; building the image won't close automatically.
     :allow-existing-directory nil		; forces you to wipe out any
						; existing delivery directory
						; so you have a clean one
						; every time
     :application-files '(			; files which will be copied
			  "SYS:astore13.adb"
			  #+MSWindows "SYS:ashook.dll"
			  )
						; into the delivery directory
     :application-type :exe
     :autoload-warning t			; do output autoload warnings
     :application-administration '()
     :copy-shared-libraries t			; we hope this is unnecessary,
						; but it may be necessary for
						; things like astore
     ;; keyword args for #'excl:build-lisp-image
     :case-mode :case-insensitive-upper		; keep case standard for delivery
     :dst t					; observe daylight savings time

     :include-debugger debug-p
     :include-compiler (and debug-p (not runtime))
     :discard-compiler nil			; this seems necessary in
						; order to make the build
						; process return a safe error
						; status.  intuitively, one
						; would put (not debug-p) but
						; that exits with status 1.
     :include-tpl t				; necessary for successful delivery
     :include-common-graphics nil		; discard common-graphics package
     :include-ide nil				; even during development, we
						; don't use the IDE

     :load-source-file-info    debug-p
     :record-source-file-info  debug-p
     :discard-source-file-info (not debug-p)

     :load-local-names-info    debug-p
     :discard-local-name-info  (not debug-p)

     :record-xref-info (and debug-p (not runtime))
     :load-xref-info   nil			; start clean for xrefs, even during the testing phase
     :discard-xref-info nil

     :discard-arglists (not debug-p)

     :debug-on-error t

     :read-init-files nil			; absolutely discard
						; customizations during the build

     :restart-init-function nil			; no init function
     :dribble-file (merge-pathnames "delivery.log" (ts50-application-target-directory))
     :exit-after-image-build t			; (not debug-p)

     ;; :INTERNAL-DEBUG should be a string, T or NIL
     :internal-debug (namestring (merge-pathnames "build.out" (ts50-application-target-directory))) ;debug-p

     :preserve-documentation-strings t		; debug-p
     :show-window :normal			; show transcript during delivery
     :splash-from-file nil			; may later be a bitmap
     :verbose t					; only affects messages during build
     :wait debug-p
     ;; optimization decls -- by commenting these out, we preserve the
     ;; values used in the :utility package.  however, since #'ts-load
     ;; loads compiled files, it's how the files were compiled in the
     ;; first place which determine optimization levels (especially
     ;; since the application currently does not use the compiler.
     ;;
     ;; :opt-debug (if debug-p 3 0)
     ;; :opt-safety (if debug-p 1 2)
     ;; :opt-space (if debug-p 0 1)
     ;; :opt-speed (if debug-p 0 2)
     ;;

     ;; make sure the c heap ends up someplace reasonable
     #+hpux   :lisp-heap-start #+hpux lisp-heap-start
     #+hpux   :lisp-heap-size  #+hpux lisp-heap-size
     #+hpux   :c-heap-start    #+hpux c-heap-start
     #+hpux   :c-heap-size     #+hpux c-heap-size

     ;; finally, the main function to call (must not return)
     :restart-app-function (find-symbol "CHANGESAFE-SERVER-TOP-LEVEL"
					(find-package :conman)))))

(defun ts50-application-target-directory ()
  "Returns the pathname of the directory to which the application and
   other associated files should be written."
  ;; We base the name of the target directory on the platform we are
  ;; building for.  That way they don't get mixed up if the TS50
  ;; hierarchy is on a shared server.
  (namestring (merge-pathnames (make-pathname :directory '(:relative "build" "delivery"
							   #+MSWindows "MSWindows"
							   #+hpux "hpux")
					      :defaults nil)
			       (ts-source-directory-pathname))))






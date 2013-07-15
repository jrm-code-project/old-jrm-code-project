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
;;;; File Name:     load.lsp
;;;; Author:        jrm
;;;;
;;;; Module Description:  Master load file for ChangeSafe.
;;;;
;;;; When adding files to system definitions, only put one file per
;;;; line so that source-compare and merging utilities can do their
;;;; jobs without requiring the user to resolve conflicts.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+lispworks (in-package "CHANGESAFE")

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq excl:*cltl1-in-package-compatibility-p* t))

#+allegro (in-package :user)
#+allegro (export '(ts-source-directory-pathname))

;; Compare only the names of files loaded/compiled, not the full device and other pathname
;; qualifiers..  If you compile and load file, then C-C C-x the module to recompile it,
;; you'll get a warning about pathnames not matching (differ in device spec).  This eliminates the
;; relatively worthless and definitely annoying warning.

#+allegro
(push (lambda (old new fspec type)
	  (declare (ignore fspec type))
	  ;; OLD or NEW can be NIL if redefining at top level.
	  (and (or (stringp old) (pathnamep old))
	       (or (stringp new) (pathnamep new))
	       (string-equal (pathname-name old) (pathname-name new))))
      excl:*redefinition-pathname-comparison-hook*)

;;;
;;; System definition and load logic
;;;

(defparameter *application-directory*
    #||
  ;; This is bogus and doesn't work when delivering.
  (let ((command-path (parse-namestring (sys:command-line-argument 0))))
    (make-pathname :device (pathname-device command-path)
		   :directory (pathname-directory command-path)))
  ||#
  (translate-logical-pathname "SYS:")
  "Directory where ACL is installed, from which we may which to load system code.")

;;; This supposed no-op allows us to use NIL as a host.
;;; I don't pretend to understand how this could do anything.

;#+lispworks
;(defadvice (make-pathname allow-nil-host :around)
;    (&rest args)
;  (call-next-advice))

;  (if (and host-supplied-p
;	   (null host))
;      (let ((new-args (copy-list args)))
;	(setf (getf new-args :host) "")
;	(apply #'make-pathname new-args))
;      (call-next-advice)))

(defparameter *ts-source-directory*
    (or (and *load-pathname*
	     (merge-pathnames
	      (make-pathname :directory '(:relative :back)
			     ;; :defaults nil
			     )
	      (make-pathname :name nil
			     :type nil
			     :version nil
			     :defaults *load-pathname*)))
	#+Franz-Inc (current-directory)))

(defun ts-source-directory-pathname (&optional (filename ""))
  (let ((pathname (if (stringp filename)
		      (parse-namestring filename)
		    filename)))
    (merge-pathnames pathname *ts-source-directory*)))

(defun ts-temporary-directory ()
  #+allegro (sys:temporary-directory)
  #+lispworks (let ((file nil))
		(unwind-protect
		    (progn (setq file (make-temp-file))
			   (make-pathname :host (pathname-host file)
					  :device (pathname-device file)
					  :directory (pathname-directory file)
					  :name nil
					  :type nil
					  :version nil))
		  (when file (delete-file file))))
  #-(or allegro lispworks)
  (error "Need a temporary directory."))

(defun ts-standard-path-translations ()
  "Set the logical path translations for TS50.  This is now a function so that it can
   be re-evaluated after the pathname-hacks file is loaded."
  (labels ((relative-subdirectory (&rest elements)
	     (ts-source-directory-pathname (make-pathname :directory `(:relative ,@elements :wild-inferiors)
							  :name :wild
							  :type :wild
							  ;; :defaults nil
							  )))
	   (wild-suffix (prefix)
	     (concatenate 'string (string-upcase prefix) ";**;*.*"))

	   (standard-translation (subdir)
	     (list (wild-suffix subdir) (relative-subdirectory subdir))))

    (setf (logical-pathname-translations "TS50")
      (list
       (standard-translation "ansi-series")
       (standard-translation "build")
       (standard-translation "conman")
       (standard-translation "core")
       (standard-translation "defsystem")
       (standard-translation "java")
       (standard-translation "java;fsa")
       (standard-translation "misc")
       ;; A `temporary' directory for testing
       (list "REPOSITORIES;**;*.*" (merge-pathnames
				    (make-pathname :directory `(:relative
								#+(and allegro unix) ,(sys:user-name)
								"ACL-temp" "repositories")
						   ;; :defaults nil
						   )
				    (ts-temporary-directory)))
       (standard-translation "readme")
       (standard-translation "rfm")
       (standard-translation "server")
       ;; A `temporary' directory for testing
       (list "SEMITEMP;**;*.*" (merge-pathnames
				(make-pathname :directory `(:relative
							    #+(and allegro unix) ,(sys:user-name)
							    "ACL-temp" "semitemp")
					       ;; :defaults nil
					       )
				(ts-temporary-directory)))
       (list ";" (ts-source-directory-pathname))
       (list "TS-PACKAGES;**;*.*" (merge-pathnames
				   (make-pathname :directory `(:relative
								#+(and allegro unix) ,(sys:user-name)
							       "ACL-temp" "packages")
						  ;; :defaults nil
						  )
				   (ts-temporary-directory)))
       (list "TS-SERVER;**;*.*" (merge-pathnames
				   (make-pathname :directory `(:relative
								#+(and allegro unix) ,(sys:user-name)
							       "ACL-temp" "server")
						  ;; :defaults nil
						  )
				   (ts-temporary-directory)))
       (standard-translation "source-compare")
       (standard-translation "test-data")
       (standard-translation "utility")
       (standard-translation "vm")
       (standard-translation "web")
       (standard-translation "web-content")
       (standard-translation "win32")

       (list "PLATFORM-PATCHES;**;*.*"
	     (relative-subdirectory "acl-patches"
				    #+allegro-v6.0 "6.0"
				    #+allegro-v5.0.1 "5.0.1"
				    #-allegro "oops!"
				    #+hpux "hpux"
				    #+MSWindows "win32"))))))

#+allegro
(eval-when (:load-toplevel :execute)
  ;; Standard Allegro options.
    (require :foreign)
    ;; User should have passed -L code/allegrostore.fasl,
    ;; but we are lazy.  Check other possibilities.
    (when (or (member :allegrostore *features*)
	      (let* ((cmdl-args (sys:command-line-arguments :application nil))
		     (implied (or (member "-as"  cmdl-args :test #'string-equal)
				  (member "ashook" cmdl-args :test #'search)
				  (member "aslisp" cmdl-args :test #'search)
				  (member "astore.dxl" cmdl-args :test #'search))))
		(when implied
		  (warn "You should start lisp with -L SYS:code;allegrostore.fasl to load Allegrostore.~%~%"))
		implied))
      (require :allegrostore "SYS:code;allegrostore.fasl"))
    (require :sock)
    (require :trace)
    (require :process)
    (require :uri)
    )

(defun load-platform-patches (&key verbose)
  ;; need to differentiate between ACL5.0.1/Astore 1.3 and ACL6.0/Astore2.0
  ;; for now, just avoid doing this in Astore 2.0
  #-(and :allegro-version>= (:version>= 6 0))
  (loop with loaded-patches = nil
	for path in (append
		     (directory (make-pathname :name :wild
					       :type "lsp"
					       :defaults (translate-logical-pathname #p"TS50:PLATFORM-PATCHES;")))
		     (directory (make-pathname :name :wild
					       :type "fasl"
					       :defaults (translate-logical-pathname #p"TS50:PLATFORM-PATCHES;"))))
	as compiled-file = (merge-pathnames (make-pathname :type "fasl"
							  :defaults nil)
					   path)
	do
	(unless (probe-file compiled-file)
	  (compile-file path))
	(unless (find compiled-file loaded-patches :test #'equalp)
	  (push compiled-file loaded-patches)
	  (load compiled-file :verbose verbose))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warn about missing Lisp patches

#+allegro
(defun warn-if-missing-patch (system-keyword patch-name-string patch-version
			      reason)
  "Sometimes we depend on certain vendor provided patches being present
   in the lisp environment.  This function will warn if a patch isn't present."
  (loop for (name-string version-number . ignore)
	in (cdr (assoc system-keyword sys:*patches*))
	do (progn ignore)
	(when (and (string-equal patch-name-string name-string)
		   (>= version-number patch-version))
	  (return))
	finally (warn "The patch ~s ~s ~d isn't loaded. ~&~a"
		      system-keyword patch-name-string patch-version reason)))

#+allegro (compile 'warn-if-missing-patch)

;;; Patches required for ts50 in Franz AllegroCL:
;; in ACL 6.0/Astore 2.0, both patches are merged into the product
#+(and :allegro-version>= (not (:version>= 6 0)))
(progn
  (warn-if-missing-patch
   :astore "2g008" 2
   "This patch is required to allow ChangeSafe to run on a machine that isn't
    running an ObjectStore server.")
  (warn-if-missing-patch
   :astore "2g009" 2
   "This is the patch that provides ASTORE::OS-FIND-SYMBOL."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compile time and load time swicthes

;;; The presence of the :PROJECT-CHANGE-SETS-UNMAINTAINED feature
;;; causes all of the code which maintains the CHANGE-SETS slot of
;;; VM::PROJECT to be no-opped out.
(pushnew :project-change-sets-unmaintained *features*)

;;; This feature enables the use of persistent ftypes for cmte's
;;; Remove pftypes to reduce dependencies on astore.  ~jrm
;;;
;;; #+(and :allegro-version>= (:version>= 6 0))
;;;   (pushnew :pftypes *features*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *ts50-load-verbose* t
  "A special variable which is bound to the value of the VERBOSE argument of
   TS-LOAD while it is active.")

;;; Hooks

(defun run-hooks-function (hook-name hook-list &rest arguments)
  (with-simple-restart (abort-all
			"Skip processing remaining ~s." hook-name)
    (dolist (hook hook-list)
      (with-simple-restart (abort-one
			    "Skip processing hook ~s in ~s." hook hook-name)
	(apply hook ':allow-other-keys 't arguments)))))

(defmacro run-hooks (name &rest args)
  `(RUN-HOOKS-FUNCTION (QUOTE ,name) ,name ,@args))

(defun add-hook-1 (hook-function hook-variable)
  (unless (symbolp hook-function)
    (warn "~s is not a symbol, trying to add to ~s"
	  hook-function hook-variable))
  (when *ts50-load-verbose*
    (format *trace-output* "~&; Adding hook function ~s to ~s."
	    hook-function hook-variable))
  (set hook-variable (adjoin hook-function
			     (symbol-value hook-variable))))

(defmacro add-hook (name value)
  `(add-hook-1 ',value ',name))

#+allegro
(defmacro package-lossage-funcall (&whole form package symbol &rest arguments)
  "Allow this file to invoke functions that will be loaded without having
 to deal with the fact that the packages have not been loaded when this file
 is read."
  ;; Heh, franz discovered this problem too.
  (let ((package-var (gensym (symbol-name :PACKAGE-LOSSAGE-PACKAGE-)))
	(symbol-var  (gensym (symbol-name :PACKAGE-LOSSAGE-SYMBOL-)))
	(check-arg   (gensym (symbol-name :PACKAGE-LOSSAGE-CHECK-ARG-))))
    `(LET ((,package-var ,package)
	   (,symbol-var ,symbol))
       (FLET ((,check-arg (ARG-NAME ARG-VALUE)
		(COND ((KEYWORDP ARG-VALUE) t)
		      ((STRINGP ARG-VALUE)
		       (WARN " ~s argument to ~s, ~s, should now be a keyword, please fix your usage. ~
                              ~%          (It is probably in your .clinit.cl file)~
                              ~%          Seen in:~
                              ~%              ~s~&"
			     ARG-NAME
			     'package-lossage-funcall
			     ARG-VALUE
			     (QUOTE ,form))
		       nil)
		      (t (error " ~s argument to ~s, ~s, must be a keyword. ~
                              ~%        Seen in:~
                              ~%            ~s~&"
			     ARG-NAME
			     'package-lossage-funcall
			     ARG-VALUE
			     (QUOTE ,form))))))
	 (UNLESS (,check-arg 'PACKAGE ,package-var)
	   (SETQ ,package-var (INTERN ,package-var (FIND-PACKAGE :KEYWORD))))
	 (UNLESS (,check-arg 'SYMBOL ,symbol-var)
	   (SETQ ,symbol-var (INTERN ,symbol-var (FIND-PACKAGE :KEYWORD))))
	 (EXCL::FUNCALL-IN-PACKAGE ,symbol-var ,package-var ,@arguments)))))

#-allegro
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

(defvar *non-ts50-packages* nil
  "Capture a list of all packages that existed prior to loading TS50, so that
   we can determine which packages are newly defined.")

(defvar *ts50-packages* nil
  "The packages that are loaded by ts50.")

(defvar *preload-occurred?* nil
  "Set to T by the preload function to avoid redundant preloading.")

(defparameter *ts-preload-hooks*
    (if (boundp '*ts-preload-hooks*)
	*ts-preload-hooks*
      nil)
  "List of functions to call at the end of every call to preload.
Functions should have no required args, but can have keyword args.")

(defparameter *ts-load-finish-hooks*
    (if (boundp '*ts-load-finish-hooks*)
	*ts-load-finish-hooks*
      nil)
  "List of functions to call at the end of every call to TS-LOAD.
Functions should have no required args, but can have keyword args.")

(defun ensure-tail-recursion ()
  #+lispworks (setq compiler::*eliminate-tail-recursion-policy* (constantly t)
		    compiler::*debug-do-tail-merge-policy* (constantly t)))

(defun preload (&key verbose clean)
  "These load forms *must* come before all others.  They set up the package system and
   other really early stuff."
  (unless *preload-occurred?*
    (ensure-tail-recursion)
    (when (null *non-ts50-packages*)
      (setq *non-ts50-packages* (list-all-packages)))
    (ts-standard-path-translations)
    ;; Don't bother with the allegrostore patches if allegrostore
    ;; isn't loaded.
    (when (member :allegrostore *features*)
      (load-platform-patches :verbose verbose))

    (when clean
      ;; get rid of the fasl files clean-system will not
      ;; must be done before the rest of the preload
      (dolist (fn (list #p"TS50:DEFSYSTEM;defsystem.fasl"
			#p"TS50:ANSI-SERIES;s-code.fasl"
			#p"TS50:ANSI-SERIES;s-package.fasl"))
	(let ((pfile (probe-file fn)))
	  (when pfile
	    (when verbose
	      (format t ";;; Cleaning out ~a~%" pfile))
	    (delete-file fn)))))

    ;; Initialize the package system
    (load "TS50:BUILD;packages.lsp" :verbose verbose)
    (setup-ts50-package-system :verbose verbose)

    ;; Load the replacement macros
    (load "TS50:UTILITY;replacement-macros.lsp" :verbose verbose)

    ;; Stubs until it is really loaded.
    (set (intern "*DEFER-INTERRUPTS*" (find-package :utility)) nil)

    ;; Replace default defsystem with MKUTILS version.
    (load "TS50:BUILD;defsystem.lsp" :verbose verbose)
    (load-mk-defsystem :verbose verbose)

    ;; Now load the defsystem files
    (load "TS50:BUILD;system.lsp" :verbose verbose)

    ;; Make the series system.
    (handler-bind ((warning #'muffle-warning))
      (package-lossage-funcall :make :load-system :ansi-series
			       :bother-user-if-no-binary nil
			       :compile-during-load t
			       :verbose verbose))

    (mapc (lambda (package)
	      (package-lossage-funcall :series :install :pkg package))
	  (list* (find-package :user)
		 (find-package :java-tools)
		 *ts50-packages*))

    (when (not (member :allegrostore *features*))
      (load "TS50:BUILD;fake-allegrostore.lsp" :verbose verbose))

    (run-hooks *ts-preload-hooks*)
    (setq *preload-occurred?* t)))

(defparameter *ad-hoc-patch-files*
    '(
      )
  "A list of patches that should be loaded.  There are not released patches
   which would be picked up by the Lisp implementation's load-patches mechanism.
   These are ad-hoc patches for cases where a released patch is not yet available.")


;;; Defsystem used to go here, but is now in a separate file.

;;; Don't use logical pathnames here (because of name encoding
;;; problems).  Instead, put relative pathnames with respect to
;;; TS50:TS50;.  They will be merged later.

;;; Use UNIX syntax since ACL seems to be able to parse that on both
;;; platforms.

(defparameter *ts50-other-files*
    '(
      "*.bat"
      "*.cl"
      "*.dll"
      "*.el"
      "*.emacs"
      "*.lsp"
      "*.log"
      "*.txt"
      "build.out"
      "Building-HP-CONMAN.txt"
      "defsystem.lisp"
      "DIFF.EXE"
      "ObjectStore-installation-notes.txt"
      "sed.exe"
      "TAGS"
      "template.java"
      "bin"
      "bin/system-dlls/msvcrt.dll"
      "conman/*.bat"
      "conman/*.java"
      "conman/.csf-prefs"
      ;; "conman/CM.class" ;derived
      "conman/conman-server-config.lsp"
      "conman/demo-ts50-checkin.lsp"
      "conman/hierarchy.txt"
      "conman/hp-feedback-17-jan.txt"
      "conman/key-concepts.txt"
      "conman/master-change.txt"
      "conman/wishlist.txt"
      "conman/hp-questions.txt"
      "conman/to-do.txt"
      ;;"conman/ServerRequest.class"    ;derived
      "conman/test-output/*.mst"
      "conman/write-cm-shell-script.sh"
      "core/Uid.lsp"
      "core/*.txt"
      "core/test-input"
      "core/test-input/__an1.txt"
      "core/test-input/__an2.txt"
      "core/test-input/__ancestor.txt"
      "core/test-input/__rep.txt"
      "core/test-input/__rep1.txt"
      "core/test-input/__rep2.txt"
      "core/test-input/__rep2a.txt"
      "core/test-input/__repository.txt"
      "core/test-input/__work1.txt"
      "core/test-input/__work2.txt"
      "core/test-input/__workspace.txt"
      "core/test-input/abc.txt"
      "core/test-input/sol1.trn"
      "core/test-input/sol2.trn"
      "core/test-input/test-1.txt"
      "core/test-input/test-2.txt"
      "core/test-input/test-3.txt"
      "core/test-input/test-4.txt"
      "core/test-input/test-5.txt"
      "core/test-input/test-6.txt"
      "core/test-input/test-7.txt"
      "core/test-input/test-w-nl.txt"
      "core/test-input/test-wo-nl.txt"
      "core/test-input/test1-w-nl.txt"
      "core/test-input/test1-wo-nl.txt"
      "core/test-input/winkin1.trn"
      "core/test-input/winkin2.trn"
      "core/test-output/*.mst"
      "Corporate-Logo-Images/*.eps"
      "Corporate-Logo-Images/*.jpg"
      "Corporate-Logo-Images/*.pdf"
      "Corporate-Logo-Images/*.tif"
      "documents/*.doc"
      "frontpage/frontpage.lsp"
      "rfm/1.lsp"
      "rfm/2.lsp"
      "rfm/3.lsp"
      "rfm/to-do.txt"
      "rfm/merge-test-input"
      "rfm/merge-test-input/**/*.*"
      "rfm/test-output/*.mst"
      "rfm/test-output/save/*.mst"
      "rfm/__snapshot-test-input/**/*.*"
      "server/*.bat"
      "server/*.doc"
      "server/*.htm"
      "server/*.html"
      "server/*.txt"
      "server/MSServices.java"
      "server/CII-TestCert.cer"
      "server/CII-TestCert.pvk"
      "server/CII-TestCert.spc"
      "server/FSTest.java"
      "server/classes"
      "server/classes/netscape"
      ;;"server/classes/netscape/security/*.class" ;derived
      "server/__snapshot-test-input/**/*.*"
      "server/__test-input/**/*.*"
      "server/dirchooser/err"
      "server/dirchooser/*.bat"
      "server/dirchooser/*.html"
      "server/dirchooser/TestCert.pvk"
      "server/dirchooser/TestCert.spc"
      "server/dirchooser/netscape"
      ;;"server/dirchooser/netscape/**/*.class" ;derived
      "server/dirchooser/netscape/**/*.java"
      "server/frontpage.lsp"
      "server/nofrontpage.lsp"
      "server/netscape/readme.txt"
      "server/netscape/security/*.*"
      "server/signtool"
      "server/signtool/ns/*.*"
      "server/signtool/ns/clean/*.*"
      "server/test-output/*.mst"
      "utility/*.htm"
      "utility/*.txt"
      "utility/*.scm"
      "utility/Copy-obj.lsp"
      "utility/homedir-fix.lsp"
      "utility/pathname-fix.lsp"
      "web/*.html"
      "web/*.txt"
      "web/dave/*.lsp"
      "web/examples/*.*"
      "web/helptext/*.html"
      "web/simple/aa-readme.txt"
      "web/simple/images/*.gif"
      "web/simple/images/*.jpg"
      "web/simple/images/*.txt"
      "web/simple/helptext/*.info"
      "web/static-html/*.gif"
      "win32/*.lsp"
      "win32/*.html"
      )
  "Other files of interest, but not in the defsystem")

#+microsoft-32
(defvar *dlls-loaded?* nil "Flag to indicate if dll's have been loaded.")

#+microsoft-32
(defun load-dlls (&key verbose)
  (let ((system-root (sys:getenv "SystemRoot")))
    (labels ((system-path (rest)
	       (concatenate 'string system-root rest))
	     (loadit (path)
	       (load (system-path path) :verbose verbose)))
      (mapc #'loadit '(
		       ;; List DLL's here
		       ;; "\\System32\\webpost.dll"
		       ))))
  (setq *dlls-loaded?* t))

(defvar *ts-default-gc-parameters*
    (if (boundp '*ts-default-gc-parameters*)
	*ts-default-gc-parameters*
      :DAVE))

(defun ts-set-gc-parameters (&optional (how *ts-default-gc-parameters*) &key verbose)
  "Set the gc parameters.

   :DAVE       sets gc parameters to Dave's preferences
   :JRM        sets gc parameters to jrm's preferences
   :JRM-SMALL  sets gc parameters to jrm's preferences for small machines
  "
  #+allegro
  (ecase how
    (:DEFAULT nil)
    ;; Dave's setup limits lisp to about 32M of memory
    ;; and 27M of virtual memory, GC is about 4% total time
    #+allegro
    (:DAVE (setf (sys:gsgc-switch :gc-old-before-expand) t)
	   (setf (sys:gsgc-parameter :generation-spread) 16)
	   (setf (sys:gsgc-parameter :expansion-free-percent-old) 20)
	   (setf (sys:gsgc-parameter :expansion-free-percent-new) 35)
	   (setf (sys:gsgc-parameter :free-percent-new) 25)
	   )
    ;; Jrm's setup limits lisp to about 135M of memory
    ;; and 225M of virtual memory, GC is about 0.5% total time
    :+allegro
    (:JRM
     ;; Suck up a lot of RAM for the twospace collector.
     (sys:resize-areas :verbose t
		       :old (* 64 (expt 2 20.))
		       :new (* 48 (expt 2 20.)))
     ;; Don't call mark sweep until there is enough there to make it worthwhile.
     (setq excl:*tenured-bytes-limit* (* 48 (expt 2 20.)))
     (setf (sys:gsgc-switch :gc-old-before-expand) t)
     ;; Don't let things slosh around in the twospace areas for too long.
     (setf (sys:gsgc-parameter :generation-spread) 2)
     )
    #+lispworks
    (:JRM nil)
    #+allegro
    (:JRM-SMALL
     ;; Suck up a lot of RAM for the twospace collector on a machine with smaller
     ;; amounts of RAM.
     (sys:resize-areas :verbose t
		       :old (* 32 (expt 2 20.))
		       :new (* 16 (expt 2 20.)))
     ;; Don't call mark sweep until there is enough there to make it worthwhile.
     (setq excl:*tenured-bytes-limit* (* 32 (expt 2 20.)))
     (setf (sys:gsgc-switch :gc-old-before-expand) t)
     ;; Don't let things slosh around in the twospace areas for too long.
     (setf (sys:gsgc-parameter :generation-spread) 2)
     )
    )
  ;; Avoid using MMAP commands on HP.
  #+allegro
  (progn
    (setf (sys:gsgc-switch :use-remap) (not (member :hpux *features*)))
    (setf (sys:gsgc-switch :print) t)
    (setf (sys:gsgc-switch :verbose) t)
    (setf (sys:gsgc-switch :stats) t)
    (when verbose
      (sys:gsgc-parameters)))			; print out switch status
  )


(defun ts-load (&key (clean
		      ;; Do not default clean if we are running under a non-allegrostore lisp.
		      ;; If statement canonicalizes clean argument to T or NIL
		      (if (member :allegrostore *features*) t nil)
		      clean-sp)
		     (clean-lisp nil clean-lisp-sp)
		     (make-java nil make-java-sp)
		     source-only as-is minimal-load
		     deliver-conman suppress-regressions (clean-delivery t)
		     delivery-load  ;; true if loading into delivery image
		     (verbose nil verbose-supplied-p))
  "Load the TS system.  If CLEAN T is specified, delete fasl files first.
   If CLEAN is specified, lisp and java code will be cleaned (all recompiled)
   If CLEAN-LISP is specified, only lisp will be cleaned.
   If AS-IS is specified, load compiled files if present, otherwise load source.
   If MINIMAL-LOAD is specified, try to load only those files which have changed since the last load.

   MAKE-JAVA can be one of NIL (don't make Java components), T (make Java components)
   or :CLEAN (remake all Java components even if they appear to be up to date).

   If DELIVER-CONMAN is true, we will build the ConMan application for delivery.
   If DELIVER-CONMAN is :debug the compiler et.al. will not be excluded from the image.
   This option is mutually exclusive with options listed above.

   If CLEAN-DELIVERY is true, we recompile the sources used for application delivery.
   This value should be true for real production builds.  However if you're debugging system builds,
   you don't usually want to take the time to freshly recompile the system each time.

   VERBOSE, if true, enables the *load-verbose* switch.

   SUPPRESS-REGRESSIONS, if true, will supress invocation of the regression tests.
   At the time of this writing, they are only invoked if DELIVER-CONMAN is true, in order to
   'train' the CLOS caching mechanisms.  This is the desired behavior for real application
   delivery, but it takes far to long if you're just minimally changing and debugging the
   system build facility."

  ;; Override verbose switch if magic symbol is T.
  (when (and (not verbose-supplied-p)
	     (find-symbol (symbol-name :ts-load-always-verbose) (find-package :user))
	     (boundp (find-symbol (symbol-name :ts-load-always-verbose) (find-package :user))))
    (setq verbose
	  (symbol-value (find-symbol (symbol-name :ts-load-always-verbose) (find-package :user)))))

  ;; If user explicitly said :clean T or :clean NIL, validate other flags.
  (cond (clean-sp (when (and clean
			     (member :allegro *features*)
			     (not (member :allegrostore *features*)))
		    (error "Don't specify :CLEAN unless you have :ALLEGROSTORE. ~
                            Compiling without :ALLEGROSTORE would be very bad. ~
                            ~%Start lisp with -L SYS:code;allegrostore.fasl to load Allegrostore."))
		  (when (and clean as-is)
		    (error ":AS-IS T is pointless with :CLEAN T."))
		  (when (and clean minimal-load)
		    (error ":MINIMAL-LOAD T is pointless with :CLEAN T."))
		  (when clean-lisp-sp
		    (unless (eq clean clean-lisp)
		      (error ":CLEAN and :CLEAN-LISP values are inconsistent.")))
		  (when make-java-sp
		    (unless (or (and clean (eq make-java :clean))
				(null clean) (not (eq make-java :clean)))
		      (error "Don't specify both :CLEAN and :MAKE-JAVA.")))
		  (setq clean-lisp clean)
		  (setq make-java (if clean :clean nil)))
	;; Clean was NOT supplied.  Look for override symbol.
	((and (find-symbol (symbol-name :ts-load-default-clean) (find-package :user))
	      (boundp (find-symbol (symbol-name :ts-load-default-clean) (find-package :user))))
	 (setq clean (symbol-value (find-symbol (symbol-name :ts-load-default-clean) (find-package :user))))))

  ;; Don't clean if the user is asking for quick load.
  (when (or minimal-load as-is)
    (setq clean nil))

  (when (not clean-lisp-sp)
    (setq clean-lisp clean))
  (when (not make-java-sp)
    (setq make-java (if clean :clean make-java)))
  ;; *** ANY CODE BELOW HERE IS NOT ALLOWED TO LOOK AT THE VALUE OF
  ;; THE "CLEAN" ARGUMENT.  It should only look at the values of
  ;; CLEAN-LISP and MAKE-JAVE.

  ;; :DELIVERY-LOAD T is only specified in the slave lisp when
  ;; devilvering an image.  The compiler is not available in the slave
  ;; image so CLEAN-LISP must be suppressed.
  (when delivery-load
    (setq clean-lisp nil)
    (setq make-java nil))

  ;; Define CII conditional compilation symbols (if any)
  ;; Remember that if any of these have been turned on and then commented out, you
  ;; need to shut down and restart your lisp (*features* is not reset by commenting
  ;; these lines of code out)
  ;;(when (not (member :cvi-usage-stats *features*))
    ;;(push :cvi-usage-stats *features*)) ;; used in core/cvi.lsp, schema change if on

  ;; Avoid using MMAP commands on HP.  Catch this as early as possible.
  #+allegro
  (setf (sys:gsgc-switch :use-remap) (not (member :hpux *features*)))

  (let ((*ts50-load-verbose* verbose))
    ;; From time to time, there may be special, undistributed patches
    ;; from our Lisp vendors which need to be loaded.  We do that here.
    (mapc #'load *ad-hoc-patch-files*)

    ;;; Enable crossreferencing unless building a legal runtime
    #+allegro
    (let ((xref-enable (not delivery-load)))
      (when xref-enable
	(require :xref))
      (setq excl:*record-xref-info* xref-enable
	    excl:*load-xref-info*   xref-enable
	    excl:*record-source-file-info* xref-enable
	    excl:*load-source-file-info*   xref-enable))

    (when clean-lisp
      (setq *preload-occurred?* nil))

    (preload :verbose verbose :clean clean-lisp) ;; pre-load packages, etc.

    ;; MINIMAL-LOAD is not the same as ":FROM X" in terms of knowing if files in the load sequence
    ;; after X should  be loaded because they have dependencies on things which may have changed in X.
    ;; May want a :FROM keyword at some point.
    (when deliver-conman
      (unless (fboundp 'deliver-conman)
	(load #p"TS50:BUILD;delivery.lsp" :verbose verbose))
      ;; deliver-conman will reinvoke ts-load...  just an fyi so you're not surprised.
      (deliver-conman :runtime (unless (eq deliver-conman :debug) :standard)
		      :recompile-p clean-delivery
		      :suppress-regressions suppress-regressions
		      :verbose verbose)
      (return-from ts-load nil))

    (when clean-lisp
      ;; get rid of the rest of the fasl files
      (package-lossage-funcall :make :clean-system :ts :verbose t))

    (cond (source-only
	   (package-lossage-funcall :make :load-system :ts
				    :load-source-instead-of-binary t
				    :minimal-load minimal-load
				    :verbose verbose))
	  (as-is
	   (package-lossage-funcall :make :load-system :ts
			     :load-source-if-no-binary t
			     :compile-during-load nil
			     :force :new-source
			     :bother-user-if-no-binary nil
			     :minimal-load minimal-load
			     :verbose verbose))
	  (t (package-lossage-funcall :make :load-system :ts
				      :bother-user-if-no-binary nil :compile-during-load t
				      :minimal-load minimal-load
				      :verbose verbose)))

    #+microsoft-32 (unless *dlls-loaded?* (load-dlls :verbose verbose))

    ;; Build the Win32 package.
    #+(and :allegro-version>= (not (:version>= 5 2)) :microsoft-32)
    (package-lossage-funcall :win32 :patch-make-client-interface-instance)

    ;; Apparently causes the stack frame bug.
    #+allegro
    (unless delivery-load
      (mp:start-scheduler))

    ;; Build java components
    (load #p"TS50:BUILD;java-components.lsp" :verbose verbose)
    ;; The file java-components.lsp must be loaded unconditionally
    ;; since the initializations for certain test suites require that
    ;; certain Java components be defined.
    (when make-java
      (package-lossage-funcall :utility :make-java
			       :clean (eq make-java :clean)
			       :verbose verbose))

    ;; Add backtranslation root.
    (package-lossage-funcall :utility :add-backtranslation-root #p"TS50;**;*.*")

    ;; Package system loses, make major entry points visible to top level prompt.
    (when (find-package :core)
      (mapcar (lambda (sym) (import (intern (symbol-name sym) (find-package :core))))
	      '(:test-all
		:run-test)))

    (when (find-package :rfm)
      (mapcar (lambda (sym) (import (intern (symbol-name sym) (find-package :rfm))))
	      '(:rfm-http-server-start
		)))

    ;; (package-lossage-funcall "UTILITY" "VALIDATE-SETUP" :verbose? verbose)

    (when (package-lossage-funcall :utility :os-file-read-only? (translate-logical-pathname #p"TS50:BUILD;lisp-version.txt"))
      (package-lossage-funcall :utility :os-set-file-read-only  (translate-logical-pathname #p"TS50:BUILD;lisp-version.txt")
			       nil))

    ;; Write some lisp version information to 'lisp-version.txt' so we can know, by looking
    ;; at a directory, what version of lisp was used to compile files based on the last TS-LOAD
    ;; invocation.
    (with-simple-restart (skip-writing-lisp-version-file
			  "Skip writing lisp-version.txt")
      (with-open-file (stream (translate-logical-pathname #p"TS50:BUILD;lisp-version.txt")
		       :direction :output :if-exists :supersede)
	(declare (special *major-software-version*
			  *minor-software-version*
			  *major-schema-version*
			  *minor-schema-version*))
	(format stream "~%TS major software version:   ~s" *major-software-version*)
	(format stream "~%TS minor software version:   ~s" *minor-software-version*)
	(format stream "~%TS major schema version:     ~s" *major-schema-version*)
	(format stream "~%TS minor schema version:     ~s~%" *minor-schema-version*)
	(format stream "~%Lisp implementation type:    ~s" (lisp-implementation-type))
	(format stream "~%Lisp implementation version: ~s" (lisp-implementation-version))
	(format stream "~%Lisp machine type:           ~s" (machine-type))
	(format stream "~%Lisp machine version:        ~s" (machine-version))
	;; Causes merge conflicts
	;;(format stream "~%Lisp machine instance:       ~s" (machine-instance))
	(format stream "~%Lisp software type:          ~s" (software-type))
	(format stream "~%Lisp software version:       ~s~%" (software-version))
	;; Print out sorted and one per line to make diffing this useful.
	(format stream "~%Lisp features: ~{~%        ~s~}~%"
		(sort (copy-list *features*) ; bad to modify this.
		      #'string< :key #'symbol-name))))

    ;; Finalize the CLOS classes just before GC if we are doing a
    ;; full load.
    (unless (or as-is source-only minimal-load)
      (when (fboundp 'clos-finalize-ts50-class-inheritance)
	(clos-finalize-ts50-class-inheritance :verbose verbose)))

    #+allegro
    (unless (or as-is source-only minimal-load)
      (sys:resize-areas :verbose verbose
			:global-gc t
			:tenure t
			:sift-old-areas t
			:pack-heap t))	;tenure the loaded code if we're not playing around

    (ts-set-gc-parameters *ts-default-gc-parameters* :verbose verbose)

    ;; Do some initialization.  Pass some flags so init forms can be smarter.
    (run-hooks *ts-load-finish-hooks*
	       :clean clean-lisp
	       :delivery-load delivery-load
	       :verbose verbose)
    ;; Do not add code after running the *ts-load-finish-hooks*.
    ))



;;; Top-level :conman command for development.

#+allegro
(defun delivery-after-load-hook-function (&key delivery-load)
  (when delivery-load
    (tpl:remove-alias "conman")
    (setf (sys:gsgc-switch :print) nil)
    (setf (sys:gsgc-switch :verbose) nil)
    (setf (sys:gsgc-switch :stats) nil)))


#+allegro
(defun after-load-hook-hack-acl-cd-command (&key delivery-load)
  (unless delivery-load
    (let ((package (find-package :utility)))
      (when package
	(let ((symbol (find-symbol (symbol-name :cd) package)))
	  (when (and symbol (fboundp symbol))
	    (tpl:alias ("cd" 1 :string) (&rest string)
	      (with-simple-restart (abort "Return to top-level.")
		(when string
		  (funcall symbol (car string)))
		(format *trace-output* "~s" (current-directory))))))))))

#+allegro
(eval-when (:load-toplevel :execute)

  ;; After load setup for delivery build.
  (push 'delivery-after-load-hook-function
	*ts-load-finish-hooks*)

  ;; Change CD command to what the utilities package does.
  (push 'after-load-hook-hack-acl-cd-command
	*ts-load-finish-hooks*)

  ;; This `trampoline' calls
  ;; CONMAN:INVOKE-CONMAN-FROM-LISP with a string (the rest of the command line).
  ;; It is funny looking because we define it before the packages are defined.

  (tpl:alias ("conman" 5 :string) (&rest string)
    "Invoke a conman command from Lisp. Arguments are just like on the command line.
   Command will be called in the current directory."
    (with-simple-restart (abort
			  "Return to top-level.")
      (let ((package (find-package "CONMAN")))
	(unless package
	  (error "Cannot invoke conman because conman package doesn't exist."))
	(let ((symbol (find-symbol "INVOKE-CONMAN-FROM-LISP" package)))
	  (unless symbol
	    (error "Cannot call (~a::~a~{ ~s~}) because symbol ~s doesn't exist in ~s."
		   "CONMAN" "INVOKE-CONMAN-FROM-LISP" string "INVOKE-CONMAN-FROM-LISP" package))
	  (unless (fboundp symbol)
	    (error "Cannot call (~s ~{ ~s~}) because symbol ~s isn't fbound."
		   symbol string symbol))
	  (if (null string)
	      (funcall symbol "help")
	    (funcall symbol (car string)))))))
  )

#|

;;;
;;; The following three routines will count lines of code in the system.
;;; They can't be loaded by default, since they reference a package that isn't defined
;;; when this module is loaded.
;;;;

(defvar *ts-line-count* 0)

(defun ts-count-lines ()
  "Count lines of lisp code in the TS system.
   To use this, you must compile the definition of COUNT-DEFSYTEM-COMPONENT-LINES
   in the LOAD source module."
  (let ((*ts-line-count* 0)
        (lisp-count 0))
    (make::component-operation :line-count 'count-defsystem-component-lines)
    (operate-on-system :ts :line-count)
    ;; Don't forget load.lsp, this module
    (let* ((pathname (ts-source-directory-pathname "load.lsp"))
           (loc (utility:line-count pathname)))
      (format t "~%~40a: ~4d" (enough-namestring pathname) loc)
      (incf *ts-line-count* loc))
    (format t "~%----------------------------------------: ------")
    (format t "~%Lisp Subtotal~40t: ~6d~%" *ts-line-count*)
    (setq lisp-count *ts-line-count*)
    (setq *ts-line-count* 0)
    (loop for pathname in (append (directory (make-pathname
                                              :name :wild
                                              :type "java"
                                              :defaults (ts-source-directory-pathname "server\\")))
                                  (directory (make-pathname
                                              :name :wild
                                              :type "java"
                                              :defaults (ts-source-directory-pathname
                                                         "server\\dirchooser\\")))
                                  (directory (make-pathname
                                              :name :wild
                                              :type "java"
                                              :defaults (ts-source-directory-pathname "conman\\"))))
        as loc = (utility:line-count pathname)
        do (format t "~%~40a: ~4d" (enough-namestring pathname) loc)
           (incf *ts-line-count* loc))
    (format t "~%----------------------------------------: ------")
    (format t "~%Java Subtotal~40t: ~6d~%" *ts-line-count*)
    (format t "~%========================================: ======")
    (format t "~%Total~40t: ~6d" (+ *ts-line-count* lisp-count))))

(defun count-defsystem-component-lines (component force)
  (declare (ignore force))
  (let* ((pathname (make::component-full-pathname component :source))
         ;; Note: can't use this until you've arleady loaded the system... that's why this
         ;; method is normally commented out.
         (loc (utility:line-count pathname)))
    (format t "~%~40a: ~4d" (enough-namestring pathname) loc)
    (incf *ts-line-count* loc)
    nil))                               ;defsystem protocol, tell it nothing changed

;;;
;;; The following routines have the same package chicken-and-egg problem
;;; They count lisp code metrics with slightly more care.
;;;;

(defun ts-documentation-string-line-count (&optional (packages
                                                      (list (find-package :utility)
                                                            (find-package :core))))
  "Print out the lines of non-comment documentation assocated with symbols in the indicate packages
   and return the total line count.  In conjunction with the DOS FIND /C ';;' *.LSP command,
   you can determine the total amount of commentary in source modules."
  ;; TO-DO: automate commented line search, and account for blank lines in statistics and reporting
  ;; Obviously low priority, but if you decide you want this information, please upgrade the routine.
  ;; Also note: package list above is incomplete.
  (loop
      with total = 0
      for package in packages
      do
        (do-symbols (symbol package)
          (loop for type in '(function method-combination structure t type compiler-macro setf)
              as documentation = (documentation symbol type)
              when (and documentation
                        (find (symbol-package symbol) packages))
              do
                (let ((nlines (length (utility:split-string documentation '#.(list #\newline #\linefeed)))))
                  (format t "~%~55s[~15s]~5d" symbol type nlines)
                  (incf total nlines))))
      finally (return total)))

;; Calculate line width statistics   TO-DO: use defsystem like we do in ts-count-lines
;; You can also get this just by dividing WC char cound by line count,
;; though it includes newlines(?) and gives a slightly higher result.

(loop
    with total-lines = 0
    with line-width = 0
    with max-line-width = 0
    for path in (directory (make-pathname :name :wild :type "lsp"))
    do
      (with-open-file (stream path :direction :input)
        (loop as line = (read-line stream nil nil nil)
            while line
            do (incf total-lines)
               (let ((line-length (length line)))
                 (incf line-width line-length)
                 (when (> line-length max-line-width)
                   (setq max-line-width line-length)))
               ))

    finally (format t "~%Total lines: ~d, Average line width: ~d, Max line width: ~d"
                    total-lines (round line-width total-lines) max-line-width))
|#

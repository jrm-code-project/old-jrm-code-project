;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright (c) 1999 Content Integrity, Inc.
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
;;;; File Name:     makejava.lsp
;;;; Author:        Joe Marshall
;;;;
;;;; Module Description:  Master make file for e-zchange Java packages.
;;;;  This is a simple ad-hoc lisp program that compiles the Java elements
;;;;  of e-zchange.  We don't use the DEFSYSTEM code because it is really
;;;;  oriented toward the simple task of loading things into the lisp
;;;;  environment and we have several more complicated tasks to perform.
;;;;  We must create and sign a number of JAR and CAB files -- each of which
;;;;  uses a different signing process -- and we must compile a number of
;;;;  java sources with the appropriate compiler.  A number of the java
;;;;  source files will be common to multiple applets.
;;;;
;;;;  SPECIAL NOTE:  A stub version of the security interface in Netscape
;;;;  is assumed to be compiled and available in a subdirectory called
;;;;  `netscape/security' below the root path of the cab files.  It is
;;;;  automagically included in the CAB archive.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(in-package "CSF/UTILITY")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; modeling java products

(eval-when (:load-toplevel :execute)
  (export '()))

(defvar *java-products* nil
  "A list of all Java products to be built")

(defclass java-product ()
  ((name
    :initarg :product-name
    :initform (error "the JAVA-PRODUCT must be given a name")
    :reader java-product-name
    :documentation
    "A symbol naming the product, for identification purposes")
   (default-directory
       :initarg :default-directory
     :initform nil
     :reader java-product-default-directory
     :documentation
     "Pathname against which components are merged")
   (java-components
    :initarg :java-components
    :initform nil
    :reader java-product-java-components
    :documentation
    "Pathnames of java source files, to be merged against DEFAULT-DIRECTORY and typed .java")
   (classpath-additions
    :initarg :classpath-additions
    :initform nil
    :reader java-product-classpath-additions
    :documentation
    "List of pathnames to be appended to the standard classpath.  Used for netscape security, etc.")
   (require-ms-extensions
    :initarg :require-ms-extensions
    :initform nil
    :reader java-product-ms-extension-java-files
    :documentation
    "These java files must be compiled with microsoft extensions turned on.  These files are only required when building Microsoft cab files")
   (referenced-class-files
    :initarg :referenced-class-files
    :initform nil
    :reader java-product-referenced-class-files
    :documentation
    "Java class files which are not built by this java product but which are required to build it.")
   (other-components
    :initarg :other-components
    :initform nil
    :reader java-product-other-components
    :documentation
    "Other files required to build the product, e.g. icons")
   (jar-file
    :initarg :jar-file
    :initform nil
    :reader java-product-jar-file
    :documentation "A .jar file to be produced for this product")
   (cab-file
    :initarg :cab-file
    :initform nil
    :reader java-product-cab-file
    :documentation "a .cab file to be produced for this product")
   (ms-executable-file
    :initarg :ms-executable-file
    :initform nil
    :reader java-product-ms-executable-file
    :documentation "a .exe file to be produced by jexegen for this product (ms only)")
   (ini-file
    :initarg :ini-file
    :initform nil
    :reader java-product-ini-file
    :documentation
    "an .ini file containing the permissions needed for the .cab file")
   ))

(defmethod initialize-instance :after ((jp java-product) &key)
  (unless (loop for cons on *java-products*
	      for this-jp = (car cons)
	      do
		(when (eq (java-product-name this-jp)
			  (java-product-name jp))
		  (setf (car cons) jp)
		  (return t))
	      finally (return nil))
    (setq *java-products*
      (append *java-products* (list jp)))))

(defmethod print-object ((jp java-product) stream)
  (print-unreadable-object (jp stream :type t :identity t)
    (format stream "~a" (java-product-name jp))))

(defun map-over-java-products (function)
  (mapc function *java-products*))

(defun find-java-product (java-product-name)
  (block found
    (map-over-java-products
     (lambda (jp)
	 (when (eq java-product-name (java-product-name jp))
	   (return-from found jp))))
    nil))

(defmethod java-product-full-pathname ((jp java-product) pn)
  (let ((directory (java-product-default-directory jp)))
    (#+mswindows nt-preserve-pathname-case #-mswindows identity
		 (merge-pathnames pn (translate-logical-pathname directory)))))

(defun java-product-all-files (jp)
  (labels ((full-path (thing)
	     (java-product-full-pathname jp thing))
	   (do-single (thing)
	     (when thing
	       (list (full-path thing)))))
    (append (mapcar #'full-path (mapcar #'java-file (java-product-java-components jp)))
	    (mapcar #'full-path (mapcar #'class-file (java-product-java-components jp)))
	    (mapcar #'full-path (java-product-other-components jp))
	    (mapcar #'full-path (java-product-referenced-class-files jp))
	    (do-single (java-product-jar-file jp))
	    (do-single (java-product-cab-file jp))
	    (do-single (java-product-ini-file jp))
	    (do-single (java-product-ms-executable-file jp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; java compilation stuff

(eval-when (:load-toplevel :execute)
  (export '()))

(defun java-file (pathname)
  (pathname-new-type (pathname pathname) "java"))

(defun class-file (pathname)
  (pathname-new-type (pathname pathname) "class"))

(defparameter sun-compile-java-flags "-O")
(defparameter ms-compile-java-flags "/O")
(defparameter ms-compile-extended-java-flags "/O /x /nomessage")

(defun make-java-compiler (compiler flags &key classpath-additions)
  (lambda (files &key verbose)
    (csf/java-tools:java-compile (mapcar #'java-file files)
                                 :verbose verbose
                                 :use-compiler compiler
                                 :flags flags
                                 :classpath-additions classpath-additions)))

(defvar *java-compiler* nil "Compilation program.")

(defun call-with-ms-compiler (thunk &key classpath-additions)
  (let ((*java-compiler* (make-java-compiler :microsoft ms-compile-java-flags
					     :classpath-additions classpath-additions)))
    (funcall thunk)))

(defun call-with-extended-ms-compiler (thunk &key classpath-additions)
  (let ((*java-compiler* (make-java-compiler :microsoft ms-compile-extended-java-flags
					     :classpath-additions classpath-additions)))
    (funcall thunk)))

(defun call-with-sun-compiler (thunk &key classpath-additions)
  (let ((*java-compiler* (make-java-compiler :sun sun-compile-java-flags
					     :classpath-additions classpath-additions)))
    (funcall thunk)))

(defun select-default-compiler (&key sun-jdk ms-sdk verbose)
  (cond ((and (or (null sun-jdk) *prefer-microsoft-java*)
	      ms-sdk)
	 (when verbose (format t "~&Using Microsoft Java compiler~%"))
	 #'call-with-ms-compiler)
	((and (or (null ms-sdk) (not *prefer-microsoft-java*))
	      sun-jdk)
	 (when verbose (format t "~&Using Sun Java compiler~%"))
	 #'call-with-sun-compiler)
	(t (when verbose (format t "We don't have a Java compiler!~%"))
	   (constantly nil))))

(defun compile-java (file &key verbose)
  (funcall *java-compiler* file :verbose verbose))

(defun up-to-date? (source target)
  (when (null (probe-file source)) (error "Missing source file ~s" source))
  (and (probe-file target)
       (>= (file-write-date target)
	   (file-write-date source))))

(defun get-class-files (filenames)
  ;; Return a list of all class files that match the filenames.
  ;; This is needed because some java files compile into multiple class files.
  (remove-if (complement
	      (lambda (classfile)
                (find-if (lambda (filename)
                           (or (equal filename (pathname-name classfile))
                               (let ((pos (search (concatenate 'string filename "$")
                                                  (enough-namestring (pathname-name classfile)
                                                                     (namestring (os-get-current-directory))))))
                                 (and pos (= pos 0))))) 
                         filenames)))
             (directory (make-pathname :name :wild
                                       :type "class"
                                       :defaults (os-get-current-directory)))))

(defun update-class-file (file &key verbose)
  (let ((javafile (java-file file))
	(classfile (class-file file)))
    (if (and (up-to-date? javafile classfile)
	     verbose)
	(format t "~&Skipping ~s~%" javafile)
      (compile-java file :verbose verbose))))

(defun clean-class-file (classfile &key verbose)
  (declare (ignore verbose))
  (delete-file-force classfile))

(defun update-class-files (filespecs &key verbose)
  (let ((to-compile nil))
    (dolist (file filespecs)
      (let ((java-file (java-file file))
	    (class-file (class-file file)))
	(unless (and (probe-file class-file)
		     (up-to-date? java-file class-file))
	  (push file to-compile))))
    (when to-compile
      (compile-java to-compile :verbose verbose))))

(defun clean-class-files (filespecs &key verbose)
  (mapc (lambda (file) (clean-class-file file :verbose verbose))
	(get-class-files filespecs)))

(defun got-required-class-files-p (output-name class-files)
  (loop
      for class-file in class-files
      unless (probe-file class-file)
      collect class-file into missing-class-files
      finally
	(return
	  (if missing-class-files
	      (prog1 nil
		(format t "~&Can't build ~s:~&   Missing class files: ~s~%"
			output-name
			missing-class-files))
	    t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jar files

(defun sign-jar-file (jar-file contents additional-files &key verbose)
  (call-with-temporary-directory
   ;; We need to collect all of the files to one place first.
   (lambda (dir-to-sign)
     ;; Copy files to tempdir
     (mapc (lambda (file)
             (os-copy-file file (merge-pathnames dir-to-sign file) :verbose verbose))
           (append (mapcar (lambda (file)
                             (enough-namestring file (os-get-current-directory)))
                           (get-class-files contents))
                   additional-files))
     (let ((jar-file (merge-pathnames jar-file (os-get-current-directory))))
       (with-new-current-directory (merge-pathnames (make-pathname :directory '(:relative :up)
                                                                  :defaults "")
                                                   dir-to-sign)
         (if (or (member :win32 *features*)
                 (member :MSWindows *features*))
             (run-subprocess
              (format nil "~a -p implementer -d \"~a\" -k \"ContentIntegrityTest\" -Z ~a -c9 ~a"
                      (translate-logical-pathname "CSF:JAVA;auxiliary;signtool;ns;signtool")
                      (translate-logical-pathname "CSF:JAVA;auxiliary;signtool;ns")
                      jar-file (enough-namestring dir-to-sign (os-get-current-directory))) :silent (not verbose))
             (progn
               (warn "Can only run signtool under MSWindows.  Jar file will not be signed.")
               (run-subprocess
                (format nil "jar -c~:[~;v~]f ~a~{ ~a~}"
                        verbose
                        jar-file
                        (mapcar #'file-namestring 
                                (directory (make-pathname :name :wild
                                                          :type :wild
                                                          :defaults dir-to-sign))))))))))))

(defun verify-jar-file (jar-file &key verbose)
  (call-with-temporary-file "tmp"
    (lambda (file)
      (with-new-current-directory (make-pathname :name nil
                                                 :type nil
                                                 :defaults file)
	(if (or (member :MSWindows *features*)
                (member :win32 *features*))
	    (progn
	      (run-subprocess
		  (format nil "~a -d \"~a\" -v ~a --outfile ~a"
			  (translate-logical-pathname "CSF:JAVA;auxiliary;signtool;ns;signtool")
			  (translate-logical-pathname "CSF:JAVA;auxiliary;signtool;ns")
			  jar-file (enough-namestring file (os-get-current-directory))) :silent (not verbose))
	      (when verbose
		(file-copy-to-stream file *standard-output*)
		(force-output *standard-output*)))
	  (warn "Can only run signtool under MSWindows."))))))

(defmethod java-product-update-jar-file ((jp java-product) &key sun-jdk ms-sdk verbose)
  (let* ((jar-file (java-product-jar-file jp))
	 (files (java-product-java-components jp))
	 (additional-files (java-product-other-components jp))
	 (classfiles (mapcar #'class-file files)))
    (when jar-file
      (with-new-current-directory (translate-logical-pathname (java-product-default-directory jp))
	(funcall (select-default-compiler :sun-jdk sun-jdk
					  :ms-sdk ms-sdk
					  :verbose verbose)
	 (lambda () (update-class-files files :verbose verbose))
	 :classpath-additions (java-product-classpath-additions jp))
	;; This test looks confusing, but UP-T0-DATE? is being used to
	;; see if any class file is newer than its corresponding java
	;; file.
	(if (some (lambda (classfile)
		      (not (up-to-date? classfile jar-file)))
		  classfiles)
	    (when (got-required-class-files-p jar-file classfiles)
	      (sign-jar-file jar-file files additional-files :verbose verbose)
	      (verify-jar-file jar-file :verbose verbose)
	      (install jar-file :verbose verbose)))))))

(defmethod java-product-clean-jar ((jp java-product) &key verbose)
  (let ((jar-file (java-product-jar-file jp)))
    (when jar-file
      (with-new-current-directory (translate-logical-pathname (java-product-default-directory jp))
	(delete-file-force jar-file))
      (uninstall jar-file :verbose verbose))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cab files

(defun cabarc (cabfile classfiles &key verbose)
  (run-subprocess
   ;; Include netscape security classes by default.
   (format nil "cabarc -p -P ..\\ n ~a~{ ~a~} ..\\netscape\\security\\*.class" cabfile
	   (mapcar (lambda (classfile)
		       (enough-namestring classfile (os-get-current-directory)))
		   classfiles))
   :silent (not verbose)))

(defun signcode (cabfile permissions &key verbose)
  (run-subprocess
   (format nil "signcode -j javasign.dll -jp ~a -spc ~a -v ~a ~a"
	   permissions
	   (translate-logical-pathname "CSF:JAVA;auxiliary;microsoft;certificates;CII-TestCert.spc")
	   (translate-logical-pathname "CSF:JAVA;auxiliary;microsoft;certificates;CII-TestCert.pvk")
	   cabfile)
   :silent (not verbose)))

(defun chkjava (cabfile)
  (run-subprocess (format nil "chkjava ~a" cabfile) :silent nil))

(defun jexegen (exefile classfiles &key verbose)
  (call-with-temporary-directory
   (lambda (dir-to-jex)
       (with-new-current-directory dir-to-jex
	 ;; Copy files to tempdir
	 (mapc (lambda (file)
		   (os-copy-file (enough-namestring file dir-to-jex)
				 (merge-pathnames dir-to-jex file) :verbose verbose))
	       classfiles)
	 (if (or (member :MSWindows *features*)
                 (member :win32 *features*))

	     (run-subprocess (format nil "jexegen /main:CM /out:~a *.class" exefile)
			     :silent (not verbose))
	   (warn "Can only run jexegen under MSWindows."))
	 (break)))))

;;; This is rather ad-hoc.  For reasons I don't want to go into, the cab files
;;; must reside in the web directory.  So as a final step in creating the cab files,
;;; we copy them to the web directory.
(defun install (cabfile &key verbose)
  (declare (ignore verbose))
  (ensure-directories-exist (translate-logical-pathname #p"CSF:WEB-CONTENT;applets;"))
  (os-copy-file cabfile (merge-pathnames cabfile (translate-logical-pathname #p"CSF:WEB-CONTENT;applets;"))))

(defun uninstall (cabfile &key verbose)
  (declare (ignore verbose))
  (delete-file-force (merge-pathnames cabfile (translate-logical-pathname #p"CSF:WEB-CONTENT;applets;"))))

(defmethod java-product-update-cab-file ((jp java-product) &key verbose)
  "Compile, archive, sign, and install a CAB file for e-zchange."
  (let* ((package-path (java-product-default-directory jp))
	 (cabfile (java-product-cab-file jp))
	 (permissions (java-product-ini-file jp))
	 (files (java-product-java-components jp))
	 (special-files (java-product-ms-extension-java-files jp))
	 (additional-files (java-product-other-components jp))
	 (classfiles (mapcar #'class-file files))
	 (special-classfiles (mapcar #'class-file special-files)))
    (when cabfile
      (with-new-current-directory (translate-logical-pathname package-path)
	(call-with-ms-compiler
	  (lambda ()
	      ;; SET-DIFFERENCE in case any elements of the SPECIAL-FILES
	      ;; list are in the FILES list also.  Files in the
	      ;; SPECIAL-FILES list shouldn't be compiled in teh regular
	      ;; way because that would cause there to be an incorrectly
	      ;; built up-to-date .class file for that .java file.
	      (update-class-files (set-difference files
						  special-files
						  :test #'equal) :verbose verbose))
	  :classpath-additions (java-product-classpath-additions jp))
	(call-with-extended-ms-compiler
	 (lambda () (update-class-files special-files :verbose verbose))
	 :classpath-additions (java-product-classpath-additions jp))
	;; This test looks confusing, but UP-T0-DATE? is being used to
	;; see if any class file is newer than its corresponding java
	;; file.
	(if (or (some (lambda (classfile)
			  (not (up-to-date? classfile cabfile)))
		      classfiles)
		(some (lambda (classfile)
			  (not (up-to-date? classfile cabfile)))
		      special-classfiles))
	    (when (and (got-required-class-files-p cabfile classfiles)
		       (got-required-class-files-p cabfile special-classfiles))
	      (cabarc cabfile (append (get-class-files files)
				      (get-class-files special-files)
				      additional-files) :verbose verbose)
	      (if permissions
		  (progn
		    (signcode cabfile permissions :verbose verbose)
		    #-lispworks (chkjava cabfile)
                    )
		(when verbose
		  (format t "~%No permissions file found, not signing CAB file.")))
	      (install cabfile :verbose verbose)))))))

(defmethod java-product-clean-cab ((jp java-product) &key verbose)
  (with-new-current-directory (translate-logical-pathname (java-product-default-directory jp))
    (let ((cab-file (java-product-cab-file jp)))
      (when cab-file
	(delete-file-force cab-file)
	(uninstall cab-file :verbose verbose)))))

;;; Microsoft executables created with jexegen

(defmethod java-product-update-ms-executable-file ((jp java-product) &key verbose)
  "Compile, and generate an exe file."
  (let* ((package-path (java-product-default-directory jp))
	 (exefile (car (java-product-ms-executable-file jp)))
	 (files (java-product-java-components jp))
	 (special-files (java-product-ms-extension-java-files jp))
	 (classfiles (mapcar #'class-file files))
	 (special-classfiles (mapcar #'class-file special-files))
	 (extra-classfiles (cdr (java-product-ms-executable-file jp)))
	 (additional-files (java-product-other-components jp)))
    (when exefile
      (with-new-current-directory (translate-logical-pathname package-path)
	(call-with-ms-compiler
	  (lambda ()
	      ;; SET-DIFFERENCE in case any elements of the SPECIAL-FILES
	      ;; list are in the FILES list also.  Files in the
	      ;; SPECIAL-FILES list shouldn't be compiled in teh regular
	      ;; way because that would cause there to be an incorrectly
	      ;; built up-to-date .class file for that .java file.
	      (update-class-files (set-difference files
						  special-files
						  :test #'equal) :verbose verbose))
	  :classpath-additions (java-product-classpath-additions jp))
	(call-with-extended-ms-compiler
	 (lambda () (update-class-files special-files :verbose verbose))
	 :classpath-additions (java-product-classpath-additions jp))
	;; This test looks confusing, but UP-T0-DATE? is being used to
	;; see if any class file is newer than its corresponding java
	;; file.
	(if (or (some (lambda (classfile)
			  (not (up-to-date? classfile (car exefile))))
		      classfiles)
		(some (lambda (classfile)
			  (not (up-to-date? classfile (car exefile))))
		      special-classfiles))
	    (when (and (got-required-class-files-p exefile classfiles)
		       (got-required-class-files-p exefile special-classfiles))
	      (jexegen (merge-pathnames exefile)
		       (append (get-class-files files)
			       (get-class-files special-files)
			       extra-classfiles
			       additional-files) :verbose verbose)))))))

(defmethod java-product-clean-ms-executable ((jp java-product) &key verbose)
  (declare (ignore verbose))
  (with-new-current-directory (translate-logical-pathname (java-product-default-directory jp))
    (let ((exe-file (java-product-ms-executable-file jp)))
      (when exe-file
	(delete-file-force (car exe-file))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cleanup

(defmethod java-product-clean-class-files ((jp java-product) &key verbose)
  (with-new-current-directory (translate-logical-pathname (java-product-default-directory jp))
    (clean-class-files (java-product-java-components jp) :verbose verbose)
    (clean-class-files (java-product-ms-extension-java-files jp) :verbose verbose)))

(defmethod java-product-clean-all ((jp java-product) &key verbose)
  (java-product-clean-class-files jp :verbose verbose)
  (java-product-clean-jar jp :verbose verbose)
  (java-product-clean-cab jp :verbose verbose)
  (java-product-clean-ms-executable jp :verbose verbose))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; update

(defmethod java-product-update ((product-name string) &key (clean-p nil)
							   (verbose nil)
							   (sun-jdk (csf/java-tools:get-sun-java-compiler))
							   (ms-sdk  (csf/java-tools:get-microsoft-java-compiler)))
  (let ((found (find-symbol product-name :utility)))
    (unless found
      (error "There is no Java product named ~s" product-name))
    (java-product-update found
			 :clean-p clean-p
			 :verbose verbose
			 :sun-jdk sun-jdk
			 :ms-sdk  ms-sdk)))

(defmethod java-product-update ((product-name symbol) &key (clean-p nil)
							   (verbose nil)
							   (sun-jdk (csf/java-tools:get-sun-java-compiler))
							   (ms-sdk  (csf/java-tools:get-microsoft-java-compiler)))
  (let ((found (find-java-product product-name)))
    (unless found
      (error "There is no Java product named ~s" product-name))
    (java-product-update found
			 :clean-p clean-p
			 :verbose verbose
			 :sun-jdk sun-jdk
			 :ms-sdk  ms-sdk)))


(defmethod java-product-update ((jp java-product) &key (clean-p nil)
						       (verbose nil)
						       (sun-jdk (csf/java-tools:get-sun-java-compiler))
						       (ms-sdk  (csf/java-tools:get-microsoft-java-compiler)))
  (loop
    (with-simple-restart (retry-java-product-update
			  "Retry updating java product ~s" (java-product-name jp))
      (let ((jar-p (java-product-jar-file jp))
	    (cab-p (java-product-cab-file jp))
	    (ms-executable-p (java-product-ms-executable-file jp)))
	;; PROBLEM: unless we do a clean build we have no idea if the
	;; class files were build with the right compiler.
	(when (and jar-p sun-jdk)
	  (when clean-p
	    (java-product-clean-class-files jp :verbose verbose)
	    (java-product-clean-jar jp :verbose verbose))
	  (java-product-update-jar-file jp :sun-jdk sun-jdk :ms-sdk ms-sdk :verbose verbose))
	(when (and cab-p ms-sdk)
	  (when clean-p
	    (java-product-clean-class-files jp :verbose verbose)
	    (java-product-clean-cab jp :verbose verbose))
	  (java-product-update-cab-file jp :verbose verbose))
	(when (and ms-executable-p ms-sdk)
	  (when clean-p
	    (java-product-clean-class-files jp :verbose verbose)
	    (java-product-clean-ms-executable jp :verbose verbose))
	  (java-product-update-ms-executable-file jp :verbose verbose))
	(unless (or jar-p cab-p)
	  ;; If our output isn't a jar or cab file, just make the class
	  ;; files
	  (with-new-current-directory (translate-logical-pathname (java-product-default-directory jp))
	    (when (and (java-product-ms-extension-java-files jp)
		       ms-sdk)
	      (when verbose
		(format t "~&Using Microsoft Java compiler with extensions~%"))
	      (call-with-extended-ms-compiler
	       (lambda ()
		   (update-class-files (java-product-ms-extension-java-files jp)
				       :verbose verbose))
	       :classpath-additions (java-product-classpath-additions jp)))
	    (funcall (select-default-compiler :sun-jdk sun-jdk
					      :ms-sdk ms-sdk
					      :verbose verbose)
		     (lambda ()
			 (update-class-files (java-product-java-components jp)
					     :verbose verbose))
		     :classpath-additions (java-product-classpath-additions jp)))))
      (return))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; top level build routine

(defun make-java (&key clean verbose)
  "Make all the class, JAR and CAB files necessary for e-zchange"
  (let ((sun-jdk t)
	(ms-sdk t))
    ;; Make sure we have the development tools we need before we go
    ;; bashing output files.
    (when (null (csf/java-tools:get-sun-java-compiler))
      (setq sun-jdk nil)
      (warn "Cannot create JAR files:  No JDK from Sun. ~
               You will not be able to build e-zchange for Netscape, though you may run it ~
               if you already have a pre-built JAR file.")
      (when clean
	(warn "Not cleaning JAR files because you cannot reconstruct them without the JDK.")))
    (when (null (csf/java-tools:get-microsoft-java-compiler))
      (setq ms-sdk nil)
      (warn "Cannot create CAB files: No SDK from Microsoft. ~
               You will not be able to build e-zchange for MSIE, though you may run it ~
               if you already have a pre-built CAB file.")
      (when clean
	(warn "Not cleaning CAB files because you cannot reconstruct them without the SDK.")))
    (map-over-java-products
     (lambda (jp)
	 (java-product-update jp
			      :clean-p clean
			      :verbose verbose
			      :sun-jdk sun-jdk
			      :ms-sdk  ms-sdk)))))


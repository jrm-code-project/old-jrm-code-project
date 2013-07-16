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


(in-package "CSF/JAVA-TOOLS")

(proclaim (standard-optimizations))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(csf/config::*prefer-microsoft-java*
            )
          "CSF/CONFIG")
  (export '(
            call-with-java-agent
            java-run
            java-command-line
            java-compile
            get-java-compiler
            get-microsoft-java-compiler
            get-sun-java-compiler
            no-java-vm-condition
            )))

(defparameter *prefer-microsoft-java*
    (cond ((boundp '*prefer-microsoft-java*) *prefer-microsoft-java*)
          (t nil))
  "Whether the microsoft java is preferred over the sun java when both are present.")

(defparameter *java-compiler-list*
    (if (boundp '*java-compiler-list*)
        *java-compiler-list*
      nil)
  "A list of java compilers found on the machine, in order of preference.")

(defparameter *java-vm-list*
    (if (boundp '*java-vm-list*)
        *java-vm-list*
      nil)
  "A list of java runtimes found on the machine, in order of preference.")

(defun java-classpath-separator ()
  "The character that separates elements of the CLASSPATH"
  (platform/path-separator (server-platform)))

(defun get-java-vm (&key no-cache)
  "Returns the absolute pathname to a java virtual machine.
Search again if no-cache is true."
  (when (or no-cache
            (null *java-vm-list*))
    (setq *java-vm-list*
          (let ((sun-jre   (search-for-executable "jre"))
                (sun-jrew  (search-for-executable "jrew"))
                (sun-java  (search-for-executable "java"))
                (sun-javaw (search-for-executable "javaw"))
                (ms-jview  (search-for-executable "jview")))
            (or
             (if *prefer-microsoft-java*
                 (append ms-jview sun-jre sun-jrew sun-java sun-javaw)
               (append sun-jre sun-jrew sun-java sun-javaw ms-jview))
             :no-java-vm))))
  (if (eq *java-vm-list* :no-java-vm)
      nil
    (car *java-vm-list*)))

(defun get-java-compiler (&key no-cache)
  "Returns the absolute pathname to a java compiler.
Search again if no-cache is true."
  (when (or no-cache
            (null *java-compiler-list*))
    (setq *java-compiler-list*
          (let ((sun-javac (search-for-executable "javac"))
                (ms-jvc    (search-for-executable "jvc")))
            (or
             (if *prefer-microsoft-java*
                 (append ms-jvc sun-javac)
               (append sun-javac ms-jvc))
             :no-java-compiler))))
  (if (eq *java-compiler-list* :no-java-compiler)
      nil
    (car *java-compiler-list*)))

(defun get-microsoft-java-compiler ()
  "Returns the absolute pathname to a microsoft java compiler, if it exists."
  (get-java-compiler) ;; for effect
  (find-if (lambda (file)
               (funcall (platform-filename-equal (server-platform))
                        "jvc"
                        (pathname-name file))) *java-compiler-list*))

(defun get-sun-java-compiler ()
  "Returns the absolute pathname to a microsoft java compiler, if it exists."
  (get-java-compiler) ;; for effect
  (find-if (lambda (file)
               (funcall (platform-filename-equal (server-platform))
                        "javac"
                        (pathname-name file))) *java-compiler-list*))

(defun java-classpath-from-executable (executable-pathname)
  "Given the absolute path to the java executable, deduce the absolute path
to the class files."
  (merge-pathnames
   (make-pathname :directory '(:relative :back "lib")
                  :defaults "")
   (make-pathname :name "classes"
                  :type "zip"
                  :version nil
                  :defaults executable-pathname)))

(define-condition no-java-vm-condition (simple-error)
  ())

(defun java-canonicalize-directory-name (dirname)
  ;; strip off trailing slash or backslash
  (let ((last-char (char dirname (1- (length dirname)))))
    (if (or (equal last-char #\\)
            (equal last-char #\/))
        (subseq dirname 0 (1- (length dirname)))
      dirname)))

(defun java-command-line (class argument-string &key properties verbose classpath-additions)
  "Return a string that can be used to invoke the java runtime with the main file
in class.  Properties is a list, each element is itself a list of a property name (symbol)
 and a value (string)."
  (flet ((file-is? (file name)
           (funcall (platform-filename-equal (server-platform))
                    (pathname-name file)
                    name)))
    (let* ((dirstring (namestring (make-pathname :name nil
                                                 :type nil
                                                 :version nil
                                                 :defaults class)))
           (directory (java-canonicalize-directory-name dirstring))
           (file (pathname-name class))
           (vm (get-java-vm)))
      (when (null vm)
        (error (make-condition 'no-java-vm-condition
                 :format-control "Could not find a java virtual machine."
                 :format-arguments nil))
                                        ;(error "JAVA-COMMAND-LINE threw an ERROR, but it was not handled.")
        )
      ;; Amusing BUG in jview.  It doesn't grok home directories correctly.
      ;; Here is the workaround.
      (when (file-is? vm "jview")
        (push `("user.home" ,(pathname->platform-namestring
                              (user-homedir-pathname) (server-platform))) properties))
      ;; Use double quotes to handle spaces in file names.
      (format nil "\"~a\"~@[ ~a~]~{ ~a \"~@{~a~}\"~}~@[~{~:@{ ~a~a=~a~}~}~] ~a ~a"
              vm
              (when (and verbose
                         (or (file-is? vm "jre")
                             (file-is? vm "java")))
                "-verbose")
              (cond ((file-is? vm "jre") `("-cp" ,directory
                                                 ,@(mapcan (lambda (addition)
                                                               (list (java-classpath-separator)
                                                                     (java-canonicalize-directory-name
                                                                      (namestring addition))))
                                                           classpath-additions)))
                    ((file-is? vm "java") `("-classpath" ,directory
                                                         ,@(mapcan (lambda (addition)
                                                               (list (java-classpath-separator)
                                                                     (java-canonicalize-directory-name
                                                                      (namestring addition))))
                                                           classpath-additions)
                                                         ,(java-classpath-separator)
                                                         ,(java-canonicalize-directory-name
                                                           (namestring
                                                            (java-classpath-from-executable vm)))))
                    ((file-is? vm "jview") `("/cp:p" ,directory
                                                     ,@(mapcan (lambda (addition)
                                                               (list (java-classpath-separator)
                                                                     (java-canonicalize-directory-name
                                                                      (namestring addition))))
                                                           classpath-additions)))
                    (t (error "Unrecognized virtual machine ~s" vm)))
              (when properties
                (cond ((file-is? vm "jre") (mapcar (lambda (prop)
                                                       (cons "-D" prop)) properties))
                      ((file-is? vm "java") (mapcar (lambda (prop)
                                                       (cons "-D" prop)) properties))
                      ((file-is? vm "jview") (mapcar (lambda (prop)
                                                         (cons "/d:" prop)) properties))
                      (t (error "Unrecognized virtual machine ~s" vm))))
              file
              argument-string))))

(defun compilation-necessary? (source-file)
  "Return T if the class file either does not exist, or if it is older than
   the source file."
  (let* ((source-path (parse-namestring source-file))
	 (target-path (make-pathname :type "class" :defaults source-file)))
    (or (null (probe-file target-path))
        (>= (file-write-date source-path)
            (file-write-date target-path)))))

(defun java-compile (path-designators &key (debug t) clean verbose use-compiler flags classpath-additions)
  "Compile all (where necessary) java programs indicated by PATH-DESIGNATORS.

   PATH-DESIGNATORS may be a list of path designators (string filenames or pathnames),
   or it may be a single path designator.

   If PATH-DESIGNATORS contains a directory specification, all java programs in that
   directory will be processed.  Directories are not recursively searched (yet).
   Note that (java-compile \".\") is very convenient.

   It is assumed that appropriate PATH variables already exist in the
   environment such that a 'javac' program will be found.

   The DEBUG argument, if true, causes '-g' to be passed to javac.

   The CLEAN argument, if true, causes all files to be compiled regardless of timestamps.
   If NIL, files are compiled only if the source (.java) file is newer than the target (.class)
   file.

   VERBOSE, when true, causes compilation logic messages to be printed to *standard-output*.

   USE-COMPILER, when nil, uses the default java compiler.
   USE-COMPILER, when :MICROSOFT, uses the microsoft compiler.
   USE-COMPILER, when :SUN, uses the sun compiler.

   FLAGS, when present is additional command line arguments to the compiler.

   CLASSPATH-ADDITIONS, when not nil, is a set of pathnames to extra classfiles.

   Returns the number of times javac reported a non-zero status code."

  (unless (listp path-designators)
    (setq path-designators (list path-designators)))

  (flet ((no-compiler ()
           (warn "Could not find a~@[ ~a~] java compiler, not compiling ~s." use-compiler path-designators)
           (return-from java-compile nil))
         (file-is? (file name)
           (funcall (platform-filename-equal (server-platform))
                    (pathname-name file)
                    name)))
    (let ((compiler
           (if (null use-compiler)
               (or (get-java-compiler)(no-compiler))
             (ecase use-compiler
               (:microsoft
                (or (get-microsoft-java-compiler) (no-compiler)))
               (:sun
                (or (get-sun-java-compiler) (no-compiler))))))
          (files-to-process nil)
          (number-of-bad-status-codes 0)
          (log-path (get-temporary-pathname :file-type "log")))
      (loop while path-designators      ;don't use FOR IN, we're destructively modifying the list
            as path = (car path-designators)
            ;; turn "." into current-directory.
            as pathname = (cond ((pathname path)
                                 (if (string-equal (namestring path) ".")
                                     (os-get-current-directory)
                                   path))
                                ((and (stringp path)
                                      (string-equal path ".")  (os-get-current-directory)))
                                (t (parse-namestring path)))
            do
            (setq path-designators (cdr path-designators))
            (cond ((probe-directory pathname)
                   (setq path-designators
                         (append (directory (make-pathname :name :wild :type "java"
                                                           :defaults pathname))
                                 path-designators)))
                  ((not (string-equal (pathname-type pathname) "java"))
                   (warn "Not a java file, skipped: ~s" pathname))
                  (t (pushnew pathname files-to-process))))
      (dolist (file files-to-process)
        (if (or clean (compilation-necessary? file))
            (progn
              (with-open-file (stream log-path :direction :output :if-exists :supersede)
                (let* ((compile-command (format nil "\"~a\"~@[ ~a~]~@[ ~a~] ~@[~a ~]\"~a\""
                                                compiler
                                                flags
                                                (when debug (if (file-is? compiler "javac") "-g" "/g"))
                                                (when classpath-additions
                                                  (cond ((file-is? compiler "javac")
                                                         (format nil "-classpath \".~{~c~a~}~c~a\""
                                                                 (mapcan (lambda (dir)
                                                                             (list (java-classpath-separator)
                                                                                   (java-canonicalize-directory-name
                                                                                    (namestring dir))))
                                                                         classpath-additions)
                                                                 (java-classpath-separator)
                                                                 (java-canonicalize-directory-name
                                                                  (namestring
                                                                   (java-classpath-from-executable compiler)))))
                                                        ((file-is? compiler "jvc")
                                                         (format nil "/cp:p \"~a~{~c~a~}\""
                                                                 (java-canonicalize-directory-name
                                                                  (namestring
                                                                   (first classpath-additions)))
                                                                 (mapcan (lambda (dir)
                                                                             (list (java-classpath-separator)
                                                                                   (java-canonicalize-directory-name
                                                                                    (namestring dir))))
                                                                         classpath-additions)))
                                                        (t (error "Unknown compiler ~s" compiler))))
                                                file))
                       (result
                        (progn
                          (when verbose
                            (format t "~%~a" compile-command))
                          (multiple-value-bind (result exit-code)
                              (csf/utility::run-subprocess compile-command :silent nil)
                            (declare (ignore result exit-code))
                            (values 0 0)))))
                  (cond ((= result 1)
                         (incf number-of-bad-status-codes)
                         (warn "Compilation warnings?  Java compile produced non-zero result."))
                        ((= result 0))
                        (t
                         (incf number-of-bad-status-codes)
                         (warn "Unexpected compilation result code: ~s" result)))))
              (when verbose
                ;; Print the compilation ouput to the lisp terminal...
                (file-copy-to-stream log-path *standard-output*))
              (delete-file log-path))
          ;; File doesn't need compilation
          (when verbose
            (format t "~%~a is up to date" file))))
      number-of-bad-status-codes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CALL-WITH-JAVA-AGENT

(defun call-with-java-agent (&key
			     java-class
			     java-argument-string
			     java-properties
			     java-verbose
			     java-classpath-additions
			     java-output-pathname
			     java-error-pathname
			     (echo-java-output t)
			     (java-current-directory (os-get-current-directory))
			     (which-return-values :parent)
			     grab-conman-error-code ;; yuck
			     receiver)
  #+allegro (declare (:fbound conman::extract-error-number)
	   (special web::*http-server-semaphore*))
  ;; Check this obvious error so we can avoid
  ;; debugging in the other process.
  (guarantee-directory-pathname java-current-directory)
  (let ((parent-return-values nil)
	(child-return-values  nil)
	(conman-error-code    0)
	(java-exit-semaphore (make-semaphore "Java process exit.")))

    (cl:unwind-protect
	(with-open-file (java-output-stream java-output-pathname
			 :direction :output :if-exists :supersede)
	  (with-open-file (java-error-stream java-error-pathname
			   :direction :output :if-exists :supersede)
	    (multiple-value-setq (parent-return-values child-return-values)
	      (call-with-child-process
	       "Java Agent"

	       (lambda (parent-process parent-exit-semaphore)
		 (declare (ignore parent-process parent-exit-semaphore))
		 (unwind-protect
		     (progn
                       (debug-message 3 "java-current-directory ~s" java-current-directory)
		       (with-new-current-directory java-current-directory
                         (sys:call-system-showing-output (csf/java-tools:java-command-line
                                                          java-class
                                                          java-argument-string
                                                          :properties java-properties
                                                          :verbose    java-verbose
                                                          :classpath-additions java-classpath-additions
                                                          )
                                                         :current-directory java-current-directory
                                                         :output-stream java-output-stream
                                                         :wait t)

;			 (call-synchronous-subprocess (csf/java-tools:java-command-line
;						       java-class
;						       java-argument-string
;						       :properties java-properties
;						       :verbose    java-verbose
;						       :classpath-additions java-classpath-additions
;						       )
;			   :output java-output-stream
;			   :error-output java-error-stream
;			   :show-window :hide)
                         ))
                   "Signal java-exit-semaphore"
		   (signal-semaphore java-exit-semaphore)))

	       (lambda (child-process start-child-semaphore)
		 (declare (ignore child-process))

		 (multiple-value-prog1 (funcall receiver start-child-semaphore)
		   (wait-for java-exit-semaphore)))
	       :which-return-values :both
	       ))))

      (when (plusp (file-bytes java-output-pathname))
	(when echo-java-output
	  (format t "~&~%Java standard output:~%")
	  (file-copy-to-stream java-output-pathname *standard-output*))
	(when (and grab-conman-error-code
		   (eql (second child-return-values) 1))
	  (setq conman-error-code (conman::extract-error-number java-output-pathname))))
      (when (plusp (file-bytes java-error-pathname))
	(when echo-java-output
	  (format t "~&~%Java error output:~%")
	  (file-copy-to-stream java-error-pathname *standard-output*)))
      (delete-file java-output-pathname)
      (delete-file java-error-pathname))

    (if grab-conman-error-code
	conman-error-code
      (ecase which-return-values
	(:parent (apply #'values parent-return-values))
	(:child  (apply #'values child-return-values))
	(:both   (values parent-return-values child-return-values))))))

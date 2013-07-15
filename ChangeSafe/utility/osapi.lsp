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

;;; API to the Operating system

(in-package "CSF/UTILITY")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(csf/config::*preserve-temporary-files*
            csf/config::*preserve-temporary-directories*)
          "CSF/CONFIG")
  (export '(
            call-with-temporary-file
            call-with-temporary-directory
            delete-directory
            delete-file-force
            file-bytes
            get-temporary-pathname
            get-temporary-directory
            os-create-directory
            os-change-current-directory
            os-copy-file
            os-file-read-only?
            os-get-current-directory
            os-get-environment-variable
            os-get-timestamp
            os-get-user-name
            os-read-write-command
            os-read-only-command
            os-server-locale
            os-set-timestamp
            os-set-file-last-modified-command
            os-set-file-read-only
            os-set-file-executable
            os-touch-file
            probe-directory
            probe-file
            scan-directory
            search-for-executable
            with-new-current-directory
            with-preserved-directory
            )))

(proclaim (standard-optimizations))

(defun probe-file (pathspec)
  #+unix (cl:probe-file pathspec)
  #+(or :microsoft-32 :win32)
  (multiple-value-bind (drive directory name type)
      (win32-probe-file (namestring
                         (merge-pathnames (if (logical-pathname-p pathspec)
                                              (translate-logical-pathname pathspec)
                                              pathspec)
                                          *default-pathname-defaults*)))
    (when (or directory name type)
      ;; return :unspecific for fields that are unfilled.  
      (make-pathname
       :host (when drive (string drive))
       :device :unspecific
       :directory directory
       :name (or name :unspecific)
       :type (or type :unspecific)
       :version :unspecific
       :case :local
       :defaults ""))))

(defun os-create-directory (pathspec)
  (ensure-directories-exist
   (merge-pathnames (if (logical-pathname-p pathspec)
                        (translate-logical-pathname pathspec)
                        pathspec)
                    *default-pathname-defaults*)))

;;; This shadows the `regular' version of directory.
;;; It requires an absolute `wild' pathname and returns pathnames.
(defun directory (pathspec)
  #+(or :microsoft-32 :win32)
  (map 'list (lambda (namestring)
               (multiple-value-bind (success drive directory name type)
                   (parse-dos-namestring namestring)
                 (when success
                   (make-pathname
                    :host (when drive (string drive))
                    :device :unspecific
                    :directory directory
                    :name (or name :unspecific)
                    :type (or type :unspecific)
                    :version :unspecific
                    :case :local
                    :defaults ""))))
       (win32-directory (if (pathnamep pathspec)
                            (namestring pathspec)
                            pathspec))))

(defun scan-directory (pathspec)
  ;; can't use native scanner.
  (declare (optimizable-series-function))
  (map-fn 'pathname
          (lambda (namestring)
            (platform/parse-namestring (server-platform) namestring))
          (scan 'list
                (win32-directory (if (pathnamep pathspec)
                                     (namestring pathspec)
                                     pathspec)))))

(defun os-get-timestamp (pathname)
  "Get the `last-modified' time of a file.  OS dependent."
  (check-type pathname pathname)
  (unless (root-pathname? pathname)
    #+:unix (unix-get-timestamp pathname)
    #+:win32 (win32-get-timestamp (pathname->windows-namestring pathname))))

(defun os-set-timestamp (pathname timestamp)
  "Set the `last-modified' time of a file.  OS dependent."
  (check-type pathname pathname)
  (check-type timestamp non-negative-integer)
  (unless (root-pathname? pathname)
    #+unix  (unix-set-timestamp pathname timestamp)
    #+win32 (win32-set-timestamp (pathname->windows-namestring pathname) timestamp)))

(defun os-set-file-read-only (pathspec read-only)
  #+(or :microsoft-32 :win32)
  (win32-set-file-read-only
   (pathname->windows-namestring pathspec)
   read-only))

(defun os-set-file-executable (pathspec executable)
  #+(or :microsoft-32 :win32) (declare (ignore pathspec executable))
  #+(or :microsoft-32 :win32) nil)

(defun os-get-current-directory ()
  (canonicalize-pathname #+allegro (excl:current-directory)
                         #+lispworks (hcl:get-working-directory)))

(defun os-change-current-directory (&optional new-directory)
  (when new-directory
    (setq new-directory
      ;; Merge the directory so that we explicitly have the
      ;; drive letter.
          (probe-directory
           (merge-pathnames (canonicalize-pathname new-directory)
                            *default-pathname-defaults*
                            nil)))
    (guarantee-directory-pathname new-directory)
    ;; Why not just always use SET-CURRENT-DIRECTORY or EXCL:CHDIR?
    #+aclpc(set-current-directory new-directory)
    #+excl(excl:chdir #| excl::set-current-working-directory |# new-directory)
    #+lispworks (hcl:change-directory new-directory)
    ;; Make sure that *default-pathname-defaults* is the
    ;; same as the current working directory.
    (setq *default-pathname-defaults* (os-get-current-directory))
    )

  (values (os-get-current-directory) new-directory))

(defmacro with-preserved-directory (&body body)
  "Execute BODY, preserving the current directory location in the event that BODY
  should change it.  This became necessary because many implementations of ACL
  file primitives (such as the CL DIRECTORY function) change the current directory"
  (with-unique-names (saved-directory saved-pathname-defaults)
    `(LET ((,saved-directory (OS-GET-CURRENT-DIRECTORY))
           (,saved-pathname-defaults *DEFAULT-PATHNAME-DEFAULTS*))
       (CHECK-TYPE ,saved-directory DIRECTORY-PATHNAME)
       (UNWIND-PROTECT
           (LOCALLY ,@body)
         "restoring current directory"
         (SETQ *DEFAULT-PATHNAME-DEFAULTS* ,saved-pathname-defaults)
         (OS-CHANGE-CURRENT-DIRECTORY ,saved-directory)))))

(defmacro with-new-current-directory (new-dir &body body)
  "Execute BODY with the current directory set to NEW-DIR.  Current-directory is
restored upon exiting."
  `(WITH-PRESERVED-DIRECTORY
     (OS-CHANGE-CURRENT-DIRECTORY ,new-dir)
     (LOCALLY ,@body)))

;;; Environment variables
(defun os-get-environment-variable (name)
  #+:allegro (sys:getenv name)
  #+:lispworks (lw:environment-variable name)
  #-(or :allegro :lispworks) (error "Can't get environment variable ~s." name))

(defun probe-directory (pathname)
  #+:lispworks (when (lw:file-directory-p pathname)
                  (probe-file pathname))
  #-:lispworks (let ((probe (probe-file pathname)))
                 (and probe
                      (directory-pathname? probe)))
  )

(defun os-get-temporary-directory ()
  (let* ((temp (probe-file (canonicalize-pathname
                            (or #+allegro(system:temporary-directory)
                                (os-get-environment-variable "TMP")
                                (os-get-environment-variable "TEMP")
                                (error "OS-GET-TEMPORARY-PATHNAME can't find an environment variable for TMP or TEMP")))))
         (lisp-temp (merge-pathnames (make-pathname :directory `(:relative ,(os-get-user-name) "Lisp-temp")
                                                    :defaults "")
                                     temp)))
    (ensure-directories-exist lisp-temp)
    lisp-temp))

(defun os-get-user-name ()
  #+:allegro (sys:user-name)
  #+:lispworks (sys:get-user-name))

(defun get-unique-pathname-in-directory (directory-pathname &key (file-type "tmp"))
  "Generate a path NAME component for a file inside DIRECTORY-PATHNAME which is unique, intended
  for subsequent creation.  If FILE-TYPE is specified, that is used for the type.
  The file NAME component is what is generated to ensure uniqueness."
  (let* ((temp-path (make-pathname :type file-type
                                   :defaults (probe-directory directory-pathname)))
         (result-path))
    ;; By using a counter that updates quickly, appending a random number,
    ;; and using a large radix (which gets in more bits), we make the namespace
    ;; quite sparse, thus reducing the probability of the race condition
    ;; mentioned below. (how much?  well, empirically, we can call this function
    ;; about 4 times before the internal real time changes, so if random is
    ;; truly random, the odds are about 1.8e-6 that we will generate the same
    ;; name twice.)
    (loop with time-value = (get-internal-real-time) ;(get-universal-time)  may be a worse choice
                            ;; for current-add from 0
        as time-string = (format nil "~36R~36,4,'0R" time-value (random (expt 36. 4.)))
        as length = (string-length time-string)
        as file-name = (if (> length 8)
                           (subseq time-string (- length 8))
                         time-string)
        do (setq result-path (make-pathname :name file-name :defaults temp-path))
        unless (probe-file result-path)
        do (return-from get-unique-pathname-in-directory result-path))))

(defun get-temporary-pathname (&key (file-type "tmp") (where (os-get-temporary-directory)))
  "Return a unique file pathname for temporary (scratch) use.  Note that this
  method doesn't actually create the file. If you call it twice in succession
  WITHOUT creating the file returned the first time immediately, it may return
  the same pathname twice!  We could fix this by adding a CREATE-IT-NOW keyword
  to stub the file.  Returns a DOS compatible 8.3 name.

  FILE-TYPE, if overridden, is the filetype of the generated file"

  (get-unique-pathname-in-directory where :file-type file-type))

(defun get-temporary-directory ()
  "Return a unique file directory for temporary (scratch) use.  Note that this
  method doesn't actually create the directory. If you call it twice in succession
  WITHOUT creating the directory returned the first time immediately, it may return
  the same directory twice!  We could fix this by adding a CREATE-IT-NOW keyword
  to stub the directory.  Returns a DOS compatible 8.3 name."

  (ensure-directory-pathname-designator
   (get-unique-pathname-in-directory (os-get-temporary-directory)
                                     :file-type nil)
   (default-directory-separators)))

(defun delete-file-force (pathspec)
  "DELETE-FILE, even if it is set read-only."
  (handler-bind (#+allegro
                 (file-error
                  (lambda (condition)
                    (when (excl::file-error-errno condition)
                      (os-set-file-read-only pathspec nil)
                      (return-from delete-file-force
                        (delete-file filespec)))

                    ;; Allegro does this when file doesn't exist.
                    (return-from delete-file-force nil)))
                 #+lispworks
                 (conditions:file-operation-error
                  (lambda (condition)
                    (when (and (eq (conditions:file-operation-error-operation condition) 'delete)
                               (= (conditions:file-operation-error-errno condition) 5))
                      (os-set-file-read-only pathspec nil)
                      (return-from delete-file-force
                        (delete-file pathspec))))))
    (delete-file pathspec)))

(defun delete-directory (pathspec &key force)
  (check-type pathspec directory-pathname)
  #+:lispworks
  (if force
      (let ((probe (probe-directory pathspec)))
        (when probe
          (dolist (file (win32-directory
                         (pathname->windows-namestring
                          (make-pathname :name :wild :type :wild :defaults probe))))
            (if (lw:file-directory-p file)
                (delete-directory (pathname file) :force t)
                (delete-file-force file)))
          (lw:delete-directory probe)))
      (lw:delete-directory pathspec)))

(defparameter *preserve-temporary-files* nil "Set this to T for debugging.")

(defun call-with-temporary-file (type receiver &key (where nil where-p))
  "Invoke receiver on the pathname of a temporary file.  File is deleted upon
   return from receiver"
  ;; The WHERE argument is provided to provide some control over where
  ;; the temporary file is created.  This is useful in environments
  ;; which do not support cross-device file renaming, for example.
  (let ((tempfile (apply #'get-temporary-pathname
                         :file-type type
                         (when where-p
                           (list :where where)))))
    (unwind-protect
        (funcall receiver tempfile)
      "deleting temporary file"
      (unless *preserve-temporary-files*
        (delete-file-force tempfile)))))

(define-compiler-macro call-with-temporary-file (&whole form type receiver &key (where nil where-p))
  ;; Beta reduce call to call-with-temporary-file.
  (destructure-function-lambda 1 receiver
    (lambda (arglist docstring declarations body)
        (if (and (consp arglist)
                 (symbolp (car arglist))
                 (null (cdr arglist)))
            `(LET ((,(car arglist) (GET-TEMPORARY-PATHNAME :FILE-TYPE ,type
                                                           ,@(when where-p
                                                               `(:WHERE ,where)))))
               ,@(when docstring (list docstring))
               (DECLARE (TYPE PATHNAME ,(car arglist)))
               ,@declarations
               (UNWIND-PROTECT
                   (PROGN ,@body)
                 "deleting temporary file"
                 (LOCALLY (DECLARE ,(standard-optimizations))
                   (UNLESS *PRESERVE-TEMPORARY-FILES*
                     (DELETE-FILE-FORCE ,(car arglist))))))
          form))
    (lambda () form)))

(defparameter *preserve-temporary-directories* nil "Set this to T for debugging.")

(defun call-with-temporary-directory (receiver)
  "Invoke receiver on the pathname of a temporary directory.  Directory is deleted upon
   return from receiver"
  (let ((tempdir (get-temporary-directory)))
    (unwind-protect
        (progn
          (ensure-directories-exist tempdir)
          (funcall receiver tempdir))
      "deleting temporary directory"
      (unless *preserve-temporary-directories*
        (delete-directory tempdir :force t)))))

(define-compiler-macro call-with-temporary-directory (&whole form receiver)
  ;; Beta reduce call to call-with-temporary-directory.
  (destructure-function-lambda 1 receiver
    (lambda (arglist docstring declarations body)
        (if (and (consp arglist)
                 (symbolp (car arglist))
                 (null (cdr arglist)))
            `(LET ((,(car arglist) (GET-TEMPORARY-DIRECTORY)))
               ,@(when docstring (list docstring))
               ,@declarations
               (UNWIND-PROTECT
                   (PROGN (ENSURE-DIRECTORIES-EXIST ,(car arglist))
                          ,@body)
                 "deleting temporary directory"
                 (LOCALLY (DECLARE ,(standard-optimizations))
                   (UNLESS *PRESERVE-TEMPORARY-DIRECTORIES*
                     (DELETE-DIRECTORY ,(car arglist) :FORCE T)))))
          form))
    (lambda () form)))

;;; finding things to run!

#+(or :mswindows :win32)
(defparameter *executable-pathname-types*
    (if (boundp '*executable-pathname-types*)
        *executable-pathname-types*
      nil)
  "Windows only.  List of strings that represent the file types
   of `executable' files.  Do not use this variable directly.
   Use get-executable-pathname-types.")

#+(or :mswindows :win32)
(defun get-executable-pathname-types (&key no-cache)
  "Returns a list of strings that represent the file types of
   `executable' files.  If no-cache is specified, the list is
   recomputed."
  #+allegro (declare (:fbound split-string))
  (when (or no-cache
            (null *executable-pathname-types*))
    (setq *executable-pathname-types*
          (if (os-get-environment-variable "PATHEXT")
              ;; Windows returns these with dots on them.
              ;; Strip 'em out.
              (mapcar (lambda (ext)
                        (subseq ext 1))
                      (split-string (os-get-environment-variable "PATHEXT") #\;))
            (list "COM" "BAT" "EXE" "CMD"))))
  *executable-pathname-types*)

#+unix
(defconstant *unix-executable-permissions* #o0005)
(defconstant *unix-writable-permissions*   #o0222)

(defun executable-pathname? (pathname)
  "Returns T iff pathname represents an `executable' file.

  Under UNIX, looks for executable permission bits.  Under NT, looks
  for executable extension."
  (check-type pathname pathname)
  (and (file-pathname? pathname)
       (or
        #+(and :allegro :unix)
        (let ((permissions (excl::filesys-permission (namestring pathname))))
          (and permissions
               (not (zerop (logand *unix-executable-permissions* permissions)))))

        #+(or :mswindows :Win32)
        (and (pathname-type pathname)
             (not (eq (pathname-type pathname) :unspecific))
             (member (pathname-type pathname)
                     (get-executable-pathname-types)
                     :test (platform-filename-equal (server-platform))))
        ;; other platforms go here
        nil
        )))

(defun get-path ()
  "Returns a list of pathnames that represent the directories that are searched
when for executables (if the OS does such a thing.)"
  #+allegro (declare (:fbound split-string))
  (mapcar #'canonicalize-pathname
          (split-string (os-get-environment-variable "PATH")
                        (platform/path-separator (server-platform)))))

(defparameter *user-executables*
    (if (boundp '*user-executables*)
        *user-executables*
      nil)
  "A cached list of pathnames that represent the executable programs that can be
run from the user shell.  Calling get-executables is preferred.")

(defun get-executables (&key no-cache)
  "Returns a list of pathnames that represent the executable programs that can be
run from the OS command line.  This list is cached for performance.  Keyword argument
no-cache causes the search to be redone."
  ;; This will loose if a unix directory is searchable (mode x) but
  ;; not readable (mode r).
  (when (or no-cache
            (null *user-executables*))
    (setq *user-executables*
          (remove-if
           (complement #'executable-pathname?)
           (mapcan (lambda (dir)
                     (sort (directory (make-pathname
                                       :name :wild
                                       :type :wild
                                       :defaults dir))
                           #'string-lessp
                           :key #'pathname-name))
                   ;; Don't use `current directory' if it is the path.
                   (remove "." 
                           (remove "" (get-path)
                                   :key #'namestring
                                   :test #'string-equal)
                            :key #'namestring 
                            :test #'string-equal)))))
  *user-executables*)

(defun search-for-executable (name &key no-cache)
  "Returns a list of full pathnames that represent the executable program that
   would be run if NAME was typed to the command line.  Keyword argument
   no-cache causes the search to be redone."
  (remove-if (complement (lambda (file)
                           (funcall (platform-filename-equal (server-platform))
                                    name
                                    (pathname-name file))))
             (get-executables :no-cache no-cache)))

;;; Particular OS programs of interest.
#+(or :microsoft-32 :win32)
(eval-when (:load-toplevel :execute)
  (defparameter *windows-directory*
      (if (boundp '*windows-directory*)
          *windows-directory*
        (canonicalize-pathname
         (parse-namestring
          (or (os-get-environment-variable "WINDIR")
              ;; Sometimes it is bound to this variable instead.
              (os-get-environment-variable "SYSTEMROOT")
              (error "Cannot locate windows directory.")))))
    "The pathname of the windows installation directory."))

#+(or :microsoft-32 :win32)
(eval-when (:load-toplevel :execute)
  (defparameter *windows-system-directory*
      (if (boundp '*windows-system-directory*)
          *windows-system-directory*
        (canonicalize-pathname
         (merge-pathnames "system\\" *windows-directory*)))
    "The pathname of the windows system directory."))

#+(or :microsoft-32 :win32)
(eval-when (:load-toplevel :execute)
  (defparameter *windows-system32-directory*
      (if (boundp '*windows-system32-directory*)
          *windows-system32-directory*
        (canonicalize-pathname
         (merge-pathnames "system32\\" *windows-directory*)))
    "The pathname of the windows system32 directory."))

(defconstant *canonical-unix-executable-directory*
    (make-pathname :directory '(:absolute "usr" "bin") :defaults "")
  "The standard path to the standard executable programs on unix.")

(defun unix-standard-program-path (program-name)
  (merge-pathnames (make-pathname :name program-name :defaults "")
                   *canonical-unix-executable-directory*))

#+(or :microsoft-32 :win32)
(defun winnt-standard-program-path (program-name)
  (merge-pathnames (make-pathname :name program-name :type "exe" :defaults "")
                   *windows-system32-directory*))

(defmacro define-os-program (name program-path-generator)
  (let ((variable-name (intern (concatenate 'string "*" (string-upcase name) "-PROGRAM*")
                               (find-package "UTILITY")))
        (getter (intern (concatenate 'string "GET-" (string-upcase name) "-PROGRAM")
                        (find-package "UTILITY"))))
    `(PROGN
       (DEFVAR ,variable-name :UNBOUND ,(format nil "Absolute path to the ~s program." name))
       #+allegro (EVAL-WHEN (:LOAD-TOPLEVEL :EXECUTE)
                   (excl:record-source-file ',variable-name))
       ;; Kludge to make name be unbound.
       (EVAL-WHEN (:LOAD-TOPLEVEL :EXECUTE)
         (WHEN (AND (BOUNDP ',variable-name) (EQ ,variable-name :UNBOUND))
           (MAKUNBOUND ',variable-name)))
       (DEFUN ,getter (&KEY NO-CACHE)
         (WHEN (OR (NOT (BOUNDP ',variable-name))
                   NO-CACHE)
           (SETQ ,variable-name
                 (OR (PROBE-FILE (,program-path-generator ,name))
                     (FIND ,name (GET-EXECUTABLES :NO-CACHE NO-CACHE)
                           :KEY #'PATHNAME-NAME
                           :TEST (PLATFORM-FILENAME-EQUAL (SERVER-PLATFORM))))))
         ,variable-name)
       )))

(define-os-program "test"    unix-standard-program-path)
(define-os-program "chmod"   unix-standard-program-path)
(define-os-program "cp"      unix-standard-program-path)
(define-os-program "touch"   unix-standard-program-path)
#+(or :microsoft-32 :win32)
(define-os-program "ATTRIB" winnt-standard-program-path)

#+:lispworks
(defun call-synchronous-subprocess (command-line &key show-window output error-output)
  (declare (ignore show-window error-output))
  (values nil
          (sys:call-system-showing-output command-line
                                          :show-cmd nil
                                          :prefix ""
                                          :output-stream (or output *standard-output*)
                                          :wait t)))

(defun os-copy-command ()
  "Returns the command that instructs the operating system to copy a file."
  #+(or :microsoft-32 :win32) "cmd /c copy"
  #+unix (let ((copy-command (get-cp-program)))
           (when copy-command (format nil "\"~a\"" copy-command))))

#+:lispworks
(defun run-subprocess (command-line &key (silent t))
  (if silent
      (sys:call-system command-line :wait t)
      (sys:call-system-showing-output command-line
                                      :show-cmd nil
                                      :prefix ";"
                                      :wait t)))

(defun os-copy-file (source dest &key verbose element-type)
  "Copy a file from SOURCE to DEST using the native OS copy mechanism.
   (Which presumably does the right thing WRT timestamps)."
  (declare (ignorable verbose)
           (ignore element-type))
  (ensure-directories-exist (pathname-syntactic-parent dest))
  #-(or :microsoft-32 :win32)
  (run-subprocess (format nil "~a ~a ~a" (os-copy-command) source dest)
                  :silent (not verbose))

  #+(or :microsoft-32 :win32)
  (win32-copy-file (pathname->windows-namestring (merge-pathnames source *default-pathname-defaults*))
                   (pathname->windows-namestring (merge-pathnames dest *default-pathname-defaults*))
                   't))

(defun os-file-read-only? (filename)
  (check-type filename pathname)
  #+(or :microsoft-32 :win32)
  (win32-file-read-only?
   (pathname->windows-namestring (merge-pathnames filename *default-pathname-defaults*))))

(defun os-touch-file (filename &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  #+(or :microsoft-32 :win32)
  (win32-touch-file
   (pathname->windows-namestring (merge-pathnames filename *default-pathname-defaults*))))

(defun os-server-locale ()
  #+(or :microsoft-32 :win32)
  (win32-server-locale)
  #-(or :microsoft-32 :win32)
  (error "Don't know how to get the locale."))

(defun os-read-only-command ()
  "Returns the command that instructs the operating system to make a file be read-only."
  (let ((attrib (or #+(or mswindows :win32) (get-attrib-program) nil))
	(chmod (get-chmod-program)))
    (cond (attrib (format nil "\"~a\" +R" attrib))
	  (chmod  (format nil "~a a-w" chmod))
	  (t (error "No apparent way to change the read-only flag.")))))

(defun os-read-write-command ()
  "Returns the command that instructs the operating system to make a file be read-write."
  (let ((attrib (or #+(or mswindows :win32) (get-attrib-program) nil))
	(chmod (get-chmod-program)))
    (cond (attrib (format nil "\"~a\" -R" attrib))
	  (chmod  (format nil "~a a+w" chmod))
	  (t (error "No apparent way to change the read-only flag.")))))

(defun os-test-executable-command ()
  "Returns the command that instructs the operating system to query if a file is executable."
  (let ((test (get-test-program)))
    (cond (test (format nil "~a -x" test))
	  (t (error "No apparent way to test if a program is executable.")))))

(defun os-executable-command ()
  "Returns the command that instructs the operating system to make a file be executable."
  (let ((chmod (get-chmod-program)))
    (cond (chmod  (format nil "~a a+x" chmod))
	  (t (error "No apparent way to change the executable flag.")))))

(defun os-not-executable-command ()
  "Returns the command that instructs the operating system to make a file be non-executable."
  (let ((chmod (get-chmod-program)))
    (cond (chmod  (format nil "~a a-x" chmod))
	  (t (error "No apparent way to change the executable flag.")))))

(defun format-time-for-touch (universal-time)
  "Touch takes a LOCAL time and either the format MMDDhhmmCCYY.ss or
   CCYYMMDDhhmm.ss.  Convert the universal-time to these formats."
  (multiple-value-bind (sec min hour day month year) (decode-universal-time universal-time)
    (values (format nil "~4,'0d~4@{~2,'0d~}.~2,'0d" year month day hour min sec)
	    (format nil "~4@{~2,'0d~}~4,'0d.~2,'0d" month day hour min year sec))))

(defvar *touch-format* :unbound
  "Function of two arguments that selects the correct format for the time string for touch.")

(eval-when (:load-toplevel :execute)
  (when (and (boundp '*touch-format*) (eq *touch-format* :unbound))
    (makunbound '*touch-format*)))

(defun get-touch-format (&key no-cache)
  (when (or (not (boundp '*touch-format*))
	    no-cache)
    (setq *touch-format*
	  (let ((touch-command (get-touch-program))
		(random-date 3141592654)) ;; July 21, 1999, 20:37:34
	    #+allegro (declare (:fbound call-with-temporary-file
					call-with-temporary-directory
					cd
					set-current-directory))
	    (when touch-command
	      ;; Switch to temporary directory so we don't create files
	      ;; that look like timestamps.
	      (call-with-temporary-directory
		(lambda (tmpdir)
		  (let ((saved-directory (current-directory))
			(saved-pathname-defaults *default-pathname-defaults*))
		    (unwind-protect
			(progn (cd tmpdir)
			       (call-with-temporary-file "txt"
				 (lambda (temp-file)
				   (multiple-value-bind (sane-format insane-format)
				       (format-time-for-touch random-date)
				     ;; This is awful.  We don't know the format of the time argument
				     ;; to touch a priori, so we try it both ways and use the one that
				     ;; works.  Actually, this isn't as bad as it could be, but why
				     ;; isn't there a better way to do this?
				     (let ((attempt-one (progn (run-subprocess
								(format nil "\"~a\" -t ~a \"~a\""
									touch-command
									sane-format
									temp-file) :silent t)
							       (os-get-timestamp (namestring temp-file))))
					   (attempt-two (progn (run-subprocess
								(format nil "\"~a\" -t ~a \"~a\""
									touch-command
									insane-format
									temp-file) :silent t)
							       (os-get-timestamp (namestring temp-file)))))
				       (cond ((= attempt-one random-date) #'car)
					     ((= attempt-two random-date) #'cadr)
					     ;; Hmm.  Must not be able to touch files.
					     ;; (error "No known time format.")
					     (t
					      (warn "Command ~s does not appear to work.  Tests will fail."
						    touch-command)
					      (setq *touch-program* nil)
					      nil)))))))
		      "restoring the current directory"
		      (setq *default-pathname-defaults* saved-pathname-defaults)
		      (set-current-directory saved-directory)
		      ))))))))
  *touch-format*)

(defun os-set-file-last-modified-command ()
  "Returns the command that instructs the operating system to change the modification
   date on a file."
  ;;; No program on MSWindows, but if you have cygwin installed,
  ;;; you can use touch.
  (let* ((touch-format  (get-touch-format)) ;; don't swap these
	 (touch-command (get-touch-program)))
    (when (and touch-command touch-format)
      (list touch-command "-t" (funcall touch-format
					(list "CCYYMMDDhhmm.ss"
					      "MMDDhhmmCCYY.ss"))))))

(defgeneric file-bytes (filename)
  (:documentation
   "In ACLPC, the common lisp FILE-LENGTH method would operate on file names.
    In ANSI/CLTL2 lisp, FILE-LENGTH will operate only on open streams.
    This method attempts to act as a more flexible replacement for FILE-LENGTH where
    streams may not be available")
  (:method ((filename stream))
    (file-length filename))
  (:method ((filename string))
    (with-open-file (stream filename
                     :direction :input
                     #-(and :allegro-version>= (:version>= 6 0)) :element-type
                     #-(and :allegro-version>= (:version>= 6 0)) '(unsigned-byte 8))
      (file-length stream)))
  (:method ((filename pathname))
    (with-open-file (stream filename
                     :direction :input
                     #-(and :allegro-version>= (:version>= 6 0)) :element-type
                     #-(and :allegro-version>= (:version>= 6 0)) '(unsigned-byte 8))
      (file-length stream))))


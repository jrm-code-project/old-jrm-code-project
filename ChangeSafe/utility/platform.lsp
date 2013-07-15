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
;;;; File Name:     platform.lsp
;;;; Author:        Joe Marshall
;;;; Creation Date: 11 November 1999
;;;;
;;;; Module Description:
;;;;
;;;; Hacks for platform dependence and independence.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CSF/UTILITY")

(eval-when (:load-toplevel :execute)
  (export '(
            *platforms*
            platform-name->platform
            guarantee-platform
            server-platform
            find-platform

            platform
            platform/directory-separators
            platform/record-separator
            platform/path-separator
            platform/case-sensitive?
            platform/command-line-switch
            platform/parse-namestring
            platform/parse-directory-namestring
            platform-set-file-read-only
            platform-set-file-read-write
            platform-has-file-executable-bit?
            filename-equal-case-sensitive
            filename-equal-fold-case
            filename-lessp-case-sensitive
            filename-lessp-fold-case
            platform-filename-equal
            platform-filename-lessp

            pathname->platform-namestring
            pathname->unix-namestring
            pathname->windows-namestring
            default-directory-separators
            default-record-separator

            parse-client-namestring
            parse-client-directory-namestring

            canonicalize-pathname
            platform-enough-pathname
            platform-pathname-match-p

            guess-pathname-syntax

            add-backtranslation-root
            backtranslate-pathname
            )))

(proclaim (standard-optimizations))

(defvar *platforms* '()
  "Platforms for which we'll conceivably have agents and do file management.
   The primary use of this information is to identify the client requirements
   for certain operations such as line termation of text files, etc.
   If we require specializing within a platform, we will create multiple file-systems,
   i.e. :DOS-NTFS, :DOS-FAT, :DOS-FAT32.  For now, let's hope it doesn't come to that.")

(defclass platform ()
  ((name
    :initarg :name
    :initform (error "Required argument :name omitted.")
    :reader platform/name
    :type string)
   (case-sensitive?
    :initarg :case-sensitive?
    :initform (error "Required argument :case-sensitive? omitted.")
    :reader platform/case-sensitive?
    :type boolean)
   (command-line-switch
    :initarg :command-line-switch
    :initform (error "Required argument :command-line-switch omitted.")
    :reader platform/command-line-switch
    :type list)
   (directory-separators
    :initarg :directory-separators
    :initform (error "Required argument :directory-separators omitted.")
    :reader platform/directory-separators
    :type list)
   (path-separator
    :initarg :path-separator
    :initform (error "Required argument :path-separator omitted.")
    :reader platform/path-separator
    :type character)
   (record-separator
    :initarg :record-separator
    :initform (error "Required argument :record-separator omitted.")
    :reader platform/record-separator
    :type symbol)))

(defmethod print-object ((platform platform) stream)
  (print-unreadable-object (platform stream :type t)
    (format stream "~a" (platform/name platform))))

(defmethod initialize-instance :after ((platform platform) &rest ignore)
  (declare (ignore ignore))
  (let ((found (member (platform/name platform)
                       *platforms*
                       :key #'platform/name
                       :test #'string-equal)))
    (if found
        (setf (car found) platform)
        (push platform *platforms*))))

(defgeneric platform/parse-namestring (platform namestring &key (start 0) end))

(defclass posix-platform (platform)
  ()
  (:default-initargs
   :case-sensitive?      t
   :command-line-switch  '(#\-)
   :directory-separators '(#\/)
   :path-separator       #\:
   :record-separator     :lf))

(defclass unix-platform (posix-platform) ())
(defclass hpux-platform (unix-platform)  ())
(defclass linux-platform (unix-platform) ())

(defclass microsoft-platform (platform)
  ()
  (:default-initargs
   :case-sensitive?      nil
   :command-line-switch  '(#\\ #\-)
   :directory-separators '(#\\ #\/)
   :path-separator       #\;
   :record-separator     :crlf))

(defmethod platform/parse-namestring ((platform microsoft-platform) namestring
                                      &key (start 0) end)
  (let ((end1  (or end (string-length namestring))))
    (multiple-value-bind (success drive directory name type stream)
        (parse-dos-namestring namestring :start start :end end1)
      (declare (ignore stream))
      (when success
        (make-pathname :host (string drive)
                       :directory directory
                       :name name
                       :type type
                       :defaults "")))))

(defmethod platform/parse-directory-namestring ((platform microsoft-platform) namestring
                                                &key (start 0) end)
  (let ((end1 (or end (string-length namestring))))
    (multiple-value-bind (success drive directory name type stream)
        (parse-dos-namestring namestring :start start :end end1)
      (declare (ignore stream))
      (when success
        (make-pathname :host (string drive)
                       :directory (if (or name type)
                                      (append directory
                                              (list (format nil "~@[~a~]~@[.~a~]" name type)))
                                      directory)
                       :defaults "")))))

(defclass win32-platform (microsoft-platform) ())

(eval-when (:load-toplevel :execute)
  (make-instance 'unix-platform         :name "Unix")
  (make-instance 'win32-platform        :name "Win32")
  (make-instance 'win32-platform        :name "WinNT")
  (make-instance 'win32-platform        :name "Windows 2000")
  (make-instance 'win32-platform        :name "Windows NT")
  (make-instance 'hpux-platform         :name "HP-UX")
  (make-instance 'linux-platform        :name "Linux"))

(defun find-platform (name)
  (check-type name string)
  (car (member name *platforms*
                   :key #'platform/name
                   :test #'string-equal)))

(defvar *server-platform* nil)

(defun server-platform ()
  (or *server-platform*
      (setq *server-platform*
            (find-platform #+:unix "Unix"
                           #+:win32 "Win32"))))

(defmethod platform/directory-separators ((platform symbol))
  (platform/directory-separators (find-platform (symbol-name platform))))

#||
(defconstant *platforms* '(:AIX :FREEBSD :HP-UX :IRIX :LINUX
                           :MAC
                           :OS2
                           :SOLARIS :UNIX
                           :VMS
                           :WIN32 :WIN95 :WIN98 :WINNT :WINCE :WINXP :WIN2K
                           :MULTICS
                           :DOS)
  "Platforms for which we'll conceivably have agents and do file management.
   The primary use of this information is to identify the client requirements
   for certain operations such as line termation of text files, etc.
   If we require specializing within a platform, we will create multiple file-systems,
   i.e. :DOS-NTFS, :DOS-FAT, :DOS-FAT32.  For now, let's hope it doesn't come to that.")

(defconstant *platform-names*
    '(
      ("aix"        :aix)
      ("freebsd"    :freebsd)
      ("hp-ux"      :hp-ux)
      ("irix"       :irix)
      ("linux"      :linux)
      ("mac os"     :mac)
      ("macos"      :mac)
      ("os/2"       :os2)
      ("solaris"    :solaris)
      ("windows 95" :win95)
      ("windows 98" :win98)
      ("windows ce" :wince)                     ; ouch!
      ("windows nt" :winnt)
      ("windowsnt"  :winnt)
      ("windows xp" :winxp)
      ("windowsxp"  :winxp)
      ("windows 2000" :win2k)
      ("windows2k"  :win2k)
      ))


(defun platform-name->platform (name)
  "Given a string naming an OS, such as one might get from a java client, return the
   platform designator for that platform."
  (let ((entry (assoc name *platform-names* :test #'string-equal)))
    (if entry
        (second entry)
      (error "Unknown platform name ~s" name))))

(defun guarantee-platform (platform)
  "Check to see that PLATFORM is a member of *PLATFORMS*.  If so, simply return it.
   If not, raise an error."
  (unless (member platform *platforms*)
    (error "~S is not a recognized platform" platform))
  platform)

(defsubst server-platform ()
  "Returns the platform on which the lisp implementation is running."
  #+unix :UNIX
  #+win32 :WIN32
  )

(defun platform-case-sensitive? (platform)
  "Returns T if files on this platform are case sensitive."
  (case platform
    ((:mac :os2 :dos :win32 :winnt :win95 :win98 :wince) nil)
    ((:aix :freebsd :hp-ux :irix :linux :solaris :unix) t)
    (t (error "Unknown case sensitivity for this os: ~s" platform))))
||#

(defun file-name-equal-case-sensitive (left right)
  "Return T iff left and right are equal filenames in a case sensitive manner."
  (cond ((and (stringp left) (stringp right)) (string= left right))
        ((pathnamep left) (file-name-equal-case-sensitive (namestring left) right))
        ((pathnamep right) (file-name-equal-case-sensitive left (namestring right)))
        (t (error "These don't seem to be filenames: ~s and ~s" left right))))

(defun file-name-equal-fold-case (left right)
  "Return T iff left and right are equal filenames in a case insensitive manner."
  (cond ((and (stringp left) (stringp right)) (string-equal left right))
        ((pathnamep left) (file-name-equal-fold-case (namestring left) right))
        ((pathnamep right) (file-name-equal-fold-case left (namestring right)))
        (t (error "These don't seem to be filenames: ~s and ~s" left right))))

(defun file-name-lessp-case-sensitive (left right)
  "Return T iff left is lexicographically before right in a case sensitive manner."
  (cond ((and (stringp left) (stringp right)) (string< left right))
        ((pathnamep left) (file-name-lessp-case-sensitive (namestring left) right))
        ((pathnamep right) (file-name-lessp-case-sensitive left (namestring right)))
        (t (error "These don't seem to be filenames: ~s and ~s" left right))))

(defun file-name-lessp-fold-case (left right)
  "Return T iff left is lexicographically before right in a case insensitive manner."
  (cond ((and (stringp left) (stringp right)) (string-lessp left right))
        ((pathnamep left) (file-name-lessp-fold-case (namestring left) right))
        ((pathnamep right) (file-name-lessp-fold-case left (namestring right)))
        (t (error "These don't seem to be filenames: ~s and ~s" left right))))

(defun platform-filename-equal (platform)
  "Returns the correct filename equality predicate for platform."
  (if (platform/case-sensitive? platform)
      #'file-name-equal-case-sensitive
    #'file-name-equal-fold-case))

(defun platform-filename-lessp (platform)
  "Returns the correct filename ordering predicate for platform."
  (if (platform/case-sensitive? platform)
      #'file-name-lessp-case-sensitive
    #'file-name-lessp-fold-case))
#||
(defun platform-path-separator (platform)
  "Returns the path separator appropriate for PLATFORM.
   That is the separator for the command search path list."
  (case platform
    ((:os2 :dos :win32 :winnt :win95 :win98 :wince) #\;)
    ((:aix :freebsd :hp-ux :irix :linux :solaris :unix) #\:)
    (t (error "Unknown path separator for this os: ~s" platform))))

(defun platform-directory-separators (platform)
  "Returns a list of the pathname directory component separators appropriate for PLATFORM."
  (case platform
    ((:DOS :WIN32 :WINNT :WIN95 :WIN98) '(#\\ #\/))
    ((:AIX :FREEBSD :HP-UX :IRIX :LINUX :SOLARIS :UNIX) '(#\/))
    ((:MAC) '(#\:))
    ((:MULTICS) '(#\>))
    (t (error "Unknown directory separator for this os: ~s" platform))))

(defun platform-record-separator (platform)
  "Returns the text record separator description appropriate for PLATFORM."
  (case platform
    ((:DOS :WIN32 :WINNT :WIN95 :WIN98) :CRLF)
    ((:AIX :FREEBSD :HP-UX :IRIX :LINUX :SOLARIS :UNIX) :LF)
    ((:MAC) :CR)
    (t (error "Unknown record separator for this os: ~s" platform))))

(defun platform-set-file-read-only (platform)
  "Returns two values, the name of the program used by the platform to set a file to
   read-only status, and the flags that are necessary to pass."
  (ecase platform
    ((:DOS :WIN32 :WINNT :WIN95 :WIN98) (values "ATTRIB" "+R"))
    ((:AIX :FREEBSD :HP-UX :IRIX :LINUX :SOLARIS :UNIX) (values "chmod" "ug-w"))))

(defun platform-set-file-read-write (platform)
  "Returns two values, the name of the program used by the platform to set a file to
   read-write status, and the flags that are necessary to pass."
  (ecase platform
    ((:DOS :WIN32 :WINNT :WIN95 :WIN98) (values "ATTRIB" "-R"))
    ((:AIX :FREEBSD :HP-UX :IRIX :LINUX :SOLARIS :UNIX) (values "chmod" "ug+w"))))

(defun platform-has-file-executable-bit? (platform)
  "Return true if the platform's standard file system has a file executable bit."
  (case platform
    ((:UNIX :HP-UX :LINUX :SOLARIS :FREEBSD :AIX :IRIX) t)
    (t nil)))

(defun pathname->platform-namestring (pname platform)
  "Given PNAME and PLATFORM, returns a namestring in the appropriate syntax for that platform."
  (ecase platform
    ((:DOS :WIN32 :WINNT :WIN95 :WIN98) (pathname->windows-namestring pname))
    ((:AIX :FREEBSD :HP-UX :IRIX :LINUX :SOLARIS :UNIX) (pathname->unix-namestring pname))
    ))
||#

(defun pathname->unix-namestring (pname)
  "Given PNAME, returns a namestring in UNIX syntax.  Consider using pathname->platform-namestring."
  (let ((pname (if (logical-pathname-p pname)
                   (translate-logical-pathname pname)
                 pname)))
    (assert (or (null (pathname-host pname))
                (eq (pathname-host pname) :unspecific)))
    (assert (or (null (pathname-device pname))
                (eq (pathname-device pname) :unspecific)))
    (assert (or (null (pathname-version pname))
                (eq (pathname-version pname) :unspecific)))
    (format nil "~@[~{~@[/~*~]~@{~a/~}~}~]~@[~a~]~@[.~a~]"
            ;; Magic format stream.  Prints initial slash if :absolute,
            ;; prints slashes between directory components, if present,
            ;; prints name, if present, and, if the type is present,
            ;; a dot and the type.
            (unless (or (null (pathname-directory pname))
                        (eq (pathname-directory pname) :unspecific))
              (cons (eq (car (pathname-directory pname)) :absolute)
                    (cdr (pathname-directory pname))))
            (pathname-name pname)
            (pathname-type pname))))

(defmethod pathname->platform-namestring ((pname pathname) (platform posix-platform))
  (pathname->unix-namestring pname))

(defun pathname->windows-namestring (pname)
  "Given PNAME, returns a namestring in WINDOWS syntax.
   Consider using pathname->platform-namestring."
  (let ((pname (if (logical-pathname-p pname)
                   (translate-logical-pathname pname)
                 pname)))
    #+lispworks
    (assert (or (null (pathname-device pname))
                (eq (pathname-device pname) :unspecific)))
    #-:lispworks
    (assert (or (null (pathname-host pname))
                (eq (pathname-host pname) :unspecific)
                (logical-pathname-p pname)))
    (assert (or (null (pathname-version pname))
                (eq (pathname-version pname) :unspecific)
                (eq (pathname-version pname) :newest)))
    (format nil "~@[~a:~]~@[~{~@[\\~*~]~@{~a\\~}~}~]~@[~a~]~@[.~a~]"
            ;; Magic format string.  Prints device and colon if
            ;; device is present, initial backslash if :absolute,
            ;; prints backslashes between directory components, if present,
            ;; prints name, if present, and, if the type is present,
            ;; a dot and the type.
            #+:lispworks
            (unless (or (null (pathname-host pname))
                        (eq (pathname-host pname) :unspecific))
              (pathname-host pname))
            #-:lispworks
            (unless (or (null (pathname-device pname))
                        (eq (pathname-device pname) :unspecific))
              (pathname-device pname))
            (unless (or (null (pathname-directory pname))
                        (eq (pathname-directory pname) :unspecific))
              (cons (eq (car (pathname-directory pname)) :absolute)
                    (map 'list
                         (lambda (component)
                           (cond ((eq component :wild) "*")
                                 ((eq component :wild-inferiors) "**")
                                 (t component)))
                    (cdr (pathname-directory pname)))))
            (cond ((eq (pathname-name pname) :wild) "*")
                  ((eq (pathname-name pname) :unspecific) nil)
                  (t (pathname-name pname)))
            (cond ((eq (pathname-type pname) :wild) "*")
                  ((eq (pathname-type pname) :unspecific) nil)
                  (t (pathname-type pname))))))

(defmethod pathname->platform-namestring ((pname pathname) (platform microsoft-platform))
  (pathname->windows-namestring pname))

(defvar *default-directory-separators* nil)

(defun default-directory-separators ()
  "Returns the directory-separator in use on the server."
  (or *default-directory-separators*
      (setq *default-directory-separators*
        (platform/directory-separators (server-platform)))))

(defvar *default-record-separator* nil)

(defun default-record-separator ()
  "Returns the record-separator in use on the server."
  (or *default-record-separator*
      (setq *default-record-separator*
        (platform/record-separator (server-platform)))))

(defgeneric parse-client-namestring (namestring os-type &key start)
  (:documentation
   "Parse the pathname NAMESTRING to a pathname.  The namestring is presumed to
    come from a system of type OS-TYPE, which determines what the syntax of the
    pathname to be parsed is.

    OS-TYPE must be a member of *PLATFORMS*.
    START is the index into NAMESTRING where parsing should commence."))

(defgeneric parse-client-directory-namestring (namestring os-type &key start)
  (:documentation
   "Parse the pathname NAMESTRING to a pathname that names a directory.
    The namestring is presumed to come from a system of type OS-TYPE, which
    determines what the syntax of the pathname to be parsed is.  Trailing directory
    separator is optional as the namestring will always be interpreted as having
    no file component.
    START is the index into NAMESTRING where parsing should commence."))

(defmethod parse-client-namestring ((namestring string) (os-type (eql :winnt))
                                    &key (start 0))
  (let ((colon (position #\: namestring
                         :test #'char-equal
                         :start start)))
    (parse-delimited-pathname namestring (platform/directory-separators os-type)
                              (if colon (1+ colon) start)
                              nil
                              (when colon
                                (subseq namestring start colon)))))

(defmacro define-parse-client-namestring-trampoline (gf from-os-type to-os-type)
  `(defmethod ,gf ((namestring string) (os-type (eql ,from-os-type))
                   &key (start 0))
     (,gf namestring ,to-os-type :start start)))

(define-parse-client-namestring-trampoline parse-client-namestring :win95 :win32)
(define-parse-client-namestring-trampoline parse-client-namestring :win98 :win32)
(define-parse-client-namestring-trampoline parse-client-namestring :win32 :win32)
(define-parse-client-namestring-trampoline parse-client-namestring :wince :win32)
(define-parse-client-namestring-trampoline parse-client-namestring :dos :win32)
(define-parse-client-namestring-trampoline parse-client-namestring :os2 :win32)

(define-parse-client-namestring-trampoline parse-client-namestring :aix     :unix)
(define-parse-client-namestring-trampoline parse-client-namestring :freebsd :unix)
(define-parse-client-namestring-trampoline parse-client-namestring :hp-ux   :unix)
(define-parse-client-namestring-trampoline parse-client-namestring :irix    :unix)
(define-parse-client-namestring-trampoline parse-client-namestring :linux   :unix)
(define-parse-client-namestring-trampoline parse-client-namestring :solaris :unix)

(defmethod parse-client-namestring ((namestring string) (os-type (eql :unix))
                                    &key (start 0))
  (parse-delimited-pathname namestring (platform/directory-separators os-type) start nil nil))

(defmethod parse-client-namestring (namestring (os-type string) &key (start 0))
  (parse-client-namestring namestring
                           (find-platform os-type)
                           :start start))

(defmethod parse-client-directory-namestring ((namestring string) (os-type (eql :winnt))
                                              &key (start 0))
  (let ((colon (position #\: namestring
                         :test #'char-equal
                         :start start)))
    (parse-delimited-directory-pathname namestring (platform/directory-separators os-type)
                                        (if colon (1+ colon) start)
                                        nil
                                        (when colon
                                          (subseq namestring start colon)))))

(define-parse-client-namestring-trampoline parse-client-directory-namestring :win95 :win32)
(define-parse-client-namestring-trampoline parse-client-directory-namestring :win98 :win32)
(define-parse-client-namestring-trampoline parse-client-directory-namestring :win32 :win32)
(define-parse-client-namestring-trampoline parse-client-directory-namestring :wince :win32)
(define-parse-client-namestring-trampoline parse-client-directory-namestring :dos :win32)
(define-parse-client-namestring-trampoline parse-client-directory-namestring :os2 :win32)

(define-parse-client-namestring-trampoline parse-client-directory-namestring :aix     :unix)
(define-parse-client-namestring-trampoline parse-client-directory-namestring :freebsd :unix)
(define-parse-client-namestring-trampoline parse-client-directory-namestring :hp-ux   :unix)
(define-parse-client-namestring-trampoline parse-client-directory-namestring :irix    :unix)
(define-parse-client-namestring-trampoline parse-client-directory-namestring :linux   :unix)
(define-parse-client-namestring-trampoline parse-client-directory-namestring :solaris :unix)

(defmethod parse-client-directory-namestring ((namestring string) (os-type (eql :unix))
                                              &key (start 0))
  (parse-delimited-directory-pathname namestring (platform/directory-separators os-type) start nil nil))

(defmethod parse-client-directory-namestring (namestring (os-type string)
                                              &key (start 0))
  (parse-client-directory-namestring
   namestring (find-platform os-type)
   :start start))

(defun platform-enough-pathname (platform)
  "Returns an enough-pathname function appropriate for PLATFORM.

   This function works as follows:
   Returns an pathname that is just sufficient to identify the file named by pathname
   when considered relative to the root-directory. It is required that

  (merge-pathnames (enough-pathname pathname defaults) defaults) == pathname

  in all cases, and the result of enough-pathname is the shortest reasonable pathname
  (in terms of directory components) that will satisfy this criterion.
  If pathname is not below root-directory, the pathname is simply returned."
  (let ((component-equal (platform-filename-equal platform)))
    (lambda (pathname root-directory)
      (block platform-enough-pathname
        ;; Make sure host and device, if present, are equal.
        (if (or (and (pathname-host pathname)
                     (stringp (pathname-host pathname))
                     (not (and (pathname-host root-directory)
                               (stringp (pathname-host root-directory))
                               (funcall component-equal
                                        (pathname-host pathname)
                                        (pathname-host root-directory)))))
                (and (pathname-device pathname)
                     (stringp (pathname-device pathname))
                     (not (and (pathname-device root-directory)
                               (stringp (pathname-device root-directory))
                               (funcall component-equal
                                        (pathname-device pathname)
                                        (pathname-device root-directory))))))
            pathname
            (make-pathname
             :host (when (logical-pathname-p pathname) (pathname-host pathname))
             :directory
             (canonicalize-pathname-directory
              (cons :RELATIVE
                    (tail-labels ((luup (pathname-dir-tail root-dir-tail)
                                    (cond ((null root-dir-tail) pathname-dir-tail)
                                          ((null pathname-dir-tail)
                                           (return-from platform-enough-pathname pathname))
                                          ((funcall component-equal (car pathname-dir-tail) (car root-dir-tail))
                                           (luup (cdr pathname-dir-tail) (cdr root-dir-tail)))
                                          (t (return-from platform-enough-pathname pathname)))))
                      (luup (cdr (pathname-directory pathname))
                            (cdr (pathname-directory root-directory))))))
             :name (pathname-name pathname)
             :type (pathname-type pathname)
             :version (pathname-version pathname)))))))

(defun platform-pathname-match-p (platform)
  "Return a function for performing pathname comparisons, examining PLATFORM, which is a client platform
   preference (one of the values of *PLATFORMS*,
   and the server platform running this function.  We want to return a function that
   will perform appropriate case sensitive comparisons for the client platform."
  (let ((server-cs?   (platform/case-sensitive? (server-platform)))
        (platform-cs? (platform/case-sensitive? platform)))
    (if server-cs?
        (if platform-cs?
            #'pathname-match-p
          #'pathname-match-p-fold-case)
      (if platform-cs?
          (error "Lucky you, you get to write a case sensitive pathname-match-p.")
        #'pathname-match-p))))

(defun guess-pathname-syntax (namestring)
  "Given a pathname namestring, try to guess what pathname
   syntax to parse t with.
   This is a complete kludge and should only be used as a last resort
   when you have no other clue how to parse the pathname."
  (collect-first
   (choose-if (lambda (platform)
                (find (car (platform/directory-separators platform))
                      namestring))
              ;; we cant look at other platforms in *PLATFORMS*
              ;; because, for example, the directory separator on the
              ;; Macintosh, ':', can naturally appear in a DOS file
              ;; namestring.
              #z(:win32 :unix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; canonicalize filenames

;;; Has to be here because it depends on the directory separators!
(defun canonicalize-pathname (pathname)
  "Given a pathname to a `file' on the local machine, turn it into a `directory' pathname
if it actually refers to a directory."
  (cond ((not (pathnamep pathname))
         (canonicalize-pathname (pathname pathname)))
        ((and (file-pathname? pathname)
              #+allegro (excl:file-directory-p pathname)
              #+lispworks (lw:file-directory-p pathname))
         (ensure-directory-pathname-designator pathname (default-directory-separators)))
        (t
         (make-pathname
          :host      (canonicalize-pathname-element   (pathname-host pathname))
          :device    (canonicalize-pathname-device    (pathname-device pathname))
          :directory (canonicalize-pathname-directory (pathname-directory pathname))
          :name      (canonicalize-pathname-element   (pathname-name pathname))
          :type      (canonicalize-pathname-element   (pathname-type pathname))
          :version   (canonicalize-pathname-element   (pathname-version pathname))))))

(defparameter +backtranslation-logical-roots+ nil
  "A list of logical pathnames corresponding to the roots of logical hosts.
BACKTRANSLATE-PATHNAME will try each of these logical pathnames when backtranslating.")

(defun add-backtranslation-root (logical-root)
  (pushnew logical-root +backtranslation-logical-roots+
           :test #'pathname-match-p))

(defun backtranslate-pathname (pathname &optional logical-pathname-root)
  ;; TRANSLATE-PATHNAME signals an error if the pathname doesn't match
  ;; the source pathname
  (flet ((try (root)
           (ignore-errors
            (translate-pathname pathname
                                (canonicalize-pathname
                                 (translate-logical-pathname root))
                                root))))
    (or (if logical-pathname-root
            (try logical-pathname-root)
	    (collect-first
	     (map-fn 't #'try (scan +backtranslation-logical-roots+))))
        pathname)))

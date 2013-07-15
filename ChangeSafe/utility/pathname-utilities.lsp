;;;; -*- Mode: LISP; coding: iso-8859-1; Syntax: COMMON-LISP; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2003 ChangeSafe, LLC
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
;;;; Module Description:
;;;;
;;;; File Name:     pathname-hacks.lsp
;;;; Author:        Mark Nahabedian
;;;; Creation Date: 28 October 1999
;;;;
;;;; Module Description:
;;;;
;;;; Utilities for dealing with pathnames, including a platform
;;;; independent parser.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CSF/UTILITY")

;;; API's exported from this module.
(eval-when (:load-toplevel :execute)
  (export '(
            absolute-pathname?
            directory-pathname?
            file-pathname?
            relative-pathname?
            root-pathname?

            file-pathname
            directory-pathname
            absolute-pathname
            absolute-directory-pathname
            absolute-file-pathname
            relative-pathname
            relative-directory-pathname
            relative-file-pathname

            guarantee-file-pathname
            guarantee-directory-pathname
            guarantee-absolute-pathname
            guarantee-absolute-directory-pathname
            guarantee-absolute-file-pathname
            guarantee-relative-pathname
            guarantee-relative-directory-pathname
            guarantee-relative-file-pathname

            canonicalize-pathname-directory
            merge-directories
            ;; better-directory
            ;; better-truename
            ;; better-probe-file

            ensure-directory-pathname-designator
            pathname-syntactic-parent
            enough-pathname

            encode-namestring
            decode-namestring
            decode-pathname

            make-wild-pathname
            push-pathname-type
            pathname->logical-pathname
            logical-pathname-parent-directory
            )))

(proclaim (standard-optimizations))

;;; A pathname that refers to a directory is different from one that refers
;;; to a file.  The functions DIRECTORY-PATHNAME? and FILE-PATHNAME? can be used to
;;; discriminate between the two.

;;; Pathname predicates

(defun directory-pathname? (pathname)
  "Returns T iff pathname NAME and TYPE are NULL or :UNSPECIFIC"
  (check-type pathname pathname)
  ;; (listp (pathname-directory pathname))
  (and (or (null (pathname-name pathname))    (eq (pathname-name pathname)    :unspecific))
       (or (null (pathname-type pathname))    (eq (pathname-type pathname)    :unspecific))
       ;; (or (null (pathname-version pathname)) (eq (pathname-version pathname) :unspecific))
       ))

(defsubst file-pathname? (pathname)
  "Returns T iff pathname has a NAME or TYPE component."
  (not (directory-pathname? pathname)))

;;; True for C:\\ or /
(defun root-pathname? (pathname)
  "Returns T iff pathname DIRECTORY, NAME, TYPE, and VERSION are NULL or :UNSPECIFIC"
  (check-type pathname pathname)
  (and (or (null (pathname-directory pathname))
           (eq (pathname-directory pathname) :unspecific)
           (and (listp (pathname-directory pathname))
                (eq (car (pathname-directory pathname)) :absolute)
                (null (cdr (pathname-directory pathname)))))
       (directory-pathname? pathname)))

(defun relative-pathname? (pathname)
  "Returns T iff pathname has no DIRECTORY, or DIRECTORY begins with :RELATIVE."
  (or (null (pathname-directory pathname))
      (eq (pathname-directory pathname) :unspecific)
      (eq (car (pathname-directory pathname)) :RELATIVE)))

(defun absolute-pathname? (pathname)
  "Returns T if pathname has a DIRECTORY and that directory begins with :ABSOLUTE."
  (and (consp (pathname-directory pathname))
       (eq (car (pathname-directory pathname)) :ABSOLUTE)))

(deftype directory-pathname () `(and pathname (satisfies directory-pathname?)))
(deftype file-pathname () `(and pathname (satisfies file-pathname?)))
(deftype absolute-pathname () `(and pathname (satisfies absolute-pathname?)))
(deftype absolute-directory-pathname () `(and pathname (satisfies absolute-pathname?) (satisfies directory-pathname?)))
(deftype absolute-file-pathname () `(and pathname (satisfies absolute-pathname?) (satisfies file-pathname?)))
(deftype relative-pathname () `(and pathname (satisfies relative-pathname?)))
(deftype relative-directory-pathname () `(and pathname (satisfies relative-pathname?) (satisfies directory-pathname?)))
(deftype relative-file-pathname () `(and pathname (satisfies relative-pathname?) (satisfies file-pathname?)))
(deftype root-pathname () `(and pathname (satisfies root-pathname?)))

;;; Error checking.
(defun guarantee-directory-pathname (pathname)
  "Return PATHNAME iff it names a directory.  Otherwise raise an error."
  (check-type pathname directory-pathname)
  pathname)

(defun guarantee-file-pathname (pathname)
  "Return PATHNAME iff it names a file.  Otherwise raise an error."
  (check-type pathname file-pathname)
  pathname)

(defun guarantee-absolute-pathname (pathname)
  "Return PATHNAME iff it is an absolute pathname  Otherwise raise an error."
  (check-type pathname absolute-pathname)
  pathname)

(defun guarantee-relative-pathname (pathname)
  "Return PATHNAME iff it is a relative pathname.  Otherwise raise an error."
  (check-type pathname relative-pathname)
  pathname)

(defun guarantee-absolute-directory-pathname (pathname)
  "Return PATHNAME iff it is an absolute pathname that names a directory.  Otherwise raise an error."
  (check-type pathname absolute-directory-pathname)
  pathname)

(defun guarantee-absolute-file-pathname (pathname)
  "Return PATHNAME iff it is an absolute pathname that names a file.  Otherwise raise an error."
  (check-type pathname absolute-file-pathname)
  pathname)

(defun guarantee-relative-directory-pathname (pathname)
  "Return PATHNAME iff it is a relative pathname that names a directory.  Otherwise raise an error."
  (check-type pathname relative-directory-pathname)
  pathname)

(defun guarantee-relative-file-pathname (pathname)
  "Return PATHNAME iff it is a relative pathname that names a file.  Otherwise raise an error."
  (check-type pathname relative-file-pathname)
  pathname)

;;; Pathname canonicalization.
;;; Pathname directories '(:RELATIVE), '(:RELATIVE "."), and NIL are equivalent,
;;; Pathname directories '(:RELATIVE "." "FOO") and '(:RELATIVE "FOO") are
;;;   equivalent.
;;; Convert to a canonical form.

(defun canonicalize-pathname-directory (directory)
  "Convert a pathname directory specifier to canonical form."
  ;; Ugly.  Turn '(:RELATIVE) and '(:RELATIVE ".") into NIL,
  ;; turn '(:RELATIVE "." "foo") into (:RELATIVE "foo")
  (cond ((null directory) nil)
        ((eq directory :UNSPECIFIC) nil)
        ((eq (car directory) :ABSOLUTE)
         (if (and (cdr directory)
                  (eq (cadr directory) :BACK))
             (canonicalize-pathname-directory (cons :ABSOLUTE (cddr directory)))
           (cons :ABSOLUTE (remove "." (cdr directory) :test #'string=))))
        ((null (cdr directory)) nil)
        ((string-equal (cadr directory) ".")
         (canonicalize-pathname-directory (cons :RELATIVE (remove "." (cddr directory) :test #'string=))))
        (t directory)))

;;; Merging directories.
;;; Merging directories is a bit complex, so we get the underlying pathname merger
;;; to do it for us.
(defun merge-directories (left right)
  (canonicalize-pathname-directory
   (pathname-directory
    (merge-pathnames
     (make-pathname :directory left :defaults "" )
     (make-pathname :directory right :defaults "")))))

(defsubst canonicalize-pathname-element (element)
  "Convert :unspecific to NIL when copying pathname elements."
  (if (eq element :unspecific)
      nil
    element))

(defun canonicalize-pathname-device (device)
  "Convert a pathname device specifier to canonical form."
  ; #+allegro (declare (:explain :types :calls))
  (cond ((null device) nil)
        ((eq device :unspecific) nil)
        ((and (stringp device)
              (= (string-length device) 1))
         (string-upcase device))
        (t device)))

(defun ensure-directory-pathname-designator (dir-spec directory-separators)
  "Ensure that DIR-SPEC is a 'pathname designator', such that a call to DIRECTORY
   on the result of this function will return contents of the directory, and not a pathname
   naming the directory.

   When DIR-SPEC is a string, that means we want a trailing '/' or '\' (host specific),
   so that DIR-SPEC will be interpreted as a directory for calls to DIRECTORY, instead of just
   a flat file.

   When DIR-SPEC is a pathanme, we ensure that pathname components such as file name and type are
   removed or adjusted if required for the pathname to work as expected in a call to DIRECTORY.

   DIR-SPEC is never modified, if adjustments are necessary, we return a new object of the same
   type."
  ;#+allegro (declare (:explain :calls :types))
  (cond ((stringp dir-spec)
         (if (member (char dir-spec (1- (string-length dir-spec))) directory-separators :test #'char-equal)
               dir-spec
             (concatenate 'string (the string dir-spec) (string (car directory-separators)))))
        ((pathnamep dir-spec)
         (if (file-pathname? (the pathname dir-spec))
             ;; Rather than invoking the string parsing mechanism, which has the quirks noted
             ;; below, just move the name (and type and version) to the end of the directory list.
             (make-pathname
              :host   (canonicalize-pathname-element (pathname-host (the pathname dir-spec)))
              :device (canonicalize-pathname-device (pathname-device (the pathname dir-spec)))
              ;; Paste up the new directory list.
              :directory (canonicalize-pathname-directory
                          (append (if (member (pathname-directory (the pathname dir-spec)) '(nil :unspecific))
                                      ;; should this return an absolute or a relative directory?
                                      ;; I think relative  ~jrm
                                      ;; '(:ABSOLUTE)
                                      '(:RELATIVE)
                                    (pathname-directory (the pathname dir-spec)))
                                  (list (format nil
                                                ;; POTENTIAL BUG HERE:  We don't know that
                                                ;; it is a `dot', but it is on most of the
                                                ;; platforms.
                                                "~@[~a~]~@[.~a~]~@[.~a~]"
                                                (canonicalize-pathname-element
                                                 (pathname-name (the pathname dir-spec)))
                                                (canonicalize-pathname-element
                                                 (pathname-type (the pathname dir-spec)))
                                                (canonicalize-pathname-element
                                                 (let ((version (pathname-version (the pathname dir-spec))))
                                                   (unless (eq version :newest) version)))))))
              :name nil
              :type nil
              :version nil)
           (the pathname dir-spec)))
        (t (error "Expected string or pathname directory specification, received ~s" dir-spec))))

(defun pathname-new-type (pathname type)
  "Return a new pathname where the type has been changed."
  (check-type pathname file-pathname)
  (make-pathname
   :host      (canonicalize-pathname-element (pathname-host pathname))
   :device    (canonicalize-pathname-device  (pathname-device pathname))
   :directory (canonicalize-pathname-directory (pathname-directory pathname))
   :name      (canonicalize-pathname-element (pathname-name pathname))
   :type      type
   :version   (canonicalize-pathname-element (pathname-version pathname))))

(defun pathname-syntactic-parent (pathname)
  "Returns the containing directory of pathname.

   This simply lops of the right hand end of the pathname, so it only
   performs a syntactic operation.  On a system with symbolic or hard
   links, it may give the incorrect answer."
  (check-type pathname pathname)
  (cond ((file-pathname? pathname)
         (make-pathname
          :host      (canonicalize-pathname-element (pathname-host pathname))
          :device    (canonicalize-pathname-device  (pathname-device pathname))
          :directory (canonicalize-pathname-directory (pathname-directory pathname))
          :name      nil
          :type      nil
          :version   nil))
        ((root-pathname? pathname)
         (error "Root pathname ~s has no parent." pathname))
        ((directory-pathname? pathname)
         (make-pathname
          :host      (canonicalize-pathname-element (pathname-host pathname))
          :device    (canonicalize-pathname-device  (pathname-device pathname))
          :directory (let ((dir (canonicalize-pathname-directory (pathname-directory pathname))))
                       (if (and dir (cdr dir))
                           (butlast dir)
                         (error "No parent for pathname ~s" pathname) ;; shouldn't happen
                         ))
          :name      nil
          :type      nil
          :version   nil))
        (t (error "Pathname is neither a file, directory, nor root pathname?!" pathname))))

;;;; What a mess!
;;;; Turns out that it is pretty damn hard to get the case preserved pathname out of NT.
;;;; The only function that will reliably do it is DIRECTORY.  (No, there is no function
;;;; in the WinAPI that does it, I checked.)  So given a file, the only way to get the
;;;; case preserved name of it is by calling DIRECTORY on the parent directory and
;;;; searching the resulting list for the filename.
;;;; However, calling directory will not give you the preserved case on the directory itself.
;;;; The only way to get that is to call directory on the PARENT of the parent
;;;; directory, and so on up to the root.
;;;; The difficulty is trying to escape the combinatoric explosion that would happen
;;;; if we did this the naive way.
;;;;
;;;; So here's the plan:
;;;;  NT-PRESERVE-PATHNAME-CASE will crawl up the directory tree and get the case
;;;;  preserved names of each of the components of the directory.
;;;;  When we expect to be calling nt-preserve-pathname-case on a *lot* of pathnames,
;;;;  like when we are processing a directory, we call CALL-WITH-DIRECTORY-CACHING.
;;;;  This function caches the results of calling directory on a pathname, and
;;;;  NT-PRESERVE-PATHNAME-CASE knows to look in the cache.
;;;;  The cache is flushed at the end of call-with-directory-caching, so it will not
;;;;  get too stale or too big.

;;;;  I've added some support for matching `FOOBAR~1'-like pathnames, but it is
;;;;  even uglier.

;(defvar *directory-cache* nil "Cache for calling the directory function.")

;(defun caching-directory (pathname)
;  (flet ((hard-way ()
;                (mapcar (lambda (pathname)
;                            (namestring
;                             (canonicalize-pathname pathname)))
;                        (cl:directory pathname))))
;  (if *directory-cache*
;    (let ((probe (gethash pathname *directory-cache*)))
;      (or probe
;          (setf (gethash pathname *directory-cache*) (hard-way))))
;    (hard-way))))

;(defun call-with-directory-caching (thunk)
;  (let ((*directory-cache* (make-hash-table :test #'equalp)))
;    (funcall thunk)))

;(defvar *nt-pathname-case-cache* (make-hash-table :test #'equalp :size 250))


;;; Guess what doesn't work in Franz....
;(defun %nt-preserve-pathname-case (pathname)
;  "Given an absolute pathname that must exist on the file system, return the pathname with
;   the case preserved as NT sees it."
;  (declare (type pathname pathname)
;           ; #+allegro (:explain :types :calls)
;           )
;  (if (root-pathname? pathname)
;      (make-pathname
;       :host     (canonicalize-pathname-element  (pathname-host pathname))
;       :device   (canonicalize-pathname-device (pathname-device pathname))
;       :directory (list :absolute)
;       :name      nil
;       :type      nil
;       :version   nil)
;    (let* ((parent (%nt-preserve-pathname-case (pathname-syntactic-parent pathname)))
;           ;; Rebuild the child with the parent's prefix.
;           (better-child-pathname (merge-pathnames
;                                   (if (file-pathname? pathname)
;                                       (make-pathname
;                                        :host nil
;                                        :device nil
;                                        :directory nil
;                                        :name (pathname-name pathname)
;                                        :type (pathname-type pathname)
;                                        :version (pathname-version pathname))
;                                     (make-pathname
;                                      :host nil
;                                      :device nil
;                                      :directory (list :relative (car (last (pathname-directory pathname))))
;                                      :name nil
;                                      :type nil
;                                      :version nil))
;                                   parent))
;           (better-child-namestring (namestring better-child-pathname))
;           ;; Find the canonical children of the parent.
;           (children (and parent (caching-directory parent)))
;           ;; Look for the child.
;           (element  (and children
;                          (car (or (member better-child-namestring children :test #'string-equal)
;                                   ;; If we didn't find it, we have one of those damned ~ names.
;                                   ;; Look for it, grab the number after it, then match on the
;                                   ;; letters before it.
;                                   (and (position #\~ better-child-namestring :from-end t)
;                                        (let* ((tilde (position #\~ better-child-namestring :from-end t))
;                                               (count (1- (the (integer 0 999)
;                                                            (parse-integer better-child-namestring
;                                                                           :start (1+ tilde) :junk-allowed t)))))
;                                          (nth-member count better-child-namestring children
;                                                      :test (lambda (child1 child2)
;                                                                (= (mismatch child1 child2 :test #'char-equal)
;                                                                   tilde))))))))))
;      (declare (type string better-child-namestring))
;      (if element
;          (parse-namestring element)
;        better-child-pathname))))

;(defun nt-preserve-pathname-case (pathname)
;  "Given an absolute pathname that must exist on the file system, return the pathname with
;   the case preserved as NT sees it."
;  (check-type pathname pathname)
;  (%nt-preserve-pathname-case pathname))

;;;; NOTE:  Should we just advise directory?
;;; Yes.  In fact....
;(defun better-directory (path)
;  "This is `better' than DIRECTORY because the pathnames returned are guaranteed to be
;directory pathnames when they point to directories."
;  (directory path))

;(defun better-probe-file (path)
;  (probe-file path))

(defgeneric decode-pathname (pathname)
  (:documentation "Given an object representing a mapped logical pathname, return a string representing
the decoded pathname.  This is for user convenience only, so do not use the result for
computation." )
  (:method ((pathname string))
    (decode-pathname (logical-pathname pathname)))

  (:method ((pathname logical-pathname))
    (namestring (make-pathname
                 :defaults ""
                                        ; no host or device components
                 :directory (canonicalize-pathname-directory
                             (and (consp (pathname-directory pathname))
                                  (cons (car (pathname-directory pathname))
                                        (mapcar #'decode-namestring
                                                (cdr (pathname-directory pathname))))))
                 :name (and (stringp (pathname-name pathname))
                            (decode-namestring (pathname-name pathname)))
                 :type (and (stringp (pathname-type pathname))
                            (decode-namestring (pathname-type pathname)))
                 :version (pathname-version pathname)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-wild-pathname (pathname)
  "Given a pathname (assumed to be that of a directory), return a wildcard
pathname which will match any pathname under that directlry."
  (make-pathname :name :wild
                 :type :wild
                 :directory (canonicalize-pathname-directory
                             (append
                              (pathname-directory pathname)
                              '(:wild-inferiors)))
                 :defaults pathname))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parsing a client pathname with respect to its OS type

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst find-directory-and-name-delimiter (string delimiters
                                                    &key (start 0)
                                                    end)
  (position-if (lambda (char)
                   (member char delimiters :test #'char-equal))
               string
               :start start
               :end (or end (string-length string))
               :from-end t))

(defun burst-directories (string delimiters &key (start 0) end)
  (declare (type array-index start)
           (type simple-string string))
  (let ((end (or end (string-length string)))
        (dirs nil))
    (flet ((pushdir (token)
             (cond ((string-equal token "."))
                   ;; Handle * ?
                   ((string-equal token "*")
                    (push :wild dirs))
                   ((string-equal token "**")
                    (push :wild-inferiors dirs))
                   ((string-equal token "..")
                    (if (or (null dirs)
                            (eq (car dirs) :back))
                        (push :back dirs)
                      (pop dirs)))
                   (t (push token dirs)))))
      (declare (type array-index end))
      (loop for index of-type array-index from start to end
            with token-start = start
            do
            (when (>= index end)
              (unless (= index token-start)
                (pushdir (subseq string token-start index)))
              (return))
            (when (member (schar string index) delimiters :test #'char-equal)
              (unless (= index token-start)
                (pushdir (subseq string token-start index)))
              (setq token-start (1+ index))))
      (nreverse dirs))))

#||
(burst-directories "/foo/bar/baz" '(#\/) :start 0 :end 8)  ==>  ("foo" "bar")
(burst-directories "/foo/bar/baz" '(#\/) :start 0 :end 9)  ==>  ("foo" "bar")
(burst-directories "/foo/bar/baz" '(#\/) :start 0 :end 10) ==>  ("foo" "bar" "b")
(burst-directories "/foo/bar/baz" '(#\/) :start 0 :end 11) ==>  ("foo" "bar" "ba")
(burst-directories "/foo/bar/baz" '(#\/) :start 0 :end 12) ==>  ("foo" "bar" "baz")
;; Ignore null components:
(burst-directories "/foo//bar/baz" '(#\/) :start 0 :end 13) ==> ("foo" "bar" "baz")
;; Handle ..
(burst-directories "../../foo/bar" '(#\/) :start 0 :end 13) ==> (:back :back "foo" "bar")
(burst-directories "foo/bar/../baz" '(#\/) :start 0 :end 14) ==> ("foo" "baz")
||#

(defun parse-filename.type (string &key (start 0) end (dot #\.)
                                        dot-dot-special-p)
  ;; Extract the name and type fields from the pathname namestring STRING.
  ;; Assumes they are separated by a single character DOT.  START and END
  ;; indicate a substring of STRING which bound the name and type.
  (declare (type simple-string string)
           (type array-index start ))
  (let* ((end1 (or end (string-length string)))
         (dot-pos (position dot string :start start :end end1 :from-end t)))
    (declare (type array-index end1))
    (cond ((= start end1) (values nil nil))
          ((and dot-dot-special-p
                (string-equal ".." string :start2 start :end2 end1))
           (values (subseq string start end1) nil))
          ((or (null dot-pos)
               (= dot-pos start))
           (if (string-equal ".*" string :start2 start :end2 end1)
               (values nil :wild)
               ;; No types.  Maybe a leading dot in the name
               (values (if (string-equal "*" string :start2 start :end2 end1)
                           :wild
                           (subseq string start end1))
                       nil)))
          (t
           (values (if (string-equal "*" string :start2 start :end2 dot-pos)
                       :wild
                       (subseq string start dot-pos))
                   (if (string-equal "*" string :start2 (1+ dot-pos) :end2 end1)
                       :wild
                       (subseq string (1+ dot-pos) end1)))))))

(defun parse-dos-namestring (namestring &key (start 0) end)
  "Attempt to parse namestring as a DOS filename
   returning 5 values:
   0  T if the parse was successful, NIL if not.
      All other values are NIL on failure.
   1  The drive letter, or NIL if absent.
   2  A list of directory components (if present).
   3  If the filename ends in \, NIL, otherwise everything up
      to the last dot.
   4  If the filename ends in \, NIL, otherwise everything after
      the last dot (if present).
   5  The NTFS stream name, if present."
  ;; Note, DOS namestrings may not use
  ;; #\\, #\/, #\:, #\*, #\?, #\<, #\>, or #\|
  ;; in their components.
  ;; Note, too, that NTFS pathnames may not end in a dot.

  ;; First, check for a drive.
  ;; The drive is a single alpha-character followed by a :
  ;; followed by a forward or back slash.
  (let* ((end1  (or end (string-length namestring)))
         (has-drive (and (> (- end1 start) 3)
                        (char= (char namestring (+ start 1)) #\:))))

    ;; It appears to be a drive, require the slash.
    (when has-drive
      (unless (and (alpha-char-p (char namestring (+ start 0)))
                   (or (char= (char namestring (+ start 2)) #\\)
                       (char= (char namestring (+ start 2)) #\/)))
        (return-from parse-dos-namestring nil)))

    (let* ((dir-start (if has-drive (+ start 2) start))
           (dir-end (find-directory-and-name-delimiter namestring (list #\\ #\/)
                                                       :start dir-start
                                                       :end end1))

           (directory
            (when dir-end
              (canonicalize-pathname-directory
               (cons (if (member (schar namestring dir-start) (list #\\ #\/) :test #'char-equal)
                         :absolute
                         :relative)
                     (burst-directories namestring (list #\\ #\/)
                                        :start dir-start
                                        :end dir-end)))))
           (name-end (position #\: namestring :start (or dir-end
                                                         dir-start)
                               :end end1))
           (stream-name (when name-end (subseq namestring (1+ name-end) end1))))

      (multiple-value-bind (name type)
          (if (or (null dir-end)
                  (< dir-end end1))
              (parse-filename.type namestring
                                   :start (if dir-end (1+ dir-end) start)
                                   :end (or name-end end1)
                                   :dot-dot-special-p t)
              (values nil nil))

        (labels ((valid-component? (component)
                   (and (notany (lambda (char)
                                  (member char '(#\\ #\/ #\: #\* #\? #\< #\> #\|)
                                          :test #'char=))
                                component)
                        (not (char= (char component (1- (string-length component)))
                                    #\.)))))
          (if (and (every (lambda (component)
                            (or (not (stringp component))
                                (valid-component? component)))
                          directory)
                   (or (null name) (eq name :wild) (valid-component? name))
                   (or (null type) (eq type :wild) (and (stringp type)
                                                        (not (zerop (string-length type)))
                                                        (valid-component? type)))
                   (or (null stream-name)
                       ;; Allow stream names such as :$DATA
                       (notany (lambda (char)
                                 (member char '(#\\ #\/ #\* #\? #\< #\> #\|)
                                         :test #'char=))
                               stream-name)))
              (values t
                      (when has-drive (char namestring 0))
                      directory
                      name
                      type
                      stream-name)
              (values nil)))))))

(defun parse-delimited-pathname (namestring delimiters start host device
                                 &optional name-is-directory-p)
  ;; If NAME-IS-DIRECTORY-P then the component after the last
  ;; delimiter is taken as the last element of the directory list,
  ;; rather than as the name and type.
  (declare (type simple-string namestring))
  (let* ((dir-end (if name-is-directory-p
                      (string-length namestring)
                    (find-directory-and-name-delimiter namestring delimiters)))
         (directory (when (or name-is-directory-p dir-end)
                      (cons (if (member (schar namestring start) delimiters :test #'char-equal)
                                :absolute
                              :relative)
                            (burst-directories namestring delimiters
                                               :start start
                                               :end dir-end)))))
    (multiple-value-bind (name type)
        (if name-is-directory-p
            (values nil nil)
          (parse-filename.type namestring
                               :start (if dir-end (1+ dir-end) start)
                               :dot-dot-special-p t))
      (make-pathname :host host
                     ;; 1999-10-29 [naha]: Under unix, ACL gives an
                     ;; error if you specify a :DEVICE.  Fortunately,
                     ;; leaving it null doesn't seem to break things.

                     ;; 2000-01-25 [jrm]: Kludged it so ACL isn't
                     ;; brain damaged.

                     :device device
                     :directory (canonicalize-pathname-directory directory)
                     :name name
                     :type type))))

#||
(parse-delimited-pathname "/foo/bar/baz.bam" '(#\/) 0 nil nil nil)
#p"\\foo\\bar\\baz.bam"

(parse-delimited-pathname "/foo/bar/baz.bam" '(#\/) 0 nil nil t)
#p"\\foo\\bar\\baz.bam\\"
||#

(defsubst parse-delimited-directory-pathname (namestring delimiters start host device)
  (parse-delimited-pathname namestring delimiters start host device t))

(defun make-logical-pathname (&key directory name type version)
  "Given DIRECTORY, NAME, TYPE, and/or VERSION components, construct
   a LOGICAL-PATHNAME with logical host REPOSITORY.  All components
   are mapped (encoded) to make a legal logical pathname."
  (make-pathname :host "REPOSITORY"
                 :directory (let ((directory (canonicalize-pathname-directory directory)))
                              (unless (null directory)
                                (cons (car directory)
                                      (mapcar
                                       (lambda (component)
                                           (if (stringp component)
                                               (encode-namestring component)
                                             component))
                                       (cdr directory)))))
                 :name (if (stringp name)
                           (encode-namestring name)
                         name)
                 :type (if (stringp type)
                           (encode-namestring type)
                         type)
                 :version (if (stringp version)
                              (encode-namestring version)
                            version)
                 :defaults ""))

(defun logical-pathname-parent-directory (logical-directory-pathname)
  "Given a logical-directory-pathname, return a LOGICAL-DIRECTORY-PATHNAME
   that names the immediate parent directory, or NIL if there is no parent."
  ;; This implementation is not particular to logical pathnames.  Why
  ;; does the name indicate that it is?
  ;; Because the mechanism used to generate the parent is a simple
  ;; syntactic transformation, whereas in a real file system it would
  ;; be necessary to ask the file-system-agent to walk one level up
  ;; the hierarchy (because of links and other crap).  A simple syntactic
  ;; transformation may not be correct.

  ;; Now I'm wondering how this differs from PATHNAME-SYNTACTIC-PARENT defined above.
  (check-type logical-directory-pathname directory-pathname)
  (unless (null (cdr (pathname-directory logical-directory-pathname)))
    (make-pathname :host (pathname-host logical-directory-pathname)
                   ;; bogus, but necessary
                   :device (pathname-device logical-directory-pathname) ;ugh!
                   :directory (canonicalize-pathname-directory
                               (butlast (pathname-directory logical-directory-pathname)))
                   :name nil
                   :type nil
                   :version nil)))

(defun pathname->logical-pathname (pathname)
  "Given PATHNAME, return a logical-pathname.  This is done by
   creating a new pathname with the HOST component set
   to the logical host REPOSITORY and mapping all the components
   to legal logical pathname values."
  (assert (member (pathname-device pathname) '(nil :unspecific)))
  (assert (member (pathname-host pathname) '(nil :unspecific)))
  (make-logical-pathname
   :directory (canonicalize-pathname-directory (pathname-directory pathname))
   :name (pathname-name pathname)
   :type (pathname-type pathname)
   :version (pathname-version pathname)))

(defun compare-directories (ldir rdir)
  (cond ((eq ldir rdir) nil)
        ((not (consp rdir)) :less)
        ((not (consp ldir)) :greater)
        ((eq (car ldir) (car rdir))
         (do ((ltail (cdr ldir) (cdr ltail))
              (rtail (cdr rdir) (cdr rtail)))
             ((not (and ltail
                        rtail
                        (string-equal (car rtail) (car ltail))))
              (cond ((null ltail) (if (null rtail) nil :less))
                    ((null rtail) :greater)
                    ((string-lessp (car ltail) (car rtail)) :less)
                    (t :greater)))))
        ((eq (car ldir) :absolute) nil)
        ((eq (car rdir) :absolute) t)
        (t (error "Cannot compare directories ~s and ~s" ldir rdir))))

(defun compare-names (lname rname)
  (cond ((eq lname rname) nil)
        ((not (stringp lname)) :greater)
        ((not (stringp rname)) :less)
        ((string-equal lname rname) nil)
        ((string-lessp lname rname) :less)
        (t :greater)))

(defun compare-logical-pathnames (left right)
  "Return :GREATER, NIL or :LESS depending if left is greater, equal or less
   than right lexicographically."
  (or (compare-directories (pathname-directory left) (pathname-directory right))
      (compare-names (pathname-name left) (pathname-name right))
      (compare-names (pathname-type left) (pathname-type right))))

(defun logical-pathname-less-p (left right)
  "Return T iff LEFT is lexicographically less than RIGHT.
   Equivalent to (string-lessp (namestring left) (namestring right)), but faster."
  (eq (compare-logical-pathnames left right) :less))

(defun pathname->encoded-logical-pathname (pathname)
  "Given PATHNAME, which is unencoded (i.e. was NOT created with make-logical-pathname
   or relativize-pathname), return a logical-pathname which has been encoded as if by MAKE-LOGICAL-PATHNAME."
  (make-logical-pathname
   :directory (pathname-directory pathname)
   :name (pathname-name pathname)
   :type (pathname-type pathname)
   :version (pathname-version pathname)))

(defun directory-is-prefix? (directory-pathname file-pathname)
  "Returns T iff all the directory elements in DIRECTORY-PATHNAME
   match the corresponding elements in FILE-PATHNAME.  In other
   words, FILE-PATHNMAME is at or below DIRECTORY-PATHNAME in
   the hierarchy."
  (check-type directory-pathname directory-pathname)
  ;;(guarantee-file-pathname file-pathname)
  (let ((mm (mismatch (pathname-directory directory-pathname)
                      (pathname-directory file-pathname)
                      :test (lambda (a b)
                              (cond ((stringp a) (when (stringp b) (string-equal a b) ))
                                    ((stringp b) nil)
                                    ((eq a b) t)
                                    (t nil))))))
    (if mm
        (= mm (length (pathname-directory directory-pathname)))
        t)))

(defun enough-pathname (pathname root-directory)
  "Returns an pathname that is just sufficient to identify the file named by pathname
   when considered relative to the root-directory. It is required that

  (merge-pathnames (enough-pathname pathname defaults) defaults) == pathname

  in all cases, and the result of enough-pathname is the shortest reasonable pathname
  (in terms of directory components) that will satisfy this criterion.
  If pathname is not below root-directory, the pathname is simply returned."

  ;; Make sure host and device, if present, are equal.
  (if (or (and (pathname-host pathname)
               (stringp (pathname-host pathname))
               (not (and (pathname-host root-directory)
                         (stringp (pathname-host root-directory))
                         (string-equal (pathname-host pathname)
                                       (pathname-host root-directory)))))
          (and (pathname-device pathname)
               (stringp (pathname-device pathname))
               (not (and (pathname-device root-directory)
                         (stringp (pathname-device root-directory))
                         (string-equal (pathname-device pathname)
                                       (pathname-device root-directory))))))
      pathname
      (make-pathname
       ;; Necessary to keep logical pathnames logical.
       :host (when (logical-pathname-p pathname) (pathname-host pathname))
       :directory
       (canonicalize-pathname-directory
        (cons :RELATIVE
              (named-let loup ((pathname-dir-tail (cdr (pathname-directory pathname)))
                               (root-dir-tail (cdr (pathname-directory root-directory))))
                   (cond ((null root-dir-tail) pathname-dir-tail)
                         ((null pathname-dir-tail)
                          (return-from enough-pathname pathname))
                         ((string-equal (car pathname-dir-tail) (car root-dir-tail))
                          (loup (cdr pathname-dir-tail) (cdr root-dir-tail)))
                         (t (return-from enough-pathname pathname))))))
       :name (pathname-name pathname)
       :type (pathname-type pathname)
       :version (pathname-version pathname))))

(defun pathname-match-p-fold-case (pathname wildcard)
  (labels ((fold (thing)
            (if (stringp thing)
                (string-upcase thing)
                thing))

           (fold-list (thing)
             (if (consp thing)
                 (mapcar #'fold thing)
                 thing)))

    (pathname-match-p
     (make-pathname :host   (canonicalize-pathname-element (pathname-host pathname))
                    :device (canonicalize-pathname-element (pathname-device pathname))
                    :directory (fold-list (canonicalize-pathname-directory (pathname-directory pathname)))
                    :name      (fold (canonicalize-pathname-element (pathname-name pathname)))
                    :type      (fold (canonicalize-pathname-element (pathname-type pathname)))
                    :version   (fold (canonicalize-pathname-element (pathname-version pathname))))
     (make-pathname :host   (canonicalize-pathname-element (pathname-host wildcard))
                    :device (canonicalize-pathname-element (pathname-device wildcard))
                    :directory (fold-list (canonicalize-pathname-directory (pathname-directory wildcard)))
                    :name      (fold (canonicalize-pathname-element (pathname-name wildcard)))
                    :type      (fold (canonicalize-pathname-element (pathname-type wildcard)))
                    :version   (fold (canonicalize-pathname-element (pathname-version wildcard)))))))

(defun neutralize-pathname (pathname directory)
  "Given PATHNAME and DIRECTORY, produces a
   repository relative logical pathname assuming the pathname is
   accessible from the directory."
  (let ((enough
         (guarantee-relative-pathname
          (enough-pathname pathname directory))))
    (make-logical-pathname
     :directory (canonicalize-pathname-directory (pathname-directory enough))
     :name (pathname-name enough)
     :type (pathname-type enough)
     :version (pathname-version enough))))

(defun push-pathname-type (pathname new-type)
  "Given a pathname and an extension, create a new pathname by
   sticking the current extension into the name.  So
   (PUSH-PATHNAME-EXTENSION #p\"foo.txt\" \"bar\") will return
   #p\"foo.txt.bar\""
  (make-pathname
   :host      (canonicalize-pathname-element (pathname-host pathname))
   :device    (canonicalize-pathname-device  (pathname-device pathname))
   :directory (canonicalize-pathname-directory (pathname-directory pathname))
   :name      (if (canonicalize-pathname-element (pathname-type pathname))
                  ;; If there is a type, concat it with the name and a period.
                  (if (logical-pathname-p pathname)
                      (encode-namestring
                       (concatenate 'string
                         (decode-namestring
                          (or (canonicalize-pathname-element (pathname-name pathname))
                              ""))
                         "."
                         (decode-namestring
                          (canonicalize-pathname-element (pathname-type pathname)))))
                    (concatenate 'string
                      (or (canonicalize-pathname-element (pathname-name pathname))
                          "")
                      "."
                      (canonicalize-pathname-element (pathname-type pathname))))
                ;; If there is currently no type, there is nothing to push.
                (canonicalize-pathname-element (pathname-name pathname)))
   :type       (if (logical-pathname-p pathname)
                   (encode-namestring new-type)
                 new-type)
   :version    (canonicalize-pathname-element (pathname-version pathname))))

(defun test-pathname-hacks ()
  (format t "~@{~%~s~}"
          ;; Funky format string means print each element on separate lines.
          (enough-pathname #p"C:\\foo\\bar\\baz\\quux.txt" #p"C:\\foo\\bar\\")
          (enough-pathname #p"\\foo\\bar\\baz\\quux.txt" #p"\\foo\\bar\\")
          (enough-pathname #p"\\foo\\bar\\baz\\quux.txt" #p"\\foo\\baz\\")
          ;; Should return "file8.txt", NOT ".\\file8.txt"
          (enough-pathname #p"C:\\TEMP\\tvw9e767\\__akrondir\\__foo\\__src\\file8.txt"
                           #p"C:\\TEMP\\tvw9e767\\__akrondir\\__foo\\__src\\")
          (let ((file (translate-logical-pathname #p"TS50:TS50;FOO;BAR;QUUX;BAZ.TXT"))
                (dir  (translate-logical-pathname #p"TS50:TS50;FOO;BAR;")))
            (list file dir
                  (equal (merge-pathnames (enough-pathname file dir) dir) file)))
          ))

(defun merge-pathnames-including-host-and-device (pathname defaults)
  "Like MERGE-PATHNAME, but unlike the ACL implementation, this is
   guaranteed to merge the HOST and DEVICE fields as well."
  (let ((merged (merge-pathnames pathname defaults)))
    (macrolet ((merge-expression (accessor)
                 `(if (member (,accessor pathname) '(nil :unspecific))
                      (,accessor defaults)
                    (,accessor pathname))))
      ;; Don't need to do anything special for MSWindows because ACL
      ;; supports both HOST and DEVICE there.
      #+UNIX
      (setq merged
        (make-pathname :defaults merged
                       :device (merge-expression pathname-device)
                       :host (merge-expression pathname-host))))
    merged))

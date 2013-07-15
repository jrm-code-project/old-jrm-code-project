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
;;;
;;; File Name:     replacement-macros.lsp
;;; Author:        jrm
;;; Creation Date: Oct 2000
;;;
;;; Module Description:  Replacements for standard ACL functions that Franz
;;;                      incorrectly implemented.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CSF/UTILITY")

;;; This function is too often misused.
;;; By requiring the second argument the user is forced to
;;; think about what he is doing.
(defun enough-namestring (absolute-namestring base-namestring)
  "Return a namestring long enough to identify absolute-namestring given base-namestring
   as a starting point.

   This differs from the standard CommonLisp version in that the base-namestring
   is required rather than optional.

   You should not use the current directory as the base-namestring because it
   will vary, and thus cause regression diffs."
  (cl:enough-namestring absolute-namestring base-namestring))

#+lispworks
(defun subtypep (type-1 type-2 &optional environment)
  (declare (ignore environment))
  (cl:subtypep type-1 type-2))

;;; NEEDS WORK!
#+lispworks
(defun nconc (&rest lists)
  (apply #'cl:nconc lists))
;  (if (null (car lists))
;      (apply #'nconc (cdr lists))
;      (let ((result (first lists)))
;        (labels ((outer-loop (list lists)
;                             (inner-loop list (car lists))
;                             (when (cdr lists)
;                               (outer-loop (car lists) (cdr lists))))

;                 (inner-loop (left right)
;                             (if (consp left)
;                                 (if (consp (cdr left))
;                                     (inner-loop (cdr left) right)
;                                     (progn (setf (cdr left) right)
;                                            left))
;                                 right)))
;          (when (cdr lists)
;            (outer-loop (car lists) (cdr lists))))
;        result)))

;;; This function seems to require some extra work on winnt.
;;; It seems that files are not immediately closed when a process
;;; exits.  This waits a bit before giving up.  Yuck.

(defvar *delete-file-max-retries* 10
  "The maximum number of times delete-file will attempt to delete the file
   before it assumes that the file is stuck open.")

(defvar *delete-file-relaxation-interval* 0.2
  "The amount of time in seconds that delete file will wait between
   attempts to delete a file that seems to be open.")

#+(and allegro mswindows (not dont-fix-franz))
(defun delete-file (name)
  (do ((retry-count 0 (1+ retry-count)))
      ((block delete-file-retry
         (handler-bind
             ((file-error
               (lambda (condition)
                   (declare (:fbound os-file-read-only?))
                   ;; If the file handle is open, we cannot delete the file.
                   ;; Under windows, this manifests itself as an ERROR 13 to
                   ;; the underlying C code.  The errno is captured within the
                   ;; condition object.
                   ;;
                   ;; So if the condition object has an errno, that errno is 13,
                   ;; the file is not read-only, and we haven't exceeded the
                   ;; retry count, we sleep for a bit and try again.
                   (when (and (excl::file-error-errno condition)
                              (= (excl::file-error-errno condition) 13)
                              (not (os-file-read-only? name))
                              (< retry-count *delete-file-max-retries*))
                     (debug-message 2 "File delete failure on ~s, retry." name)
                     (mp:process-sleep *delete-file-relaxation-interval* "Waiting for file to close.")
                     (return-from delete-file-retry nil)))))
           (cl:delete-file name)))
       t)))

#+(and allegro mswindows (not dont-fix-franz))
(defun directory (path &rest keyword-args)
  "This is `better' than DIRECTORY because the pathnames returned are guaranteed to be
   directory pathnames when they point to directories."
  #+allegro (declare (:fbound call-with-directory-caching canonicalize-pathname #+mswindows nt-preserve-pathname-case))
  (call-with-directory-caching
   (lambda ()
       (mapcar (lambda (file)
                   #+mswindows (nt-preserve-pathname-case (canonicalize-pathname file))
                   #-mswindows (canonicalize-pathname file)
                   )
               (cl:directory path)))))

#+(and allegro (not dont-fix-franz))
(defun probe-file (filespec)
  (declare (:fbound relative-pathname? #+mswindows nt-preserve-pathname-case))
  (cond ((stringp filespec) (probe-file (parse-namestring filespec)))
        ((relative-pathname? filespec) (probe-file (merge-pathnames filespec)))
        (t  #-mswindows (cl:probe-file filespec)
           #+mswindows (let ((answer (cl:probe-file filespec)))
                         (when answer (nt-preserve-pathname-case answer))))))

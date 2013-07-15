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
;;;; File Name:     tags.lsp
;;;; Author:        jrm
;;;; Creation Date: 23-Oct-2000
;;;;
;;;; Module Description:
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CSF/UTILITY")

;; Build a TAGS file for Emacs.
;; To use this, put the following in your .clinit.cl file:
;;
;; (if (not (boundp '*ts-load-finish-hooks*))
;;     (setq *ts-load-finish-hooks* nil))
;;
;; (push (lambda (&key)
;;           (load #p"TS50:BUILD;emacs-tags.lsp")
;;           (package-lossage-funcall "UTILITY" "MAYBE-BUILD-TAGS-FILE" :force-p nil))
;;       *ts-load-finish-hooks*)

#||
;;; *** The above is old.  Try this
(load (merge-pathnames "build/emacs-tags.lsp"
      changesafe::*changesafe-source-directory*))
||#

(defun user::ts-source-directory-pathname (&optional pn)
  (if pn
      (merge-pathnames pn changesafe::*changesafe-source-directory*)
    changesafe::*changesafe-source-directory*))

(defun pathnames-of-interest ()
  (flet ((files-of-type (type)
	   (directory
            (merge-pathnames (make-pathname :name :wild
                                            :directory '(:relative :wild-inferiors)
                                            :type type)
                             changesafe::*changesafe-source-directory*))))
    (mapcan #'files-of-type '("cl" "lisp" "lsp" "java"))))

(defun maybe-build-tags-file (&key (force-p t))
  (let ((etags-program (car (search-for-executable "etags")))
	(current-tags-file (probe-file (user::ts-source-directory-pathname "TAGS"))))
    (when (and etags-program		; if we can generate a tags file
	       (or force-p
		   (null current-tags-file)
		   ;; File is over one week old.
		   (< (+ (file-write-date current-tags-file)
			 (* 60		; secs per min
			    60		; mins per hour
			    24		; hours per day
			    7))		; days per week
		      (get-universal-time))))

      (with-new-current-directory (user::ts-source-directory-pathname)
	(format t "~&Attempting to update TAGS file...")
	(run-subprocess (format nil "~a~{ ~a~}"
				(car (search-for-executable "etags"))
				(mapcar #'cl:enough-namestring (pathnames-of-interest))))))))

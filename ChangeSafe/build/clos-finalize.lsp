;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;	      Copyright (c) 2000 Content Integrity, Inc.
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
;;;; File Name:	    clos-finalize.lsp
;;;; Author:        Mark Nahabedian
;;;; Creation Date: 2000-07-20
;;;;
;;;; Module Description:
;;;;
;;;; As the last step in loading ts50, we finalize the inheritance of
;;;; any classes that were defined.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :user)

;;; API's exported from this module.
(eval-when (:load-toplevel :execute)
  (export '()))

(defun ts50-packages ()
  "Returns a list of all CommonLisp packages defined by ts50."
  (cond ((and (boundp '*ts50-packages*) *ts50-packages*) *ts50-packages*)
	((and (boundp '*non-ts50-packages*) *non-ts50-packages*)
	 (set-difference (list-all-packages) *non-ts50-packages*))
	(t
	 (mapcar #'find-package '("UTILITY" "CORE" "VM" "RFM" "SERVER" "CONMAN")))))

(defun clos-finalize-ts50-class-inheritance (&key verbose &allow-other-keys)
  "Finalize the CLOS inheritcance of all classes defined in ts50.
   Steve Jacobson of Franz believes this will help to avoid spurious
   ALLEGROSTORE::ERR_WRITE_DURING_QUERY errors."
  (cond ((member :allegrostore *features*)
	 (let ((total-count 0))
	   (dolist (pkg (list-all-packages))
	     (let ((count 0))
	       (do-symbols (symb pkg)
		 (when (eq pkg (symbol-package symb))
		   (let ((class (find-class symb nil)))
		     (when (and class
				(typep class 'astore:persistent-standard-class))
		       (when verbose
			 (format t "~&   Finalizing inheritance for class ~s" symb))
		       (incf count)
		       (clos:finalize-inheritance class)))))
	       (when verbose
		 (format t "~&~d classes finalized in package ~a."
			 count (package-name pkg)))
	       (incf total-count count)))
	   (when verbose
	     (format t "~&~d classes finalized in total.~%" total-count))))
	(verbose (format t ";~&Not finalizing classes because allegrostore is not loaded."))))

;;; load.lsp now calls clos-finalize-ts50-class-inheritance
;;; just before the final GC.
;(eval-when (:load-toplevel :execute)
;  (pushnew 'clos-finalize-ts50-class-inheritance
;	   *ts-load-finish-hooks*))

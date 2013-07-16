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
;;;;
;;;; File Name:     named-object.lsp
;;;; Author:        Dave Tenny
;;;;
;;;; Module Description:
;;;;
;;;; A NAMED-OBJECT provides for a versioned name for an object (which
;;;; is expected to change over time). Not all versioned objects are
;;;; described objects.  By putting this into a mixin, we avoid having
;;;; to embed a scalar versioned name slot into many classes, along
;;;; with method overloads for object-change-interesting-p, object-user-name,
;;;; and a few other things which always seem to accompany named objects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CSF/VERSION-MANAGEMENT")

(proclaim (standard-optimizations))

(eval-when (:load-toplevel :execute)
  (export '(named-object
            named-object/name
            named-object/historical-names
            #||
            named-object-search-all-names
            named-object-initialize
            ||#
            )))

;;; Name slot accessed via OBJECT-USER-NAME, UPDATE-OBJECT-USER-NAME

(defclass named-object ()
  ((name :initarg :name
         :initform ""
         :version-technique :scalar
         :reader named-object/name))
  (:documentation "Mixin for any object which merits a per-instance and changeable name.")
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defmethod print-object ((object named-object) stream)
  (print-unreadable-object (object stream :type t)
    (multiple-value-bind (name errorp)
        (ignore-errors (named-object/name object))
      (unless errorp
        (prin1 name stream)))))

(defun named-object/historical-names (named-object)
  "Return a list of every name this object ever had.
   EXPENSIVE OPERATION"
  (declare (ignore named-object))
  (error "homey don't do this yet."))

#||
(defsubst named-object-initialize (named-object name)
  "Initialize the NAME slot of a named object."
  (set-named-object-name named-object name)
  name)

(defmethod object-user-name ((named-object named-object))
  (named-object-name named-object))

(defmethod update-object-user-name ((named-object named-object) (new-name string))
  (set-named-object-name named-object new-name))

(defmethod object-change-interesting-p or ((named-object named-object) (slot-name symbol) birth-cid-p)
  (and (not birth-cid-p)
       (eq slot-name 'name)))

(defmethod object-change-user-semantics or ((named-object named-object) (slot-name symbol)
                                            change-type valid-p new-or-old old)
  (declare (ignore change-type valid-p new-or-old old))
  (and (eq slot-name 'name)
       "change name"))

(defun named-object-search-all-names (named-object for-name &key (test #'string=))
  "Ignore the versioned cid-set context and search all history values of the named object for
   FOR-NAME, using the TEST comparison predicate which defaults to STRING=.

   Return FOR-NAME if there is a match, NIL otherwise."
  (loop for cid in (versioned-object-slot-cids named-object 'name)
        do (versioned-object-map-changed-slot
            named-object 'name cid
            (lambda (action valid-p value replaced-value);action is always :CHANGE for scalar slots
                (declare (ignore valid-p replaced-value))
                (assert (eq action :change))
                (when (funcall test value for-name)
                  (return-from named-object-search-all-names for-name)))))) ;NIL returned if this not hit
||#


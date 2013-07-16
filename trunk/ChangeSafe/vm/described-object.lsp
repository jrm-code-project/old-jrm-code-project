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
;;;; File Name:     described-object.lsp
;;;; Author:        Dave Tenny
;;;;
;;;; Module Description:
;;;;
;;;; A DESCRIBED-OBJECT provides for descriptive text that users may attach
;;;; to versioned objects.  Not all versioned objects are described objects,
;;;; but just about anything which conceivably merits a desription on its
;;;; use and existence is a candidate.
;;;;
;;;; Long term, described objects may have attached (versioned!) file contents,
;;;; or simply a (versioned) string.  Only the latter is implemented
;;;; at this time.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CSF/VERSION-MANAGEMENT")

(proclaim (standard-optimizations))

(eval-when (:load-toplevel :execute)
  (export '(described-object
            described-object/description
            #||
            described-object-text
            set-described-object-text
            ||#
            )))

(defclass described-object ()
  ((text :initarg :description
         :initform ""
         :accessor described-object/description
         :version-technique :scalar))
  (:documentation ("Mixin for any object which merits potential descriptive text on a per-instance basis."))
  (:metaclass versioned-standard-class)
  (:schema-version 0))

#||
(defmethod object-change-interesting-p or ((object described-object) (slot-name symbol) birth-cid-p)
  ;; Hmmm, maybe the lispy thing to do is use an (EQL) method arg discriminator...  Ah well...
  (and (not birth-cid-p)
       (eq slot-name 'text)))           ;changes to text slot of a described-object are interesting

(defmethod object-change-user-semantics or ((object described-object) (slot-name symbol)
                                            change-type valid-p new-or-old old)
  (declare (ignore change-type valid-p new-or-old old))
  (and (eq slot-name 'text)
       "change description"))
||#

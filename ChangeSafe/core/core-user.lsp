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
;;;;
;;;; File Name:     core-user.lsp
;;;; Author:        Dave Tenny
;;;;
;;;; Module Description:
;;;;
;;;; A CORE-USER object is required for all repository transactions.
;;;; The repository currently does nothing in the form of security checks
;;;; w.r.t. CORE-USER objects, but it does require them in order to view and update
;;;; repository content.  Security is handled in models implemented outside
;;;; the CORE package.
;;;;
;;;; CORE-USER objects are meant to be subclassed versioned objects.
;;;; This way various models can define their own user behaviors and
;;;; properties.  The core package assumes only a versioned user name
;;;; property, and the distributed object capabilities inherent in all
;;;; versioned objects.
;;;;
;;;; This class/module was named 'CORE-USER' instead of 'USER' because
;;;; I've had difficulties with lisps before, which were fond of having their
;;;; own USER objects, USER-NAME functions, etc.  (and the USER package,
;;;; of course).  So this way we hopefully avoid symbol conflicts.
;;;;
;;;; Note that CORE-USER object references are often referred to in this
;;;; package as UIDs (short for user id's).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CSF/CORE")

(proclaim (standard-optimizations))

(eval-when (:load-toplevel :execute)
  (export '(core-user
            core-user/name)))

(defclass core-user (distributed-object)
  ((name :initarg :name
         :initform (error "Required initarg :name omitted.")
         :type string
         :reader core-user/name
         :version-technique :nonversioned))
  (:documentation "User identity in repository transactions, see CORE-USER module for details.")
  (:metaclass versioned-standard-class)
  (:schema-version 0))

#||
(defun core-user? (object)
  "Return true if OBJECT is of type CORE-USER.  Useful as a function since TYPEP
   can't be compiled in some modes if the code being compiled is referencing CORE-USER
   before its class definition is encountered."
  (typep object 'core-user))

(defun core-user-initialize (core-user name)
  "Initialize the NAME slot of a CORE-USER object.
   NAME must be a string, and is returned by the function."
  (check-type name string)
  (set-core-user-name core-user name)
  name)

(defun core-user-create (name)
  "Create and return a CORE-USER object.
  It is recommended that this function be used only in the CORE package,
  and that other packages subclass the CORE-USER object and call CORE-USER-INITIALIZE instead.

  NAME must be a string which encodes a 'user name', whatever that is (whatever you want it to be)."
  (let ((user (make-instance 'core-user)))
    (core-user-initialize user name)
    user))
||#
;;;; We do not defined object-user-name or update-object-user-name semantics here.
;;;; we leave that for subtypes, since that particular protocol is part of the VM package.

;;;; We may later wish to add method protocols which are overloaded for things like
;;;; CORE-USER-AUTHORIZED-FOR-UPDATE, *-READ, CORE-USER-AUTHORIZED-FOR-ACCESS (object access-type), etc..
;;;; to control access to repository resources or specific types of objects.  *TBD*

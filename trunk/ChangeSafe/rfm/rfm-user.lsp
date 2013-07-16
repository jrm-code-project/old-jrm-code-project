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

(in-package "CSF/REPOSITORY-FILE-MANAGEMENT")

(proclaim (standard-optimizations))

(eval-when (:load-toplevel :execute)
  (export '(rfm-user)))

(defclass rfm-user (core-user described-object)
  ((power :initarg  :power
          :initform (error "Required initarg :power omitted.")
          :accessor   rfm-user/power
          :version-technique :nonversioned)
   (password :initarg :password
             :initform (error "Required initarg :password omitted.")
             :accessor rfm-user/password
             :version-technique nil))
  (:documentation)
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defmacro rfm-user/name (rfm-user)
  `(CORE-USER/NAME ,rfm-user))

(defmethod print-object ((ru rfm-user) stream)
  (print-unreadable-object (ru stream :type t)
    (format stream "~s" (rfm-user/name ru))))

(defclass rfm-user-info ()
  ((user-name :initarg :user-name
              :initform (error "Required initarg :user-name omitted.")
              :type string)
   (user-did  :initarg :user-did
              :initform (error "Required initarg :user-did omitted.")
              :type distributed-identifier)
   (power     :initarg :power
              :initform nil))
  (:documentation "Transient information for the user associated with this request."))

(defun rfm-user->rfm-user-info (rfm-user)
  (make-instance 'rfm-user-info
                 :user-name (rfm-user/name rfm-user)
                 :user-did  (distributed-object-identifier rfm-user)
                 :power     (rfm-user/power rfm-user)))


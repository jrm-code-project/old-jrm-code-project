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

(in-package "CSF/UTILITY")

(eval-when (:compile-toplevel :load-toplevel)
  (export '(
            hatcheck/save
            hatcheck/retrieve
            )))

;; A hatcheck table is a table where you can put in a
;; object and get a unique ticket.  Later on, you can
;; take the ticket and get the object.  The tickets are
;; guaranteed to not collide, but the objects don't last
;; beyond the lisp invocation.  Retrieving an object
;; removes it from the table.  

;; this is primarily used to hold on to object while
;; redirecting the browser to a different page that
;; will be needing the object.

(defconstant *hatcheck-table*
  (load-time-value (make-hash-table :test #'eq)))

(defun hatcheck/save (object &optional (ticket (generate-guid)))
  (setf (gethash ticket *hatcheck-table*) object)
  ticket)

(defun hatcheck/retrieve (ticket)
  (prog1 (gethash ticket *hatcheck-table*)
    (remhash ticket *hatcheck-table*)))

(defun hatcheck/%peek (ticket)
  "Peeks at the ticket value, but does not remove it
   from the table.  FOR DEBUGGING ONLY."
  (gethash ticket *hatcheck-table*))

;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright (c) 2002 ChangeSafe, LLC
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

(proclaim (standard-optimizations))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(
            command-line-arguments
            command-line-argument
            gsgc-parameter
            reap-os-subprocess
            )))

(defunimplemented get-command-line-argument ())

(defun get-command-line-arguments ()
  "Return the list of arguments from the command line."
  (macrolet ((code ()
               (flet ((featurep (feature)
                        (member feature *features*)))
                 (cond ((featurep :allegro)
                        `(sys:command-line-arguments :application t))
                       ((featurep :lispworks)
                        `sys:*line-arguments-list*)
                       (t (error "Don't know how to get command argument list for this platform"))))))
    (code)))

(eval-when (:load-toplevel :execute)
  (let ((lw:*handle-warn-on-redefinition* :quiet))
    (setf (symbol-function 'sys:command-line-arguments)
          (symbol-function 'get-command-line-arguments)
          (symbol-function 'sys:command-line-argument)
          (symbol-function 'get-command-line-argument))))

(defun gsgc-parameter (name)
  (declare (ignore name))
  "Not available.")

(defunimplemented reap-os-subprocess ())

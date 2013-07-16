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
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Author:        Joe Marshall
;;;; Creation Date: 2003
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CONMAN")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(
            conman-request
            call-with-request-variables
            )))

(proclaim (standard-optimizations))

(defclass conman-request ()
  (
   ;; The first several fields tell us about the context in which the
   ;; user is making the request.  Note that the fields that don't
   ;; depend on the database have been parsed at this point.

   ;; Who the user is.
   (user-name :initarg :user-name
              :initform (error "Required initarg :user-name omitted.")
              :reader conman-request/user-name
              :type string)

   ;; Where the user is, and what language he prefers.
   (locale :initarg :locale
           :initform (error "Required initarg :locale omitted.")
           :reader conman-request/client-locale
           :type locale)

   ;; What time is it in user land?
   ;; Offset from GMT in ???  
   (client-timezone :initarg :client-timezone
                    :initform (default-time-zone)
                    :reader   conman-request/client-timezone
                    :type    integer)

   ;; What OS the user is using.
   (client-platform :initarg   :client-platform
                    :initform  (error "Required initarg :client-platform omitted.")
                    :reader    conman-request/client-platform
                    :type      platform)

   ;; What workspace are we `in'?
   (workspace-id      :initarg :workspace-id
                      :initform nil
                      :reader conman-request/workspace-id
                      :type   (or null integer))

   ;; What directory relative pathnames are relative to.
   ;; We ought to resolve relative pathnames via the file-system-agent,
   ;; but if we do server-relative access to the user's file system,
   ;; this will be needed.
   (current-directory :initarg  :current-directory
                      :initform (error "Required initarg :current-directory omitted.")
                      :reader   conman-request/current-directory
                      :type     pathname)

   ;; The user's home directory.
   (home-directory :initarg  :home-directory
                   :initform (error "Required initarg :home-directory omitted.")
                   :reader   conman-request/home-directory
                   :type     pathname)

;   ;; The file system agent that is handling the users disk.
;   (file-system-agent :initarg :file-system-agent
;                      :initform (error "Required initarg :file-system-agent omitted.")
;                      :reader   conman-request/file-system-agent
;                      :type     file-system)

   ;; These fields are pertinent to the request processing.

   ;; How noisy should we be?
   ;; 0 = none, no output at all.  Return code indicates success.
   ;; 1 = minimum, no output on success.  Error or warning print a message.
   ;; 2 = standard, single line indicating success or failure
   ;; 3 = wordy, occasional status updates printed in addition
   ;; 4 = noisy, lots of printing, progress indication, etc.
   (verbosity         :initarg :verbosity
                      :initform (error "Required initarg :verbosity omitted.")
                      :reader   conman-request/verbosity
                      :type    non-negative-fixnum)


   ;; These fields determine the primary object that the user
   ;; is operating upon.

   ;; Which repository are we talking about?
   (repository-dbpath :initarg :master-repository
                      :initform (error "Required initarg :master-repository omitted.")
                      :reader conman-request/repository-dbpath
                      :type   dbpath)

   ;; Which branch are we talking about?
   ;; This can be a product name (and we use the default branch),
   ;; a branch name (and infer the product), a CONS containing both,
   ;; or the DIDs of either.
   (branch-specifier :initarg :branch-specifier
                     :reader conman-request/branch-specifier
                     :type (or null cons distributed-identifier string))

   ;; Which subsystem are we talking about?
   ;; This can be a class name (subsystem located relative to branch above),
   ;; or a subsystem DID.
   (subsystem-specifier :initarg :subsystem-specifier
                     :reader conman-request/subsystem-specifier
                     :type (or null distributed-identifier string))

   ;; What subdirectory or file in the subsystem are we talking about?
   ;; This can be a file or directory (or null).
   (path-specifier :initarg :path-specifier
                   :reader conman-request/path-specifier
                   :type (or null pathname))


   ;; When do we want to set the master clock?
   (timespec :initarg :timespec
             :initform nil
             :reader   conman-request/timespec)
   )
  (:documentation "Request to be made to the changesafe server."))

(defun call-with-request-variables (request receiver)
  (funcall receiver
           :master-repository (conman-request/repository-dbpath request)))


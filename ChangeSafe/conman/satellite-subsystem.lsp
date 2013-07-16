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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Module Description:
;;;   Satellite subsystems
;;;;
;;;; Author:        Joe Marshall
;;;; Creation Date: 2003
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CONMAN")

(proclaim (standard-optimizations))

(defclass satellite-subsystem (rfm-branch)
  ()
  (:documentation "A manifest subsystem object.")
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defun satellite-subsystem/publish-to-file-system (satellite-project satellite-subsystem file-system
                                                                     &key create?
                                                                     publish-read-only?
                                                                     report-file-system
                                                                     unpublish?
                                                                     when-overwriting-directory
                                                                     when-overwriting-file)
  (rfm-project/publish-to-file-system satellite-project satellite-subsystem file-system
                                      :create? create?
                                      :publish-read-only? publish-read-only?
                                      :report-file-system report-file-system
                                      :unpublish? unpublish?
                                      :when-overwriting-directory when-overwriting-directory
                                      :when-overwriting-file when-overwriting-file))

(defun satellite-subsystem/publish-to-file-list (satellite-subsystem)
  (rfm-branch/publish-to-file-list satellite-subsystem))

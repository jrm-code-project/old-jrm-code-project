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

(in-package "NET.URI")

(proclaim (csf/utility::standard-optimizations))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(utility:uri))
  (export '(
            copy-uri
            merge-uris
            parse-uri
            utility:uri
            uri-fragment
            uri-host
            uri-path
            uri-port
            uri-plist
            uri-query
            uri-scheme
            render-uri
            )))

(defun uri-scheme (uri)
  (csf/utility:uri/scheme uri))

(defsetf uri-scheme (uri) (new-value)
  `(CSF/UTILITY::URI/SET-SCHEME ,uri ,new-value))

(defun uri-host (uri)
  (let ((raw-host (csf/utility:uri/host uri)))
    (and raw-host
         (csf/utility:ascii->ucs-2 raw-host))))

(defsetf uri-host (uri) (new-value)
  `(CSF/UTILITY::URI/SET-HOST ,uri ,new-value))

(defun uri-port (uri)
  (csf/utility:uri/port uri))

(defsetf uri-port (uri) (new-value)
  `(CSF/UTILITY::URI/SET-PORT ,uri ,new-value))

(defun uri-path (uri)
  (csf/utility:uri/raw-path uri))

(defsetf uri-path (uri) (new-value)
  `(CSF/UTILITY::URI/SET-PATH ,uri ,new-value))

(defun uri-parsed-path (uri)
  (csf/utility:uri/path uri))

(defun uri-plist (uri)
  (csf/utility:uri/plist uri))

(defun uri-query (uri)
  (csf/utility:uri/raw-query uri))

(defsetf uri-query (uri) (new-value)
  `(CSF/UTILITY::URI/SET-QUERY ,uri ,new-value))

(defun uri-fragment (uri)
  (csf/utility:uri/fragment uri))

(defsetf uri-fragment (uri) (new-value)
  `(CSF/UTILITY::URI/SET-FRAGMENT ,uri ,new-value))

(defun uri-string (uri)
  (csf/utility::net-uri-string uri))

(defun merge-uris (uri defaults)
  (csf/utility:merge-uris (etypecase uri
                            ((string) (csf/utility:parse-uri (csf/utility:ucs-2->ascii uri)))
                            ((uri) uri))
                          defaults))

(defun copy-uri (uri &key
                     (scheme nil new-scheme-p)
                     (host nil new-host-p)
                     (port nil new-port-p)
                     (path nil new-path-p)
                     (query nil new-query-p)
                     (fragment nil new-fragment-p))
  (make-instance 'uri
                 :scheme (if new-scheme-p scheme (csf/utility:uri/scheme uri))
                 :authority (if (null new-host-p)
                                (if (null new-port-p)
                                    (csf/utility::uri/authority uri)
                                    ;; Just a new port
                                    (let ((old-authority (csf/utility::uri/authority uri)))
                                      (list (car old-authority)
                                            (cadr old-authority)
                                            port)))
                                (let ((old-authority (csf/utility::uri/authority uri)))
                                  (if (null new-port-p)
                                      ;; Just a new host
                                      (list (car old-authority)
                                            host
                                            (caddr old-authority))
                                      ;; New host and port
                                      (list (car old-authority)
                                            host
                                            port))))
                 :path (if new-path-p path (csf/utility:uri/path uri))
                 :query (if new-query-p query (csf/utility:uri/query uri))
                 :fragment (if new-fragment-p fragment (csf/utility:uri/fragment uri))))

(defun render-uri (uri stream)
  (csf/utility:render-uri uri stream))

(defun parse-uri (string)
  (csf/utility:parse-uri
   (csf/utility:ucs-2->ascii string)))

;; Hack to allow allegrostore to compile.
(provide :uri)

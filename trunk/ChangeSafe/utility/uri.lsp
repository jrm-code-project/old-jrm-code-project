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

(proclaim (standard-optimizations))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(csf/config::*error-on-bad-uri-escape*
            )
          "CSF/CONFIG")
  (export '(
            extend-uri-query
            merge-uris
            parse-uri
            parse-uri-query
            render-uri
            uri
            uri/fragment
            uri/host
            uri/path
            uri/plist
            uri/port
            uri/query
            uri/raw-path
            uri/raw-query
            uri/scheme
            scan-query
            uri-query/lookup
            uri-query/lookup-all
            )))

;;; See RFC 2396

(defconstant *ascii-code-set/uri-reserved*
  (load-time-value
   (lisp-characters->ascii-code-set
    '(#\; #\/ #\? #\: #\@ #\& #\= #\+ #\$ #\,)))
  "`Reserved' ascii characters, RFC 2396, section 2.2")

(defconstant *ascii-code-set/uri-mark*
  (load-time-value
   (lisp-characters->ascii-code-set
    '(#\- #\_ #\. #\! #\~ #\* #\' #\( #\))))
  "`Mark' ascii characters, RFC 2396, section 2.3")

(defconstant *ascii-code-set/uri-unreserved*
  (load-time-value
   (ascii-code-set/union *ascii-code-set/alphanumeric*
                         *ascii-code-set/uri-mark*))
  "`Unreserved' ascii characters, RFC 2396, section 2.3")

(defconstant *ascii-code-set/uri-escaped*
  (load-time-value
   (ascii-code-set/union *ascii-code-set/hex-digit*
                         (lisp-characters->ascii-code-set '(#\%))))
  "`Escaped' ascii characters, RFC 2396, section 2.4.1")

(defconstant *ascii-code-set/uri-scheme*
  (load-time-value
   (ascii-code-set/union *ascii-code-set/alphanumeric*
                         (lisp-characters->ascii-code-set '(#\+ #\- #\.))))
  "RFC 2396, section 3.1")

(defconstant *ascii-code-set/uri-authority*
  (load-time-value
   (ascii-code-set/union *ascii-code-set/uri-unreserved*
                         *ascii-code-set/uri-escaped*
                         (lisp-characters->ascii-code-set
                          '(#\; #\: #\@ #\& #\= #\+ #\$ #\,)))))

(defconstant *ascii-code-set/uri-reserved-in-authority*
  (load-time-value
   (ascii-code-set/complement
    (ascii-code-set/difference *ascii-code-set/uri-authority*
                               (lisp-characters->ascii-code-set
                                '(#\% #\; #\: #\@ #\? #\/))))))

(defconstant *ascii-code-set/uri-path*
  (load-time-value
   (ascii-code-set/union *ascii-code-set/uri-authority*
                         (lisp-characters->ascii-code-set '(#\/)))))

(defconstant *ascii-code-set/uri-reserved-in-path*
  (load-time-value
   (ascii-code-set/complement
    (ascii-code-set/difference *ascii-code-set/uri-path*
                               (lisp-characters->ascii-code-set
                                '(#\% #\/ #\; #\= #\?))))))

(defconstant *ascii-code-set/uri-char*
  (load-time-value
   (ascii-code-set/union *ascii-code-set/uri-unreserved*
                         *ascii-code-set/uri-reserved*
                         *ascii-code-set/uri-escaped*)))

(defconstant *ascii-code-set/uri-reserved-in-query*
  (load-time-value
   (ascii-code-set/complement
    (ascii-code-set/difference *ascii-code-set/uri-char*
                               (lisp-characters->ascii-code-set
                                '(#\% #\; #\/ #\? #\: #\@ #\& #\=
                                  #\+ #\, #\$))))))

(defconstant *ascii-code-set/uri-reserved-in-fragment*
  (load-time-value
   (ascii-code-set/complement
    (ascii-code-set/difference *ascii-code-set/uri-char*
                               (lisp-characters->ascii-code-set
                                '(#\% #\#))))))

(defconstant *ascii-code-set/uri-authority-userinfo*
  (load-time-value
   (ascii-code-set/union *ascii-code-set/uri-unreserved*
                         *ascii-code-set/uri-escaped*
                         (lisp-characters->ascii-code-set
                          '(#\; #\: #\& #\= #\+ #\$ #\,)))))

(defconstant *ascii-code-set/uri-authority-host*
  (load-time-value
   (ascii-code-set/union *ascii-code-set/alphanumeric*
                         (lisp-characters->ascii-code-set
                          '(#\- #\.)))))

(defconstant *ascii-code-set/uri-path-segment*
  (load-time-value
   (ascii-code-set/union *ascii-code-set/uri-unreserved*
                         *ascii-code-set/uri-escaped*
                         (lisp-characters->ascii-code-set
                          '(#\; #\: #\@ #\& #\= #\+ #\$ #\,)))))

(defconstant *ascii-code-set/uri-path-segment-param*
  (load-time-value
   (ascii-code-set/union *ascii-code-set/uri-unreserved*
                         *ascii-code-set/uri-escaped*
                         (lisp-characters->ascii-code-set
                          '(#\: #\@ #\& #\= #\+ #\$ #\,)))))

(defconstant *ascii-code->hex-value*
  (let ((codes (make-array 128 :initial-element nil)))
    ;; do the digits
    (dotimes (i 10)
      (setf (elt codes (+ i 48)) i))
    ;; A-F
    (dotimes (i 6)
      (setf (elt codes (+ i 65)) (+ i 10))
      (setf (elt codes (+ i 97)) (+ i 10)))
    codes)
  "An array of 128 values that maps the ascii value
   of a hexidecimal digit to the digit value.
   Other characters are mapped to NIL.")

(defparameter *error-on-bad-uri-escape* nil
  "If T, signal an error when encountering a % sign in a uri
   that is not followed by a hex code.  If NIL, silently accept
   the percent sign as if it had been encoded.")

(defun uri-decode-series (ascii-codes decode-plus-sign)
  "A series transducer that takes ascii-codes in and produces octets
   by interpreting the hex escaped characters.  If decode-plus-sign is T,
   then the `+' character will be decoded as a space.

   Note that the returned series is a series of octets.  It used to be
   specified that these octets were iso-8859-1, but now the
   convention is UTF-8."
  (declare (optimizable-series-function)
           (off-line-port ascii-codes))
  (choose-if #'plusp
    (decode-series ascii-codes
      :padding -1
      :decoder (lambda (c0 c1 c2)
                 (cond ((= c0 #.(char-code #\%))
                        (if (= c2 -1)
                            (if *error-on-bad-uri-escape*
                                (error "End of string in uri-decode.")
                                (values c0 0))
                            (let ((hi (elt *ascii-code->hex-value* c1))
                                  (lo (elt *ascii-code->hex-value* c2)))
                              (cond ((and hi lo) (values (+ (* hi 16) lo) 2))
                                    (*error-on-bad-uri-escape*
                                     (error "Illegal hex code %~c~c"
                                            (code-char c1)
                                            (code-char c2)))
                                    (t (values c0 0))))))
                       ((and decode-plus-sign
                             (= c0 #.(char-code #\+)))
                        (values #.(char-code #\Space) 0))
                       (t (values c0 0)))))))

;; Other possibility for string-type is iso-8859-1-string
(defun uri-decode (string decode-plus-sign &optional (string-type 'utf-8-string))
  (make-instance string-type
                 :code-unit-vector
                 (collect 'vector
                          (uri-decode-series
                           (scan-code-units string)
                           decode-plus-sign))))

(defun uri-encode-series (utf-8-series escaped-codes encode-space)
  (declare (optimizable-series-function)
           (off-line-port utf-8-series))
  (multiple-value-bind (c0 c1)
      (chunk 2 1
             (catenate
              ;; Add two extra octets (-1) in the
              ;; series at the places we need to insert escapes.
              (spread (map-fn '(integer 0 2)
                              (lambda (octet)
                                (cond ((and encode-space
                                            (= octet #.(char-code #\Space)))
                                       0)
                                      ((or (>= octet 128)
                                           (ascii-code-set/member octet escaped-codes))
                                      2)
                                      (t 0)))
                              utf-8-series)
                      utf-8-series
                      -1)
              ;; pad out by one so we check each pair.
              (scan 'list '( -1))))
    ;; Now consider each pair in the series in turn.
    ;; If they are both -1, substitute a percent sign.
    ;; If the first is -1, substitute the high nybble of
    ;; the second.
    ;; If the first isn't ascii, substitute the low nybble of it.
    (map-fn '(integer 0 (128))
            (lambda (c0 c1)
              (cond ((and encode-space
                          (= c0 #.(char-code #\Space)))
                     #.(char-code #\+))
                    ((= c0 -1)
                     (if (= c1 -1)
                         #.(char-code #\%)
                         (char-code (schar "0123456789ABCDEF" (ldb (byte 4 4) c1)))))
                    ((or (>= c0 128)
                         (ascii-code-set/member c0 escaped-codes))
                     (char-code (schar "0123456789ABCDEF" (ldb (byte 4 0) c0))))
                    (t c0)))
            c0 c1)))

(defun uri-encode (string escaped-codes encode-space)
  (check-type string (or ascii-string
                         utf-8-string
                         simple-base-string
                         #+lispworks lw:simple-text-string))
  (make-instance 'ascii-string
                 :code-unit-vector
                 (collect 'vector
                          (uri-encode-series
                           (scan-code-units string)
                           escaped-codes
                           encode-space))))

(defclass uri ()
  ((scheme :accessor uri/scheme
           :initarg :scheme
           :initform :unspecific)

   (authority :accessor uri/authority
              :initarg :authority
              :initform :unspecific)

   (path :accessor uri/path
         :initarg :path
         :initform nil)

   (query :accessor uri/query
          :initarg :query
          :initform :unspecific)

   (fragment :accessor uri/fragment
             :initarg :fragment
             :initform nil)

   (plist :accessor uri/plist
          :initarg :plist
          :initform nil)))

;;; Since we'll be wanting to encode lisp values within URI queries,
;;; we add some micro syntax to the query.  The colon character before
;;; the query key will cause the key to be decoded as a keyword.
;;; (this makes it easy to write the html for forms)
;;;
;;; We don't want to have magic coding on the value because the user
;;; generally types in values, so the magic coding for values is again
;;; on the key.  If the key begins and ends with #% %#, this indicates
;;; that the key should be read as a keyword, *and* the value should be
;;; read as a lisp form.

;;; The value is base64 encoded.

(defun parse-uri-query (string &optional (index 0) (limit (string-length string)))
  "Return a list of uri query elements."
  (let ((split-ampersand (match-split (match-ascii-code #.(char-code #\&))))
        (search-equals   (match-search (match-ascii-code #.(char-code #\=)))))

    (flet ((parse-uri-query-pair (string)
             (let ((start 0)
                   (limit (ustring-code-unit-count string)))
               (funcall
                (funcall search-equals
                        (lambda (string match-start match-end limit)
                          (parse-uri-query-and-value string 0 match-start match-end limit))
                        (lambda ()
                          (parse-uri-query-singleton string 0 limit)))
                string start limit))))

    (map 'list #'parse-uri-query-pair
         (funcall split-ampersand string index limit)))))

(defun parse-uri-query-and-value (string key-start key-limit value-start value-limit)
  (let* ((raw-key (uri-decode (usubstring-code-units string key-start key-limit) t))
         (raw-value (uri-decode (usubstring-code-units string value-start value-limit) t))
         (key-string (utf-8->ucs-2 raw-key))
         (keylength  (string-length key-string)))
    (cond ((and (> keylength 4)
                (char= (char key-string 0) #\#)
                (char= (char key-string 1) #\%)
                (char= (char key-string (- keylength 2)) #\%)
                (char= (char key-string (- keylength 1)) #\#))
           (cons
            (find-keyword (subseq key-string 2 (- keylength 2)));; key is a keyword
            (let ((*package* (keyword-package)))
              (read-from-string (base64-decode-string-to-string (utf-8->ucs-2 raw-value))))))
          ((and (> keylength 1)
                (char= (char key-string 0) #\:))
           (cons (find-keyword (subseq key-string 1 keylength))
                 raw-value))
          (t (cons raw-key raw-value)))))

(defun parse-uri-query-singleton (string key-start key-limit)
  (let* ((raw-key (uri-decode (usubstring-code-units string key-start key-limit) t))
         (key-string (utf-8->ucs-2 raw-key))
         (keylength  (string-length key-string)))
    (cond ((and (> keylength 1)
                (char= (char key-string 0) #\:))
           (find-keyword (subseq key-string 1 (string-length string))))
          (t raw-key))))

(defun render-uri-query (pair-or-singleton stream)
  (cond ((consp pair-or-singleton)
         (let ((key   (car pair-or-singleton))
               (value (cdr pair-or-singleton)))
           (cond ((not (keywordp key))
                  (format stream "~/format-ustring/=~/format-ustring/"
                          (uri-encode key *ascii-code-set/uri-reserved-in-query* t)
                          (uri-encode value *ascii-code-set/uri-reserved-in-query* t)))
                 ((typep value 'universal-string)
                  (format stream "~/format-ustring/~/format-ustring/=~/format-ustring/"
                          (uri-encode ":" *ascii-code-set/uri-reserved-in-query* t)
                          (uri-encode (symbol-name key) *ascii-code-set/uri-reserved-in-query* t)
                          (uri-encode value *ascii-code-set/uri-reserved-in-query* t)))
                 (t
                  (format stream "~/format-ustring/~/format-ustring/~/format-ustring/=~/format-ustring/"
                          (uri-encode "#%" *ascii-code-set/uri-reserved-in-query* t)
                          (uri-encode (symbol-name key) *ascii-code-set/uri-reserved-in-query* t)
                          (uri-encode "%#" *ascii-code-set/uri-reserved-in-query* t)
                          (uri-encode (let ((*package* (keyword-package)))
                                        (base64-encode-string (prin1-to-string value)))
                                      *ascii-code-set/uri-reserved-in-query* t))))))
        ((keywordp pair-or-singleton)
         (format stream "~/format-ustring/~/format-ustring/"
                          (uri-encode ":" *ascii-code-set/uri-reserved-in-query* t)
                          (uri-encode (symbol-name pair-or-singleton) *ascii-code-set/uri-reserved-in-query* t)))
        (t
         (format stream "~/format-ustring/"
                          (uri-encode pair-or-singleton *ascii-code-set/uri-reserved-in-query* t)))))


(defun render-uri (uri stream)
  (if (null stream)
      (with-output-to-string (result)
        (render-uri uri result))
      (progn
        (when (and (uri/scheme uri)
                   (not (eq (uri/scheme uri) :unspecific)))
          (format stream "~(~a~):" (symbol-name (uri/scheme uri))))
        (when (and (uri/authority uri)
                   (not (eq (uri/authority uri) :unspecific)))
          (destructuring-bind (user host port) (uri/authority uri)
            (format stream "//~@[~/format-ustring/@~]~@[~/format-ustring/~]~@[:~d~]"
                    (and user
                         (uri-encode user *ascii-code-set/uri-reserved-in-authority* nil))
                    (and host
                         (uri-encode host *ascii-code-set/uri-reserved-in-authority* nil))
                    port)))
        (when (uri/path uri)
          (when (eq (car (uri/path uri)) :absolute)
            (write-char #\/ stream))
          (when (cdr (uri/path uri))
            (let ((first-segment (cadr (uri/path uri))))
              (cond ((eq first-segment :dot) (write-char #\. stream))
                    ((eq first-segment :back) (write-string ".." stream))
                    ((consp first-segment)
                     (format stream "~{~/format-ustring/~@{;~/format-ustring/~}~}"
                             (map 'list (lambda (segment-element)
                                          (uri-encode segment-element *ascii-code-set/uri-reserved-in-path* nil))
                                  first-segment)))
                    (t (format stream "~/format-ustring/"
                               (uri-encode first-segment *ascii-code-set/uri-reserved-in-path* nil)))))
            (dolist (segment (cddr (uri/path uri)))
              (cond ((eq segment :dot) (write-string "/." stream))
                    ((eq segment :back) (write-string "/.." stream))
                    ((consp segment)
                     (format stream "~{/~/format-ustring/~@{;~/format-ustring/~}~}"
                             (map 'list (lambda (segment-element)
                                          (uri-encode segment-element *ascii-code-set/uri-reserved-in-path* nil))
                                  segment)))
                    (t (format stream "/~/format-ustring/"
                               (uri-encode segment *ascii-code-set/uri-reserved-in-path* nil)))))))
        (when (and (uri/query uri)
                   (not (eq (uri/query uri) :unspecific)))
          (write-char #\? stream)
          (render-uri-query (car (uri/query uri)) stream)
          (dolist (argpair (cdr (uri/query uri)))
            (write-char #\& stream)
            (render-uri-query argpair stream)))

        (when (uri/fragment uri)
          (format stream "#~/format-ustring/" (uri-encode (uri/fragment uri)
                                                          *ascii-code-set/uri-reserved-in-fragment* nil))))))

(defmethod print-object ((object uri) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (render-uri object stream)))

(defun match-ascii-code-set (ascii-code-set)
  (check-type ascii-code-set ascii-code-set)
  (match-code-unit-predicate
   (lambda (ascii-code)
     (ascii-code-set/member ascii-code ascii-code-set))))

(defun match-ascii-code (ascii-code)
  (check-type ascii-code ascii-code)
  (match-code-unit-predicate
   (lambda (other-code)
     (= other-code ascii-code))))

(defun decode-uri-scheme (string)
  (find-keyword (string-upcase (ascii->ucs-2 string))))

(defun parse-uri-authority (string index limit)
  "Returns the three primary pieces of the URI authority."
  (let ((match-userinfo
         (match-sequence
          (match-star
           (match-ascii-code-set *ascii-code-set/uri-authority-userinfo*))
          (match-ascii-code #.(char-code #\@))))

        (match-host
         (match-sequence
          (match-ascii-code-set *ascii-code-set/alphanumeric*)
          (match-star
           (match-ascii-code-set *ascii-code-set/uri-authority-host*))))

        (match-port
         (match-sequence
          (match-ascii-code #.(char-code #\:))
          (match-ascii-code-set *ascii-code-set/decimal-digit*)
          (match-star
           (match-ascii-code-set *ascii-code-set/decimal-digit*)))))

    (labels ((parse-authority-userinfo (index)
               (let ((userinfo-end (match match-userinfo index)))
                 (multiple-value-bind (authority-host authority-port)
                     (parse-authority-hostport (or userinfo-end index))
                   (values (and userinfo-end
                                (uri-decode (usubstring-code-units string index (1- userinfo-end)) nil))
                           authority-host
                           authority-port))))

             (parse-authority-hostport (index)
               (let* ((hostend (match match-host index))
                      (portend (and hostend (match match-port hostend))))
                 (values (and hostend (uri-decode (usubstring-code-units string index hostend) nil))
                         (and portend
                              (= portend limit)
                              (parse-port-digits (uri-decode (usubstring-code-units string (1+ hostend) portend) nil))))))

             (parse-port-digits (ustring)
               (parse-integer (ascii->ucs-2 ustring)))

             (match (matcher index)
                    (funcall (funcall matcher
                                      (lambda (string beginning index end)
                                        (declare (ignore string beginning end))
                                        index)
                                      (constantly nil)) string index limit)))

      (parse-authority-userinfo index))))

(defun is-dot? (ustring)
  (and (ustring? ustring)
       (= (ustring-code-unit-count ustring) 1)
       (= (ustring-code-unit ustring 0) (lisp-char->ascii-code #\.))))

(defun is-dot-dot? (ustring)
  (and (ustring? ustring)
       (= (ustring-code-unit-count ustring) 2)
       (= (ustring-code-unit ustring 0) (lisp-char->ascii-code #\.))
       (= (ustring-code-unit ustring 1) (lisp-char->ascii-code #\.))))

(defun parse-uri-path (string index limit)
  "Returns a list of path segments."
  (let ((split-slash (match-split (match-ascii-code #.(char-code #\/))))
        (split-semicolon (match-split (match-ascii-code #.(char-code #\;)))))

    (flet ((parse-uri-path-segment (string)
             (let ((params (funcall split-semicolon string 0 (ustring-code-unit-count string))))
               (if (null (cdr params))
                   (let ((item (uri-decode (car params) nil)))
                     (cond ((is-dot? item) :dot)
                           ((is-dot-dot? item) :back)
                           (t item)))
                   (map 'list (lambda (param) (uri-decode param nil))
                        params)))))

      (let ((split-path (map 'list #'parse-uri-path-segment
                             (funcall split-slash string index limit))))
        (cond ((and (ustring? (car split-path))
                    (zerop (ustring-code-unit-count (car split-path))))
               (if (null (cdr split-path))
                   '()
                   (cons :absolute (cdr split-path))))
              (t (cons :relative split-path)))))))

(defun parse-uri-aux (string index limit)
  "Returns the five primary pieces of a hierarchical uri."

  (let ((match-scheme
         (match-sequence
          (match-ascii-code-set *ascii-code-set/alpha*)
          (match-star (match-ascii-code-set *ascii-code-set/uri-scheme*))
          (match-ascii-code #.(char-code #\:))))

        (match-authority
         (match-sequence
          (match-ascii-code #.(char-code #\/))
          (match-ascii-code #.(char-code #\/))
          (match-star (match-ascii-code-set *ascii-code-set/uri-authority*))))

        (match-path
         (match-star (match-ascii-code-set *ascii-code-set/uri-path*)))

        (match-query
         (match-sequence
          (match-ascii-code #.(char-code #\?))
          (match-star (match-ascii-code-set *ascii-code-set/uri-char*))))

        (match-fragment
         (match-sequence
          (match-ascii-code #.(char-code #\#))
          (match-star (match-ascii-code-set *ascii-code-set/uri-char*)))))

  (labels ((get-uri-scheme (index)
             (let ((scheme-end (match match-scheme index)))
               (multiple-value-bind (authority path query fragment)
                   (get-uri-authority (or scheme-end index))
                 (values (if scheme-end
                             (decode-uri-scheme
                              (uri-decode
                               (usubstring-code-units string index (1- scheme-end)) nil))
                             :unspecific)
                         authority
                         path
                         query
                         fragment))))

           (get-uri-authority (index)
             (let ((authority-end (match match-authority index)))
               (multiple-value-bind (path query fragment)
                   (get-uri-path (or authority-end index))
                 (values (if authority-end
                             (multiple-value-list
                              (parse-uri-authority string (+ index 2) authority-end))
                             :unspecific)
                         path
                         query
                         fragment))))

           (get-uri-path (index)
             (let ((path-end (match match-path index)))
               (multiple-value-bind (query fragment)
                   (get-uri-query (or path-end index))
               (values (and path-end
                            (parse-uri-path string index path-end))
                       query
                       fragment))))

           (get-uri-query (index)
             (let* ((query-end (match match-query index))
                    (fragment  (get-uri-fragment (or query-end index))))
               (values (if query-end
                           (parse-uri-query string (1+ index) query-end)
                           :unspecific)
                       fragment)))

           (get-uri-fragment (index)
             (let ((fragment-end (match match-fragment index)))
               (and fragment-end
                    (uri-decode
                     (usubstring-code-units string (1+ index) fragment-end) nil))))

           (match (matcher index)
             (funcall (funcall matcher
                               (lambda (string beginning index end)
                                 (declare (ignore string beginning end))
                                 index)
                               (constantly nil)) string index limit)))

    (get-uri-scheme index))))

(defun parse-uri (string)
  (check-type string ascii-string)
  (multiple-value-bind (scheme authority path query fragment)
      (parse-uri-aux string 0 (ustring-code-unit-count string))
    (make-instance 'uri
                   :scheme scheme
                   :authority authority
                   :path path
                   :query query
                   :fragment fragment
                   :plist nil)))

(defun uri/raw-path (uri)
  "Returns the `path' component of the URI, with appropriate escapes."
  (ascii->ucs-2
   (apply #'ustring-join
    (ucs-2->ascii "/")
    (map 'list
         (lambda (component)
           (if (consp component)
               (apply #'ustring-join
                      (ucs-2->ascii ";")
                      (map 'list (lambda (component)
                                   (uri-encode component
                                               *ascii-code-set/uri-reserved-in-path*
                                               nil))
                           component))
               (uri-encode component *ascii-code-set/uri-reserved-in-path*
                           nil)))
         (if (eq (car (uri/path uri)) :absolute)
             (cons (ucs-2->ascii "") (cdr (uri/path uri)))
             (cdr (uri/path uri)))))))

(defun uri/raw-query (uri)
  "Returns the `query' component of the URI, with appropriate escapes."
  (and (uri/query uri)
       (ascii->ucs-2
        (apply
         #'ustring-join
         (ucs-2->ascii "&")
         (map 'list
              (lambda (component)
                (if (consp component)
                    (ustring-join
                     (ucs-2->ascii "=")
                     (uri-encode (car component) *ascii-code-set/uri-reserved-in-query*
                                 nil)
                     (uri-encode (cdr component) *ascii-code-set/uri-reserved-in-query*
                                 nil))
                    (uri-encode component *ascii-code-set/uri-reserved-in-query*
                                nil)))
             (uri/query uri))))))

(defunimplemented net-uri-string (uri))

(defun append-uri-paths (relative base)
  ;; This implements step 6 of the rfc.  Yuck.
  ;; This could have more performance, but it is ok for now.
  (labels ((outer-loop (path)
             (if (length< path 2)
                 path
                 (inner-loop (list (car path)) (cadr path) (cddr path))))

           ;; this removes <segment>/../
           (inner-loop (before scan after)
             (cond ((null after)
                    (if (and (eq scan :back)
                             (not (eq (car before) :back))
                             (not (eq (car before) :absolute)))
                        ;; this removes trailing <segment>/..
                        (reverse (cons (ucs-2->ascii "") (cdr before)))
                        (reverse (cons scan before))))
                   ;; Remove leftmost occurrance of <segment>/../
                   ((and (eq scan :back)
                         (not (eq (car before) :back))
                         (not (eq (car before) :absolute)))
                    (outer-loop (revappend (cdr before) after)))
                   (t (inner-loop (cons scan before)
                                  (car after)
                                  (cdr after))))))

    (let ((appended
                 ;; Step 6 b, append the relative
                 (append
                  ;; Step 6 a, lop off the last of the base.
                  (butlast base)
                  (cdr relative))))
      ;; Remove "./" components and trailing "."
      (do ((tail appended (cdr tail))
           (head '() (cond ((and (cdr tail) (eq (car tail) :dot)) head)
                           ((eq (car tail) :dot) (cons (ucs-2->ascii "") head))
                           (t (cons (car tail) head)))))
          ((null tail) (outer-loop (reverse head)))))))

(defun merge-uris (uri default)
  (cond ((not (eq (uri/scheme uri) :unspecific)) uri)
        ((not (eq (uri/authority uri) :unspecific))
         (make-instance 'uri
                        :scheme (uri/scheme default)
                        :authority (uri/authority uri)
                        :path (uri/path uri)
                        :query (uri/query uri)
                        :fragment (uri/fragment uri)))
        (t
         (make-instance 'uri
                        :scheme (if (eq (uri/scheme uri) :unspecific)
                                    (uri/scheme default)
                                    (uri/scheme uri))
                        :authority (if (eq (uri/authority uri) :unspecific)
                                       (uri/authority default)
                                       (uri/authority uri))

                        :path (cond ((null (uri/path uri)) (append-uri-paths
                                                            (list :relative :dot (ucs-2->ascii ""))
                                                            (uri/path default)))
                                    ((or
                                      (eq (car (uri/path uri)) :absolute)
                                      (eq (car (uri/path default)) :relative)) (uri/path uri))
                                    ((and (eq (car (uri/path uri)) :relative)
                                          (eq (cadr (uri/path uri)) :dot)
                                          (null (cddr (uri/path uri))))
                                     (append-uri-paths
                                      (list :relative :dot (ucs-2->ascii ""))
                                      (uri/path default)))
                                    (t
                                     (append-uri-paths (uri/path uri) (uri/path default))))
                        :query (uri/query uri)
                        :fragment (uri/fragment uri)))))

(defun uri/set-scheme (uri new-scheme)
  (setf (uri/scheme uri) new-scheme))

(defun uri/set-host (uri new-host)
  (setf (uri/authority uri)
        (let ((old-authority (uri/authority uri)))
          (cond ((or (null old-authority)
                     (eq old-authority :unspecific))
                 (list nil new-host nil))
                (t (list (car old-authority)
                         new-host
                         (and (cddr old-authority)
                              (caddr old-authority))))))))

(defun uri/set-port (uri new-port)
  (check-type uri uri)
  (check-type new-port (optional non-negative-fixnum))
  (setf (uri/authority uri)
        (let ((old-authority (uri/authority uri)))
          (cond ((or (null old-authority)
                     (eq old-authority :unspecific))
                 (list nil nil new-port))
                (t
                 (list (car old-authority)
                       (cadr old-authority)
                       new-port))))))

(defunimplemented uri/set-path (uri new-path))
(defunimplemented uri/set-query (uri new-query))
(defunimplemented uri/set-fragment (uri new-fragment))

(defun uri/host (uri)
  (and (consp (uri/authority uri))
       (cadr (uri/authority uri))))

(defun uri/port (uri)
  (let ((auth (uri/authority uri)))
    (and (consp auth)
         (consp (cdr auth))
         (or (third auth)
             (let ((scheme (uri/scheme uri)))
               (ecase scheme
                 (:http 80)))))))

(defun merge-query (new-query-pairs base-query-pairs)
  (cond ((eq base-query-pairs :unspecific) new-query-pairs)
        ((null base-query-pairs) new-query-pairs)
        ((assoc (caar base-query-pairs) new-query-pairs)
         (merge-query new-query-pairs (cdr base-query-pairs)))
        (t (merge-query (cons (car base-query-pairs) new-query-pairs)
                        (cdr base-query-pairs)))))

(defun extend-uri-query (base-uri new-path new-query)
  (merge-uris (make-instance 'uri
                             :path new-path
                             :query (merge-query new-query
                                                 (uri/query base-uri)))
              base-uri))

(defun scan-query (query)
  (declare (optimizable-series-function 2))
  ;; Similar to scan-alist, but does not suppress duplicate values.
  (map-fn '(values t t)
          (lambda (element)
            (values (car element)
                    (cdr element)))
          (scan 'list query)))

(defun uri-query/lookup (query key)
  "Given <uri-query> and <key>, return the first matching value."
  (multiple-value-bind (keys values)
      (scan-query query)
    (collect-first
     (choose (map-fn 'boolean
                     (lambda (this-key) (eql this-key key))
                     keys)
             values))))

(defun uri-query/lookup-all (query key)
  "Given <uri-query> and <key>, return all the matching values in a list."
  (multiple-value-bind (keys values)
      (scan-query query)
    (collect 'list
             (choose (map-fn 'boolean
                             (lambda (this-key) (eql this-key key))
                             keys)
                     values))))

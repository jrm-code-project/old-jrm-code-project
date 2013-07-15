;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright (c) 1999 Content Integrity, Inc.
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;;          Content Integrity, Inc. CONFIDENTIAL and PROPRIETARY material.
;;;;
;;;;          Content Integrity, Inc
;;;;          Braintree Executive Office Park
;;;;          P.O. Box 850942
;;;;          Braintree, MA 02185-0942
;;;;
;;;; This software and information comprise valuable intellectual property
;;;; and trade secrets of Content Integrity, Inc., developed at substantial
;;;; expense by Content Integrity, which Content Integrity intends to
;;;; preserve as trade secrets.  This software is furnished pursuant to a
;;;; written license agreement and may be used, copied, transmitted, and
;;;; stored only in accordance with the terms of such license and with the
;;;; inclusion of the above copyright notice.  This software and
;;;; information or any other copies thereof may not be provided or
;;;; otherwise made available to any other person.  NO title to or
;;;; ownership of this software and information is hereby transferred.
;;;; Content Integrity assumes no responsibility for the use or reliability
;;;; of this software.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; File Name:     db-name.lsp
;;;; Author:        Mark Nahabedian
;;;; Creation Date: 2000-03-01
;;;;
;;;; Module Description:
;;;;
;;;; The ObjectStore client (and AllegroStore) allow the specification
;;;; of a database file that is on some remote ObjectStore server.  In
;;;; this case the database file name is prefixed by the ObjectStore
;;;; server host name and a colon.
;;;;
;;;; Our software quite frequently needs to manipulate the names of
;;;; databases in the same way that ConnonLisp pathnames are
;;;; manipulated.  Regrettably, because of shortcomings in AllegroCL's
;;;; pathname support, we can not use the CommonLisp pathnames
;;;; directly if the database file isn't local.  This is because we
;;;; need to keep track of the server host name, and the server host
;;;; platform type as well as the database file name.  ACL's pathname
;;;; implementation does not allow for storing this host information
;;;; in the PATHNAME-HOST of a PATHNAME.
;;;;
;;;; To circumvent this gross shortsightedness on Franz's part, we
;;;; implement DBPATH objects for encapsulating all the information
;;;; we need for manipulating database names.  We also define a number
;;;; of operations on them similar to the operations on PATHNAMEs,
;;;; e.g. construction, merging, etc.
;;;;
;;;; For backwards compatibility with existing code, many of these
;;;; operations are defined to work on pathnames as well.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CSF/UTILITY")

(proclaim (standard-optimizations))

;;; API's exported from this module.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(
            *database-server-hosts*

            canonicalize-host-name

            db-host
            ;; accessors
            db-host/name
            db-host/type
            db-host/plist
            ;; finding and creating
            db-host/lookup

            dbpath
            dbpath?
            dbpath/host
            dbpath/pathname
            ;; Operations on DBPATHs
            dbpath/local-p
            dbpath/parse
            dbpath/namestring
            dbpath/merge

            ->dbpath
            )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DBPATH canonicalization
;;; this is kind of ugly, but it does generate a canonical name.
(defconstant +canonical-hostname-cache+
    (if (boundp '+canonical-hostname-cache+)
        (symbol-value '+canonical-hostname-cache+)
      (make-hash-table :test #'equalp))
  "A hash table mapping strings to canonical host names.")

#+allegro
(defun get-canonical-host-name (hostname)
  "Get the canonical name by querying the DNS.  Yuck."
  (socket:ipaddr-to-hostname
   (socket:lookup-hostname hostname :ignore-cache t)
   :ignore-cache t))

#+lispworks
(defun get-canonical-host-name (hostname)
  (debug-message 5 "get-canonical-host-name ~s" hostname)
  (let ((result (comm:get-host-entry hostname :fields '(:name))))
    (debug-message 5 "get-canonical-host-name ~s => ~s" hostname result)
    result))

(defun canonical-local-host-name ()
  (handler-case (canonicalize-host-name (machine-instance))
    (error () "127.0.0.1")))

(defun canonicalize-host-name (hostname)
  "Given a STRING or NIL (which means the local host), return the canonical name
   as a string."
  (check-type hostname (or null string))
  (let ((probe (gethash hostname +canonical-hostname-cache+)))
    (or probe
        (let ((canonical-name (if (or (null hostname)
                                      (string-equal hostname "localhost")
                                      (string-equal hostname "127.0.0.1"))
                                  (canonical-local-host-name)
                                (get-canonical-host-name hostname))))
          (when (null canonical-name)
            (error "Could not obtain canonical name for host ~s" hostname))
          (setf (gethash hostname +canonical-hostname-cache+) canonical-name)
          canonical-name))))

;;; End of gross canonicalization hack

(defvar *database-server-hosts* nil
  "The list of defined DB-HOST objects")

(defclass db-host ()
  ;; A string naming a host
  ((name :initarg :name
         :initform (error "Required initarg :name omitted.")
         :reader db-host/name
         :type string)
  ;; A keyword representing a platform type, as might be returned by
  ;; SERVER-PLATFORM, for example.
   (type :initarg :type
         :initform (error "Required initarg :type omitted.")
         :reader db-host/type)
  ;; A property list for storing application specific information.
   (plist :initform nil
          :reader db-host/plist))
  (:documentation
   "These objects are used to identify ObjectStore database server hosts.
      They are interned in *DATABSE-SERVER-HOSTS*, so there will only be one
      such object per host.
      In addition to encapsulating the host-specific portion of a DBPATH object
      these can also be used to associate application-specific information with
      the database server host through the PLIST slot."))

(defmethod initialize-instance :after ((object db-host) &rest initargs)
  (declare (ignore initargs))
  (let ((found (member (db-host/name object)
                       *database-server-hosts*
                       :key #'db-host/name)))
    (if found
        (setf (car found) object)
        (setq *database-server-hosts* (cons object *database-server-hosts*)))))

(defmethod print-object ((object db-host) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a ~s"
            (db-host/name object)
            (db-host/type object))))

(defmethod objects-equalp ((host1 db-host) (host2 db-host))
  (or (eq host1 host2)
      (and (eq (db-host/type host1) (db-host/type host2))
           (string-equal (db-host/name host1) (db-host/name host2)))))

(define-objects-equalp-type-mismatch-methods db-host)

(defun db-host/lookup (host-name)
  "Find the DB-HOST object named HOST-NAME.
   HOST-NAME can be NIL to identify the local host."
  (debug-message 5 "db-host/lookup ~s" host-name)
  (or (find (canonicalize-host-name host-name)
            *database-server-hosts*
            :test #'string-equal
            :key #'db-host/name)
      (when (eq (canonicalize-host-name host-name)
                (canonical-local-host-name))
        (make-instance 'db-host
                       :name (canonical-local-host-name)
                       :type (server-platform)))))

#||
(defun db-host-create (host-name host-type)
  "Construct a DB-HOST object of the given name and type.
   If you specify a HOST-NAME corresponding to an existing DB-HOST
   then the existing host will be returned.  If HOST-TYPE is different
   from the DB-HOST-TYPE of that DB-HOST, the DB-HOST will be altered
   accordingly."
  ;; HOST-NAME of NIL means the local host.
  (check-type host-name (or null string))
  (when host-type
    (guarantee-platform host-type))
  (when (null host-name)
    (setq host-type (server-platform)))

  (let* ((canonical-host-name
          (if host-name
              (canonicalize-host-name host-name)
            (canonical-local-host-name)))
         (existing-db-host (db-host-lookup canonical-host-name)))
    (if existing-db-host
        (setf (db-host-type existing-db-host) host-type)
      (let ((new (make-instance 'db-host
                   :db-host-name canonical-host-name
                   :db-host-type host-type)))
        (push new *database-server-hosts*)
        new))))
||#

;; Not sure this is necessary anymore.
(defmethod db-host/local-p ((db-host db-host))
  "Returns true if the DB-HOST object is that of the local host."
  (string-equal
   (db-host/name db-host)
   (canonical-local-host-name)))

#||
(progn (db-host-create "canterbury" :winnt)
       (db-host-create "conman" :unix)
       (db-host-create "keating" :unix))
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass dbpath ()
  ((host :initarg :host
         :initform (error "Required initarg :host omitted.")
         :reader dbpath/host
         :type db-host)
   (pathname :initarg :pathname
             :initform (error "Required initarg :pathname omitted.")
             :reader dbpath/pathname
             :type pathname))
  (:documentation
   "These objects name database files on the local and
      remote servers.  They are necessary because we are
      unable to encapsulate the notion of host name and host platform
      type using Franz's pathetic pathname implementation."))

(defmethod print-object ((object dbpath) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s" (dbpath/namestring object))))

(defun dbpath? (object)
  (eq (class-of object) (find-class 'dbpath)))

(defmethod objects-equalp ((name1 dbpath) (name2 dbpath))
  (or (eq name1 name2)
      (and (objects-equalp (dbpath/host name1) (dbpath/host name2))
           (equalp (dbpath/pathname name1) (dbpath/pathname name2)))))

(defmethod objects-equalp ((name1 dbpath) (name2 pathname))
  ;; Note that the generic function template covers the case where both operands are pathnames.
  (equalp (dbpath/pathname name1) name2))

(defmethod objects-equalp ((name1 pathname) (name2 dbpath))
  (objects-equalp name2 name1))

(define-objects-equalp-type-mismatch-methods dbpath)

#||
(defun dbpath-create (db-host pathname)
  "Construct a DBPATH object based on DB-HOST and PATHNAME.
   DBPATH objects are used to identify database files."
  (check-type db-host db-host)
  (make-instance 'dbpath
    :host db-host
    :pathname (typecase pathname
                        (pathname pathname)
                        (string (parse-client-namestring pathname
                                                         (db-host-type db-host))))))
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric dbpath/local-p (dbpath)
  (:documentation
   "Returns true if DBPATH names a database that is served by the database
    server on this machine."))

(defmethod dbpath/local-p ((dbpath dbpath))
  (db-host/local-p (dbpath/host dbpath)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dbpath/parse (database-namestring &key directory?)
  "Parse DATABASE-NAMESTRING into a DBPATH object.
   If :DIRECTORY is true then parse as a directory name."
  (check-type database-namestring string)
  ;; hostname ':' pathname-in-host-syntax
  (debug-message 5 "dbpath/parse ~s" database-namestring)
  (let ((colon-pos (position #\: database-namestring))
        db-host)
    (unless (or (null colon-pos)
                ;; if colon is in position 1, and we are on DOS,
                ;; it is a local name.
                (and (= colon-pos 1)
                     (typep (db-host/type (db-host/lookup nil)) 'microsoft-platform)))
      (let ((host-name (subseq database-namestring 0 colon-pos)))
        (setq db-host (ignore-errors (db-host/lookup host-name)))))
    ;; If there was no host prefix, or the host prefix did not match
    ;; the name of a known DB-HOST, assume the colon (if any) was part
    ;; of the pathname and that the database is local.
    (unless db-host
      (setq db-host (db-host/lookup nil)
            colon-pos -1))
    (make-instance 'dbpath
                   :host db-host
                   :pathname
                    (funcall (if directory?
                                 ;; e.g. this effectively adds a trailing '/' if needed
                                 #'platform/parse-directory-namestring
                                 #'platform/parse-namestring)
                             (db-host/type db-host)
                             database-namestring
                             :start (1+ colon-pos)))))

#||
(dbpath-parse "canterbury:\\csf-repositories\\foo.db")
(dbpath-parse "c:\\csf-repositories\\foo.db")
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric dbpath/namestring (dbpath)
  (:documentation
   "Return a string suitable for passing to OPEN-DATABASE."))

(defmethod dbpath/namestring ((dbpath dbpath))
  (with-output-to-string (stream)
    (let ((host (dbpath/host dbpath)))
      (write-string (db-host/name host) stream)
      (write-char #\: stream)
      (write-string (pathname->platform-namestring (dbpath/pathname dbpath)
                                                   (db-host/type host))
                    stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric dbpath/merge (name1 name2)
  (:documentation
   "Generic function for merging DBPATHs and PATHNAMEs.
    Similar in functionality to MERGE-PATHNAMES but can work on
    DBPATHs as well.

    Returns a DBPATH object or a PATHNAME object."))

(defmethod dbpath/merge ((pn pathname) (dbpath dbpath))
  (make-instance 'dbpath
                 :host (dbpath/host dbpath)
                 :pathname (merge-pathnames pn (dbpath/pathname dbpath))))

(defmethod dbpath/merge ((dbpath dbpath) (pn pathname))
  (make-instance 'dbpath
                 :host (dbpath/host dbpath)
                 :pathname (merge-pathnames (dbpath/pathname dbpath) pn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ->dbpath (possible-dbpath)
  "Convert the possible-dbpath into a dbpath object (so that it will have a known
   type and could be used by objects-equalp).

   Returns the (possibly converted) dbpath (or the original value, if we can't convert it)."
  (typecase possible-dbpath
    (dbpath possible-dbpath)
    (string   (dbpath/parse possible-dbpath))
    (pathname (dbpath/parse (namestring possible-dbpath)))
    (t possible-dbpath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

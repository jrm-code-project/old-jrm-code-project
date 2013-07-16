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

(in-package "CSF/PERSISTENT-STORE")

(proclaim (standard-optimizations))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (export '(
            did->list
            did->string
            did/class
            did/class
            did/domain
            did/numeric-id
            did/repository
            distributed-identifier
            distributed-identifier?
            list->did
            make-distributed-identifier
            parse-did
            )))

;;; This file implements the syntactic construct of a distributed
;;; identifier.  Later on, the core system will implement the semantics.
;;; This file just considers DIDs as 4-component interned objects.

;;;
;;; DISTRIBUTED-IDENTIFIER
;;;

(defstruct (distributed-identifier
            (:conc-name did/)
            (:constructor %make-distributed-identifier (domain repository class numeric-id))
            (:copier nil)
            (:predicate distributed-identifier?))
  "DIDs are interned, so you can use EQ to test for equality.  However, you should
   never modify the slots in a DID."

  (domain ""     :read-only t :type string)
  (repository "" :read-only t :type string)
  (class nil     :read-only t :type (or null keyword))
  (numeric-id 0  :read-only t :type non-negative-integer))

(defun write-did-components (did stream)
  "Emit the components of DID to STREAM."
  (flet ((write-component (component)
           (when component
             (write-string component stream)
             (write-char #\. stream))))

    (unless (string= (did/domain did) "")
      (write-component (did/domain did)))

    ;; Repository component should always be present (non-nil)
    ;; However (assert (distributed-identifier-repository did)) doesn't work well with print subsystem.
    ;; We look for null components and print something which probably isn't usually present
    ;; as a valid repository name.

    ;; Quote names which contain periods
    (if (not (string= (did/repository did) ""))
        (if (position #\. (did/repository did) :test #'char=)
          (prin1 (did/repository did) stream)
          (write-string (did/repository did) stream))
        (write-string "<<MISSING REPOSITORY COMPONENT>>" stream))

    ;; The following two components are optional, however if one is present, so is the other
    (when (did/class did)
      (write-char #\. stream)           ; terminate previous token
      ;; We don't print the package name, it ought to be a keyword.
      (write-component (string-downcase (symbol-name (did/class did))))
      (format stream "~d" (did/numeric-id did)))))

(defmethod print-object ((did distributed-identifier) stream)
  ;; DIDs are readable!
  (write-string "[" stream)
  (write-did-components did stream)
  (write-string "]" stream))

(defun %distributed-identifier-as-string-slow (did)
  "Return a string representation of a distributed identifier by printing
   it to a string."
  (let ((result-stream (make-string-output-stream)))
    (write-did-components did result-stream)
    (get-output-stream-string result-stream)))

#||
(defun test1 (count)
  (let ((did (make-distributed-identifier :domain nil :repository "foo" :class :subsystem :numeric-id 33)))
    (time
     (dotimes (i count)
       (%distributed-identifier-as-string-slow did)))))

(defun test2 (count)
  (let ((did (make-distributed-identifier :domain nil :repository "foo" :class :subsystem :numeric-id 33)))
    (time
     (dotimes (i count)
       (%distributed-identifier-as-string did)))))
||#

(defun %distributed-identifier-as-string (did)
  "Fast way to turn something known to be a distributed identifier into
   a string.  This is severely bummed code because we do this a LOT.

   Test results show that this is e^3.5 times faster than doing it the slow way.
   (on lispworks it's e^3.2)"
  (declare #.(performance-optimizations))
  (let ((domain     (if (string= (did/domain     did) "")
                        nil
                        (did/domain did)))
        (repository (if (string= (did/repository did) "")
                        "<<MISSING REPOSITORY COMPONENT>>"
                        (did/repository did)))
        (class      (and (did/class      did)
                         (symbol-name (did/class      did))))
        (id         (did/numeric-id did)))

    ;; try to do it the fast way
    (let ((result (make-string
                   (fix:+ (if domain (1+ (simple-string-length domain)) 0)
                          (simple-string-length repository);; always something
                          (if class
                              (fix:+ 1
                                     (simple-string-length class)
                                     (if id (1+ (fixnum-base10-digit-count id)) 0))
                              0))))
          (pos 0))
      (declare (type (simple-array character) result)
               (type array-index position)
               )
      (when domain
        (utility::%simple-substring-move-right domain 0 (simple-string-length domain) result 0)
        (setq pos (fix:+ pos (simple-string-length domain)))
        (setf (schar result pos) #\.)
        (setq pos (fix:+ pos 1)))
      (do ((i 0 (fix:+ i 1)))
          ((= i (simple-string-length repository)))
        (declare ;(type array-index i)
                 )
        (let ((c (schar repository i)))
          (when (char= c #\.)
            ;; Gotta escape this sucker, punt.
            (return-from %distributed-identifier-as-string
              (%distributed-identifier-as-string-slow did)))
          (setf (schar result pos) c)
          (setq pos (fix:+ pos 1))))
      (when class
        (setf (schar result pos) #\.)
        (setq pos (fix:+ pos 1))
        (utility::%simple-substring-move-right class 0 (string-length class) result pos)
        (setq pos (fix:+ pos (string-length class)))
        (setf (schar result pos) #\.)
        (setq pos (fix:+ pos 1))
        (when id
          (%write-fixnum-to-string id result pos)))
      result)))

(defun did->string (did &optional stream)
  "Turn a DID into a string.  Optional stream arg is for when you want
   to print the string form to a stream, a common operation."
  (check-type did distributed-identifier)
  (if stream
      (write-did-components did stream)
    (%distributed-identifier-as-string did)))

(defun parse-did (string &key (start 0) (end (simple-string-length string))
                              (junk-allowed nil)
                              (brackets-allowed nil))
  "Parse the string form of a DID as created by PRINT-OBJECT, and return a distributed-identifier object.
   PACKAGE is used to specify the symbol package of the class symbol of the DID, and defaults to the
   current package."
  (declare (ignore junk-allowed))
  (check-type string simple-string)
  ;; Peel off possible square brackets.
  ;; Kinda cheesy, but hey...
  (when (and brackets-allowed
             (> (- end start) 2)
             (char= (schar string start) #\[)
             (char= (schar string (- end 1)) #\]))
    (incf start)
    (decf end))
  (let ((tokens nil))
    ;; Doesn't handle escaped quotes in string for now.  Don't put them in your entity names.
    (loop with reading-string? = nil
          and start of-type array-index = start
          and end of-type array-index = 0
          and limit of-type array-index = end
          as char = (and (< end limit) (schar (the simple-string string) end))
          while char
          do (cond ((char= char #\.)            ;we've accrued a token
                    (pushlast (subseq (the simple-string string) start end) tokens)
                    (setq start (incf end)))    ; skip the "."
                   ((char= char #\")
                    (cond (reading-string?
                           ;; We've reached the end of the string
                           (pushlast (subseq (the simple-string string) (1+ start) end) tokens)
                           (setq start (incf end)) ;skip the "." following the closing quote
                           (setq reading-string? nil))
                          (t
                           ;; This token is a string, we're at the start
                           (setq reading-string? t)
                           (incf end))))
                   (t (incf end)))
          finally
          (when reading-string?
            (error "Missing closing quote in string"))
          (when (> end start)
            ;; We out of chars, that's the end of the token which we know was not a string subelement
            (pushlast (subseq (the simple-string string) start end) tokens)))
    ;; So we have the tokens.  They represent the right-most portions of the DID, left-most optional.
    (let* ((n-tokens (length tokens))
           (missing-tokens (- 4 n-tokens)))     ;if 4 are present, they're all present
      (declare (type (integer 0 4) n-tokens missing-tokens))
      (unless (>= n-tokens 3)
        (error "Distributed object identifier '~a' is missing a repository name component" string))

             (make-distributed-identifier
              :domain (if (= missing-tokens 0) (first tokens) "")
              :repository (elt tokens (- 1 missing-tokens))
              :class (find-keyword (string-upcase (elt tokens (- 2 missing-tokens))))
              :numeric-id (parse-integer (elt tokens (- 3 missing-tokens)))))))

(defun |[-reader| (stream arg)
  (declare (ignore arg))
  (let ((buffer (collect 'string (until-if (lambda (char)
                                             (char= char #\]))
                                           (scan-stream stream #'read-char)))))
    (parse-did buffer)))

;; Weak table for interning DIDs.
(defconstant *distributed-identifier-hash-table*
  (let ((table (if (boundp '*distributed-identifier-hash-table*)
                   (symbol-value '*distributed-identifier-hash-table*)
                   (make-hash-table
                    :test #'equal
                    #+allegro :values #+allegro :weak))))
    #+lispworks (hcl:set-hash-table-weak table t)
    table))

(defun unintern-distributed-identifier (did)
  (debug-message 5 "Removing ~s from ~s" did '*distributed-identifier-hash-table*)
  (remhash (did->list did) *distributed-identifier-hash-table*))

(defun make-distributed-identifier (&key domain repository class (numeric-id 0))
  "Create a distributed identifier.  Typically this is only invoked by
   the DISTRIBUTED-OBJECT-IDENTIFIER method, but there are some cases where it is useful
   to construct partial dids for matching with OBJECTS-EQUALP?

   Note that the REPOSITORY must always be specified.
   DOMAIN, SITE, HOST, and REPOSITORY are all strings (if not nil).
   CLASS is a symbol or nil.
   NUMERIC-ID is a number or nil."
  (check-type domain (or null string))
  (check-type repository string)
  (check-type class (or null keyword))
  (check-type numeric-id non-negative-fixnum)
  (let* ((hashkey (list (or domain "") repository class numeric-id))
         (probe (gethash hashkey *distributed-identifier-hash-table*))
         (result (or probe
                     (%make-distributed-identifier
                      domain
                      repository
                      class
                      numeric-id))))
    (if (null probe)
        (progn
          (debug-message 6 "New ~s" result)
          (setf (gethash hashkey *distributed-identifier-hash-table*) result)
          #+allegro (excl:schedule-finalization result #'unintern-distributed-identifier)
          )
      (debug-message 6 "Reusing ~s" result))
    result))

(defun did/locale (source-did)
  "Create and return a distributed identifier with unspecific CLASS and NUMERIC-ID components.
   This is sometimes useful for matching repository-locale information between objects of disparate
   type with OBJECTS-EQUALP."
  (make-distributed-identifier
   :domain     (did/domain     source-did)
   :repository (did/repository source-did)
   :class :||
   :numeric-id -1))

(defmethod objects-equalp ((left distributed-identifier) right)
  (declare (ignore right))
  nil)

(defmethod objects-equalp ((object1 distributed-identifier) (object2 distributed-identifier))
  "DIDs are interned.  However the intent of this method is to do an EQUALP check
   on DID components if they are not interned."
  (eq object1 object2))

(defun did->list (did)
  "Use this if you want a print form which can persist, since DIDs aren't PCLOS objects.
   This method is also useful when you want a component-by-component breakdown of a DID in
   printed form"
  (list
   (did/domain did)
   (did/repository did)
   (did/class did)
   (did/numeric-id did)))

(defun list->did (canonical-form)
  "Create a DID from the expression returned by EXPORT-VIEW on a DID."
  (make-distributed-identifier
   :domain     (or (first canonical-form) "")
   :repository (second canonical-form)
   :class      (third canonical-form)
   :numeric-id (or (fourth canonical-form) 0)))

(defun list-of-dids? (thing)
  (every #'distributed-identifier? thing))

(deftype did-list () `(or null (satisfies list-of-dids?)))

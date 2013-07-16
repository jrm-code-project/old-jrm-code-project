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

(in-package "CSF/CORE")

(proclaim (standard-optimizations))

(defclass canonical-object ()
  ()
  (:documentation
   "Abstract base class which must be inherited by any object intended to support canonical object
      behavior as defined in this module.  Canonical-object subtypes MUST NOT inherit distributed object,
      because their goals are contrary in nature.  Both may be exported, but canonical objects
      do not support distributed identity.")
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defclass canonical-class-dictionary ()
  ((entries :initform (make-instance 'persistent-vector :size 0)
            :reader canonical-class-dictionary/entries))
  (:documentation "A dictionary stored in the repository which maps all CANONICAL-OBJECT subtypes
      to dictionaries in which we search for existing elements before creating new ones.
      There is one canonical-class-dictionary per repository, responsible for maintaining the pool
      of all existing canonical-object elements, and resolving searches for existing elements.

      For now, the class dictionary is an alist (keyed by class), which in turn
      has a list of canonical-objects of that class.  Later this list of objects should be
      a hashtable for performance reasons (*FINISH*).
      Note that conversion to a hash table requires support for user-supplied equality predicates,
      which are not part of the common lisp standard, but are typically available in vendor lisps,
      (including Franz')")
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defun canonical-class-dictionary/find-object-class-symbol (canonical-class-dictionary class-symbol)
  "Finds the CLASS-SYMBOL in the CANONICAL-CLASS-DICTIONARY.
   RETURN: the 2nd half of the canonical-class-dictionary-entries 2-tuple (this is an
   astore-disk-vector-2 of the canonical objects of that class-symbol) or NIL if the class-symbol
   does not exist"
  (multiple-value-bind (class-symbols entry-vectors)
      (chunk 2 2
             (scan-persistent-vector
              (canonical-class-dictionary/entries canonical-class-dictionary)))
    (collect-first
     (choose (map-fn 'boolean (lambda (class-sym)
                                (eq class-sym class-symbol)) class-symbols)
             entry-vectors))))

(defgeneric canonical-class-dictionary-entry/find-object (canonical-class-dictionary-entry canonical-object)
  (:documentation
   "Takes the CANONICAL-CLASS-DICTIONARY-ENTRY (the astore-disk-vector-2 which is the 2nd half of
    the 2-tuple) and the CANONICAL-OBJECT.  Tries to find the CANONICAL-OBJECT in the vector (by
    some means that depends on the type of CANONICAL-OBJECT).  Either returns NIL (if not found)
    or the CANONICAL-OBJECT found")

  ;; note that, in general, adding new methods to this defgeneric is a schema change.

  ;; There is a method for this in core/cid-set.lsp for cid-objects

  ;; this is the default generic method
  (:method ((canonical-class-dictionary-entry persistent-vector) canonical-object)
    (debug-message 4 "canonical-class-dictionary-find-object unknown class-symbol ~a (~a)"
                   (type-of canonical-object) canonical-object)
    (collect-first
     (choose-if
      (lambda (probe)
        (objects-equalp probe canonical-object))
      ;; Note: the layout in this  vector is an unorganised list and
      ;;       so must be searched serially.  This is very inefficient and
      ;;       may need to be fixed.  If so, it is a schema change.  Act
      ;;       accordingly.     *FINISH*
      (scan-persistent-vector canonical-class-dictionary-entry)))))

(defgeneric canonical-class-dictionary-entry/add-object (canonical-class-dictionary-entry canonical-object)
  (:documentation
   "Takes the CANONICAL-CLASS-DICTIONARY-ENTRY (the astore-disk-vector-2 which is the 2nd half of
    the 2-tuple) and the CANONICAL-OBJECT.  Tries to add the CANONICAL-OBJECT in the vector (by
    some means that depends on the type of CANONICAL-OBJECT).  It assumes that the CANONICAL-OBJECT
    does NOT already exist in the dictionary.
    Returns the CANONICAL-OBJECT added")
  ;; note that, in general, adding new methods to this defgeneric is a schema change.

  ;; There is a method for this in core/cid-set.lsp for cid-objects

  ;; this is the default generic method
  (:method ((canonical-class-dictionary-entry persistent-vector) canonical-object)
    (debug-message 4 "canonical-class-dictionary-add-object unknown class-symbol ~a (~a)"
                   (type-of canonical-object) canonical-object)
    ;; Note: the layout in this astore vector is an unorganised list and
    ;;       so must be searched serially.  This is very inefficient and
    ;;       may need to be fixed.  If so, it is a schema change.  Act
    ;;       accordingly.     *FINISH*
    (persistent-vector-push canonical-class-dictionary-entry canonical-object)
    canonical-object))

(defun canonical-class-dictionary/find-object (canonical-class-dictionary canonical-object)
  "Find a CANONICAL-OBJECT whish is considered the equal of the supplied prototype CANONICAL-OBJECT.
   RETURN: NIL if no such match is found, in which case the caller will probably want to call
   CANONICAL-CLASS-DICTIONARY-ADD-OBJECT."
  (let* ((class-symbol (type-of canonical-object))
         (cde-entry                     ; the 2nd half of the 2-tuple
          (canonical-class-dictionary/find-object-class-symbol canonical-class-dictionary class-symbol)))
    (and cde-entry
         (canonical-class-dictionary-entry/find-object cde-entry canonical-object))))

(defun canonical-class-dictionary/add-object (canonical-class-dictionary canonical-object)
  "Add OBJECT to the canonical object dictionary.  CANONICAL-OBJECT must be of type CANONICAL-OBJECT, and
   is assumed not to exist in the dictionary (i.e. a search has already been performed).
   RETURN: the object added."
  (let* ((class-symbol (type-of canonical-object))
         (class-dictionary-entries (canonical-class-dictionary/entries canonical-class-dictionary))
         (cde-entry                     ; the 2nd half of the 2-tuple
          (canonical-class-dictionary/find-object-class-symbol canonical-class-dictionary class-symbol)))
    ;; Note: the layout in this astore vector is an unorganised list and
    ;;       so must be searched serially.  This is very inefficient and
    ;;       may need to be fixed.  If so, it is a schema change.  Act
    ;;       accordingly.     *FINISH*
    (unless cde-entry
      (debug-message 5 "creating a CDE entry")
      (setq cde-entry (make-instance 'persistent-vector :size 0))
      (persistent-vector-push2 class-dictionary-entries class-symbol cde-entry))
    (canonical-class-dictionary-entry/add-object cde-entry canonical-object)))

(defun canonical-class-dictionary/find-or-add-object (canonical-class-dictionary canonical-object)
  "Find the canonical object if it exists, or add it if it does not.

   **** WARNING **** WARNING **** WARNING **** WARNING **** WARNING **** WARNING ****

   If (the passed in) CANONICAL-OBJECT is not used as *the* canonical instance, it is deleted
   from the database, which has undesirable semantic effects in the event of its use prior to GC."
  (or (canonical-class-dictionary/find-object canonical-class-dictionary canonical-object)
      (canonical-class-dictionary/add-object canonical-class-dictionary canonical-object)))

;;; Forward references.
(declaim (ftype (function (t) boolean) distributed-object?)
         (ftype (function (t) canonical-class-dictionary) repository/canonical-class-dictionary))

(defun canonical-object/find-or-create (instance)
  "Find and return the canonical object which matches the prototype INSTANCE,
   or install INSTANCE as the canonical object in the repository and return it.
   Subtypes of canonical-object should return this function's value as their creation routine value.

   **** WARNING **** WARNING **** WARNING **** WARNING **** WARNING **** WARNING ****

   If INSTANCE is not used as *the* canonical instance, it is deleted from the database,
   which has undesirable semantic effects in the event of its use prior to GC.
   So the input argument should *never* be used after this function has returned it's result.

   New canonical objects should thus always be created as:

     (setq my-object (canonical-object-find-or-create (make-instance 'my-canonical-object-subtype ...)))

   And **** NEVER  **** as:
     (setq my-object (make-instance 'my-canonical-object-subtype ...))
     (canonical-object-find-or-create my-object)"
  ;; PERFORMANCE, it would be nice to have a way of doing the canonical object search without
  ;; first having to create the subtype instance to pass to the search, but for now, we'll live with it.
  ;; Fortunately, the number of canonical objects is meant to be small, or at least infrequently
  ;; added to even if there are many.

  ;; *FINISH*: the performance is actually worse than just the fact that a prototype instance is created,
  ;; considering that you had to allocate a persistent
  ;; object which will have to be deleted if you don't use it as "the" canonical object, that
  ;; we need to have a version of this function, and the equality function, and the search functions,
  ;; which operate on slot values in absense of a prototype instance.  For DEMO, we make do and
  ;; delete the prototype instance if we don't use it.
  (check-type instance canonical-object)
  (when (distributed-object? instance)
    (error "Canonical objects may not inherit DISTRIBUTED-OBJECT (erroneous instance is of type ~s)"
           (type-of instance)))
  (canonical-class-dictionary/find-or-add-object
   (repository/canonical-class-dictionary *repository*)
   instance))

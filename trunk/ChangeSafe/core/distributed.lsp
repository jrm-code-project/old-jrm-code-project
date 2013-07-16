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

(export '(distributed-object
          distributed-object-identifier))



;;;
;;; DISTRIBUTED-OBJECT
;;;

(defclass distributed-object ()
  ((repository-mapper :initarg :repository-mapper
                      :initform (error "Required initarg :repository-mapper omitted.")
                      :reader distributed-object/repository-mapper
                      :version-technique :nonversioned)
   (numeric-id :initarg :numeric-id
               :initform (error "Required initarg :numeric-id omitted.")
               :reader distributed-object/numeric-id
               :type positive-integer
               :version-technique :nonversioned))
  (:documentation
     "Any object which will have a distributed identity and conceptually exist simultaneously in
    more than one repository must support distributed protocols enabling access the object's
    DISTRIBUTED-IDENTIFIER and a handle for that identifier in any repository which attempts to
    dereference the handle.  This includes all instances of VERSIONED-OBJECT, and potentially a
    few other classes in the CORE package.  Any DISTRIBUTED-OBJECT instance supports a SYMMETRIC
    mapping of identifier to object and vice versa, such that if an object is known by a
    particular DID in one repository, it will have precisely the same DID in any other
    reposiotry, including transport packages.

    There are some low level things which have distributed identities but do not use the
    DISTRIBUTED-OBJECT protocols (and do not inherit the class).  These entities are few and are
    restricted to the CORE package.  They include things like versioned indexes, IONS, etc.
    These exceptions may also imply ASYMMETRIC mappings, such that no two repositories reflect
    the same maps for low level distributed objects which aren't DISTRIBUTED-OBJECT subtypes.

    Creation of these entities must be very careful to correctly fill out these slots.
    In the originating repositories, we refer to the repository mapper of the effecting repository.
    In non-originating repositories, we must derive the information from the canonical form
    of the DID which identifies the distributed object.  The content of the REPOSITIORY-MAPPER
    slot will be either a MAPPER subtype, or a canonical representation of a DID.

    DISTRIBUTED-OBJECT is a MIXIN class.")
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defmethod clos::initialize-instance :around ((instance distributed-object) &rest initargs &key &allow-other-keys)
  (let* ((local-mapper (repository/local-mapper *repository*))
         (key (intern (symbol-name (type-of instance)) (keyword-package)))
         (instance-mapper
          (or (mapper/resolve local-mapper key)
              ;; Instances of this type haven't existed in this repository before,
              ;; add the new class to the repository class map
              (let ((new-mapper (make-instance 'ordered-mapper
                                               :mapping-level (format nil "Class ~s" key)
                                               :key key
                                               :parent local-mapper)))
                (mapper/install-child-mapper local-mapper new-mapper)
                new-mapper)))
         (numeric-id (ordered-mapper/reserve-entry instance-mapper))
         (instance (apply #'call-next-method instance :repository-mapper local-mapper :numeric-id numeric-id initargs)))
    (check-type numeric-id positive-integer)
    (ordered-mapper/set-entry instance-mapper numeric-id instance)
    instance))

(defun distributed-object? (object)
  (typep object 'distributed-object))

(defmethod distributed-object-identifier ((object distributed-object))
  "For a DISTRIBUTED-OBJECT, return a DISTRIBUTED-IDENTIFIER representing that object.
   Note: the current implementation conses a canonical form of DID, and the resulting DID
   itself, so this isn't the cheapest of operations.  It's typically used only when
   printing or transporting objects."
  ;; I don't think we cons canonical DIDs.
  (let ((did (distributed-object-identifier (distributed-object/repository-mapper object)))
        (class (type-of object)))
    ;; We have a partial DID, fill in the class and numeric ID keys
    (assert (typep (distributed-object/numeric-id object) 'positive-integer))
    (assert (symbolp class))
    (make-distributed-identifier
     :domain      (did/domain     did)
     :repository  (did/repository did)
     :class       (when class (make-keyword (symbol-name class)))
     :numeric-id  (distributed-object/numeric-id object))))

(defmethod marshal-from-transaction ((element distributed-object))
  (distributed-object-identifier element))

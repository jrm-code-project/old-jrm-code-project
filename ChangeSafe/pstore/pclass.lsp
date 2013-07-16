;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002, 2003 ChangeSafe, LLC
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

;;; Persistent classes for the log-based persistent store.
(in-package "CSF/PERSISTENT-STORE")

(proclaim (standard-optimizations))

(eval-when (:compile-toplevel :load-toplevel)
  (export '(
            persistent-node-id
            persistent-standard-class
            persistent-standard-object
            persistent-slot-definition
            remake-instance
            slot-initial-value
            slot-definition-standard-initargs)))

(defclass persistent-standard-class (standard-class)
  ((schema-version :initarg :schema-version
                   :initform (error "Class initialization :schema-version omitted.")
                   :reader class-schema-version
                   :type (integer 0 *)))
  (:documentation "The metaclass for persistent objects."))

;; Oh well.
;; Have to suppress optimization of slot-value and (setf slot-value)
;; because we have modified those methods.
(defmethod clos::can-optimize-access-using-class ((class persistent-standard-class))
  nil)

(defun slot-name->persistent-initarg (slot-name)
  (intern (concatenate 'string
                       "PERSISTENT-INITARG-FOR-"
                       (symbol-name slot-name))
          (find-package "CSF/UTILITY")))

;;; Some slots are persistent and some are transient only.  The
;;; persistent slots will inherit from this class of slot definition
;;; so we can tell them apart.
(defclass persistent-slot-definition (clos:standard-slot-definition)
  ((persistent-initarg :initarg :persistent-initarg
                       :reader persistent-slot-definition/persistent-initarg)))

(defmethod clos:shared-initialize :around ((instance persistent-slot-definition) slot-names
                                           &rest initargs &key name &allow-other-keys)
  (apply #'call-next-method instance slot-names
         :persistent-initarg (slot-name->persistent-initarg name)
         initargs))

(defmethod clos:slot-definition-initargs ((slot-definition persistent-slot-definition))
  (cons (persistent-slot-definition/persistent-initarg slot-definition)
        (call-next-method)))

(defmethod slot-definition-standard-initargs ((slot-definition persistent-slot-definition))
  (cdr (clos:slot-definition-initargs slot-definition)))

;;; In theory, the inheritance list ought to be
;;; (persistent-slot-definition clos:direct-slot-definition) but there
;;; is apparently an initialization on
;;; clos:standard-direct-slot-definition that we need.
(defclass persistent-direct-slot-definition (persistent-slot-definition
                                             clos:standard-direct-slot-definition)
  ())

(defmethod clos:direct-slot-definition-class
  ;; lispworks bug: arglist is nonstandard
  #+lispworks ((class persistent-standard-class) slot-specification)
  (if (getf slot-specification :transient-only)
      (find-class 'clos:standard-direct-slot-definition)
      (find-class 'persistent-direct-slot-definition)))

(defclass persistent-effective-slot-definition (persistent-slot-definition
                                                clos:standard-effective-slot-definition)
  ())

(defmethod clos:effective-slot-definition-class
  ;; lispworks bug: arglist is nonstandard
  #+lispworks ((class persistent-standard-class) slot-specifications)
  (if (typep (car slot-specifications) 'persistent-slot-definition)
      (find-class 'persistent-effective-slot-definition)
      (find-class 'clos:standard-effective-slot-definition)))

(defun scan-class-persistent-effective-slots (class)
  (declare (optimizable-series-function))
  (choose-if (lambda (slot)
               (typep slot 'persistent-slot-definition))
             (scan 'list (clos:class-effective-slots class))))

;;; The :transient-only option is rejected during class slot
;;; canonicalization.  This function fixes that by stripping off the
;;; :transient-only option before canonicalization and replacing it
;;; after.
#+lispworks
(defmethod clos::canonicalize-defclass-slot ((prototype persistent-standard-class) slot)
  ;; make sure the slot is a list.
  (unless (consp slot)
    (setq slot (list slot)))
  (let ((transient-only (or (getf (cdr slot) :transient-only)
                            nil)))
    (remf (cdr slot) :transient-only)
  `(,@(call-next-method prototype slot)
      :transient-only ,transient-only)))

;;; The :schema-version initializer for persistent metaclasses is
;;; rejected during class canonicalization in lispworks.  This
;;; function fixes that by stripping off the :schema-version option
;;; before canonicalizing the other options, then replacing it.
#+lispworks
(defmethod clos::canonicalize-class-options ((prototype persistent-standard-class) options)
  (let ((schema-version-option (find :schema-version options :key #'car)))
    `(,@(call-next-method prototype (delete schema-version-option options))
        ,@(when schema-version-option
            schema-version-option))))

;;; All persistent objects inherit from persistent-standard-object.
(defclass persistent-standard-object (standard-object)
  ((pstore :accessor persistent-standard-object/pstore
           :initform *default-persistent-store*)
   (node-id :accessor persistent-standard-object/node-id)
   (node-index :accessor persistent-standard-object/node-index)))

(defmethod objects-equalp ((left persistent-standard-object) right) nil)
(defmethod objects-equalp (left (right persistent-standard-object)) nil)

(defmethod objects-equalp ((left persistent-standard-object) (right persistent-standard-object))
  (or (eq left right)
      (and (eq (persistent-store/guid (persistent-standard-object/pstore left))
               (persistent-store/guid (persistent-standard-object/pstore right)))
           (= (persistent-standard-object/node-id left)
              (persistent-standard-object/node-id right))
           (= (persistent-standard-object/node-index left)
              (persistent-standard-object/node-index right)))))

;;; Only persistent-standard-object and its subclasses are valid
;;; superclasses for persistent-objects
(defmethod clos:validate-superclass ((class persistent-standard-class) superclass)
  (subtypep superclass 'persistent-standard-object))

;;; If you leave the direct superclass list blank, it will
;;; be defaulted to persistent-standard-object.
(defmethod clos:shared-initialize ((class persistent-standard-class) slot-names &rest initargs)
  "When initializing a persistent-standard-class, ensure that
   persistent-standard-object appears in the direct-superclasses."
  (let ((direct-superclasses (getf initargs :direct-superclasses)))
    (when (or (null direct-superclasses)
              (and (null (cdr direct-superclasses))
                   (eq (car direct-superclasses) (find-class 'standard-object))))
      (setf (getf initargs :direct-superclasses) (list (find-class 'persistent-standard-object))))
    (call-next-method)))

(defvar *dereference-persistent-slots* T
  "When T, persistent slot refs return the object rather than the OID.")

(defclass persistent-node-id ()
  ((node-id :initarg :node-id :reader persistent-node-id/number)
   (index   :initarg :node-index :reader persistent-node-id/index))
  (:documentation "What gets returned from a slot if there isn't a transaction in progress."))

(defmethod print-object ((object persistent-node-id) stream)
  (print-unreadable-object (object stream :type t)
    (princ (persistent-node-id/number object) stream)
    (unless (= (persistent-node-id/index object) +node-index-scalar+)
      (write-char #\[ stream)
      (princ (persistent-node-id/index object) stream)
      (write-char #\] stream))))

;;; The persistent slots in a persistent object actually contain the
;;; node-id of the value that is supposed to be there.  This is for
;;; two reasons:  to break circularity and to allow atomic rollback
;;; via the object map.
(defmethod clos:slot-value-using-class ((class persistent-standard-class)
                                        (object persistent-standard-object)
                                        slot)
  (declare #.(performance-optimizations))
  (let ((slot-descriptor (find slot (clos:class-effective-slots class) :key #'clos:slot-definition-name)))
    (if (and (typep slot-descriptor 'persistent-slot-definition)
             *dereference-persistent-slots*
             (boundp '*current-transaction*))
        (persistent-object/find (persistent-standard-object/pstore object) (call-next-method))
        (make-instance 'persistent-node-id
                       :node-id (call-next-method)
                       :node-index +node-index-scalar+))))

(defmethod (setf clos:slot-value-using-class) (new-value (class persistent-standard-class)
                                                         (object persistent-standard-object)
                                                         slot)
  (declare #.(performance-optimizations))
  (let ((slot-descriptor (find slot (clos:class-effective-slots class) :key #'clos:slot-definition-name)))
    (if (typep slot-descriptor 'persistent-slot-definition)
        (error "Persistent slot ~s in ~s is immutable." slot-descriptor object)
        (call-next-method))))

(defvar *restoring-instance* nil
  "Bound to an instance being restored.")

(declaim (ftype (function (persistent-slot-definition)
                          (values list t symbol)) effective-slot-initialization-info)
         (inline effective-slot-initialization-info))

(defun effective-slot-initialization-info (slot)
  "Return 3 values of interest in initializing this slot:

   First, the user-supplied initargs for this slot.  These will
   be matched against the initargs supplied to shared-initialize.

   Second, the initfunction, which will be called if none of the
   initargs match.

   Third, the persistent-initarg which will be passed to the next
   layer down to stuff in a value."
  (declare #.(performance-optimizations))
  (let ((initargs (clos:slot-definition-initargs slot)))
    (values (cdr initargs)
            (clos:slot-definition-initfunction slot)
            (car initargs))))

(declaim (ftype (function (list list t t) t) slot-initial-value)
         (inline slot-initial-value))

(defun slot-initial-value (initargs slot-initargs slot-initfunction default)
  "Finds the leftmost initarg that matches one of SLOT-INITARGS,
   or invokes SLOT-INITFUNCTION to determine what the initial value
   of a slot should be.  IF there is no SLOT-INITFUNCTION, default is
   returned.

   Performance critical!"
  (declare #.(performance-optimizations))
  (do* ((plist initargs (cddr plist))
        (key (car plist) (car plist))
        (value (cadr plist) (cadr plist)))
      ((null plist) (if slot-initfunction
                        (funcall slot-initfunction)
                        default))
    (when (member key slot-initargs :test #'eq)
      (return-from slot-initial-value value))))

(declaim (ftype (function (persistent-store t) non-negative-integer) slot-value->persistent-node)
         (inline slot-value->persistent-node))

(defun slot-value->persistent-node (persistent-store value)
  "Given the value to place in a persistent node, return
   the node-id for the persistent version of the value.

   If value is a persistent-instance, just get the node-id,
   but if value is a lisp object, persist it."
  (declare #.(performance-optimizations))
  (if (typep value 'persistent-standard-object)
      (progn
        (assert (eq (persistent-standard-object/pstore value) persistent-store))
        (persistent-standard-object/node-id value))
      (persistent-object/save value persistent-store)))

(defun compute-persistent-slot-initargs (class persistent-store initargs)
  "Scan over the persistent effective slots in CLASS,
   determine the value to be assigned to each slot, either
   from the initargs or from the initfunction, then
   using the persistent-initarg as a key, construct a
   plist for use in the persistent initializer and in
   the inner call to shared-initialize."
  (declare #.(performance-optimizations))
  (let ((result nil))
    (iterate (((slot-initargs slot-initfunction slot-persistent-initarg)
               (map-fn '(values t t symbol)
                       #'effective-slot-initialization-info
                       (scan-class-persistent-effective-slots class))))
      (let ((initial-value (slot-initial-value initargs slot-initargs slot-initfunction
                                               (clos::slot-unbound-value))))
        (unless (eq initial-value (clos::slot-unbound-value))
          (push slot-persistent-initarg result)
          (push (slot-value->persistent-node persistent-store initial-value) result))))
    (nreverse result)))

;; Override the primary shared-instance method
;; in order to deal with persistent slots.
;; This actually has to perform because instances are
;; immutable:  we will be consing more of them.
(defmethod clos:shared-initialize ((instance persistent-standard-object) slot-names
                                   &rest initargs
                                   &key persistent-store node-id node-index
                                   &allow-other-keys)

  (if (eq instance *restoring-instance*)
      (call-next-method)
      ;; If we are being called from elsewhere,
      ;; we have to wrap the initargs and initforms
      ;; in persistent-objects and create an initializer
      ;; for this object.
      (let* ((class (class-of instance))

             (init-plist (compute-persistent-slot-initargs class
                                                           (or persistent-store  *default-persistent-store*)
                                                           initargs))

             (node-id (persistent-object/save
                       (make-initializer class
                                         (class-schema-version class)
                                         init-plist)
                       (or persistent-store  *default-persistent-store*)
                       node-id)))

        (apply #'call-next-method instance slot-names (nconc init-plist initargs))

        (setf (persistent-standard-object/node-id instance) node-id)
        (setf (persistent-standard-object/node-index instance) node-index)
        (setf (object-map-info/%cached-value
               (persistent-object/find-object-map-info
                (or persistent-store  *default-persistent-store*)  node-id))
              instance)

        instance)))

(defmethod restore-instance ((class persistent-standard-class) schema persistent-store node-id node-index init-plist)
  "The standard restore method for a persistent object."
  (cond ((< (class-schema-version class) schema)
         (error 'changesafe-schema-mismatch
                :format-control "Schema of object in database (~d) appears newer than that in memory (~d)."
                :format-arguments (list schema (class-schema-version class))))
        ;; This should not happen because when we upgrade the schema we're supposed
        ;; to include schema migration code.  But if we didn't we end up here.
        ((> (class-schema-version class) schema)
         (warn 'changesafe-schema-upgrade
               :format-control "Schema of object in database (~d) is older than that in memory (~d)."
               :format-arguments (list schema (class-schema-version class))))
        ;; If it is the same, we're ok.
        (t nil))

  (let ((*restoring-instance* (allocate-instance class nil)))
    (setf (persistent-standard-object/pstore *restoring-instance*) persistent-store)
    (setf (persistent-standard-object/node-id *restoring-instance*) node-id)
    (setf (persistent-standard-object/node-index *restoring-instance*) node-index)
    (apply #'clos::shared-initialize *restoring-instance* t init-plist)
    *restoring-instance*))

(defgeneric remake-instance (original-instance &rest initargs)
  (:documentation "Recreates original-instance with new initargs
                   installing the new instance in the object map at the old location.")
  (:method ((original-instance persistent-standard-object) &rest initargs)
    (debug-message 5 "Remaking ~s with ~s" original-instance initargs)
    (let ((class (class-of original-instance)))
      (apply #'clos::shared-initialize (allocate-instance class) t
             :persistent-store (persistent-standard-object/pstore original-instance)
             :node-id (persistent-standard-object/node-id original-instance)
           (append initargs
                   (multiple-value-bind (slot-persistent-initargs values)
                       (map-fn '(values symbol integer)
                               (lambda (persistent-slot)
                                 (values (car (clos:slot-definition-initargs persistent-slot))
                                         (slot-value original-instance
                                                     (clos:slot-definition-name persistent-slot))))
                               (choose-if (lambda (persistent-slot)
                                            (clos:slot-boundp-using-class class original-instance
                                                                          (clos:slot-definition-name persistent-slot)))
                                          (scan-class-persistent-effective-slots class)))
                     (collect-plist
                      slot-persistent-initargs
                      values)))))))

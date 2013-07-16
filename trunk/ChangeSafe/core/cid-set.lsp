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

(in-package "CSF/CORE")

(proclaim (standard-optimizations))

(eval-when (:load-toplevel :execute)
  (export '(
            cid
            cid-set
            cid-set?
            cid-set/adjoin
            cid-set/empty?
            cid-set/equal?
            cid-set/exclusive-or
            cid-set/highest-active-cid
            cid-set/intersection
            cid-set/intersection?
            cid-set/last-cid
            cid-set/member
            cid-set/remove
            cid-set/repository
            cid-set/union
            collect-cid-set-union

            cid-set/empty

            bitmap->cid-set
            list->cid-set
            range->cid-set
            )))

(deftype cid () `(integer 0 ,array-dimension-limit))


;;;
;;; CID-SET
;;;

;;
;; Unless otherwise noted, CID-SETS ARE NOT CURRENTLY PERSISTENT.
;; Since classes must be either persistent or not persistent, We'll
;; put bulk of functionality into transient classes, and make special
;; persistent versions which can be translated into transient ones and
;; vice versa.  If we must, laster, we'll add methods to the
;; persistent copies too.
;;
;; IMPLEMENTATION NOTE: we might use ASTORE:ENCODE-IN-DATABASE and DECODE-FROM-DATABASE
;; primitives to achieve persistence on these classes without a duplicate class schema.
;;

(defgeneric cid-set->bitmap (cid-set) 
  (:documentation "Returns the cid-set as a bitmap."))

(defgeneric cid-set/repository (cid-set)
  (:documentation "Returns the repository the cid-set is associated with."))

(defclass cid-set ()
  ;; The repository slot is for sanity checks.
  ;; CID-SETs don't make sense outside their own repository.
  ((repository :initarg :repository :reader cid-set/repository
               :initform (error "Required initarg :repository omitted.")))
  (:documentation
   "Abstract class representing a set of change-set identifiers which are considered active in
    viewing a versioned world.

    The status of CID 0 is always UNDEFINED.  Note that vector-based representations
    may chose to represent CID 0 or not, however the generic function interfaces which
    query CIDs should accept the CID value for what they are, and not require the caller
    to adjust for zero or one-based index schemes.  Valid CIDs are always numbers starting with 1.
    CID queries must then support numbers starting with one. "))

(defmethod marshal-from-transaction ((element cid-set))
  element)

(defun cid-set? (object)
  (typep object 'cid-set))

(defgeneric cid-set/adjoin (original cid)
  (:documentation "Create a cid set like ORIGINAL with CID included."))

(defgeneric cid-set/empty? (cid-set)
  (:documentation "Return T iff there are no CIDS on in the CID-SET."))

(defgeneric cid-set/equal? (left right)
  (:documentation "Return T iff the same CIDS are on in both LEFT and RIGHT.
    It actually returns two values.  The first is as described above.  The second
    is either T or NIL.  T is returned when we had to do a detailed examination (i.e.
    expensive) to determine the first value returned.")
  (:method :around (left right)
           ;; Short circuit expensive test if objects are eq.
           (if (eq left right)
               (values t nil);; cheap examination
               (call-next-method)))
  (:method :before (left right)
           (assert (eq (cid-set/repository left)
                       (cid-set/repository right)))))

(defgeneric cid-set/exclusive-or (left right)
  (:documentation
   "Return a CID-SET that represents those cids that appear in either left or right,
 but not both.  This represents the changes between these two cid sets.")
  (:method :before (left right)
           (assert (eq (cid-set/repository left)
                       (cid-set/repository right)))))

(defgeneric cid-set/highest-active-cid (cid-set)
  (:documentation
   "Return the CID of the highest numbered active cid in the cid-set, or NIL if no cids are active."))

(defgeneric cid-set/intersection (left right)
  (:documentation
   "Return the intersection of the two cid-sets.")
  (:method :before (left right)
           (assert (eq (cid-set/repository left)
                       (cid-set/repository right)))))

(defgeneric cid-set/intersection? (left right)
  (:documentation
   "Return T if there are any CIDs in both left and right.")
  (:method :around (left right)
    ;; Of course they intersect if they are eq.
    (or (eq left right)
        (call-next-method)))
  (:method :before (left right)
           (assert (eq (cid-set/repository left)
                       (cid-set/repository right)))))

(defgeneric cid-set/last-cid (cid-set)
  (:documentation "Biggest cid that can be accomodated by the underlying cid-set."))

(defgeneric cid-set/member (cid-set cid)
  (:documentation
   "Return NIL if the CID is inactive in the specified CID-set, non-NIL if it is active.
    Subtypes must explicitly support CIDs of any value, even if they're beyond the range
    of those actually mapped by a specific implementation."))

(defgeneric cid-set/remove (original cid)
  (:documentation "Create a cid set like ORIGINAL with CID omitted."))

(defgeneric cid-set/union (left right)
  (:documentation "Create a cid set that is the union of left and right."))

;; Return the union of all cid-sets in series
(defun collect-cid-set-union (repository series)
  (declare (optimizable-series-function))
  (collect-fn 'cid-set
              (lambda () (cid-set/empty repository))
              #'cid-set/union
              series))


;;;
;;; DENSE-CID-SET - non-resizeable, most efficient access subtype of CID-SET
;;;

(defclass dense-cid-set (cid-set)
  ((bitmap :initarg :bitmap
           :initform (error "Required initarg :bitmap omitted.")
           :type simple-bit-vector
           :reader dense-cid-set/bitmap))
  (:documentation
   "A CID-set which expects to densely represent many CIDs, and uses the most efficient
    representation possible, typically something involving bitmaps, and later potentially employing
    nodal compression techniques in hierarchical bitmap representations."))

(defmethod print-object ((object dense-cid-set) stream)
  (print-unreadable-object (object stream :type t)
    (princ (dense-cid-set/bitmap object) stream)
    (write-char #\space stream)
    (princ (cid-set/repository object) stream)))

(defmethod cid-set->bitmap ((object dense-cid-set))
  (dense-cid-set/bitmap object))

;(defmethod serialize ((object dense-cid-set) stream symbol-table)
;  (declare (ignore symbol-table))
;  (let ((bitmap (dense-cid-set/bitmap object)))
;    (write-byte serialization-code/dense-cid-set stream)
;    (write-fixnum (simple-bit-vector-length bitmap) stream)
;    (write-simple-bit-vector bitmap stream)))

;(defmethod deserialize-dispatch ((code (eql serialization-code/dense-cid-set)) stream symbol-table)
;  (debug-message 5 "deserialize-dispatch cid-set, *repository* is ~s" *repository*)
;  (let* ((length (read-fixnum stream))
;         (bitmap (read-simple-bit-vector length stream)))
;    (make-instance 'dense-cid-set :repository *repository* :bitmap bitmap)))

(defun cid-set/empty (repository)
  (make-instance 'dense-cid-set
                 :repository repository
                 :bitmap #*0))

(defun dense-cid-set/size (cid-set)
  (check-type cid-set dense-cid-set)
  (simple-bit-vector-length (dense-cid-set/bitmap cid-set)))

(defmethod cid-set/adjoin ((original dense-cid-set) cid)
  (check-type cid cid)
  (let* ((bv (dense-cid-set/bitmap original))
         (length (simple-vector-1b-length bv)))
    (if (and (< cid length)
             (= (sbit bv cid) 1))
        original
        (let ((new-bitmap (simple-vector-1b-allocate (max length (1+ cid)))))
          (unless (zerop length)
            (simple-vector-1b-copy bv new-bitmap))
          (setf (sbit new-bitmap cid) 1)
          (make-instance 'dense-cid-set
                         :repository (cid-set/repository original)
                         :bitmap new-bitmap)))))

(defmethod cid-set/empty? ((cid-set dense-cid-set))
  (bit-set-empty? (dense-cid-set/bitmap cid-set)))

(defmethod cid-set/equal? ((left dense-cid-set) (right dense-cid-set))
  (values (zero-extended-bit-set-equal?
           (dense-cid-set/bitmap left)
           (dense-cid-set/bitmap right)) t)) ;; expensive examination

(defmethod cid-set/exclusive-or ((left dense-cid-set) (right dense-cid-set))
  ;; Should probably return a sparse cid set as that is the most likely outcome.
  (make-instance 'dense-cid-set
                 :repository (cid-set/repository left)
                 :bitmap (zero-extended-bit-set-xor
                          (dense-cid-set/bitmap left)
                          (dense-cid-set/bitmap right))))

(defmethod cid-set/intersection? ((left dense-cid-set) (right dense-cid-set))
  (zero-extended-bit-set-and?
   (dense-cid-set/bitmap left)
   (dense-cid-set/bitmap right)))

(defmethod cid-set/last-cid ((cid-set dense-cid-set))
  (- (dense-cid-set/size cid-set) 1)) ;; -1 since the indexing is zero based

(defmethod cid-set/member ((cid-set dense-cid-set) cid)
  (check-type cid cid)
  ;; For now, we always consider zero an active cid, though it is properly used only as an
  ;; insertion point.
  (or (= cid +cid-unassigned+)
      (let ((bitmap (dense-cid-set/bitmap cid-set)))
        ; (declare (type simple-bit-vector bitmap))
        (and (< cid (simple-bit-vector-length bitmap))
             (= (sbit bitmap cid) 1)))))

(defmethod cid-set/remove ((original dense-cid-set) cid)
  (check-type cid cid)
  (let* ((bv (dense-cid-set/bitmap original))
         (length (simple-vector-1b-length bv)))
    (if (>= cid (simple-vector-1b-length bv))
        original
        (let ((new-bitmap (simple-vector-1b-allocate length)))
          (simple-vector-1b-copy bv new-bitmap)
          (setf (sbit new-bitmap cid) 0)
          (make-instance 'dense-cid-set
                         :repository (cid-set/repository original)
                         :bitmap new-bitmap)))))

(defmethod cid-set/union ((left dense-cid-set) (right dense-cid-set))
  (make-instance 'dense-cid-set
                 :repository (cid-set/repository left)
                 :bitmap (zero-extended-bit-set-or 
                          (dense-cid-set/bitmap left)
                          (dense-cid-set/bitmap right))))

(defun list->cid-set (repository list)
  "Given a list of CIDs, return a cid-set that represents them."
  (make-instance 'dense-cid-set
                 :repository repository
                 :bitmap (let* ((size (cond ((null list) 0)
                                            ((null (cdr list)) (car list))
                                            (t (reduce #'max list))))
                                (bitmap (simple-vector-1b-allocate (1+ size))))
                           (simple-vector-1b-fill bitmap 0)
                           (dolist (cid list)
                             (setf (simple-vector-1b-ref bitmap cid) 1))
                           bitmap)))

(defun range->cid-set (repository start end)
  (let ((bitmap (simple-vector-1b-allocate end)))
    (fill bitmap 0 :start 0 :end start)
    (fill bitmap 1 :start start :end end)
    (make-instance 'dense-cid-set :repository repository :bitmap bitmap)))

;;; This is gross.
;;; The basis cid set in a transaction has to be in the database,
;;; but it really can't be correctly recovered because the repository
;;; field isn't ready yet.  So we strip the repository info going in
;;; and paste it back on when it comes out.


(defun bitmap->cid-set (repository bitmap)
  (check-type bitmap simple-vector-1b)
  (make-instance 'dense-cid-set :repository repository :bitmap bitmap))


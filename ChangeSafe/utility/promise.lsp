;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright (c) 2000 Content Integrity, Inc.
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
;;;; File Name:     promise.lsp
;;;; Author:        Joe Marshall
;;;;
;;;; Module Description:  Promise
;;;;
;;;;
;;;;  Implements call-by-need evaluation.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CSF/UTILITY")

(eval-when (:load-toplevel :execute)
  (export '(delay
            force
            promise?
            promise-forced?
            lazy-cons
            lazy-car
            lazy-cdr
            lazy-list/tail
            lazy-list/last
            lazy-list/elt
            vector->lazy-list
            subvector->lazy-list
            subvector->reverse-lazy-list
            )))

(proclaim (standard-optimizations))

(defstruct (promise
            (:constructor make-promise (thunk-or-values))
            (:copier nil)
            (:predicate promise?))
  (forced? nil)
  (thunk-or-values nil))

(defmethod print-object ((object promise) stream)
  (call-with-deeper-print-level stream
    (lambda ()                                
      (print-unreadable-object (object stream :type t :identity t)
        (if (promise-forced? object)
            (let ((vals (promise-thunk-or-values object)))
              (format stream "~s" (if (and (consp vals)
                                           (null (cdr vals)))
                                      (car vals)
                                      (cons 'values vals))))
            (write-string "unforced" stream))))))

(defmacro delay (&body expressions)
  "Delay the evaluation of EXPRESSIONS by making a promise.
   Expressions will be evaluated when PROMISE is first forced."
  `(THE PROMISE (MAKE-PROMISE (LAMBDA () ,@expressions))))

(defun pseudodelay (&rest values)
  "Evaluate expressions now packaging them in a pre-forced promise.
   Useful when you have to pass something delayed, but have the
   data now and want to avoid the overhead of really delaying it."
  (let ((promise (make-promise values)))
    (setf (promise-forced? promise) t)
    promise))

(declaim (ftype (function (promise) t) %force %force-1)
         (inline %force %force-1))

(defun %force (promise)
  "Helper for when it is known that promise is of the correct type.
   Do not use externally to this file."
  (declare (type promise promise)
           #.(performance-optimizations))
  (unless (promise-forced? promise)
    (let ((vals (multiple-value-list (funcall (promise-thunk-or-values promise)))))
      (setf (promise-forced? promise) t
            (promise-thunk-or-values promise) vals)))
  (let ((vals (promise-thunk-or-values promise)))
    (if (and (consp vals)
             (null (cdr vals)))
        (car vals)
        (values-list vals))))

(defun %force-1 (promise)
  "Helper for when it is known that promise is of the correct type,
   and that what is promised is exactly one value.
   Do not use externally to this file."
  (declare (type promise promise)
           #.(performance-optimizations))
  (unless (promise-forced? promise)
    (let ((vals (list (funcall (promise-thunk-or-values promise)))))
      (setf (promise-forced? promise) t
            (promise-thunk-or-values promise) vals)))
  (car (promise-thunk-or-values promise)))

(defun force (promise)
  "FORCE evaluation of PROMISE.  If PROMISE has been previously forced, return
   the cached values."
  (check-type promise promise)
  (%force promise))

(defstruct (lazy-list
            (:conc-name lazy-list/)
            (:constructor make-lazy-list (head tail))
            (:copier nil)
            (:predicate lazy-list?))
  (head nil :read-only t)
  (tail (delay nil) :read-only t :type promise))

(defmethod print-object ((object lazy-list) stream)
  (call-with-deeper-print-level stream
    (lambda ()                                
      (print-unreadable-object (object stream :type t :identity t)
        (write-char #\( stream)
        (if (and (numberp *print-length*)
                 (zerop *print-length*))
            (write-string "..." stream)
            (progn (call-with-deeper-print-level stream
                     (lambda ()
                       (format stream "~s" (lazy-list/head object))))
                   (do ((count 1 (1+ count))
                        (ll (lazy-list/tail object) (lazy-list/tail (%force-1 ll))))
                       ((or (and *print-length* (>= count *print-length*))
                            (not (promise-forced? ll))) (write-string " ..." stream))
                     (let ((value (%force-1 ll)))
                       (cond ((null value) (return));; reached end of lazy list
                             ((not (lazy-list? value))
                              (write-string " . " stream)
                              (call-with-deeper-print-level stream
                                (lambda () (format stream "~s" value)))
                              (return))
                             (t (call-with-deeper-print-level stream
                                  (lambda ()
                                    (format stream " ~s" (lazy-list/head value))))))))))
        (write-char #\) stream)))))

(defmacro lazy-cons (head tail)
  `(MAKE-LAZY-LIST ,head (DELAY ,tail)))

(defun lazy-pseudocons (head tail)
  "Like lazy-cons, but doesn't delay the tail."
  (make-lazy-list head (pseudodelay tail)))

(declaim (ftype (function (lazy-list) t) %lazy-car %lazy-cdr)
         (inline %lazy-car %lazy-cdr))

(defun %lazy-car (lazy-list)
  (declare #.(performance-optimizations))
  (lazy-list/head (the lazy-list lazy-list)))

(defun %lazy-cdr (lazy-list)
  (declare #.(performance-optimizations))
  (%force-1 (lazy-list/tail (the lazy-list lazy-list))))

(defun lazy-car (lazy-list)
  (check-type lazy-list lazy-list)
  (%lazy-car lazy-list))

(defun lazy-cdr (lazy-list)
  (check-type lazy-list lazy-list)
  (%lazy-cdr lazy-list))

(defun lazy-list/%last (lazy-list)
  (declare (type lazy-list lazy-list)
           #.(performance-optimizations))
  (let ((this (%lazy-car lazy-list))
        (next (%lazy-cdr lazy-list)))
    (cond ((null next) this)
          ((lazy-list? next) (lazy-list/%last lazy-list))
          (t (error "improper lazy list")))))

(defun lazy-list/last (lazy-list)
  "Unlike the LAST function, returns the last ELEMENT of a lazy list."
  (check-type lazy-list lazy-list)
  (lazy-list/%last lazy-list))

(defun lazy-list/%elt (lazy-list index)
  (declare (type lazy-list lazy-list)
           (type non-negative-fixnum index))
  (do ((i  index (1- i))
       (ll lazy-list (let ((next (%lazy-cdr ll)))
                       (if (lazy-list? next)
                           next
                           (error "Ran out of elements in lazy-list/elt")))))
      ((zerop i) (%lazy-car ll))
    (declare (type fixnum i)
             (type lazy-list ll))))

(defun lazy-list/elt (lazy-list index)
  (check-type lazy-list (or null lazy-list))
  (check-type index non-negative-fixnum)
  (lazy-list/%elt lazy-list index))

(defun list->lazy-list (list)
  (cond ((consp list)
         (lazy-pseudocons (car list) (list->lazy-list (cdr list))))
        ((null list) nil)
        (t (error "Improper list ~s" list))))

(defun lazy-list/%fold-right-delayed (combiner initial-value lazy-list)
  (if (null lazy-list)
      initial-value
      (funcall combiner (prog1 (%lazy-car lazy-list)
                          (setq lazy-list (lazy-list/tail lazy-list)))
               (delay (lazy-list/%fold-right-delayed
                       combiner initial-value
                       (force lazy-list))))))

(defun lazy-list/fold-right-delayed (combiner initial-value lazy-list)
  (check-type lazy-list (or null lazy-list))
  (lazy-list/%fold-right-delayed combiner initial-value lazy-list))

(defun lazy-list->list (lazy-list)
  (lazy-list/fold-right-delayed
   (lambda (a delayed-b)
     (cons a (force delayed-b)))
   nil lazy-list))

(defun subvector->lazy-list (vector start end)
  (declare (type vector vector)
           (type array-index start end))
  (unless (= start end)
    (lazy-pseudocons (elt vector start)
                     (subvector->lazy-list vector (1+ start) end))))

(defun vector->lazy-list (vector)
  (declare (type vector vector))
  (subvector->lazy-list vector 0 (vector-length vector)))

(defun subvector->reverse-lazy-list (vector start end)
  (unless (= start end)
    (lazy-pseudocons (aref vector (- end 1))
                     (subvector->reverse-lazy-list vector start (- end 1)))))

(defun lazy-list/map (func lazy-list)
  (check-type lazy-list (or null lazy-list))
  (unless (null lazy-list)
    (lazy-cons (funcall func (%lazy-car lazy-list))
               (lazy-list/map func (%lazy-cdr lazy-list)))))

(defun lazy-list/mapc (func lazy-list)
  "Analagous to mapc, but note that this FORCES the entire lazy list."
  (check-type lazy-list (or null lazy-list))
  (do ((tail lazy-list (%lazy-cdr tail)))
      ((null tail))
    (funcall func (%lazy-car tail))))

(defun lazy-list/append-delayed (left delayed-right)
  (if (null left)
      (force delayed-right)
      (lazy-cons (prog1 (%lazy-car left)
                   (setq left (lazy-list/tail left)))
                 (lazy-list/append-delayed (force left) delayed-right))))

(defun lazy-list/interleave-delayed (left delayed-right)
  (if (null left)
      (force delayed-right)
      (lazy-cons (prog1 (%lazy-car left)
                   ;; free some storage
                   (setq left (lazy-list/tail left)))
                 (lazy-list/interleave-delayed
                  (force delayed-right)
                  left))))

(defun lazy-list/flatten (lazy-list)
  (lazy-list/fold-right-delayed #'lazy-list/append-delayed nil lazy-list))

(defun lazy-list/flatten-interleaved (lazy-list)
  (lazy-list/fold-right-delayed #'lazy-list/interleave-delayed nil lazy-list))

(defun lazy-list/flatmap (func lazy-list)
  (lazy-list/flatten (lazy-list/map func lazy-list)))

(defun lazy-list/flatmap-interleaved (func lazy-list)
  (lazy-list/flatten-interleaved (lazy-list/map func lazy-list)))

(defun lazy-list/filter (func lazy-list)
  (cond ((null lazy-list) nil)
        ((funcall func (%lazy-car lazy-list))
         (lazy-cons (%lazy-car lazy-list)
                    (lazy-list/filter func (%lazy-cdr lazy-list))))
        (t (lazy-list/filter func (%lazy-cdr lazy-list)))))

(defmacro nested-mapping (bindings restriction-form &body result-forms)
  (with-unique-names (tuple)
    `(lazy-list/map
      (lambda (,tuple) (apply (lambda ,(map 'list #'car bindings)
                                ,@result-forms)
                              ,tuple))
      (lazy-list/filter
       (lambda (,tuple)
         (apply (lambda ,(map 'list #'car bindings) ,restriction-form)
                ,tuple))
       ,(do ((tailb (reverse bindings) (cdr tailb))
             (result `(LAZY-CONS (LIST ,@(map 'list #'car bindings)) NIL)
                     `(LAZY-LIST/FLATMAP-INTERLEAVED
                       (LAMBDA (,(caar tailb))
                               ,result)
                       ,(cadar tailb))))
            ((null tailb) result))))))

(defun lazy-list/enumerate-interval (low high)
  (when (< low high)
    (lazy-cons low (lazy-list/enumerate-interval (1+ low) high))))

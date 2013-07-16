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

;;; Persistent nodes are entries in a wt-tree kept in
;;; a persistent file.

(defstruct (persistent-node
            (:conc-name persistent-node/)
            (:constructor %%make-persistent-node (tree-data
                                                 %cached-left-child
                                                 %cached-right-child
                                                 info))
            (:copier nil)
            (:predicate persistent-node?))

  ;; The persistent node data contains 4 32-bit fields:
  ;; the file offset of the left child,
  ;; the file offset of the right child,
  ;; the weight of this node, and
  ;; the offset of this node (transient only)
  (tree-data (simple-vector-32b-allocate 4)
        :type (simple-vector-32b 4) :read-only t)

  (%cached-left-child  nil) ;; transient left child
  (%cached-right-child nil) ;; transient right child

  ;; The info is where the particular type of persistent node
  ;; is kept.
  (info  nil :read-only t)
  )

(defmethod print-object ((node persistent-node) stream)
  (print-unreadable-object (node stream)
    (write-string "PERSISTENT-NODE for " stream)
    (princ (persistent-node/info node) stream)))

(deftype file-offset () `(unsigned-byte 32))

(declaim (ftype (function (persistent-node) non-negative-fixnum) persistent-node/%weight)
         (ftype (function (persistent-node) file-offset) persistent-node/%left-child-offset)
         (ftype (function (persistent-node) file-offset) persistent-node/%right-child-offset)
         (ftype (function (persistent-node) file-offset) persistent-node/%node-offset)
         (ftype (function ((simple-vector-32b 4) t t t) persistent-node) %%make-persistent-node)
         (ftype (function (non-negative-fixnum ; weight
                           file-offset         ; left-child-offset
                           file-offset         ; right-child-offset
                           file-offset         ; node-offset
                           t
                           t
                           t) persistent-node) %make-persistent-node)
         (inline persistent-node/%left-child-offset
                 persistent-node/%right-child-offset
                 persistent-node/%weight
                 persistent-node/%node-offset
                 %make-persistent-node))

(defun %make-persistent-node (weight
                              left-child-offset
                              right-child-offset
                              this-node-offset
                              left-child
                              right-child
                              info)
  (declare #.(performance-optimizations))
  (let ((tree-data (simple-vector-32b-allocate 4)))
    (setf (simple-vector-32b-ref tree-data 0) weight
          (simple-vector-32b-ref tree-data 1) left-child-offset
          (simple-vector-32b-ref tree-data 2) right-child-offset
          (simple-vector-32b-ref tree-data 3) this-node-offset)
    (%%make-persistent-node tree-data left-child right-child info)))

(defun persistent-node/%weight (persistent-node)
  (declare (type persistent-node persistent-node)
           #.(performance-optimizations))
  (simple-vector-32b-ref (persistent-node/tree-data persistent-node) 0))

(defun persistent-node/%left-child-offset (persistent-node)
  (declare (type persistent-node persistent-node)
           #.(performance-optimizations))
  (simple-vector-32b-ref (persistent-node/tree-data persistent-node) 1))

(defun persistent-node/%right-child-offset (persistent-node)
  (declare (type persistent-node persistent-node)
           #.(performance-optimizations))
  (simple-vector-32b-ref (persistent-node/tree-data persistent-node) 2))

(defun persistent-node/%node-offset (persistent-node)
  (declare (type persistent-node persistent-node)
           #.(performance-optimizations))
  (simple-vector-32b-ref (persistent-node/tree-data persistent-node) 3))

;; Critical
(defsubst persistent-node/left-child (persistent-node)
  (declare (type persistent-node persistent-node)
           #.(performance-optimizations))
  (or (persistent-node/%cached-left-child persistent-node)
      (unless (zerop (persistent-node/%left-child-offset persistent-node))
        (error 'changesafe-database-error
               :format-control "Persistent node left child not found ~s."
               :format-arguments (list persistent-node)))))

;; Critical
(defsubst persistent-node/right-child (persistent-node)
  (declare (type persistent-node persistent-node)
           #.(performance-optimizations))
  (or (persistent-node/%cached-right-child persistent-node)
      (unless (zerop (persistent-node/%right-child-offset persistent-node))
        (error 'changesafe-database-error
               :format-control "Persistent node right child not found ~s."
               :format-arguments (list persistent-node)))))

(declaim (ftype (function (t) non-negative-fixnum) persistent-node/weight)
         (ftype (function (t) file-offset) persistent-node/node-offset)
         (inline persistent-node/weight)
         (inline persistent-node/node-offset))

(defun persistent-node/weight (persistent-node)
  (declare #.(performance-optimizations))
  (if (null persistent-node)
      0
      (persistent-node/%weight persistent-node)))

(defun persistent-node/node-offset (persistent-node)
  (declare #.(performance-optimizations))
  (if (null persistent-node)
      0
      (persistent-node/%node-offset persistent-node)))

(defconstant *file-addressing-granularity* 8)

(defun persistent-node/singleton (log-stream emit-info info)
  (declare #.(performance-optimizations))
  (let ((this-node-offset (align-stream log-stream *file-addressing-granularity*))
        (weight 1))
    (%write-fixnum weight log-stream)
    ;; If the weight is 1, then left and right children are nil.
    ;; (write-unsigned32 0                  log-stream)
    ;; (write-unsigned32 0                  log-stream)

    (funcall emit-info info log-stream)
    (%make-persistent-node weight 0 0 this-node-offset nil nil info)))

(defun persistent-node/n-join (log-stream emit-info info left-child right-child)
  (declare #.(performance-optimizations))
  (if (and (null left-child) (null right-child))
      (persistent-node/singleton log-stream emit-info info)
      (let ((this-node-offset   (align-stream log-stream *file-addressing-granularity*))
            (left-child-offset  (persistent-node/node-offset left-child))
            (right-child-offset (persistent-node/node-offset right-child))
            (weight (+ 1
                       (the non-negative-fixnum (persistent-node/weight left-child))
                       (the non-negative-fixnum (persistent-node/weight right-child)))))
        (declare (type file-offset
                       this-node-offset
                       left-child-offset
                       right-child-offset)
                 (type non-negative-fixnum weight)
                 )
        (%write-fixnum     weight             log-stream)
        (%write-unsigned32 left-child-offset  log-stream)
        (%write-unsigned32 right-child-offset log-stream)
        (funcall emit-info info              log-stream)
        (%make-persistent-node weight
                               left-child-offset
                               right-child-offset
                               this-node-offset
                               left-child right-child info))))

;;(defun persistent-node/cons (log-stream emit-info info right-child)
;;  "CONS a new persistent node onto a persistent list of nodes.
;;   This doesn't do tree balancing, so it is used for those data that
;;   are accumulated in a non-indexed set."
;;  (persistent-node/n-join log-stream emit-info info nil right-child))

(defun persistent-node/single-l (log-stream emit-info info x r)
  (declare (type persistent-node r)
           #.(performance-optimizations))
  (persistent-node/n-join
   log-stream emit-info (persistent-node/info r)
   (persistent-node/n-join
    log-stream emit-info info
    x
    (persistent-node/left-child r))
   (persistent-node/right-child r)))

(defun persistent-node/single-r (log-stream emit-info info l z)
  (declare (type persistent-node l)
           #.(performance-optimizations))
  (persistent-node/n-join
   log-stream emit-info (persistent-node/info l)
   (persistent-node/left-child l)
   (persistent-node/n-join
    log-stream emit-info info
    (persistent-node/right-child l)
    z)))

(defun persistent-node/double-l (log-stream emit-info info x r)
  (declare (type persistent-node r)
           #.(performance-optimizations))
  (let ((r.l (persistent-node/left-child r)))
    (declare (type persistent-node r.l))
    (persistent-node/n-join log-stream emit-info
                            (persistent-node/info r.l)
                            (persistent-node/n-join log-stream emit-info
                                                    info
                                                    x
                                                    (persistent-node/left-child  r.l))
                            (persistent-node/n-join log-stream emit-info
                                                    (persistent-node/info r)
                                                    (persistent-node/right-child r.l)
                                                    (persistent-node/right-child r)))))

(defun persistent-node/double-r (log-stream emit-info info l z)
  (declare (type persistent-node l)
           #.(performance-optimizations))
  (let ((l.r (persistent-node/right-child  l)))
    (declare (type persistent-node l.r))
    (persistent-node/n-join log-stream emit-info
                            (persistent-node/info l.r)
                            (persistent-node/n-join log-stream emit-info
                                                    (persistent-node/info l)
                                                    (persistent-node/left-child  l)
                                                    (persistent-node/left-child  l.r))
                            (persistent-node/n-join log-stream emit-info
                                                    info
                                                    (persistent-node/right-child l.r)
                                                    z))))

(defconstant *weight-balancing-parameter* 5
  "Affects weight balancing algorithm.  DO NOT TOUCH!")

(defun persistent-node/t-join (log-stream emit-info info left-child right-child)
  (declare #.(performance-optimizations))
  (let ((l.n (persistent-node/weight left-child))
        (r.n (persistent-node/weight right-child)))
    (cond ((fix:< (fix:+ l.n r.n) 2)
           (persistent-node/n-join log-stream emit-info info left-child right-child))

          ((fix:> r.n (* *weight-balancing-parameter* l.n))
           (if (fix:< (persistent-node/weight (persistent-node/left-child right-child))
                      (persistent-node/weight (persistent-node/right-child right-child)))
               (persistent-node/single-l log-stream emit-info info left-child right-child)
               (persistent-node/double-l log-stream emit-info info left-child right-child)))

          ((fix:> l.n (* *weight-balancing-parameter* r.n))
           (if (fix:< (persistent-node/weight (persistent-node/right-child left-child))
                      (persistent-node/weight (persistent-node/left-child left-child)))
               (persistent-node/single-r log-stream emit-info info left-child right-child)
               (persistent-node/double-r log-stream emit-info info left-child right-child)))

          (t
           (persistent-node/n-join log-stream emit-info info left-child right-child)))))

;;; The main entry points follow.

;;; PERSISTENT-NODE/ADD <predicate> <log-stream> <emit-info> <info> <root-node>
;;;
;;; Returns a new persistent node.
;;;
;;; The persistent tree rooted at <root-node>

(defun persistent-node/add (predicate log-stream emit-info info root-node)
  "Returns the root node of a new persistent tree which will include
   the original tree nodes and a new node containing info.
   The original persistent tree is not modified.

   The persistent tree rooted at <root-node> is searched for the appropriate
   location for <info>.  The <predicate> is invoked on <info> and the node
   info of some of the nodes in the tree.  <predicate> should take two arguments
   and return TRUE only if the first argument is less than the second.

   <log-stream> will be augmented by new persistent nodes and <emit-info>
   will be invoked on the <info> and the <log-stream> at an
   appropriate time to make the info persistent."
  (declare #.(performance-optimizations))
  (if (null root-node)
      (persistent-node/singleton log-stream emit-info info)
      (let ((node-info   (persistent-node/info root-node))
            (left-child  (persistent-node/left-child root-node))
            (right-child (persistent-node/right-child root-node)))
        (cond ((funcall predicate info node-info)
               (persistent-node/t-join
                log-stream emit-info (persistent-node/info root-node)
                (persistent-node/add predicate log-stream emit-info
                                     info left-child)
                right-child))
              ((funcall predicate node-info info)
               (persistent-node/t-join
                log-stream emit-info (persistent-node/info root-node)
                left-child
                (persistent-node/add predicate log-stream emit-info
                                     info right-child)))
              (t (persistent-node/n-join log-stream emit-info
                                         info
                                         left-child
                                         right-child))))))

;;; CRITICAL CODE
(defun persistent-node/%find< (get-key key this-node best-node)
  "Helper function for persistent-node/find<.  Use persistent-node/find< instead."
  (declare (type (function (persistent-node) integer) get-key)
           (type integer key)
           (type (or null persistent-node) this-node)
           (type (or null persistent-node) best-node)
           #.(performance-optimizations))
  (cond ((null this-node) best-node)
        ((< key (funcall get-key (persistent-node/info this-node)))
         (persistent-node/%find< get-key
                                 key (persistent-node/left-child this-node) best-node))
        (t (persistent-node/%find< get-key
                                   key (persistent-node/right-child this-node) this-node))))

;;; CRITICAL CODE
(defun persistent-node/find< (get-key key root-node)
  "Traverse the persistent-tree starting at <root-node>. < is invoked
   on the <key> and result of calling <get-key> on the info fields of the nodes.

   Specialized for #'<."
  (declare (type (function (persistent-node) integer) get-key)
           (type integer key)
           (type persistent-node root-node)
           #.(performance-optimizations))
  (let ((best (persistent-node/%find< get-key key root-node nil)))
    (when (and best
               (not (< (funcall get-key (persistent-node/info best)) key)))
      best)))

(defun persistent-node/%find (predicate get-key key this-node best-node)
  "Helper function for persistent-node/find.  Use persistent-node/find instead."
  (cond ((null this-node) best-node)
        ((funcall predicate key
                  (funcall get-key (persistent-node/info this-node)))
         (persistent-node/%find predicate get-key
                                key (persistent-node/left-child this-node) best-node))
        (t
         (persistent-node/%find predicate get-key
                                key (persistent-node/right-child this-node) this-node))))

(defun persistent-node/find (predicate get-key key root-node)
  "Traverse the persistent-tree starting at <root-node>.  <predicate> is invoked
   on the <key> and result of calling <get-key> on the info fields of the nodes.

   <predicate> should take two arguments and return TRUE only if the second argument
   is greater than the first."

  (let ((best (persistent-node/%find predicate get-key key root-node nil)))
    (when (and best
               (not (funcall predicate (funcall get-key (persistent-node/info best)) key)))
      best)))

(defun fetch-persistent-node (symbol-table stream location read-info)
  "Given <location> within <stream>, read the persistent node at that location.

   This is typically used to locate the root node of a persistent tree."
  (file-position stream location)
  (let* ((weight (read-fixnum stream))
         (left-child-offset (if (= weight 1)
                                0
                                (read-unsigned32 stream)))
         (right-child-offset (if (= weight 1)
                                 0
                                 (read-unsigned32 stream)))

         (info (funcall read-info symbol-table stream))

         ;; make transient stuff

         ;; try to move from lower to higher
         ;; in the file to make life easier on the
         ;; computer.
         (lower-child (if ;(< left-child-offset right-child-offset)
                       t
                          (unless (zerop left-child-offset)
                            (fetch-persistent-node symbol-table stream left-child-offset read-info))
                          (unless (zerop right-child-offset)
                            (fetch-persistent-node symbol-table stream right-child-offset read-info))))

         (upper-child (if ;(< left-child-offset right-child-offset)
                       t
                          (unless (zerop right-child-offset)
                            (fetch-persistent-node symbol-table stream right-child-offset read-info))
                          (unless (zerop left-child-offset)
                            (fetch-persistent-node symbol-table stream left-child-offset read-info)))))

    (%make-persistent-node weight
                           left-child-offset
                           right-child-offset
                           location
                           (if ;(< left-child-offset right-child-offset)
                            t
                               lower-child upper-child)
                           (if ;(< left-child-offset right-child-offset)
                            t
                               upper-child lower-child)
                           info)))

(defun persistent-node/for-each (f root-node)
  (unless (null root-node)
    (persistent-node/for-each f (persistent-node/left-child root-node))
    (funcall f (persistent-node/info root-node))
    (persistent-node/for-each f (persistent-node/right-child root-node))))

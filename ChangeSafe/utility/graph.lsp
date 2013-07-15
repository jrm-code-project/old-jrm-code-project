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
;;;;
;;;; File Name:     graph.lsp
;;;; Author:        Joe Marshall
;;;;
;;;; Module Description:
;;;;
;;; Generalized graph abstraction.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CSF/UTILITY")

(proclaim (standard-optimizations))

(eval-when (:load-toplevel :execute)
  (export '(
            graph
            graph-roots
            graph-node-children
            graph-node-eq
            graph-node-lessp
            transitive-closure
            transitive-scan
            graph-difference
            )))

(defgeneric graph-node-eq (left right)
  (:documentation
   "True iff two nodes are the same node.")

  ;; Short circuit equivalence procedure if the objects are EQ.
  (:method :around (left right)
    (or (eq left right)
        (call-next-method)))

  ;; By default, they are *not* the same node.
  (:method ((left t) (right t))
    ;; nil  ;; change back to this some day?
    (error "Cannot compare ~s (~s) and ~s (~s)." left (class-of left) right (class-of right)))

  (:method ((left pathname) (right pathname))
    (funcall (platform-filename-equal (server-platform)) left right))
  )

(defgeneric graph-node-lessp (left right)
  (:documentation
   "True iff node LEFT is less than node RIGHT.")

  ;; Short circuit equivalence procedure if the objects are EQ.
  (:method :around ((left t) (right t))
    (if (eq left right)
        nil
      (call-next-method)))

  (:method ((left t) (right t))
    (error "Cannot compare ~s (~s) and ~s (~s)." left (class-of left) right (class-of right)))

  (:method ((left pathname) (right pathname))
    (funcall (platform-filename-lessp (server-platform)) left right))
  )

(defgeneric graph-node-children (node)
  (:documentation
   "Returns a list of the children of the node."
   )
  (:method ((node pathname))
    (if (directory-pathname? node)
        (directory node)
      nil)))

(defgeneric graph-roots (graph)
  (:documentation
   "Return the root nodes of a graph.")
  (:method ((node t))
    (graph-node-children node))
  (:method ((node pathname))
    (list node))
  )

(defun transitive-closure (graph combine initial)
  "Reduce a general graph (which may contain cycles) with COMBINE.

COMBINE is a function of two arguments, the first argument will be
a node in the graph, the second will be the result of the last COMBINE
or INITIAL on the first iteration.  Thus if the graph contains nodes
A, B, C, D, the result of this function will be
COMBINE (A, COMBINE (D, COMBINE (C, COMBINE (B, INITIAL)))) where
the nodes are traversed in no particular order.

No node is visited twice."
  (let ((nodes-visited  (make-ordered-set #'graph-node-eq #'graph-node-lessp))
        (nodes-to-visit (make-ordered-set #'graph-node-eq #'graph-node-lessp)))
    (labels ((traverse-step (state)
               (if (ordered-set/empty? nodes-to-visit)
                   state
                   (let ((this-node (ordered-set/pick! nodes-to-visit)))
                     (ordered-set/adjoin! nodes-visited this-node)
                     (mapc (lambda (child)
                             (unless (ordered-set/member? nodes-visited child)
                               (ordered-set/adjoin! nodes-to-visit child)))
                           (graph-node-children this-node))
                     (traverse-step (funcall combine this-node state))
                     ))))
      (ordered-set/adjoin-list! nodes-to-visit (graph-roots graph))
      (traverse-step initial))))

(defun transitive-scan-internal (graph)
  (declare (optimizable-series-function 3))
  (scan-fn-inclusive '(values t rbtree rbtree)
           (lambda ()
             (values nil
                     (make-ordered-set #'graph-node-eq #'graph-node-lessp)
                     (let ((nodes-to-visit (make-ordered-set #'graph-node-eq #'graph-node-lessp)))
                       (ordered-set/adjoin-list! nodes-to-visit (graph-roots graph))
                       nodes-to-visit)))
           (lambda (node nodes-visited nodes-to-visit)
             (declare (ignore node))
             (let* ((this-node (ordered-set/pick! nodes-to-visit)))
               (ordered-set/adjoin! nodes-visited this-node)
               (dolist (child (graph-node-children this-node))
                 (unless (ordered-set/member? nodes-visited child)
                   (debug-message 5 "transitive-scan-internal: adjoining ~s" child)
                   (ordered-set/adjoin! nodes-to-visit child)))
               (values this-node nodes-visited nodes-to-visit)))
           (lambda (node nodes-visited nodes-to-visit)
             (declare (ignore node nodes-visited))
             (ordered-set/empty? nodes-to-visit))))

(defun transitive-scan (graph)
  (declare (optimizable-series-function))
  (multiple-value-bind (nodes state1 state2) (transitive-scan-internal graph) (subseries nodes 1)))

(defun graph-difference (new-graph old-graph)
  "Returns five values:
COMMON-NODES is all nodes that appear in both OLD-GRAPH and NEW-GRAPH,
NODES-REMOVED is all nodes that appear in OLD-GRAPH but do not appear in NEW-GRAPH,
NODES-ADDED is all nodes that appear in NEW-GRAPH but do not appear in OLD-GRAPH,
EDGES-REMOVED is all edges that appear in OLD-GRAPH but do not appear in NEW-GRAPH,
EDGES-ADDED is all edges that appear in NEW-GRAPH but do not appear in OLD-GRAPH.

COMMON-NODES isn't really part of the graph difference, but it is often needed
around the time that the graph-difference is computed, and it will be easy to compute
at this point."

  ;; The edge abstraction isn't too well developed.  Oh well.

  ;; Performance note:  we do a *lot* of set manipulation in these routines.

  (flet ((make-edge (parent child) (list parent child)))
    (let ((old-nodes (make-ordered-set #'graph-node-eq #'graph-node-lessp))
          (old-edges nil)
          (new-nodes (make-ordered-set #'graph-node-eq #'graph-node-lessp))
          (new-edges nil))
      (debug-message 4 "Taking transitive closure of old graph...")
      (transitive-closure old-graph (lambda (node accumulate)
                                        (declare (ignore accumulate))
                                        (ordered-set/adjoin! old-nodes node)
                                        (dolist (child (graph-node-children node))
                                          (push (make-edge node child) old-edges))) nil)
      (debug-message 4 "Taking transitive closure of new graph...")
      (transitive-closure new-graph (lambda (node accumulate)
                                        (declare (ignore accumulate))
                                        (ordered-set/adjoin! new-nodes node)
                                        (dolist (child (graph-node-children node))
                                          (push (make-edge node child) new-edges))) nil)

      (values
       (progn (debug-message 4 "Computing intersection...")
              (ordered-set->list (ordered-set/intersection old-nodes new-nodes))) ;left these nodes alone
       (progn (debug-message 4 "Computing difference...")
              (ordered-set->list (ordered-set/difference old-nodes new-nodes))) ; Removed these nodes
       (progn (debug-message 4 "Computing difference...")
              (ordered-set->list (ordered-set/difference new-nodes old-nodes))) ; Added these nodes
       (progn (debug-message 4 "Computing difference...")
              (set-difference old-edges new-edges :test (lambda (left right)
                                                            (and (eq (car left) (car right))
                                                                 (eq (cadr left) (cadr right))))))
       (progn (debug-message 4 "Computing difference...")
              (set-difference new-edges old-edges :test (lambda (left right)
                                                            (and (eq (car left) (car right))
                                                                 (eq (cadr left) (cadr right))))))))))

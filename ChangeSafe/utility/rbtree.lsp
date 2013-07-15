;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright © 1993 Massachusetts Institute of Technology

;; This material was developed by the Scheme project at the Massachusetts
;; Institute of Technology, Department of Electrical Engineering and
;; Computer Science.  Permission to copy and modify this software, to
;; redistribute either the original software or a modified version, and
;; to use this software for any purpose is granted, subject to the
;; following restrictions and understandings.

;; 1. Any copy made of this software must include this copyright notice
;; in full.

;; 2. Users of this software agree to make their best efforts (a) to
;; return to the MIT Scheme project any improvements or extensions that
;; they make, so that these may be included in future releases; and (b)
;; to inform MIT of noteworthy uses of this software.

;; 3. All materials developed as a consequence of the use of this
;; software shall duly acknowledge such use, in accordance with the usual
;; standards of acknowledging credit in academic research.

;; 4. MIT has made no warrantee or representation that the operation of
;; this software will be error-free, and MIT is under no obligation to
;; provide any services, by way of maintenance, update, or otherwise.

;; 5. In conjunction with products arising from the use of this material,
;; there shall be no use of the name of the Massachusetts Institute of
;; Technology nor of any adaptation thereof in any advertising,
;; promotional, or sales literature without prior written consent from
;; MIT in each case.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; File Name:     rbtree.lsp
;;;; Author:        Joe Marshall
;;;;
;;;; Module Description:
;;;;
;;;; Red-Black Trees
;;;;
;;;; If you are managing a set of items that have a total ordering, i.e.
;;;; less-than and equal are well defined on any pair, an RBTREE gives
;;;; about logarithmic performance for insertion, lookup, and deletion.
;;;; Furthermore, you can easily iterate through the tree enumerating all
;;;; the nodes.
;;;;
;;;; The main operations are
;;;;
;;;; make-tree <less-predicate> <equal-predicate>
;;;;   Returns a new rbtree where the keys are compared with less-predicate
;;;; and equal-predicate.
;;;;
;;;; rbtree/lookup <tree> <key> <default>
;;;;   Finds <key> in <tree> and returns the associated datum if found,
;;;; returns <default> if not found.
;;;;
;;;; rbtree/insert! <tree> <key> <datum>
;;;;   Modifies <tree> so that <key> is associated with <datum>.  <Key>
;;;; is entered into the tree if it is not already present.
;;;;
;;;; rbtree/delete! <tree> <key>
;;;;   Modifies <tree> so that <key> is no longer in the tree.
;;;;
;;;; rbtree/foreach <tree> <function>
;;;;   Applies <function> to each node in <tree> in order from smallest
;;;; to largest (as ordered by the less-predicate and the equal-predicate).
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Red-Black Trees
;;; package: (runtime rb-tree)

;;; Cormen, Leiserson, and Rivest, "Introduction to Algorithms",
;;; Chapter 14, "Red-Black Trees".

;;; Properties of Red-Black Trees:
;;; 1. Every node is either red or black.
;;; 2. Every leaf (#F) is black.
;;; 3. If a node is red, then both its children are black.
;;; 4. Every simple path from a node to a descendent leaf contains the
;;;    same number of black nodes.  Thus there are no leaf nodes that
;;;    are unusually far away from the root.
;;; These algorithms additionally assume:
;;; 5. The root of a tree is black.


(in-package "CSF/UTILITY")

(proclaim (standard-optimizations))

(eval-when (:load-toplevel :execute)
  (export '(
            ;; Red Black trees
            make-tree
            make-integer-tree
            rb-tree?
            rb-tree/insert!
            rb-tree/delete!
            rb-tree/pick!
            rb-tree/push!
            rb-tree/lookup
            rb-tree/map
            rb-tree/fold-left
            rb-tree/fold-right
            rb-tree/foreach
            rb-tree/foreach-backwards
            rb-tree/least-element
            #+series-ansi rb-tree/scan
            #+series-ansi rb-tree/scan-keys
            #+series-ansi integer-rb-tree/scan-keys
            #+series-ansi rb-tree/scan-values
            rb-tree/nodes-bounding-key
            )))

(defstruct (rbtree
            (:predicate rb-tree?)
            (:constructor make-tree (key=? key<?)))

  (root nil)
  (key=? nil :read-only t)
  (key<? nil :read-only t))

(defstruct (integer-rbtree
            (:predicate integer-rb-tree?)
            (:constructor make-integer-tree ()))

  (root nil))

(defmethod print-object ((object rbtree) stream)
  (print-unreadable-object (object stream :type t :identity t)))

(defmethod print-object ((object integer-rbtree) stream)
  (print-unreadable-object (object stream :type t :identity t)))

(defstruct (rbnode
            (:constructor make-rbnode (key datum)))
  key
  datum
  (up    nil)
  (left  nil)
  (right nil)
  (color nil))

(defmethod print-object ((object rbnode) stream)
  (print-unreadable-object (object stream :type t :identity t)))

(declaim (inline b->d
                 -d
                 get-link+
                 set-link+!
                 get-link-
                 set-link-!))

(defun b->d (left?)
  (declare (optimize (speed 3) (safety 3)))
  (if left? 'LEFT 'RIGHT))

(defun -d (d)
  (declare (optimize (speed 3) (safety 3)))
  (if (eq 'LEFT d) 'RIGHT 'LEFT))

(defun get-link+ (p d)
  (declare (optimize (speed 3) (safety 3)))
  (if (eq 'LEFT d)
      (rbnode-left p)
    (rbnode-right p)))

(defun set-link+! (p d l)
  (declare (optimize (speed 3) (safety 3)))
  (if (eq 'LEFT d)
      (setf (rbnode-left p) l)
    (setf (rbnode-right p) l)))

(defun get-link- (p d)
  (declare (optimize (speed 3) (safety 3)))
  (if (eq 'RIGHT d)
      (rbnode-left p)
    (rbnode-right p)))

(defun set-link-! (p d l)
  (declare (optimize (speed 3) (safety 3)))
  (if (eq 'right d)
      (setf (rbnode-left p) l)
    (setf (rbnode-right p) l)))

(defun rotate+! (tree x d)
  ;; Assumes (NOT (NOT (GET-LINK- X D))).
  (declare (optimize (speed 3) (safety 3)))
  (let ((y (get-link- x d)))
    (let ((beta (get-link+ y d)))
      (set-link-! x d beta)
      (if beta (setf (rbnode-up beta) x)))
    (let ((u (rbnode-up x)))
      (setf (rbnode-up y) u)
      (cond ((not u)
             (setf (rbtree-root tree) y))
            ((eq x (get-link+ u d))
             (set-link+! u d y))
            (t
             (set-link-! u d y))))
    (set-link+! y d x)
    (setf (rbnode-up x) y)))

(defun integer-rotate+! (tree x d)
  ;; Assumes (NOT (NOT (GET-LINK- X D))).
  (declare (optimize (speed 3) (safety 3)))
  (let ((y (get-link- x d)))
    (let ((beta (get-link+ y d)))
      (set-link-! x d beta)
      (if beta (setf (rbnode-up beta) x)))
    (let ((u (rbnode-up x)))
      (setf (rbnode-up y) u)
      (cond ((not u)
             (setf (integer-rbtree-root tree) y))
            ((eq x (get-link+ u d))
             (set-link+! u d y))
            (t
             (set-link-! u d y))))
    (set-link+! y d x)
    (setf (rbnode-up x) y)))

(declaim (inline rotate-!))
(declaim (inline integer-rotate-!))

(defun rotate-! (tree x d)
  (declare (optimize (speed 3) (safety 3)))
  (rotate+! tree x (-d d)))

(defun integer-rotate-! (tree x d)
  (declare (optimize (speed 3) (safety 3)))
  (integer-rotate+! tree x (-d d)))

(defun first-rbnode (tree)
  (declare (optimize (speed 3) (safety 3)))
  (and (rbtree-root tree)
       (do ((x (rbtree-root tree) next)
            (next (rbnode-left (rbtree-root tree)) (rbnode-left next)))
           ((null next) x))))

(defun integer-first-rbnode (tree)
  (declare (optimize (speed 3) (safety 3)))
  (and (integer-rbtree-root tree)
       (do ((x (integer-rbtree-root tree) next)
            (next (rbnode-left (integer-rbtree-root tree)) (rbnode-left next)))
           ((null next) x))))

(defun last-rbnode (tree)
  (declare (optimize (speed 3) (safety 3)))
  (and (rbtree-root tree)
       (do ((x (rbtree-root tree) next)
            (next (rbnode-right (rbtree-root tree)) (rbnode-right next)))
           ((null next) x))))

(defun next-rbnode (x)
  (declare (optimize (speed 3) (safety 3)))
  (if (rbnode-right x)
      (do ((x (rbnode-right x) next)
           (next (rbnode-left (rbnode-right x)) (rbnode-left next)))
          ((null next) x))
      (do ((x x y)
           (y (rbnode-up x) (rbnode-up y)))
          ((not (and y (eq x (rbnode-right y)))) y))))

(defun previous-rbnode (x)
  (declare (optimize (speed 3) (safety 3)))
  (if (rbnode-left x)
      (do ((x (rbnode-left x) next)
           (next (rbnode-right (rbnode-left x)) (rbnode-right next)))
          ((null next) x))
      (do ((x  x  y)
           (y (rbnode-up x) (rbnode-up y)))
          ((not (and y (eq x (rbnode-left y)))) y))))

(defun insert-fixup! (tree x)
  ;; Assumptions: X is red, and the only possible violation of the
  ;; tree properties is that (RBNODE-UP X) is also red.
  (declare (optimize (speed 3) (safety 3)))
  (do ((x  x)
       (u (rbnode-up x) (rbnode-up x)))
      ((or (null u)
           (eq 'BLACK (rbnode-color u))
           (let* ((d (b->d (eq u (rbnode-left (rbnode-up u)))))
                  (y (get-link- (rbnode-up u) d)))
             (if (or (null y)
                     (eq 'BLACK (rbnode-color y)))
                 (let ((x
                        (if (eq x (get-link- u d))
                            ;; case 2
                            (progn
                              (rotate+! tree u d)
                              u)
                            x)))
                   ;; case 3
                   (let ((u (rbnode-up x)))
                     (setf (rbnode-color u) 'BLACK)
                     (setf (rbnode-color (rbnode-up u)) 'RED)
                     (rotate-! tree (rbnode-up u) d))
                   t)
                 ;; case 1
                 (progn (setf (rbnode-color y) 'BLACK) nil)))))
    (setf (rbnode-color u) 'BLACK)
    (setf (rbnode-color (rbnode-up u)) 'RED)
    (setq x (rbnode-up u)))
  (setf (rbnode-color (rbtree-root tree)) 'BLACK))

(defun rb-tree/%insert! (tree key datum key=? key<? x y d)
  (declare (optimize (speed 3) (safety 3)))
  (cond ((not x) (let ((z (make-rbnode key datum)))
                   (mp:without-interrupts
                    (setf (rbnode-up z) y)
                    (cond ((not y) (setf (rbtree-root tree) z))
                          ((eq 'left d) (setf (rbnode-left y) z))
                          (t (setf (rbnode-right y) z)))
                    (setf (rbnode-color z) 'red)
                    (insert-fixup! tree z))))
        ((funcall key=? key (rbnode-key x)) (setf (rbnode-datum x) datum))
        ((funcall key<? key (rbnode-key x)) (rb-tree/%insert! tree key datum key=? key<? (rbnode-left x) x 'left))
        (t (rb-tree/%insert! tree key datum key=? key<? (rbnode-right x) x 'right))))

(defun rb-tree/insert! (tree key datum)
  (check-type tree rbtree)
  (rb-tree/%insert! tree key datum (rbtree-key=? tree) (rbtree-key<? tree) (rbtree-root tree) nil nil))

(defun integer-insert-fixup! (tree x)
  ;; Assumptions: X is red, and the only possible violation of the
  ;; tree properties is that (RBNODE-UP X) is also red.
  (declare (optimize (speed 3) (safety 3)))
  (do ((x  x)
       (u (rbnode-up x) (rbnode-up x)))
      ((or (null u)
           (eq 'BLACK (rbnode-color u))
           (let* ((d (b->d (eq u (rbnode-left (rbnode-up u)))))
                  (y (get-link- (rbnode-up u) d)))
             (if (or (null y)
                     (eq 'BLACK (rbnode-color y)))
                 (let ((x
                        (if (eq x (get-link- u d))
                            ;; case 2
                            (progn
                              (integer-rotate+! tree u d)
                              u)
                            x)))
                   ;; case 3
                   (let ((u (rbnode-up x)))
                     (setf (rbnode-color u) 'BLACK)
                     (setf (rbnode-color (rbnode-up u)) 'RED)
                     (integer-rotate-! tree (rbnode-up u) d))
                   t)
                 ;; case 1
                 (progn (setf (rbnode-color y) 'BLACK) nil)))))
    (setf (rbnode-color u) 'BLACK)
    (setf (rbnode-color (rbnode-up u)) 'RED)
    (setq x (rbnode-up u)))
  (setf (rbnode-color (integer-rbtree-root tree)) 'BLACK))

(defun integer-rb-tree/%insert! (tree key datum x y d)
  (declare (type integer key)
           (optimize (speed 3) (safety 3)))
  (cond ((not x) (let ((z (make-rbnode key datum)))
                   (mp:without-interrupts
                    (setf (rbnode-up z) y)
                    (cond ((not y) (setf (integer-rbtree-root tree) z))
                          ((eq 'left d) (setf (rbnode-left y) z))
                          (t (setf (rbnode-right y) z)))
                    (setf (rbnode-color z) 'red)
                    (integer-insert-fixup! tree z))))
        ((= key (the integer (rbnode-key x))) (setf (rbnode-datum x) datum))
        ((< key (the integer (rbnode-key x))) (integer-rb-tree/%insert! tree key datum (rbnode-left x) x 'left))
        (t (integer-rb-tree/%insert! tree key datum (rbnode-right x) x 'right))))

(defun integer-rb-tree/insert! (tree key datum)
  (check-type tree integer-rbtree)
  (integer-rb-tree/%insert! tree key datum (integer-rbtree-root tree) nil nil))

(defun rb-tree/push! (tree key datum)
  "Returns two values, the node key and the datum."
  (check-type tree rbtree)
  (let ((key=? (rbtree-key=? tree))
        (key<? (rbtree-key<? tree)))
    (labels ((luup (x y d)
                    (cond ((not x)
                           (let ((z (make-rbnode key (list datum))))
                             (mp:without-interrupts
                               (setf (rbnode-up z) y)
                               (cond ((not y) (setf (rbtree-root tree) z))
                                     ((eq 'LEFT d) (setf (rbnode-left y) z))
                                     (t (setf (rbnode-right y) z)))
                               (setf (rbnode-color z) 'RED)
                               (insert-fixup! tree z))
                             (values (rbnode-key z) (rbnode-datum z))))
                          ((funcall key=? key (rbnode-key x))
                           (push datum (rbnode-datum x))
                           (values (rbnode-key x) (rbnode-datum x)))
                          ((funcall key<? key (rbnode-key x)) (luup (rbnode-left x) x 'LEFT))
                          (t (luup (rbnode-right x) x 'RIGHT)))))
      (luup (rbtree-root tree) nil nil))))

(defun alist->rb-tree (alist key=? key<?)
  ;; Is there a more efficient way to do this?
  (let ((tree (make-tree key=? key<?)))
    (do ((alist alist (cdr alist)))
        ((null alist) tree)
      (rb-tree/insert! tree (caar alist) (cdar alist)))))

(defun delete-fixup! (tree x u)
  (declare (optimize (speed 3) (safety 3)))
  (tail-labels ((luup (x u)
                  (if (or (not u)
                          (and x (eq 'RED (rbnode-color x))))
                      (if x (setf (rbnode-color x) 'BLACK))
                    (let ((d (b->d (eq x (rbnode-left u)))))
                      (let ((w
                             (let ((w (get-link- u d)))
                               (if (eq 'RED (rbnode-color w))
                                   ;; case 1
                                   (progn
                                     (setf (rbnode-color w) 'BLACK)
                                     (setf (rbnode-color u) 'RED)
                                     (rotate+! tree u d)
                                     (get-link- u d))
                                 w))))
                        (flet ((case-4 (w)
                                 (setf (rbnode-color w) (rbnode-color u))
                                 (setf (rbnode-color u) 'BLACK)
                                 (setf (rbnode-color (get-link- w d)) 'BLACK)
                                 (rotate+! tree u d)
                                 (setf (rbnode-color (rbtree-root tree)) 'BLACK)))
                          (if (let ((n- (get-link- w d)))
                                (and n-
                                     (eq 'RED (rbnode-color n-))))
                              (case-4 w)
                            (let ((n+ (get-link+ w d)))
                              (if (or (not n+)
                                      (eq 'BLACK (rbnode-color (get-link+ w d))))
                                  ;; case 2
                                  (progn
                                    (setf (rbnode-color w) 'RED)
                                    (luup u (rbnode-up u)))
                                ;; case 3
                                (progn
                                  (setf (rbnode-color n+) 'BLACK)
                                  (setf (rbnode-color w) 'RED)
                                  (rotate-! tree w d)
                                  (case-4 (get-link- u d))))))))))))
    (luup x u)))

(defun integer-delete-fixup! (tree x u)
  (declare (optimize (speed 3) (safety 3)))
  (tail-labels ((luup (x u)
                  (if (or (not u)
                          (and x (eq 'RED (rbnode-color x))))
                      (if x (setf (rbnode-color x) 'BLACK))
                    (let ((d (b->d (eq x (rbnode-left u)))))
                      (let ((w
                             (let ((w (get-link- u d)))
                               (if (eq 'RED (rbnode-color w))
                                   ;; case 1
                                   (progn
                                     (setf (rbnode-color w) 'BLACK)
                                     (setf (rbnode-color u) 'RED)
                                     (integer-rotate+! tree u d)
                                     (get-link- u d))
                                 w))))
                        (flet ((case-4 (w)
                                 (setf (rbnode-color w) (rbnode-color u))
                                 (setf (rbnode-color u) 'BLACK)
                                 (setf (rbnode-color (get-link- w d)) 'BLACK)
                                 (integer-rotate+! tree u d)
                                 (setf (rbnode-color (integer-rbtree-root tree)) 'BLACK)))
                          (if (let ((n- (get-link- w d)))
                                (and n-
                                     (eq 'RED (rbnode-color n-))))
                              (case-4 w)
                            (let ((n+ (get-link+ w d)))
                              (if (or (not n+)
                                      (eq 'BLACK (rbnode-color (get-link+ w d))))
                                  ;; case 2
                                  (progn
                                    (setf (rbnode-color w) 'RED)
                                    (luup u (rbnode-up u)))
                                ;; case 3
                                (progn
                                  (setf (rbnode-color n+) 'BLACK)
                                  (setf (rbnode-color w) 'RED)
                                  (integer-rotate-! tree w d)
                                  (case-4 (get-link- u d))))))))))))
    (luup x u)))

(defun delete-rbnode! (tree z)
  (declare (optimize (speed 3) (safety 3)))
  (mp:without-interrupts
    (let ((z
           (if (and (rbnode-left z) (rbnode-right z))
               (let ((y (next-rbnode z)))
                 (setf (rbnode-key z) (rbnode-key y))
                 (setf (rbnode-datum z) (rbnode-datum y))
                 y)
             z)))
      (let ((x (or (rbnode-left z) (rbnode-right z)))
            (u (rbnode-up z)))
        (if x (setf (rbnode-up x) u))
        (cond ((not u) (setf (rbtree-root tree) x))
              ((eq z (rbnode-left u)) (setf (rbnode-left u) x))
              (t (setf (rbnode-right u) x)))
        (if (eq 'BLACK (rbnode-color z))
            (delete-fixup! tree x u))))))

(defun integer-delete-rbnode! (tree z)
  (declare (optimize (speed 3) (safety 3)))
  (mp:without-interrupts
    (let ((z
           (if (and (rbnode-left z) (rbnode-right z))
               (let ((y (next-rbnode z)))
                 (setf (rbnode-key z) (rbnode-key y))
                 (setf (rbnode-datum z) (rbnode-datum y))
                 y)
             z)))
      (let ((x (or (rbnode-left z) (rbnode-right z)))
            (u (rbnode-up z)))
        (if x (setf (rbnode-up x) u))
        (cond ((not u) (setf (integer-rbtree-root tree) x))
              ((eq z (rbnode-left u)) (setf (rbnode-left u) x))
              (t (setf (rbnode-right u) x)))
        (if (eq 'BLACK (rbnode-color z))
            (integer-delete-fixup! tree x u))))))

(defun rb-tree/delete! (tree key)
  (check-type tree rbtree)
  (let ((key=? (rbtree-key=? tree))
        (key<? (rbtree-key<? tree)))
    (labels ((luup (x)
                    (cond ((not x) nil)
                          ((funcall key=? key (rbnode-key x)) (delete-rbnode! tree x))
                          ((funcall key<? key (rbnode-key x)) (luup (rbnode-left x)))
                          (t (luup (rbnode-right x))))))
      (luup (rbtree-root tree)))))

(defun rb-tree/pick! (tree)
  (check-type tree rbtree)
  (when (rbtree-root tree)
    (let* ((first (first-rbnode tree))
           (key   (rbnode-key first))
           (datum (rbnode-datum first)))
      (delete-rbnode! tree first)
      (values key datum))))

(defun integer-rb-tree/pick! (tree)
  (check-type tree integer-rbtree)
  (when (integer-rbtree-root tree)
    (let* ((first (integer-first-rbnode tree))
           (key   (rbnode-key first))
           (datum (rbnode-datum first)))
      (integer-delete-rbnode! tree first)
      (values key datum))))

(defun rb-tree/lookup (tree key default)
  (check-type tree rbtree)
  (let ((key=? (rbtree-key=? tree))
        (key<? (rbtree-key<? tree)))
    (do ((x (rbtree-root tree) (if (funcall key<? key (rbnode-key x))
                                   (rbnode-left x)
                                   (rbnode-right x))))
        ((or (null x)
             (funcall key=? key (rbnode-key x)))
         (if x (rbnode-datum x) default))
      (declare (optimize (speed 3) (safety 3))))))

(defun integer-rb-tree/lookup (tree key default)
  (declare (type integer key))
  (check-type tree integer-rbtree)
  (do ((x (integer-rbtree-root tree) (if (< key (rbnode-key x))
                                         (rbnode-left x)
                                         (rbnode-right x))))
      ((or (null x)
           (= key (rbnode-key x)))
       (if x (rbnode-datum x) default))
      (declare (optimize (speed 3) (safety 3)))))

(defun rb-tree/copy (tree)
  (check-type tree rbtree)
  (let ((result (make-tree (rbtree-key=? tree) (rbtree-key<? tree))))
    (setf (rbtree-root result)
      (labels ((luup (rbnode up)
                      (and rbnode
                           (let ((rbnode* (make-rbnode (rbnode-key rbnode) (rbnode-datum rbnode))))
                             (setf (rbnode-color rbnode*) (rbnode-color rbnode))
                             (setf (rbnode-up rbnode*) up)
                             (setf (rbnode-left rbnode*) (luup (rbnode-left rbnode) rbnode*))
                             (setf (rbnode-right rbnode*) (luup (rbnode-right rbnode) rbnode*))
                             rbnode*))))
        (luup (rbtree-root tree) nil)))
    result))

(defun rb-tree/height (tree)
  (check-type tree rbtree)
  (labels ((luup (rbnode)
                  (if rbnode
                      (+ 1 (max (luup (rbnode-left rbnode)) (luup (rbnode-right rbnode))))
                    0)))
    (luup (rbtree-root tree))))

(defun rb-tree/size (tree)
  (check-type tree rbtree)
  (labels ((luup (rbnode)
                  (if rbnode
                      (+ 1 (luup (rbnode-left rbnode)) (luup (rbnode-right rbnode)))
                    0)))
    (luup (rbtree-root tree))))

(defun rb-tree/empty? (tree)
  (check-type tree rbtree)
  (not (rbtree-root tree)))

(defun integer-rb-tree/empty? (tree)
  (check-type tree integer-rbtree)
  (not (integer-rbtree-root tree)))

(defun rb-tree/equal? (x y datum=?)
  (check-type x rbtree)
  (check-type y rbtree)
  (let ((key=? (rbtree-key=? x)))
    (and (eq key=? (rbtree-key=? y))
         (labels ((luup (nx ny)
                         (if (not nx)
                             (not ny)
                           (and ny
                                (funcall key=? (rbnode-key nx) (rbnode-key ny))
                                (funcall datum=? (rbnode-datum nx) (rbnode-datum ny))
                                (luup (next-rbnode nx) (next-rbnode ny))))))
           (luup (first-rbnode x) (first-rbnode y))))))

(defun rb-tree/fold-left (function initial rb-tree)
  "Compute (function (function (function initial rbk0 rbd0) rbk1 rbd1) rbk2 rbd2) etc.
   Where rbk0 rbd0 are the least key and value in the rb-tree."
  (do ((answer initial (funcall function answer (rbnode-key rbnode) (rbnode-datum rbnode)))
       (rbnode (first-rbnode rb-tree) (next-rbnode rbnode)))
      ((null rbnode) answer)))

(defun rb-tree/fold-right (function initial rb-tree)
  "Compute (function rbk0 rbd0 (function rbk1 rbd1 (function rbk2 rbd2 initial))) etc.
   Where rbk0 rbd0 are the least key and value in the rb-tree."
  (labels ((luup (rbnode)
                  (if (null rbnode)
                      initial
                    (funcall function
                             (rbnode-key rbnode)
                             (rbnode-datum rbnode)
                             (luup (next-rbnode rbnode))))))
    (luup (first-rbnode rb-tree))))

(defun rb-tree/map (function tree)
  "Apply func to each entry in rb-tree, from least entry on up.

   Func must have a signature congruent to (KEY VALUE)"
  (check-type tree rbtree)
  (let ((rbnode (first-rbnode tree)))
    (when rbnode
      (let ((result (list (funcall function (rbnode-key rbnode) (rbnode-datum rbnode)))))
        (labels ((luup (rbnode prev)
                        (when rbnode
                          (let ((pair (list (funcall function (rbnode-key rbnode) (rbnode-datum rbnode)))))
                            (setf (cdr prev) pair)
                            (luup (next-rbnode rbnode) pair)))))
          (luup (next-rbnode rbnode) result))
        result))))

(defun rb-tree->alist (tree)
  "Return an alist of (KEY . VALUE) pairs generated from the rbtree"
  (rb-tree/map #'cons tree))

(defun rb-tree/key-list (tree)
  "Return a list of KEYS generated from the rbtree"
  (check-type tree rbtree)
  (rb-tree/map (lambda (key value)
                   (declare (ignore value))
                   key)
               tree))

(defun rb-tree/least-element (tree)
  (check-type tree rbtree)
  (let ((first (first-rbnode tree)))
    (when first
      (values (rbnode-key first) (rbnode-datum first)))))

(defun rb-tree/foreach (tree function)
  (check-type tree rbtree)
  (do ((rbnode (first-rbnode tree) (next-rbnode rbnode)))
      ((not rbnode))
    (funcall function (rbnode-key rbnode) (rbnode-datum rbnode))))

(defun rb-tree/foreach-backwards (tree function)
  (check-type tree rbtree)
  (do ((rbnode (last-rbnode tree) (previous-rbnode rbnode)))
      ((not rbnode))
    (funcall function (rbnode-key rbnode) (rbnode-datum rbnode))))

#+series-ansi
(defun rb-tree/scan-nodes (tree)
  (declare (optimizable-series-function))
  (scan-fn 'rbnode
           (lambda () (first-rbnode tree))
           #'next-rbnode
           #'null))

#+series-ansi
(defun integer-rb-tree/scan-nodes (tree)
  (declare (optimizable-series-function))
  (scan-fn 'rbnode
           (lambda () (integer-first-rbnode tree))
           #'next-rbnode
           #'null))

#+series-ansi
(defun rb-tree/scan-keys (tree)
  (declare (optimizable-series-function))
  (map-fn t #'rbnode-key (rb-tree/scan-nodes tree)))

#+series-ansi
(defun integer-rb-tree/scan-keys (tree)
  (declare (optimizable-series-function))
  (map-fn 'integer #'rbnode-key (integer-rb-tree/scan-nodes tree)))

#+series-ansi
(defun rb-tree/scan-values (tree)
  (declare (optimizable-series-function))
  (map-fn t #'rbnode-datum (rb-tree/scan-nodes tree)))

#+series-ansi
(defun rb-tree/scan (tree)
  (declare (optimizable-series-function 2))
  (map-fn '(values t t) (lambda (node)
                            (values (rbnode-key node) (rbnode-datum node)))
          (rb-tree/scan-nodes tree)))

(defun rb-tree/foreach-key (tree function)
  (check-type tree rbtree)
  (do ((rbnode (first-rbnode tree) (next-rbnode rbnode)))
      ((not rbnode))
    (funcall function (rbnode-key rbnode))))

(defun rb-tree/datum-list (tree)
  (check-type tree rbtree)
  (let ((rbnode (first-rbnode tree)))
    (if rbnode
        (let ((result (list (rbnode-datum rbnode))))
          (labels ((luup (rbnode prev)
                          (if rbnode
                              (let ((pair (list (rbnode-datum rbnode))))
                                (setf (cdr prev) pair)
                                (luup (next-rbnode rbnode) pair)))))
            (luup (next-rbnode rbnode)  result))
          result))))

(defun rb-tree/print (tree)
  (check-type tree rbtree)
  (labels ((luup (rbnode depth)
                  (if rbnode
                      (progn (format t "~&~vt~s:~s" (* depth 2) (rbnode-key rbnode) (rbnode-datum rbnode))
                             (luup (rbnode-left rbnode) (1+ depth))
                             (luup (rbnode-right rbnode) (1+ depth))))))
    (luup (rbtree-root tree) 0)))

(defun rb-tree/nodes-bounding-key (tree key)
  "Returns two values.  The RBNODE of tree whose key is the greatest one
   not greater than KEY and the RBNODE whose key is the least one not less
   than KEY.  Either of these can be NIL if there is no node whose key is
   less than or greater than key."
  (let ((key=? (rbtree-key=? tree))
        (key<? (rbtree-key<? tree)))
    (labels ((luup (current-node lower-bounding-node upper-bounding-node)
                    (cond ((null current-node)
                           (values lower-bounding-node upper-bounding-node))
                          ((funcall key=? key (rbnode-key current-node))
                           (values current-node current-node))
                          ((funcall key<? key (rbnode-key current-node))
                           (luup (rbnode-left current-node)
                                 lower-bounding-node
                                 current-node))
                          (t
                           (luup (rbnode-right current-node)
                                 current-node
                                 upper-bounding-node)))))
      (luup (rbtree-root tree) nil nil))))

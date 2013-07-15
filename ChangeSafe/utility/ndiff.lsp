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

(in-package "CSF/UTILITY")

(proclaim (standard-optimizations))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (export '(simple-diff
            edit-path
            svf-edit-path
            indel
            indel/left-subseq-start
            indel/left-subseq-limit
            indel/right-subseq-start
            indel/right-subseq-limit)))

(deftype rectilinear-row-index ()
  `(integer 0 ,(floor array-dimension-limit 2)))

(deftype rectilinear-column-index ()
  `(integer 0 ,(floor array-dimension-limit 2)))

(deftype diagonal-row-index ()
  `(integer ,(- (floor array-dimension-limit 2))
            ,(floor array-dimension-limit)))

(deftype diagonal-column-index ()
  `(integer 0 ,array-dimension-limit))

;; Convert from rectilinear row and column to
;; diagonal row and column.
(declaim (ftype (function (rectilinear-row-index rectilinear-column-index)
                          (values diagonal-row-index diagonal-column-index))
                rect->diag)
         (ftype (function (diagonal-row-index diagonal-column-index)
                          (values rectilinear-row-index rectilinear-column-index))
                diag->rect)
         (inline rect->diag)
         (inline diag->rect))

(defun rect->diag (row col)
  (declare (type rectilinear-row-index row)
           (type rectilinear-column-index col)
           #.(performance-optimizations))
  (values (- row col) (+ row col)))

(defun diag->rect (diag-row diag-col)
  (declare (type diagonal-row-index diag-row)
           (type diagonal-column-index diag-col)
           #.(performance-optimizations))
  (values (ash (+ diag-col diag-row) -1)
          (ash (- diag-col diag-row) -1)))

;; Imagine that we rotate the edit matrix by 45 degrees
;; counterclockwise.  Now it will look like this:
;;
;;             alim.
;;                   .
;;                  5  .
;;                4   4  .
;;              3   3   3  .   + 
;;            2   2   2   2  . 
;;amin      1   1   3   3   +  .
;;  - .   2   2   4   4   +
;;bmin  1   3   3   3   +
;;        2   4   4   +
;;          3   5   +
;;            4   +
;;              +
;;       blim +   .

(declaim (ftype (function (rectilinear-row-index
                           rectilinear-column-index
                           rectilinear-row-index
                           rectilinear-column-index)
                          (values diagonal-row-index diagonal-column-index
                                  diagonal-row-index diagonal-column-index
                                  diagonal-row-index diagonal-column-index
                                  diagonal-row-index diagonal-column-index))
                rect-block->diag-block)
         (inline rect-block->diag-block))

(defun rect-block->diag-block (min-row max-row min-col max-col)
  "Given the min (inclusive) and max (exclusive) ranges of
   a rectangular block, return the eight values that are the
   diagonal coordinates of each corner.

   In diagonal coordinates, they are in order:

     The diagonal row on which the upper left corner begins,
     The minimum diagonal column,

     The minimum diagonal row,
     The diagonal column containing the minimal row,

     The maximum diagonal row,
     The diagonal column containing the maximal row,

     The diagonal row on which the lower right corner ends,
     The maximum diagonal column."
  (multiple-value-bind (ulc-diag-row min-diag-col)
      (rect->diag min-row min-col)
    (declare (type diagonal-row-index ulc-diag-row)
             (type diagonal-column-index min-diag-col))
    (multiple-value-bind (lrc-diag-row max-diag-col)
        (rect->diag max-row max-col)
      (declare (type diagonal-row-index lrc-diag-row)
               (type diagonal-column-index max-diag-col))
      (multiple-value-bind (min-diag-row urc-diag-col)
          (rect->diag min-row max-col)
        (declare (type diagonal-row-index min-diag-row)
                 (type diagonal-column-index urc-diag-col))
        (multiple-value-bind (max-diag-row llc-diag-col)
            (rect->diag max-row min-col)
          (declare (type diagonal-row-index max-diag-row)
                   (type diagonal-column-index llc-diag-col))
          (values ulc-diag-row min-diag-col
                  lrc-diag-row max-diag-col
                  min-diag-row urc-diag-col
                  max-diag-row llc-diag-col))))))


(defun edit-distance (before bmin blim after amin alim equality-function)
  "Compute the `edit-distance' between sequence before and sequence after.
   The edit distance is the length of the weighted path in the edit graph
   from before to after."
  (declare (type rectilinear-row-index bmin blim amin alim)
           #.(performance-optimizations))
  (multiple-value-bind (ulc-diag-row min-diag-col
                        lrc-diag-row max-diag-col
                        min-diag-row urc-diag-col
                        max-diag-row llc-diag-col)
      (rect-block->diag-block bmin blim amin alim)
    (declare (ignore lrc-diag-row urc-diag-col llc-diag-col
                     min-diag-row max-diag-row)
             (type diagonal-row-index
                   ulc-diag-row lrc-diag-row
                   min-diag-row max-diag-row)
             (type diagonal-column-index
                   min-diag-col max-diag-col
                   urc-diag-col llc-diag-col))
    (let* ((vsize  (fix:* (fix:1+ (fix:- max-diag-col min-diag-col)) 2))
           (varray (make-array vsize)))
      (declare (type simple-vector varray))
      (macrolet ((V (index)
                    `(SVREF VARRAY (fix:+
                                    (fix:- ,index ULC-DIAG-ROW)
                                    (fix:- max-diag-col min-diag-col)))))
        (setf (V (fix:1+ ulc-diag-row)) bmin)
        (iterate ((d (scan-range :from min-diag-col :upto max-diag-col)))
          (declare (type diagonal-column-index d))
          (iterate ((k (scan-range :from (fix:- ulc-diag-row (fix:- d min-diag-col))
                                   :upto (fix:+ ulc-diag-row (fix:- d min-diag-col))
                                   :by 2)))
            (declare (type diagonal-row-index k))
            (do* ((row (cond ((fix:= k (fix:- ulc-diag-row (fix:- d min-diag-col))) (V (fix:1+ k)))
                             ((fix:= k (fix:+ ulc-diag-row (fix:- d min-diag-col))) (fix:1+ (V (fix:1- k))))
                             (t (let ((v1 (V (fix:1- k)))
                                      (v2 (V (fix:1+ k))))
                                  (if (fix:< v1 v2)
                                      v2
                                      (fix:1+ v1)))))
                       (fix:1+ row))
                  (col (fix:- row k) (fix:1+ col)))
                ((or (fix:< row bmin) (fix:>= row blim)
                     (fix:< col amin) (fix:>= col alim)
                     (not (funcall equality-function
                                   (elt before row)
                                   (elt after col))))
                 (setf (V k) row)
                 (when (and (fix:>= row blim)
                            (fix:>= col alim))
                   (return-from edit-distance (fix:- d min-diag-col))))
              (declare (type fixnum row col)))))))))

(defun edit-path-midpoint (before bmin blim after amin alim equality-function)
  "Compute the midpoint of the edit path."
  (declare (type rectilinear-row-index bmin blim amin alim)
           ;; #.(performance-optimizations)
           )

  ;; Must start and end on diff, must have at least 2 elements in each.
  (assert (fix:>= (fix:- blim bmin) 2))
  (assert (fix:>= (fix:- alim amin) 2))
  (assert (not (funcall equality-function
                        (elt before bmin)
                        (elt after amin))))
  (assert (not (funcall equality-function
                        (elt before (fix:1- blim))
                        (elt after  (fix:1- alim)))))
  (multiple-value-bind (ulc-diag-row min-diag-col
                        lrc-diag-row max-diag-col
                        min-diag-row urc-diag-col
                        max-diag-row llc-diag-col)
      (rect-block->diag-block bmin blim amin alim)
    (declare (type diagonal-row-index
                   ulc-diag-row lrc-diag-row
                   min-diag-row max-diag-row)
             (type diagonal-column-index
                   min-diag-col max-diag-col
                   urc-diag-col llc-diag-col))
    (let* ((odd (oddp (- ulc-diag-row lrc-diag-row)))
           (vsize  (fix:* (fix:+ (fix:- max-diag-row min-diag-row) 2) 2))
           (fwd-vec (make-array vsize :initial-element  (1- bmin)))
           (rev-vec (make-array vsize :initial-element (1+ blim)))
           (vec-offset (fix:1- min-diag-row)))
      (declare (type simple-vector fwd-vec rev-vec)
               (type diagonal-row-index vec-offset))
      (macrolet ((FV (index)
                     `(SVREF FWD-VEC (FIX:- ,index VEC-OFFSET)))
                 (RV (index)
                     `(SVREF REV-VEC (FIX:- ,index VEC-OFFSET))))
        (setf (FV ulc-diag-row) bmin)
        (setf (RV lrc-diag-row) blim)
        (let ((edit-distance 0)
              (fwd-col (fix:1- min-diag-col))
              (fwd-min ulc-diag-row)
              (fwd-lim ulc-diag-row)
              (rev-col max-diag-col)
              (rev-min lrc-diag-row)
              (rev-lim lrc-diag-row))
          (declare (type fixnum count)
                   (type diagonal-row-index
                         fwd-min fwd-lim
                         rev-min rev-lim)
                   (type diagonal-column-index fwd-col rev-col))
          ;; It's in the nature of algorithms to be like this.
          (loop

           ;; bump the step and move the forward trace over by one.
           (incf edit-distance)
           (incf fwd-col)
           ;; bottom of fwd col moves down until we reach the
           ;; upper right corner, then it moves up.
           (setq fwd-min (if (fix:< fwd-col urc-diag-col)
                             (fix:1- fwd-min)
                             (fix:1+ fwd-min)))
           ;; top of fwd col moves up until we reach the
           ;; lower left corner, then it moves down.
           (setq fwd-lim (if (fix:< fwd-col llc-diag-col)
                             (fix:1+ fwd-lim)
                             (fix:1- fwd-lim)))

           ;; Sanity check
           ;(assert (fix:< fwd-min fwd-lim))
           ;(assert (fix:<= min-diag-row fwd-min))
           ;(assert (fix:<= fwd-lim max-diag-row))

           ;; Scan the forward column extending the snakes
           (do ((diag-row fwd-lim    (fix:- diag-row 2)))
               ((fix:< diag-row fwd-min))
             (declare (type diagonal-row-index diag-row))
             ;; Check how far the diagonal above and below us got
             (let ((thi (FV (fix:1+ diag-row)))
                   (tlo (FV (fix:1- diag-row))))
               (declare (type rectilinear-row-index thi tlo))

               ;; Start extending this snake
               (do* ((srow (if (fix:>= tlo thi) (fix:1+ tlo) thi)
                           (fix:1+ srow))
                     (scol (fix:- srow diag-row) (fix:1+ scol)))
                   ((or (fix:>= srow blim)
                        (fix:>= scol alim)
                        (not (funcall equality-function
                                      (elt before srow)
                                      (elt after scol))))
                    ;; record how far we got
                    (setf (FV diag-row) srow)
                    ;; check if met up with reverse
                    (when (and odd
                               (fix:<= rev-min diag-row)
                               (fix:<= diag-row rev-lim)
                               (fix:<= (RV diag-row) srow))
                      (return-from edit-path-midpoint
                        (values edit-distance srow scol))))
                 (declare (type rectilinear-row-index srow)
                          (type rectilinear-column-index scol)))))


           ;; If we have passed the other column, we ought to have
           ;; found the midpoint!
           (when (fix:> fwd-col rev-col)
             (error "Couldn't find midpoint"))


           ;; Now do the reverse column
           (incf edit-distance)
           ;; adjust top and bottom of reverse column
           (setq rev-min (if (fix:> rev-col urc-diag-col)
                             (fix:1- rev-min)
                             (fix:1+ rev-min)))
           (setq rev-lim (if (fix:> rev-col llc-diag-col)
                             (fix:1+ rev-lim)
                             (fix:1- rev-lim)))
           (decf rev-col)

           ;; Sanity check
           ;(assert (fix:< rev-min rev-lim))
           ;(assert (fix:<= min-diag-row rev-min))
           ;(assert (fix:<= rev-lim max-diag-row))

           ;; Scan the reverse column extending the snakes
           (do ((diag-row rev-min    (fix:+ diag-row 2)))
               ((fix:> diag-row rev-lim))
             (declare (type diagonal-row-index diag-row))
             (let ((tlo (RV (fix:1- diag-row)))
                   (thi (RV (fix:1+ diag-row))))
               (declare (type rectilinear-row-index thi tlo))
               
               (do* ((srow (if (fix:< tlo thi) tlo (fix:1- thi)) (fix:1- srow))
                     (scol (fix:- srow diag-row) (fix:1- scol)))
                   ((or (not (fix:> srow bmin))
                        (not (fix:> scol amin))
                        (not (funcall equality-function
                                      (elt after  (fix:1- scol))
                                      (elt before (fix:1- srow)))))

                    (setf (RV diag-row) srow)
                    (when (and (not odd)
                               (fix:<= fwd-min diag-row)
                               (fix:<= diag-row fwd-lim)
                               (fix:<= srow (FV diag-row)))
                      (return-from edit-path-midpoint
                        (values edit-distance srow scol))))

                     (declare (type rectilinear-row-index srow)
                              (type rectilinear-column-index scol)))))

           ;; If we pass the other guy, we've lost,
           ;; otherwise it's back to the top of the loop.
           (when (fix:< rev-col fwd-col)
             (error "Couldn't find midpoint"))))))))

(defun svf-edit-path-midpoint (before bmin blim after amin alim)
  "Compute the midpoint of the edit path.
   Specialized for the case where before and after are simple vectors
   containing only fixnums."
  (declare (type rectilinear-row-index bmin blim amin alim)
           (type simple-vector before after)
           #.(performance-optimizations)
           )

;  ;; Must start and end on diff, must have at least 2 elements in each.
;  (assert (fix:>= (fix:- blim bmin) 2))
;  (assert (fix:>= (fix:- alim amin) 2))
;  (assert (not (fix:=
;                        (svref before bmin)
;                        (svref after amin))))
;  (assert (not (fix:=
;                        (svref before (fix:1- blim))
;                        (svref after  (fix:1- alim)))))
  (multiple-value-bind (ulc-diag-row min-diag-col
                        lrc-diag-row max-diag-col
                        min-diag-row urc-diag-col
                        max-diag-row llc-diag-col)
      (rect-block->diag-block bmin blim amin alim)
    (declare (type diagonal-row-index
                   ulc-diag-row lrc-diag-row
                   min-diag-row max-diag-row)
             (type diagonal-column-index
                   min-diag-col max-diag-col
                   urc-diag-col llc-diag-col))
    (let* ((odd (oddp (- ulc-diag-row lrc-diag-row)))
           (vsize  (fix:* (fix:+ (fix:- max-diag-row min-diag-row) 2) 2))
           (fwd-vec (make-array vsize :initial-element (1- bmin)))
           (rev-vec (make-array vsize :initial-element (1+ blim)))
           (vec-offset (fix:1- min-diag-row)))
      (declare (type simple-vector fwd-vec rev-vec)
               (type diagonal-row-index vec-offset))
      (macrolet ((FV (index)
                     `(SVREF FWD-VEC (FIX:- ,index VEC-OFFSET)))
                 (RV (index)
                     `(SVREF REV-VEC (FIX:- ,index VEC-OFFSET))))
        (setf (FV ulc-diag-row) bmin)
        (setf (RV lrc-diag-row) blim)
        (let ((edit-distance 0)
              (fwd-col (fix:1- min-diag-col))
              (fwd-min ulc-diag-row)
              (fwd-lim ulc-diag-row)
              (rev-col max-diag-col)
              (rev-min lrc-diag-row)
              (rev-lim lrc-diag-row))
          (declare (type fixnum count)
                   (type diagonal-row-index
                         fwd-min fwd-lim
                         rev-min rev-lim)
                   (type diagonal-column-index fwd-col rev-col))
          ;; It's in the nature of algorithms to be like this.
          (loop

           ;; bump the step and move the forward trace over by one.
           (incf edit-distance)
           (incf fwd-col)
           ;; bottom of fwd col moves down until we reach the
           ;; upper right corner, then it moves up.
           (setq fwd-min (if (fix:< fwd-col urc-diag-col)
                             (fix:1- fwd-min)
                             (fix:1+ fwd-min)))
           ;; top of fwd col moves up until we reach the
           ;; lower left corner, then it moves down.
           (setq fwd-lim (if (fix:< fwd-col llc-diag-col)
                             (fix:1+ fwd-lim)
                             (fix:1- fwd-lim)))

           ;; Sanity check
           ;(assert (fix:< fwd-min fwd-lim))
           ;(assert (fix:<= min-diag-row fwd-min))
           ;(assert (fix:<= fwd-lim max-diag-row))

           ;; Scan the forward column extending the snakes
           (do ((diag-row fwd-lim    (fix:- diag-row 2)))
               ((fix:< diag-row fwd-min))
             (declare (type diagonal-row-index diag-row))
             ;; Check how far the diagonal above and below us got
             (let ((thi (FV (fix:1+ diag-row)))
                   (tlo (FV (fix:1- diag-row))))
               (declare (type rectilinear-row-index thi tlo))

               ;; Start extending this snake
               (do* ((srow (if (fix:>= tlo thi) (fix:1+ tlo) thi)
                           (fix:1+ srow))
                     (scol (fix:- srow diag-row) (fix:1+ scol)))
                   ((or (fix:>= srow blim)
                        (fix:>= scol alim)
                        (not (fix:=
                              (svref before srow)
                              (svref after scol))))
                    ;; record how far we got
                    (setf (FV diag-row) srow)
                    ;; check if met up with reverse
                    (when (and odd
                               (fix:<= rev-min diag-row)
                               (fix:<= diag-row rev-lim)
                               (fix:<= (RV diag-row) srow))
                      (return-from svf-edit-path-midpoint
                        (values edit-distance srow scol))))
                 (declare (type rectilinear-row-index srow)
                          (type rectilinear-column-index scol)))))

           ;; Now do the reverse column
           (incf edit-distance)
           ;; adjust top and bottom of reverse column
           (setq rev-min (if (fix:> rev-col urc-diag-col)
                             (fix:1- rev-min)
                             (fix:1+ rev-min)))
           (setq rev-lim (if (fix:> rev-col llc-diag-col)
                             (fix:1+ rev-lim)
                             (fix:1- rev-lim)))
           (decf rev-col)

           ;; Sanity check
           ;(assert (fix:< rev-min rev-lim))
           ;(assert (fix:<= min-diag-row rev-min))
           ;(assert (fix:<= rev-lim max-diag-row))

           ;; Scan the reverse column extending the snakes
           (do ((diag-row rev-min    (fix:+ diag-row 2)))
               ((fix:> diag-row rev-lim))
             (declare (type diagonal-row-index diag-row))
             (let ((tlo (RV (fix:1- diag-row)))
                   (thi (RV (fix:1+ diag-row))))
               (declare (type rectilinear-row-index thi tlo))
               
               (do* ((srow (if (fix:< tlo thi) tlo (fix:1- thi)) (fix:1- srow))
                     (scol (fix:- srow diag-row) (fix:1- scol)))
                   ((or (not (fix:> srow bmin))
                        (not (fix:> scol amin))
                        (not (fix:=
                              (svref after  (fix:1- scol))
                              (svref before (fix:1- srow)))))

                    (setf (RV diag-row) srow)
                    (when (and (not odd)
                               (fix:<= fwd-min diag-row)
                               (fix:<= diag-row fwd-lim)
                               (fix:<= srow (FV diag-row)))
                      (return-from svf-edit-path-midpoint
                        (values edit-distance srow scol))))

                     (declare (type rectilinear-row-index srow)
                              (type rectilinear-column-index scol)))))
           ))))))

;;; Edit path
;;;
;;; An edit path is a list of INDELs
;;; Each INDEL contains the bounds of two subsequences, one from each
;;; record set being diffed.

(defstruct (indel
            (:conc-name indel/))
  (left-subseq-start  0 :type array-index)
  (left-subseq-limit  0 :type array-index)
  (right-subseq-start 0 :type array-index)
  (right-subseq-limit 0 :type array-index))

(defun join-indel-lists (l1 l2)
  (cond ((null l1) l2)
        ((null l2) l1)
        (t (let* ((r1 (nreverse l1))
                  (last1 (car r1))
                  (first2 (car l2)))
             (if (and (= (indel/left-subseq-limit last1)
                         (indel/left-subseq-start first2))
                      (= (indel/right-subseq-limit last1)
                         (indel/right-subseq-start first2)))
                 (nreconc (cdr r1)
                          (cons (make-indel
                                 :left-subseq-start (indel/left-subseq-start last1)
                                 :left-subseq-limit (indel/left-subseq-limit first2)
                                 :right-subseq-start (indel/right-subseq-start last1)
                                 :right-subseq-limit (indel/right-subseq-limit first2))
                                 (cdr l2)))
                 (nreconc r1 l2))))))

(defparameter *edit-path-max-recursion-depth* 256)

(defun edit-path-loop (recursion-depth left l-start l-limit right r-start r-limit equality-function)
  (declare ;; #.(performance-optimizations)
           (type array-index l-start l-limit r-start r-limit)
           (type non-negative-fixnum recursion-depth))

  ;; Sanity check
;  (assert (<= l-start l-limit))
;  (assert (<= r-start r-limit))

  (cond ((= r-start r-limit)
         (unless (= l-start l-limit)
           (list (make-indel :left-subseq-start l-start
                             :left-subseq-limit l-limit
                             :right-subseq-start r-start
                             :right-subseq-limit r-limit))))

        ((= l-start l-limit)
         (list (make-indel :left-subseq-start l-start
                           :left-subseq-limit l-limit
                           :right-subseq-start r-start
                           :right-subseq-limit r-limit)))

        ((funcall equality-function
                  (elt left l-start)
                  (elt right r-start))
         ;; base case 3, at least one common head element
         (let ((mis (mismatch left right
                              :start1 l-start :end1 l-limit
                              :start2 r-start :end2 r-limit
                              :test equality-function)))
           (unless (null mis);; base case 4, identical sequences, return NIL
             ;; (assert (<= l-start (the array-index mis) l-limit));; sanity
             (edit-path-loop recursion-depth
                             left mis l-limit
                             right (+ r-start (- mis l-start)) r-limit
                             equality-function))))

        ((funcall equality-function
                  (elt left (1- l-limit))
                  (elt right (1- r-limit)))
         ;; base case 5, common tail element
         (let ((mis (mismatch left right
                              :start1 l-start :end1 l-limit
                              :start2 r-start :end2 r-limit
                              :from-end t
                              :test equality-function)))
           (edit-path-loop recursion-depth
                           left l-start mis
                           right r-start (- r-limit (- l-limit mis))
                           equality-function)))

        ((or (= (- l-limit l-start) 1)
             (= (- r-limit r-start) 1))
         ;; base case 2, replacing single element
         (list (make-indel :left-subseq-start l-start
                           :left-subseq-limit l-limit
                           :right-subseq-start r-start
                           :right-subseq-limit r-limit)))

        ((> recursion-depth *edit-path-max-recursion-depth*)
         ;; punt case just call the whole thing different.
         ;; (format t "~&Exceeding recursion limit on ndiff.")
         (list (make-indel :left-subseq-start l-start
                           :left-subseq-limit l-limit
                           :right-subseq-start r-start
                           :right-subseq-limit r-limit)))

        (t (multiple-value-bind (count mid-row mid-col)
               (edit-path-midpoint left l-start l-limit
                                    right r-start r-limit equality-function)
             ;; (assert (> count 0))
             (if (= (+ (- l-limit l-start)
                       (- r-limit r-start)
                       1)
                    count)
                 (list (make-indel :left-subseq-start l-start
                                   :left-subseq-limit l-limit
                                   :right-subseq-start r-start
                                   :right-subseq-limit r-limit))
                 (join-indel-lists
                  (edit-path-loop (1+ recursion-depth)
                                  left l-start mid-row
                                  right r-start mid-col
                                  equality-function)
                  (edit-path-loop  (1+ recursion-depth)
                                   left mid-row l-limit
                                   right mid-col r-limit
                                   equality-function)))))))

(defun svf-mismatch (left right l-start l-limit r-start r-limit)
  "Like mismatch, but explicit start and end.
   Sequences are assumed to be simple vector of fixnums."
  (declare #.(performance-optimizations)
           (type simple-vector left right)
           (array-index l-start l-limit r-start r-limit))
  (cond ((= l-start l-limit) (unless (= r-start r-limit)
                               l-limit))
        ((= r-start r-limit) l-start)
        ((fix:= (svref left l-start)
                (svref right r-start)) (svf-mismatch left right
                                                     (fix:1+ l-start) l-limit
                                                     (fix:1+ r-start) r-limit))
        (t l-start)))

(defun svf-mismatch-from-end (left right l-start l-limit r-start r-limit)
  "Like mismatch, but explicit start and end.
   Sequences are assumed to be simple vector of fixnums."
  (declare #.(performance-optimizations)
           (type simple-vector left right)
           (array-index l-start l-limit r-start r-limit))
  (decf l-limit)
  (decf r-limit)
  (cond ((> l-start l-limit) (unless (> r-start r-limit)
                               (1+ l-limit)))
        ((> r-start r-limit) (1+ l-limit))
        ((fix:= (svref left l-limit)
                (svref right r-limit)) (svf-mismatch-from-end left right
                                                     l-start l-limit
                                                     r-start r-limit))
        (t (1+ l-limit))))

(defun svf-edit-path-loop (recursion-depth left l-start l-limit right r-start r-limit)
  "Edit path loop specialized for the case of simple vectors of fixnums."
  (declare #.(performance-optimizations)
           (type simple-vector left right)
           (type array-index l-start l-limit r-start r-limit)
           (type non-negative-fixnum recursion-depth))

  (cond ((= r-start r-limit)
         (unless (= l-start l-limit)
           (list (make-indel :left-subseq-start l-start
                             :left-subseq-limit l-limit
                             :right-subseq-start r-start
                             :right-subseq-limit r-limit))))

        ((= l-start l-limit)
         (list (make-indel :left-subseq-start l-start
                           :left-subseq-limit l-limit
                           :right-subseq-start r-start
                           :right-subseq-limit r-limit)))

        ((fix:=
          (svref left l-start)
          (svref right r-start))
         ;; base case 3, at least one common head element
         (let ((mis (svf-mismatch left right l-start l-limit r-start r-limit)))
           (unless (null mis);; base case 4, identical sequences, return NIL
             ;; (assert (<= l-start (the array-index mis) l-limit));; sanity
             (svf-edit-path-loop recursion-depth
                                 left mis l-limit
                                 right (+ r-start (- mis l-start)) r-limit
                                 ))))

        ((fix:=
          (svref left (1- l-limit))
          (svref right (1- r-limit)))
         ;; base case 5, common tail element
         (let ((mis (svf-mismatch-from-end left right l-start l-limit r-start r-limit)))
           (svf-edit-path-loop recursion-depth
                               left l-start mis
                               right r-start (- r-limit (- l-limit mis))
                               )))

        ((or (= (- l-limit l-start) 1)
             (= (- r-limit r-start) 1))
         ;; base case 2, replacing single element
         (list (make-indel :left-subseq-start l-start
                           :left-subseq-limit l-limit
                           :right-subseq-start r-start
                           :right-subseq-limit r-limit)))

        ((> recursion-depth *edit-path-max-recursion-depth*)
         ;; punt case just call the whole thing different.
         ;; (format t "~&Exceeding recursion limit on ndiff.")
         (list (make-indel :left-subseq-start l-start
                           :left-subseq-limit l-limit
                           :right-subseq-start r-start
                           :right-subseq-limit r-limit)))

        (t (multiple-value-bind (count mid-row mid-col)
               (svf-edit-path-midpoint left l-start l-limit
                                       right r-start r-limit)
             ;; (assert (> count 0))
             (if (= (+ (- l-limit l-start)
                       (- r-limit r-start)
                       1)
                    count)
                 (list (make-indel :left-subseq-start l-start
                                   :left-subseq-limit l-limit
                                   :right-subseq-start r-start
                                   :right-subseq-limit r-limit))
                 (join-indel-lists
                  (svf-edit-path-loop (1+ recursion-depth)
                                      left l-start mid-row
                                      right r-start mid-col
                                      )
                  (svf-edit-path-loop  (1+ recursion-depth)
                                       left mid-row l-limit
                                       right mid-col r-limit
                                       )))))))

(defun edit-path (left l-start l-limit right r-start r-limit equality-function)
  (edit-path-loop 0
                  left l-start l-limit
                  right  r-start r-limit equality-function))


(defun svf-edit-path (left l-start l-limit right r-start r-limit)
  "Assumes that LEFT and RIGHT are simple vectors with FIXNUM elements.
   Test is assumed to be =."
  (svf-edit-path-loop 0
                      left l-start l-limit
                      right  r-start r-limit))

#||

  ;; 0 1 2

  ;; a
  ;;   1
  ;; 0   2
  ;;   1
  ;; b


  ;; 0 1 2 3 4 
  ;;   d
  ;; c   2
  ;;   1   3
  ;; 0   2   4
  ;;   1   3
  ;; a   2
  ;;   b

  ;; 0 1 2 3 4 5 6
  ;;         
  ;;       d 4       -4
  ;;     c 3   3     -3
  ;;   b 2   2   2   -2
  ;; a 1   1   1     -1
  ;; 0   0   0        0
  ;; a 1   1          1
  ;;   b 2            2
  ;;   

  ;;                     1 1
  ;; 0 1 2 3 4 5 6 7 8 9 0 1
  ;;
  ;;           y 6            -6
  ;;         e 5   5          -5
  ;;       v 4   4   4        -4
  ;;     r 3   3   3   3      -3
  ;;   a 2   2   2   2   4    -2
  ;; g 1   1   1   3   3   3  -1
  ;; 0   2   2   4   4   4     0
  ;; a 1   3   3   3   5       1
  ;;   v 2   4   4   4         2
  ;;     e 3   5   5           3
  ;;       r 4   6             4
  ;;         y 5               5


(defun edit-path-midpoint (before bmin blim
                                  after  amin alim)
  (declare (type (integer 0 #.(floor array-dimension-limit 2))  bmin amin)
           (type (integer 0 #.(1+ (floor array-dimension-limit 2))) blim alim)
           ;; #.(performance-optimizations)
           )

  ;; Must start and end on diff, must have at least 2 elements in each.
  (assert (>= (- blim bmin) 2))
  (assert (>= (- alim amin) 2))
  (assert (not (eql (elt before bmin)
                    (elt after amin))))
  (assert (not (eql (elt before (1- blim))
                    (elt after (1- alim)))))

  (multiple-value-bind (fwd-diag-row min-diag-col)
      (r&c->dr&dc bmin amin)
    (declare (type (integer #.(- (floor array-dimension-limit 2))
                            #.(floor array-dimension-limit 2)) fwd-diag-row)
             (type array-index min-diag-col))
    (multiple-value-bind (rev-diag-row max-diag-col)
        (r&c->dr&dc blim alim)
      (declare (type (integer #.(- (floor array-dimension-limit 2))
                              #.(floor array-dimension-limit 2)) rev-diag-row)
               (type array-index max-diag-col))
      (multiple-value-bind (min-diag-row ndr-col)
          (r&c->dr&dc bmin alim)
        (declare (type (integer #.(- (floor array-dimension-limit 2))
                                #.(floor array-dimension-limit 2)) min-diag-row)
                 (type array-index ndr-col))
        (multiple-value-bind (max-diag-row xdr-col)
            (r&c->dr&dc blim amin)
          (declare (type (integer #.(- (floor array-dimension-limit 2))
                                  #.(floor array-dimension-limit 2)) max-diag-row)
                   (type array-index xdr-col))
          (let ((odd (oddp (- fwd-diag-row rev-diag-row)))
                (fwd-vec (make-array (+ (- max-diag-row min-diag-row) 4)
                                     :initial-element  (1- bmin)))
                (rev-vec (make-array (+ (- max-diag-row min-diag-row) 4)
                                     :initial-element (1+ blim)))
                (vec-offset (1- min-diag-row)))
            (declare (type simple-vector fwd-vec rev-vec)
                     (type (integer #.(1- (- (floor array-dimension-limit 2)))
                                    #.(1- (floor array-dimension-limit 2))) vec-offset))
            (format t "~&~@{~&~s = ~s~}"
                    'min-diag-col min-diag-col
                    'max-diag-col max-diag-col
                    'min-diag-row min-diag-row
                    'max-diag-row max-diag-row
                    'ndr-col ndr-col
                    'xdr-col xdr-col)
            (setf (svref fwd-vec (- fwd-diag-row vec-offset)) bmin)
            (setf (svref rev-vec (- rev-diag-row vec-offset)) blim)
            (let ((count 1)
                  (fwd-col  (1- min-diag-col))
                  (fwd-min  fwd-diag-row)
                  (fwd-lim  fwd-diag-row)
                  (rev-col max-diag-col)
                  (rev-min rev-diag-row)
                  (rev-lim rev-diag-row))
              (declare (type fixnum count)
                       (type (integer 0 #.(floor array-dimension-limit 2))
                             fwd-min fwd-lim
                             rev-min rev-lim)
                       (type array-index fwd-col rev-col))
              ;; sigh
              (tagbody
               :forward-step
                                           (incf fwd-col)
                                           (setq fwd-min (if (< fwd-col ndr-col)
                                                             (1- fwd-min)
                                                             (1+ fwd-min)))
                                           (setq fwd-lim (if (< fwd-col xdr-col)
                                                             (1+ fwd-lim)
                                                             (1- fwd-lim)))
                                           (incf count)


               (format t "~&Forward step ~s ~s, fwd-col= ~d, fwd-min = ~d, fwd-lim = ~d"
                       fwd-vec rev-vec fwd-col fwd-min fwd-lim)
               (assert (< fwd-min fwd-lim))
               (assert (<= min-diag-row fwd-min))
               (assert (<= fwd-lim max-diag-row))
               (do ((diag-row fwd-lim (- diag-row 2)))
                   ((< diag-row fwd-min) (unless (> fwd-col rev-col)
                                           (go :reverse-step))
                    (return-from edit-path-midpoint))

                 (declare (type (integer #.(- (floor array-dimension-limit 2))
                                         #.(floor array-dimension-limit 2))
                                diag-row))
                         
                 (format t "~&Scanning diagonal row ~d" diag-row)
                 (let* ((tlo (svref fwd-vec (- diag-row vec-offset 1)))
                        (thi (svref fwd-vec (- diag-row vec-offset -1)))
                        (start (if (>= tlo thi) (1+ tlo) thi)))

                   (declare (type (integer -1 #.array-dimension-limit) tlo thi)
                            (type array-index start))
                   (format t "~&tlo = ~d, thi = ~d, start = ~d" tlo thi start)
                   (do ((srow start (1+ srow))
                        (scol (- start diag-row) (1+ scol)))
                       ((progn
                          (format t "~&scan loop srow = ~s, scol = ~s" srow scol)
                          (or (>= srow blim)
                            (>= scol alim)
                            (not (eql (aref before srow)
                                      (aref after scol)))))
                        (format t "~&mismatch, ~s vs. ~s at ~s, ~s"
                                (and (< srow blim) (aref before srow))
                                (and (< scol alim) (aref after scol))
                                srow scol)
                        (setf (svref fwd-vec (- diag-row vec-offset)) srow)
                        (when (and odd
                                   (<= rev-min diag-row rev-lim)
                                   (<= (svref rev-vec (- diag-row vec-offset)) srow))
                          (format t "~&fwdstep return") (finish-output)
                          (return-from edit-path-midpoint
                            (values count srow scol))))
                     
                     (declare (type (integer -1 #.array-dimension-limit) srow scol))
                     )))

               :reverse-step
                                           (setq rev-min (if (> rev-col ndr-col)
                                                             (1- rev-min)
                                                             (1+ rev-min)))
                                           (setq rev-lim (if (> rev-col xdr-col)
                                                             (1+ rev-lim)
                                                             (1- rev-lim)))
                                           (incf count)
                                           (decf rev-col)
               (format t "~&Reverse step ~s ~s ~d ~d" fwd-vec rev-vec rev-min rev-lim)
               (do ((diag-row rev-min (+ diag-row 2)))
                   ((> diag-row rev-lim) (unless (< rev-col fwd-col)
                                           (go :forward-step))
                    (return-from edit-path-midpoint))
                 (declare (type (integer #.(- (floor array-dimension-limit 2))
                                         #.(floor array-dimension-limit 2))
                                diag-row))

                 (format t "~&Scanning diagonal row ~d" diag-row)
                 (let* ((tlo (svref rev-vec (- diag-row vec-offset 1)))
                        (thi (svref rev-vec (- diag-row vec-offset -1)))
                        (end (if (< tlo thi) tlo (1- thi))))
                   (declare (type (integer -1 #.array-dimension-limit) tlo thi)
                            (type array-index end))
                   (format t "~&tlo = ~d, thi = ~d, end = ~d" tlo thi end)
                   (do ((srow end (1- srow))
                        (scol (- end diag-row) (1- scol)))
                       ((progn
                          (format t "~&scan loop srow = ~s, scol = ~s" (1- srow) (1- scol))
                          (or (not (> srow bmin))
                              (not (> scol amin))
                              (not (eq (svref after  (1- scol))
                                       (svref before (1- srow))))))
                          (format t "~&mismatch, ~s vs. ~s at ~s, ~s"
                                (and (> srow 0) (aref before (1- srow)))
                                (and (> scol 0) (aref after (1- scol)))
                                (1- srow) (1- scol))
                        (setf (svref rev-vec (- diag-row vec-offset)) srow)
                        (when (and (not odd)
                                   (<= fwd-min diag-row fwd-lim)
                                   (<= srow (svref fwd-vec (- diag-row vec-offset))))
                          (format t "~&revstep return") (finish-output)
                          (return-from edit-path-midpoint
                            (values count srow scol))))

                     (declare (type (integer -1 #.array-dimension-limit) srow scol)))))))))))))

(defun make-insertion-record (element-count)
  (cons :insert element-count))

(defun make-deletion-record (element-count)
  (cons :delete element-count))

(defun make-retain-record (element-count)
  (cons :keep element-count))

(defun edit-path (before after)
  (let ((recursion-limit 32));; greater than this, we punt.
    (labels ((edit-path-loop (before bmin blim after amin alim)
               (format t "~&edit-path-loop ~s ~s"
                       (subseq before bmin blim)
                       (subseq after amin alim))

               (when (minusp (decf recursion-limit))
                 (error "Could not find edit path."))
               
               (cond
                ((zerop (- blim bmin))
                 (unless (zerop (- alim amin));; base case 1, both empty
                   (list (make-insertion-record (- alim amin)))));; base case 2, no deletions
                ((zerop (- alim amin))
                 (list (make-deletion-record (- blim bmin))));; base case 3, no insertions
                ((eql (elt before bmin)
                      (elt after amin))
                 ;; base case 4, common head
                 (let ((mis (mismatch before after
                                      :start1 bmin :end1 blim
                                      :start2 amin :end2 alim)))
                   (cond ((null mis);; base case 5, identical sequences
                          (list (make-retain-record (- blim bmin))))
                         ((<= mis bmin) (error "mismatch returned a bogus answer"))
                         (t (cons (make-retain-record (- mis bmin))
                                  (edit-path-loop before mis blim
                                                  after (+ amin (- mis bmin)) alim))))))
                ((eql (elt before (1- blim))
                      (elt after  (1- alim)))
                 ;; base case 6, common tail
                 (let ((mis (mismatch before after
                                      :start1 bmin :end1 blim
                                      :start2 amin :end2 alim
                                      :from-end t)))
                   (cond ((null mis) (error "Identical sequences caught in wrong branch."))
                         ((>= mis blim) (error "mismatch returned a bogus answer"))
                         (t (append (edit-path-loop before bmin mis
                                                    after amin (- alim (- blim mis)))
                                    (list (make-retain-record (- blim mis))))))))

                ((= (- blim bmin) 1) ;; base case 7 delete one element
                 (let ((p (position (elt before bmin) after :start amin :end alim)))
                   (if (null p)
                       (list (make-deletion-record 1) (make-insertion-record (- alim amin)))
                       (list (make-insertion-record (- p amin))
                             (make-retain-record 1)
                             (make-insertion-record (- alim p 1))))))

                ((= (- alim amin) 1) ;; base case 8 insert one element
                 (let ((p (position (elt after amin) before :start bmin :end blim)))
                   (if (null p)
                       (list (make-insertion-record 1) (make-deletion-record (- blim bmin)))
                       (list (make-deletion-record (- p bmin))
                             (make-retain-record 1)
                             (make-deletion-record (- blim p 1))))))


                (t (multiple-value-bind (count mid-row mid-col)
                                   (edit-path-midpoint before bmin blim
                                                       after amin alim)
                                 (cond
                                  ((zerop count) (error "sequences identical?"))
                                  ((= (1- count) (+ (- blim bmin)
                                               (- alim amin)))
                                   ;; no commonality at all
                                   (list (make-deletion-record (- blim bmin))
                                         (make-insertion-record (- alim amin))))
                                  (t
                                   (append
                                    (edit-path-loop before bmin mid-row after amin mid-col)
                                    (edit-path-loop before mid-row blim after mid-col alim)))))))))
      (edit-path-loop before 0 (length before)
                      after  0 (length after)))))

(defun list-length= (list amount)
  (cond ((consp list) (and (plusp amount)
                           (list-length= (cdr list) (1- amount))))
        ((null list) (zerop amount))
        (t (error "Improper list."))))

(defun length= (sequence amount)
  (check-type amount array-index)
  (etypecase sequence
    (null (zerop amount))
    (cons (and (plusp amount)
               (list-length= (cdr sequence) (1- amount))))
    (vector (= (length sequence) amount))))

(defun edit-path-tests ()
  "Test the damn edit path stuff."
  ;; base case 1, both empty
  (assert (null (edit-path #() #())))
  ;; base case 2, no deletions
  (let ((ep (edit-path #() #(a b c d))))
    (assert (and (length= ep 1)
                 (eq (car (first ep)) :insert))))
  ;; base case 3, no insertions
  (let ((ep (edit-path #(a b c d) #())))
    (assert (and (length= ep 1)
                 (eq (car (first ep)) :delete))))
  ;; base case 4, common head
  (let ((ep (edit-path #(a b c d) #(a b))))
    (assert (and (length= ep 2)
                 (eq (car (first ep)) :keep))))
  (let ((ep (edit-path #(a b) #(a b c d))))
    (assert (and (length= ep 2)
                 (eq (car (first ep)) :keep))))
  (let ((ep (edit-path #(a b x) #(a b c d))))
    (assert (and (length= ep 3)
                 (eq (car (first ep)) :keep))))

  ;; base case 5, identical sequences
  (let ((ep (edit-path #(a) #(a))))
    (assert (and (length= ep 1)
                 (eq (car (first ep)) :keep))))
  (let ((ep (edit-path #(a a a a a a) #(a a a a a a))))
    (assert (and (length= ep 1)
                 (eq (car (first ep)) :keep))))
  (let ((ep (edit-path #(a a a b a a) #(a a a b a a))))
    (assert (and (length= ep 1)
                 (eq (car (first ep)) :keep))))
  (let ((ep (edit-path #(a b c d e f g) #(a b c d e f g))))
    (assert (and (length= ep 1)
                 (eq (car (first ep)) :keep))))

  ;; base case 6, common tail
  (let ((ep (edit-path #(a b c d) #(c d))))
    (assert (and (length= ep 2)
                 (eq (car (second ep)) :keep))))
  (let ((ep (edit-path #(c d) #(a b c d))))
    (assert (and (length= ep 2)
                 (eq (car (second ep)) :keep))))
  (let ((ep (edit-path #(x c d) #(a b c d))))
    (assert (and (length= ep 3)
                 (eq (car (third ep)) :keep))))

  ;; base case 7, `delete' single element
  (let ((ep (edit-path #(x) #(a b c d))))
    (assert (and (length= ep 2)
                 (eq (car (first ep)) :delete))))

  (let ((ep (edit-path #(x) #(a b x d))))
    (assert (and (length= ep 3)
                 (eq (car (second ep)) :keep))))

  ;; base case 8, `insert' single element
  (let ((ep (edit-path #(a b c d) #(x))))
    (assert (and (length= ep 2)
                 (or (eq (car (first ep)) :insert)
                     (eq (car (second ep)) :insert)))))

  (let ((ep (edit-path #(a b x d) #(x))))
    (assert (and (length= ep 3)
                 (eq (car (second ep)) :keep))))
  )


(defun tryit (s1 s2)
  (let* ((e  (edit-distance s1 s2)))
    (multiple-value-bind (c a b)
        (edit-path-midpoint s1 0 (length s1) s2 0 (length s2))
      (format t "~&e = ~d, c = ~d" e c)
      (values e c a b))))

||#

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

(export '(
          +simple-vector/size-limit+
          +simple-vector-1b/size-limit+
          +simple-vector-4b/size-limit+
          +simple-vector-16b/size-limit+
          +simple-vector-32b/size-limit+
          +simple-vector-8b/size-limit+
          +simple-string-8b/size-limit+

          simple-vector-1b-allocate
          simple-vector-8b-allocate
          simple-vector-16b-allocate
          simple-vector-32b-allocate
          simple-vector-64b-allocate
          simple-string-8b-allocate
          simple-string-16b-allocate
          simple-vector-1b-ref
          simple-vector-8b-ref
          simple-vector-16b-ref
          simple-vector-32b-ref
          simple-vector-64b-ref
          simple-string-8b-ref
          simple-string-16b-ref
          simple-vector-1b-copy
          simple-vector-8b-copy
          simple-vector-16b-copy
          simple-vector-32b-copy
          simple-vector-64b-copy
          simple-string-8b-copy
          simple-string-16b-copy
          simple-string-8b->16b-copy
          simple-vector-1b-length
          simple-vector-8b-length
          simple-vector-16b-length
          simple-vector-32b-length
          simple-string-8b-length
          simple-string-16b-length
          simple-vector-1b-fill
          simple-vector-8b-fill
          simple-vector-16b-fill
          simple-vector-32b-fill
          simple-vector-64b-fill
          simple-string-8b-fill
          simple-string-16b-fill
          simple-string-fill

          simple-vector-1b-adjust
          simple-vector-8b-adjust
          simple-vector-16b-adjust
          simple-vector-32b-adjust
          simple-string-8b-adjust
          simple-string-16b-adjust
          simple-string-adjust

          simple-subvector-1b-move
          simple-subvector-8b-move
          simple-subvector-16b-move
          simple-subvector-32b-move
          simple-subvector-64b-move
          simple-string-8b-move
          simple-string-16b-move
          simple-string-move

          ;; Fast versions
          %simple-subvector-1b-move-left
          %simple-subvector-1b-move-right
          %simple-subvector-8b-move-left
          %simple-subvector-8b-move-right
          %simple-subvector-16b-move-left
          %simple-subvector-16b-move-right
          %simple-subvector-32b-move-left
          %simple-subvector-32b-move-right
          %simple-subvector-64b-move-left
          %simple-subvector-64b-move-right
          %simple-substring-8b-move-left
          %simple-substring-8b-move-right
          %simple-substring-16b-move-left
          %simple-substring-16b-move-right
          %simple-substring-move-left
          %simple-substring-move-right
          %simple-subvector-1b-fill
          %simple-subvector-8b-fill
          %simple-subvector-16b-fill
          %simple-subvector-32b-fill
          ))

(proclaim (performance-optimizations))

;; Experimentally determined constants for this lisp implementation.
;; Primary use is to avoid trying to allocate an array too big for the element type.

;;; These are *exclusive* limits.
(eval-when (:compile-toplevel :execute :load-toplevel)
(defconstant +simple-vector/size-limit+     (- (floor (+ array-dimension-limit 1) 4) 257))
(defconstant +simple-vector-1b/size-limit+  (- (+ array-dimension-limit 1) 2))
(defconstant +simple-vector-4b/size-limit+  (- (+ array-dimension-limit 1) 2))
(defconstant +simple-vector-16b/size-limit+ (- (floor (+ array-dimension-limit 1) 2) 9))
(defconstant +simple-vector-32b/size-limit+ (- (floor (+ array-dimension-limit 1) 4) 5))
(defconstant +simple-vector-8b/size-limit+  (- array-dimension-limit 16))
(defconstant +simple-string-8b/size-limit+  (- array-dimension-limit 16))
)

(declaim (ftype (function (array-index) simple-vector-1b)  simple-vector-1b-allocate)
         (ftype (function (array-index) simple-vector-8b)  simple-vector-8b-allocate)
         (ftype (function (array-index) simple-vector-16b) simple-vector-16b-allocate)
         (ftype (function (array-index) simple-vector-32b) simple-vector-32b-allocate)
         (ftype (function (array-index) simple-vector-64b) simple-vector-64b-allocate)
         (ftype (function (array-index) simple-string-8b)  simple-string-8b-allocate)
         (ftype (function (array-index) simple-string-16b) simple-string-16b-allocate)
         (ftype (function (simple-vector-1b  array-index) (unsigned-byte  1))  simple-vector-1b-ref)
         (ftype (function (simple-vector-8b  array-index) (unsigned-byte  8))  simple-vector-8b-ref)
         (ftype (function (simple-vector-16b array-index) (unsigned-byte 16)) simple-vector-16b-ref)
         (ftype (function (simple-vector-32b array-index) (unsigned-byte 32)) simple-vector-32b-ref)
         (ftype (function (simple-vector-64b array-index) (unsigned-byte 64)) simple-vector-64b-ref)
         (ftype (function (simple-string-8b  array-index) char-8b)  simple-string-8b-ref)
         (ftype (function (simple-string-16b array-index) char-16b) simple-string-16b-ref)
         (inline simple-vector-1b-ref
                 simple-vector-8b-ref
                 simple-vector-16b-ref
                 simple-vector-32b-ref
                 simple-vector-64b-ref
                 simple-string-8b-ref
                 simple-string-16b-ref
                 (setf simple-vector-1b-ref)
                 (setf simple-vector-8b-ref)
                 (setf simple-vector-16b-ref)
                 (setf simple-vector-32b-ref)
                 (setf simple-vector-64b-ref)
                 (setf simple-string-8b-ref)
                 (setf simple-string-16b-ref)))

(defun simple-vector-1b-allocate (size)
  (make-array size :element-type '(unsigned-byte 1)))

(defun simple-vector-8b-allocate (size)
  (make-array size :element-type '(unsigned-byte 8)))

(defun simple-vector-16b-allocate (size)
  (make-array size :element-type '(unsigned-byte 16)))

(defun simple-vector-32b-allocate (size)
  (make-array size :element-type '(unsigned-byte 32)))

(defun simple-vector-64b-allocate (size)
  (make-array size :element-type '(unsigned-byte 64)))

(defun simple-string-8b-allocate (size)
  (make-array size :element-type 'char-8b))

(defun simple-string-16b-allocate (size)
  (make-array size :element-type 'char-16b))

(declaim (ftype (function (simple-vector-1b) array-index) simple-vector-1b-length)
         (ftype (function (simple-vector-8b) array-index) simple-vector-8b-length)
         (ftype (function (simple-vector-16b) array-index) simple-vector-16b-length)
         (ftype (function (simple-vector-32b) array-index) simple-vector-32b-length)
         (inline simple-vector-1b-length simple-vector-8b-length
                 simple-vector-16b-length simple-vector-32b-length
                 simple-string-8b-length simple-string-16b-length))

(defmacro simple-vector-1b-length (simple-vector)
  `(THE (INTEGER 0 ,array-dimension-limit)
     (LET ((VECTOR ,simple-vector))
       (DECLARE (TYPE VECTOR SIMPLE-VECTOR-1B)
                ,(performance-optimizations))
       (LENGTH VECTOR))))

(defmacro simple-vector-8b-length (simple-vector)
  `(THE (INTEGER 0 ,array-dimension-limit)
     (LET ((VECTOR ,simple-vector))
       (DECLARE (TYPE VECTOR SIMPLE-VECTOR-8B)
                ,(performance-optimizations))
       (LENGTH VECTOR))))

(defmacro simple-vector-16b-length (simple-vector)
  `(THE (INTEGER 0 ,array-dimension-limit)
     (LET ((VECTOR ,simple-vector))
       (DECLARE (TYPE VECTOR SIMPLE-VECTOR-16B)
                ,(performance-optimizations))
       (LENGTH VECTOR))))

(defmacro simple-vector-32b-length (simple-vector)
  `(THE (INTEGER 0 ,array-dimension-limit)
     (LET ((VECTOR ,simple-vector))
       (DECLARE (TYPE VECTOR SIMPLE-VECTOR-32B)
                ,(performance-optimizations))
       (LENGTH VECTOR))))

(defmacro simple-string-8b-length (simple-base-string)
  `(THE (INTEGER 0 ,array-dimension-limit)
      (LET ((SIMPLE-BASE-STRING ,simple-base-string))
        (DECLARE (TYPE SIMPLE-BASE-STRING SIMPLE-BASE-STRING)
                ,(performance-optimizations))
        (LENGTH SIMPLE-BASE-STRING))))

(defmacro simple-string-16b-length (simple-text-string)
  `(THE (INTEGER 0 ,array-dimension-limit)
      (LET ((SIMPLE-TEXT-STRING ,simple-text-string))
        (DECLARE (TYPE LISPWORKS:SIMPLE-TEXT-STRING SIMPLE-TEXT-STRING)
                ,(performance-optimizations))
        (LENGTH SIMPLE-TEXT-STRING))))

(defmacro simple-string-length (simple-string)
  `(THE (INTEGER 0 ,array-dimension-limit)
      (LET ((SIMPLE-STRING ,simple-string))
        (DECLARE (TYPE SIMPLE-STRING SIMPLE-STRING)
                ,(performance-optimizations))
        (LENGTH SIMPLE-STRING))))

(defun simple-vector-1b-ref (simple-vector offset)
  (declare #.(performance-optimizations)
           (type simple-vector-1b simple-vector)
           (type array-index offset)
           )
  (the (unsigned-byte 1) (aref simple-vector offset)))

(defun simple-vector-8b-ref (simple-vector offset)
  (declare #.(performance-optimizations)
           (type simple-vector-8b simple-vector)
           (type array-index offset)
           )
  (the (unsigned-byte 8) (aref simple-vector offset)))

(defun simple-vector-16b-ref (simple-vector offset)
  (declare #.(performance-optimizations)
           (type simple-vector-16b simple-vector)
           (type array-index offset)
           )
  (the (unsigned-byte 16) (aref simple-vector offset)))

(defun simple-vector-32b-ref (simple-vector offset)
  (declare #.(performance-optimizations)
           (type simple-vector-32b simple-vector)
           (type array-index offset)
           )
  (the (unsigned-byte 32) (aref simple-vector offset)))

(defun simple-vector-64b-ref (simple-vector offset)
  (declare #.(performance-optimizations)
           (type simple-vector-64b simple-vector)
           (type array-index offset)
           )
  (the (unsigned-byte 64) (aref simple-vector offset)))

(defun simple-string-8b-ref (simple-vector offset)
  (declare #.(performance-optimizations)
           (type simple-string-8b simple-vector)
           (type array-index offset)
           )
  (the char-8b (aref simple-vector offset)))

(defun simple-string-16b-ref (simple-vector offset)
  (declare #.(performance-optimizations)
           (type simple-string-16b simple-vector)
           (type array-index offset)
           )
  (the char-16b (aref simple-vector offset)))

(defsetf simple-vector-1b-ref (simple-vector offset) (new-value)
  `(LET ((SIMPLE-VECTOR ,simple-vector)
         (OFFSET ,offset)
         (NEW-VALUE ,new-value))
        (DECLARE ,(performance-optimizations)
                 (TYPE SIMPLE-VECTOR-1B SIMPLE-VECTOR)
                 (TYPE ARRAY-INDEX OFFSET)
                 (TYPE (UNSIGNED-BYTE 1) NEW-VALUE)
                 )
        (SETF (AREF SIMPLE-VECTOR OFFSET) NEW-VALUE)))

(defsetf simple-vector-8b-ref (simple-vector offset) (new-value)
  `(LET ((SIMPLE-VECTOR ,simple-vector)
         (OFFSET ,offset)
         (NEW-VALUE ,new-value))
        (DECLARE ,(performance-optimizations)
                 (TYPE SIMPLE-VECTOR-8B SIMPLE-VECTOR)
                 (TYPE ARRAY-INDEX OFFSET)
                 (TYPE (UNSIGNED-BYTE 8) NEW-VALUE)
                 )
        (SETF (AREF SIMPLE-VECTOR OFFSET) NEW-VALUE)))

(defsetf simple-vector-16b-ref (simple-vector offset) (new-value)
  `(LET ((SIMPLE-VECTOR ,simple-vector)
         (OFFSET ,offset)
         (NEW-VALUE ,new-value))
        (DECLARE ,(performance-optimizations)
                 (TYPE SIMPLE-VECTOR-16B SIMPLE-VECTOR)
                 (TYPE ARRAY-INDEX OFFSET)
                 (TYPE (UNSIGNED-BYTE 16) NEW-VALUE)
                 )
        (SETF (AREF SIMPLE-VECTOR OFFSET) NEW-VALUE)))

(defsetf simple-vector-32b-ref (simple-vector offset) (new-value)
  `(LET ((SIMPLE-VECTOR ,simple-vector)
         (OFFSET ,offset)
         (NEW-VALUE ,new-value))
        (DECLARE ,(performance-optimizations)
                 (TYPE SIMPLE-VECTOR-32B SIMPLE-VECTOR)
                 (TYPE ARRAY-INDEX OFFSET)
                 (TYPE (UNSIGNED-BYTE 32) NEW-VALUE)
                 )
        (SETF (AREF SIMPLE-VECTOR OFFSET) NEW-VALUE)))

(defsetf simple-vector-64b-ref (simple-vector offset) (new-value)
  `(LET ((SIMPLE-VECTOR ,simple-vector)
         (OFFSET ,offset)
         (NEW-VALUE ,new-value))
        (DECLARE ,(performance-optimizations)
                 (TYPE SIMPLE-VECTOR-64B SIMPLE-VECTOR)
                 (TYPE ARRAY-INDEX OFFSET)
                 (TYPE (UNSIGNED-BYTE 64) NEW-VALUE)
                 )
        (SETF (AREF SIMPLE-VECTOR OFFSET) NEW-VALUE)))

(defsetf simple-string-8b-ref (simple-string offset) (new-value)
  `(LET ((SIMPLE-STRING ,simple-string)
         (OFFSET ,offset)
         (NEW-VALUE ,new-value))
        (DECLARE ,(performance-optimizations)
                 (TYPE SIMPLE-STRING-8B SIMPLE-STRING)
                 (TYPE ARRAY-INDEX OFFSET)
                 (TYPE CHAR-8B NEW-VALUE)
                 )
        (SETF (AREF SIMPLE-STRING OFFSET) NEW-VALUE)))

(defsetf simple-string-16b-ref (simple-string offset) (new-value)
  `(LET ((SIMPLE-STRING ,simple-string)
         (OFFSET ,offset)
         (NEW-VALUE ,new-value))
        (DECLARE ,(performance-optimizations)
                 (TYPE SIMPLE-STRING-16B SIMPLE-STRING)
                 (TYPE ARRAY-INDEX OFFSET)
                 (TYPE CHAR-16B NEW-VALUE)
                 )
        (SETF (AREF SIMPLE-STRING OFFSET) NEW-VALUE)))

;;; Vector moving is critical.  This function, DEFINE-SUBVECTOR-MOVER, defines functions that
;;; can move simple vector elements as quickly as possible.
;;; As an example,
;;; (define-subvector-mover %subvector-move t)
;;; generates 4 functions.  In all these functions, src-start and dest-start
;;; are positive and inclusive, src-limit is positive and exclusive.  The destination
;;; vector must have a size >= (+ (- src-limit src-start) dest-start) or you will run
;;; off the end.
;;;
;;; %subvector-move-right src src-start src-limit dest dest-start
;;;   Copies elements from src to dest.  The order of access is from highest
;;;   to lowest, so this is used for moving vector elements to the RIGHT.
;;;
;;; %subvector-move-left src src-start src-limit dest dest-start
;;;   Copies elements from src to dest.  The order of access is from lowest
;;;   to highest, so this is used for moving vector elements to the LEFT.
;;;   Moving left is slightly faster than moving right.
;;;
;;;  If src and dest are the same vector, and you choose the wrong direction
;;;  to move, then these will not work correctly.

(defmacro define-fast-subvector-mover (function-name source-array-type vector-element-type
                                                     &optional (dest-array-type source-array-type)
                                                     (dest-vector-element-type vector-element-type))
  (labels ((symbol-append (&rest syms)
            (intern (format nil "~a"
                            (apply #'concatenate 'string
                                   (map 'list #'symbol-name syms)))
                    (symbol-package function-name)))

           (source-vector-type ()
            `(,source-array-type ,vector-element-type (*)))

           (dest-vector-type ()
            `(,dest-array-type ,dest-vector-element-type (*)))

           (decls ()
            `(DECLARE (TYPE ,(source-vector-type) SOURCE)
                      (TYPE ,(dest-vector-type)     DEST)
                      (TYPE ARRAY-INDEX SRC-START SRC-LIMIT DEST-START)
                      ,(performance-optimizations)
                      #+allegro (:EXPLAIN :CALLS)))

           ;; These loops have been empirically determined to be about as fast
           ;; as I can get lisp to go.
           (inner-loop-left ()
                            `(#| #+allegro excl::atomically #-allegro PROGN |# progn
                               (LOOP
                                (PROGN
                                 (WHEN (= SRC-START SRC-LIMIT)
                                       (RETURN-FROM NIL NIL))
                                 (SETF (AREF DEST DEST-START) (AREF SOURCE SRC-START))
                                 (INCF SRC-START)
                                 (INCF DEST-START)))))

           (inner-loop-right ()
                             `(#| #+allegro excl::atomically #-allegro PROGN |# progn
                                (PROGN
                                 (INCF DEST-START (- SRC-LIMIT SRC-START))
                                 (LOOP
                                  (WHEN (= SRC-LIMIT SRC-START)
                                        (RETURN-FROM NIL NIL))
                                  (DECF SRC-LIMIT)
                                  (DECF DEST-START)
                                  (SETF (AREF DEST DEST-START) (AREF SOURCE SRC-LIMIT))))))

           )

    (let ((left  (symbol-append function-name '#:-left))
          (right (symbol-append function-name '#:-right)))
      `(PROGN
        (DECLAIM (FTYPE (FUNCTION (,(source-vector-type) ARRAY-INDEX ARRAY-INDEX
                                   ,(dest-vector-type) ARRAY-INDEX))
                        ,left ,right)
                 (INLINE ,left ,right)
                 )
        (DEFUN ,left (SOURCE SRC-START SRC-LIMIT DEST DEST-START)
          ,(decls)
          ,(inner-loop-left))
        (DEFUN ,right (SOURCE SRC-START SRC-LIMIT DEST DEST-START)
          ,(decls)
          ,(inner-loop-right))))))

(define-fast-subvector-mover %simple-subvector-1b-move  simple-array bit)
(define-fast-subvector-mover %simple-subvector-8b-move  simple-array (unsigned-byte 8))
(define-fast-subvector-mover %simple-subvector-16b-move simple-array (unsigned-byte 16))
(define-fast-subvector-mover %simple-subvector-32b-move simple-array (unsigned-byte 32))
(define-fast-subvector-mover %simple-subvector-64b-move simple-array (unsigned-byte 64))
(define-fast-subvector-mover %simple-subvector-fixnum-move simple-array fixnum)

;;; In the lispworks world, BASE-CHARs are 8 bits, and LW:SIMPLE-CHARs are
;;; 16 bits.
(define-fast-subvector-mover %simple-substring-8b-move  simple-array char-8b)
(define-fast-subvector-mover %simple-substring-16b-move simple-array char-16b)
;; This one expands 8-bit base chars to 16-bit simple-chars on the fly.
(define-fast-subvector-mover %simple-substring-8b->16b-move
  simple-array char-8b
  simple-array char-16b)

;;; This dispatches to the correct mover depending on the type of the
;;; source and dest string.
(defun %simple-substring-move-left (source src-start src-limit dest dest-start)
  (cond ((lw:simple-text-string-p dest)
         (cond ((lw:simple-text-string-p source)
                (%simple-substring-16b-move-left
                 source src-start src-limit dest dest-start))
               ((lw:simple-base-string-p source)
                (%simple-substring-8b->16b-move-left
                 source src-start src-limit dest dest-start))
               (t (error "Cannot move characters from ~s to ~s" source dest))))
        ((lw:simple-base-string-p dest)
         (cond ((lw:simple-base-string-p source)
                (%simple-substring-8b-move-left
                 source src-start src-limit dest dest-start))
               (t (error "Cannot move characters from ~s to ~s" source dest))))
        (t (error "Cannot move characters from ~s to ~s" source dest))))

(defun %simple-substring-move-right (source src-start src-limit dest dest-start)
  (cond ((lw:simple-text-string-p dest)
         (cond ((lw:simple-text-string-p source)
                (%simple-substring-16b-move-right
                 source src-start src-limit dest dest-start))
               ((lw:simple-base-string-p source)
                (%simple-substring-8b->16b-move-right
                 source src-start src-limit dest dest-start))
               (t (error "Cannot move characters from ~s to ~s" source dest))))
        ((lw:simple-base-string-p dest)
         (cond ((lw:simple-base-string-p source)
                (%simple-substring-8b-move-right
                 source src-start src-limit dest dest-start))
               (t (error "Cannot move characters from ~s to ~s" source dest))))
        (t (error "Cannot move characters from ~s to ~s" source dest))))

;;; The FILL routines return the object filled for convenience in writing
;;; code like this:
;;; (simple-subvector-8b-fill (simple-subvector-8b-allocate 20) 0)
(defmacro define-fast-subvector-filler (function-name array-type vector-element-type &optional (fill-value-type vector-element-type))
  (labels ((symbol-append (&rest syms)
                          (intern (format nil "~a"
                                          (apply #'concatenate 'string
                                                 (map 'list #'symbol-name syms)))
                                  (symbol-package function-name)))

           (vector-type ()
                        `(,array-type ,vector-element-type (*)))

           (decls ()
                  `(DECLARE (TYPE ,fill-value-type FILL-VALUE)
                            (TYPE ,(vector-type) VECTOR)
                            (TYPE ARRAY-INDEX INDEX LIMIT)
                            ,(performance-optimizations)
                            #+allegro (:EXPLAIN :CALLS)))

           (inner-loop ()
                       `(LOOP
                         (PROGN
                          (WHEN (= INDEX LIMIT)
                                (RETURN-FROM NIL VECTOR))
                          (SETF (AREF VECTOR INDEX) (THE ,fill-value-type FILL-VALUE))
                          (INCF INDEX)))))
    `(PROGN
      (DECLAIM (FTYPE (FUNCTION (,(vector-type) ,fill-value-type ARRAY-INDEX ARRAY-INDEX) ,(vector-type))
                      ,function-name)
               (INLINE ,function-name)
               )
      (DEFUN ,function-name (VECTOR FILL-VALUE INDEX LIMIT)
        ,(decls)
        ,(inner-loop)))))

(define-fast-subvector-filler %simple-subvector-1b-fill  simple-array bit)
(define-fast-subvector-filler %simple-subvector-8b-fill  simple-array (unsigned-byte 8))
(define-fast-subvector-filler %simple-subvector-16b-fill simple-array (unsigned-byte 16))
(define-fast-subvector-filler %simple-subvector-32b-fill simple-array (unsigned-byte 32))
(define-fast-subvector-filler %simple-substring-8b-fill     simple-array char-8b)
(define-fast-subvector-filler %simple-substring-16b-fill    simple-array char-16b)
;; Not needed because char is a subtype.
;;(define-fast-subvector-filler %subvector-base-char-expand-fill lw:simple-char base-char)
(define-fast-subvector-filler %simple-subvector-fixnum-fill simple-array fixnum)

(defun %simple-substring-fill (vector fill-value index limit)
  (cond ((lw:simple-text-string-p vector)
         (%simple-substring-16b-fill vector fill-value index limit))
        ((lw:simple-base-string-p vector)
         (%simple-substring-8b-fill vector fill-value index limit))
        (t (error "Can't fill string ~s" vector))))

(proclaim (standard-optimizations))

(defmacro define-vector-filler (name vector-type element-type subvector-filler vector-length)
  `(DEFUN ,name (VECTOR FILL-VALUE &key (START 0) END)
     (CHECK-TYPE VECTOR ,vector-type)
     (CHECK-TYPE FILL-VALUE ,element-type)
     (CHECK-TYPE START NON-NEGATIVE-INTEGER)
     (CHECK-TYPE END (OPTIONAL NON-NEGATIVE-INTEGER))
     (IF (ZEROP (,vector-length VECTOR))
         ;; Degenerate case of filling an empty vector does nothing.
         (PROGN
           (CHECK-TYPE START (INTEGER 0 (1)))
           (WHEN END (CHECK-TYPE END (INTEGER 0 (1))))
           VECTOR)
         (PROGN
           (CHECK-RANGE START 0 (,vector-length VECTOR))
           (WHEN END (CHECK-RANGE END START (,vector-length VECTOR)))
           (,subvector-filler VECTOR FILL-VALUE START (OR END (,vector-length VECTOR)))))))

(define-vector-filler simple-vector-1b-fill  simple-vector-1b (unsigned-byte 1)
  %simple-subvector-1b-fill
  simple-vector-1b-length)
(define-vector-filler simple-vector-8b-fill  simple-vector-8b (unsigned-byte 8)
  %simple-subvector-8b-fill
  simple-vector-8b-length)
(define-vector-filler simple-vector-16b-fill simple-vector-16b (unsigned-byte 16)
  %simple-subvector-16b-fill
  simple-vector-16b-length)
(define-vector-filler simple-vector-32b-fill simple-vector-32b (unsigned-byte 32)
  %simple-subvector-32b-fill
  simple-vector-32b-length)
(define-vector-filler simple-string-8b-fill  simple-string-8b char-8b
  %simple-substring-8b-fill
  simple-string-8b-length)
(define-vector-filler simple-string-16b-fill simple-string-16b char-16b
  %simple-substring-16b-fill
  simple-string-16b-length)
(define-vector-filler simple-string-fill     simple-string character
  %simple-substring-fill
  simple-string-length)

(defmacro define-subvector-mover (function-name array-type element-type)

  "Define a function named FUNCTION-NAME, which is optimized to move blocks of elements
   within a vector of element-type ELEMENT-TYPE.  SVREF and/or AREF are used,
   with declarations which hopefully achieve open-coding of array accesses.

   ARRAY-TYPE should be either ARRAY, SIMPLE-ARRAY, or VECTOR.  SIMPLE-ARRAY can often
   be open coded.  Remember that SIMPLE-ARRAY may not have a fill pointer or be adjustable.

   If ELEMENT-TYPE is T, and ARRAY-TYPE is SIMPLE-ARRAY, we'll use SVREF.
   For anything else, we'll use AREF, and whether it's open coded depends on the
   compiler's interpretation of ELEMENT-TYPE.

   The generated function has the form:
       (defun function-name (array n-elements from-index to-index
                            &key (fill-vacancies-with nil fill-requested))

   The following describes the operation of the generated function, and is set as the
   documentation string for the function.

   Move N-ELEMENTS from FROM-INDEX to TO-INDEX
   in ARRAY, handling overlapping copies.  We assume array is a simple-array.
   If FILL-VACANCIES-WITH is specified, its value is used to set the slots which were vacated in the
   FROM-INDEX block.  This may be useful if you want contents to be eligible for GC sooner
   and the array is the sole referencer of those contents."
  (let ((type-spec `(,array-type ,element-type (*))) ;assume one dimension
        )
    (flet ((symbol-append (&rest syms)
             (intern (format nil "~a"
                             (apply #'concatenate 'string
                                    (map 'list #'symbol-name syms)))
                     (symbol-package function-name))))

      `(defun ,function-name
         (array n-elements from-index to-index)
         "Move N-ELEMENTS from FROM-INDEX to TO-INDEX
       in ARRAY, handling overlapping copies.  We assume array is a simple-array."

         (declare ,(standard-optimizations)
           ;(type array-index n-elements from-index to-index)
           )
         (check-type array ,type-spec)
         (check-range from-index 0 (length (the ,type-spec array)))
         (check-range to-index   0 (length (the ,type-spec array)))
         (check-range n-elements 0 (1+ (- (length (the ,type-spec array))
                                          (max from-index to-index))))
         ;; Dispatch to the right primitive mover, move right if from is earlier than to,
         ;; and left otherwise.
         (cond ((or (= from-index to-index)
                    (zerop n-elements)))
           ((< from-index to-index)
            (,(symbol-append '#:% function-name '#:-right)
             array from-index (+ from-index n-elements) array to-index))
           (t (,(symbol-append '#:% function-name '#:-left)
               array from-index (+ from-index n-elements) array to-index)))))))

(define-subvector-mover simple-subvector-1b-move  simple-array bit)
(define-subvector-mover simple-subvector-8b-move  simple-array (unsigned-byte 8))
(define-subvector-mover simple-subvector-16b-move simple-array (unsigned-byte 16))
(define-subvector-mover simple-subvector-32b-move simple-array (unsigned-byte 32))
(define-subvector-mover simple-subvector-64b-move simple-array (unsigned-byte 64))
(define-subvector-mover simple-substring-8b-move  simple-array char-8b)
(define-subvector-mover simple-substring-16b-move simple-array char-16b)
(define-subvector-mover simple-subvector-fixnum-move simple-array fixnum)

(defun simple-substring-move (vector n-elements from-index to-index)
  (cond ((lw:simple-text-string-p vector)
         (simple-substring-16b-move vector n-elements from-index to-index))
        ((lw:simple-base-string-p vector)
         (simple-substring-8b-move vector n-elements from-index to-index))
        (t (error "Can't move string ~s" vector))))

(defmacro define-vector-copier (function-name subvector-mover array-type source-element-type
                                              &optional (dest-element-type source-element-type))
  "Defines a function for FUNCTION-NAME
   which will copy elements from some point in a source array to a point in a destination array
   in the most efficient way possible.

   The generated function has the form:

   (defun function-name (source-vector target-vector n-elements &key (from-index 0) (to-index 0)) ...)

   Array arguments to the resulting function are assumed to have the SAME TYPE."
  (let ((source-type-spec `(,array-type ,source-element-type (*))) ;assume one dimension
        (dest-type-spec   `(,array-type ,dest-element-type (*))))
    (flet ((symbol-append (&rest syms)
             (intern (format nil "~a"
                             (apply #'concatenate 'string
                                    (map 'list #'symbol-name syms)))
                     (symbol-package function-name))))

      `(defun ,function-name
         (source-array target-array
           &key (start-1 0) end-1 (to-index 0))
         "Copy N-ELEMENTS from START-1 in SOURCE-ARRAY to TO-INDEX in TARGET-ARRAY.
        Both indices default to zero.  It is an error if the target area isn't large enough
        to contain bytes in designated by the source area.

        END-1, if specified, is the 'bounding index designator' (exclusive) of the last element
        to copy.  It defaults to the length of the source array."
         (declare ,(standard-optimizations)
           ;(type array-index start-1 to-index)
           )
         (check-type source-array ,source-type-spec)
         (check-type target-array ,dest-type-spec)
         (let ((limit-1 (or end-1 (length (the ,source-type-spec source-array)))))
           ;(declare (type array-index limit-1))
           (check-range start-1  0 (length (the ,source-type-spec source-array)))
           (check-range limit-1 start-1 (1+ (length (the ,source-type-spec source-array))))
           (check-range to-index 0 (length (the ,dest-type-spec target-array)))
           (when (eq source-array target-array)
             (error "Arrays are the same (EQ) object.  Use a SUBVECTOR-MOVE-* function instead of an ~
                SUBVECTOR-COPY-* function."))
           #||                          ;
           ;; Huh?  We checked the type just above!
           (unless (equalp (array-element-type source-array)
           (array-element-type target-array))
           (error "~s: vector arguments are not of same type, ~s and ~s"
           ',function-name (type-of source-array) (type-of target-array)))
           ||#
           (let ((element-count (- limit-1 start-1))
                 (target-size (- (length target-array) to-index)))
             (check-range element-count 0 (1+ target-size))
             ;; Just move right.  Source and target are supposed to be different.
             (when (> element-count 0)
               (,(symbol-append subvector-mover '#:-right)
                source-array start-1 limit-1 target-array to-index))))))))

(define-vector-copier simple-vector-1b-copy  %simple-subvector-1b-move  simple-array bit)
(define-vector-copier simple-vector-8b-copy  %simple-subvector-8b-move  simple-array (unsigned-byte 8))
(define-vector-copier simple-vector-16b-copy %simple-subvector-16b-move simple-array (unsigned-byte 16))
(define-vector-copier simple-vector-32b-copy %simple-subvector-32b-move simple-array (unsigned-byte 32))
(define-vector-copier simple-vector-64b-copy %simple-subvector-64b-move simple-array (unsigned-byte 64))
(define-vector-copier simple-string-8b-copy %simple-substring-8b-move simple-array char-8b)
(define-vector-copier simple-string-8b->16b-copy %simple-substring-8b->16b-move simple-array char-8b char-16b)
(define-vector-copier simple-string-16b-copy %simple-substring-16b-move simple-array char-16b)
(define-vector-copier simple-vector-fixnum-copy %simple-subvector-fixnum-move simple-array fixnum)

(defun simple-string-copy (source-array target-array
                                      &key (start-1 0) end-1 (to-index 0))
  (cond ((lw:simple-text-string-p target-array)
         (cond ((lw:simple-text-string-p source-array)
                (simple-string-16b-copy source-array
                                         target-array
                                         :start-1 start-1
                                         :end-1 (or end-1 (length source-array))
                                         :to-index to-index))

               ((lw:simple-base-string-p source-array)
                (simple-string-8b->16b-copy source-array
                                              target-array
                                              :start-1 start-1
                                              :end-1 (or end-1 (length source-array))
                                              :to-index to-index))

               (t (error "Cannot copy characters from ~s to ~s" source-array target-array))))
        ((lw:simple-base-string-p target-array)
         (cond ((lw:simple-base-string-p source-array)
                (simple-string-8b-copy source-array
                                       target-array
                                       :start-1 start-1
                                       :end-1 (or end-1 (length source-array))
                                       :to-index to-index))
               (t (error "Cannot copy characters from ~s to ~s" source-array target-array))))
        (t (error "Cannot copy characters from ~s to ~s" source-array target-array))))

(defmacro define-vector-adjuster (function-name array-type element-type mover filler)
  `(DEFUN ,function-name (SOURCE NEW-LENGTH FILL-VALUE)
     (CHECK-TYPE SOURCE (,array-type ,element-type))
     (CHECK-TYPE NEW-LENGTH ARRAY-INDEX)
     (CHECK-TYPE FILL-VALUE ,element-type)
     (CHECK-RANGE NEW-LENGTH (LENGTH (THE (,array-type ,element-type) SOURCE)) ,ARRAY-DIMENSION-LIMIT)
     (LET ((OLD-LENGTH (LENGTH (THE (,array-type ,element-type) SOURCE)))
           (NEW-VECTOR (MAKE-ARRAY NEW-LENGTH :ELEMENT-TYPE ',ELEMENT-TYPE)))
       (DECLARE ,(performance-optimizations))
       ;; copy the existing elements
       (,mover SOURCE 0 OLD-LENGTH NEW-VECTOR 0)
       (,filler NEW-VECTOR FILL-VALUE OLD-LENGTH NEW-LENGTH)
       NEW-VECTOR)))

(define-vector-adjuster simple-vector-1b-adjust simple-array bit                 %simple-subvector-1b-move-left  %simple-subvector-1b-fill)
(define-vector-adjuster simple-vector-8b-adjust simple-array (unsigned-byte 8)   %simple-subvector-8b-move-left  %simple-subvector-8b-fill)
(define-vector-adjuster simple-vector-16b-adjust simple-array (unsigned-byte 16) %simple-subvector-16b-move-left %simple-subvector-16b-fill)
(define-vector-adjuster simple-vector-32b-adjust simple-array (unsigned-byte 32) %simple-subvector-32b-move-left %simple-subvector-32b-fill)
(define-vector-adjuster simple-string-8b-adjust simple-array char-8b %simple-substring-8b-move-left %simple-substring-8b-fill)
(define-vector-adjuster simple-string-16b-adjust simple-array char-16b %simple-substring-16b-move-left %simple-substring-16b-fill)
;;(define-vector-adjuster vector-fixnum-adjust t %subvector-fixnum-move-left %subvector-fixnum-fill)

(defun simple-string-adjust (source new-length fill-value)
  (cond ((lw:simple-text-string-p source)
         (simple-string-16b-adjust source new-length fill-value))
        ((lw:simple-base-string-p source)
         (simple-string-8b-adjust source new-length fill-value))
        (t (error "Cannot adjust string ~s" source))))

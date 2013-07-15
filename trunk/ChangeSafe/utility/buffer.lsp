;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002 - 2004 ChangeSafe, LLC
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

;;; An implementation of a byte buffer for use in streams.

(defconstant +standard-byte-buffer-transfer-size+ (* (expt 2 10) 8) ;; 8 k
    "This constant is used for requesting refills of the buffer.
The source of the bytes will never be asked for more than this amount.
Watch out for dependencies when changing this.")

(defconstant +standard-byte-buffer-initial-capacity+ (* (expt 2 10) 8) ;; 8 k
    "This constant is used for initializing the byte buffer for encoding.
It should be the same as the standard-transfer-size, but if it is too small,
the buffer will be grown to accomodate a line.")

(defconstant +standard-byte-buffer-growth-factor+ (exp .4)
  "This is how much larger to make the byte buffer when it overflows.  It should be a float larger
   than 1 and less than 2.")

(defstruct (byte-buffer
            (:conc-name buffer-)
            (:constructor make-byte-buffer)
            (:copier nil))
  (read-point 0  :type array-index)
  (write-point 0 :type array-index)
  (contents (simple-vector-8b-allocate +standard-byte-buffer-initial-capacity+)
            :type simple-vector-8b))

(declaim (ftype (function (byte-buffer) array-index) buffer/used buffer/capacity)
         (ftype (function (byte-buffer) boolean) buffer/empty?)
         (inline buffer/used buffer/capacity buffer/empty?))

(defun buffer/used (byte-buffer)
  (check-type byte-buffer byte-buffer)
  (- (buffer-write-point byte-buffer)
     (buffer-read-point  byte-buffer)))

(defun buffer/empty? (byte-buffer)
  (check-type byte-buffer byte-buffer)
  (zerop (buffer/used byte-buffer)))

(defun buffer/capacity (byte-buffer)
  (check-type byte-buffer byte-buffer)
  (simple-vector-8b-length (buffer-contents byte-buffer)))

(defmethod print-object ((object byte-buffer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format t "~d/~d"
            (buffer/used object)
            (buffer/capacity object))))

(defun buffer/grow (buffer new-size)
  (check-type buffer byte-buffer)
  (check-type new-size array-index)
  (debug-message 4 "Growing buffer.")
  (let ((new-contents (simple-vector-8b-allocate new-size)))
    (declare (type simple-vector-8b new-contents))
    (simple-vector-8b-copy (buffer-contents buffer) new-contents)
    (setf (buffer-contents buffer) new-contents)))

(defun buffer/ensure-capacity (buffer n-bytes)
  (check-type buffer byte-buffer)
  (check-type n-bytes array-index)
  (when (> n-bytes (buffer/capacity buffer))
    (buffer/grow buffer (max +standard-byte-buffer-initial-capacity+
                             (ceiling (* +standard-byte-buffer-growth-factor+ n-bytes))))))

(declaim (ftype (function (byte-buffer (or list (unsigned-byte 8))) (or null array-index))
                buffer/position)
         (inline buffer/position)
         (ftype (function (byte-buffer (unsigned-byte 8)) (or null array-index))
                buffer/%position-1)
         (inline buffer/%position-1)
         (ftype (function (byte-buffer (unsigned-byte 8) (unsigned-byte 8)) (or null array-index))
                buffer/%position-2)
         (inline buffer/%position-2))

;;; In critical path
(defun buffer/%position (buffer element)
  "Given an element find its position or return NIL."
  (declare #.(performance-optimizations)
           (type byte-buffer buffer)
           (type (unsigned-byte 8) element)
           )
  (let ((contents (buffer-contents buffer))
        (start    (buffer-read-point buffer))
        (limit    (buffer-write-point buffer)))
    (declare (type simple-vector-8b contents)
             (type array-index start limit))
    (do ((i start (+ i 1)))
        ((>= i limit) nil)
      (declare (type array-index i))
      (when (= (simple-vector-8b-ref contents i) element)
        (return (- i start))))))

(defun buffer/%position-2 (buffer element1 element2)
  "Given two elements find their position or return NIL."
  (declare #.(performance-optimizations)
           (type byte-buffer buffer)
           (type (unsigned-byte 8) element1 element2)
           )
  (let ((contents (buffer-contents buffer))
        (start    (buffer-read-point buffer))
        (limit    (- (buffer-write-point buffer) 1)))
    (declare (type simple-vector-8b contents)
             (type array-index start limit))
    (do ((i start (+ i 1)))
        ((>= i limit) nil)
      (declare (type array-index i))
      (when (and (= (simple-vector-8b-ref contents i) element1)
                 (= (simple-vector-8b-ref contents (+ i 1)) element2))
        (return (- i start))))))

(defun buffer/position (buffer element-or-list)
  (check-type buffer byte-buffer)
  (cond ((numberp element-or-list) (buffer/%position buffer element-or-list))
        ((null (cdr element-or-list)) (buffer/%position buffer (car element-or-list)))
        (t (buffer/%position-2 buffer (car element-or-list) (cadr element-or-list)))))

(defun buffer/refill-from-stream (buffer stream)
  "Returns T if EOF was reached, NIL if not."
  (check-type buffer byte-buffer)
  (let* ((in-use (buffer/used buffer))
         (bytes-desired (min (- (buffer/capacity buffer) in-use)
                             +standard-byte-buffer-transfer-size+)))
    (declare (type array-index in-use bytes-desired))
    (when (zerop bytes-desired)
      ;; Buffer is full, and we need more bytes.  Grow the buffer.
      (buffer/grow buffer (ceiling (* in-use +standard-byte-buffer-growth-factor+)))
      (setq bytes-desired (min (- (buffer/capacity buffer) in-use)
                               +standard-byte-buffer-transfer-size+)))

    ;; move bytes to beginning of buffer
    (%simple-subvector-8b-move-left
     (buffer-contents buffer) (buffer-read-point buffer) (buffer-write-point buffer)
     (buffer-contents buffer) 0)

    ;; Fill the tail
    (let ((bytes-available (min (- (file-length stream) (file-position stream))
                                bytes-desired)))
      (declare (type array-index bytes-available))
      (named-let loup ((start in-use)
                       (end (+ in-use bytes-available)))
                     (debug-message 5 "Start: ~d, End: ~d" start end)
                     (let ((count (- (read-sequence (buffer-contents buffer)
                                                    stream
                                                    :start start
                                                    :end end)
                                     start)))
                       (declare (type array-index count))
                       (when (< count bytes-available)
                         (loup (+ start count) end))))

      (setf (buffer-read-point buffer) 0
            (buffer-write-point buffer) (+ in-use bytes-available))
      (zerop bytes-available))))

(defun byte-buffer/reset (buffer)
  (check-type buffer byte-buffer)
  (setf (buffer-read-point buffer) 0)
  (setf (buffer-write-point buffer) 0))

;;; In critical path
(defun byte-buffer/extract (buffer n discard)
  "Returns a vector-8b of the next n elements in the byte-buffer,
   and removes those elements plus the discard number from the buffer."
  (declare #.(performance-optimizations))
  (check-type buffer byte-buffer)
  (check-type n array-index)
  (check-type discard array-index)
  (let* ((result (simple-vector-8b-allocate n))
         (rp     (buffer-read-point buffer))
         (nrp    (+ rp n)))
    (%simple-subvector-8b-move-left (buffer-contents buffer) rp nrp
                                      result 0)
    (setf (buffer-read-point buffer) (+ nrp discard))
    result))

(defun scan-byte-buffer (buffer)
  (declare (optimizable-series-function))
  (the (series (unsigned-byte 8))
    (subseries (scan 'simple-vector-8b
                     (the simple-vector-8b (buffer-contents buffer)))
               (buffer-read-point buffer)
               (buffer-write-point buffer))))

;;; Buffered stream

(defstruct (buffered-stream
            (:conc-name buffered-stream/)
            (:constructor make-buffered-stream (stream))
            (:copier nil))
  (buffer (make-byte-buffer) :type byte-buffer)
  (stream nil :type (optional stream)))

(defun buffered-stream/at-eof? (buffered-stream)
  (and (buffer/empty? (buffered-stream/buffer buffered-stream))
       (null (buffered-stream/stream buffered-stream))))

(defun buffered-stream/get-line (buffered-stream eol-marker)
  (let* ((buffer (buffered-stream/buffer buffered-stream))
         (eolpos (buffer/position buffer eol-marker)))
    (cond (eolpos (byte-buffer/extract buffer eolpos (if (numberp eol-marker)
                                                         1
                                                         (length eol-marker))))
          ((buffered-stream/stream buffered-stream)
           (when (buffer/refill-from-stream
                  buffer (buffered-stream/stream buffered-stream))
             (setf (buffered-stream/stream buffered-stream) nil))
           (buffered-stream/get-line buffered-stream eol-marker))
          (t (byte-buffer/extract buffer (buffer/used buffer) 0)))))

(defun scan-stream-records (stream record-separator)
  (declare (optimizable-series-function))
  (map-fn 'simple-vector-8b
          (lambda (buffered-stream)
            (buffered-stream/get-line buffered-stream record-separator))
          (until-if
           #'buffered-stream/at-eof?
           (series (make-buffered-stream stream)))))

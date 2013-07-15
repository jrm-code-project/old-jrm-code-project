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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(
            %mirror-fixnum
            %write-fixnum
            %write-fixnum-to-string
            %write-unsigned32

            align-stream
            alist->plist
            assoc-all
            average
            bit-set-and?
            bit-set-empty?
            bit-set-equal?
            byte-counting-stream
            bytes-to-simple-string-8b
            count-duplicates
            simple-bytes-to-simple-string-8b
            ;; DO NOT USE THESE
            ;; bytes-to-string
            ;; bytes-to-string-1
            define-objects-equalp-type-mismatch-methods
            define-enumeration
            dump-file
            encode-namestring
            decode-namestring
            exit
            exit-lisp
            false
            fixnum-base10-digit-count
            file-copy-to-stream
            find-keyword
            keyword-package
            make-keyword
            mirror-bits
            n-digit-prime
            nth-member
            objects-equalp
            permutations
            plist->alist
            prime?
            pushlast
            quit
            ratio->percentage
            read-simple-bit-vector
            read-fixnum
            read-string-data
            read-unsigned16
            read-unsigned32
            safe-read
            ;; DO NOT USE THESE
            ;; string-to-bytes
            split-string
            write-fixnum
            write-simple-bit-vector
            write-string-data
            write-unsigned16
            write-unsigned32
            zero-extended-bit-set-and?
            zero-extended-bit-set-equal?
            zero-extended-bit-set-or
            zero-extended-bit-set-xor
            )))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant *alignment-vector*
  (if (boundp '*alignment-vector*)
      (symbol-value '*alignment-vector*)
      (let ((alignment-vector (simple-vector-8b-allocate 512)))
        (simple-vector-8b-fill alignment-vector (char-code #\space))
        (setf (simple-vector-8b-ref alignment-vector 511) (char-code #\newline))
        alignment-vector))))

(defun align-stream (stream alignment)
  "Write padding bytes to STREAM until (mod (file-position stream) alignment)
   is zero."
  (let ((current-offset (file-position stream)))
    (cond ((null current-offset) (error "Could not determine offset in file."))
          ((= alignment 1) current-offset)
          (t (let ((remainder      (mod current-offset alignment)))
               (cond ((zerop remainder) current-offset)
                     ;; A hack.  When padding to align the data
                     ;; structures, terminate the padding with a
                     ;; newline.  Makes it easier to read database in
                     ;; to a text editor!
                     ((< (- alignment remainder) 512)
                      (write-sequence *alignment-vector*
                                      stream
                                      :start (- 512 (- alignment remainder))
                                      :end 512)
                      (+ current-offset (- alignment remainder)))
                     (t
                      (dotimes (i (- alignment remainder 1)
                                  (progn (write-byte (char-code #\newline) stream)
                                         (+ current-offset (- alignment remainder))))
                        (write-byte 32 stream)))))))))

(defun alist->plist (alist)
  "Convert an alist to a plist.  Note that shadowed values are omitted."
  (multiple-value-bind (keys values) (scan-alist alist)
    (collect-plist keys values)))

(defun assoc-all (item alist &key (key #'identity) (test #'eql) test-not)
  (cond ((null alist) '())
        ((funcall (or test-not (complement test)) item (funcall key (caar alist)))
         (assoc-all item (cdr alist) :key key :test test :test-not test-not))
        (t
         (cons (cdar alist) (assoc-all item (cdr alist) :key key :test test :test-not test-not)))))

(defun average (numerator denominator &optional (places 2))
  "Returns a number that is numerator / denominator, rounded to places of
   accuracy after the decimal point.  Places may be negative, as well as
   positive.  If denominator is zero, then the numerator is returned with
   no rounding or conversion, and it need not even be a number in this case.
   Note that places is rounded to the nearest integer before use, and defaults to 2.

   If places is negative or zero, an integer will be returned.
   If places is positive, a float will be returned, that has the number of
   places behind the decimal point, unless the magnitude is too large for those
   places to be significant.

   It is not supported for any arguments to be complex numbers."

  (if (zerop denominator)
      numerator
      (let* ((pl (round places))
             (pow (expt 10 (abs pl))))
        (if (plusp pl)
            (/ (float (round (float (* numerator pow)) denominator)) pow)
            (* (round numerator (* denominator pow)) pow)))))

(defun %bit-set-and? (left right count)
  "Unsafe inner-loop of bit-set-and?
   Use BIT-SET-AND?, not this function.

   Returns T if the logical AND of the bits in LEFT and RIGHT
   would produce a non-zero value.  i.e., if there is *any* index
   for which (sbit left index) and (sbit right index) both
   return 1.

   Assumes LEFT and RIGHT are (simple-array bit), and that they
   are the same length (see zero-extended-bit-set-and? if not)."
  (declare #.(performance-optimizations)
           (type (simple-array bit) left right)
           (type array-index count)
           #+allegro (:explain :calls))
  ;; Work from MSB down, this is used for merge and
  ;; the interesting bits are likely to be near the MSB end.
  (loop
   (setq count (the array-index (1- count)))
   (cond ((and (= (sbit left count) 1)
               (= (sbit right count) 1))
          (return-from %bit-set-and? t))
         ((zerop count)
          (return-from %bit-set-and? nil))
         ;; otherwise, continue scanning
         (t nil))))

(defun %bit-subset-empty? (bv scan limit)
  "Unsafe inner-loop of bit-set-empty?
   Use BIT-SET-EMPTY?, not this function.

   Returns T if all bits in BV are zero.
   Scan points at MSB to be checked, limit points one before
   LSB to be checked.  We check MSB to LSB because we expect
   to find differences in the more significant bits.

   Assumes bv is (simple-array bit)."
  (declare #.(performance-optimizations)
           (type (simple-array bit) bv)
           (type array-index scan limit)
           #+allegro (:explain :calls))
  (loop
   (cond ((= scan limit) (return-from %bit-subset-empty? t))
         ((zerop (sbit bv scan)) (decf scan))
         (t (return-from %bit-subset-empty? nil)))))

(defun %aligned-bit-subset-equal? (left right scan limit)
  "Unsafe inner-loop of BIT-SET-EQUAL? and ZERO-EXTENDED-BIT-SET-EQUAL?
   Use BIT-SET-EQUAL?, not this function.

   Returns T if the set of bits in LEFT and RIGHT are the same.
   i.e., there is *no* index for which (sbit left index) does
   not equal (sbit right index).

   Assumes LEFT and RIGHT are (simple-array bit)."
  ;; also assumes:
  ;; EITHER scan = limit OR
  ;;  1. scan > limit and
  ;;  2. limit >= -1  and
  ;;  3. scan < (length left) and
  ;;  4. scan < (length right)
  ;; BEWARE, the callers MUST ensure this, and the local
  ;; saftey declarations are insufficient to catch the ensuing
  ;; out-of-bounds indexing (for some values of left and right)
  ;; and can return random crap for an answer.
  (declare #.(performance-optimizations)
           (type (simple-array bit) left right)
           (type array-index scan limit)
           #+allegro (:explain :calls))
  ;; Work from MSB down, this is used for merge and
  ;; the interesting bits are likely to be near the MSB end.
  (loop
   (cond ((= scan limit) (return-from %aligned-bit-subset-equal? t))
         ((= (sbit left scan)
             (sbit right scan)) (decf scan))
         (t (return-from %aligned-bit-subset-equal? nil)))))

(defun bit-set-and? (left right)
  "Return T iff (bit-and left right) has any non-zero bits.
   An error is signalled if (length left) != (length right)

   I.E., return T if there exists an index i for which
   (sbit left i) = 1  and   (sbit right i) = 1

   Presumably, this would be more efficient to compute than
   the equivalent (not (bit-set-empty? (bit-set-and left right)))"
  ;; Presumably this is faster than anding them.
  (check-type left (simple-array bit))
  (check-type right (simple-array bit))
  (let ((left-length  (simple-bit-vector-length left))
        (right-length (simple-bit-vector-length right)))
    ;(declare (type array-index left-length right-length))
    (if (= left-length right-length)
        (or (zerop left-length)
            (%bit-set-and? left right left-length))
      (error "BIT-SET-AND? called on two bit-sets of different lengths, ~s and ~s."
             left right))))

(defun zero-extended-bit-set-and? (left right)
  "Return T iff (bit-and left right) has any non-zero bits regardless of length.
   The shorter bit-map is assumed to be extended with zeros to match the length
   of the longer.

   However, given the nature of the AND operation, all we really do is compare
   for the length of the shorter because any ON bits in the longer will be
   negated by the (assumed) OFF bits in the shorter.
   "
  ;; Presumably this is faster than anding them.
  (check-type left (simple-array bit))
  (check-type right (simple-array bit))
  (let ((left-length  (simple-bit-vector-length left))
        (right-length (simple-bit-vector-length right)))
    (declare (type array-index left-length right-length))
    (%bit-set-and? left right (min left-length right-length))))

(defun bit-set-equal? (left right)
  "Returns T if the set of bits in LEFT and RIGHT are the same.
   i.e., there is *no* index for which (sbit left index) does
   not equal (sbit right index).

   Will *always* return false if (length left) != (length right)
   Use zero-extended-bit-set-equal? if you don't want this behavior.

   This is equivalent to the CommonLisp EQUAL function applied to
   the two bitmaps, but it is considerably faster."
  (check-type left (simple-array bit))
  (check-type right (simple-array bit))
  (or (eq left right)
      (let ((left-length (simple-bit-vector-length left))
            (right-length (simple-bit-vector-length right)))
        (declare (type array-index left-length right-length))
        (when (= left-length right-length)
          (%aligned-bit-subset-equal? left right (1- left-length) -1)))))

(defun zero-extended-bit-set-equal? (left right)
  "Returns T if the set of bits in LEFT and RIGHT are the same.
   i.e., there is *no* index for which (sbit left index) does
   not equal (sbit right index).

   If one bit-set is shorter than the other, the shorter of the two
   is assumed to be extended with zeroes at the higher indices.

   See also bit-set-equal?"
  ;; Note we check for empty extensions first, since the testing of
  ;; a single bit-vector is faster per bit, and because frequently
  ;; the differences are found at the right-hand end.
  (check-type left (simple-array bit))
  (check-type right (simple-array bit))
  (or (eq left right)
      (let ((left-limit  (1- (simple-bit-vector-length left)))
            (right-limit (1- (simple-bit-vector-length right))))
        (declare (type array-index left-length right-length))
        (if (< left-limit right-limit)
            (and
             (%bit-subset-empty? right right-limit left-limit)
             (%aligned-bit-subset-equal? left right left-limit -1))
          (and
           (or (= left-limit right-limit)
               (%bit-subset-empty? left left-limit right-limit))
           (%aligned-bit-subset-equal? left right right-limit -1))))))

(defun bit-set-empty? (bv)
  "Return T if bit-set has no non-zero bits.
   (zero-length bit-sets are empty)."
  (check-type bv (simple-array bit))
  (%bit-subset-empty? bv (1- (simple-bit-vector-length bv)) -1))

(defun %bit-set-or (little-length little-bitvector big-length big-bitvector)
  (declare #.(performance-optimizations)
           (type array-index little-length big-length)
           )
  (let ((result (simple-vector-1b-allocate big-length)))
    (declare (type simple-bit-vector result))
    (do ((idx 0 (1+ idx)))
        ((= idx little-length)
         ;; copy the remaining bits
         (%simple-subvector-1b-move-left big-bitvector idx big-length
                                         result idx))
      (declare (type array-index idx))
      (setf (simple-vector-1b-ref result idx)
            (logior (simple-vector-1b-ref little-bitvector idx)
                    (simple-vector-1b-ref big-bitvector idx))))
    result))

(defun zero-extended-bit-set-or (left right)
  "Returns a new simple bit vector such that a bit is on IFF
   a bit is on in LEFT or RIGHT."
  (check-type left (simple-array bit))
  (check-type right (simple-array bit))
  (let ((left-length  (simple-bit-vector-length left))
        (right-length (simple-bit-vector-length right)))
    (if (< left-length right-length)
        (%bit-set-or left-length left right-length right)
        (%bit-set-or right-length right left-length left))))

(defun %bit-set-xor (little-length little-bitvector big-length big-bitvector)
  (declare #.(performance-optimizations)
           (type array-index little-length big-length)
           )
  (let ((result (simple-vector-1b-allocate big-length)))
    (declare (type simple-bit-vector result))
    (do ((idx 0 (1+ idx)))
        ((= idx little-length)
         ;; copy the remaining bits
         (%simple-subvector-1b-move-left big-bitvector idx big-length
                                         result idx))
      (declare (type array-index idx))
      (setf (simple-vector-1b-ref result idx)
            (logxor (simple-vector-1b-ref little-bitvector idx)
                    (simple-vector-1b-ref big-bitvector idx))))
    result))

(defun zero-extended-bit-set-xor (left right)
  "Returns a new simple bit vector such that a bit is on IFF
   a bit is on in LEFT or RIGHT, but not both."
  (check-type left (simple-array bit))
  (check-type right (simple-array bit))
  (let ((left-length  (simple-bit-vector-length left))
        (right-length (simple-bit-vector-length right)))
    (if (< left-length right-length)
        (%bit-set-xor left-length left right-length right)
        (%bit-set-xor right-length right left-length left))))

#+lispworks
(defclass byte-counting-stream (stream:fundamental-binary-output-stream)
  ((position :initarg :initial-position
             :initform 0
             :accessor stream:stream-file-position)))

#+lispworks
(defmethod stream:stream-write-byte ((s byte-counting-stream) byte)
  (declare (ignore byte))
  (incf (stream:stream-file-position s)))

#+lispworks
(defmethod stream:stream-write-sequence ((s byte-counting-stream) sequence start end)
  (incf (stream:stream-file-position s) (- end start)))

(defun count-duplicates (list &key (key #'identity) (test #'eql))
  "Count the number of duplicate items in the list, and return the count.
  Note that common lisp requires TEST be one of EQ, EQL, EQUAL, EQUALP for hashtables.
  The result is zero if there are no duplicates."
  (let ((hash-table (make-hash-table :size (length list) :test test)))
    (do ((tail list (cdr tail))
         (dups 0    (+ dups
                       (let* ((hashkey (funcall key (car tail)))
                              (probe   (gethash hashkey hash-table)))
                         (setf (gethash hashkey hash-table) t)
                         (if probe 1 0)))))
        ((null tail) dups))))

(defun dump-file (pathname &key (start 0) end (bytes-per-line 16))
  "Write the contents of a binary file to *standard-output* in a human readable format."
  (with-open-file (stream pathname
                   :direction :input
                   #-(and :allegro-version>= (:version>= 6 0)) :element-type
                   #-(and :allegro-version>= (:version>= 6 0)) '(unsigned-byte 8))
    (let ((index (* bytes-per-line (floor start bytes-per-line))))
      (file-position stream index)
      (block done
        (loop
          (format t "~&~8,'0x  " index)
          (dotimes (i bytes-per-line)
            (let ((byte (read-byte stream nil nil)) char)
              (unless byte (return-from done))
              (write-char #\space)
              (setq char (code-char byte))
              (case char
                (#\space (write-string "__"))
                (#\tab   (write-string "\\t"))
                (#\page  (write-string "^L"))
                (#\vt    (write-string "vt"))
                (t (case byte
                     (10 (write-string "lf"))
                     (13 (write-string "cr"))
                     (t (if (<= (char-code #\space) byte (char-code #\~))
                            (format t " ~c" char)
                            (format t "~2,'0x" byte)))))))
            (incf index))
          (when (and end (>= index end))
            (return-from done))))))
  pathname)

(defmacro define-enumeration ((&key (start 0) set-symbol last-value-parameter-name)
                              &rest items)
  "Define every symbol in ITEMS as a DEFCONSTANT with an increasing integer value, starting
   from START which defaults to zero.

   If SET-SYMBOL is specified, it names a symbol which will be defined as with DEFCONSTANT,
   and which symbolically names the symbols passed as ITEMS.  In this way, doing
   (elt set-symbol <n>) => (elt items <n>).  Where <n> = <n> - START.
   Useful from getting the symbol naming an enumerated integer.

   If LAST-VALUE-PARAMETER-NAME is specified, it must be a symbol which will be used to
   generate a DEFPARAMETER'ed name whose value is the last allocated (therefore inclusive)
   constant in the enumeration."
  (loop for entry in items
        for (symbol . docstring) = (if (listp entry)
                                       entry
                                     (list entry))
        for count from (eval start)
        collect `(defparameter ,symbol ,count ,@docstring)
                ;; If these were defined using DEFCONSTANT instead of
                ;; DEFPARAMETER then ACL's xref database would fail to
                ;; keep track of what functions used which members of
                ;; the enumeration set.
        into defconstant-forms
        collect symbol into symbols
        finally
        (return `(progn
                   ,@(when set-symbol
                       `((defparameter ,set-symbol ',symbols "Enumeration symbol set")))
                   ,@(when last-value-parameter-name
                       `((defparameter ,last-value-parameter-name ,count
                           "Last inclusive value of the an enumeration of values.")))
                   ,@defconstant-forms))))

(defun file-copy-to-stream (file stream)
  (collect-stream stream (scan-file file #'read-char) #'write-char))

;;
;; Fast number<->string
;;

;; Assuming that most-positive-fixnum is <=  8388607

(defun fixnum-base10-digit-count (fixnum)
  (declare #.(performance-optimizations)
           (type fixnum fixnum))
  (if (< fixnum #.(expt 10 4))
      (if (< fixnum #.(expt 10 2))
          (if (< fixnum #.(expt 10 1))
              1
            2)
        (if (< fixnum #.(expt 10 3))
            3
          4))
    (if (< fixnum #.(expt 10 6))
        (if (< fixnum #.(expt 10 5))
            5
          6)
        7)))

(defun %write-fixnum-to-string (number string position)
  "Write the digits of FIXNUM to STRING in base 10, most significant digit
   at POSITION."
  (declare #.(performance-optimizations)
           (type fixnum number)
           (type array-index position)
           (type lw:simple-text-string string)
           )

  (do ((pos (1- (+ position (the (integer 0 (8)) (fixnum-base10-digit-count number))))
            (1- pos)))
      ((< pos position))
    (declare (type fixnum pos))
    (multiple-value-bind (quotient remainder)
        (floor number 10)
      (declare (type fixnum quotient)
               (type (integer 0 (10)) remainder))
      (setq number quotient)
      (setf (char string pos)
            (char "0123456789" remainder)))))

(defun keyword-package ()
  #+lispworks (sys:keyword-package)
  #-lispworks (find-package "KEYWORD"))

(defun make-keyword (string)
  (intern (string string) (keyword-package)))

(defun find-keyword (string)
  (find-symbol (string string) (keyword-package)))

(defun %mirror-bits (integer width result)
  (declare (type integer integer result)
           (type (integer 0 #.most-positive-fixnum) width)
           #.(performance-optimizations))
  (if (zerop width)
      result
      (%mirror-bits (ash integer -1) (1- width) (if (evenp integer)
                                                  (ash result 1)
                                                  (1+ (ash result 1))))))

(defun mirror-bits (integer width)
  "Return an integer of length WIDTH whose bit pattern is the
   mirror image of INTEGER."
  (check-type integer integer)
  (check-type width (integer 0 #.most-positive-fixnum))
  (%mirror-bits integer width 0))

(defun %mirror-fixnum (fixnum)
  "Return a fixnum whose bit pattern is the mirror image of the input."
  (declare (type fixnum fixnum)
           #.(performance-optimizations))
  (do ((input  fixnum (floor input 2))
       (output      0 (if (evenp input)
                          (fix:* output 2)
                          (fix:1+ (fix:* output 2))))
       (count       0 (fix:1+ count)))
      ((= count #.(integer-length most-positive-fixnum)) output)
    (declare (type fixnum input output)
             (type (integer 0 #.(integer-length most-positive-fixnum)) count))
    ))

(defun plist->alist (plist)
  (multiple-value-bind (keys values) (scan-plist plist)
    (collect-alist keys values)))

(defun permutations (list)
  "Compute a list of the perumations of <list>."
  (if (null list)
      (list '())
      (let ((element (car list)))
        (mapcan
         (lambda (tail)
           (named-let loup ((pre '())
                            (post tail)
                            (answers '()))
                      (let ((more-answers (cons (revappend pre (cons element post))
                                                answers)))
                        (if (null post)
                            (reverse more-answers)
                            (loup (cons (car post) pre)
                                  (cdr post)
                                  more-answers)))))
         (permutations (cdr list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; read lisp expressions safely

(defun safe-read (&rest read-args)
  "Calls READ with *READ-EVAL* bound to NIL."
  (let ((*read-eval* nil))
    (apply #'read read-args)))

(defun split-string-aux (string delimiter start limit)
  (declare (type string string)
           (type character delimiter)
           (type array-index start limit)
           #.(performance-optimizations))

  (let ((index (position delimiter string :start start :end limit :test #'char=)))
    (if (null index)
        (list (if (and (= start 0)
                       (= limit (string-length string)))
                  ;; Don't cons a new string if it isn't necessary.
                  string
                  (subseq string start limit)))
        (cons (subseq string start index)
              (split-string-aux string delimiter (1+ index) limit)))))

(defun split-string (string delimiter &key (start 0) end)
  "Return a list of strings generated by splitting STRING
   on the DELIMITER characters."
  (check-type string string)
  (check-type delimiter character)
  (check-type start array-index)
  (check-type end (or null array-index))
  (split-string-aux string delimiter start (or end (string-length string))))

(defun split-string-first (string delimiter &key (start 0) end)
  "Return two values, the subsequence of characters up to the
   first occurrance of delimiter, and the sequence of characters
   after that character.  If the delimiter does not appear in string,
   the second value is NIL."
  (check-type string string)
  (check-type delimiter character)
  (check-type start array-index)
  (check-type end (or null array-index))
  (let* ((limit (or end (string-length string)))
         (index (position delimiter string :start start :end limit :test #'char=)))
    (if (null index)
        (if (and (= start 0)
                 (= limit (string-length string)))
            (values string nil)
            (values (subseq string start end) nil))
        (values (subseq string start index)
                (subseq string (1+ index) limit)))))

(defun split-string-last (string delimiter &key (start 0) end)
  "Return two values, the subsequence of characters up to the
   last occurrance of delimiter, and the sequence of characters
   after that character.  If the delimiter does not appear in string,
   the second value is NIL."
  (check-type string string)
  (check-type delimiter character)
  (check-type start array-index)
  (check-type end (or null array-index))
  (let* ((limit (or end (string-length string)))
         (index (position delimiter string
                          :from-end t
                          :start start :end limit
                          :test #'char=)))
    (if (null index)
        (if (and (= start 0)
                 (= limit (string-length string)))
            (values string nil)
            (values (subseq string start limit) nil))
        (values (subseq string start index)
                (subseq string (1+ index) limit)))))

(defun ratio->percentage (numerator denominator)
  "Return an integer between 0 and 100 that is closest
   to numerator/denominator."
  (round (* numerator 100) denominator))

(defun read-fixnum (stream)
  (declare #.(performance-optimizations))
  (let* ((b0 (read-byte stream))
         (b1 (read-byte stream))
         (b2 (read-byte stream)))
    ;(declare (type (unsigned-byte 8) b0 b1 b2))
    ;; by checking for the top byte being zero,
    ;; we can avoid a trip through the bignum
    ;; routines.
    (if (zerop b2)
        (the (unsigned-byte 16)
          (+ (the (unsigned-byte 16) (ash b1 8))
             b0))
        (+ (the (unsigned-byte 24)
             (ash b2 16))
           (the (unsigned-byte 16)
             (+ (the (unsigned-byte 16) (ash b1 8))
                b0))))))

(declaim (ftype (function (input-stream) (unsigned-byte 16)) read-unsigned16)
         (inline read-unsigned16))

(defun read-unsigned16 (stream)
  (declare #.(performance-optimizations))
  (let* ((b0 (read-byte stream))
         (b1 (read-byte stream)))
    (declare (type (unsigned-byte 8) b0 b1))
    (the (unsigned-byte 16)
      (+ (the (unsigned-byte 16) (ash b1 8))
         b0))))

(defun read-unsigned32 (stream)
  (declare #.(performance-optimizations))
  (let* ((word0 (read-unsigned16 stream))
         (word1 (read-unsigned16 stream)))
    (declare (type (unsigned-byte 16) word0 word1))
    (if (< word1 #.(ash most-positive-fixnum -16))
        (the fixnum
          (+ (the fixnum (ash word1 16))
             word0))
        (the (unsigned-byte 32)
          (+ (the (unsigned-byte 32) (ash word1 16))
             word0)))))

(defun read-string-data (length stream)
  (declare (type array-index length)
           #.(performance-optimizations))
  (let ((temp (simple-vector-8b-allocate length))
        (result (simple-string-8b-allocate length)))
    (declare (type (simple-vector-8b) temp)
             (type (simple-string-8b) result))
    (read-sequence temp stream)
    (dotimes (i length result)
      (setf (schar result i) (code-char (aref temp i))))))

(defun write-string-data (string stream)
  (write-sequence (map 'simple-vector-8b #'char-code string) stream))

(defun read-string-16b-data-le (length stream)
  (let ((temp (simple-vector-8b-allocate (* length 2))))
    (read-sequence temp stream)
    (multiple-value-bind (lobytes hibytes) (chunk 2 2 (scan 'simple-vector-8b temp))
      (collect 'simple-string-16b
               (map-fn 'char-16b
                       (lambda (hibyte lobyte)
                         (+ (ash hibyte 8) lobyte))
                       hibytes
                       lobytes)))))

(defun write-string-16b-data-le (string stream)
  (write-sequence
   (collect 'simple-vector-8b
            (16bit->8bit-le
             (#M char-code
                 (scan 'string-16b string))))
   stream))

;;; Make these perform better.
(defun write-simple-bit-vector (vector stream)
  (let ((bytes (ceiling (simple-bit-vector-length vector) 8)))
    (do ((byte-count 0 (1+ byte-count))
         (index 0 (+ index 8)))
        ((>= byte-count bytes))
      (do ((byte 0 (+ (* byte 2)
                      (if (>= idx (simple-bit-vector-length vector))
                          0
                          (sbit vector idx))))
           (count 0 (+ count 1))
           (idx (+ index 7) (- idx 1)))
          ((>= count 8) (write-byte byte stream))))))

(defun read-simple-bit-vector (length stream)
  (let* ((result (simple-vector-1b-allocate length))
         (bytes (ceiling length 8)))
    (do ((byte-count 0 (1+ byte-count))
         (index 0 (+ index 8)))
        ((>= byte-count bytes) result)
      (do ((byte (read-byte stream) (floor byte 2))
           (count 0 (+ count 1))
           (idx  index (+ idx 1)))
          ((>= count 8))
        (when (< idx length)
          (setf (sbit result idx) (if (evenp byte) 0 1)))))))

(declaim (ftype (function ((fixnum output-stream) (values)) %write-fixnum))
         (ftype (function (((unsigned-byte 16) output-stream) (values)) %write-unsigned16))
         (ftype (function (((unsigned-byte 32) output-stream) (values)) %write-unsigned32))
         (inline %write-fixnum
                 %write-unsigned16
                 %write-unsigned32))

(defun %write-fixnum (value stream)
  (declare #.(performance-optimizations)
           (type fixnum value)
           )
  (write-byte (ldb (byte 8 0)  value) stream)
  (write-byte (ldb (byte 8 8)  value) stream)
  (write-byte (ldb (byte 8 16) value) stream))

(defun %write-unsigned16 (value stream)
  (declare #.(performance-optimizations)
           (type (unsigned-byte 16) value)
           )
  (write-byte (ldb (byte 8 0)  value) stream)
  (write-byte (ldb (byte 8 8)  value) stream))

(defun %write-unsigned32 (value stream)
  (declare #.(performance-optimizations)
           (type (unsigned-byte 32) value)
           )
  (%write-unsigned16 (the (unsigned-byte 16) (logand value #xFFFF)) stream)
  (%write-unsigned16 (the (unsigned-byte 16) (ash value -16)) stream))

(defun write-fixnum (value stream)
  (check-type value fixnum)
  (%write-fixnum value stream))

(defun write-unsigned16 (value stream)
  (check-type value (unsigned-byte 16))
  (%write-unsigned16 value stream))

(defun write-unsigned32 (value stream)
  (check-type value (unsigned-byte 32))
  (%write-unsigned32 value stream))

(defun %expmod (base exponent mod)
  (declare (type (integer 0 *) exponent)
           (type (integer 1 *) base mod))
  (cond ((zerop exponent) 1)
        ((evenp exponent) (mod (let ((x (%expmod base (/ exponent 2) mod)))
                                 (* x x))
                               mod))
        (t (mod (* base (%expmod base (1- exponent) mod))
                mod))))

(defun expmod (base exponent mod)
  "Raise BASE to EXPONENT power modulo MOD."
  (check-type base (integer 1 *))
  (check-type mod (integer 1 *))
  (check-type exponent (integer 0 *))
  (%expmod base exponent mod))

(defun fermat-test (n)
  "Use Fermat's little theorem to test N for primality.
   Return value of NIL means N is not prime.
   Return value of T means N is likely to be prime with 50% confidence."
  (let ((trial-number (1+ (random (1- n)))))
    (= (expmod trial-number n n) trial-number)))

(defun prime? (n)
  (dotimes (i 128 t)
    (unless (fermat-test n)
      (return nil))))

(defun n-digit-prime (n)
  (let ((trial (random (1- (expt 2 n)))))
    (if (prime? trial)
        trial
        (n-digit-prime n))))

;;; Character<->byte sequence conversions
;;; These functions are critical to the file-system operations, so they are
;;; bummed.

(defun %simple-string-8b-to-bytes (src-start src-end string byte-array dest-start)
  "Unsafe, high-performance version of string-to-bytes.

   Assumes that the string is lw:simple-text-string,
   the byte array is (simple-array (unsigned-byte 8) (*)),
   and that the indices are within bounds and there is enough room.

   Extracts characters from STRING and inserts their char-code into
   BYTE-ARRAY."
  (declare #.(performance-optimizations)
           (type simple-string-8b string)
           (type simple-vector-8b byte-array)
           (type array-index src-start src-end dest-start)
           #+allegro (:explain :calls :types)
           )
  (do ()
      ((= src-start src-end) NIL)
    (setf (simple-vector-8b-ref byte-array dest-start)
          (char-code (simple-string-8b-ref string src-start)))
    (incf src-start)
    (incf dest-start)))

(defun %simple-string-16b-to-bytes (src-start src-end string byte-array dest-start)
  "Unsafe, high-performance version of string-to-bytes.

   Assumes that the string is lw:simple-text-string,
   the byte array is (simple-array (unsigned-byte 8) (*)),
   and that the indices are within bounds and there is enough room.

   Extracts characters from STRING and inserts their char-code into
   BYTE-ARRAY.

   DISCARDS THE TOP BYTE"
  (declare #.(performance-optimizations)
           (type simple-string-16b string)
           (type simple-vector-8b byte-array)
           (type array-index src-start src-end dest-start)
           #+allegro (:explain :calls :types)
           )
  (do ()
      ((= src-start src-end) NIL)
    (setf (simple-vector-8b-ref byte-array dest-start)
          (logand #xFF (char-code (simple-string-16b-ref string src-start))))
    (incf src-start)
    (incf dest-start)))

;; bummed
(defun %bytes-to-simple-string-8b (src-start src-end byte-array string dest-start)
  "Unsafe, high-performance version of bytes-to-string

   Assumes that the string is lw:simple-text-string,
   the byte array is (simple-array (unsigned-byte 8) (*)),
   and that the indices are within bounds and there is enough room.

   Extracts bytes from BYTE-ARRAY and inserts their code-char into STRING."
  (declare #.(performance-optimizations)
           (type simple-vector-8b byte-array)
           (type simple-string-8b string)
           (type array-index src-start src-end dest-start)
           #+allegro (:explain :types :calls)
           )
  (do ()
      ((= src-start src-end) NIL)
    (setf (simple-string-8b-ref string dest-start)
          (code-char (simple-vector-8b-ref byte-array src-start)))
    (incf src-start)
    (incf dest-start)))

(defun %bytes-to-simple-string-16b (src-start src-end byte-array string dest-start)
  "Unsafe, high-performance version of bytes-to-string

   Assumes that the string is simple-string-16b
   the byte array is (simple-array (unsigned-byte 8) (*)),
   and that the indices are within bounds and there is enough room.

   Extracts bytes from BYTE-ARRAY and inserts their code-char into STRING."
  (declare #.(performance-optimizations)
           (type simple-vector-8b byte-array)
           (type simple-string-16b string)
           (type array-index src-start src-end dest-start)
           #+allegro (:explain :types :calls)
           )
  (do ()
      ((= src-start src-end) NIL)
    (setf (simple-string-8b-ref string dest-start)
          (code-char (simple-vector-16b-ref byte-array src-start)))
    (incf src-start)
    (incf dest-start)))

(defun simple-string-8b-to-bytes (string &key byte-array (start 0) end)
  "Return an array with element-type (UNSIGNED-BYTE 8) which reflects the character codes of STRING.

   START and END are the 'bounding index designators' of STRING.
   START is the inclusive index of the first character in STRING to translate, and defaults to zero.
   END is the exclusive index of the last character in STRING to translate, and defaults to NIL
   which implies the end (length) of the string.

   If BYTE-ARRAY is supplied, it is used as the result buffer and returned.  It must be of type
   (ARRAY (UNSIGNED-BYTE 8) (*)).   Should this array have fewer elements than is indicated by START and END,
   only only those (fewer) elements are translated.

   If BYTE-ARRAY is not supplied, we return a newly consed array of sufficient size to hold all
   elements designated by the bounding index designators for the string.

   WARNING: if BYTE-ARRAY is longer than the number of characters to be processed in STRING,
   unfilled element contents are unspecified, and should probably be initialized (or return value
   for last index observed) before access.

   We return two values, the byte array, and the exclusive upper bound (<= end) which represents
   index of the next character in STRING which was not processed."
  (declare (type array-index start))
  (let ((end1 (or end (string-length string))))
    (declare (type array-index end1))
    (if byte-array
        (check-type byte-array simple-vector-8b)
        (setq byte-array (simple-vector-8b-allocate (- end1 start))))
    (locally
     (declare #.(performance-optimizations)
              (type simple-string-8b string)
              (type simple-vector-8b byte-array)
              #+allegro (:explain :calls :types)
              )
     (let* ((len (length byte-array))
            (limit (if (< len (- end1 start))
                       (+ start len)
                       end1)))
       (declare (type array-index len))
       (%simple-string-8b-to-bytes start limit string byte-array 0)
       (values byte-array limit)))))

(defun simple-string-16b-to-bytes (string &key byte-array (start 0) end)
  "Return an array with element-type (UNSIGNED-BYTE 8) which reflects the character codes of STRING.

   START and END are the 'bounding index designators' of STRING.
   START is the inclusive index of the first character in STRING to translate, and defaults to zero.
   END is the exclusive index of the last character in STRING to translate, and defaults to NIL
   which implies the end (length) of the string.

   If BYTE-ARRAY is supplied, it is used as the result buffer and returned.  It must be of type
   (ARRAY (UNSIGNED-BYTE 8) (*)).   Should this array have fewer elements than is indicated by START and END,
   only only those (fewer) elements are translated.

   If BYTE-ARRAY is not supplied, we return a newly consed array of sufficient size to hold all
   elements designated by the bounding index designators for the string.

   WARNING: if BYTE-ARRAY is longer than the number of characters to be processed in STRING,
   unfilled element contents are unspecified, and should probably be initialized (or return value
   for last index observed) before access.

   We return two values, the byte array, and the exclusive upper bound (<= end) which represents
   index of the next character in STRING which was not processed."
  (declare (type array-index start))
  (let ((end1 (or end  (string-length string))))
    (declare (type array-index end1))
    (if byte-array
        (check-type byte-array simple-vector-8b)
        (setq byte-array (simple-vector-8b-allocate (- end1 start))))
    (locally
     (declare #.(performance-optimizations)
              (type simple-string-16b string)
              (type simple-vector-8b byte-array)
              #+allegro (:explain :calls :types)
              )
     (let* ((len (length byte-array))
            (limit (if (< len (- end1 start))
                       (+ start len)
                       end1)))
       (declare (type array-index len))
       (%simple-string-16b-to-bytes start limit string byte-array 0)
       (values byte-array limit)))))

(defun simple-string-to-bytes (string &key byte-array (start 0) end)
  "Return an array with element-type (UNSIGNED-BYTE 8) which reflects the character codes of STRING.

   START and END are the 'bounding index designators' of STRING.
   START is the inclusive index of the first character in STRING to translate, and defaults to zero.
   END is the exclusive index of the last character in STRING to translate, and defaults to NIL
   which implies the end (length) of the string.

   If BYTE-ARRAY is supplied, it is used as the result buffer and returned.  It must be of type
   (ARRAY (UNSIGNED-BYTE 8) (*)).   Should this array have fewer elements than is indicated by START and END,
   only only those (fewer) elements are translated.

   If BYTE-ARRAY is not supplied, we return a newly consed array of sufficient size to hold all
   elements designated by the bounding index designators for the string.

   WARNING: if BYTE-ARRAY is longer than the number of characters to be processed in STRING,
   unfilled element contents are unspecified, and should probably be initialized (or return value
   for last index observed) before access.

   We return two values, the byte array, and the exclusive upper bound (<= end) which represents
   index of the next character in STRING which was not processed."
  (etypecase string
    (simple-string-8b (simple-string-8b-to-bytes string
                                                 :byte-array byte-array
                                                 :start start
                                                 :end (or end (string-length string))))
    (simple-string-16b (simple-string-16b-to-bytes string
                                                   :byte-array byte-array
                                                   :start start
                                                   :end (or end (string-length string))))))

(defun simple-bytes-to-simple-string-8b (byte-array &optional (start 0) end)
  "Return an simple-array with element-type CHARACTER (a string) which reflects
   the character values of bytes in BYTE-ARRAY, which must be of type (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).

   START and END must be positive fixnums that are the 'bounding index designators' of BYTE-ARRAY.
   START is the inclusive index of the first byte in BYTE-ARRAY to translate.
   END is the exclusive index of the last byte in BYTE-ARRAY to translate.

   This procedure is optimized for speed, so no kind of validity checking is done
   on the arguments.

   We return a newly consed string of sufficient size to hold all
   elements designated by the bounding index designators for the byte-array

   We return the string."
  (declare (type array-index start)
           (type simple-vector-8b byte-array)
           )
  (let* ((limit (or end (length byte-array)))
         (len (- limit start))
         (string (simple-string-8b-allocate len)))
    (%bytes-to-simple-string-8b start limit byte-array string 0)
    string))

(defun simple-bytes-to-string-16b (byte-array start end)
  "Return an simple-array with element-type CHARACTER (a string) which reflects
   the character values of bytes in BYTE-ARRAY, which must be of type (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).

   START and END must be positive fixnums that are the 'bounding index designators' of BYTE-ARRAY.
   START is the inclusive index of the first byte in BYTE-ARRAY to translate.
   END is the exclusive index of the last byte in BYTE-ARRAY to translate.

   This procedure is optimized for speed, so no kind of validity checking is done
   on the arguments.

   We return a newly consed string of sufficient size to hold all
   elements designated by the bounding index designators for the byte-array

   We return the string."
  (declare (type array-index start end)
           (type simple-vector-16b byte-array)
           )
  (let* ((len (- end start))
         (string (simple-string-16b-allocate len)))
    (%bytes-to-simple-string-16b start end byte-array string 0)
    string))

(defun bytes-to-string (byte-array &key string (start 0) end)
  "Return an simple-array with element-type CHARACTER (a string) which reflects
   the character values of bytes in BYTE-ARRAY, which must be of type (ARRAY (UNSIGNED-BYTE 8) (*)).

   START and END are the 'bounding index designators' of BYTE-ARRAY.
   START is the inclusive index of the first byte in BYTE-ARRAY to translate, and defaults to zero.
   END is the exclusive index of the last byte in BYTE-ARRAY to translate, and defaults to NIL
   which implies the end (length) of the BYTE-ARRAY.

   If STRING is supplied, it is used as the result buffer and returned.  It must be of type
   (SIMPLE-ARRAY CHARACTER (*)).   Should this array have fewer elements than is indicated by START and END,
   only only those (fewer) elements are translated.

   If STRING is not supplied, we return a newly consed string of sufficient size to hold all
   elements designated by the bounding index designators for the byte-array

   WARNING: if STRING is longer than the number of characters to be processed in BYTE-ARRAY,
   unfilled element contents are unspecified, and should probably be initialized (or return value
   for last index observed) before access.

   We return two values, the string, and the exclusive upper bound (<= end) which represents
   index of the next byte in BYTE-ARRAY which was not processed."
  (declare (type array-index start))
  (check-type byte-array simple-vector-8b)
  (let ((end1 (or end (vector-length byte-array))))
    (declare (type array-index end1))
    (if string
        (check-type  string simple-string)
        (setq string (simple-string-8b-allocate (- end1 start))))
    (etypecase string
      (simple-string-8b (let* ((len (simple-string-8b-length string))
                               (limit (if (< len (- end1 start))
                                          (+ start len)
                                          end1)))
                          (declare (type array-index len))
                          (%bytes-to-simple-string-8b  start limit byte-array string 0)
                          (values string limit)))
      (simple-string-16b (let* ((len (simple-string-16b-length string))
                                (limit (if (< len (- end1 start))
                                           (+ start len)
                                           end1)))
                           (declare (type array-index len))
                           (%bytes-to-simple-string-16b  start limit byte-array string 0)
                           (values string limit))))))

(defun nth-member (n element series &key (test #'eql))
  (declare (type array-index n)
           (type list series)
           )
  (let ((found (member element series :test test)))
    (when found
      (if (zerop n)
          found
        (nth-member (1- n) element (cdr found) :test test)))))

#||
(defun test-bytes-to-string ()
  (let ((foo (make-string 10000))
        (bytes (make-array 10000 :element-type '(unsigned-byte 8))))
    (map-into foo (lambda (char)
                    (code-char (logand #xFF (char-code char)))) foo)
    (assert (string-equal foo (bytes-to-string (string-to-bytes foo :byte-array bytes))))
    (time (loop repeat 5000 do (bytes-to-string (string-to-bytes foo :byte-array bytes) :string foo)))))
||#

;;; Logical name encoding
;;;; Name mapping.
;;;;
;;;; It should be possible to represent file names in a reasonable
;;;; portable manner.  Logical pathnames provide us with much of the
;;;; necessary abstraction, but there is a problem:  Only numbers,
;;;; characters, and hyphens may appear in a logical pathname.  Most
;;;; systems make use of additional characters.
;;;;
;;;; In order to deal with this in an intelligent manner, we encode
;;;; the non-conforming characters.
;;;;
;;;; Details of the encoding mechanism are in
;;;; ACM Ada Letters XIII, 5 (Sep/Oct 1993), 43-47
;;;; Strategies for the Lossless Encoding of Strings as Ada Identifiers
;;;; (See ftp://ftp.netcom.com/pub/hb/hbaker/Encode.html)
;;;;
;;;; Not the obvious choice, I admit, but read the paper and you will become
;;;; enlightened.

(defparameter *character-map*
    '((#\! . "BNG")                     ; bang.  "exclamation point" is too long
      (#\" . "QUT2")                    ; "double quote" is too long
      (#\# . "SHRP")                    ; sharp.  "pound sign" is too parochial
      (#\$ . "DLLR")                    ; dollar
      (#\% . "PCT")                     ; percent
      (#\& . "AMPR")                    ; ampersand. "and" assumes C usage.
      (#\' . "QUT1")                    ; "single quote" is too long
      (#\( . "LPAR")                    ; "left parenthesis" is too long
      (#\) . "RPAR")                    ; "right parenthesis" is too long
      (#\* . "STR")                     ; star.  "asterisk" is too long
      (#\, . "CMMA")                    ; comma
      (#\: . "CLN")                     ; colon
      (#\; . "SMICLN")    ; semicolon
      (#\< . "LSS")                     ; less.  "less than" is too long
      (#\= . "EQUL")                    ; equal. "equal to" is too long
      (#\> . "GRT")                     ; greater. "greater than" is too long
      (#\? . "QST")                     ; question. "question mark" is too long
      (#\@ . "ATSGN")                   ; at sign.  "at" is too ambiguous
      (#\[ . "LBRK")                    ; "left square bracket" is too long
      (#\\ . "BSLSH")                   ; "back slash" is too long
      (#\] . "RBRK")                    ; "right square bracket" is too long
      (#\^ . "UPARW")                   ; up arrow. "circumflex" is too pedantic
      (#\_ . "UNDR")                    ; under. "underline" and "underscore" are too long
      (#\` . "BQUOT")                   ; "back quote" is too long
      (#\{ . "LBRC")                    ; "left curly brace" is too long
      (#\| . "VBAR")                    ; "vertical bar" is too long
      (#\} . "RBRC")                    ; "right curly brace" is too long
      (#\~ . "TLD")                     ; tilde
      (#\+ . "PLS")                     ; plus
      (#\- . "MNS")                     ; minus
      (#\. . "PRD")                     ; "period" is too long
      (#\/ . "SLSH")                    ; slash.  "divided" ignores "not" (/=) connotation
      (#\space  . "SPCE")               ; space
      ;; Add more definitions here
      )
  "List of characters that cannot appear in filename and the upper-case string mapping
that is substituted in place.")

(defsubst self-mapping-char-p (char)
  "Return T iff the char is a lower-case alphabetic or a numeric character."
  (or (and (alpha-char-p char)
           (lower-case-p char))
      (digit-char-p char)))

(defconstant *quote-syllable* "QQ")
(defconstant *quote-upcase-syllable* "QQU")
(defconstant *quote-capitalize-syllable* "QQC")

(defun capitalized-p (word)
  (and (> (string-length word) 0)
       (alpha-char-p (char word 0))
       (upper-case-p (char word 0))
       (not (find-if (lambda (char)
                       (and (alpha-char-p char)
                            (upper-case-p char)))
                     word :start 1))))

(defun encode-namestring-loop (namestring start end)
  (declare (type simple-string namestring)
           (type array-index start end)
           )
  (unless (>= start end)
    (let ((scan (position-if (complement #'self-mapping-char-p)
                             namestring
                             :start start
                             :end end)))
      (if (null scan)
          (let* ((syl (string-upcase (subseq namestring start end)))
                 (probe (rassoc syl *character-map* :test #'equal)))
            (if (or probe
                    (string= syl *quote-capitalize-syllable*)
                    (string= syl *quote-upcase-syllable*)
                    (string= syl *quote-syllable*))
                (list *quote-syllable* syl)
                (list syl)))
          (locally (declare (type array-index scan))
            (if (= scan start)
                (let* ((bogon (schar namestring scan))
                       (probe (assoc bogon *character-map*)))
                  (cond ((and (eql bogon #\-)
                              (> scan 0)
                              (< scan (1- end))
                              (self-mapping-char-p (schar namestring
                                                          (1- scan)))
                              (self-mapping-char-p (schar namestring
                                                          (1+ scan))))
                         (encode-namestring-loop namestring (1+ scan) end))
                        (probe (cons (cdr probe) (encode-namestring-loop
                                                  namestring
                                                  (1+ scan) end)))
                        ((upper-case-p bogon)
                         (let* ((scan1 (or (position-if (complement (lambda (char)
                                                                      (and (alpha-char-p char)
                                                                           (upper-case-p char))))
                                                        namestring
                                                        :start scan
                                                        :end end)
                                           end))
                                (scan2 (if (and (= scan1 (1+ scan)) ;; just one upcase character
                                                (< scan1 end))       ;; more chars
                                           (or
                                            (position-if (complement (lambda (char)
                                                                       (and (alpha-char-p char)
                                                                            (not (upper-case-p char)))))
                                                         namestring
                                                         :start scan1
                                                         :end end)
                                            end)
                                           scan)))
                           (declare (type array-index scan1 scan2))
                           (if (> scan2 scan1)
                               (list* *quote-capitalize-syllable*
                                      (string-upcase (subseq namestring start scan2))
                                      (encode-namestring-loop namestring scan2 end))
                               (list* *quote-upcase-syllable*
                                      (subseq namestring start scan1)
                                      (encode-namestring-loop namestring scan1 end)))))
                        (t (cons (string bogon)
                                 (encode-namestring-loop
                                  namestring
                                  (1+ scan) end)))))
                (let* ((syl (string-upcase (subseq namestring start scan)))
                       (probe (rassoc syl *character-map* :test #'equal)))
                  (if (or probe
                          (string= syl *quote-capitalize-syllable*)
                          (string= syl *quote-upcase-syllable*)
                          (string= syl *quote-syllable*))
                      (list* *quote-syllable*
                             syl
                             (encode-namestring-loop namestring scan end))
                      (cons syl
                            (encode-namestring-loop namestring scan end))))))))))

;;; The main entry points.
(defun encode-namestring (namestring)
  "Given a NAMESTRING that represents a pathname component, return a new string that
can be used as a logical pathname component.

This can be done in a machine independent way."
  (check-type namestring simple-string)
  (reduce (lambda (accum more)
            (concatenate 'string accum "-" more))
          (encode-namestring-loop namestring 0 (simple-string-length namestring))))

(defun decode-namestring (encoded)
  "Given an ENCODED namestring that represents a logical pathname component, return a
new string that can be used as a physical pathname component.

Note, this mapping should be done in a machine dependent way, but for
the common case, UNIX and Windows, we can get away with a generic mapping."
  (declare (type simple-string encoded))
  (tail-labels ((generate-syllables (start end)
                  (declare (type array-index start end))
                  (let ((scan (position #\- encoded :start start :end end)))
                    (if (null scan)
                        (list (subseq encoded start end))
                      (cons (subseq encoded start scan)
                            (generate-syllables (1+ scan) end)))))

                (decode-syllables (sylls)
                  (cond ((null sylls) nil)
                        ((string= (car sylls) *quote-syllable*) (cons (string-downcase (cadr sylls))
                                                        (decode-syllables (cddr sylls))))
                        ((string= (car sylls) *quote-capitalize-syllable*) (cons (string-capitalize (cadr sylls))
                                                                                (decode-syllables (cddr sylls))))
                        ((string= (car sylls) *quote-upcase-syllable*) (cons (cadr sylls)
                                                         (decode-syllables (cddr sylls))))
                        ((rassoc (car sylls) *character-map* :test #'equal)
                         => (lambda (probe) (cons (string (car probe)) (decode-syllables (cdr sylls)))))
                        (t (cons (string-downcase (car sylls)) (decode-syllables (cdr sylls)))))))

    (reduce (lambda (accum more)
                (declare (type simple-string accum more))
                (if (and (self-mapping-char-p (schar accum (1- (length accum))))
                         (self-mapping-char-p (schar more 0)))
                    (concatenate 'string accum "-" more)
                  (concatenate 'string accum more)))
            (decode-syllables
             (generate-syllables 0 (length encoded))))
    ))

;;; Object equality

(defvar *objects-equalp-circularity-stack* nil
  "A stack that holds a list of calls to objects-equalp to avoid
 circularity problems.")

(defgeneric objects-equalp (left right)
  (:documentation
   "Routine to use in place of EQUALP to determine if two objects are semantically equal.
    CLOS and structure types are free to overload this method as appropriate.

    DEFINE-TENN-CLASS and DEFINE-TENN-STRUCTURE should initially provide an equality method
    which is functionally equivalent to EQUALP, but this hasn't been implemented yet.  So if you don't
    overload the method, the default methods will perform EQUALP on their objects.")

  ;; Don't define this.
  ;; (:method ((left t) (right t))
  ;;   (equalp object-1 object-2))

  ;; JRM's reason for not defining the above is that it is easier to
  ;; debug the case of someone not defining a method between two
  ;; different types which are meant to be OBJECTS-EQUALP if that case
  ;; causes a NO-APPLICABLE-METHOD error.  I (naha) will humor him
  ;; even though I feel having a default method on (T T) is the right
  ;; thing to do.

  ;; I truly appreciate this.  I agree that a default method is probably
  ;; correct in the long term, but I got bitten by this and find that the
  ;; error condition helps me debug. ~jrm

  ;; Instead of having the default method, for each new
  ;; type for which we are defining behavior for OBJECTS-EQUALP, we
  ;; also need to define a pair of methods for that type and T (one
  ;; method for each specializer order.  See
  ;; DEFINE-OBJECTS-EQUALP-TYPE-MISMATCH-METHODS below.

  (:method ((left number) (right number))
    "Numbers are equalp if they are =."
    (= left right))

  ;;  (:method ((left symbol) (right symbol))
  ;;    (eq left right))

  (:method ((left cons) (right cons))
    "Recursively descend conses."
    (and (objects-equalp (car left) (car right))
         (objects-equalp (cdr left) (cdr right))))

  (:method ((left vector) (right vector))
    "Recursively descend simple vectors."
    (or (and (simple-vector-p left)
             (simple-vector-p right)
             (= (simple-vector-length left) (simple-vector-length right))
             (not (mismatch left right :test #'objects-equalp)))
        (error "Not simple vectors.")))

  (:method ((left string) (right string))
    (not (mismatch left right :test #'char=)))

  (:method ((left pathname) (right pathname))
    "This is kind of a kludge.  Should know what platform the pathnames are on."
    (equalp left right))
  )

(defmacro define-objects-equalp-type-mismatch-methods (type &optional doc
                                                       &environment env)
  "Defines methods on OBJECTS-EQUALP which return NIL if an object of type TYPE
   is compared with some arbitrary object."
  (check-type type symbol)
  (assert (find-class type nil env))
  `(PROGN
     (DEFMETHOD OBJECTS-EQUALP ( (OBJECT1 ,type) (OBJECT2 t)     ) ,doc NIL)
     (DEFMETHOD OBJECTS-EQUALP ( (OBJECT2 t)     (OBJECT1 ,type) ) ,doc NIL) ))

(define-objects-equalp-type-mismatch-methods cons
    "Conses are never equal to anything but other conses.")
(define-objects-equalp-type-mismatch-methods number
    "Numbers are never equal to anything but numbers.")
(define-objects-equalp-type-mismatch-methods string
  "Strings are never equal to anything but other strings.")
(define-objects-equalp-type-mismatch-methods symbol
    "Symbols are never equal to anything but symbols, and the
     eq test in the :around method will catch that case.")
(define-objects-equalp-type-mismatch-methods vector
    "Vectors are never equal to anything but vectors.")

(defmethod objects-equalp :around (left right)
  "First check to see if the objects are eql.  If so, no need to go further.
   Otherwise, look to see if we're recursively testing for equality on these
   some objects.  If so, the objects are equal.
   Finally, to avoid recursion death, we push the objects on the circularity
   stack so we won't test them again."
  (let (last-time)
    (cond ((eql left right) t)
          ((and (setq last-time (assoc left utility::*objects-equalp-circularity-stack* :test #'eql))
                (eql right (cdr last-time)))
           ;; Recursive loop.  We will typically get here because some
           ;; method for an aggregate object recurses on components of
           ;; that aggregate.  Return T here.  If the objects being
           ;; compared arn't really OBJECTS-EQUALP then some other
           ;; condition in that other method will catch it.
           t)
          (t (let* ((new-entry (cons left right))
                    (new-stack (cons new-entry utility::*objects-equalp-circularity-stack*)))
               ; (declare (dynamic-extent new-entry new-stack))
               (let ((utility::*objects-equalp-circularity-stack* new-stack))
                 (call-next-method)))))))

(defmacro pushlast (item location)
  "Push an element onto the last location in a list.
   Traverses the list twice and makes one copy, so
   it is a tad expensive, but it is what you want
   to use for maintaining versioned lists of items."
  `(SETF ,location
         (NREVERSE (CONS ,item (REVERSE ,location)))))

(defun false (&rest whatever)
  (declare (ignore whatever))
  nil)

;; Interactive use only.
(defun exit ()
  "Quit lisp."
  #+lispworks (lw:quit))

(defun quit ()
  "Quit lisp."
  #+lispworks (lw:quit))

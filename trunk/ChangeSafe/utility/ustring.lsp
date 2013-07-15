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
            ascii->ucs-2
            ustring?
            ucs-2->ascii
            utf-8-output-stream
            utf-8->ucs-2
            )))

;;; standard-char:  one of the 96 defined characters
;;;
;;; base-char:  upgraded array element type of standard char
;;;             so will hold more than 96 values
;;;
;;; extended-char:  any character that is not a base-char
;;;
;;; So base-char and extended-char form a complete partition of character.
;;;
;;;
;;; In lispworks, a base-char has a code less than 256 (8-bit)
;;;  and an lw:simple-char has a code less than 65536 (16-bit)
;;;  (thus an lw:simple-char may be an extended-char)
;;;
;;;  8-bit-character:  #\e
;;;  16-bit-character:  #\Ideographic-Space
;;;                     #\U+0411  (Cyrillic capital letter be)
;;;  not a base char *or* a simple-char:  #\Hyper-Control-e
;;;
;;; In lispworks, a `base-string' holds 8-bit characters,
;;; a `lw:text-string' holds 16-bit characters

;;; Lw:text-string is UCS-2

(defun ucs-4->ucs-2 (ucs-4-series)
  "Series transducer that maps unicode extension characters
   into the unicode replacement character."
  (declare (optimizable-series-function)
           (type (series ucs-4-code) ucs-4-series))
  (map-fn 'ucs-2-code (lambda (code)
                        (declare (type ucs-4-code code))
                        (if (<= code #xFFFF)
                            code
                            #xFFFD)) ;; unicode replacement char
          ucs-4-series))

;;;; CLASS SPECIAL-UNIVERSAL-STRING
(defclass special-universal-string ()
  ((code-unit-vector :type vector
                     :documentation "A vector of code units for the string."
                     :initarg :code-unit-vector
                     :reader code-unit-vector)))

(deftype universal-string ()
  `(or simple-base-string
       #+lispworks lw:simple-text-string
       special-universal-string))

(defun ustring? (thing)
  (typep thing 'universal-string))

(defun ustring-code-unit (string index)
  "Returns the code-unit of STRING at INDEX."
  (declare #.(performance-optimizations))
  (check-type index array-index)
  (cond ((typep string 'simple-base-string)
         (char-code (the base-char
                      (#+lispworks lw:sbchar
                       #-lispworks schar string index))))
        #+lispworks
        ((typep string 'lw:simple-text-string)
         (char-code (the lw:simple-char (lw:stchar string index))))
        ((typep string 'special-universal-string)
         (elt (code-unit-vector string) index))
        (t (error "This isn't a string ~s" string))))

(defun usubstring-code-units (ustring start end)
  "Returns the substring of USTRING between the code-units START and END."
  (cond ((or (typep ustring 'simple-base-string)
             #+lispworks (typep ustring 'lw:simple-text-string))
         (subseq ustring start end))
        ((typep ustring 'special-universal-string)
         (make-instance (class-of ustring)
                        :code-unit-vector (subseq (code-unit-vector ustring) start end)))
        (t (error "Not a ustring ~s" ustring))))

(defgeneric render-ustring (ustring stream)
  (:documentation "Emit the universal string USTRING to STREAM.")
  (:method ((ustring simple-string) stream)
     (write-string ustring stream)))

(defun common-lisp-user::format-ustring (stream ustring &optional colonp atsignp)
  (check-type stream stream)
  (check-type ustring universal-string)
  (when colonp (error "FORMAT-USTRING directive does not take a colon."))
  (when atsignp (error "FORMAT-USTRING directive does not take an atsign."))
  (render-ustring ustring stream))

(defgeneric ustring-append-binary (left right)
  (:documentation "Concatenate two ustrings.")
  (:method ((left simple-string) (right simple-string))
     (lw:string-append left right))
  (:method ((left special-universal-string) (right special-universal-string))
     (if (eq (class-of left) (class-of right))
         (make-instance (class-of left)
                        :code-unit-vector (concatenate 'vector
                                                       (code-unit-vector left)
                                                       (code-unit-vector right)))
         (error "Incompatible."))))

(defun ustring-append (ustring0 &rest ustrings)
  (reduce #'ustring-append-binary ustrings :initial-value ustring0))

(defun ustring-join (joiner ustring0 &rest ustrings)
  (reduce (lambda (left right)
            (ustring-append left joiner right))
          ustrings :initial-value ustring0))

(defgeneric ustring-code-unit-count (ustring)
  (:documentation "Returns the number of code-units in USTRING.")
  (:method ((ustring simple-string))
     (simple-string-length ustring))
  (:method ((ustring special-universal-string))
     (vector-length (code-unit-vector ustring))))

(defgeneric ustring-code-point-count (ustring)
  (:documentation "Returns the number of code-points in USTRING.")
  (:method ((ustring simple-string))
     (simple-string-length ustring)))

;; This may be more efficient for coded ustrings.
(defgeneric ustring-code-point-count<= (ustring number)
  (:documentation
   #.(concatenate 'string
                  "True iff number of code points in USTRING is less than or"
                  " equal to NUMBER."))
  (:method ((ustring simple-string) number)
    (<= (simple-string-length ustring) number)))

(defgeneric usubstring (ustring start limit)
  (:documentation
   #.(concatenate 'string
                  "Substring of ustring between start and limit.  This is in"
                  " code points, not code units, but you still have to deal"
                  " with combining characters."))
  (:method ((ustring simple-string) start limit)
    (subseq ustring start limit)))

(defmethod print-object ((object special-universal-string) stream)
  (print-unreadable-object (object stream :type t :identity t)
    ;(write-char #\U+201C stream)  ;; open double quote
    (write-char #\U+0022 stream) ;; normal double quote
    (if (or (null *print-length*)
            (ustring-code-point-count<= object *print-length*))
        (render-ustring object stream)
        (progn (render-ustring (usubstring object 0 *print-length*) stream)
               ;(write-char #\U+2026 stream) ;; horizontal ellipsis
               (write-string "..." stream)
               ))
    ;(write-char #\U+201D stream)  ;; close double quote
    (write-char #\U+0022 stream) ;; normal double quote
    ))

(defun scan-code-units (universal-string)
  (declare (optimizable-series-function))
  (map-fn 'fixnum
          (lambda (thing)
            (if (characterp thing)
                (char-code (the character thing))
                thing))
          (scan 'vector
                (if (typep universal-string 'special-universal-string)
                    (code-unit-vector universal-string)
                    universal-string))))

;;;; CLASS ASCII STRING
;;; An ascii string is a series of ascii-code-points (integer 0 127)

(defclass ascii-string (special-universal-string) ())

(defun scan-ascii-string (ascii-string)
  (declare (optimizable-series-function))
  (scan '(vector ascii-code) (code-unit-vector ascii-string)))

(defun ascii->ucs-2 (string &key substitute-char)
  (declare (ignore substitute-char))
  (collect 'string (map-fn 'character #'code-char (scan-ascii-string string))))

(defun ucs-2->ascii (string &key substitute-char)
  (check-type string simple-string)
  (make-instance 'ascii-string
                 :code-unit-vector
                 (collect '(vector ascii-code)
                          (map-fn 'ascii-code
                                  (lambda (code)
                                    (cond ((<= 0 code 127) code)
                                          ((null substitute-char) (error "Illegal character."))
                                          (t substitute-char)))
                                  (#m char-code (scan 'string string))))))

;;; Because ascii is a subset of UCS-2, we can use CODE-CHAR
;;; and WRITE-CHAR to render the code points.
(defmethod render-ustring ((ustring ascii-string) stream)
  (collect-stream stream
                  (#m code-char (scan-ascii-string ustring))
                  #'write-char))

;;; One code point per code unit.
(defmethod ustring-code-point-count ((ustring ascii-string))
  (vector-length (code-unit-vector ustring)))

(defmethod ustring-code-point-count<= ((ustring ascii-string) number)
  (<= (vector-length (code-unit-vector ustring)) number))

(defmethod usubstring ((ustring ascii-string) start limit)
  (make-instance 'ascii-string
                 :code-unit-vector (subseq (code-unit-vector ustring)
                                           start limit)))

;;;; CLASS ISO-8859-1-STRING
;;; An ISO-8859-1-STRING is a series of 8-bit octets.
(defclass iso-8859-1-string (special-universal-string) ())

(defun scan-iso-8859-1-string (iso-8859-1-string)
  (declare (optimizable-series-function))
  (scan '(vector (integer 0 255)) (code-unit-vector iso-8859-1-string)))

(defmethod render-ustring ((ustring iso-8859-1-string) stream)
  (collect-stream stream
                  (#m code-char (scan-iso-8859-1-string ustring))
                  #'write-char))

;;; One code point per code unit.
(defmethod ustring-code-point-count ((ustring iso-8859-1-string))
  (vector-length (code-unit-vector ustring)))

(defmethod ustring-code-point-count<= ((ustring iso-8859-1-string) number)
  (<= (vector-length (code-unit-vector ustring)) number))

(defmethod usubstring ((ustring iso-8859-1-string) start limit)
  (make-instance 'iso-8859-1-string
                 :code-unit-vector (subseq (code-unit-vector ustring)
                                           start limit)))

;;;; CLASS UTF-8-STRING
;;; A UTF-8 string is a series of 8-bit octets in which UCS-4 (31-bit
;;; values) or UTF-32 (unicode values) are encoded.

(defclass utf-8-string (special-universal-string) ())

(defun utf-8->ucs-4 (utf-8-octets)
  "A transducer that takes utf-8 octets and returns UCS-4 code points."
  (declare (optimizable-series-function)
           (off-line-port utf-8-octets))
  (choose-if #'plusp
    (decode-series
     utf-8-octets
     :padding -1
     :decoder (lambda (c0 c1 c2 c3 c4 c5)
                (cond ((<= c0 #b01111111) (values c0 0))
                      ((<= c0 #b10111111) (error "Unexpected multibyte code."))
                      ((<= c0 #b11011111) (if (<= #b10000000 c1 #b10111111)
                                              (let ((result (logior (ash (logand #b00011111 c0) (* 6 1))
                                                                    (ash (logand #b00111111 c1) (* 6 0)))))
                                                (values result 1))
                                              (error "Illegal multibyte code.")))
                      ((<= c0 #b11101111) (if (and (<= #b10000000 c1 #b10111111)
                                                   (<= #b10000000 c2 #b10111111))
                                              (let ((result (logior (ash (logand #b00001111 c0) (* 6 2))
                                                                    (ash (logand #b00111111 c1) (* 6 1))
                                                                    (ash (logand #b00111111 c2) (* 6 0)))))
                                                (values result 2))
                                              (error "Illegal multibyte code.")))
                      ((<= c0 #b11110111) (if (and (<= #b10000000 c1 #b10111111)
                                                   (<= #b10000000 c2 #b10111111)
                                                   (<= #b10000000 c3 #b10111111))
                                              (let ((result (logior (ash (logand #b00000111 c0) (* 6 3))
                                                                    (ash (logand #b00111111 c1) (* 6 2))
                                                                    (ash (logand #b00111111 c2) (* 6 1))
                                                                    (ash (logand #b00111111 c3) (* 6 0)))))
                                                (values result 3))
                                              (error "Illegal multibyte code.")))
                      ((<= c0 #b11111011) (if (and (<= #b10000000 c1 #b10111111)
                                                   (<= #b10000000 c2 #b10111111)
                                                   (<= #b10000000 c3 #b10111111)
                                                   (<= #b10000000 c4 #b10111111))
                                              (let ((result (logior (ash (logand #b00000011 c0) (* 6 4))
                                                                    (ash (logand #b00111111 c1) (* 6 3))
                                                                    (ash (logand #b00111111 c2) (* 6 2))
                                                                    (ash (logand #b00111111 c3) (* 6 1))
                                                                    (ash (logand #b00111111 c4) (* 6 0)))))
                                                (values result 4))
                                              (error "Illegal multibyte code.")))
                      ((<= c0 #b11111101) (if (and (<= #b10000000 c1 #b10111111)
                                                   (<= #b10000000 c2 #b10111111)
                                                   (<= #b10000000 c3 #b10111111)
                                                   (<= #b10000000 c4 #b10111111)
                                                   (<= #b10000000 c5 #b10111111))
                                              (let ((result (logior (ash (logand #b00000001 c0) (* 6 5))
                                                                    (ash (logand #b00111111 c1) (* 6 4))
                                                                    (ash (logand #b00111111 c2) (* 6 3))
                                                                    (ash (logand #b00111111 c3) (* 6 2))
                                                                    (ash (logand #b00111111 c4) (* 6 1))
                                                                    (ash (logand #b00111111 c5) (* 6 0)))))
                                                (values result 5))
                                              (error "Illegal multibyte code.")))
                      (t (error "Illegal UTF-8 code.")))))))

(defun ucs-4->utf-8 (ucs-4-series)
  "A series transducer that converts UCS-4 elements (31-bit unsigned values
   as per ISO 10646) into UTF-8 elements (variable byte)."
  (declare (optimizable-series-function)
           (off-line-port ucs-4-series)
           (off-line-port 0))
  (producing (utf-8-out) ((ucs-4-in ucs-4-series)
                          (current-code-point 0)
                          (utf-8-byte-0 0)
                          (utf-8-byte-1 0)
                          (utf-8-byte-2 0)
                          (utf-8-byte-3 0)
                          (utf-8-byte-4 0)
                          (utf-8-byte-5 0)
                          (bytes-to-emit 0))
    (declare (type (integer 0 #x8FFFFFFF) current-code-point)
             (type (integer 0 6) bytes-to-emit)
             (type (unsigned-byte 8)
                   utf-8-byte-0
                   utf-8-byte-1
                   utf-8-byte-2
                   utf-8-byte-3
                   utf-8-byte-4
                   utf-8-byte-5))
    (loop
      (tagbody
       (setq current-code-point (next-in ucs-4-in (terminate-producing)))
       (cond ((< current-code-point #x80)
              (setq bytes-to-emit 1
                    utf-8-byte-0 (the (integer 0 (#x80)) current-code-point)))
             ((< current-code-point #x800)
              (setq bytes-to-emit 2
                    utf-8-byte-1 (logior #b11000000
                                         (ldb (byte 5 (* 6 1))
                                              (the (integer #x80 (#x800)) current-code-point)))
                    utf-8-byte-0 (logior #b10000000
                                         (ldb (byte 6 (* 6 0))
                                              (the (integer #x80 (#x800)) current-code-point)))))

             ((or (<= #x0000D800 current-code-point #x0000DFFF)
                  (= current-code-point #x0000FFFE)
                  (= current-code-point #x0000FFFF))
              (error "Illegal UCS-4 code #x~8,0x" current-code-point))

             ((< current-code-point #x00010000)
              (setq bytes-to-emit 3
                    utf-8-byte-2 (logior #b11100000
                                         (ldb (byte 4 (* 6 2))
                                              (the (integer #x800 (#x10000)) current-code-point)))
                    utf-8-byte-1 (logior #b10000000
                                         (ldb (byte 6 (* 6 1))
                                              (the (integer #x800 (#x10000)) current-code-point)))
                    utf-8-byte-0 (logior #b10000000
                                         (ldb (byte 6 (* 6 0))
                                              (the (integer #x800 (#x10000)) current-code-point)))))

             ((< current-code-point #x00200000)
              (when (= (ldb (byte 15 1) current-code-point) #x7FFF)
                (error "Illegal UCS-4 character #x~8,'0x" current-code-point))

              (setq bytes-to-emit 4
                    utf-8-byte-3 (logior #b11110000
                                         (ldb (byte 3 (* 6 3))
                                              (the (integer #x10000 (#x200000)) current-code-point)))
                    utf-8-byte-2 (logior #b10000000
                                         (ldb (byte 6 (* 6 2))
                                              (the (integer #x10000 (#x200000)) current-code-point)))
                    utf-8-byte-1 (logior #b10000000
                                         (ldb (byte 6 (* 6 1))
                                              (the (integer #x10000 (#x200000)) current-code-point)))
                    utf-8-byte-0 (logior #b10000000
                                         (ldb (byte 6 (* 6 0))
                                              (the (integer #x10000 (#x200000)) current-code-point)))))

             ((< current-code-point #x04000000)
              (setq bytes-to-emit 5
                    utf-8-byte-4 (logior #b11111000
                                         (ldb (byte 2 (* 6 4))
                                              (the (integer #x200000 (#x4000000)) current-code-point)))
                    utf-8-byte-3 (logior #b10000000
                                         (ldb (byte 6 (* 6 3))
                                              (the (integer #x200000 (#x4000000)) current-code-point)))
                    utf-8-byte-2 (logior #b10000000
                                         (ldb (byte 6 (* 6 2))
                                              (the (integer #x200000 (#x4000000)) current-code-point)))
                    utf-8-byte-1 (logior #b10000000
                                         (ldb (byte 6 (* 6 1))
                                              (the (integer #x200000 (#x4000000)) current-code-point)))
                    utf-8-byte-0 (logior #b10000000
                                         (ldb (byte 6 (* 6 0))
                                              (the (integer #x200000 (#x4000000)) current-code-point)))))
             ((< current-code-point #x80000000)
              (setq bytes-to-emit 6
                    utf-8-byte-5 (logior #b11111100
                                         (ldb (byte 1 (* 6 5)) current-code-point))
                    utf-8-byte-4 (logior #b10000000
                                         (ldb (byte 6 (* 6 4)) current-code-point))
                    utf-8-byte-3 (logior #b10000000
                                         (ldb (byte 6 (* 6 3)) current-code-point))
                    utf-8-byte-2 (logior #b10000000
                                         (ldb (byte 6 (* 6 2)) current-code-point))
                    utf-8-byte-1 (logior #b10000000
                                         (ldb (byte 6 (* 6 1)) current-code-point))
                    utf-8-byte-0 (logior #b10000000
                                         (ldb (byte 6 (* 6 0)) current-code-point))))

             (t (error "Illegal UCS-4 code point ~8,'0x" current-code-point)))

       ;; (format t "~&bytes to emit: ~d" bytes-to-emit)
       :emit-bytes
       (if (zerop bytes-to-emit) (go :next-loop))
       ;; (format t "~&emit.")
       (decf bytes-to-emit)
       (next-out utf-8-out (cond ((= bytes-to-emit 5) utf-8-byte-5)
                                 ((= bytes-to-emit 4) utf-8-byte-4)
                                 ((= bytes-to-emit 3) utf-8-byte-3)
                                 ((= bytes-to-emit 2) utf-8-byte-2)
                                 ((= bytes-to-emit 1) utf-8-byte-1)
                                 ((= bytes-to-emit 0) utf-8-byte-0)
                                 (t (error "Too many bytes to emit: ~d." bytes-to-emit))))
       (go :emit-bytes)

       :next-loop))))

(defun utf-8->ucs-2 (ustring)
  "Convert a ustring to a lw:text-string."
  (collect 'string
           (#m code-char
               (ucs-4->ucs-2
                (utf-8->ucs-4
                 (scan 'simple-vector-8b
                       (code-unit-vector ustring)))))))

(defmethod render-ustring ((ustring utf-8-string) stream)
  (collect-stream
   stream
   (#m code-char
       (ucs-4->ucs-2
        (utf-8->ucs-4
         (scan 'simple-vector-8b
               (code-unit-vector ustring)))))
   #'write-char))

(defun utf-8-sample ()
  (make-instance 'utf-8-string
                 :code-unit-vector
                 #(#xE6 #x88 #x91 #xE8 #x83 #xBD #xE5 #x90 #x9E #xE4 #xB8 #x8B #xE7 #x8E #xBB #xE7
                        #x92 #x83 #xE8 #x80 #x8C #xE4 #xB8 #x8D #xE4 #xBC #xA4 #xE8 #xBA #xAB #xE4 #xBD
                        #x93 #xE3 #x80 #x82 #x20 #x20
                        #xCE #x9C #xCF #x80 #xCE #xBF #xCF #x81 #xCF
                              #x8E #x20 #xCE #xBD #xCE #xB1 #x20 #xCF #x86 #xCE #xAC #xCF #x89 #x20 #xCF #x83
                              #xCF #x80 #xCE #xB1 #xCF #x83 #xCE #xBC #xCE #xAD #xCE #xBD #xCE #xB1 #x20 #xCE
                              #xB3 #xCF #x85 #xCE #xB1 #xCE #xBB #xCE #xB9 #xCE #xAC #x20 #xCF #x87 #xCF #x89
                              #xCF #x81 #xCE #xAF #xCF #x82 #x20 #xCE #xBD #xCE #xB1 #x20 #xCF #x80 #xCE #xAC
                              #xCE #xB8 #xCF #x89 #x20 #xCF #x84 #xCE #xAF #xCF #x80 #xCE #xBF #xCF #x84 #xCE
                              #xB1
                        #xE7 #xA7 #x81 #xE3 #x81 #xAF #xE3
                        #x82 #xAC #xE3 #x83 #xA9 #xE3 #x82 #xB9 #xE3 #x82 #x92 #xE9 #xA3 #x9F #xE3 #x81
                        #xB9 #xE3 #x82 #x89 #xE3 #x82 #x8C #xE3 #x81 #xBE #xE3 #x81 #x99 #xE3 #x80 #x82
                        #xE3 #x81 #x9D #xE3 #x82 #x8C #xE3 #x81 #xAF #xE7 #xA7 #x81 #xE3 #x82 #x92 #xE5
                        #x82 #xB7 #xE3 #x81 #xA4 #xE3 #x81 #x91 #xE3 #x81 #xBE #xE3 #x81 #x9B #xE3 #x82
                        #x93 #xE3 #x80 #x82)))

;; These could be done much more efficiently.
(defmethod ustring-code-point-count ((ustring utf-8-string))
  (collect-length (utf-8->ucs-4 (scan 'vector (code-unit-vector ustring)))))

(defmethod ustring-code-point-count<= ((ustring utf-8-string) number)
  (<= (ustring-code-point-count ustring) number))

(defmethod usubstring ((ustring utf-8-string) start limit)
  (make-instance 'utf-8-string
                 :code-unit-vector
                 (collect 'simple-vector-8b
                          (ucs-4->utf-8
                           (subseries (utf-8->ucs-4 (scan 'vector (code-unit-vector ustring))) start limit)))))

;;; UTF-8 stream

(defclass utf-8-stream ()
  ((underlying-stream :initarg :underlying-stream
                      :accessor stream/underlying-stream)))

(defclass utf-8-output-stream (utf-8-stream
                               stream:fundamental-character-output-stream)
  ())

(defmethod stream:stream-write-char ((stream utf-8-output-stream) char)
  (collect-stream
   (stream/underlying-stream stream)
   (ucs-4->utf-8 (singleton-series (char-code char)))
   #'write-byte))

(defmethod stream:stream-line-column ((stream utf-8-output-stream))
  nil)

;;; String Matching

(defun match-code-unit-predicate (predicate)
  (lambda (success fail)
    (lambda (string start end)
      (if (and (< start end)
               (funcall predicate (ustring-code-unit string start)))
          (funcall success string start (1+ start) end)
          (funcall fail)))))

(defun match-anything ()
  (lambda (success fail)
    (lambda (string start end)
      (if (< start end)
          (funcall success string start (1+ start) end)
          (funcall fail)))))

(defun match-search (match-one)
  (named-lambda search-loop (success fail)
                (lambda (string start end)
                  (if (< start end)
                      (funcall
                       (funcall match-one
                                success
                                (lambda () (funcall (search-loop success fail)
                                                    string
                                                    (1+ start) end)))
                       string start end)
                      (funcall fail)))))

(defun match-sequence (match-first &rest other-matchers)
  (lambda (success fail)
    (funcall match-first
             (if (null other-matchers)
                 success
                 (lambda (string start first-match-end limit)
                   (funcall
                    (funcall
                     (apply #'match-sequence other-matchers)
                     (lambda (string final-match-start final-match-end limit)
                       (declare (ignore final-match-start))
                       (funcall success string start final-match-end limit))
                     fail)
                    string first-match-end limit)))
             fail)))

(defun match-star (match-one)
  (named-lambda star-loop (success fail)
            (declare (ignore fail)) ;; Kleene * always matches
            (lambda (string start end)
             (funcall
              (funcall match-one
                       (lambda (string this-start this-end limit)
                         (funcall (star-loop
                                   (lambda (string rest-start rest-end limit)
                                     (declare (ignore rest-start))
                                     (funcall success string this-start rest-end limit))
                                   (lambda ()
                                     (funcall success string start this-start limit)))
                                  string
                                  this-end
                                  end))
                       (lambda ()
                         (funcall success string start start end)))
                      string start end))))

(defun match-split (pattern)
  (named-lambda match-split-loop (string start end)
                (funcall (funcall (match-search pattern)
                                  (lambda (string match-begin match-end limit)
                                    (cons (usubstring-code-units string start match-begin)
                                          (match-split-loop string match-end limit)))
                                  (lambda () (list (usubstring-code-units string start end))))
                         string start end)))

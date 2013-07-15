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

;;; This file defines a regression test suite
(eval-when (:load-toplevel :execute)
  (make-instance 'regression-test-suite
                 :name "UTILITY"
                 :directory (translate-logical-pathname "CSF:UTILITY;")))

(define-test-input-file (make-pathname :name "__abc" :type "txt" :defaults "")
  :default-ascii '(#\a :linebreak #\b :linebreak #\c :linebreak))
(define-test-input-file (make-pathname :name "__test-w-nl" :type "txt" :defaults "")
  :default-ascii '(#\a :linebreak #\b :linebreak #\c :linebreak))
(define-test-input-file (make-pathname :name "__test-wo-nl" :type "txt" :defaults "")
  :default-ascii '(#\a :linebreak #\b :linebreak #\c))
(define-test-input-file (make-pathname :name "__test1-w-nl" :type "txt" :defaults "")
  :default-ascii '(#\a :linebreak))
(define-test-input-file (make-pathname :name "__test1-wo-nl" :type "txt" :defaults "")
  :default-ascii '(#\a))

(eval-when (:load-toplevel :execute)
  (regression-test-suite/add-initialization "UTILITY" 'generate-test-input-files))

(define-regression-test test-base-64-strings ()
  (dolist (s '("xyzzy" "abc" "abcd" "abcde" "abcdef" "abcdefg" "abcdefgh"))
    (verify (verify-one-return-value
             (verify-value-string= s))
            (base64-decode-string-to-string (base64-encode-string s)))))

(define-regression-test test-rbtree ()
  (let ((tree (make-tree #'string-equal #'string-lessp))
        (words (mapcar #'string (remove-duplicates
                                 '(But deep conceptual shifts within twentieth century
                                   science have undermined this Cartesian Newtonian
                                   metaphysics  revisionist studies in the history
                                   and philosophy of science have cast further doubt on
                                   its credibility  and most recently feminist and
                                   poststructuralist critiques have demystified the
                                   substantive content of mainstream Western scientific
                                   practice revealing the ideology of domination
                                   concealed behind the facade of objectivity It
                                   has thus become increasingly apparent that physical
                                   reality no less than social reality is at
                                   bottom a social and linguistic construct that
                                   scientific knowledge far from being objective
                                   reflects and encodes the dominant ideologies and
                                   power relations of the culture that produced it
                                   that the truth claims of science are inherently
                                   theory laden and self referential and consequently
                                   that the discourse of the scientific community for
                                   all its undeniable value cannot assert a privileged
                                   epistemological status with respect to
                                   counter hegemonic narratives emanating from
                                   dissident or marginalized communities These themes
                                   can be traced despite some differences of emphasis
                                   in Aronowitzs analysis of the cultural fabric that
                                   produced quantum mechanics in Ross discussion
                                   of oppositional discourses in post quantum science
                                   in Irigarays and Hayles exegeses of gender
                                   encoding in fluid mechanics and in Hardings
                                   comprehensive critique of the gender ideology
                                   underlying the natural sciences in general and
                                   physics in particular

                                   It is in Einsteins general theory of relativity that
                                   the radical conceptual break occurs the space time
                                   geometry becomes contingent and dynamical encoding
                                   in itself the gravitational field Mathematically
                                   Einstein breaks with the tradition dating back to
                                   Euclid and which is inflicted on high-school
                                   students even today and employs instead the
                                   non Euclidean geometry developed by
                                   Riemann Einsteins equations are highly nonlinear
                                   which is why traditionally trained mathematicians
                                   find them so difficult to solve Newtons
                                   gravitational theory corresponds to the crude and
                                   conceptually misleading truncation of Einsteins
                                   equations in which the nonlinearity is simply
                                   ignored Einsteins general relativity therefore
                                   subsumes all the putative successes of Newtons
                                   theory while going beyond Newton to predict
                                   radically new phenomena that arise directly from the
                                   nonlinearity the bending of starlight by the sun
                                   the precession of the perihelion of Mercury and the
                                   gravitational collapse of stars into black holes
                                   )))))
    (labels ((populate (list count)
                    (if (null list)
                        '()
                      (progn (rb-tree/insert! tree (car list) count)
                             (populate (cdr list) (1+ count)))))
                  (test (list count)
                    (if (null list)
                        t
                      (progn
                        (verify (verify-one-return-value
                                 (verify-value-eql count))
                                (rb-tree/lookup tree (car list) nil))
                        (test (cdr list) (1+ count))))))
      (populate words 0)
      (test words 0))))

(defun test-rbtree-permutations-1 (permuted-list)
  (let ((tree (make-tree #'= #'<)))
    (dolist (element permuted-list)
      (rb-tree/insert! tree element element))
    (named-let loup ((answer '()))
               (let ((element (rb-tree/pick! tree)))
                 (if (null element)
                     (nreverse answer)
                     (loup (cons element answer)))))))

(define-regression-test test-rbtree-permutations ()
  (dotimes (i 2)
    (let ((answer (collect 'list (scan-range :from 0 :below (+ i 7)))))
      (dolist (perm (permutations answer))
        (verify (verify-one-return-value
                 (verify-value-equal answer))
                (test-rbtree-permutations-1 perm))))))

(defun test-integer-rbtree-permutations-1 (permuted-list)
  (let ((tree (make-integer-tree)))
    (dolist (element permuted-list)
      (integer-rb-tree/insert! tree element element))
    (named-let loup ((answer '()))
               (let ((element (integer-rb-tree/pick! tree)))
                 (if (null element)
                     (nreverse answer)
                     (loup (cons element answer)))))))

(define-regression-test test-integer-rbtree-permutations ()
  (dotimes (i 2)
    (let ((answer (collect 'list (scan-range :from 0 :below (+ i 7)))))
      (dolist (perm (permutations answer))
        (verify (verify-one-return-value
                 (verify-value-equal answer))
                (test-integer-rbtree-permutations-1 perm))))))

(define-regression-test test-integer-rbtree ()
  (let ((tree (make-integer-tree)))
    (dotimes (i 5000)
      (integer-rb-tree/insert! tree (* i i) i))
    (dotimes (i 5000)
      (verify (verify-one-return-value
               (verify-value-eql i))
              (integer-rb-tree/lookup tree (* i i) nil)))))

(define-regression-test test-osets ()
  (verify (verify-one-return-value
           (verify-value-eql t))
          (let ((oset1 (make-ordered-set #'string-equal #'string-lessp))
                (oset2 (make-ordered-set #'string-equal #'string-lessp)))
            (do-symbols (sym)
              (let ((name (symbol-name sym)))
                (ordered-set/adjoin! oset1 name)
                (ordered-set/adjoin! oset2 (reverse name))
                (when (> (length name) 5)
                  (ordered-set/adjoin! oset2 (reverse (subseq (reverse name) 0 5))))
                (when (> (length name) 10)
                  (ordered-set/adjoin! oset1 (reverse (subseq (reverse name) 0 10))))))
            (ordered-set/union! oset1 oset2)
            (collect-length (ordered-set/scan oset1))
            t)))

(define-regression-test test-integer-osets ()
  (let ((oset1 (make-ordered-integer-set))
        (oset2 (make-ordered-integer-set)))
    (dotimes (i 5000)
      (ordered-integer-set/adjoin! oset1 (* i 2))
      (ordered-integer-set/adjoin! oset2 (1+ (* i 2))))
    (ordered-integer-set/union! oset1 oset2)
    (dotimes (i 10000)
      (verify
       (verify-one-return-value
        (verify-value-eql t))
       (ordered-integer-set/member? oset1 i)))))

(define-regression-test test-namemap ()
  "Make sure that filename encoding is bijective by applying it to some
   nasty examples."
  (iterate ((name #z("foo"
                     "Bar"
                     "BAZ"
                     "foo-bar"
                     "foo-"
                     "-bar"
                     "FooBar"
                     "23Skiddoo"
                     "this.is.a.~test"
                     "QQ_QQU_LPAR"
                     "(*&#$)(*@#$"
                     "ACM Ada Letters XIII, 5 (Sep-Oct 1993), 43-47.htm"
                     "__foo.txt"
                     "__TEST-9A.ZIP"
                     "ui_cfg_str"
                     "ui_cfg_*"
                     )))
    (verify-assertion
        (not (null (string= (decode-namestring (encode-namestring name)) name)))))
  (let ((nasties '(#\space #\- #\Q #\q #\u #\U #\c #\C)))
    (dolist (i nasties)
      (dolist (j nasties)
        (dolist (k nasties)
          (dolist (l nasties)
            (let ((nasty-string (coerce (list i j k l) 'string)))
              (verify-assertion
               (not (null (string= (decode-namestring (encode-namestring nasty-string)) nasty-string)))))))))))

(define-regression-test test-burst-directories ()
  (verify (verify-one-return-value
           (verify-value-equal '("foo" "bar")))
          (burst-directories "/foo/bar/baz" '(#\/) :start 0 :end 8))
  (verify (verify-one-return-value
           (verify-value-equal '("foo" "bar")))
          (burst-directories "/foo/bar/baz" '(#\/) :start 0 :end 9))
  (verify (verify-one-return-value
           (verify-value-equal '("foo" "bar" "b")))
          (burst-directories "/foo/bar/baz" '(#\/) :start 0 :end 10))
  (verify (verify-one-return-value
           (verify-value-equal '("foo" "bar" "ba")))
          (burst-directories "/foo/bar/baz" '(#\/) :start 0 :end 11))
  (verify (verify-one-return-value
           (verify-value-equal '("foo" "bar" "baz")))
          (burst-directories "/foo/bar/baz" '(#\/) :start 0 :end 12))
  (verify (verify-one-return-value
           (verify-value-equal '("foo" "bar" "baz")))
          (burst-directories "/foo//bar/baz" '(#\/) :start 0 :end 13))
  (verify (verify-one-return-value
           (verify-value-equal '(:back :back "foo" "bar")))
          (burst-directories "../../foo/bar" '(#\/) :start 0 :end 13))
  (verify (verify-one-return-value
           (verify-value-equal '("foo" "baz")))
          (burst-directories "foo/bar/../baz" '(#\/) :start 0 :end 14))
  (verify (verify-one-return-value
           (verify-value-equal '("23456" "8901234" "567" "9012")))
          (burst-directories "0123456/8901234/567/90123456" '(#\/)
                             :start 2 :end 24))
  (verify (verify-one-return-value
           (verify-value-equal '("23456" "8901234" "567")))
          (burst-directories "0123456/8901234/567/90123456" '(#\/)
                              :start 2 :end 19)))

(define-regression-test test-pathname-parsing ()
  (verify (verify-one-return-value
           (verify-value-equal #p"\\foo\\bar\\baz.bam"))
          (parse-delimited-pathname "/foo/bar/baz.bam" '(#\/) 0 nil nil nil))
  (verify (verify-one-return-value
           (verify-value-equal #p"\\foo\\bar\\baz.bam\\"))
          (parse-delimited-pathname "/foo/bar/baz.bam" '(#\/) 0 nil nil t))
  (verify (verify-multiple-values 2
           (verify-value-eql nil)
           (verify-value-eql nil))
          (parse-filename.type "" :dot-dot-special-p t))
  (verify (verify-multiple-values 2
           (verify-value-equal "c")
           (verify-value-eql nil))
          (parse-filename.type "c" :dot-dot-special-p t))
  (verify (verify-multiple-values 2
           (verify-value-equal "foo")
           (verify-value-eql nil))
          (parse-filename.type "foo" :dot-dot-special-p t))
  (verify (verify-multiple-values 2
           (verify-value-equal ".")
           (verify-value-eql nil))
           (parse-filename.type "." :dot-dot-special-p t))
  (verify (verify-multiple-values 2
           (verify-value-equal "..")
           (verify-value-eql nil))
           (parse-filename.type ".." :dot-dot-special-p t))
  (verify (verify-multiple-values 2
           (verify-value-equal ".foo")
           (verify-value-eql nil))
           (parse-filename.type ".foo" :dot-dot-special-p t))
  (verify (verify-multiple-values 2
           (verify-value-equal "foo")
           (verify-value-equal ""))
           (parse-filename.type "foo." :dot-dot-special-p t))
  (verify (verify-multiple-values 2
           (verify-value-equal "foo")
           (verify-value-equal "bar"))
           (parse-filename.type "foo.bar" :dot-dot-special-p t))
  (verify (verify-multiple-values 2
           (verify-value-equal "foo.bar")
           (verify-value-equal "baz"))
           (parse-filename.type "foo.bar.baz" :dot-dot-special-p t))

  (dolist (test-info '(("c:\\foo\\bar\\bam.txt" :winnt
                        :host nil :device "c" :directory (:absolute "foo" "bar") :name "bam" :type "txt")
                       ("\\foo\\bar\\bam.txt" :winnt
                        :host nil :directory (:absolute "foo" "bar") :name "bam" :type "txt")
                       ("foo\\bar\\bam.txt" :winnt
                        :host nil :directory (:relative "foo" "bar") :name "bam" :type "txt")
                       (".\\foo\\bar.txt" :winnt
                        :host nil :directory (:relative "foo") :name "bar" :type "txt")
                       (".\\bar.txt" :winnt
                        :host nil :directory nil :name "bar" :type "txt")
                       ("bam.txt" :winnt
                        :host nil :directory nil :name "bam" :type "txt")
                       ("foo\\bar\\" :winnt
                        :host nil :directory (:relative "foo" "bar") :name nil :type nil)
                       ("/foo/bar/bam.txt" :unix
                        :host nil :directory (:absolute "foo" "bar") :name "bam" :type "txt")
                       ("foo/bar/bam.txt" :unix
                        :host nil :directory (:relative "foo" "bar") :name "bam" :type "txt")
                       ("./bar/bam.txt" :unix
                        :host nil :directory (:relative "bar") :name "bam" :type "txt")
                       ("./bam.txt" :unix
                        :host nil :directory nil :name "bam" :type "txt")
                       ("bam.txt" :unix
                        :host nil :directory nil :name "bam" :type "txt")
                       ("foo/bar/" :unix
                        :host nil :directory (:relative "foo" "bar") :name nil :type nil)
                       ("foo/bar" :unix
                        :host nil :directory (:relative "foo") :name "bar" :type nil)
                       ("/foo/bar/bam.txt" :winnt
                        :host nil :directory (:absolute "foo" "bar") :name "bam" :type "txt")
                       ("foo/bar/bam.txt" :winnt
                        :host nil :directory (:relative "foo" "bar") :name "bam" :type "txt")
                       ("./bar/bam.txt" :winnt
                        :host nil :directory (:relative "bar") :name "bam" :type "txt")
                       ("./bam.txt" :winnt
                        :host nil :directory nil :name "bam" :type "txt")
                       ("bam.txt" :winnt
                        :host nil :directory nil :name "bam" :type "txt")
                       ("foo/bar/" :winnt
                        :host nil :directory (:relative "foo" "bar") :name nil :type nil)
                       ("foo/bar" :winnt
                        :host nil :directory (:relative "foo") :name "bar" :type nil)
                       ("c:\\foo\\bar\\.csf" :winnt
                        :host nil :device "c" :directory (:absolute "foo" "bar") :name ".csf" :type nil)
                       ("/foo/bar/.csf" :unix
                        :host nil :directory (:absolute "foo" "bar") :name ".csf" :type nil)
                       ("/foo/bar/.csf" :winnt
                        :host nil :directory (:absolute "foo" "bar") :name ".csf" :type nil)))

    (destructuring-bind (string platform . pathname-args) test-info
        (verify (verify-one-return-value
                 (verify-value-equal (apply #'make-pathname :defaults "" pathname-args)))
                (parse-client-namestring string platform)))))

(define-regression-test test-pathname-utilities ()
  (verify (verify-one-return-value
           (verify-value-equal #p"baz\\quux.txt"))
          (enough-pathname #p"C:\\foo\\bar\\baz\\quux.txt" #p"C:\\foo\\bar\\"))
  (verify (verify-one-return-value
           (verify-value-equal #p"baz\\quux.txt"))
          (enough-pathname #p"\\foo\\bar\\baz\\quux.txt" #p"\\foo\\bar\\"))
  (verify (verify-one-return-value
           (verify-value-equal "file8.txt"))  ; NOT "./file8.txt"
          (namestring (enough-pathname #p"C:\\TEMP\\tvw9e767\\__akrondir\\__foo\\__src\\file8.txt"
                                       #p"C:\\TEMP\\tvw9e767\\__akrondir\\__foo\\__src\\")))
  (verify (verify-one-return-value
           (verify-value-equal t))
          (let ((file (translate-logical-pathname #P"CSF:UTILITY;TESTS.LSP"))
                (dir  (translate-logical-pathname #P"CSF:UTILITY;")))
            (equal (namestring (merge-pathnames (enough-pathname file dir) dir))
                   (namestring file)))))

(define-regression-test test-scan-file-records ()
  "Ensures that files can be read line-at-a-time."
  (verify (verify-one-return-value
           (verify-value-eql 4))
    (with-open-file (stream (ensure-test-input-file "__abc.txt")
                            :direction :input
                            :element-type '(unsigned-byte 8))
      (collect-length (scan-stream-records stream *ascii-linefeed*)))))

(define-regression-test test-edit-distance ()
  "Ensure edit distance function works."
  (mapc (lambda (example)
          (let ((seq-a (first example))
                (seq-b (second example))
                (expected-value (third example)))
          (verify (verify-one-return-value
                   (verify-value-eql expected-value))
            (edit-distance seq-a 0 (length seq-a)
                                        seq-b 0 (length seq-b)
                                        #'eql))
          (verify (verify-one-return-value
                   (verify-value-eql expected-value))
            (edit-distance seq-b 0 (length seq-b)
                                        seq-a 0 (length seq-a)
                                        #'eql))))
        '((#(a v e r y) #(g a r v e y) 3)
          (#(a)       #(a b c d)       3)
          (#(a b)     #(a b c d)       2)
          (#(a b c)   #(a b c d)       1)
          (#(a b c d) #(a b c d)       0)
          (#(b c d)   #(a b c d)       1)
          (#(c d)     #(a b c d)       2)
          (#(d)       #(a b c d)       3)
          (#(w)       #(a b c d)       5)
          (#(w x)     #(a b c d)       6)
          (#(w x y)   #(a b c d)       7)
          (#(w x y z) #(a b c d)       8)
          (#(w x y z) #(a b c)         7)
          (#(w x y z) #(a b)           6)
          (#(w x y z) #(a)             5)
          (#()        #()              0)
          (#()        #(a)             1)
          (#()        #(a b)           2)
          (#()        #(a b c)         3)
          (#(a p p r o p r i a t e m e a n i n g)
           #(a p p r o x i m a t e m a t c h i n g) 9)
          (#(a c g t a c g t a c g t)
           #(a c t a c c t a c c g t) 4)
          (#(a g c a c a c a) #(a c a c a c t a) 2)
          (#(i n d u s t r y)
           #(i n t r e s t) 7)
          (#(v i r g i n i a)
           #(v e r m o n t) 9)
          (#(a i m s) #(a m o s) 2)
          (#(h o u s e) #(c o w s) 5)
          (#(g r i n d) #(a g r e e d) 5)
          (#(a b c d e f g h i j k l) #(b c d e f f g h i x k l) 4)))
  ;; The indices get all funky when you don't start at zero,
  ;; so here's some nasty ones.
  (verify (verify-one-return-value
           (verify-value-eql 4))
    (edit-distance
     #(g r i n d a p p r o  p r     i   a t e m e a  n i n g a c)
     10 19
     #(c a c a c a a p      p r o x i m a t e m a    t c h i n g)
     8 19
     #'eql))
  (verify (verify-one-return-value
           (verify-value-eql 4))
    (edit-distance
     #(g r i n d a p p r o  p r     i   a t e m e a  n i n g a c)
     10 19
     #(p p r o x i m a t e m a    t c h i n g)
     1 12
     #'eql))
  (verify (verify-one-return-value
           (verify-value-eql 4))
    (edit-distance
     #(r o  p r     i   a t e m e a  n i n g a c)
     2 11
     #(c a c a c a a p      p r o x i m a t e m a    t c h i n g)
     8 19
     #'eql)))

(define-regression-test test-edit-path-midpoint ()
  (verify (verify-multiple-values
           3
           (verify-value-eql 8)
           (verify-value-eql 0)
           (verify-value-eql 4))
     (edit-path-midpoint #(a b c d) 0 4 #(w x y z) 0 4 #'eql))
  (verify (verify-multiple-values
           3
           (verify-value-eql 9)
           (verify-value-eql 15)
           (verify-value-eql 14))
     (edit-path-midpoint "appropriate meaning" 5 16
                         "approximate matching" 5 17
                                      #'char=))
  ;; make sure it's symmetric!
  (verify (verify-multiple-values
           3
           (verify-value-eql 9)
           (verify-value-eql 14)
           (verify-value-eql 15))
     (edit-path-midpoint "approximate matching" 5 17
                                      "appropriate meaning" 5 16
                                      #'char=))
  (verify (verify-multiple-values
           3
           (verify-value-eql 22)
           (verify-value-eql 5)
           (verify-value-eql 6))
     (edit-path-midpoint
      #(g r i n d a p p r o p r i a t e m e a n i n g a c) 0 25
      #(c a c a c a a p p r o x i m a t e m a t c h i n g) 0 25 #'eql))
  (flet ((champernowne (start end)
           (format nil "~{~d~}"
                   (collect 'list
                            (scan-range :from start :below end)))))

   (let ((c1 (champernowne 100 500))
         (c2 (champernowne 200 599)))

     (verify (verify-multiple-values
              3
              (verify-value-eql 429)
              (verify-value-eql 322)
              (verify-value-eql 321))
             (edit-path-midpoint c1 0 500 c2 0 599 #'char=))
     (verify (verify-multiple-values
              3
              (verify-value-eql 100)
              (verify-value-eql 242)
              (verify-value-eql 422))
             (edit-path-midpoint c1 200 280 c2 400 458 #'char=))
     (verify (verify-multiple-values
              3
              (verify-value-eql 100)
              (verify-value-eql 422)
              (verify-value-eql 242))
             (edit-path-midpoint c2 400 458 c1 200 280 #'char=)))))

(define-regression-test test-diff (&optional (size 10000)
                                             (ndels (floor size 10))
                                             (nins  (floor size 10))
                                             (maxlid (floor (* size 3/4))))

  (let* ((ion-list (collect 'list
                            (map-fn 'non-negative-integer
                                    (lambda (i)
                                      (declare (ignore i))
                                      (random maxlid))
                                    (scan-range :below size))))

         ;; Create a vector that points to each cons
         ;; in the ION list.
         (ion-vec (collect 'vector
                           (scan-sublists (cons 0 ion-list))))

         ;; Get ion list into vector before we edit it.
         (old-lid-vec (coerce ion-list 'vector)))

    ;; Simulate editing the file

    ;; Select a CONS at random and delete the tail
    (dotimes (i ndels)
      (do* ((which (random size) (random size))
            (cb (svref ion-vec which)
                (svref ion-vec which)))
          ((and (cdr cb)
                (cddr cb))
           (setf (cdr cb) (cddr cb)))))

    ;; (setf (second cb) nil))))

    ;; Select a CONS at random and insert a lid
    (dotimes (i nins)
      (let* ((which (random size))
             (cb  (svref ion-vec which)))
        (push (random maxlid) (cdr cb))))

    ;; (format t "~&~s ~& ~S" old-lid-vec (coerce (cdr (svref ion-vec 0)) 'vector))

    (let* ((new-lid-vec (coerce (cdr (svref ion-vec 0)) 'vector))

           (edits (edit-path old-lid-vec 0 (length old-lid-vec)
                             new-lid-vec 0 (length new-lid-vec)
                             #'=)))
      ;; now verify it worked
      (let ((total-del 0)
            (total-ins 0)
            (total-copy 0)
            (del-count 0)
            (ins-count 0)
            (copy-count 0)
            (oldidx 0)
            (newidx 0))
        (dolist (indel edits)
          (let* ((left-start (indel/left-subseq-start indel))
                 (left-limit (indel/left-subseq-limit indel))
                 (right-start (indel/right-subseq-start indel))
                 (right-limit (indel/right-subseq-limit indel))
                 (copy1 (- left-start oldidx))
                 (copy2 (- right-start newidx))
                 (deletions (- left-limit left-start))
                 (insertions (- right-limit right-start)))

            (verify-assertion (= copy1 copy2))

            ;; copy the unchanged elements
            (dotimes (i copy1)
              (verify-assertion (eql (svref old-lid-vec oldidx)
                                     (svref new-lid-vec newidx))))
            (incf oldidx copy1)
            (incf newidx copy2)
            (incf total-copy copy1)
            (unless (zerop copy1) (incf copy-count))

            ;; process deletions
            (incf oldidx deletions)
            (incf total-del deletions)
            (unless (zerop deletions) (incf del-count))
            (verify-assertion (<= oldidx (length old-lid-vec)))

            ;; process insertions
            (incf newidx insertions)
            (incf total-ins insertions)
            (unless (zerop insertions) (incf ins-count))
            (verify-assertion (<= newidx (length new-lid-vec)))))

        ;; process final copy
        (let ((copy1 (- (length old-lid-vec) oldidx))
              (copy2 (- (length new-lid-vec) newidx)))


          (verify-assertion (= copy1 copy2))
          (dotimes (i copy1)
            (verify-assertion (eql (svref old-lid-vec oldidx)
                                   (svref new-lid-vec newidx))))
          (incf oldidx copy1)
          (incf newidx copy2))

        #||                             ;
        (format t
        "~s in ~s (~s) del, ~s in ~s (~s) ins, ~s in ~s (~s) copy~
             ~&Edit path length: ~d"
        total-del del-count (average total-del del-count)
        total-ins ins-count (average total-ins ins-count)
        total-copy copy-count (average total-copy copy-count)
        (length edits))
        ||#
        ))))

(define-regression-test test-uri-merging ()
  (let ((base-uri (parse-uri (ucs-2->ascii "http://a/b/c/d;p?q"))))
    (make-keyword "G")
    (dolist (example '(("g:h" "g:h")
                       ("g"   "http://a/b/c/g")
                       ("./g" "http://a/b/c/g")
                       ("g/"  "http://a/b/c/g/")
                       ("/g"  "http://a/g")
                       ("//g" "http://g")
                       ("?y"  "http://a/b/c/?y")
                       ("g?y" "http://a/b/c/g?y")
                       ("#s"  "http://a/b/c/#s")
                       ("g#s" "http://a/b/c/g#s")
                       ("g?y#s"    "http://a/b/c/g?y#s")
                       (";x"       "http://a/b/c/;x")
                       ("g;x"      "http://a/b/c/g;x")
                       ("g;x?y#s"  "http://a/b/c/g;x?y#s")
                       ("."        "http://a/b/c/")
                       ("./"       "http://a/b/c/")
                       (".."       "http://a/b/")
                       ("../"      "http://a/b/")
                       ("../g"     "http://a/b/g")
                       ("../.."    "http://a/")
                       ("../../"   "http://a/")
                       ("../../g"  "http://a/g")
                       ;; Edge cases as per the RFC.
                       ("../../../g"     "http://a/../g")
                       ("../../../../g"  "http://a/../../g")
                       ("/./g"           "http://a/./g")
                       ("/../g"          "http://a/../g")
                       ("g."             "http://a/b/c/g.")
                       (".g"             "http://a/b/c/.g")
                       ("g.."            "http://a/b/c/g..")
                       ("..g"            "http://a/b/c/..g")
                       ("./../g"         "http://a/b/g")
                       ("./g/."          "http://a/b/c/g/") ;;??
                       ("g/./h"          "http://a/b/c/g/h")
                       ("g/../h"         "http://a/b/c/h")
                       ("g;x=1/./y"      "http://a/b/c/g;x%3D1/y")
                       ("g;x=1/../y"     "http://a/b/c/y")
                       ("g?y/./x"        "http://a/b/c/g?y%2F.%2Fx")
                       ("g?y/../x"       "http://a/b/c/g?y%2F..%2Fx")
                       ("g#s/./x"        "http://a/b/c/g#s/./x")
                       ("g#s/../x"       "http://a/b/c/g#s/../x")
                       ))
      (destructuring-bind (relative answer) example
        (verify
         (verify-one-return-value
          (verify-value-equal answer))
         (render-uri (merge-uris (parse-uri (ucs-2->ascii relative)) base-uri) nil))))))

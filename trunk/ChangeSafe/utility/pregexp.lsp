(in-package "CSF/UTILITY")

;pregexp.scm
;Portable regular expressions for Scheme
;Dorai Sitaram
;http://www.ccs.neu.edu/~dorai
;ds26 AT gte.com
;Oct 2, 1999
;;; ported by jrm.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(
            pregexp
            pregexp-match
            pregexp-match-positions
            pregexp-replace
            pregexp-replace*
            pregexp-split
            )))

(defparameter *pregexp-comment-char* #\;)

(defvar *pregexp-space-sensitive-p*)

(defun pregexp (s)
  (check-type s string)
  (let ((*pregexp-space-sensitive-p* t))
    `(:SUB ,(car (pregexp-read-pattern s 0 (length s))))))

(defun canonicalize-pat (pat)
  (if (stringp pat)
      (pregexp pat)
      pat))

(defun %pregexp-match-positions (pat str start end)
  (declare (type array-index start end)
           (type string str))
  (let ((match (cadr
                (collect-first
                 (choose-if #'identity
                   (mapping ((i (scan-range :from start :below end)))
                     (pregexp-match-positions-aux
                      pat
                      str start end i)))))))
    (values (caar match) (cdar match) match)))

(defun pregexp-match-positions (pat str &key (start 0) end)
  (check-type pat (or string cons))
  (check-type str string)
  (check-type start (integer 0 #.array-dimension-limit))
  (let ((end1 (or end (string-length str))))
    (check-type end1 (integer 0 #.array-dimension-limit))
    (%pregexp-match-positions (canonicalize-pat pat) str start end1)))
        
(defun pregexp-match (pat str &rest keywords)
  (check-type str string)
  (multiple-value-bind (start end backrefs)
      (apply #'pregexp-match-positions pat str keywords)
    (declare (ignore start end))
    (and backrefs
         (map 'list (lambda (ix-pr)
                      (subseq str (car ix-pr) (cdr ix-pr)))
              backrefs))))

(defun pregexp-scan-matching-positions (pat str start end)
  (declare (optimizable-series-function 3)
           (type array-index start end)
           (type string str))
    (scan-fn '(values
               (integer 0 #.array-dimension-limit)
               (integer 0 #.array-dimension-limit)
               t)
             (lambda () (%pregexp-match-positions pat str start end))
             (lambda (last-start last-end last-backrefs)
               (declare (ignore last-start last-backrefs))
               (%pregexp-match-positions pat str last-end end))
             (lambda (last-start last-end last-backrefs)
               (declare (ignore last-end last-backrefs))
               (null last-start))))

(defun pregexp-split (pat str)
  "Split str into substrings, using pat as delimiter."
  (check-type str string)
  (let ((n (length str)))
    (named-let loup ((i 0)
                     (r '())
                     (picked-up-one-undelimited-char-p nil))
      (if (>= i n)
          (nreverse r)
          (multiple-value-bind (j k backrefs)
              (pregexp-match-positions pat str :start i :end n)
            (declare (ignore backrefs))
            (cond ((or (null j) (null k))
                   (loup n (cons (subseq str i n) r) nil))
                  ((= j k)
                   (loup (1+ k)
                         (cons (subseq str i (1+ j)) r) t))
                  ((and (= j i) picked-up-one-undelimited-char-p)
                   (loup k r nil))
                  (t
                   (loup k (cons (subseq str i j) r) nil))))))))

(defun pregexp-replace (pat str ins)
  (check-type str string)
  (check-type ins string)
  (let ((n (length str))
        (ins-len (length ins)))
    (multiple-value-bind (m-i m-n backrefs)
        (pregexp-match-positions pat str :start 0 :end n)
      (if (null m-i)
          str
          (concatenate 'string
                       (subseq str 0 m-i)
                       (pregexp-replace-aux str ins ins-len backrefs)
                       (subseq str m-n n))))))

(defun pregexp-replace* (pat str ins)
  (check-type str string)
  (check-type ins string)
  (let ((pat     (canonicalize-pat pat))
        (n       (length str))
        (ins-len (length ins)))
    (named-let loup ((i 0)
                     (r ""))
      (multiple-value-bind (start end backrefs)
          (pregexp-match-positions pat str :start i :end n)
        (if (null start)
            (concatenate 'string r (subseq str i n))
            (loup end
                  (concatenate 'string
                               r
                               (subseq str i start)
                               (pregexp-replace-aux str ins ins-len backrefs))))))))

(defun pregexp-match-positions-aux (re s start n i)
  (declare (type array-index start n i)
           (type string s)
           #.(performance-optimizations))
  (let ((case-sensitive-p t))
    (named-let sub ((re re)
                    (i i)
                    (backrefs '())
                    (success #'list)
                    (fail (constantly nil)))
      (declare (type array-index i))
      (cond ((eql re :bos)
             (if (= i start)
                 (funcall success i backrefs)
                 (funcall fail)))
            ((eql re :eos)
             (if (< i n)
                 (funcall fail)
                 (funcall success i backrefs)))
            ((eql re :empty)
             (funcall success i backrefs))
            ((eql re :wbdry)
             (if (pregexp-at-word-boundary-p s i n)
                 (funcall success i backrefs)
                 (funcall fail)))
            ((eql re :not-wbdry)
             (if (pregexp-at-word-boundary-p s i n)
                 (funcall fail)
                 (funcall success i backrefs)))
            ((and (characterp re)
                  (< i n))
             (if (if case-sensitive-p
                     (char= (char s i) re)
                     (char-equal (char s i) re))
                 (funcall success (1+ i) backrefs)
                 (funcall fail)))
            ((and (not (consp re)) (< i n))
             (if (pregexp-check-if-in-char-class-p (char s i) re)
                 (funcall success (1+ i) backrefs)
                 (funcall fail)))
            ((and (consp re)
                  (eql (car re) :char-range)
                  (< i n))
             (let ((c (char s i)))
               (if (if case-sensitive-p
                       (char<= (cadr re) c (caddr re))
                       (char-not-greaterp (cadr re) c (caddr re)))
                   (funcall success (1+ i) backrefs)
                   (funcall fail))))
            ((consp re)
             (ecase (car re)
               (:char-range
                (unless (< i n)
                  (error ":char-range in pregexp-match-positions-aux"))
                (funcall fail))
               (:one-of-chars
                (if (< i n)
                    (named-let loop-one-of-chars
                        ((chars (cdr re)))
                      (if (null chars)
                          (funcall fail)
                          (sub (car chars) i backrefs success
                               (lambda ()
                                 (loop-one-of-chars (cdr chars))))))
                    (funcall fail)))

               (:neg-char
                (if (< i n)
                    (sub (cadr re) i backrefs
                         (lambda (i1 backrefs1)
                           (declare (ignore i1 backrefs1))
                           (funcall fail))
                         (lambda () (funcall success (1+ i) backrefs)))
                    (funcall fail)))

               (:seq
                (named-let loop-seq ((res (cdr re))
                                     (i i)
                                     (backrefs backrefs))
                  (if (null res)
                      (funcall success i backrefs)
                      (sub (car res) i backrefs
                           (lambda (i1 backrefs1)
                             (loop-seq (cdr res) i1 backrefs1))
                           fail))))

               (:or
                (named-let loop-or ((res (cdr re)))
                  (if (null res)
                      (funcall fail)
                      (sub (car res) i backrefs
                           (lambda (i1 backrefs1)
                             (or (funcall success i1 backrefs1)
                                 (loop-or (cdr res))))
                           (lambda ()
                             (loop-or (cdr res)))))))

               (:backref
                (let ((backref (pregexp-list-ref backrefs (cadr re))))
                  (if (null backref)
                      (funcall success i backrefs)
                      (let ((i (pregexp-string-match
                                (subseq s (car backref) (cdr backref))
                                s i n)))
                        (if i
                            (funcall success i backrefs)
                            (funcall fail))))))

               (:sub
                (let* ((sub-backref (cons i i))
                       (backrefs (append backrefs (list sub-backref))))
                  (sub (cadr re) i backrefs
                       (lambda (i1 backrefs1)
                         (setf (cdr sub-backref) i1)
                         (funcall success i1 backrefs1))
                       fail)))

               (:lookahead
                (if (sub (cadr re) i backrefs (constantly t) (constantly nil))
                    (funcall success i backrefs)
                    (funcall fail)))

               (:neg-lookahead
                (if (sub (cadr re) i backrefs (constantly t) (constantly nil))
                    (funcall fail)
                    (funcall success i backrefs)))

               (:lookbehind
                (let ((n-actual n))
                  (setq n i)
                  (let ((foundp
                         (sub `(:seq (:between nil 0 nil :any)
                                     ,(cadr re)
                                     :eos) 0 backrefs
                                     (constantly t)
                                     (constantly nil))))
                    (setq n n-actual)
                    (if foundp
                        (funcall success i backrefs)
                        (funcall fail)))))

               (:neg-lookbehind
                (let ((n-actual n))
                  (setq n i)
                  (let ((foundp
                         (sub `(:seq (:between nil 0 nil :any)
                                     ,(cadr re)
                                     :eos) 0 backrefs
                                     (constantly t)
                                     (constantly nil))))
                    (setq n n-actual)
                    (if foundp
                        (funcall fail)
                        (funcall success i backrefs)))))

               (:no-backtrack
                (let ((foundp (sub (cadr re) i backrefs #'cons (constantly nil))))
                  (if (null foundp)
                      (funcall fail)
                      (funcall success (car foundp) (cdr foundp)))))

               (:case-sensitive
                (let ((old case-sensitive-p))
                  (setq case-sensitive-p
                        (eql (car re) :case-sensitive))
                  (sub (cadr re) i backrefs
                       (lambda (i1 backrefs1)
                         (setq case-sensitive-p old)
                         (funcall success i1 backrefs1))
                       (lambda ()
                         (setq case-sensitive-p old)
                         (funcall fail)))))

               (:case-insensitive
                (let ((old case-sensitive-p))
                  (setq case-sensitive-p
                        (eql (car re) :case-sensitive))
                  (sub (cadr re) i backrefs
                       (lambda (i1 backrefs1)
                         (setq case-sensitive-p old)
                         (funcall success i1 backrefs1))
                       (lambda ()
                         (setq case-sensitive-p old)
                         (funcall fail)))))

               (:between
                (let* ((maximalp (not (cadr re)))
                       (p (caddr re))
                       (q (cadddr re))
                       (re (car (cddddr re)))
                       (subpatp (and (consp re)
                                     (eql (car re) :sub))))
                  (named-let loop-p
                      ((k 0) (i i) (cbackrefs 'no-match-yet))
                    (if (< k p)
                        (sub re i backrefs
                             (lambda (i1 backrefs1)
                               (loop-p (1+ k) i1 backrefs1))
                             fail)
                        (let ((q (and q (- q p))))
                          (named-let loop-q
                              ((k 0) (i i) (cbackrefs cbackrefs))
                            (let ((fk (lambda ()
                                        (funcall success i
                                                 (if (eql cbackrefs
                                                          'no-match-yet)
                                                     (if subpatp
                                                         (append backrefs
                                                                 (list nil))
                                                         backrefs)
                                                     cbackrefs)))))
                              (if (and q (>= k q))
                                  (funcall fk)
                                  (if maximalp
                                      (sub re i backrefs
                                           (lambda (i1 backrefs1)
                                             (or (loop-q (1+ k) i1 backrefs1)
                                                 (funcall fk)))
                                           fk)
                                      (or (funcall fk)
                                          (sub re i backrefs
                                               (lambda (i1 backrefs1)
                                                 (loop-q (1+ k) i1 backrefs1))
                                               fk)))))))))))))
            ((< i n) (error "pregexp-match-positions-aux"))
            (t (funcall fail))))))

(defun pregexp-replace-aux (str ins n backrefs)
  (named-let loup ((i 0)
                   (r ""))
    ;; (format t "~&r = ~s i = ~d" r i)
    (if (>= i n)
        r
        (let ((c (char ins i)))
          (if (char= c #\\)
              (let* ((br-i (pregexp-read-escaped-number ins i n))
                     (br   (cond (br-i (car br-i))
                                 ((char= (char ins (1+ i)) #\&) 0)
                                 (t nil)))
                     (i (cond (br-i (cadr br-i))
                              (br (+ i 2))
                              (t (1+ i)))))
                (if (not br)
                    (let ((c2 (char ins i)))
                      (loup (1+ i)
                            (if (char= c2 #\$)
                                r
                                (concatenate 'string r (string c2)))))
                    (loup i
                          (let ((backref (pregexp-list-ref backrefs br)))
                            (if (null backref)
                                r
                                (concatenate 'string r
                                             (subseq str (car backref) (cdr backref))))))))
              (loup (1+ i) (concatenate 'string r (string c))))))))

(defun pregexp-list-ref (s i)
    ;like list-ref but returns #f if index is
    ;out of bounds
  (do ((k 0 (1+ k))
       (s s (cdr s)))
      ((or (= k i)
           (null s)) (car s))))

(defun pregexp-check-if-in-char-class-p (c char-class)
  (ecase char-class
    (:any   (not (char= c #\newline)))

    (:alnum (alphanumericp c))
    (:alpha (alpha-char-p c))
    (:ascii (< (char-code c) 128))
    (:blank (or (char= c #\space)
                (char= c #\tab)))
    (:cntrl (< (char-code c) 32))
    (:digit (digit-char-p c))
    (:graph (and (>= (char-code c) 32)
                 (not (lw:whitespace-char-p c))))
    (:lower (lower-case-p c))
    (:print (>= (char-code c) 32))
    (:punct (and (>= (char-code c) 32)
                 (not (lw:whitespace-char-p c))
                 (not (alphanumericp c))))
    (:space (lw:whitespace-char-p c))
    (:upper (upper-case-p c))
    (:word (or (alphanumericp c)
               (char= c #\_)))
    (:xdigit (or (alphanumericp c)
                 (char-equal c #\a) (char-equal c #\b)
                 (char-equal c #\c) (char-equal c #\d)
                 (char-equal c #\e) (char-equal c #\f)))))

(defun pregexp-string-match (s1 s i n)
  (declare (type string s1 s)
           (type array-index i)
           (type array-index+ n)
           #.(performance-optimizations))
  (let ((n1 (length s1)))
    (declare (type array-index+ n1))
    (when (<= n1 n)
      (do ((j 0 (1+ j))
           (k i (1+ k)))
          ((or (>= j n1)
               (>= k n)
               (char/= (char s1 j)
                       (char s k)))
           (when (>= j n1) k))
        (declare (type array-index j k))))))

(defun pregexp-char-word-p (c)
  ;; too restrictive for Lisp but this
  ;; is what \w is in most regexp notations
    (or (alphanumericp c)
        (char= c #\_)))

(defun pregexp-at-word-boundary-p (s i n)
  (or (= i 0)
      (>= i n)
      (let ((c/i (char s i))
            (c/i-1 (char s (1- i))))
        (let ((c/i/w-p (pregexp-check-if-in-char-class-p c/i :word))
              (c/i-1/w-p (pregexp-check-if-in-char-class-p c/i-1 :word)))
          (or (and c/i/w-p (not c/i-1/w-p))
              (and (not c/i/w-p) c/i-1/w-p))))))

(defun pregexp-read-char-list (s i n)
  (labels ((luup (r i)
             (if (>= i n)
                 (error "character class ended too soon")
                 (let ((c (char s i)))
                   (case c
                     (#\] (if (null r)
                              (luup (cons c r) (1+ i))
                              (list (cons :one-of-chars (nreverse r))
                                    (1+ i))))
                     (#\\
                      (let ((char-i (pregexp-read-escaped-char s i n)))
                        (if char-i
                            (luup (cons (car char-i) r)
                                  (cadr char-i))
                            (error "backslash"))))
                     (#\- (let ((c-prev (car r)))
                            (if (characterp c-prev)
                                (luup (cons (list :char-range c-prev
                                                  (char s (1+ i)))
                                            (cdr r))
                                      (+ i 2))
                                (luup (cons c r) (1+ i)))))
                     (#\[ (if (char= (char s (1+ i)) #\:)
                              (let ((posix-char-class-i
                                     (pregexp-read-posix-char-class s (+ i 2) n)))
                                (luup (cons (car posix-char-class-i) r)
                                      (cadr posix-char-class-i)))
                              (luup (cons c r) (1+ i))))
                     (t (luup (cons c r) (1+ i))))))))

    (luup '() i)))

(defun pregexp-invert-char-list (vv)
  (setf (car vv) :none-of-chars)
  vv)

(defun pregexp-read-nums (s i n)
    ; s[i-1] = {
    ; returns (p q k) where s[k] = }
  (labels ((luup (p q k reading)
             (when (>= k n)
               (error "pregexp-read-nums"))
             (let ((c (char s k)))
               (cond ((digit-char-p c)
                      (if (= reading 1)
                          (luup (cons c p) q (1+ k) 1)
                          (luup p (cons c q) (1+ k) 2)))
                     ((and (lw:whitespace-char-p c) (not *pregexp-space-sensitive-p*))
                      (luup p q (1+ k) reading))
                     ((and (char= c #\,) (= reading 1))
                      (luup p q (1+ k) 2))
                     ((char= c #\})
                      (let ((p (parse-integer (coerce (nreverse p) 'string) :junk-allowed t))
                            (q (parse-integer (coerce (nreverse q) 'string) :junk-allowed t)))
                        (cond ((and (not p) (= reading 1)) (list 0 nil k))
                              ((= reading 1) (list p p k))
                              (t (list p q k)))))
                     (t nil)))))
    (luup '() '() i 1)))

(defun pregexp-wrap-quantifier-if-any (vv s n)
  (let ((re (car vv)))
    (labels ((luup (i)
              (if (>= i n)
                  vv
                  (let ((c (char s i)))
                    (cond ((and (lw:whitespace-char-p c) (not *pregexp-space-sensitive-p*))
                           (luup (1+ i)))
                          ((or (char= c #\*)
                               (char= c #\+)
                               (char= c #\?)
                               (char= c #\{))
                           (let* ((new-re (list :between 'minimal? 'at-least 'at-most re))
                                  (new-vv (list new-re 'next-i)))
                             (ecase c
                               (#\* (setf (caddr new-re) 0
                                          (cadddr new-re) nil))
                               (#\+ (setf (caddr new-re) 1
                                          (cadddr new-re) nil))
                               (#\? (setf (caddr new-re) 0
                                          (cadddr new-re) 1))
                               (#\{ (let ((pq (pregexp-read-nums s (1+ i) n)))
                                      (unless pq
                                          (error "left bracket must be followed by a number"))
                                      (setf (caddr new-re) (car pq)
                                            (cadddr new-re) (cadr pq))
                                      (setq i (caddr pq)))))
                             (labels ((luup (i)
                                        (if (>= i n)
                                            (setf (cadr new-re) nil
                                                  (cadr new-vv) i)
                                            (let ((c (char s i)))
                                              (cond ((and (lw:whitespace-char-p c)
                                                          (not *pregexp-space-sensitive-p*))
                                                     (luup (1+ i)))
                                                    ((char= c #\?)
                                                     (setf (cadr new-re) t
                                                           (cadr new-vv) (1+ i)))
                                                    (t
                                                     (setf (cadr new-re) nil
                                                           (cadr new-vv) i)))))))
                               (luup (1+ i)))
                             new-vv))
                          (t vv))))))
      (luup (cadr vv)))))

(defun pregexp-read-subpattern (s i n)
  (let* ((remember-space-sensitive? *pregexp-space-sensitive-p*)
         (ctyp-i (pregexp-read-cluster-type s i n))
         (ctyp (car ctyp-i))
         (i (cadr ctyp-i))
         (vv (pregexp-read-pattern s i n)))
    (setq *pregexp-space-sensitive-p* remember-space-sensitive?)
    (let ((vv-re (car vv))
          (vv-i  (cadr vv)))
      (if (and (< vv-i n)
               (char= (char s vv-i) #\)))
          (do ((ctyp ctyp (cdr ctyp))
               (re   vv-re (list (car ctyp) re)))
              ((null ctyp) (list re (1+ vv-i))))
          (error "pregexp read subpattern")))))

(defun pregexp-read-cluster-type (s i n)
  (declare (ignore n))
  ;; s[i-1] = left-paren
  (let ((c (char s i)))
    (case c
      (#\?
       (let ((i (1+ i)))
         (case (char s i)
           (#\: (list '() (1+ i)))
           (#\= (list '(:lookahead) (1+ i)))
           (#\! (list '(:neg-lookahead) (1+ i)))
           (#\> (list '(:no-backtrack) (1+ i)))
           (#\< (list (ecase (char s (1+ i))
                        (#\= '(:lookbehind))
                        (#\! '(:neg-lookbehind)))
                      (+ i 2)))
           (t (labels ((luup (i r inv-p)
                         (let ((c (char s i)))
                           (ecase c
                             (#\- (luup (1+ i) r t))
                             (#\i (luup (1+ i) (cons (if inv-p
                                                         :case-sensitive
                                                         :case-insensitive)
                                                     r)
                                        nil))
                             (#\x
                              (setq *pregexp-space-sensitive-p* inv-p)
                              (luup (1+ i) r nil))
                             (#\: (list r (1+ i)))))))
                (luup i '() nil))))))
      (t (list '(:sub) i)))))

(defun pregexp-read-posix-char-class (s i n)
    ; lbrack, colon already read
  (let ((neg-p nil))
    (labels ((luup (i r)
               (if (>= i n)
                   (error "pregexp-read-posix-char-class")
                   (let ((c (char s i)))
                     (cond ((char= c #\^)
                            (setq neg-p t)
                            (luup (1+ i) r))
                           ((alpha-char-p c)
                            (luup (1+ i) (cons c r)))
                           ((char= c #\:)
                            (if (or (>= (1+ i) n)
                                    (not (char= (char s (1+ i)) #\])))
                                (error "pregexp-read-posix-char-class"))
                            (let ((posix-class (read-from-string (coerce (nreverse r) 'string) )))
                              (list (if neg-p
                                        (list :neg-char posix-class)
                                        posix-class)
                                    (+ i 2))))
                           (t (error "pregexp-read-posix-char-class")))))))
      (luup i (list #\:)))))

(defun pregexp-read-escaped-char (s i n)
  (and (< (1+ i) n)
       (let ((c (char s (1+ i))))
         (case c
           (#\b (list :wbdry (+ i 2)))
           (#\B (list :not-wbdry (+ i 2)))
           (#\d (list :digit (+ i 2)))
           (#\D (list '(:neg-char :digit) (+ i 2)))
           (#\n (list #\newline (+ i 2)))
           (#\r (list #\return (+ i 2)))
           (#\s (list :space (+ i 2)))
           (#\S (list '(:neg-char :space) (+ i 2)))
           (#\t (list #\tab (+ i 2)))
           (#\w (list :word (+ i 2)))
           (#\W (list '(:neg-char :word) (+ i 2)))
           (t (list c (+ i 2)))))))

(defun pregexp-read-escaped-number (s i n)
  (and (< (1+ i) n)
       (let ((c (char s (1+ i))))
         (and (digit-char-p c)
              (labels ((luup (i r)
                         (if (>= i n)
                             (let ((num (parse-integer
                                    (coerce (nreverse r) 'string)
                                    :junk-allowed t)))
                               (when num (list num i)))
                             (let ((c (char s i)))
                               (if (digit-char-p c)
                                   (luup (1+ i) (cons c r))
                                   (let ((num (parse-integer
                                               (coerce (nreverse r) 'string)
                                               :junk-allowed t)))
                                     (when num (list num i))))))))
                (luup (+ i 2) (list c)))))))

(defun pregexp-read-piece (s i n)
  (let ((c (char s i)))
    (case c
      (#\^ (list :bos (1+ i)))
      (#\$ (list :eos (1+ i)))
      (#\. (pregexp-wrap-quantifier-if-any
             (list :any (1+ i)) s n))
      (#\[ (pregexp-wrap-quantifier-if-any
              (case (char s (1+ i))
                (#\^
                 (let ((vv (pregexp-read-char-list s (+ i 2) n)))
                   (list (list :neg-char (car vv)) (cadr vv))))
                (t (pregexp-read-char-list s (1+ i) n)))
              s n))
      (#\(
       (pregexp-wrap-quantifier-if-any
        (pregexp-read-subpattern s (1+ i) n) s n))
      (#\\
       (pregexp-wrap-quantifier-if-any
        (let ((num-i (pregexp-read-escaped-number s i n)))
          (if num-i
              (list (list :backref (car num-i)) (cadr num-i))
              (let ((char-i (pregexp-read-escaped-char s i n)))
                (if char-i
                    (list (car char-i) (cadr char-i))
                    (error "pregexp-read-piece backslash")))))
        s n))
      (t
       (if (or *pregexp-space-sensitive-p*
               (and (not (lw:whitespace-char-p c))
                    (not (char= c *pregexp-comment-char*))))
           (pregexp-wrap-quantifier-if-any
            (list c (1+ i)) s n)
           (labels ((luup (i in-comment?)
                      (if (>= i n)
                          (list :empty i)
                          (let ((c (char s i)))
                            (cond (in-comment? (luup (1+ i)
                                                     (not (char= c #\newline))))
                                  ((lw:whitespace-char-p c)
                                   (luup (1+ i) nil))
                                  ((char= c *pregexp-comment-char*)
                                   (luup (1+ i) t))
                                  (t (list :empty i)))))))
             (luup i nil)))))))

(defun pregexp-read-branch (s i n)
  (labels ((luup (pieces i)
                 (cond ((>= i n)
                        (list (cons :seq (nreverse pieces)) i))
                       ((let ((c (char s i)))
                          (or (char= c #\|)
                              (char= c #\))))
                        (list (cons :seq (nreverse pieces)) i))
                       (t (let ((vv (pregexp-read-piece s i n)))
                            (luup (cons (car vv) pieces) (cadr vv)))))))
    (luup '() i)))

(defun pregexp-read-pattern (s i n)
    (if (>= i n)
        (list
          (list :or (list :seq)) i)
        (labels ((luup (branches i)
                       (if (or (>= i n)
                               (char= (char s i) #\)))
                           (list (cons :or (nreverse branches)) i)
                           (let ((vv (pregexp-read-branch
                                      s
                                      (if (char= (char s i) #\|)
                                          (1+ i)
                                          i)
                                      n)))
                             (luup (cons (car vv) branches) (cadr vv))))))
          (luup '() i))))

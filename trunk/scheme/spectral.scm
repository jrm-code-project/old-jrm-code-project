(declare (usual-integrations))

(define-syntax when
  (syntax-rules ()
    ((when predicate action0 . actions)
     (if predicate
	 (begin action0 . actions)
	 #f))))

(define-syntax unless
  (syntax-rules ()
    ((unless predicate action0 . actions)
     (if predicate
	 #f
	 (begin action0 . actions)))))

(define-syntax dotimes
  (syntax-rules ()
    ((dotimes (variable limit-expr return-value ...) body0 body ...)
       (let ((limit limit-expr))
         (do ((variable 0 (fix:+ variable 1)))
             ((fix:>= variable limit) return-value ...)
           body0
           body ...)))))

(define-syntax summation
  (syntax-rules ()
    ((summation index-variable start limit-expr body)
     (let ((limit limit-expr))
       (do ((index-variable start (fix:+ index-variable 1))
            (answer 0 (+ answer body)))
	   ((fix:>= index-variable limit) answer))))))

(define (adjoin/eq list element)
  (if (memq element list)
      list
      (cons element list)))

;;; Represent matrices as vectors of rows.
;;; Cheesy, but effective.
(define-integrable (matrix-height matrix)
  (vector-length matrix))

(define-integrable (matrix-width matrix)
  (vector-length (vector-ref matrix 0)))

(define-integrable (matrix-ref matrix i j)
  (vector-ref (vector-ref matrix i) j))

(define-integrable (matrix-ref/c matrix i)
  (let ((row (vector-ref matrix i)))
    ;; don't integrate row!
    (lambda (j) (vector-ref row j))))

(define-integrable (matrix-set! matrix i j value)
  (vector-set! (vector-ref matrix i) j value))

(define-integrable (matrix-set/c! matrix i)
  (let ((row (vector-ref matrix i)))
    ;; don't integrate row!
    (lambda (j value)
      (vector-set! row j value))))

(define (allocate-matrix height width)
  (let ((rows (make-vector height #f)))
    (dotimes (i height rows)
      (vector-set! rows i (make-vector width 0)))))

;(define (transpose matrix)
;  (let* ((height (matrix-height matrix))
;	 (width  (matrix-width matrix))
;	 (new-matrix (allocate-matrix width height)))
;    (dotimes (i height new-matrix)
;      (let ((refm (matrix-ref/c matrix i)))
;	(dotimes (j width)
;          (matrix-set! new-matrix j i (refm j)))))))

(define (transpose matrix)
  (let ((height (matrix-height matrix))
	(width  (matrix-width matrix)))
    (let ((new-matrix (allocate-matrix width height)))

      (define (scan i j row)
	(if (fix:>= j width)
	    (if (fix:>= i height)
		new-matrix
		(let ((next-i (fix:+ i 1)))
		  (scan next-i 0 (vector-ref matrix i))))
	    (begin
	      (matrix-set! new-matrix j i (vector-ref row j))
	      (scan i (fix:+ j 1) row))))

      (scan 0 0 (vector-ref matrix 0)))))

(define (matrix-multiply m1 m2)
  (let* ((result-height (matrix-height m1))
	 (result-width  (matrix-width m2))
	 (result-matrix (allocate-matrix result-height result-width)))
    (dotimes (i result-height result-matrix)
      (let ((refm1 (matrix-ref/c m1 i))
	    (setmr (matrix-set/c! result-matrix i)))
	(dotimes (j result-width)
	  (setmr j
	    (summation k 
		       0 (matrix-height m2)
		       (* (refm1 k)
			  (matrix-ref m2 k j)))))))))

(define (test-multiply)
  (let ((matrix-a '#(#(1 0 2)
                     #(-1 3 1)))
	(matrix-b '#(#(3 1)
		     #(2 1)
		     #(1 0))))
    (let ((result (matrix-multiply matrix-a matrix-b)))
      (do ((i 0 (+ i 1)))
	  ((>= i (matrix-height result)))
	(let ((refmr (matrix-ref/c result i)))
	  (display "[ ")
	  (do ((j 0 (+ j 1)))
	      ((>= j (matrix-width result)))
	    (display (refmr j))
	    (display " "))
	  (display "] ")
	  (newline)))
      (newline))))

(define (matrix-row-sums matrix)
  (let* ((h (matrix-height matrix))
	 (w (matrix-width matrix))
	 (answer (make-vector h 0)))
    (dotimes (i h answer)
      (let ((refm (matrix-ref/c matrix i)))
	(vector-set! answer i
		     (summation j 0 w (refm j)))))))

(define (compute-R row-sums)
  (let* ((h (vector-length row-sums))
	 (r (allocate-matrix h h)))
    (dotimes (i h r)
      (matrix-set! r i i (vector-ref row-sums i)))))

(define (compute-pi row-sums)
  (let* ((h (vector-length row-sums))
	 (total (summation i 0 h (vector-ref row-sums i)))
	 (pi (make-vector h 0)))
    (dotimes (i h pi)
      (vector-set! pi i (/ (vector-ref row-sums i) total)))))

(define (compute-d pi)
  (let* ((h (vector-length pi))
	 (d (allocate-matrix h h)))
    (dotimes (i h d)
      (matrix-set! d i i (sqrt (vector-ref pi i))))))

(define (inverse-diagonal m)
  (let* ((h (matrix-height m))
	 (w (matrix-width m))
	 (inv (allocate-matrix h w)))
    (dotimes (i h inv)
      (matrix-set! inv i i (/ 1 (matrix-ref m i i))))))

(define (vector-times-matrix vector matrix)
  (let* ((h      (vector-length vector))
	 (answer (make-vector h 0)))
    (dotimes (i h answer)
	     (vector-set! answer i
			  (summation k 
				     0 h
				     (* (matrix-ref matrix k i)
					(vector-ref vector i)))))))

(define (matrix-times-vector matrix vector)
  (let* ((h      (vector-length vector))
	 (answer (make-vector h 0)))
    (dotimes (i h answer)
      (let ((refm (matrix-ref/c matrix i)))
	(vector-set! answer i
		     (summation k 
				0 h
				(* (refm k)
				   (vector-ref vector k))))))))

(define (vector-normalize v)
  (let* ((h (vector-length v))
	 (answer (make-vector h 0))
	 (total (sqrt (summation i
				 0 h
				 (let ((x (vector-ref v i)))
				   (* x x))))))
    (dotimes (i h answer)
      (vector-set! answer i (/ (vector-ref v i) total)))))

(define (close-enuf? v v1)
  (do ((i 0 (fix:+ i 1))
       (answer #t (and answer
		       (< (abs (- (vector-ref v i) (vector-ref v1 i))) .0001))))
      ((fix:>= i (vector-length v)) answer)))

(define (analyze matrix)
  (display "Computing A*AT... ")
  (let* ((start (runtime))
	 (att   (matrix-multiply matrix (transpose matrix)))
	 (rho   (let ((mid (runtime)))
		  (display (- mid start))
		  (display " seconds.")
		  (newline)
		  (matrix-row-sums att)))
	 (r     (begin (pp rho) (compute-r  rho)))
	 (pi    (begin ;;(pp r)
		  (compute-pi rho)))
	 (d     (begin (pp pi) (compute-d  pi)))
	 (q     (begin (display "Computing Q...")
		       (let* ((qstart (runtime))
		       (q (matrix-multiply d (matrix-multiply (inverse-diagonal r) (matrix-multiply att (inverse-diagonal d))))))
		  (display (- (runtime) qstart))
		  (display " seconds.")
		  (newline)
		  q)))

	 (initial-v
	  (let* ((x (vector-times-matrix pi (inverse-diagonal d)))
		 (limit (vector-length pi))
		 (answer (make-vector limit 0)))
	    (display x)(newline)
	    (vector-set! answer 0 (vector-ref x (- limit 1)))
	    (vector-set! answer (- limit 1) (- (vector-ref x 0)))
	    answer)))
    (display initial-v)(newline)

    (do ((i 0 (+ i 1))
	 (v (vector-normalize initial-v) (vector-normalize (matrix-times-vector q v)))
	 (v1 #f v))
	((and v1 (close-enuf? v v1))
	 (let ((end (runtime)))
	   (newline)
	   (do ((i 0 (+ i 1)))
	       ((>= i (vector-length v1)))
	     (display (vector-ref v i))
	     (display " ")
	     (display (vector-ref v1 i))
	     (display " ")
	     (newline))
	   (display "Took ")
	   (display (- end start))
	   (display " seconds.")))
      (display ".") (flush-output))))

(define *all-entries* '())
(define *cluster-matrix* '())
(define *entry-count* 0)
(define *entry-limit* 0)
(define *integer->token* '())
(define *key->data-points* '())
(define *popularity-cache* '())

;; A hash table for interning.
;; symbols seem to break Scheme.
(define *string->canonical* '())
(define *token->integer* '())
(define *token-counter* 0)

(define-integrable (key->integer key)
  (hash-table/get *token->integer* key #f))

(define-integrable (integer->key int)
  (vector-ref *integer->token* int))

(define (popularity key)
  (or (hash-table/get *popularity-cache* key #f)
      (let ((answer (length (hash-table/get *key->data-points* key #f))))
	(hash-table/put! *popularity-cache* key answer)
	answer)))

(define (intern-token string)
  (or (hash-table/get *string->canonical* string #f)
      (begin
	(hash-table/put! *string->canonical* string string)
	(hash-table/put! *token->integer* string *token-counter*)
	(set! *token-counter* (+ *token-counter* 1))
	string)))

(define (parse-host host-string)
  (let ((limit (string-length host-string)))
    (do ((i 0 (+ i 1))
	 (start 0 (if (char=? (string-ref host-string i) #\.)
		      (+ i 1)
		      start))
	 (components '() (if (char=? (string-ref host-string i) #\.)
			     (let ((token (substring host-string start i)))
			       (if (or (> start 0)
				       (not (string=? token "www")))
				   (cons (intern-token token) components)
				   components))
			     components)))
	 ((>= i limit)
	  (let ((last-token (substring host-string start limit)))
	    (if (string=? last-token "com")
		(reverse components)
		(reverse (cons (intern-token last-token ) components))))))))

(define (parse-path path-string)
  (let ((limit (string-length path-string)))
    (if (> limit 0)

    (do ((i 1 (+ i 1))
	 (start 0 (if (char=? (string-ref path-string i) #\/)
		      i
		      start))
	 (components '() (if (char=? (string-ref path-string i) #\/)
			     (cons (intern-token (substring path-string start i)) components)
			     components)))
	 ((>= i limit)
	  (let ((last-token  (substring path-string start limit)))
	    (reverse (cons (intern-token last-token) components)))))
    "")))

(define (process-entry host path query-args)
  (when (< *entry-count* *entry-limit*)
	(set! *entry-count* (+ *entry-count* 1))
  (let ((entry (list (parse-host host)
		     (parse-path path)
		     (map (lambda (qa) (and qa (intern-token qa))) query-args))))
    (set! *all-entries* (cons entry *all-entries*))
    (for-each (lambda (key)
		(when key
		      (hash-table/put! *key->data-points* key 
				       (adjoin/eq (hash-table/get *key->data-points* key '()) entry))))
	      (first entry))
    (for-each (lambda (key)
		(when key
		      (hash-table/put! *key->data-points* key 
				       (adjoin/eq (hash-table/get *key->data-points* key '()) entry))))
	      (second entry))
    (for-each (lambda (key)
		(when key
		      (hash-table/put! *key->data-points* key 
				       (adjoin/eq (hash-table/get *key->data-points* key '()) entry))))
	      (third entry))
    )))

(define (process-unique count)
  (set! *entry-count* 0)
  (set! *entry-limit* count)
  (set! *string->canonical* (make-string-hash-table))
  (set! *all-entries* '())
  (set! *token-counter* 0)
  (set! *token->integer* (make-string-hash-table))
  (set! *key->data-points* (make-string-hash-table))
  (set! *popularity-cache* (make-string-hash-table))
  (load "xaa.bin")
  (load "xab.bin")
  (load "xac.bin")
  (load "xad.bin")
  (load "xae.bin")
  (load "xaf.bin")
  (set! *integer->token* (make-vector *token-counter*))
  (hash-table/for-each *token->integer*
		       (lambda (key value)
			 (vector-set! *integer->token* value key)))
  (set! *cluster-matrix* (make-vector (length *all-entries*)))
  (do ((i 0 (+ i 1))
       (tail *all-entries* (cdr tail)))
      ((null? tail) #t)
    (vector-set! *cluster-matrix* i (make-vector *token-counter*))
    (do ((j 0 (+ j 1)))
	((>= j *token-counter*) #f)
      (vector-set! (vector-ref *cluster-matrix* i) j 0))
    (let ((entry (car tail)))
      (for-each (lambda (term)
		  (and term
		       (vector-set! (vector-ref *cluster-matrix* i) (key->integer term) 1)))
		(car entry))
      (for-each (lambda (term)
		  (and term
		       (vector-set! (vector-ref *cluster-matrix* i) (key->integer term) 1)))
		(cadr entry))
      (for-each (lambda (term)
		  (and term
		       (vector-set! (vector-ref *cluster-matrix* i) (key->integer term) 1)))
		(caddr entry))))
  #t)

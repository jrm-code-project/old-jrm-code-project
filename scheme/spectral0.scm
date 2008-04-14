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

(define-syntax sum
  (syntax-rules ()
    ((sum index-variable start limit-expr term)
     (let ((limit limit-expr))
       (do ((index-variable start (+ index-variable 1))
            (answer 0 (+ answer term)))
           ((>= index-variable limit) answer))))))

(define-syntax time-expression
  (syntax-rules ()
    ((time-expression phase form0 forms ...)
     (time-a-thunk phase (lambda () form0 forms ...)))))

(define (adjoin/eq list element)
  (if (memq element list)
      list
      (cons element list)))

;;; Represent matrices as vectors of rows.
;;; Cheesy, but effective.
(define (matrix-height matrix)
  (vector-length matrix))

(define (matrix-width matrix)
  (vector-length (vector-ref matrix 0)))

(define (matrix-ref matrix i j)
  (vector-ref (vector-ref matrix i) j))

(define (matrix-set! matrix i j value)
  (vector-set! (vector-ref matrix i) j value))

(define (allocate-matrix height width)
  (let ((rows (make-vector height #f)))
    (dotimes (i height rows)
      (vector-set! rows i (make-vector width 0)))))

(define (test-multiply matrix-multiply)
  (let ((matrix-a (allocate-matrix 2 3))
        (matrix-b (allocate-matrix 3 2)))
    ;; a, row 0 (1 0 2)
    (matrix-set! matrix-a 0 0 1)
    (matrix-set! matrix-a 0 1 0)
    (matrix-set! matrix-a 0 2 2)
    ;; a, row 1 (-1 3 1)
    (matrix-set! matrix-a 1 0 -1)
    (matrix-set! matrix-a 1 1 3)
    (matrix-set! matrix-a 1 2 1)
    ;; b, row 0 (3 1)
    (matrix-set! matrix-b 0 0 3)
    (matrix-set! matrix-b 0 1 1)
    ;; b, row 1 (2 1)
    (matrix-set! matrix-b 1 0 2)
    (matrix-set! matrix-b 1 1 1)
    ;; b, row 2 (1 0)
    (matrix-set! matrix-b 2 0 1)
    (matrix-set! matrix-b 2 1 0)

    (let ((result (matrix-multiply matrix-a matrix-b)))
      (or (and (= (matrix-ref result 0 0) 5)
               (= (matrix-ref result 0 1) 1)
               (= (matrix-ref result 1 0) 4)
               (= (matrix-ref result 1 1) 2))
          (error "broken")))))

(define (matrix-multiply left right)
  (unless (= (matrix-width left) (matrix-height right))
    (error "Cannot multiply"))
  (let* ((result-height (matrix-height left))
         (result-width  (matrix-width right))
         (result-matrix (allocate-matrix result-height result-width)))
    (dotimes (row result-height result-matrix)
      (dotimes (column result-width)
        (matrix-set! 
           result-matrix row column
          (sum k
               0 (matrix-height right)
               (* (matrix-ref left  row k)
                  (matrix-ref right k column))))))))

(define (transpose matrix)
  (let* ((height (matrix-height matrix))
         (width  (matrix-width matrix))
         (new-matrix (allocate-matrix width height)))
    (dotimes (i height new-matrix)
      (dotimes (j width)
        (matrix-set! new-matrix j i (matrix-ref matrix i j))))))

(define (similarity-matrix matrix)
  (matrix-multiply matrix (transpose matrix)))

(define (matrix-row-sums matrix)
  (let* ((height (matrix-height matrix))
         (width  (matrix-width matrix))
         (answer (make-vector height 0)))
    (dotimes (i height answer)
      (vector-set! answer i
                   (sum j
                        0 width
                          (matrix-ref matrix i j))))))

(define (compute-R row-sums)
  (let* ((h (vector-length row-sums))
         (r (allocate-matrix h h)))
    (dotimes (i h r)
      (matrix-set! r i i (vector-ref row-sums i)))))

(define (compute-pi row-sums)
  (let* ((length (vector-length row-sums))
         (total  (sum i 0 length (vector-ref row-sums i)))
         (pi     (make-vector length 0)))
    (dotimes (i length pi)
      (vector-set! pi i (/ (vector-ref row-sums i) total)))))

(define (compute-d pi)
  (let* ((h (vector-length pi))
         (d (allocate-matrix h h)))
    (dotimes (i h d)
      (matrix-set! d i i (sqrt (vector-ref pi i))))))

(define (compute-diagonal-inverse diagonal)
  (let* ((height  (matrix-height diagonal))
         (width   (matrix-width diagonal))
         (inverse (allocate-matrix height width)))
    (dotimes (i height inverse)
      (matrix-set! inverse i i (/ 1 (matrix-ref diagonal i i))))))

(define (vector->column-matrix vector)
  (let* ((height (vector-length vector))
         (answer (allocate-matrix height 1)))
    (dotimes (i height answer)
      (matrix-set! answer i 0 (vector-ref vector i)))))

(define (vector->row-matrix vector)
  (let* ((width (vector-length vector))
         (answer (allocate-matrix 1 width)))
    (dotimes (i width answer)
      (matrix-set! answer 0 i (vector-ref vector i)))))

(define (matrix->column-vector matrix)
  (let* ((length (matrix-height matrix))
         (answer (make-vector length 0)))
    (dotimes (i length answer)
      (vector-set! answer i (matrix-ref matrix i 0)))))

(define (matrix->row-vector matrix)
  (let* ((length (matrix-width matrix))
         (answer (make-vector length 0)))
    (dotimes (j length answer)
      (vector-set! answer j (matrix-ref matrix 0 j)))))

(define (vector-times-matrix vector matrix)
  (matrix->row-vector
   (matrix-multiply (vector->row-matrix vector) matrix)))

(define (matrix-times-vector matrix vector)
  (matrix->column-vector
   (matrix-multiply matrix (vector->column-matrix vector))))

(define (close-enuf? left right tolerance)
  (let ((limit (vector-length left)))
    (define (scan i)
      (or (>= i limit)
	  (and (< (abs (- (vector-ref left i) 
			  (vector-ref right i))) 
		  tolerance)
	       (scan (+ i 1)))))
    (scan 0)))

(define (test-eigenvector matrix eigenvector)
  (or 
   (close-enuf? eigenvector
		(matrix-times-vector matrix eigenvector)
		.00001)
   (error "It's not an eigenvector")))

(define (vector-normalize v)
  (let* ((h (vector-length v))
         (answer (make-vector h 0))
         (total (sqrt (sum i
                           0 h
                           (let ((x (vector-ref v i)))
                             (* x x))))))
    (dotimes (i h answer)
      (vector-set! answer i (/ (vector-ref v i) total)))))

(define (orthogonal-vector v)
  (let ((limit (vector-length v))
        (n (make-vector (vector-length v) 0)))

    (dotimes (i (floor (/ limit 2)))
      (vector-set! n i (vector-ref v (- limit i 1)))
      (vector-set! n (- limit i 1) (- (vector-ref v i))))
    (vector-normalize n)))

(define (analyze a)
  (let* ((atrans (time-expression "Transpose a" (transpose a)))
         (att    (time-expression "Multiply a, atrans" (matrix-multiply a atrans)))
         (rho    (time-expression "Row sums att" (matrix-row-sums att)))
         (r      (compute-r rho))
         (pi     (compute-pi rho))
         (d      (compute-d pi))
         (rinverse (compute-diagonal-inverse r))
         (dinverse (compute-diagonal-inverse d))
         (q (time-expression "Compute q"
	      (matrix-multiply d
                             (matrix-multiply rinverse
                                              (matrix-multiply att
                                                               dinverse)))))
         (first-eigenvector (vector-times-matrix pi dinverse)))

    (test-eigenvector q first-eigenvector)

    (let ((initial-vector (orthogonal-vector first-eigenvector)))
      (time-expression "Converge to second eigenvector"
      (do ((v (vector-normalize (matrix-times-vector q initial-vector))
              (vector-normalize (matrix-times-vector q v)))
           (old-v initial-vector v))
          ((close-enuf? v old-v 0.0001) v))))))

(define (time-a-thunk phase thunk)
  (let ((start (runtime)))
    (let ((answer (thunk)))
      (let ((end (runtime)))
	(display ";; Phase ")
	(display phase)
	(display " took ")
	(display (- end start))
	(display " seconds.")
	(newline)
	answer))))

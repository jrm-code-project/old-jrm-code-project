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
    (dotimes (j width answer)
      (vector-set! answer j (matrix-ref matrix 0 j)))))

(define (vector-times-matrix vector matrix)
  (matrix->column-vector
   (matrix-multiply (vector->row-matrix vector) matrix)))

(define (matrix-time-vector vector matrix)
  (matrix->row-vector
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
  (let* ((atrans (transpose a))
         (att    (matrix-multiply a atrans))
         (rho    (matrix-row-sums att))
         (r      (compute-r rho))
         (pi     (compute-pi rho))
         (d      (compute-d pi))
         (rinverse (compute-diagonal-inverse r))
         (dinverse (compute-diagonal-inverse d))
         (q (matrix-multiply d
                             (matrix-multiply rinverse
                                              (matrix-multiply att
                                                               dinverse))))
         (first-eigenvector (vector-times-matrix pi dinverse)))

    (test-eigenvector q first-eigenvector)

    (let ((initial-vector (orthogonal-vector first-eigenvector)))
      (do ((v (vector-normalize (matrix-times-vector q initial-vector))
              (vector-normalize (matrix-times-vector q v)))
           (old-v initial-vector v))
          ((close-enuf? v old-v 0.0001) v)))))

(define (time-a-thunk thunk)
  (let ((start (runtime)))
    (let ((answer (thunk)))
      (let ((end (runtime)))
    (display ";; Took ")
    (display (- end start))
    (display " seconds.")
    (newline)
    answer))))


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
                            (let ((a (refm1 k)))
                              ;; save some time
                              (if (zero? a)
                                  0
                                  (* a
                                     (matrix-ref m2 k j)))))))))))

(define (matrix-multiply-transpose m1 m2)
  ;; like matrix multiply, but result is transposed.
  (let* ((result-height (matrix-height m1))
         (result-width  (matrix-width m2))
         (result-matrix (allocate-matrix result-width result-height)))
    (dotimes (i result-height result-matrix)
      (let ((refm1 (matrix-ref/c m1 i)))
        (dotimes (j result-width)
          (matrix-set! result-matrix j i
                 (summation k 
                            0 (matrix-height m2)
                            (let ((a (refm1 k)))
                              ;; save some time
                              (if (zero? a)
                                  0
                                  (* a
                                     (matrix-ref m2 k j)))))))))))

(define (matrix-multiply-transpose-m2t m1 m2)
  ;; like matrix multiply, but result is transposed.
  ;; and m2 is transposed
  (let* ((result-height (matrix-height m1))
         (result-width  (matrix-height m2))
         (result-matrix (allocate-matrix result-width result-height)))
    (dotimes (i result-height result-matrix)
      (let ((refm1 (matrix-ref/c m1 i)))
        (dotimes (j result-width)
          (matrix-set! result-matrix j i
                 (summation k 
                            0 (matrix-width m2)
                            (let ((a (refm1 k)))
                              ;; save some time
                              (if (zero? a)
                                  0
                                  (* a
                                     (matrix-ref m2 j k)))))))))))


(define (vector-times-matrix vector matrix)
  (let* ((h      (vector-length vector))
         (answer (make-vector h 0)))
    (dotimes (i h answer)
             (let ((ival (vector-ref vector i)))
               (vector-set! answer i
                            (if (zero? ival)
                                0
                                (summation k 
                                           0 h
                                           (* (matrix-ref matrix k i)
                                              ival))))))))

(define (matrix-times-vector m1 v2)
  ;; This should be the same as a matrix multiply
  ;; with the matrix on the right being column
  ;; zero and the other columns being zero.
  (let* ((result-height (matrix-height m1))
         (result-vector (make-vector result-height 0)))
    (dotimes (i result-height result-vector)
      (let ((row (vector-ref m1 i)))
        (vector-set! result-vector i
                     (summation k 
                                0 (vector-length v2)
                                (let ((a (vector-ref row k)))
                                  ;; save some time
                                  (if (zero? a)
                                      0
                                      (* a
                                         (vector-ref v2 k))))))))))

(define (transposed-matrix-times-vector mt1 v2)
  ;; This should be the same as a matrix multiply
  ;; with the matrix on the right being column
  ;; zero and the other columns being zero.

  ;; matrix1 is transposed
  (let* ((result-height (matrix-width mt1))
         (result-vector (make-vector result-height 0)))
;    (dotimes (i result-height result-vector)
;       (vector-set! result-vector i
;                    (summation k 
;                               0 (vector-length v2)
;                               (let ((a (vector-ref (vector-ref mt1 k) i)))
;                                 ;; save some time
;                                 (if (zero? a)
;                                     0
;                                     (* a
;                                        (vector-ref v2 k)))))))

    ;; the problem with the above is that we are iterating in the
    ;; wrong direction.  This version avoids that by saving
    ;; the partial sums in the result vector.
    (dotimes (k (vector-length v2) result-vector)
      (let ((term (vector-ref v2 k)))
        (unless (zero? term)
          (let ((row (vector-ref mt1 k)))
        (dotimes (i result-height)
          (let ((a (vector-ref row i)))
            (unless (zero? a)
              (vector-set! result-vector i
                       (+ (vector-ref result-vector i)
                          (* a term))))))))))


    ))

(define (diagonal-matrix-times-vector m1 v2)
  ;; This should be the same as a matrix times vector
  ;; except we know a-priori that the non-diagonal
  ;; elements are zero.
  (let* ((result-height (matrix-height m1))
         (result-vector (make-vector result-height 0)))
    (dotimes (i result-height result-vector)
             (let ((row (vector-ref m1 i)))
               (let ((a (vector-ref row i)))
                 (unless (zero? a)
                   (vector-set! result-vector i
                                (* a
                                   (vector-ref v2 i)))))))))

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

(define (orthogonal-vector v)
  (let ((limit (vector-length v))
        (n (make-vector (vector-length v) 0)))

    (dotimes (i (floor (/ limit 2)))
      (vector-set! n i (vector-ref v (- limit i 1)))
      (vector-set! n (- limit i 1) (- (vector-ref v i))))
    (vector-normalize n)))





(define (analyze matrix)
  (display "Computing A*AT... ")
  (let* ((start (runtime))
         (atrans (transpose matrix))
         (att   (matrix-multiply matrix atrans))
         (rho   (let ((mid (runtime)))
                  (display (- mid start))
                  (display " seconds.")
                  (newline)
                  (matrix-row-sums att)))
         (r     (begin (pp rho) (compute-r  rho)))
         (pi    (begin ;;(pp r)
                  (compute-pi rho)))
         (d     (begin (pp pi) (compute-d  pi)))
;        (q     (begin (display "Computing Q...")
;                      (let* ((qstart (runtime))
;                      (q (matrix-multiply d (matrix-multiply (inverse-diagonal r) (matrix-multiply att (inverse-diagonal d))))))
;                 (display (- (runtime) qstart))
;                 (display " seconds.")
;                 (newline)
;                 q)))
         (rinverse (inverse-diagonal r))
         (dinverse (inverse-diagonal d))
         (foo (lambda (v)
                (let (;(m1 (matrix-times-vector att v))
                      (m2 (matrix-times-vector matrix (matrix-times-vector atrans v))))
                  ;(display m1) (newline)
                  ;(display m2) (newline) (newline)
                  m2)))

         (q-prime (lambda (v)
                    (diagonal-matrix-times-vector
                     d
                     (diagonal-matrix-times-vector
                      rinverse
                      (matrix-times-vector
                       matrix
                       (transposed-matrix-times-vector
                        matrix
                        (diagonal-matrix-times-vector 
                         dinverse v)))))))

         (first-eigenvector (vector-times-matrix pi (inverse-diagonal d)))

         (initial-v 
          (let* ((x (vector-normalize (orthogonal-vector first-eigenvector))))
            (display x)(newline)
            x)))

    (display initial-v)(newline)

    (do ((i 0 (+ i 1))
         (v (vector-normalize initial-v) (vector-normalize (q-prime v)))
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
           (display " seconds."))
         (matrix-times-vector (inverse-diagonal d) v))
      (display ".")
      (flush-output))))

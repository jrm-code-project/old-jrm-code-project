(declare (usual-integrations))

(declare (integrate-operator head-reduce))
(define (head-reduce procedure init list)
  (declare (integrate procedure init))

  (define (head-reduce-error)
    (error:wrong-type-argument list "list" 'HEAD-REDUCE))

  (define (head-reduce-loop state element tail)
    (if (pair? tail)
        (head-reduce-loop (procedure state element) (car tail) (cdr tail))
        (begin (if (not (null? tail))
                   (head-reduce-error))
               (procedure state element))))

  (if (pair? list)
      (head-reduce-loop init (car list) (cdr list))
      (begin (if (not (null? list))
                 (head-reduce-error))
             init)))

(define (adjoin/eq list element)
  (if (memq element list)
      list
      (cons element list)))

(define (adjoin/eqv list element)
  (if (memv element list)
      list
      (cons element list)))

(define (adjoin/equal list element)
  (if (member element list)
      list
      (cons element list)))

(define-integrable (union/eq left right)
  (head-reduce adjoin/eq left right))

(define-integrable (union/eqv left right)
  (head-reduce adjoin/eqv left right))

(define-integrable (union/equal left right)
  (head-reduce adjoin/equal left right))

(declare (integrate-operator intersection/eq))
(define (intersection/eq left right)
  (declare (integrate left))
  (head-reduce (lambda (result left-element)
                 (if (memq left-element right)
                     (adjoin/eq result left-element)
                     result))
               '()
                left))

(declare (integrate-operator intersection/eqv))
(define (intersection/eqv left right)
  (declare (integrate left))
  (head-reduce (lambda (result left-element)
                 (if (memv left-element right)
                     (adjoin/eqv result left-element)
                     result))
               '()
                left))

(declare (integrate-operator intersection/equal))
(define (intersection/equal left right)
  (declare (integrate left))
  (head-reduce (lambda (result left-element)
                 (if (member left-element right)
                     (adjoin/equal result left-element)
                     result))
               '()
                left))

(declare (integrate-operator difference/eq))
(define (difference/eq left right)
  (declare (integrate left))
  (head-reduce (lambda (result left-element)
                 (if (memq left-element right)
                     result
                     (adjoin/eq result left-element)))
               '()
               left))

(declare (integrate-operator difference/eqv))
(define (difference/eqv left right)
  (declare (integrate left))
  (head-reduce (lambda (result left-element)
                 (if (memv left-element right)
                     result
                     (adjoin/eqv result left-element)))
               '()
               left))

(declare (integrate-operator difference/equal))
(define (difference/equal left right)
  (declare (integrate left))
  (head-reduce (lambda (result left-element)
                 (if (member left-element right)
                     result
                     (adjoin/eq result left-element)))
               '()
               left))

(declare (integrate-operator interdifference/eq))
(define (interdifference/eq left right receiver)
  (declare (integrate left receiver))
  ;; Compute the set intersection and difference between
  ;; left and right.
  (define (iter tail intersection difference)
    (cond ((pair? tail) (let ((item (car tail)))
			  (if (memq item right)
			      (iter (cdr tail) (adjoin/eq intersection item) difference)
			      (iter (cdr tail) intersection (adjoin/eq difference item)))))
	  ((null? tail) (receiver intersection difference))
	  (else (error "Improper list" left))))
  (iter left '() '()))

(declare (integrate-operator is-nan?))
(define (is-nan? x)
  (not (or (flo:negative? x)
           (flo:positive? x)
           (flo:zero? x))))

(define-integrable (flo:10log10 x)
  (flo:* 4.3429448190325175 (flo:log x)))

(define-integrable (10log10 x)
  (flo:10log10 (exact->inexact x)))

(define-integrable (flo:log2 x)
  (flo:* 1.4426950408889634 (flo:log x)))

(define (log2 x)
  (cond ((inexact? x) (flo:log2 x))
        ((not (integer? x)) (flo:log2 (exact->inexact x)))
        (else (do ((x1 x (/ x1 2))
                   (a  0 (+ a 1)))
                  ((not (even? x1))
                   (if (= x1 1)
                       a
                       (+ a (flo:log2 (exact->inexact x1)))))))))

(define (int:gamma x)
  (define (iter x answer)
    (if (fix:< x 11)
	answer
	(iter (fix:- x 1) (int:* answer x))))

  (cond	((not (int:positive? x))
	 (error "int:gamma defined only for positive integers."))
	((int:= x 1) 1)
	((int:= x 2) 1)
	((int:= x 3) 2)
	((int:= x 4) 6)
	((int:= x 5) 24)
	((int:= x 6) 120)
	((int:= x 7) 720)
	((int:= x 8) 5040)
	((int:= x 9) 40320)
	((int:= x 10) 362880)
	(else (iter x 3628800))))

(define-integrable (factorial x)
  (gamma (+ x 1)))

(define (double-factorial x)
  (define (iter x answer)
    (if (< x 2)
	answer
	(iter (- x 2) (* answer x))))
  (iter x 1))

(define (gamma x)
  (cond ((integer? x) (int:gamma x))
	((integer? (- x 1/2)) (let ((n (- x 1/2)))
				(if (zero? n)
				    (inexact->exact 1.772453850905516027298167483341)
				    (* (inexact->exact 1.772453850905516027298167483341)
				       (/ (double-factorial (- (* n 2) 1))
					  (expt 2 n))))))
	(else (error "Can't take gamma of this: " x))))

(declare (integrate-operator probability->entropy/bits))
(define (probability->entropy/bits p)
  (- (* p (log2 p))))

(declare (integrate-operator probability->odds))
(define (probability->odds p)
  (/ p (- 1 p)))

(declare (integrate-operator odds->probability))
(define (odds->probability o)
  (/ o (+ o 1)))

(define-integrable (odds->entropy/bits o)
  (probability->entropy/bits (odds->probability o)))

(define-integrable (probability->log-odds p)
  (10log10 (probability->odds p)))

(declare (integrate-operator product))
(define (product weight list)
  (declare (integrate list))
  (head-reduce (lambda (total item)
                 (declare (integrate total item))
                 (* total (weight item)))
               1
               list))

(declare (integrate-operator sum))
(define (sum weight list)
  (declare (integrate list))
  (head-reduce (lambda (total item)
                 (declare (integrate total item))
                 (+ total (weight item)))
               0
               list))

(define-integrable (count-if predicate list)
  (head-reduce (lambda (total element)
                 (declare (integrate element))
                 (if (predicate element)
                     (fix:+ total 1)
                     total))
               0
               list))

(define-integrable (count-if-not predicate list)
  (head-reduce (lambda (total element)
                 (declare (integrate element))
                 (if (predicate element)
                     total
                     (fix:+ total 1)))
               0
               list))

(define-integrable (divide-list predicate input receiver)
  (define (iter tail yes no)
    (cond ((pair? tail) (let ((this-element (car tail)))
                          (if (predicate this-element)
                              (iter (cdr tail) (cons this-element yes) no)
                              (iter (cdr tail) yes (cons this-element no)))))
          ((null? tail) (receiver yes no))
          (else (error "Bad input list"))))
  (iter input '() '()))

(define (collect-if predicate)
  (lambda (list)
    (head-reduce (lambda (answer item)
                   (if (predicate item)
                       (cons item answer)
                       answer))
                 '()
                 list)))

(define (collect-if-not predicate)
  (lambda (list)
    (head-reduce (lambda (answer item)
                   (if (predicate item)
                       answer
                       (cons item answer)))
                 '()
                 list)))

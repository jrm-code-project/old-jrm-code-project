(declare (usual-integrations))

(define (error:not-positive object #!optional caller)
  (error:wrong-type-argument object "positive number" (if (eq? caller #!default) #f caller)))

(define (error:not-nonnegative object #!optional caller)
  (error:wrong-type-argument object "non-negative number" (if (eq? caller #!default) #f caller)))

(define (guarantee-positive object #!optional caller)
  (if (not (positive? object))
      (error:not-positive object caller)))

(define (guarantee-nonnegative object #!optional caller)
  (if (negative? object)
      (error:not-nonnegative object caller)))

(declare (integrate-operator head-reduce))
(define (head-reduce procedure init list)
  (declare (integrate procedure init))

  (define-integrable (head-reduce-error irritant)
    (error:not-list irritant 'HEAD-REDUCE))

  (define (head-reduce-loop state element tail)
    (if (pair? tail)
        (head-reduce-loop (procedure state element) (car tail) (cdr tail))
        (begin (if (not (null? tail))
                   (head-reduce-error tail))
               (procedure state element))))

  (if (pair? list)
      (head-reduce-loop init (car list) (cdr list))
      (begin (if (not (null? list))
                 (head-reduce-error tail))
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
	  (else (error:not-list left interdifference/eq))))
  (iter left '() '()))

(declare (integrate-operator is-nan?))
(define (is-nan? x)
  (not (or (flo:negative? x)
           (flo:positive? x)
           (flo:zero? x))))

(define-integrable (log->10log10 x)
  (flo:* 4.3429448190325175 x))

(define-integrable (10log10->log x)
  (flo:/ x 4.3429448190325175))

(define-integrable (flo:10log10 x)
  (log->10log10 (flo:log x)))

(define-integrable (10log10 x)
  (flo:10log10 (exact->inexact x)))

(define-integrable (flo:inverse-10log10 x)
  (flo:exp (10log10->log x)))

(define-integrable (inverse-10log10 x)
  (flo:inverse-10log10 (exact->inexact x)))

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

(define-integrable *pi* 3.141592653589793)
(define-integrable *log-pi* (flo:log *pi*))

(define *lanczos-coefficients*
  '(
    0.99999999999999709182
    57.156235665862923517
    -59.597960355475491248
    14.136097974741747174
    -0.49191381609762019978
    .33994649984811888699e-4
    .46523628927048575665e-4
    -.98374475304879564677e-4
    .15808870322491248884e-3
    -.21026444172410488319e-3
    .21743961811521264320e-3
    -.16431810653676389022e-3
    .84418223983852743293e-4
    -.26190838401581408670e-4
    .36899182659531622704e-5
    ))

(define (flo:log-gamma xx)
  (if (flo:positive? xx)
      (if (flo:< xx 1.0)
	  ;; Reflection
	  (let ((one-minus-x (flo:- 1.0 xx)))
	    (flo:- (flo:+ *log-pi* (flo:log one-minus-x))
		   (flo:+ (flo:log-gamma (flo:+ 1.0 one-minus-x))
			  (flo:log (flo:sin (flo:* *pi* one-minus-x))))))
	  ;; Lanczos approximation

	  (let* ((g (/ 607.0 128.0))
		 (sum 0.0))
	    (do ((i (- (length *lanczos-coefficients*) 1) (- i 1)))
		((= i 0))
	      (set! sum (+ sum (/ (list-ref *lanczos-coefficients* i) (+ xx i)))))
	    (set! sum (+ sum (car *lanczos-coefficients*)))
	    (let ((tmp (+ xx g .5)))
	      (+ (* .5 (log (* *pi* 2)))
		 (log (/ sum xx))
		 (- tmp)
		 (* (+ xx .5) (log tmp))))))

	  (error:not-positive xx 'log-gamma)))

(define (log-gamma xx)
  (flo:log-gamma (exact->inexact xx)))

(define (10log10-gamma x)
  (log->10log10 (log-gamma x)))

(define (int:factorial x)
  (define (iter x answer)
    (if (fix:< x 2)
	answer
	(iter (fix:- x 1) (int:* answer x))))
  (if (int:negative? x)
      (error:not-nonnegative x 'int:factorial)
      (iter x 1)))

(define (int:gamma x)
  (define (iter x answer)
    (if (fix:< x 2)
	answer
	(iter (fix:- x 1) (int:* answer x))))
  (if (int:negative? (- x 1))
      (error:not-positive x 'int:gamma)
      (iter (- x 1) 1)))

(define (double-factorial x)
  (define (iter x answer)
    (if (< x 2)
	answer
	(iter (- x 2) (* answer x))))
  (iter x 1))

(define (gamma x)
  (cond ((integer? x) (int:gamma (inexact->exact x)))
	((integer? (- x 1/2)) (let ((n (* 2 (- x 1))))
				(if (= n -3/2)
				    (inexact->exact 1.772453850905516027298167483341)
				    (* (inexact->exact 1.772453850905516027298167483341)
				       (/ (double-factorial n)
					  (expt 2 (/ (+ n 1) 2)))))))
	(else (exp (log-gamma x)))))

(define (10log10-gamma-ratio num den)
  ;; Compute log (gamma(num)/gamma(den))
  (flo:- (10log10-gamma num) (10log10-gamma den)))

(define (10log10-bernoulli-beta alpha beta m N)
  (guarantee-positive alpha '10log10-bernoulli-beta)
  (guarantee-positive beta '10log10-bernoulli-beta)
  (guarantee-exact-nonnegative-integer m '10log10-bernoulli-beta)
  (guarantee-exact-positive-integer N '10log10-bernoulli-beta)
  (flo:+ (10log10-gamma-ratio (+ alpha m) alpha)
	 (flo:+ (10log10-gamma-ratio (+ beta (int:- N m)) beta)
		(10log10-gamma-ratio (+ alpha beta) (+ (+ alpha beta) N)))))

;; Compute the log (x + y) given log(x) and log(y)
;; avoiding the problem of overflow.
(define-integrable (flo:log-sum left right)
  (if (flo:> right left)
      (flo:+ right (flo:10log10 (flo:+ 1.0 (flo:inverse-10log10 (flo:- left right)))))
      (flo:+ left (flo:10log10 (flo:+ 1.0 (flo:inverse-10log10 (flo:- right left)))))))

(define (log-sum left right)
  (flo:log-sum (exact->inexact left) (exact->inexact right)))

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

(declare (integrate-operator flo:sum))
(define (flo:sum weight list)
  (declare (integrate list))
  (head-reduce (lambda (total item)
                 (declare (integrate total item))
                 (flo:+ total (weight item)))
               0.0
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
          (else (error:not-list tail 'divide-list))))
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

(define (shuffle-list list)
  (map cdr (sort (map (lambda (element) (cons (random 1.0) element)) list) (lambda (l r) (< (car l) (car r))))))

(define (newtons-method f guess dx)

  (define (fprime x)
    (/ (- (f (+ x dx)) (f (- x dx)))
       (* dx 2)))

  (define (iter guess)
    (let ((fx (f guess)))
      (if (< (abs fx) dx)
	  guess
	  (iter (- guess (/ fx (fprime guess)))))))

  (iter guess))

;; Similar to Newton's method,
;; returns x such that (f x) is within dx of zero.
(define (secant-method f guess0 guess1 dx)
  (define (iter guess last-guess last-answer)
    (let ((fx (f guess)))
      (if (< (abs fx) dx)
	  guess
	  (iter (- guess (* fx (/ (- guess last-guess)
				  (- fx last-answer)))) 
		guess
		fx))))
  (iter guess1 guess0 (f guess0)))

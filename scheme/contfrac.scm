;;; -*- Mode: Scheme; coding: iso-8859-1 -*-
;;;
;;; Copyright © 2002 Joseph Marshall
;;;
;;;
;;; You may use and modify this software free of charge for whatever
;;; purpose you wish.  I hope you find it useful.  If you use this
;;; software you agree to keep this entire copyright notice in the
;;; source code.  You also agree to acknowledge credit for what you
;;; use.  You assume all risks associated with using this software.
;;;
;;;; Continued fraction arithmetic
;;;
;;;
;;; ``... the problem of finding the continued
;;;   fraction for a sum from the continued fractions
;;;   representing the addends is exceedingly complicated,
;;;   and unworkable in computational practice.''
;;;
;;;                     --A. YA. KHinchin, 1935
;;;
;;; Well, it is *more* complicated than simple addition, but it really
;;; isn't that hard at all.
;;;
;;; This implements simple arithmetic on continued fractions as described
;;; by Gosper in
;;;
;;; http://www.inwap.com/pdp10/hbaker/hakmem/cf.html
;;;   and
;;;
;;; http://www.tweedledum.com/rwg/cfup.htm
;;;
;;;
;;; Notes
;;; 1.  Equality is non-decidable with continued fractions.
;;;
;;; 2.  Some continued fractions don't converge, the process may get
;;;     stuck.  Example: (cf:* sqrt-two sqrt-two)  will not produce
;;;     2.   It can't decide between 1.99999 or 2.00000 until it reaches
;;;     the end of the (infinite) stream.
;;;
;;; 3.  If continued fractions were held by a structure, rather than as
;;;     a stream of coefficients, a print-method could be defined on them.
;;;     However, they couldn't easily be printed in a conforming manner
;;;     because the exactness prefix is supposed to be printed first, but
;;;     exactness can't be determined a-priori.  You find out when you
;;;     run out of coefficients.
;;;
;;; 4.  More complicated functions could be defined, (like sqrt), using
;;;     standard formulae, but performance is greatly improved if you can
;;;     express these formulae as functions on the state variables in
;;;     the gosper-1 and gosper-2 routines.
;;;
;;; 5.  This implements CFs with integer coefficients.  If you are
;;;     so inclined, you can use matrices as coefficients.  Then you
;;;     enter the realm of moebius transforms.  It allows you a cleaner
;;;     mechanism for implementation, but the math is much hairier
;;;     (you end up with tensors and stuff).  The code below should be
;;;     easy for someone with a background in calculus.
;;;
;;; 6.  This is somewhat bummed for speed.
;;;
;;; 7.  Try computing this function (from Muller):
;;;
;;;     a0 = 61/11
;;;
;;;     a1 = 11/2
;;;
;;;                           3000
;;;                  1130 - --------
;;;                           an-2
;;;     an = 111 - ------------------
;;;                   an-1
;;;
;;;     (imagine that the n-1 and n-2 are subscripts)
;;;
;;;     The answer converges to 6, but if you use floating point, it will
;;;     converge to 100.  (even if you use an enormous amount of precision!)

(declare (usual-integrations))

;;; Continued fractions are represented as streams of coefficients:
;;;
;;;                         1
;;;  sqrt 2 = 1 + --------------------
;;;                               1
;;;                2 + --------------------
;;;                                  1
;;;                     2 + --------------------
;;;                                     1
;;;                          2 + --------------------
;;;
;;;                               2 + ....
;;;
;;; represented as the stream:
;;; (1 2 2 2 2 ...)

;;; some fun continued fractions
(define phi
  ;; (1 1 1 ....)
  (cons-stream 1 phi))

(define sqrt-two
  ;; (1 2 2 2 ...)
  (cons-stream 1 (cons-stream 2 (tail sqrt-two))))

(define sqrt-three
  ;; (1 1 2 1 2 1 2 ...)
  (cons-stream 1 (cons-stream 1 (cons-stream 2 (tail sqrt-three)))))

(define e
  ;; (2  1 2 1  1 4 1  1 6 1  1 8 1 ...)
  ;; bizarre indentation emphasizes coefficients.
  (cons-stream
   2 (let loop ((i 2))
       (cons-stream
        1 (cons-stream
           i (cons-stream
              1 (loop (+ i 2))))))))

;; Some continued fractions don't converge.
;; When incrementally computing, error if this many
;; digits are absorbed without emitting any.
(define *cf-spin-limit* 32)

;;; Simple functions on continued fractions.

(define-integrable (cf:floor cf)
  (head cf))

(define (cf:recip cf)
  (cond ((empty-stream? cf) (error "Divide by zero"))
        ((zero? (head cf)) (tail cf))
        (else (cons-stream 0 cf))))

;;; Compare two continued fractions.
;;; One thing you give up when you have an incremental representation
;;; is the ability to reliably determine equality!

(define (cf:= cfl cfr #!optional precision)
  (let ((precision (if (default-object? precision)
                       *cf-spin-limit*
                       precision)))
    (cond ((empty-stream? cfl) (empty-stream? cfr))
          ((empty-stream? cfr) false)
          ((zero? precision) (error "Can't determine."))
          ((int:= (cf:floor cfl) (cf:floor cfr))
           (cf:= (tail cfl) (tail cfr) (- precision 1)))
          (else false))))

(define (cf:> cfl cfr #!optional precision)
  (let ((precision (if (default-object? precision)
                       *cf-spin-limit*
                       precision)))
    (cond ((empty-stream? cfl) (not (empty-stream? cfr)))
          ((empty-stream? cfr) false)
          ((zero? precision) (error "Can't determine."))
          ((int:= (cf:floor cfl) (cf:floor cfr))
           (cf:< (tail cfl) (tail cfr) (- precision 1)))
          (else (int:> (cf:floor cfl) (cf:floor cfr))))))

(define (cf:< cfl cfr #!optional precision)
  (cf:> cfr cfl (if (default-object? precision)
                    *cf-spin-limit*
                    precision)))

;;; Construct a continued fraction from a ratio.
(define (ratio->cf numerator denominator)
  ;; This is euclids GCD algorithm in disguise.
  (cond ((or (not (integer? numerator))
             (not (integer? denominator)))
         (error "Numerator and denominator must both be integers."))

        ((negative? numerator)
         (cf:negate (ratio->cf (- numerator) denominator)))

        ((zero? denominator) the-empty-stream)

        (else
         (let ((quotient&remainder (integer-divide numerator denominator)))
           (cons-stream
            (car quotient&remainder)
            (ratio->cf denominator
                       (cdr quotient&remainder)))))))

(define (rat:->cf rat)
  (ratio->cf
   (numerator rat)
   (denominator rat)))

(define (cf*rat:= cf rat)
  (cf:= cf (rat:->cf rat)))

(define (rat*cf:= rat cf)
  (cf:= (rat:->cf rat) cf))

(define (cf*rat:> cf rat)
  (cf:> cf (rat:->cf rat)))

(define (cf*rat:< cf rat)
  (cf:< cf (rat:->cf rat)))

(define (rat*cf:> rat cf)
  (cf:> (rat->cf rat) cf))

(define (rat*cf:< rat cf)
  (cf:< (rat:->cf rat) cf))

(define-integrable (guarantee-exact-integer x)
  (if (not (int:integer? x))
      (error "Not an exact integer.")))

(define-integrable (same-sign? a b)
  (or (and (positive? a) (positive? b))
      (and (negative? a) (negative? b))))

(define-integrable (euclid-step-0 a b c d success fail)
  ;; determine the integer part
  ;;
  ;;        ax + b
  ;; of    --------
  ;;        cx + d
  ;;
  ;; where a, b, c, and d are integers, and x
  ;; is known to be 0 <= x < +inf

  ;; This is undefined at cx + d = 0
  ;;
  ;; Since x is positive, this can only
  ;; happen if c is not the same sign as d, or
  ;; c or d is zero.
  (cond ((same-sign? c d)
         (let ((target (int:quotient b d)))
           (if (int:= (int:quotient a c) target)
               (success target)
               (fail))))
        ;; At some value of x, the denominator is zero.
        ;; However, there are certain cases where we can
        ;; get away it.

        ;; Case 1:  c = a = 0  reduces to b/d
        ((and (int:zero? c) (int:zero? a) (not (int:zero? d)))
         (success (int:quotient b d)))

        ;; Case 2:  d = b = 0 reduces to a/c
        ((and (int:zero? d) (int:zero? b) (not (int:zero? c)))
         (success (int:quotient a c)))

        (else (fail))))

(define-integrable (euclid-step-1 a b c d success fail)
  ;; determine the integer part
  ;;
  ;;        ax + b
  ;; of    --------
  ;;        cx + d
  ;;
  ;; where a, b, c, and d are integers, and x
  ;; is known to be 1 <= x < +inf

  ;; This is undefined at cx + d = 0

  (let ((cd (int:+ c d)))
    (cond ((same-sign? c cd)
           (let ((target (int:quotient (int:+ a b) cd)))
             (if (int:= (int:quotient a c) target)
                 (success target)
                 (fail))))
          ;; At some value of x, the denominator is zero.
          ;; However, there are certain cases where we can
          ;; get away it.

          ;; Case 1: c = a = 0 reduces to b/d
          ((and (int:zero? c) (int:zero? a) (not (int:zero? d)))
           (success (int:quotient b d)))

          ;; Case 2: d = b = 0 reduces to a/c
          ((and (int:zero? d) (int:zero? b) (not (int:zero? c)))
           (success (int:quotient a c)))

          (else (fail)))))

(define-integrable (euclid-step-4-1 a b c d e f g h
                                    success
                                    fail-x
                                    fail-y
                                    fail)
  ;; determine the integer part
  ;;
  ;;        axy + bx + cy + d
  ;; of    -------------------
  ;;        exy + fx + gy + h
  ;;
  ;; where a, b, c, d, e, f, g, h are integers,
  ;; and x and y are both 1 <= (x,y) < +inf

  ;; If the integer part cannot be determined, call
  ;; fail-x, fail-y, or fail.  Fail-x indicates failure and hints that
  ;; more information about x is likely to improve things.
  ;; Fail-y indicates failure and hints that more information about
  ;; y is likely to improve things.
  ;; Fail indicates failure and provides no hints.

  (let ((ef (int:+ e f))                ; denominator as x -> inf
        (eg (int:+ e g))                ; denominator as y -> inf
        (efgh (int:+ (int:+ e f)
                     (int:+ g h))))     ; denominator when x = y = 1

    (define (compare-quotients)
      (let ((target (int:quotient (int:+ (int:+ a b) (int:+ c d))
                                  efgh)))
        (cond ((not (int:= (int:quotient (int:+ a b) ef)
                           target)) (fail-x))
              ((not (int:= (int:quotient (int:+ a c) eg)
                           target)) (fail-y))
              (else (success target)))))

    ;; Note that this conditional would be much simpler if we
    ;; didn't supply hints.

    (cond ((int:positive? efgh)
           (cond ((not (int:positive? ef)) (fail-x))
                 ((not (int:positive? eg)) (fail-y))
                 (else (compare-quotients))))

          ((int:negative? efgh)
           (cond ((not (int:negative? ef)) (fail-x))
                 ((not (int:negative? eg)) (fail-y))
                 (else (compare-quotients))))

          (else (fail)))))

(define (gosper-1 cf a b c d)
  ;; Incrementally compute the coefficients of
  ;;
  ;;      ax + b
  ;; y = --------
  ;;      cx + d
  ;;
  ;; where x is a continued fraction and a, b, c, and d
  ;; are integers.

  (define (step spin-count cftail a b c d)
    ;; At each step, we either output a coefficient, or input
    ;; one from x.  Output is preferred.

    (define (output coeff)
      (let ((new-c (int:- a (int:* c coeff)))
            (new-d (int:- b (int:* d coeff))))
        (cons-stream
         coeff
         (if (and (int:zero? new-c) (int:zero? new-d))
             the-empty-stream
             (step 0
                   cftail
                   c     d
                   new-c new-d)))))

    (define (input)
      (if (empty-stream? cftail)
          ;; An empty stream represents `infinity'.
          ;; This has the effect of discarding b and d.
          (step (fix:1+ spin-count) cftail
                a a
                c c)

          ;; Force the tail and update the state
          (let ((coeff (head cftail)))
            (guarantee-exact-integer coeff)
            (step (fix:1+ spin-count)
                  (tail cftail)
                  (int:+ (int:* a coeff) b) a
                  (int:+ (int:* c coeff) d) c))))

    (cond ((and (int:zero? a) (int:zero? b))
           ;; If numerator hits zero, the answer is an exact rational.
           ;; We truncate the output.
           the-empty-stream)

          ;; If we keep inputting digits, but don't output any,
          ;; the fraction is not converging.
          ((>= spin-count *cf-spin-limit*) (error "Stuck computation"))

          ;; otherwise, attempt to perform a step of
          ;; euclid reduction.
          (else
           (euclid-step-1
            a b c d
            output
            input))))

  (guarantee-exact-integer a)
  (guarantee-exact-integer b)
  (guarantee-exact-integer c)
  (guarantee-exact-integer d)
  (step 0 cf a b c d))

;;; Functions based on single-input gosper algorithm.

(define (cf:negate cf)
  (gosper-1 cf -1 0 0 1))

(define (cf*rat:+ cf rat)
  (gosper-1 cf (denominator rat) (numerator rat)
                0                (denominator rat)))

(define (cf*rat:- cf rat)
  (gosper-1 cf (denominator rat) (- (numerator rat))
                0                (denominator rat)))

(define (cf*rat:* cf rat)
  (gosper-1 cf (numerator rat)   0
               0 (denominator rat)))

(define (cf*rat:/ cf rat)
  (gosper-1 cf (denominator rat) 0
               0   (numerator rat)))

(define-integrable (rat*cf:+ rat cf) (cf*rat:+ cf rat))

(define (rat*cf:- rat cf)
  (gosper-1 cf (- (denominator rat)) (numerator rat)
                0                    (denominator rat)))

(define-integrable (rat*cf:* rat cf) (cf*rat:* cf rat))

(define (rat*cf:/ rat cf)
  (gosper-1 cf 0                 (numerator rat)
               (denominator rat) 0))

;;; Printing a continued fraction.
;;; This is simply a variation on gosper-1,
;;; but instead of reciprocating when we output a coefficient,
;;; we subtract and multiply the remainder by 10 (or the base).

;; Number of digits to print by default
;; Set this to -1 to keep printing forever
(define *cf-print-length* 64)

(define (cf:render cf #!optional alternate-base n-digits)
  ;; Output digits on the standard output stream.
  (let ((base (if (default-object? alternate-base) 10 alternate-base))
        (limit (if (default-object? n-digits) *cf-print-length* n-digits)))

    (define (step spin-count digit-count cftail a b c d)

      (define (output digits)
        ;; Gosper-1 tolerates negative digits; it essentially
        ;; allows the stream to `back up'.  Can't do that when
        ;; printing, though.  Shouldn't happen here, anyway.
        (if (int:negative? digits)
            (error "negative digits??"))
        (write digits)
        (if (int:zero? digit-count)
            (write-char #\.))
        ;; in effect, multiply the remainder
        ;; of the fraction by the base
        (let ((next-a (int:* base (int:- a (int:* c digits))))
              (next-b (int:* base (int:- b (int:* d digits)))))
          (step 0
                (int:1+ digit-count)
                cftail
                next-a next-b
                c      d)))

      (define (input)
        (if (empty-stream? cftail)
            (step (fix:1+ spin-count)
                  digit-count
                  cftail
                  a a
                  c c)
            (let ((coeff (head cftail)))
              (guarantee-exact-integer coeff)
              (step (fix:1+ spin-count)
                    digit-count
                    (tail cftail)
                    (int:+ (int:* a coeff) b) a
                    (int:+ (int:* c coeff) d) c))))

      (cond
       ;; If numerator hits zero, answer is exact
       ((and (int:zero? a) (int:zero? b)) cf)

       ;; If we reach the limit, truncate the output
       ;; could be more clever here, but this is adequate for now.
       ((int:= digit-count limit) (display "...") cf)

       ((int:>= spin-count *cf-spin-limit*)
        (error "Stuck computation"))

       (else
        (euclid-step-1
         a b c d
         output
         input))))

    (define (do-positive)
      (step 0 0 cf 1 0 0 1))

    (define (do-negative)
      (display "-")
      (step 0 0 (cf:negate cf) 1 0 0 1))

    (cond ((empty-stream? cf) (display "+inf"))
          ((negative? (head cf)) (do-negative))
          ((positive? (head cf)) (do-positive))
          ((empty-stream? (tail cf)) (display "0"))
          ((negative? (head (tail cf))) (do-negative))
          (else (do-positive)))))

(define (gosper-2 cfx cfy a b c d e f g h)
  ;; Incrementally compute
  ;;
  ;;      axy + bx + cy + d
  ;; z = -------------------
  ;;      exy + fx + gy + h
  ;;
  ;; where cfx and cfy are continued fractions and
  ;; a, b, c, d,
  ;; e, f, g, and h are integers.
  ;;
  ;; This is the foundation of all the two-argument
  ;; continued fraction functions.

  (define (step spin-count
                xtail ytail
                a b c d
                e f g h)

    (define (output coeff)
      ;; Spit out a coefficient
      (let* ((new-e (int:- a (int:* coeff e)))
             (new-f (int:- b (int:* coeff f)))
             (new-g (int:- c (int:* coeff g)))
             (new-h (int:- d (int:* coeff h))))
        (cons-stream
         coeff
         (if (and (int:zero? new-e)
                  (int:zero? new-f)
                  (int:zero? new-g)
                  (int:zero? new-h))
             the-empty-stream
             (step 0
                   xtail ytail
                   e f g h
                   new-e new-f new-g new-h)))))

    (define (input-something)
      ;; Input from either x or y.
      ;; alternating is fine if we have no other clues.
      (if (fix:zero? (fix:quotient spin-count 2))
          (if (empty-stream? xtail) (input-from-y) (input-from-x))
          (if (empty-stream? ytail) (input-from-x) (input-from-y))))

    (define (input-from-x)
      ;; Pull something in from the x stream
      (if (empty-stream? xtail)
          (step (fix:1+ spin-count)
                xtail ytail
                a b a b
                e f e f)
          (let ((x-head (head xtail)))
            (guarantee-exact-integer x-head)
            (step (fix:1+ spin-count)
                  (tail xtail) ytail
                  (int:+ (int:* x-head a) c)
                  (int:+ (int:* x-head b) d)
                  a
                  b
                  (int:+ (int:* x-head e) g)
                  (int:+ (int:* x-head f) h)
                  e
                  f))))

    (define (input-from-y)
      ;; Pull something in from the y stream
      (if (empty-stream? ytail)
          (step (fix:1+ spin-count)
                xtail ytail
                a a c c
                e e g g)
          (let ((y-head (head ytail)))
            (step (fix:1+ spin-count)
                  xtail (tail ytail)
                  (int:+ (int:* y-head a) b)
                  a
                  (int:+ (int:* y-head c) d)
                  c
                  (int:+ (int:* y-head e) f)
                  e
                  (int:+ (int:* y-head g) h)
                  g))))

    (cond
       ;; If no digits for a while, give up
       ((int:>= spin-count *cf-spin-limit*)     (error "Stuck computation"))

       ;; If numerator hits zero, answer is rational
       ;; but we have already emitted stuff, so just
       ;; truncate.
       ((and (int:zero? a) (int:zero? b) (int:zero? c) (int:zero? d))
        the-empty-stream)

       ;; If e is zero, we'll be doing two inputs anyway.
       ;; get it over with.
       ((int:zero? e) (input-something))

       ;; Try a reduction step.
       (else (euclid-step-4-1 a b c d e f g h output
                              input-from-x input-from-y
                              input-something))))


  (guarantee-exact-integer a)
  (guarantee-exact-integer b)
  (guarantee-exact-integer c)
  (guarantee-exact-integer d)
  (guarantee-exact-integer e)
  (guarantee-exact-integer f)
  (guarantee-exact-integer g)
  (guarantee-exact-integer h)
  (step 0 cfx cfy a b c d e f g h))

;;; What is neat is that add, subtract, multiply and divide
;;; are all implemented with the same function!!  We just tweak
;;; the initial conditions.

(define (cf*cf:+ cfl cfr)
  (gosper-2 cfl cfr 0 1 1 0 0 0 0 1))

(define (cf*cf:- cfl cfr)
  (gosper-2 cfl cfr 0 1 -1 0 0 0 0 1))

(define (cf*cf:* cfl cfr)
  (gosper-2 cfl cfr 1 0 0 0 0 0 0 1))

(define (cf*cf:/ cfl cfr)
  (gosper-2 cfl cfr 0 1 0 0 0 0 1 0))

(define (normalize-cf numerators denominators a b c d)
  ;; Given a stream of numerators and denominators,
  ;; return a stream of the coefficients of the equivalent
  ;; regular continued fraction.

  ;; Again, this is a variant of gosper-1

  (define (step spin-count ntail dtail a b c d)

    (define (output number)
      ;; Note the call to GCD.
      ;; Non-normalized CFs benefit from having the
      ;; the state variables kept in lowest form.
      ;; Normalized ones are always that way.
      (let* ((new-c (int:- a (int:* c number)))
             (new-d (int:- b (int:* d number)))
             (g (gcd c d new-c new-d)))

        (cons-stream
         number
         (step 0
               ntail
               dtail
               (int:quotient c g)    (int:quotient d g)
               (int:quotient new-c g)(int:quotient new-d g)
               ))))

    (define (input)
      (if (empty-stream? ntail)
          (step 0
                ntail
                dtail
                a a
                c c)
          (let ((coeff (head dtail))
                (num   (head ntail)))
            (guarantee-exact-integer coeff)
            (guarantee-exact-integer num)
            (step (fix:1+ spin-count)
                  (tail ntail)
                  (tail dtail)
                  (int:+ (int:* a coeff) (int:* b num)) a
                  (int:+ (int:* c coeff) (int:* d num)) c))))

    (cond
     ;; If no digits for a while, give up
     ((>= spin-count *cf-spin-limit*) (error "Stuck computation"))

     ((and (int:zero? a) (int:zero? b)) the-empty-stream)

     ;; try a euclid step
     (else
      (euclid-step-0
       a b c d
       output
       input))))

  (guarantee-exact-integer a)
  (guarantee-exact-integer b)
  (guarantee-exact-integer c)
  (guarantee-exact-integer d)
  (step 0 numerators denominators a b c d))

(define pi
  (let ()
    (define odds
      (let loop ((i 1))
        (cons-stream i (loop (+ i 2)))))

    (define squares
      (let loop ((i 1))
        (cons-stream (* i i) (loop (1+ i)))))

    (normalize-cf (cons-stream 1 squares) odds 0 4 1 0)))


#||
;;; Random testing code.

(define (print-pi ndigits)
  (cf:render pi 10 ndigits))

(define (test-0)
  (map (lambda (cf)
         (newline)
         (cf:render cf)
         (newline)
         (cf:render (cf:recip cf))
         (newline)
         (cf:render (cf:negate cf))
         (newline)
         (cf:render (cf:negate (cf:recip cf)))
         (newline)
         (cf:render (gosper-1 cf -2 5 -3 6)))
       (list phi sqrt-two sqrt-three e pi)))

(define (test-1)
  (let loop ((count 0)
             (start (runtime)))
    (if (= count 100)
        (floor (* (- (runtime) start) 10))
        (begin
          (stream-ref (gosper-1 sqrt-two 0 2 -1 3) 1000)
          (stream-ref (gosper-1 pi 1 2 0 3) 1000)
          (stream-ref (gosper-1 e 1 1 1 -1) 1000)
          (stream-ref (gosper-1 phi 0 1 6 2) 1000)
          (loop (1+ count) start)))))

(define (test-2)
  (let l1 ((i 0))
    (if (= i 8)
        #f
        (let l2 ((j (+ i 1)))
          (if (= j 8)
              (l1 (1+ i))
              (let l3 ((k (+ j 1)))
                (if (= k 8)
                    (l2 (1+ j))
                    (begin (newline) (display i) (display " ")
                           (display j) (display " ")
                           (display k) (display " ")
                           (cf:render (gosper-1 pi i j k (- i)))
                           (cf:render (gosper-1 e i j k (- i)))
                           (l3 (+ k 1))))))))))

(define (test)
  (newline)
  (display "")
  (newline)
  (cf:render (gosper-1 sqrt-two 0 2 -1 3))
  (newline)

  (newline)
  (display
   "0.367879441171442321595523770161460867445811131031767834507836801...")
  (newline)
  (cf:render (cf:recip e))
  (newline)
  (display
   "0.423310825130748003102355911926840386439922305675146246007976964...")
  (newline)
  (cf:render (cf*cf:- pi e))
  (newline)
  (display
   "2.816305093398751331727331379663195459013258742431006753294691576...")
  (newline)
  (cf:render (cf*cf:/ (cf*cf:* pi e)
                      (cf*cf:+ phi sqrt-two))))

(define (muller)
  (do ((n 0 (1+ n))

       ;; use exact rationals
       (an (/ 61 11) (- 111 (/ (- 1130 (/ 3000 an-1)) an)))
       (an-1 (/ 11 2) an)

       ;; use floating point
       (fan (/ 61.0 11.0) (- 111 (/ (- 1130 (/ 3000 fan-1)) fan)))
       (fan-1 (/ 11 2) fan)

       ;; use continued fractions
       (can (ratio->cf 61 11) (rat*cf:- 111 (cf*cf:/ (rat*cf:- 1130 (rat*cf:/ 3000 can-1)) can)))
       (can-1 (ratio->cf 11 2) can))

      ((> n 30)
       (newline)
       (cf:render can 10 8))
    (newline)
    (display n)
    (display " ")
    (display (exact->inexact an))
    (display " ")
    (display fan)))

||#

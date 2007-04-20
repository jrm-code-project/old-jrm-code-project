(declare (usual-integrations))

;;; Ackermann's function
(define (ackermann i k)
  (cond ((= i 0) (+ k 1))
	((= k 0) (ackermann (- i 1) 1))
	(else (ackermann (- i 1)
			 (ackermann i (- k 1))))))

;;; (ackermann 3 11) => 16381
;;; (ackermann 4 0)  => 13
;;; You probably can't go much beyond these.

;;; Buck's recurrence
;;; If i = 1, computes 2 + k
;;;    i = 2, computes 2 * k
;;;    i = 3, computes 2 ^ k
;;;    i = 4, computes (2 ^ (2 ^ (2 ...))) k times
(define (bucks i k)
  (cond ((= k 0) (cond ((= i 1) 2)
		       ((= i 2) 0)
		       (else    1)))
	((= i 0) (+ k 1))
	(else (bucks (- i 1)
		     (bucks i (- k 1))))))

;;; (bucks 3 14) => 16384
;;; (bucks 4  3) =>    16
;;; You probably can't go much beyond these, either.

;;; Source:  Jeremy Wertheimer of ITA Software
;;; This is a generalization of Buck's recurrance
;;; where j is used as the base rather than the
;;; fixed number 2.  That is,
;;; (bucks i k) = (m i 2 k)

(define (m i j k)
  (cond ((= k 0) (cond ((= i 1) j)
                       ((= i 2) 0)
                       (else    1)))
        ((= i 0) (+ k 1))
        (else (m (- i 1)
		 j
		 (m i j (- k 1))))))

;;; The ITA Quiz question was
;;; What is (m 4 4 4)?
;;;
;;; The answer is (expt 4 (expt 4 (expt 4 4)))
;;; = (expt 4 (expt 4 256))
;;; = (expt 4 1.34E154)
;;; = A number with 8.07 * 10^153 digits, way too big to print.

;;; You probably know the trick of fast exponentiation:
(define (fast-exp base power)
  (cond ((= power 1) base)
	((even? power) (fast-exp (* base base)
				 (/ power 2)))
	(else (* base (fast-exp base (- power 1))))))

;;; And you probably know the trick of fast exponentiation
;;; with a modulus:
(define (fast-expmod base power modulus)
  (cond ((= power 1) base)
	((even? power) (fast-expmod (modulo (* base base) modulus)
				    (/ power 2)
				    modulus))
	(else (modulo
	       (* base (fast-expmod base (- power 1) modulus))
	       modulus))))

;;; But here is a trick to compute the log of an exponentiation
;;; without expanding it all the way.
;;; (log (exp base power)) => (fast-logexp (log base) power)

(define (fast-logexp logbase power)
  (cond ((= power 1) logbase)
	((even? power) (fast-logexp (+ logbase logbase)
				    (/ power 2)))
	(else (+ logbase (fast-logexp logbase (- power 1))))))

;;; (log (fast-exp 4 256))    => 354.891356446692
;;; (fast-logexp (log 4) 246) => 354.891356446692
;;;
;;; (fast-logexp (/ (log 4) (log 10)) (expt 4 256))
;;; => 8.072304726028224e153
;;; which is the number of decimal digits in (m 4 4 4)

;;; Here is a trick for finding the leftmost digit of
;;; a number in any base *really* fast.
;;; Thanks to Bill Rozas for showing me this.

(define (leftmost-digit n base)
  (define (scan i next)
    (if (> next n)
	(leftmost-digit (quotient n i) base)
	(scan next (* next next))))

  (if (> base n)
      n
      (scan base (* base base))))

;;; Bill Dubuque showed me this:
;;;
;;; Take any positive integer greater than or equal to 2.
;;; Write it out in the base 2 representation.
;;; For example the base 2 representation of 266 is
;;;
;;;                           8    3
;;;                    266 = 2  + 2  + 2
;;;
;;; Now recursively do the same for the exponents:
;;;
;;;
;;;                            3
;;;                           2        2 + 1
;;;                    266 = 2      + 2      + 2
;;;
;;; keep going:
;;;
;;;
;;;                            2 + 1
;;;                           2        2 + 1
;;;                    266 = 2      + 2      + 2
;;;
;;; This is the `hereditary base 2 representation of 266'.
;;;
;;; Now we define the `base bump' operator as taking the hereditary
;;; base representation of a number and syntactically replacing the
;;; base with the next biggest base.  For instance, to bump the
;;; base of 266 from 2 to 3, we replace the 2s with 3s:
;;;
;;;                       3 + 1
;;;                      3        3 + 1
;;;                     3      + 3      + 3
;;;
;;;     = 443426488243037769948249630619149892887
;;;
;;; We can base bump from 3 to 4:
;;;
;;;                       4 + 1
;;;                      4        4 + 1
;;;                     4      + 4      + 4
;;;
;;;    ~= 3 x 10^616
;;;
;;; Here is the base-bump function in Scheme:

(define (base-bump base n)
  (if (zero? n)
      0
      (do ((exponent  0 (+ exponent 1))
	   (divisor   1  next)
	   (next   base (* next base)))
	  ((> next n)
	   (let* ((remainder (modulo n divisor))
		  (quotient  (/ (- n remainder) divisor)))
	     (+ (* quotient (expt (+ base 1) (base-bump base exponent)))
		(base-bump base remainder)))))))

;;; If we alternate bumping the base with subtracting by one, we have
;;; a Goodstein sequence.  For a smaller starting number it doesn't
;;; grow *quite as fast.  Here are the first few values starting at 4.

;;;  2
;;; 2   = 4
;;;
;;;    2
;;; 2*3  + 2*3 + 2 = 26
;;;
;;;    2
;;; 2*4  + 2*4 + 1 = 41
;;;
;;;    2
;;; 2*5  + 2*5     = 60
;;;
;;;    2
;;; 2*6  + 6 + 5   = 83
;;;
;;;    2
;;; 2*7  + 7 + 4   = 109

;;; Goodstein's theorem states that every Goodstein sequence
;;; eventually terminates at 0, no matter what the start value is.

(define (goodstein seed)
  (do ((base 2   (+ base 1))
       (n   seed (- (base-bump base n) 1)))
      ((zero? n) 0)))

;;; Here's a question:  how many steps does it take for the sequence
;;; starting with 4 to converge?

;;; It turns out that one cannot prove Goodstein's theorem using Peano
;;; arithmetic.  You have to go outside of PA to set theory to prove
;;; that the sequence always converges.  This is an example of a
;;; `natural' independence: a theorem that is true, but not provable
;;; from the axioms.  Godel's incompleteness theorem stated that Peano
;;; arithmetic was incomplete, but his proof was based on the clever
;;; construction of a self-referential statement.  It seems like
;;; cheating.  But Godel's theorem is much more interesting.  Sure you
;;; *can* construct weird non-provable statements, but Godel was
;;; saying that there exist an infinite number of non-provable
;;; statements (he was just using self-reference as a technique for
;;; constructing an example).


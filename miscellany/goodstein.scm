
(define (goodstein seed)
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

  (do ((base 2   (+ base 1))
       (n   seed (- (base-bump base n) 1)))
      ((zero? n) 0)))

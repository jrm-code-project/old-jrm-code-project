(declare (usual-integrations)
         (integrate-external "utilities"))

(define (weight-if-present has-feature? positive-examples negative-examples)
  (10log10
   (/ (* (+ (count-if has-feature? positive-examples) 1/2) 
	 (+ (length negative-examples) 1/2))
      (* (+ (count-if has-feature? negative-examples) 1/2)
	 (+ (length positive-examples) 1/2)))))

(define (weight-if-absent has-feature? positive-examples negative-examples)
  (10log10
   (/ (* (+ (count-if-not has-feature? positive-examples) 1/2)
	 (+ (length negative-examples) 1/2))
      (* (+ (count-if-not has-feature? negative-examples) 1/2)
	 (+ (length positive-examples) 1/2)))))

(define (weight has-feature? positive-examples negative-examples)
  (let ((w+ (weight-if-present has-feature? positive-examples negative-examples))
        (w- (weight-if-absent  has-feature? positive-examples negative-examples)))
    (lambda (element)
      (if (has-feature? element) w+ w-))))

(define (naive-bayesian-classifier feature-list positive-examples negative-examples)
  (let ((weighters (map (lambda (feature)
                          (weight feature positive-examples negative-examples))
                        feature-list)))
    (lambda (test-element)
      (sum (lambda (weighter)
             (weighter test-element))
           weighters))))

(define (bayesian-list-splitter classifier prior)
  (lambda (list receiver)
    (divide-list (lambda (item)
                  (positive? (+ prior (classifier item))))
                list
                receiver)))

(define (test-naive-bayesian-classifier)
  ;; return true if the code is ok.

  (define (has-feature? x)
    (lambda (y) (memq x y)))

  (let ((negative-examples
         '((the quick brown fox)
           (rabbit run run run)))
        (positive-examples
         '((the quick rabbit run and run)
           (rabbit at rest)))
        (feature-list (map has-feature?
                           '(brown fox quick rabbit rest run))))

    (= ((naive-bayesian-classifier feature-list positive-examples negative-examples)
        '(the quick rabbit rest))
       11.426675035687314)))

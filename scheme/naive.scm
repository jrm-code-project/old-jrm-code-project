(declare (usual-integrations)
	 (integrate-external "utilities"))

(define (weight-if-present has-feature? positive-examples
                                        negative-examples)
  (10log10
   (/ (* (+ (count-if has-feature? positive-examples) .5)
         (length negative-examples))
      (* (+ (count-if has-feature? negative-examples) .5)
         (length positive-examples)))))

(define (weight-if-absent has-feature? positive-examples 
                                       negative-examples)
  (10log10
   (/ (* (+ (count-if-not has-feature? positive-examples) .5)
         (length negative-examples))
      (* (+ (count-if-not has-feature? negative-examples) .5)
         (length positive-examples)))))

(define (weight has-feature? positive-examples negative-examples)
  (let ((w1 (weight-if-present has-feature? positive-examples 
			       negative-examples))
	(w2 (weight-if-absent  has-feature? positive-examples 
			       negative-examples)))
    (lambda (test-element)
      (if (has-feature? test-element) w1 w2))))

(define (naive-bayesian-classifier feature-list positive-examples 
				   negative-examples)
  (let ((weighters (map (lambda (feature)
			  (weight feature positive-examples negative-examples))
			feature-list)))

    (lambda (test-element)
      (sum (lambda (weighter)
	     (weighter test-element))
	   weighters))))

(define (bayesian-list-splitter classifier)
  (lambda (list receiver)
    (split-list (lambda (item)
		  (positive? (classifier item)))
		list
		receiver)))

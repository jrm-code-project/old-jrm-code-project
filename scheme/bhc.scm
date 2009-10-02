(declare (usual-integrations)
	 (integrate-external "c:\\jrm-code-project\\scheme\\utilities"))

;; Magic constant to determine the aggressiveness
;; of clustering.

(define *alpha* 3)

(define (gamma-ratio num den)
  ;; Compute gamma(num)/gamma(den) efficiently
  ;; with the assumption that abs(num - den) is an integer.

  (if (> den num)
      (/ 1 (gamma-ratio den num))
      (/ (gamma num) (gamma den))))

(define (bernoulli-beta alpha beta m N)
  (* (gamma-ratio (+ alpha m) alpha)
     (gamma-ratio (+ beta (- N m)) beta)
     (gamma-ratio (+ alpha beta) (+ (+ alpha beta) N))))

(define-integrable (make-tree all-terms left-child right-child)
  all-terms
  ;; Ugh.  We use the fact that leaf nodes are vectors
  ;; of length 3, so this must *not* be a vector of length 3.
  (vector 'tree left-child right-child 0))

(define-integrable (left-child tree)
  (vector-ref tree 1))

(define-integrable (right-child tree)
  (vector-ref tree 2))

(define-integrable (combine all-terms left-child right-child)
  (make-tree all-terms left-child right-child))

(define (leaf-node? thing)
  (and (vector? thing)
       (= (vector-length thing) 3)))

(define (n tree)
  (if (leaf-node? tree)
      1
      (+ (n (left-child tree))
	 (n (right-child tree)))))

(define (d tree)
  (if (leaf-node? tree)
      *alpha*
      (+ (* *alpha* (gamma (n tree)))
	 (* (d (left-child tree))
	    (d (right-child tree))))))

(define (pi tree)
  (if (leaf-node? tree)
      1
      (/ (* *alpha* (gamma (n tree))) 
	 (d tree))))

;; Determines the likelihood of the bernoulli distribution.
;; These values make it heavily skewed towards zero.
(define (alpha term) term 1)  ; 1/16
(define (beta term) term 1)   ; 15/16

(define (ph1 all-terms tree)
  (let ((term-table (make-eq-hash-table))
	(n_k (n tree)))

    (define (do-term term)
      (hash-table/modify! term-table term 1+ 0))

    (define (scan-data-point dp)
      (for-each do-term (data-point/keys dp)))

    (define (scan-tree tree)
      (if (leaf-node? tree)
	  (scan-data-point tree)
	  (begin (scan-tree (left-child tree))
		 (scan-tree (right-child tree)))))

    (scan-tree tree)
    (product (lambda (term)
	       (bernoulli-beta (alpha term) (beta term)
			       (hash-table/get term-table term 0)
			       n_k))
	     all-terms)))

(define (p all-terms tree)
  (if (leaf-node? tree)
      (ph1 all-terms tree)
      (let ((pi_k (pi tree)))
	(+ (* pi_k (ph1 all-terms tree))
	   (* (- 1 pi_k)
	      (p all-terms (left-child tree))
	      (p all-terms (right-child tree)))))))

(define (r all-terms tree)
  (/ (* (pi tree) (ph1 all-terms tree))
     (p all-terms tree)))

(define (all-terms trees)
  (let ((term-table (make-eq-hash-table)))

    (define (do-term term)
      (hash-table/modify! term-table term 1+ 0))

    (define (do-data-point dp)
      (for-each do-term (data-point/keys dp)))

    (define (do-tree tree)
      (if (leaf-node? tree)
	  (do-data-point tree)
	  (begin (do-tree (left-child tree))
		 (do-tree (right-child tree)))))

    (for-each do-tree trees)
    (fold-left (lambda (termlist entry)
		 (if (> (cdr entry) 1)
		     (adjoin/eq termlist (car entry))
		     termlist))
	       '()
	       (hash-table->alist term-table))))

(define (print-tree terms tree)
  (define (indent n)
    (do ((i 0 (+ i 1))) ((>= i n)) (write-char #\space)))

  (define (dashes n)
    (do ((i 0 (+ i 1))) ((>= i n)) (write-char #\-)))

  (define (do-it level tree)
    (if (leaf-node? tree)
	(begin (indent level) (display tree)
	       )
	(begin (dashes level)
	       (display " ")
	       (display (probability->log-odds (r terms tree)))
	       (newline)
	       (let ((rl (r terms (left-child tree)))
		     (rr (r terms (right-child tree))))
		 (if (> rl rr)
		     (begin
		       (do-it (+ level 2) (left-child tree))
		       (do-it (+ level 2) (right-child tree)))
		     (begin
		       (do-it (+ level 2) (right-child tree))
		       (do-it (+ level 2) (left-child tree)))))
	       (dashes level)))
    (newline))
  (do-it 0 tree))

(define (cluster-data-points terms data-points)

    (define (step trees)
      (display "; Step ")
      (display (length trees))
      (newline)
      (let ((best-a #f)
	    (best-b #f)
	    (best-combined #f)
	    (best-r -1))

	(define (test-combination a b)
	  (let* ((combined-tree (combine terms a b))
		 (r_k           (r terms combined-tree)))
	    (display "; Test combination")
	    (newline)
	    (display "; a = ")
	    (display a)
	    (newline)
	    (display "; b = ")
	    (display b)
	    (newline)
	    (display "; r = ")
	    (display r_k)
	    (newline)
	    (if (> r_k best-r)
		(begin (set! best-a a)
		       (set! best-b b)
		       (set! best-combined combined-tree)
		       (set! best-r r_k)))))

	(for-each-list-pairs test-combination trees)
	(display "; Best score = ")
	(display best-r)
	(newline)
	(display "; Left = ")
	(display best-a)
	(newline)
	(display "; Right = ")
	(display best-b)
	(newline)
	(cons best-combined
	      (delete best-a (delete best-b trees)))))

    (define (iter trees)
      (cond ((and (pair? trees)
		  (pair? (cdr trees)))
	     (iter (step trees)))
	    ((pair? trees) (begin
			     (display "; Answer")
			     (newline)
			     (car trees)))
	    (else (error "Bogus trees"))))

    (iter data-points))

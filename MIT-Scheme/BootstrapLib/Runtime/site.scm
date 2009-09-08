(declare (usual-integrations))
;; Site specific initialization

(define-integrable (log->10log10 x)
  (flo:* 4.3429448190325175 x))

(define-integrable (flo:10log10 x)
  (log->10log10 (flo:log x)))

(define-integrable (10log10 x)
  (flo:10log10 (exact->inexact x)))

(define (finish-cold-load)
  (define (time-phase phase thunk)
    (let ((start-time (runtime)))
      (let ((answer (thunk)))
	(for-each write-string 
		  (list "; " phase " took "
			(number->string (- (runtime) start-time))
			" ("
			(number->string
			 (10log10 (- (runtime) start-time)))
			")."))
	(newline)
	answer)))

  (display "\\BootstrapLib\\runtime\\site")

  (time-phase
   "Load rb-tree"
   (lambda () (load-option 'rb-tree)))

  (time-phase
   "Load sf"
   (lambda () (load-option 'sf)))

  (identify-world)
  )

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

  (display "\\StagingLib\\runtime\\site")

  (time-phase
   "Load rb-tree"
   (lambda () (load-option 'rb-tree)))

  (time-phase
   "Load sf"
   (lambda () (load-option 'sf)))

  (time-phase
   "Load star-parser"
   (lambda () (load-option '*parser)))

  (time-phase
   "Load cref"
   (lambda () (load-option 'cref)))

  (time-phase
   "Syntax runtime"
   (lambda ()
     (with-working-directory-pathname 
      "C:\\jrm-code-project\\Mit-scheme\\lib\\Runtime\\"
      (lambda () 
	(load "runtime.sf")
	(sf "site.scm")))))

  (time-phase
   "Syntax sf"
   (lambda ()
     (with-working-directory-pathname 
      "C:\\jrm-code-project\\Mit-scheme\\lib\\Sf\\"
      (lambda () (load "sf.sf")))))

  (time-phase
   "Syntax star parser"
   (lambda ()
     (with-working-directory-pathname 
      "C:\\jrm-code-project\\Mit-scheme\\lib\\star-parser\\"
      (lambda () 
	(fluid-let ((sf/default-syntax-table (->environment '(RUNTIME))))
	  (sf-directory ".")
	  (cref/generate-constructors "parser" 'ALL))))))

  (time-phase
   "Syntax cref"
   (lambda ()
     (with-working-directory-pathname 
      "C:\\jrm-code-project\\Mit-scheme\\lib\\Cref\\"
      (lambda () (load "cref.sf")))))

   (identify-world)
  )

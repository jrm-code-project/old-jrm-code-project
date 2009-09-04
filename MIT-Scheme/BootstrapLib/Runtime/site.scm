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
   "Load sf"
   (lambda () (load-option 'sf)))

  (identify-world)

;  (time-phase 
;   "Load rbtree"
;   (lambda ()
;     (with-working-directory-pathname
;      "C:\\jrm-code-project\\mit-scheme\\runtime\\"
;      (lambda () 
;	(((access standard-option-loader (package/environment (find-package '(runtime options))))
;	  '(runtime rb-tree) #f "C:\\jrm-code-project\\MIT-Scheme\\runtime\\rbtree.bin"))))))

;  (time-phase 
;   "Load cref"
;   (lambda ()
;     (with-working-directory-pathname
;      "C:\\jrm-code-project\\MIT-scheme\\cref\\"
;      (lambda () 
;	(load "make")))))

;  (time-phase 
;   "Load sf"
;   (lambda ()
;     (with-working-directory-pathname
;	 "C:\\jrm-code-project\\mit-scheme\\sf\\"
;       (lambda () (load "make")))))

;  (time-phase 
;   "sf runtime"
;   (lambda ()
;     (with-working-directory-pathname
;	 "C:\\jrm-code-project\\mit-scheme\\runtime\\"
;       (lambda () (load "runtime.sf")))))

;  (time-phase
;   "sf cref1"
;   (lambda ()
;     (with-working-directory-pathname
;      "C:\\jrm-code-project\\MIT-scheme\\Cref1\\"
;      (lambda () (load "cref.sf")))))

;  (time-phase
;   "sf sf"
;   (lambda ()
;     (with-working-directory-pathname
;	 "C:\\jrm-code-project\\mit-scheme\\sf2\\"
;       (lambda () (load "sf.sf")))))

;  (time-phase
;   "CTakTest"
;   (lambda ()
;     (with-working-directory-pathname
;      "C:\\jrm-code-project\\mit-scheme\\Benchmark\\"
;      (lambda ()
;	(load "ctak.bin" user-initial-environment)
;	 (eval '(let ((time-phase
;		      (lambda (phase thunk)
;			(let ((start-time (runtime)))
;			  (let ((answer (thunk)))
;			    (for-each write-string (list "; " phase " took " (number->string (10log10 (- (runtime) start-time)))))
;			    (newline)
;			    answer)))))
;		 (time-phase 
;		  "Run ctak"
;		  (lambda ()
;		    (ctaktest))))
;	       user-initial-environment)))))

  ;(time-phase 
;   "Total Earley Boyer"
;   (lambda ()
;     (with-working-directory-pathname
;	 "C:\\jrm-code-project\\mit-scheme\\Benchmark\\"
;       (lambda ()
;	 (time-phase
;	  "Load Earley"
;	  (lambda () (load "earley.bin" user-initial-environment)))
;	 (time-phase
;	  "Load Nboyer"
;	  (lambda () (load "nboyer.bin" user-initial-environment)))
;	 (eval '(let ((time-phase
;		      (lambda (phase thunk)
;			(let ((start-time (runtime)))
;			  (let ((answer (thunk)))
;			    (for-each write-string (list "; " phase " took " (number->string (10log10 (- (runtime) start-time)))))
;			    (newline)
;			    answer)))))
;		 (time-phase 
;		  "Run earley-boyer"
;		  (lambda ()
;		    (earley-boyer-weird))))
;	       user-initial-environment)))))

;  (time-phase 
;   "Total"
;   (lambda ()
;     (with-working-directory-pathname 
;      "C:\\Home\\jrm\\larceny\\testsuite\\gc\\"
;      (lambda ()
;	(time-phase 
;	 "Load dynamic"
;	 (lambda () (load "dynamic.bin" user-initial-environment)))
;	(load "sboyer.bin" user-initial-environment)
;	(load "lattice.bin" user-initial-environment)
;	(eval '(let ((time-phase
;		      (lambda (phase thunk)
;			(let ((start-time (runtime)))
;			  (let ((answer (thunk)))
;			    (for-each write-string (list "; " phase " took " (number->string (10log10 (- (runtime) start-time)))))
;			    (newline)
;			    answer)))))
;		 (time-phase 
;		  "Run dynamic"
;		  (lambda ()
;		    (i!)
;		    (let ((foo (dynamic-parse-forms *forms*)))
;		      (normalize-global-constraints!)
;		      (reset-counters!)
;		      (tag-ast*-show foo)
;		      (counters-show))))
;		 (setup-boyer)
;		 (time-phase
;		  "Run boyer"
;		  (lambda ()
;		    (test-boyer 1)))
;		 (time-phase
;		  "Run lattice"
;		  (lambda ()
;		    (let* ((l2 (lb0))
;			   (l3 (maps l2 l2))
;			   (l4 (maps l3 l3)))
;		      (list (count-maps l3 l4)
;			    (count-maps l4 l3))))))
;	      user-initial-environment)))))

)

;;

;(define (doit)
;  (let ((start-time (runtime)))
;    (((access standard-option-loader (package/environment (find-package '(runtime options))))
;      '(runtime wt-tree) #f "C:\\jrm-code-project\\MIT-Scheme\\runtime\\wttree.bin"))

;    (with-working-directory-pathname "C:\\jrm-code-project\\scheme\\"
;				     (lambda ()
;				       (load "macros.bin" user-initial-environment)
;				       (load "utilities.bin" user-initial-environment)))

;    (with-working-directory-pathname "C:\\users\\jmarshall\\"
;				     (lambda ()
;				       (load "bhc2a.bin" user-initial-environment)
;				       (load "bhc6.bin" user-initial-environment)
;				       ((access initialize! user-initial-environment))
;                                       (load "c:\\documents and settings\\jmarshall\\xaa.bin" user-initial-environment)
;  				       ((access canonicalize-entries! user-initial-environment))
;  				       ((access cleanup user-initial-environment))))

;    ; ((access load-entries user-initial-environment))
;    (let ((end-time (runtime)))
;      (display "; Took ")
;      (display (- end-time start-time)))))

;    (((access standard-option-loader (package/environment (find-package '(runtime options))))
;      '(runtime rb-tree) #f "C:\\jrm-code-project\\MIT-Scheme\\runtime\\rbtree.bin"))

;    (with-working-directory-pathname "C:\\jrm-code-project\\MIT-Scheme\\cref\\"
;				     (lambda ()
;				       (load "make")))

;    (with-working-directory-pathname "C:\\jrm-code-project\\MIT-Scheme\\sf\\"
;				     (lambda ()
;				       (load "make")))

;;(load-entries)
;    (let ((end-time (runtime)))
;      (display "; Took ")
;      (display (- end-time start-time))))
;  ;(with-working-directory-pathname
;  ; "C:\\jrm-code-project\\MIT-Scheme\\sf\\"
;  ;  (lambda () (load "sf.sf")))
;  )



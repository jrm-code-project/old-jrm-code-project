(declare (usual-integrations))

(define-structure (durable-store
		   (conc-name durable-store/)
		   (constructor %make-durable-store (file output-port input-port)))
  (file #f read-only #t)
  (output-port #f read-only #t)
  (input-port #f read-only #t))

(define (make-durable-store file)
  (let* ((iport (if (file-exists? file)
		    (open-binary-input-file file)
		    #f))
	 (oport (open-binary-output-file file #t)))
    (if (not iport)
	(begin 
	  (write-string
	   ";;; A persistent store.  Do not edit."
	   oport)
	  (newline oport)))
    (%make-durable-store 
     file
     oport
     iport)))

(define (close-durable-store store)
  (close-port (durable-store/output-port store))
  (if (durable-store/input-port store)
      (close-port (durable-store/input-port store))))


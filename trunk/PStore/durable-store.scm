;;; -*- Mode: Scheme -*-

;;; Durable store abstraction.
(declare (usual-integrations))

(define (write-commit-record! durable-store write-contents!)
  (write-record! durable-store 'COMMIT 1 write-contents!))

(define (write-branch-record! durable-store write-contents!)
  (write-record! durable-store 'BRANCH 1 write-contents!))

(define (write-leaf-record! durable-store write-contents!)
  (write-record! durable-store 'LEAF 1 write-contents!))

(define (write-object-record! durable-store write-contents!)
  (write-record! durable-store 'OBJECT 1 write-contents!))

(define (write-record! durable-store record-type record-version write-contents!)
  (let* ((output-port (durable-store/output-port durable-store))
	 (address (port-position output-port)))
    (write-char #\( output-port)
    (write record-type output-port)
    (write-char #\space output-port)
    (write record-version output-port)
    (write-char #\space output-port)
    (write-contents! output-port)
    (write-char #\) output-port)
    (newline output-port)
    address))

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
	  (newline oport))
	(find-commit-record iport (port-position oport)))
    (%make-durable-store 
     file
     oport
     iport)))

(define (close-durable-store store)
  (close-port (durable-store/output-port store))
  (if (durable-store/input-port store)
      (close-port (durable-store/input-port store))))


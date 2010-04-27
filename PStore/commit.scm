;;; -*- Mode: scheme -*-
(declare (usual-integrations))

(define (commit durable-store object-map)
  (display "calling commit, I think.")(newline)
  (write-commit-record!
   durable-store
   (lambda (address oport)
     (display "committing, I think.")(newline)
     (write (object-map/address object-map) port))))

(define (find-commit-record input-port limit)
  (define (scan best count line)
    (cond ((eof-object? line) 
	   (display "Scanned ")
	   (display count)
	   (display " lines.")
	   best)
	  ((string-prefix? "(commit " line)
	   (display "Found one at line")
	   (display line)
	   (newline)
	   (scan line (+ count 1) (read-line input-port)))
	  (else
	   (scan best (+ count 1) (read-line input-port)))))
  (scan #f 0 (read-line input-port)))


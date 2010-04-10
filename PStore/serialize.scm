;;; -*- Mode: Scheme; keyword-style: prefix -*-
;;; A truly simple-minded serializer.

(declare (usual-integrations))

(define-generic serialize (durable-store object))

(define-method serialize (durable-store (object <number>))
  (primitive-serialize durable-store object))

(define-method serialize (durable-store (object <string>))
  (primitive-serialize durable-store object))

(define-method serialize (durable-store (object <symbol>))
  (primitive-serialize durable-store object))

(define (primitive-serialize durable-store object)
  (call-with-primitive-serialization 
   durable-store
   (lambda (oport)
     (write (list (list 
		   ;; tag
		   (class-name (object-class object))
		   ;; version
		   1)
		  object) oport))))

(define (call-with-primitive-serialization durable-store receiver)
  (let* ((oport (durable-store/output-port durable-store))
	 (address (port-position oport)))
    (receiver oport)
    (newline oport)
    address))

;;; Deserialize

(define *deserialiation-registry* (make-eq-hash-table))

(define (deserialize durable-store address)
  (let ((iport (durable-store/input-port durable-store)))
    (set-port-position! iport address)
    (let ((record (read iport)))
      (if (pair? record)
	  (let ((deserializer-spec (car record)))
	    (if (pair? deserializer-spec)
		(let ((vector (hash-table/get *deserialiation-registry* 
					      (car deserializer-spec)
					      #f)))
		  (if (not vector)
		      (error "No deserializers for" (car deserializer-spec))
		      (apply (vector-ref vector (cadr deserializer-spec))
			     (cdr record))))
		(error "Bad record format, first element not a list." record)))
	  (error "Bad record format, not a list." record)))))

(define (no-deserializer key)
  (lambda (version)
    (lambda args
      (error "No deserializer for this version." key version))))

(define (register-deserializer! key version function)
  (let ((vector (hash-table/get *deserialiation-registry* key #f)))
    (if (not vector)
	(let ((new-vector (make-initialized-vector (+ version 1) (no-deserializer key))))
	  (hash-table/put! *deserialiation-registry* key new-vector)
	  (set! vector new-vector)))
    (if (>= version (vector-length vector))
	(let ((new-vector (make-initialized-vector (+ version 1) (no-deserializer key))))
	  (subvector-move-right! vector 0 (vector-length vector) new-vector 0)
	  (hash-table/put! *deserialiation-registry* key new-vector)
	  (set! vector new-vector)))
    (vector-set! vector version function)))

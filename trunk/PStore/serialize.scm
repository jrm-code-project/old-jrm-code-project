;;; -*- Mode: Scheme; keyword-style: prefix -*-
;;; A truly simple-minded serializer.

(declare (usual-integrations))

(define-generic serialize-object (durable-store object-id object))

(define-method serialize-object (durable-store object-id (object <number>))
  (primitive-serialize-object durable-store object-id object))

(define-method serialize-object (durable-store object-id (object <string>))
  (primitive-serialize-object durable-store object-id object))

(define-method serialize-object (durable-store object-id (object <symbol>))
  (primitive-serialize-object durable-store object-id object))

(define (primitive-serialize-object durable-store object-id object)
  (call-with-primitive-serialization 
   durable-store
   (lambda (address oport)
     (declare (ignore address))
     (write (make-object-record
	     object-id
	     (class-name (object-class object))
	     1
	     object) oport))))

(define (make-object-record object-id class version object)
  (list 'object object-id class version object))

(define (call-with-primitive-serialization durable-store receiver)
  (let* ((oport (durable-store/output-port durable-store))
	 (address (port-position oport)))
    (receiver address oport)
    (newline oport)
    address))

;;; Deserialize

(define *deserialiation-registry* (make-eq-hash-table))

(define (deserialize durable-store address)
  (let ((iport (durable-store/input-port durable-store)))
    (set-port-position! iport address)
    (let ((record (read iport)))
      (if (pair? record)
	  (case (car record)
	    ((object) (deserialize-object-record record))
	    ((leaf) (deserialize-ptree-leaf address record))
	    ((branch-a) (deserialize-ptree-branch-a address record))
	    ((branch-b) (deserialize-ptree-branch-b address record))
	    (else (error "Unrecognized record" (car record))))
	  (error "Bad record format, not a list." record)))))

(define (deserialize-object-record record)
  (let ((object-id (second record))
	(object-type (third record))
	(version (fourth record)))
    (let ((vector (hash-table/get *deserialiation-registry* 
				  object-type
				  #f)))
      (if (not vector)
	  (error "No deserializers for" object-type)
	  (apply (vector-ref vector version)
		 object-id
		 (cdddr record))))))

;	  (let ((deserializer-spec (car record)))
;	    (if (pair? deserializer-spec)
;		  (if (not vector)
;		      (error "No deserializers for" (car deserializer-spec))
;		      (apply (vector-ref vector (cadr deserializer-spec))
;			     id
;			     address
;			     (cdr record))))
;		(error "Bad record format, first element not a list." record)))
;	  (error "Bad record format, not a list." record)))))))

(define (no-deserializer key)
  (lambda (version)
    (lambda args
      (declare (ignore args))
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

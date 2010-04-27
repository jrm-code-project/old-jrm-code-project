;;; -*- Mode: Scheme; keyword-style: prefix -*-
;;; A truly simple-minded serializer.

(declare (usual-integrations))

(define-generic serialize-object (durable-store object-id object))

(define-method serialize-object (durable-store object-id (object <number>))
  (standard-serialize-object durable-store object-id object))

(define-method serialize-object (durable-store object-id (object <string>))
  (standard-serialize-object durable-store object-id object))

(define-method serialize-object (durable-store object-id (object <symbol>))
  (standard-serialize-object durable-store object-id object))

(define (standard-serialize-object durable-store object-id object)
  (write-object-record!
   durable-store
   (lambda (port)
     (write object-id port)
     (write-char #\space port)
     (write (class-name (object-class object)) port)
     (write-char #\space port)			
     (write 1 port)
     (write-char #\space port)
     (write object port))))

;;; Deserialize

(define *deserialiation-registry* (make-eq-hash-table))

(define (deserialize durable-store address)
  (let ((iport (durable-store/input-port durable-store)))
    (set-port-position! iport address)
    (let ((record (read iport)))
      (if (pair? record)
	  (case (car record)
	    ((commit) (deserialize-commit address record))
	    ((branch) (deserialize-ptree-branch address record))
	    ((leaf) (deserialize-ptree-leaf address record))
	    ((object) (deserialize-object-record record))
	    (else (error "Unrecognized record" (car record))))
	  (error "Bad record format, not a list." record)))))

(define (deserialize-object-record record)
  (let ((record-version (second record))
	(object-id (third record))
	(object-type (fourth record))
	(version (fifth record)))
    (let ((vector (hash-table/get *deserialiation-registry* 
				  object-type
				  #f)))
      (if (not vector)
	  (error "No deserializers for" object-type)
	  (apply (vector-ref vector version)
		 object-id
		 (cddddr record))))))

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

(declare (usual-integrations))

(define quotation (list 4 'score "and" 7.0 "years" "ago"
			"our" "fathers" "brought" "forth" "on" "this" "continent" "a" "new" "nation"))

(define (test-create)
  (close-all-open-files)
  (if (file-exists? "c:\\tmp\\store.txt") (delete-file  "c:\\tmp\\store.txt"))
  (let ((s (make-durable-store "c:\\tmp\\store.txt")))
    (do ((i 0 (+ i 1))
	 (oid 3 (+ oid 3))
	 (object-map (object-map/create s) (object-map/add object-map oid i)))
	((>= i 100)
	 (do ((tail quotation (cdr tail))
	      (oid  0 (+ oid 1))
	      (object-map object-map (object-map/add object-map oid (car tail))))
	     ((null? tail)
	      (close-durable-store s)
	      (object-map/address object-map)))))))

(define (test-restore)
  (close-all-open-files)
  (register-standard-deserializers!)
  (let* ((s (make-durable-store  "c:\\tmp\\store.txt"))
	(o (object-map/recover s 109442)))
    (do ((i 0 (+ i 1)))
	((>= i (length quotation)) #f)
      (display (object-map/lookup o i))
      (newline))))

(define (register-standard-deserializers!)
  (set! *deserialiation-registry* (make-eq-hash-table))
  (register-deserializer! '<fixnum> 1 identity-procedure)
  (register-deserializer! '<flonum> 1 identity-procedure)
  (register-deserializer! '<string> 1 identity-procedure)
  (register-deserializer! '<symbol> 1 identity-procedure)
  (register-deserializer! '<ptree-leaf> 1 deserialize-ptree-leaf)
  (register-deserializer! '<ptree-branch> 1 deserialize-ptree-branch))





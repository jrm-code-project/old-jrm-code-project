;;; -*- Mode: Scheme -*-

(declare (usual-integrations))

;;; Feel free to copy or adapt this code as you wish.


;;;; A persistent weight-balanced tree abstraction.

(define-structure (persistent-tree
		   (conc-name persistent-tree/)
		   (constructor make-persistent-tree (left-child-address
						       right-child-address
						       %weight
						       %address
						       left-child
						       right-child
						       object-map-entry)))

  ;; Address of left child of the tree.
  ;; This field is persistent.
  (left-child-address #f read-only #t)

  ;; Address of right child of the tree.
  ;; This field is persistent.
  (right-child-address #f read-only #t)

  ;; Weight of this tree.
  ;; This field is persistent.
  (%weight #f read-only #t)

  ;; Where the persistent version is in the durable store.
  ;; This field is transient and reconstructed upon deserialization.
  (%address #f read-only #t)

  ;; Cached left child.
  ;; A transient copy of the deserialized left child.
  (left-child #f read-only #t)

  ;; Cached right child.
  ;; A transient copy of the deserialized right child.
  (right-child #f read-only #t)

  ;; The object map entry stored at the root of this tree.
  (object-map-entry #f read-only #t))

(define (persistent-tree/address tree)
  (if (null? tree) 0 (persistent-tree/%address tree)))

(define (persistent-tree/weight tree)
  (if (null? tree) 0 (persistent-tree/%weight tree)))

(define (persistent-tree/add durable-store root new-entry)
  (if (null? root)
      (persistent-tree/singleton durable-store new-entry)
      (let ((root-entry (persistent-tree/object-map-entry root))
	    (left-child (persistent-tree/left-child root))
	    (right-child (persistent-tree/right-child root)))
	(cond ((< (object-map-entry/object-id new-entry)
		  (object-map-entry/object-id root-entry))
	       (persistent-tree/t-join 
		durable-store 
		(persistent-tree/add durable-store left-child new-entry)
		right-child
		root-entry))
	      ((< (object-map-entry/object-id root-entry)
		  (object-map-entry/object-id new-entry))
	       (persistent-tree/t-join
		durable-store 
		left-child
		(persistent-tree/add durable-store right-child new-entry)
		root-entry))
	      (else
	       (persistent-tree/n-join durable-store 
				       left-child right-child
				       new-entry))))))

(define (persistent-tree/descend object-id current-tree best-tree)
  (cond ((null? current-tree) best-tree)
	((< object-id (object-map-entry/object-id (persistent-tree/object-map-entry current-tree)))
	 (persistent-tree/descend object-id (persistent-tree/left-child current-tree) best-tree))
	(else
	 (persistent-tree/descend object-id (persistent-tree/right-child current-tree) current-tree))))

(define (persistent-tree/find-entry root object-id)
  (let ((best-tree (persistent-tree/descend object-id root '())))
    (if (null? best-tree)
	#f
	(let ((entry (persistent-tree/object-map-entry best-tree)))
	  (if (< (object-map-entry/object-id entry) object-id)
	      #f
	      entry)))))

(define (recover-persistent-tree durable-store address)
  (if (zero? address)
      '()
      ;; Deserialize the components of the tree that are persistent.
      ;; The other slots in the structure are computed.  See how
      ;; persistent-tree/singleton and persistent-tree/n-join do it.
      (let ((info (deserialize durable-store address)))
	(if (not (pair? info))
	    (error "Bad info for persistent-tree.")
	    (let ((weight (first info))
		  (object-id (second info))
		  (object-address (third info)))
	      (if (= weight 1);; leaf node
		  (make-persistent-tree 0
					0
					weight
					address
					'()
					'()
					(%make-object-map-entry object-id object-address))
		  (let* ((left-child-address (second info))
			 (right-child-address (third info))
			 (object-id (fourth info))
			 (object-address (fifth info)))
		    (make-persistent-tree left-child-address
					   right-child-address
					   weight
					   address
					   (recover-persistent-tree durable-store left-child-address)
					   (recover-persistent-tree durable-store right-child-address)
					   (%make-object-map-entry object-id object-address)))))))))

(define (persistent-tree/t-join durable-store left-child right-child entry)
  (let ((l.n (persistent-tree/weight left-child))
        (r.n (persistent-tree/weight right-child)))
    (cond ((< (+ l.n r.n) 2)
	   (persistent-tree/n-join durable-store left-child right-child entry))

	  ((> r.n (* 5 l.n))
	   (persistent-tree/l-join durable-store left-child right-child entry))

	  ((> l.n (* 5 r.n))
	   (persistent-tree/r-join durable-store left-child right-child entry))

	  (else
	   (persistent-tree/n-join durable-store left-child right-child entry)))))

;;; These next two routines write records in the durable store.
(define (persistent-tree/singleton durable-store entry)
  (let* ((weight 1)
	 ;; Serialize the weight of this node and the
	 ;; persistent information from the object-map-entry.
	 ;; (the object-id and the address).
	 (address (serialize durable-store 
			     (list weight
				   (object-map-entry/object-id entry)
				   (object-map-entry/object-address entry)))))
    (make-persistent-tree 0 0 weight address '() '() entry)))

(define (persistent-tree/n-join durable-store left-child right-child entry)
  (if (and (null? left-child)
	   (null? right-child))
      (persistent-tree/singleton durable-store entry)
      (let* ((weight (+ 1
			(persistent-tree/weight left-child)
			(persistent-tree/weight right-child)))
	     (left-child-address (persistent-tree/address left-child))
	     (right-child-address (persistent-tree/address right-child))
	     ;; Serialize the weight of this node, the addresses of the
	     ;; left and right child, and the persistent information 
	     ;; from the object-map-entry (the object-id and the address).
	     (address (serialize 
		       durable-store
		       (list weight left-child-address right-child-address
			     (object-map-entry/object-id entry)
			     (object-map-entry/object-address entry)))))
	(make-persistent-tree left-child-address
			      right-child-address
			      weight
			      address
			      left-child
			      right-child
			      entry))))

(define (persistent-tree/l-join durable-store left-child right-child entry)
  (if (< (persistent-tree/weight (persistent-tree/left-child right-child))
	 (persistent-tree/weight (persistent-tree/right-child right-child)))
      (persistent-tree/single-l durable-store left-child right-child entry)
      (persistent-tree/double-l durable-store left-child right-child entry)))

(define (persistent-tree/single-l durable-store x r entry)
  (persistent-tree/n-join 
   durable-store 
   (persistent-tree/n-join durable-store 
			   x (persistent-tree/left-child r) entry)
   (persistent-tree/right-child r)
   (persistent-tree/object-map-entry r)))

(define (persistent-tree/double-l durable-store x r entry)
  (let ((r.l (persistent-tree/left-child r)))
    (persistent-tree/n-join 
     durable-store
     (persistent-tree/n-join durable-store
			     x
			     (persistent-tree/left-child  r.l)
			     entry)
     (persistent-tree/n-join durable-store
			     (persistent-tree/right-child r.l)
			     (persistent-tree/right-child r)
			     (persistent-tree/object-map-entry r))
     (persistent-tree/object-map-entry r.l))))

(define (persistent-tree/r-join durable-store left-child right-child entry)
  (if (< (persistent-tree/weight (persistent-tree/right-child left-child))
	 (persistent-tree/weight (persistent-tree/left-child left-child)))
      (persistent-tree/single-r durable-store left-child right-child entry)
      (persistent-tree/double-r durable-store left-child right-child entry)))

(define (persistent-tree/single-r durable-store l z entry)
  (persistent-tree/n-join
   durable-store
   (persistent-tree/left-child l)
   (persistent-tree/n-join durable-store
			   (persistent-tree/right-child l)
			   z
			   entry)
   (persistent-tree/object-map-entry l)))

(define (persistent-tree/double-r durable-store l z entry)
  (let ((l.r (persistent-tree/right-child  l)))
    (persistent-tree/n-join 
     durable-store
     (persistent-tree/n-join durable-store
			     (persistent-tree/left-child  l)
			     (persistent-tree/left-child  l.r)
			     (persistent-tree/object-map-entry l))
     (persistent-tree/n-join durable-store
			     (persistent-tree/right-child l.r)
			     z
			     entry)
     (persistent-tree/object-map-entry l.r))))

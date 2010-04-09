;;; -*- Mode: Scheme -*-

;;; Feel free to copy or adapt this code as you wish.

(declare (usual-integrations))

;;;; Object map abstraction.

(define-structure (object-map-entry
		   (type list)
		   (conc-name object-map-entry/)
		   (constructor %make-object-map-entry
				(object-id address)))
  (object-id #f read-only #t)
  (address #f read-only #t)

  ;; *** A slot holding a cached value will greatly improve
  ;;     performance!

  )

(define-structure (object-map
		   (conc-name object-map/)
		   (constructor make-object-map
				(durable-store root)))
  (durable-store #f read-only #t)
  (root #f read-only #t))

;;; OBJECT-MAP/ADD object-map object-id object
;;;  => new object map

;;; Returns a new object-map that contains all the mappings of the
;;; argument object-map in addition to a new mapping from
;;; object-id to object.  If object-id had a
;;; previous mapping, that mapping is not accessible in the result. 

;;; Object will be serialized into the durable store associated with the
;;; object-map.  The object-map itself will also be serialized into the
;;; store.

(define (object-map/add object-map object-id value)
  (let* ((store (object-map/durable-store object-map))
	 (object-address (serialize store value)))
    (make-object-map
     store
     (persistent-tree/add store 
			  (object-map/root object-map)
			  (%make-object-map-entry object-id object-address)))))

;;; object-map/address object-map
;;;  => a durable address

;;; Returns the durable address at which the object-map was serialized.
(define (object-map/address object-map)
  (persistent-tree/tree-address (object-map/root object-map)))


;;; OBJECT-MAP/CREATE durable-store
;;;  => new object map with no entries

;;; Argument durable-store is forever associated with this object
;;; map.
(define (object-map/create durable-store)
  (make-object-map durable-store '()))

;;; object-map/durable-store object-map
;;;  => a durable store

;;; Returns the durable-store is associated with this object map.
;;; defined as part of defstruct.


;;; OBJECT-MAP/LOOKUP object-map object-id
;;;  => an object previously added
;;;     Error if object-id was never added.

;;; Returns the deserialized object associated with object-id.
;;; The object is deserialized from the durable-store associated with the
;;; object-map.
(define (object-map/lookup object-map object-id)
  (let ((entry (persistent-tree/find-entry (object-map/root object-map) object-id)))
    (if entry
	;; *** Rather than deserialize the same object over and over,
	;;     deserialize it once and cache the value.
	(deserialize (object-map/durable-store object-map)
		     (object-map-entry/address entry))
	(error "Object not found in map." object-id))))


;;; OBJECT-MAP/RECOVER durable-store address
;;;  => object-map or
;;;     Error if address does not conain an object map.

;;; Deserialize an object-map from a durable store at the given address.

(define (object-map/recover durable-store address)
  (make-object-map durable-store
		   (recover-persistent-tree durable-store address)))

;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002, 2003 ChangeSafe, LLC
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;;          ChangeSafe, LLC CONFIDENTIAL and PROPRIETARY material.
;;;;
;;;;          ChangeSafe, LLC
;;;;
;;;; This software and information comprise valuable intellectual
;;;; property and trade secrets of ChangeSafe, LLC, developed at
;;;; substantial expense by ChangeSafe, which ChangeSafe intends to
;;;; preserve as trade secrets.  This software is furnished pursuant
;;;; to a written license agreement and may be used, copied,
;;;; transmitted, and stored only in accordance with the terms of such
;;;; license and with the inclusion of the above copyright notice.
;;;; This software and information or any other copies thereof may not
;;;; be provided or otherwise made available to any other person.  NO
;;;; title to or ownership of this software and information is hereby
;;;; transferred.  ChangeSafe, LLC assumes no responsibility for the
;;;; use or reliability of this software.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CSF/PERSISTENT-STORE")

(proclaim (standard-optimizations))

;;; The object map maps node-ids to file offsets.
;;; It is an immutable weight-balanced tree that will be stored
;;; in the persistent file.

;;; In the file, the layout is as follows:
;;; This is little endian!!!!
;;;
;;;    NODE-ID     fixnum
;;;    NODE-INDEX  fixnum
;;;    LOCATION    A file-offset.

;;; These values are pulled in to RAM when the database is recovered.

(defconstant +node-index-scalar+ 0)

(defstruct (object-map-info
            (:conc-name object-map-info/)
            (:constructor %make-object-map-info (encoded-node-id node-index location %cached-value))
            (:copier nil)
            (:predicate object-map-info?))

  ;; Persistent slots.  These values are appended to the
  ;; file when this node is created.

  ;; this is the key to the object map node
  ;; by encoding it, we can make the persistent tree balancing
  ;; algorithm work better because sequential allocations
  ;; won't appear sequential.
  (encoded-node-id  0 :type non-negative-fixnum :read-only t)
  ;; The node-index allows us to associate numbered subnodes
  ;; within a node.  We use this to get persistent vectors.
  (node-index -1 :type fixnum :read-only t)

  ;; If we need multiple extents, it'll have to be
  ;; encoded in the object map.
  ;; (extent   0 :type non-negative-fixnum :read-only t)

  ;; Value offset.  Where in the file this value lives.
  (location 0 :type file-offset :read-only t)

  ;; The actual value.  This is either reconstructed from the database
  ;; upon recovery, or is the value that is saved.
  (%cached-value)
  )

;;; This only gives us about 8 million objects! augh!
;;; But if we go higher, we start consing bignums!
(deftype persistent-object-id () `(INTEGER 0 #.most-positive-fixnum))

(declaim (ftype (function (non-negative-fixnum) persistent-object-id) decode-node-id)
         (ftype (function (persistent-object-id) non-negative-fixnum) encode-node-id)
         (ftype (function (object-map-info) non-negative-fixnum) object-map-info/node-id)
         (inline encode-node-id decode-node-id object-map-info/node-id))

(defun decode-node-id (encoded)
  (declare ;(type non-negative-fixnum encoded)
           #.(performance-optimizations))
  ;; (mirror-bits encoded #.(integer-length most-positive-fixnum))
  (%mirror-fixnum encoded))

(defun encode-node-id (node-id)
  (declare ;(type persistent-object-id node-id)
           #.(performance-optimizations))
  ;;(%mirror-bits node-id #.(integer-length most-positive-fixnum) 0)
  (%mirror-fixnum node-id))

(defun object-map-info/node-id (info)
  (declare (type object-map-info info)
           #.(performance-optimizations))
  (decode-node-id (object-map-info/encoded-node-id info)))

(declaim (ftype (function (non-negative-fixnum non-negative-fixnum non-negative-fixnum non-negative-fixnum) boolean)
                object-map-key/compare-elements)
         (inline object-map-key/compare-elements))

(defun object-map-key/compare-elements (left-encoded-node-id
                                        left-node-index
                                        right-encoded-node-id
                                        right-node-index)
  (declare #.(performance-optimizations))
  (or (fix:< left-encoded-node-id
             right-encoded-node-id)
      (and (not (fix:< right-encoded-node-id
                       left-encoded-node-id))
           (fix:< left-node-index
                  right-node-index))))

(defun object-map-info/compare (left right)
  (declare #.(performance-optimizations))
  (object-map-key/compare-elements
   (object-map-info/encoded-node-id left)
   (object-map-info/node-index left)
   (object-map-info/encoded-node-id right)
   (object-map-info/node-index right)))

(defun object-map-info/compare-pair (left-pair right-pair)
  (declare (type cons left-pair right-pair)
           #.(performance-optimizations))
  (object-map-key/compare-elements
   (car left-pair) (cdr left-pair)
   (car right-pair) (cdr right-pair)))

(defun object-map-info/key-pair (object-map-info)
  (declare (type object-map-info object-map-info)
           #.(performance-optimizations))
  (cons (object-map-info/encoded-node-id object-map-info)
        (object-map-info/node-index object-map-info)))

(defun object-map-info/emit (info stream)
  (%write-fixnum (object-map-info/encoded-node-id  info) stream)
  (%write-fixnum (object-map-info/node-index       info) stream)
  (%write-unsigned32 (object-map-info/location     info) stream)
  ;; Do not write the actual object.  It should already have
  ;; been written.
  )

(defun object-map-info/read (symbol-table stream)
  (let* ((encoded-node-id  (read-fixnum stream))
         (node-index       (read-fixnum stream))
         (location         (read-unsigned32 stream))
         (value            (unless (zerop location)
                             (deserialize stream symbol-table location))))
    (%make-object-map-info encoded-node-id node-index location value)))

(defmethod print-object ((node object-map-info) stream)
  (print-unreadable-object (node stream)
    (write-string "OBJECT-MAP-INFO " stream)
    (princ (object-map-info/node-id node) stream)
    (unless (= (object-map-info/node-index node) +node-index-scalar+)
      (write-char #\[ stream)
      (princ (object-map-info/node-index node) stream)
      (write-char #\] stream))))

(defun object-map/add (log-stream root-node node-id location value)
  "Returns a new object map by inserting an entry for <node-id>, and
   <location> into <root-node>.  The original object map is unchanged.

   The new object map will be written to <log-stream>."
  (check-type root-node (optional persistent-node))
  (check-type node-id  fixnum)
  ;; (check-type extent   fixnum)
  (check-type location file-offset)
  (persistent-node/add #'object-map-info/compare
                       log-stream
                       #'object-map-info/emit
                       (%make-object-map-info
                        (encode-node-id node-id)
                        +node-index-scalar+
                        ;; extent
                        location value)
                       root-node))

(defun object-map/add2 (log-stream root-node node-id node-index location value)
  "Returns a new object map by inserting an entry for <node-id>, and
   <location> into <root-node>.  The original object map is unchanged.

   The new object map will be written to <log-stream>."
  (check-type root-node (optional persistent-node))
  (check-type node-id  non-negative-fixnum)
  (check-type node-index fixnum)
  ;; (check-type extent   fixnum)
  (check-type location file-offset)
  (persistent-node/add #'object-map-info/compare
                       log-stream
                       #'object-map-info/emit
                       (%make-object-map-info
                        (encode-node-id node-id)
                        node-index
                        ;; extent
                        location value)
                       root-node))

;;; CRITICAL CODE
(defun object-map/find (object-map node-id)
  "Search for <node-id> in <object-map> returning the object map info."
  (declare (type persistent-node object-map)
           (type persistent-object-id node-id)
           #.(performance-optimizations))
  (let ((pnode (persistent-node/find< #'object-map-info/encoded-node-id
                                      (encode-node-id node-id)
                                      object-map)))
    (if (null pnode)
        (error 'changesafe-database-error
               :format-control "Object map node ~d not found."
               :format-arguments (list node-id))
        (persistent-node/info (the persistent-node pnode)))))

(defun object-map/find2 (object-map node-id node-index)
  "Search for <node-id> in <object-map> returning the object map info."
  (declare (type persistent-node object-map)
           (type persistent-object-id node-id)
           #.(performance-optimizations))
  (let ((pnode (persistent-node/find #'object-map-info/compare-pair
                                     #'object-map-info/key-pair
                                     (cons (encode-node-id node-id) node-index)
                                     object-map)))
    (if (null pnode)
        (error 'changesafe-database-error
               :format-control "Object map node ~d[~d] not found."
               :format-arguments (list node-id node-index))
        (persistent-node/info (the persistent-node pnode)))))

(declaim (ftype (function (object-map-info) t) object-map-info/value)
         (inline object-map-info/value))

(defun object-map-info/value (object-map-info)
  (declare (type object-map-info object-map-info)
           #.(performance-optimizations))
  (object-map-info/%cached-value object-map-info))

(defun object-map/for-each (func object-map)
  (persistent-node/for-each func object-map))

(defgeneric restore-instance (class schema persistent-store node-id node-index init-alist)
  (:documentation "Reconstruct a persistent object from a CLASS, SCHEMA, and INITARGS.")

  (:method (class schema persistent-store node-id node-index init-alist)
    (error 'changesafe-database-recovery-error
           :format-control "Could not restore object of class ~s, schema ~d"
           :format-arguments (list class schema))))

(defun restore-from-initializer (persistent-store node-id node-index initializer)
  "Invoked to re-create a persistent object from its initial form."
  (restore-instance
   (initializer/class initializer)
   (initializer/schema-version initializer)
   persistent-store
   node-id
   node-index
   (initializer/init-plist initializer)))

(defun fetch-object-map (persistent-store symbol-table stream location)
  "Given a location within an input-stream, return the object map node at
   that location.  This is used to fetch the root node of the
   object map."
  (debug-message 5 "Reloading object map.")
  ;; Fetching the root node will suck in the rest of the nodes.
  (let ((object-map
           (fetch-persistent-node symbol-table stream location #'object-map-info/read)))
    ;; Once the object map is loaded,
    ;; make a second pass to restore persistent objects from their
    ;; initializers.
    (debug-message 5 "Reconstructing persistent objects.")
    (let ((count 0))
      (object-map/for-each
       (lambda (object-map-info)
         (when (initializer? (object-map-info/value object-map-info))
           (setf (object-map-info/%cached-value object-map-info)
                 (restore-from-initializer
                  persistent-store
                  (object-map-info/node-id object-map-info)
                  (object-map-info/node-index object-map-info)
                  (object-map-info/value object-map-info)))
           (incf count)))
       object-map)
      (debug-message 4 "Reconstructed ~d persistent objects." count))
    object-map))

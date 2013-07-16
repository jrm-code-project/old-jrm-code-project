;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002 ChangeSafe, LLC
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

(in-package "CSF/CORE")

(proclaim (standard-optimizations))

(eval-when (:load-toplevel :execute)
  (export '(
            )))

(defclass mapper ()
  ((mapping-level :initarg :mapping-level
                  :initform "Unknown"
                  :reader mapper/mapping-level)
   (key :initarg :key
        :initform nil
        :reader mapper/key)
   (parent :initarg :parent
           :initform nil
           :reader mapper/parent))
  (:documentation
   "Abstract class which is the root of a mapper hierarchy of classes, all of which
     resolve individual portions of a DISTRIBUTED-IDENTIFIER.  When properly implemented,
     most mapper levels are essentially btrees.  For prototype, we have used hashtable
     representations, which are space inefficient.")
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defgeneric mapper/install-child-mapper (parent-mapper child-mapper)
  (:documentation
   "Install CHILD-MAPPER in PARENT-MAPPER.
   It is an error for the mapper to already have a child mapper with the given key."))

(defgeneric mapper/resolve (mapper did)
  (:documentation
   "Resolve a DISTRIBUTED-IDENTIFIER and return a handle to what is usually a persistent object.")
;  (:method :before (mapper did)
;    (debug-message 5 "Resolving did ~s against mapper ~s" did mapper))
  )

(defmethod distributed-object-identifier ((mapper mapper))
  "Generate a partial DID representing the portions of a distributed identifier hierarchy
   reachable from the indicated mapper."
  (let ((keys nil))
    (loop with current-map = mapper
        while current-map
        do (push (mapper/key current-map) keys)
           (setq current-map (mapper/parent current-map)))
    ;; Keys are in order, but we've pushed a nil key at the start for the root, remove it
    (list->did (cdr keys))))

(defclass unordered-mapper (mapper)
  ;; The mapping level is a debugging slot.  It lets you know if
  ;; you are looking at the root, domain, or class map.
  ((hash-table
    :initarg :hash-table
    :reader unordered-mapper/hash-table))
  (:documentation
   "An UNORDERED mapper uses a hashtable or btree to perform fast searches on its input.
      Unordered is perhaps not the best name.  We use Allegrostore primitives to avoid
      copying the whole tree into memory, so beware the DEFINE-TENN-CLASS optimizations
      for caching slot access.")
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defmethod print-object ((object unordered-mapper) stream)
  (print-unreadable-object (object stream :type t)
    (princ (mapper/mapping-level object) stream)))

(defmethod make-instance :around ((class (eql (find-class 'unordered-mapper)))
                                  &rest initargs &key size hash-table hash-test &allow-other-keys)
  (apply #'call-next-method class
         :hash-table (or hash-table
                         (if hash-test
                             (make-instance 'persistent-hash-table :size size :test hash-test)
                             (make-instance 'persistent-hash-table :size size)))
         initargs))

(defmethod mapper/install-child-mapper ((parent-mapper unordered-mapper) child-mapper)
  (let ((key (mapper/key child-mapper))
        (persistent-ht (unordered-mapper/hash-table parent-mapper)))
    (when (persistent-hash-table/gethash persistent-ht key)
      (error "Mapper of type ~s already has child mapper key ~s"
             (type-of parent-mapper) key))
    (setf (persistent-hash-table/gethash persistent-ht key) child-mapper)
    (assert (eq (persistent-hash-table/gethash persistent-ht key) child-mapper))))

(defmethod mapper/resolve ((mapper unordered-mapper) key)
  "Most mapper-resolve methods take a DID for a key, but if you call this particular
   method, it should be the object which is the key to the unordered representation,
   typically a string or a symbol."
  (persistent-hash-table/gethash (unordered-mapper/hash-table mapper) key))

(defclass ordered-mapper (mapper)
  ((instance-vector :initarg :instance-vector
                    :initform (make-instance 'persistent-vector :size 1)
                    :reader ordered-mapper/instance-vector))
  (:documentation
   "An ORDERED mapper uses a vector to perform indexed retrieval on its input.
      ORDERED is perhaps not the best name.  We use Allegrostore primitives to avoid
      copying the whole tree into memory, so beware the DEFINE-TENN-CLASS optimizations
      for caching slot access.

      For mappers referring to non-local repository contents, the ordered mapper
      may not be complete.  There may be blank entries referring to unimported entities.
      The number of entities listed by the mapper also doesn't necessarily describe
      the number of imported entries, or those which exist in the remote repository.
      It descibes only the highest numbered entry which has been imported locally.

      We might wish to use the ASTORE-VECTOR class for this.  Note that when it does
      extensions however, we must ensure that newly opened entries are initialized to NIL. ")
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defmethod print-object ((object ordered-mapper) stream)
  (print-unreadable-object (object stream :type t)
    (princ (mapper/mapping-level object) stream)))

(defmethod mapper/install-child-mapper ((parent-mapper ordered-mapper) child-mapper)
  (declare (ignore child-mapper))
  (error "MAPPER/INSTALL-CHILD-MAPPER: ordered-mapper objects do not have child mappers"))

(defmethod mapper/resolve ((mapper ordered-mapper) (key integer))
  "Most mapper-resolve methods take a DID for a key, but if you call this particular
   method, it should be NUMBER which is the key to the ORDERED representation"
  (persistent-vector-ref (ordered-mapper/instance-vector mapper) key))

(defun ordered-mapper/reserve-entry (ordered-mapper)
  (let ((pvector (ordered-mapper/instance-vector ordered-mapper)))
    (prog1 (persistent-vector-length pvector)
      (persistent-vector-push (ordered-mapper/instance-vector ordered-mapper) nil))))

(defun ordered-mapper/set-entry (ordered-mapper numeric-id instance)
  (setf (persistent-vector-ref (ordered-mapper/instance-vector ordered-mapper) numeric-id) instance))


;;;
;;; INTEGER-MAPPER
;;;

;;; Note: INTEGER-MAPPER should probably be subtype of ORDERED-MAPPER, but
;;; to do this we have to make ORDERED-MAPPER abstract, and it isn't right now.

(defclass integer-mapper ()
  ((mapping-level :initarg :mapping-level
                  :initform "Unknown"
                  :reader mapper/mapping-level)

   (pseudo-class :initarg :pseudo-class
                 :initform (error "Required initarg :pseudo-class omitted.")
                 :reader integer-mapper/pseudo-class))
  (:documentation
     "INTEGER-MAPPER defines the abstract class an interface to subtypes which map
      repository-local INTEGER values to distributed identities.  These mappers are used
      for integers which must be managed as distributed handles such as CIDs, IONS, UIDs,
      and other things.

      For example, a reference to LONDON:CID:00053 may end up as BOSTON:CID:00099.  This
      class provides and manages this mapping.  When handles managed by integer mappers
      are exported, they are exported in canonical form, such that the importing
      repository will use its mapper to provide potentially different local
      interpretations of the integers managed by these mappers.

      Which subtype of INTEGER-MAPPER you should use depends on your integer mapping
      clustering properties.  If you have largely unclustered mappings of local to remote
      integers, use the INTEGER-SEQUENCE-MAPPER subtype.  If you have clustered mappings,
      use the INTEGER-RANGE-MAPPER subtype.  INTEGER-RANGE-MAPPER can yield substantially
      faster search times and smaller representations, but only if the clustering is
      adequate.  Refer to that class for more details.

      In all cases, the INTEGER-MAPPER is designed to manage a sequentially increasing
      series of local integers.  Gaps may be supported at a later date, but they aren't
      currently supported.

      Theoretically, we could map INTEGERs the same way we map object handles in the
      mappers of the reposiotry.  The implication is that we think of INTEGER as a class
      which can be resolved and handle values are integers.  However that essentially
      means that the DISTRIBUTED-IDENTIFIER method on handles must have an entry for
      integer, and we would have to assume that integer handles are INTEGERs.  That's
      probably fine, as long as INTEGER is the ONLY integer type requiring mapping in the
      mappers (which it is, as of this writing, since IONS are managed specially).

      We know that CIDS, UIDS, and IONS are all mapped, though not necessarily in the
      same 'space' of the problem domain (for instance, IONS are local to CVI space, while
      CIDS and UIDS are global to the repository space.)  Thus we support this
      behavior with a discrete class (INTEGER-MAPPER).

      Also, the current Allegrostore implementation would stupidly allocate 12
      bytes/integer with it's vector and fixnum representations.  By having a custom
      mappers, we can reduce that to 4 bytes/integer, and potentially tailor special
      lookups, etc..  INTEGERs are also a much more prevalent object type than normal
      versioned objects, outnumbering versioned objects by probably 1000:1.  For range
      based clustered partitioning, we can reduce space requirements to a minimum.")
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defmethod print-object ((object integer-mapper) stream)
  (print-unreadable-object (object stream :type t)
    (princ (mapper/mapping-level object) stream)))

(defgeneric integer-mapper/allocate-integers (integer-mapper count repository-map &optional remote-integer)
  (:documentation
   "Allocate and return COUNT new and unique integers in the INTEGER-MAPPER for the indicated repository
    reference with the indicated REMOTE-INTEGER.  If REMOTE-INTEGER is omitted, then we're
    allocating a new local integer.

    Note that we permit NIL as the zero'th INTEGER value for remote map reference.  INTEGER
    zero is always invalid."))

(defgeneric integer-mapper/distributed-identifier (integer-mapper integer)
  (:documentation
   "This function is similar to the DISTRIBUTED-OBJECT-IDENTIFIER generic function.
    However since INTEGERs are handled by specialized mappers, and since we don't want to
    risk the need to discriminate on multiple integer interpretations of the
    DISTRIBUTED-OBJECT-IDENTIFIER generic function, you must call this function to get a DID
    for local INTEGERs mapped by a specific integer-mapper instance. "))

(defgeneric integer-mapper/resolve-distributed-identifier (integer-mapper did repository-map)
  (:documentation
   "Resolve a DID whose class is an INTEGER pseudo class, and return the local INTEGER for
    the current repository.  If the INTEGER isn't in the INTEGER-MAPPER, return NIL.

    This method is intended to be called only by REPOSITORY/RESOLVE-DISTRIBUTED-IDENTIFIER,
    REPOSITORY-MAP is the map resulting from partial resolution of DID, so that what's left
    to search for is the unique pair of repository-map/remote-integer (in the DID)
    whose index is the local INTEGER.  Without special additional indexing or search smarts,
    this is a linear search, to be used sparingly."))

(defun integer-mapper/allocate-integer (integer-mapper repository-map &optional remote-integer)
  (integer-mapper/allocate-integers integer-mapper 1 repository-map remote-integer))

(defclass integer-range-mapper-entry ()
  ((local-start  :initarg :local-start
                 :initform (error "Required initiarg :local-start omitted.")
                 :type integer
                 :reader integer-range-mapper-entry/local-start)
   (local-limit  :initarg :local-limit
                 :initform (error "Required initarg :local-limit omitted.")
                 :type integer
                 :reader integer-range-mapper-entry/local-limit)
   (remote-start :initarg :remote-start
                 :initform (error "Required initarg :remote-start omitted.")
                 :type integer
                 :reader integer-range-mapper-entry/remote-start)
   (repository-mapper :initarg :repository-mapper
                      :initform (error "Required initarg :repository-mapper omitted.")
                      :reader integer-range-mapper-entry/repository-mapper))
  (:documentation
   "Tuple which maps local integer range to remote integer range.
    This class is conceptually private to the INTEGER-RANGE-MAPPER class.")
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defmethod print-object ((object integer-range-mapper-entry) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "[~d,~d) -> [~d, ~d)"
            (integer-range-mapper-entry/local-start object)
            (integer-range-mapper-entry/local-limit object))
            (integer-range-mapper-entry/remote-start object)
            (+ (integer-range-mapper-entry/remote-start object)
               (- (integer-range-mapper-entry/local-limit object)
                  (integer-range-mapper-entry/local-start object)))))

(defmethod make-instance :around ((class (eql (find-class 'integer-range-mapper-entry)))
                                        &rest initargs &key local-start local-limit repository-mapper &allow-other-keys)
  (check-type repository-mapper mapper)
  (apply #'call-next-method class
         :local-limit (or local-limit (1+ local-start))
         initargs))

(defun integer-range-mapper-entry/remote-limit (integer-range-mapper-entry)
  "Compute the remote end-point for this entry"
  (+ (integer-range-mapper-entry/remote-start integer-range-mapper-entry)
     (- (integer-range-mapper-entry/local-limit integer-range-mapper-entry)
        (integer-range-mapper-entry/local-start integer-range-mapper-entry))))

(defun integer-range-mapper-entry/contains-local-integer (integer-range-mapper-entry local-integer)
  "Return the mapper if the integer is in the local range of this entry,return NIL otherwise."
  (if (and (>= local-integer (integer-range-mapper-entry/local-start integer-range-mapper-entry))
           (< local-integer (integer-range-mapper-entry/local-limit integer-range-mapper-entry)))
      integer-range-mapper-entry
    nil))

(defun integer-range-mapper-entry/translate-local-integer (integer-range-mapper-entry local-integer)
  "Translate the local integer into its corresponding remote value for this entry.
   It is an error if the local integer isn't mapped by this entry."
  (assert (<= (integer-range-mapper-entry/local-start integer-range-mapper-entry) local-integer))
  (assert (< local-integer (integer-range-mapper-entry/local-limit integer-range-mapper-entry)))
  (+ (integer-range-mapper-entry/remote-start integer-range-mapper-entry)
     (- local-integer (integer-range-mapper-entry/local-start integer-range-mapper-entry))))

(defun integer-range-mapper-entry/contains-remote-integer (integer-range-mapper-entry remote-integer
							   repository-mapper)
  "Return the entry if it contains the remote integer for the remost repository map.
   Return NIL if it does not."
  (if (and (eq repository-mapper (integer-range-mapper-entry/repository-mapper integer-range-mapper-entry))
	   (>= remote-integer (integer-range-mapper-entry/remote-start integer-range-mapper-entry))
	   (< remote-integer (integer-range-mapper-entry/remote-limit integer-range-mapper-entry)))
      integer-range-mapper-entry
    nil))

(defun integer-range-mapper-entry/translate-remote-integer (integer-range-mapper-entry remote-integer)
  "Translate the remote integer into its corresponding local value for this entry.
  It is an error if the remote integer isn't mapped by this entry."
  (assert (<= (integer-range-mapper-entry/remote-start integer-range-mapper-entry) remote-integer))
  (assert (< remote-integer (integer-range-mapper-entry/remote-limit integer-range-mapper-entry)))
  (+ (integer-range-mapper-entry/local-start integer-range-mapper-entry)
     (- remote-integer (integer-range-mapper-entry/remote-start integer-range-mapper-entry))))

(defclass integer-range-mapper (integer-mapper)
  ;; We'll have to optimize this when we have more advanced ASTORE capabilities.
  ;; Like the INTEGER-MAPPER, we reserve element zero.  We also keep the notion
  ;; of a current range in progress, which is pushed on to the list of entries when
  ;; we must begin a new range with a different repository mapping.

  ;; The ENTRIES list contains tuples of the following conceptual form (as lists or structs)
  ;; (local-range-start local-range-end repository-reference remote-range-start remote-range-end)
  ;; Local and remote ranges must always be contiguous, so while local ranges are contiguous,
  ;; if we should receive a non-contiguous remote range entry, we'll start a new block.
  ;; This means that the entry of information into this structure should be carefully ordered!
  ;; We could add a function to coalesce entries on demand, should unnecessary fragmentation
  ;; occur by hindsight (i.e, evolution of the information) or poor coding (unsorted input).

  ;; The following logical slot has been translated to a physical mapping into a vector
  ;; that permits update of the integer value without allocating a new object in the database
  ;; every time, to overcome allegrostore lameness.

  ((next-available-integer :initform 1
                           :initarg :next-available-integer
                           :type integer
                           :accessor integer-range-mapper/next-available-integer)
   (last-allocated-integer :initform 0
                           :initarg :last-allocated-integer
                           :type integer
                           :accessor integer-range-mapper/last-allocated-integer)
   (current-entry :initarg :current-entry
                  :initform (error "Initarg :current-entry omitted.")
                  :accessor integer-range-mapper/current-entry)
   (entries :initform nil
            :initarg :entries
            :reader integer-range-mapper/entries))
  (:documentation
     "This class serves the identical function as the INTEGER-MAPPER class,
      however it is optimized for mappings where integers are clustered such that
      a sequential range of integers has the same repository mapping.  The denser
      the clustering properties, the better this class works.  Chances are that this
      class isn't worth using unless you have at least a 10:1 clustering in integer
      ranges.  And unless the astore fixnum and other repsentations relevant to this
      class are fixed, it may require a 100:1 mapping to render real advantages
      over the INTEGER-MAPPER class.")
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defmethod make-instance :around ((class (eql (find-class 'integer-range-mapper)))
                                  &rest initargs &key repository-mapper &allow-other-keys)
  (apply #'call-next-method class
         :current-entry (make-instance 'integer-range-mapper-entry
                                       :local-start 0
                                       :remote-start 0
                                       :repository-mapper repository-mapper)
         initargs))

(defun integer-range-mapper/start-new-entry (integer-range-mapper repository-map remote-integer)
  "We need to start a new entry with because we're mapping a different repository
  from the one which was in progress.  Do so, and return the local integer which
  was allocated with it.  The old integer-range-mapper-entry will be pushed
  in the entries list.  If REMOTE-INTEGER is specified, that is used for the remote-start
  of the new entry.  If it is nil, we use the local value, because REPOSITORY-MAP is local."
  (let ((local-integer (1+ (integer-range-mapper/last-allocated-integer integer-range-mapper))))
    (remake-instance integer-range-mapper
                     :entries (cons (integer-range-mapper/current-entry integer-range-mapper)
                                    (integer-range-mapper/entries integer-range-mapper))
                     :current-entry (make-instance 'integer-range-mapper-entry
                                                   :local-start local-integer
                                                   :remote-start (or remote-integer local-integer)
                                                   :repository-map repository-map))
    local-integer))

(defmethod integer-mapper/allocate-range-end ((integer-mapper integer-range-mapper)
                                              (range-end integer))


  (assert (>= range-end (integer-range-mapper/last-allocated-integer integer-mapper)))
  ;; Avoid bashing value if it didn't change.
  (remake-instance integer-mapper
                   :last-allocated-integer range-end
                   :current-entry
                   (if (= (integer-range-mapper-entry/local-limit (integer-range-mapper/current-entry integer-mapper))
                          range-end)
                       (integer-range-mapper/current-entry integer-mapper)
                       (remake-instance (integer-range-mapper/current-entry integer-mapper)
                                        :local-limit range-end))))

(defun integer-range-mapper/resolve-remote-reference (integer-mapper remote-repository-reference remote-key
						      &key return-entry)
  "Given remote integer mapped to remote-repository (in local mapper tree), return the local mapping
   of the remote key, or NIL if there is no mapping.

   If RETURN-ENTRY is true, we'll return the integer-range-mapper-entry instead of the local key,
   or nil if there is no match.  This is useful if you want to determine if two keys lay in the same
   range entry."
  (let ((entry
	 (or (integer-range-mapper-entry/contains-remote-integer
	      (integer-range-mapper/current-entry integer-mapper) remote-key remote-repository-reference)
	     (find-if (lambda (entry)
			  (integer-range-mapper-entry/contains-remote-integer
			   entry remote-key remote-repository-reference))
		      (integer-range-mapper/entries integer-mapper)))))
    (if return-entry
	entry
      (and entry
	   (integer-range-mapper-entry/translate-remote-integer entry remote-key)))))

(defmethod integer-mapper/resolve-distributed-identifier ((integer-mapper integer-range-mapper)
							  did repository-map)
  ;; See generic function documentation for semantic description.
  ;; Partial sanity check, to ensure that the repository-map was (probably) correctly resolved.
  ;; We say "probably" because the sanity check doesn't to a full resolution check, just the repository name.
  (assert (string= (did/repository did) (mapper/key repository-map)))
  (integer-range-mapper/resolve-remote-reference integer-mapper repository-map (did/numeric-id did)))

;; Forward reference
(declaim (ftype (function (t) t) repository/local-mapper))

(defmethod integer-mapper/allocate-integers ((integer-mapper integer-range-mapper)
                                             count repository-map &optional remote-integer)
  ;; See generic function documentation for semantic description.
  ;; PAY ATTENTION NOW: THIS IS MEDIUM LEVEL TRICKINESS.
  (let ((current-entry (integer-range-mapper/current-entry integer-mapper)))
    ;; The remote-integer, if specified, has already been found not to have a local
    ;; mapping.  The current entry in progress may well be appying to the range in progress
    ;; for a remote sequence.  If the remote-integer is the current limit, then we'll
    ;; add it to the range in progress.  If it is nil and the repository-map references match,
    ;; we'll add it to the (local) range in progress.  The remote-integer arg should never
    ;; be nil for a remote repository!
    (cond ((null remote-integer)
           ;; Must be a local repository.  Note that ASSERT and ASSERT-ERROR have different result semantics
           (assert (eq repository-map (repository/local-mapper *repository*)))
           (if (eq repository-map (integer-range-mapper-entry/repository-mapper current-entry))
               ;; We were already working on an entry for the current repository, augment it.
               (let ((new-mapper
                      (remake-instance
                       integer-mapper
                       :current-entry (remake-instance
                                       current-entry
                                       :local-limit (+ count
                                                       (integer-range-mapper-entry/local-limit current-entry)))
                       :next-available-integer  (+ count
                                                   (integer-range-mapper/next-available-integer integer-mapper)))))
;                 (debug-message 5 "Remade integer-mapper ~s ~s" integer-mapper new-mapper)
;                 (debug-message 5 "old current entry ~s" current-entry)
;                 (debug-message 5 "new current entry ~s" (integer-range-mapper/current-entry new-mapper))
;                 (debug-message 5 "old mapper ~s" (integer-range-mapper-entry/repository-mapper current-entry))
;                 (debug-message 5 "new mapper ~s" (integer-range-mapper-entry/repository-mapper (integer-range-mapper/current-entry new-mapper)))
                 (- (integer-range-mapper/next-available-integer new-mapper) count))
             ;; We must start a new current entry, the existing one isn't for the current repository
             (integer-range-mapper/start-new-entry integer-mapper repository-map remote-integer)))

          ;; Does the repository match the current repository? If not, we'll have to start a new entry.
          ((not (eq repository-map (integer-range-mapper-entry/repository-mapper current-entry)))
           ;; Not the same repository, start a new entry, return the local id.
           (integer-range-mapper/start-new-entry integer-mapper repository-map remote-integer))

          ;; It's a remote integer, and the repositories match.  If the new remote-integer
          ;; is compatible with the range in progress, augment the current range, otherwise
          ;; create a new range.
          ((= remote-integer (integer-range-mapper-entry/remote-limit current-entry))
           ;; Compatible range, augment, return newly allocated local id
           (let ((new-mapper
                  (remake-instance
                   integer-mapper
                   :current-entry (remake-instance
                                   (integer-range-mapper/current-entry integer-mapper)
                                   :local-limit (+ count
                                                   (integer-range-mapper-entry/local-limit
                                                    (integer-range-mapper/current-entry integer-mapper))))
                   :next-available-integer (+ count
                                              (integer-range-mapper/next-available-integer integer-mapper)))))

             (assert (= (integer-range-mapper-entry/local-limit (integer-range-mapper/current-entry new-mapper))
                        (integer-range-mapper/next-available-integer new-mapper)
                        count))
             (- (integer-range-mapper/next-available-integer new-mapper) count)))

          (t                            ;remote, same repository, but incompatible remote range
           (integer-range-mapper/start-new-entry integer-mapper repository-map remote-integer)))))

(defmethod integer-mapper/distributed-identifier ((integer-mapper integer-range-mapper) integer)
  ;; See generic function documentation for semantic description.
  ;; INTEGER is the local key
  (let ((entry
         ;; Find the range mapper entry which corresponds to the local integer
         (or (integer-range-mapper-entry/contains-local-integer
              (integer-range-mapper/current-entry integer-mapper) integer)
             (find-if (lambda (entry)
                          (integer-range-mapper-entry/contains-local-integer entry integer))
                      (integer-range-mapper/entries integer-mapper)))))
    (unless entry
      (error "Unable to resolve integer ~s in integer range mapper for ~s, ~s"
             integer (integer-mapper/pseudo-class integer-mapper)
             integer-mapper))

    ;; Build the DID from the entry
    (let* ((remote-map (integer-range-mapper-entry/repository-mapper entry))
           (did (distributed-object-identifier remote-map)))
      ;; Fill in the (pseudo)class and remote integer values
      (make-distributed-identifier
       :domain      (did/domain     did)
       :repository  (did/repository did)
       :class       (integer-mapper/pseudo-class integer-mapper)
       :numeric-id  (integer-range-mapper-entry/translate-local-integer entry integer)))))

(defparameter *distributed-dictionary-default-unordered-map-buckets* 5
  "Number of buckets to preallocate in unordered maps")

(defparameter *distributed-dictionary-default-class-map-buckets* 25
  "Number of buckets to preallocate in unordered maps for classes, which depends on the
   number of subclasses of DISTRIBUTED-OBJECT in the system.  Note that we could potentially
   identify a good value for this dynamically with the MOP.")

(defun distributed-identifier-create-mapper-hierarchy (did)
  "Given a DIRECTORY-IDENTIFIER which uniquely describes a REPOSITORY, create a hierarchy
   for the repository.  This is used to initialize newly created repositories, including
   transport packages.  The DID must specify a repository name, and should optionally specify
   any relevent information above that, up to the domain.

   Return TWO values: the root mapper of the hierarchy, and the repository local mapper
   which is reachable from the root."
  (assert (did/repository did))
  (let* (;; Root map: keyed by domain, yields domain map
         (root-map (make-instance 'unordered-mapper
                                  :size 5
                                  :mapping-level "Root"
                                  :hash-test 'string=
                                  :size *distributed-dictionary-default-unordered-map-buckets*))

         ;; Domain map, keyed by site, yields repository map
         (domain-map (make-instance 'unordered-mapper
                                    :mapping-level "Domain"
                                    :key (did/domain did)
                                    :hash-test 'string=
                                    :parent root-map
                                    :size *distributed-dictionary-default-unordered-map-buckets*))

         ;; Repository map, keyed by repository name, yields class map for a repository
         (repository-map (make-instance 'unordered-mapper
                                        :mapping-level "Repository"
                                        :key (did/repository did)
                                        :parent domain-map
                                        :size *distributed-dictionary-default-class-map-buckets*)))


    (mapper/install-child-mapper domain-map repository-map)
    (mapper/install-child-mapper root-map domain-map)
    #||
    ;; Note that here we guess the number of classes in a persistent repository schema.
    ;; We could do this accurately, programmatically.
    ;; Create the class map for the repository map.  Note that there aren't any actual
    ;; elements IN the class map yet.
    (unordered-mapper-create (did/class did) repository-map
                             *distributed-dictionary-default-class-map-buckets*)
    ;; We don't allocated ordered-mappers in the class map yet, since we let the classes fill in
    ;; dynamically over the life of the repository.
    ||#
    ;; The maps are all linked, return the root and local maps
    (values root-map repository-map)))

(defun distributed-identifier/resolve (did root-map &key repository-only (error-if-missing t))
  "Resolve the DID into an object handle.  Return the handle if found, or NIL if not found.
   If REPOSITORY is TRUE, return the repository mapper and not a distributed object handle.
   This method never instantiates entries, it only finds existing entries."
  (let ((domain-map (mapper/resolve root-map (did/domain did))))
    (if (null domain-map)
        (when error-if-missing
          (error "DID resolution failed for domain.  DID is ~s" did))
        (let ((repository-map (mapper/resolve domain-map (did/repository did))))
          (cond ((null repository-map)
                 (when error-if-missing
                   (error "DID resolution failed for repository.  DID is ~s" did)))
                (repository-only repository-map)
                (t (let ((class-map (mapper/resolve repository-map (did/class did))))
                     (if (null class-map)
                         (when error-if-missing
                           (error "DID resolution failed for class.  DID is ~s" did))
                         (let ((object (mapper/resolve class-map (did/numeric-id did))))
                           (cond ((null object) (when error-if-missing
                                                  (error "DID resolution failed for numeric id.  DID is ~s" did)))
                                 (t object)))))))))))

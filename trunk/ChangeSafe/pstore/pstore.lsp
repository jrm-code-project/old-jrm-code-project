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

;;; Jrm's log-based persistent store

(in-package "CSF/PERSISTENT-STORE")

(proclaim (standard-optimizations))

(eval-when (:compile-toplevel :load-toplevel)
  (export '(
            persistent-store
            persistent-store/probe
            persistent-store/open
            persistent-store/open?
            persistent-store/open-mode
            persistent-store/close
            +object-id-of-root+
            )))

;;;; COMMIT-RECORD
;;;
;;; A commit record is for recovery of the database.
;;; When a restart occurs (because of a crash or otherwise)
;;; the most recent commit record is located and the transient
;;; data is restored from this record.
;;;
;;; We don't keep track of where the most recent commit
;;; record is, we scan for it on restart.  It therefore has
;;; to be unique.  We use a message digest at a fixed offset
;;; to mark the record.

(defstruct (commit-record
            (:conc-name commit-record/)
            (:constructor %make-commit-record (transaction-master
                                               node-id
                                               previous-record-offset
                                               location
                                               reason))
            (:copier nil))

  ;; Persistent slots.  These values are stored in the database.

  ;; Normally NIL, but if this is a nested transaction,
  ;; this will contain the ID of the controlling database
  ;; and the object id of a cell in the controller that determines
  ;; if the transaction succeeded.
  (transaction-master nil)

  ;; The ID of this commit record.
  (node-id      0 :type fixnum)

  ;; The count of the number of objects in the store
  (object-count 0 :type fixnum)

  ;; Offset of previous commit record
  (previous-record-offset 0 :type file-offset)

  ;; The object map that was current when this record was committed.
  (object-map-offset 0 :type file-offset)

  ;; The symbol table that was current when this record was committed.
  (symbol-table-offset 0 :type file-offset)

  ;; The reason for this commit.
  (reason "" :type string)

  ;; Transient slots.
  (object-map nil)
  (timestamp nil)
  (location 0 :type file-offset))

(defconstant *commit-record-start-cookie* :commit-record-starts)
(defconstant *commit-record-end-cookie*   :commit-record-ends)

(defconstant *commit-record-version* 0.4)

(eval-when (:load-toplevel :compile-toplevel :execute)
;;; We align commit records on coarse boundaries so we can
;;; find them again if we lose them due to a crash.
 (defconstant *commit-record-alignment* 512)
 ;; The entire commit record must fit into the seek buffer,
 ;; so the reason will be truncated to +maximum-reason-length+
 ;; to ensure there is room.
 (defconstant +seek-buffer-size+ 2048)
 (defconstant +maximum-reason-length+ 512)
)

(defun write-commit-record (commit-record stream continuation)
  (let ((bytes (map 'simple-vector-8b #'char-code
                    (write-to-string
                     `((,*commit-record-start-cookie*)
                       (:record-format ,*commit-record-version*)
                       (:timestamp ,(let ((timestamp (get-universal-time)))
                                      (setf (commit-record/timestamp commit-record) timestamp)
                                      timestamp))
                       (:location ,(commit-record/location commit-record))
                       (:node-id ,(commit-record/node-id commit-record))
                       (:object-count ,(commit-record/object-count commit-record))
                       (:object-map-offset ,(commit-record/object-map-offset commit-record))
                       (:previous-record-offset ,(commit-record/previous-record-offset commit-record))
                       (:reason ,(commit-record/reason commit-record))
                       (:symbol-table-offset ,(commit-record/symbol-table-offset commit-record))
                       (:transaction-master ,@(commit-record/transaction-master commit-record))
                       (,*commit-record-end-cookie*))
                     :case :downcase
                     :escape t
                     :readably t
                     :right-margin 40
                     :pretty t))))
    ;; It must fit.
    (assert (< (length bytes) +seek-buffer-size+))
    (multiple-value-prog1
     (mp:without-interrupts
      (write-sequence bytes stream) ;; completes the transaction
      (funcall continuation))
     (finish-output stream))))

(defconstant *seek-buffer*
  (load-time-value
        (make-array 1024 :element-type '(unsigned-byte 8))))

(defconstant *seek-buffer-as-string*
  (load-time-value
        (make-array 1024 :element-type 'character)))

(defun read-at-location (stream location)
  (file-position stream location)
  (read-sequence *seek-buffer* stream)
  (map-into *seek-buffer-as-string* #'code-char *seek-buffer*)
  *seek-buffer-as-string*)

;;;; PERSISTENT-STORE
(defstruct (persistent-store
            (:conc-name persistent-store/)
            (:constructor %make-persistent-store (pathname symbol-table log-stream guid))
            (:copier nil))

  ;; Absolute directory pathname where this store resides.
  (pathname nil :type absolute-file-pathname)

  ;; Open count.  Who wants us around?
  ;; Of course we start in the OPEN state.
  (open-count 1)

  ;; A persistent symbol table associated with this store.
  (symbol-table nil)

  ;; We append the object map nodes to this stream.
  ;; We simply write to this stream to create objects.
  (log-stream nil)

  ;; The globally unique id of this store.
  (guid nil)

  ;; Count of objects in the store
  ;; Node 0 is always NIL,
  ;; Node 1 is always 0,
  ;; Node 2 is always the root.
  (object-count 2 :type fixnum)

  ;; The current object map.
  (object-map   nil)

  ;; Most recent commit record.
  (most-recent-commit-record nil))

(defmethod print-object ((object persistent-store) stream)
  (print-unreadable-object (object stream :type t)
    (princ (persistent-store/guid object) stream)))

(defun persistent-store/open-mode (pstore)
  "Answer is meaningless when store isn't open."
  (if (persistent-store/log-stream pstore)
      :read-write
      :read-only))

(defun persistent-store/open? (pstore)
  (> (persistent-store/open-count pstore) 0))

(defconstant +persistent-store-cookie+ :jrm-persistent-store)
(defconstant +persistent-store-format+ 1)

(defun recover-store-header (stream)
  (let* ((probe (read-at-location stream 0))
         (header (read-from-string probe)))
    (when (assoc +persistent-store-cookie+ header)
      (debug-message 2 "Opening existing database ~s." header)
      (cadr (assoc :guid header)))))

;; It's important to find commit records rapidly when
;; recovering the database, so we do a quick probe for the
;; magic beginning string.
(defconstant *commit-record-probe-buffer*
  (load-time-value
   (make-array 23 :element-type '(unsigned-byte 8))))

(defconstant *commit-record-bytes*
  (load-time-value
   (make-array 23 
               :element-type '(unsigned-byte 8)
               :initial-contents (map 'list #'char-code "((:commit-record-starts"))))

(defun attempt-read-commit-record (stream location)
  (file-position stream location)
  (read-sequence *commit-record-probe-buffer* stream)
  (when (equalp *commit-record-probe-buffer* *commit-record-bytes*)
    (let ((probe (read-at-location stream location)))
;    (when (string-equal probe "((:commit-record-starts" :end1 23 :end2 23)
      (let ((info (ignore-errors (read-from-string probe))))
        (when (assoc :commit-record-ends info)
          info)))))

(defun scan-potential-commit-record-locations (file-length)
  ;; Return a series of file offsets where commit records
  ;; might be store.  Used to find the latest record.
  (declare (optimizable-series-function))
  (scan-fn 'file-offset
           (lambda () (* (floor file-length *commit-record-alignment*)
                         *commit-record-alignment*))
           (lambda (previous-location) (- previous-location *commit-record-alignment*))
           (lambda (previous-location) (< previous-location 0))))

(defun scan-potential-commit-records (stream)
  (declare (optimizable-series-function))
  (map-fn 'list (lambda (location)
                  (attempt-read-commit-record stream location))
          (scan-potential-commit-record-locations
           (file-length stream))))

(defun most-recent-commit-record (stream)
  (debug-message 5 "Seeking most recent commit record.")
  (collect-first (choose (scan-potential-commit-records stream))))

(defun next-commit-record (stream)
  (lambda (commit-record)
    (let ((commit-record-offset (or (cadr (assoc :previous-record-offset commit-record))
                                    (error 'changesafe-database-recovery-error
                                           :format-string "Missing :previous-record-offset in commit-record ~s"
                                           :format-control (list commit-record)))))
      (when (> commit-record-offset 0)
        (attempt-read-commit-record stream commit-record-offset)))))

;;; A series scanner that returns a series of
;;; commit records from most recent to earliest.

(defun scan-commit-records (stream)
  (declare (optimizable-series-function 2))
  (scan-fn-inclusive
   'commit-record
   (lambda () (most-recent-commit-record stream))
   (next-commit-record stream)
   (lambda (commit-record)
     (zerop (cadr (assoc :previous-record-offset commit-record))))))

(defun recover-symbol-table-segment (commit-record symbol-table stream)
  (let ((symbol-table-offset (or (cadr (assoc :symbol-table-offset commit-record))
                                 (error 'changesafe-database-recovery-error
                                        :format-string "Missing :symbol-table-offset in commit record ~s"
                                        :format-control (list commit-record)))))
    (file-position stream symbol-table-offset)
    (symbol-table/recover-segment symbol-table stream)))

(defun recover-symbol-table (stream)
  (debug-message 5 "Recovering symbol table.")
  ;; Have to recover in order from oldest to newest.
  (let ((symbol-table (make-symbol-table)))
    (mapc (lambda (commit-record)
            (recover-symbol-table-segment commit-record symbol-table stream))
          (nreverse (collect 'list (scan-commit-records stream))))
    (symbol-table/note-committed symbol-table)
    (debug-message 5 "Recovered ~d strings." (symbol-table/string-count symbol-table))
    (debug-message 5 "Recovered ~d symbols." (symbol-table/symbol-count symbol-table))
    (debug-message 5 "Recovered ~d dids." (symbol-table/did-count symbol-table))
    symbol-table))

(defun recover-commit-record (pathname stream)
  (debug-message 5 "Recovering commit record.")
  (collect-first
   (choose-if (lambda (commit-record)
               (debug-message 5 "Checking commit record ~{~s~}" commit-record)
               (let ((transaction-master (assoc :transaction-master commit-record)))
                 (debug-message 5 "Transaction master is ~s" transaction-master)
                 (and transaction-master
                      (or (null (cdr transaction-master))
                          (let* ((master-guid    (cadr transaction-master))
                                 (master-cell-id (cddr transaction-master))
                                 (master-pstore  (find-open-persistent-store pathname master-guid)))
                            (cond ((null master-pstore)
                                   (error 'changesafe-database-recovery-error
                                          :format-control "Two phase commit record database is missing."
                                          :format-arguments (list master-pstore)))
                                  ((not (persistent-store/open? master-pstore))
                                   (error 'changesafe-database-recovery-error
                                          :format-control "Two phase commit record is in unopen store ~s"
                                          :format-arguments (list master-pstore)))
                                  (t (prog1 (car (object-map-info/value
                                                  (object-map/find (persistent-store/object-map master-pstore)
                                                                   master-cell-id)))
                                       (persistent-store/close master-pstore)))))))))
             (scan-commit-records stream))))

(defvar *open-persistent-stores* '()
  "Mapping from GUID to persistent store.")

(defun register-open-persistent-store (store-id store)
  (push (cons store-id store) *open-persistent-stores*))

(defun unregister-open-persistent-store (store-id)
  (setq *open-persistent-stores*
        (delete store-id *open-persistent-stores*
                :key #'car)))

(defun find-open-persistent-store (pathname store-id)
  (let ((probe1 (cdr (assoc store-id *open-persistent-stores*))))
    (if probe1
        ;; Found one already open.  Since `recover-commit-record' will
        ;; close this again, bump the open-count.
        (progn
          (incf (persistent-store/open-count probe1))
          probe1)
        ;; Look in the directory.  Just open it read only while we
        ;; check the cell.  `recover-commit-record' will close it
        ;; again.
        (let ((probe2 (persistent-store/find-file pathname store-id)))
          (persistent-store/open probe2 :mode :read-only)
          (cdr (assoc store-id *open-persistent-stores*))))))

(defun find-open-persistent-store-by-pathname (pathname)
  (cdr (rassoc pathname *open-persistent-stores*
               ;; eta convert the macro.  sigh
               :key (lambda (store) (persistent-store/pathname store))
               :test #'equal)))

(defun close-all-open-persistent-stores ()
  "User utility for recovering from bad mistakes."
  (or (null *open-persistent-stores*)
      (progn (persistent-store/close (cdar *open-persistent-stores*))
             (close-all-open-persistent-stores))))

(defconstant +object-id-of-nil+ 0)
(defconstant +object-id-of-zero+ 1)
(defconstant +object-id-of-root+ 2)

(defun persistent-store/allocate-object-id (pstore)
  (if (eq (persistent-store/open-mode pstore) :read-only)
      (error "Cannot allocate objects from read-only store.")
      (1- (incf (persistent-store/object-count pstore)))))

(defun persistent-store/next-location (persistent-store)
  (if (eq (persistent-store/open-mode persistent-store) :read-only)
      (error "Cannot allocate objects from read-only store.")
      (align-stream (persistent-store/log-stream persistent-store)
                    *file-addressing-granularity*)))

(defun persistent-store/probe (pathname)
  "Check for the existance of a persistent store at PATHNAME.
   If it exists, the GUID is returned."
  (check-type pathname absolute-file-pathname)
  (with-open-file (scan pathname
                        :direction :input
                        :element-type 'character
                        :if-does-not-exist nil)
    (when scan
      (file-position scan 0)
      (let ((info (ignore-errors (read scan))))
        (when (and info (assoc +persistent-store-cookie+ info))
          info)))))

(defun persistent-store/find-file (directory guid)
  "Scan the files in <directory> and find the persistent store matching <guid>."
  (check-type directory absolute-pathname)
  (collect-first
   (choose-if (lambda (file)
                (eq guid (cadr (assoc :guid (persistent-store/probe file)))))
              (choose-if 
               (lambda (file)
                 (typep file 'absolute-file-pathname))
               (scan-directory (make-pathname :name :wild
                                              :type :wild
                                              :defaults directory))))))

(defun persistent-store/open (pstore-pathname &key (mode :read-write) (auxiliary-info nil))
  ;; Auxiliary info is simply printed into the header.
  "Create a new persistent store, or open an existing one."
  (check-type pstore-pathname absolute-file-pathname)
  (let ((probe (find-open-persistent-store-by-pathname pstore-pathname)))
    (if probe
        (if (or (eq (persistent-store/open-mode probe) mode)
                (eq mode :read-only))
            (progn
              (incf (persistent-store/open-count probe))
              probe)
            (let ((id         (persistent-store/guid probe))
                  (open-count (persistent-store/open-count probe)))
              (debug-message 5 "Closing and re-opening store ~s to upgrade mode." id)
              (unregister-open-persistent-store id)
              (let ((reopened (persistent-store/open pstore-pathname
                                                     :mode mode
                                                     :auxiliary-info auxiliary-info)))
                (incf (persistent-store/open-count reopened) open-count)
                reopened)))
        (let ((index-stream nil)
              (close-it? t))
          (unless (eq mode :read-only)
            (ensure-directories-exist (pathname-syntactic-parent pstore-pathname)))
          (unwind-protect
              (progn
                (when (eq mode :read-write)
                  (setq index-stream (open pstore-pathname
                                           :buffered t
                                           :direction :output
                                           :element-type 'unsigned-byte
                                           :if-exists :append
                                           :if-does-not-exist :create)))
                (if (and (eq mode :read-write)
                         (zerop (file-position index-stream)))
                    ;; Brand new store.  Need to get an ID and create a symbol table.
                    (let ((store-id (generate-guid)))
                      (debug-message 2 "Creating new database ~s." store-id)
                      (format index-stream "~s~%~%"
                              `((,+persistent-store-cookie+ ,+persistent-store-format+)
                                (:guid ,store-id)
                                ,@auxiliary-info))
                      (finish-output index-stream)
                      (let* ((symbol-table (make-symbol-table))
                             (store (%make-persistent-store pstore-pathname
                                                            symbol-table
                                                            index-stream
                                                            store-id))
                             (zero-loc  (persistent-store/next-location store)))
                        (register-open-persistent-store store-id store)
                        (serialize 0 index-stream symbol-table)
                        (setf (persistent-store/object-map store)
                              ;; install 0
                              (object-map/add index-stream
                                              ;; install NIL
                                              (object-map/add index-stream nil +object-id-of-nil+ 0 nil)
                                              +object-id-of-zero+
                                              zero-loc
                                              0))
                        (persistent-store/commit store (format nil "Create database ~s" store-id))
                        (multiple-value-prog1 (values store store-id t)
                          (setq close-it? nil))))

                    ;; Previously existing store.
                    ;; need to find the last commit record.
                    (with-open-file (scan pstore-pathname
                                          :direction :input
                                          :element-type 'unsigned-byte
                                          :if-does-not-exist :error);; shouldn't happen!
                      (let* ((store-id (recover-store-header scan))
                             (commit-record (recover-commit-record pstore-pathname scan))
                             (symbol-table (recover-symbol-table scan))
                             (store (%make-persistent-store pstore-pathname symbol-table index-stream store-id)))
                        (declare (ignore dummy))

                        (register-open-persistent-store store-id store)

                        (setf (persistent-store/most-recent-commit-record store)
                              (%make-commit-record (cadr (assoc :transaction-master commit-record))
                                                   (cadr (assoc :node-id commit-record))
                                                   (cadr (assoc :previous-record-offset commit-record))
                                                   (cadr (assoc :location commit-record))
                                                   (cadr (assoc :reason commit-record)))
                              (persistent-store/guid store) store-id
                              (persistent-store/symbol-table store) symbol-table

                              ;; Fetching the object map should restore the entire
                              ;; contents of the store to memory.
                              (persistent-store/object-map store)
                              (fetch-object-map store (persistent-store/symbol-table store) scan
                                                (cadr (assoc :object-map-offset (cdr commit-record))))
                              (persistent-store/object-count store)
                              (cadr (assoc :object-count (cdr commit-record))))
                        (setq close-it? nil)
                        (values store store-id nil)))))

            "cleanup after opening"
            (when close-it?
              (when index-stream (close index-stream))))))))

(defun persistent-store/commit (persistent-store reason &key transaction-master)
  "Makes saved objects available to later lisp incarnations."
  (when (null (persistent-store/log-stream persistent-store))
    (error "Cannot commit changes to read-only store."))
  (let* ((previous-commit-record (persistent-store/most-recent-commit-record persistent-store))
         (log-stream             (persistent-store/log-stream         persistent-store))
         (symbol-table           (persistent-store/symbol-table persistent-store))

         ;; First, we need to save any symbols that haven't yet been
         ;; made persistent.  These have to end up in the file before
         ;; the commit record.
         (symbol-table-offset (prog1 (persistent-store/next-location persistent-store)
                                (symbol-table/sync symbol-table log-stream)))
         (node-id      (persistent-store/allocate-object-id persistent-store))
         (object-map   (persistent-store/object-map         persistent-store))

         (commit-record-position
          ;; Interesting problem.  The commit record ought to have a
          ;; node-id assigned to it.  But if we write the commit record
          ;; *before* the node-id is mapped, the node-id won't be in the
          ;; map.  But if we map the node-id *before* the commit record
          ;; is written, we won't know the location of the commit record!
          ;;
          ;; The solution?  Pretend to add the object for the purpose
          ;; of determining its size, then actually add the object.
          (let ((counting-stream (make-instance 'byte-counting-stream
                                                :initial-position (persistent-store/next-location persistent-store))))
            (object-map/add counting-stream object-map node-id 0 "dummy string")
            (* *commit-record-alignment*
               (ceiling (file-position counting-stream) *commit-record-alignment*))))

         (commit-record
          (%make-commit-record transaction-master
                               node-id
                               (if (null previous-commit-record)
                                   0
                                   (commit-record/location previous-commit-record))
                               commit-record-position
                               ;; Truncate reason if necessary.
                               (if (> (length reason) +maximum-reason-length+)
                                   (subseq reason 0 +maximum-reason-length+)
                                   reason)))

         ;; Now actually update the object map.
         (new-map (object-map/add log-stream object-map node-id commit-record-position commit-record)))

    (setf (commit-record/symbol-table-offset commit-record)
          symbol-table-offset
          (commit-record/object-map-offset commit-record)
          (persistent-node/%node-offset new-map)
          (commit-record/object-count commit-record)
          (persistent-store/object-count persistent-store))
    (align-stream log-stream *commit-record-alignment*)
    (assert (= (file-position log-stream) commit-record-position));; sanity check
    (debug-message 5 "Writing commit record to ~s, transaction master is ~s" persistent-store transaction-master)
    (write-commit-record commit-record log-stream
      (lambda ()
        (symbol-table/note-committed symbol-table)
        (setf (persistent-store/object-map       persistent-store) new-map
              (persistent-store/most-recent-commit-record persistent-store) commit-record)))
    (debug-message 3 "Wrote commit record to ~s" persistent-store)
    (debug-message 3 "Reason:  ~a" reason)))

(defun persistent-store/close (pstore)
  (let ((new-open-count (decf (persistent-store/open-count pstore))))
    (debug-message 5 "Decrementing open count (~d -> ~d) on ~s."
                   (1+ new-open-count)
                   new-open-count
                   pstore)
    (when (zerop new-open-count)
      (debug-message 2 "Closing ~s" pstore)
      (unregister-open-persistent-store (persistent-store/guid pstore))
      (when (persistent-store/log-stream pstore)
        (close (persistent-store/log-stream pstore)))
      (setf (persistent-store/object-map pstore) nil
            (persistent-store/most-recent-commit-record pstore) nil))))

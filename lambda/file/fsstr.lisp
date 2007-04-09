;;; -*- Mode:LISP; Package:FILE-SYSTEM; Base:10; Readtable:ZL -*-

;;; All "MAP-" streams are internal to the file system and support only simple
;;; serial IO.  All "LM-" streams are streams used when opening files.

(DEFFLAVOR MAP-STREAM-MIXIN
        ((STATUS :OPEN)                 ;:OPEN, :EOF, :BEING-CLOSED, :CLOSED
         (MAP (MAP-CREATE))                     ;The map this stream is acting upon.
         (MAP-INDEX 0)                          ;Current index within map.
         (BYTE-SIZE 8)                          ;The byte size of the STREAM.
         (RQB (GET-DISK-RQB STANDARD-BLOCK-SIZE)) ;The RQB in which the physical buffer lives.
         (RQB-VALID-PAGES NIL))                 ;The number of pages of data in RQB
        ()
  (:INCLUDED-FLAVORS SI:STREAM)
  (:INITABLE-INSTANCE-VARIABLES MAP BYTE-SIZE)
  (:GETTABLE-INSTANCE-VARIABLES BYTE-SIZE))

(DEFWRAPPER (MAP-STREAM-MIXIN :CLOSE) ((&OPTIONAL IGNORE) . BODY)
  `(AND (NEQ STATUS :CLOSED)
        (PROG1 (PROGN ,@BODY) (SETQ STATUS :CLOSED))))

(DEFMETHOD (MAP-STREAM-MIXIN :CLOSE) (&OPTIONAL IGNORE)
  (AND RQB (RETURN-DISK-RQB RQB))
  (SETQ RQB NIL))

(DEFFLAVOR MAP-INPUT-STREAM-MIXIN
        ()
        (MAP-STREAM-MIXIN)
  (:INCLUDED-FLAVORS SI:BUFFERED-INPUT-STREAM))

(DEFFLAVOR MAP-OUTPUT-STREAM-MIXIN
        ()
        (MAP-STREAM-MIXIN)
  (:INCLUDED-FLAVORS SI:BUFFERED-OUTPUT-STREAM))

(DEFMETHOD (MAP-INPUT-STREAM-MIXIN :NEXT-INPUT-BUFFER) (&OPTIONAL IGNORE)
  (COND ((EQ STATUS :CLOSED)
         (FERROR NIL "Attempt to get input from ~S, which is closed." SELF)))
  (LET ((MAP-NBLOCKS (MAP-NBLOCKS MAP))
        SIZE)
    (COND ((< MAP-INDEX MAP-NBLOCKS)
           (SETQ STATUS :OPEN)                  ;May have been at :EOF
           (SETQ SIZE (MAP-BLOCK-SIZE MAP MAP-INDEX)
                 RQB-VALID-PAGES (CEILING SIZE PAGE-SIZE-IN-BITS))
           (LM-DISK-READ RQB (MAP-BLOCK-LOCATION MAP MAP-INDEX) RQB-VALID-PAGES)
           (VALUES (GET-RQB-ARRAY RQB BYTE-SIZE) 0 (FLOOR SIZE BYTE-SIZE)))
          ;; If we're out of range, simply call it an :EOF.
          (T (SETQ STATUS :EOF)
             NIL))))

;; This doesn't really discard the buffer (why bother), but does increment
;; the buffer pointer for the next :NEXT-INPUT-BUFFER message.
(DEFMETHOD (MAP-INPUT-STREAM-MIXIN :DISCARD-INPUT-BUFFER) (IGNORE)
  (INCF MAP-INDEX))

;; This message would work, but would not do anything near what
;; the user would want -- it would just use up more disk space.
(DEFMETHOD (MAP-OUTPUT-STREAM-MIXIN :FORCE-OUTPUT) IGNORE)

(DEFMETHOD (MAP-OUTPUT-STREAM-MIXIN :NEW-OUTPUT-BUFFER) (&AUX LOC)
  (COND ((EQ STATUS :CLOSED)
         (FERROR NIL "Attempt to do output on ~S, which is closed." SELF)))
  (IF (= MAP-INDEX (MAP-NBLOCKS MAP))
      ;; At end: add a new block.
      (PROGN
        (MULTIPLE-VALUE (LOC RQB-VALID-PAGES)
          (ALLOCATE-DISK-BLOCK))
        (MAP-APPEND-BLOCK MAP LOC 0)
        (VALUES (GET-RQB-ARRAY RQB BYTE-SIZE)
                0
                (FLOOR (* RQB-VALID-PAGES PAGE-SIZE-IN-BITS) BYTE-SIZE)))
    ;; In the middle: read contents of next existing block
    ;; so that we can change only the bytes actually output.
    (LET ((SIZE (MAP-BLOCK-SIZE MAP MAP-INDEX)))
      (SETQ RQB-VALID-PAGES (CEILING SIZE PAGE-SIZE-IN-BITS))
      (LM-DISK-READ RQB (MAP-BLOCK-LOCATION MAP MAP-INDEX) RQB-VALID-PAGES)
      (VALUES (GET-RQB-ARRAY RQB BYTE-SIZE) 0 (FLOOR SIZE BYTE-SIZE)))))

;; The :NEW-OUTPUT-BUFFER method gets the old data, so this has nothing to do.
(DEFMETHOD (MAP-OUTPUT-STREAM-MIXIN :GET-OLD-DATA) (&REST IGNORE) NIL)

(DEFMETHOD (MAP-OUTPUT-STREAM-MIXIN :SEND-OUTPUT-BUFFER) (BUFFER TO-INDEX)
  (OR (AND (ARRAY-INDIRECT-P BUFFER)
           (EQ (%P-CONTENTS-OFFSET BUFFER
                                   (1+ (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG BUFFER 0)))
               RQB))
      (FERROR NIL "Attempt to :SEND-OUTPUT-BUFFER ~S, which is not indirected into ~S"
              BUFFER RQB))
  (LET* ((LOC (MAP-BLOCK-LOCATION MAP MAP-INDEX))
         (SIZE-FROM-INDEX (* TO-INDEX BYTE-SIZE))
         (SIZE (MAX SIZE-FROM-INDEX (MAP-BLOCK-SIZE MAP MAP-INDEX)))
         (USED-NPAGES (CEILING SIZE PAGE-SIZE-IN-BITS)))
    (WHEN (PLUSP SIZE)
      (LM-DISK-WRITE RQB LOC USED-NPAGES)
      (WHEN (< (MAP-BLOCK-SIZE MAP MAP-INDEX) SIZE-FROM-INDEX) ;NECESSARY NOW?
        (SETF (MAP-BLOCK-SIZE MAP MAP-INDEX) SIZE-FROM-INDEX))
      (INCF MAP-INDEX))
    (WHEN (AND ( USED-NPAGES RQB-VALID-PAGES)
               (NULL (AND (TYPEP SELF 'LM-OUTPUT-STREAM-MIXIN)
                          (FUNCALL-SELF :KEEP-RESERVED-BLOCKS))))
      (USING-PUT
        (CHANGE-BLOCK-DISK-SPACE (+ LOC USED-NPAGES) (- RQB-VALID-PAGES USED-NPAGES)
                                 PUT-RESERVED PUT-FREE)
        ;; Try to minimize small holes by backing up the search pointer.
        (SETQ PUT-SCANNING-INDEX (+ LOC USED-NPAGES)))
      (WHEN (ZEROP SIZE)
        (DECF (MAP-NBLOCKS MAP))))))

(DEFMETHOD (MAP-OUTPUT-STREAM-MIXIN :MAYBE-UPDATE-MAP-BLOCK-SIZE) (CHAR-INDEX)
  (AND MAP                                      ;Don't do anything if stream closed
       (LET ((CHAR-BITS (* CHAR-INDEX BYTE-SIZE)))
         (WHEN (< (MAP-BLOCK-SIZE MAP MAP-INDEX) CHAR-BITS)
           (SETF (MAP-BLOCK-SIZE MAP MAP-INDEX) CHAR-BITS)))))

(DEFMETHOD (MAP-OUTPUT-STREAM-MIXIN :CURRENT-LENGTH-FROM-MAP) ()
  (AND MAP
       (// (MAP-LENGTH MAP) BYTE-SIZE)))

; RETURN MAP-INDEX IF FILE IS EMPTY OR MAP-INDEX POINTS TO LAST BLOCK
(DEFMETHOD (MAP-OUTPUT-STREAM-MIXIN :LAST-BLOCK-P) ()
  (LET ((MAP-NBLOCKS (MAP-NBLOCKS MAP)))
    (WHEN (OR (ZEROP MAP-NBLOCKS)
              (= MAP-NBLOCKS (1+ MAP-INDEX)))
      MAP-INDEX)))

(DEFMETHOD (MAP-OUTPUT-STREAM-MIXIN :MAP-NBLOCKS) ()
  (AND MAP
       (MAP-NBLOCKS MAP)))

(DEFMETHOD (MAP-OUTPUT-STREAM-MIXIN :UPDATE-RQB-VALID-PAGES) (BYTES)
  (SETQ RQB-VALID-PAGES (CEILING (* BYTES BYTE-SIZE) PAGE-SIZE-IN-BITS)))

; RETURN NIL IF MAP IS EMPTY
(DEFMETHOD (MAP-OUTPUT-STREAM-MIXIN :SET-MAP-FOR-APPEND) (&OPTIONAL APPEND-NEW-BLOCK)
  (LET ((BLOCKS (MAP-NBLOCKS MAP)))
    (COND ((NOT (PLUSP BLOCKS)) ;file is empty
           (SETQ MAP-INDEX 0)
           NIL)
          (T
           (SETQ MAP-INDEX (IF APPEND-NEW-BLOCK
                              BLOCKS
                            (1- BLOCKS)))))))

(DEFMETHOD (MAP-OUTPUT-STREAM-MIXIN :MAX-BYTES-IN-BLOCK) (&OPTIONAL (BLOCK MAP-INDEX))
  (COND ((ZEROP (MAP-NBLOCKS MAP))
         0)
        (T
         (LET* ((PAGES-IN-BLOCK (CEILING (MAP-BLOCK-SIZE MAP BLOCK) PAGE-SIZE-IN-BITS))
                (BYTES-IN-BLOCK (CEILING (* PAGES-IN-BLOCK PAGE-SIZE-IN-BITS) BYTE-SIZE)))
           (MAX BYTES-IN-BLOCK
                (IF RQB-VALID-PAGES
                    (CEILING (* RQB-VALID-PAGES PAGE-SIZE-IN-BITS) BYTE-SIZE)
                  0))))))


(DEFMETHOD (MAP-OUTPUT-STREAM-MIXIN :BYTES-IN-BLOCK) (&OPTIONAL (BLOCK MAP-INDEX))
  (COND ((ZEROP (MAP-NBLOCKS MAP))
         0)
        (T
         (CEILING (MAP-BLOCK-SIZE MAP BLOCK) BYTE-SIZE))))


(DEFMETHOD (MAP-OUTPUT-STREAM-MIXIN :DISCARD-OUTPUT-BUFFER) (BUFFER)
  (SEND SELF :SEND-OUTPUT-BUFFER BUFFER 0))

;These flavors are not used.
;(DEFFLAVOR MAP-INPUT-STREAM
;       ()
;       (MAP-INPUT-STREAM-MIXIN SI:BUFFERED-INPUT-STREAM))
;
;(DEFFLAVOR MAP-OUTPUT-STREAM
;       ()
;       (MAP-OUTPUT-STREAM-MIXIN SI:BUFFERED-OUTPUT-STREAM))

;; The next two flavors are used internally by the file system.
(DEFFLAVOR MAP-CHARACTER-INPUT-STREAM
        ()
        (MAP-INPUT-STREAM-MIXIN SI:BUFFERED-LINE-INPUT-STREAM))

(DEFFLAVOR MAP-CHARACTER-OUTPUT-STREAM
        ()
        (MAP-OUTPUT-STREAM-MIXIN SI:BUFFERED-OUTPUT-CHARACTER-STREAM))

(DEFMETHOD (MAP-CHARACTER-OUTPUT-STREAM :MAP) () MAP)

;; The next flavor is for input streams opened in an odd byte size.
(DEFFLAVOR ODD-BYTE-SIZE-MIXIN () ()
  (:REQUIRED-FLAVORS SI:MAP-INPUT-STREAM-MIXIN))

(DEFWRAPPER (ODD-BYTE-SIZE-MIXIN :NEXT-INPUT-BUFFER) ((&OPTIONAL IGNORE) . BODY)
  (LET ((VAL1 (GENTEMP)) (VAL2 (GENTEMP)) (VAL3 (GENTEMP)) (I (GENTEMP)))
    `(MULTIPLE-VALUE-PROG1 (,VAL1 ,VAL2 ,VAL3)
         (PROGN . ,BODY)
       (COND (,VAL1
              (LOOP FOR ,I FROM ,VAL2 BELOW ,VAL3
                    DO (ASET (LDB BYTE-SIZE (AREF ,VAL1 ,I)) ,VAL1 ,I))))
       (VALUES ,VAL1 ,VAL2 ,VAL3))))


(DEFFLAVOR LM-STREAM-MIXIN
        (TRUENAME)
        (SI:PROPERTY-LIST-MIXIN SI:FILE-STREAM-MIXIN)
  (:GETTABLE-INSTANCE-VARIABLES TRUENAME)
  (:INIT-KEYWORDS :FILE :APPEND :TRUENAME :IF-EXISTS))

(DEFMETHOD (LM-STREAM-MIXIN :INIT) (PLIST)
  (LET ((FILE (GET PLIST :FILE)))
    (IF FILE
        (SETQ TRUENAME (FILE-TRUENAME FILE)
              SI:PROPERTY-LIST (LMFS-FILE-PROPERTIES FILE))
      (SETQ TRUENAME (GET PLIST :TRUENAME)))))

(DEFMETHOD (LM-STREAM-MIXIN :QFASLP) ()
  (GET (LOCF SI:PROPERTY-LIST) :QFASLP))

;; For probes, assume you really mean :LENGTH-IN-BYTES
(DEFMETHOD (LM-STREAM-MIXIN :LENGTH) ()
  (GET (LOCF SI:PROPERTY-LIST) :LENGTH-IN-BYTES))

(DEFMETHOD (LM-STREAM-MIXIN :PROPERTIES) (&OPTIONAL ERROR-P)
  ERROR-P ; would be quite hard to get an error here
  (VALUES (CONS TRUENAME SI:PROPERTY-LIST) LM-UNSETTABLE-PROPERTIES))

(DEFFLAVOR LM-DATA-STREAM-MIXIN
        (FILE)
        (LM-STREAM-MIXIN)
  (:INCLUDED-FLAVORS MAP-STREAM-MIXIN)
  (:INITABLE-INSTANCE-VARIABLES FILE))

(DEFMETHOD (LM-DATA-STREAM-MIXIN :STATUS) () STATUS)

(DEFMETHOD (LM-DATA-STREAM-MIXIN :DELETE) (&OPTIONAL (ERROR-P T))
  (IDENTIFY-FILE-OPERATION :DELETE
    (HANDLING-ERRORS ERROR-P
      (LMFS-DELETE-FILE FILE))))

;;This didn't used to ever return an error condition, and the
;;file-server expected it to when ERROR-P is NIL.  The problem was, the
;;SETQ was outside the HANDLING-ERRORS. Now, if ERROR-P is NIL and we
;;get an error, return an error condition; otherwise always return
;;the file TRUENAME. -KmC 8/2/88

(DEFMETHOD (LM-DATA-STREAM-MIXIN :RENAME) (NEW-NAME &OPTIONAL (ERROR-P T))
  (IDENTIFY-FILE-OPERATION :RENAME
    (HANDLING-ERRORS ERROR-P
      (LMFS-RENAME-FILE FILE
                        (PATHNAME-DIRECTORY NEW-NAME)
                        (PATHNAME-NAME NEW-NAME)
                        (PATHNAME-TYPE NEW-NAME)
                        (PATHNAME-VERSION NEW-NAME))
      (SETQ TRUENAME (FILE-TRUENAME FILE)))))

(DEFMETHOD (LM-DATA-STREAM-MIXIN :CHANGE-PROPERTIES) (ERROR-P &REST PROPERTIES)
  (IDENTIFY-FILE-OPERATION :CHANGE-PROPERTIES
    (HANDLING-ERRORS ERROR-P
      (LMFS-CHANGE-FILE-PROPERTIES FILE PROPERTIES))))

(DEFMETHOD (LM-DATA-STREAM-MIXIN :INIT) (IGNORE)
  (SETQ MAP (FILE-MAP FILE)
        TRUENAME (FILE-TRUENAME FILE)
        SI:PROPERTY-LIST (LMFS-FILE-PROPERTIES FILE))
  (WITHOUT-INTERRUPTS
    (SETQ LM-FILE-STREAMS-LIST
          (CONS-IN-AREA SELF LM-FILE-STREAMS-LIST LOCAL-FILE-SYSTEM-AREA))))

(DEFMETHOD (LM-DATA-STREAM-MIXIN :SET-BUFFER-POINTER) (NEW-POINTER)
  (DO ((NBLOCKS (MAP-NBLOCKS MAP))
       (I 0 (1+ I))
       (P 0 P1)
       (P1))
      (NIL)
    (COND ((OR ( I NBLOCKS)
               (> (SETQ P1 (+ P (FLOOR (MAP-BLOCK-SIZE MAP I) BYTE-SIZE)))
                  NEW-POINTER))
           (SETQ MAP-INDEX I)
           (RETURN P)))))

(DEFMETHOD (LM-DATA-STREAM-MIXIN :SET-BUFFER-POINTER-TO-END) ()
  (WHEN (PLUSP (MAP-NBLOCKS MAP))
    (SETQ MAP-INDEX (1- (MAP-NBLOCKS MAP)))
    (- (FUNCALL-SELF :LENGTH) (// (MAP-BLOCK-SIZE MAP MAP-INDEX) BYTE-SIZE))))



(DEFMETHOD (LM-DATA-STREAM-MIXIN :BYTES-BEFORE-CURRENT-BLOCK) ()
  (LOOP FOR SIZE = 0 THEN (+ SIZE (FLOOR (MAP-BLOCK-SIZE MAP COUNT) BYTE-SIZE))
        FOR COUNT = 0 THEN (1+ COUNT)
        UNTIL (= COUNT MAP-INDEX)
        FINALLY (RETURN SIZE)))

(DEFFLAVOR LM-INPUT-STREAM-MIXIN
        ()
        (LM-DATA-STREAM-MIXIN SI:INPUT-FILE-STREAM-MIXIN)
  (:INCLUDED-FLAVORS MAP-STREAM-MIXIN))


(DEFFLAVOR LM-OUTPUT-STREAM-MIXIN
        (APPEND-OR-OVERWRITE
         KEEP-RESERVED-BLOCKS)
        (LM-DATA-STREAM-MIXIN SI:OUTPUT-FILE-STREAM-MIXIN)
  (:INCLUDED-FLAVORS MAP-STREAM-MIXIN)
  (:REQUIRED-FLAVORS SI:OUTPUT-POINTER-REMEMBERING-MIXIN)
  (:GETTABLE-INSTANCE-VARIABLES APPEND-OR-OVERWRITE KEEP-RESERVED-BLOCKS))

(DEFMETHOD (LM-INPUT-STREAM-MIXIN :AFTER :CLOSE) (&OPTIONAL IGNORE)
  (LET ((DEFAULT-CONS-AREA LOCAL-FILE-SYSTEM-AREA))
    (LMFS-CLOSE-FILE FILE)
    (WITHOUT-INTERRUPTS
      (SETQ LM-FILE-STREAMS-LIST (DELQ SELF LM-FILE-STREAMS-LIST)))
    (SETQ FILE NIL MAP NIL)))                   ;Remove pointers into temp area.

;; For input streams, :LENGTH may be different from :LENGTH-IN-BYTES
(DEFMETHOD (LM-INPUT-STREAM-MIXIN :LENGTH) ()
  (FLOOR (* (GET (LOCF SI:PROPERTY-LIST) :LENGTH-IN-BYTES)
            (GET (LOCF SI:PROPERTY-LIST) :BYTE-SIZE))
         BYTE-SIZE))

(DEFMETHOD (LM-OUTPUT-STREAM-MIXIN :AFTER :INIT) (INIT-PLIST)
  (SETQ KEEP-RESERVED-BLOCKS T)
  (LET ((IF-EXISTS (GET INIT-PLIST :IF-EXISTS)))
    (CASE IF-EXISTS
      (:APPEND
       (SETQ APPEND-OR-OVERWRITE :APPEND)
       (USING-PUT
         (CHANGE-MAP-DISK-SPACE MAP PUT-USED PUT-RESERVED))
       (FUNCALL-SELF :SETUP-BUFFER-FOR-APPEND))
      (:OVERWRITE
       (SETQ APPEND-OR-OVERWRITE :OVERWRITE)
       (USING-PUT
         (CHANGE-MAP-DISK-SPACE MAP PUT-USED PUT-RESERVED))
       (FUNCALL-SELF :SETUP-BUFFER-FOR-APPEND)
       (FUNCALL-SELF :SET-POINTER :START))
      (T
       (SETQ APPEND-OR-OVERWRITE NIL)))))

(DEFMETHOD (LM-OUTPUT-STREAM-MIXIN :LENGTH) ()
  (WHEN SI:STREAM-OUTPUT-BUFFER
    (FUNCALL-SELF :MAYBE-UPDATE-MAP-BLOCK-SIZE SI:STREAM-OUTPUT-INDEX))
  (FUNCALL-SELF :CURRENT-LENGTH-FROM-MAP))


(DEFMETHOD (LM-OUTPUT-STREAM-MIXIN :BEFORE :CLOSE) (&OPTIONAL ABORTP)
  ABORTP
  (LET ((LEN (FUNCALL-SELF :LENGTH)))
    (SETQ KEEP-RESERVED-BLOCKS NIL)
    (WHEN (PLUSP LEN)
      (FUNCALL-SELF :SET-POINTER LEN))))


(DEFMETHOD (LM-OUTPUT-STREAM-MIXIN :AFTER :CLOSE) (&OPTIONAL ABORTP)
  (LET ((DEFAULT-CONS-AREA LOCAL-FILE-SYSTEM-AREA))
    (COND
      ((AND ABORTP (NOT (FILE-CLOSED? FILE))
            (NOT APPEND-OR-OVERWRITE))
       (SETF (FILE-DELETED? FILE) T)
       (LMFS-EXPUNGE-FILE FILE)
       (WRITE-DIRECTORY-FILES (FILE-DIRECTORY FILE)))
      (T
       (LMFS-CLOSE-FILE FILE)))
    (WITHOUT-INTERRUPTS
      (SETQ LM-FILE-STREAMS-LIST (DELQ SELF LM-FILE-STREAMS-LIST)))
    (SETQ FILE NIL MAP NIL)))   ;Remove pointers into temp area.

(DEFFLAVOR LM-INPUT-STREAM
        ()
        (LM-INPUT-STREAM-MIXIN MAP-INPUT-STREAM-MIXIN
         SI:BUFFERED-INPUT-STREAM))

(DEFFLAVOR LM-OUTPUT-STREAM
        ()
        (LM-OUTPUT-STREAM-MIXIN MAP-OUTPUT-STREAM-MIXIN
         SI:BUFFERED-OUTPUT-STREAM))

(DEFFLAVOR LM-IO-STREAM ()
           (SI:IO-STREAM-INPUT-MIXIN LM-OUTPUT-STREAM))

(DEFFLAVOR LM-CHARACTER-INPUT-STREAM
        ()
        (LM-INPUT-STREAM-MIXIN MAP-INPUT-STREAM-MIXIN
         SI:BUFFERED-INPUT-CHARACTER-STREAM))

(DEFFLAVOR LM-CHARACTER-OUTPUT-STREAM
        ()
        (LM-OUTPUT-STREAM-MIXIN MAP-OUTPUT-STREAM-MIXIN
         SI:BUFFERED-OUTPUT-CHARACTER-STREAM))

(DEFFLAVOR LM-CHARACTER-IO-STREAM ()
           (SI:IO-STREAM-INPUT-MIXIN LM-CHARACTER-OUTPUT-STREAM))

(DEFFLAVOR LM-PROBE-STREAM
        ()
        (LM-STREAM-MIXIN SI:STREAM)
  ;; Kludge so that OPEN can pass this a :BYTE-SIZE :DEFAULT
  (:INIT-KEYWORDS :BYTE-SIZE))

(DEFMETHOD (LM-PROBE-STREAM :STATUS) () :CLOSED)
(DEFMETHOD (LM-PROBE-STREAM :DIRECTION) () NIL)
(DEFMETHOD (LM-PROBE-STREAM :BYTE-SIZE) () (GETF SI:PROPERTY-LIST :BYTE-SIZE))

(COMPILE-FLAVOR-METHODS ;MAP-INPUT-STREAM MAP-OUTPUT-STREAM
                        MAP-CHARACTER-INPUT-STREAM MAP-CHARACTER-OUTPUT-STREAM
                        LM-INPUT-STREAM LM-OUTPUT-STREAM
                        LM-CHARACTER-INPUT-STREAM LM-CHARACTER-OUTPUT-STREAM
                        LM-PROBE-STREAM)

(DEFUN MAKE-MAP-STREAM (FLAVOR &REST INIT-PLIST)
  (INSTANTIATE-FLAVOR FLAVOR (LOCF INIT-PLIST) NIL NIL LOCAL-FILE-SYSTEM-AREA))

(DEFUN MAKE-MAP-STREAM-IN (MAP)
  (MAKE-MAP-STREAM 'MAP-CHARACTER-INPUT-STREAM
                   :MAP MAP))

(DEFUN MAKE-MAP-STREAM-OUT ()
  (MAKE-MAP-STREAM 'MAP-CHARACTER-OUTPUT-STREAM
                   :MAP (MAP-CREATE 4)))        ;Internal things tend to be small.

;; A simple stream for reading and writing the disk configuration.
;; DISK-CONFIGURATION-BUFFER-POINTER must be bound for this stream to work.

(DEFSELECT (DISK-CONFIGURATION-STREAM DISK-CONFIGURATION-STREAM-DEFAULT)
  (:TYI (&OPTIONAL IGNORE)
    (AREF DISK-CONFIGURATION-BUFFER
          (PROG1 DISK-CONFIGURATION-BUFFER-POINTER
                 (INCF DISK-CONFIGURATION-BUFFER-POINTER))))
  (:UNTYI (CHAR)
    (DECF DISK-CONFIGURATION-BUFFER-POINTER)
    CHAR)
  (:TYO (CHAR)
    (ASET CHAR DISK-CONFIGURATION-BUFFER
          (PROG1 DISK-CONFIGURATION-BUFFER-POINTER
                 (INCF DISK-CONFIGURATION-BUFFER-POINTER)))))

(DEFUN DISK-CONFIGURATION-STREAM-DEFAULT (OP &OPTIONAL ARG1 &REST ARGS)
  (STREAM-DEFAULT-HANDLER #'DISK-CONFIGURATION-STREAM OP ARG1 ARGS))

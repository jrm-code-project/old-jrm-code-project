;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright (c) 1999 Content Integrity, Inc.
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;;          Content Integrity, Inc. CONFIDENTIAL and PROPRIETARY material.
;;;;
;;;;          Content Integrity, Inc
;;;;          Braintree Executive Office Park
;;;;          P.O. Box 850942
;;;;          Braintree, MA 02185-0942
;;;;
;;;; This software and information comprise valuable intellectual property
;;;; and trade secrets of Content Integrity, Inc., developed at substantial
;;;; expense by Content Integrity, which Content Integrity intends to
;;;; preserve as trade secrets.  This software is furnished pursuant to a
;;;; written license agreement and may be used, copied, transmitted, and
;;;; stored only in accordance with the terms of such license and with the
;;;; inclusion of the above copyright notice.  This software and
;;;; information or any other copies thereof may not be provided or
;;;; otherwise made available to any other person.  NO title to or
;;;; ownership of this software and information is hereby transferred.
;;;; Content Integrity assumes no responsibility for the use or reliability
;;;; of this software.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; File Name:     file-system.lsp
;;;; Author:        Dave Tenny
;;;;
;;;; Module Description:
;;;;
;;;; This module defines abstractions multiple file system implementaions
;;;; which are accessed through lisp file system primitives, and socket-based
;;;; protocols which interact with client agents.
;;;;
;;;; By using these abstractions, we are able to code file system maintenance
;;;; logic once to manipulate file systems of arbitrary origin.  File systems
;;;; agents potentially anticipated include those written in
;;;; Lisp, Perl, Java, and perhaps FTP.
;;;;
;;;; Client file system types anticipated include UNIX, Windows, and VMS.
;;;;
;;;; There may be multiple agents serving the same file system type, for
;;;; instnace a Perl and Java agents on most platforms, one for command line
;;;; use, and one for browser-based applet use.
;;;;
;;;; Ideally, agents for the same platform would return equivalent
;;;; FILE-DESCRIPTOR information.  However this may not always be possible
;;;; or desirable (for example, perhaps a PERL based agent for VMS will return
;;;; more accurate information than an FTP agent for VMS).
;;;;
;;;; Because of this important aspect of file system and agent interaction,
;;;; the following rule must hold true:
;;;; FILE-DESCRIPTORS ARE ONLY USABLE IN CONJUNCTION WITH THE FILE-SYSTEM
;;;; FROM WHICH THEY WERE OBTAINED.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TO-DO: we might save ourselves a lot of work, long term
;;; (supporting many client platforms if we used a standard FTP agent
;;; which runs as a java applet on the client, and support FTP
;;; services server-side.

;;; The various aspects of supporting mock-FTP on systems like VMS are
;;; the DOWNSIDE to the current approach, this is potentially a
;;; high-port maintenance module. It is further potentially
;;; complicated by things like proxy servers.  THIS IS A POTENTIALY
;;; LARGE DISADVANTAGE.

;;; The advantages of this approach are:
;;; (1) We require only one socket instead of two.
;;; (2) The socket can be the existing Web server connection
;;;     (bypassing many firewall problems), or any a freshly allocated
;;;     port if necessary (if web proxy servers have to be bypassed).
;;; (3) We have generally more flexibility in bridging various
;;;     client/server/socket problems because it's our own protocol.

;;; DESIGN NOTE:
;;; Ideally the FILE-SYSTEM-OPEN would return a real lisp STREAM object in all cases,
;;; and these objects would then be used for reading and writing.  This may still be a worthy
;;; project.   However it's a lot of work and the users of FILE-SYSTEM objects really
;;; aren't doing all the fancy things supported by streams, like FILE-POSITION, Y-OR-N-P, etc..
;;; In order to sustain our simple needs for the RAD server project for now, we use
;;; FILE-SYSTEM-READ/WRITE-LINE and READ/WRITE-BYTES.  Someday, we should
;;; do away with these and simply have FILE-SYSTEM-OPEN return streams tailored to work with the
;;; FILE-SYSTEM from which they were created.  Note however that this requires an extensible stream
;;; protocol.  Most lisps support the "Gray Streams" proposal, but still, beware, since it isn't ANSI CL.

;;; Warning!
;;;
;;; The UNIX world is rather confused about pathnames.  Is /foo/bar a directory or a file?
;;; Does it make sense to talk about files or directories that don't exist?  I think it does,
;;; so this little ambiguity has to be dealt with.  Here is the convention:  A filename that
;;; ends in a separator is a directory, otherwise it is a path.  Clear, simple, and works well
;;; with pathname parsing as provided for in common lisp.  However, it means that the clients of
;;; this package should not be sloppy!



(in-package "CSF/SERVER")

(proclaim (standard-optimizations))

(eval-when (:load-toplevel :execute)
  (export '(csf/config::*binary-file-size-limit*
            )
          "CSF/CONFIG")
  (export '(                            ;*file-system-platforms*
            file-system

            delegate-file-system/underlying-file-system

            fsa-file-system
            filtered-file-system
            lisp-file-system
            logical-file-system
            logical-file-system/change-directory
            logical-file-system/root

            file-descriptor
            file-descriptor/content-encoding
            file-descriptor/content-type
            file-descriptor/crc-background
            file-descriptor/is-file?
            file-descriptor/is-directory?
            file-descriptor/file-system
            file-descriptor/size
            file-descriptor/friendly-name
            file-descriptor/modification-date
            file-descriptor/namestring
            file-descriptor/path
            file-descriptor/read-only?
            file-descriptor/record-separator
            call-with-file-descriptor-content

            file-system/backup
            file-system/close
            file-system/copy-file
            file-system/create-directory
            file-system/delete-directory
            file-system/delete-file
            file-system/directory-exists-p
            file-system/directory-separators
            file-system/ensure-directory-and-parents
            file-system/friendly-name
            file-system/locale
            file-system/note-progress
            file-system/platform
            file-system/probe-directory
            file-system/probe-file
            file-system/rename
            file-system/record-separator
            file-system/set-read-only
            file-system/shutdown
            file-system/touch-file
            file-system/write-bytes
            file-system/write-line

            with-file-system-stream
            )))

(defparameter *binary-file-size-limit* (* (expt 2 20) 1) ;; 1 meg
  "User parameter to limit the size of binary files that ChangeSafe will load.")

(defconstant +binary-file/absolute-size-limit+ +simple-vector-8b/size-limit+
   "ChangeSafe will refuse to load a binary file larger than this.")

(defconstant +real-file-system/standard-transfer-size+ (* (expt 2 10) 8) ;; 8 k
    "This constant is used for requesting byte transfers in a real file system.
 FSAs will never be asked for more than this amount and can plan accordingly.
 (Note, if you increase this, you should check the buffer sizes in all the FSAs).")

(defconstant +real-file-system/standard-buffer-size+ (* (expt 2 10) 8) ;; 8 k
    "This constant is used for initializing the byte buffer in a real file system.
 It should be the same as the standard-transfer-size, but if it is too small,
 the buffer will be grown to accomodate a line.")

(defconstant +real-file-system/standard-buffer-growth-factor+ 1.5
  "This is how much larger to make the byte buffer when it overflows.  It should be a float larger
   than 1 and less than 2.")

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun setup-repository-logical-host ()
  ;; This makes lisp recognize "repository" as a logical host.
  ;; Setup translations for both absolute and relative logical pathnames.
  (setf (logical-pathname-translations "REPOSITORY")
    `(("**;*.*" ,(make-pathname :directory '(:relative :wild-inferiors)
                                :name :wild
                                :type :wild))
      (";**;*.*" ,(make-pathname :directory '(:relative :wild-inferiors)
                                 :name :wild
                                 :type :wild))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setup-repository-logical-host))

(defmacro with-file-system-stream ((stream file-system path &rest file-system-open-args) &body body)
  "Execute BODY as an implicit PROGN and return what BODY returns.
   STREAM is bound for the duration of BODY in to the result of FILE-SYSTEM-OPEN as applied
   to FILE-SYSTEM-OPEN-ARGS.  Upon exit from BODY, STREAM is closed with FILE-SYSTEM-CLOSE.
   Should an error be signalled during the FILE-SYSTEM-OPEN, BODY, or FILE-SYSTEM-CLOSE,
   the signal is thrown (unless arising from and trapped by BODY), and FILE-SYSTEM-CLOSE is
   called if possible.

   Example:

   (with-file-system-stream (stream file-system #p\"repository:;foo.txt\" :direction :input)
     (loop
        with buffer = nil
        while (not file-system-eof? file-system stream)
       (print (setq buffer (file-system-read-sequence file-system stream buffer)))))"

  ;; We go to some trouble to note if an error occurs during the execution of BODY,
  ;; because if a FILE-SYSTEM-CONDITION is signalled, chances are that the attempt to do
  ;; the FILE-SYSTEM-CLOSE will also fail, and will mask the original signal whose unwind is in
  ;; progress.  It's at least nice to note the original signal before invoking the close operation.

  (with-unique-names (wth-file-system-stream-fs block-var path-var)
    (let ((open-args nil))
      (dotimes (i (length file-system-open-args))
        (push (gensym "OPEN-ARG-") open-args))
      `(BLOCK ,block-var
         (LET* ((,wth-file-system-stream-fs ,file-system)
                (,path-var ,path)
                ,@(mapcar #'list open-args file-system-open-args)
                (,stream nil))
           (UNWIND-PROTECT
               (PROGN
                 (WITH-DEFERRED-INTERRUPTS  (FORMAT NIL "opening ~s" ,path-var)
                   (SETQ ,stream (FILE-SYSTEM/OPEN ,wth-file-system-stream-fs ,path-var ,@open-args)))
                 ;; We only give notice if the signal is a file-system-condition, since other
                 ;; conditions aren't as much of interest, and shouldn't necessarily interfere with
                 ;; file-system-close (though this is far from iron-clad).
                 (HANDLER-BIND
                     ((ERROR
                       (LAMBDA (CONDITION)
                           #+lispworks (DECLARE (IGNORE CONDITION))
                           (UNLESS #+allegro (TYPEP CONDITION 'EXCL:INTERRUPT-SIGNAL)
                                   #-allegro NIL
                             (FORMAT *ERROR-OUTPUT* "~%ERROR in WITH-FILE-SYSTEM-STREAM, closing file system")
                             (RETURN-FROM ,block-var NIL)))))
                   (DEBUG-MESSAGE 4 "WITH-FILE-SYSTEM-STREAM body, path is ~s" ,path-var)
                   (LOCALLY ,@body)))
             "closing file system"
             (IGNORE-ERRORS-UNLESS-DEBUGGING
              (debug-message 4 "WITH-FILE-SYSTEM-STREAM close")
              (FILE-SYSTEM/CLOSE ,wth-file-system-stream-fs ,stream))))))))

(defconstant *file-record-separators* '(:CR :LF :CRLF)
  "Possible values for record separators of text files.")

(defclass base-file-descriptor ()
  ((file-system
    :reader file-descriptor/file-system
    :initarg :file-system
    :initform (error "Required initarg :file-system is missing."))
   (path :type (or null pathname)
         :initarg :path
         :initform (error "Required initarg :path missing.")
         :reader file-descriptor/path)
   (modification-date :type integer
                      :initarg :modification-date
                      :initform (error "Required initarg :modification-date is missing.")
                      :reader file-descriptor/modification-date))
  (:documentation
   "Information which describes various essential attributes of files and directories.
      Most information in this object describes information relative to the specific agent
      which supplied the information, such as the fully qualified pathname of the file
      on the remote (agent) host, and the size of the file in bytes.

      Some data is normalized so that comparable attributes, such as file modification date,
      may be meaningfully tested without further interpretation.

      NOTE: FILE-DESCRIPTORS may only be used in conjunction with the FILE-SYSTEM which
      created them.

      NOTE: at this time we lose precision in file modification dates on some platforms."))

(defmethod graph-node-children      ((d base-file-descriptor)) nil)

;;; NOTE
;;;  This function determines whether two `files' are the `same' file.
;;; It is a critical function in the system.

;;; Right now, it simply checks for EQ-ness.
;;; If it is possible to alias files on the file system in question (perhaps with a
;;; hard link?), then this function will need to be modified to detect that.

(defvar *fd-graph-node-equal-count* 0)

(defmethod graph-node-eq ((left base-file-descriptor) (right base-file-descriptor))
  (incf *fd-graph-node-equal-count*)
  (or (eq left right)
      (and (not (eq (file-descriptor/file-system left)
                    (file-descriptor/file-system right)))
           ;; Slower case, file systems are on the same server and may overlap.

           ;; Not handled:  where file systems are on different servers, but are aliased via
           ;; some sort of sharing like NFS.
           (compatible-file-system? (file-descriptor/file-system left)
                                    (file-descriptor/file-system right))
           (error "Non compatible file systems not handled."))))

(defmethod graph-node-lessp  ((left base-file-descriptor) (right base-file-descriptor))
  (let ((platform (file-system/platform (file-descriptor/file-system left))))
    (funcall (platform-filename-lessp platform)
             (pathname->platform-namestring (file-descriptor/path left) platform)
             (pathname->platform-namestring (file-descriptor/path right) platform))))

(defclass directory-descriptor (base-file-descriptor)
  ()
  (:documentation
   "Describes directories on the client machine."))

(defmethod graph-node-children ((d directory-descriptor))
  (file-system/probe-directory (file-descriptor/file-system d)
                               (file-descriptor/path d)))

(defclass link-descriptor (base-file-descriptor)
  ((target :type (or null pathname)))
  (:documentation
   "Describes symbolic links (or their equivalent) on the client machine."))

(defclass file-descriptor (base-file-descriptor)
  ((size :type (or null non-negative-integer)
         :reader file-descriptor/size
         :initarg :size
         :initform (error "Required initarg :size is missing."))
   (mode-list :type list
              :reader file-descriptor/mode-list
              :initarg :mode-list
              :initform (error "Required initarg :mode-list is missing."))
   (crc-cache :type integer
              :accessor file-descriptor/crc-cache
              :initform -1)
   (record-separator-cache
    :accessor file-descriptor/record-separator-cache
    :initform nil)
   (content-type-cache
    :accessor file-descriptor/content-type-cache
    :initform nil)
   (content-encoding-cache
    :accessor file-descriptor/content-encoding-cache
    :initform nil))
  (:documentation
   "Information which describes various essential attributes of files and directories.
      Most information in this object describes information relative to the specific agent
      which supplied the information, such as the fully qualified pathname of the file
      on the remote (agent) host, and the size of the file in bytes.

      Some data is normalized so that comparable attributes, such as file modification date,
      may be meaningfully tested without further interpretation.

      NOTE: FILE-DESCRIPTORS may only be used in conjunction with the FILE-SYSTEM which
      created them.

      NOTE: at this time we lose precision in file modification dates on some platforms."))

(defmethod print-object ((fd link-descriptor) stream)
  (print-unreadable-object (fd stream :type t)
    (format stream "~s ~s ~s"
            (file-descriptor/file-system fd)
            (file-descriptor/path fd)
            (if *within-regression-tests*
                "(suppressed)"
                (and (file-descriptor/file-system fd)
                     (file-system/platform (file-descriptor/file-system fd))))
            ;; (if (file-descriptor-read-only? fd) :ro :rw)
            )))

(defmethod print-object ((fd directory-descriptor) stream)
  (print-unreadable-object (fd stream :type t)
    (format stream "~s ~s ~s"
            (file-descriptor/file-system fd)
            (file-descriptor/path fd)
            (if *within-regression-tests*
                "(suppressed)"
                (and (file-descriptor/file-system fd)
                     (file-system/platform (file-descriptor/file-system fd))))
            #||                         ;
            ;; DIRECTORY-DESCRIPTORs don't have a MODE-LIST.
            (if (member :read-only (file-descriptor-mode-list fd))
            "RO"
            "RW")
            ||#
            )))


(defmethod print-object ((fd file-descriptor) stream)
  ;; Note the use of :NO-READER and :NO-WRITER on class definition to avoid a ACL compilation warning
  ;; about defining PRINT-OBJECT twice in this module.
  (print-unreadable-object (fd stream :type t)
    (format stream "~s ~s ~s"
           (file-descriptor/file-system fd)
           (file-descriptor/path fd)
           (if *within-regression-tests*
               "(suppressed)"
             (and (file-descriptor/file-system fd)
                  (file-system/platform (file-descriptor/file-system fd))))
           ;; (if (file-descriptor-read-only? fd) :ro :rw)
           )))

(defun file-descriptor/is-file? (fd)
  (typep fd 'file-descriptor))

(defun file-descriptor/is-directory? (fd)
  (typep fd 'directory-descriptor))

(defun file-descriptor/is-link? (fd)
  (typep fd 'link-descriptor))

(defun file-descriptor/read-only? (file-descriptor)
  "Return T iff the file named by the descriptor is currently read-only in the file-system."
  (file-system/read-only? (file-descriptor/file-system file-descriptor) file-descriptor))

(defun (setf file-descriptor/read-only?) (new-value file-descriptor)
  "Return T iff the file named by the descriptor is currently read-only in the file-system."
  (file-system/set-read-only (file-descriptor/file-system file-descriptor) file-descriptor new-value))

(defun file-descriptor/content-encoding (fd)
  "Return the cached content type {:ZERO, :TEXT, :BINARY}, or
   call FILE-SYSTEM-DETECT-FILE-CONTENT-TYPE on this FD, cache and return its value.
   **** WARNING: THIS ROUTINE MAY CALL FILE-SYSTEM-OPEN."
  (cond ((file-descriptor/is-directory? fd) :directory)
        ((file-descriptor/content-encoding-cache fd))
        (t (setf (file-descriptor/content-encoding-cache fd)
                 :default-8bit))))

(defun file-descriptor/content-type (fd)
  "Return the cached content type {:ZERO, :TEXT, :BINARY}, or
   call FILE-SYSTEM-DETECT-FILE-CONTENT-TYPE on this FD, cache and return its value.
   **** WARNING: THIS ROUTINE MAY CALL FILE-SYSTEM-OPEN."
  (cond ((file-descriptor/is-directory? fd) :directory)
        ((file-descriptor/content-type-cache fd))
        (t (setf (file-descriptor/content-type-cache fd)
                 (file-system/detect-file-content-type
                  (file-descriptor/file-system fd) fd)))))

(defun file-descriptor/crc (fd)
  "Calculate and return  the CRC for the file indicated by FD.
   **** WARNING: THIS ROUTINE MAY CALL FILE-SYSTEM-OPEN.
   *PERFORMANCE*: this information should probably be cached once retrieved."
  (file-system/crc-file
   (file-descriptor/file-system fd) fd))

(defun file-descriptor/crc-background (fd file-type foreground-procedure)
  "Calculate and return  the CRC for the file indicated by FD.
   Run foreground-procedure immediately.
   FILE-TYPE is :binary or :text and is the type of the file in the repository.
   **** WARNING: THIS ROUTINE MAY CALL FILE-SYSTEM-OPEN.
   *PERFORMANCE*: this information should probably be cached once retrieved."
  (file-system/crc-file-background
   (file-descriptor/file-system fd) fd file-type foreground-procedure))

(defun file-descriptor/friendly-name (file-descriptor)
  "Given a FILE-DESCRIPTOR, perform the equivalent of the lisp ENOUGH-NAMESTRING operation on it,
which is actually done by the FILE-SYSTEM which created the FILE-DESCRIPTOR.

This string is not useful to identify the file in internal code, but represents a name that
would make sense to the human using the remote client.  Thus it is for pretty display only."
  (file-system/friendly-name
   (file-descriptor/path file-descriptor)
   (file-descriptor/file-system file-descriptor)))

(defun file-descriptor/namestring (fd)
  "Given a FILE-DESCRIPTOR, perform the equivalent of the lisp  FILE-NAMESTRING operation on it,
   which is actually done by the FILE-SYSTEM which created the FILE-DESCRIPTOR.
   FILE-NAMESTRING returns a string with just the NAME, TYPE, and VERSION components of a pathname.

   Normally you'd just invoke (file-system-file-namestring file-descriptor),
   but this particular variant is useful if you want to it as a key function while mapping
   on lists of file descriptors."
  (file-system/file-namestring (file-descriptor/file-system fd) fd))

(defun file-descriptor/record-separator (fd)
  "Calculate and return  the record-separator for the file indicated by FD.
   **** WARNING: THIS ROUTINE MAY CALL FILE-SYSTEM-OPEN.
   *PERFORMANCE*: this information should probably be cached once retrieved."
  (or (file-descriptor/record-separator-cache fd)
      (let ((uncached-record-separator
             (file-system/detect-file-record-separator
              (file-descriptor/file-system fd)
              (file-descriptor/path fd))))
        (setf (file-descriptor/record-separator-cache fd) uncached-record-separator)
        uncached-record-separator)))

(defun call-with-file-descriptor-content (fd record-separator receiver)
  "Apply RECEIVER to the contents of the FILE-DESCRIPTOR (by opening an reading content of FD).
   FD must be a file-descriptor.
   RECORD-TERMINATOR must be one of *FILE-RECORD-TERMINATORS*.

   RECEIVER must be a function of one argument, which takes on values according to
   the content type of the file.  If FILE-DESCRIPTOR-CONTENT-TYPE is:

    :ZERO - content is NIL.
    :BINARY - content is an array of type '(unsigned-byte 8)
    :TEXT - content is a VI-RECORD-STREAM"

  ;; *FINISH*: implement chunked input, and binary diff.  Regardless of what we stuff in this slot,
  ;; it must be diffable with VI-VALUE-SAME?.  For DEMO, we're just creating a single
  ;; array which must hold the entire binary contents, and which will diff with EQUALP.

  ;; *FINISH* *WARNING*
  ;; Relying on the size in the file descriptor for the ultimate number of bytes stored isn't
  ;; reliable, the file may change between the time the file-descriptor is obtained and the time
  ;; we actually do the I/O below.
  (if (media-type/binary? (file-descriptor/content-type fd))
      (let ((n-bytes (file-descriptor/size fd))
            (chunk (simple-vector-8b-allocate +real-file-system/standard-transfer-size+))
            (result nil))
        (when (> n-bytes 1500000)
          (warn "Current demo has not been optimized for large binary objects, and will use memory ~
                proportional to the size of the object.  ~%~
                Currently processing a binary file of size ~d bytes."
                n-bytes))
        (cond
         ((> n-bytes +binary-file/absolute-size-limit+)
          (error "File ~s has a size in excess of the capacity of the current implementation, ~%~
                 It cannot be read in."))

         ((< n-bytes *binary-file-size-limit*)
          ;; Allegro store has a bug in which it does not preserve the last byte of
          ;; unsigned-byte 8 arrays.  We workaround this by padding the array.
          ;; See FILE-CREATE and FILE-SET-CONTENTS.
          (setq result (simple-vector-8b-allocate #+allegro (+ n-bytes *binary-file-padding*)
                                                  #+lispworks n-bytes))
                                        ; (set-file-binary-size file n-bytes) ;keep this in sync with binary contents
          (with-file-system-stream (stream (file-descriptor/file-system fd) fd
                                           :direction :input
                                           :record-separator :NONE
                                           #-(and :allegro-version>= (:version>= 6 0)) :element-type
                                           #-(and :allegro-version>= (:version>= 6 0)) '(unsigned-byte 8))
                                   (loop
                                     with to-index = 0
                                     do (multiple-value-bind (result-sequence n-bytes-read)
                                            (file-system/read-bytes (file-descriptor/file-system fd) stream chunk)
                                          (declare (ignore result-sequence))
                                          (if (> n-bytes-read 0)
                                              ;; PERFORMANCE: add a starting read position to FILE-SYSTEM-READ-BYTES so
                                              ;; we don't have to do this copy.
                                              (%simple-subvector-8b-move-left chunk result 0 n-bytes-read to-index)
                                              (return)) ;return from the loop reading input
                                          (incf to-index n-bytes-read)))))
         (t;; File is too big, skip it
          (error "File ~s is a wee bit large for the current capabilities of this demo.  ~%~
                Please remove it from your set of items to check into the system."
                 (file-descriptor/namestring fd))))
        (funcall receiver result))

      (let ((fs (file-descriptor/file-system fd)))
        (with-file-system-stream (stream fs
                                         (file-descriptor/path fd)
                                         :direction :input
                                         :record-separator record-separator)
         (do ((lines '() (cons (file-system/read-line fs stream) lines)))
             ((eq (car lines) :eof) (funcall receiver (nreverse (cdr lines))))
           (debug-message 5 "Read: ~s" (car lines)))))))

;;; FILE SYSTEMS

(defconstant *file-system-condition-reasons*
    '(:NON-EXISTENT-PATH                ;file or directory doesn't exist
      :NOT-A-DIRECTORY                  ;directory expected, but not found
      :WRITE-PROTECTED                  ;file can't be overwritten
      :OUT-OF-SPACE                     ;disk is out of room
      :TARGET-EXISTS                    ;move/rename target already exists, or attempting to overwrite
                                        ; file without specifying :IF-EXISTS.
      :MODE-FAILURE                     ;invalid or "unprocessable" modes specified for SET-MODE
      :OPERATION-FAILURE                ;something relayed from some alien, generic agent operation failure
                                        ; agent-specific failure information is 'other args'
      ;; Socket-file-system specific codes
      :INVALID-AGENT-COMMAND-RESPONSE   ;FSA didn't reply properly to server request
      :UNEXPECTED-RESPONSE-CODE         ;FSA command response was not what was expected.
      :CHUNK-READ-BYTE-SHORTAGE         ;FSA didn't supply as many bytes as it said it would for a
                                        ;chunk of transmitted bytes whose length was known in advance.
      :DELETE-DIRECTORY-FAILED          ;attempt to delete a directory failed
      )
  "Reasons for which various file-system operations may fail.  Constrained to codes
   enumerated by this list.")

#+(and :allegro-version>= (:version>= 6 0))
(declaim (:fbound file-system-condition-other-args
                  file-system-condition-path
                  file-system-condition-reason))

(define-condition file-system-condition (changesafe-error)
  ((reason                              ;one of *FILE-SYSTEM-CONDITION-REASONS*
    :reader file-system-condition/reason
    :initarg :file-system-condition-reason)
   (path                                ;path of source operand
    :reader file-system-condition/path
    :initarg :file-system-condition-path)
   (other-args                          ;target path, mode arguments, etc..  catch-all
    :reader file-system-condition/other-args
    :initarg :file-system-condition-other-args))
  (:report (lambda (condition stream)
             (format stream "~s (~s) on ~a~a"
                     (type-of condition)
                     (file-system-condition/reason condition)
                     (file-system-condition/path condition)
                     (if (file-system-condition/other-args condition)
                         (format nil ", ~a" (file-system-condition/other-args condition))
                       "")))))

(defun file-system-condition-signal (reason-symbol path &rest other-args)
  "Signal a condition with the indicated REASON-SYMBOL and PATH.
   REASON-SYMBOL should be one of *FILE-SYSTEM-CONDITION-REASONS*.
   PATH should be the pathname which was active.
   OTHER-ARGS may contain target path operands for RENAME/MOVE, modes for SET-MODE, etc.."
  ;; Whant reason domain enforced so people can query the reason and deal with it intelligently
  (assert (find reason-symbol *file-system-condition-reasons*))
  (error 'file-system-condition
           :file-system-condition-reason reason-symbol
           :file-system-condition-path path
           :file-system-condition-other-args other-args))

(defclass file-system () ()
    (:documentation
     "Abstract file system, allowing manipulation of files, directories, links, etc.
      Actual work is accomplished by FILE-SYSTEM subtypes.   FILE-SYSTEM objects
      acquire and operate upon FILE-DESCRIPTOR objects by communicating with AGENTS
      implemented in various languages and supporting various protocols.")
  ;; NOTE: Subtypes are not permitted to manipulate the slots defined in the abstract FILE-SYSTEM base class.
  ;; They are maintained by qualified methods defined on the base class.
  )

(defgeneric compatible-file-system? ((left file-system) (right file-system))
  ;; True if it is possible that file might be EQ in these systems.
  ;; NOTE:  IF FILE SYSTEM ALIASING (like multiple mount points or NFS) EXISTS,
  ;; THIS COULD CAUSE DIFFICULTIES.
  (:method :around ((left file-system) (right file-system))
    (or (eq left right)                 ;same is always compatible
        (call-next-method)))
  (:method ((left file-system) (right file-system))
    nil))

(defun file-system/analyze-file-contents (buffer nBytes)
  "Analyze buffer to determine the file content type for implementations of
   FILE-SYSTEM-DETECT-FILE-CONTENT-TYPE.

   BUFFER is assumed to have zero or more bytes, don't call this routine if there was
   no file to analyze.  Buffer must be a vector, not a list.

   nBytes is the number of bytes to analyze in the buffer, since the length of the buffer may not be
   representative of the number of meaningful elements it contains."
  (check-type buffer simple-vector-8b)
  (when (zerop nBytes)
    (return-from file-system/analyze-file-contents :ZERO))
  ;; This test might want to be liberalized a bit.
  (loop
      for x of-type array-index from 0 below nBytes             ;to bound the loop
      for byte of-type (unsigned-byte 8) across (the simple-vector-8b buffer)
      as char = (code-char byte)
      do (cond ((standard-char-p char)) ;ok
               (t                       ;graphic char or unknown, check for some common whitespace
                (unless (find char '(#\space #\newline ; note that graphic-char-p is T for space/nl
                                        ;and nil for the following codes
                                     #\backspace #\tab #\rubout #\linefeed #\return #\page
                                     ;; Emacs TAGS files have these.
                                     #\null #\^a
                                     )
                              :test #'char=)
                  (debug-message 4 "Binary file assumed because of ~C (~d) character."
                                 char byte)
                  ;; Not graphic, a not a common control character
                  (return-from file-system/analyze-file-contents :BINARY)))))
  :text)

(defun file-system/backup (file-system pathname &key (pattern "bak~@[~d~]")
                                                     (copy/move :move))
  "Rename file PATHNAME on FILE-SYSTEM to a new name generated from PATTERN.

   PATTERN is a format string applied to NIL or a number, it is used to generate
   the new file extension.  The new filename is probed, and if it doesn't exist,
   the old file is renamed to the new.

   Returns the new file name."
  (check-type pathname file-pathname)
  (do ((trial-new-pathname (push-pathname-type pathname (format nil pattern nil))
                           (push-pathname-type pathname (format nil pattern counter)))
       (counter 1 (+ counter 1)))
      ((null (file-system/probe-file file-system trial-new-pathname))
       (ecase copy/move
         (:copy (file-system/copy-file file-system pathname trial-new-pathname))
         (:move (file-system/rename file-system pathname trial-new-pathname)))
       trial-new-pathname)))

(defgeneric file-system/close (file-system stream)
  (:documentation
   "Close a stream returned by FILE-SYSTEM-OPEN.

   **** WARNING ****
   This is a logical close operation which may result in a physical close operation
   on a client file system.  However you should NEVER call CLOSE on a stream returned
   by FILE-SYSTEM-OPEN.

   **** WARNING ****
   Unlike a lisp stream CLOSE operation, it is NOT permissible to call close multiple times
   on a FILE-SYSTEM, and it is not permissible to query the state of the stream or stream-handle
   after the FILE-SYSTEM-CLOSE.

   Return value: nil if close operation was successful, an exception otherwise.

   This routine MUST be overloaded by FILE-SYSTEM subtypes.")
  )

(defgeneric file-system/copy-file (file-system file-descriptor-or-path new-path)
  (:documentation
   "Copy the file described by FILE-DESCRIPTOR-OR-PATH to the place indicated by NEW-PATH.
    Return nil if we succeed, or signal a FILE-SYSTEM-CONDITION if we fail.

    FILE-DESCRIPTOR-OR-PATH must name a file, and not a directory, and NEW-PATH must
    also name a file (it may not name a directory into which the copy would preserve the file name).

    In the event that any error is signaled, the new copy is first removed (if possible or necessary),
    and any reasonable attempt is made to act as if nothing was done.

    This routine MUST be overloaded by FILE-SYSTEM subtypes.")
  (:method (fs (old-name string) new-name)
    (file-system/copy-file fs (pathname old-name) new-name))
  (:method (fs old-name (new-name string))
    (file-system/copy-file fs old-name (pathname new-name)))
  (:method :before (fs (old-name pathname) (new-name pathname))
    (declare (ignore fs))
    (check-type old-name file-pathname)
    (check-type new-name file-pathname)
    )
  )

(defgeneric file-system/crc-binary-file (file-system file-descriptor-or-path)
  (:documentation
   "Compute the CCITT-32 CRC of the file.  CRC is computed in little-endian order
    with an initial seed of *CRC-SEED*.  All bytes in the file are used in the
    computation.

    It is an error to call this on a directory.

    This routine MUST be overloaded by FILE-SYSTEM subtypes.")

  (:method :before (fs (path pathname))
    (declare (ignore fs))
    (check-type path file-pathname))
  )

(defgeneric file-system/crc-binary-file-background (file-system file-descriptor-or-path foreground-procedure)
  (:documentation
   "Compute the CCITT-32 CRC of the file.  CRC is computed in little-endian order
    with an initial seed of *CRC-SEED*.  All bytes in the file are used in the
    computation.

    It is an error to call this on a directory.

    This routine MUST be overloaded by FILE-SYSTEM subtypes.")

  (:method :before (fs (path pathname) foreground-procedure)
    (declare (ignore fs foreground-procedure))
    (check-type path file-pathname))
  )

(defgeneric file-system/crc-file (file-system file-descriptor-or-path)
  (:documentation
   "Compute the 24-bit CRC of the file.  CRC is computed in little-endian order
    with an initial seed of #xFFFFFFFF.  All bytes in the file are used in the
    computation.

    It is an error to call this on a directory.")
  (:method (fs (fd file-descriptor))
    (declare (ignore fs))
    (if (= (file-descriptor/crc-cache fd) -1)
        (let ((uncached-crc (file-system/crc-file
                             (file-descriptor/file-system fd)
                             (file-descriptor/path fd))))
          (setf (file-descriptor/crc-cache fd) uncached-crc)
          uncached-crc)
      (file-descriptor/crc-cache fd)))
  (:method (fs (fd pathname))
    (ecase (file-system/detect-file-content-type fs fd)
      ((:zero) *crc-seed*)
      ((:text) (file-system/crc-text-file fs fd))
      ((:binary) (file-system/crc-binary-file fs fd))))
  )

(defgeneric file-system/crc-file-background (file-system
                                             file-descriptor-or-path
                                             file-type
                                             foreground-procedure)
  (:documentation
   "Compute the 24-bit CRC of the file.  CRC is computed in little-endian order
    with an initial seed of #xFFFFFFFF.  All bytes in the file are used in the
    computation.

    FILE-TYPE is :binary or :text and is the type of the file in the repository.  The
    value may be nil, in which case, the heuristic based on the given FILE-DESCRIPTOR-OR-PATH
    will be used.

    It is an error to call this on a directory.")
  (:method (fs (fd file-descriptor) file-type foreground-procedure)
    (declare (ignore fs))
    (if (= (file-descriptor/crc-cache fd) -1)
        (file-system/crc-file-background
         (file-descriptor/file-system fd)
         (file-descriptor/path fd)
         file-type
         (lambda (get-crc)
             (funcall foreground-procedure
                      (lambda ()
                          (let ((uncached-crc (funcall get-crc)))
                            (setf (file-descriptor/crc-cache fd) uncached-crc)
                            uncached-crc)))))
      (funcall foreground-procedure (lambda () (file-descriptor/crc-cache fd)))))
  (:method (fs (fd pathname) file-type foreground-procedure)
    (ecase (or file-type (file-system/detect-file-content-type fs fd))
      ((:zero)   (funcall foreground-procedure (lambda () *crc-seed*)))
      ((:text)   (file-system/crc-text-file-background fs fd foreground-procedure))
      ((:binary) (file-system/crc-binary-file-background fs fd foreground-procedure))))
  )

(defgeneric file-system/crc-text-file (file-system file-descriptor-or-path)
  (:documentation
   "Compute the 24-bit CRC of the file.  CRC is computed in little-endian order
    with an initial seed of *CRC-SEED*. The CRC for a text file is computed as if
    each line in the file were separated by a newline character and, if the file
    is terminated, as if terminated by a newline character.  This is to provide
    cross-platform neutrality.

    It is an error to call this on a directory.

    This routine MUST be overloaded by FILE-SYSTEM subtypes.")

  (:method :before (fs (path pathname))
    (declare (ignore fs))
    (check-type path file-pathname))
  )

(defgeneric file-system/crc-text-file-background (file-system file-descriptor-or-path foreground-procedure)
  (:documentation
   "Compute the 24-bit CRC of the file.  CRC is computed in little-endian order
    with an initial seed of *CRC-SEED*. The CRC for a text file is computed as if
    each line in the file were separated by a newline character and, if the file
    is terminated, as if terminated by a newline character.  This is to provide
    cross-platform neutrality.

    It is an error to call this on a directory.

    This routine MUST be overloaded by FILE-SYSTEM subtypes.")

  (:method :before (fs (path pathname) foreground-procedure)
    (declare (ignore fs foreground-procedure))
    (check-type path file-pathname))
  )

(defgeneric file-system/create-directory (file-system path)
  ;; Since the directory doesn't exist, we can't probe-file it and pass a file descriptor.
  (:documentation
   "Create the indicated directory.  Return true if successful, NIL otherwise.
    This method requires the intermediate directories in the specification already exist, it will
    only create leaf nodes in the file hierarchy. ")
  (:method (fs (path string))
    (file-system/create-directory fs
                                  (platform/parse-directory-namestring (file-system/platform fs) path)))

  (:method :before (fs (path pathname))
    (declare (ignore fs))
    (check-type path directory-pathname)
    )
  )

(defun file-system/directory-separators (file-system)
  "Return the characters used to separate directory components on file-system."
  (platform/directory-separators (file-system/platform file-system)))

(defgeneric file-system/delete-directory (file-system file-descriptor-or-path &key recursive force)
  (:documentation
   "Delete the specified directory on the file system.  Return NIL if successful, or
    signal FILE-SYSTEM-CONDITION if unsuccessful.  FILE-DESCRIPTOR-OR-PATH must name a
    directory.

    If RECURSIVE is true, we'll delete all files and subdirectories, and fail only if we are
    unable to delete something.

    If RECURSIVE is NIL, we will attempt to delete only the directory, which may fail if there
    are any files in the directory.

    This routine MUST be overloaded by FILE-SYSTEM subtypes.")
  (:method (fs (namestring string) &key recursive force)
    (file-system/delete-directory fs (pathname namestring) :recursive recursive :force force))
  (:method :before (fs (path pathname) &key recursive force)
    (declare (ignore fs recursive force))
    (check-type path directory-pathname)
    )
  )

(defgeneric file-system/delete-file (file-system file-descriptor-or-path &key force)
  (:documentation
   "Delete the specified file on the file system.  Return NIL if successful, or
    signal a FILE-SYSTEM-CONDITION if unsuccessful. FILE-DESCRIPTOR-OR-PATH must name
    a file, and not a directory.

    This routine MUST be overloaded by FILE-SYSTEM subtypes.")
  (:method :before (fs (desc file-descriptor) &key force)
    (declare (ignore fs))
    (when (file-descriptor/read-only? desc)
      (if force
          (file-system/set-read-only (file-descriptor/file-system desc) desc nil)
        (file-system-condition-signal :write-protected (file-descriptor/path desc)))))

  (:method :before (fs (path pathname) &key force)
    (declare (ignore fs force))
    (check-type path file-pathname)
    )
  )

(defgeneric file-system/detect-file-content-type (file-system file-descriptor-or-path)
  (:documentation
   "Sample some bytes from the indicated file in order to determine if the file contents are text or binary.
    Return NIL if the file doesn't exist, otherwise return :ZERO, :TEXT or :BINARY.

    :ZERO means that the file had no bytes to sample (though you might reasonable treat it as text
    for initial change management purposes).

    It is an error to call this on files which are directories.

    This routine MUST be overloaded by FILE-SYSTEM subtypes.

    Note that subtypes will wish to use the utility function FILE-SYSTEM-ANALYZE-FILE-CONTENTS
    to analyze a given byte buffer.")

  (:method :before (fs (path pathname))
    (declare (ignore fs))
    (check-type path file-pathname))
  )

(defgeneric file-system/detect-file-record-separator (file-system path)
  (:documentation
   "Return the record-separator for PATH.

    PATH must name a file
    Return NIL if no such file exists.

    This routine MUST be overloaded by FILE-SYSTEM subtypes.")
  )

(defgeneric file-system/directory-exists-p (file-system file-descriptor-or-path)
  (:documentation
   "Return true if relative-path names an EXISTING directory, NIL otherwise.
    This routine MAY (optionally) be overloaded by FILE-SYSTEM subtypes.
    FILE-DESCRIPTOR-OR-PATH must specify a directory syntactically, this may be ensured
    through the use of ENSURE-DIRECTORY-PATHNAME-DESIGNATOR.")
  (:method (file-system file-descriptor-or-path)
    (let ((file-descriptor (file-system/probe-file file-system file-descriptor-or-path)))
      (and file-descriptor (file-descriptor/is-directory? file-descriptor))))
  (:method :before (fs (path pathname))
    (declare (ignore fs))
    (check-type path directory-pathname)))

(defun file-system/ensure-directory-and-parents (file-system directory-path)
  "Ensure that all parent directories of the directory indicated by DIRECTORY-PATH
   exist, creating them if necessary.  Then create the directory indicated by DIRECTORY-PATH
   if necessary.  If it should happen to designate a pre-existing file which is not a directory,
   signal an error.

   DIRECTORY-PATH should name a directory on disk.

   Return value: N/A"
  (debug-message 4 "Walking up to ~s" directory-path)
  (let ((probe nil))

    (when directory-path
      (or (setq probe (file-system/probe-file file-system directory-path))
          (file-system/ensure-directory-and-parents file-system
                                                    (logical-pathname-parent-directory directory-path)))
      (cond ((null probe) (file-system/create-directory file-system directory-path))
            ((file-descriptor/is-directory? probe))
            ;; Problem exists if we are attempting to create a
            ;; subdirectory, but there is a file with the same
            ;; name already here.
            (t (error "Attempt to create a subdirectory named ~s, but a file by that ~
                                  name already exists."
                      (file-descriptor/namestring probe))))))
  (values))                                     ;nothing meaningful returned


(defgeneric file-system/file-namestring (file-system file-descriptor-or-path)
  (:documentation
   "Given a file-system relative path,
    perform the equivalent of the lisp FILE-NAMESTRING operation on it.
    Return a string with just the NAME, TYPE, and VERSION components of a pathname.

    This routine will return nil if you pass a file descriptor/namestring which
    contains no file name/type/version information.

    This routine MAY be overloaded by FILE-SYSTEM subtypes.")
  (:method (fs (pathstring string))
    (file-namestring (platform/parse-namestring (file-system/platform fs) pathstring)))

  (:method (fs (path pathname))
    (declare (ignore fs))
    (file-namestring path))

  (:method ((fs file-system) (relative-path base-file-descriptor))
    (file-system/file-namestring (file-descriptor/file-system relative-path)
                                 (file-descriptor/path relative-path)))

  ;; This can't work!
;  (:method ((fs file-system) (relative-path logical-pathname))
;    ;; Call the platform-specific dispatcher for manipulating path namestrings.
;    (file-system-file-namestring (file-system-platform fs) relative-path))
  )



(defgeneric file-system/locale (file-system)
  (:documentation
   "Return the parsed locale of the file system."))

(defun file-system-if-does-not-exist-default (if-exists direction)
  "Calculate the ANSI spec default for IF-DOES-NOT-EXIST given values for IF-EXISTS and DIRECTION
   as specified to FILE-SYSTEM-OPEN."
  (cond ((eq direction :input)
         :error)
        ((find direction '(:overwrite :append))
         :error)
        ((and (find direction '(:output :io))
              (not (find if-exists '(:overwrite :append))))
         :create)
        ((eq direction :probe)
         nil)
        (t :error)))

(defgeneric file-system/open (file-system file-descriptor-or-path
                              &key direction record-separator element-type if-exists if-does-not-exist)
  (:documentation
   "Open a file in the file system for reading or writing.  FILE-DESCRIPTOR-OR-PATH must
   name a file, not a directory.  Subsequent I/O must be performed with FILE-SYSTEM-READ-SEQUENCE
   and FILE-SYSTEM-WRITE-SEQUENCE, and the resulting streams should ultimately be closed
   with FILE-SYSTEM-CLOSE (not CLOSE!!!).

   There are numerous caveats here
   due to the nature of agent interaction protocols, socket stream foibles in Allegro Common Lisp,
   etc, so supported keywords and return values do not match similar lisp functions.
   Keywords match those semantics for their namesakes in OPEN, unless otherwise noted.

   Default values for keywords are almost the same as for the OPEN function, except we don't
   currently support version :NEWEST path behavior in terms of its impact on keywords.

   DIRECTION must be one of    :INPUT  (default) or :OUTPUT.
                               (:IO not supported right now, but maybe for no good reason...)
   RECORD-SEPARATOR must be one of :CR :LF :CRLF or :NONE.
   ELEMENT-TYPE must be one of 'CHARACTER (default) or 'UNSIGNED-BYTE.
   IF-EXISTS must be one of    :ERROR (default), :NEW-VERSION, :OVERWRITE, :APPEND, :SUPERSEDE, or NIL.
   IF-DOES-NOT-EXIST must be   :ERROR (default), :CREATE, or NIL.

   This function returns a STREAM-HANDLE according to its arguments, or NIL if an error occurred
   and :ERROR wasn't specified for :IF-EXISTS or :IF-DOES-NOT-EXIST.

   Note that a STREAM-HANDLE isn't necessarily a lisp stream object, and may be usefully manipulated
   only by FILE-SYSTEM methods.  Some FILE-SYSTEMS may map multiple logical streams to a single
   physical stream.

   **** WARNING **** WARNING **** WARNING **** WARNING **** WARNING **** WARNING ****

   Do NOT use CLOSE on the resulting stream-handle, use FILE-SYSTEM-CLOSE on the stream
   when it is time for a logical CLOSE operation.  The reason for this is that
   FILE-SYSTEM objects may be reusing an open socket connection to do work, and if you CLOSE
   the stream, you'll cause the file system to be inoperable.

   To ensure that FILE-SYSTEM-CLOSE is used instead of CLOSE, and done in a timely
   manner, consider using the WITH-FILE-SYSTEM-STREAM macro to perform the
   FILE-SYSTEM-OPEN and FILE-SYSTEM-CLOSE calls.

   **** WARNING **** WARNING **** WARNING **** WARNING **** WARNING **** WARNING ****

   The ELEMENT-TYPE of a stream returned by this file may not be what you requested.
   This is due to the nature of socket usage and FRANZ's pecularities in socket streams
   vs. other streams.  We do this to allow multi-purpose use of a TCP connection without
   trying to open and close a socket with binary/text attributes on every FILE-SYSTEM request
   which may be operating via a socket opened as either a text or binary stream.

   In order to deal with this, clients of FILE-SYSTEM-OPEN must always check
   to see if (STREAM-ELEMENT-TYPE stream) => CHARACTER, and use READ-CHAR or READ-BYTE as
   appropriate on the returned stream based on actual stream type (or WRITE-CHAR/WRITE-BYTE).

   **** WARNING **** WARNING **** WARNING **** WARNING **** WARNING **** WARNING ****

   NOTE: At the time of this writing,
   FILE-SYSTEM objects support only one open file at a time.  Attempting to call FILE-SYSTEM-OPEN
   multiple times without intervening calls to FILE-SYSTEM-CLOSE results in an error.
   We may augment the protocols to support multiple open streams at a later time, possibly
   in order to support operations like MOVE, if not for general ease of use.

   This routine MUST be overloaded by FILE-SYSTEM subtypes.")

  (:method :before (fs path &rest args)
    (declare (ignore path args))
    (file-system/platform fs))

  (:method :before (fs (path pathname) &rest args)
    ;; Check to see if file system already has open stream.
    (declare (ignore fs args))
    (check-type path file-pathname))
  )

(defgeneric file-system/note-progress (file-system status-text percentage-done)
  (:documentation
   "Relay a note to the file-system, which often is working through a proxy file system agent,
    to display some sort of progress message.

    STATUS-TEXT is a string which typically updates a single status line, though a poor man's
    progress indicator may simply print the text one per line.

    PERCENTAGE-DONE is an integer from 0 to 100 describing the percent completion of the overall process
    employing the file-system.

    Either argument may be NIL, however it is not particularly useful to call this method with both
    arguments NIL.

    Return value: T if the file-system actually does something with your indicator, in which case
    it is probably useful to consinue calling the progress notes, or NIL if the file-system is
    ignoring your advisories, and continued calls are not necessarily useful.

    This routine MUST be overloaded by FILE-SYSTEM subtypes."))

(defgeneric file-system/platform (file-system)
  (:documentation
   "Return one of *PLATFORMS* which identifies the agent host conventions
    which may be necessary for interpretation of file contents on the server.

    This routine MUST be overloaded by FILE-SYSTEM subtypes."))

(defgeneric file-system/probe-file (file-system path &key known-p)
  (:documentation
   "Return a fully initialized FILE-DESCRIPTOR object for the file indicated the agent-relative PATH.
    Do not attempt special pathnames for 'current' or 'parent' directory, use methods specifically
    defined for retrieving this information.

    PATH may name a file or directory.

    There is no FILE-DESCRIPTOR binding for this method, since this method is the producer for
    most FILE-DESCRIPTORS.

    If KNOWN-P is true, we can sometimes optimize out an extra call to PROBE-FILE or equivalent to verify
    the file's existence.  Specify TRUE for this only if you're sure.

    Return NIL if no such file exists.

    This routine MUST be overloaded by FILE-SYSTEM subtypes.")
  )

(defgeneric file-system/read-bytes (file-system stream-handle sequence)
  (:documentation
   "Read from STREAM-HANDLE which was returned by FILE-SYSTEM-OPEN on the indicated FILE-SYSTEM.
    This operation is similar but not identical to READ-SEQUENCE on binary streams.

    SEQUENCE may have one of two types:
    (1) '(SIMPLE-ARRAY (UNSIGNED-BYTE 8)).

        If this is specified we will read as many bytes as the array can hold, or as are available
        from the input stream.

    (2) NULL

        We will cons a fresh buffer and return it, it will not be reused unless you
        specify the buffer in a subsequent call.

    STREAM-HANDLE should be opened with :ELEMENT-TYPE '(UNSIGNED-BYTE 8).

    This method returns TWO values:
    (1) The sequence passed or consed to contain the bytes, or NIL if EOF is encountered.
    (2) An integer which is the index of the first character in SEQUENCE which was not updated.
        If the sequence is filled, then this value is equal to the length of the sequence.
        If the value returned is zero, this usually indicates an EOF condition on the stream.

    The resulting sequence is always filled starting with the first (really zero'th) element.

    This routine MUST be overloaded by FILE-SYSTEM subtypes.")
  (:method :before (fs stream-handle sequence)
    (declare (ignore fs stream-handle))
    (check-type sequence (or null simple-vector-8b))))

(defgeneric file-system/read-line (file-system stream-handle)
  ;; NOTE: we require the stream-handle in anticipation of supporting multiple concurrent open
  ;; streams on a single FILE-SYSTEM, even though this isn't initially supported.
  (:documentation
   "Read from STREAM-HANDLE which was returned by FILE-SYSTEM-OPEN on the indicated FILE-SYSTEM.
    Similar to READ-LINE, this operation  returns one textual record stripped of any
    line terminating characters.

    It is an error to call this function if FILE-SYSTEM-EOF? returns true on the indicated STREAM.
    The STREAM-HANDLE should be opened with element-type 'CHARACTER.

    This method returns TWO values:  LINE, MISSING-NEWLINE-P
    (1) The primary value, LINE, is of type (SIMPLE-ARRAY 'CHARACTER)
        or :EOF if there were no chars to read.
    (2) The secondary value, MISSING-NEWLINE-P is false if the line was terminated by a newline,
        or true if the line was terminated by the end of file for the stream
        (or if the line is the eof-value).

    Note that the second value is completely consistent with the lisp READ-LINE function.

    This routine MUST be overloaded by FILE-SYSTEM subtypes."))

(defgeneric file-system/read-only? (file-system file-descriptor-or-path)
  (:documentation
   "Returns whether the file is read-only or read-write.

    This routine MUST be overloaded by FILE-SYSTEM subtypes.")

  (:method :before (fs (path pathname))
    (declare (ignore fs))
    (check-type path file-pathname))
  )

(defgeneric file-system/record-separator (file-system)
  (:documentation
   "Return one of :CR, :LF, :CRLF, or :NONE which identifies the agent host conventions
    which may be necessary for interpretation of file contents on the server.

    This routine MUST be overloaded by FILE-SYSTEM subtypes."))

(defgeneric file-system/stream-eof? (file-system)
  (:documentation
   "Returns T if the current open stream on <file-system> is at the end of file."))

(defgeneric file-system/shutdown (file-system &key success)
  (:documentation
   "Instruct the file system to shut down.  For some file systems, this is a NOP.
    However for others, such as those based on socket-based agents, instructing the file
    system to shut down is synonymous with instructing the file system agent to shut down.

    AFTER INVOKING SHUTDOWN, A FILE-SYSTEM OBJECT MAY NO LONGER BE USED.

    SUCCESS, if specified, indicates the success status of the overall file system operation.
    Its use in file-system subtypes is optional.  It should generally default to T,
    valid values are T or NIL.

    Return NIL if successful, otherwise signal a FILE-SYSTEM-CONDITION.

    This routine MUST be overloaded by FILE-SYSTEM subtypes."))

(defgeneric file-system/set-read-only (file-system file-descriptor-or-path read-only?)
  (:documentation
   "Make the file be read-only or read-write by whatever mechanism the operating system
    provides.  Return NIL if successful, or signal a FILE-SYSTEM-CONDITION otherwise.

    This routine MUST be overloaded by FILE-SYSTEM subtypes.")
  (:method :before (fs (path pathname) read-only?)
    (declare (ignore fs read-only?))
    (check-type path file-pathname))
  )

(defgeneric file-system/rename (file-system file-descriptor-or-path new-path)
  (:documentation
   "Attempt to rename the file or directory specified by FILE-DESCRIPTOR-OR-PATH
    to NEW-PATH.

    Rename operations never move files.  Thus NEW-PATH should contain only changes to
    file name, type, and potentially version information.  All other components of NEW-PATH should
    be unspecified, or identical to those in FILE-DESCRIPTOR-OR-PATH.

    Return NIL if successful, or signal a FILE-SYSTEM-CONDITION.

    This routine MUST be overloaded by FILE-SYSTEM subtypes.")
  (:method (fs (old-name string) new-name)
    (file-system/rename fs (pathname old-name) new-name))
  (:method (fs old-name (new-name string))
    (file-system/rename fs old-name (pathname new-name)))
  (:method :before (fs (old-name pathname) (new-name pathname))
    (declare (ignore fs))
    (assert (or (and (file-pathname? old-name)
                     (file-pathname? new-name))
                (and (directory-pathname? old-name)
                     (directory-pathname? new-name)))))
  )

(defgeneric file-system/touch-file (file-system file-descriptor-or-path
                                    timestamp
                                    &key if-does-not-exist
                                         set-read-only
                                         set-executable)
  (:documentation
   "Set the timestamp on the indicated file to reflect the current system time on the system
    on which a file-system resides.

    If the file doesn't exist, control depends on the IF-DOES-NOT-EXIST keyword, which defaults to
    :IGNORE but may also be :CREATE.  If the file doesn't exist and you don't specify :CREATE,
    no action is taken.

    If SET-READ-ONLY is T, also set the file to read-only.
    If SET-EXECUTABLE is T, also set the file to be executable.
    This is because touch and chmod commands almost always occur together, and this will save much time.

    The file, if it already exists, must not name a directory, to do otherwise 'is an error'.

    Touching a file can be expensive, especially for zero length files which must be opened twice.

    Return true if the file was touched (or created), and NIL if the file was neither created nor
    touched.

    We will probably want to add a 'set-specific-time' capability to this later.

    This routine MUST be overloaded by FILE-SYSTEM subtypes.")
  (:method :before (fs (path pathname) timestamp
                    &key (if-does-not-exist :ignore)
                         set-read-only
                         set-executable)
    (declare (ignore fs timestamp if-does-not-exist set-read-only set-executable))
    (check-type path file-pathname))
  )

(defgeneric file-system/write-bytes (file-system stream-handle sequence &key end)
  (:documentation
   "Write SEQUENCE to the STREAM-HANDLE which was returned by FILE-SYSTEM-OPEN on the indicated FILE-SYSTEM,
    ans which was opened with element-type '(UNSIGNED-BYTE 8)

    SEQUENCE must be of type (ARRAY '(UNSIGNED-BYTE 8)).
    STREAM-HANDLE is used to derive a stream logically of element-type '(UNSIGHED-BYTE 8).

    END, if specified, is the number of characters in SEQUENCE which should be written.
    It is an exclusive 'upper bounding-index' one larger than any actual valid index, and defaults
    to the length of the sequence.

    Return NIL if successful, otherwise signal a FILE-SYSTEM-CONDITION.

    This routine MUST be overloaded by FILE-SYSTEM subtypes."))

(defgeneric file-system/write-line (file-system stream-handle sequence &key end suppress-termination)
  (:documentation
   "Write SEQUENCE to the STREAM-HANDLE which was returned by FILE-SYSTEM-OPEN on the indicated FILE-SYSTEM.
    STREAM-HANDLE must reflect a stream opened with element-type CHARACTER.

    This routine is similar to WRITE-LINE except with respect to terminating character conventions.

    SEQUENCE represents a logical line of text, and should not possess any line terminating characters.
    The file-system/file-system-agent will append them as appropriate.  If SUPPRESS-TERMINATION is true,
    no line termination characters will be written.  This should be true, if ever, only for the
    very last record of a file. SEQUENCE must be of type (SIMPLE-ARRAY CHARACTER).

    STREAM-HANDLE is used to derive a stream logically of element-type CHARACTER.
    (File system subtypes may actually be using binary mode streams for the real work however).

    END, if specified, is the number of characters in SEQUENCE which should be written.
    It is an exclusive 'upper bounding-index' one larger than any actual valid index.

    Return NIL if successful, otherwise signal a FILE-SYSTEM-CONDITION.

    This routine MUST be overloaded by FILE-SYSTEM subtypes."))

(defmethod graph-roots ((fs file-system))
  ;; This method should be defined on LOGICAL-FILE-SYSTEM rather than
  ;; FILE-SYSTEM, but I don't know what the appropriate roots are for
  ;; other kinds of file system and SERVER::TEST-1A will break if we
  ;; change it.
  (file-system/probe-directory fs #p"REPOSITORY:;"))


;;;
;;; REAL-FILE-SYSTEM
;;;

(defclass real-file-system (file-system)

  ;; Right now, a FILE-SYSTEM supports only one open file at a time.
  ;; The following slots are both maintained by FILE-SYSTEM-OPEN and FILE-SYSTEM-CLOSE.
  ;; Later we may allow multiple open files, in which both slots will become lists and need to be treated
  ;; accordingly.

  (
   ;; FILE-DESCRIPTOR or path being processed by FILE-SYSTEM-OPEN, limit 1, or nil if nothing open.
   (open-file-descriptor-or-path
    :accessor real-file-system/open-file-descriptor-or-path
    :initform nil)

   (open-file-record-separator
    :accessor real-file-system/open-file-record-separator)

   ;; ELEMENT-TYPE for the open file.
   (open-file-element-type
    :accessor real-file-system/open-file-element-type)

   ;; for every open file descriptor, there is an entity which is an open file system stream
   ;; indicator.  Note that these may not be real streams, they may be integers or other handles.
   ;; This is subject to interpretation by subtypes, some of which will have multiple physical streams
   ;; corresponding to multiple logical streams (when and if we support multiple streams), and others
   ;; which will have only one physical stream for multiple logical streams.
   (open-file-stream-handle   ;logical stream handle associated with open FILE-DESCRIPTOR, or nil
    :accessor real-file-system/open-stream-handle)

   (stream-eof?                 ;true if open stream has reached EOF
    :accessor file-system/stream-eof?)

;   ;; This buffer is of type SIMPLE-ARRAY, in order to allow efficient element access in compiled code.
;   ;; That means that the length of the buffer isn't necessarily reflective of the number of active
;   ;; elements, which may be computed from buffer-position and buffer-limit below.
   (byte-buffer :type simple-vector-8b
                :accessor real-file-system/byte-buffer
                :initform (simple-vector-8b-allocate +real-file-system/standard-buffer-size+))
                                        ;for read-byte and read-line requests
                                        ; depending on stream type
;   ;; current position (start of logical line) in data we've cached
;   ;; probably only used for text-mode operations
   (buffer-position
    :accessor real-file-system/buffer-position
    :type array-index
    :initform 0)

;   ;; first unused position, not necessarily same as
;   ;; buffer length! (buffer-size supplied by FSA in READ-* calls)
;   ;; number of bytes in buffer is always
;   ;; (- buffer-limit buffer-position)
   (buffer-limit
    :accessor real-file-system/buffer-limit
    :type array-index
    :initform 0)
   )

  (:documentation
   "Abstract base class representing an actual file system (as opposed to a logical file sytem.)
      This class also provides for buffering and text file translation.")
  )

(defun file-system/has-open-stream? (file-system &optional stream-handle)
  "Unexported method to determine if the file system has an open stream.
   If STREAM-HANDLE is nil, return TRUE if the file system has any open stream, NIL otherwise.
   If STREAM-HANDLE is not nil, return TRUE if the file system has the specified stream open, NIL otherwise."
  (if stream-handle
      (eq stream-handle (real-file-system/open-stream-handle file-system))
    (real-file-system/open-stream-handle file-system)))

(defmethod file-system/close :around ((fs real-file-system) stream-handle)
  ;; Note that for a lisp-file-system, stream-handle will always be a stream, or it's a programming error
  (unless (file-system/has-open-stream? fs stream-handle)
    (error "Stream is not currently open on file-system: ~s ~s" stream-handle fs))
  (call-next-method)
  (setf (real-file-system/open-file-descriptor-or-path fs) nil)
  (setf (real-file-system/open-stream-handle fs) nil)
  (setf (real-file-system/open-file-record-separator fs) nil)
  (setf (real-file-system/open-file-element-type fs) nil)
  nil)

(defmethod file-system/open :before ((fs real-file-system) file-descriptor-or-path
                                     &key direction element-type record-separator if-exists if-does-not-exist)
  (declare (ignore direction element-type record-separator if-exists if-does-not-exist))
  (when (real-file-system/open-file-descriptor-or-path fs)
    (error "Attempt to open multiple files when via FILE-SYSTEM-OPEN.  Files must be opened ~
              serially and with intervening calls to FILE-SYSTEM-CLOSE.  Already open: ~s, ~
              attempting to open: ~s"
           (real-file-system/open-file-descriptor-or-path fs)
           file-descriptor-or-path)))

(defmethod file-system/open :around ((fs real-file-system) file-descriptor-or-path
                                     &key direction record-separator element-type if-exists if-does-not-exist)
  (declare (ignore direction if-exists if-does-not-exist))
  (when (equal element-type '(unsigned-byte 8))
    (assert (eq record-separator :NONE)))
  (when (eq element-type 'character)
    (assert (member record-separator *file-record-separators*)))

  (let ((stream-handle (call-next-method)))
    (debug-message 5 "Setting stream handle")
    (when stream-handle
      (setf (real-file-system/open-file-descriptor-or-path fs) file-descriptor-or-path)
      (setf (real-file-system/open-file-record-separator fs) record-separator)
      (setf (real-file-system/open-file-element-type fs) element-type)
      (setf (real-file-system/open-stream-handle fs) stream-handle)

      ;; Reset these, necessary for READ-LINE to operate correctly on
      ;; non-first file reads in SFS
      (setf (real-file-system/buffer-position fs) 0)
      (setf (real-file-system/buffer-limit fs) 0)
      (setf (file-system/stream-eof? fs) nil)
      stream-handle)))

(defgeneric real-file-system/refill-buffer (fs stream-handle)
  (:documentation
   "Refill (or extend) the byte-buffer.  This is called when more characters
    are desired.  If the buffer is near empty (buffer-position is near buffer-limit),
    This should move the active bytes to the beginning of the buffer and fill the
    tail of the buffer.  If the buffer is full (buffer-position is zero and
    buffer-limit is = (length buffer), then more bytes are needed.  The buffer should
    be extended and more bytes read.

    If an EOF is encountered when filling the buffer, the STREAM-EOF? slot must
    be set to T.

    This routine MUST be overloaded by FILE-SYSTEM subtypes."
   ;; :super-class-list (file-system) ;; huh?
   )
  (:method ((fs real-file-system) stream-handle)
    (declare (ignore stream-handle))
    (error "Method REAL-FILE-SYSTEM/REFILL-BUFFER was not overloaded by the subtype."))
  )

(defun real-file-system/ensure-byte-buffer (fs n-bytes)
  "Use this function as an accessor for the BYTE-BUFFER slot where you want to ensure
  that the buffer is allocated and is sufficient to hold at least N-BYTES of data."
  (declare (type array-index n-bytes)
           ;; #+allegro (:explain :calls :types)
           )
  (let ((buffer (the (simple-vector-8b *) (real-file-system/byte-buffer fs))))
    ;(declare (type (simple-vector-8b *) buffer))
    (if (> n-bytes (simple-vector-8b-length buffer))
        (setf (real-file-system/byte-buffer fs)
              (simple-vector-8b-allocate (max +real-file-system/standard-buffer-size+
                                              (* n-bytes 2))))
      buffer)))

(defgeneric real-file-system/read-bytes-into-some-buffer (file-system stream n-bytes buffer)
  (:documentation
   "Utility routine to read N-BYTES into BUFFER, which must be at least N-BYTES in size,
   and print out the usual traces.

   Returns NIL.

   It is an error if we don't receive those N-BYTES from the input stream.

   This routine MUST be overloaded by REAL-FILE-SYSTEM subtypes."
   ;; :super-class-list (file-system) ;; huh?
   )
  (:method ((fs real-file-system) stream n-bytes buffer)
    (declare (ignore stream n-bytes buffer))
    (error "Method REAL-FILE-SYSTEM/READ-BYTES-INTO-SOME-BUFFER was not overloaded by the subtype."))
  )

(defun real-file-system/read-line-cr (sfs stream-handle)
  (let ((next-cr (position *ascii-carriage-return*
                           (real-file-system/byte-buffer sfs)
                           :start (real-file-system/buffer-position sfs)
                           :end (real-file-system/buffer-limit sfs))))
    (if (null next-cr)
        (if (file-system/stream-eof? sfs)
            (if (= (real-file-system/buffer-position sfs)
                   (real-file-system/buffer-limit sfs))
                (values :EOF t)
              (let ((string (csf/utility::bytes-to-string
                             (real-file-system/byte-buffer sfs)
                                             :start (real-file-system/buffer-position sfs)
                                             :end (real-file-system/buffer-limit sfs))))
                (setf (real-file-system/buffer-position sfs) 0)
                (setf (real-file-system/buffer-limit sfs) 0)
                (values string t)))
          (progn
            (real-file-system/refill-buffer sfs stream-handle)
            ;; Try again
            (real-file-system/read-line-cr sfs stream-handle)))
      (let ((result (csf/utility::bytes-to-string
                     (real-file-system/byte-buffer sfs)
                                     :start (real-file-system/buffer-position sfs)
                                     :end next-cr)))
        (setf (real-file-system/buffer-position sfs) (+ next-cr 1))
        (values result nil)))))

(defun real-file-system/read-line-lf (sfs stream-handle)
  (let ((next-lf (position *ascii-linefeed*
                           (real-file-system/byte-buffer sfs)
                           :start (real-file-system/buffer-position sfs)
                           :end (real-file-system/buffer-limit sfs))))
    (if (null next-lf)
        (if (file-system/stream-eof? sfs)
            (if (= (real-file-system/buffer-position sfs)
                   (real-file-system/buffer-limit sfs))
                (values :EOF t)
              (let ((string (csf/utility::bytes-to-string
                             (real-file-system/byte-buffer sfs)
                                             :start (real-file-system/buffer-position sfs)
                                             :end (real-file-system/buffer-limit sfs))))
                (setf (real-file-system/buffer-position sfs) 0)
                (setf (real-file-system/buffer-limit sfs) 0)
                (values string t)))
          (progn
            (real-file-system/refill-buffer sfs stream-handle)
            ;; Try again
            (real-file-system/read-line-lf sfs stream-handle)))
      (let ((result (csf/utility::bytes-to-string
                     (real-file-system/byte-buffer sfs)
                                     :start (real-file-system/buffer-position sfs)
                                     :end next-lf)))
        (setf (real-file-system/buffer-position sfs) (+ next-lf 1))
        (values result nil)))))

(defun scan-buffer-for-crlf (buffer start limit)
  ;; Critical speed code.
  (declare #.(performance-optimizations)
           ;(type array-index start limit)
           ;(type simple-vector-8b buffer)
           #+allegro (:explain :calls)
           )
  (unless (>= start limit)              ; buffer is empty,
    (do ((scan (+ start 1) (+ scan 1)))
        ((= scan limit) nil)
      ;(declare (type array-index scan))
      (when (and (= (aref buffer scan) *ascii-linefeed*)
                 (= (aref buffer (1- scan)) *ascii-carriage-return*))
        (return-from scan-buffer-for-crlf (1- scan))))))

(defun real-file-system/read-line-crlf (sfs stream-handle)
  (let ((next-crlf (scan-buffer-for-crlf
                    (real-file-system/byte-buffer sfs)
                    (real-file-system/buffer-position sfs)
                    (real-file-system/buffer-limit sfs))))
    (if (null next-crlf)
        (if (file-system/stream-eof? sfs)
            (if (= (real-file-system/buffer-position sfs)
                   (real-file-system/buffer-limit sfs))
                (values :EOF t)
              (let ((string (csf/utility::bytes-to-string
                             (real-file-system/byte-buffer sfs)
                                              :start (real-file-system/buffer-position sfs)
                                              :end (real-file-system/buffer-limit sfs))))
                (setf (real-file-system/buffer-position sfs) 0)
                (setf (real-file-system/buffer-limit sfs) 0)
                (values string t)))
          (progn
            (real-file-system/refill-buffer sfs stream-handle)
            ;; Try again
            (real-file-system/read-line-crlf sfs stream-handle)))
      (let ((result (csf/utility::bytes-to-string
                     (real-file-system/byte-buffer sfs)
                                       :start (real-file-system/buffer-position sfs)
                                       :end next-crlf)))
        (setf (real-file-system/buffer-position sfs) (+ next-crlf 2))
        (values result nil)))))


;;;
;;; LISP-FILE-SYSTEM (see generic function documentation for semantics and arguments)
;;;

(defclass lisp-file-system (real-file-system)
  ()
  (:documentation "A FILE-SYSTEM subtype which performs via lisp I/O primitives on lisp-local files."))

(defmethod print-object ((object lisp-file-system) stream)
  (print-unreadable-object (object stream :type t)))

(defmethod file-system/close ((fs lisp-file-system) (stream-handle stream))
  ;; Note that for a lisp-file-system, stream-handle will always be a stream, or it's a programming error
  (handler-case (close stream-handle)
    (file-error (condition)
      (file-system-condition-signal :operation-failure
                                    (real-file-system/open-file-descriptor-or-path fs)
                                    condition)))
  )

(defun deduce-record-separator (file)
  (with-open-file (in file :direction :input
                   #-(and :allegro-version>= (:version>= 6 0)) :element-type
                   #-(and :allegro-version>= (:version>= 6 0)) '(unsigned-byte 8)
                   :if-does-not-exist :error)
    (declare #.(performance-optimizations))
    ;; Use value of 1000. to indicate eof.
    (do ((this-char (read-byte in nil 1000.) (read-byte in nil 1000.))
         (previous-char 0 this-char)
         (lf-count 0 (if (and (= this-char 10.)
                              (not (= previous-char 13.)))
                         (1+ lf-count)
                       lf-count))
         (cr-count 0 (if (and (= previous-char 13.)
                              (not (= this-char 10.)))
                         (1+ cr-count)
                       cr-count))
         (crlf-count 0 (if (and (= previous-char 13.)
                                (= this-char 10.))
                           (1+ crlf-count)
                         crlf-count)))
        ((= this-char 1000.)            ;eof = 1000.
         (if (= previous-char 13.)
             (incf cr-count))
         (let ((crlf-wins?
                (and
                 (>= crlf-count lf-count)
                 (>= crlf-count cr-count)))
               (lf-wins?
                (and
                 (>= lf-count crlf-count)
                 (>= lf-count cr-count)))
               (cr-wins?
                (and
                 (>= cr-count crlf-count)
                 (>= cr-count lf-count)))
               (default (default-record-separator)))
           ;; Ties favor default
           (cond ((and (eq default :crlf) crlf-wins?) :crlf)
                 ((and (eq default :lf) lf-wins?) :lf)
                 ((and (eq default :cr) cr-wins?) :cr)
                 (crlf-wins? :crlf)
                 (lf-wins? :lf)
                 (cr-wins? :Cr)
                 (t default))))
      ;(declare (type (integer 0 257) this-char previous-char)
      ;         (type fixnum cr-count lf-count crlf-count))
      )))

(defmethod file-system/copy-file ((fs lisp-file-system) (path file-descriptor) new-path)
  (file-system/copy-file
   (file-descriptor/file-system path)
   (file-descriptor/path path)
   new-path))

(defmethod file-system/copy-file ((fs lisp-file-system) (path pathname) (new-path pathname))
  (os-copy-file path new-path)
  nil)

;; slow, improve someday
(defmethod file-system/crc-binary-file ((fs lisp-file-system) (path pathname))
  (with-open-file (stream path
                   #-(and :allegro-version>= (:version>= 6 0)) :element-type
                   #-(and :allegro-version>= (:version>= 6 0)) '(unsigned-byte 8) :direction :input)
    (do ((byte (read-byte stream nil nil) (read-byte stream nil nil))
         (crc *crc-seed* (byte-crc crc byte)))
        ((null byte) crc)
      )))

(defmethod file-system/crc-binary-file-background ((fs lisp-file-system) (path pathname) foreground-procedure)
  (let ((crc (file-system/crc-binary-file fs path)))
    (funcall foreground-procedure (lambda () crc))))

;;; Ugly, but this is the reference implementation.  Whatever
;;; you do to compute CRC's in any file system agent must match what
;;; this function returns.

(defmethod file-system/crc-text-file ((fs lisp-file-system) (path pathname))
  (with-file-system-stream (stream fs path
                            :element-type 'character :direction :input
                            :record-separator   (deduce-record-separator path))
   (named-let loup ((crc *crc-seed*))
     (multiple-value-bind (line unterminated)
         (file-system/read-line fs stream)
       (if (eq line :eof)
           crc
           (let ((new-crc (string-crc crc line)))
             (loup
              (if unterminated
                  new-crc
                  (byte-crc new-crc *ascii-linefeed*)))))))))

(defmethod file-system/crc-text-file-background ((fs lisp-file-system) (path pathname) foreground-procedure)
  (let ((crc (file-system/crc-text-file fs path)))
    (funcall foreground-procedure (lambda () crc))))

(defmethod file-system/create-directory ((fs lisp-file-system) (path pathname))
  (ensure-directories-exist path))

(defmethod file-system/delete-directory ((fs lisp-file-system) (path directory-descriptor) &key recursive force)
  (file-system/delete-file (file-descriptor/file-system path) (file-descriptor/path path)
                           :recursive recursive :force force))

(defmethod file-system/delete-directory ((fs lisp-file-system) (path pathname) &key recursive force)
  (handler-case
      (if recursive
          (progn (when force
                   (transitive-closure path
                     (lambda (path ignore)
                         (declare (ignore ignore))
                         (when (file-pathname? path)
                           (os-set-file-read-only (pathname->platform-namestring path (file-system/platform fs)) nil)))
                     nil))
                 (delete-directory path :force force))
        (progn (when force
                 (mapc (lambda (path)
                           (when (file-pathname? path)
                             (os-set-file-read-only (pathname->platform-namestring path (file-system/platform fs)) nil)))
                       (directory path)))
               (delete-directory path :force force)))
    (file-error (condition)
      (file-system-condition-signal :operation-failure path condition)))
  nil)

(defmethod file-system/delete-file ((fs lisp-file-system) (path file-descriptor) &key force)
  (file-system/delete-file (file-descriptor/file-system path) (file-descriptor/path path) :force force))

(defmethod file-system/delete-file ((fs lisp-file-system) (path pathname) &key force)
  (when force
    (os-set-file-read-only path nil))
  (handler-case (delete-file path)
    (file-error (condition)             ;delete-file failed, translate into a FILE-SYSTEM condition
      (file-system-condition-signal :operation-failure path condition)))
  nil)

(defmethod file-system/detect-file-content-type ((fs lisp-file-system) (relative-path file-descriptor))
  (file-system/detect-file-content-type
   (file-descriptor/file-system relative-path)
   (file-descriptor/path relative-path)))

(defmethod file-system/detect-file-content-type ((fs lisp-file-system) (path pathname))
  (if (not (file-system/directory-exists-p fs
                                           (make-pathname :name nil :type nil :version nil
                                                          :defaults path)))
      :binary
    (with-open-file (stream path
                     #-(and :allegro-version>= (:version>= 6 0)) :element-type
                     #-(and :allegro-version>= (:version>= 6 0)) '(unsigned-byte 8)
                     :direction :input :if-does-not-exist nil)
      (and stream
           (let ((buffer (simple-vector-8b-allocate 1024)))
             (file-system/analyze-file-contents buffer
                                                (read-sequence buffer stream)))))))

(defmethod file-system/detect-file-record-separator ((fs lisp-file-system) (path string))
  (file-system/detect-file-record-separator fs (platform/parse-namestring (file-system/platform fs) path)))

(defmethod file-system/detect-file-record-separator ((fs lisp-file-system) (path pathname))
  (deduce-record-separator path))

(defmethod real-file-system/refill-buffer ((sfs lisp-file-system) stream-handle)
  (let* ((head-length (- (the array-index (real-file-system/buffer-limit sfs))
                         (the array-index (real-file-system/buffer-position sfs))))
         (bytes-desired (min (- (simple-vector-8b-length (real-file-system/byte-buffer sfs)) head-length)
                             +real-file-system/standard-transfer-size+)))
    ;(declare (type array-index head-length bytes-desired))

    (when (zerop bytes-desired)
      ;; Buffer is full, and we need more bytes.  Grow the buffer.
      (debug-message 4 "Growing lisp file system buffer.")
      (setf (real-file-system/byte-buffer sfs)
            (simple-vector-8b-adjust (real-file-system/byte-buffer sfs)
                              (ceiling
                               (* (simple-vector-8b-length (real-file-system/byte-buffer sfs))
                                  +real-file-system/standard-buffer-growth-factor+))
                              0))
        (setq bytes-desired (min (- (simple-vector-8b-length (real-file-system/byte-buffer sfs)) head-length)
                                 +real-file-system/standard-transfer-size+)))

    ;; Move unscanned lines to head of buffer.
    (unless (zerop head-length)
      (%simple-subvector-8b-move-left (real-file-system/byte-buffer sfs)
                               (real-file-system/buffer-position sfs)
                               (real-file-system/buffer-limit sfs)
                               (real-file-system/byte-buffer sfs)
                               0))
    ;; Fill tail of buffer
    (let ((bytes-available (min (- (file-length stream-handle) (file-position stream-handle))
                                bytes-desired)))
      ;(declare (type array-index bytes-available))
      ;; File system agent will give us all we ask for unless EOF is seen.
      ;; This avoids another call.
      (when (< bytes-available bytes-desired)
        (setf (file-system/stream-eof? sfs) t))
      (named-let luup ((start head-length)
                       (end (+ head-length bytes-available)))
                 (debug-message 5 "Start: ~d, End: ~d" start end)
                 (let ((count (- (read-sequence (real-file-system/byte-buffer sfs)
                                                stream-handle
                                                :start start
                                                :end end)
                                 start)))
                   ;(declare (type array-index count))
                   (when (< count bytes-available)
                     (luup (+ start count) end))))

      (setf (real-file-system/buffer-position sfs) 0)
      (setf (real-file-system/buffer-limit sfs) (+ head-length bytes-available)))))

(defmethod file-system/note-progress ((fs lisp-file-system) status-text percentage-done)
  (when status-text
    (format t "~%File system progress: ~a" status-text))
  (when percentage-done
    (format t "~%File system progress: ~d% complete." percentage-done))
  t)

(defmethod file-system/open ((fs lisp-file-system) (path file-descriptor)
                             &key (direction :input)
                                  #-(and :allegro-version>= (:version>= 6 0)) (element-type 'character)
                                  #+(and :allegro-version>= (:version>= 6 0)) element-type
                                  record-separator

                                  (if-exists :error)
                                  (if-does-not-exist (file-system-if-does-not-exist-default
                                                      if-exists direction)))
  #-(and :allegro-version>= (:version>= 6 0))
  (file-system/open (file-descriptor/file-system path) (file-descriptor/path path)
                    :direction direction
                    :element-type element-type
                    :record-separator record-separator
                    :if-exists if-exists
                    :if-does-not-exist if-does-not-exist)
  #+(and :allegro-version>= (:version>= 6 0))
  (if element-type
      (file-system/open (file-descriptor/file-system path) (file-descriptor/path path)
                        :direction direction
                        :element-type element-type
                        :record-separator record-separator
                        :if-exists if-exists
                        :if-does-not-exist if-does-not-exist)
    (file-system/open (file-descriptor/file-system path) (file-descriptor/path path)
                      :direction direction
                      :record-separator record-separator
                      :if-exists if-exists
                      :if-does-not-exist if-does-not-exist)))

(defmethod file-system/open ((fs lisp-file-system) (path pathname)
                             &key (direction :input) (element-type 'character)
                                  record-separator
                                  (if-exists :error)
                                  (if-does-not-exist (file-system-if-does-not-exist-default
                                                      if-exists direction)))
  (declare (ignore element-type record-separator))
  (handler-case (open path :direction direction
                      #-(and :allegro-version>= (:version>= 6 0)) :element-type
                      #-(and :allegro-version>= (:version>= 6 0)) '(unsigned-byte 8)
                      :if-exists if-exists
                      :if-does-not-exist if-does-not-exist)
    (file-error (condition)
      (file-system-condition-signal :operation-failure path condition))))

(defmethod file-system/platform ((fs lisp-file-system))
  (server-platform))

(defmethod file-system/probe-directory ((fs lisp-file-system) (path pathname))
  (unless (probe-directory path)
    (if (probe-file path)
        (file-system-condition-signal :non-a-directory path) ;path exists, but it isn't a directory
        (file-system-condition-signal :non-existent-path path))) ;path doesn't even exist
  (mapcar (lambda (pathname)
              (file-system/probe-file fs pathname :known-p t))
          (directory (make-pathname :name :wild :type :wild :defaults path))))

(defmethod file-system/probe-directory ((fs lisp-file-system) (path base-file-descriptor))
  (file-system/probe-directory (file-descriptor/file-system path) (file-descriptor/path path)))

(defmethod file-system/probe-file ((fs lisp-file-system) (path string) &key known-p)
  (file-system/probe-file fs (platform/parse-namestring (file-system/platform fs) path) :known-p known-p))

(defmethod file-system/probe-file ((fs lisp-file-system) (path pathname) &key known-p)
  (let ((proper-pathname (if known-p
                             path
                             (probe-file path))))
    (when proper-pathname
      (let ((timestamp (os-get-timestamp proper-pathname)))
;                        (pathname->platform-namestring
;                         proper-pathname
;                         (file-system/platform fs)))))

        (if (probe-directory proper-pathname)
            (make-instance 'directory-descriptor
                           :file-system fs
                           ;; Can't open directories on DOS, so size is nil for now.
                           :path proper-pathname
                           :modification-date timestamp)
            (make-instance 'file-descriptor
                           :file-system fs
                           :path proper-pathname
                           :modification-date timestamp
                           :size (file-bytes proper-pathname)
                           :mode-list nil
                           ;; *FINISH*: specify :MODE-LIST, could require ACLWIN and use
                           ;; FILE-READ-ONLY-P?
                           ))))))

(defmethod file-system/read-bytes ((fs lisp-file-system) (stream-handle stream) sequence)
  (declare #.(performance-optimizations)
           ;; #+allegro (:explain :types :calls)
           )
  (debug-message 5 "Lisp file system read bytes")
  (if sequence
      (check-type sequence simple-vector-8b)
    (setq sequence (simple-vector-8b-allocate +real-file-system/standard-buffer-size+)))
  (let ((n-bytes (read-sequence (the simple-vector-8b sequence) stream-handle)))
    (values (and (plusp n-bytes) sequence)
            n-bytes)))

(defmethod real-file-system/read-bytes-into-some-buffer ((fs lisp-file-system) stream n-bytes buffer)
  "Utility routine to read N-BYTES into BUFFER, which must be at least N-BYTES in size,
   and print out the usual traces.  Buffer must be of the same element type as the stream.
   (only an issue because we currently support text socket streams in binary mode).

   STREAM must be a real stream, not a STREAM-HANDLE.

   Returns NIL.

   It is an error if we don't receive those N-BYTES from the input stream."
  (declare #.(performance-optimizations)
           ;; #+allegro (:explain :types :calls)
           )
  ;; NOTE: this routine will require a stream or stream handle when we support multiple concurrent open
  ;; logical streams.
  (check-type stream stream)
  (check-type buffer simple-vector-8b)
  (check-type n-bytes array-index)
  (unless (zerop n-bytes)
    (let ((actual-bytes (read-sequence (the simple-vector-8b buffer) stream :end n-bytes)))
      ;(declare (type array-index actual-bytes))
      (unless (= actual-bytes (the array-index n-bytes))
        (file-system-condition-signal :CHUNK-READ-BYTE-SHORTAGE
                                      (real-file-system/open-file-descriptor-or-path fs)
                                      actual-bytes))))
  nil)

(defmethod file-system/read-line ((fs real-file-system) stream-handle)
  "**** YOU!  PAY ATTENTION **** If you're implementating another FSA, fix the read-line protocol
   to the one which is simplest for the FSA while preserving reasonable performance metrics.
   You'll need to fix the java FSA to match the new protocol."
  (case (real-file-system/open-file-record-separator fs)
    ((:CR)   (real-file-system/read-line-cr fs stream-handle))
    ((:LF)   (real-file-system/read-line-lf fs stream-handle))
    ((:CRLF) (real-file-system/read-line-crlf fs stream-handle))))

(defmethod file-system/read-only? ((fs lisp-file-system) (path file-descriptor))
  (file-system/read-only? (file-descriptor/file-system path) (file-descriptor/path path)))

(defmethod file-system/read-only? ((fs lisp-file-system) (path pathname))
  (os-file-read-only? path))

(defmethod file-system/record-separator ((fs lisp-file-system))
  (default-record-separator))

(defmethod file-system/rename ((fs lisp-file-system) (path pathname) (new-path pathname))
  (rename-file path new-path))

(defmethod file-system/set-read-only ((fs lisp-file-system) (path file-descriptor) read-only?)
  (file-system/set-read-only (file-descriptor/file-system path) (file-descriptor/path path) read-only?))

(defmethod file-system/set-read-only ((fs lisp-file-system) (path pathname) read-only?)
  (os-set-file-read-only (pathname->platform-namestring path (file-system/platform fs)) read-only?)
  t)

(defmethod file-system/touch-file ((fs lisp-file-system) (path file-descriptor)
                                   timestamp
                                   &key (if-does-not-exist :IGNORE)
                                        set-read-only
                                        set-executable)
  (file-system/touch-file (file-descriptor/file-system path)
                          (file-descriptor/path path) timestamp
                          :if-does-not-exist if-does-not-exist
                          :set-read-only set-read-only
                          :set-executable set-executable))

(defmethod file-system/touch-file ((fs lisp-file-system) (path pathname)
                                   timestamp
                                   &key (if-does-not-exist :IGNORE)
                                        set-read-only
                                        set-executable)
  ;; TO-DO: check to ensure that operand names a file, not a directory, if it already exists.
  (when (os-touch-file path :if-does-not-exist if-does-not-exist)
    (os-set-timestamp ;(pathname->platform-namestring path (file-system/platform fs))
     path
     timestamp)
    (when set-read-only
      (os-set-file-read-only  ;(pathname->platform-namestring path (file-system/platform fs))
       path t))
    (when set-executable
      (os-set-file-executable ;(pathname->platform-namestring path (file-system/platform fs))
       path t))
    t))

(defmethod file-system/write-bytes ((fs lisp-file-system) (stream-handle stream) (sequence sequence)
                                    &key end)
  (declare #.(performance-optimizations)
           ;; #+allegro (:explain :types :calls)
           )
  (write-sequence (the simple-vector-8b sequence) stream-handle :end end)
  nil)

(defmethod file-system/write-line ((fs lisp-file-system) (stream-handle stream) (sequence string)
                                   &key (end (simple-string-length sequence)) suppress-separator)
  (declare #.(performance-optimizations)
           ;(type array-index end)
           ;(type lw:simple-text-string sequence)
           ;; #+allegro (:explain :types :calls)
           )
  (do ((index 0 (1+ index)))
      ((= index end))
    ;(declare (type array-index index))
    (write-byte (logand #xFF (char-code (schar sequence index))) stream-handle))
  (unless suppress-separator
    (ecase (real-file-system/open-file-record-separator fs)
      ((:CR) (write-byte *ascii-carriage-return* stream-handle))
      ((:LF) (write-byte *ascii-linefeed* stream-handle))
      ((:crlf) (write-byte *ascii-carriage-return* stream-handle)
               (write-byte *ascii-linefeed* stream-handle))))
  nil)

(defmethod file-system/shutdown ((fs lisp-file-system) &key (success t))
  (declare (ignore success))
  nil)                                  ;NOP


;;;
;;; DELEGATE-FILE-SYSTEM
;;;
;;; A delegate file system simply delegates all file system operations to an underlying
;;; real file system.  It is the base class for LOGICAL-FILE-SYSTEM and FILTERED-FILE-SYSTEM.

(defclass delegate-file-system (file-system)
  ((underlying-file-system :type file-system
                           :initarg :underlying-file-system
                           :reader delegate-file-system/underlying-file-system))
  (:documentation "A FILE-SYSTEM that delegates operations to another file system."))

(defmethod file-system/close ((fs delegate-file-system) stream-handle)
  (file-system/close
   (delegate-file-system/underlying-file-system fs)
   stream-handle))

(defmethod file-system/copy-file ((fs delegate-file-system) old new)
  (file-system/copy-file (delegate-file-system/underlying-file-system fs) old new))

(defmethod file-system/crc-file ((fs delegate-file-system) path)
  (file-system/crc-file (delegate-file-system/underlying-file-system fs) path))

(defmethod file-system/crc-file-background ((fs delegate-file-system)
                                            path file-type foreground-procedure)
  (file-system/crc-file-background
   (delegate-file-system/underlying-file-system fs)
   path file-type foreground-procedure))

(defmethod file-system/create-directory ((fs delegate-file-system) path)
  (file-system/create-directory (delegate-file-system/underlying-file-system fs) path))

(defmethod file-system/delete-directory ((fs delegate-file-system) path &key recursive force)
  (file-system/delete-directory (delegate-file-system/underlying-file-system fs)
                                path :recursive recursive :force force))

(defmethod file-system/delete-file ((fs delegate-file-system) path &key force)
  (file-system/delete-file (delegate-file-system/underlying-file-system fs) path :force force))

(defmethod file-system/detect-file-content-type ((fs delegate-file-system) path)
  (file-system/detect-file-content-type (delegate-file-system/underlying-file-system fs) path))

(defmethod file-system/detect-file-record-separator ((fs delegate-file-system) path)
  (file-system/detect-file-record-separator (delegate-file-system/underlying-file-system fs) path))

(defmethod file-system/note-progress ((fs delegate-file-system) status-text percentage-done)
  (file-system/note-progress (delegate-file-system/underlying-file-system fs) status-text percentage-done))

(defmethod file-system/platform ((fs delegate-file-system))
  (file-system/platform (delegate-file-system/underlying-file-system fs)))

(defmethod file-system/probe-file ((fs delegate-file-system) path &key known-p)
  (let ((descriptor (file-system/probe-file (delegate-file-system/underlying-file-system fs) path :known-p known-p)))
    (when descriptor
      (if (file-descriptor/is-directory? descriptor)
          (make-instance 'directory-descriptor
           :file-system fs
           :path (file-descriptor/path descriptor)
           :modification-date (file-descriptor/modification-date descriptor))
        (make-instance 'file-descriptor
                       :file-system fs
                       :path (file-descriptor/path descriptor)
                       :modification-date (file-descriptor/modification-date descriptor)
                       :size (file-descriptor/size descriptor)
                       :mode-list (file-descriptor/mode-list descriptor))))))

(defmethod file-system/read-bytes ((fs delegate-file-system) stream-handle sequence)
  (debug-message 5 "Delegate file system read-bytes")
  (file-system/read-bytes
   (delegate-file-system/underlying-file-system fs)
   stream-handle
   sequence))

(defmethod file-system/read-line ((fs delegate-file-system) stream-handle)
  (file-system/read-line (delegate-file-system/underlying-file-system fs) stream-handle))

(defmethod file-system/read-only? ((fs delegate-file-system) path)
  (file-system/read-only?
   (delegate-file-system/underlying-file-system fs)
   path))

(defmethod file-system/record-separator ((fs delegate-file-system))
  (file-system/record-separator (delegate-file-system/underlying-file-system fs)))

(defmethod file-system-rename ((fs delegate-file-system) old new)
  (file-system/rename (delegate-file-system/underlying-file-system fs) old new))

(defmethod file-system/set-read-only ((fs delegate-file-system) path read-only?)
  (file-system/set-read-only
   (delegate-file-system/underlying-file-system fs)
   path
   read-only?))

(defmethod file-system/stream-eof? ((fs delegate-file-system))
  (file-system/stream-eof?
   (delegate-file-system/underlying-file-system fs)))

(defmethod file-system/shutdown ((fs delegate-file-system) &key (success t))
  (file-system/shutdown
   (delegate-file-system/underlying-file-system fs)
   :success success))

(defmethod file-system/touch-file ((fs delegate-file-system) path
                                   timestamp
                                   &key (if-does-not-exist :ignore)
                                        set-read-only
                                        set-executable)
  (file-system/touch-file
   (delegate-file-system/underlying-file-system fs) path timestamp
   :if-does-not-exist if-does-not-exist
   :set-read-only set-read-only
   :set-executable set-executable))

(defmethod file-system/write-bytes ((fs delegate-file-system) stream-handle sequence
                                    &key (end (length sequence)))
  (file-system/write-bytes
   (delegate-file-system/underlying-file-system fs)
   stream-handle
   sequence
   :end end))


;;;
;;; FSA-FILE-SYSTEM
;;;
;;; A file system that communicates via the file-system-agent applet.

(defconstant *fsa-protocol-version* 1
  "The version of the protocol that the client should speak.

   NOTE:  INCREMENT THIS EVERY TIME YOU MODIFY THE PROTOCOL!
   NOTE:  ALSO INCREMENT THE CORRESPONDING VALUE IN FileSystemAgentProtocol.java")

;; Indexes for manipulating operation time statistics in *fsa-operation-times*
(define-enumeration (:start 0 :set-symbol *fsa-optime-symbols*)
  *fsa-optime-index/helo*
  *fsa-optime-index/plat*
  *fsa-optime-index/pfil*
  *fsa-optime-index/pdir*
  *fsa-optime-index/curr*
  *fsa-optime-index/gpar*
  *fsa-optime-index/cdir*
  *fsa-optime-index/dfil*
  *fsa-optime-index/ddir*
  *fsa-optime-index/renm*
  *fsa-optime-index/cfil*
  *fsa-optime-index/tuch*
  *fsa-optime-index/chmd*
  *fsa-optime-index/prog*
  *fsa-optime-index/open*
  *fsa-optime-index/clos*
  *fsa-optime-index/getl*
  *fsa-optime-index/putl*
  *fsa-optime-index/dtrt*
  *fsa-optime-index/dtct*
  *fsa-optime-index/dtct-extended*
  *fsa-optime-index/getb*
  *fsa-optime-index/getb-extended*
  *fsa-optime-index/putb*
  *fsa-optime-index/putb-extended*
  *fsa-optime-index/crcb*
  *fsa-optime-index/crct*
  *fsa-optime-index/what*
  ;; **WARNING** if you add elements, re-evaluate the form defining *FSA-OPERATION-TIMES*
  ;; **WARNING** *FSA-OPERATION-TIMES* has a size based on and assuming that the following element
  ;; is the last element in the enumeration.
  *fsa-optime-index/quit*
  )

(defstruct (fsa-operation-stats
            (:conc-name fsa-operation-stats/)
            (:constructor make-fsa-operation-stats))
  (hits 0 :type fixnum)
  (cumulative-time 0 :type integer)
  (max-time 0 :type integer)
  (min-time 0 :type integer))

(defun fsa-operation-stats/reset (fsa-op-stats)
  "Clear info in an FSA-OPERATION-STATS structure.  Return the input structure."
  (setf (fsa-operation-stats/hits fsa-op-stats) 0)
  (setf (fsa-operation-stats/cumulative-time fsa-op-stats) 0)
  (setf (fsa-operation-stats/max-time fsa-op-stats) 0)
  (setf (fsa-operation-stats/min-time fsa-op-stats) 0))

(defun fsa-operation/record-hit (fsa-operation-stats-object elapsed-run-time)
  "Record a hit on an operation, and the elapsed time for the operation."
  (incf (fsa-operation-stats/hits fsa-operation-stats-object))
  (incf (fsa-operation-stats/cumulative-time fsa-operation-stats-object) elapsed-run-time)
  (when (> elapsed-run-time (fsa-operation-stats/max-time fsa-operation-stats-object))
    (setf (fsa-operation-stats/max-time fsa-operation-stats-object) elapsed-run-time))
  (when (or (zerop (fsa-operation-stats/min-time fsa-operation-stats-object))
            (< elapsed-run-time (fsa-operation-stats/min-time fsa-operation-stats-object)))
    (setf (fsa-operation-stats/min-time fsa-operation-stats-object) elapsed-run-time))
  fsa-operation-stats-object)

(defparameter *fsa-operation-times*
    (make-array (1+ *fsa-optime-index/quit*))
  "Array for tracking socket protocol operation times.  Each element is an fsa-operation-time structure.")
(declaim (type (simple-array t (*)) *fsa-operation-times*))

(defun fsa-operation-times/initialize-if-necessary ()
  "Initialize the operation times statistics objects if they haven't been, since they must
   always be initialized at least once for correct FSA operation, since right now timing information
   is always maintained"
  (when (< (length (the (simple-array t (*)) *fsa-operation-times*))
           (1+ *fsa-optime-index/quit*))
    (setq *fsa-operation-times* (make-array (1+ *fsa-optime-index/quit*))))
  (when (null (elt *fsa-operation-times* 0))
    (fsa-operation-times/reset)))

(defun fsa-operation-times/reset ()
  "Initialize the operation times tracking array"
  (loop for x from 0 below (length *fsa-operation-times*)
      as stats-obj = (svref *fsa-operation-times* x)
      do (if stats-obj
             (fsa-operation-stats/reset stats-obj)
           (setf (svref *fsa-operation-times* x) (make-fsa-operation-stats))))
  nil)

(defmacro with-fsa-time-track ((index) &rest body)
  "Execute BODY such that we track time for the operation indicated by INDEX, which must be
   on of the *FSA-* enumerated constants defined with the above DEFINE-ENUMERATION"
  (with-unique-names (before)
    `(let ((,before (get-internal-real-time)))
       (multiple-value-prog1 (locally ,@body)
         (fsa-operation/record-hit
          (svref *fsa-operation-times* ,index)
          (- (get-internal-real-time) ,before))))))

(defclass fsa-file-system (real-file-system)
  ((stream :initarg :stream
           :initform (error "Required initarg :stream omitted.")
           :reader fsa/stream)
   (response-buffer :initform (make-array 80 :element-type 'character :fill-pointer 0 :adjustable t)
                    :reader fsa/response-buffer)
   (locale :initform nil
           :accessor fsa/locale)
   (cached-platform :initform nil
                    :accessor fsa/cached-platform)
   (cached-directory-separators :initform nil
                                :accessor fsa/directory-separators)
   (cached-record-separator :initform nil
                            :accessor fsa/record-separator)
   (cached-clock-skew :initform nil
                      :accessor fsa/clock-skew)
   (open-direction :initform nil
                   :accessor fsa/open-direction)
   (ignoring-progress :initform nil
                      :accessor fsa/ignoring-progress))
  (:documentation "A FILE-SYSTEM subtype which performs via FSA applet."))

(defmethod print-object ((object fsa-file-system) stream)
  (print-unreadable-object (object stream :type t)))

(defvar *fsa-trace-io* *debug-io*
  "Set this variable to a stream if you want diagnostic information on SOCKET-FILE-SYSTEM activities")

(defun socket-trace-function (format-string arglist)
  (apply #'format (or *fsa-trace-io*
                      *debug-io*) format-string arglist)
  (force-output (or *fsa-trace-io* *debug-io*)))

(defmacro socket-trace (format-string &rest args)
  `(WHEN (OR *FSA-TRACE-IO*
             (DEBUG-LEVEL-MEETS-OR-EXCEEDS? 4))
     (SOCKET-TRACE-FUNCTION ,format-string (list ,@args))))

(defgeneric fsa/send-request (fsa request-string-or-bytes
                                             &key expected-response
                                                  time-track
                                                  background-request)
  (:documentation
   "Send REQUEST-STRING-OR-BYTES over the socket as a request to the file system agent.
    FSA is the SOCKET-FILE-SYSTEM instance which contains the socket.  The request argument
    may be in string or unsigned-byte form.

    PROTOCOL:
    All requests are 4 character commands, optionally followed by a space and command arguments,
    and terminated by a newline.  This method will insert the newline, the command and arguments
    are expected to be present in the request argument.

    EXPECTED-RESPONSE is as specified in SOCKET-FILE-SYSTEM-RECEIVE-RESPONSE.

    TIME-TRACK, if specified, is one of the enumerated *FSA-* indexes, and will be used
    to track the time required to emit the request and receive a response.

    Background-request, if specified, is a procedure of one argument.  It will be invoked
    immediately the request is sent, so that lisp can continue processing while the FSA is
    working.  The argument it is invoked upon is a procedure of no arguments that will
    wait for the return result.  This is used for CRC computation which is a bit of a bear.

    Return the response-code as per FSA-SYSTEM-REPONSE.")

  (:method :before (fsa request &key expected-response &allow-other-keys)
    (declare (ignore fsa))
    (socket-trace "~%SEND: ~a~@[, expected response: ~s~]"
                  (if (stringp request)
                      request
                    (simple-bytes-to-simple-string-8b request))
                  expected-response))

  (:method :around (fsa request &key time-track &allow-other-keys)
    (declare (ignore fsa))
    (declare (ignore request))
    (if time-track
        (with-fsa-time-track (time-track)
          (call-next-method))
      (call-next-method)))

  (:method (fsa (request string) &key expected-response background-request &allow-other-keys
            &aux (stream (fsa/stream fsa)))
    ;; STRING driven input
    (write-sequence (csf/utility::simple-string-to-bytes request) stream)
    (write-byte *ascii-linefeed* stream)        ;terminate command
    (force-output stream)

    (if background-request
        (funcall background-request
                 (lambda ()
                     (fsa/receive-response fsa request expected-response)))
      ;; Get the reply
      (fsa/receive-response fsa request expected-response)))

  (:method (fsa (request array) &key expected-response background-request &allow-other-keys
            &aux (stream (fsa/stream fsa)))
    ;; BYTE driven input
    (write-sequence request stream)
    (write-byte *ascii-linefeed* stream)        ;terminate command
    (force-output stream)

    (if background-request
        (funcall background-request
                 (lambda ()
                     (fsa/receive-response fsa (csf/utility::bytes-to-string request) expected-response)))
      ;; Get the reply
      (fsa/receive-response fsa (csf/utility::bytes-to-string request) expected-response))))

(defmethod real-file-system/read-bytes-into-some-buffer ((fsa fsa-file-system) stream n-bytes buffer)
  "Utility routine to read N-BYTES into BUFFER, which must be at least N-BYTES in size,
   and print out the usual traces.  Buffer must be of the same element type as the stream.
   (only an issue because we currently support text socket streams in binary mode).

   STREAM must be a real stream, not a STREAM-HANDLE.

   Returns NIL.

   It is an error if we don't receive those N-BYTES from the input stream."
  (declare #.(performance-optimizations)
           ;; #+allegro (:explain :calls :types)
           )
  ;; NOTE: this routine will require a stream or stream handle when we support multiple concurrent open
  ;; logical streams.
  (check-type stream stream)
  (cond ((> n-bytes 0)
         (socket-trace "~%RECB: <~d bytes expected, " n-bytes) ;receive BYTE chunk
         (fsa/read-sequence buffer stream 0 n-bytes)
;; The above call hangs until the bytes show up.
;        (let ((actual-bytes (read-sequence buffer stream :end n-bytes)))
;          (socket-trace "~d bytes received>" actual-bytes)
;          (unless (= actual-bytes n-bytes)
;            (file-system-condition-signal :CHUNK-READ-BYTE-SHORTAGE
;                                          (real-file-system-open-file-descriptor-or-path sfs)
;                                          actual-bytes))))
         )
        ((= n-bytes 0)
         (socket-trace "~%RECB: <0 bytes expected, no read was attempted>"))
        (t (error "Read negative bytes?")))
  nil)

(defun fsa/read-sequence (buffer stream start-index bytes-available)
  "Uses read-sequence to deposit BYTES-AVAILABLE bytes from STREAM into BUFFER at START-INDEX.
   Makes multiple calls to read sequence until all bytes are read, in case the read-sequence
   returns without the right amount read.  This call will `hang' until the bytes show up."
  (named-let loup ((start start-index)
                   (end   (+ start-index bytes-available))
                   (count 0))

                  (debug-message 5 "Start: ~d, End: ~d " start end)
                  (let ((this-transfer (- (read-sequence buffer
                                                         stream
                                                         :start start
                                                         :end end)
                                          start)))
                    (if (< (+ this-transfer count) bytes-available)
                        (loup (+ start this-transfer) end (+ count this-transfer))
                      (socket-trace "~d bytes received>" (+ count this-transfer))))))

(defun fsa/read-bytes-into-internal-buffer (fsa stream n-bytes)
  "Utility routine to manage the FSA internal byte buffer of a fsa for various read operations on the socket.
   We expect N-BYTES Of input.  It is an error if we don't receive those N-BYTES.

   Read the bytes, update the FSA byte-buffer if necessary to accomodate N-BYTES.
   Return NIL, or signal an error if we don't obtain the desired bytes.

   Note: requires the FSA STREAM, and not a STREAM-HANDLE

   NOTE: this routine always assumes it can wipe the buffer, and that no previous contents exist."
  ;; NOTE: this routine will require a stream or stream handle when we support multiple concurrent open
  ;; logical streams.
  (let ((buffer (real-file-system/ensure-byte-buffer fsa n-bytes)))
    (real-file-system/read-bytes-into-some-buffer fsa stream n-bytes buffer))
  nil)

(defun fsa/receive-response (fsa command
                                            &optional expected-response
                                            &aux (stream (fsa/stream fsa)))
  "A request has been sent to the file system agent with SOCKET-FILE-SYSTEM-SEND-REQUEST.
   Listen to the socket and look for a response.

   COMMAND is passed purely for exception reporting purposes, and should be the command
   sent as the server request in SOCKET-FILE-SYSTEM-SEND-REQUEST.
   *** It would be nice if this argument provided a structure of information including
   the file system operation and pathname of the file being operated on.  When we ultimately
   report such problems to the user, we should be providing the pathname unobscured by the
   surrounding FSA command noise.  Right now there are too many places that call us with a
   useless COMMAND argument and it would be too much effort to parse a pathname out of it,
   assuming there were one there in all cases.

   PROTOCOL:
   All responses consist of 3-digit textual numbers, optionally followed by text, finally terminated
   by a newline character.

   If EXPECTED-RESPONSE is specified, and isn't the response received, we signal a FILE-SYSTEM-CONDITION.
   If EXPECTED-RESPONSE is nil, we don't do this check.

   In the event we don't abnoramlly exit, we return the following values:

   1) The rest of response if there was one, or nil.

   2) The numeric reply code, as a number (i.e. converted from string).

   We return the code second since checking is usually done by supplying EXPECTED-RESPONSE,
   and if you're looking for text, you can then use SETQ/SETF instead of MULTIPLE-VALUE-BIND."

  (socket-trace "~%RECV: ")
  ;; Reply should have 3-char response number, and terminating newline, or space if args present
  (let* ((number 0)
         (separator-char 0)
         (response-buffer (fsa/response-buffer fsa)))
    ;; Profiling showed that we spent way too much time in an unoptimized read-sequence flavor
    ;; in this call, so we've inlined the reads.  Of course it could just have been waiting
    ;; for a response from Java...  We also eliminate consing a couple of 4 byte buffers here
    ;; Further performance improvement: make response vector a simple vector
    ;(declare (type (integer 0 (1000)) number)
    ;         (type (integer 0  (256)) separator-char))
    (incf number (* (- (read-byte stream nil 0) *ascii-zero*) 100))
    (incf number (* (- (read-byte stream nil 0) *ascii-zero*)  10))
    (incf number (* (- (read-byte stream nil 0) *ascii-zero*)   1))
    (setq separator-char (read-byte stream nil))

    (socket-trace "~d" number)

    (flet ((response-error (&optional (reason :invalid-agent-command-response)
                                      (optional-response-text nil ort-p))
             ;; It would be real useful to see the text which is sent back with the numeric response,
             ;; but I haven't shown it here yet...  Didn't want to get errors reading it while
             ;; essentially throwing errors about the numeric code...
             (apply #'file-system-condition-signal
                    reason
                    (format nil "Command: ~s, Reply expected: ~s, received: '~d ~a'"
                            command expected-response number
                            (if optional-response-text
                                optional-response-text
                              ""))
                    ;; Store everything we know in the error condition
                    ;; so that it's available for some handler to
                    ;; report it to the user in a reasonable way.
                    :command command
                    :expected-response expected-response
                    :received-response number
                    (when ort-p
                      (list :response-text optional-response-text)))))
      (unless separator-char
        (response-error))               ;insufficient chars in response, abort computation

      ;; If there was extended text after number, grab it.
      (setf (fill-pointer response-buffer) 0) ;reset the buffer (affects routine return value too)
      (unless (= separator-char *ascii-linefeed*)
        (loop as char-or-byte = (read-byte stream nil :eof)
            until (eql char-or-byte *ascii-linefeed*)
            do
              (when (eq char-or-byte :eof)
                (response-error :unexpected-eof))
              (vector-push-extend (code-char char-or-byte)
                                  response-buffer 80)))

      ;; Now that we've grabbed the rest of the text, check the result code (if requested)
      (when expected-response
        (unless (= number expected-response)
          (response-error :unexpected-response-code response-buffer)))
      ;; No TERPRI necesssary, buffer has newline and SEND trace starts with newline
      (let ((response-text-p (> (length response-buffer) 0)))
        (when response-text-p
          (socket-trace " ~a" response-buffer))
        ;; We really want the caller to use, stash, and otherwise manipulate the
        ;; return vector (if present), and it isn't good to require them to copy it.
        ;; Here we copy it, this will return a simple vector of chars compatible with SCHAR, SVREF, etc
        (values (and response-text-p (copy-seq response-buffer)) number)))))

(defun fsa/receive-multi-line-response (fsa function expected-terminating-response)
  "The caller has just initiated a request which will transmit a multi-line response, and which
   received a reply code of 125 (transfer beginning).

   This function will read every textual line of the multi-line response until it receives
   the sequence terminator, which is an empty line.  It will then read one more line which
   must be be a final terminating response code, and which should match EXPECTED-TERMINATING-RESPONSE.

   For every line which is read, FUNCTION will be called with one argument, a STRING which
   represents the line read.  FUNCTION is not called for the empty terminating line.

   PROTOCOL: multi-line responses are sequences of text records of known maximum size,
   in which the number of records to be received is unknown.  End of sequence is signalled by the FSA
   by sending a zero length record.

   Return value: NIL."

  (let* ((stream (fsa/stream fsa))
         (line (make-array 256 :element-type 'character :fill-pointer 0 :adjustable t)))
    ;; Let's not cut and paste too much.  I did it this way to move the stream-type check out of
    ;; all oops.  But it's ugly when duplicated with all the diagnostic and other code outside the innermost
    ;; char/byte reading loop.
    (flet ((getline-binary ()
             (socket-trace "~%RECA: ")
             (setf (fill-pointer line) 0)
             (loop as byte = (read-byte stream)
                 until (= byte *ascii-linefeed*)
                 do (vector-push-extend (code-char byte) line 1024))
             (socket-trace "~a" line)   ; no terpri necessary, buffer has newline and SEND starts with newline
             (length line)))            ;return number of chars read
      (loop as nChars = (getline-binary)
          while (> nChars 0)
          do (funcall function line))
      ;; Ok, all response lines read, read the terminating response
      (fsa/receive-response
       fsa "receiving multi-line response" expected-terminating-response)))
  nil)

(defmethod initialize-instance :after ((instance fsa-file-system) &rest ignore &key stream)
  "Create an FSA-FILE-SYSTEM instance.
   SOCKET-STREAM is required to be a bidirectional stream of element-type '(UNSIGNED-BYTE 8)
   which will be used for both command channel and data channel purposes.

   At the time the file system is instantiated (i.e. this function is called),
   there should be a process on the other end of the socket which is in read-wait on the socket
   and ready to accept commands from this server (which is instantiating this file system).

   PROTOCOL: 'HELO' => '200'
   Anything else is an error."
  (debug-message 5 "Creating an FSA, stream: ~s ~s" (fsa/stream instance) stream)
  ;; This won't be true, but it will work because we implement the stream, too.
  ;; (assert (subtypep (stream-element-type (fsa/stream instance)) 'unsigned-byte))
  (fsa-operation-times/initialize-if-necessary)
  (let* ((reply (fsa/send-request instance "HELO" :expected-response 200 :time-track *fsa-optime-index/helo*))
         (client-version  (and reply (with-input-from-string (stream reply) (safe-read stream nil nil)))))
    (cond ((or (null client-version)
               (< client-version *fsa-protocol-version*))
           (error "The file-system-agent is using an older version of the protocol (version ~s).~%~
                   The server is using version ~d.~%~
                   You must update the class files." client-version *fsa-protocol-version*))
          ((> client-version *fsa-protocol-version*)
           (error "The file-system-agent is using a newer version of the protocol (version ~s).~%~
                   The server is using version ~d.~%~
                   Perhaps you are running the wrong server?" client-version *fsa-protocol-version*))
          (t nil)) ;; copacetic

    ;; Get this before we need it.
    (file-system/platform instance)

    ;; Try out the WHAT command.
    (let ((reply (fsa/send-request
                  instance "WHAT foo"
                  :expected-response 200
                  :time-track *fsa-optime-index/what*)))
      (with-input-from-string (stream reply)
        (let ((answer (safe-read stream)))
          nil)))))

(defmethod file-system/close ((fsa fsa-file-system) (stream-handle integer))
  "PROTOCOL 'CLOS <handle>' => '200'"
  ;; Flush buffers for write-line if we were opened for output (should be nop for binary writes...)
  (when (eq (fsa/open-direction fsa) :output)
    (fsa/write-line-flush-buffer fsa stream-handle))

  ;; Reset the open direction, just to be a useful debuggin indicator
  (setf (fsa/open-direction fsa) nil)

  ;; Reset these, necessary for READ-LINE to operate correctly on non-first file reads in FSA
  (setf (real-file-system/buffer-position fsa) 0)
  (setf (real-file-system/buffer-limit fsa) 0)
  (setf (file-system/stream-eof? fsa) nil)

  ;; Send the CLOS call to the FSA
  (fsa/send-request fsa (concatenate 'string "CLOS " (princ-to-string stream-handle))
                    :expected-response 200 :time-track *fsa-optime-index/clos*)
  )

(defmethod file-system/crc-binary-file ((fsa fsa-file-system) (path pathname))
  (with-input-from-string
   (stream
    (fsa/send-request fsa
                      (format nil "CRCB ~s"
                              (pathname->platform-namestring
                               path
                               (file-system/platform fsa)))
                      :expected-response 200
                      :time-track *fsa-optime-index/crcb*))
   (safe-read stream)))

(defmethod file-system/crc-binary-file-background ((fsa fsa-file-system) (path pathname) foreground-procedure)
  (fsa/send-request
   fsa
   (format nil "CRCB ~s"
           (pathname->platform-namestring
            path
            (file-system/platform fsa)))
   :expected-response 200
   :time-track *fsa-optime-index/crcb*
   :background-request
   (lambda (get-response)
       (funcall foreground-procedure
                (lambda ()
                    (with-input-from-string
                        (stream (funcall get-response))
                      (safe-read stream)))))))

(defmethod file-system/crc-text-file ((fsa fsa-file-system) (path pathname))
  (with-input-from-string
      (stream
       (fsa/send-request fsa (format nil "CRCT ~s"
                                     (pathname->platform-namestring
                                      path
                                      (file-system/platform fsa)))
                         :expected-response 200
                         :time-track *fsa-optime-index/crct*))
    (safe-read stream)))

(defmethod file-system/crc-text-file-background ((fsa fsa-file-system) (path pathname) foreground-procedure)
  (fsa/send-request
   fsa
   (format nil "CRCT ~s"
           (pathname->platform-namestring
            path
            (file-system/platform fsa)))
   :expected-response 200
   :time-track *fsa-optime-index/crct*
   :background-request
   (lambda (get-response)
       (funcall foreground-procedure
                (lambda ()
                    (with-input-from-string
                        (stream (funcall get-response))
                      (safe-read stream)))))))

(defmethod file-system/create-directory ((fsa fsa-file-system) (path pathname))
  "PROTOCOL: 'CDIR unquoted-path-spec' => '200'.
   Another return code indicates the directory was not created.  We convert this logic into the true/nil
   generic function API results."
  (multiple-value-bind (reply-text reply-code)
      ;; Note failure to specify expected reply, we want to check it ourselves.
      (fsa/send-request fsa (format nil "CDIR ~s"
                                    (pathname->platform-namestring path
                                                                   (file-system/platform fsa)))
                                       :time-track *fsa-optime-index/cdir*)
    (when (/= reply-code 200)
      (socket-trace "~%CDIR failed, code: ~s reason: ~s" reply-code reply-text))
    (= reply-code 200)))

(defmethod file-system/delete-directory ((fsa fsa-file-system) (path pathname)
					 &key recursive force)
  "PROTOCOL: 'DDIR path-spec T|F' => '200'.
   Where T or F is specified such that T means do a recursive delete, and means do NOT do a recursive
   delete."
  (declare (ignore force))
  ;; should check for force here!
  (fsa/send-request fsa
                    (format nil "DDIR ~s ~a"
                            (pathname->platform-namestring path
                                                           (file-system/platform fsa))
                            (if recursive "T" "F"))
                    :expected-response 200 :time-track *fsa-optime-index/ddir*)
  nil)

(defmethod file-system/delete-directory ((fsa fsa-file-system) (fd directory-descriptor)
					 &key recursive force)
  (file-system/delete-directory (file-descriptor/file-system fd) (file-descriptor/path fd)
				:recursive recursive :force force))

(defmethod file-system/delete-file ((fsa fsa-file-system) (path pathname) &key force)
  "PROTOCOL: 'DFIL unquoted-path-spec' => '200'.
   Another return code indicates the file was not deleted.  See GF (Generic Function) for method details."
  (declare (ignore force))
  ;; should check for force here!
  (fsa/send-request
   fsa (format nil "DFIL ~s"
               (pathname->platform-namestring path (file-system/platform fsa)))
   :expected-response 200 :time-track *fsa-optime-index/dfil*)
  nil)

(defmethod file-system/rename ((fsa fsa-file-system) (path pathname) (new-path pathname))
  "PROTOCOL: 'RENM old-path new-path' => '200'"
  (fsa/send-request fsa
                    (format nil "RENM ~s ~s"
                            (pathname->platform-namestring path
                                                           (file-system/platform fsa))
                            (pathname->platform-namestring new-path
                                                           (file-system/platform fsa))
                            )
                    :expected-response 200 :time-track *fsa-optime-index/renm*)
  nil)

(defmethod file-system/copy-file ((fsa fsa-file-system) (path pathname) (new-path pathname))
  "PROTOCOL: 'CFIL relative-path' => '200'"
  (fsa/send-request fsa
                    (format nil "CFIL ~s ~s"
                            (pathname->platform-namestring path
                                                           (file-system/platform fsa))
                            (pathname->platform-namestring new-path
                                                           (file-system/platform fsa)))
                    :expected-response 200 :time-track *fsa-optime-index/cfil*)
  nil)

(defmethod file-system/locale ((fsa fsa-file-system))
  (fsa/locale fsa))

(defmethod file-system/note-progress ((fsa fsa-file-system) status-text percentage-done)
  "PROTOCOL: 'PROG status-text percentage-done' => '200 result'
   Where result is T if progress is being noted by the file system, and NIL if it is not.
   In order to alleviate the caller from checking the value of this function and suppressing
   calls to socket-file-system agents to minimize wasted time in handshaking, this method
   will arrange for subsequent calls to be NOPs if the FSA returns NIL on any call tot his
   method."
  (unless (fsa/ignoring-progress fsa)
    (setf (fsa/ignoring-progress fsa)
      (not (read-from-string
            (fsa/send-request
             fsa (format nil "PROG ~s ~s" status-text percentage-done)
             :expected-response 200 :time-track *fsa-optime-index/prog*)))))
  (not (fsa/ignoring-progress fsa)))

(defmethod file-system/open ((fsa fsa-file-system) (path file-descriptor)
                             &key (direction :input) (element-type 'character) record-separator

                                  (if-exists :error)
                                  (if-does-not-exist (file-system-if-does-not-exist-default
                                                      if-exists direction)))
  (file-system/open (file-descriptor/file-system path) (file-descriptor/path path)
                    :direction direction
                    :element-type element-type
                    :record-terminator record-separator
                    :if-exists if-exists
                    :if-does-not-exist if-does-not-exist))

(defmethod file-system/open ((fsa fsa-file-system) (path pathname)
                             &key (direction :input) (element-type 'character) record-separator
                             (if-exists :error)
                             (if-does-not-exist (file-system-if-does-not-exist-default
                                                 if-exists direction)))
  "PROTOCOL: 'OPEN relative-path <direction> <element-type> <if-exists> <if-does-not-exist>' => '200 <handle>'
  Where each of the <x> args sent correspond to permissible keyword values above, minus any
  quotes or symbol prefixes.  The response, if ok, should return an integer handle which is used
  to identify the stream open on the FSA, and which is also used as the handle server-side in the
  FS.

  <element-type> is translated to TEXT or BINARY, rather than requiring FSA to parse '(UNSIGNED-BYTE 8)"
  ;; Note that we either get an integer, or NIL indicating that some situation arose for which
  ;; the keywords suppressed errors.
  (declare (ignore element-type record-separator))
  (setf (fsa/open-direction fsa) direction)
  (read-from-string
   (fsa/send-request
    fsa
    (format nil "OPEN ~s ~a ~a ~a ~a"
            (pathname->platform-namestring path (file-system/platform fsa))
            (write-to-string direction :case :upcase :readably nil :escape nil)
            ;; Element type
            "BINARY"
            (write-to-string if-exists :case :upcase :readably nil :escape nil)
            (write-to-string if-does-not-exist :case :upcase :readably nil :escape nil))
    :expected-response 200 :time-track *fsa-optime-index/open*)))

(defmethod file-system/platform ((fsa fsa-file-system))
  "PROTOCOL: 'PLAT' => '200 keyword'
   This method's behavior is assumed idempotent, and the results are potentially cached so that subsequent
   calls aren't socket-driven."
  (or (fsa/cached-platform fsa)
      (progn
        (when (real-file-system/open-file-descriptor-or-path fsa)
          (error "Attempt to open multiple files when via FILE-SYSTEM-OPEN.  Files must be opened ~
              serially and with intervening calls to FILE-SYSTEM-CLOSE.  Already open: ~s, ~
              attempting to open: "
                 (real-file-system/open-file-descriptor-or-path fsa)
                 ))

        (let ((reply (fsa/send-request
                      fsa "PLAT" :expected-response 200 :time-track *fsa-optime-index/plat*)))
          (with-input-from-string (stream reply)
                                  ;; We could trap this, but let's assume that we'll get a reasonable response
                                  (let* ((keyword (find-platform (safe-read stream)))
                                         (record-separator (safe-read stream))
                                         (clock-skew (- (get-universal-time) (safe-read stream)))
                                         (locale (parse-locale (safe-read stream)))
                                         ;; We *should* ask the client, but we can tell from the platform.
                                         (directory-separators (platform/directory-separators keyword)))
                                    (setf (fsa/cached-platform fsa) keyword)
                                    (setf (fsa/locale fsa) locale)
                                    (setf (fsa/directory-separators fsa) directory-separators)
                                    (setf (fsa/record-separator fsa) record-separator)
                                    (setf (fsa/clock-skew fsa) clock-skew)
                                    keyword))))))

(defmethod file-system/probe-directory ((fsa fsa-file-system) (path pathname))
  "PROTOCOL: 'PDIR unquoted-name' => 125, multi-line-response, 212"
  (let ((fd (file-system/probe-file fsa path)))
    (unless fd
      (file-system-condition-signal :non-existent-path path))
    (unless (file-descriptor/is-directory? fd)
      (file-system-condition-signal :not-a-directory path)))

  ;; This operation is similar to probe-file, however the response from the initial
  ;; query will indicate that we're going to begin transmission of multiple lines of data,
  ;; which will be terminated by an empty line.

  ;; We might wish to have the FSA respond with the number of entries it will return.
  ;; For now, we have a stream of replies of unspecified length.
  (fsa/send-request fsa (format nil "PDIR ~s"
                                (pathname->platform-namestring path
                                                               (file-system/platform fsa)))
                                   :expected-response 125 :time-track *fsa-optime-index/pdir*)
  (let* ((file-descriptors nil)                 ;list of file descriptors we're going to build
         (clock-skew (fsa/clock-skew fsa))
         (accumulate-descriptor
          (lambda (line)
              ;; The input to this function is a text string identical to that of PROBE-FILE *except*
              ;; that the EXISTS-P token is missing.  By convention, all files reported here exist.
              ;; PERFORMANCE: Would cons less if we didn't use SPLIT-STRING.
              ;; See FILE-SYSTEM-PROBE-DIRECTORY comments for more details on the line contents.
              (with-input-from-string (stream line)
                (when (safe-read stream)
                  (push
                   (fsa/read-file-info stream
                     (lambda (&key name size modification-date is-directory? &allow-other-keys)
                         (if is-directory?
                             (make-instance 'directory-descriptor
                                            :file-system fsa
                                            :path (platform/parse-namestring (file-system/platform fsa)
                                                                             name)
                                            :modification-date modification-date)
                             (make-instance 'file-descriptor
                                            :file-system fsa
                                            :path (platform/parse-namestring (file-system/platform fsa)
                                                                             name)
                                            :modification-date modification-date
                                            :size size
                                            :mode-list nil))))
                   file-descriptors))))))
    (declare ;(dynamic-extent accumulate-descriptor)
             (ignore clock-skew))

    (fsa/receive-multi-line-response
     fsa accumulate-descriptor
     212)                                       ;terminating reply code
    file-descriptors))

(defmethod file-system/probe-file ((fsa fsa-file-system) (path string) &key known-p)
  (file-system/probe-file fsa (platform/parse-namestring
                               (file-system/platform fsa)
                               path) :known-p known-p))

(defmethod file-system/record-separator ((fsa fsa-file-system))
  (fsa/record-separator fsa))

(defun fsa/read-file-info (stream receiver)
  (funcall receiver
           :name              (safe-read stream)
           :size              (safe-read stream)
           :modification-date (safe-read stream)
           :is-directory?     (safe-read stream)
           :can-read?         (safe-read stream)
           :can-write?        (safe-read stream)
           :executable?       (safe-read stream)))

(defmethod file-system/probe-file ((fsa fsa-file-system) (path pathname) &key known-p)
  "PROTOCOL: 'PFIL unquoted-name' => '200 file information text'"
  (declare (ignore known-p))
  (let ((clock-skew (fsa/clock-skew fsa))
        (reply-text (fsa/send-request
                     fsa (format nil "PFIL ~s"
                                 (pathname->platform-namestring path
                                                                (file-system/platform fsa)))
                     :expected-response 200 :time-track *fsa-optime-index/pfil*)))
    (declare (ignore clock-skew))
    ;; Tokens are strings to be parsed:
    ;;   => exists-p adjusted-relative-path size modification-date isdir-p record-terminator

    ;; EXISTS-P is T or NIL and indicates whether the specified path exists.
    ;; If EXISTS-P is true, the rest of these values follow, otherwise they do not.

    ;; The following information is used in the same order and fashion for the PROBE-DIRECTORY
    ;; implementation, if you change it, change PROBE-DIRECTORY:

    ;; ADJUSTED-RELATIVE-PATH may equal original specification, but is a lisp-readable string
    ;; which encodes the path.

    ;; SIZE is size in bytes on client FSA file system
    ;; MODIFICATION-DATE is universal time (seconds since 00:00:00 Jan 1 1900 GMT ignoring leap year)
    ;; ISDIR-P is T or NIL indicating whether or not the indicated relative-path is a directory.

    ;; If it is not a directory, the record-terminator is the next token.

    ;; TO-DO: add file mode bit specifications and other things
    (with-input-from-string (stream reply-text)
      (and (safe-read stream)                   ;exists
           (fsa/read-file-info stream
            (lambda (&key name size modification-date is-directory? &allow-other-keys)
                (if is-directory?
                    (make-instance 'directory-descriptor
                                   :file-system fsa
                                   :path (platform/parse-namestring (file-system/platform fsa) name)
                                   :modification-date modification-date)
                    (make-instance 'file-descriptor
                                   :file-system fsa
                                   :path (platform/parse-namestring (file-system/platform fsa) name)
                                   :modification-date modification-date
                                   :size size
                                   :mode-list nil))))))))

(defmethod file-system/detect-file-content-type ((fsa fsa-file-system) (path file-descriptor))
  (file-system/detect-file-content-type (file-descriptor/file-system path) (file-descriptor/path path)))

(defmethod file-system/detect-file-content-type ((fsa fsa-file-system) (path pathname))
  "PROTOCOL: 'DTCT unquoted-name' => '125 NBYTES, bytes, 226' **OR** '200 <status>'

   If the file exists, the former response is used, otherwise the latter is used.
   The former response is a multi-part response, and is received only when the file exists,
   is not a directory, and has bytes to sample.

   If the latter response is used, the file exists, is a directory, or has no bytes.
   <status> will be either :NOFILE, :DIRECTORY, or :ZERO.

   We could use the OPEN/CLOSE/READ-BYTES operations for this task, but it's much faster
   to wrap it up as one operation."
  ;; We report two times in the stats, one for the extended time to perform this operation, and one
  ;; for the basic request.  Should to this logically for GETL, PUTL, GETB, and PUTB.
  (when (real-file-system/open-file-descriptor-or-path fsa)
    (error "Attempt to open multiple files when via FILE-SYSTEM-OPEN.  Files must be opened ~
              serially and with intervening calls to FILE-SYSTEM-CLOSE.  Already open: ~s, ~
              attempting to open: ~s"
           (real-file-system/open-file-descriptor-or-path fsa)
           path))

  (with-fsa-time-track (*fsa-optime-index/dtct-extended*)
    (multiple-value-bind (args response-code)
        (fsa/send-request fsa (format nil "DTCT ~s"
                                                     (pathname->platform-namestring
                                                      path
                                                      (file-system/platform fsa)))
                                         :time-track *fsa-optime-index/dtct*)
      (cond ((= response-code 200)
             (let ((reason (read-from-string args)))
               (ecase reason
                 (:NOFILE nil)
                 (:DIRECTORY :BINARY)
                 (:ZERO :ZERO))))
            ((= response-code 125)
             (let ((nbytes (parse-integer args)))
               (fsa/read-bytes-into-internal-buffer fsa (fsa/stream fsa) nbytes)
               ;; Recieve the terminating reply
               (fsa/receive-response fsa "DTCT completion" 226)
               ;; Return the analysis of the buffer
               (file-system/analyze-file-contents (real-file-system/byte-buffer fsa) nbytes)))
            (t
             (error "Flow control error"))))))

(defmethod file-system/detect-file-record-separator ((fsa fsa-file-system) (path string))
  (file-system/detect-file-record-separator fsa (platform/parse-namestring
                                                  (file-system/platform fsa)
                                                  path)))

(defmethod file-system/detect-file-record-separator ((fsa fsa-file-system) (path pathname))
  "PROTOCOL: 'DTRT unquoted-name' => '200 file information text'"
  (declare (ignore known-p))
  (let ((clock-skew (fsa/clock-skew fsa))
        (reply-text (fsa/send-request
                     fsa (format nil "DTRT ~s"
                                 (pathname->platform-namestring path
                                                                (file-system/platform fsa)))
                     :expected-response 200 :time-track *fsa-optime-index/dtrt*)))
    (declare (ignore clock-skew))
    ;; Tokens are strings to be parsed:
    ;;   => record-terminator

    (with-input-from-string (stream reply-text)
      (safe-read stream))))

(defmethod file-system/read-only? ((fsa fsa-file-system) (path pathname))
  "Temporarily re-use the probe file protocol.  This should be
   moved to a separate command in the near future.

  PROTOCOL: 'PFIL unquoted-name' => '200 file information text'"
  ;; Drop for HP pending, don't want to hack and test a new protocol element.
  ;; See documentation for PFIL for details.
  (let ((reply-text (fsa/send-request
                     fsa (format nil "PFIL ~s"
                                 (pathname->platform-namestring path
                                                                (file-system/platform fsa)))
                     :expected-response 200 :time-track *fsa-optime-index/pfil*)))
    (with-input-from-string (stream reply-text)
      (and (safe-read stream)                   ;exists
           (fsa/read-file-info stream
            (lambda (&key can-write? &allow-other-keys)
                (not can-write?)))))))

(defmethod file-system/set-read-only ((fsa fsa-file-system) (path pathname) read-only?)
  "PROTOCOL: 'CHMD pathname <modes>+' => '200 <result>'.
   Where modes is a (list of) upper case keywords as per generic function documentation.
   <RESULT> must be T"
  (read-from-string
   (fsa/send-request
    fsa
    (format nil "CHMD ~s ~s"
            (pathname->platform-namestring path (file-system/platform fsa))
            (if read-only? :read-only :read-write))
    :expected-response 200 :time-track *fsa-optime-index/chmd*)))

(defmethod real-file-system/refill-buffer ((fsa fsa-file-system) stream-handle)
  (let* ((head-length (- (the array-index (real-file-system/buffer-limit fsa))
                         (the array-index (real-file-system/buffer-position fsa))))
         ;; Never want more than the transfer size.
         (bytes-desired (min (- (simple-vector-8b-length (real-file-system/byte-buffer fsa))
                                head-length)
                             +real-file-system/standard-transfer-size+)))

    (when (zerop bytes-desired)
      ;; Buffer is full, and we need more bytes.  Grow the buffer.
      (debug-message 4 "Growing socket file system buffer.")
      (let ((new-buffer (simple-vector-8b-allocate
                         (ceiling
                          (* (simple-vector-8b-length (real-file-system/byte-buffer fsa))
                             +real-file-system/standard-buffer-growth-factor+)))))
        ;(declare (type (simple-vector-8b *) new-buffer))
        (simple-vector-8b-copy (real-file-system/byte-buffer fsa) new-buffer)
        (setf (real-file-system/byte-buffer fsa) new-buffer)
        (setq bytes-desired (min (- (simple-vector-8b-length
                                     (real-file-system/byte-buffer fsa))
                                    head-length)
                                 +real-file-system/standard-transfer-size+))))

    (unless (zerop head-length)
      ;; Move unscanned lines to head of buffer.
      (simple-subvector-8b-move (real-file-system/byte-buffer fsa)
                                head-length
                                (real-file-system/buffer-position fsa)
                                0))
    ;; Fill tail of buffer
    (with-fsa-time-track (*fsa-optime-index/getb-extended*)
      (let* ((args (fsa/send-request fsa
                                                    (format nil "GETB ~d ~d"
                                                            stream-handle
                                                            bytes-desired)
                                                    :time-track *fsa-optime-index/getb*
                                                    :expected-response 125))
             (bytes-available (parse-integer args)))
        ;(declare (type array-index bytes-available))
        ;; File system agent will give us all we ask for unless EOF is seen.
        ;; This avoids another call.
        (when (< bytes-available bytes-desired)
          (setf (file-system/stream-eof? fsa) t))
        (socket-trace "~%RECB: <~d bytes expected, " bytes-available) ;receive BYTE chunk

        (fsa/read-sequence (real-file-system/byte-buffer fsa)
                                          (fsa/stream fsa)
                                          head-length
                                          bytes-available)

        (setf (real-file-system/buffer-position fsa) 0)
        (setf (real-file-system/buffer-limit fsa) (+ head-length bytes-available))
        ;; Bytes are read, receive terminating reply and return appropriate values
        (fsa/receive-response fsa "GETB completion" 226)))))

(defmethod file-system/shutdown ((fsa fsa-file-system) &key (success t))
  "PROTOCOL: 'QUIT <success>' => '221' (close connection)
   <success> is T or NIL.  The FSA serve() operation will return a boolean which
   corresponds to <success>.  So if SUCCESS is T, serve() returns TRUE, otherwise NIL is returned."
  (assert (or (eq success t) (eq success nil)))
  (fsa/send-request fsa (format nil "QUIT ~a" success)
                    :expected-response 221 :time-track *fsa-optime-index/quit*)
  nil)

(defmethod file-system/touch-file ((fsa fsa-file-system) (path string)
                                   timestamp
                                   &key (if-does-not-exist :IGNORE)
                                        set-read-only
                                        set-executable)
  (file-system/touch-file fsa (platform/parse-namestring
                               (file-system/platform fsa)
                               path)
                          timestamp :if-does-not-exist if-does-not-exist
                          :set-read-only set-read-only
                          :set-executable set-executable))

(defmethod file-system/touch-file ((fsa fsa-file-system) (path file-descriptor)
                                   timestamp
                                   &key (if-does-not-exist :IGNORE)
                                        set-read-only
                                        set-executable)
  (file-system/touch-file (file-descriptor/file-system path)
                          (file-descriptor/path path) timestamp
                          :if-does-not-exist if-does-not-exist
                          :set-read-only set-read-only
                          :set-executable set-executable))

(defmethod file-system/touch-file ((fsa fsa-file-system) (path pathname)
                                   timestamp
                                   &key (if-does-not-exist :IGNORE)
                                        set-read-only
                                        set-executable)
  "PROTOCOL: 'TUCH relative-path timestamp <if-does-not-exist>' => '200 <result>'.
   Where if-does-not-exist is an upper case keyword as per generic function documentation,
   stripped of it's keyword package colon (':').  (i.e. (CREATE IGNORE)).  <RESULT> must be T or NIL."
  (assert (or (eq if-does-not-exist :IGNORE)
              (eq if-does-not-exist :CREATE)))  ;sanity check, keep it simpler for client FSA.
  (read-from-string                             ;convert response to T/NIL symbol
   (fsa/send-request
    fsa
    (format nil "TUCH ~s ~a ~a ~a ~a"
            (pathname->platform-namestring path (file-system/platform fsa))
            timestamp
            ;; Escape NIL indicates don't print the leading colon, and make sure they're
            ;; printed in upper case.
            (write-to-string if-does-not-exist :case :upcase :readably nil :escape nil)
            set-read-only
            set-executable
            )
    :expected-response 200 :time-track *fsa-optime-index/tuch*)))

(defun fsa/write-line-flush-buffer (fsa stream-handle &optional buffer
                                        &aux (nElements
                                              (- (the array-index
                                                   (real-file-system/buffer-limit fsa))
                                                 (the array-index
                                                   (real-file-system/buffer-position fsa)))))
  "Flush the current buffer contents, and reset maintaining buffer variables.
   If we already know which buffer we're using, pass it to save the discrimination step.

   Returns the new buffer limit."
  (declare
           ;; #+allegro (:explain :calls :types)
           )
  (if (> nElements 0)
      (progn
        (unless buffer                          ;figure out if we're using char or byte buffer
          (setq buffer (real-file-system/byte-buffer fsa)))
        ;; ship out the bytes
        (do ((start (the array-index (real-file-system/buffer-position fsa)) (+ start +real-file-system/standard-buffer-size+)))
            ((>= start (the array-index (real-file-system/buffer-limit fsa))) nil)
          (fsa/write-buffer fsa
                            stream-handle
                            buffer
                            start
                            (min (+ start +real-file-system/standard-buffer-size+)
                                 (the array-index (real-file-system/buffer-limit fsa)))))
        ;; Clear the buffer indicator
        (setf (real-file-system/buffer-limit fsa)
              (real-file-system/buffer-position fsa)))
    ;; Nothing to transmit?  Just compute return value
    (real-file-system/buffer-limit fsa)))

(defun fsa/write-line-ensure-buffer (fsa stream-handle sequence end)
  "Ensure that we have an appropriate buffer for WRITE-LINE given the indicated arguments.
   Return the appropriate (possibly newly allocated or expanded) buffer.
   May flush the buffer if necessary for expansion, and maintains buffer variables. in the FSA.

   The buffer element type is that of the underlying socket stream, and not the input sequence."
  (declare ;(type (simple-array character (*)) sequence)
           ;; #+allegro (:explain :calls :types)
            )
  ;; The (+ 2 ...) is to accomodate record separator characters for the longest possible
  ;; canonical record separator sequence
  (let* ((needed-buffer-allocation (+ 2 (the array-index (or end (length sequence)))))
         (buffer (the (simple-vector-8b *)
                   (or (real-file-system/byte-buffer fsa)
                       (real-file-system/ensure-byte-buffer fsa needed-buffer-allocation))))
         (current-buffer-length (simple-vector-8b-length buffer)))
    (declare ;(type (simple-vector-8b *) buffer)
             ;(type array-index current-buffer-length needed-buffer-allocation)
             )
    (when (< current-buffer-length needed-buffer-allocation)
      ;; Sequence passed in is would overflow buffer, flush existing contents and
      ;; reallocate.
      (when (> (the array-index (real-file-system/buffer-limit fsa)) 0)
        (fsa/write-line-flush-buffer fsa stream-handle buffer))
      (setq buffer (real-file-system/ensure-byte-buffer fsa needed-buffer-allocation)))
    buffer))

(defmethod file-system/write-line ((fsa fsa-file-system) (stream-handle integer) (string string)
                                   &key (end (simple-string-length string)) suppress-separator)
  "PROTOCOL: 'PUTB <nBytes>' => '125' ; <bytes> => '226'.
   This protocol genrates two transmits, one which is preliminary, and one which is the data.

   PUTB does all byte translation server-side.  The FSA need only relay the bytes to disk as binary data.

   NOTE: WRITE-LINE calls buffer the output, and it isn't necessarily flushed until CLOSE.
   This is to package up many short lines as one large transmission to minimize socket synchronization.
   The PUTB calls represent this buffer flush, so PUTB's aren't necessarily generated for every WRITE-LINE.

   STRING is assumed to represent a logical line without any line terminator.
   We will prepare it for transmission and insert canonical line separation characters."
  (declare ;(type array-index end)
           ;(type (simple-array character (*)) string)
           ;; #+allegro (:explain :types :calls)
           )

  (let ((buffer (fsa/write-line-ensure-buffer fsa stream-handle string end))
        (limit  (real-file-system/buffer-limit fsa)))
    (declare ;(type (simple-vector-8b *) buffer)
             ;(type array-index limit)
             )

    ;; The above call ensured a buffer was allocated which is guaranteed to be a sane length, and hold
    ;; the input string.  However it didn't necessarily ensure that room remains for the string
    ;; given any current contents.  If the new string won't fit in the room remaining in the buffer,
    ;; flush the buffer
    (when (> (+ (simple-string-length string)
                (if suppress-separator 0 2)                     ;maximum # chars for termination
                limit)
             (simple-vector-8b-length buffer))
      (setq limit
            (fsa/write-line-flush-buffer fsa stream-handle buffer)))

    (unless (zerop (simple-string-length string))
      ;; Buffer the characters, don't worry about writing them, 'ensure-buffer' did that
      ;; and CLOSE will also do that. We're appending to the buffer here.
      ;; Buffer is bytes, string is chars, convert chars to bytes
      (dotimes (sequence-x (the array-index (or end (simple-string-length string))))
        ;(declare (type array-index sequence-x))
        (setf (aref buffer limit) (char-code (schar string sequence-x)))
        (incf limit)))
    (unless suppress-separator
      (let ((separator (real-file-system/open-file-record-separator fsa)))
        (when (or (eq separator :cr)
                  (eq separator :crlf))
          (setf (aref buffer limit) *ascii-carriage-return*)
          (incf limit))
        (when (or (eq separator :lf)
                  (eq separator :crlf))
          (setf (aref buffer limit) *ascii-linefeed*)
          (incf limit))))
    (assert (<= limit (simple-vector-8b-length buffer)))
    (setf (real-file-system/buffer-limit fsa) limit)
    nil))

(defmethod file-system/read-bytes ((fsa fsa-file-system) (stream-handle integer) sequence)
  "PROTOCOL: 'GETB stream-handle max-Bytes' => '125 n-bytes, bytes, 226'
   Where MAX-BYTES is the maximum bytes to be returned in this read.  It should typically be large
   for the SFS requests."

  ;; Don't call socket again if it's reported EOF (zero bytes).
  (when (file-system/stream-eof? fsa)
    (return-from file-system/read-bytes (values nil 0)))

  (unless sequence
    (setq sequence
          (simple-vector-8b-allocate +real-file-system/standard-buffer-size+)))

  (with-fsa-time-track (*fsa-optime-index/getb-extended*)
    (let* ((args (fsa/send-request fsa (format nil "GETB ~d ~d"
							      stream-handle
							      (length sequence))
						  :time-track *fsa-optime-index/getb*
						  :expected-response 125))
	   (n-bytes-available (parse-integer args)))

      ;; Read bytes and terminating reply, return values
      ;; Note that unlike DTCT, we always have a terminating reply, even for zero bytes.
      (cond ((= n-bytes-available 0)
	     (setf (file-system/stream-eof? fsa) t))

	    ;; Otherwise, bytes are available, read them
	    (t
	     (when (< n-bytes-available (length sequence))
	       ;; We requested more than we got, assume EOF so we avoid extra GETB later.
	       (setf (file-system/stream-eof? fsa) t))

	     (real-file-system/read-bytes-into-some-buffer
	      fsa (fsa/stream fsa) n-bytes-available sequence)))
      ;; Bytes are read, receive terminating reply and return appropriate values
      (fsa/receive-response fsa "GETB completion" 226)
      (values (and (> n-bytes-available 0) sequence) n-bytes-available))))

(defun fsa/write-buffer (fsa stream-handle buffer start end)
  "PROTOCOL: 'PUTB nbytes' => 125, <bytes>, 226'
   Where <bytes> is read by the FSA, not returned to the FS.
   Note that 125 may be ok, but 226 will not be as expected if there was some problem writing the bytes
   on the FSA, and that this is a useful terminating reply to keep (performance aside) for
   error detection value.  For PERFORMANCE: reasons however, we may still decide to eliminate one of
   the two replies at a later date."
  ;; This HELPER FUNCTION does the actual byte output for FILE-SYSTEM-WRITE-BYTES.
  ;; Arguments are assumed to be correct, so no checking is done.
  (declare
           ;(type array-index start end)
           ;(type simple-vector-8b buffer)
           ;; #+allegro (:explain :calls)
           )
  (let ((nbytes (- end start)))
    (with-fsa-time-track (*fsa-optime-index/putb-extended*)
      (fsa/send-request
       fsa (format nil "PUTB ~d ~d" stream-handle nbytes)
       :time-track *fsa-optime-index/putb* :expected-response 125)
      ;; Write the bytes to the FSA
      (socket-trace "~%PUTB: <~d bytes>" nbytes)
      ;; Put raw bytes
      (write-sequence buffer
                      (fsa/stream fsa)
                      :start start
                      :end end)
      (force-output (fsa/stream fsa))
      ;; Bytes are sent, receive terminating reply and return appropriate values
      (fsa/receive-response fsa "PUTB completion" 226)
      nil)))

(defmethod file-system/write-bytes ((fsa fsa-file-system) (stream-handle integer) (sequence sequence)
				    &key (end (length sequence)))
  "PROTOCOL: 'PUTB nbytes' => 125, <bytes>, 226'
   Where <bytes> is read by the FSA, not returned to the FS.
   Note that 125 may be ok, but 226 will not be as expected if there was some problem writing the bytes
   on the FSA, and that this is a useful terminating reply to keep (performance aside) for
   error detection value.  For PERFORMANCE: reasons however, we may still decide to eliminate one of
   the two replies at a later date."
  (declare #.(performance-optimizations)
	   ;; #+allegro (:explain :calls)
	   )
  (check-type sequence (simple-array (unsigned-byte 8)))
  (check-type end array-index)
  (do ((start 0 (+ start +real-file-system/standard-buffer-size+)))
      ((>= start end) nil)
    (declare (type array-index start))
    (fsa/write-buffer fsa
                      stream-handle
                      sequence
                      start
                      (min (+ start +real-file-system/standard-buffer-size+)
                           end))))


;;;
;;;
;;; FILTERED-FILE-SYSTEM
;;;
;;; A filtered file system has a predicate that is called on filenames to determine
;;; whether they are interesting or not.  Uninteresting files are not included in
;;; the file-system-probe-directory call, and thus are not seen by the upper layers
;;; of E-Zchange.

(defclass filtered-file-system (delegate-file-system)
  ;; This is the filter.  IT is a predicate that takes a pathname as an argument
  ;; and returns T iff the pathname is interesting.
  ((filter :type function
           :initarg :filter
           :reader filtered-file-system/filter
           :initform (constantly t)))
    (:documentation "A FILE-SYSTEM subtype that ignores certain files."))

(defun filtered-file-system-create (filter underlying-file-system)
  "Create an instance of a FILTERED-FILE-SYSTEM"
  (make-instance 'filtered-file-system
    :delegate-file-system-underlying-file-system underlying-file-system
    :filtered-file-system-filter filter
    ))

(defmethod file-system/probe-directory ((fs filtered-file-system) (path file-descriptor))
  (file-system/probe-directory (file-descriptor/file-system path) (file-descriptor/path path)))

(defmethod file-system/probe-directory ((fs filtered-file-system) (path pathname))
  (mapcar (lambda (descriptor)
              (if (file-descriptor/is-directory? descriptor)
                  (make-instance 'directory-descriptor
                   :file-system fs
                   :path (file-descriptor/path descriptor)
                   :modification-date (file-descriptor/modification-date descriptor))
                (make-instance 'file-descriptor
                 :file-system fs
                 :path (file-descriptor/path descriptor)
                 :modification-date (file-descriptor/modification-date descriptor)
                 :size (file-descriptor/size descriptor)
                 :mode-list (file-descriptor/mode-list descriptor))))
          (remove-if (complement (lambda (fd)
                                     (funcall (filtered-file-system/filter fs)
                                              (file-descriptor/path fd))))
                     (file-system/probe-directory (delegate-file-system/underlying-file-system fs) path))))


;;;
;;; LOGICAL-FILE-SYSTEM
;;;
;;; A logical file system is a file system that uses logical pathnames for its arguments.
;;; It delegates all operations to a physical file system (like a lisp file system or a
;;; socket file system) after translating the logical pathnames into the appropriate
;;; absolute pathnames.

;;; Each file system uses Lisp pathname objects to represent file and directory names in the syntax
;;; supported by the host that it represents.  The repository uses logical pathnames to refer to
;;; repository files and directories in a file system neutral way.  This is a slight abuse of the
;;; logical pathname idea, but it is a conceptual extension to it.  So logical pathnames should
;;; be used as filenames in this API.
;;;
;;; Furthermore, the logical pathnames are RELATIVE pathnames in that they have no particular root
;;; directory.  Each file system has a root directory, so the logical pathnames are resolved by
;;; converting the pathname to the syntax appropriate for the file system in question, and then
;;; merging the resulting relative path with the absolute root path.
;;;
;;; Filenames obtained from the file system are converted back to relative logical form by
;;; using ENOUGH-NAMESTRING to find the relative path, then creating a logical pathname from that
;;; result.
;;;
;;; The file system API's can also take strings rather than logical pathnames.  These strings are
;;; simply converted to logical pathnames.  This is a convenience.
;;;
;;; All logical pathnames have a logical host associated with them.  I use `repository'.  This has
;;; no meaning other than to signal to the programmer that he is dealing with a file system neutral
;;; file.
;;;
;;; Problems:  There is no portable way to create a logical pathname.
;;; The logical pathname facilities in Allegro Common Lisp seem to be broken or only partly
;;; implemented.  This system depends on LOGICAL-PATHNAME being distinguishable from PATHNAME
;;; (as a subtype), and on the two procedures RELATIVIZE-PATH and RESOLVE-RELATIVE-PATH.  The
;;; former is a kludge and should be handled better.

(defsubst valid-file-system-root? (root)
  (guarantee-absolute-directory-pathname root))

(defclass logical-file-system (delegate-file-system)
  ((root :type logical-pathname
         :initarg :root
         :reader logical-file-system/root))
  (:documentation "A FILE-SYSTEM subtype that operates on logical pathnames."))

(defmethod print-object ((object logical-file-system) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "to ~s" (logical-file-system/root object))))

(defmethod initialize-instance :after ((instance logical-file-system)
                                       &rest ignore
                                       &key underlying-file-system)
  (debug-message 5 "Creating logical-file-system ~s" instance)
  (assert (typep underlying-file-system 'real-file-system)))

(defmethod initialize-instance :before ((instance logical-file-system) &rest ignore)
  (debug-message 5 "Created logical-file-system ~s" instance))

(defun relativize-path (absolute-path file-system)
  "Given an absolute path in a file-system, return an OS neutral relative path (logical pathname).
Inverse of RESOLVE-RELATIVE-PATH."
  (let* ((relative (funcall (platform-enough-pathname (file-system/platform file-system))
                            absolute-path
                            (logical-file-system/root file-system)))
         (result (make-pathname :host "REPOSITORY"
                                ;; NOTE:  We need to force this to be '(:relative) in the case
                                ;; that we are hanging off the root directory.
                                :directory (or (mapcar
                                                (lambda (component)
                                                    (if (stringp component)
                                                        (encode-namestring component)
                                                      component))
                                                (canonicalize-pathname-directory (pathname-directory relative)))
                                               '(:relative))
                                :name (cond ((stringp (pathname-name relative))
                                             (encode-namestring (pathname-name relative)))
                                            ((eq (pathname-name relative) :unspecific) nil)
                                            (t (pathname-name relative)))
                                :type (cond ((stringp (pathname-type relative))
                                             (encode-namestring (pathname-type relative)))
                                            ((eq (pathname-type relative) :unspecific)
                                             nil)
                                            (t (pathname-type relative)))
                                :version (cond ((stringp (pathname-version relative))
                                                (encode-namestring (pathname-version relative)))
                                               ((eq (pathname-version relative) :unspecific)
                                                #+lispworks :newest
                                                #-lispworks nil)
                                               (t (pathname-version relative)))
                                :defaults ""))
         )
    ;; This thing has been problematic.  Make sure it did the right thing.
    (assert (logical-pathname-p result))
    result))

(defun relativize-file-descriptor (fd lfs)
  (if (file-descriptor/is-directory? fd)
      (make-instance 'directory-descriptor
                     :file-system lfs
                     :path (relativize-path (file-descriptor/path fd) lfs)
                     :modification-date (file-descriptor/modification-date fd))
      (make-instance 'file-descriptor
                     :file-system lfs
                     :path (relativize-path (file-descriptor/path fd) lfs)
                     :modification-date (file-descriptor/modification-date fd)
                     :size (file-descriptor/size fd)
                     :mode-list (file-descriptor/mode-list fd))))

(defun file-system/friendly-name (path file-system)
  "Given an OS neutral relative path (logical pathname), return a path relative to the file
system root.  This is used to print pathnames in file-system specific format for the user and
should not be used for computation."
  (pathname->platform-namestring
   (funcall (platform-enough-pathname (file-system/platform file-system))
            (resolve-relative-path path file-system)
            (logical-file-system/root file-system))
   (file-system/platform file-system)))

(defun resolve-relative-path (path lfs)
  "Given an OS neutral relative path (logical pathname), return an absolute path in a file-system.
Inverse of RELATIVIZE-PATH."
  (let ((root (logical-file-system/root lfs)))
    (make-pathname :host (pathname-host root)
                   :device (pathname-device root)
                   :directory (if (pathname-directory path)
                                  (append (pathname-directory root)
                                          (mapcar #'decode-namestring (cdr (pathname-directory path))))
                                (pathname-directory root))
                   :name (if (stringp (pathname-name path))
                             (decode-namestring (pathname-name path))
                           (pathname-name path))
                   :type (if (stringp (pathname-type path))
                             (decode-namestring (pathname-type path))
                           (pathname-type path))
                   :version (if (stringp (pathname-version path))
                                (decode-namestring (pathname-version path))
                              (pathname-version path)))))

(defun logical-file-system/change-directory (old-lfs new-dir)
  "Return a new logical file system rooted at NEW-DIR."
  (cond ((stringp new-dir)
         (logical-file-system/change-directory old-lfs
                                               (ensure-directory-pathname-designator
                                                (pathname new-dir)
                                                (file-system/directory-separators old-lfs))))
        ;; If the new dir is a logical pathname and it is relative directory,
        ;; this means we want to re-root at the inner directory.
        ((and (logical-pathname-p new-dir)
              (eq (car (pathname-directory new-dir)) :RELATIVE))
         (make-instance 'logical-file-system
                        :root (resolve-relative-path new-dir old-lfs)
                        :underlying-file-system (delegate-file-system/underlying-file-system old-lfs)))
        (t
         (make-instance 'logical-file-system
                        :root (merge-pathnames new-dir (logical-file-system/root old-lfs))
                        :underlying-file-system (delegate-file-system/underlying-file-system old-lfs)))))

(defmethod file-system/copy-file ((fs logical-file-system)
                                  (relative-path file-descriptor)
                                  new-relative-path)
  (file-system/copy-file (file-descriptor/file-system relative-path)
                         (file-descriptor/path relative-path) new-relative-path))

(defmethod file-system/copy-file ((fs logical-file-system)
                                  relative-path
                                  (new-relative-path file-descriptor))
  (file-system/copy-file fs relative-path (file-descriptor/path new-relative-path)))

(defmethod file-system/copy-file ((fs logical-file-system)
                                  (relative-path string)
                                  new-relative-path)
  (file-system/copy-file fs (logical-pathname relative-path) new-relative-path))

(defmethod file-system/copy-file ((fs logical-file-system)
                                  (relative-path logical-pathname)
                                  (new-relative-path string))
  (file-system/copy-file fs relative-path (logical-pathname new-relative-path)))

(defmethod file-system/copy-file ((fs logical-file-system)
                                  (relative-path logical-pathname)
                                  (new-relative-path logical-pathname))
  (file-system/copy-file
   (delegate-file-system/underlying-file-system fs)
   (resolve-relative-path relative-path fs)
   (resolve-relative-path new-relative-path fs)))

(defmethod file-system/crc-file ((fs logical-file-system) (relative-path file-descriptor))
  (file-system/crc-file
   (file-descriptor/file-system relative-path)
   (file-descriptor/path relative-path)))

(defmethod file-system/crc-file ((fs logical-file-system) (relative-path logical-pathname))
  (file-system/crc-file
   (delegate-file-system/underlying-file-system fs)
   (resolve-relative-path relative-path fs)))

(defmethod file-system/crc-file-background ((fs logical-file-system)
                                            (relative-path file-descriptor)
                                            file-type
                                            foreground-procedure)
  (file-system/crc-file-background
   (file-descriptor/file-system relative-path)
   (file-descriptor/path relative-path)
   file-type
   foreground-procedure))

(defmethod file-system/crc-file-background ((fs logical-file-system)
                                            (relative-path logical-pathname)
                                            file-type
                                            foreground-procedure)
  (file-system/crc-file-background
   (delegate-file-system/underlying-file-system fs)
   (resolve-relative-path relative-path fs)
   file-type
   foreground-procedure))

(defmethod file-system/create-directory ((fs logical-file-system) (relative-path string))
  (file-system/create-directory fs (logical-pathname relative-path)))

(defmethod file-system/create-directory ((fs logical-file-system) (relative-path logical-pathname))
  (file-system/create-directory
   (delegate-file-system/underlying-file-system fs)
   (resolve-relative-path relative-path fs)))

(defmethod file-system/delete-directory ((fs logical-file-system) (relative-path string) &key recursive force)
  (file-system/delete-directory fs (logical-pathname relative-path) :recursive recursive :force force))

(defmethod file-system/delete-directory ((fs logical-file-system) (relative-path logical-pathname)
                                         &key recursive force)
  (file-system/delete-directory
   (delegate-file-system/underlying-file-system fs)
   (resolve-relative-path relative-path fs)
   :recursive recursive :force force))

(defmethod file-system/delete-directory ((fs logical-file-system) (fd directory-descriptor)
                                         &key recursive force)
  (file-system/delete-directory fs (file-descriptor/path fd)
                                :recursive recursive :force force))

(defmethod file-system/delete-file ((fs logical-file-system) (fd file-descriptor) &key force)
  (file-system/delete-file (file-descriptor/file-system fd) (file-descriptor/path fd) :force force))

(defmethod file-system/delete-file ((fs logical-file-system) (relative-path string) &key force)
  (file-system/delete-file fs (logical-pathname relative-path) :force force))

(defmethod file-system/delete-file ((fs logical-file-system) (relative-path logical-pathname) &key force)
  (file-system/delete-file
   (delegate-file-system/underlying-file-system fs)
   (resolve-relative-path relative-path fs) :force force))

(defmethod file-system/detect-file-content-type ((fs logical-file-system) (relative-path file-descriptor))
  (file-system/detect-file-content-type
   (file-descriptor/file-system relative-path)
   (file-descriptor/path relative-path)))

(defmethod file-system/detect-file-content-type ((fs logical-file-system) (relative-path logical-pathname))
  (file-system/detect-file-content-type
   (delegate-file-system/underlying-file-system fs)
   (resolve-relative-path relative-path fs)))

(defmethod file-system/detect-file-record-separator ((fs logical-file-system) (relative-path file-descriptor))
  (file-system/detect-file-record-separator
   (file-descriptor/file-system relative-path)
   (file-descriptor/path relative-path)))

(defmethod file-system/detect-file-record-separator ((fs logical-file-system) (relative-path logical-pathname))
  (file-system/detect-file-record-separator
   (delegate-file-system/underlying-file-system fs)
   (resolve-relative-path relative-path fs)))

(defmethod file-system/open ((fs logical-file-system) (relative-path string)
                             &key (direction :input) (element-type 'character) record-separator
                                  (if-exists :error)
                                  (if-does-not-exist (file-system-if-does-not-exist-default
                                                      if-exists direction)))
  (file-system/open fs (pathname relative-path)
                    :direction direction
                    :element-type element-type
                    :record-separator record-separator
                    :if-exists if-exists
                    :if-does-not-exist if-does-not-exist))

(defmethod file-system/open ((fs logical-file-system) (relative-path file-descriptor)
                             &key (direction :input)
                                  #-(and :allegro-version>= (:version>= 6 0)) (element-type 'character)
                                  #+(and :allegro-version>= (:version>= 6 0)) element-type
                                  record-separator
                                  (if-exists :error)
                                  (if-does-not-exist (file-system-if-does-not-exist-default
                                                      if-exists direction)))
  #-(and :allegro-version>= (:version>= 6 0))
  (file-system/open (file-descriptor/file-system relative-path) (file-descriptor/path relative-path)
                    :direction direction
                    :element-type element-type
                    :record-separator   record-separator
                    :if-exists if-exists
                    :if-does-not-exist if-does-not-exist)
  #+(and :allegro-version>= (:version>= 6 0))
  (if element-type
      (file-system/open (file-descriptor/file-system relative-path) (file-descriptor/path relative-path)
                        :direction direction
                        :element-type element-type
                        :record-separator       record-separator
                        :if-exists if-exists
                        :if-does-not-exist if-does-not-exist)
    (file-system/open (file-descriptor/file-system relative-path) (file-descriptor/path relative-path)
                      :direction direction
                      :record-separator record-separator
                      :if-exists if-exists
                      :if-does-not-exist if-does-not-exist)))

(defmethod file-system/open ((fs logical-file-system) (relative-path logical-pathname)
                             &key (direction :input)
                                  #-(and :allegro-version>= (:version>= 6 0)) (element-type 'character)
                                  #+(and :allegro-version>= (:version>= 6 0)) element-type
                                  record-separator
                                  (if-exists :error)
                                  (if-does-not-exist (file-system-if-does-not-exist-default
                                                      if-exists direction)))
  #-(and :allegro-version>= (:version>= 6 0))
  (file-system/open (delegate-file-system/underlying-file-system fs)
                    (resolve-relative-path relative-path fs)
                    :direction direction
                    :element-type element-type
                    :record-separator record-separator
                    :if-exists if-exists
                    :if-does-not-exist if-does-not-exist)
  #+(and :allegro-version>= (:version>= 6 0))
  (if element-type
      (file-system/open (delegate-file-system/underlying-file-system fs)
                        (resolve-relative-path relative-path fs)
                        :direction direction
                        :element-type element-type
                        :record-separator record-separator
                        :if-exists if-exists
                        :if-does-not-exist if-does-not-exist)
    (file-system/open (delegate-file-system/underlying-file-system fs)
                      (resolve-relative-path relative-path fs)
                      :direction direction
                      :record-separator record-separator
                      :if-exists if-exists
                      :if-does-not-exist if-does-not-exist)))

(defmethod file-system/probe-directory ((fs logical-file-system) (path string))
  (file-system/probe-directory fs (logical-pathname path)))

(defmethod file-system/probe-directory ((fs logical-file-system) (fd base-file-descriptor))
  (file-system/probe-directory (file-descriptor/file-system fd) (file-descriptor/path fd)))

(defmethod file-system/probe-directory ((fs logical-file-system) (relative-path logical-pathname))
  (mapcar (lambda (fd)
              (relativize-file-descriptor fd fs))
          (file-system/probe-directory
           (delegate-file-system/underlying-file-system fs)
           (resolve-relative-path relative-path fs))))

(defmethod file-system/probe-file ((fs logical-file-system) (path string) &key known-p)
  (file-system/probe-file fs (logical-pathname path) :known-p known-p))

(defmethod file-system/probe-file ((fs logical-file-system) (relative-path logical-pathname)
                                   &key known-p)
  (let ((real-path
         (file-system/probe-file (delegate-file-system/underlying-file-system fs)
                                 (resolve-relative-path relative-path fs)
                                 :known-p known-p)))
    (if real-path
        (relativize-file-descriptor real-path fs)
      nil)))

(defmethod file-system/read-only? ((fs logical-file-system) (fd file-descriptor))
  (file-system/read-only? (file-descriptor/file-system fd) (file-descriptor/path fd)))

(defmethod file-system/read-only? ((fs logical-file-system) (relative-path logical-pathname))
  (file-system/read-only?
   (delegate-file-system/underlying-file-system fs)
   (resolve-relative-path relative-path fs)))

(defmethod file-system/set-read-only ((fs logical-file-system) (fd file-descriptor) read-only)
  (file-system/set-read-only (file-descriptor/file-system fd) (file-descriptor/path fd) read-only))

(defmethod file-system/set-read-only ((fs logical-file-system) (relative-path logical-pathname) read-only?)
  (file-system/set-read-only
   (delegate-file-system/underlying-file-system fs)
   (resolve-relative-path relative-path fs)
   read-only?))

(defmethod file-system/rename ((fs logical-file-system)
                               (relative-path file-descriptor)
                               new-relative-path)
  (file-system/rename (file-descriptor/file-system relative-path) (file-descriptor/path relative-path) new-relative-path))

(defmethod file-system/rename ((fs logical-file-system)
                               relative-path
                               (new-relative-path file-descriptor))
  (file-system/rename fs relative-path (file-descriptor/path new-relative-path)))

(defmethod file-system/rename ((fs logical-file-system)
                               (relative-path string)
                               new-relative-path)
  (file-system/rename fs (logical-pathname relative-path) new-relative-path))

(defmethod file-system/rename ((fs logical-file-system)
                               (relative-path logical-pathname)
                               (new-relative-path string))
  (file-system/rename fs relative-path (logical-pathname new-relative-path)))

(defmethod file-system/rename ((fs logical-file-system)
                               (relative-path logical-pathname)
                               (new-relative-path logical-pathname))
  (file-system/rename
   (delegate-file-system/underlying-file-system fs)
   (resolve-relative-path relative-path fs)
   (resolve-relative-path new-relative-path fs)))

(defmethod file-system/touch-file ((fs logical-file-system) (relative-path file-descriptor)
                                   timestamp
                                   &key (if-does-not-exist :ignore)
                                        set-read-only
                                        set-executable)
  (file-system/touch-file
   (file-descriptor/file-system relative-path) (file-descriptor/path relative-path) timestamp
   :if-does-not-exist if-does-not-exist
   :set-read-only set-read-only
   :set-executable set-executable))

(defmethod file-system/touch-file ((fs logical-file-system) (relative-path logical-pathname)
                                   timestamp
                                   &key (if-does-not-exist :ignore)
                                        set-read-only
                                        set-executable)
  (assert (or (eq if-does-not-exist :IGNORE)
              (eq if-does-not-exist :CREATE))) ;sanity check, keep it simpler for client FSA.
  (file-system/touch-file
   (delegate-file-system/underlying-file-system fs)
   (resolve-relative-path relative-path fs)
   timestamp
   :if-does-not-exist if-does-not-exist
   :set-read-only set-read-only
   :set-executable set-executable))

(defmethod file-system/write-line ((fs delegate-file-system) stream-handle sequence
                                   &key (end (simple-string-length sequence)) suppress-termination)
  (file-system/write-line
   (delegate-file-system/underlying-file-system fs)
   stream-handle
   sequence
   :end end
   :suppress-termination suppress-termination))

(defmethod graph-roots ((fs logical-file-system))
  (file-system/probe-directory fs #p"REPOSITORY:;"))



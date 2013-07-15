;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002-2003 ChangeSafe, LLC
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

(in-package "SOCKET")

(proclaim (standard-optimizations))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(
            accept-connection
            dotted-to-ipaddr
            ipaddr-to-dotted
            local-host
            local-port
            lookup-hostname
            make-socket
            remote-host
            socket-control
            set-socket-options
            with-pending-connect
            ))
  (intern "MAKE-SSL-SERVER-STREAM")
  (intern "MAKE-SSL-CLIENT-STREAM"))

(defunimplemented dotted-to-ipaddr (dotted &key (errorp t)))

(defun ipaddr-to-dotted (ipaddr &key values)
  (declare (type (unsigned-byte 32) ipaddr))
  (if values
      (values (ldb (byte 8 24) ipaddr)
              (ldb (byte 8 16) ipaddr)
              (ldb (byte 8 8)  ipaddr)
              (ldb (byte 8 0)  ipaddr))
      (multiple-value-bind (a b c d) (ipaddr-to-dotted ipaddr :values t)
        (format nil "~d.~d.~d.~d" a b c d))))

(defunimplemented lookup-hostname (host))

(defun remote-host (socket)
  (comm:socket-stream-peer-address socket))

(defclass internet-socket ()
  ((open :initform t
         :accessor socket/open)
   (underlying-socket :initarg :underlying-socket
                      :accessor socket/underlying-socket)))

(defmethod close ((stream internet-socket) &key abort)
  (declare (ignore abort))
  (if (not (socket/open stream))
      (error 'changesafe-stream-closed-error
             :stream stream))
  (etypecase (socket/underlying-socket stream)
    (stream (close (socket/underlying-socket stream) :abort abort))
    (number (comm::close-socket (socket/underlying-socket stream))))
  (setf (socket/open stream) nil))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defconstant +output-buffer-size+ 1024 "Number of bytes in output buffer."))

(defclass stream-socket (internet-socket
                         stream:fundamental-binary-input-stream
                         stream:fundamental-binary-output-stream)
  ((output-chunking :initform nil
                    :accessor socket/output-chunking?)
   (output-buffer :initarg nil
                  :initform (make-array (list +output-buffer-size+)
                                        :element-type '(unsigned-byte 8)
                                        :fill-pointer 0)
                  :accessor socket/output-buffer
                  :type (vector (unsigned-byte 8) #.+output-buffer-size+))
   (input-chunking :initform nil
                   :accessor socket/input-chunking?)
   (backup-char :initform nil
                :accessor socket/backup-char
                :type char-8b)))

(defsubst clear-buffer (stream)
  (setf (fill-pointer (socket/output-buffer stream)) 0))

(defsubst emit-buffer (stream)
  (write-sequence
   (socket/output-buffer stream)
   (socket/underlying-socket stream))
  (clear-buffer stream))

(defun emit-chunk (stream)
  (let* ((chunk (socket/output-buffer stream))
         (chunk-size (fill-pointer chunk))
         (underlying (socket/underlying-socket stream)))
    (unless (zerop chunk-size)
      (write chunk-size
             :base 16
             :stream underlying)
      (write-byte *ascii-carriage-return* underlying)
      (write-byte *ascii-linefeed* underlying)
      (emit-buffer stream)
      (write-byte *ascii-carriage-return* underlying)
      (write-byte *ascii-linefeed* underlying))))

(defsubst clear-buffer-or-chunk (stream)
  (clear-buffer stream))

(defun flush-buffer-or-chunk (stream)
  (if (socket/output-chunking? stream)
      (emit-chunk stream)
      (emit-buffer stream)))

(defmethod stream:stream-write-byte ((stream stream-socket) byte)
  (check-type byte (unsigned-byte 8))
  (or (vector-push byte (socket/output-buffer stream))
      (progn (flush-buffer-or-chunk stream)
             (stream:stream-write-byte stream byte))))

(defmethod stream:stream-write-char ((stream stream-socket) character)
  (check-type character char-8b)
  (stream:stream-write-byte stream (char-code character)))

(defmethod stream:stream-read-byte ((stream stream-socket))
  (handler-case (stream:stream-read-byte (socket/underlying-socket stream))
    (comm::socket-error (condition)
      (declare (ignore condition))
      ;; If we get a socket error, assume that it is because
      ;; the peer closed the connection and return :EOF to
      ;; make lispworks happy.  Sigh.
      :eof)))

(defmethod stream:stream-read-char ((stream stream-socket))
  (if (socket/backup-char stream)
      (prog1 (socket/backup-char stream)
        (setf (socket/backup-char stream) nil))
      (let ((byte (stream:stream-read-byte stream)))
        (if (eq byte :eof)
            byte
            (code-char byte)))))

(defmethod stream:stream-read-char-no-hang ((stream stream-socket))
  (if (socket/backup-char stream)
      (prog1 (socket/backup-char stream)
        (setf (socket/backup-char stream) nil))
      nil))

(defmethod stream:stream-unread-char ((stream stream-socket) char)
  (check-type char char-8b)
  (setf (socket/backup-char stream) char))

(defmethod stream:stream-write-sequence ((stream stream-socket) sequence start end)
  (let ((sequence-type (array-element-type sequence))
        (stream-type   (stream-element-type (socket/underlying-socket stream))))
    (debug-message 5 "Writing ~d elements of type ~s to socket stream of type ~s"
                   (- end start)
                   sequence-type stream-type)
    (let* ((bytes-to-write (- end start))
           (output-buffer (socket/output-buffer stream))
           (fill-point    (length output-buffer))
           (limit         +output-buffer-size+)
           (room          (- limit fill-point)))
      (if (>= room (- end start))
          (progn
            (incf (fill-pointer output-buffer) bytes-to-write)
            (do ((src start      (1+ src))
                 (dst fill-point (1+ dst))
                 (count 0        (1+ count)))
                ((= count bytes-to-write))
              (setf (aref output-buffer dst)
                    (aref sequence src))))
          (progn
            (incf (fill-pointer output-buffer) room)
            (do ((src start      (1+ src))
                 (dst fill-point (1+ dst))
                 (count 0        (1+ count)))
                ((= count room))
              (setf (aref output-buffer dst)
                    (aref sequence src)))
            (emit-buffer stream)
            (stream:stream-write-sequence stream sequence (+ start room) end))))))

(defmethod stream:stream-force-output ((stream stream-socket))
  ;; FORCE OUTPUT initiates the emptying of internal buffers
  ;; but does not wait for completion.

  ;; Punt if nothing is buffered.
  (unless (zerop (length (socket/output-buffer stream)))
    (flush-buffer-or-chunk stream)
    (handler-case (stream:stream-force-output (socket/underlying-socket stream))
      (comm::socket-error (condition)
        (declare (ignore condition))
        nil))))

(defmethod stream:stream-finish-output ((stream stream-socket))
  ;; FINISH OUTPUT attempts to ensure that the bytes get
  ;; to their destination.

  (unless (zerop (length (socket/output-buffer stream)))
    (flush-buffer-or-chunk stream))
  (finish-output (socket/underlying-socket stream)))

(defmethod stream:stream-clear-output ((stream stream-socket))
  (unless (zerop (length (socket/output-buffer stream)))
    (clear-buffer-or-chunk stream))
  (clear-output (socket/underlying-socket stream)))

(defun close-chunk (stream)
  (unless (socket/output-chunking? stream)
    (error "Output chunking disabled on stream ~s" stream))
  (emit-chunk stream)
  (write-sequence #(#.(lisp-char->ascii-code #\0)
                      #.*ascii-carriage-return*
                      #.*ascii-linefeed*
                      #.*ascii-carriage-return*
                      #.*ascii-linefeed*)
                  (socket/underlying-socket stream))
  ;; Force the output because we just sent a chunk.
  (force-output (socket/underlying-socket stream))
  (setf (socket/output-chunking? stream) nil
        (socket/input-chunking? stream) nil))

(defmethod close ((stream stream-socket) &key abort)
  (if abort
      (stream:stream-clear-output stream)
      (stream:stream-finish-output stream))
  (call-next-method))

(defclass client-tcp-socket (stream-socket) ())

(defmethod comm:socket-stream-socket ((socket client-tcp-socket))
  (comm:socket-stream-socket (socket/underlying-socket socket)))

(defclass server-connection-socket (internet-socket)
  ((local-port :initarg :local-port)))

(defclass server-tcp-socket (internet-socket) ())

(defun local-host (socket-stream)
  (multiple-value-bind (host port)
      (comm:socket-stream-address socket-stream)
    (declare (ignore port))
    host))

(defgeneric local-port (socket)
  (:method ((socket socket-stream))
     (multiple-value-bind (host port)
         (comm:socket-stream-address socket)
       (declare (ignore host))
       port))

  (:method ((socket internet-socket))
           (multiple-value-bind (host port)
               (comm:socket-stream-address (socket/underlying-socket socket))
             (declare (ignore host))
             port))

  (:method ((socket server-connection-socket))
     (slot-value socket 'local-port)))

(defun ensure-underlying-socket (server-connection-socket)
  (unless (slot-boundp server-connection-socket 'underlying-socket)
    (multiple-value-bind (underlying-socket error-location error-code)
        (comm::create-tcp-socket-for-service (local-port server-connection-socket))
      (if underlying-socket
          (setf (slot-value server-connection-socket 'underlying-socket) underlying-socket)
          (error "Socket creation failed: ~A (~a)" error-location error-code)))))

(defgeneric accept-connection (socket &key (wait t))
  (:method ((socket server-connection-socket) &key (wait t))
    (if (not (socket/open socket))
        (error 'changesafe-stream-closed-error
               :stream socket))

    (ensure-underlying-socket socket)

    (unless wait
      (cerror "Proceed and block anyway." ()
              "Nonblocking accept not implemented."))
    (make-instance
     'client-tcp-socket
     :underlying-socket (make-instance
                         'comm:socket-stream
                         :socket (comm::get-fd-from-socket
                                  (socket/underlying-socket socket))
                         :direction :io
                         :element-type 'unsigned-byte))))

(defun make-socket (&rest args
                          &key
                          (type :stream)
                          (format :text)
                          (connect :active)
                          (address-family :internet)
                          (eol :crlf)
                          &allow-other-keys)
  (check-type type           (member :stream :datagram))
  (check-type format         (member :text :binary :bivalent))
  (check-type address-family (member :internet :file))
  (check-type connect        (member :active :passive))
  (check-type eol            (member :lf :crlf))
  (apply
   (cond ((and (eql address-family :internet)
               (eql type           :stream))
          #'make-internet-stream-socket)
         ((and (eql address-family :internet)
               (eql type           :datagram))
          #'make-internet-datagram-socket args)
         ((and (eql address-family :file)
               (eql type           :stream))
          #'make-file-stream-socket)
         ((and (eql address-family :file)
               (eql type           :datagram))
          #'make-file-datagram-socket)
         (t (error 'changesafe-cond-failure)))
   args))

(defun canonicalize-port (port)
  (if (numberp port)
      port
      (error "Have to lookup port ~s." port)))

(defun make-internet-stream-socket (&key
                                    type
                                    format
                                    address-family
                                    eol
                                    local-port
                                    local-host
                                    remote-host
                                    remote-port
                                    (connect (cond ((and (or remote-host remote-port)
                                                         (null local-host)
                                                         (null local-port))
                                                    :active)
                                                   ((and (or local-host local-port)
                                                         (null remote-host)
                                                         (null remote-port))
                                                    :passive)
                                                   (t (error "Can't figure out what sort of socket."))))

                                    backlog
                                    reuse-address
                                    broadcast
                                    keepalive)
  (declare (ignore type format address-family eol))

  (check-type local-port  (optional (or string (unsigned-byte 16))))
  (check-type remote-port (optional (or string (unsigned-byte 16))))
  (check-type remote-host (optional (or string (unsigned-byte 32))))
  (check-type backlog     (optional number))
  (check-type reuse-address boolean)
  (check-type broadcast     boolean)
  (check-type keepalive     boolean)


  (when (eq connect :active)
    (assert (null backlog)))

  (when (eq connect :passive)
    (assert (null remote-host))
    (assert (null remote-port)))

  (assert (null broadcast))

  (if (eq connect :active)
      (make-instance 'client-tcp-socket
                     :underlying-socket (open-tcp-stream remote-host remote-port
                                                         :errorp t
                                                         :direction :io
                                                         :element-type 'unsigned-byte
                                                         :timeout 120))

      (make-instance 'server-connection-socket
                     :local-port (canonicalize-port local-port))))

(defunimplemented make-file-stream-socket (&key
                                           type
                                           format
                                           connect
                                           address-family
                                           eol
                                           local-filename
                                           remote-filename
                                           backlog))

(defunimplemented make-internet-datagram-socket (&key
                                           type
                                           format
                                           connect
                                           address-family
                                           eol
                                           local-port
                                           remote-host
                                           remote-port))

(defunimplemented make-file-datagram-socket (&key
                                             type
                                             format
                                             connect
                                             address-family
                                             eol
                                             local-filename
                                             remote-filename))

(defunimplemented make-ssl-server-stream ())
(defunimplemented make-ssl-client-stream ())

(defun socket-control (stream &key
                              (input-chunking nil ic-supplied-p)
                              (output-chunking nil oc-supplied-p)
                              output-chunking-eof)
  (when output-chunking-eof
    (close-chunk stream))
  (when oc-supplied-p
    (setf (socket/output-chunking? stream) output-chunking))
  (when ic-supplied-p
    (setf (socket/input-chunking? stream) input-chunking)))

(defunimplemented set-socket-options (socket &key nodelay))

(defmacro with-pending-connect (&body body)
  `(PROGN ,@body))

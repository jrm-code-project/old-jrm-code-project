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

;;; API to the Windows File system
(in-package "CSF/UTILITY")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '()))

(proclaim (standard-optimizations))

#-(or :microsoft-32 :win32)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This file may only be loaded or compiled under windows."))

;;; Use the foreign function interface to get at the
;;; Win32 natives that can read and set the file timestamp.

;;; These are the constants I need to get a file handle to modify the
;;; timestamp.
(progn
(defconstant *GENERIC_READ*  #x80000000)
(defconstant *GENERIC_WRITE* #x40000000)
(defconstant *OPEN_ALWAYS* 4)
(defconstant *FILE_ATTRIBUTE_READ_ONLY*  #x00000001)
(defconstant *FILE_ATTRIBUTE_HIDDEN*     #x00000002)
(defconstant *FILE_ATTRIBUTE_SYSTEM*     #x00000004)
(defconstant *FILE_ATTRIBUTE_DIRECTORY*  #x00000010)
(defconstant *FILE_ATTRIBUTE_ARCHIVE*    #x00000020)

(defconstant *FILE_FLAG_POSIX_SEMANTICS*  #x01000000)
(defconstant *FILE_FLAG_BACKUP_SEMANTICS* #x02000000)
(defconstant *FILE_FLAG_DELETE_ON_CLOSE*  #x04000000)
(defconstant *FILE_FLAG_SEQUENTIAL_SCAN*  #x08000000)
(defconstant *FILE_FLAG_RANDOM_ACCESS*    #x10000000)
(defconstant *FILE_FLAG_NO_BUFFERING*     #x20000000)
;;; 0x40000000  ???
(defconstant *FILE_FLAG_WRITE_THROUGH*    #x80000000)

(defconstant *FILE_SHARE_ANY*  #x00000007)
(defconstant *FILE_SHARE_NONE* #x00000000)
)

;;; A Win32 filetime structure.  Represents the number of
;;; 100ns intervals since Jan 1 1600 as an unsigned 64-bit
;;; number (split into 2 32 bit halves).
#+:allegro
(ff:def-foreign-type filetime
    (:struct (dwLowDateTime :unsigned-int)
             (dwHighDateTime :unsigned-int)))

#+:lispworks
(fli:define-c-struct filetime
    (dwLowDateTime  (:unsigned :int))
    (dwHighDateTime (:unsigned :int)))

#+:cormanlisp
(win32:defwinstruct win32::filetime
    ((win32::dwLowDateTime win32:DWORD)
     (win32::dwHighDateTime win32:DWORD)))

;;; WIN32_FIND_DATA
#+:allegro
(ff:def-foreign-type win32_find_data
    (:struct (dwFileAttributes :unsigned-int)
             (ftCreationTime   filetime)
             (ftLastAccessTime filetime)
             (ftLastWriteTime  filetime)
             (nFileSizeHigh    :unsigned-int)
             (nFileSizeLow     :unsigned-int)
             (dwReserved0      :unsigned-int)
             (dwReserved1      :unsigned-int)
             (cFileName            (:array :char 260))
             (cAlternativeFileName (:array :char 14))))

#+:lispworks
(fli:define-c-struct win32_find_data
    (dwFileAttributes (:unsigned :int))
    (ftCreationTime   filetime)
    (ftLastAccessTime filetime)
    (ftLastWriteTime  filetime)
    (nFileSizeHigh    (:unsigned :int))
    (nFileSizeLow     (:unsigned :int))
    (dwReserved0      (:unsigned :int))
    (dwReserved1      (:unsigned :int))
    (cFileName        (:foreign-array :short (260)))
    (cAlternativeFileName (:foreign-array :short (14))))

#+:cormanlisp
(win32:defwinstruct win32::WIN32_FIND_DATA
    ((win32::dwFileAttributes win32:DWORD)
     (win32::ftCreationTime win32:FILETIME)
     (win32::ftLastAccessTime win32:FILETIME)
     (win32::ftLastWriteTime win32:FILETIME)
     (win32::nFileSizeHigh win32:DWORD)
     (win32::nFileSizeLow win32:DWORD)
     (win32::dwReserved0 win32:DWORD)
     (win32::dwReserved1 win32:DWORD)
     (win32::cFileName (char 260))
     (win32::cAlternativeFileName (char 14))))

(defun win32-find-data/filename (find-data)
  #+cormanlisp (ct:c-string-to-lisp-string
                (ct:cref win32::WIN32_FIND_DATA info win32::cFileName))
  #+lispworks (fli:convert-from-foreign-string
               (fli:foreign-slot-pointer find-data 'cFileName
                                         :type (fli:foreign-slot-type find-data 'cFileName))
               :external-format :unicode))

(defun win32-find-data/attributes (find-data)
  #+cormanlisp (ct:cref win32::WIN32_FIND_DATA find-data win32::dwFileAttributes)
  #+lispworks (fli:foreign-slot-value find-data 'dwFileAttributes
                                      :type (fli:foreign-slot-type find-data 'dwFileAttributes)))

(defun win32-find-data/directory-p (find-data)
  (not (zerop (logand *FILE_ATTRIBUTE_DIRECTORY* (win32-find-data/attributes find-data)))))

(defun allocate-win32-find-data ()
  #+:cormanlisp (ct:malloc (ct:sizeof 'win32::WIN32_FIND_DATA))
  #+:lispworks  (fli:allocate-foreign-object :type '(:struct win32_find_data)))

(defun free-win32-find-data (data)
  #+:cormanlisp nil
  #+:lispworks (fli:free-foreign-object data))

(defun call-with-win32-find-data (receiver)
  (let ((data nil))
    (with-deferred-interrupts "Allocating or freeing win32 find data."
      (cl:unwind-protect
          (progn
            (setq data (allocate-win32-find-data))
            (with-deferred-interrupts-restored
                (funcall receiver data)))
        "freeing win32 find data"
        (when data
          (free-win32-find-data data))))))

;;; Close a Win32 handle.
;;; Necessary to release resources.
#+:allegro
(ff:def-foreign-call (close-handle "CloseHandle")
    ((hObject :int))
  :convention :stdcall
  :returning :int)

#+:lispworks
(fli:define-foreign-function (close-handle "CloseHandle")
   ((hObject :int))
   :calling-convention :stdcall
   :result-type :int)

#+:lispworks
(fli:define-foreign-function (copy-file "CopyFile" :dbcs)
  ((lpSourceFile :pointer)
   (lpDestFile   :pointer)
   (bForce       :boolean))
  :calling-convention :stdcall
  :result-type :int)

;;; Given a filename string, open a handle to the file.
#+:allegro
(ff:def-foreign-call (create-file
                excl::ics-mode-convert "CreateFile")
    ((lpFileName (* :char))
     (dwDesiredAccess :int)
     (dwShareMode :int)
     (lpSecurityAttributes (* :void))
     (dwCreationDisposition :int)
     (dwFlagsAndAttributes :int)
     (hTemplateFile :int))
  :convention :stdcall
  #+(and :allegro-version>= (:version>= 6 0)) :strings-convert
  #+(and :allegro-version>= (:version>= 6 0)) nil
  :returning :int)

#+:lispworks
(fli:define-foreign-function (create-file "CreateFile" :dbcs)
   ((lpFileName :pointer)
    (dwDesiredAccess (:unsigned :int))
    (dwShareMode (:unsigned :int))
    (lpSecurityAttributes (:pointer :void))
    (dwCreationDisposition (:unsigned :int))
    (dwFlagsAndAttributes (:unsigned :int))
    (hTemplateFile (:pointer :void)))
   :calling-convention :stdcall
   :result-type :int)

#+:allegro
(ff:def-foreign-call (find-close "FindClose")
    ((hObject :int))
  :convention :stdcall
  :returning :int)

#+:lispworks
(fli:define-foreign-function (find-close "FindClose")
   ((hObject :int))
   :calling-convention :stdcall
   :result-type :int)

#+:allegro
(ff:def-foreign-call (%win32-find-first-file excl::ics-mode-convert "FindFirstFile")
   ((lpFileName (* :char))
    (lpFindFileData (* :void)))
   :convention :stdcall
  #+(and :allegro-version>= (:version>= 6 0)) :strings-convert
  #+(and :allegro-version>= (:version>= 6 0)) nil
  :returning :int)

#+:lispworks
(fli:define-foreign-function (%win32-find-first-file "FindFirstFile" :dbcs)
   ((lpFileName :pointer)
    (lpFindFileData (:pointer (:struct win32_find_data))))
   :calling-convention :stdcall
   :result-type :int)

#+:cormanlisp
(win32:defwinapi win32::%findfirstfile ((win32::filename win32::LPSTR)
                                        (win32::lpFindFileData (win32::void *)))
  :return-type win32::HANDLE
  :library-name "kernel32.dll"
  :entry-name "FindFirstFileA"
  :linkage-type :pascal)

(defun win32-find-first-file (filename info)
  (check-type filename string)
  #+:cormanlisp (win32::%findfirstfile (ct:lisp-string-to-c-string filename) info)
  #+:lispworks  (fli:with-foreign-string (fstring element-count byte-count
                                                  :external-format :unicode)
                                         filename
                      (declare (ignore element-count byte-count))
                      (let ((handle (%win32-find-first-file fstring info)))
                        (unless (= handle -1)
                          handle))))
#+:allegro
(ff:def-foreign-call (%win32-find-next-file excl::ics-mode-convert "FindNextFile")
   ((lpFindFileData (* :void)))
   :convention :stdcall
  #+(and :allegro-version>= (:version>= 6 0)) :strings-convert
  #+(and :allegro-version>= (:version>= 6 0)) nil
  :returning :int)

#+:lispworks
(fli:define-foreign-function (%win32-find-next-file "FindNextFile" :dbcs)
   ((handle :int)
    (lpFindFileData (:pointer (:struct win32_find_data))))
   :calling-convention :stdcall
   :result-type :int)

#+:cormanlisp
(win32:defwinapi win32::%findnextfile ((win32::lpFindFileData (win32::void *)))
  :return-type win32::HANDLE
  :library-name "kernel32.dll"
  :entry-name "FindNextFileA"
  :linkage-type :pascal)

(defun win32-find-next-file (handle info)
  #+:cormanlisp (win32::%findnextfile handle info)
  #+:lispworks (not (zerop (%win32-find-next-file handle info))))

(defun win32-scan-directory (namestring)
  (declare (optimizable-series-function))
  (encapsulated
   (lambda (body)
     `(common-lisp:let (handle
                        find-info)
        (cl:unwind-protect
         (progn ,body)
         (when handle
           (find-close handle))
         (when find-info
           (free-win32-find-data find-info)))))
   (scan-fn 't
            (lambda ()
              (setq find-info (allocate-win32-find-data)))
            #'identity
            (lambda (find-info)
              (not (if handle
                       (win32-find-next-file handle find-info)
                       (setq handle (win32-find-first-file namestring find-info))))))))

(defun test-scan-directory (namestring)
  (iterate ((name (#m win32-find-data/filename (win32-scan-directory namestring))))
    (format t "~&~s" name)))

#+:allegro
(ff:def-foreign-call (get-file-attributes
                      excl::ics-mode-convert "GetFileAttributes")
    ((lpFileName (* :char)))
  :convention :stdcall
  #+(and :allegro-version>= (:version>= 6 0)) :strings-convert
  #+(and :allegro-version>= (:version>= 6 0)) nil
  :returning :int)

#+:lispworks
(fli:define-foreign-function (%get-file-attributes "GetFileAttributes" :dbcs)
  ((lpFileName (:pointer (:unsigned :short))))
  :calling-convention :stdcall
  :result-type (:unsigned :int))

#+:lispworks
(defun get-file-attributes (filename)
  (fli:with-foreign-string (fstring element-count byte-count
                                    :external-format :unicode)
                           filename
    (declare (ignore element-count byte-count))
    (%get-file-attributes fstring)))

;;; Fills in 3 filetime structures with the timestamp associated
;;; with a file handle.
#+:allegro
(ff:def-foreign-call (win32-get-file-time "GetFileTime")
    ((hFile :int)
     (lpCreationTime filetime)
     (lpLastAccessTime filetime)
     (lpLastWriteTime  filetime))
  :convention :stdcall
  :returning :int)

#+:lispworks
(fli:define-foreign-function (win32-get-file-time "GetFileTime")
    ((hFile :int)
     (lpCreationTime   (:pointer filetime))
     (lpLastAccessTime (:pointer filetime))
     (lpLastWriteTime  (:pointer filetime)))
    :calling-convention :stdcall
    :result-type :int)

;; Returns last error code.
#+:allegro
(ff:def-foreign-call (get-last-error "GetLastError")
    ()
  #+(and :allegro-version>= (:version>= 6 0)) :strings-convert
  #+(and :allegro-version>= (:version>= 6 0)) nil
  :convention :stdcall
  :returning :int)

#+:lispworks
(fli:define-foreign-function (get-last-error "GetLastError")
   ()
   :calling-convention :stdcall
   :result-type :int)

#+lispworks
(fli:define-foreign-function (win32-get-system-time "GetSystemTimeAsFileTime")
    ((lpFileTime       (:pointer filetime)))
    :calling-convention :stdcall
    :result-type :int)

#+:allegro
(ff:def-foreign-call (set-file-attributes
                      excl::ics-mode-convert "SetFileAttributes")
    ((lpFileName (* :char))
     (attributes :int))
  :convention :stdcall
  #+(and :allegro-version>= (:version>= 6 0)) :strings-convert
  #+(and :allegro-version>= (:version>= 6 0)) nil
  :returning :int)

#+:lispworks
(fli:define-foreign-function (%set-file-attributes "SetFileAttributes" :dbcs)
  ((lpFileName (:pointer (:unsigned :short)))
   (attributes (:unsigned :int)))
  :calling-convention :stdcall
  :result-type (:unsigned :int))

#+:lispworks
(defun win32-file-read-only? (filename)
  (check-type filename string)
  (fli:with-foreign-string (fstring element-count byte-count
                                    :external-format :unicode)
                           filename
    (declare (ignore element-count byte-count))
    (not (zerop (logand (%get-file-attributes fstring) *FILE_ATTRIBUTE_READ_ONLY*)))))

#+:lispworks
(defun set-file-attributes (filename attributes)
  (check-type filename string)
  (fli:with-foreign-string (fstring element-count byte-count
                                    :external-format :unicode)
                           filename
    (declare (ignore element-count byte-count))
    (%set-file-attributes fstring attributes)))

(defun win32-set-file-read-only (filename read-only)
  (check-type filename string)
  #+allegro (excl:with-native-string (native-filename filename)
              (let ((attributes (get-file-attributes native-filename)))
                (set-file-attributes native-filename
                                     (if read-only
                                         (logior attributes *FILE_ATTRIBUTE_READ_ONLY*)
                                         (logand attributes (lognot *FILE_ATTRIBUTE_READ_ONLY*))))))
  #+lispworks (fli:with-foreign-string (fstring element-count byte-count
                                                :external-format :unicode)
                                       filename
                 (declare (ignore element-count byte-count))
                 (let ((attributes (%get-file-attributes fstring)))
                   (%set-file-attributes fstring
                                         (if read-only
                                             (logior attributes *FILE_ATTRIBUTE_READ_ONLY*)
                                             (logand attributes (lognot *FILE_ATTRIBUTE_READ_ONLY*)))))))

;;; Writes the timestamp associated with a file handle with
;;; the 3 filetime structures.
#+:allegro
(ff:def-foreign-call (set-native-file-time "SetFileTime")
  ((hFile :int)
   (lpCreationTime filetime)
   (lpLastAccessTime filetime)
   (lpLastWriteTime  filetime))
:convention :stdcall
:returning :int)

#+:lispworks
(fli:define-foreign-function (win32-set-file-time "SetFileTime")
  ((hFile :int)
   (lpCreationTime (:pointer filetime))
   (lpLastAccessTime (:pointer filetime))
   (lpLastWriteTime  (:pointer filetime)))
  :calling-convention :stdcall
  :result-type :int)

;;;;;;;;;;;;;;;;;
;;; Now some lisp code to interface to the above.

#+:lispworks
(defun win32-copy-file (source-file dest-file force?)
  (check-type source-file string)
  (check-type dest-file string)
  (fli:with-foreign-string (native-source-filename
                            source-element-count
                            source-byte-count
                            :external-format :unicode)
                           (if (pathnamep source-file)
                               (namestring source-file)
                               source-file)
    (declare (ignore source-element-count source-byte-count))
    (fli:with-foreign-string (native-dest-filename
                              dest-element-count
                              dest-byte-count
                              :external-format :unicode)
                             (if (pathnamep dest-file)
                                 (namestring dest-file)
                                 dest-file)
      (declare (ignore dest-element-count dest-byte-count))
      (copy-file native-source-filename native-dest-filename force?))))

;;; Lisp interface that copies filename to temporary C storage
;;; for use in call to create-file.

;;; This call to create file has the parameters hard-wired in so that
;;; it is only useful for getting and setting the timestamp.
;;; Note that we must specify *file_share_any* so that we can read
;;; the timestamps on open files.  We also use *file_flag_backup_semantics*.
#+:allegro
(defun win32-create-file (filename read-only?)
  (check-type filename string)
  (excl:with-native-string (native-filename filename)
    (create-file native-filename
           (if read-only? *generic_read* *generic_write*)
           *file_share_any*
           0
           *open_always*
           *file_flag_backup_semantics*
           0)))

#+:lispworks
(defun win32-create-file (filename read-only?)
  (check-type filename string)
  (fli:with-foreign-string (native-filename element-count byte-count
                                            :external-format :unicode)
                           filename
    (declare (ignore element-count byte-count))
    (create-file native-filename
                 (if read-only? *generic_read* *generic_write*)
                 *file_share_any*
                 nil
                 *open_always*
                 *file_flag_backup_semantics*
                 nil)))

;;; Open filename, invoke thunk on handle, close handle when done.
(defun call-with-win32-file-handle (filename read-only? receiver)
  (check-type filename string)
  (let ((handle nil))
    (with-deferred-interrupts "Getting or closing native file handle."
      (cl:unwind-protect
          (progn
            (setq handle (win32-create-file filename read-only?))
            (with-deferred-interrupts-restored
                (funcall receiver handle)))
        "closing native file handle"
        (when handle
          (close-handle handle))))))

(defun win32-touch-file (filename)
  (call-with-win32-file-handle filename nil
    #'identity))

;;; Convert lisp timebase to win32 timebase.
(defsubst lisp-time->win32-time (lisp-time)
  (* (+ lisp-time 9435484800) 10000000))

;;; Convert win32 timebase to lisp timebase.
(defsubst win32-time->lisp-time (win32-time)
  ;; Doing it this way gives us ticks as the second
  ;; return value.
  (floor (- win32-time 94354848000000000) 10000000))

(defun win32-get-timestamp (pathname)
  (call-with-win32-file-handle pathname t ;; read only
    (lambda (handle)
        #+lispworks
        (fli:with-dynamic-foreign-objects ((creation-time  filetime)
                                           (access-time    filetime)
                                           (write-time     filetime))

          ;; Get the time.
          (let ((code (win32-get-file-time handle creation-time access-time write-time)))
            (if (zerop code)
                (error "Get win32 file time failed with code ~d" (get-last-error))
                (win32-time->lisp-time
                 (+ (* (fli:foreign-slot-value write-time 'dwHighDateTime) (expt 2 32))
                    (* (fli:foreign-slot-value write-time 'dwLowDateTime)  (expt 2 0))))))))))

(defun win32-system-time ()
  #+lispworks
  (fli:with-dynamic-foreign-objects ((the-time filetime))
    (let ((code (win32-get-system-time the-time)))
      (if (zerop code)
          (error "Get win32 system time failed with code ~d" (get-last-error))
          (win32-time->lisp-time
           (+ (* (fli:foreign-slot-value the-time 'dwHighDateTime) (expt 2 32))
              (* (fli:foreign-slot-value the-time 'dwLowDateTime)  (expt 2 0))))))))

(defun win32-set-timestamp (pathname timestamp)
  (call-with-win32-file-handle pathname nil
      (lambda (handle)
        ;; Allocate 3 timestamps on the stack.
        #+allegro
        (ff:with-stack-fobject (creation-time 'filetime)
          (ff:with-stack-fobject (access-time 'filetime)
            (ff:with-stack-fobject (write-time 'filetime)
              ;; Get the time.
              (let ((code (get-native-file-time handle creation-time access-time write-time)))
                (if (zerop code)
                    (error "Get native file time failed with code ~d"
                           (get-last-error))
                    ;; Allocate new write time on the stack.
                    (ff:with-stack-fobject (new-last-write-time 'filetime)
                      (let ((nt-time (nt-time-from-lisp-time timestamp)))
                        ;; Fill in the values
                        (setf (ff:fslot-value-typed 'filetime nil new-last-write-time 'dwLowDateTime)
                              (mod nt-time (expt 2. 32.)))
                        (setf (ff:fslot-value-typed 'filetime nil new-last-write-time 'dwHighDateTime)
                              (floor nt-time (expt 2. 32.)))
                        ;; Write the timestamp.
                        (set-native-file-time handle creation-time access-time new-last-write-time))))))))
        #+lispworks
        (fli:with-dynamic-foreign-objects ((creation-time  filetime)
                                           (access-time    filetime)
                                           (write-time     filetime)
                                           (new-write-time filetime))
          ;; Get the time.
          (let ((code (win32-get-file-time handle creation-time access-time write-time)))
            (if (zerop code)
                (error "Get win32 file time failed with code ~d" (get-last-error))
                (let ((win32-time (lisp-time->win32-time timestamp)))

                  (setf (fli:foreign-slot-value new-write-time 'dwLowDateTime)
                        (mod win32-time (expt 2 32)))
                  (setf (fli:foreign-slot-value new-write-time 'dwHighDateTime)
                        (floor win32-time (expt 2 32)))
                  (win32-set-file-time handle creation-time access-time new-write-time))))))))

;;; This version probes the file and returns the pathname in the canonical case.

(defun win32-probe-file (namestring)
  "Returns 4 values:
    1.  The drive
    2.  The directory
    3.  The file name
    4.  The file type"
  (check-type namestring string)
  (multiple-value-bind (success drive directory name type stream)
      (parse-dos-namestring namestring)
    (declare (ignore stream))
    (when success
;      (unless drive
;        (error "Namestring does not contain a drive."))
      (unless (and directory
                   (eq (car directory) :absolute))
        (error "Namestring does not name an absolute pathname."))
      (let* ((drive (and drive (concatenate 'string
                                            (string-upcase (string drive))
                                            ":")))
             (namestring (if drive drive ""))
             (canonical-directory (list :absolute))
             (canonical-name nil)
             (canonical-type nil))
        (debug-message 5 "Probing ~s ~s ~s ~s" drive directory name type)
        (call-with-win32-find-data
         (lambda (find-data)
           (dolist (component (if (or name type)
                                  (if (or (eq name :wild)
                                          (eq type :wild))
                                      (error "Illegal component in win32-probe-file.")
                                      (append (cdr directory)
                                              (list (format nil "~{~A~}~{.~A~}"
                                                            (and name (list name))
                                                            (and type (list type))))))
                                  (cdr directory))
                    (values (if drive (char drive 0))
                            (nreverse canonical-directory)
                            canonical-name
                            canonical-type))
             (when (or (eq component :wild)
                       (eq component :wild-inferiors)
                       (eq component :back)
                       (eq component :up))
               (error "Illegal component ~s in win32-probe-file" component))

             ;; this call fills in the file-info
             (let ((handle nil))
               (unwind-protect (setq handle (win32-find-first-file
                                             (concatenate 'string namestring "\\" component)
                                             find-data))
                 (if (null handle)
                     (return nil)
                     (find-close handle))))

             (let ((canonical-component (win32-find-data/filename find-data)))
               (if (win32-find-data/directory-p find-data)
                   (setq namestring (concatenate 'string
                                                 namestring "\\" canonical-component)
                         canonical-directory (cons canonical-component canonical-directory))
                   (let ((dot (position #\. canonical-component :from-end t)))
                     (setq namestring nil
                           canonical-name (if (and dot (> dot 0))
                                              (subseq canonical-component 0 dot)
                                              canonical-component)
                           canonical-type (and dot
                                               (> dot 0)
                                               (subseq canonical-component (1+ dot))))))))))))))

(defun win32-directory (namestring)
  (check-type namestring string)
  (multiple-value-bind (success drive directory name type stream)
      (parse-dos-namestring namestring)
    (declare (ignore stream))
    (when success
      (unless (and directory
                   (eq (car directory) :absolute))
        (error "Namestring does not name an absolute pathname."))
      (named-let loup ((prefix (and drive (concatenate 'string
                                                       (string-upcase (string drive))
                                                       ":\\")))
                       (elements (cdr directory)))
                 (if (and (null elements)
                          (null name)
                          (null type))
                     (list prefix)
                     ;; scan the current prefix
                     (collect-append
                      'list
                      (mapping (((namestring directory-p)
                                 (map-fn '(values string boolean)
                                         (lambda (info)
                                           (values (win32-find-data/filename info)
                                                   (win32-find-data/directory-p info)))
                                         (win32-scan-directory (concatenate 'string prefix "*")))))
                               (debug-message 5 "namestring: ~s" namestring)
                               (debug-message 5 "directory-p: ~s" directory-p)
                               ;; skip the backlinks
                               (unless (or (equal namestring ".")
                                           (equal namestring ".."))
                                 (if directory-p
                                     (if elements
                                         (let ((dirname (concatenate 'string prefix namestring "\\")))

                                           (append (when (or (eq (car elements) :wild)
                                                             (eq (car elements) :wild-inferiors)
                                                             (equalp (car elements) namestring))
                                                     (loup dirname
                                                           (cdr elements)))
                                                   (when (eq (car elements) :wild-inferiors)
                                                     (loup dirname
                                                           elements))))
                                         (when (or (and (eq name :wild) (or (eq type :wild)
                                                                            (equalp type "")))
                                                   (and (equalp name namestring) (eq type :wild)))
                                           (list (concatenate 'string prefix namestring "\\"))))

                                     ;; match the files
                                     (when (null elements)
                                       (multiple-value-bind (thisname thistype)
                                           (parse-filename.type namestring)
                                         (debug-message 5 "thisname, thistype = ~s ~s" thisname thistype)
                                         (debug-message 5 "name, match = ~s ~s" name (or (eq name :wild)
                                                                                         (equalp thisname name)))
                                         (debug-message 5 "type, match = ~s ~s" type (or (eq name :wild)
                                                                                         (equalp thistype type)))
                                         (when (and (or (eq name :wild)
                                                        (equalp thisname name))
                                                    (or (eq type :wild)
                                                        (equalp thistype type)))
                                           (list (concatenate 'string prefix thisname
                                                              (if thistype "." "") thistype))))))))))))))

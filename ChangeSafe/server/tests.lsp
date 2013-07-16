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
;;;; File Name:     tests.lsp
;;;; Author:        Dave Tenny
;;;;
;;;; Module Description: Regression tests for the SERVER package.
;;;; These tests utilize the CORE test harness, which looks for tests by
;;;; name, in grouped series:  TEST-1A, TEST-1B, TEST-2A, etc..
;;;;
;;;; To run these tests, do the following:
;;;; (1) CD to the SERVER directory
;;;;     (very important, or you'll have the wrong master files).
;;;; (2) Set *package* to the SERVER package, e.g. (in-package :server)
;;;; (3) Invoke (TEST-ALL) with appropriate args.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CSF/SERVER")

(proclaim (standard-optimizations))

(eval-when (:load-toplevel :compile-toplevel :execute) ; execute needed for loading .lsp file fresh
  (import '(
	    ;; core:with-test-function-environment
	    *within-regression-tests*
	    ;map-over-tests
	    ;test-output-file
	    ;test-input-file
	    ;ensure-test-input-file
	    generate-test-input-files
	    ;test-repository-file
	    ;run-test
	    ;test-all
	    ))
  )

;;; This file defines a regression test suite
(eval-when (:load-toplevel :execute)
  (make-instance 'regression-test-suite
    :name "SERVER"
    :directory (translate-logical-pathname "CSF:SERVER;")))

(define-test-input-file (make-pathname :name "__test-w-nl-cr" :type "txt")
  :ascii '(#\a :cr #\b :cr #\c :cr))
(define-test-input-file (make-pathname :name "__test-w-nl-crlf" :type "txt")
  :ascii '(#\a :crlf #\b :crlf #\c :crlf))
(define-test-input-file (make-pathname :name "__test-w-nl-lf" :type "txt")
  :ascii '(#\a :lf #\b :lf #\c :lf))
(define-test-input-file (make-pathname :name "__test-wo-nl-cr" :type "txt")
  :ascii '(#\a :cr #\b :cr #\c))
(define-test-input-file (make-pathname :name "__test-wo-nl-crlf" :type "txt")
  :ascii '(#\a :crlf #\b :crlf #\c))
(define-test-input-file (make-pathname :name "__test-wo-nl-lf" :type "txt")
  :ascii'(#\a :lf #\b :lf #\c))
(define-test-input-file (make-pathname :name "__test1-w-nl-cr" :type "txt")
  :ascii '(#\a :cr))
(define-test-input-file (make-pathname :name "__test1-w-nl-crlf" :type "txt")
  :ascii '(#\a :crlf))
(define-test-input-file (make-pathname :name "__test1-w-nl-lf" :type "txt")
  :ascii '(#\a :lf))
(define-test-input-file (make-pathname :name "__test1-wo-nl"  :type "txt")
  :ascii '(#\a))
(define-test-input-file (make-pathname :name "__zero" :type "txt") :ascii nil  #| empty file |#)
(define-test-input-file (make-pathname :name "__test-mixed" :type "txt")
  :ascii
    '("This file is terminated with CRLFs, but it has" :crlf
      "a few embedded newlines and carriage returns in it." :crlf
      "For instance, this is an embedded newline:" :lf
      "(which has some text after it)," :crlf
      "and this is an embedded carriage return:" :cr
      "(with some text after it)." :crlf
      "Enjoy." :crlf))

#||
;;; Other test input files whose descriptions would be too long.
;;; This was the old way that test input files were defined.

(defparameter +server-test-input-file-descriptions+
    '(
      fs-input-1.txt
      FSTest.class
      longline
      manyline
      reading-socket.otp
      scm-server.lsp
      test-8c.mst
      tests.fasl
      ))
||#


#|
(generate-test-input-files :server)
|#

(regression-test-suite/add-initialization "SERVER" 'generate-test-input-files)

(defun compile-java-stuff-for-server-tests (test-suite)
  (declare (ignore test-suite))
  (utility::java-product-update "FSTest"))

(regression-test-suite/add-initialization "SERVER" 'compile-java-stuff-for-server-tests)

;;; **** NOTE **** TEST-1A java/http-server logic is copied in an SCM test package, chance are
;;; if you need to change logic here pertaining to the running of the FSA from lisp, and the http
;;; server which listens on a socket for the FSA, that you'll need to change it in the
;;; SCM tests.lsp module.

(defun simple-byte-diff (original-file new-file)
  "Simple byte comparator for files, until we have full binary diff support."
  (with-open-file (old-stream original-file :direction :input
		   #-(and :allegro-version>= (:version>= 6 0)) :element-type
		   #-(and :allegro-version>= (:version>= 6 0)) 'unsigned-byte)
    (with-open-file (new-stream new-file :direction :input
		     #-(and :allegro-version>= (:version>= 6 0)) :element-type
		     #-(and :allegro-version>= (:version>= 6 0)) 'unsigned-byte)
      (loop for position from 0
	  as old-byte = (read-byte old-stream nil nil)
	  as new-byte = (read-byte new-stream nil nil)
	  do (unless (equalp old-byte new-byte)
	       (error "Byte mismatch at position ~d, old file: ~s, new file: ~s~%Old byte: ~s, new byte: ~s"
		      position original-file new-file old-byte new-byte))
	     (when (or (null old-byte)
		       (null new-byte))
	       (return))))))		;break the loop

(defparameter +relative-test-input-dir+
    (merge-pathnames
     (make-pathname :directory '(:relative "server"))
     (translate-logical-pathname "CSF:TEST-DATA;"))
  "Inappropriately named, but correct.  Location of the input data for the server tests.")

(defparameter +test-input-file-special-cases+
    ;; list of (pathname record-separator)
    '((#p"__test1-wo-nl.txt" :none)
      (#p"__zero.txt" :none)
      ))

(defun test-input-special-case-lookup (pathname)
  (assoc pathname
	 +test-input-file-special-cases+
	 :test (lambda (pn1 pn2)
		   (string-equal (pathname-name pn1)
				 (pathname-name pn2)))))

(defun test-file-system (file-system raw-file-system &key remote-agent)
  "Test the indicated file-system

   If REMOTE-AGENT is true, we perform a less exacting test right now than we do if it is running
   on the same machine and native file system.  Useful for debugging new FSA's, even though it
   isn't as complete a test.  We could upgrade the test to be fully exacting, but to do so
   requires that things like 'simple-byte-diff' use the remote fS to test the remote-fs,
   and we haven't implemented that yet.  It also has potential chicken and egg problems."

  (mapc #'ensure-test-input-file
	'("__test-w-nl-cr.txt"
	  "__test-w-nl-crlf.txt"
	  "__test-w-nl-lf.txt"
	  "__test-wo-nl-cr.txt"
	  "__test-wo-nl-crlf.txt"
	  "__test-wo-nl-lf.txt"
	  "__test1-w-nl-cr.txt"
	  "__test1-w-nl-crlf.txt"
	  "__test1-w-nl-lf.txt"
	  "__test1-wo-nl.txt"
	  "__zero.txt"
	  "__test-mixed.txt"))


  (format t "~%Progress noted? ~s" (file-system/note-progress file-system "Let the games begin!" 50))
  (format t "~%Progress noted? ~s" (file-system/note-progress file-system "Let the games begin!" nil))
  (format t "~%Progress noted? ~s" (file-system/note-progress file-system nil 75))
  (format t "~%Progress noted? ~s" (file-system/note-progress file-system nil nil))

  ;; If these exist, we abnormally terminated the test in a prior run:
  (iterate ((dir #z(#p"REPOSITORY:;UNDR-UNDR-FSTESTDIR2;" #p"REPOSITORY:;UNDR-UNDR-FSTESTDIR3;")))
    (when (file-system/directory-exists-p file-system dir)
      (warn "~s exists from abnormal completion prior test run, deleting it, regression ouput invalid"
            dir)
      (file-system/delete-directory file-system dir :recursive t)))

  (file-system/create-directory file-system "REPOSITORY:;UNDR-UNDR-TEST-INPUT;")

  ;; Copy the input files to the tempdir.
  (mapc (lambda (fd)
	  (unless (file-descriptor/is-directory? fd)
	    (let ((destination  (merge-pathnames
				 (make-pathname :directory '(:relative "__test-input")
						:name (pathname-name (file-descriptor/path fd))
						:type (pathname-type (file-descriptor/path fd))
						:defaults "")
				 (logical-file-system/root file-system))))

	      (file-system/copy-file raw-file-system fd destination)
	      (let ((dest-fd (file-system/probe-file raw-file-system destination)))
		(if (file-descriptor/read-only? dest-fd)
		    (progn (file-system/set-read-only raw-file-system dest-fd nil)
			   (file-system/touch-file raw-file-system dest-fd (get-universal-time) :set-read-only t))
		  (file-system/touch-file raw-file-system dest-fd (get-universal-time) :set-read-only nil))))))
	(append
	 (file-system/probe-directory raw-file-system
                                      (translate-logical-pathname "CSF:TEST-DATA;generated;server;"))
	 (file-system/probe-directory raw-file-system
                                       (translate-logical-pathname "CSF:TEST-DATA;server;"))))

  (flet ((fd-print (fd)			;print a file descriptor
	   (let ((exceptional (test-input-special-case-lookup (file-descriptor/path fd))))
	     (format t "~%Path: ~s, size: ~s, sep: ~s, dir? => ~s, read-only? => ~s" ; "~%~7tModification date: ~a"
		     (file-descriptor/path fd)
		     (if (file-descriptor/is-directory? fd) 0 (file-descriptor/size fd))
		     (unless (eq (second exceptional) :none)
		       (if (file-descriptor/is-directory? fd) :none (file-descriptor/record-separator fd)))
		     (file-descriptor/is-directory? fd)
		     ;; Lose this because CVS doesn't preserve timestamps
		     ;;(time-string (file-descriptor/modification-date fd))
		     (file-descriptor/read-only? fd)
		     )))
	 (sort-file-descriptors (file-descriptors)
	   (sort (copy-list file-descriptors) #'string-lessp
		 :key (lambda (fd)
			  (pathname-name (file-descriptor/path fd)))))
	 )

    ;; Binary file read test
    ;; TO-DO: add a check to verify that some of the bytes are correct in the read test!
    (format t "~%Binary file read test")
    (loop for path in '(#p"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-FS-INPUT-1.TXT" ;ascii
			#p"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-QQU-FST-EST.CLASS" ;binary
			#p"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-TESTS.FASL") ;binary
	  for f-type in '(:text :binary :binary)
	do
	  (let* ((nbytes 0)
		(fd (file-system/probe-file file-system path))
		(known-crc (file-system/crc-file-background
		     file-system fd f-type
		     (lambda (get-crc)
			 (funcall get-crc))))
		 (auto-detect-crc (file-system/crc-file-background
		     file-system fd nil
		     (lambda (get-crc)
			 (funcall get-crc)))))
	    ;; Note: the values from the two CRC's should match
	    (format t "~%File: is ~a~%Size: ~d~%Known type  CRC: ~a~%Auto-detect CRC: ~a~%CRCs match? ~a"
		    path
		    (file-descriptor/size fd)
		    known-crc auto-detect-crc
		    (= known-crc auto-detect-crc))
	    (with-file-system-stream (stream file-system path
					     :direction :input
					     #-(and :allegro-version>= (:version>= 6 0)) :element-type
					     #-(and :allegro-version>= (:version>= 6 0)) 'unsigned-byte)
	      (with-open-file (out-stream
			       (merge-pathnames
				(make-pathname :directory '(:relative "__test-input")
					       :name "test-1a"
					       :type "tmp"
					       :defaults "")
				(logical-file-system/root file-system))
			       :direction :output :if-exists :supersede
			       #-(and :allegro-version>= (:version>= 6 0)) :element-type
			       #-(and :allegro-version>= (:version>= 6 0)) 'unsigned-byte)
		(let ((sequence nil))
		  (loop
		    (multiple-value-bind (return-sequence length)
			(file-system/read-bytes file-system stream sequence)
		      (unless sequence
			(setq sequence return-sequence))
		      (when (zerop length)
			(return))
		      (debug-message 5 "Writing ~s" (csf/utility::bytes-to-string sequence))
		      (write-sequence sequence out-stream :end length)
		      (incf nbytes length)))))
	      (format t "~%Bytes in ~s: ~d" path nbytes)
	      (assert (= nbytes (file-descriptor/size fd)))))
	  (simple-byte-diff (resolve-relative-path path file-system)
			    (merge-pathnames
			     (make-pathname :directory '(:relative "__test-input")
					    :name "test-1a"
					    :type "tmp"
					    :defaults "")
			     (logical-file-system/root file-system))))




    ;; Binary file write test
    (format t "~%Binary file write test")
    (loop for path in (flet ((relative-test-input-file (f)
			       (merge-pathnames f +relative-test-input-dir+)))
			(list (relative-test-input-file "__fs-input-1.txt") ;ascii
			      (relative-test-input-file "__FSTest.class") ;binary
			      (relative-test-input-file "__tests.fasl"))) ;binary
	with buffer = (make-array 4096 :element-type '(unsigned-byte 8))
	do
	  (with-open-file (in-stream path :direction :input
			   #-(and :allegro-version>= (:version>= 6 0)) :element-type
			   #-(and :allegro-version>= (:version>= 6 0)) 'unsigned-byte)
	    (with-file-system-stream (out-stream file-system #p"REPOSITORY:;UNDR-UNDR-TEST-1A.TMP"
				      :if-exists :supersede
				      :direction :output
				      #-(and :allegro-version>= (:version>= 6 0)) :element-type
				      #-(and :allegro-version>= (:version>= 6 0)) 'unsigned-byte)
	      (loop as n-bytes = (read-sequence buffer in-stream)
		  while (plusp n-bytes)
		  do (file-system/write-bytes file-system out-stream buffer :end n-bytes))))
	  ;; Now compare the resulting file with the original
	  ;; Later, when we have byte-compare algorith, use that.  (TO-DO)
	  (unless remote-agent
	    (simple-byte-diff path
			      (resolve-relative-path #p"REPOSITORY:;UNDR-UNDR-TEST-1A.TMP" file-system))))

    ;; File content type detection test
    (format t "~%File content type detection test")
    (loop for path in (cons #p"REPOSITORY:;TEST-INPUT;UNDR-UNDR-I-DO-NOT-EXIST"
			    ;; ***WARNING*** Not compatible with remote agent, presents absolute
			    ;; server-relative pathnames which won't be found in the DTCT calls.
			    (sort (mapcar (lambda (path)
					      (relativize-path path file-system))
					  (remove-if #'directory-pathname?
						     (directory
						      (merge-pathnames
						       (make-pathname :directory '(:relative "__test-input")
								      :defaults "")
						       (logical-file-system/root file-system)))))
				  #'string-lessp
				  :key #'pathname-name))
	do (format t "~%~20a, file content type is ~s"
		   (pathname-name path)
		   (file-system/detect-file-content-type file-system path)))

    ;; File write test
    (format t "~%File write test")
    (with-file-system-stream (stream-handle file-system #p"REPOSITORY:;UNDR-UNDR-TEST-1A.TMP"
			      :direction :output
			      :if-exists :supersede
			      :element-type 'character
			      :record-separator (file-system/record-separator file-system))
      (loop for x from 0 to 255
	  as char = (code-char x)
	  do
	    (file-system/write-line
	     file-system stream-handle
	     (format nil "~3d is a ~a" x
		     (cond ((alpha-char-p char)
			    "alphabetic character")
			   ((digit-char-p char)
			    "digit")
			   ((graphic-char-p char)
			    "graphic character")
			   ((standard-char-p char)
			    "standard character")
			   (t "unknown character type")))))

      ;; A HUGE line.
      (file-system/write-line file-system stream-handle (make-string 10000 :initial-element #\space))

      ;; And a *lot* of lines.
      (dotimes (i 500)
	(mapc (lambda (line)
		  (file-system/write-line file-system stream-handle line))
	      `(
		"I will not waste chalk."
		"Beans are neither fruit nor musical."
		"I will not expose the ignorance of the faculty."
		"I will not conduct my own fire drills."
		"Funny noises are not funny."
		"I will not snap bras."
		"This punishment is not boring and pointless."
		"I will not prescribe medication."
		"A burp is not an answer."
		"Teacher is not a leper."
		"The principal's toupee is not a Frisbee."
		"Goldfish don't bounce."
		"Mud is not one of the 4 food groups."
		"No one is interested in my underpants."
		"I will not torment the emotionally frail."
		"I will not pledge allegiance to Bart."
		"High explosives and school don't mix."
		))))

    ;; *TEXT* file read test.  Perhaps best put at end of test if you're debugging a new
    ;; file system agent.  Use a file longer than 4296 bytes so we can be sure we
    ;; exceed FSA buffers
    (format t "~%Text File read test")
    (loop for test-file in '(#p"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-FS-INPUT-1.TXT"
			     #P"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-TEST-W-NL-LF.TXT"
			     #P"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-TEST-W-NL-CR.TXT"
			     #P"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-TEST-W-NL-CRLF.TXT"
			     #P"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-TEST-WO-NL-LF.TXT"
			     #P"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-TEST-WO-NL-CR.TXT"
			     #P"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-TEST-WO-NL-CRLF.TXT"
			     #P"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-TEST1-W-NL-LF.TXT"
			     #P"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-TEST1-W-NL-CR.TXT"
			     #P"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-TEST1-W-NL-CRLF.TXT"
			     #P"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-TEST1-WO-NL.TXT"
			     #P"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-TEST-MIXED.TXT"
			     #P"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-SCM-SERVER.LSP"
			     #P"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-DEFAULT.EL"
			     #P"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-LONGLINE"
			     #P"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-MANYLINE"
			     )
	as record-separator = (file-descriptor/record-separator
				(file-system/probe-file file-system test-file))
	do
	  (let ((exceptional (test-input-special-case-lookup test-file)))
	    (format t "~%File is ~s~%Record separator is ~s~%CRC: ~8,'0X"
		    test-file
		    (unless (eq (second exceptional) :none)
		      record-separator)
		    (file-system/crc-file file-system test-file)))

	  (with-file-system-stream (stream-handle file-system test-file
				    :record-separator record-separator
				    :direction :input :element-type 'character)
	    (let ((first-10 nil)
		  (last-10 nil)
		  (last-line-terminated nil)
		  (line-number 0))
	      (loop
		(multiple-value-bind (line newline-missing-p)
		    (file-system/read-line file-system stream-handle)
		  (unless (eq line :EOF)
		    ;; Must stash this because when line is eof, newline-missing-p is always true
		    (setq last-line-terminated (not newline-missing-p))
		    (incf line-number)
		    (when (<= line-number 10)
		      (push line first-10))
		    ;; A proper queue would be more efficient... TO-DO, add a utility
		    (push line last-10))
		  (when (eq line :eof)
		    (format t "~%Last line ~:[was not~;was~] terminated" last-line-terminated)
		    (return))))		;exit the loop
	      (let ((last-cons-we-want (nthcdr 9 last-10)))
		(when last-cons-we-want
		  (setf (cdr last-cons-we-want) nil)))
              (setq first-10 (nreverse first-10))
	      (setf last-10 (nreverse last-10))
	      (format t "~%Lines read from file: ~d" line-number)
	      (format t "~%First 10 lines of file:~%----------")
	      (mapc (lambda (string)
			(print (subseq string 0 (min 100 (length string))))) first-10)
	      (when (> line-number 10)
		(format t "~%----------~%Last 10 lines of file:~%----------")
		(mapc (lambda (string)
			  (print (subseq string 0 (min 100 (length string))))) last-10))
	      (format t "~%----------")))) ;delimit the end of displayed file contents

    ;; Delete the temporary file used for the file write test.
    (file-system/delete-file file-system "REPOSITORY:;UNDR-UNDR-TEST-1A.TMP")

    (format t "~%Test graph structure.")
    (let ((collected
	   (transitive-closure
	    (make-instance 'filtered-file-system
                           :underlying-file-system file-system
                           :filter #'test-filtered-file-system-filter-function)
	    #'cons
	    '())))
      (mapc #'print (sort-file-descriptors collected)))

    ;; Perform various non-file-i/o fs requests.
    (format t "~%Platform test ~%~s ~s"
	    (file-system/platform file-system)
	    (file-system/record-separator file-system))
    ;; Generate a PROBE-FILE on a file which is known to exist.
    ;; TO-DO: In this case we're using a test file which works for
    ;; current test mode, but we really need to use this after we've written files using
    ;; other services so the test will run with in any location with any FSA.

    (format t "~%Probe file test")
    (let ((fd (file-system/probe-file file-system #p"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-FS-INPUT-1.TXT")))
      (fd-print fd)
      ;; This assert-warnion will only work if the file is server accessible, and if
      ;; the date representations are accurate to the second.
      ;; And if the java call lastModified() returns the time, which, as it turns out
      ;; it is not guaranteed to do!

      ;; Note, we check against the value returned by `get-timestamp' or against the
      ;; value returned by `file-write-date'.  A bug in _stat can cause these to return
      ;; different values, and the netscape jvm uses _stat.

      (if (and (/= (os-get-timestamp (merge-pathnames
                                      (make-pathname :directory '(:relative "__test-input")
                                                     :name "__fs-input-1"
                                                     :type "txt"
                                                     :defaults "")
                                      (logical-file-system/root file-system)))
		   (file-descriptor/modification-date fd))
	       (/= (file-write-date (namestring
				      (merge-pathnames
				       (make-pathname :directory '(:relative "__test-input")
						      :name "__fs-input-1"
						      :type "txt"
						      :defaults "")
				       (logical-file-system/root file-system))))
		   (file-descriptor/modification-date fd)))
	  (warn "~%File write date differs between file ~s and fd ~s."
		(merge-pathnames "__fs-input-1.txt" +relative-test-input-dir+)
		fd))
      )

    ;; Generate a PFIL request for a non-existent file
    (format t "~%Probe non-existant file test")
    (assert (null (file-system/probe-file file-system #p"REPOSITORY:;UNDR-UNDR-123BLARNEY.321")))

    ;; Get the current directory, and do a probe on the directory
    ;; MEANINGLESS
    ;; (format t "~%Current directory test")
    ;; (test-walk-up-directories file-system)


    (format t "~%Probe directory test")
    (let ((file-descriptors
	   (sort-file-descriptors
	    (file-system/probe-directory file-system #p"REPOSITORY:;UNDR-UNDR-TEST-INPUT;"))))
      (format t "~%Number of files: ~d" (length file-descriptors))
      (loop for file-descriptor in file-descriptors
	  do (fd-print file-descriptor)))
    ;; Try to create a subdirectory
    ;; Note, test will fail if fstestdir wasn't deleted from last test run...
    (format t "~%Create directory test")
    (assert (file-system/create-directory file-system #p"REPOSITORY:;UNDR-UNDR-FSTESTDIR;"))
    (format t "~%Delete directory test")
    (file-system/delete-directory file-system #p"REPOSITORY:;UNDR-UNDR-FSTESTDIR;")

    ;; Test subdirectory deletion with recursive flag on
    (file-system/create-directory file-system #p"REPOSITORY:;UNDR-UNDR-FSTESTDIR2;") ;CDIR request

    (with-file-system-stream (stream file-system #p"REPOSITORY:;UNDR-UNDR-FSTESTDIR2;UNDR-UNDR-XXX.TST"
			      :record-separator (file-system/record-separator file-system)
			      :direction :output)
      (file-system/write-line file-system stream "hi"))
    (format t "~%Rename file test")
    (file-system/rename file-system #p"REPOSITORY:;UNDR-UNDR-FSTESTDIR2;" #p"REPOSITORY:;UNDR-UNDR-FSTESTDIR3;") ;RENM request on directory
    (assert (file-system/probe-file file-system #p"REPOSITORY:;UNDR-UNDR-FSTESTDIR3;"))
    (format t "~%Copy file test")
    (file-system/copy-file file-system #p"REPOSITORY:;UNDR-UNDR-FSTESTDIR3;UNDR-UNDR-XXX.TST" #p"REPOSITORY:;UNDR-UNDR-YYY.TST") ;test CFIL across directory boundary

    ;; Sneak in a touch test
    (format t "~%Touch file test")
    (assert (file-system/touch-file file-system #p"REPOSITORY:;UNDR-UNDR-FSTESTDIR3;UNDR-UNDR-TOUCH.TST"
					  (get-universal-time)
					  ;; Don't create if it doesn't exist
					  :if-does-not-exist :ignore))
    (assert (file-system/probe-file file-system #p"REPOSITORY:;UNDR-UNDR-FSTESTDIR3;UNDR-UNDR-TOUCH.TST"))
    (format t "~%Touch file test2")
    (assert (file-system/touch-file file-system #p"REPOSITORY:;UNDR-UNDR-FSTESTDIR3;UNDR-UNDR-TOUCH.TST"
				    (get-universal-time)
				    :if-does-not-exist :create)) ; create if it doesn't exist
    (assert (file-system/probe-file file-system #p"REPOSITORY:;UNDR-UNDR-FSTESTDIR3;UNDR-UNDR-TOUCH.TST"))
    (let ((fd1 (file-system/probe-file file-system #p"REPOSITORY:;UNDR-UNDR-YYY.TST")))
      (sleep 1)
;      (print (true (file-system/touch-file file-system fd1 (get-universal-time)))) ;relies on fact that FD isn't modified
      ;; >= instead of > because of poor granularity of timestamp, currently at the 'second' level
      (assert (>= (file-descriptor/modification-date (file-system/probe-file file-system #p"REPOSITORY:;UNDR-UNDR-YYY.TST"))
		  (file-descriptor/modification-date fd1)))
      (file-system/touch-file file-system fd1 3141592654)
      (assert (= (file-descriptor/modification-date (file-system/probe-file file-system #p"REPOSITORY:;UNDR-UNDR-YYY.TST"))
                3141592654))
      )

    ;; Sneak in a chmod test, too!
    (format t "~%Chmod test")
    (assert (file-system/set-read-only file-system #p"REPOSITORY:;UNDR-UNDR-FSTESTDIR3;UNDR-UNDR-TOUCH.TST"
				       t))
    (assert (file-system/touch-file file-system #p"REPOSITORY:;UNDR-UNDR-YYY.TST" 3141592654 :set-read-only t))
    (fd-print (file-system/probe-file file-system #p"REPOSITORY:;UNDR-UNDR-FSTESTDIR3;UNDR-UNDR-TOUCH.TST"))
    (fd-print (file-system/probe-file file-system #p"REPOSITORY:;UNDR-UNDR-YYY.TST"))
    (assert (file-system/set-read-only file-system #p"REPOSITORY:;UNDR-UNDR-FSTESTDIR3;UNDR-UNDR-TOUCH.TST"
				       nil))
    (assert (file-system/set-read-only file-system #p"REPOSITORY:;UNDR-UNDR-YYY.TST"
				       nil))
    (fd-print (file-system/probe-file file-system #p"REPOSITORY:;UNDR-UNDR-FSTESTDIR3;UNDR-UNDR-TOUCH.TST"))
    (fd-print (file-system/probe-file file-system #p"REPOSITORY:;UNDR-UNDR-YYY.TST"))
    (file-system/delete-file file-system #p"REPOSITORY:;UNDR-UNDR-YYY.TST")

    (format t "~%Delete non-empty directory test")
    (multiple-value-bind (result condition)
	;; This should fail, because directory isn't empty
	(ignore-errors (file-system/delete-directory file-system #p"REPOSITORY:;UNDR-UNDR-FSTESTDIR3;"))
      (assert (and (null result) condition))
      (let* ((error-string (with-output-to-string (stream)
			     (format stream "~a" condition)))
	     (marker "(Failed to DELETE file ")
	     (search-where (search marker error-string)))
	;; Capture the condition's error message into a string.  If
	;; MARKER is present in the string, strip off everything after
	;; it.  We need to strip it off because it varies from
	;; platform to platform.
	(when search-where
	  (setq error-string
	    (concatenate 'string
	      (subseq error-string 0 (+ search-where (length marker)))
	      " ...")))
	(format t "~%Expected failure of delete-directory, lisp condition signalled was: ~a"
		error-string)))

    ;; Now we do not expect errors
    (format t "~%Delete directory test")
    (file-system/delete-directory file-system #p"REPOSITORY:;UNDR-UNDR-FSTESTDIR3;" :recursive t :force t)
    ;; Some RENM and CFIL testing

    (with-file-system-stream (stream file-system  #p"REPOSITORY:;UNDR-UNDR-XXX.TST"
			      :record-separator (file-system/record-separator file-system)
			      :direction :output :if-exists :supersede)
      (file-system/write-line file-system stream "hi"))
    (assert (file-system/probe-file file-system #p"REPOSITORY:;UNDR-UNDR-XXX.TST"))
    (format t "~%Rename file test")
    (file-system/rename file-system #p"REPOSITORY:;UNDR-UNDR-XXX.TST" #p"REPOSITORY:;UNDR-UNDR-YYY.TST") ;RENM request on file
    (assert (file-system/probe-file file-system #p"REPOSITORY:;UNDR-UNDR-YYY.TST"))
    (format t "~%Copy file test")
    (file-system/copy-file file-system #p"REPOSITORY:;UNDR-UNDR-YYY.TST" #p"REPOSITORY:;UNDR-UNDR-ZZZ.TST") ;Test CFIL in same dir, returns nil or error
    (format t "~%Modify file test")
    (file-system/copy-file file-system #p"REPOSITORY:;UNDR-UNDR-TEST-INPUT;UNDR-UNDR-TEST-8C.MST"
			   #p"REPOSITORY:;UNDR-UNDR-TEST-8C.MST")
    (file-system/set-read-only file-system #p"REPOSITORY:;UNDR-UNDR-TEST-8C.MST" nil)
    (with-file-system-stream (stream file-system (file-system/probe-file file-system #p"REPOSITORY:;UNDR-UNDR-TEST-8C.MST")
			      :record-separator
			      (file-descriptor/record-separator
			       (file-system/probe-file file-system #p"REPOSITORY:;UNDR-UNDR-TEST-8C.MST"))
			      :direction :output
			      :if-exists :append
			      :if-does-not-exist :error)
      (file-system/write-line file-system stream "I'm going to add three lines to this file.")
      (file-system/write-line file-system stream "This is the second line.")
      (file-system/write-line file-system stream "And this is the third, and final line.")
      )

    (file-system/delete-file file-system #p"REPOSITORY:;UNDR-UNDR-YYY.TST") ; Test DFIL, returns nil or error
    (format t "~&Backup test.")
    (file-system/backup file-system #p"REPOSITORY:;UNDR-UNDR-ZZZ.TST"
			:copy/move :copy
			:pattern "foo~@[~d~]")
    (file-system/backup file-system #p"REPOSITORY:;UNDR-UNDR-ZZZ.TST"
			:copy/move :copy
			:pattern "foo~@[~d~]")
    (file-system/backup file-system #p"REPOSITORY:;UNDR-UNDR-ZZZ.TST"
			:copy/move :copy
			:pattern "foo~@[~d~]")
    (file-system/backup file-system #p"REPOSITORY:;UNDR-UNDR-ZZZ.TST"
			:copy/move :move
			:pattern "foo~@[~d~]")

    (file-system/delete-file file-system #p"REPOSITORY:;UNDR-UNDR-ZZZ-PRD-TST.FOO")
    (file-system/delete-file file-system #p"REPOSITORY:;UNDR-UNDR-ZZZ-PRD-TST.FOO1")
    (file-system/delete-file file-system #p"REPOSITORY:;UNDR-UNDR-ZZZ-PRD-TST.FOO2")
    (file-system/delete-file file-system #p"REPOSITORY:;UNDR-UNDR-ZZZ-PRD-TST.FOO3")
    (file-system/delete-file file-system #p"REPOSITORY:;UNDR-UNDR-TEST-8C.MST")

    ;; Shutdown sends QUIT request
    (file-system/shutdown file-system)
))

#||

(defun test-walk-up-directories	 (file-system)
  ;; Get parent directory of current path until we hit the root
  ;; We are at the top level as far as the repository is concerned

  ;; Don't go up to the parent of the TS50 directory since the
  ;; pathname hacking tools in the test output comparisson code
  ;; won't understand how to canonicalize it.
  (loop with upper =
	(probe-file
		     (canonicalize-pathname (os-get-temporary-directory)))
	with initial-path = (file-system/get-current-directory file-system)
	for fs-path = initial-path then fs-parent
	while fs-path
	for fs-parent = (file-system/get-parent-directory file-system fs-path)
	for path-path = initial-path then path-parent
	for path-parent = (pathname-syntactic-parent path-path)
	do (unless (pathname-match-p fs-path path-path)
	     (format t "~&Parent mismatch:~19t ~30s ~30s~&~19t~30s ~30s"
		     fs-path fs-parent
		     path-path path-parent)
	     (return))
	until (pathname-match-p path-parent upper)))

||#

(defparameter *server-test-port-base* 7001
  "The base port number used for the server tests.")

(defun server-test-port ()
  "Returns the test port for running the serven tests.
   On Windows NT, this is the *server-test-port-base*, but on HP-UX,
   the current user-id is added to the test-port-base in order to
   ensure that multiple users can simultaneously run tests."
  (+ #+mswindows           0
     #+(and unix allegro) (excl::getuid)
     *server-test-port-base*))


(defun http-server-start (publisher &key port reuse-port-p)
  (let ((http-server (make-instance 'net.aserve:wserver)))
    (let ((net.aserve:*wserver* http-server))
      (net.aserve:unpublish :all t)

      (funcall publisher)

      (catch 'exit-server
        (net.aserve:start
         :listeners 0
         :port port))
      (debug-message 3 "Server exited.")
      nil)))

(defun test-1a-listener (&key remote-agent)
  "A port listener for TEST-1A, which may be run standalone if you want to run TEST-1A's
   java program by hand."
  (format t "~%Starting HTTP server...")
  (http-server-start
   (lambda ()
     (net.aserve:publish
      :path "/test-1a"
      :function (lambda (req ent)
                  ;; We don't really care what the URI is, just we assume the caller has invoked a
                  ;; FileSystemAgent protocol and is awaiting a response
                  ;; We must include the response, because the HttpFileSystemAgent will require valid
                  ;; HTTP protocol headers from the server.

                  ;; TO-DO: NOTE: this is superficial FS and FSA capability testing only.
                  ;; Considerably more tests should exist in the regression system for
                  ;; EVERY OPERATOR, such as TOUCH and COPY-DIRECTORY.
                  (net.aserve:with-http-response (req ent)
                    (net.aserve:with-http-body (req ent)

      (when (socket::socket/output-chunking? (net.aserve:request-reply-stream req))
        ;; turn of chunking and fool allegroserve into not sending a final chunk
        (socket::socket-control (net.aserve:request-reply-stream req)
                                :input-chunking nil
                                :output-chunking nil
                                :output-chunking-eof t)
        (setf (net.aserve::request-reply-strategy req)
              (delete :chunked (net.aserve::request-reply-strategy req) :test #'eq)))

                    (call-with-temporary-directory
                     (lambda (tempdir)
                       (let* (
                              (*package* (find-package "CSF/SERVER"))
                              (*fsa-trace-io* *standard-output*)
                              ;; Socket-file-system creation sends HELO
                              (sfs (make-instance 'logical-file-system
                                    :root (canonicalize-pathname tempdir)
                                    :underlying-file-system (make-instance 'fsa-file-system
                                                   :stream (net.aserve:request-reply-stream req)))))
                         (test-file-system sfs (make-instance 'lisp-file-system) :remote-agent remote-agent)))) ;test rest of fs capabilities

                     ;; Right now, if there are errors resulting in an unwind, we don't trap them,
                     ;; and the QUIT isn't sent to the HTTP-SERVER-START interface, so it doesn't exit its
                     ;; listening loop.  We may want to fix that at some point to let people debug FSA's
                     ;; without requiring that they keep restarting lisp.  Actually, maybe this *is* the desired
                     ;; behavior.  We want the loop to continue if someone is testing.  So this test harness
                     ;; minus the :QUIT allows people to debug FSA's, theoretically.
                     :QUIT)))))         ;:QUIT signals http-server-start to stop after this connection.
   :port (server-test-port)
   :reuse-port-p t))

;;; TEST-1A NOTE: TO-DO: when we figure out how to background the HTTP server process, do it,
;;; and do it BEFORE executing the java process.  The only reason this test works
;;; now is that it takes a moment for the java process to start, during which time
;;; we're able (usually) to get the HTTP server running.

;;; Possible problem: if host names appear in test ouput,
;;; master files generated on one platform won't diff test files on another
;;; platform, we need diff to ignore the hostname.  Also, isn't there are better way to
;;; get a hostname besides SHORT-SITE-NAME?

#|

**** FOR REASONS I DON'T UNDERSTAND, this version of TEST-1A which used EXCL:RUN-SHELL-COMMAND
Ceased to work once I added the PDIR test to TEST-1A-LISTENER.
The lisp would hand up waiting on the newline at the end of the PDIR multi-line reply.
If I ran TEST-1A-LISTENER and the corresponding java program by hand, things work alright.
Similarly, using the recoded TEST-1A following this one, things also work.
I suspect I may have been shutting down the java streams at a bad time in the RUN-SHELL-COMMAND variant.

(defun test-1a ()
  ;; THIS ONE IS BROKEN after adding PDIR tests.  Don't know why.
  ;; Make sure the java code has been compiled
  (java-tools:java-compile ".")

  ;; Start the java agent in the background.
  (multiple-value-bind (java-output-stream java-error-stream process-id)
      (excl:run-shell-command
       (format nil "java FSTest http://~a:~d/test-1a"
               (short-site-name) (server-test-port))
       :output :stream :error-output :stream :wait nil :show-window :minimized)

    ;; Protect the streams.  The documentation doesn't say who's responsible for closing them,
    ;; but I think we are.
    (with-open-stream (java-output-stream java-output-stream)
      (with-open-stream (java-error-stream java-error-stream)
        ;; Start the lisp server to intercept the request.
        (unwind-protect
            (progn
              (test-1a-listener)
              (format t "~%~%Java standard output:~%")
              (stream-copy-text java-output-stream *standard-output*)
              (format t "~%~%Java error output:~%")
              (stream-copy-text java-error-stream *standard-output*)))
        ;; The server returned (or thrown), make sure we reap the java process
        (sleep 1)
        (sys:reap-os-subprocess :pid process-id :wait t)
        ))))

(defun test-1a ()
  ;; THIS ALSO WORKS, but it doesn't capture standard error.

  ;; Make sure the java code has been compiled
  (java-tools:java-compile ".")

  ;; Possible problem: if host names appear in test ouput,
  ;; master files generated on one platform won't diff test files on another
  ;; platform, we need diff to ignore the hostname.  Also, isn't there are better way to
  ;; get a hostname besides SHORT-SITE-NAME?

  ;; Start the java agent in the background.
  ;; Technically, this ought to be done second, if it fires up too fast, the server won't be ready.
  ;; But I haven't yet mastered getting the listener to run with all the lisp streams properly bound.
  (process-run-function
   "java"
   (lambda ()
       (excl:shell (format nil "java FSTest http://~a:~d/test-1a > java.otp"
                           (short-site-name) (server-test-port)))))

  ;; Start the listener
  (test-1a-listener)
  (format t "~%~%Java standard output:~%")
  (file-copy-to-stream "java.otp" *standard-output*)
  nil)

|#

(define-regression-test test-lisp-file-system-agent ()
  "Test LISP-FILE-SYSTEM. Same as TEST-1A, but not a socket based FSA"
  (call-with-temporary-directory
   (lambda (tempdir)
     (let ((raw-file-system (make-instance 'lisp-file-system)))
       (test-file-system
        (make-instance 'logical-file-system
                       :root tempdir
                       :underlying-file-system raw-file-system)
        raw-file-system)))))


(defun java-fsa-properties ()
  `((SET_FILE_READ_ONLY  ,(format nil "~s" (os-read-only-command)))
    (SET_FILE_READ_WRITE ,(format nil "~s" (os-read-write-command)))
    #+unix ,@(when (os-test-executable-command)
	       `((GET_FILE_EXECUTABLE
		  ,(format nil "~s" (os-test-executable-command)))))
    #+unix ,@(when (os-executable-command)
	       `((SET_FILE_EXECUTABLE
		  ,(format nil "~s" (os-executable-command)))))
    #+unix ,@(when (os-not-executable-command)
	       `((SET_FILE_NOT_EXECUTABLE
		  ,(format nil "~s" (os-not-executable-command)))))
    ,@(when (os-set-file-last-modified-command)
	`((SET_FILE_LAST_MODIFIED
	   ,(format nil "\"~{~a ~a ~a~}\"" (os-set-file-last-modified-command)))))
    ))


(defun test-1a (&key report-times)

  ;; THIS ONE WORKS, when I supply my own streams to the process.

  "Test FILE-SYSTEM lisp code and FileSystemAgent java code.
   This test requires that the java files be compiled, and will invoke both the java
   client code and the lisp server code such that they (hopefully) work together."

  (when report-times (sfs-operation-times-reset))

  (call-with-java-agent
   :java-class           (canonicalize-pathname
                          (make-pathname :name "FSTest"
                                         :defaults (translate-logical-pathname "CSF:JAVA;fsa;")))
   :java-argument-string (format nil "http://~a:~d/test-1a" (short-site-name) (server-test-port))
   :java-properties      (java-fsa-properties)
   :java-output-pathname (translate-logical-pathname #p"CSF:SERVER;test-output;java.out")
   :java-error-pathname  (translate-logical-pathname #p"CSF:SERVER;test-output;java.err")
   :receiver (lambda (start-java-semaphore)
	       (let (;;(web::*http-server-semaphore* start-java-semaphore)
		     (excl::*print-autoload* nil)) ;suppress autoloader messages, SPR18868
		 ;;(declare (special web::*http-server-semaphore*))
                 (signal-semaphore start-java-semaphore)
		 (test-1a-listener)
		 (when report-times
		   (sfs-operation-times-report))))))

#||
(defun test-1b ()
  "Test LISP-FILE-SYSTEM. Same as TEST-1A, but not a socket based FSA"
  (let ((excl::*print-autoload* nil))	;suppress autoloader messages, SPR18868
    (call-with-temporary-directory
     (lambda (tempdir)
	 (let ((raw-file-system (lisp-file-system-create)))
	   (test-file-system
	    (logical-file-system-create tempdir raw-file-system)
	    raw-file-system))))))


(defun test-listener (test-fn test-fn-args)
  "Establish a listener on the server for a socket FSA.  Once a connection is established,
   run TEST-FN on a socket-file-system bound to the connection.
   This is an auxilliary routine for TEST-JAVA-FSA."
  ;; TO-DO: replace test-1a-listener and guts of test-1a with calls to TEST-LISTENER
  ;; and TEST-JAVA-FSA.  Do this after JOE's change is checked in to minimize merge activity.
  (format t "~%Starting HTTP server...")
  (http-server-start
   :port (server-test-port)
   :reuse-port-p t
   :get-method
   (lambda (http-connection)
       ;; We don't really care what the URI is, just we assume the caller has invoked a
       ;; FileSystemAgent protocol and is awaiting a response
       ;; We must include the response, because the HttpFileSystemAgent will require valid
       ;; HTTP protocol headers from the server.

       ;; TO-DO: NOTE: this is superficial FS and FSA capability testing only.
       ;; Considerably more tests should exist in the regression system for
       ;; EVERY OPERATOR, such as TOUCH and COPY-DIRECTORY.
       (http-response http-connection *http-status-okay*)
       (let* ((*socket-file-system-trace-io* *standard-output*)
	      ;; Socket-file-system creation sends HELO
	      (sfs (logical-file-system-create
		    (canonicalize-pathname (translate-logical-pathname #p"TS50:TS50;server;"))
		    (socket-file-system-create (http-connection-stream http-connection)))))
	 (apply test-fn sfs test-fn-args)) ;test rest of fs capabilities

       ;; Right now, if there are errors resulting in an unwind, we don't trap them,
       ;; and the QUIT isn't sent to the HTTP-SERVER-START interface, so it doesn't exit its
       ;; listening loop.  We may want to fix that at some point to let people debug FSA's
       ;; without requiring that they keep restarting lisp.  Actually, maybe this *is* the desired
       ;; behavior.  We want the loop to continue if someone is testing.  So this test harness
       ;; minus the :QUIT allows people to debug FSA's, theoretically.
       :QUIT)))

(defun test-java-fsa (test-fn &key test-fn-arglist report-times)
  "Invoke TEST-FN on a SOCKET-FILE-SYSTEM.  It should take one arg, a file-system, and
   any other args as indicated by test-fn-arglist, which may be nil, and which will be passed
   to APPLY after the file-system argument.
   Return value: N/A"
  (when report-times (sfs-operation-times-reset))
  (call-with-java-agent
   :java-class (canonicalize-pathname (translate-logical-pathname "TS50:TS50;server;FSTest"))
   :java-argument-string (format nil "http://~a:~d/test-1a" (short-site-name) (server-test-port))
   :java-properties      (java-fsa-properties)
   :java-output-pathname (translate-logical-pathname #p"TS50:SERVER;test-output;java.out")
   :java-error-pathname  (translate-logical-pathname #p"TS50:SERVER;test-output;java.err")
   :receiver (lambda (start-java-semaphore)
	       (let ((excl::*print-autoload* nil)) ;suppress autoloader messages, SPR18868
		 (test-listener
		  (lambda (&rest arglist)
		    (signal-semaphore start-java-semaphore)
		    (apply test-fn arglist))
		  test-fn-arglist)))))

(defun test-timing ()
  "Run some file-system timing tests to help discover bottlenecks in the various file systems.
   Note that this test isn't a part of the regression because its results will change on every run,
   either the timing output or the contents of the ts50 hierarchy."
  (tail-labels				;i.e. flet*
      (;; Here's a thing to recursively examine directories
       (testdir (root-dir-name fd-list file-system)
	 (format t "~%Probing directory ~s, ~d descriptors" root-dir-name (length fd-list))
	 (loop for fd in fd-list
	     when (file-descriptor-is-directory? fd)
	     do
	       (let ((root-dirname (ensure-directory-pathname-designator
				    (file-descriptor/path fd)
				    (file-system-directory-separators file-system)
				    )))
		 (testdir root-dirname (file-system-probe-directory file-system root-dirname)
			  file-system))))
       ;; Here's the actual test code, which we'll invoke with SFS and LFS systems below.
       (test (file-system)
	 (time (testdir (canonicalize-pathname (translate-logical-pathname #p"TS50:TS50;server;"))
			(file-system-probe-directory file-system "REPOSITORY:;") file-system))))
    ;; Test invocations
    (test (logical-file-system-create (get-current-directory) (lisp-file-system-create)))
    (test-java-fsa #'test)))

(defun test-3a ()
  "Test HTTP server multiprocessing"
  (let (#+(and :allegro-version>= (:version>= 6 0)) (excl::*print-autoload* nil)
	(server-output-stream (make-string-output-stream))
	http-server-process)
    (flet ((http-server-function ()
	     (utility::with-standard-stream-variables-bound (server-output-stream)

	       (http-server-start :port (server-test-port)
				  :reuse-port-p t
				  :get-method (lambda (http-connection)
						  (utility::with-standard-stream-variables-bound (server-output-stream)
						    (format *trace-output* "~&Got a connection.")
						    (finish-output *trace-output*)
						    (http-response http-connection *http-status-okay*)
						    (format (http-connection-stream http-connection)
							    "Got it!")
						    (finish-output (http-connection-stream http-connection))
						    :quit))))))
      (format t "~%Starting HTTP server in multiprocessing mode ...")
      (setq http-server-process
	    (mp:process-run-function (list :name "HTTP server"
					   :initial-bindings
					   `(,@(generate-initial-binding-list)
					       ,@excl:*cl-default-special-bindings*))
				     #'http-server-function))
      (wait-for *http-server-semaphore*)
      (format t "~&*** Client output:~%")
      (process-allow-schedule http-server-process)
      (let ((url (url-create :scheme :http
			     :host "localhost"
			     :port (server-test-port)
			     :path "")))
	(with-simple-restart (nil "Give up trying to connect")
	  (with-http-connection (stream response headers url
				 :get
				 `((:host . ,(uri-authority url))
				   (:CONNECTION . "close")
				   (:ACCEPT-ENCODING . "gzip, deflate")
				   (:ACCEPT-LANGUAGE . "en-us")
				   (:ACCEPT
				    . "image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*")))
	    (print response)
	    (loop for (keyword . value) in headers
		  do
		  (unless (member keyword '(:date))
		    (format t "~&~20s~25,5t~s~%" keyword value)))
	    (finish-output stream)
	    (loop for line = (read-line stream nil nil)
		  while line
		  do (print line)))))
      (format t "~&*** Server output:~%")
      (write-string (get-output-stream-string server-output-stream))
      (process-kill http-server-process)
      nil)))
||#

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

(in-package "CSF/UTILITY")

(eval-when (:load-toplevel :execute)
  (export '(
            *within-regression-tests*
            call-with-java-agent
            define-regression-test
            define-test-input-file
            ensure-test-input-file
            generate-test-input-files
            regression-test-suite ;; for make-instance
            regression-test-suite/add-initialization
            test-all
            test-filtered-file-system-filter-function
            verify
            verify-assertion
            verify-multiple-values
            verify-one-return-value
            verify-thunk
            verify-value-eql
            verify-value-equal
            verify-value-equalp
            )))

(proclaim (standard-optimizations))

(defvar *regression-test-suites* nil)

(defclass regression-test-suite ()
  ((name :initarg :name
         :reader regression-test-suite/name)
   (package :initarg :package
            :reader regression-test-suite/package)
   (input-files :initform nil
                :accessor regression-test-suite/input-files)
   (initializations :initform nil
                    :accessor regression-test-suite/initializations)
   (tests :initform nil
          :accessor regression-test-suite/tests))
  (:default-initargs
      :name (package-name *package*)
    :package *package*))

(defmethod print-object ((test-suite regression-test-suite) stream)
  (print-unreadable-object (test-suite stream :type t :identity t)
    (format stream "~a"
            (regression-test-suite/name test-suite))))

(defmethod initialize-instance :after ((suite regression-test-suite) &rest ignore)
  (declare (ignore ignore))
  (let ((found (member (regression-test-suite/name suite)
                       *regression-test-suites*
                       :key #'regression-test-suite/name)))
    (if found
        (setf (car found) suite)
        (setq *regression-test-suites*
              (nconc *regression-test-suites* (list suite))))))

(defun find-regression-test-suite (thing)
  (etypecase thing
    (null (error "There is no null regression test."))
    (regression-test-suite thing)
    (package
     (let ((found (find thing *regression-test-suites*
                        :key #'regression-test-suite/package)))
       (unless found
         (error "There is no regression test suite associated with package ~s" thing))
       found))
    ((or symbol string)
     (let ((found (find thing *regression-test-suites*
                        :key #'regression-test-suite/name
                        :test #'string-equal)))
       (unless found
         (error "There is no regression test suite named ~s" thing))
       found))))

(defclass regression-test ()
  ((name :initarg :name
         :reader regression-test/name)
   (documentation :initarg :documentation
                  :initform "Undocumented."
                  :reader regression-test/documentation)
   (level :initarg :level
          :reader regression-test/level)
   (suite :initarg :suite
          :reader regression-test/suite))
  (:default-initargs :suite (find-regression-test-suite *package*)
                     :level 0))

(defmethod print-object ((test regression-test) stream)
  (print-unreadable-object (test stream :type t :identity t)
    (format stream "~a:~a"
            (regression-test-suite/name (regression-test/suite test))
            (regression-test/name test))))

(defmethod initialize-instance :after ((test regression-test) &rest ignore)
  (declare (ignore ignore))
  (let* ((suite (regression-test/suite test))
         (found (member (regression-test/name test)
                        (regression-test-suite/tests suite)
                        :key #'regression-test/name)))
    (if found
        (setf (car found) test)
        (setf (regression-test-suite/tests suite)
              (nconc (regression-test-suite/tests suite) (list test))))))

(defun test-failure (test-name format-string &rest values)
  (error "Regression test ~a failed:  ~?" test-name format-string values))

;;; This is the workhorse of the regression test system.
;;; Gotta love lisp.  Do *this* in java?  I think not.
(defun verify-thunk (test-name verifier thunk escape-hatch)
  (restart-case
   (let ((normal-return nil)
         (results nil))
     (unwind-protect
         (progn
           (setq results (multiple-value-list (funcall thunk)))
           (setq normal-return t))
       (unless normal-return
         (with-simple-restart (continue
                               "Continue running this test.")
           (funcall verifier test-name :throw nil))))
     (with-simple-restart (continue
                           "Continue running this test.")
       (funcall verifier test-name :return results))
     (values-list results))
   (abort ()
          :report "Skip remainder of this test."
          (funcall escape-hatch (list nil)))))

(defun verify-one-return-value (&optional further-test)
  (lambda (test-name disposition return-values)
    (cond ((not (eq disposition :return))
           (test-failure test-name "~&Expected normal return, got ~d." disposition))
          ((and (consp return-values)
                (null (cdr return-values)))
           (or (null further-test)
               (funcall further-test test-name (car return-values))))
          (t (test-failure test-name "~&Expected 1 value, got ~d values." (length return-values))))))

(defun verify-multiple-values (expected-count &rest further-tests)
  ;; make sure we have enough tests!
  (assert (length= further-tests expected-count))
  (lambda (test-name disposition return-values)
    (cond ((not (eq disposition :return))
           (test-failure test-name "~&Expected normal return, got ~d." disposition))
          ((length= return-values expected-count)
           (mapc (lambda (further-test return-value)
                   (funcall further-test test-name return-value))
                 further-tests return-values))
          (t (test-failure test-name "~&Expected ~d value~:P, got ~d value~:P."
                           expected-count
                           (length return-values))))))

(defun verify-value-eql (object)
  (lambda (test-name return-value)
    (or (eql return-value object)
        (test-failure test-name "~&Expected result ~s, got ~s" object return-value))))

(defun verify-value-equal (object)
  (lambda (test-name return-value)
    (or (equal return-value object)
        (test-failure test-name "~&Expected result ~s, got ~s" object return-value))))

(defun verify-value-equalp (object)
  (lambda (test-name return-value)
    (or (equalp return-value object)
        (test-failure test-name "~&Expected result ~s, got ~s" object return-value))))

(defun verify-value-string= (object)
  (lambda (test-name return-value)
    (or (string= return-value object)
        (test-failure test-name "~&Expected result ~s, got ~s" object return-value))))

(defmacro define-regression-test (name lambda-list &body body)
  `(PROGN
    (DEFUN ,name ,lambda-list
      (MACROLET ((VERIFY (VERIFIER &BODY BODY)
                   `(VERIFY-THUNK ',',name ,verifier
                                 (LAMBDA () ,@body)
                                 (LAMBDA (VALS)
                                   (RETURN-FROM ,',name (VALUES-LIST VALS)))))
                 (VERIFY-ASSERTION (FORM)
                    `(VERIFY (VERIFY-ONE-RETURN-VALUE
                               (VERIFY-VALUE-EQL 't))
                             ,form)))
        ,@body))
    (EVAL-WHEN (:LOAD-TOPLEVEL :EXECUTE)
      (MAKE-INSTANCE 'REGRESSION-TEST
                     :NAME ',name
                     :DOCUMENTATION ,(if (stringp (car body))
                                         (car body)
                                         "Undocumented test.")
                     :SUITE (FIND-REGRESSION-TEST-SUITE (SYMBOL-PACKAGE ',name))))))

(defvar *within-regression-tests* nil)

(defvar *current-regression-test-suite* nil
  "Bound to the regression test suite object which is currently being run.")

(defvar *current-regression-test* nil)

(defmethod regression-test-suite/initialize ((test-suite regression-test-suite))
  (when (regression-test-suite/initializations test-suite)
    (format t "~&~%***** Initializing ~s"  test-suite)
    (call-within-regression-test-suite test-suite
      (lambda ()
        (mapc (lambda (initialize)
                (cl:funcall initialize test-suite))
              (regression-test-suite/initializations test-suite))))))

(defmethod regression-test-suite/add-initialization ((test-suite regression-test-suite)
                                                     (function symbol))
  ;; Function should be a symbol that names a function which will be
  ;; called with the test suite as argument.  It will be called within
  ;; a CALL-WITHIN-REGRESSION-TEST-SUITE.
  (pushnew function (regression-test-suite/initializations test-suite)))

(defmethod regression-test-suite/add-initialization (test-suite function)
  (regression-test-suite/add-initialization (find-regression-test-suite test-suite) function))

(defun scan-test-suites (test-suite-designator)
  (declare (optimizable-series-function))
  (let ((filter
         (cond ((eq test-suite-designator :all) nil)
               ((null test-suite-designator) (if (or (eq *package* (find-package "COMMON-LISP-USER"))
                                                     (eq *package* (find-package "CHANGESAFE")))
                                                 nil
                                                 (list (package-name *package*))))
               ((consp test-suite-designator) (mapcar (lambda (designator)
                                                        (package-name (find-package designator)))
                                                      test-suite-designator))
               ((or (symbolp test-suite-designator)
                    (stringp test-suite-designator)) (list (package-name (find-package test-suite-designator))))
               ((packagep test-suite-designator) (list (package-name test-suite-designator)))
               (t (error "~S does not designate a test suite." test-suite-designator)))))

    (choose-if (lambda (suite)
                 (or (null filter)
                     (member (package-name (regression-test-suite/package suite))
                             filter :test #'string-equal)))
               (scan 'regression-test-suite *regression-test-suites*))))

(defun call-within-regression-test-suite (suite receiver)
  (let ((*current-regression-test-suite* suite)
        (*package* (regression-test-suite/package suite)))
    (funcall receiver)))

;;; Don't change these.
(defconstant +regression-test-print-pretty+ nil
  "*print-pretty* is bound to this during regression tests.")
(defconstant +regression-test-print-right-margin+ nil
  "*print-right-margin* is bound to this during regression tests.")
(defconstant +regression-test-print-length+ 100
    "*print-length* is bound to this during regression tests.")
(defconstant +regression-test-print-level+  5
    "*print-level* is bound to this during regression tests.")
(defconstant +regression-test-print-case+ :upcase
    "*print-case* is bound to this during regression tests.")

(defun call-within-regression-test-environment (test receiver)
  (declare (ignore test)) ;; should we bind this?
  (let ((*within-regression-tests* t)
        (*print-pretty*       +regression-test-print-pretty+)
        (*print-right-margin* +regression-test-print-right-margin+)
        (*print-length*       +regression-test-print-length+)
        (*print-level*        +regression-test-print-level+)
        (*print-case*         +regression-test-print-case+)
        (*ignore-errors-even-if-debugging* t))
    (funcall receiver)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; constructing test input files

;;; This function is used for constructing test input files, so the files
;;; can be carefully crafted on the test platform to ensure that they are
;;; not damaged by any file transfer mechanisms.

(defun define-test-input-file (filename format data &key (test-suite *package*))
  "Define a test input file named FILENAME (which is a file namestring in
   UNIX syntax) for TEST-SUITE.  DATA is a list of STRINGs CHARACTERs one
   of the special tokens :CR, :LF, :CRLF, or :LINEBREAK, or a symbol naming
   a function of two arguments, the symbol and a stream to write file
   contents to.
   :CR, :LF and :CRLF will generate a carriage return character, a linefeed
   character or a carriage return/linefeed pair, respecitvely.
   :LINEBREAK will output characters as appropriate to represent a line break
   in accordance with the conventions of the platform on which we are running."
  (declare (ignore format))
  (let* ((test-suite (find-regression-test-suite test-suite))
         ;; (package (regression-test-suite-package test-suite))
         (pathname (merge-pathnames filename
                                    (merge-pathnames
                                     (make-pathname :directory `(:relative ,(string-downcase
                                                                             (regression-test-suite/name test-suite)))
                                                    :defaults "")
                                     (translate-logical-pathname #p"CSF:TEST-DATA;GENERATED;"))))
         (new (list pathname data)))
    (let ((existing (member-if (lambda (fd) (funcall (platform-filename-equal (server-platform)) (car fd) pathname))
                               (regression-test-suite/input-files test-suite))))
      (if existing
          (setf (car existing) new)
          (setf (regression-test-suite/input-files test-suite)
                (nconc (regression-test-suite/input-files test-suite)
                       (list new)))))
    new))

(defun next-byte (data)
  (if (null data)
      (values nil nil)
    (let ((datum (car data))
          (more (cdr data)))
      (etypecase datum
        (character (values (char-code datum) more))

        (string (let ((l (string-length datum)))
                  (cond ((zerop l) (next-byte more))
                        ((= l 1)   (next-byte (cons (char datum 0) more)))
                        (t (let ((chars (coerce datum 'list)))
                             (next-byte (nconc chars more)))))))

        (keyword (ecase datum
                   (:cr   (values (char-code #\return)   more))
                   (:lf   (values (char-code #\linefeed) more))
                   (:crlf (values (char-code #\return) (cons #\linefeed more)))
                   (:linebreak          ; platform default linebreak character
                    #+win32 (next-byte (cons :crlf more))
                    #+unix  (next-byte (cons :lf   more)))))

        ((or function symbol) (next-byte (cons (funcall datum) more)))))))

(defun scan-test-data (data)
  (declare (optimizable-series-function))
  (until-if #'null
            (scan-fn '(values (or null (unsigned-byte 8)) t)
                     (lambda () (next-byte data))
                     (lambda (byte state)
                       (declare (ignore byte))
                       (next-byte state)))))

(defun generate-test-input-file (pathname data)
  "Create the specified test input file based on the description in DATA.
   See DEFINE-TEST-INPUT-FILE for a description of what can be in DATA."
  (with-simple-restart (retry-generate-test-input
                        "Retry generating test input file ~a"
                        pathname)
    (ensure-directories-exist (make-pathname :defaults pathname
                                             :name nil
                                             :type nil))
      (collect-binary-file pathname (scan-test-data data) #'write-byte)))

(defun generate-test-input-files (test-suite)
  "Create all of the automatically generated test input files for TEST-SUITE."
  (let* ((ts (find-regression-test-suite test-suite))
         (directory (merge-pathnames
                     (make-pathname :directory `(:relative ,(string-downcase (regression-test-suite/name ts)))
                                    :name nil
                                    :type nil
                                    :version nil
                                    :defaults "")
                     (translate-logical-pathname #p"CSF:TEST-DATA;GENERATED;")))
         (descriptions (regression-test-suite/input-files ts)))
    (collect 'list
             (map-fn '(values t t)
                     (lambda (description)
                       (generate-test-input-file
                        (merge-pathnames (car description) directory)
                        (cadr description)))
                     (scan descriptions)))))

(defun ensure-test-input-file (filename &key (package *package*))
  "Certain test input files are automatically generated so that their
   contents is appropriate for the platform on which the tests are run.
   This function creates a test input file from its description if it's
   not already present."
  (let* ((test-suite   (find-regression-test-suite package))
         (descriptions (regression-test-suite/input-files test-suite))
         (description  (assoc filename descriptions
                              :test #'string-equal
                              :key #'file-namestring))
         (directory (merge-pathnames
                     (make-pathname :directory `(:relative ,(string-downcase (regression-test-suite/name test-suite)))
                                    :defaults "")
                     (translate-logical-pathname #p"CSF:TEST-DATA;GENERATED;")))
         (pathname (merge-pathnames filename directory)))
    (if (probe-file pathname)
        (os-set-timestamp pathname (get-universal-time))
        (generate-test-input-file pathname (cadr description)))
    pathname))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *required-test-input-files* nil)
  )

(define-compiler-macro ensure-test-input-file (&whole form
                                                      filename &key (package *package*))
  (pushnew (list package filename) *required-test-input-files*
           :test #'equal)
  form)

;(defun describe-tests (&key test-suites)
;  (iterate ((test (scan-regression-tests test-suites)))
;    (format t "~&~s  ~a" test (documentation test 'function))))

(defun run-test (test &key (verbose t))
  (declare (ignore verbose))
  (with-simple-restart (skip-this-test "Skip test ~s" test)
    (loop
      (with-simple-restart (repeat-this-test "Try running ~s again" test)
        (let (start-time
              end-time)
          (call-within-regression-test-environment test
            (lambda ()
              (let ((*package* (find-package "KEYWORD")))
                (multiple-value-bind (sec min hour)
                    (decode-universal-time (get-universal-time))
                  (format *trace-output* "~&*****~55,,,'*< started at ~d:~2,'0d:~2,'0d ~;~> ~s"
                          hour min sec (regression-test/name test))))
              (setq start-time (get-internal-real-time))
              (funcall (regression-test/name test)) ;; symbol
              (setq end-time   (get-internal-real-time))))

          (format *trace-output* "~&unit test ~a took ~:/format-elapsed-time/~%"
                  (regression-test/name test) (elapsed-time-in-seconds start-time end-time))
          (return-from run-test (values (- end-time start-time))))))))

(defun test-all (&optional (designator *package*) &key (verbose t))
  (iterate ((suite (scan-test-suites designator)))
    (regression-test-suite/initialize suite)
    (call-within-regression-test-suite suite
      (lambda ()
        (iterate ((test (scan 'list (regression-test-suite/tests suite))))
          (run-test test :verbose verbose))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; weeding out uninteresting files

;;; This is not used by the core tests but is is used by the server
;;; and rfm tests.

(defun test-filtered-file-system-filter-function (pathname)
  "Returns NIL if the file should not be considered as part of the test data."
  (let ((ns (namestring pathname)))
    (not (or (search "QQU-CVS;" ns)
             (search "CVS/" ns)
             (search "CVS\\" ns)
             (search "QQU-WS-UNDR-QQU-FTP;" ns)
             (search "WS_FTP/" ns)
             (search "WS_FTP\\" ns)))))


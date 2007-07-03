;;; A simple bayesian spam filter for testing.

(in-package "CL-USER")

(defconstant *keyword-package*
  (if (boundp '*keyword-package*)
      (symbol-value '*keyword-package*)
      (find-package "KEYWORD")))

(defclass message ()
  ((file :initarg :file
	 :reader file
	 :initform (error "Required initarg :file omitted."))
   (subject :initarg :subject
	    :reader  subject
	    :initform (error "Required initarg :subject omitted."))
   (body :initarg :body
	    :reader  body
	    :initform (error "Required initarg :body omitted."))))

(defun spam? (message)
  (not (null (search "spmsg" (file message)))))

(defun scan-breaks (string)
  (declare (optimizable-series-function))
  (scan-fn-inclusive 'fixnum
		     (lambda () -1)
		     (lambda (prev) (or (position #\space string :start (+ prev 1)) (length string)))
		     (lambda (x) (= x (length string)))))

(defun scan-tokens (string)
  (declare (optimizable-series-function))
  (mapping (((start end) (chunk 2 1 (scan-breaks string))))
    (intern (string-upcase (subseq string (+ start 1) end))
	    *keyword-package*)))

(defun tokenize (string)
  (collect 'list (scan-tokens string)))

(defun parse-subject (subject)
  (if (string-equal (subseq subject 0 9) "Subject: ")
      (tokenize (subseq subject 9))
      (error "Ill-formed subject")))

(defun parse-message (pathname subject empty body)
  (if (not (zerop (length empty)))
      (error "Ill-formed message")
      (make-instance 'message
		     :file (enough-namestring pathname (translate-logical-pathname "LINGSPAM:bare;"))
		     :subject (remove-duplicates (parse-subject subject))
		     :body (remove-duplicates (tokenize body)))))

(defun read-message (pathname)
  (apply #'parse-message 
	 pathname
	 (collect 'list (scan-file pathname #'read-line))))

(defun scan-subcorpus (n)
  (declare (optimizable-series-function))
  (#m read-message
      (scan-directory
       (logical-pathname (format nil "LINGSPAM:bare;part~d;*.txt" n)))))

(defun subcorpus (n)
  (collect 'list (scan-subcorpus n)))

(defun subcorpora (ns)
  (if (null ns)
      '()
      (append (subcorpus (car ns))
	      (subcorpora (cdr ns)))))

(defun scan-subject (message)
  (declare (optimizable-series-function))
  (scan 'list (subject message)))

(defun scan-body (message)
  (declare (optimizable-series-function))
  (scan 'list (body message)))

(defun scan-message (message)
  (declare (optimizable-series-function))
  (catenate (scan-subject message) (scan-body message)))

(defun count-terms (ns)
  (let ((ham-count 0)
	(spam-count 0)
	(words (make-hash-table)))
    (labels ((get-entry (word)
	       (let* ((probe (gethash word words))
		      (entry (or probe (cons 0 0))))
		 (when (null probe)
		   (setf (gethash word words) entry))
		 entry)))
      (dolist (message (subcorpora ns))
	(if (spam? message)
	    (progn
	      (incf spam-count)
	      (dolist (word (body message))
		(incf (cdr (get-entry word)))))
	    (progn
	      (incf ham-count)
	      (dolist (word (body message))
		(incf (car (get-entry word))))))))
    (list ham-count spam-count words)))

(defclass filter ()
  ((ham-count :initarg :ham-count
	      :reader ham-count
	      :initform (error "Required initarg :ham-count omitted."))
   (spam-count :initarg :spam-count
	       :reader  spam-count
	       :initform (error "Required initarg :spam-count omitted."))
   (terms :initarg :terms
	  :reader  terms
	  :initform (error "Required initarg :terms omitted."))))


(defun prob->odds (prob)
  (/ prob (- 1.0 prob)))

(defun 10log10 (n)
  (* 10.0 (/ (log n) (log 10.0))))


(defun compute-odds (ham-count spam-count hash-table)
  (let ((new-table (make-hash-table)))
    (maphash (lambda (key value)
	       (let ((if-present
		      (10log10 (/ (* (+ (car value) .5) spam-count)
				  (* (+ (cdr value) .5) ham-count))))
		     (if-absent
		      (10log10 (/ (* (+ (- ham-count (car value)) .5) spam-count)
				  (* (+ (- spam-count (cdr value)) .5) ham-count)))))
;	       (format t "~&~s ~s ~s ~s ~s ~s ~s ~s ~s"  key  (car value) (cdr value)
;		       (/  (car value) ham-count)
;		       (/  (cdr value) spam-count)
;		       (/  (- ham-count (car value)) ham-count)
;		       (/  (- spam-count (cdr value)) spam-count)
;		       if-present
;		       if-absent)

		 (if (> (+ (car value) (cdr value)) 5)
		     (setf (gethash key new-table)
			   (list (car value) (cdr value) 		       
				 if-present
				 if-absent
				 )))))
	     hash-table)
    (make-instance 'filter
		   :ham-count ham-count
		   :spam-count spam-count
		   :terms new-table)))

(defun corpus->filter (n)
  (let ((results (count-terms n)))
    (apply #'compute-odds results)))

(defun score-message (filter msg)
  (let ((score (10log10 (/ (ham-count filter) (spam-count filter)))))
    (maphash (lambda (key value)
	       (if (member key (body msg))
		   (incf score (third value))
		   (incf score (fourth value))))
	     (terms filter))
    score))

(defun score-corpus (filter n)
  (iterate ((msg (scan-subcorpus n)))
    (format t "~&~S ~s" (file msg) (score-message filter msg))))

(defun cross-validate ()
  (dotimes (ix 10)
    (let ((i (+ ix 1)))
      (score-corpus (corpus->filter (remove i '(1 2 3 4 5 6 7 8 9 10))) i))))

;(collect 'list (#m read-message (scan-directory (logical-pathname "LINGSPAM:bare;part1;*.txt"))))

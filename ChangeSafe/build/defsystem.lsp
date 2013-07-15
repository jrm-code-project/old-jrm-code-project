;;; Replace the standard Allegro defsystem with Mark Kantrowitz' version.

(in-package "USER")

;;; Replace allegro defsystem with our own defsystem.

#+lispworks
(eval-when (:load-toplevel :execute)

(defun lispworks-version ()
  (values sys::*major-version-number*
	  sys::*minor-version-number*))

(unless (fboundp 'sys::lispworks-version)
  (setf (symbol-function 'sys::lispworks-version) #'lispworks-version))
)

(defun fasload (file &rest options &key verbose)
  #+allegro (apply #'load file options)
  #+lispworks (sys::load-fasl-file file verbose)
  #-(or allegro lispworks) (error "No way to load fasl files."))

(defun load-mk-defsystem (&key verbose)
  (let ((copacetic nil))
    (unwind-protect
	(progn
	  #+allegro (unuse-package "DEFSYSTEM" (find-package "USER"))
	  #+lispworks (when (and (find-package "MAKE")
				 (not (member :mk-defsystem *features*)))
			(rename-package (find-package "MAKE") "ORIGINAL-MAKE"))
	  #+lispworks (unless (find-package "FOREIGN")
			(let ((pkg (make-package "FOREIGN")))
			  (import '(sys:call-system
				    sys:call-system-showing-output)
				  pkg)
			  (export '(sys:call-system
				    sys:call-system-showing-output)
				  pkg)))

	  (let* ((defsystem-fasl #+allegro #p"TS50:DEFSYSTEM;defsystem.fasl"
				 #+lispworks #p"TS50:DEFSYSTEM;defsystem.fsl")
		 (defsystem-lisp #p"TS50:DEFSYSTEM;defsystem.lisp"))
	    (cond
	     ((and (file-write-date defsystem-lisp)
		   (or (null (file-write-date defsystem-fasl))
		       (> (file-write-date defsystem-lisp)
			  (file-write-date defsystem-fasl))))
	      (compile-file defsystem-lisp :output-file defsystem-fasl :verbose verbose)
	      (fasload defsystem-fasl :verbose verbose))
	     ((probe-file defsystem-fasl) (fasload defsystem-fasl :verbose verbose))
	     (t (error "Neither defsystem.fasl nor defsystem.lisp were found in ~s"
		       (translate-logical-pathname #p"TS50:DEFSYSTEM;")))))
	  (setq copacetic t))
      (unless copacetic
	#+lispworks (delete-package "MAKE")
	#+lispworks (rename-package "ORIGINAL-MAKE" "MAKE")))))

;;; This file is loaded into the base image of CLisp distribution.

(in-package "CL-USER")

(setf (logical-pathname-translations "JRM")
      `(("**;*.*" ,(namestring (merge-pathnames
				(make-pathname :directory '(:relative 
							    :back
							    :wild-inferiors)
					       :defaults "")
				(make-pathname :name :wild
					       :type :wild
					       :version nil
					       :defaults *load-pathname*))))))

;;; Create the packages needed for slime/swank.
(load "c:\\jrm-code-project\\emacs\\slime-2.0j\\package.lisp")
(load (translate-logical-pathname "JRM:build;image.lsp"))

;; Dump the image
(format t "~%Saving ~s..." (car ext:*args*))
(ext:saveinitmem (car ext:*ARGS*))
(format t "done.~%")
(finish-output t)
(sleep 1)

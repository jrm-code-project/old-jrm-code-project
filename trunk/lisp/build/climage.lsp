;;; This file is loaded into the base image of CLisp distribution.

(in-package "CL-USER")

;;; Create the packages needed for slime/swank.
(load "c:\\jrm-code-project\\emacs\\slime-2.0j\\package.lisp")

;; Dump the image
(format t "~%Saving ~s..." (car ext:*args*))
(ext:saveinitmem (car ext:*ARGS*) 
		 :executable t)
(format t "done.~%")
(finish-output t)
(sleep 1)

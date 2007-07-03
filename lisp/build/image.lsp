(in-package "CL-USER")

;;; Common image initialization goes here.

;;; A MUST-HAVE utility
(defun package-lossage-funcall (package-name symbol-name &rest arguments)
  "Allow this file to invoke functions that will be loaded without having
 to deal with the fact that the packages have not been loaded when this file
 is read."
  (check-type package-name keyword)
  (check-type symbol-name keyword)
  (with-simple-restart (skip-calling
                        "Skip calling (~a::~a~{ ~s~})"
                        package-name symbol-name arguments)
    (let ((package (find-package (symbol-name package-name))))
      (unless package
        (error "Cannot call (~a::~a~{ ~s~}) because package ~s doesn't exist."
               package-name symbol-name arguments package-name))
      (let ((symbol (find-symbol (symbol-name symbol-name) package)))
        (unless symbol
          (error "Cannot call (~a::~a~{ ~s~}) because symbol ~s doesn't exist in ~s."
                 package-name symbol-name arguments symbol-name package))
        (unless (fboundp symbol)
          (error "Cannot call (~a::~a~{ ~s~}) because symbol ~s isn't fbound."
                 package-name symbol-name arguments symbol))
        (apply symbol arguments)))))

(defpackage "JRM/CONFIG"
  (:documentation "Configuration variables.")
  (:use "COMMON-LISP"))

;;; Create the utility package so we can load our package defs into it.
(defpackage "JRM/UTILITY"
  (:nicknames "UTILITY" "UTILS")
  (:documentation "The basic utility package.")
  (:use "COMMON-LISP" "JRM/CONFIG"))

(load (logical-pathname "JRM:series;s-package.lisp"))
(load (logical-pathname "JRM:build;packages.lsp"))

(let ((new-packages (package-lossage-funcall
		     :utility
		     :setup-package-system :verbose t)))
  (load (logical-pathname "JRM:utility;replacement-macros.lsp"))
  (load (logical-pathname "JRM:series;s-code.lisp"))
  (load (logical-pathname "JRM:series;s-extra.lisp"))
  (mapc (lambda (package)
	  (package-lossage-funcall :series :install :pkg package))
	(list* (find-package "CL-USER")
	       new-packages)))


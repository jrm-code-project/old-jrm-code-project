(in-package "JRM/UTILITY")

(defun setup-package-system (&key verbose)
  (let ((broken-symbols
	 #+allegro
         '(cl:delete-file
           cl:directory
           cl:enough-namestring                 ; technically not broken, but hazardous
           ;; They moved this symbol and broke it.
           #+(and :allegro-version>= (:version>= 6 1)) excl:enough-pathname
           cl:ignore-errors
           cl:probe-file
           cl:unwind-protect
           cl:with-open-file
           cl:with-open-stream)

	 #+clisp
	 '(cl:cond
	   cl:enough-namestring
	   cl:unwind-protect
	   )

	 #+lispworks
         '(cl:cond  ;; adding some functionality to cond
           cl:directory
           cl:enough-namestring
           cl:nconc     ;; actually an error!
           cl:probe-file
           cl:subtypep
           ;; cl:the       ;; again, paranoid
           cl:unwind-protect
           cl:with-open-file
           cl:with-open-stream))
	(utility-package (find-package "JRM/UTILITY")))

    ;; The utility package will contain replacements for the broken symbols
    #+allegro
    (mapc (lambda (sym) (shadow sym utility-package))
          (if (member :dont-fix-franz *features*)
              (list 'excl::directory 'excl::delete-directory)
              (list*
               'excl::without-interrupts
               'excl::delete-directory
               broken-symbols)))

    #+clisp
    (mapc (lambda (sym) (shadow sym utility-package))
	  (list*
	   broken-symbols))

    #+lispworks
    (mapc (lambda (sym) (shadow sym utility-package))
          (list*
           'cl:error              ;; shadowed for error reporting
           'cl:warn               ;; shadowed for error reporting
           'cl:check-type         ;; shadowed for error reporting
           broken-symbols))

    ;; patch-up the third-party packages
    (mapc (lambda (package-name)
            (let ((package (find-package package-name)))
              (when package
                (mapc (lambda (sym)
                        (shadowing-import
                         (intern (symbol-name sym) utility-package) package))
                      broken-symbols))))
          '(
            "SERIES"
            ))

    (let* (
	   ;; Our packages go here
	   (spam-package (or (find-package "JRM/SPAM")
			     (make-package "JRM/SPAM"
					   :nicknames '("SPAM")
					   :use '("COMMON-LISP"))))
	   )

      ;; These stub definitions must be set before we proceed.
      (unless (member :dont-fix-franz *features*)
        (mapc (lambda (original)
                (let ((sym (find-symbol (symbol-name original) utility-package))
                      (fun (symbol-function original)))
                  (when (functionp fun)
                    (when verbose
                      (let ((*package* (find-package :keyword)))
                        (format *trace-output* "~&; Installing stub for ~s" sym)))
                    (setf (symbol-function sym) fun))))
              broken-symbols))

      (setf (macro-function (intern "COND" utility-package))
            (lambda (form env) (declare (ignore env)) `(CL:COND ,@(cdr form))))


      (mapc  (lambda (package)
                (when package
                  (mapc (lambda (sym)
                          (shadowing-import
                           (intern (symbol-name sym) utility-package) package))
                        broken-symbols)))
	     (list
	      spam-package
	      ))
      (list spam-package))))





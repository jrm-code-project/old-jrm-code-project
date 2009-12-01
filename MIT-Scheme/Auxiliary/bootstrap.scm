(display "Auxiliary\\Bootstrap.scm")
(newline)
(toggle-gc-notification!)
(let ((safe-load
	(lambda (file env)
	  (if (file-exists? file)
              (load file (->environment env))))))
  (safe-load "c:\\GitRepository\\mit-scheme\\src\\runtime\\list.com" '(runtime list))
  (safe-load "c:\\GitRepository\\mit-scheme\\src\\runtime\\symbol.com" '(runtime symbol))
  (safe-load "c:\\GitRepository\\mit-scheme\\src\\sf\\subst.com" '(scode-optimizer integrate)))

(with-working-directory-pathname 
 "C:\\jrm-code-project\\Mit-scheme\\BootstrapLib\\Runtime\\"
 (lambda () 
   (load "runtime.sf")
   (sf "site.scm")))

(with-working-directory-pathname 
 "C:\\jrm-code-project\\Mit-scheme\\BootstrapLib\\Sf\\"
 (lambda () (load "sf.sf")))

(with-working-directory-pathname 
 "C:\\jrm-code-project\\Mit-scheme\\BootstrapLib\\Cref\\"
 (lambda () (load "cref.sf")))

(with-working-directory-pathname 
 "C:\\jrm-code-project\\Mit-scheme\\BootstrapLib\\star-parser\\"
 (lambda () 
   (load "compile.scm") ;; no sf, alas
   (delete-file "matcher.bci")
   (delete-file "parser.bci")
   (delete-file "shared.bci")
   (delete-file "matcher.com")
   (delete-file "parser.com")
   (delete-file "shared.com")
   ))

(%exit)



				 

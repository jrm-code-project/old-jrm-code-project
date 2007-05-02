#ci(module evil (lib "swindle.ss" "swindle")
  (require "generics.ss")
  (require "utils.ss")
  (require "cparse.ss")
  (require "comp.ss")
  (require "cgen.ss")
  (require "emit.ss")

;;; Compile a c++ parse tree to scheme.
;;; Why?

  (define (phase-wrapper phase-name phase-function)
    (lambda arguments
      (let ((start-time (current-seconds)))
        (newline)
        (display ";; Beginning ")
        (display phase-name)
        (flush-output)
        (let ((return-values (multiple-value-list (apply phase-function arguments)))
              (end-time (current-seconds)))
          (newline)
          (display ";; Ending ")
          (display phase-name)
          (display ".  Time:  ")
          (display (- end-time start-time))
          (display " seconds.")
          (flush-output)
          (apply values return-values)))))


  (define (compile-top-level expression)
    (if (or (not (pair? expression))
            (not (eq? (car expression) 'declaration-list)))
        (error "Invalid top-level form.")
        (multiple-value-bind (code augmented-environment)
            ((phase-wrapper "Parse" parse-top-level) (cdr expression))
          (evil-message "code is" code)
          ((dynamic-require '(lib "pretty.ss") 'pretty-print)
           ((phase-wrapper "Emit" lisp-code->sexp-top-level)
            ((phase-wrapper "Generate" generate-top-level-code)
             ((phase-wrapper "Compile" compile-top-level-forms) code)))))))


  (provide compile-top-level))

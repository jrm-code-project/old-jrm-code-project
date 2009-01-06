;; -*- Scheme -*-

(define (make-unassigned-value)
   (letrec ((x x)) x))

(define-syntax mit-lambda-helper
  (syntax-rules (&optional &rest |#!optional| |#!rest|)
    ((mit-lambda-helper "parse-required" body required (|#!rest| restarg bogus . more))
     (mit-lambda-helper "Error:  Extra parameters after #!rest argument."))

    ((mit-lambda-helper "parse-required" body required (&rest restarg bogus . more))
     (mit-lambda-helper "Error:  Extra parameters after &rest argument."))

    ((mit-lambda-helper "parse-required" body required (|#!rest| restarg))
     (mit-lambda-helper "emit" required () restarg body))

    ((mit-lambda-helper "parse-required" body required (&rest restarg))
     (mit-lambda-helper "emit" required () restarg body))

    ((mit-lambda-helper "parse-required" body required (|#!rest|))
     (mit-lambda-helper "Error:  No parameter after #!rest."))

    ((mit-lambda-helper "parse-required" body required (&rest))
     (mit-lambda-helper "Error:  No parameter after &rest."))

    ((mit-lambda-helper "parse-required" body required (|#!optional| . params))
     (mit-lambda-helper "parse-optional" body required () params))

    ((mit-lambda-helper "parse-required" body required (&optional . params))
     (mit-lambda-helper "parse-optional" body required () params))

    ((mit-lambda-helper "parse-required" body (required ...) (param . params))
     (mit-lambda-helper "parse-required" body (required ... param) params))

    ((mit-lambda-helper "parse-required" body required param)
     (mit-lambda-helper "emit" required () param body))

    ((mit-lambda-helper "parse-optional" body required optional (|#!rest| restarg bogus . more))
     (mit-lambda-helper "Error:  Extra parameters after #!rest argument."))

    ((mit-lambda-helper "parse-optional" body required optional (|#!rest| restarg))
     (mit-lambda-helper "emit" required optional restarg body))

    ((mit-lambda-helper "parse-optional" body required optional (|#!rest|))
     (mit-lambda-helper "Error:  No parameter after #!rest."))

    ((mit-lambda-helper "parse-optional" body required optional (&rest restarg bogus . more))
     (mit-lambda-helper "Error:  Extra parameters after &rest argument."))

    ((mit-lambda-helper "parse-optional" body required optional (&rest restarg))
     (mit-lambda-helper "emit" required optional restarg body))

    ((mit-lambda-helper "parse-optional" body required optional (&rest))
     (mit-lambda-helper "Error:  No parameter after &rest."))

    ((mit-lambda-helper "parse-optional" body required optional (|#!optional| . params))
     (mit-lambda-helper "Error:  Duplicate #!optional encountered."))

    ((mit-lambda-helper "parse-optional" body required optional (&optional . params))
     (mit-lambda-helper "Error:  Duplicate #!optional encountered."))

    ((mit-lambda-helper "parse-optional" body required (optional ...) (param . params))
     (mit-lambda-helper "parse-optional" body required (optional ... param) params))

    ((mit-lambda-helper "parse-optional" body required optional ())
     (mit-lambda-helper "emit" required optional () body))

    ((mit-lambda-helper "parse-optional" body required optional param)
     (mit-lambda-helper "emit" required optional param body))

    ((mit-lambda-helper "emit" () () rest body)
     (lambda rest . body))

    ((mit-lambda-helper "emit" (required ...) () rest body)
     (lambda (required ... . rest) . body))

    ((mit-lambda-helper "emit" (required ...) (optional ...) () body)
     (mit-lambda-helper "generate-unassigned"
                        unassigned-value (required ...) (optional ...) () body
                        (unassigned-value)
                        (optional ...)))

    ((mit-lambda-helper "emit" (required ...) (optional ...) rest body)
     (mit-lambda-helper "generate-unassigned"
                        unassigned-value (required ...) (optional ...) rest body
                        ('())
                        (optional ... rest)))

    ((mit-lambda-helper "generate-unassigned"
                        unassigned-value required optional rest body
                        unassigned
                        (parameter . parameters))
     (mit-lambda-helper "generate-unassigned"
                        unassigned-value required optional rest body
                        (unassigned-value . unassigned)
                        parameters))

    ((mit-lambda-helper "generate-unassigned"
                        unassigned-value (required ...) (optional ...) () body
                        unassigned
                        ())
     (mit-lambda-helper "generate-clauses"
                        unassigned-value function (required ... optional ... ) () body
                        (required ...) (optional ...) unassigned
                        ()))

    ((mit-lambda-helper "generate-unassigned"
                        unassigned-value (required ...) (optional ...) rest body
                        unassigned
                        ())
     (mit-lambda-helper "generate-clauses"
                        unassigned-value function (required ... optional ... rest) rest body
                        (required ...) (optional ...) unassigned
                        ()))

    ((mit-lambda-helper "generate-clauses"
                        unassigned-value function full-arglist rest body
                        (prefix ...) (param . params) (unassigned0 . unassigned)
                        (clauses ...))
     (mit-lambda-helper "generate-clauses"
                        unassigned-value function full-arglist rest body
                        (prefix ... param) params unassigned
                        (clauses ... ((prefix ...) (function prefix ... . unassigned)))))

    ((mit-lambda-helper "generate-clauses"
                        unassigned-value function full-arglist () body
                        prefix () unassigned
                        (clauses ...))
     (mit-lambda-helper "emit-cases" unassigned-value function full-arglist body
                        (clauses ... (prefix (function . prefix)))))

    ((mit-lambda-helper "generate-clauses"
                        unassigned-value function full-arglist rest body
                        (prefix ...) () (unassigned0 . unassigned)
                        (clauses ...))
     (mit-lambda-helper "emit-cases"
                        unassigned-value function full-arglist body
                        (clauses ...
                                 ((prefix ...) (function prefix ... . unassigned))
                                 ((prefix ... . rest) (function prefix ... rest)))))

    ((mit-lambda-helper "emit-cases" unassigned-value function full-arglist body cases)
     (let ((unassigned-value (make-unassigned-value))
           (function (lambda full-arglist . body)))
       (case-lambda . cases)))))

(define-syntax mit-lambda
  (syntax-rules ()
    ((mit-lambda () . body) (lambda () . body))
    ((mit-lambda (parameter . parameters) . body)
     (mit-lambda-helper "parse-required" body () (parameter . parameters)))
    ((mit-lambda restarg . body) (lambda restarg . body))))

(define-syntax clambda-1
  (syntax-rules (&optional &rest &key &allow-other-keys)
    ;; Ensures partial expansions don't match.
    ((clambda-1 () . body) (lambda () . body))
    ((clambda-1 (paramspec . paramspecs) . body)
     (clambda-1 "parse-required" body () (paramspec . paramspecs)))

    ;; PARSE-REQUIRED


    ;; A trailing rest arg, append to arglist and emit.
    ((clambda-1 "parse-required" body (required ...) (&rest rest))
     (clambda-1 "emit" body (required ... . rest)))

    ;; Only required args, emit.
    ((clambda-1 "parse-required" body required ())
     (clambda-1 "emit" body required))

    ;; Error checking
    ((clambda-1 "parse-required" body required (&rest rest &allow-other-keys . paramspecs))
     (clambda-1 "Error:  &KEY must preceed use of &ALLOW-OTHER-KEYS"))

    ((clambda-1 "parse-required" body required (&allow-other-keys . paramspecs))
     (clambda-1 "Error:  &KEY must preceed use of &ALLOW-OTHER-KEYS"))

    ;; &rest arg followed by &key, enter &key loop
    ((clambda-1 "parse-required" body (required ...) (&rest rest &key . paramspecs))
     (clambda-1 "parse-key-begin" (body (required ... . rest)) rest paramspecs))

    ;; &key, enter &key loop
    ;; note that rest arg introduced here will be a temporary
    ((clambda-1 "parse-required" body required (&key . paramspecs))
     (clambda-1 "introduce-rest" body required temp-rest paramspecs))

    ((clambda-1 "introduce-rest" body (required ...) rest paramspecs)
     (clambda-1 "parse-key-begin" (body (required ... . rest)) rest paramspecs))

    ;; Error check.
    ((clambda-1 "parse-required" body required (&rest param0 param1 . paramspecs))
     (clambda-1 "Error:  &rest may only be followed by one parameter"))

    ((clambda-1 "parse-required" body required (&rest))
     (clambda-1 "Error:  &rest must be followed by one parameter"))

    ;; Found &optional, enter optional loop
    ((clambda-1 "parse-required" body required (&optional . paramspecs))
     (clambda-1 "parse-optional" (body required) () () paramspecs))


    ;; Parse a single required argument.
    ((clambda-1 "parse-required" body (required ...) (param . paramspecs))
     (clambda-1 "parse-required" body (required ... param) paramspecs))

    ;; &OPTIONAL

    ;; Degenerate cases of no actual optionals.
    ((clambda-1 "parse-optional" parsed optional optional-init (&rest rest &allow-other-keys . paramspecs))
     (clambda-1 "Error:  &KEY must preceed use of &ALLOW-OTHER-KEYS"))

    ((clambda-1 "parse-optional" parsed optional optional-init (&allow-other-keys . paramspecs))
     (clambda-1 "Error:  &KEY must preceed use of &ALLOW-OTHER-KEYS"))

    ((clambda-1 "parse-optional" (body (required ...)) () () (&rest rest))
     (clambda-1 "emit" body (required ... . rest)))

    ((clambda-1 "parse-optional" (body (required ...)) () () (&rest rest &key . paramspecs))
     (clambda-1 "parse-key-begin" (body (required ... . rest)) rest paramspecs))

    ;; note that rest arg will be temporary here
    ((clambda-1 "parse-optional" (body (required ...)) () () (&key . paramspecs))
     (clambda-1 "introduce-rest-1" body (required ...) temp-rest paramspecs))

    ((clambda-1 "introduce-rest-1" body (required ...) rest paramspecs)
     (clambda-1 "parse-key-begin" (body (required ... . rest)) rest paramspecs))

    ((clambda-1 "parse-optional" (body (required ...)) () () ())
     (clambda-1 "emit" body (required ...)))

    ;; Optionals end in &rest
    ((clambda-1 "parse-optional" (body (required ...)) (optional ...) optional-init (&rest rest))
     (clambda-1 "emit" body (required ... |#!optional| optional ... . rest)
                           optional-init))

    ;; Optionals just end
    ((clambda-1 "parse-optional" (body (required ...)) optional optional-init ())
     (clambda-1 "emit" body (required ... |#!optional| . optional) optional-init))

    ;; Optionals end, keys start, bundle and enter key-parse
    ((clambda-1 "parse-optional" (body (required ...)) (optional ...) optional-init (&rest rest &key . paramspecs))
     (clambda-1 "parse-key-begin"
              (body (required ... |#!optional| optional ... . rest) optional-init)
              rest
              paramspecs))

    ;; Optionals end, keys start, bundle and enter key-parse,
    ;; note rest arg introduced here will be a temporary
    ((clambda-1 "parse-optional" (body (required ...)) (optional ...) optional-init (&key . paramspecs))
     (clambda-1 "parse-key-begin"
              (body (required ... |#!optional| optional ... . rest) optional-init)
              rest
              paramspecs))

    ;; Error check.
    ((clambda-1 "parse-optional" parsed optional optional-init (&rest param0 param1 . paramspecs))
     (clambda-1 "Error:  &rest may only be followed by one parameter"))

    ((clambda-1 "parse-optional" parsed optional optional-init (&rest))
     (clambda-1 "Error:  &rest must be followed by one parameter"))

    ;; Woohoo, found an optional to parse!
    ((clambda-1 "parse-optional" parsed optional optional-init (optspec . paramspecs))
     (clambda-1 "parse-one-optional" parsed optional optional-init optspec paramspecs))

    ((clambda-1 "parse-one-optional" parsed (optional ...) (optional-init ...) (param initform supplied-p) paramspecs)
     (clambda-1 "parse-optional" parsed
              (optional ... param)
              (optional-init ...
                             (supplied-p (not (default-object? param)))
                             (param (if (default-object? param) initform param)))
              paramspecs))

    ((clambda-1 "parse-one-optional" parsed (optional ...) (optional-init ...) (param initform) paramspecs)
     (clambda-1 "parse-optional" parsed
              (optional ... param)
              (optional-init ...
                             (param (if (default-object? param) initform param)))
              paramspecs))

    ((clambda-1 "parse-one-optional" parsed (optional ...) optional-init (param) paramspecs)
     (clambda-1 "parse-optional" parsed
              (optional ... param)
              optional-init
              paramspecs))

    ((clambda-1 "parse-one-optional" parsed (optional ...) optional-init param paramspecs)
     (clambda-1 "parse-optional" parsed
              (optional ... param)
              optional-init
              paramspecs))

    ;; &KEY
    ((clambda-1 "parse-key-begin" parsed rest paramspecs)
     (clambda-1 "parse-key" parsed rest
              () ;; loop variables
              () ;; expanded-keyspecs
              paramspecs))

    ;; No more keyspecs, generate the clauses
    ((clambda-1 "parse-key" parsed rest ((loopvar loopinit) ...) expanded (&allow-other-keys bogus . more))
     (clambda-1 "Error:  illegal parameters following &allow-other-keys"))

    ;; No more keyspecs, generate the clauses
    ((clambda-1 "parse-key" parsed rest ((loopvar loopinit) ...) expanded (&allow-other-keys))
     (clambda-1 "generate-key-clause" parsed rest #t
                loop ((loopvar loopinit) ...) remaining allow-other-keys key value next
                ();; condclause starts empty
                ();; initclause starts empty
                () (loopvar ...) expanded))

    ;; No more keyspecs, generate the clauses
    ((clambda-1 "parse-key" parsed rest ((loopvar loopinit) ...) expanded ())
     (clambda-1 "generate-key-clause" parsed rest #f
              loop ((loopvar loopinit) ...) remaining allow-other-keys key value next
              ();; condclause starts empty
              ();; initclause starts empty
              () (loopvar ...) expanded))


    ((clambda-1 "parse-key" parsed rest loopvars expanded (param . paramspecs))
     (clambda-1 "parse-one-key" parsed rest loopvars expanded param paramspecs))

    ((clambda-1 "parse-one-key" parsed rest
              (loopvars ...)
              (expanded ...)
              ((keyword-name var) initform supplied-p-var) paramspecs)
     (clambda-1 "parse-key" parsed rest
              (loopvars ... (var unassigned-key-value) (supplied-p-var #f))
              (expanded ... (keyword-name var initform supplied-p-var))
              paramspecs))

    ((clambda-1 "parse-one-key" parsed rest (loopvars ...) (expanded ...) ((keyword-name var) initform) paramspecs)
     (clambda-1 "parse-key" parsed rest (loopvars ... (var unassigned-key-value)) (expanded ... (keyword-name var initform)) paramspecs))

    ((clambda-1 "parse-one-key" parsed rest (loopvars ...) (expanded ...) ((keyword-name var)) paramspecs)
     (clambda-1 "parse-key" parsed rest (loopvars ... (var unassigned-key-value)) (expanded ... (keyword-name var)) paramspecs))

    ((clambda-1 "parse-one-key" parsed rest (loopvars ...) (expanded ...) (var initform supplied-p-var) paramspecs)
     (clambda-1 "parse-key" parsed rest  (loopvars ...
                                                            (var unassigned-key-value)
                                                            (supplied-p-var #f)) (expanded ... (var var initform supplied-p-var)) paramspecs))

    ((clambda-1 "parse-one-key" parsed rest (loopvars ...) (expanded ...) (var initform) paramspecs)
     (clambda-1 "parse-key" parsed rest (loopvars ... (var unassigned-key-value)) (expanded ... (var var initform)) paramspecs))

    ((clambda-1 "parse-one-key" parsed rest (loopvars ...) (expanded ...) (var) paramspecs)
     (clambda-1 "parse-key" parsed rest (loopvars ... (var unassigned-key-value)) (expanded ... (var var)) paramspecs))

    ((clambda-1 "parse-one-key" parsed rest (loopvars ...) (expanded ...) var paramspecs)
     (clambda-1 "parse-key" parsed rest (loopvars ... (var unassigned-key-value)) (expanded ... (var var)) paramspecs))


    ;; When out of clauses, emit the code
    ((clambda-1 "generate-key-clause" (parsed ...) rest default-aok
              loop loopvars remaining allow-other-keys key value next
              condclause initclause before after ())

     (clambda-1 "emit" parsed ... rest default-aok
              loop loopvars remaining allow-other-keys key value next
              condclause initclause))

    ((clambda-1 "generate-key-clause" parsed rest default-aok
              loop loopvars remaining allow-other-keys key value next
              (condclause ...)
              (initclause ...)
              (before ...) (after0 after1 after2 ...)
              ((paramkey param initform param-supplied-p) . params))

     (clambda-1 "generate-key-clause" parsed rest default-aok
              loop loopvars remaining allow-other-keys key value next
              (condclause ... ((eq? key (quote paramkey))
                               (loop next allow-other-keys
                                     before ...
                                     (if (eq? param unassigned-key-value)
                                         value
                                         param)
                                     #t
                                     after2 ...)))
              (initclause ... (if (eq? param unassigned-key-value)
                                  (set! param initform)))
              (before ... param param-supplied-p)
              (after2 ...)
              params))

    ((clambda-1 "generate-key-clause" parsed rest default-aok
              loop loopvars remaining allow-other-keys key value next
              (condclause ...)
              (initclause ...)
              (before ...) (after0 after1 ...) ((paramkey param initform) . params))

     (clambda-1 "generate-key-clause" parsed rest default-aok
              loop loopvars remaining allow-other-keys key value next
              (condclause ... ((eq? key (quote paramkey)) (loop next allow-other-keys
                                                        before ...
                                                        (if (eq? param unassigned-key-value)
                                                            value
                                                            param)
                                                        after1 ...)))
              (initclause ... (if (eq? param unassigned-key-value)
                                  (set! param initform)))
              (before ... param)
              (after1 ...)
              params))

    ((clambda-1 "generate-key-clause" parsed rest default-aok
              loop loopvars remaining allow-other-keys key value next
              (condclause ...)
              (initclause ...)
              (before ...) (after0 after1 ...) ((paramkey param) . params))

     (clambda-1 "generate-key-clause" parsed rest default-aok
              loop loopvars remaining allow-other-keys key value next
              (condclause ... ((eq? key (quote paramkey)) (loop next allow-other-keys
                                                        before ...
                                                        (if (eq? param unassigned-key-value)
                                                            value
                                                            param)
                                                        after1 ...)))
              (initclause ... (if (eq? param unassigned-key-value)
                                  (set! param (make-unassigned-value))))
              (before ... param)
              (after1 ...)
              params))

    ;;; EMIT

    ;; no optional, no key
    ((clambda-1 "emit" body lambda-args)
     (lambda lambda-args . body))

    ;; optionals, no optional-inits, no key
    ((clambda-1 "emit" body lambda-args ())
     (mit-lambda lambda-args . body))

    ;; optional, no key
    ((clambda-1 "emit" body lambda-args optional-inits)
     (mit-lambda lambda-args
       (let* optional-inits . body)))

    ;; full blown, no optional inits
    ((clambda-1 "emit" body lambda-args rest default-aok
              loop ((loopvar init) ...) remaining allow-other-keys key value next
              (condclause ...)
              (initclause ...))

     (lambda lambda-args
       (let loop ((remaining rest)
                  (allow-other-keys default-aok)
                  (loopvar init) ...)
         (cond ((pair? remaining)
                (let ((key (car remaining))
                      (tail (cdr remaining)))
                  (cond ((pair? tail) (let ((value (car tail))
                                            (next  (cdr tail)))
                                        (cond condclause ...
                                              ((eq? key 'allow-other-keys) (loop next value loopvar ...))
                                              (allow-other-keys (loop next allow-other-keys loopvar ...))
                                              (else (error "Unrecognized keyword" key)))))
                        ((null? key) (error "Supplied key args not balanced." rest))
                        ;; impossible?
                        (else (error "Improper list for key args." rest)))))
               ((null? remaining)
                initclause ...
                . body)
               (else (error "Supplied key args not balanced." rest))))))

    ((clambda-1 "emit" body lambda-args optional-inits rest default-aok
              loop ((loopvar init) ...) remaining allow-other-keys key value next
              (condclause ...)
              (initclause ...))

     (mit-lambda lambda-args
       (let* optional-inits
         (let loop ((remaining rest)
                    (allow-other-keys default-aok)
                    (loopvar init) ...)
           (cond ((pair? remaining)
                  (let ((key (car remaining))
                        (tail (cdr remaining)))
                    (cond ((pair? tail) (let ((value (car tail))
                                              (next  (cdr tail)))
                                          (cond condclause ...
                                                ((eq? key 'allow-other-keys) (loop next value loopvar ...))
                                                (allow-other-keys (loop next allow-other-keys loopvar ...))
                                                (else (error "Unrecognized keyword" key)))))
                          ((null? key) (error "Supplied key args not balanced." rest))
                          ;; impossible?
                          (else (error "Improper list for key args." rest)))))
                 ((null? remaining)
                  initclause ...
                  . body)
                 (else (error "Supplied key args not balanced." rest)))))))))

(define-syntax rewrite-aux
  (syntax-rules (&aux)
    ((rewrite-aux "start" lambda lambda-list body)
     (rewrite-aux "scan-lambda-list" lambda () lambda-list body))

    ((rewrite-aux "scan-lambda-list" lambda (lambda-list ...) (&aux . params) body)
     (rewrite-aux "reverse-aux" lambda (lambda-list ...) () params body))

    ((rewrite-aux "scan-lambda-list" lambda (lambda-list ...) (param . params) body)
     (rewrite-aux "scan-lambda-list" lambda (lambda-list ... param) params body))

    ;; Didn't find an aux, just paste it back together.
    ((rewrite-aux "scan-lambda-list" lambda lambda-list () body)
     (lambda lambda-list . body))

    ((rewrite-aux "reverse-aux" lambda lambda-list reversed (aux . params) body)
     (rewrite-aux "reverse-aux" lambda lambda-list (aux . reversed) params body))

    ((rewrite-aux "reverse-aux" lambda lambda-list reversed () body)
     (rewrite-aux "aux-found" lambda lambda-list reversed body))

    ((rewrite-aux "aux-found" lambda lambda-list (aux . params) body)
     (rewrite-aux "rewrite-one-aux" lambda lambda-list aux params body))

    ((rewrite-aux "rewrite-one-aux" lambda lambda-list (param init) params body)
     (rewrite-aux "aux-found" lambda lambda-list params ((let ((param init)) . body))))

    ((rewrite-aux "rewrite-one-aux" lambda lambda-list (param) params body)
     (rewrite-aux "aux-found" lambda lambda-list params ((let ((param (default-aux-value))) . body))))

    ((rewrite-aux "rewrite-one-aux" lambda lambda-list param params body)
     (rewrite-aux "aux-found" lambda lambda-list params ((let ((param (default-aux-value))) . body))))

    ((rewrite-aux "aux-found" lambda lambda-list () body)
     (lambda lambda-list . body))))

(define-syntax clambda
  (syntax-rules ()
    ((clambda () . body) (lambda () . body))
    ((clambda (paramspec . paramspecs) . body)
     (rewrite-aux "start" clambda-1 (paramspec . paramspecs) body))))

(define-syntax defun
  (syntax-rules ()
    ((defun name lambda-list form . forms)
     (define name (clambda lambda-list form . forms)))))

(define-syntax funcall
  (syntax-rules ()
    ((funcall procedure . arguments) (procedure . arguments))))

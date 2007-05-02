#ci(module emit (lib "swindle.ss" "swindle")
     (require "utils.ss")
  (require "lmodel.ss")

  (defgeneric lisp-code->sexp (object))

  (defmethod (lisp-code->sexp object)
    `(BOGUS-OBJECT ,object))

  (defmethod (lisp-code->sexp (object <lisp-access>))
    (access-components object
      (lambda (environment name)
        (if (not environment)
            name
            `(ACCESS ,name ,(lisp-code->sexp environment))))))

  (defmethod (lisp-code->sexp (object <lisp-assignment>))
    `(SET! ,(variable/name (assignment/variable object)) ,(lisp-code->sexp (assignment/value object))))

  (defmethod (lisp-code->sexp (object <lisp-definition>))
    (definition-components object
      (lambda (var val)
        (define (emit-normal-define)
          `(DEFINE ,(lisp-code->sexp var) ,(lisp-code->sexp val)))

        (if (instance-of? val <lisp-lambda>)
            (lambda-components val
              (lambda (name required optional rest aux body)
                (if (eq? name var)
                    `(DEFINE ,(make-bound-variable-list (cons name required) optional rest aux)
                       ,@(lisp-code->sexp-list body))
                    (emit-normal-define))))
            (emit-normal-define)))))

  ;; If #t, use call-with-current-continuation instead of
  ;; let/ec
  (define *avoid-let/ec* #t)

  (defmethod (lisp-code->sexp (object <lisp-combination>))
    (let ((operator (combination/operator object))
          (operands (combination/operands object)))

      (define (default)
        `(,(lisp-code->sexp (combination/operator object))
          ,@(map lisp-code->sexp (combination/operands object))))

      (define (emit-normal-let name required operands body)
        (evil-message 'emit-normal-let name required operands body)
        `(LET ,@(if (eq? name lambda-tag:let)
                    '()
                    (list name))
              ,(map (lambda (name value)
                      (list name (lisp-code->sexp value)))
                    required operands)
              ,@(lisp-code->sexp-list body)))

      (cond ((instance-of? operator <lisp-lambda>)
             (lambda-components operator
               (lambda (name required optional rest aux body)
                 (if (and (null? optional)
                          (not rest)
                          (null? aux))
                     (emit-normal-let name required operands body)
                     (default)))))
            ((and (instance-of? operator <lisp-access>)
                  (not (access/environment operator))
                  (eq? (access/name operator) 'call-with-escaping-continuation)
                  (pair? operands)
                  (instance-of? (car operands) <lisp-lambda>)
                  (null? (cdr operands)))
             (lambda-components (car operands)
               (lambda (name required optional rest aux body)
                 (if (and (eq? name lambda-tag:unnamed)
                          (null? optional)
                          (not rest)
                          (null? aux))
                     (if *avoid-let/ec*
                         `(CALL-WITH-CURRENT-CONTINUATION
                           (LAMBDA (,(variable/name (car required)))
                                   ,@(lisp-code->sexp-list body)))
                         `(LET/EC ,(variable/name (car required))
                                  ,@(lisp-code->sexp-list body)))
                     (default)))))
            (else
             (default)))))

  (define (emit-conditional/default predicate consequent alternative)
    `(IF ,(lisp-code->sexp predicate)
         ,(lisp-code->sexp consequent)
         ,(lisp-code->sexp alternative)))

  (define (emit-conditional predicate consequent alternative)
    (cond ;((not alternative)
          ; (if (and (instance-of? predicate <lisp-combination>)
          ;          (instance-of? (combination/operator predicate) <lisp-access>)
          ;          (not (access/environment (combination/operator predicate)))
          ;          (eq? (access/name (combination/operator predicate)) 'not)
          ;          (pair? (combination/operands predicate))
          ;          (null? (cdr (combination/operands predicate))))
          ;     `(UNLESS ,(lisp-code->sexp (car (combination/operands predicate)))
          ;              ,@(lisp-code->sexp-list consequent))
          ;     `(WHEN ,(lisp-code->sexp predicate)
          ;            ,@(lisp-code->sexp-list consequent))))
          ((instance-of? alternative <lisp-conditional>)
           `(COND ,@(emit-cond-conditional predicate consequent alternative)))
          (else (emit-conditional/default predicate consequent alternative))))

  (define (emit-cond-conditional predicate consequent alternative)
    `((,(lisp-code->sexp predicate) ,@(lisp-code->sexp-list consequent))
      ,@(emit-cond-alternative alternative)))

  (define (emit-cond-disjunction predicate alternative)
    `((,(lisp-code->sexp predicate))
      ,@(emit-cond-alternative alternative)))

  (define (emit-cond-alternative alternative)
    (cond ((not alternative)
           '(#F))
          ((instance-of? alternative <lisp-conditional>)
           (conditional-components alternative emit-cond-conditional))
          (else
           `((ELSE ,@(lisp-code->sexp-list alternative))))))

  (defmethod (lisp-code->sexp (object <lisp-conditional>))
    (conditional-components object emit-conditional))

  (define (make-bound-variable-list required optional rest auxiliary)
    (cond ((and (null? required) (null? optional) rest (null? auxiliary)) rest)
          ((and required (null? optional) rest (null? auxiliary))
           (append required rest))
          (else `(,@required
                  ,@(if (not (null? optional)) (cons '&optional optional) '())
                  ,@(if rest (cons '&rest (list rest)) '())
                  ,@(if (not (null? auxiliary)) (cons '&aux auxiliary) '())))))

  (defmethod (lisp-code->sexp (object <lisp-lambda>))
    (lambda-components object
      (lambda (name required optional rest aux body)
        (cond ((eq? name lambda-tag:unnamed)
               `(LAMBDA ,(make-bound-variable-list required optional rest aux)
                        ,@(lisp-code->sexp-list body)))
              (else
               `(NAMED-LAMBDA
                 ,(make-bound-variable-list (cons name required) optional rest aux)
                 ,@(lisp-code->sexp-list body)))))))

  (defmethod (lisp-code->sexp (object <lisp-method>))
    (lambda-components (method/lambda object)
      (lambda (name required optional rest aux body)
        `(DEFINE-METHOD (,(method/return-type object)
                         (,(method/class object) ,(method/name object))
                         ,@(make-bound-variable-list required optional rest aux))
           ,@(lisp-code->sexp-list body)))))

  (defmethod (lisp-code->sexp (object <lisp-sequence>))
    `(BEGIN ,@(lisp-code->sexp-list object)))

  (defmethod (lisp-code->sexp (object <lisp-variable>))
    (variable/name object))

  (defmethod (lisp-code->sexp (object <boolean>)) object)
  (defmethod (lisp-code->sexp (object <char>))    object)
  (defmethod (lisp-code->sexp (object <number>))  object)
  (defmethod (lisp-code->sexp (object <string-like>))  object)

  (defmethod (lisp-code->sexp (object <list>))   `(QUOTE ,object))
  (defmethod (lisp-code->sexp (object <symbol>)) `(QUOTE ,object))
  (defmethod (lisp-code->sexp (object <vector>)) `(QUOTE ,object))

  (defmethod (lisp-code->sexp-list (seq <lisp-sequence>))
    (let loop ((actions (sequence/actions seq))
               (result '()))
      (cond ((not (pair? actions)) (reverse result))
            ((instance-of? (car actions) <lisp-sequence>)
             (loop (append (sequence/actions (car actions)) (cdr actions))
                   result))
            (else
             (loop (cdr actions)
                   (cons (lisp-code->sexp (car actions)) result))))))

  (defmethod (lisp-code->sexp-list object)
    (list (lisp-code->sexp object)))

  (define (lisp-code->sexp-top-level code)
    (lisp-code->sexp code))

  (provide
   *avoid-let/ec*
   lisp-code->sexp-top-level
   lisp-code->sexp
  )
  )

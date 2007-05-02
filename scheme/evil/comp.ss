;;; Compiled code
#ci(module comp (lib "swindle.ss" "swindle")
     (require (lib "setf.ss" "swindle"))
  (require "generics.ss")
  (require "utils.ss")
  (require "cmodel.ss")
  (require "cemit.ss")
  (require "xmodel.ss")

  (define (c-identifier->lisp-identifier c-identifier)
    (convert-c-name (c-identifier->string c-identifier)))

  (defclass <compilation-target> ()
    (parent :initarg :parent :reader target/parent)
    (type   :initarg :type :reader target/type))

  (defclass <value-target>     (<compilation-target>))
  (defclass <boolean-target>   (<value-target>))
  (defclass <void-target>      (<value-target>))
  (defclass <predicate-target> (<boolean-target>))
  (defclass <effect-target>    (<void-target>))
  (defclass <statement-target> (<effect-target>))
  (defclass <return-target>    (<compilation-target>))
  (defclass <return-void-target>  (<return-target> <void-target>))
  (defclass <return-value-target> (<return-target> <value-target>))
  (defclass <return-boolean-target>  (<return-target> <boolean-target>))

  (define return-target? (class-predicate <return-target>))

  (defgeneric compile-expression (target block expression))

;  (defgeneric elide-return (procedure-body))

;  (defmethod (elide-return procedure-body)
;    procedure-body)

;  (defmethod (elide-return (procedure-body <compiled-return>))
;    ;; Remove the topmost return.
;    (return/value procedure-body))

  (define (bind-escape-continuation variable-name binder-type)
    (lambda (block receiver)
      ;; Grab a continuation to return to if necessary.
      (let* ((block (block/make block '()))
             (cont-var (binding/variable (variable/make&bind! block variable-name #f #f)))
             (result (receiver block)))
        (cond ((and (non-local-exit? result)
                    (eq? (non-local-exit/variable result) cont-var)
                    (not (variable/referenced? cont-var (non-local-exit/body result))))
               (evil-message "Optimize:  Coalesce catch/throw" cont-var)
               (non-local-exit/body result))
              ((not (variable/referenced? cont-var result))
               (evil-message "Optimize:  Remove binding for non-local-exit" cont-var)
               result)
              (else (make binder-type
                      :block    block
                      :variable cont-var
                      :body     result))))))

  (defgeneric compile-statements* (target block statement statements))

  (defmethod (compile-statements* target block statement (statements <null>))
    (compile-expression target block statement))

  ;; If we run off the tail of a function that returns void,
  ;; insert a return expression.
  (defmethod (compile-statements* (target <return-void-target>) block statement (statements <null>))
    (let ((final-statement (compile-expression (make <statement-target> :parent target) block statement)))
      (if (non-local-exit? final-statement)
          final-statement
          (cons-expression block final-statement
                           (make <non-local-exit>
                             :block block
                             :variable (binding/variable
                                        (or (block/lookup-name block 'return #f)
                                            (evil-error "Return isn't bound?")))
                             :body (make-combination*
                                     block
                                     (global-ref/make 'void)))))))

  (defmethod (compile-statements* target block statement statements)
    (cons-expression
     block
     (compile-expression (make <statement-target> :parent target) block statement)
     (compile-statements* target block (car statements) (cdr statements))))

  (defmethod (compile-statements* target block (statement <c-expression-list>) statements)
    (compile-statements target block (append (expression-list/actions statement) statements)))

  (defmethod (compile-statements* target block (statement <c-empty-statement>) statements)
    (compile-statements* target block (car statements) (cdr statements)))

  (defmethod (compile-statements* target block (statement <c-empty-statement>) (statements <null>))
    (make <compiled-constant> :value "empty"))

  (defmethod (compile-statements* target block (statement <c-declaration>) statements)
    (let* ((inner-block (block/make block '()))
           (binding (variable/make&bind! inner-block
                                          (c-identifier->lisp-identifier
                                           (declaration/identifier statement))
                                          (declaration/expand-type statement)
                                          #f)))
      (make-let block (binding/variable binding)
                (if (declaration/initial-value statement)
                    (compile-expression (make <value-target> :parent target) block
                                        (declaration/initial-value statement))
                    #f)
                (compile-statements target inner-block statements))))

  (define (compile-statements target block statements)
    (if (null? statements)
        (make <compiled-constant> :value "empty")
        (compile-statements* target block (car statements) (cdr statements))))

;;; Compile expression for value
  (defmethod (compile-expression target block expression)
    (evil-message "compile-expression: " target expression)
    (newline)
    (emit-c-code expression)
    (newline)
    (flush-output)
    (error "compile-expression:  Unrecognized target, expression" target expression))

  (defmethod :around (compile-expression target block expression)
    (if (null? (c-code/labels expression))
        (call-next-method)
        (begin
          (evil-message "wrapping label" target block expression)
          (make-combination* block
                             (global-ref/make 'label)
                             (make <compiled-constant> :value (c-code/labels expression))
                             (call-next-method target block expression)))))

  (defmethod (compile-expression (target <value-target>) block (expression <c-relational-expression>))
    (make-convert-from-boolean
     block
     (compile-expression (make <predicate-target> :parent target) block expression)))

  (defmethod (compile-expression (target <value-target>) block (expression <c-unary-expression>))
    (make-combination*
      block
      (global-ref/make (operator/symbol (expression/operator expression)))
      (compile-expression (make <value-target> :parent target) block (expression/operand expression))))

  (defmethod (compile-expression (target <value-target>) block (expression <c-binary-expression>))
    (make-combination*
     block
     (global-ref/make (operator/symbol (expression/operator expression)))
     (compile-expression (make <value-target> :parent target) block (left expression))
     (compile-expression (make <value-target> :parent target) block (right expression))))

  (defmethod (compile-expression (target <value-target>) block (expression <c-arrow-expression>))
    (let ((object (compile-expression (make <value-target> :parent target) block (left expression)))
          (field (c-identifier->lisp-identifier (right expression))))
      (make-class-ref block
                      object
                      :arrow
                      (make <compiled-constant> :value field))))

  (defmethod (compile-expression (target <value-target>) block (expression <c-assignment>))
    (make <compiled-constant> :value "value of assignment"))

  (defmethod (compile-expression (target <value-target>) block (expression <c-cast-expression>))
    (let ((type (type-specifier->list (c-cast/type expression)))
          (value (compile-expression (make <value-target> :parent target) block (expression/operand expression))))
      (define (default)
        (make-combination*
         block
         (global-ref/make 'cast)
         (make <compiled-constant> :value type)
         value))
      (cond ((equal? type '(:pointer "Scheme_Object")) value)
            ((and (equal? type '(:pointer "void"))
                  (compiled-constant? value)
                  (eq? (constant/value value) 0))
             (make <compiled-constant> :value #f))
            (else (default)))))


  (defmethod (compile-expression (target <value-target>) block (expression <c-conditional-expression>))
    (make-conditional
      (compile-expression (make <predicate-target> :parent target) block (left expression))
      (compile-expression target block (middle expression))
      (compile-expression target block (right expression))))

;  (make-compiled-conditional
;   (compile-expression-for-predicate block (left expression))
;   (compile-expression-for-value block (middle expression))
;   (compile-expression-for-value block (right expression))))

  (defmethod (compile-expression (target <value-target>) block (expression <c-dot-expression>))
    (let ((dotl (left expression))
          (dotr (c-identifier->lisp-identifier (right expression))))
      (define (default)
        (make-class-ref block
                        (compile-expression (make <value-target> :parent target) block dotl)
                        :dot
                        (make <compiled-constant> :value dotr)))
      (if (and (c-dot-expression? dotl)
               (or (eq? dotr 'car)
                   (eq? dotr 'cdr)))
          (let ((dotll (left dotl))
                (dotlr (c-identifier->lisp-identifier (right dotl))))
            (if (and (c-arrow-expression? dotll)
                     (eq? dotlr 'pair-val))
                (let ((dotlll (left dotll))
                      (dotllr (c-identifier->lisp-identifier (right dotll))))
                  (if (and (c-cast-expression? dotlll)
                           (eq? dotllr 'u))
                      (make-combination*
                       block
                       (global-ref/make dotr)
                       (compile-expression (make <value-target> :parent target) block (expression/operand dotlll)))
                      (default)))
                (default)))
          (default))))

  (defmethod (compile-expression (target <value-target>) block (expression <c-expression-list>))
    (let* ((revactions (reverse (expression-list/actions expression)))
           (last-expr  (car revactions))
           (butlast-expr (reverse (cdr revactions))))
      (make-sequence
       (append (map (lambda (expr)
                      (compile-expression (make <effect-target> :parent target) block expr))
                    butlast-expr)
               (list
                (compile-expression target block last-expr))))))

  (defmethod (compile-expression (target <value-target>) block (expression <c-funcall>))
    (let ((op (expression/operator expression)))
      (if (c-variable? op)
          (let* ((op-name (c-identifier->lisp-identifier (variable/name op)))
                 (probe (block/lookup-name block op-name #f)))
            (cond ((not probe) (evil-message 'compile-expression "Operator not found" op-name)
                   (make-combination
                    block
                    (make <compiled-reference>
                      :block block
                      :variable (make <compiled-variable>
                                  :block (top-level-block)
                                  :name op-name))
                    (map (lambda (argument)
                           (compile-expression (make <value-target> :parent target) block argument))
                         (expression/operands expression))))
                  ((instance-binding? probe)
                   (make-combination
                    block
                    (global-ref/make 'send)
                    (list* (global-ref/make 'self)
                           (make <compiled-constant>
                             :value op-name)
                           (map (lambda (argument)
                                  (compile-expression (make <value-target> :parent target) block argument))
                                (expression/operands expression)))))
                  (else
                   (make-combination
                    block
                    (make <compiled-reference>
                      :block block
                      :variable (make <compiled-variable>
                                  :block (top-level-block)
                                  :name op-name))
                    (map (lambda (argument)
                           (compile-expression (make <value-target> :parent target) block argument))
                         (expression/operands expression))))))
          (evil-error 'compile-expression-for-value "operator is not a variable"))))

  (defmethod (compile-expression (target <value-target>) block (expression <c-literal>))
    (make <compiled-constant> :value (value expression)))

  (defmethod (compile-expression (target <boolean-target>) block (expression <c-literal>))
    (make <compiled-constant>
      :value (if (number? (value expression))
                 (not (zero? (value expression)))
                 (value expression))))

  (defmethod (compile-expression (target <value-target>) block (expression <c-method-call>))
    (let* ((callpart (expression/operator expression))
           (object   (left callpart))
           (name     (right callpart)))
      (make-combination
       block
       (global-ref/make 'send)
       (list* (compile-expression (make <value-target> :parent target) block object)
              (make <compiled-constant>
                :value (c-identifier->lisp-identifier name))
              (map (lambda (operand)
                     (compile-expression (make <value-target> :parent target) block operand))
                   (expression/operands expression))))))

  (defmethod (compile-expression (target <value-target>) block (expression <c-new>))
    (make-combination
     block
     (global-ref/make 'make-object)
     (cons (make <compiled-constant> :value (let ((it (c-new/type expression)))
                                              (cond ((c-identifier? it) (c-identifier->lisp-identifier it))
                                                    ((c-type-specifier? it) (type-specifier->list it))
                                                    (else "some type expression"))))
           (map (lambda (operand)
                  (compile-expression (make <value-target> :parent target) block operand))
                (c-new/operands expression)))))

  (defmethod (compile-expression (target <value-target>) block (expression <c-new-array>))
    (make-combination*
     block
     (global-ref/make 'make-vector)
     (compile-expression (make <value-target> :parent target) block (c-new-array/size expression))))

  (defmethod (compile-expression (target <value-target>) block (expr <c-sizeof-expression>))
    (make-combination*
     block
     (global-ref/make 'sizeof)
     (make <compiled-constant>
       :value (let ((arg (expression/operand expr)))
                (if (c-type-specifier? arg)
                    (type-specifier->list arg)
                    "sizeof operand")))))

  (defmethod (compile-expression (target <value-target>) block (expression <c-this>))
    (global-ref/make 'self))

  (defgeneric compile-variable (target block binding))

  (defmethod (compile-variable (target <boolean-target>) block (binding <lexical-binding>))
    (if (equal? (binding/type binding) "Bool")
        (make <compiled-reference>
          :block block
          :variable (binding/variable binding))
        (make-convert-to-boolean block
                                 (make <compiled-reference>
                                   :block block
                                   :variable (binding/variable binding)))))

  (defmethod (compile-variable (target <value-target>) block (binding <lexical-binding>))
    (cond ((equal? (binding/type binding) "Bool")
           (make-convert-from-boolean block
                                      (make <compiled-reference>
                                        :block block
                                        :variable (binding/variable binding))))
          ((eq? (variable/name (binding/variable binding)) 'scheme-null)
           (make <compiled-constant> :value #f))

          ((eq? (variable/name (binding/variable binding)) 'scheme-false)
           (make <compiled-constant> :value '()))
          (else
           (make <compiled-reference>
             :block block
             :variable (binding/variable binding)))))

  (defmethod (compile-variable (target <value-target>) block (binding <instance-binding>))
    (if (equal? (binding/type binding) "Bool")
        (make-convert-from-boolean block
         (make-class-ref block (global-ref/make 'self)
                         :arrow
                         (make <compiled-constant>
                           :value (variable/name (binding/variable binding)))))
        (make-class-ref block (global-ref/make 'self)
                         :arrow
                         (make <compiled-constant>
                           :value (variable/name (binding/variable binding))))))

  (defmethod (compile-variable (target <boolean-target>) block (binding <instance-binding>))
    (if (equal? (binding/type binding) "Bool")
        (make-class-ref block (global-ref/make 'self)
                        :arrow
                        (make <compiled-constant>
                          :value (variable/name (binding/variable binding))))
        (make-convert-to-boolean
         block
         (make-class-ref block (global-ref/make 'self) :arrow
                         (make <compiled-constant>
                           :value (variable/name (binding/variable binding)))))))

  (defmethod (compile-expression (target <value-target>) block (expression <c-variable>))
    (let ((binding (block/lookup-name block (c-identifier->lisp-identifier (variable/name expression)) #f)))
      (if (not binding)
          (evil-error 'compile-expression "No binding for" expression)
          (compile-variable target block binding))))


  (defmethod (compile-expression (target <value-target>) block (expvoid <c-void-expression>))
    (make-combination*
      block
      (global-ref/make 'void)))

;;; Compile expression for predicate

  (defmethod (compile-expression (target <value-target>) block (expression <c-variable>))
    (let ((binding (block/lookup-name block (c-identifier->lisp-identifier (variable/name expression)) #f)))
      (if (not binding)
          (evil-error 'compile-expression "No binding for" expression)
          (compile-variable target block binding))))

  (defmethod (compile-expression (target <boolean-target>) block (expression <c-relational-binary-combination>))
    (make-combination*
     block
     (global-ref/make (operator/symbol (expression/operator expression)))
     (compile-expression (make <predicate-target> :parent target) block (left expression))
     (compile-expression (make <predicate-target> :parent target) block (right expression))))



  (define (type-check3 type p1 c1 a1 if-tc if-not)
    (evil-message 'type-check3 p1 c1 a1)
    (if (and (c-cast-expression? p1)
             (equal? (type-specifier->list (c-cast/type p1)) '("long"))

             (c-cast-expression? (expression/operand p1))
             (equal? (type-specifier->list (c-cast/type (expression/operand p1))) '("__w64" "long"))

             (c-variable? c1)
             (eq? (c-identifier->lisp-identifier (variable/name c1)) 'scheme-integer-type))
        (if-tc type (expression/operand (expression/operand p1)))
        (if-not)))

  (define (type-check2 type pred consequent alt if-tc if-not)
    (evil-message 'type-check2 pred consequent alt)
    (if (and (c-binary-expression? pred)
             (eq? (expression/operator pred) c-operator:and-bits)

             (c-cast-expression? consequent)
             (equal? (type-specifier->list (c-cast/type consequent)) '("Scheme_Type"))

             (c-arrow-expression? alt)
             (eq? (c-identifier->lisp-identifier (right alt)) 'type))
        (type-check3 type (left pred) (expression/operand consequent) (left alt) if-tc if-not)
        (if-not)))

  (define (type-check1 type expression if-tc if-not)
    (evil-message 'type-check1 expression)
    (if (c-conditional-expression? expression)
        (type-check2 type (left expression) (middle expression) (right expression) if-tc if-not)
        (if-not)))

  (define (type-check? expression if-tc if-not)
    (evil-message 'type-check expression)
    (if (and (eq? (expression/operator expression) c-operator:equal)
             (c-cast-expression? (left expression))
             (equal? (type-specifier->list (c-cast/type (left expression))) '("Scheme_Type"))

             (c-cast-expression? (right expression))
             (equal? (type-specifier->list (c-cast/type (right expression))) '("Scheme_Type"))
             (c-variable? (expression/operand (right expression))))

        (type-check1 (c-identifier->lisp-identifier (variable/name (expression/operand (right expression))))
                     (expression/operand (left expression)) if-tc if-not)
        (if-not)))

  (defmethod (compile-expression (target <boolean-target>) block (expression <c-relational-binary-expression>))
    (type-check? expression
                 (lambda (type expr)
                   (let ((probe (assq type '((scheme-bignum-type . bignum?)
                                             (scheme-box-type . box?)
                                             (scheme-module-index-type . modidx?)
                                             (scheme-pair-type . pair?)
                                             (scheme-rename-table-type . rename-table?)
                                             (scheme-symbol-type . symbol?)
                                             (scheme-stx-type . stx?)
                                             (scheme-wrap-chunk-type . wrap-chunk?)))))

                     (if probe
                         (make-combination*
                          block
                          (global-ref/make (cdr probe))
                          (compile-expression (make <value-target> :parent target) block expr))
                         (make-combination*
                          block
                          (global-ref/make 'check-type)
                          (make <compiled-constant> :value type)
                          (compile-expression (make <value-target> :parent target) block expr)))))


                 (lambda ()

                   (let ((compiled-left  (compile-expression (make <value-target> :parent target) block (left expression)))
                         (compiled-right (compile-expression (make <value-target> :parent target) block (right expression))))
                     (define (default)
                       (make-combination*
                        block
                        (global-ref/make (operator/symbol (expression/operator expression)))
                        (compile-expression (make <value-target> :parent target) block (left expression))
                        (compile-expression (make <value-target> :parent target) block (right expression))))

                     (if (eq? (expression/operator expression) c-operator:not-equal)
                         (make-not
                          block
                          (make-combination*
                           block
                           (global-ref/make 'eq?)
                           (compile-expression (make <value-target> :parent target) block (left expression))
                           (compile-expression (make <value-target> :parent target) block (right expression))))
                         (default))))))

  (defmethod (compile-expression (target <boolean-target>) block (expression <c-relational-unary-combination>))
    (if (eq? (expression/operator expression) c-operator:not)
        (make-not
         block
         (compile-expression (make <predicate-target> :parent target) block (expression/operand expression)))

        (make-combination*
         block
         (global-ref/make (operator/symbol (expression/operator expression)))
         (compile-expression (make <predicate-target> :parent target) block (expression/operand expression)))))

  (defmethod (compile-expression (target <boolean-target>) block (expression <c-relational-unary-expression>))
    (make-combination*
      block
      (global-ref/make (operator/symbol (expression/operator expression)))
      (compile-expression (make <value-target> :parent target) block (expression/operand expression))))

  (defgeneric compile-assignment (target block operator place value))

  (defmethod (compile-assignment (target <void-target>) block operator place value)
    (make <compiled-constant> :value (string-append "assignment with operator '"
                                                    (symbol->string operator)
                                                    "'")))

  (defmethod (compile-assignment (target <void-target>) block (operator (singleton '=)) place value)
    (evil-error 'compile-expression-for-effect "assignment to lvalue" place))

  (defmethod (compile-assignment (target <void-target>) block
                                 (operator (singleton '=))
                                 (place <c-array-expression>)
                                 value)
    (make-combination*
     block
     (global-ref/make 'vector-set!)
     (compile-expression (make <value-target> :parent target) block (left place))
     (compile-expression (make <value-target> :parent target) block (right place))
     value))

  (defmethod (compile-assignment (target <void-target>) block
                                 (operator (singleton '=))
                                 (place <c-arrow-expression>)
                                 value)
    (make-class-set! block
                     (compile-expression (make <value-target> :parent target) block (left place))
                     :arrow
                     (make <compiled-constant> :value (c-identifier->lisp-identifier (right place)))
                     value))

  (defmethod (compile-assignment (target <void-target>) block
                                 (operator (singleton '=))
                                 (place <c-asterisk-expression>)
                                 value)
    (make-combination*
     block
     (global-ref/make 'assign-pointer!)
     (compile-expression (make <value-target> :parent target) block (expression/operand place))
     value))

  (defmethod (compile-assignment (target <void-target>) block
                                 (operator (singleton '=))
                                 (place <c-dot-expression>)
                                 value)
    (make-class-set! block
                     (compile-expression (make <value-target> :parent target) block (left place))
                     :dot
                     (make <compiled-constant> :value (c-identifier->lisp-identifier (right place)))
                     value))


  (defgeneric compile-variable-assignment (target block operator binding value))

  (defmethod (compile-variable-assignment target block
                                          (operator (singleton '=))
                                          binding
                                          value)
    (evil-error "Unrecognized binding" binding))

  (defmethod (compile-variable-assignment target block
                                          (operator (singleton '=))
                                          (binding <lexical-binding>)
                                          value)
    (evil-message 'compile-variable-assignment "Making assignment" (binding/variable binding))
    (make <compiled-assignment>
      :block block
      :variable (binding/variable binding)
      :value value))

  (defmethod (compile-variable-assignment target block
                                          (operator (singleton '=))
                                          (binding <instance-binding>)
                                          value)
    (make-class-set! block
                     (global-ref/make 'self)
                     :arrow
                     (make <compiled-constant> :value (variable/name (binding/variable binding)))
                     value))

  (defmethod (compile-assignment (target <void-target>) block
                                 (operator (singleton '=))
                                 (place <c-variable>)
                                 value)
    (compile-variable-assignment target block operator
                                 (block/lookup-name block (c-identifier->lisp-identifier (variable/name place)) #f)
                                 value))


;;; Compile expression for effect
  (defmethod (compile-expression (target <void-target>) block (assignment <c-assignment>))
    (compile-assignment target block
                        (assignment/operator assignment)
                        (assignment/place assignment)
                        (compile-expression (make <value-target> :parent target) block (assignment/value assignment))))

  (defmethod (compile-expression (target <void-target>) block (delete <c-delete>))
    (make-combination* block
                      (global-ref/make 'delete)
                      (compile-expression (make <value-target> :parent target) block (delete/place delete))))

  (define (find-return-target current-target)
    (if (return-target? current-target)
        current-target
        (find-return-target (target/parent current-target))))

  (defmethod (compile-expression (target <return-target>) block (expression <return>))
    (make <non-local-exit>
      :block block
      :variable (binding/variable
                 (or (block/lookup-name block 'return #f)
                     (evil-error "Return isn't bound?")))
      :body (compile-expression target block (return/value expression))))

  (defmethod (compile-expression target block (ret <return>))
    (compile-expression (find-return-target target) block ret))

  (define (compile-increment-decrement-for-effect block place modify)
    (cond ((c-variable? place)
           (let ((info (block/lookup-name block (c-identifier->lisp-identifier (variable/name place)) #f)))
             (cond ((not info) (evil-error 'compile-increment-decrement-for-effect "not found" place))
                   ((lexical-binding? info)
                    (evil-message 'compile-increment-decrement-for-effect "Making assignment" (binding/variable info))
                    (make <compiled-assignment>
                      :block block
                      :variable (binding/variable info)
                      :value (make-combination*
                              block
                              modify
                              (make <compiled-reference>
                                :variable (binding/variable info)))))
                   ((instance-binding? info)
                    (make-class-set! block (global-ref/make 'self)
                                     :arrow
                                     (make <compiled-constant>
                                       :value (variable/name (binding/variable info)))
                                     (make-combination*
                                      block
                                      modify
                                      (make-class-ref block (global-ref/make 'self)
                                                      :arrow
                                                      (make <compiled-constant>
                                                        :value (variable/name (binding/variable info)))))))
                   (else (error "unrecognized binding" info)))))

;           (cond ((instance-variable? info)
;                  (make-class-set! block (global-ref/make 'self)
;                                   (make <compiled-constant>
;                                     :value (c-identifier->lisp-identifier (identifier (third info))))
;                                   (make <compiled-combination>
;                                     :block block
;                                     :operator modify
;                                     :operands (list
;                                                (make-class-ref block (global-ref/make 'self)
;                                                                (make <compiled-constant>
;                                                                  :value (c-identifier->lisp-identifier (identifier (third info)))))))))
;                 (else
;                  (make <compiled-assignment>
;                         :block block
;                         :variable (make <compiled-variable>
;                                    :name (first info))
;                         :value (make <compiled-combination>
;                                  :block block
;                                  :operator modify
;                                  :operands (list (make <compiled-reference>
;                                                    :variable (make <compiled-variable> :name (first info))))))))))
          (else (evil-message 'compile-increment-decrement-for-effect "non-variable place")
                (make <compiled-constant> :value "compile-increment-decrement-for-effect"))))

  (defmethod (compile-expression (target <void-target>) block (expression <c-unary-modify>))
    (let ((operator (expression/operator expression)))
      (cond ((or (eq? operator c-operator:pre-increment)
                 (eq? operator c-operator:post-increment))
             (compile-increment-decrement-for-effect block (expression/operand expression) (global-ref/make 'add1)))
            ((or (eq? operator c-operator:pre-decrement)
                 (eq? operator c-operator:post-decrement))
             (compile-increment-decrement-for-effect block (expression/operand expression) (global-ref/make 'sub1)))
            (else (evil-error 'compile-expression-for-effect "Unknown operator")))))

  (defmethod (compile-expression (target <return-target>) block (statement <c-block>))
    (compile-statements target block (block-contents statement)))

;;; Compile statement
  (defmethod (compile-expression (target <void-target>) block (statement <c-block>))
    (compile-statements target block (block-contents statement)))

  (defmethod (compile-expression (target <statement-target>) block (statement <c-goto>))
    (make-combination*
     block
     (global-ref/make 'goto)
     (make <compiled-constant> :value (goto/target statement))))

  (defmethod (compile-expression (target <statement-target>) block (statement <c-break>))
    (make <non-local-exit>
      :block block
      :variable (binding/variable
                 (or (block/lookup-name block 'break #f)
                     (evil-error 'compile-statement "break isn't bound")))
      :body #f))

  (defmethod (compile-expression (target <statement-target>) block (statement <c-continue>))
    (make <non-local-exit>
      :block block
      :variable (binding/variable
                 (or (block/lookup-name block 'continue #f)
                     (evil-error 'compile-statement "continue isn't bound")))
      :body #f))

  (defmethod (compile-expression (target <statement-target>) block (statement <c-declaration>))
    ;; (evil-message "Compiling declaration" statement (c-identifier->lisp-identifier (declaration/identifier statement)))
    (let ((compiled   (make <compiled-declaration>
                        :name (c-identifier->lisp-identifier (declaration/identifier statement)))))
      (variable/make&bind! block
                           (c-identifier->lisp-identifier (declaration/identifier statement))
                           (declaration/expand-type statement)
                           compiled)
      compiled))

  (defmethod (compile-expression target block (statement <c-do-while>))
    ((bind-escape-continuation 'break <compiled-bind-break>)
     block
     (lambda (break-block)
       (let* ((do-while-block (block/make break-block '()))
              (do-while-var   (binding/variable (variable/make&bind! block 'do-while #f #f))))
         (make-iteration
          (variable/name do-while-var)
          (make-sequence
           (list ((bind-escape-continuation 'continue <compiled-bind-continue>)
                  do-while-block
                  (lambda (continue-block)
                    (let ((continue-binding (block/lookup-name continue-block 'continue #f)))
                      ;; stow the step part
                      (setf! (binding/value continue-binding)
                             (make-combination*
                                     block
                                     (global-ref/make 'void)))
                      (cons-expression continue-block
                                       (compile-expression (make <statement-target> :parent target)
                                                           continue-block (do-while/body statement))
                                       (make <non-local-exit>
                                         :block continue-block
                                         :variable (binding/variable continue-binding)
                                         :body (binding/value continue-binding))))))
                 (make-conditional
                  (compile-expression (make <predicate-target> :parent target)
                                      do-while-block (do-while/predicate statement))
                  (make <compiled-combination>
                    :operator (make <compiled-reference> :variable do-while-var)
                    :operands '())
                  (make <non-local-exit>
                    :block do-while-block
                    :variable (binding/variable
                               (block/lookup-name break-block 'break #f))
                    :body #f)))))))))

  (defmethod (compile-expression (target <statement-target>) block (statement <c-enum-declaration>))
    (evil-message "Compiling enum declaration" statement)
    (make <compiled-enum-declaration>
      :name (c-identifier->lisp-identifier (declaration/identifier statement))
      :members (map (lambda (decl)
                      (let ((enumerate (make <compiled-declaration>
                                         :name (c-identifier->lisp-identifier decl))))
                        ;; (evil-message "Defining" decl "as" enumerate)
                        (variable/make&bind! block (c-identifier->lisp-identifier decl) c-type:int enumerate)
                        enumerate))
                    (declaration/members statement))))

  (defmethod (compile-expression (target <void-target>) block (statement <c-for>))
    (cons-expression
     block
     (compile-expression (make <effect-target> :parent target) block (for/initialize statement))
     ((bind-escape-continuation 'break <compiled-bind-break>)
      block
      (lambda (break-block)
        (let* ((for-block (block/make break-block '()))
               (for-var   (binding/variable (variable/make&bind! block 'for #f #f))))
          (make-iteration
           (variable/name for-var)
           (make-sequence
            (list (make-conditional
                   (compile-expression (make <predicate-target> :parent target)
                                       for-block (for/predicate statement))
                   ((bind-escape-continuation 'continue <compiled-bind-continue>)
                    for-block
                    (lambda (continue-block)
                      (let ((continue-binding (block/lookup-name continue-block 'continue #f)))
                        ;; stow the step part.
                        (setf! (binding/value continue-binding)
                               (compile-expression (make <statement-target> :parent target)
                                                   break-block (for/step statement)))
                        (cons-expression continue-block
                                         (compile-expression (make <statement-target> :parent target)
                                                             continue-block (for/body statement))
                                         (make <non-local-exit>
                                           :block continue-block
                                           :variable (binding/variable continue-binding)
                                           :body (binding/value continue-binding))))))
                   (make <non-local-exit>
                     :block for-block
                     :variable (binding/variable
                                (block/lookup-name break-block 'break #f))
                     :body #f))
                  (make <compiled-combination>
                    :operator (make <compiled-reference> :variable for-var)
                    :operands '()))
            )))))))


  (defmethod (compile-expression (target <statement-target>) block (statement <c-function-definition>))
    (let ((defined-variable
            (binding/variable
             (variable/make&bind! block
                                  (c-identifier->lisp-identifier (declaration/identifier statement))
                                  (declaration/expand-type statement)
                                  #f))))

      (evil-message "Compiling" (variable/name defined-variable))

      (make <compiled-definition>
        :name  defined-variable
        :value (let ((inner-block (block/make block '())))

                 (define (name->variable name type)
                   (binding/variable (variable/make&bind! inner-block name type #f)))

                 (make <compiled-procedure>
                   :block block
                   :name defined-variable
                   :required (map (lambda (argument)
                                    (name->variable
                                     (c-identifier->lisp-identifier
                                      (declaration/identifier argument))
                                     (declaration/expand-type argument)))
                                  (arglist (declaration/declarator statement)))
                   :body ((bind-escape-continuation 'return <compiled-bind-return>)
                          inner-block
                          (lambda (return-block)
                             (compile-expression
                                      (make (cond ((equal? (function-definition/return-type statement) "Bool")
                                                   <return-boolean-target>)
                                                  ((equal? (function-definition/return-type statement) "void")
                                                   <return-void-target>)
                                                  (else <return-value-target>))
                                        :parent target
                                        :type (function-definition/return-type statement))
                                      return-block (definition/body statement)))))))))

  (defmethod (compile-expression target block (conditional <c-if>))
    (make-conditional
      (compile-expression (make <predicate-target> :parent target) block (conditional/predicate conditional))
      (compile-expression target block (conditional/consequent conditional))
      (and (conditional/alternative conditional)
           (compile-expression target block (conditional/alternative conditional)))))

  (defmethod (compile-expression (target <statement-target>) block (statement <c-method-definition>))
    (let ((method-name (identifier/base (declaration/identifier statement)))
          (class-name  (identifier/scope (declaration/identifier statement))))
      (let* ((class-name-for-lisp (convert-c-name class-name))
             (method-name-for-lisp (convert-c-name method-name))
             (binding (block/lookup-name block class-name-for-lisp #f)))
        (evil-message "Compiling " class-name-for-lisp method-name)
        (if (not binding)
            (evil-error 'compile-statement "Could not find class" class-name)

            (let ((class-info (binding/value binding)))
              (evil-message 'compile-statement "Class info is" class-info)

              (make <compiled-method>
                :block block
                :class class-name-for-lisp
                :name  method-name-for-lisp
                :body  (let* ((instance-variable-block (block/make block '()))
                              (inner-block (block/make instance-variable-block '())))

                         (define (declaration->instance-variable decl)
                           ;; (evil-message "Binding instance variable" (c-identifier->lisp-identifier (declaration/identifier decl)))
                           (binding/variable
                            (instance-variable/make&bind!
                             instance-variable-block
                             (c-identifier->lisp-identifier (declaration/identifier decl))
                             (declaration/expand-type decl)
                             #f)))

                         (define (name->variable name type)
                           (binding/variable (variable/make&bind! inner-block name type #f)))

                         (for-each declaration->instance-variable (instance-variables class-info))

                         (make <compiled-procedure>
                           :block block
                           :name (name->variable method-name (declaration/expand-type statement))
                           :required (map (lambda (argument)
                                            (name->variable (c-identifier->lisp-identifier (declaration/identifier argument))
                                                            (declaration/expand-type argument)))
                                          (arglist (declaration/declarator statement)))

                           :body ((bind-escape-continuation 'return <compiled-bind-return>)
                                  inner-block
                                  (lambda (return-block)
                                     (compile-expression
                                      (make (cond ((or (equal? (function-definition/return-type statement) "void")
                                                       ;; constructor
                                                       (eq? class-name-for-lisp method-name-for-lisp)
                                                       ;; delete
                                                       (and (char=? (string-ref (symbol->string method-name-for-lisp) 0) #\~)
                                                            (equal? (symbol->string class-name-for-lisp)
                                                                    (substring (symbol->string method-name-for-lisp) 1))))
                                                       <return-void-target>)
                                                  ((equal? (function-definition/return-type statement) "Bool")
                                                   <return-boolean-target>)
                                                  (else <return-value-target>))
                                        :type (function-definition/return-type statement)
                                        :parent target)
                                      return-block (definition/body statement))))))
                :return-type (function-definition/return-type statement)))))))

  (defmethod (compile-expression (target <statement-target>) block (statement <incomplete-struct-declaration>))
    (let ((probe (block/lookup-name block (c-identifier->lisp-identifier (declaration/identifier statement)) #f)))
      (if probe
          (begin (evil-message "Ignoring redefinition of" statement)
                 (make <compiled-declaration>
                   :name (c-identifier->lisp-identifier (declaration/identifier statement))))
          (call-next-method))))

  (defmethod (compile-expression (target <statement-target>) block (statement <c-struct-declaration>))
    (let ((name (c-identifier->lisp-identifier (declaration/identifier statement)))
          (base (base-classes statement))
          (members (declaration/members statement)))
      (let ((base-class (cond ((null? base) #f)
                              ((null? (cdr base)) (binding/value
                                                   (block/lookup-name block (c-identifier->lisp-identifier (car base)) #f)))
                              (else (error "Multiple base classes?")))))
        ;; (evil-message "base class is" base-class)
        (let ((compiled (make <compiled-struct-declaration>
                          :name name
                          :base base-class
                          :members members)))
          (variable/make&bind! block
                               (c-identifier->lisp-identifier (declaration/identifier statement))
                               (declaration/expand-type statement)
                               compiled)
          compiled))))

  (defmethod (compile-expression (target <statement-target>) block (tu <c-translation-unit>))
;;  (evil-message "compile-statement" block)
    (let loop ((remaining-statements (translation-unit/contents tu))
               (compiled-statements '()))
      (cond ((pair? remaining-statements)
;;             (emit-c-code (car remaining-statements))
;;             (display ";")
;;             (newline)
;;             (flush-output)
             (let ((result (compile-expression (make <statement-target> :parent target)  block (car remaining-statements))))
               (loop (cdr remaining-statements)
                     (cons result compiled-statements))))
            ((null? remaining-statements)
             (make <compiled-quotation>
               :block block
               :expression (make-sequence (reverse! compiled-statements))))
            (else (evil-error 'compile-statement "improper list in compile-statements for c-translation-unit")))))

  (defmethod (compile-expression target block (statement <c-switch>))
     ((bind-escape-continuation 'break <compiled-bind-break>)
      block
      (lambda (break-block)
        (let ((switch-block (block/make break-block '()))
              (switch-var   (binding/variable (variable/make&bind! block 'switch #f #f))))
          (make-let switch-block switch-var
                    (compile-expression (make <value-target> :parent target) block
                                        (switch/expression statement))
                    (compile-expression (make <statement-target> :parent target)
                                        switch-block (switch/body statement)))))))

  (defmethod (compile-expression target block (statement <c-while>))
     ((bind-escape-continuation 'break <compiled-bind-break>)
      block
      (lambda (break-block)
        (let* ((while-block (block/make break-block '()))
               (while-var   (binding/variable (variable/make&bind! block 'while #f #f))))
          (make-iteration
           (variable/name while-var)
           (make-sequence
            (list (make-conditional
                   (compile-expression (make <predicate-target> :parent target)
                                       while-block (while/predicate statement))
                   ((bind-escape-continuation 'continue <compiled-bind-continue>)
                    while-block
                    (lambda (continue-block)
                      (let ((continue-binding (block/lookup-name continue-block 'continue #f)))
                        ;; stow the step part.
                        (setf! (binding/value continue-binding)
                               (make-combination*
                                block
                                (global-ref/make 'void)))
                        (cons-expression continue-block
                                         (compile-expression (make <statement-target> :parent target)
                                                             continue-block (while/body statement))
                                         (make <non-local-exit>
                                           :block continue-block
                                           :variable (binding/variable continue-binding)
                                           :body (binding/value continue-binding))))))
                   (make <non-local-exit>
                     :block while-block
                     :variable (binding/variable
                                (block/lookup-name break-block 'break #f))
                     :body #f))
                  (make <compiled-combination>
                    :operator (make <compiled-reference> :variable while-var)
                    :operands '()))
            ))))))

;;;
  (define (make-top-level-block)
    (block/make #f '()))

  (define top-level-block (make-parameter #f))
  (define root-block (make-parameter #f))

  (define (compile-top-level-form top-level? tl-block block expression)
    ;; (evil-message "compile-top-level-form" tl-block block)
    (parameterize ((top-level-block tl-block)
                   (root-block block))
      (compile-expression (make <statement-target> :parent #f) block expression)))

  (define (compile-top-level-forms expression)
    (let ((block (make-top-level-block)))
      (compile-top-level-form #t block block expression)))

  (provide
   compile-top-level-forms))

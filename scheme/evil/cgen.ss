#ci(module cgen (lib "swindle.ss" "swindle")

  (require "generics.ss")
  (require "utils.ss")
  (require "xmodel.ss")
  (require "lmodel.ss")

;;; generate-expression converts the internal format to
;;; a lisp-oriented format.

  (defgeneric generate-expression (block expression))

  (defmethod (generate-expression block expression)
    (evil-message 'generate-expression "Unknown block or expression" block expression)
    expression)

  (define (generate-expressions block expressions)
    (map (lambda (expression) (generate-expression block expression)) expressions))

  (defmethod (generate-expression block (access <compiled-access>))
    (make <lisp-access>
      :environment (access/environment access)
      :name (access/name access)))

  (defmethod (generate-expression block (assignment <compiled-assignment>))
    (evil-message 'generate-expression "Assignment variable is" (assignment/variable assignment))
    (make <lisp-assignment>
      :variable (make <lisp-variable> :name (variable/name (assignment/variable assignment)))
      :value (generate-expression block (assignment/value assignment))))

  (define (generate-ec-expression name block body)
    (let ((variable (binding/variable (%block/lookup-name block name))))
      (cond ((not variable) (evil-error 'generate-ec-expression "Name not found" name))
            ((variable/referenced? variable body)
             (make-let/ec
              variable
              (generate-expression block body)))
            (else (generate-expression block body)))))

  (defmethod (generate-expression block (expr <compiled-bind-break>))
    (generate-ec-expression 'break (bind-break/block expr) (bind-break/body expr)))

  (defmethod (generate-expression block (expr <compiled-bind-continue>))
    (generate-ec-expression 'continue (bind-continue/block expr) (bind-continue/body expr)))

  (defmethod (generate-expression block (expr <compiled-bind-return>))
    (generate-ec-expression 'return (bind-return/block expr) (bind-return/body expr)))

;  (defmethod (generate-expression block (expr <compiled-break>))
;    (make <lisp-combination>
;      :operator (make <lisp-variable> :name (variable/name (break/variable expr)))
;      :operands (list (make <lisp-combination>
;                        :operator (make <lisp-access>
;                                    :environment #f
;                                    :name 'void)
;                        :operands '()))))

  (defmethod (generate-expression block (combo <compiled-combination>))
    (make <lisp-combination>
      :operator (generate-expression block (combination/operator combo))
      :operands (map (lambda (operand)
                       (if (not operand)
                           'uninitialized
                           (generate-expression block operand)))
                     (combination/operands combo))))

  (defmethod (generate-expression block (expression <compiled-conditional>))
    ;;  (evil-message "generate-expression: " expression)
    (make-lisp-conditional
     (generate-expression block (conditional/predicate expression))
     (generate-expression block (conditional/consequent expression))
     (if (conditional/alternative expression)
         (generate-expression block (conditional/alternative expression))
         #f)))

  (defmethod (generate-expression block (expression <compiled-constant>))
    (constant/value expression))

  (defmethod (generate-expression block (expression <non-local-exit>))
    (make <lisp-combination>
      :operator (make <lisp-variable> :name (variable/name (non-local-exit/variable expression)))
      :operands (list
                 (if (non-local-exit/body expression)
                     (generate-expression block (non-local-exit/body expression))
                     (make <lisp-combination>
                       :operator
                       (make <lisp-access>
                         :environment #f
                         :name 'void)
                       :operands '())))))

  (defmethod (generate-expression block (expression <compiled-iteration>))
    (make <lisp-combination>
      :operator
      (make <lisp-lambda>
        :name (compiled-iteration/name expression)
        :body (generate-expression block (compiled-iteration/body expression)))
      :operands '()))

  (defmethod (generate-expression block (expression <compiled-method>))
    ;; (evil-message "generate-expression" expression)
    (make <lisp-method>
      :class  (compiled-method/class expression)
      :name   (compiled-method/name expression)
      :lambda (generate-expression block (compiled-method/body expression))
      :return-type (compiled-method/return-type expression)))

  (defmethod (generate-expression block (expression <compiled-procedure>))
    ;; (evil-message "generate-expression" expression)
    (make <lisp-lambda>
      :name     (variable/name (compiled-procedure/name expression))
      :required (map variable/name (compiled-procedure/required expression))
      :optional (map variable/name (compiled-procedure/optional expression))
      :rest     (and (compiled-procedure/rest expression)
                     (variable/name (compiled-procedure/rest expression)))
      :body (generate-expression (compiled-procedure/block expression) (compiled-procedure/body expression))))

  (defmethod (generate-expression block (expression <compiled-reference>))
    (make <lisp-variable>
      :name (variable/name (reference/variable expression))))

  (defmethod (generate-expression block (expression <compiled-sequence>))
    (let ((actions (sequence/actions expression)))
      (if (null? (cdr actions))
          (generate-expression block (car actions))
          (make-lisp-sequence (generate-expressions block actions)))))

  (defmethod (generate-expression block (expression <compiled-declaration>))
    'compiled-declaration)

  (defmethod (generate-expression block (expression <compiled-enum-declaration>))
    'compiled-enum-declaration)

  (defmethod (generate-expression block (expression <compiled-definition>))
    (make <lisp-definition>
      :variable (variable/name (definition/name expression))
      :value    (generate-expression block (definition/value expression))))

  (defmethod (generate-expression block (expression <compiled-struct-declaration>))
    'compiled-struct-declaration)

  (define (generate-top-level-code quotation)
    (let ((block (quotation/block quotation))
          (expression (quotation/expression quotation)))
      (let ((result (generate-expression block expression)))
        (if (compiled-open-block? expression)
            result
            (generate-declaration (block/declarations block) result)))))

  (define (generate-declaration declarations expression)
    (let ((declarations (maybe-flush-declarations declarations)))
      (if (null? declarations)
          expression
          (make-declaration declarations expression))))

  (define (declarations/known? declaration)
    #f)

  (define (maybe-flush-declarations declarations)
    (if (or (not declarations) (null? declarations))
        '()
        (let ((declarations (declarations/original declarations)))
          (let loop ((declarations declarations))
            (cond ((or (not declarations) (null? declarations)) '())
                  ((declarations/known? (car declarations))
                   (loop (cdr declarations)))
                  (else
                   (if (not (known-compiler-declaration? (car declarations)))
                       (warn "Unused declaration" (car declarations)))
                   (cons (car declarations) (loop (cdr declarations)))))))))

  (provide
   generate-top-level-code))

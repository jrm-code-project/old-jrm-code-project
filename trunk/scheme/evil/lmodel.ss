#ci(module lmodel (lib "swindle.ss" "swindle")
     (require "utils.ss")
  (require "generics.ss")
;;; A simple model for lisp code.

  (defclass <lisp-code> ())

  (defclass <lisp-access> (<lisp-code>)
    (environment :initarg :environment :reader access/environment)
    (id          :initarg :name        :reader access/name)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (if (not (access/environment object))
                   (display "#<lisp-absolute-reference " port)
                   (display "#<lisp-access "))
               (display (access/name object) port)
               (display ">" port)))

  (define (access-components access receiver)
    (receiver (access/environment access) (access/name access)))

  (define (make-absolute-reference name)
    (make <lisp-access> :environment #f :name name))

  (defclass <lisp-assignment> (<lisp-code>)
    (variable :initarg :variable :reader assignment/variable)
    (value    :initarg :value    :reader assignment/value)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (display " " port)
               (display (variable/name (assignment/variable object)) port)
               (display ">" port)))


  (define (assignment-components assignment receiver)
    (receiver (assignment/variable assignment)
              (assignment/value    assignment)))

  (defclass <lisp-definition> (<lisp-code>)
    (variable :initarg :variable :reader definition/variable)
    (value :initarg :value :reader definition/value)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (display " " port)
               (display (variable/name (definition/variable object)) port)
               (display ">" port)))

  (define (definition-components definition receiver)
    (receiver (definition/variable definition)
              (definition/value definition)))

  (defclass <lisp-combination> (<lisp-code>)
    (operator :initarg :operator :reader combination/operator)
    (operands :initarg :operands :reader combination/operands))

  (define (combination-components combination receiver)
    (receiver (combination/operator combination)
              (combination/operands combination)))

  (defclass <lisp-conditional> (<lisp-code>)
    (predicate   :initarg :predicate   :reader conditional/predicate)
    (consequent  :initarg :consequent  :reader conditional/consequent)
    (alternative :initarg :alternative :reader conditional/alternative))

  (define (make-lisp-conditional predicate consequent alternative)
    (cond ((and (instance-of? predicate <lisp-combination>)
                (instance-of? (combination/operator predicate) <lisp-access>)
                (not (access/environment (combination/operator predicate)))
                (eq? (access/name (combination/operator predicate)) 'not))
           (make-lisp-conditional (car (combination/operands predicate))
                                  alternative
                                  consequent))
          (else (make <lisp-conditional>
                  :predicate predicate
                  :consequent consequent
                  :alternative alternative))))

  (define (conditional-components conditional receiver)
    (receiver (conditional/predicate conditional)
              (conditional/consequent conditional)
              (conditional/alternative conditional)))

  (define lambda-tag:unnamed (string->symbol "#[unnamed-procedure]"))
  (define lambda-tag:let     (string->symbol "#[let-procedure]"))

  (defclass <lisp-lambda> (<lisp-code>)
    (id       :initarg :name     :initvalue lambda-tag:unnamed :reader lambda/name)
    (required :initarg :required :initvalue '() :reader lambda/required)
    (optional :initarg :optional :initvalue '() :reader lambda/optional)
    (aux      :initarg :aux      :initvalue '() :reader lambda/aux)
    (rest     :initarg :rest     :initvalue #f  :reader lambda/rest)
    (body     :initarg :body                    :reader lambda/body)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display (name-sans-<> (class-name (class-of object))) port)
               (let ((name (lambda/name object)))
                 (cond ((eq? name lambda-tag:unnamed))
                       ((eq? name lambda-tag:let))
                       (else (display " " port)
                             (display name port))))
               (display ">" port)))

  (define (lambda-components lambda receiver)
    (receiver (lambda/name lambda)
              (lambda/required lambda)
              (lambda/optional lambda)
              (lambda/rest lambda)
              (lambda/aux lambda)
              (lambda/body lambda)))

  (define (make-let/ec variable body)
    (make <lisp-combination>
      :operator (make-absolute-reference 'call-with-escaping-continuation)
      :operands (list (make <lisp-lambda>
                        :required (list variable)
                        :body body))))

  (defclass <lisp-method> (<lisp-code>)
    (class  :initarg :class  :reader method/class)
    (id     :initarg :name   :reader method/name)
    (lambda :initarg :lambda :reader method/lambda)
    (return-type :initarg :return-type :reader method/return-type)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (display " " port)
               (display (method/class object) port)
               (display " " port)
               (display (method/name  object) port)
               (display ">" port)))

  (define (method-components method receiver)
    (receiver (method/class  method)
              (method/name   method)
              (method/lambda method)))

  (defclass <lisp-sequence> (<lisp-code>)
    (actions :initarg :actions :reader sequence/actions))

  (defgeneric cons-lisp-sequence (first more))

  (defmethod (cons-lisp-sequence first (more <null>))
    first)

  (defmethod (cons-lisp-sequence first (more <lisp-sequence>))
    (make <lisp-sequence> :actions (cons first (sequence/actions more))))

  (defmethod (cons-lisp-sequence first more)
    (make <lisp-sequence> :actions (list first more)))

  (define (make-lisp-sequence actions)
    (foldr cons-lisp-sequence '() actions))

  (defclass <lisp-variable> (<lisp-code>)
    (id :initarg :name :reader variable/name)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (display " " port)
               (display (variable/name object) port)
               (display ">" port)))

  (define (known-compiler-declaration? thing)
    #f)

  (define (make-declaration declarations expression)
    #f)

  (provide
   <lisp-access>
   <lisp-assignment>
   <lisp-code>
   <lisp-combination>
   <lisp-conditional>
   <lisp-definition>
   <lisp-lambda>
   <lisp-method>
   <lisp-sequence>
   <lisp-variable>
   access-components
   access/environment
   access/name
   assignment-components
   assignment/value
   assignment/variable
   combination-components
   combination/operands
   combination/operator
   conditional-components
   conditional/alternative
   conditional/consequent
   conditional/predicate
   definition-components
   definition/value
   definition/variable
   lambda-components
   lambda-tag:let
   lambda-tag:unnamed
   lambda/aux
   lambda/body
   lambda/name
   lambda/optional
   lambda/required
   lambda/rest
   make-absolute-reference
   make-declaration
   make-let/ec
   make-lisp-conditional
   make-lisp-sequence
   method-components
   method/class
   method/lambda
   method/name
   method/return-type
   sequence/actions
   variable/name
   known-compiler-declaration?
  ))

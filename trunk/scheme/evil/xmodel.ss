#ci(module xmodel (lib "swindle.ss" "swindle")

  (require "generics.ss")
  (require "utils.ss")

;;; A model for compiled lisp code.
  (defclass <compiled-code> ())

  (defclass <declaration-set> ()
    (original :initarg :original :reader declarations/original)
    (declarations :initarg :declarations))

  (define (declarations/make-null)
    (make <declaration-set> :original #f :declarations #f))

  (defclass <compiled-access> (<compiled-code>)
    (environment :initarg :environment :reader access/environment)
    (id          :initarg :name        :reader access/name)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (if (not (access/environment object))
                   (display "#<compiled-absolute-reference " port)
                   (display "#<compiled-access " port))
               (display (access/name object) port)
               (display ">" port)))

  (defclass <compiled-assignment> (<compiled-code>)
    (block    :initarg :block    :reader assignment/block)
    (variable :initarg :variable :reader assignment/variable)
    (value    :initarg :value    :reader assignment/value)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (display " to " port)
               (display (variable/name (assignment/variable object)) port)
               (display ">" port)))

  (defclass <non-local-exit> (<compiled-code>)
    (block    :initarg :block    :reader non-local-exit/block)
    (variable :initarg :variable :reader non-local-exit/variable)
    (body     :initarg :body     :reader non-local-exit/body))

  (define non-local-exit? (class-predicate <non-local-exit>))

  (define (non-local-exit/nearer? left right)
    (block/nearer?
     (variable/block (non-local-exit/variable left))
     (variable/block (non-local-exit/variable right))))

;  (defclass <compiled-break> (<non-local-exit>)
;    (block    :initarg :block    :reader break/block)
;    (variable :initarg :variable :reader break/variable))

  (defclass <compiled-bind-break> (<compiled-code>)
    (block    :initarg :block    :reader bind-break/block)
    (variable :initarg :variable :reader bind-break/variable)
    (body     :initarg :body     :reader bind-break/body))

  (defclass <compiled-bind-continue> (<compiled-code>)
    (block    :initarg :block    :reader bind-continue/block)
    (variable :initarg :variable :reader bind-continue/variable)
    (body     :initarg :body     :reader bind-continue/body))

  (defclass <compiled-bind-return> (<compiled-code>)
    (block    :initarg :block :reader bind-return/block)
    (variable :initarg :block :reader bind-return/variable)
    (body     :initarg :body  :reader bind-return/body))

  (defclass <compiled-block> (<compiled-code>)
    (parent          :initarg :parent          :reader block/parent)
    (children          :initvalue '()          :reader block/children :writer set-block/children!)
    (declarations    :initarg :declarations    :reader block/declarations)
    (bound-variables :initarg :bound-variables :reader block/bound-variables)
    (flags           :initvalue '()))

  (define (%block/make parent bound-variables)
    (make <compiled-block>
      :parent parent
      :declarations (declarations/make-null)
      :bound-variables (cons 0 (make-hash-table))))

  (define (block/make parent bound-variables)
    (let ((block (%block/make parent (cons (length bound-variables) bound-variables))))
      (if parent
          (set-block/children! parent (cons block (block/children parent))))
      block))

  (define (block/nearer? left right)
    (and (block/parent left)
         (or (eq? (block/parent left) right)
             (block/nearer? (block/parent left) right))))

  (defclass <compiled-combination> (<compiled-code>)
    (block    :initarg :block    :reader combination/block)
    (operator :initarg :operator :reader combination/operator)
    (operands :initarg :operands :reader combination/operands))

  (defclass <compiled-conditional> (<compiled-code>)
    (predicate   :initarg :predicate   :reader conditional/predicate)
    (consequent  :initarg :consequent  :reader conditional/consequent)
    (alternative :initarg :alternative :reader conditional/alternative))

  (defclass <compiled-constant> (<compiled-code>)
    (value :initarg :value :reader constant/value))

  (define compiled-constant? (class-predicate <compiled-constant>))

;  (defclass <compiled-continue> (<non-local-exit>)
;    (block    :initarg :block    :reader continue/block)
;    (variable :initarg :variable :reader continue/variable)
;    (body     :initarg :body     :reader continue/body))

  (defclass <compiled-declaration> (<compiled-code>)
    (id :initarg :name :reader compiled-declaration/name)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (display " " port)
               (display (compiled-declaration/name object) port)
               (display ">" port)))

  (defclass <compiled-definition> (<compiled-code>)
    (id    :initarg :name  :reader definition/name)
    (value :initarg :value :reader definition/value))

  (defclass <compiled-enum-declaration> (<compiled-code>)
    (id      :initarg :name    :reader compiled-declaration/name)
    (members :initarg :members :reader compiled-declaration/members))

  (defclass <compiled-goto> (<compiled-code>)
    (target :initarg :target :reader compiled-goto/target))

  ;; For DO, WHILE, or FOR
  (defclass <compiled-iteration> (<compiled-code>)
    (name           :initarg :name           :reader compiled-iteration/name)
    (body           :initarg :body           :reader compiled-iteration/body))

;  (defclass <compiled-let/ec> (<compiled-code>)
;    (block    :initarg :block    :reader compiled-let/ec/block)
;    (variable :initarg :variable :reader compiled-let/ec/variable)
;    (body     :initarg :body     :reader compiled-let/ec/body))

  (defclass <compiled-method> (<compiled-code>)
    (class :initarg :class :reader compiled-method/class)
    (id    :initarg :name  :reader compiled-method/name)
    (body  :initarg :body  :reader compiled-method/body)
    (return-type :initarg :return-type :reader compiled-method/return-type))

  (defclass <compiled-open-block> (<compiled-code>)
    (block)
    (variables)
    (values)
    (actions))

  (define compiled-open-block? (class-predicate <compiled-open-block>))

  (defclass <compiled-procedure> (<compiled-code>)
    (block    :initarg :block                   :reader compiled-procedure/block)
    (name     :initarg :name     :initvalue #f  :reader compiled-procedure/name)
    (required :initarg :required :initvalue '() :reader compiled-procedure/required)
    (optional :initarg :optional :initvalue '() :reader compiled-procedure/optional)
    (rest     :initarg :rest     :initvalue #f  :reader compiled-procedure/rest)
    (body     :initarg :body                    :reader compiled-procedure/body))

  (defclass <compiled-quotation> (<compiled-code>)
    (block      :initarg :block      :reader quotation/block)
    (expression :initarg :expression :reader quotation/expression))

  (defclass <compiled-reference> (<compiled-code>)
    (block    :initarg :block    :reader reference/block)
    (variable :initarg :variable :reader reference/variable)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (display " to " port)
               (display (variable/name (reference/variable object)) port)
               (display ">" port)))

;  (defclass <compiled-return> (<non-local-exit>)
;    (block    :initarg :block    :reader return/block)
;    (variable :initarg :variable :reader return/variable)
;    (value    :initarg :value    :reader return/value))

  (defclass <compiled-sequence> (<compiled-code>)
    (actions :initarg :actions :reader sequence/actions))

  (defclass <compiled-struct-declaration> (<compiled-code>)
    (id      :initarg :name :reader compiled-struct-declaration/name)
    (base    :initarg :base :reader compiled-struct-declaration/base)
    (members :initarg :members :reader compiled-struct-declaration/members))

  (define (instance-variables struct-declaration)
    (cond ((not struct-declaration) '())
          ((instance-of? struct-declaration <compiled-struct-declaration>)
           (append (instance-variables (compiled-struct-declaration/base struct-declaration))
                   (compiled-struct-declaration/members struct-declaration)))
          (else (evil-error 'instance-variables "What is this?" struct-declaration))))

  (defclass <compiled-variable> (<compiled-code>)
    (block :initarg :block :reader variable/block)
    (id    :initarg :name  :reader variable/name)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (display " " port)
               (display (variable/name object) port)
               (display ">" port)))

;;; Binding model
  (defclass <binding> ()
    (variable :initarg :variable :reader binding/variable)
    (type     :initarg :type     :reader binding/type  :writer set-binding/type!)
    (value    :initarg :value    :reader binding/value :writer set-binding/value!)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (display " " port)
               (display (variable/name (binding/variable object)) port)
               (display ">" port)))

  (defclass <lexical-binding> (<binding>))

  (define lexical-binding? (class-predicate <lexical-binding>))

  (defclass <instance-binding> (<binding>))

  (define instance-binding? (class-predicate <instance-binding>))

  (define (binding/name binding)
    (variable/name (binding/variable binding)))

  (define (%block/lookup-name block name)
    (hash-table-get (cdr (block/bound-variables block)) name (lambda () #f)))

  (define (block/lookup-name block name intern?)
    (let search ((block block))
      (or (%block/lookup-name block name)
          (if (block/parent block)
              (search (block/parent block))
              (and intern? (%variable/make&bind! block name #f))))))

  (define (%variable/make&bind! block name type value)
    (let* ((variable (make <compiled-variable> :block block :name name))
           (binding  (make <lexical-binding> :variable variable :type type :value value))
           (bound-variables (block/bound-variables block)))
      (set-car! bound-variables (+ (car bound-variables) 1))
      ;; (set-cdr! bound-variables (cons binding (cdr bound-variables)))
      (hash-table-put! (cdr bound-variables) name binding)
      binding))

  (define (%instance-variable/make&bind! block name type value)
    (let* ((variable (make <compiled-variable> :block block :name name))
           (binding  (make <instance-binding> :variable variable :type type :value value))
           (bound-variables (block/bound-variables block)))
      (set-car! bound-variables (+ (car bound-variables) 1))
      ;; (set-cdr! bound-variables (cons binding (cdr bound-variables)))
      (hash-table-put! (cdr bound-variables) name binding)
      binding))

  (define (variable/make&bind! block name type value)
    (let ((probe (%block/lookup-name block name)))
      (if (not probe)
          (%variable/make&bind! block name type value)
          (begin
            (evil-message 'variable/make&bind! "Rebinding" probe value)
            (set-binding/type!  probe type)
            (set-binding/value! probe value)
            probe))))

  (define (instance-variable/make&bind! block name type value)
    (let ((probe (%block/lookup-name block name)))
      (if (not probe)
          (%instance-variable/make&bind! block name type value)
          (begin
            ;; (evil-message 'variable/make&bind! "Rebinding" probe value)
            (set-binding/type! probe type)
            (set-binding/value! probe value)
            probe))))

  (define (global-ref/make name)
    (make <compiled-access>
      :environment #f
      :name name))

  (define (global-ref? object)
    (and (instance-of? object <compiled-access>)
         (or (not (access/environment object))
             (and (instance-of? (access/environment object) <compiled-constant>)
                  (not (constant/value (access/environment object)))))
         (access/name object)))

  (define (make-sequence actions)
    (if (null? actions)
        (evil-error 'make-sequence "Sequence must have at least one action.")
        (let loop ((revacts '())
                   (acts actions))
          (cond ((null? (cdr acts)) (if (null? revacts)
                                        (car acts)
                                        (make <compiled-sequence>
                                          :actions (reverse (cons (car acts) revacts)))))
                ((instance-of? (car acts) <compiled-sequence>)
                 (loop (append (reverse (sequence/actions (car acts))) revacts) (cdr acts)))
                (else (loop (cons (car acts) revacts) (cdr acts)))))))

  (define (make-sequence* . actions)
    (make-sequence actions))

;;;;;;;;;;;;;;;;
;;;
;;; CONS-EXPRESSION concatenates two expressions into a sequence.
;;; It optimizes certain combinations.
;;;

  (defgeneric cons-expression (block first remaining))

  (defmethod (cons-expression block first remaining)
    (evil-message "cons-expression * *" first remaining)
    (if (not remaining)
        first
        (make-sequence (list first remaining))))

;  (defmethod (cons-expression block first (remaining <compiled-continue>))
;    (evil-message "cons-expression * (continue)" first remaining)
;    (if (exits? first '())
;        (call-next-method)
;        (begin
;          (evil-message "Optimize:  Hoisting continue.")
;          (make <compiled-continue>
;            :block (continue/block remaining)
;            :variable (continue/variable remaining)
;            :body (cons-expression block first (continue/body remaining))))))

;  (defmethod (cons-expression block first (remaining <non-local-exit>))
;    (evil-message "cons-expression * (non-local-exit)" first remaining)
;    (if (exits? first '())
;        (call-next-method)
;        (begin
;          (evil-message "Optimize:  Hoisting non-local-exit.")
;          (make <non-local-exit>
;            :block    (non-local-exit/block remaining)
;            :variable (non-local-exit/variable remaining)
;            :body     (cons-expression block first (non-local-exit/body remaining))))))

  (defmethod (cons-expression block first (remaining <compiled-sequence>))
    (evil-message "cons-expression * (sequence)" first (sequence/actions remaining))
    (make-sequence (cons first (sequence/actions remaining))))

  (defmethod (cons-expression block (first <compiled-sequence>) remaining)
    (evil-message "cons-expression (sequence) *" (sequence/actions first) remaining)
    (foldr (lambda (expr tail)
             (cons-expression block expr tail))
           remaining
           (sequence/actions first)))

  (define (try-move-tail block conditional remaining if-success if-fail)
    (cond ((not (instance-of? (conditional/consequent conditional) <non-local-exit>)) (if-fail))
          ((not (conditional/alternative conditional))
           (evil-message "Optimize:  Moving block tail into alternative of 1-arm returning conditional.")
           (if-success (make-conditional (conditional/predicate conditional)
                                         (conditional/consequent conditional)
                                         remaining)))
          ((instance-of? (conditional/alternative conditional) <compiled-conditional>)
           (try-move-tail
            block (conditional/alternative conditional) remaining
            (lambda (new-alternative)
              (if-success (make-conditional (conditional/predicate conditional)
                                            (conditional/consequent conditional)
                                            new-alternative)))
            if-fail))
          (else (if-fail))))

  (defmethod (cons-expression block (first <compiled-conditional>) remaining)
    (evil-message "cons-expression (conditional) *")
    (try-move-tail block first remaining identity call-next-method))




;;;;;;;;;;;;;;;;;;;;;

  (define (make-class-of environment block object)
    (make-combination*
     block
     (global-ref/make 'nth-value)
     (make <compiled-constant> :value 0)
     (make <compiled-combination>
       :block block
       :operator (global-ref/make 'object-info)
       :operands (list object))))

  (define (make-class-ref block object kind field)
    (make-combination* block (global-ref/make 'instance-ref) (make <compiled-constant> :value kind) object field)

    ;; this version is more technically correct, but the above could be a macro.
    #||
    (if (or (instance-of? object <compiled-reference>)
            (instance-of? object <compiled-access>))
        (make <compiled-combination>
          :block block
          :operator (make <compiled-combination>
                      :operator (global-ref/make 'class-field-accessor)
                      :operands (list (make-class-of environment block object)
                                      field))
          :operands (list object))

        (let* ((inner-block (block/make block '()))
               (object-var (variable/make&bind! inner-block 'object #f)))

          (make <compiled-combination>
            :block block
            :operator (make <compiled-procedure>
                        :name (make <compiled-variable> :name lambda-tag:let)
                        :required (list object-var)
                        :body (make-class-ref inner-block (make <compiled-reference> :variable object-var) field))
            :operands (list object))))
    ||#
    )

  (define (make-class-set! block object kind field value)
    (make-combination* block (global-ref/make 'instance-set!) (make <compiled-constant> :value kind) object field value)

    ;; this version is more technically correct, but the above could be a macro.
    #||
    (if (or (instance-of? object <compiled-reference>)
            (instance-of? object <compiled-access>))
        (make <compiled-combination>
          :block block
          :operator (make <compiled-combination>
                      :operator (global-ref/make 'class-field-mutator)
                      :operands (list (make-class-of environment block object)
                                      field))
          :operands (list object value))
        (let* ((inner-block (block/make block '()))
               (object-var (variable/make&bind! inner-block 'object #f)))

          (make <compiled-combination>
            :block block
            :operator (make <compiled-procedure>
                        :name (make <compiled-variable> :name lambda-tag:let)
                        :required (list object-var)
                        :body (make-class-set! inner-block (make <compiled-reference> :variable object-var) field value))
            :operands (list object))))
    ||#
    )

  (define (not-expression? form)
    (and (instance-of? form <compiled-combination>)
         (eq? (global-ref? (combination/operator form)) 'not)
         (pair? (combination/operands form))
         (null? (cdr (combination/operands form)))))

  (define (make-not block form)
    (cond ((not (instance-of? form <compiled-code>))
           (error "Bogus form in make-not."))
          ((not-expression? form)
           (evil-message "Optimize:  Not of a not.")
           (car (combination/operands form)))
          (else (make <compiled-combination>
                  :block block
                  :operator (global-ref/make 'not)
                  :operands (list form)))))

  (define (make-conjunction block forms)
    (if (positive? (foldr (lambda (form tally)
                            (cond ((not (instance-of? form <compiled-code>))
                                   (error "Bogus form in make-conjunction."))
                                   ((not-expression? form)
                                   (add1 tally))
                                  (else (sub1 tally))))
                          0
                          forms))
        (begin
          (evil-message "Optimize:  Inverting a conjunction.")
          (make-not block
                    (make-combination
                     block
                     (global-ref/make 'or)
                     (map (lambda (form)
                            (make-not block form))
                          forms))))
        (make <compiled-combination>
          :block block
          :operator (global-ref/make 'and)
          :operands forms)))

  (define (make-disjunction block forms)
    (if (positive? (foldr (lambda (form tally)
                            (cond ((not (instance-of? form <compiled-code>))
                                   (error "Bogus form in make-disjunction."))
                                  ((not-expression? form)
                                   (add1 tally))
                                  (else
                                   (sub1 tally))))
                          0
                          forms))
        (begin
          (evil-message "Optimize:  Inverting a disjunction.")
          (make-not
           block
           (make-combination
            block
            (global-ref/make 'and)
            (map (lambda (form)
                   (make-not block form))
                 forms))))
        (make <compiled-combination>
          :block block
          :operator (global-ref/make 'or)
          :operands forms)))

  (define *foldable-operators*
    `((- . ,-)
      (+ . ,+)))

  (define (make-combination block operator operands)
    (cond ((and (every? (lambda (operand)
                          (and (instance-of? operand <compiled-constant>)
                               ;; The compiler inserts bogus strings for things
                               ;; I've punted on.
                               (number? (constant/value operand))))
                        operands)
                (assoc (global-ref? operator) *foldable-operators*))
           => (lambda (folder)
                (evil-message "Optimize:  Constant folding.")
                (make <compiled-constant>
                  :value (apply (cdr folder)
                                (map constant/value operands)))))

          ((eq? (global-ref? operator) 'and)
           (make-conjunction
            block
            (foldr (lambda (operand operands)
                     (cond ((and (instance-of? operand <compiled-combination>)
                                 (eq? (global-ref? (combination/operator operand)) 'and))
                            (append (combination/operands operand) operands))
                           ((and (instance-of? operand <compiled-combination>)
                                 (eq? (global-ref? (combination/operator operand)) 'not)
                                 (instance-of? (car (combination/operands operand)) <compiled-combination>)
                                 (eq? (global-ref? (combination/operator (car (combination/operands operand)))) 'or))
                            (evil-message "Hacking nested and not or")
                            (append (map (lambda (op)
                                           (make-not block op))
                                         (combination/operands (car (combination/operands operand))))
                                    operands))
                           (else (cons operand operands))))
                   '()
                   operands)))

          ((and (eq? (global-ref? operator) 'not)
                (pair? operands)
                (null? (cdr operands)))
           (make-not block (car operands)))

          ((eq? (global-ref? operator) 'or)
           (make-disjunction
            block
            (foldr (lambda (operand operands)
                     (cond ((and (instance-of? operand <compiled-combination>)
                              (eq? (global-ref? (combination/operator operand)) 'or))
                            (append (combination/operands operand) operands))
                           ((and (instance-of? operand <compiled-combination>)
                                 (eq? (global-ref? (combination/operator operand)) 'not)
                                 (instance-of? (car (combination/operands operand)) <compiled-combination>)
                                 (eq? (global-ref? (combination/operator (car (combination/operands operand)))) 'and))
                            (evil-message "Hacking nested or not and")
                            (append (map (lambda (op)
                                           (make-not block op))
                                         (combination/operands (car (combination/operands operand))))
                                    operands))
                           (else (cons operand operands))))
                   '()
                   operands)))

          ((and (eq? (global-ref? operator) '-)
                (pair? operands)
                (instance-of? (car operands) <compiled-combination>)
                (eq? (global-ref? (combination/operator (car operands))) '-))
           (make-combination block operator
                             (append (combination/operands (car operands))
                                     (foldr (lambda (operand operands)
                                              (if (and (instance-of? operand <compiled-combination>)
                                                       (eq? (global-ref? (combination/operator operand)) '+))
                                                  (append (combination/operands operand) operands)
                                                  (cons operand operands)))
                                            '()
                                            (cdr operands)))))

          ((eq? (global-ref? operator) '+)
           (let ((rands (foldr (lambda (operand operands)
                                 (if (and (instance-of? operand <compiled-combination>)
                                          (eq? (global-ref? (combination/operator operand)) '+))
                                     (append (combination/operands operand) operands)
                                     (cons operand operands)))
                               '()
                               operands)))
             (if (every? (lambda (form) (or (not form) (instance-of? form <compiled-code>))) rands)
                 (make <compiled-combination>
                   :block block
                   :operator operator
                   :operands rands)
                 (evil-error "Bogus operands" rands))))

          ((and (instance-of? operator <compiled-procedure>)
                (eq? (variable/name (compiled-procedure/name operator)) lambda-tag:let)
                (null? (compiled-procedure/optional operator))
                (not (compiled-procedure/rest operator))
                (instance-of? (compiled-procedure/body operator) <compiled-sequence>)
                (instance-of? (car (sequence/actions (compiled-procedure/body operator))) <compiled-assignment>))
           ;; This is a let expression with a body that starts with an assigment,
           ;; typically this can be changed into a set of bindings.
           ;; The first loop separates the initialized and uninitialized-variables.
           (let loop ((initialized-variables   '())
                      (initial-values          '())
                      (uninitialized-variables '())
                      (variables-to-check (compiled-procedure/required operator))
                      (values-to-check   operands))
             (cond ((null? variables-to-check)
                    (if (not (null? values-to-check))
                        (evil-error "too few values in let")
                        ;; the second loop peels off the assignments
                        (let loop ((initialized-variables initialized-variables)
                                   (initial-values initial-values)
                                   (uninitialized-variables uninitialized-variables)
                                   (body-forms (sequence/actions (compiled-procedure/body operator))))
                          (if (and (pair? body-forms)
                                   (instance-of? (car body-forms) <compiled-assignment>)
                                   (member (assignment/variable (car body-forms)) uninitialized-variables))
                              (loop (cons (assignment/variable (car body-forms)) initialized-variables)
                                    (cons (assignment/value (car body-forms)) initial-values)
                                    (remove (assignment/variable (car body-forms)) uninitialized-variables)
                                    (cdr body-forms))
                              ;; the third loop sets up the remaning uninitialized-variables
                              (let loop ((initialized-variables initialized-variables)
                                         (initial-values        initial-values)
                                         (uninitialized-variables uninitialized-variables))
                                (cond ((pair? uninitialized-variables)
                                       (loop (cons (car uninitialized-variables) initialized-variables)
                                             (cons #f initial-values)
                                             (cdr uninitialized-variables)))
                                      ((null? uninitialized-variables)
                                       (if (every? (lambda (op) (or (not op) (instance-of? op <compiled-code>)))
                                                   operands)
                                           (make <compiled-combination>
                                             :block block
                                             :operator (make <compiled-procedure>
                                                         :block (compiled-procedure/block operator)
                                                         :name  (compiled-procedure/name  operator)
                                                         :required (reverse initialized-variables)
                                                         :optionals (compiled-procedure/optional operator)
                                                         :rest (compiled-procedure/rest operator)
                                                         :body (make-sequence body-forms))
                                             :operands (reverse initial-values))
                                           (evil-error "bogus operands in make combination")))
                                      (else (evil-error "screwup in make combination"))))))))
                   ((null? values-to-check) (evil-error "too few variables in let"))
                   ((not (car values-to-check)) (loop initialized-variables
                                                      initial-values
                                                      (cons (car variables-to-check) uninitialized-variables)
                                                      (cdr variables-to-check)
                                                      (cdr values-to-check)))
                   ((not (instance-of? (car values-to-check) <compiled-code>))
                    (evil-error "Found bogosity in combination arguments."))
                   (else (loop (cons (car variables-to-check) initialized-variables)
                               (cons (car values-to-check) initial-values)
                               uninitialized-variables
                               (cdr variables-to-check)
                               (cdr values-to-check))))))

          ((and (instance-of? operator <compiled-code>)
                (every? (lambda (op) (or (not op) (instance-of? op <compiled-code>))) operands))
            (make <compiled-combination>
            :block block
            :operator operator
            :operands operands))

          (else (error "bogus call to make-combination"))))

  (define (make-combination* block operator . operands)
    (make-combination
     block
     operator
     operands))

  (define (make-convert-to-boolean block thing)
    (make-combination*
      block
      (global-ref/make '->boolean)
      thing))

  (define (make-convert-from-boolean block thing)
    (make-combination*
      block
      (global-ref/make '->value)
      thing))

  (define lambda-tag:unnamed (string->symbol "#[unnamed-procedure]"))
  (define lambda-tag:let     (string->symbol "#[let-procedure]"))

  (defgeneric make-let (block variable value body))

  (defmethod (make-let block variable value body)
    (evil-message "make-let" block variable value body)
    (make-combination* block (make <compiled-procedure>
                               :name (make <compiled-variable> :name lambda-tag:let)
                               :required (list variable)
                               :body body)
                       value))

  (defmethod (make-let block variable value (body <compiled-combination>))
    (let ((operator (combination/operator body)))
      (if (and (not value)
               (instance-of? operator <compiled-procedure>)
               (eq? (variable/name (compiled-procedure/name operator)) lambda-tag:let))
          (begin
            (evil-message "Optimize:  Adjoining binding to LET.")
            (make-combination block (make <compiled-procedure>
                                      :block    (compiled-procedure/block operator)
                                      :name     (compiled-procedure/name operator)
                                      :required (cons variable (compiled-procedure/required operator))
                                      :optional (compiled-procedure/optional operator)
                                      :rest     (compiled-procedure/rest operator)
                                      :body     (compiled-procedure/body operator))
                              (cons value
                                    (combination/operands body))))
          (call-next-method))))

;  (defmethod (make-let block variable value (body <non-local-exit>))
;    (evil-message "Optimize:  Hoisting non-local-exit out of let.")
;    (make <non-local-exit>
;      :block (non-local-exit/block body)
;      :variable (non-local-exit/variable body)
;      :body (make-let block variable value (non-local-exit/body body))))

  (defmethod (make-let block variable value (body <compiled-sequence>))
    (if value
        (call-next-method)
        (let ((actions (sequence/actions body)))
          (cond ((and (instance-of? (car actions) <compiled-assignment>)
                      (eq? (assignment/variable (car actions)) variable))
                 (let ((value (assignment/value (car actions)))
                       (remaining-body (make-sequence (cdr actions))))
                   (if (and (instance-of? remaining-body <compiled-reference>)
                            (eq? (reference/variable remaining-body) variable))
                       (begin
                         (evil-message "Optimize:  Punting on assignment altogether.")
                         value)
                       (begin
                         (evil-message "Optimize:  Changing assignment into binding.")
                         (make-combination*
                          block
                          (make <compiled-procedure>
                            :name (make <compiled-variable> :name lambda-tag:let)
                            :required (list variable)
                            :body remaining-body)
                          value)))))
                ((not (variable/referenced? variable (car actions)))
                 (evil-message "Optimize:  Moving LET into sequence.")
                 (cons-expression block
                                  (car actions)
                                  (make-let block variable value (make-sequence (cdr actions)))))
                (else
                 (call-next-method))))))

  (defmethod (make-let block variable value (body <compiled-conditional>))
    (cond ((variable/referenced? variable (conditional/predicate body))
           (call-next-method))
          ((or (not (conditional/alternative body))
               (not (variable/referenced? variable (conditional/alternative body))))
           (evil-message "Optimize:  Moving LET inside conditional consequent.")
           (make-conditional (conditional/predicate body)
                             (make-let block variable value (conditional/consequent body))
                             (conditional/alternative body)))
          ((or (not (conditional/consequent body))
               (not (variable/referenced? variable (conditional/consequent body))))
           (evil-message "Optimize:  Moving LET inside conditional alternative.")
           (make-conditional (conditional/predicate body)
                             (conditional/consequent body )
                             (make-let block variable value (conditional/alternative body))))
          (else (call-next-method))))

  (defgeneric make-conditional (predicate consequent alternative))

  (defmethod (make-conditional predicate consequent alternative)
    (make <compiled-conditional>
      :predicate predicate
      :consequent consequent
      :alternative alternative))

  (defmethod :around (make-conditional (predicate <compiled-combination>) consequent (alternative <compiled-code>))
    (if (and (eq? (global-ref? (combination/operator predicate)) 'not)
             (pair? (combination/operands predicate))
             (null? (cdr (combination/operands predicate))))
        (begin (evil-message "Optimize:  Inverting conditional.")
               (make-conditional (car (combination/operands predicate)) alternative consequent))
        (call-next-method)))

  (defmethod (make-conditional predicate (consequent <compiled-conditional>) (alternative (singleton #f)))
    ;; (if e1 (if e2 e3 #f) #f) => (if (and e1 e2) e3 #f)
    (if (not (conditional/alternative consequent))
        (begin
          (evil-message "Optimize:  Rewriting nested conditional as and.")
          (make-conditional (make-combination*
                             #f
                             (global-ref/make 'and)
                             predicate
                             (conditional/predicate consequent))
                            (conditional/consequent consequent)
                            #f))
        (call-next-method)))

  (defmethod (make-conditional predicate (consequent <compiled-combination>) (alternative <compiled-combination>))
    (if (and (pair? (combination/operands consequent))
             (null? (cdr (combination/operands consequent)))
             (pair? (combination/operands alternative))
             (null? (cdr (combination/operands alternative)))
             (global-ref? (combination/operator consequent))
             (eq? (global-ref? (combination/operator consequent))
                  (global-ref? (combination/operator alternative))))
        (begin (evil-message "Optimize:  Moving unary operator outside conditional")
               (make-combination*
                (combination/block consequent)
                (combination/operator consequent)
                (make-conditional predicate
                                  (car (combination/operands consequent))
                                  (car (combination/operands alternative)))))
        (call-next-method)))

  (defmethod (make-conditional predicate (consequent <compiled-assignment>) (alternative <compiled-assignment>))
    (if (eq? (assignment/variable consequent)
             (assignment/variable alternative))
        (begin
          (evil-message "Optimize:  hoisting assignment from conditional")
          (evil-message "Assignment variable is now " (assignment/variable consequent))
          (make <compiled-assignment>
            :block (assignment/block consequent)
            :variable (assignment/variable consequent)
            :value (make-conditional predicate (assignment/value consequent) (assignment/value alternative))))
        (call-next-method)))

;  (defmethod (make-conditional predicate (consequent <non-local-exit>) (alternative <non-local-exit>))
;    (cond ((eq? (non-local-exit/variable consequent)
;                (non-local-exit/variable alternative))
;           (evil-message "Optimize:  hoisting non-local-exit from conditional")
;           (make <non-local-exit>
;             :block    (non-local-exit/block consequent)
;             :variable (non-local-exit/variable consequent)
;             :body     (make-conditional predicate
;                                         (non-local-exit/body consequent)
;                                         (non-local-exit/body alternative))))
;          ((non-local-exit/nearer? consequent alternative)
;           (evil-message "Optimize:  hoisting nearer non-local-exit from conditional")
;           (make <non-local-exit>
;             :block (non-local-exit/block consequent)
;             :variable (non-local-exit/variable consequent)
;             :body     (make-conditional predicate
;                                         (non-local-exit/body consequent)
;                                         alternative)))
;          ((non-local-exit/nearer? alternative consequent)
;           (evil-message "Optimize:  hoisting nearer non-local-exit from conditional")
;           (make <non-local-exit>
;             :block (non-local-exit/block alternative)
;             :variable (non-local-exit/variable alternative)
;             :body     (make-conditional predicate
;                                         consequent
;                                         (non-local-exit/body alternative))))
;          (else (call-next-method))))

  (defmethod (make-conditional predicate (consequent <compiled-constant>) alternative)
    (cond ((eq? (constant/value consequent) #t)
           (evil-message "Optimize:  Changing conditional into disjunction.")
           (make-combination* #f
                              (global-ref/make 'or)
                              predicate
                              alternative))
          ((eq? (constant/value consequent) #f)
           (evil-message "Optimize:  Changing conditional into conjunction.")
           (make-combination* #f
                              (global-ref/make 'and)
                              (make-not #f predicate)
                              alternative))
          (else (call-next-method))))

  (defmethod (make-conditional predicate consequent (alternative <compiled-constant>))
    (cond ((eq? (constant/value alternative) #f)
           (evil-message "Optimize:  Changing conditional into conjunction.")
           (make-combination* #f
                              (global-ref/make 'and)
                              predicate
                              consequent))
          ((eq? (constant/value alternative) #t)
           (evil-message "Optimize:  Changing conditional into disjunction.")
           (make-combination* #f
                              (global-ref/make 'or)
                              (make-not #f predicate)
                              consequent))
          (else
           (call-next-method))))

  (defmethod (make-conditional predicate (consequent <compiled-constant>) (alternative <compiled-constant>))
    (cond ((and (eq? (constant/value consequent) #t)
                (eq? (constant/value alternative) #f))
           (evil-message "Optimize:  Conditional return of #t #f")
           predicate)
          ((and (eq? (constant/value consequent) #f)
                (eq? (constant/value alternative) #t))
           (evil-message "Optimize:  Conditional return of #f #t")
           (make-not #f predicate))
          (else (call-next-method))))

  (define (make-iteration name body)
    (make <compiled-iteration>
      :name name
      :body body))

;;;
(defgeneric exits? (compiled-code exclusions))

;(defmethod :before (exits? code exclusions)
;  (evil-message "exits? " code))

(defmethod (exits? (code <compiled-access>) exclusions)
  (and (access/environment code)
       (exits? (access/environment code) exclusions)))

(defmethod (exits? (code <compiled-assignment>) exclusions)
  (exits? (assignment/value code) exclusions))

;(defmethod (exits? (code <compiled-break>) exclusions)
;  (not (member (break/variable code) exclusions)))

(defmethod (exits? (code <compiled-bind-break>) exclusions)
  (exits? (bind-break/body code) (cons (bind-break/variable code) exclusions)))

(defmethod (exits? (code <compiled-bind-continue>) exclusions)
  (exits? (bind-continue/body code) (cons (bind-continue/variable code) exclusions)))

(defmethod (exits? (code <compiled-combination>) exclusions)
;  (evil-message "Non-local-exit combination"
;                (combination/operator code)
;                (combination/operands code))
  (or (exits? (combination/operator code) exclusions)
      (any? (lambda (operand)
              (and operand
                   (exits? operand exclusions)))
            (combination/operands code))))

(defmethod (exits? (code <compiled-conditional>) exclusions)
  (or (exits? (conditional/predicate code) exclusions)
      (exits? (conditional/consequent code) exclusions)
      (and (conditional/alternative code)
           (exits? (conditional/alternative code) exclusions))))

(defmethod (exits? (code <compiled-constant>) exclusions)
  #f)

(defmethod (exits? (code <non-local-exit>) exclusions)
  (or (not (member (non-local-exit/variable code) exclusions))
      (and (non-local-exit/body code)
           (exits? (non-local-exit/body code) exclusions))))

(defmethod (exits? (code <compiled-declaration>) exclusions)
  #f)

(defmethod (exits? (code <compiled-definition>) exclusions)
  (exits? (definition/value code) exclusions))

(defmethod (exits? (code <compiled-iteration>) exclusions)
  (exits? (compiled-iteration/body code) exclusions))

(defmethod (exits? (code <compiled-procedure>) exclusions)
  #f)

(defmethod (exits? (code <compiled-quotation>) exclusions)
  (exits? (quotation/expression code) exclusions))

(defmethod (exits? (code <compiled-reference>) exclusions)
  #f)

(defmethod (exits? (code <compiled-sequence>) exclusions)
  (any? (lambda (code) (exits? code exclusions)) (sequence/actions code)))

;(defmethod (exits? (code <compiled-struct-declaration>))
;  #f)

(defmethod (exits? (code <compiled-variable>) exclusions)
  #f)


;;;
;;; exits-or-loops? walks the code to determine if all paths exit the given loop.
(defgeneric exits-or-loops? (loopname compiled-code))

(defmethod (exits-or-loops? variable (code <compiled-sequence>))
  (exits-or-loops? variable (last (sequence/actions code))))

(defmethod (exits-or-loops? variable (code <compiled-combination>))
  (or (and (instance-of? (combination/operator code) <compiled-reference>)
           (eq? (reference/variable (combination/operator code)) variable))
      (and (instance-of? (combination/operator code) <compiled-procedure>)
           (exits-or-loops? variable (compiled-procedure/body (combination/operator code))))))

(defmethod (exits-or-loops? variable (code <compiled-assignment>)) #f)

;;;
;;; variable/referenced? walks the code to determine if a variable is
;;; used within the body.

(defgeneric variable/referenced? (variable compiled-code))

(defmethod (variable/referenced? variable code)
  (evil-error 'variable/referenced? "Unknown code" code))

(defmethod (variable/referenced? variable (code <compiled-code>))
  (evil-error 'variable/referenced? "Unknown code" code))

(defmethod (variable/referenced? variable (code <compiled-access>))
  ;; (evil-message "Variable/referenced?" variable code)
  (and (access/environment code)
       (variable/referenced? variable (access/environment code))))

(defmethod (variable/referenced? variable (code <compiled-assignment>))
  ;; (evil-message "Variable/referenced?" variable code)
  (or (eq? variable (assignment/variable code))
      (variable/referenced? variable (assignment/value code))))

(defmethod (variable/referenced? variable (code <compiled-bind-break>))
;;  (evil-message "Variable/referenced?" variable code)
  (variable/referenced? variable (bind-break/body code)))

(defmethod (variable/referenced? variable (code <compiled-bind-continue>))
;;  (evil-message "Variable/referenced?" variable code)
  (variable/referenced? variable (bind-continue/body code)))

(defmethod (variable/referenced? variable (code <compiled-bind-return>))
;;  (evil-message "Variable/referenced?" variable code)
  (variable/referenced? variable (bind-return/body code)))

;(defmethod (variable/referenced? variable (code <compiled-break>))
;;  (evil-message "Variable/referenced?" variable code)
;  (eq? variable (break/variable code)))

(defmethod (variable/referenced? variable (code <compiled-combination>))
;;  (evil-message "Variable/referenced?" variable code)
  (or (variable/referenced? variable (combination/operator code))
      (any? (lambda (operand)
              (and operand
                   (variable/referenced? variable operand)))
            (combination/operands code))))

(defmethod (variable/referenced? variable (code <compiled-conditional>))
;;  (evil-message "Variable/referenced?" variable code)
  (or (variable/referenced? variable (conditional/predicate code))
      (and (conditional/consequent code)
           (variable/referenced? variable (conditional/consequent code)))
      (and (conditional/alternative code)
           (variable/referenced? variable (conditional/alternative code)))))

(defmethod (variable/referenced? variable (code <compiled-constant>))
;;  (evil-message "Variable/referenced?" variable code)
  #f)

;(defmethod (variable/referenced? variable (code <compiled-continue>))
;;  (evil-message "Variable/referenced?" variable code)
;  (eq? variable (continue/variable code)))

(defmethod (variable/referenced? variable (code <compiled-declaration>))
;;  (evil-message "Variable/referenced?" variable code)
  #f)

(defmethod (variable/referenced? variable (code <compiled-iteration>))
;;  (evil-message "Variable/referenced?" variable code)
  (variable/referenced? variable (compiled-iteration/body code)))

(defmethod (variable/referenced? variable (code <compiled-procedure>))
;;  (evil-message "Variable/referenced?" variable code)
  (variable/referenced? variable (compiled-procedure/body code)))

(defmethod (variable/referenced? variable (code <compiled-reference>))
;;  (evil-message "Variable/referenced?" variable code)
  (eq? variable (reference/variable code)))

(defmethod (variable/referenced? variable (code <non-local-exit>))
;;  (evil-message "Variable/referenced?" variable code)
  (or (eq? variable (non-local-exit/variable code))
      (and (non-local-exit/body code)
           (variable/referenced? variable (non-local-exit/body code)))))

(defmethod (variable/referenced? variable (code <compiled-sequence>))
;;  (evil-message "Variable/referenced?" variable code)
  (any? (lambda (action)
          (variable/referenced? variable action))
        (sequence/actions code)))

  (provide

   <compiled-access>
   <compiled-assignment>
;   <compiled-break>
   <compiled-bind-break>
   <compiled-bind-continue>
   <compiled-bind-return>
   <compiled-block>
   <compiled-combination>
   <compiled-conditional>
   <compiled-constant>
;   <compiled-continue>
   <compiled-declaration>
   <compiled-definition>
   <compiled-enum-declaration>
   <compiled-goto>
   <compiled-iteration>
;   <compiled-let/ec>
   <compiled-method>
   <compiled-open-block>
   <compiled-procedure>
   <compiled-quotation>
   <compiled-reference>
;   <compiled-return>
   <compiled-sequence>
   <compiled-struct-declaration>
   <compiled-variable>
   <lexical-binding>
   <non-local-exit>
   <instance-binding>

   binding/type
   binding/variable
   binding/value
   %block/lookup-name
   bind-break/block
   bind-break/body
   bind-continue/block
   bind-continue/body
   bind-return/block
   bind-return/body
   block/lookup-name
   block/make
   block/declarations
 ;  break/variable
   declarations/original
   definition/name
   definition/value
   compiled-constant?
   compiled-iteration/name
   compiled-iteration/body
   compiled-open-block?
   compiled-procedure/name
   compiled-procedure/required
   compiled-procedure/optional
   compiled-procedure/rest
   compiled-procedure/block
   compiled-procedure/body
   compiled-method/class
   compiled-method/body
   compiled-method/name
   compiled-method/return-type
   cons-expression
;   continue/variable
;   continue/body
   instance-binding?
   instance-variables
   lexical-binding?
   make-combination
   make-combination*
   make-conditional
   make-conjunction
   make-convert-to-boolean
   make-convert-from-boolean
   make-disjunction
   make-let
   make-not
   make-sequence
   make-sequence*
   make-iteration
   non-local-exit?
   non-local-exit/body
   non-local-exit/variable
   reference/variable
   quotation/block
   quotation/expression

   global-ref/make

   make-class-ref
   make-class-set!

   variable/make&bind!
   instance-variable/make&bind!
   variable/referenced?
   set-binding/value!
   ))

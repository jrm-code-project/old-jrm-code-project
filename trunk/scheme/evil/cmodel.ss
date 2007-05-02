#ci(module cmodel (lib "swindle.ss" "swindle")

  (require "utils.ss")
  (require "generics.ss")

;;; Model of C code
  (defclass <c-code> ()
    (labels :initarg :labels :initvalue '() :reader c-code/labels)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (display ">" port)))

  (define *c-identifiers* (make-hash-table 'equal))

  (defclass <c-identifier> (<c-code>)
    (scope :initarg :scope :initvalue #f :reader identifier/scope)
    (base  :initarg :base                :reader identifier/base)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (if (identifier/scope object)
                   (begin (display (identifier/scope object) port)
                          (display "::" port)))
               (display (identifier/base object) port)
               (display ">" port)))

  (define c-identifier? (class-predicate <c-identifier>))

  (define (c-identifier->string id)
    (if (identifier/scope id)
        (string-append (identifier/scope id) "::" (identifier/base id))
        (identifier/base id)))

  (define (make-c-identifier scope base)
    (let ((key (cons scope base)))
      (hash-table-get *c-identifiers* key
                      (lambda ()
                        (let ((new-identifier (make <c-identifier>
                                                :scope scope
                                                :base  base)))
                          (hash-table-put! *c-identifiers* key new-identifier)
                          new-identifier)))))

  (define (symbol->c-identifier symbol)
    (make-c-identifier #f (symbol->string symbol)))

  (defgeneric ->c-identifier (thing))

  (defmethod (->c-identifier (thing <c-identifier>))
    thing)

  (defmethod (->c-identifier (thing <string-like>))
    (make-c-identifier #f thing))

  (defmethod (->c-identifier (thing <symbol>))
    (make-c-identifier #f (symbol->string thing)))

  (define c-identifier:*anonymous* (->c-identifier '*anonymous*))
  (define c-identifier:*ignore*    (->c-identifier '*ignore*))
  (define c-identifier:_int64      (->c-identifier '_int64))
  (define c-identifier:__int64     (->c-identifier '__int64))
  (define c-identifier:__ptr64     (->c-identifier '__ptr64))
  (define c-identifier:__w64       (->c-identifier '__w64))
  (define c-identifier:char        (->c-identifier 'char))
  (define c-identifier:class       (->c-identifier 'class))
  (define c-identifier:const       (->c-identifier 'const))
  (define c-identifier:double      (->c-identifier 'double))
  (define c-identifier:enum        (->c-identifier 'enum))
  (define c-identifier:float       (->c-identifier 'float))
  (define c-identifier:int         (->c-identifier 'int))
  (define c-identifier:long        (->c-identifier 'long))
  (define c-identifier:short       (->c-identifier 'short))
  (define c-identifier:signed      (->c-identifier 'signed))
  (define c-identifier:struct      (->c-identifier 'struct))
  (define c-identifier:union       (->c-identifier 'union))
  (define c-identifier:unsigned    (->c-identifier 'unsigned))
  (define c-identifier:void        (->c-identifier 'void))
  (define c-identifier:volatile    (->c-identifier 'volatile))
  (define c-identifier:wchar_t     (->c-identifier 'wchar_t))

  (define *type-specifier-hash-table* (make-hash-table 'equal))

  (defgeneric type-specifier->list (typespec))

  (defclass <c-type-specifier> (<c-code>)
    (idlist :initarg :idlist :reader type-specifier/idlist)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (for-each (lambda (id)
                           (display " " port)
                           (display (c-identifier->string id) port))
                         (type-specifier/idlist object))
               (display ">" port)))

  (define c-type-specifier? (class-predicate <c-type-specifier>))

  (defmethod (type-specifier->list (typespec <c-type-specifier>))
    (map c-identifier->string (type-specifier/idlist typespec)))

  (defclass <c-wrapper-type-specifier> (<c-type-specifier>)
    (inner :initarg :inner :reader type-specifier/inner))

  (defclass <c-pointer-type-specifier>   (<c-wrapper-type-specifier>))

  (defmethod (type-specifier->list (typespec <c-pointer-type-specifier>))
    (cons :pointer (type-specifier->list (type-specifier/inner typespec))))

  (defclass <c-reference-type-specifier> (<c-wrapper-type-specifier>))

  (defmethod (type-specifier->list (typespec <c-reference-type-specifier>))
    (cons :reference (type-specifier->list (type-specifier/inner typespec))))

  (defclass <c-array-type-specifier>     (<c-wrapper-type-specifier>))

  (defmethod (type-specifier->list (typespec <c-array-type-specifier>))
    (cons :array (type-specifier->list (type-specifier/inner typespec))))

  (defclass <c-function-type-specifier>  (<c-type-specifier>)
    (return-type :initarg :return-type :reader function-type-specifier/return-type)
    (argtypes    :initarg :argtypes    :reader function-type-specifier/argtypes))

  (defmethod (type-specifier->list (typespec <c-function-type-specifier>))
    (list :function
          (type-specifier->list (function-type-specifier/return-type typespec))
          (map type-specifier->list (function-type-specifier/argtypes typespec))))

  (define (list->type-specifier list)
    (cond ((null? list) (error "No elements in type specifier"))
          ((not (pair? list)) (make <c-type-specifier> :idlist (cons (->c-identifier list) '())))
          ((eq? (car list) :pointer) (make <c-pointer-type-specifier>
                                       :inner (list->type-specifier (cdr list))))
          ((eq? (car list) :array) (make <c-array-type-specifier>
                                     :inner (list->type-specifier (cdr list))))
          ((eq? (car list) :reference) (make <c-reference-type-specifier>
                                         :inner (list->type-specifier (cdr list))))
          ((eq? (car list) :function) (make <c-function-type-specifier>
                                        :inner (list->type-specifier (cdr list))))
          (else (make <c-type-specifier>
                  :idlist (map ->c-identifier list)))))

  (define (make-type-specifier key)
    (hash-table-get *type-specifier-hash-table* key
                    (lambda ()
                      (let ((new-spec (list->type-specifier key)))
                        (hash-table-put! *type-specifier-hash-table* key new-spec)
                        new-spec))))

  (define (make-type-specifier* . rest)
    (make-type-specifier rest))

  (define c-type:_int64           (make-type-specifier* c-identifier:_int64))
  (define c-type:__int64          (make-type-specifier* c-identifier:__int64))
  (define c-type:__ptr64          (make-type-specifier* c-identifier:__ptr64))
  (define c-type:__w64-int        (make-type-specifier* c-identifier:__w64 c-identifier:int))
  (define c-type:__w64-long       (make-type-specifier* c-identifier:__w64 c-identifier:long))
  (define c-type:__w64-uint       (make-type-specifier* c-identifier:__w64 c-identifier:unsigned c-identifier:int))
  (define c-type:__w64-ulong      (make-type-specifier* c-identifier:__w64 c-identifier:unsigned c-identifier:long))
  (define c-type:__w64-unsigned   (make-type-specifier* c-identifier:__w64 c-identifier:unsigned))
  (define c-type:anonymous-enum   (make-type-specifier* c-identifier:enum  c-identifier:*anonymous*))
  (define c-type:anonymous-struct (make-type-specifier* c-identifier:struct c-identifier:*anonymous*))
  (define c-type:anonymous-union  (make-type-specifier* c-identifier:union c-identifier:*anonymous*))
  (define c-type:char             (make-type-specifier* c-identifier:char))
  (define c-type:double           (make-type-specifier* c-identifier:double))
  (define c-type:float            (make-type-specifier* c-identifier:float))
  (define c-type:int              (make-type-specifier* c-identifier:int))
  (define c-type:long             (make-type-specifier* c-identifier:long))
  (define c-type:long-double      (make-type-specifier* c-identifier:long c-identifier:double))
  (define c-type:long-long        (make-type-specifier* c-identifier:long c-identifier:long))
  (define c-type:schar            (make-type-specifier* c-identifier:signed c-identifier:char))
  (define c-type:short            (make-type-specifier* c-identifier:short))
  (define c-type:short-int        (make-type-specifier* c-identifier:short c-identifier:int))
  (define c-type:signed           (make-type-specifier* c-identifier:signed))
  (define c-type:signed-__int64   (make-type-specifier* c-identifier:signed c-identifier:__int64))
  (define c-type:sint             (make-type-specifier* c-identifier:signed c-identifier:int))
  (define c-type:slong            (make-type-specifier* c-identifier:signed c-identifier:long))
  (define c-type:sshort           (make-type-specifier* c-identifier:signed c-identifier:short))
  (define c-type:uchar            (make-type-specifier* c-identifier:unsigned c-identifier:char))
  (define c-type:uint             (make-type-specifier* c-identifier:unsigned c-identifier:int))
  (define c-type:ulong            (make-type-specifier* c-identifier:unsigned c-identifier:long))
  (define c-type:unsigned         (make-type-specifier* c-identifier:unsigned))
  (define c-type:unsigned-__int64 (make-type-specifier* c-identifier:unsigned c-identifier:__int64))
  (define c-type:ushort           (make-type-specifier* c-identifier:unsigned c-identifier:short))
  (define c-type:ushort-wchar     (make-type-specifier* c-identifier:unsigned
                                                        c-identifier:short c-identifier:wchar_t))
  (define c-type:void             (make-type-specifier* c-identifier:void))
  (define c-type:wchar_t          (make-type-specifier* c-identifier:wchar_t))

  (define (make-union-type . rest)
    (make-type-specifier (cons c-identifier:union rest)))

  (define (make-enum-type . rest)
    (make-type-specifier (cons c-identifier:enum rest)))

  (define (make-structure-type . rest)
    (make-type-specifier (cons c-identifier:struct rest)))

  (define (make-class-type . rest)
    (make-type-specifier (cons c-identifier:class rest)))

;;; Keeps a table of type-name to type.
  (defclass <type-table> ()
    (entries :initarg :entries :reader type-table/entries))

  (define (initial-type-table)
    (make <type-table> :entries (make-hash-table 'equal)))

  (define (lookup-type type-table name)
    (hash-table-get (type-table/entries type-table) name (lambda () #f)))

  (define (declare-type! type-table name value)
    ;; (evil-message 'declare-type! (c-identifier->string name))
    (let ((probe (lookup-type type-table name)))
      (cond ((not probe) (hash-table-put! (type-table/entries type-table) name value))
            ((and (c-struct-declaration? probe)
                  (incomplete-struct-declaration? value))
             (evil-message "Ignoring redefinition of" (c-identifier->string name) "from" probe "to" value))
            ((and (incomplete-struct-declaration? probe)
                  (c-struct-declaration? value)
                  (equal? (declaration/expand-type probe) (declaration/expand-type value)))
             (hash-table-put! (type-table/entries type-table) name value))
            (else
             (evil-message "Redefining" (c-identifier->string name) "from" probe "to" value)
             (hash-table-put! (type-table/entries type-table) name value))))
    type-table)

  (defclass <declarator> (<c-code>))

  (define declarator? (class-predicate <declarator>))

  (defgeneric declarator/identifier (declarator))
  (defgeneric declarator/expand-type (declarator type))

  (defclass <direct-declarator> (<declarator>)
    (identifier :initarg :identifier :reader declarator/identifier)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (display " " port)
               (display (c-identifier->string (declarator/identifier object)) port)
               (display ">" port)))

  (defmethod (declarator/expand-type (decl <direct-declarator>) type)
    (let ((info (type-specifier->list type)))
      (if (and (pair? info)
               (null? (cdr info)))
          (car info)
          info)))

  (defclass <wrapping-declarator> (<declarator>)
    (inner-declarator :initarg :inner-declarator :reader inner-declarator))

  (defmethod (declarator/identifier (declarator <wrapping-declarator>))
    (declarator/identifier (inner-declarator declarator)))

  (define (expand-wrapper-type prefix decl type extras)
    `(,prefix ,@extras ,(declarator/expand-type (inner-declarator decl) type)))

  (defclass <array-declarator> (<wrapping-declarator>)
    (size :initarg :size :reader array-declarator/size))

  (defmethod (declarator/expand-type (decl <array-declarator>) type)
    (expand-wrapper-type
     :array
     decl type (if (array-declarator/size decl)
                   (list :size (array-declarator/size decl))
                   '())))

  (defclass <function-declarator> (<wrapping-declarator>)
    (arglist :initarg :arglist :reader arglist))

  (define function-declarator? (class-predicate <function-declarator>))

  (defmethod (declarator/expand-type (decl <function-declarator>) type)
    `(:function;; :arglist ,(arglist decl)
      :returning ,(declarator/expand-type (inner-declarator decl) type)))

  (define (make-function-declarator inner arglist)
    (make <function-declarator>
      :inner-declarator inner
      :arglist arglist))

  (defclass <method-declarator> (<function-declarator>))

  (defmethod (declarator/expand-type (decl <method-declarator>) type)
    `(:method;; :arglist ,(arglist decl)
      :returning ,(declarator/expand-type (inner-declarator decl) type)))

  (defclass <pointer-declarator> (<wrapping-declarator>)
    (qualifier :initarg :qualifier :reader declarator/qualifier))

  (defmethod (declarator/expand-type (decl <pointer-declarator>) type)
    (expand-wrapper-type
     :pointer
     decl type (if (declarator/qualifier decl)
                   (list (declarator/qualifier decl))
                   '())))

  (defclass <reference-declarator> (<wrapping-declarator>)
    (qualifier :initarg :qualifier :reader declarator/qualifier))

  (defmethod (declarator/expand-type (decl <reference-declarator>) type)
    (expand-wrapper-type
     :reference
     decl type  (if (declarator/qualifier decl)
                    (list (declarator/qualifier decl))
                    '())))

  (defclass <c-declaration> (<c-code>)
    (storage-class :initarg :storage-class :reader declaration/storage-class)
    (qualifier     :initarg :qualifier     :reader declaration/qualifier)
    (type          :initarg :type          :reader declaration/type)
    (declarator    :initarg :declarator    :reader declaration/declarator)
    (initial-value :initvalue #f
                   :reader declaration/initial-value
                   :writer set-declaration/initial-value!)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (display " " port)
               (display (declaration/storage-class object) port)
               (display " " port)
               (display (c-identifier->string (declaration/identifier object)) port)
               (display " " port)
               (display (declaration/expand-type object) port)
               (display ">" port)))

  (define (make-c-declaration labels storage-class qualifier type declarator)
    (unless (c-type-specifier? type)
      (evil-error 'make-c-declaration "bad type specifier" type))
    (unless (declarator? declarator)
      (evil-error 'make-c-declaration "bad declarator" declarator))
    (make <c-declaration>
      :labels labels
      :storage-class storage-class
      :qualifier qualifier
      :type type
      :declarator declarator))

  (defmethod (declaration/identifier (declaration <c-declaration>))
    (declarator/identifier (declaration/declarator declaration)))

  (define (declaration/expand-type declaration)
    (declarator/expand-type (declaration/declarator declaration) (declaration/type declaration)))

  (defclass <incomplete-struct-declaration> (<c-declaration>)
    (identifier :initarg :identifier :reader declaration/identifier)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (display " " port)
               (display (declaration/expand-type object) port)
               (display "(incomplete)>" port)))

  (define incomplete-struct-declaration? (class-predicate <incomplete-struct-declaration>))

  (defclass <c-enum-declaration> (<c-declaration>)
    (members :initarg :members :reader declaration/members))

  (defclass <c-struct-declaration> (<c-declaration>)
    (base-classes :initarg :base-classes :reader base-classes)
    (members :initarg :members :reader declaration/members))

  (define c-struct-declaration? (class-predicate <c-struct-declaration>))

  (defclass <c-function-definition> (<c-declaration>)
    (body :initarg :body :reader definition/body)
    :printer  (lambda (object esc? &optional (port (current-output-port)))
                (display "#<" port)
                (display (name-sans-<> (class-name (class-of object))) port)
                (display " " port)
                (display (c-identifier->string (declaration/identifier object)) port)
                (display " " port)
                (display (declaration/expand-type object) port)
                (display ">" port)))

  (define (function-definition/return-type fundef)
    ;; yuck
    (third (declaration/expand-type fundef)))

  (defclass <c-method-definition> (<c-function-definition>))

  (defclass <c-translation-unit> (<c-code>)
    (contents :initarg :contents :reader translation-unit/contents))

  (defclass <c-statement> (<c-code>))

  (defclass <c-assignment> (<c-statement>)
    (operator :initarg :operator :reader assignment/operator)
    (place    :initarg :place    :reader assignment/place)
    (value    :initarg :value    :reader assignment/value))

  (defclass <c-block> (<c-statement>)
    (contents :initarg :contents :reader block-contents))

  (define (accumulate-block statement block)
    (make <c-block> :labels (c-code/labels block) :contents (cons statement (block-contents block))))

  (defclass <c-break> (<c-statement>))

  (defclass <c-continue> (<c-statement>))

  (defclass <c-delete> (<c-statement>)
    (place :initarg :place :reader delete/place))

  (defclass <c-do-while> (<c-statement>)
    (body       :initarg :body       :reader do-while/body)
    (predicate  :initarg :predicate  :reader do-while/predicate))

  (defclass <c-empty-statement> (<c-statement>))

  (defclass <c-expression-list> (<c-statement>)
    (actions :initarg :actions :reader expression-list/actions))

  (defclass <c-if> (<c-statement>)
    (predicate   :initarg :predicate :reader conditional/predicate)
    (consequent  :initarg :consequent :reader conditional/consequent)
    (alternative :initarg :alternative :initvalue #f :reader conditional/alternative))

  (defclass <c-for> (<c-statement>)
    (initialize :initarg :initialize :reader for/initialize)
    (predicate  :initarg :predicate  :reader for/predicate)
    (step       :initarg :step       :reader for/step)
    (body       :initarg :body       :reader for/body))

  (defclass <c-goto> (<c-statement>)
    (target :initarg :target :reader goto/target))

  (defclass <c-while> (<c-statement>)
    (predicate  :initarg :predicate  :reader while/predicate)
    (body       :initarg :body       :reader while/body))

  (defclass <c-switch> (<c-statement>)
    (expression  :initarg :expression :reader switch/expression)
    (body        :initarg :body       :reader switch/body))

  (defclass <return> (<c-statement>)
    (return-value :initarg :return-value :reader return/value))

  (defclass <c-expression> (<c-code>))

  (defclass <c-this> (<c-expression>))

  (defclass <c-void-expression> (<c-expression>))

  (defclass <c-operator> (<c-code>)
    (name          :initarg :name          :reader operator/name)
    (symbol        :initarg :symbol        :reader operator/symbol)
    (precedence    :initarg :precedence    :reader operator/precedence)
    (associativity :initarg :associativity :reader operator/associativity)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (display " " port)
               (display (operator/name object) port)
               (display ">" port)))

  (defclass <c-unary-operator> (<c-operator>))
  (defclass <c-binary-operator> (<c-operator>))
  (defclass <c-relational-operator> (<c-operator>))
  (defclass <c-trinary-operator> (<c-operator>))

  (define c-operator:ampersand
    (make <c-unary-operator>
      :name "&"
      :symbol 'address-of
      :precedence 2
      :associativity :right->left))

  (define c-operator:asterisk
    (make <c-unary-operator>
      :name "*"
      :symbol 'asterisk
      :precedence 2
      :associativity :right->left))

  (define c-operator:cast
    (make <c-unary-operator>
      :name "cast"
      :symbol 'cast
      :precedence 2
      :associativity :right->left))

  (define c-operator:complement
    (make <c-unary-operator>
      :name "~"
      :symbol 'complement
      :precedence 2
      :associativity :right->left))

  (define c-operator:minus
    (make <c-unary-operator>
      :name "-"
      :symbol '-
      :precedence 2
      :associativity :right->left))

  (define c-operator:arrow-star
    (make <c-binary-operator>
      :name "->*"
      :symbol 'pointer-to-member-pointer
      :precedence 3
      :associativity :left->right))

  (define c-operator:dot-star
    (make <c-binary-operator>
      :name ".*"
      :symbol 'pointer-to-member
      :precedence 3
      :associativity :left->right))

  (define c-operator:plus
    (make <c-unary-operator>
      :name "+"
      :symbol '+
      :precedence 2
      :associativity :right->left))

  (define c-operator:sizeof
    (make <c-unary-operator>
      :name "sizeof"
      :symbol 'sizeof
      :precedence 2
      :associativity :right->left))

  (define c-operator:pre-increment
    (make <c-unary-operator>
      :name "++"
      :symbol 'pre-increment
      :precedence 2
      :associativity :right->left))

  (define c-operator:pre-decrement
    (make <c-unary-operator>
      :name "--"
      :symbol 'pre-decrement
      :precedence 2
      :associativity :right->left))

  (define c-operator:post-increment
    (make <c-unary-operator>
      :name "++"
      :symbol 'post-increment
      :precedence 1
      :associativity :left->right))

  (define c-operator:post-decrement
    (make <c-unary-operator>
      :name "--"
      :symbol 'post-decrement
      :precedence 1
      :associativity :left->right))

  (define c-operator:add
    (make <c-binary-operator>
      :name "add"
      :symbol '+
      :precedence 5
      :associativity :left->right))

  (define c-operator:subtract
    (make <c-binary-operator>
      :name "subtract"
      :symbol '-
      :precedence 5
      :associativity :left->right))

  (define c-operator:shift-left
    (make <c-binary-operator>
      :name "<<"
      :symbol '<<
      :precedence 6
      :associativity :left->right))

  (define c-operator:shift-right
    (make <c-binary-operator>
      :name ">>"
      :symbol '>>
      :precedence 6
      :associativity :left->right))

  (define c-operator:multiply
    (make <c-binary-operator>
      :name "multiply"
      :symbol '*
      :precedence 4
      :associativity :left->right))

  (define c-operator:modulus
    (make <c-binary-operator>
      :name "%"
      :symbol 'mod
      :precedence 4
      :associativity :left->right))

  (define c-operator:divide
    (make <c-binary-operator>
      :name "divide"
      :symbol '/
      :precedence 4
      :associativity :left->right))

  (define c-operator:dot
    (make <c-binary-operator>
      :name "."
      :symbol 'dot
      :precedence 42
      :associativity :left->right))

  (define c-operator:and-bits
    (make <c-binary-operator>
      :name "&"
      :symbol 'bitwise-and
      :precedence 9
      :associativity :left->right))

  (define c-operator:xor-bits
    (make <c-binary-operator>
      :name "^"
      :symbol 'bitwise-xor
      :precedence 10
      :associativity :left->right))

  (define c-operator:or-bits
    (make <c-binary-operator>
      :name "|"
      :symbol 'bitwise-or
      :precedence 11
      :associativity :left->right))

  (defclass <c-array-operator> (<c-binary-operator>))

  (define c-operator:array-reference
    (make <c-array-operator>
      :name "[]"
      :symbol 'vector-ref
      :precedence 1
      :associativity :left->right))

  (defclass <c-arrow-operator> (<c-binary-operator>))

  (define c-operator:arrow
    (make <c-arrow-operator>
      :name "->"
      :symbol 'arrow
      :precedence 1
      :associativity :left->right))

  (define c-operator:and
    (make <c-relational-operator>
      :name "&&"
      :symbol 'and
      :precedence 12
      :associativity :left->right))

  (define c-operator:equal
    (make <c-relational-operator>
      :name "=="
      :symbol 'eq?
      :precedence 8
      :associativity :left->right))

  (define c-operator:not-equal
    (make <c-relational-operator>
      :name "!="
      :symbol 'neq?
      :precedence 8
      :associativity :left->right))

  (define c-operator:less-than
    (make <c-relational-operator>
      :name "<"
      :symbol '<
      :precedence 7
      :associativity :left->right))

  (define c-operator:less-equal
    (make <c-relational-operator>
      :name "<="
      :symbol '<=
      :precedence 7
      :associativity :left->right))

  (define c-operator:greater-than
    (make <c-relational-operator>
      :name ">"
      :symbol '>
      :precedence 7
      :associativity :left->right))

  (define c-operator:greater-equal
    (make <c-relational-operator>
      :name ">="
      :symbol '>=
      :precedence 7
      :associativity :left->right))

  (define c-operator:not
    (make <c-relational-operator>
      :name "!"
      :symbol 'not
      :precedence 2
      :associativity :right->left))

  (define c-operator:or
    (make <c-relational-operator>
      :name "||"
      :symbol 'or
      :precedence 13
      :associativity :left->right))

  (define c-operator:conditional
    (make <c-trinary-operator>
      :name "?:"
      :symbol 'conditional
      :precedence 14
      :associativity :left->right))

  (defclass <c-unary-expression> (<c-expression>)
    (operator :initarg :operator :reader expression/operator)
    (operand  :initarg :operand  :reader expression/operand)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (display " " port)
               (display (operator/name (expression/operator object)) port)
               (display ">" port)))

  (defclass <c-unary-modify> (<c-unary-expression>))

  (defclass <c-binary-expression> (<c-expression>)
    (operator :initarg :operator :reader expression/operator)
    (left  :initarg :left  :reader left)
    (right :initarg :right :reader right)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (display " " port)
               (display (operator/name (expression/operator object)) port)
               (display ">" port)))

  (define c-binary-expression? (class-predicate <c-binary-expression>))

  (defclass <c-array-expression> (<c-binary-expression>)
    (operator :allocation :class :initvalue c-operator:array-reference))
  ;; :default-initargs (:operator c-operator:array-reference))

  (defclass <c-arrow-expression> (<c-binary-expression>)
    (operator :allocation :class :initvalue c-operator:arrow))
    ;; :default-initargs (:operator c-operator:arrow))

  (define c-arrow-expression? (class-predicate <c-arrow-expression>))

  (defclass <c-dot-expression> (<c-binary-expression>)
    (operator :allocation :class :initvalue c-operator:dot))
    ;; :default-initargs (:operator c-operator:dot))

  (define c-dot-expression? (class-predicate <c-dot-expression>))

  (defclass <c-asterisk-expression> (<c-unary-expression>)
    (operator :allocation :class :initvalue c-operator:asterisk))
;;    :default-initargs (:operator c-operator:asterisk))

  (defclass <c-sizeof-expression> (<c-unary-expression>)
    (operator :allocation :class :initvalue c-operator:sizeof))
;;    :default-initargs (:operator c-operator:sizeof))

  (defclass <c-relational-expression> (<c-expression>)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (display " ")
               (display (operator/name (expression/operator object)) port)
               (display ">" port)))

  (defclass <c-relational-unary-expression> (<c-relational-expression> <c-unary-expression>))

  (defclass <c-relational-unary-combination> (<c-relational-expression> <c-unary-expression>))

  (defclass <c-relational-binary-expression> (<c-relational-expression> <c-binary-expression>))

  (defclass <c-relational-binary-combination> (<c-relational-expression> <c-binary-expression>))

  (defclass <c-trinary-expression> (<c-expression>)
    (operator :initarg :operator    :reader expression/operator)
    (left     :initarg :test        :reader left)
    (middle   :initarg :consequent  :reader middle)
    (right    :initarg :alternative :reader right)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (display " " port)
               (display (operator/name (expression/operator object)) port)
               (display ">" port)))

  (defclass <c-conditional-expression> (<c-trinary-expression>)
    (operator :allocation :class :initarg c-operator:conditional))
  ;; :default-initargs (:operator c-operator:conditional))

  (define c-conditional-expression? (class-predicate <c-conditional-expression>))

  (defclass <c-cast> (<c-unary-expression>)
    (operator :allocation :class :initarg c-operator:cast)
    (type :initarg :type :reader c-cast/type))

  (defclass <c-cast-expression> (<c-cast>))
  ;; :default-initargs (:operator c-operator:cast))

  (define c-cast-expression? (class-predicate <c-cast-expression>))

  (defclass <c-reinterpret-cast-expression> (<c-cast>))

  (defclass <c-expression-sequence> (<c-expression>)
    (actions :initarg :actions :reader actions))

  (defclass <c-new> (<c-expression>)
    (type :initarg :type :reader c-new/type)
    (new-parameters :initarg :parameters :initvalue #f :reader c-new/parameters)
    (arguments :initarg :arguments :reader c-new/operands))

  (defclass <c-new-array> (<c-expression>)
    (type :initarg :type)
    (size :initarg :size :reader c-new-array/size)
    (arguments :initarg :arguments))

  (defclass <c-literal> (<c-expression>)
    (value :initarg :value :reader value))

  (defclass <c-call> (<c-expression>)
    (operator :initarg :operator :reader expression/operator)
    (operands :initarg :operands :reader expression/operands))

  (defclass <c-funcall> (<c-call>))

  (defclass <c-method-call> (<c-call>))

  (defclass <c-variable> (<c-expression>)
    (id :initarg :name :reader variable/name)
    :printer (lambda (object esc? &optional (port (current-output-port)))
               (display "#<" port)
               (display (name-sans-<> (class-name (class-of object))) port)
               (display " " port)
               (display (c-identifier->string (variable/name object)) port)
               (display ">" port)))

  (define c-variable? (class-predicate <c-variable>))

  (provide
   ->c-identifier
   <array-declarator>
   <c-array-expression>
   <c-arrow-expression>
   <c-assignment>
   <c-asterisk-expression>
   <c-binary-expression>
   <c-block>
   <c-break>
   <c-cast-expression>
   <c-code>
   <c-conditional-expression>
   <c-continue>
   <c-declaration>
   <c-delete>
   <c-do-while>
   <c-dot-expression>
   <c-empty-statement>
   <c-enum-declaration>
   <c-expression-list>
   <c-expression-sequence>
   <c-expression>
   <c-for>
   <c-funcall>
   <c-function-definition>
   <c-function-type-specifier>
   <c-goto>
   <c-identifier>
   <c-if>
   <c-literal>
   <c-method-call>
   <c-method-definition>
   <c-new-array>
   <c-new>
   <c-pointer-type-specifier>
   <c-reinterpret-cast-expression>
   <c-relational-binary-combination>
   <c-relational-binary-expression>
   <c-relational-expression>
   <c-relational-unary-combination>
   <c-relational-unary-expression>
   <c-sizeof-expression>
   <c-statement>
   <c-struct-declaration>
   <c-switch>
   <c-this>
   <c-translation-unit>
   <c-trinary-expression>
   <c-type-specifier>
   <c-unary-expression>
   <c-unary-modify>
   <c-variable>
   <c-void-expression>
   <c-while>
   <direct-declarator>
   <function-declarator>
   <incomplete-struct-declaration>
   <method-declarator>
   <pointer-declarator>
   <reference-declarator>
   <return>
   accumulate-block
   actions
   arglist
   assignment/operator
   assignment/place
   assignment/value
   base-classes
   block-contents
   c-arrow-expression?
   c-binary-expression?
   c-cast/type
   c-cast-expression?
   c-code/labels
   c-conditional-expression?
   c-dot-expression?
   c-identifier->string
   c-identifier:*anonymous*
   c-identifier:*ignore*
   c-identifier:__int64
   c-identifier:_int64
   c-identifier:__ptr64
   c-identifier:__w64
   c-identifier:char
   c-identifier:class
   c-identifier:const
   c-identifier:double
   c-identifier:enum
   c-identifier:float
   c-identifier:int
   c-identifier:long
   c-identifier:short
   c-identifier:signed
   c-identifier:struct
   c-identifier:union
   c-identifier:unsigned
   c-identifier:void
   c-identifier:volatile
   c-identifier:wchar_t
   c-identifier?
   c-new-array/size
   c-new/operands
   c-new/type
   c-operator:add
   c-operator:ampersand
   c-operator:and
   c-operator:and-bits
   c-operator:complement
   c-operator:divide
   c-operator:equal
   c-operator:greater-equal
   c-operator:greater-than
   c-operator:less-equal
   c-operator:less-than
   c-operator:minus
   c-operator:modulus
   c-operator:multiply
   c-operator:not
   c-operator:not-equal
   c-operator:or
   c-operator:or-bits
   c-operator:plus
   c-operator:post-decrement
   c-operator:post-increment
   c-operator:pre-decrement
   c-operator:pre-increment
   c-operator:shift-left
   c-operator:shift-right
   c-operator:subtract
   c-struct-declaration?
   c-type:_int64
   c-type:__int64
   c-type:__ptr64
   c-type:__w64-int
   c-type:__w64-long
   c-type:__w64-uint
   c-type:__w64-ulong
   c-type:__w64-unsigned
   c-type:anonymous-enum
   c-type:anonymous-struct
   c-type:anonymous-union
   c-type:char
   c-type:double
   c-type:float
   c-type:int
   c-type:long
   c-type:long-double
   c-type:long-long
   c-type:schar
   c-type:short
   c-type:short-int
   c-type:signed
   c-type:signed-__int64
   c-type:sint
   c-type:slong
   c-type:sshort
   c-type:uchar
   c-type:uint
   c-type:ulong
   c-type:unsigned
   c-type:unsigned-__int64
   c-type:ushort
   c-type:ushort-wchar
   c-type:void
   c-type:wchar_t
   c-type-specifier?
   c-variable?
   declaration/declarator
   declaration/expand-type
   declaration/identifier
   declaration/initial-value
   declaration/members
   declaration/qualifier
   declaration/storage-class
   declaration/type
   declarator?
   declarator/identifier
   declare-type!
   delete/place
   definition/body
   do-while/body
   do-while/predicate
   expression-list/actions
   expression/operand
   expression/operands
   expression/operator
   for/body
   for/initialize
   for/predicate
   for/step
   function-declarator?
   function-definition/return-type
   goto/target
   identifier/base
   identifier/scope
   incomplete-struct-declaration?
   initial-type-table
   inner-declarator
   left middle right
   lookup-type
   make-c-declaration
   make-c-identifier
   make-class-type
   make-enum-type
   make-structure-type
   make-union-type
   make-function-declarator
   make-type-specifier
   operator/name
   operator/precedence
   operator/symbol
   set-declaration/initial-value!
   switch/expression
   switch/body
   translation-unit/contents
   type-specifier->list
   value
   while/predicate
   while/body
   )
  )

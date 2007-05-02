#ci(module cparse (lib "swindle.ss" "swindle")

  (require (prefix plt: (lib "plt-match.ss")))

  (require "generics.ss")
  (require "utils.ss")
  (require "cmodel.ss")
  (require "cemit.ss")

  (define (parse-name ctenv name)
    (plt:match name
      ((? string? name)
       (->c-identifier name))

      (`(OPERATOR-FUNCTION (,operator))
       (->c-identifier operator))

      (`(NESTED-ID (NESTED-SCOPE ,scope)
                   ,(or '(OPERATOR-FUNCTION (OPERATOR-DELETE))
                        '(DESTRUCTOR-ID)))
       (let ((class-name (parse-name ctenv scope)))
         (make-c-identifier
          (identifier/base class-name)
          (string-append "~" (identifier/base class-name)))))

      (`(NESTED-ID (NESTED-SCOPE ,scope) ,id)
       (make-c-identifier
        (identifier/base (parse-name ctenv scope))
        (identifier/base (parse-name ctenv id))))

      (_ (evil-error 'parse-name "Can't parse name" name))))

  (define (parse-parameter ctenv parameter)
    (let loop ((parameter parameter)
               (qualifier #f))
      (plt:match parameter
        (`(DECL-SPECIFIER-PARAMETER ,inner TOKEN/CONST)
         (loop inner :const))
        (`(EXPRESSION/PARAMETER (EXPRESSION/ELLIPSES))
         '())
        (`(EXPRESSION/ELLIPSES)
         '())
        (`(EXPRESSION/PARAMETER ,decl)
         (parse-declaration ctenv '() #f qualifier identity decl))
        (_ (evil-error 'parse-parameter "Can't parse this parameter" parameter)))))

  (define (parse-arglist ctenv arglist)
    ;; (evil-message "PARSE-ARGLIST" arglist)
    (plt:match arglist
      (`(PARENTHESISED
         #F
         #F
         #F)
       '())

      (`(PARENTHESISED
         (PARAMETER-LIST
          (EXPRESSION/PARAMETER TOKEN/VOID))
         #F
         #F)
       '())

      (`(PARENTHESISED
         (PARAMETER-LIST . ,parameters)
         #F
         #F)
       (reverse (foldl (lambda (parameter plist)
                         (let ((parsed (parse-parameter ctenv parameter)))
                           (if (null? parsed)
                               plist
                               (cons parsed plist))))
                       '() parameters)))
      (_ (evil-error 'parse-arglist "Unrecognized arglist" arglist))))

  (define (parse-asterisk-qualifier asterisk-qualifier)
    (let loop ((unparsed asterisk-qualifier)
               (parsed #f))
      (plt:match unparsed
        ('(POINTER-DECLARATOR) parsed)
        (`(CV-DECLARATOR ,inner TOKEN/CONST)
         (if parsed
             (evil-error 'parse-asterisk-qualifier "a Multiple qualifiers."
                         (list asterisk-qualifier unparsed parsed))
             (loop inner :const)))
        (`(CV-DECLARATOR ,inner TOKEN/VOLATILE)
         (if parsed
             (evil-error 'parse-asterisk-qualifier "b Multiple qualifiers."
                         (list asterisk-qualifier unparsed parsed))
             (loop inner :volatile)))
        (_ (evil-error 'parse-asterisk-qualifier "Unrecognized pointer qualifier." unparsed)))))

  (define (parse-array-declarator-size ctenv sizespec)
    (plt:match sizespec
      ('(EXPRESSION-LIST #f) #f);; []
      (`(EXPRESSION/INTEGER-LITERAL ,(? string? x)) (string->number x))
      (`(EXPRESSION/NAME ,(? string? x)) x)
      (_ (parse-expression ctenv '() sizespec))))

  (define (parse-array-declarator ctenv wrapper inner size)
    ;; (evil-message "parse-array-declarator")
    (parse-declarator ctenv (lambda (base)
                              (make <array-declarator>
                                :inner-declarator (wrapper base)
                                :size (parse-array-declarator-size ctenv size)))
                      inner))

  (define (parse-pointer-declarator ctenv wrapper qualifier inner)
    ;; (evil-message "PARSE-POINTER-DECLARATOR" inner size)
    (plt:match inner
      (`(ARRAY (EXPRESSION/ABSTRACT-FUNCTION
                (PARENTHESISED
                 (PARAMETER-LIST
                  (EXPRESSION/PARAMETER ,inparens))
                 #f #f))
               ,insquare)
       ;; (evil-message "Pointer to array declarator")
       (parse-array-declarator ctenv (lambda (base)
                                       (make <pointer-declarator>
                                         :qualifier (parse-asterisk-qualifier qualifier)
                                         :inner-declarator (wrapper base)))
                               inparens
                               insquare))
      (_
       ;; (evil-message "Default pointer declarator")
       (parse-declarator ctenv (lambda (base)
                                 (make <pointer-declarator>
                                   :qualifier (parse-asterisk-qualifier qualifier)
                                   :inner-declarator (wrapper base)))
                         inner))))

  (define (parse-declarator ctenv wrapper declarator)
    ;; (evil-message "Parse declarator" declarator)
    (plt:match declarator
      (`(ARRAY ,thing ,size)
       (parse-array-declarator ctenv wrapper thing size))

        (`(CAST
           (EXPRESSION/ABSTRACT-FUNCTION (PARENTHESISED (PARAMETER-LIST (DECL-SPECIFIER-PARAMETER (EXPRESSION/PARAMETER ,left-component) TOKEN/__STDCALL)) #f #f))
           (EXPRESSION/ABSTRACT-FUNCTION ,arglist))
         (parse-declarator ctenv (lambda (base)
                                   (make-function-declarator
                                    (wrapper base)
                                    (parse-arglist ctenv arglist)))
                           left-component))

      (`(CAST
         (EXPRESSION/ABSTRACT-FUNCTION (PARENTHESISED (PARAMETER-LIST (EXPRESSION/PARAMETER ,left-component)) #f #f))
         (EXPRESSION/ABSTRACT-FUNCTION ,arglist))
       (parse-declarator ctenv (lambda (base)
                                 (make-function-declarator
                                  (wrapper base)
                                  (parse-arglist ctenv arglist)))
                         left-component))

      (`(EXPRESSION/ABSTRACT-FUNCTION
         (PARENTHESISED
          (PARAMETER-LIST
           (EXPRESSION/PARAMETER ,inner))
          #F #F))
       (parse-declarator ctenv wrapper inner))

      (`(EXPRESSION/ASSIGNMENT = ,inner ,initial-value)
       ;; (evil-message "Ignoring assignment...")
       (parse-declarator ctenv wrapper inner))

      (`(EXPRESSION/CALL ,thing ,arglist)
       (make-function-declarator
        (parse-declarator ctenv wrapper thing)
        (parse-arglist ctenv arglist)))

      (`(EPSILON)
       (wrapper (make <direct-declarator> :identifier c-identifier:*anonymous*)))

      (`(EXPRESSION/NAME ,name)
       (wrapper (make <direct-declarator> :identifier (parse-name ctenv name))))

      (`(EXPRESSION-LIST #f)
       (wrapper (make <direct-declarator> :identifier c-identifier:*ignore*)))

      (`(EXPRESSION/POINTER ,qualifier ,inner)
       (parse-pointer-declarator ctenv wrapper qualifier inner))

      (_ (evil-error 'PARSE-DECLARATOR "Unknown declarator" declarator))))

  (define (parse-array-declaration ctenv labels storage-class qualifier wrapper left-component right-component)
    ;; (evil-message "PARSE-ARRAY-DECLARATION" storage-class qualifier left-component right-component)
    ;; (if (null? ctenv) (error (list 'parse-array-declaration ctenv)))
    (plt:match left-component
      (`(EXPRESSION/CALL ,left-left
                         (PARENTHESISED (PARAMETER-LIST (EXPRESSION/PARAMETER ,middle)) #f #f))
       ;; (evil-message "Parenthesized array")
       (make-c-declaration
        labels
        storage-class
        qualifier
        (parse-type-specifier ctenv left-left)
        (parse-declarator ctenv (lambda (base)
                                  (make <array-declarator>
                                    :inner-declarator (wrapper base)
                                    :size  (parse-array-declarator-size ctenv right-component)))
                          middle)))

      (_
       ;;(evil-message "Normal array path")
       (let ((parsed-left
              (parse-declaration ctenv '() storage-class qualifier
                                 (lambda (inner)
                                   (make <array-declarator>
                                     :inner-declarator   (wrapper inner)
                                     :size (parse-array-declarator-size ctenv right-component)))
                                 left-component)))
         (make-c-declaration
          labels
          storage-class
          qualifier
          (declaration/type parsed-left)
          (declaration/declarator parsed-left))))))

  (define (parse-asterisk-declaration ctenv labels storage-class qualifier wrapper asterisk-qualifier left right)
    ;; (evil-message "PARSE-ASTERISK-DECLARATION" asterisk-qualifier left right)
    ;; (if (null? ctenv) (error 'parse-asterisk-declaration))
    (plt:match right
      (`(ARRAY ,array-left ,insquare)
       ;; (evil-message "Pointer to array declaration")
       (make-c-declaration
        labels
        storage-class
        qualifier
        (parse-type-specifier ctenv left)
        (parse-array-declarator ctenv (lambda (base)
                                        (make <pointer-declarator>
                                          :inner-declarator (wrapper base)
                                          :qualifier (parse-asterisk-qualifier asterisk-qualifier)))
                                array-left
                                insquare)))

      (_
       ;; (evil-message "Normal asterisk declaration")
       (make-c-declaration
        labels
        storage-class
        qualifier
        (parse-type-specifier ctenv left)
        (wrapper
         (parse-declarator ctenv (lambda (base)
                                   (make <pointer-declarator>
                                     :inner-declarator base
                                     :qualifier (parse-asterisk-qualifier asterisk-qualifier)))
                           right))))))


  (define (parse-function-declaration ctenv labels storage-class qualifier
                                      function-wrapper left-component right-component)
    ;; (if (null? ctenv) (error 'parse-function-declaration))
    ;; (evil-message "PARSE-FUNCTION-DECLARATION:" left-component right-component)
    (plt:match left-component
      ;; No return type given.
      (`(EXPRESSION/NAME ,name-components)
       (make-c-declaration
        labels
        storage-class
        qualifier
        c-type:int ;; assume int
        (function-wrapper (make-function-declarator
                           (make <direct-declarator>
                             :identifier (parse-name ctenv name-components))
                           (parse-arglist ctenv right-component)))))

      (`(TYPED-NAME (DECL-SPECIFIER-NAME ,type TOKEN/__CDECL) (EXPRESSION/NAME ,name))
       (make-c-declaration
        labels
        storage-class
        (list :cdecl qualifier)
        (parse-type-specifier ctenv type)
        (function-wrapper
         (make-function-declarator
          (make <direct-declarator>
            :identifier (parse-name ctenv name))
          (parse-arglist ctenv right-component)))))


      (`(TYPED-NAME (EXPRESSION/NAME (DECL-SPECIFIER-NAME ,type TOKEN/__CDECL)) (EXPRESSION/NAME ,name))
       (make-c-declaration
        labels
        storage-class
        (list :cdecl qualifier)
        (parse-type-specifier ctenv `(EXPRESSION/NAME ,type))
        (function-wrapper
         (make-function-declarator
          (make <direct-declarator>
            :identifier (parse-name ctenv name))
          (parse-arglist ctenv right-component)))))

      (`(TYPED-NAME (DECL-SPECIFIER-NAME ,type TOKEN/__STDCALL) (EXPRESSION/NAME ,name))
       (make-c-declaration
        labels
        storage-class
        (list :stdcall qualifier)
        (parse-type-specifier ctenv type)
        (function-wrapper
         (make-function-declarator
          (make <direct-declarator>
            :identifier (parse-name ctenv name))
          (parse-arglist ctenv right-component)))))

      (`(TYPED-NAME (EXPRESSION/NAME (DECL-SPECIFIER-NAME ,type TOKEN/__STDCALL)) (EXPRESSION/NAME ,name))
       (make-c-declaration
        labels
        storage-class
        (list :stdcall qualifier)
        (parse-type-specifier ctenv `(EXPRESSION/NAME ,type))
        (function-wrapper
         (make-function-declarator
          (make <direct-declarator>
            :identifier (parse-name ctenv name))
          (parse-arglist ctenv right-component)))))

      (`(EXPRESSION/CALL ,left-left-component
                         (PARENTHESISED
                          (PARAMETER-LIST
                           (EXPRESSION/PARAMETER ,middle-left-component))
                          #f #f))
       (make-c-declaration
        labels
        storage-class
        qualifier
        (parse-type-specifier ctenv left-left-component)
        (function-wrapper
         (parse-declarator ctenv (lambda (base)
                                   (make-function-declarator
                                    base
                                    (parse-arglist ctenv right-component)))
                           middle-left-component))))

      (`(EXPRESSION/CALL ,left-left-component
                         (PARENTHESISED
                          (PARAMETER-LIST
                           (DECL-SPECIFIER-PARAMETER (EXPRESSION/PARAMETER ,middle-left-component) TOKEN/__CDECL))
                          #f #f))
       (make-c-declaration
        labels
        storage-class
        qualifier
        (parse-type-specifier ctenv left-left-component)
        (function-wrapper
         (parse-declarator ctenv (lambda (base)
                                   (make-function-declarator
                                    base
                                    (parse-arglist ctenv right-component)))
                           middle-left-component))))

      (`(EXPRESSION/CALL ,left-left-component
                         (PARENTHESISED
                          (PARAMETER-LIST
                           (DECL-SPECIFIER-PARAMETER (EXPRESSION/PARAMETER ,middle-left-component) TOKEN/__STDCALL))
                          #f #f))
       (make-c-declaration
        labels
        storage-class
        qualifier
        (parse-type-specifier ctenv left-left-component)
        (function-wrapper
         (parse-declarator ctenv (lambda (base)
                                   (make-function-declarator
                                    base
                                    (parse-arglist ctenv right-component)))
                           middle-left-component))))

      (_ (let ((parsed-left
                (parse-declaration ctenv '() storage-class qualifier function-wrapper left-component)))
           (make-c-declaration
            labels
            storage-class
            qualifier
            (declaration/type parsed-left)
            (make-function-declarator
             (declaration/declarator parsed-left)
             (parse-arglist ctenv right-component)))))))

  (define (parse-type-for-cast ctenv possible-type if-type if-not)
    ;; (if (null? ctenv) (error (list 'parse-type-for-cast ctenv)))
    ;; (evil-message "PARSE-TYPE-FOR-CAST: " possible-type)
    (let loop ((possible-type possible-type)
               (qualifier #f))

      (if (string? possible-type)
          (let* ((converted-name (->c-identifier possible-type))
                 (user-type (lookup-type ctenv converted-name)))
            (if user-type
                (if-type (make-type-specifier converted-name))
                (if-not)))

          (plt:match possible-type

            ('TOKEN/__INT64  (if-type c-type:__int64))
            ('TOKEN/__PTR64  (if-type c-type:__ptr64))
            ('TOKEN/CHAR     (if-type c-type:char))
            ('TOKEN/DOUBLE   (if-type c-type:double))
            ('TOKEN/FLOAT    (if-type c-type:float))
            ('TOKEN/INT      (if-type c-type:int))
            ('TOKEN/LONG     (if-type c-type:long))
            ('TOKEN/SHORT    (if-type c-type:short))
            ('TOKEN/SIGNED   (if-type c-type:signed))
            ('TOKEN/UNSIGNED (if-type c-type:unsigned))
            ('TOKEN/VOID     (if-type c-type:void))
            ('TOKEN/WCHAR_T  (if-type c-type:wchar_t))

            ('(BUILT-IN-NAME (BUILT-IN-NAME TOKEN/UNSIGNED TOKEN/SHORT) TOKEN/WCHAR_T) (if-type c-type:ushort-wchar))
            ('(BUILT-IN-NAME (BUILT-IN-NAME TOKEN/__W64 TOKEN/UNSIGNED) TOKEN/INT)     (if-type c-type:__w64-uint))
            ('(BUILT-IN-NAME (BUILT-IN-NAME TOKEN/__W64 TOKEN/UNSIGNED) TOKEN/LONG)    (if-type c-type:__w64-ulong))
            ('(BUILT-IN-NAME TOKEN/LONG TOKEN/DOUBLE)                                  (if-type c-type:long-double))
            ('(BUILT-IN-NAME TOKEN/LONG TOKEN/LONG)                                  (if-type c-type:long-long))
            ('(BUILT-IN-NAME TOKEN/SHORT TOKEN/INT)                                    (if-type c-type:short-int))
            ('(BUILT-IN-NAME TOKEN/SIGNED TOKEN/CHAR)                                  (if-type c-type:schar))
            ('(BUILT-IN-NAME TOKEN/SIGNED TOKEN/INT)                                   (if-type c-type:sint))
            ('(BUILT-IN-NAME TOKEN/SIGNED TOKEN/LONG)                                  (if-type c-type:slong))
            ('(BUILT-IN-NAME TOKEN/SIGNED TOKEN/SHORT)                                 (if-type c-type:sshort))
            ('(BUILT-IN-NAME TOKEN/SIGNED TOKEN/__INT64)                               (if-type c-type:signed-__int64))
            ('(BUILT-IN-NAME TOKEN/UNSIGNED TOKEN/CHAR)                                (if-type c-type:uchar))
            ('(BUILT-IN-NAME TOKEN/UNSIGNED TOKEN/INT)                                 (if-type c-type:uint))
            ('(BUILT-IN-NAME TOKEN/UNSIGNED TOKEN/LONG)                                (if-type c-type:ulong))
            ('(BUILT-IN-NAME TOKEN/UNSIGNED TOKEN/SHORT)                               (if-type c-type:ushort))
            ('(BUILT-IN-NAME TOKEN/UNSIGNED TOKEN/__INT64)                             (if-type c-type:unsigned-__int64))
            ('(BUILT-IN-NAME TOKEN/__W64 TOKEN/INT)                                    (if-type c-type:__w64-int))
            ('(BUILT-IN-NAME TOKEN/__W64 TOKEN/LONG)                                   (if-type c-type:__w64-long))
            ('(BUILT-IN-NAME TOKEN/__W64 TOKEN/UNSIGNED)                               (if-type c-type:__w64-unsigned))

            (`(ASTERISK (POINTER-DECLARATOR) ,inner (EXPRESSION-LIST #f))
             (parse-type-for-cast
              ctenv inner
              (lambda (inner-type)
                (if-type (make <c-pointer-type-specifier>
                           :inner inner-type)))
              (lambda ()
                (evil-error 'parse-type-for-cast "Unrecognized type" possible-type))))

            (`(ASTERISK (POINTER-DECLARATOR) ,inner (EXPRESSION/POINTER (POINTER-DECLARATOR) (EXPRESSION-LIST #f)))
             (parse-type-for-cast
              ctenv inner
              (lambda (inner-type)
                (if-type (make <c-pointer-type-specifier>
                           :inner (make <c-pointer-type-specifier>
                                    :inner  inner-type))))
              (lambda ()
                (evil-error 'parse-type-for-cast: "Unrecognized type" possible-type))))


            (`(ASTERISK (POINTER-DECLARATOR) ,inner
                        (CAST (EXPRESSION/ABSTRACT-FUNCTION
                               (PARENTHESISED
                                (PARAMETER-LIST (EXPRESSION/PARAMETER
                                                 (EXPRESSION/POINTER (POINTER-DECLARATOR) (EPSILON))))
                                #f #f))
                              (EXPRESSION/ABSTRACT-FUNCTION
                               (PARENTHESISED
                                (PARAMETER-LIST . ,params)
                                #f #f))))

             (if-type (make <c-function-type-specifier>
                        :return-type (make <c-pointer-type-specifier>
                                       :inner (parse-type-specifier ctenv inner))
                        :argtypes (map (lambda (at)
                                         (parse-type-specifier ctenv (cadr at)))
                                       params))))

            ;; gratuitous parens
            (`(EXPRESSION/ABSTRACT-FUNCTION
               (PARENTHESISED
                (PARAMETER-LIST
                 (EXPRESSION/PARAMETER ,inner))
                #F #F))
             (parse-type-for-cast ctenv inner if-type if-not))

            (`(EXPRESSION/NAME (DECL-SPECIFIER-NAME ,inner TOKEN/VOLATILE))
             (loop inner :volatile))

            (`(EXPRESSION/NAME ,(? string? name))
             (let* ((converted-name (->c-identifier name))
                    (user-type (lookup-type ctenv converted-name)))
               (if user-type
                   (if-type (make-type-specifier converted-name))
                   (if-not))))

            (_ ;;(evil-error 'parse-type-for-cast "Unrecognized type" possible-type)
              (evil-message "parse-type-for-cast no match for: " possible-type)
             (if-not))))))

  (define (parse-and-expression ctenv labels left right)
    ;; (evil-message "PARSE-AND-EXPRESSION: " left right)
    (plt:match left
      (`(EXPRESSION/ABSTRACT-FUNCTION
         (PARENTHESISED
          (PARAMETER-LIST
           (EXPRESSION/PARAMETER ,possible-type))
          #f #f))
       (parse-type-for-cast
        ctenv possible-type
        (lambda (type)
          (make <c-cast-expression>
            :labels labels
            :type type
            :operand (make <c-unary-expression>
                       :operator c-operator:ampersand
                       :operand (parse-expression ctenv '() right))))
        (lambda ()
          (make <c-binary-expression>
            :labels labels
            :operator c-operator:and-bits
            :left  (parse-expression ctenv '() left)
            :right (parse-expression ctenv '() right)))))
      (`(EXPRESSION/POINTER (POINTER-DECLARATOR)
                            (EXPRESSION/ABSTRACT-FUNCTION
                             (PARENTHESISED
                              (PARAMETER-LIST
                               (EXPRESSION/PARAMETER ,possible-type))
                              #f #f)))
       (parse-type-for-cast
        ctenv possible-type
        (lambda (type)
          (make <c-asterisk-expression>
            :labels labels
            :operand (make <c-cast-expression>
                       :type type
                       :operand (make <c-unary-expression>
                                  :operator c-operator:ampersand
                                  :operand (parse-expression ctenv '() right)))))
        (lambda ()
          (make <c-binary-expression>
            :labels labels
            :operator c-operator:and-bits
            :left  (parse-expression ctenv '() left)
            :right (parse-expression ctenv '() right)))))

      (`(,binop ,left-left (EXPRESSION/ABSTRACT-FUNCTION
                            (PARENTHESISED
                             (PARAMETER-LIST
                              (EXPRESSION/PARAMETER ,possible-type))
                             #f #f)))
       (parse-type-for-cast
        ctenv possible-type
        (lambda (type)
          (parse-expression ctenv labels `(,binop ,left-left (CAST (EXPRESSION/ABSTRACT-FUNCTION
                                                             (PARENTHESISED
                                                              (PARAMETER-LIST
                                                               (EXPRESSION/PARAMETER ,possible-type))
                                                              #f #f))
                                                            ,right))))
        (lambda ()
          (make <c-binary-expression>
            :labels labels
            :operator c-operator:and-bits
            :left  (parse-expression ctenv '() left)
            :right (parse-expression ctenv '() right)))))


      (_ (make <c-binary-expression>
            :labels labels
            :operator c-operator:and-bits
            :left  (parse-expression ctenv '() left)
            :right (parse-expression ctenv '() right)))))

  (define (parse-argument ctenv argument)
    (plt:match argument
      (`(EXPRESSION/PARAMETER ,arg)
       (parse-expression ctenv '() arg))
      (_ (evil-error 'parse-argument "Unrecognized argument" argument))))

  (define (parse-arguments ctenv arguments)
    (if (equal? arguments '(PARENTHESISED #f #f #f))
        '()
        (map (lambda (arg)
               (parse-argument ctenv arg))
             arguments)))

  (define (parse-call ctenv labels function arguments)
    ;; (evil-message "PARSE-CALL: " function arguments)
    (let ((parsed-function (parse-expression ctenv '() function)))
      (cond ((c-variable? parsed-function)
             (make <c-funcall>
               :labels labels
               :operator parsed-function
               :operands (parse-arguments ctenv arguments)))
            ((c-arrow-expression? parsed-function)
             (make <c-method-call>
               :labels labels
               :operator parsed-function
               :operands (parse-arguments ctenv arguments)))
            (else
             (evil-error 'parse-call "Unrecognized call" parsed-function)))))

  (define (parse-expression ctenv labels expression)
    ;;(if (null? ctenv) (error 'parse-expression))
    ;;(evil-message "PARSE-EXPRESSION: " expression)
    (plt:match expression
      (`(ADD ,left ,right)
       (make <c-binary-expression>
         :labels labels
         :operator c-operator:add
         :left  (parse-expression ctenv '() left)
         :right (parse-expression ctenv '() right)))

      (`(AND ,left ,right)
       (parse-and-expression ctenv labels left right))

      (`(ARRAY ,left ,insquare)
       (make <c-array-expression>
         :labels labels
         :left (parse-expression ctenv '() left)
         :right (parse-expression ctenv '() insquare)))

      (`(ASTERISK (POINTER-DECLARATOR) ,left ,right)
       (if (equal? right '(EXPRESSION-LIST #f))
           (make <c-pointer-type-specifier>
             :labels labels
             :inner (parse-type-specifier ctenv left))
           (make <c-binary-expression>
            :labels labels
             :operator c-operator:multiply
             :left (parse-expression ctenv '() left)
             :right (parse-expression ctenv '() right))))

      (`(CAST (EXPRESSION/ABSTRACT-FUNCTION
               (PARENTHESISED (PARAMETER-LIST (EXPRESSION/PARAMETER ,type))
                              #f #f))
              ,value)
       (parse-type-for-cast
        ctenv type
        (lambda (type)
          (make <c-cast-expression>
            :labels labels
            :type type
            :operand (parse-expression ctenv '() value)))
        (lambda ()
          (evil-error 'parse-expression "Bogus cast?" expression))))

      (`(CAST (EXPRESSION/ABSTRACT-FUNCTION
               (PARENTHESISED (PARAMETER-LIST (DECL-SPECIFIER-PARAMETER (EXPRESSION/PARAMETER ,type) TOKEN/CONST))
                              #f #f))
              ,value)
       (parse-type-for-cast
        ctenv type
        (lambda (type)
          (make <c-cast-expression>
            :labels labels
            :type type
            :operand (parse-expression ctenv '() value)))
        (lambda ()
          (evil-error 'parse-expression "Bogus cast?" expression))))

      (`(DIVIDE ,left ,right)
       (make <c-binary-expression>
         :labels labels
         :operator c-operator:divide
         :left  (parse-expression ctenv '() left)
         :right (parse-expression ctenv '() right)))

      (`(EQUAL ,left ,right)
       (make <c-relational-binary-expression>
            :labels labels
         :operator c-operator:equal
         :left (parse-expression ctenv '() left)
         :right (parse-expression ctenv '() right)))

      (`(EXCLAMATION ,inner)
       (make <c-relational-unary-combination>
            :labels labels
         :operator c-operator:not
         :operand (parse-expression ctenv '() inner)))

      ;; gratuitous parenthesis
      (`(EXPRESSION/ABSTRACT-FUNCTION
         (PARENTHESISED
          (PARAMETER-LIST
           (EXPRESSION/PARAMETER ,inner))
          #f #f))
       (parse-expression ctenv labels inner))

      ;; sequence of expressions
      (`(EXPRESSION/ABSTRACT-FUNCTION
         (PARENTHESISED
          (PARAMETER-LIST
           (EXPRESSION/PARAMETER ,inner) ...)
          #f #f))
       (make <c-expression-list>
         :labels labels
         :actions (map (lambda (subexpression)
                         (parse-expression ctenv '() subexpression))
                       inner)))

      (`(EXPRESSION/ARROW ,left ,(? string? right))
       (make <c-arrow-expression>
         :labels labels
         :left (parse-expression ctenv '() left)
         :right (->c-identifier  right)))

      (`(EXPRESSION/ASSIGNMENT ,assignment-operator ,place ,value)
       (make <c-assignment>
         :labels labels
         :operator assignment-operator
         :place (parse-expression ctenv '() place)
         :value (parse-expression ctenv '() value)))

      (`(EXPRESSION/CALL
         ,function
         (PARENTHESISED #f #f #f))
       (parse-call ctenv labels function '()))

      (`(EXPRESSION/CALL
         ,function
         (PARENTHESISED
          (PARAMETER-LIST . ,arguments)
          #f #f))
       (parse-call ctenv labels function arguments))

      (`(EXPRESSION/CONDITIONAL ,test ,consequent ,alternative)
       (make <c-conditional-expression>
            :labels labels
         :test (parse-expression ctenv '() test)
         :consequent  (parse-expression ctenv '() consequent)
         :alternative (parse-expression ctenv '() alternative)))

      (`(EXPRESSION/CHARACTER-LITERAL ,(? string? value))
       (make <c-literal>
         :labels labels
         :value (string-ref value 0)))

      (`(EXPRESSION/DOT ,left ,right)
       (make <c-dot-expression>
            :labels labels
         :left (parse-expression ctenv '() left)
         :right (->c-identifier right)))

      (`(EXPRESSION/INTEGER-LITERAL ,(? string? number))
       (make <c-literal>
         :labels labels
         :value (string->number number)))

      (`(EXPRESSION/NAME ,name)
       (make <c-variable>
         :labels labels
         :name (parse-name ctenv name)))

      (`(EXPRESSION/NEW-TYPE-ID #f (EXPRESSION/TYPED (BUILT-IN-ID-ID)
                                                     (ABSTRACT-ARRAY ,size)) #f)
       (make <c-new-array>
         :labels labels
         :size (parse-expression ctenv '() size)
         :arguments '()))

      (`(EXPRESSION/NEW-TYPE-ID #f (EXPRESSION/TYPED ,type
                                                     (EXPRESSION/POINTER
                                                      (POINTER-DECLARATOR)
                                                      (ABSTRACT-ARRAY  ,size)))
                                #f)
       (make <c-new-array>
         :labels labels
         :type (make <c-pointer-type-specifier> :inner (parse-type-specifier ctenv type))
         :size (parse-expression ctenv '() size)
         :arguments '()))

      (`(EXPRESSION/NEW-TYPE-ID #f (EXPRESSION/TYPED ,type (EXPRESSION-LIST #f))
                                #f)
       (make <c-new>
         :labels labels
         :type (parse-type-specifier ctenv type)
         :arguments '()))

      (`(EXPRESSION/NEW-TYPE-ID #f (EXPRESSION/TYPED ,type (EXPRESSION-LIST #f))
                                (EXPRESSION-LIST . ,constructor-args))
       (make <c-new>
         :labels labels
         :type (parse-type-specifier ctenv type)
         :arguments (map (lambda (arg)
                           (parse-expression ctenv '() arg))
                         constructor-args)))

      (`(EXPRESSION/NEW-TYPE-ID
         (PARAMETER-LIST (EXPRESSION/PARAMETER ,params) ...) (EXPRESSION/TYPED ,type (EXPRESSION-LIST #f))
                                #f)
       (make <c-new>
         :labels labels
         :type (parse-type-specifier ctenv type)
         :parameters (map (lambda (param) (parse-expression ctenv '()  param)) params)
         :arguments '()))

      (`(EXPRESSION/NEW-TYPE-ID
         (PARAMETER-LIST (EXPRESSION/PARAMETER ,params) ...)
         (EXPRESSION/TYPED ,type (EXPRESSION-LIST #f))
         (EXPRESSION-LIST . ,constructor-args))
       (make <c-new>
         :labels labels
         :type (parse-type-specifier ctenv type)
         :parameters (map (lambda (param) (parse-expression ctenv '() param)) params)
         :arguments (map (lambda (arg)
                           (parse-expression ctenv '() arg))
                         constructor-args)))

      (`(EXPRESSION/POINTER (POINTER-DECLARATOR) ,place)
       (make <c-asterisk-expression>
         :labels labels
         :operand (parse-expression ctenv '() place)))

      (`(EXPRESSION/POINTER (REFERENCE-DECLARATOR) ,place)
       (make <c-unary-expression>
         :labels labels
         :operator c-operator:ampersand
         :operand (parse-expression ctenv '() place)))

      (`(EXPRESSION/STRING-LITERAL ,(? string? value))
       (make <c-literal>
         :labels labels
         :value value))

      (`(EXPRESSION/THIS)
       (make <c-this>
         :labels labels))

      (`(EXPRESSION-LIST #f)
       (make <c-void-expression>
         :labels labels))

      (`(EXPRESSION-LIST ,element)
       (parse-expression ctenv labels element))

      (`(EXPRESSION-LIST . ,elements)
       (make <c-expression-list>
         :labels labels
         :actions (map (lambda (expr)
                         (parse-expression ctenv '() expr))
                       elements)))

      (`(GREATER-EQUAL ,left ,right)
       (make <c-relational-binary-expression>
         :labels labels
         :operator c-operator:greater-equal
         :left (parse-expression ctenv '() left)
         :right (parse-expression ctenv '() right)))

      (`(GREATER-THAN ,left ,right)
       (make <c-relational-binary-expression>
         :labels labels
         :operator c-operator:greater-than
         :left (parse-expression ctenv '() left)
         :right (parse-expression ctenv '() right)))

      (`(INCLUSIVE-OR ,left ,right)
       (make <c-binary-expression>
         :labels labels
         :operator c-operator:or-bits
         :left (parse-expression ctenv '() left)
         :right (parse-expression ctenv '() right)))

      (`(LESS-EQUAL ,left ,right)
       (make <c-relational-binary-expression>
         :labels labels
         :operator c-operator:less-equal
         :left (parse-expression ctenv '() left)
         :right (parse-expression ctenv '() right)))

      (`(LESS-THAN ,left ,right)
       (make <c-relational-binary-expression>
         :labels labels
         :operator c-operator:less-than
         :left (parse-expression ctenv '() left)
         :right (parse-expression ctenv '() right)))

      (`(LOGICAL-AND ,left ,right)
       (make <c-relational-binary-combination>
         :labels labels
         :operator c-operator:and
         :left  (parse-expression ctenv '() left)
         :right (parse-expression ctenv '() right)))

      (`(LOGICAL-OR ,left ,right)
       (make <c-relational-binary-combination>
         :labels labels
         :operator c-operator:or
         :left (parse-expression ctenv '() left)
         :right (parse-expression ctenv '() right)))

      (`(MOD ,left ,right)
       (make <c-binary-expression>
         :labels labels
         :operator c-operator:modulus
         :left  (parse-expression ctenv '() left)
         :right (parse-expression ctenv '() right)))

      (`(NOT-EQUAL ,left ,right)
       (make <c-relational-binary-expression>
         :labels labels
         :operator c-operator:not-equal
         :left (parse-expression ctenv '() left)
         :right (parse-expression ctenv '() right)))

      (`(PLUS ,expression)
       (make <c-unary-expression>
         :labels labels
         :operator c-operator:plus
         :operand (parse-expression ctenv '() expression)))

      (`(PRE-INCREMENT ,place)
       (make <c-unary-modify>
         :labels labels
         :operator c-operator:pre-increment
         :operand (parse-expression ctenv '() place)))

      (`(PRE-DECREMENT ,place)
       (make <c-unary-modify>
         :labels labels
         :operator c-operator:pre-decrement
         :operand (parse-expression ctenv '() place)))

      (`(POST-INCREMENT ,place)
       (make <c-unary-modify>
         :labels labels
         :operator c-operator:post-increment
         :operand (parse-expression ctenv '() place)))

      (`(POST-DECREMENT ,place)
       (make <c-unary-modify>
         :labels labels
         :operator c-operator:post-decrement
         :operand (parse-expression ctenv '() place)))

      (`(REINTERPRET-CAST ,type ,value)
       ;; punt on type for today
       (make <c-reinterpret-cast-expression>
         :labels labels
         :type type
         :operand (parse-expression ctenv '() value)))

      (`(SHIFT-LEFT ,left ,right)
       (make <c-binary-expression>
         :labels labels
         :operator c-operator:shift-left
         :left (parse-expression ctenv '() left)
         :right (parse-expression ctenv '() right)))

      (`(SHIFT-RIGHT ,left ,right)
       (make <c-binary-expression>
         :labels labels
         :operator c-operator:shift-right
         :left (parse-expression ctenv '() left)
         :right (parse-expression ctenv '() right)))

      (`(SIZEOF ,thing)
       (parse-type-for-cast
        ctenv thing
        (lambda (type)
          (make <c-sizeof-expression>
         :labels labels
            :operand type))
        (lambda ()
          (make <c-sizeof-expression>
         :labels labels
            :operand (parse-expression ctenv '() thing)))))

      (`(SUBTRACT ,left ,right)
       (make <c-binary-expression>
         :labels labels
         :operator c-operator:subtract
         :left (parse-expression ctenv '() left)
         :right (parse-expression ctenv '() right)))

      (`(TILDE ,expression)
       (make <c-unary-expression>
         :labels labels
         :operator c-operator:complement
         :operand (parse-expression ctenv '() expression)))

      (`(UNARY-MINUS ,expression)
       (make <c-unary-expression>
         :labels labels
         :operator c-operator:minus
         :operand (parse-expression ctenv '() expression)))

      (_ (evil-error 'parse-expression "Unrecognized expression" expression))))

  (define (parse-asterisk-statement ctenv labels qualifier left right)
    ;; (evil-message "PARSE-ASTERISK-STATEMENT" qualifier left right )
    (parse-asterisk-declaration ctenv labels #f #f identity qualifier left right))

  (define (parse-expression-list-declarator ctenv storage-class qualifier type declarator)
    (make-c-declaration
     '()
     storage-class
     qualifier
     type
     (parse-declarator ctenv identity declarator)))

  (define (parse-expression-list-statement ctenv labels declaration declarators)
    ;; (evil-message "PARSE-EXPRESSION-LIST-STATEMENT" declaration declarators)
    ;; this is a multiple declaration
    (let* ((parsed-declaration (parse-declaration ctenv '() #f #f identity declaration))
           (typespec (declaration/type parsed-declaration)))
      (make <c-expression-list>
        :labels labels
        :actions (cons parsed-declaration
                       (map (lambda (declarator)
                              (parse-expression-list-declarator
                               ctenv
                               (declaration/storage-class parsed-declaration)
                               (declaration/qualifier parsed-declaration)
                               typespec declarator))
                            declarators)))))

  (define (parse-array-statement ctenv labels left insquare)
    ;; (evil-message "PARSE-ARRAY-STATEMENT" left insquare)
    (parse-array-declaration ctenv labels #f #f identity left insquare))

  (define (parse-statement-declaration ctenv labels stmtdcl)
    ;; (evil-message "PARSE-STATEMENT-DECLARATION" stmtdcl)
    (plt:match stmtdcl
      (`(ARRAY ,left ,insquare)
       (parse-array-statement ctenv labels left insquare))

      (`(ASTERISK ,qualifier ,left ,right)
       (parse-asterisk-statement ctenv labels qualifier left right))

      (`(CAST (EXPRESSION/ABSTRACT-FUNCTION (PARENTHESISED (PARAMETER-LIST (EXPRESSION/PARAMETER TOKEN/VOID)) #f #f))
              ,right-group)
       (evil-message "Ignoring cast to void at statement level, parsing as expression.")
       (parse-expression ctenv labels right-group))

      (`(CAST ,left-group ,right-group)
       (evil-error 'parse-statement-declaration "Cast statement?" stmtdcl)
       ;;(parse-cast-statement ctenv left-group right-group)
       )

      (`(DELETE ,place)
       (make <c-delete>
         :labels labels
         :place (parse-expression ctenv '() place)))

      ;; Random double semicolon.
      (`(EXPRESSION-LIST #F)
       (make <c-empty-statement>
         :labels labels))

      (`(EXPRESSION-LIST ,declaration . ,declarators)
       (parse-expression-list-statement ctenv labels declaration declarators))

      ;; gratuitous parenthesis
      (`(EXPRESSION/ABSTRACT-FUNCTION
         (PARENTHESISED
          (PARAMETER-LIST
           (EXPRESSION/PARAMETER ,inner1)
           . ,inner)
          #f #f))
       (if (null? inner)
           (parse-statement-declaration ctenv labels inner1)
           (parse-expression ctenv labels stmtdcl)))

      (`(EXPRESSION/ASSIGNMENT = (typed-name ,type ,name) ,value)
       (let ((decl (parse-declaration ctenv labels #f #f identity stmtdcl))
             (initial-value (parse-expression ctenv '() value)))
         (evil-message "Setting initial value of declaration.")
         (set-declaration/initial-value! decl initial-value)
         decl))

      (`(EXPRESSION/ASSIGNMENT = (asterisk (pointer-declarator) ,type ,name) ,value)
       (let ((decl (parse-declaration ctenv labels #f #f identity stmtdcl))
             (initial-value (parse-expression ctenv '() value)))
         (evil-message "Setting initial value of declaration.")
         (set-declaration/initial-value! decl initial-value)
         decl))

      (`(EXPRESSION/ASSIGNMENT ,assignment-operator ,place ,value)
       (make <c-assignment>
         :labels labels
         :operator assignment-operator
         :place (parse-expression ctenv '() place)
         :value (parse-expression ctenv '() value)))

      (`(EXPRESSION/CALL ,left ,inparens)
       (parse-call-statement ctenv labels left inparens))

      (`(EXPRESSION/DECL-SPECIFIER ,inner TOKEN/CONST)
       ;; just ignore the CONST
       (parse-statement-declaration ctenv labels inner))

      (`(EXPRESSION/DECL-SPECIFIER ,inner TOKEN/STATIC)
       (parse-declaration ctenv labels :static #f identity inner))

      (`(EXPRESSION/DECL-SPECIFIER ,inner TOKEN/VOLATILE)
       ;; just ignore the VOLATILE
       (parse-statement-declaration ctenv labels inner))

      (`(POST-DECREMENT ,place)
       (make <c-unary-modify>
         :labels labels
         :operator c-operator:post-decrement
         :operand (parse-expression ctenv '() place)))

      (`(POST-INCREMENT ,place)
       (make <c-unary-modify>
         :labels labels
         :operator c-operator:post-increment
         :operand (parse-expression ctenv '() place)))

      (`(PRE-DECREMENT ,place)
       (make <c-unary-modify>
         :labels labels
         :operator c-operator:pre-decrement
         :operand (parse-expression ctenv '() place)))

      (`(PRE-INCREMENT ,place)
       (make <c-unary-modify>
         :labels labels
         :operator c-operator:pre-increment
         :operand (parse-expression ctenv '() place)))

      (`(TYPED-NAME ,type ,name)
       (parse-declaration ctenv labels #f #f identity stmtdcl))

      (_ (evil-error 'parse-statement-declaration "Unrecognized statement-declaration" stmtdcl))))

  (define (parse-call-statement ctenv labels left inparens)
    ;; (evil-message "PARSE-CALL-STATEMENT" left inparens)
    (plt:match inparens
      (`(PARENTHESISED #f #f #f)
       (parse-call ctenv labels left '()))

      (`(PARENTHESISED
         (PARAMETER-LIST . ,args)
         #f #f)
       (parse-call ctenv labels left args))

      (_ (evil-error 'parse-call-statement inparens))))

  (define (parse-do-while-statement ctenv labels body test)
    ;; (evil-message "PARSE-DO-WHILE-STATEMENT" body test)
    (make <c-do-while>
      :labels labels
      :body body
      :predicate test))

  (define (parse-if-statement ctenv labels condition consequent alternative)
    ;; (evil-message "PARSE-IF-STATEMENT" condition consequence alternative)
    (make <c-if>
      :labels labels
      :predicate condition
      :consequent consequent
      :alternative alternative))

  (define (parse-for-statement ctenv labels init test step body)
    ;; (evil-message "PARSE-FOR-STATEMENT" init test step body)
    (make <c-for>
      :labels labels
      :initialize init
      :predicate test
      :step step
      :body body))

  (define (parse-while-statement ctenv labels test body)
    ;; (evil-message "PARSE-WHILE-STATEMENT" test body)
    (make <c-while>
      :labels labels
      :predicate test
      :body body))

  (define (parse-switch-statement ctenv labels test body)
    ;; (evil-message "PARSE-WHILE-STATEMENT" test body)
    (make <c-switch>
      :labels labels
      :expression test
      :body body))

  (define (parse-condition ctenv condition)
    ;; (evil-message "PARSE-CONDITION" condition)
    (plt:match condition
      (`(CONDITION (PARAMETER-LIST (EXPRESSION/PARAMETER ,body)))
       (parse-expression ctenv '() body))

      (_ (evil-error 'parse-condition "Unrecognized condition" condition))))

  (define (parse-statement ctenv statement)
    ;; (if (null? ctenv) (error "bad ctenv"))
    ;; (evil-message "PARSE-STATEMENT" statement)
    (let loop ((labels '())
               (statement statement))

      (plt:match statement
        (`(COMPOUND-STATEMENT #f)
         (make <c-block> :labels labels :contents '()))

        (`(COMPOUND-STATEMENT (STATEMENT-LIST . ,statements))
         (foldr (lambda (statement block)
                  (accumulate-block (parse-statement ctenv statement) block))
                (make <c-block> :labels labels :contents '()) statements))

        (`(STATEMENT/BREAK) (make <c-break> :labels labels))

        (`(STATEMENT/CASE (EXPRESSION/NAME ,symbol) ,inner)
         (loop (cons (list 'case symbol) labels) inner))

        (`(STATEMENT/CONTINUE) (make <c-continue> :labels labels))

        (`(STATEMENT/DECLARATION
           (SIMPLE-DECLARATION ,body))
         (parse-statement-declaration ctenv labels body))

        (`(STATEMENT/DEFAULT ,inner)
         (loop (cons 'default labels) inner))

        (`(STATEMENT/DO-WHILE ,body ,test)
         (parse-do-while-statement ctenv labels (parse-statement ctenv body) (parse-expression ctenv '() test)))

        (`(STATEMENT/IF ,condition ,consequence ,alternative)
         (parse-if-statement ctenv
                             labels
                             (parse-condition ctenv condition)
                             (parse-statement ctenv consequence)
                             (if alternative (parse-statement ctenv alternative) #f)))

        (`(STATEMENT/FOR ,init ,test ,step ,body)
         (parse-for-statement ctenv
                              labels
                              (parse-expression ctenv '() init)
                              (parse-condition ctenv test)
                              (parse-expression ctenv '() step)
                              (parse-statement ctenv body)))

        (`(STATEMENT/GOTO ,target)
         (make <c-goto>
           :labels labels
           :target target))

        (`(STATEMENT/LABEL ,label ,inner)
         (loop (cons label labels) inner))

        (`(STATEMENT/RETURN ,expression)
         (make <return>
           :labels labels
           :return-value (parse-expression ctenv '() expression)))

        (`(STATEMENT/SWITCH ,test ,body)
         (parse-switch-statement ctenv labels (parse-condition ctenv test) (parse-statement ctenv body)))

        (`(STATEMENT/WHILE ,test ,body)
         (parse-while-statement ctenv labels (parse-condition ctenv test) (parse-statement ctenv body)))

        (_
         (evil-error 'parse-statement "Unrecognized statement" statement)))))

;; Should do class-local scope here, but punt
  (define (enter-scope-for-parse ctenv decl)
    ctenv)

  (define (parse-function-body ctenv body)
    (plt:match body
      ('(FUNCTION-BLOCK (COMPOUND-STATEMENT #f))
       (make <c-block> :contents '()))

      (`(FUNCTION-BLOCK (COMPOUND-STATEMENT (STATEMENT-LIST . ,statements)))
       (foldr (lambda (statement block)
                (accumulate-block (parse-statement ctenv statement) block))
              (make <c-block> :labels '() :contents '())
              statements))

      (_ (evil-error 'parse-function-body "Unrecognized function body" body))))

  (define (parse-classdef-list ctenv base-classes members class-definitions)
    (if (null? class-definitions)
        (values (reverse base-classes)
                (reverse members))
        (let ((this-declaration (car class-definitions))
              (remaining-declarations (cdr class-definitions)))
          (plt:match this-declaration
            (`(SIMPLE-DECLARATION ,decl)
             (let loop ((parsed (parse-simple-declarations ctenv decl))
                        (new-members members))
               (if (null? parsed)
                   (parse-classdef-list ctenv base-classes new-members remaining-declarations)
                   (let ((this-parsed (first parsed))
                         (more-parsed (rest parsed)))
                     ;; (evil-message "Declaring member" (qualified-name (identifier this-parsed)))
                     (loop more-parsed (cons this-parsed new-members))))))

            (`(ACCESSIBILITY-SPECIFIER ,spec)
             ;; (evil-message "Ignoring" spec)
             (parse-classdef-list ctenv base-classes members remaining-declarations))

            (_
             (evil-error 'parse-classdef-list "Unrecognized decl element" this-declaration))))))

  (define (parse-base-specifier-list ctenv basespec)
    (let loop ((specs basespec)
               (bases '()))
      (if (null? specs)
          (reverse bases)
          (plt:match (car specs)
            (`(ACCESS-BASE-SPECIFIER ,basespec ,access)
             ;; (evil-message "Ignoring access mode" access)
             (loop (cons basespec (cdr specs)) bases))
            (`(BASE-SPECIFIER ,(? string? name))
             (loop (cdr specs)
                   (cons (->c-identifier name)
                         bases)))
            (_
             (evil-error 'parse-base-specifier-list "unrecognized base specifier" (car specs)))))))

  (define (parse-classdef ctenv base classdef)
    (plt:match base
      (#f
       (parse-members ctenv '() classdef))
      (`(BASE-SPECIFIER-LIST . ,specs)
       (parse-members ctenv (parse-base-specifier-list ctenv specs) classdef))
      (_
       (evil-error 'parse-classdef "  unrecognized base specifier" base))))

  (define (parse-members ctenv base classdef)
    (plt:match classdef
      (#f
       (parse-classdef-list ctenv base '() '()))
      (`(CXXMEMBERDECLARATIONS . ,decls)
       (parse-classdef-list ctenv base '() decls))
      (_
       (evil-error 'parse-members "Unrecognized classdef" classdef))))

  (define (parse-enumerator ctenv enumerator)
    (plt:match enumerator
      (`(ENUMERATOR ,(? string? name) ,value)
       (->c-identifier name))
      (_
       (evil-error 'parse-enumerator enumerator))))

  (define (parse-enumdef ctenv enumdef)
    (plt:match enumdef
      (`(ENUMERATOR-LIST . ,elements)
       (map (lambda (element) (parse-enumerator ctenv element)) elements))
      (_
       (evil-error 'parse-enumdef enumdef))))

  (define (parse-type-specifier ctenv typespec)
    ;; (if (null? ctenv) (error (list 'parse-type-specifier ctenv)))
    ;; (evil-message "Parse type specifier"  typespec)
    (let loop ((typespec typespec)
               (prefix #f))
      ;; (evil-message "Parse type specifier loop"  typespec)
      (if (string? typespec)
          (let* ((converted-name (make-c-identifier
                                  prefix typespec))
                 (user-type (lookup-type ctenv converted-name)))
            (if user-type
                ;; (make-type-specifier   user-type)
                (make-type-specifier (list converted-name))
                (evil-error 'parse-type-specifier
                            "Unknown user type" (list typespec
                                                      converted-name
                                                      ))))

          (plt:match typespec
            ('TOKEN/__INT64  c-type:__int64)
            ('TOKEN/__PTR64  c-type:__ptr64)
            ('TOKEN/CHAR     c-type:char)
            ('TOKEN/DOUBLE   c-type:double)
            ('TOKEN/FLOAT    c-type:float)
            ('TOKEN/INT      c-type:int)
            ('TOKEN/LONG     c-type:long)
            ('TOKEN/SHORT    c-type:short)
            ('TOKEN/SIGNED   c-type:signed)
            ('TOKEN/UNSIGNED c-type:unsigned)
            ('TOKEN/VOID     c-type:void)
            ('TOKEN/WCHAR_T  c-type:wchar_t)

            ('(BUILT-IN-NAME (BUILT-IN-NAME TOKEN/UNSIGNED TOKEN/SHORT) TOKEN/WCHAR_T) c-type:ushort-wchar)
            ('(BUILT-IN-NAME (BUILT-IN-NAME TOKEN/__W64 TOKEN/UNSIGNED) TOKEN/INT)     c-type:__w64-uint)
            ('(BUILT-IN-NAME (BUILT-IN-NAME TOKEN/__W64 TOKEN/UNSIGNED) TOKEN/LONG)    c-type:__w64-ulong)
            ('(BUILT-IN-NAME TOKEN/LONG TOKEN/DOUBLE)                                  c-type:long-double)
            ('(BUILT-IN-NAME TOKEN/LONG TOKEN/LONG)                                    c-type:long-long)
            ('(BUILT-IN-NAME TOKEN/SHORT TOKEN/INT)                                    c-type:short-int)
            ('(BUILT-IN-NAME TOKEN/SIGNED TOKEN/CHAR)                                  c-type:schar)
            ('(BUILT-IN-NAME TOKEN/SIGNED TOKEN/INT)                                   c-type:sint)
            ('(BUILT-IN-NAME TOKEN/SIGNED TOKEN/LONG)                                  c-type:slong)
            ('(BUILT-IN-NAME TOKEN/SIGNED TOKEN/SHORT)                                 c-type:sshort)
            ('(BUILT-IN-NAME TOKEN/SIGNED TOKEN/__INT64)                               c-type:signed-__int64)
            ('(BUILT-IN-NAME TOKEN/UNSIGNED TOKEN/CHAR)                                c-type:uchar)
            ('(BUILT-IN-NAME TOKEN/UNSIGNED TOKEN/INT)                                 c-type:uint)
            ('(BUILT-IN-NAME TOKEN/UNSIGNED TOKEN/LONG)                                c-type:ulong)
            ('(BUILT-IN-NAME TOKEN/UNSIGNED TOKEN/SHORT)                               c-type:ushort)
            ('(BUILT-IN-NAME TOKEN/UNSIGNED TOKEN/__INT64)                             c-type:unsigned-__int64)
            ('(BUILT-IN-NAME TOKEN/__W64 TOKEN/INT)                                    c-type:__w64-int)
            ('(BUILT-IN-NAME TOKEN/__W64 TOKEN/LONG)                                   c-type:__w64-long)
            ('(BUILT-IN-NAME TOKEN/__W64 TOKEN/UNSIGNED)                               c-type:__w64-unsigned)

            (`(DECL-SPECIFIER-NAME ,inner TOKEN/CONST)
             ;; just ignore the const
             (loop inner prefix))

            (`(EXPRESSION/NAME ,(? string? name))
             (loop name prefix))

            ;; anonymous struct
            (`(EXPRESSION/NAME (CLASS-MEMBER-LIST (CLASS-SPECIFIER-ID TOKEN/STRUCT #f ,thing) ,decls))
             c-type:anonymous-struct)

            (`(EXPRESSION/NAME (CLASS-MEMBER-LIST (CLASS-SPECIFIER-ID TOKEN/STRUCT ,(? string? name) ,thing) ,decls))
             (make-structure-type (make-c-identifier prefix name)))

            ;; anonymous union
            (`(EXPRESSION/NAME (CLASS-MEMBER-LIST (CLASS-SPECIFIER-ID TOKEN/UNION #f ,thing) ,decls))
             c-type:anonymous-union)

            (`(EXPRESSION/NAME (CLASS-MEMBER-LIST (CLASS-SPECIFIER-ID TOKEN/UNION ,(? string? name) ,thing) ,decls))
             (make-union-type (make-c-identifier prefix name)))

            (`(EXPRESSION/NAME (DECL-SPECIFIER-NAME ,(? string? name) TOKEN/CONST))
             (let* ((converted-name (make-c-identifier prefix name))
                    (user-type (lookup-type ctenv converted-name)))
               (if user-type
                   (make-type-specifier (list c-identifier:const converted-name))
                   (evil-error 'parse-type-specifier "Unknown user type" converted-name))))

            (`(EXPRESSION/NAME (DECL-SPECIFIER-NAME ,(? string? name) TOKEN/VOLATILE))
             (let* ((converted-name (make-c-identifier
                                     prefix name))
                    (user-type (lookup-type ctenv converted-name)))
               (if user-type
                   (make-type-specifier (list c-identifier:volatile converted-name))
                   (evil-error 'parse-type-specifier "Unknown user type" converted-name))))

            (`(EXPRESSION/NAME (ELABORATED-TYPE-SPECIFIER TOKEN/CLASS ,(? string? name)))
             (make-class-type (make-c-identifier prefix name)))

            (`(EXPRESSION/NAME (ELABORATED-TYPE-SPECIFIER TOKEN/STRUCT ,(? string? name)))
             (make-structure-type (make-c-identifier prefix name)))

            (`(EXPRESSION/NAME (ELABORATED-TYPE-SPECIFIER TOKEN/UNION ,(? string? name)))
             (make-union-type (make-c-identifier prefix name)))

            (`(EXPRESSION/NAME (ENUM ,(? string? name) ,values))
             (make-enum-type (make-c-identifier prefix name)))

            ;; anonymous enum
            (`(EXPRESSION/NAME (ENUM #f ,values))
             c-type:anonymous-enum)

            (`(TYPED-NAME ,inner (EXPRESSION/NAME ,(? string? affix)))
             (loop inner (cond ((string=? affix "_cdecl") #f)
                               (prefix (make-c-identifier
                                        prefix
                                        affix))
                               (else (make-c-identifier
                                      #f
                                      affix)))))

            (_ (evil-error 'parse-type-specifier "Unrecognized type specifier" typespec))))))

  (defgeneric process-declaration (declaration ctenv))

  (defmethod (process-declaration declaration ctenv)
    (evil-error 'process-declaration "Cannot process declaration" declaration))

  (defmethod (process-declaration (declaration <c-declaration>) ctenv)
    (cond ((eq? (declaration/storage-class declaration) :typedef)
           ;; (evil-message "Declaring typedef" (declaration/identifier declaration))
           (declare-type! ctenv (declaration/identifier declaration) declaration))
          ((function-declarator? (declaration/declarator declaration))
           (display ".")
           (flush-output))
          (else (evil-message "Ignoring declaration:  ")
                (emit-c-code declaration)
                (display "; ")))
    ctenv)

  (defmethod (process-declaration (declaration <c-enum-declaration>) ctenv)
    ;; (evil-message "processing enum")
    (let ((ft (type-specifier->list (declaration/type declaration))))
;      (evil-message "ft"
;                    ft
;                    (pair? ft)
;                    (and (pair? ft) (equal? (car ft) "enum"))
;                    (and (pair? ft)
;                         (equal? (car ft) "enum")
;                         (not (eq? (cadr ft) '*anonymous*))))
      (if (and (pair? ft)
               (equal? (car ft) "enum")
               (not (equal? (cadr ft) "*anonymous*")))
          (begin
            ;; (evil-message "Declaring enum" (cadr ft))
            (declare-type! ctenv (->c-identifier (cadr ft)) declaration))
          (begin
            (evil-message "Ignoring enum" declaration ft (declaration/storage-class declaration) (declaration/identifier declaration))
            #f)))
    ctenv)

  (defmethod (process-declaration (declaration <incomplete-struct-declaration>) ctenv)
    ;; (evil-message "processing incomplete struct/class")
    (let ((ft (type-specifier->list (declaration/type declaration))))
      ;; (evil-message "ft" ft)
      (if (and (pair? ft)
               (or (equal? (car ft) "struct")
                   (equal? (car ft) "class"))
               (not (eq? (cadr ft)  "*anonymous*")))
          (begin
            ;; (evil-message "Declaring incomplete struct" (cadr ft))
            (declare-type! ctenv (->c-identifier (cadr ft)) declaration))
          (evil-message "Ignoring" declaration)))
    ctenv)

  (defmethod (process-declaration (declaration <c-struct-declaration>) ctenv)
    ;; (evil-message "processing struct/class")
    (let ((ft (type-specifier->list (declaration/type declaration))))
      ;; (evil-message "ft" ft)
      (if (and (pair? ft)
               (or (equal? (car ft) "struct")
                   (equal? (car ft)  "class"))
               (not (eq? (cadr ft)  "*anonymous*")))
          (begin
            ;; (evil-message "Declaring struct" (cadr ft))
            (declare-type! ctenv (->c-identifier (cadr ft)) declaration))
          (evil-message "Ignoring" declaration)))
    ctenv)


  (define (parse-declaration ctenv labels storage qual wrapper decl)
    ;; A C type declaration can be broken into two distinct parts:  a type
    ;; specifier and a declarator.  The type specifier is the easy part:  it
    ;; declares the (optional) storage class (either auto, static,
    ;; register or extern) and a type (such as int or unsigned char).  The
    ;; declarator is the part that causes confusion, and specifies whether
    ;; a variable is to be considered a simple type, a pointer, an array,
    ;; a function, or a combination of these.  The identifier, the variable
    ;; name, is found in the middle of the declarator.

    ;; But what we have is a syntactic parse, so we have to a bit of poking around.
    ;; (evil-message "PARSE-DECLARATION" decl)
    ;; (if (null? ctenv) (error 'parse-declaration))
    (plt:match decl
      (`(AND ,type ,name)
       (make-c-declaration
        labels
        storage
        qual
        (parse-type-specifier ctenv type)
        (parse-declarator ctenv (lambda (inner)
                                  (make <reference-declarator>
                                    :inner-declarator inner
                                    :qualifier qual))
                          name)))

      (`(ASTERISK ,asterisk-qualifier ,left (ABSTRACT-ARRAY ,size))
       (parse-array-declaration ctenv labels storage qual wrapper
                                `(ASTERISK ,asterisk-qualifier ,left (EXPRESSION-LIST #f)) size))

      (`(ASTERISK ,asterisk-qualifier ,left ,right)
       (parse-asterisk-declaration ctenv labels storage qual wrapper asterisk-qualifier left right))

      (`(ARRAY ,left-component ,right-component)
       (parse-array-declaration ctenv labels storage qual  wrapper left-component right-component))

      (`(COLON ,inner ,width)
       ;; (evil-message "Ignoring bitfield width")
       (parse-declaration ctenv labels storage qual wrapper inner))

      (`(DECL-SPECIFIER-PARAMETER
         (EXPRESSION/PARAMETER ,inner)
         TOKEN/CONST)
       (parse-declaration ctenv labels storage :const  wrapper inner))

      (`(DECL-SPECIFIER-PARAMETER
         (EXPRESSION/PARAMETER ,inner)
         TOKEN/VOLATILE)
       (parse-declaration ctenv labels storage :volatile  wrapper inner))

      (`(EXPRESSION/ASSIGNMENT = ,declaration ,initial-value)
       (let ((decl (parse-declaration ctenv labels storage qual  wrapper declaration)))
         ;; (evil-message "Ignoring assignment...")
         decl))

      (`(EXPRESSION/CALL ,left-component ,right-component)
       (let ((decl (parse-function-declaration ctenv labels storage qual  wrapper left-component right-component)))
         (evil-message "Function " (c-identifier->string (declaration/identifier decl)))
         decl))

      (`(EXPRESSION/CTOR-DEFINITION (EXPRESSION-LIST (COLON ,signature ,initializations)) ,body)
       (let* ((decl (parse-declaration ctenv '() storage qual  wrapper signature))
              (id  (declaration/identifier decl)))
         (make <c-method-definition>
           :labels labels
           :storage-class (declaration/storage-class decl)
           :qualifier (declaration/qualifier decl)
           :type (declaration/type decl)
           :declarator (declaration/declarator decl)
           :body (parse-function-body (enter-scope-for-parse ctenv decl) body))))

      (`(EXPRESSION/DECL-SPECIFIER ,inner TOKEN/FRIEND)
       ;; (evil-message "Ignoring friend specification")
       (parse-declaration ctenv labels storage qual wrapper inner))

      (`(EXPRESSION/DECL-SPECIFIER ,inner TOKEN/INLINE)
       ;; (evil-message "Ignoring inline specification")
       (parse-declaration ctenv labels storage qual wrapper inner))

      (`(EXPRESSION/DECL-SPECIFIER ,inner TOKEN/VIRTUAL)
       ;; (evil-message "Ignoring virtual specification")
       (parse-declaration ctenv labels storage qual wrapper inner))

      (`(EXPRESSION/FUNCTION-DEFINITION ,signature ,body)
       (let* ((decl (parse-declaration ctenv '() storage qual  wrapper signature))
              (id  (declaration/identifier decl)))
         (if (identifier/scope id)
             (make <c-method-definition>
               :labels labels
               :storage-class (declaration/storage-class decl)
               :qualifier (declaration/qualifier decl)
               :type (declaration/type decl)
               :declarator (declaration/declarator decl)
               :body (parse-function-body (enter-scope-for-parse ctenv decl) body))
             (make <c-function-definition>
               :labels labels
               :storage-class (declaration/storage-class decl)
               :qualifier (declaration/qualifier decl)
               :type (declaration/type decl)
               :declarator (declaration/declarator decl)
               :body (parse-function-body (enter-scope-for-parse ctenv decl) body)))))

      (`(EXPRESSION/NAME (ELABORATED-TYPE-SPECIFIER TOKEN/STRUCT ,(? string? name)))
       (make <incomplete-struct-declaration>
         :labels labels
         :type (make-structure-type (->c-identifier name))
         :declarator (make <direct-declarator>
                       :identifier c-identifier:*anonymous*)
         :identifier (->c-identifier name)))

      (`(EXPRESSION/NAME (ELABORATED-TYPE-SPECIFIER TOKEN/CLASS ,(? string? name)))
       (make <incomplete-struct-declaration>
         :labels labels
         :type (make-class-type (->c-identifier name))
         :declarator (make <direct-declarator>
                       :identifier c-identifier:*anonymous*)
         :identifier (->c-identifier name)))

      (`(EXPRESSION/NAME (ENUM ,name ,enumdef))
       (make <c-enum-declaration>
         :labels labels
         :storage-class storage
         :qualifier qual
         :type (make-enum-type (->c-identifier (or name '*anonymous*)))
         :declarator (wrapper (make <direct-declarator>
                                :identifier c-identifier:*anonymous*))
         :members (parse-enumdef ctenv enumdef)))

      (`(EXPRESSION/NAME (CLASS-MEMBER-LIST (CLASS-SPECIFIER-ID TOKEN/CLASS ,(? string? name) ,base) ,classdef))
       (declare-type! ctenv (->c-identifier name)
                      (make <incomplete-struct-declaration>
                        :type (make-class-type (->c-identifier name))
                        :declarator (make <direct-declarator>
                                      :identifier c-identifier:*anonymous*)
                        :identifier (->c-identifier name)))
       (multiple-value-bind (base-classes members) (parse-classdef
                                                    ctenv
                                                    base
                                                    classdef)
         ;; (evil-message "base-classes" base-classes)
         (make <c-struct-declaration>
           :labels labels
           :storage-class storage
           :qualifier qual
           :type (make-class-type (->c-identifier name))
           :declarator (wrapper (make <direct-declarator>
                                  :identifier (->c-identifier name)))
           :base-classes base-classes
           :members members)))

      (`(EXPRESSION/NAME (CLASS-MEMBER-LIST (CLASS-SPECIFIER-ID TOKEN/STRUCT ,(? string? name) ,base) ,classdef))
       (declare-type! ctenv (->c-identifier name)
                      (make <incomplete-struct-declaration>
                        :type (make-structure-type (->c-identifier name))
                        :declarator (make <direct-declarator>
                                      :identifier c-identifier:*anonymous*)
                        :identifier (->c-identifier name)))
       (multiple-value-bind (base-classes members) (parse-classdef ctenv base classdef)
         ;; (evil-message "base-classes:  " base-classes)
         (make <c-struct-declaration>
           :labels labels
           :storage-class storage
           :qualifier qual
           :type (make-structure-type (->c-identifier  name))
           :declarator (wrapper (make <direct-declarator>
                                  :identifier (->c-identifier name)))
           :base-classes base-classes
           :members members)))

      (`(EXPRESSION/NAME (CLASS-MEMBER-LIST (CLASS-SPECIFIER-ID TOKEN/UNION ,name ,base) ,classdef))
       (make <c-struct-declaration>
         :labels labels
         :storage-class storage
         :qualifier qual
         :type (make-union-type (->c-identifier (if (string? name) name '*anonymous*)))
         :declarator (wrapper (make <direct-declarator>
                                :identifier (if (string? name)
                                                (->c-identifier name)
                                                '*anonymous*)))))

      (`(TILDE (EXPRESSION/CALL (EXPRESSION/NAME ,name) ,arguments))
       (make-c-declaration
        labels
        storage
        qual
        c-type:void
        (make <method-declarator>
          :inner-declarator
          (make <direct-declarator>
            :identifier (->c-identifier
                         (string-append "~" name))))))

      (`(TYPED-NAME (EXPRESSION/NAME (ENUM ,enum-name ,enumdef)) (EXPRESSION/NAME ,(? string? name)))
       ;; (evil-message 'parse-declaration "Enum" enum-name enumdef name)
       (let* ((decl (make <c-enum-declaration>
                      :labels labels
                     :storage-class storage
                     :qualifier qual
                     :type (make-enum-type (->c-identifier (or enum-name '*anonymous*)))
                     :declarator (wrapper (make <direct-declarator>
                                            :identifier (->c-identifier name)))
                     :members (parse-enumdef ctenv enumdef))))
         (if (eq? storage :typedef)
             (declare-type! ctenv (->c-identifier name) decl))
         decl))

      (`(TYPED-NAME ,type ,declarator)
                                        ;; (evil-message "Parse-declaration: " decl)
       (make-c-declaration
        labels
        storage
        qual
        (parse-type-specifier ctenv type)
        (parse-declarator ctenv wrapper declarator)))

      (_
       (make-c-declaration
        labels
        storage
        qual
        (parse-type-specifier ctenv decl)
        (wrapper (make <direct-declarator>
                   :identifier c-identifier:*ignore*))))

                                        ;;(_ (evil-message "parse-declaration:  Unknown declaration" decl)
                                        ;;   (error "parse-declaration:  Unknown declaration"))
      ))


;; return a list of variables, their storage, their types, and their initial values.
  (define (parse-simple-declarations ctenv decl)
;  (evil-message "")
;  (evil-message "")
;  (evil-message "parse-simple-declarations" decl)
    (let loop ((decl        decl)
               (storage-class #f)
               (qualifier    #f))
      (plt:match decl

        ;;  unsigned short wchar_t;
        (`(BUILT-IN-NAME (BUILT-IN-NAME TOKEN/UNSIGNED TOKEN/SHORT) TOKEN/WCHAR_T)
         ;; superfluous WCHAR_T definition
         (evil-message "Ignoring superfluous WCHAR_T typedef.")
         '())

        ;; __declspec(dllimport) <function-declaration>;
        (`(EXPRESSION/DECL-SPECIFIER
           (CAST (EXPRESSION/ABSTRACT-FUNCTION
                  (PARENTHESISED
                   (PARAMETER-LIST
                    (EXPRESSION/PARAMETER ,declspec))
                   #F #F))
                 ,inner)
           TOKEN/__DECLSPEC)
         ;; (evil-message "Ignoring __declspec(" declspec ")")
         (loop inner storage-class qualifier))

        ;; __declspec(dllimport) <function-declaration>;
        (`(EXPRESSION/DECL-SPECIFIER
           (CAST (EXPRESSION/ABSTRACT-FUNCTION
                  (PARENTHESISED
                   (PARAMETER-LIST
                    (EXPRESSION/PARAMETER ,declspec))
                   (CV-QUALIFIER-LIST #F TOKEN/VOLATILE) #F))
                 ,inner)
           TOKEN/__DECLSPEC)
         ;; (evil-message "Ignoring __declspec(" declspec ")")
         (if qualifier
             (error "multiple qualifiers")
             (loop inner storage-class :volatile)))

        ;; __declspec(dllimport) char * <function-declaration>;
        (`(EXPRESSION/DECL-SPECIFIER
           (ASTERISK (POINTER-DECLARATOR)
                     (CAST (EXPRESSION/ABSTRACT-FUNCTION
                            (PARENTHESISED
                             (PARAMETER-LIST
                              (EXPRESSION/PARAMETER ,declspec))
                             #F #F))
                           ,innera)
                     ,innerb)
           TOKEN/__DECLSPEC)
         ;; (evil-message "Ignoring __declspec(" declspec ")")
         (loop `(ASTERISK (POINTER-DECLARATOR) ,innera ,innerb) storage-class qualifier))

        ;; __declspec(dllimport) char * <function-declaration>;
        (`(EXPRESSION/DECL-SPECIFIER
           (ASTERISK (POINTER-DECLARATOR)
                     (CAST (EXPRESSION/ABSTRACT-FUNCTION
                            (PARENTHESISED
                             (PARAMETER-LIST
                              (EXPRESSION/PARAMETER ,declspec))
                             (CV-QUALIFIER-LIST #F TOKEN/CONST) #F))
                           ,innera)
                     ,innerb)
           TOKEN/__DECLSPEC)
         ;; (evil-message "Ignoring __declspec(" declspec ")")
         (if qualifier
             (error "Multiple qualifiers")
             (loop `(ASTERISK (POINTER-DECLARATOR) ,innera ,innerb) storage-class :const)))

        (`(EXPRESSION/DECL-SPECIFIER ,inner TOKEN/__INLINE)
         ;; (evil-message "Ignoring __inline declaration.")
         (loop inner storage-class qualifier))

        (`(EXPRESSION/DECL-SPECIFIER ,inner TOKEN/__FORCEINLINE)
         ;; (evil-message "Ignoring __forceinline declaration.")
         (loop inner storage-class qualifier))

        (`(EXPRESSION/DECL-SPECIFIER ,inner TOKEN/_INLINE)
         ;; (evil-message "Ignoring _inline declaration.")
         (loop inner storage-class qualifier))

        (`(EXPRESSION/DECL-SPECIFIER ,inner TOKEN/INLINE)
         ;; (evil-message "Ignoring inline declaration.")
         (loop inner storage-class qualifier))

        (`(EXPRESSION/DECL-SPECIFIER ,inner TOKEN/AUTO)
         (if storage-class
             (error "Multiple storage classes.")
             (loop inner 'auto qualifier)))

        (`(EXPRESSION/DECL-SPECIFIER ,inner TOKEN/EXTERN)
         (if storage-class
             (error "Multiple storage classes.")
             (loop inner :extern qualifier)))

        (`(EXPRESSION/DECL-SPECIFIER ,inner TOKEN/REGISTER)
         (if storage-class
             (error "Multiple storage classes.")
             (loop inner 'register qualifier)))

        (`(EXPRESSION/DECL-SPECIFIER ,inner TOKEN/STATIC)
         (if storage-class
             (error "Multiple storage classes.")
             (loop inner 'static qualifier)))

        (`(EXPRESSION/DECL-SPECIFIER ,inner TOKEN/TYPEDEF)
         (if storage-class
             (error "Multiple storage classes.")
             (loop inner :typedef qualifier)))

        (`(EXPRESSION/DECL-SPECIFIER ,inner TOKEN/CONST)
         (if qualifier
             (error "Multiple qualifiers.")
             (loop inner storage-class 'const)))

        (`(EXPRESSION/DECL-SPECIFIER ,inner TOKEN/VOLATILE)
         (if qualifier
             (error "Multiple qualifiers.")
             (loop inner storage-class 'volatile)))

        (`(EXPRESSION-LIST #f) '())

        (`(EXPRESSION-LIST ,declaration . ,declarators)
         ;; (evil-message "Parsing expression-list")
         (let* ((parsed-declaration (parse-declaration ctenv '() storage-class qualifier identity declaration))
                (typespec (declaration/type parsed-declaration)))
           (cons parsed-declaration
                 (map (lambda (declarator)
                        (plt:match declarator
                          (`(EXPRESSION/ASSIGNMENT = ,declarator ,initial-value)
                           (evil-message "Ignoring assignment...")
                           (make-c-declaration
                            '()
                            storage-class
                            qualifier
                            typespec
                            (parse-declarator
                             ctenv identity
                             declarator)))
                          (_ (make-c-declaration
                              '()
                              storage-class
                              qualifier
                              typespec
                              (parse-declarator
                               ctenv identity
                               declarator)))))
                      declarators))))

        (`(EXPRESSION/NAME LONG64)
         (evil-message "Ignoring this:" decl))

        (_ (list (parse-declaration ctenv '() storage-class qualifier identity decl))))))

  (define (parse-top-level-declaration ctenv expression)
    (let ((decls (parse-simple-declarations ctenv expression)))
      ;; (evil-message "Decls are" decls)
      (values decls
              (foldl process-declaration ctenv decls))))

  (define (parse-top-level-form ctenv expression)
    (plt:match expression
      (`(LINKAGE-SPECIFICATION
         (LINKAGE-SPECIFIER ,language #F))
       (values '() ctenv))

      (`(LINKAGE-SPECIFICATION
         (LINKAGE-SPECIFIER ,language
                            (DECLARATION-LIST . ,decls)))
       (parse-top-level-forms ctenv decls))

      (`(SIMPLE-DECLARATION ,form)
       (parse-top-level-declaration ctenv form))

      (`(TEMPLATE-DECLARATION ,etc ...)
       (evil-message "Ignoring template declaration.")
       (values '() ctenv))

      (`(,key ,etc ...) (error "Unrecognized top level form:  " key etc))
      (_ (error "Unrecognized top level form:  " expression))))

  (define (parse-top-level-forms ctenv form-list)
    (let loop ((parsed '())
               (remaining form-list)
               (env    ctenv))
      (cond ((pair? remaining)
             (multiple-value-bind (parsed-form augmented-ctenv)
                 (parse-top-level-form env (car remaining))
               (loop (append (reverse parsed-form) parsed)
                     (cdr remaining)
                     augmented-ctenv)))
            ((null? remaining)
             (values (reverse! parsed) env))
            (else (error "Not a list of forms: " form-list)))))

  (define (parse-top-level form-list)
    (multiple-value-bind (parsed augmented-ctenv)
        (parse-top-level-forms (initial-type-table) form-list)
      (values (make <c-translation-unit>
                :contents parsed)
              augmented-ctenv)))

  (provide
   parse-top-level))

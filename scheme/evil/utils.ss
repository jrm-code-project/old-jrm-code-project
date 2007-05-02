#ci(module utils (lib "swindle.ss" "swindle")
;;; Evil utilities
  (require (lib "list.ss"))

  (define (any? procedure list)
    (and (pair? list)
         (or (procedure (car list))
             (any? procedure (cdr list)))))

  (define (not-any? procedure list)
    (cond ((pair? list) (and (not (procedure (car list)))
                             (not-any? procedure (cdr list))))
          ((null? list) #t)
          (else (error "Improper list."))))

  (define (every? procedure list)
    (cond ((pair? list) (and (procedure (car list))
                             (every? procedure (cdr list))))
          ((null? list) #t)
          (else (error "Improper list."))))

  (define (find thing list key)
    (let ((probe (memf (lambda (item)
                         (eq? (key item) thing)) list)))
      (if (pair? probe)
          (car probe)
          probe)))

  (define-syntax multiple-value-bind
    (syntax-rules ()
      ((multiple-value-bind bindings values-producer . body)
       (CALL-WITH-VALUES
        (LAMBDA () values-producer)
        (LAMBDA bindings . body)))))

  (define-syntax multiple-value-list
    (syntax-rules ()
      ((multiple-value-list values-producer)
       (CALL-WITH-VALUES (LAMBDA () values-producer) LIST))))

  (define-syntax multiple-value-setq
    (syntax-rules ()
      ;; no rule for 0 (pointless)
      ;; no rule for 1 (use setq)
      ((multiple-value-setq (var0 var1) values-producer)
       (CALL-WITH-VALUES (LAMBDA () values-producer)
         (LAMBDA (TEMP0 TEMP1)
           (SET! var0 TEMP0)
           (SET! var1 TEMP1))))
      ((multiple-value-setq (var0 var1 var2) values-producer)
       (CALL-WITH-VALUES (LAMBDA () values-producer)
         (LAMBDA (TEMP0 TEMP1 TEMP2)
           (SET! var0 TEMP0)
           (SET! var1 TEMP1)
           (SET! var2 TEMP2))))
      ((multiple-value-setq (var0 var1 var2 var3) values-producer)
       (CALL-WITH-VALUES (LAMBDA () values-producer)
         (LAMBDA (TEMP0 TEMP1 TEMP2 TEMP3)
           (SET! var0 TEMP0)
           (SET! var1 TEMP1)
           (SET! var2 TEMP2)
           (SET! var3 TEMP3))))
      ((multiple-value-setq (var0 var1 var2 var3 var4) values-producer)
       (CALL-WITH-VALUES (LAMBDA () values-producer)
         (LAMBDA (TEMP0 TEMP1 TEMP2 TEMP3 TEMP4)
           (SET! var0 TEMP0)
           (SET! var1 TEMP1)
           (SET! var2 TEMP2)
           (SET! var3 TEMP3)
           (SET! var4 TEMP4))))))

  (define-syntax nth-value
    (syntax-rules ()
      ((nth-value 0 values-producer)
       (CALL-WITH-VALUES (LAMBDA () values-producer)
         (LAMBDA (VALUE0 . OTHERS) VALUE0)))

      ((nth-value 1 values-producer)
       (CALL-WITH-VALUES (LAMBDA () values-producer)
         (LAMBDA (VALUE0 VALUE1 . OTHERS) VALUE1)))

      ((nth-value 2 values-producer)
       (CALL-WITH-VALUES (LAMBDA () values-producer)
         (LAMBDA (VALUE0 VALUE1 VALUE2 . OTHERS) VALUE2)))

      ((nth-value 3 values-producer)
       (CALL-WITH-VALUES (LAMBDA () values-producer)
         (LAMBDA (VALUE0 VALUE1 VALUE2 VALUE3 . OTHERS) VALUE3)))

      ((nth-value n values-producer)
       (LIST-REF (MULTIPLE-VALUE-LIST values-producer) N))))

  (define *evil-verbose* #t)

  (define (evil-message text . objects)
    (if *evil-verbose*
        (begin
          (newline)
          (display "evil:  ")
          (display text)
          (for-each (lambda (object)
                      (display " ")
                      (display object))
                    objects)
          (flush-output))))

  (define *evil-error-depth* (make-parameter 0))

  (define (evil-error where . stuff)
    (if (> (*evil-error-depth*) 5)
        (begin (display "error too deep") #f)
        (parameterize ((*evil-error-depth* (add1 (*evil-error-depth*))))
          (apply evil-message (string-append (symbol->string where) ": ") stuff)
          (newline)
          (error "Evil error" (cons where stuff)))))

  (define (warn text . stuff)
    (apply evil-message "WARNING: " text stuff))

  (define (convert-c-name string)
    (string->symbol
     (list->string
      (let loop ((chars (string->list string))
                 (prev '())
                 (result '()))
        (if (null? chars)
            (reverse! result)
            (loop (cdr chars)
                  (car chars)
                  (let ((c (car chars)))
                    (cond ((and (char-upper-case? c)
                                (char? prev)
                                (char-lower-case? prev))
                           (list* (char-downcase c) #\- result))
                          ((char=? c #\_) (cons #\- result))
                          (else (cons (char-downcase c) result))))))))))

  (define-syntax declare
    (syntax-rules (usual-integrations)
      ((declare (usual-integrations)) '())))

  (define (class-predicate class)
    (lambda (instance)
      (instance-of? instance class)))

  (provide
   *evil-verbose*
   any?
   class-predicate
   convert-c-name
   every?
   evil-error
   evil-message
   find
   multiple-value-bind
   multiple-value-list
   multiple-value-setq
   nth-value
   warn
   declare
   ))

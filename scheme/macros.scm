(declare (usual-integrations))

;;; Some macros that come in handy.

(define-syntax dotimes
  (syntax-rules ()
    ((dotimes (variable limit-expr return-value ...) body0 body ...)
       (let ((limit limit-expr))
         (letrec ((dotimes (lambda (variable)
			     (if (fix:>= variable limit)
				 ;; Return #F if optional return value
				 ;; clause is omitted.
				 (begin #f return-value ...)
				 (begin body0 body ... (dotimes (fix:+ variable 1)))))))
	   (dotimes 0))))))

(define-syntax unless
  (syntax-rules ()
    ((unless predicate action0 . actions)
     (if predicate
         #f
         (begin action0 . actions)))))

(define-syntax when
  (syntax-rules ()
    ((when predicate action0 . actions)
     (if predicate
         (begin action0 . actions)
         #f))))
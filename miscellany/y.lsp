(in-package "USER")

(defun Y (f)
  ((lambda (x) (funcall f (lambda () (funcall x x))))
   (lambda (x) (funcall f (lambda () (funcall x x))))))

(defvar *f1*
    (lambda (f)
      (lambda (n)
        (if (zerop n)
            1
            (* n (funcall (funcall f) (- n 1)))))))

(defvar *countdown*
    (lambda (f)
      (lambda (n body)
        (funcall body n)
        (unless (zerop n)
          (funcall (funcall f) (- n 1) body)))))

(setf (symbol-function 'fact) (Y *f1*))
(setf (symbol-function 'countdown) (Y *countdown*))

(print (fact 20)) ; 2432902008176640000

(countdown 10 #'print)
;10
;9
;8
;7
;6
;5
;4
;3
;2
;1
;0
NIL

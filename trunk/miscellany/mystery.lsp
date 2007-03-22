(in-package "CL-USER")

;;; Consider these two functions.
;;; The KERNEL function clearly terminates.
;;; The call to REDUCE in MYSTERY clearly terminates.
;;;
;;; MYSTERY is a simple loop that only invokes
;;; functions that terminate, but it is not known
;;; whether or not MYSTERY terminates for all
;;; finite lists.  (It is suspected that it does,
;;; but no one can prove it.)

;;; Nonetheless, the return value of MYSTERY
;;; is obviously T.

(defun kernel (s i)
  (list (not (car s))
        (if (car s)
            (cadr s)
          (cons i (cadr s)))
        (cons 'y (cons i (cons 'z (caddr s))))))

(defconstant k0 '(t () (x)))

(defun mystery (list)
  (let ((result (reduce #'kernel list :initial-value k0)))
    (cond ((null (cadr result)))
          ((car result) (mystery (cadr result)))
          (t (mystery (caddr result))))))

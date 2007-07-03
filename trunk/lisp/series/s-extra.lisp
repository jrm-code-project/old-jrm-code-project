(in-package "SERIES")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(
            collect-adjoin
            collect-union
            scan-directory
            )))

(defun collect-adjoin (series &optional (key #'identity) (test #'eql))
  (declare (optimizable-series-function))
  (collect-fn 'list
              (lambda () '())
              (lambda (set item) (adjoin item set :key key :test test))
              series))

(defun collect-union (series &optional (key #'identity) (test #'eql))
  (declare (optimizable-series-function))
  (collect-fn 'list
              (lambda () '())
              (lambda (set more) (union more set :key key :test test))
              series))

(defun scan-directory (directory)
  (declare (optimizable-series-function))
  (scan 'list (directory directory)))

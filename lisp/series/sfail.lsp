(in-package "CL-USER")

(defun scan-file-lines (filename)
  (declare (optimizable-series-function))
  (scan-file filename #'read-line))

(defun losing-collect ()
  (collect 'list (scan-file-lines "c:\\temp\\test.txt")))

(defun winning-collect ()
  (let ((file "c:\\temp\\test.txt"))
    (collect 'list (scan-file-lines file))))




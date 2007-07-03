(in-package "JRM/UTILITY")

;;; NOTE NOTE NOTE NOTE
;;; If you edit this file, you must rebuild the development image.

(eval-when (:load-toplevel :execute)
  (export '(
	    ))
  (export '(jrm/config::*disable-debug-messages*
            jrm/config::*debug-noise-level*
            jrm/config::*debug-noise-print-level*
            jrm/config::*debug-noise-print-length*)
          "JRM/CONFIG"))




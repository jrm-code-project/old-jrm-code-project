;;; Some extras that I use.
(require 'advice)

;; I mistype things too much.
(defadvice switch-to-buffer (before existing-buffers-only activate)
  "When called interactively switch to existing buffers only, unless
when called with a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: " (other-buffer)
                      (null current-prefix-arg)))))

(defadvice getenv (around windows-home activate)
  "Fix what HOME is on windows."
  (interactive "sEnvironment Variable: ")
  (if (and (eq system-type 'windows-nt)
	   (string= (ad-get-arg 0) "HOME"))
      (setq ad-return-value
	    (concat (ad-Orig-getenv "HOMEDRIVE")
		    (ad-Orig-getenv "HOMEPATH")))
    ad-do-it))

;;; Some lisps do a `helpful' thing of massaging the command line to
;;; strip off `accidental' quotation and such.  This kind of dwimming
;;; makes it impossible to put in *deliberate* quotation when needed!
;;; This is a truly insane hack that bypasses the problem by
;;; generating a lisp form that reconstructs the string from the ascii
;;; character codes.

(defun jrm-encode-string (string)
  (let ((result '("))"))
	(count (length string)))
    (while (> count 0)
      (setq count (1- count))
      (setq result (list* (prin1-to-string (char-int (aref string count)))
			  " "
			  result)))
    (apply 'concat (cons "(map 'string #'code-char '(" result))))


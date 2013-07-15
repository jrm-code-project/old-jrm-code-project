;;; -*-Mode: Lisp -*-

(in-package "FAKE-EXCL")

(eval-when (:load-toplevel :compile-toplevel :execute)
  (export '(if*)))

;;; JRM SEZ:  You get caught using this, I will shoot you!

;; the if* macro used in Allegro:
;;
;; This is in the public domain... please feel free to put this definition
;; in your code or distribute it with your version of lisp.

(defvar if*-keyword-list '("then" "thenret" "else" "elseif"))

(defmacro if* (&rest args)
   (do ((xx (reverse args) (cdr xx))
        (state :init)
        (elseseen nil)
        (totalcol nil)
        (col nil))
       ((null xx)
        (if (eq state :compl)
            `(CL:cond ,@totalcol)
            (error "if*: illegal form ~s" args)))
     (let ((lookat (when (and (symbolp (car xx))
                              (member (symbol-name (car xx))
                                      if*-keyword-list
                                      :test #'string-equal))
                     (symbol-name (car xx)))))
       (ecase state
         (:init (cond ((null lookat)
                       (setq col nil
                             state :col)
                       (push (car xx) col))
                      ((string-equal lookat "thenret")
                       (setq col nil
                             state :then))
                      (t (error "if*: bad keyword ~a" lookat))))

         (:col (cond ((null lookat) (push (car xx) col))
                     ((string-equal lookat "then")
                      (setq state :then))
                     ((string-equal lookat "else")
                      (when elseseen (error "if*: multiple elses"))
                      (setq elseseen t)
                      (setq state :init)
                      (push `(t ,@col) totalcol))
                     (t (error "if*: bad keyword ~s" lookat))))

         (:then (if lookat
                    (error "if*: keyword ~s at the wrong place " (car xx))
                    (progn
                      (setq state :compl)
                      (push `(,(car xx) ,@col) totalcol))))

         (:compl (if (string-equal lookat "elseif")
                     (setq state :init)
                     (error "if*: missing elseif clause ")))))))

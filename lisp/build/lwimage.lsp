(in-package "CL-USER")

(declaim (optimize (compilation-speed 0)
                   (debug 1)
                   (safety 1)
                   (space 0)
                   (speed 3)
                   (hcl:fixnum-safety 3)
                   (float 3)
                   (sys:interruptable 3)))

(load-all-patches)

(let ((*default-pathname-defaults*
       (make-pathname :name nil :type nil :version nil
                      :defaults (first sys:*line-arguments-list*))))
  (load "lib\\4-2-0-0\\config\\configure.lisp"))

;;; We need to ensure that tail recursion is on no matter what.
(setq compiler::*debug-do-tail-merge-policy*      'true
      compiler::*eliminate-tail-recursion-policy* 'true)
(compiler::update-policy-switches)

;; Need these
(require "arraymac")
(require "comm")
(require "formatter")
(require "packiter")
(require "specmac")
(require "twoway-stream")

;;; Create the packages needed for slime/swank.
(if (member "-console" sys:*line-arguments-list* :test #'string-equal)
    (progn
      (load "c:\\jrm-code-project\\emacs\\slime-2.0j\\package.lisp")))

;; Dump the image
(if (member "-console" sys:*line-arguments-list* :test #'string-equal)
    (save-image (cadr (member "-image" sys:*line-arguments-list* :test #'string-equal))
                :console t
                :environment nil
                :restart-function 'mp:initialize-multiprocessing)
    (save-image (cadr (member "-image" sys:*line-arguments-list* :test #'string-equal))))

(quit)

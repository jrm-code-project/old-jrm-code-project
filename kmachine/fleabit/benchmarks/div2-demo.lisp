;;; -*- Mode:LISP; Package:NEW-MATH; Base:10; Readtable:CL -*-


(defun create-n (n)
  (do ((n n (1- n))
       (a () (push () a)))
      ((= n 0) a)))


(defun iterative-div2 (l)
       (do ((l l (cddr l))
            (a () (push (car l) a)))
           ((null l) a)))


(defun recursive-div2 (l)
       (cond ((null l) ())
             (t (cons (car l) (recursive-div2 (cddr l))))))


(defun test-1 (l)
  (do ((i 300. (1- i)))
      ((= i 0))
    (iterative-div2 l)
    (iterative-div2 l)
    (iterative-div2 l)
    (iterative-div2 l)))


(defun test-2 (l)
  (do ((i 300. (1- i)))
      ((= i 0))
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)))

(defun init-vm ()
  (setq gr:*cons-cache-free* (hw:unboxed-constant 0))
  (dotimes (i 1024.)
    (hw:write-vma (hw:dpb i (byte 14. 10.) gr:*all-zero*))
    (hw:nop)
    (hw:write-map (hw:dpb (+ i 1024.) (byte 20. 12.) #x8f))
    (hw:write-md gr:*all-zero*)
    (let ((base (hw:dpb i (byte 14. 10.) gr:*all-zero*)))
      (dotimes (j 1024.)
               (hw:vma-start-write (+ base j))))))

(defun cons (car cdr)
  (hw:write-md car)
  (hw:vma-start-write gr:*cons-cache-free*)
  (setq gr:*cons-cache-free* (1+ gr:*cons-cache-free*))
  (hw:write-md cdr)
  (hw:vma-start-write gr:*cons-cache-free*)
  (setq gr:*cons-cache-free* (1+ gr:*cons-cache-free*))
  (- gr:*cons-cache-free* 2))

(defun push (car cdr)
  (hw:write-md car)
  (hw:vma-start-write gr:*cons-cache-free*)
  (setq gr:*cons-cache-free* (1+ gr:*cons-cache-free*))
  (hw:write-md cdr)
  (hw:vma-start-write gr:*cons-cache-free*)
  (setq gr:*cons-cache-free* (1+ gr:*cons-cache-free*))
  (- gr:*cons-cache-free* 2))

(defun car (l)
  (hw:vma-start-read-early l)
  (hw:read-md))

(defun cdr (l)
  (hw:vma-start-read-cdr-early l)
  (hw:read-md))

(defun cddr (l)
  (hw:vma-start-read-cdr-early l)
  (hw:vma-start-read (hw:read-md))
  (hw:read-md))


(defun illop ()
  (hw:nop)
  (hw:nop)
  (hw:write-processor-control #x80)
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (hw:write-processor-control 0)
  (hw:nop)
  (hw:nop)
  nil)

(defafun init-gr ()
  (movei gr:*all-zero* 0)
  (movei gr:*nil* 0)
  (movei gr:*zero* '0)
  (movei gr:*one* '1)
  (movei gr:*two* '2)
  (movei gr:*three* '3)
  (movei gr:*minus-one* '-1)
  (returni nil))

(defun test-div2 ()
  (do () (())
    (init-gr)
    (hw:write-processor-control 7.)
    (init-vm)
    (hw:write-processor-control #x80)
    (hw:nop)
    (hw:nop)
    (hw:nop)
    (hw:nop)
    (hw:write-processor-control 0.)
    (let ((l (create-n 100.)))
      (do ((led 0 (- 1 led)))
          (())
        (setq gr:*cons-cache-free* #x1000)
        (hw:write-memory-control (hw:dpb led (byte 1. 7.) (hw:read-memory-control)))
        (iterative-div2 l)
        ))))

(defun kbug:run-div2 ()
  (lam:k-reset)
  (lam:k-init)
  (lam:k-init-virtual-memory)
  (lam:ch-init)
  (kbug:load-fcns 'new-math:(init-gr init-vm
                                     test-div2 create-n iterative-div2 recursive-div2
                                     test-1 test-2 push
                                 ))
  (kbug:kbug 'test-div2))

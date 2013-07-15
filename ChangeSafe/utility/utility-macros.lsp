;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002 ChangeSafe, LLC
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;;          ChangeSafe, LLC CONFIDENTIAL and PROPRIETARY material.
;;;;
;;;;          ChangeSafe, LLC
;;;;
;;;; This software and information comprise valuable intellectual
;;;; property and trade secrets of ChangeSafe, LLC, developed at
;;;; substantial expense by ChangeSafe, which ChangeSafe intends to
;;;; preserve as trade secrets.  This software is furnished pursuant
;;;; to a written license agreement and may be used, copied,
;;;; transmitted, and stored only in accordance with the terms of such
;;;; license and with the inclusion of the above copyright notice.
;;;; This software and information or any other copies thereof may not
;;;; be provided or otherwise made available to any other person.  NO
;;;; title to or ownership of this software and information is hereby
;;;; transferred.  ChangeSafe, LLC assumes no responsibility for the
;;;; use or reliability of this software.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; File Name:     utility-macros.lsp
;;;; Author:        Joe Marshall
;;;;
;;;; Module Description:  Utility macros
;;;;
;;;; Provides basic macros used by the utility package
;;;; (among others).  These macros are loaded before any
;;;; other macros.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CSF/UTILITY")

(proclaim (standard-optimizations))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(
            cond
            defsubst
            defunimplemented
            defvar-unbound
            deletef
            destructure-function-lambda
            doatimes
            ignore-errors-unless
            ignore-errors-unless-debugging
            ;; letf
            ;; letf*
            logical-pathname-p
            named-lambda
            named-let
            tail-labels
            with-unique-names
            ))
  (export '(csf/config::*ignore-errors-even-if-debugging*)
          "CSF/CONFIG"))


#||
(defun letf-bindings (bindings environment)
  (let ((savers ())
        (setters ())
        (restorers ()))
    (loop for (place values-form) in bindings do
          (multiple-value-bind (vars vals stores setter getter)
              (get-setf-expansion place environment)
        (let ((save (gensym (symbol-name :LETF-SAVED-VALUE-)))
              (store (first stores))
              (multiple-values (rest stores)))
          (setf savers (nconc (nreverse (mapcar #'list vars vals)) savers))
          (push `(,save ,(if multiple-values
                           `(MULTIPLE-VALUE-LIST ,getter)
                           getter))
                savers)
          (push (if multiple-values
                  `(MULTIPLE-VALUE-BIND ,stores ,values-form ,setter)
                  `(LET ((,store ,values-form)) ,setter))
                setters)
          (push (if multiple-values
                  `(MULTIPLE-VALUE-BIND ,stores (VALUES-LIST ,save) ,setter)
                  `(LET ((,store ,save)) ,setter))
                restorers))))
    (values (nreverse savers) (nreverse setters) (nreverse restorers))))

(defmacro letf (bindings &body body &environment environment)
  "Simulate parallel shallow binding of places in BINDINGS around BODY."
  (if bindings
    (multiple-value-bind (savers setters restorers)
        (letf-bindings bindings environment)
      `(LET* (,@savers)
         (UNWIND-PROTECT
             (PROGN
               ,@setters
               ,@body)
             "LETF unbinding"
             ,@restorers)))
    `(progn ,@body)))

(defmacro letf* (bindings &body body &environment environment)
  "Simulate serial shallow binding of places in BINDINGS around BODY."
  (if bindings
      (multiple-value-bind (savers setters restorers)
          (letf-bindings (list (first bindings)) environment)
        `(LET* (,@savers)
           (UNWIND-PROTECT
               (PROGN
                 ,@setters
                 ,`(LETF* ,(rest bindings) ,@body))
             "LETF* unbinding"
             ,@restorers)))
      `(PROGN ,@body)))
||#

 ;;; Allegro doesn't obey INLINE declaimations.
;;; DEFSUBST is a macro that gets around this restriction.
;;; DEFSUBST works like DEFUN, but defines a COMPILER-MACRO that inlines the
;;; form.  Use this judiciously on small functions only.
;;; While it works with unusual lambda lists, you might get
;;; unused variable warnings if you defsubst something with keyword or
;;; optional args.

#+allegro
(defparameter *warn-if-inlining* nil "If T, be verbose about inlining when compiling.")

#+allegro
(defparameter *warn-if-not-inlining* nil "If T, be verbose about not inlining when compiling.")

#+allegro
(defmacro defsubst (name lambda-list &body body)
  "Identical to DEFUN except that it arranges for the body to be INLINED
   in the places it is called."
  (let ((form-var (gensym (symbol-name :FORM-))))
    `(PROGN (DEFUN ,name ,lambda-list ,@body)
            (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
              (DEFINE-COMPILER-MACRO ,name (&WHOLE ,form-var ,@lambda-list)
                (DECLARE (IGNORE ,@lambda-list))
                ;; Inline only if speed is >= debug
                ;; and space < 3
                (IF (AND (>= excl::.speed. excl::.debug.)
                         (< excl::.space. 3))
                    (PROGN
                      (WHEN *WARN-IF-INLINING*
                        (warn "Inlining call ~s" ,form-var))
                      `(,',`(LAMBDA ,lambda-list
                              (BLOCK ,name (LOCALLY ,@(if (stringp (car body))
                                                              (cdr body)
                                                            body))))
                           ,@(CDR ,form-var)))
                  (PROGN
                    (WHEN *WARN-IF-NOT-INLINING*
                      (warn "Not inlining call ~s" ,form-var))
                    ,form-var))))
            )))

#-allegro
(defmacro defsubst (name lambda-list &body body)
  "Identical to DEFUN except that it arranges for the body to be INLINED
   in the places it is called."
  `(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     (DECLAIM (INLINE ,name))
     (DEFUN ,name ,lambda-list ,@body)))

(defmacro deletef (item sequence-place &key (test '(function eq)) &environment env)
  "Delete ITEM from SEQUENCE-PLACE destructively and using EQ for test,
  updating SEQUENCE-PLACE, which must be a setf'able form.
  Return the updated value of SEQUENCE-PLACE."
  ;;`(setf ,sequence-place (delete ,item ,sequence-place :test ,test))
  ;; Safe version.
  (multiple-value-bind (dummies vals new setter getter)
      (get-setf-expansion sequence-place env)
    (when (cdr new) (error "Can't expand this"))
    `(LET* (,@(mapcar #'list dummies vals)
            (,(car new) ,getter)
            )
       (SETQ ,(car new) (DELETE ,item ,(car new) :test ,test))
       ,setter)))

(defun split-declarations (body)
  "Given a LAMBDA body, return three values, the declarations, the doc string,
   if present, and the body stripped of the declarations and doc string."
  (labels ((scan (forms docstring declarations)
             (cl:cond ((and (consp (car forms))
                         (eq (caar forms) 'declare))
                    (scan (cdr forms) docstring (cons (car forms) declarations)))

                   ((or (null forms)
                        (not (stringp (car forms)))
                        docstring
                        (null (cdr forms)))
                    (values docstring (nreverse declarations) forms))

                   (t (scan (cdr forms) (car forms) declarations)))))

    (scan body nil nil)))

;; Tear apart a closure so that we can beta-reduce it.
(defun destructure-function-lambda (arity fl receiver if-not-function)
  "If fl is of the form (FUNCTION (LAMBDA (bound-variable-list) docstring decls body))
   invoke receiver on the bound-variable-list, docstring, decls, and the body.

   If fl is of the form (FUNCTION name), invoke receiver on a
   fake eta-expanded form.

   If fl is of the form NAME, invoke receiver on a
   fake eta-expanded form.

   Otherwise invoke if-not-function."
  (macrolet ((list-length-equals-one (list)
               `(AND (CONSP ,list)
                     (NULL (CDR ,list))))

             (list-length-greater-than-one (list)
               `(AND (CONSP ,list)
                     (CONSP (CDR ,list))))

             (is-function-form (form)
               `(AND (CONSP ,form)
                     (EQ (CAR ,form) 'FUNCTION)
                     (LIST-LENGTH-EQUALS-ONE (CDR ,form))))

             (function-form-body (function-form)
               `(CADR ,function-form))

             (is-lambda-form (form)
               `(AND (CONSP ,form)
                     (EQ (CAR ,form) 'LAMBDA)
                     (LIST-LENGTH-GREATER-THAN-ONE (CDR ,form))))

             (lambda-form-arguments (lambda-form)
               `(CADR ,lambda-form))

             (lambda-form-body (lambda-form)
               `(CDDR ,lambda-form)))

    (cl:cond ((is-function-form fl)
           (let ((pl (function-form-body fl)))
             ;; Look for `(LAMBDA ...)
             (cl:cond ((is-lambda-form pl)
                    (multiple-value-bind (docstring declarations body)
                        (split-declarations (lambda-form-body pl))
                      (funcall receiver (lambda-form-arguments pl) docstring declarations body)))

                   ;; can't fake eta expand if arity is unknown
                   ((null arity) (funcall if-not-function))

                   ((symbolp pl)                ; is something like (function foo)
                    ;; perform eta expansion
                    (let ((arglist nil))
                      (dotimes (i arity)
                        (push (gensym "ARG-") arglist))
                      (funcall receiver arglist nil nil `((,pl ,@arglist)))))

                   (t (funcall if-not-function)))))

          ;; Look for naked '(lambda ...)
          ;; treat as if it were '(function (lambda ...))
          ((is-lambda-form fl)
           (multiple-value-bind (docstring declarations body)
               (split-declarations (lambda-form-body fl))
             (funcall receiver (lambda-form-arguments fl) docstring declarations body)))

          ;; Can't fake an eta expansion if we don't know the arity.
          ((null arity) (funcall if-not-function))

          ;; Perform an ETA expansion
          ((symbolp fl)
           (let ((arglist nil))
             (dotimes (i arity)
               (push (gensym "ARG-") arglist))
             (funcall receiver arglist nil nil `((FUNCALL ,fl ,@arglist)))))

          (t (funcall if-not-function)))))

#| Example:

(defmacro my-mapc (func list)
  (destructure-function-lambda 1 func
    (lambda (bvl docstr decls body)
      (declare (ignore docstr))
      `(DOLIST (,(car bvl) ,list)
         ,@decls
         ,@body))
    (lambda ()
      (error "~s cannot be destructured." func))))

(macroexpand-1 '(my-mapc #'(lambda (x) (+ x 2)) some-list))
  =>  (DOLIST (X SOME-LIST) (+ X 2))

(macroexpand-1 '(my-mapc (lambda (x) (+ x 2)) some-list))
  =>  (DOLIST (X SOME-LIST) (+ X 2))

(macroexpand-1 '(my-mapc #'print some-list)
  =>  (DOLIST (#:ARG-9909 SOME-LIST) (PRINT #:ARG-9909))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For when we want more fine grained control of which tail calls are
;;; optimized.

;;; Note that because of a Franz bug [spr21443],
;;; COMPILER::TAIL-CALL-NON-SELF-MERGE-SWITCH must be T for
;;; optimization of self-calls by internal functions to be optimized.

#||
(defmacro with-tail-call-optimization ((&key (self-calls t) (other-calls t))
                                       &body body)
  #+allegro
  `(excl::compiler-let ((compiler::tail-call-self-merge-switch ,self-calls)
                        (compiler::tail-call-non-self-merge-switch ,other-calls))
     ,@body)
  #-allegro
  `(progn ,@body))
||#

(defun expand-cond (clause-list)
  (labels ((make-if (test consequence alternative)
             (if (eq test 't)
                 consequence
                 (if (eq test 'nil)
                     alternative
                     (if (and (consp test)
                              (eq (car test) 'NOT)
                              (consp (cdr test))
                              (not (consp (cddr test))))
                         (make-if (cadr test)
                                  alternative
                                  consequence)
                         `(IF ,test ,consequence ,alternative)))))

           (make-locally (decls body)
             (if (null decls)
                 (make-progn body)
                 `(LOCALLY ,@decls ,@body)))

           (make-progn (body)
             (if (null (cdr body))
                 (car body)
                 `(PROGN ,@body))))

    (if (consp clause-list)
        (let ((first-clause (car clause-list))
              (more-clauses (cdr clause-list)))
          (if (consp first-clause)
              (let ((test (car first-clause))
                    (consequence (cdr first-clause)))
                (if (null more-clauses)
                    (if (not (eq test 't))
                        (warn "No default clause for COND.")))
                (if (eq test 't)
                    (if more-clauses
                        (warn "Unreachable clauses in COND.")))
                (if (null consequence)
                    (let ((cv (gensym "COND-VALUE-")))
                      `(LET ((,cv ,test))
                            ,(make-if cv cv (if (null more-clauses)
                                                'NIL
                                                (expand-cond (cdr clause-list))))))
                    (if (and (symbolp (car consequence))
                             (string-equal (symbol-name (car consequence)) "=>"))
                        (if (consp (cdr consequence))
                            (if (consp (cddr consequence))
                                (error "Only one thing may follow => in COND clause")
                                (destructure-function-lambda 1 (cadr consequence)
                                  (lambda (bvl docstring declarations body)
                                    (declare (ignore docstring))
                                    (if (null more-clauses)
                                        `(LET ((,(car bvl) ,test))
                                           ,(make-if (car bvl)
                                                     (make-locally declarations body)
                                                     NIL))
                                        `(FUNCALL (OR (LET ((,(car bvl) ,test))
                                                        ,(make-if (car bvl)
                                                                  `(LAMBDA () ,@declarations ,@body)
                                                                  NIL))
                                                      (LAMBDA () ,(expand-cond (cdr clause-list)))))))
                                  (lambda () (error "Non function follows => in COND clause"
                                                    (car consequence)))))
                            (error "Nothing follows => in COND clause"))
                        (MAKE-IF test
                                 (make-progn consequence)
                                 (if (null more-clauses)
                                     'NIL
                                     (expand-cond (cdr clause-list)))))))
              (error "Illegal COND clause ~s" first-clause)))
        (if (null clause-list)
            (error "Empty COND clause.")
            (error "Improper tail ~s found in COND clauses." clause-list)))))

(defmacro cond (&rest more-clauses)
  (expand-cond more-clauses))

(defconstant *spr21443* t "True if the Franz self tail call optimizations require the
tail-call-non-self-merge-switch to be T.  This is a Franz bug.")

(defmacro tail-labels (&whole form bindings &rest body)
  "Like LABELS, but guarantees that the bindings are mutually tail-recursive."
  #+lispworks  (declare (ignore bindings body))
  #+lispworks `(CL:LABELS ,@(cdr form))

  #-(or allegro lispworks)
  (error "Please ensure that this LABELS form can be called tail-recursively.~%~s" form)
  #+allegro (declare (ignore form))
  #+allegro
  (flet ((method1 (name arglist &rest body)
           (multiple-value-bind (docstring decls body) (split-declarations body)
             `(,name ,arglist
                     ,@(when docstring (list docstring))
                     ,@decls
                     (EXCL::COMPILER-LET        ;; eq to cltl1:compiler-let
                         ((COMPILER:TAIL-CALL-SELF-MERGE-SWITCH T)
                          ,@(when *spr21443*
                              '((COMPILER:TAIL-CALL-NON-SELF-MERGE-SWITCH T))))
                        ,@body))))

         (method2 (name arglist &rest body)
           `(,name ,arglist
                   (DECLARE (OPTIMIZE (SPEED 1)))
                   ,@body)))
    `(LABELS (,@(mapcar
                    (lambda (binding)
                        (apply #'method1 binding))
                  bindings))
       ,@body))
  )

(defmacro with-unique-names ((&rest variables) &body body)
  `(lw:with-unique-names (,@variables) ,@body))

#||
;;; USE WITH-UNIQUE-NAMES
(defmacro with-macro-variables ((&rest variables) &body body)
  "Generate variables for use within a macro definition.
   The variables named in the &REST list will be assigned gensymed names, and should
   be used with comma substitution throughout the macro definition.
   Body may be anything, its result is returned.

   A typical use of this feature is:

   (defmacro my-macro (stuff)
      (with-macro-variables (result-var)
        `(let ((,result-var ,stuff))
             ,result-var)))

   This saves writing lots of macro code of the form:

   (defmacro my-macro (stuff)
     (let ((result-var-1 (gensym \"RESULT-VAR-1\"))
           (result-var-2 (gensym \"RESULT-VAR-2\")))
        `(let ((,result-var-1 ,stuff)
               (,result-var-2 ...)))))

   I.e., it largely eliminates the LET with GENSYM bindings and compresses it to a single
   line of code.
   "
  ;; By using the symbol name as an argument to gensym, we can make more
  ;; useful debugging output.
  `(LET (,@(mapcar (lambda (variable)
                       `(,variable (QUOTE ,(gensym (concatenate 'string (symbol-name variable) "-")))))
                   variables))
     ,@body))
||#

(defmacro define-working-implementation (old-symbol new-symbol)
  "Fix the implementation of some internal function by bashing the function
   cell of OLD-SYMBOL with the value in the function cell of NEW-SYMBOL.

   Used to fix franz brain damage."
  `(EVAL-WHEN (:LOAD-TOPLEVEL :EXECUTE)
     #+allegro (EXCL::WITHOUT-PACKAGE-LOCKS
                (SETF (SYMBOL-FUNCTION ',old-symbol)
                      (SYMBOL-FUNCTION ',new-symbol))
                (EXCL:RECORD-SOURCE-FILE ',old-symbol))
     #-allegro (SETF (SYMBOL-FUNCTION ',old-symbol)
                     (SYMBOL-FUNCTION ',new-symbol))))

(defmacro defvar-unbound (variable-name documentation)
  "Like DEFVAR, but the variable will be unbound rather than getting an initial value.
   This is useful for variables which should have no global value but might have a
   dynamically bound value."
  `(EVAL-WHEN (:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
              (DEFVAR ,variable-name)
              (SETF (DOCUMENTATION ',variable-name 'VARIABLE)
                    ,documentation)))

(defmacro logical-pathname-p (thing)
  #+allegro `(excl::logical-pathname-p ,thing)
  #-allegro `(typep ,thing 'logical-pathname)
  )

(defmacro doatimes ((var count-form &optional result-form) &body body)
  (let ((cf (if (or (numberp count-form)
                    (and (consp count-form)
                         (eq (car count-form) 'THE)))
                count-form
              `(THE (INTEGER ,most-negative-fixnum ,array-dimension-limit) ,count-form)))
        (sf (if (numberp count-form)
                `(THE (INTEGER 0 ,(1+ count-form)) (1+ ,var))
                `(THE (INTEGER 0 ,array-dimension-limit) (1+ ,var))))
        (dcl (if (numberp count-form)
                 `(TYPE (INTEGER 0 ,(1+ count-form)) ,var)
                 `(TYPE (INTEGER 0 ,array-dimension-limit) ,var))))
  `(DO ((,var 0 ,sf))
       ((NOT (< ,var ,cf)) ,result-form)
       (DECLARE ,dcl)
    ,@body)))

(defmacro ignore-errors-unless (condition &body forms)
  "Unless CONDITION evaluates to TRUE, act as IGNORE-ERRORS does while executing FORMS, with the
   the same return values.  If CONDITION evaluates to TRUE, when we don't ignore errors, however
   the return value is as if an IGNORE-ERRORS were around the form.
   Example:  (ignore-errors-unless *debugging* (do stuff) (do more stuff) ...)
             (ignore-errors-unless (eq *debugging* 2) (do stuff) (do more stuff) ...)"
  (let ((block-name (gensym "IGNORE-ERRORS-UNLESS-BLOCK-"))
        (cond-value (gensym "IGNORE-ERRORS-UNLESS-COND-VALUE-")))
    ;; Evaluate the cond value before the body for less confusing semantics.
    ;; (We don't want the block established around the conditional).
    `(LET ((,cond-value ,condition))
       (BLOCK ,block-name
         (HANDLER-BIND (#+allegro (EXCL:INTERRUPT-SIGNAL #'SIGNAL)

                                  (CL:ERROR (FUNCTION
                                             (LAMBDA (CONDITION)
                                               #+ALLEGRO (DECLARE (:FBOUND DEBUG-NOTE-CONDITION))
                                               (UNLESS ,cond-value
                                                 (DEBUG-NOTE-CONDITION "caught by an ignore-errors-unless form" CONDITION)
                                                 (RETURN-FROM ,block-name
                                                              (values NIL CONDITION)))))))
                               (VALUES (LOCALLY ,@forms) NIL))))))

(defparameter *ignore-errors-even-if-debugging* t
  "If T, IGNORE-ERRORS-UNLESS-DEBUGGING will ignore the errors, even if
   *debug-noise-level* is non-nil.")

(defmacro ignore-errors-unless-debugging (&rest body)
  "Just like IGNORE-ERRORS, except that if *debug-noise-level* is non-nil,
   errors are not ignored."
  `(IGNORE-ERRORS-UNLESS (AND *DEBUG-NOISE-LEVEL*
                              (NULL *IGNORE-ERRORS-EVEN-IF-DEBUGGING*))
     ,@body))

;;; This is just too damned useful.
;;; It'd be nice to overload CL:LET, but the series package
;;; is doing that already.
(defmacro named-let (name bindings &body body)
  `(LABELS ((,name ,(map 'list #'car bindings) ,@body))
     (,name ,@(map 'list #'cadr bindings))))

(defmacro named-lambda (name bindings &body body)
  `(LABELS ((,name ,bindings ,@body))
     (FUNCTION ,name)))

(defmacro defunimplemented (name lambda-list)
  "A macro for writing a stub function."
  (labels ((lambda-list-arguments (scan result)
             (cl:cond ((null scan) (nreverse result))
                   ((consp (car scan))
                    (lambda-list-arguments
                     (if (caddr scan)
                         (cons (caddr scan) (cdr scan))
                         (cdr scan))
                     (if (consp (caar scan))
                         (cons (cadaar scan) result)
                         (cons (caar scan) result))))
                   ((member (car scan) lambda-list-keywords)
                    (lambda-list-arguments (cdr scan) result))
                   (t (lambda-list-arguments (cdr scan) (cons (car scan) result))))))
  `(DEFUN ,name ,lambda-list
     (ERROR 'changesafe-unimplemented-function
            :subsystem ,(package-name *package*)
            :name ',name
            :arguments (list ,@(lambda-list-arguments lambda-list '()))))))

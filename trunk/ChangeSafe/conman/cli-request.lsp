;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright (c) 1999-2000 Content Integrity, Inc.
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;;          Content Integrity, Inc. CONFIDENTIAL and PROPRIETARY material.
;;;;
;;;;          Content Integrity, Inc
;;;;          Braintree Executive Office Park
;;;;          P.O. Box 850942
;;;;          Braintree, MA 02185-0942
;;;;
;;;; This software and information comprise valuable intellectual property
;;;; and trade secrets of Content Integrity, Inc., developed at substantial
;;;; expense by Content Integrity, which Content Integrity intends to
;;;; preserve as trade secrets.  This software is furnished pursuant to a
;;;; written license agreement and may be used, copied, transmitted, and
;;;; stored only in accordance with the terms of such license and with the
;;;; inclusion of the above copyright notice.  This software and
;;;; information or any other copies thereof may not be provided or
;;;; otherwise made available to any other person.  NO title to or
;;;; ownership of this software and information is hereby transferred.
;;;; Content Integrity assumes no responsibility for the use or reliability
;;;; of this software.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; File Name:     cli-http-request.sp
;;;; Author:        Dave Tenny
;;;; Creation Date: September 1999
;;;;
;;;; Module Description: Command Line Interface HTTP request support.
;;;;
;;;; Given an HTTP-CONNECTION for a GET or POST, this module contains
;;;; routines that will help you parse and otherwise package
;;;; command line arguments and other information in the request.
;;;;
;;;; This module currently assumes that the requests are packaged
;;;; for HTTP protocols by the ServerRequest.java module.
;;;;
;;;; ****** NOTE ****** NOTE ****** NOTE ****** NOTE ****** NOTE ******
;;;; This module should NOT contain CONMAN specific information, and should
;;;; really reside in a separate CLI tools package, or in the WEB package.
;;;; This should be a fairly generic and reusable CLI tool.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CONMAN")

(proclaim (standard-optimizations))

(defun cli-request/parse-ordered-args (args arg-descriptors)
  "Parse arguments as in cli-request/parse-http-uri-args, except that ARGS is any
   list of strings which specify command line argument values in left-to-right order.
   Pleaes refer to CLI-REQUEST-PARSE-HTTP-URI-ARGS for a description of the ARG-DESCRIPTORS parameter."
  (cli-request/validate-arg-descriptions arg-descriptors)
  ;; Sort the arg descriptors so that all positionally-independent args are first,
  ;; the cli-request-match-arg-to-description implementation relies on this.
  (setq arg-descriptors
    ;; Shouldn't this sorting happen at the time that the command is
    ;; defined, so it need only happen once?
    (sort (copy-seq arg-descriptors)
          (lambda (desc1 desc2)
              (let ((type1 (cli-request/arg-desc-type desc1))
                    (type2 (cli-request/arg-desc-type desc2)))
                (and (eq type1 :positional)
                     (not (eq type2 :positional)))))))
  (let ((args-remaining args)
        (descriptions-remaining arg-descriptors)
        (result-bindings nil)
        (result-missing-arg-descriptions nil)
        (result-excess-args nil))
    ;; Iterate over inputs and accumulate result, pushing in reverse order for efficiency
    ;; Caution, args-remaining and descriptions remaining destructively modified!
    (loop while (or args-remaining descriptions-remaining)
        do                              ;pick off an arg and/or description
          (multiple-value-bind (result-tuple excess-descriptions excess-args
                                next-args-remaining next-descriptions-remaining)
              (cli-request/match-arg-to-description args-remaining descriptions-remaining)
            (when result-tuple
              (push result-tuple result-bindings))
            (when excess-descriptions
              (setq result-missing-arg-descriptions
                (nconc result-missing-arg-descriptions excess-descriptions)))
            (when excess-args
              (setq result-excess-args
                (nconc result-excess-args excess-args)))
            (setq args-remaining next-args-remaining)
            (setq descriptions-remaining next-descriptions-remaining)))
    (values (nreverse result-bindings)
            result-missing-arg-descriptions
            result-excess-args)))

(defconstant +cli-request/arg-description-types+
  (if (boundp '+cli-request-arg-description-types+)
      (symbol-value '+cli-request-arg-description-types+)
      (list :switch :keyword :positional :rest))
  "Valid :TYPE values for argument descriptions in CLI-REQUEST-PARSE-HTTP-URI-ARGS")

(defun cli-request/validate-arg-descriptions (arg-descriptions)
  "Perform a sanity check on arg-descriptions provided to (and documented in)
  cli-request-parse-http-uri-args.  For instance, two :REST args is not supported, and
  optional args must go first, for now.

  Return value: N/A, throws error if there is a problem."
  (loop with optionals-okay = t
        with rests-okay = nil
        with rests-done-p = nil
        for desc in arg-descriptions
        as desc-props = (cdr desc)      ;property list form
        as type = (getf desc-props :type)
        as optional-p = (or (eq type :switch)
                            (eq type :keyword))
        do
        ;; Check for valid type
        (unless (find type +cli-request/arg-description-types+)
          (error "Invalid type (~s) in descriptor ~s, must be one of ~s"
                 type desc
                 +cli-request/arg-description-types+))
        ;; Syntax required on optional args, switch/key id
        (when optional-p
          #|;; This restriction has been relaxed, optional arg descriptions may follow required arg
          ;; descriptions.
          (unless optionals-okay
          (error "An optional argument description (~s) was encountered in an unsupported position."
          desc))
          |#
          (let ((syntax (getf desc-props :syntax)))
            (unless (stringp syntax)
              (error "Missing or invalid :SYNTAX specification for optional argument description ~s"
                     desc))))
        (unless optional-p
          (setq optionals-okay nil))    ;no longer ok to have 'em
        (unless (or (eq type :positional)
                    optionals-okay
                    rests-done-p)
          (setq rests-okay t))
        (when (eq type :rest)
          (unless rests-okay
            (error "Invalid :REST parameter use: ~s" desc))
          (setq rests-done-p t)
          (setq rests-okay nil)))
  (when (> (count-duplicates (remove-if-not (lambda (desc)
                                             (getf (cdr desc) :syntax))
                                         arg-descriptions)
                             :test #'equal)
           0)
    (error "Duplicate :SYNTAX specifications in parameter descriptions: ~s" arg-descriptions)))

(defun cli-request/arg-desc-type (arg-desc)
  "Get the :TYPE specification for an argument descriptor, return NIL if it wasn't specified."
  (getf (cdr arg-desc) :type))

(defun cli-request/arg-desc-has-default? (arg-desc)
  "Return true if there is a :DEFAULT specification for an argument descriptor, nil otherwise."
  (let ((unique-value (cons nil nil)))
    (not (eq (getf (cdr arg-desc) :default unique-value) unique-value))))

(defun cli-request/remove-optional-descriptors (descriptors)
  "Utility routine for CLI-REQUEST-MATCH-ARG-TO-DESCRIPTION.

   Delete any optional descriptors from the list of descriptors
   This includes :SWITCH and :KEYWORD descriptors, as well as :REST.

   This is only useful for failed bindings, we don't delete them otherwise until we've
   processed any defaults.  This routine removes ALL optional descriptors for purposes
   of reporting unmatched required descriptors."
  (remove-if (lambda (desc)
                 (let ((arg-type (cli-request/arg-desc-type desc)))
                   (and (or (eq arg-type :switch)
                            (eq arg-type :keyword)
                            (eq arg-type :rest))
                        (not (getf (cdr desc) :required)))))
             descriptors))

(defun cli-request/match-arg-to-description (args-remaining descriptions-remaining)
  "Attempt to match an argument in args-remaining to an argument description in descriptions-remaining.

   ARGS-REMAINING is a list of input argument strings.
   DESCRIPTIONS-REMAINING is a list of argument descriptions documented in cli-request-parse-http-uri-args.

   Return the following values:
   1) A binding list for an argument and a description if we find one, or NIL if we don't.
      (an element in the first value/list of cli-request-parse-http-uri-args).
   2) A list of descriptions from DESCRIPTIONS-REMAINING if there are no more arguments in ARGS-REMAINING,
      or nil, value #1 or value #3  is returned, assuming one of the inputs is non-nil.
   3) A list of arguments from ARGS-REMAINING if there is no matching description in descriptions-remaining,
      or nil, in which case #1 or #3 is returned, assuming one of the inputs is non-nil.
   4) *some* cdr of args-remaining if we consume one or more, or args-remaining if we don't.
   5) subset (list) of  descriptions-remaining if we consume one, or original
      descriptions-remaining if we don't.

   Note that if the first description in descriptions-remaining is for a :REST parameter,
   we'll return a binding in value #1 which consumes all args in args-remaining.
   "
  (unless (and args-remaining descriptions-remaining)
    ;; We've exhausted one input or another, return the appropriate result.
    ;; If descriptions-remaining has a :REST descriptor, we bind that.
    ;; If descriptions-remaining has an :OPTIONAL descriptor, we bind that.
    ;; Otherwise, all things in the non-empty list are excess baggage.
    (cond (descriptions-remaining
           (let* ((desc (car descriptions-remaining))
                  (desc-type (cli-request/arg-desc-type desc))
                  (desc-required (getf (cdr desc) :required))
                  (default-exists-p (cli-request/arg-desc-has-default? desc))
                  (desc-default (getf (cdr desc) :default)))
             (when (eq desc-type :rest)
               ;; :REST requires different result than optional args
               (return-from cli-request/match-arg-to-description
                 (if default-exists-p
                     (values (list (car desc) desc-default) ; :rest var may be nil
                             nil        ;no excess descriptions
                             nil        ;no excess args
                             nil        ;end of args
                             nil)
                   (values nil          ;no binding
                           nil          ;:REST desc is excess, but we don't return it as such (it's optional)
                           nil          ;no excess args
                           nil          ;end of args
                           nil))        ;end of descs, there are none which follow :REST
                 ))                     ;descriptor is :REST
             (when (not desc-required)
               ;; Optional arg, bind it's default value since there are no args, provided default was
               ;; specified
               (if default-exists-p
                   (return-from cli-request/match-arg-to-description
                     (values (list (car desc) desc-default)
                             nil        ;no excess descriptions
                             nil        ;no excess args
                             nil        ;no more args
                             (cdr descriptions-remaining)))
                 ;; no default, don't return binding for missing arg
                 (return-from cli-request/match-arg-to-description
                   (values nil          ;no binding
                           ;; Could consider this an excess description, but right now
                           ;; we don't return unmatched optional descriptions as excess
                           nil          ;no excess descriptions
                           nil          ;no excess args
                           nil          ;no more args
                           (cdr descriptions-remaining))))))
           ;; More than one non-optional description remains, we have excess descriptions, report them
           ;; minus any optional descriptors
           (return-from cli-request/match-arg-to-description
             (values nil                ;no binding
                     ;; Return excess required descriptors which weren't matched.
                     ;; This short circuit assumes we don't want to bind any defaulted args which may
                     ;; remain...
                     (cli-request/remove-optional-descriptors descriptions-remaining) ;excess descriptions
                     nil                ;no excess args
                     nil                ;end of args
                     nil)))             ;end of descriptions
          (args-remaining
           ;; We have args, but no descriptions
           (return-from cli-request/match-arg-to-description
             (values nil                ;no binding
                     nil                ;no excess descriptions
                     args-remaining     ;excess args
                     nil                ;end of args
                     nil)))             ;end of descriptions
          (t (error "Flow control, should have at least args or descriptions."))))

  ;; If we reach this point, we have both args and descriptions.
  (let ((arg-value (car args-remaining)))
    ;; Examine all optional args for a match against arg.  If we don't find any match,
    ;; then we can discard all contiguous optional argument descriptions and proceed with
    ;; non-optional argument match.
    (loop for arg-desc in descriptions-remaining
        as arg-name = (car arg-desc)
        as arg-desc-props = (cdr arg-desc) ;properties minus argument name
        as arg-type = (getf arg-desc-props :type)
        as arg-syntax = (getf arg-desc-props :syntax)
        when (and arg-syntax (string= (find-parameter-alias arg-value arg-value)
                                      arg-syntax))
        do
          ;; Process switch vs. keyword accordingly.  Usually they're optional, but :required can be used
          (cond ((eq arg-type :switch)
                 ;; Have a match, one for one, return it.
                 (return-from cli-request/match-arg-to-description
                   (values (list arg-name arg-syntax) ;the binding
                           nil          ;no excess descriptions
                           nil          ;no excess args
                           (cdr args-remaining)
                           ;; Strip selected optional arg description from descriptions remaining
                           (remove arg-desc descriptions-remaining))))
                ((eq arg-type :keyword)
                 ;; Need to pick off another argument from args, and return missing arg value if it's missing
                 (cond ((null (cdr args-remaining))
                        ;; Missing value for keyword in args
                        (return-from cli-request/match-arg-to-description
                          (values nil   ;no binding
                                  ;; Hmmm, not clear which desc or descs to return as excess.
                                  ;; Let's return the keyword desc only since it was partially matched
                                  ;; That way the caller will report missing keyword argument
                                  (list arg-desc)
                                  nil   ;no excess args
                                  nil   ;no args remaining
                                  ;; Again, could return all descriptions remaining which aren't optional
                                  ;; but we won't for now.  Nil says we're done processing descs.
                                  nil)))
                       (t
                        ;; Have keyword value
                        (return-from cli-request/match-arg-to-description
                          (values (list arg-name (cadr args-remaining)) ;binding
                                  nil   ;no excess descriptions
                                  nil   ;no excess args
                                  (cddr args-remaining) ;strip two args, not just one
                                  (remove arg-desc descriptions-remaining))))))
                (t
                 (error "Invalid syntax-bearing argument descriptor ~s" arg-desc))))

    ;; If we made it this far, the current argument does not match any switch/key arg description.
    ;; (it may be positional).  Strip those which don't have :DEFAULT attributes,
    ;; but for any which do, we need to return a binding for that optional but defaulted argument
    ;; as the return value of this function.
    ;; Don't strip :REQUIRED switch/key elements.  Don't strip any if there are positional args
    ;; to process, assume the current arg *is* the positional arg.
    (unless (find-if (lambda (desc)
                         (and (eq (getf (cdr desc) :type) :positional)
                              (getf (cdr desc) :required)))
                     descriptions-remaining)
      (setq descriptions-remaining
        (remove-if (lambda (desc)
                       (let ((arg-type (getf (cdr desc) :type)))
                         ;; Weed out only those descriptors which haven't requested default bindings
                         (and (not (cli-request/arg-desc-has-default? desc))
                              (not (getf (cdr desc) :required))
                              (or (eq arg-type :switch)
                                  (eq arg-type :keyword)))))
                   descriptions-remaining)))

    ;; Process remaining defaulted or required but unmatched switches and keywords,
    ;; positional, and :REST args
    (if descriptions-remaining
        (let* ((arg-desc (car descriptions-remaining))
               (arg-name (car arg-desc))
               (arg-desc-props (cdr arg-desc)) ;properties minus argument name
               (arg-type (getf arg-desc-props :type))
               (default-exists? (cli-request/arg-desc-has-default? arg-desc))
               (required? (getf arg-desc-props :required))
               (default-value (and default-exists? (getf arg-desc-props :default))))
          (cond ((or (eq arg-type :switch)
                     (eq arg-type :keyword))
                 ;; It either has a default, or it was marked as :REQUIRED
                 (cond (default-exists?
                           (return-from cli-request/match-arg-to-description
                             (values (list arg-name default-value) ;the binding
                                     nil ;no excess descriptions
                                     nil ;no excess args
                                     args-remaining ;we haven't stripped off any args here
                                     (cdr descriptions-remaining) ; descriptions minus the one we just processed
                                     )))
                       (required?
                        ;; Arg doesn't match description, and description is positionally flexible
                        ;; but required.  That we didn't find it before indicates that one of the following
                        ;; is true:
                        ;; 1) the required but moveable arg was not supplied
                        ;; 2) the required but moveable arg exists further down in the arguments
                        ;;    and the current argument we examined doesn't match any known non-positional
                        ;;    descriptor.
                        ;; If there's a positional argument descriptor remaining, match that.
                        ;; Otherwise return the current argument as unmatched.
                        (let ((positional-desc (find-if (lambda (desc)
                                                            (eq (cli-request/arg-desc-type desc) :positional))
                                                        descriptions-remaining)))
                          (if positional-desc
                              ;; Have positional desc, return a binding for that
                              (return-from cli-request/match-arg-to-description
                                (values (list (car positional-desc) arg-value) ; positional match
                                        nil ;no excess description
                                        nil ;no excess args
                                        (cdr args-remaining)
                                        (remove positional-desc descriptions-remaining)
                                        ))
                            ;; Current argument is unmatched
                            (return-from cli-request/match-arg-to-description
                              (values nil ;no binding here
                                      nil ;excess descriptions
                                      (list arg-value) ; excess args
                                      (cdr args-remaining) ;remaining args
                                      descriptions-remaining ; descriptions remaining
                                      )))))
                       (t (error "FLOW CONTROL: position-independent argument is neither defaulted ~
                                 nor required."))))

                ((eq arg-type :positional)
                 (return-from cli-request/match-arg-to-description
                   (values (list arg-name arg-value) ;the binding
                           nil          ;no excess descriptions
                           nil          ;no excess args
                           (cdr args-remaining) ;args minus the one we just bound
                           (cdr descriptions-remaining) ; descriptions minus the one we just processed
                           )))
                ((eq arg-type :rest)
                 (when (> (length descriptions-remaining) 1)
                   (error "Have some argument descriptor following a :REST argument descriptor"))
                 (return-from cli-request/match-arg-to-description
                   (values (list arg-name args-remaining) ;all remaining args
                           nil          ;no excess descriptions
                           nil          ;no excess args
                           nil          ;all args consumed by this one
                           nil          ;shouldn't be more descriptors
                           )))
                (t nil)))
      ;; We don't have any  more descriptions, we ate them up when we stripped the defaults,
      ;; return appropriate values and the caller will decide if they should call us again
      (return-from cli-request/match-arg-to-description
        (values nil                     ;no bindings this run
                nil                     ;no excess descriptions
                nil                     ;no excess args detected ... yet
                args-remaining          ;we didn't consume any args
                nil                     ;no more descriptions remain
                )))
    ;; If we reach this point, we've screwed up
    (error "FLOW CONTROL in cli-request-match-arg-to-description")))

#||

(in-package :conman)

(eval-when (:load-toplevel :execute)
  (export '(cli-request-parse-ordered-args
            cli-request-parse-http-uri-args
            cli-request-argument-value)))
*
;;; HACK ALERT!
;;; This is perhaps one of the most poorly specified and coded modules
;;; I've personally written in this product.  However it serves the
;;; initial deliverable needs admirably.  One anomaly in the
;;; terms used in this modules is the notion of "optional" parameters
;;; which was coined before :SWITCH and :KEYWORD descriptors became allowable as
;;; required parameters with position indepdence.

(defun cli-request-parse-http-uri-args (http-connection arg-descriptors)
  "Parse the URI <query> args in HTTP-CONNECTION and return bindings for argument values.

   HTTP-CONNECTION contains URI <query> args as packaged by ServerRequest.java.
   This routine is sensitive to the way in which ServerRequest packages arguments, because
   the package annotates the order of arguments, which is required to interpret positional parameters.

   ARG-DESCRIPTORS specifies keys for expected arguments, their nature, and
   any default values.  Best described by example:

   ((force-p :type :switch :syntax \"-f\" :default nil) ;'-f' optional switch, defaults to nil
    (reason :type :keyword :syntax \"-r\" :default nil) ;'-r' optional, paired with value
    (old-name :type :positional :required t)    ;old name is positional and required
    (new-name :type :positional :required t)    ;new name is positional and required
    (other-name :type :positional :required nil
                :default nil)           ;other-name is positional, but optional, and defaults to nil
    (remaining-names :type :rest))      ;all other arguments which aren't covered above

    Non-positional (those which possess :SYNTAX values including :SWITCH and :KEYWORD)
    arguments must be given first.

    This routine does not support dynamic options configurations
    based on the values encountered during argument processing.

    :TYPE is always required.
    :SYNTAX is required for :SWITCH and :KEYWORD args.
    :REQUIRED is assumed T unless otherwise specified, except for :SWITCH, :KEYWORD, and :REST args.

    If :DEFAULT is specified, and the argument is not supplied, we provide a binding with the
    default.  If :DEFAULT is not specified, and the argument is not supplied, no binding is provided
    for the binding.  There is currently no 'arg-supplied-p' result in the binding, if the
    binding exists, the arg was supplied.

    If :TYPE is :REST, this variable is bound only to those arguments which do not match
    positional and optional arguments.  In this regard, it differs from lisp lambda lists.

    In addition to the properties which describe how to parse the argument,
    a property of :SECRET T can be used to supress documentation of the
    argument by the \"help\" command.

    Given an http-connection with an URI like this:

    http://www.bar.com/cli-me?0=-r&1=abcdef&2=x&3=y&4=mdb&5=jo

    The return value would be: (there are three, see below)
    ((force-p nil)
     (reason \"abcdef\")
     (old-name \"x\")
     (new-name \"y\")
     (other-name nil)
     (remaining-names nil))

     Note that unless a default is supplied, optional parameters which are missing from the input
     will not be returned in the result.

     Also note that the only time a result will have a non-string return value is when
     a default is supplied and used.

     *TBD* We may want to add :COERCE <type> to the ARG-DESCRIPTIONS list to convert strings to
     a given type or throw an error if it can't happen, or return a failure sentinal for
     arguments which didn't parse/coerce when a type is specified.  This is probably best left for
     a caller of this routine which further augments its capabilities.

     A second value is also returned, which is NIL if things go well.
     If non-nil, it indicates that required arguments were missing, and returns a list of
     elements from ARG-DESCRIPTIONS which match those required arguments which are missing.
     These elements share structure with the input list elements.

     A third value is returned and indicates any arguments which were present in the input,
     but not accounted for in the ARG-DESCRIPTIONS list.  We return the conses from the input
     association list (http-connection uri args) which were extraneous.  The result may share
     structure with the input data.

     Note that if a :REST argument is specified, this value will always be nil.

     All parameter types except :REST permit the :REQUIRED attributed.
     :REQUIRED is implicitly NIL for :SWITCH, :KEYWORD, and :REST, and is implicitly T for all others.
     :REQUIRED can be specified as T for :SWITCH and :KEYWORD, but not for :REST.
    "
  ;; **NOTE**: other than sortin the uri arg list and ensuring we get a list of values in order,
  ;; the bulk of this routine could be used for any list of args values.

  ;; Derive a sorted list of arguments.  Note that switches appear as list elements in ARGS.
  ;; It assumes that uri arg keys are strings representing integers from zero to N-1 labeling
  ;; relative argument positions.
  (let ((args
         (mapcar
          #'cdr
          (sort (copy-seq (http-connection-decoded-uri-args-alist http-connection)) #'<
                :key
                (lambda (acons)
                    (let ((number (parse-integer (car acons) :junk-allowed t)))
                      (unless number
                        (error "URI args are not compatible with CLI-REQUEST-PARSE-HTTP-URI-ARGS, ~
                               they're not keyed with integers.  URI arg alist: ~S"
                               (http-connection-decoded-uri-args-alist http-connection)))
                      (parse-integer (car acons))))))))
    (cli-request-parse-ordered-args args arg-descriptors)))

(defun cli-request-arg-desc-type (arg-desc)
  "Get the :TYPE specification for an argument descriptor, return NIL if it wasn't specified."
  (getf (cdr arg-desc) :type))

(defun cli-request-arg-desc-has-default? (arg-desc)
  "Return true if there is a :DEFAULT specification for an argument descriptor, nil otherwise."
  (not (eq (getf (cdr arg-desc) :default *unique-value*) *unique-value*)))

(defun cli-request-parse-ordered-args (args arg-descriptors)
  "Parse arguments as in cli-request-parse-http-uri-args, except that ARGS is any
   list of strings which specify command line argument values in left-to-right order.
   Pleaes refer to CLI-REQUEST-PARSE-HTTP-URI-ARGS for a description of the ARG-DESCRIPTORS parameter."
  (cli-request-validate-arg-descriptions arg-descriptors)
  ;; Sort the arg descriptors so that all positionally-independent args are first,
  ;; the cli-request-match-arg-to-description implementation relies on this.
  (setq arg-descriptors
    ;; Shouldn't this sorting happen at the time that the command is
    ;; defined, so it need only happen once.
    (sort (copy-seq arg-descriptors)
          (lambda (desc1 desc2)
              (let ((type1 (cli-request-arg-desc-type desc1))
                    (type2 (cli-request-arg-desc-type desc2)))
                (and (eq type1 :positional)
                     (not (eq type2 :positional)))))))
  (let ((args-remaining args)
        (descriptions-remaining arg-descriptors)
        (result-bindings nil)
        (result-missing-arg-descriptions nil)
        (result-excess-args nil))
    ;; Iterate over inputs and accumulate result, pushing in reverse order for efficiency
    ;; Caution, args-remaining and descriptions remaining destructively modified!
    (loop while (or args-remaining descriptions-remaining)
        do                              ;pick off an arg and/or description
          (multiple-value-bind (result-tuple excess-descriptions excess-args
                                next-args-remaining next-descriptions-remaining)
              (cli-request-match-arg-to-description args-remaining descriptions-remaining)
            (when result-tuple
              (push result-tuple result-bindings))
            (when excess-descriptions
              (setq result-missing-arg-descriptions
                (nconc result-missing-arg-descriptions excess-descriptions)))
            (when excess-args
              (setq result-excess-args
                (nconc result-excess-args excess-args)))
            (setq args-remaining next-args-remaining)
            (setq descriptions-remaining next-descriptions-remaining)))
    (values (nreverse result-bindings)
            result-missing-arg-descriptions
            result-excess-args)))

(defconstant +cli-request-arg-description-types+ '(:switch :keyword :positional :rest)
  "Valid :TYPE values for argument descriptions in CLI-REQUEST-PARSE-HTTP-URI-ARGS")

(defun cli-request-validate-arg-descriptions (arg-descriptions)
  "Perform a sanity check on arg-descriptions provided to (and documented in)
  cli-request-parse-http-uri-args.  For instance, two :REST args is not supported, and
  optional args must go first, for now.

  Return value: N/A, throws error if there is a problem."
  (loop with optionals-okay = t
      with rests-okay = nil
      with rests-done-p = nil
      for desc in arg-descriptions
      as desc-props = (cdr desc)        ;property list form
      as type = (getf desc-props :type)
      as optional-p = (or (eq type :switch)
                          (eq type :keyword))
      do
        ;; Check for valid type
        (unless (find type +cli-request-arg-description-types+)
          (error "Invalid type (~s) in descriptor ~s, must be one of ~s"
                 type desc
                 +cli-request-arg-description-types+))
        ;; Syntax required on optional args, switch/key id
        (when optional-p
          #|;; This restriction has been relaxed, optional arg descriptions may follow required arg
          ;; descriptions.
          (unless optionals-okay
(error "An optional argument description (~s) was encountered in an unsupported position."
desc))
          |#
          (let ((syntax (getf desc-props :syntax)))
            (unless (stringp syntax)
              (error "Missing or invalid :SYNTAX specification for optional argument description ~s"
                     desc))))
        (unless optional-p
          (setq optionals-okay nil))    ;no longer ok to have 'em
        (unless (or (eq type :positional)
                    optionals-okay
                    rests-done-p)
          (setq rests-okay t))
        (when (eq type :rest)
          (unless rests-okay
            (error "Invalid :REST parameter use: ~s" desc))
          (setq rests-done-p t)
          (setq rests-okay nil)))
  (when (> (count-duplicates (collect-if (lambda (desc)
                                             (getf (cdr desc) :syntax))
                                         arg-descriptions)
                             :test #'equal)
           0)
    (error "Duplicate :SYNTAX specifications in parameter descriptions: ~s" arg-descriptions)))

(defun cli-request-remove-optional-descriptors (descriptors)
  "Utility routine for CLI-REQUEST-MATCH-ARG-TO-DESCRIPTION.

   Delete any optional descriptors from the list of descriptors
   This includes :SWITCH and :KEYWORD descriptors, as well as :REST.

   This is only useful for failed bindings, we don't delete them otherwise until we've
   processed any defaults.  This routine removes ALL optional descriptors for purposes
   of reporting unmatched required descriptors."
  (remove-if (lambda (desc)
                 (let ((arg-type (cli-request-arg-desc-type desc)))
                   (and (or (eq arg-type :switch)
                            (eq arg-type :keyword)
                            (eq arg-type :rest))
                        (not (getf (cdr desc) :required)))))
             descriptors))

(defun cli-request-match-arg-to-description (args-remaining descriptions-remaining)
  "Attempt to match an argument in args-remaining to an argument description in descriptions-remaining.

   ARGS-REMAINING is a list of input argument strings.
   DESCRIPTIONS-REMAINING is a list of argument descriptions documented in cli-request-parse-http-uri-args.

   Return the following values:
   1) A binding list for an argument and a description if we find one, or NIL if we don't.
      (an element in the first value/list of cli-request-parse-http-uri-args).
   2) A list of descriptions from DESCRIPTIONS-REMAINING if there are no more arguments in ARGS-REMAINING,
      or nil, value #1 or value #3  is returned, assuming one of the inputs is non-nil.
   3) A list of arguments from ARGS-REMAINING if there is no matching description in descriptions-remaining,
      or nil, in which case #1 or #3 is returned, assuming one of the inputs is non-nil.
   4) *some* cdr of args-remaining if we consume one or more, or args-remaining if we don't.
   5) subset (list) of  descriptions-remaining if we consume one, or original
      descriptions-remaining if we don't.

   Note that if the first description in descriptions-remaining is for a :REST parameter,
   we'll return a binding in value #1 which consumes all args in args-remaining.
   "
  (unless (and args-remaining descriptions-remaining)
    ;; We've exhausted one input or another, return the appropriate result.
    ;; If descriptions-remaining has a :REST descriptor, we bind that.
    ;; If descriptions-remaining has an :OPTIONAL descriptor, we bind that.
    ;; Otherwise, all things in the non-empty list are excess baggage.
    (cond (descriptions-remaining
           (let* ((desc (car descriptions-remaining))
                  (desc-type (cli-request-arg-desc-type desc))
                  (desc-required (getf (cdr desc) :required))
                  (default-exists-p (cli-request-arg-desc-has-default? desc))
                  (desc-default (getf (cdr desc) :default)))
             (when (eq desc-type :rest)
               ;; :REST requires different result than optional args
               (return-from cli-request-match-arg-to-description
                 (if default-exists-p
                     (values (list (car desc) desc-default) ; :rest var may be nil
                             nil        ;no excess descriptions
                             nil        ;no excess args
                             nil        ;end of args
                             nil)
                   (values nil          ;no binding
                           nil          ;:REST desc is excess, but we don't return it as such (it's optional)
                           nil          ;no excess args
                           nil          ;end of args
                           nil))        ;end of descs, there are none which follow :REST
                 ))                     ;descriptor is :REST
             (when (not desc-required)
               ;; Optional arg, bind it's default value since there are no args, provided default was
               ;; specified
               (if default-exists-p
                   (return-from cli-request-match-arg-to-description
                     (values (list (car desc) desc-default)
                             nil        ;no excess descriptions
                             nil        ;no excess args
                             nil        ;no more args
                             (cdr descriptions-remaining)))
                 ;; no default, don't return binding for missing arg
                 (return-from cli-request-match-arg-to-description
                   (values nil          ;no binding
                           ;; Could consider this an excess description, but right now
                           ;; we don't return unmatched optional descriptions as excess
                           nil          ;no excess descriptions
                           nil          ;no excess args
                           nil          ;no more args
                           (cdr descriptions-remaining))))))
           ;; More than one non-optional description remains, we have excess descriptions, report them
           ;; minus any optional descriptors
           (return-from cli-request-match-arg-to-description
             (values nil                ;no binding
                     ;; Return excess required descriptors which weren't matched.
                     ;; This short circuit assumes we don't want to bind any defaulted args which may
                     ;; remain...
                     (cli-request-remove-optional-descriptors descriptions-remaining) ;excess descriptions
                     nil                ;no excess args
                     nil                ;end of args
                     nil)))             ;end of descriptions
          (args-remaining
           ;; We have args, but no descriptions
           (return-from cli-request-match-arg-to-description
             (values nil                ;no binding
                     nil                ;no excess descriptions
                     args-remaining     ;excess args
                     nil                ;end of args
                     nil)))             ;end of descriptions
          (t (error "Flow control, should have at least args or descriptions."))))

  ;; If we reach this point, we have both args and descriptions.
  (let ((arg-value (car args-remaining)))
    ;; Examine all optional args for a match against arg.  If we don't find any match,
    ;; then we can discard all contiguous optional argument descriptions and proceed with
    ;; non-optional argument match.
    (loop for arg-desc in descriptions-remaining
        as arg-name = (car arg-desc)
        as arg-desc-props = (cdr arg-desc) ;properties minus argument name
        as arg-type = (getf arg-desc-props :type)
        as arg-syntax = (getf arg-desc-props :syntax)
        when (and arg-syntax (string= (find-parameter-alias arg-value arg-value)
                                      arg-syntax))
        do
          ;; Process switch vs. keyword accordingly.  Usually they're optional, but :required can be used
          (cond ((eq arg-type :switch)
                 ;; Have a match, one for one, return it.
                 (return-from cli-request-match-arg-to-description
                   (values (list arg-name arg-syntax) ;the binding
                           nil          ;no excess descriptions
                           nil          ;no excess args
                           (cdr args-remaining)
                           ;; Strip selected optional arg description from descriptions remaining
                           (remove arg-desc descriptions-remaining))))
                ((eq arg-type :keyword)
                 ;; Need to pick off another argument from args, and return missing arg value if it's missing
                 (cond ((null (cdr args-remaining))
                        ;; Missing value for keyword in args
                        (return-from cli-request-match-arg-to-description
                          (values nil   ;no binding
                                  ;; Hmmm, not clear which desc or descs to return as excess.
                                  ;; Let's return the keyword desc only since it was partially matched
                                  ;; That way the caller will report missing keyword argument
                                  (list arg-desc)
                                  nil   ;no excess args
                                  nil   ;no args remaining
                                  ;; Again, could return all descriptions remaining which aren't optional
                                  ;; but we won't for now.  Nil says we're done processing descs.
                                  nil)))
                       (t
                        ;; Have keyword value
                        (return-from cli-request-match-arg-to-description
                          (values (list arg-name (cadr args-remaining)) ;binding
                                  nil   ;no excess descriptions
                                  nil   ;no excess args
                                  (cddr args-remaining) ;strip two args, not just one
                                  (remove arg-desc descriptions-remaining))))))
                (t
                 (error "Invalid syntax-bearing argument descriptor ~s" arg-desc))))

    ;; If we made it this far, the current argument does not match any switch/key arg description.
    ;; (it may be positional).  Strip those which don't have :DEFAULT attributes,
    ;; but for any which do, we need to return a binding for that optional but defaulted argument
    ;; as the return value of this function.
    ;; Don't strip :REQUIRED switch/key elements.  Don't strip any if there are positional args
    ;; to process, assume the current arg *is* the positional arg.
    (unless (find-if (lambda (desc)
                         (and (eq (getf (cdr desc) :type) :positional)
                              (getf (cdr desc) :required)))
                     descriptions-remaining)
      (setq descriptions-remaining
        (remove-if (lambda (desc)
                       (let ((arg-type (getf (cdr desc) :type)))
                         ;; Weed out only those descriptors which haven't requested default bindings
                         (and (not (cli-request-arg-desc-has-default? desc))
                              (not (getf (cdr desc) :required))
                              (or (eq arg-type :switch)
                                  (eq arg-type :keyword)))))
                   descriptions-remaining)))

    ;; Process remaining defaulted or required but unmatched switches and keywords,
    ;; positional, and :REST args
    (if descriptions-remaining
        (let* ((arg-desc (car descriptions-remaining))
               (arg-name (car arg-desc))
               (arg-desc-props (cdr arg-desc)) ;properties minus argument name
               (arg-type (getf arg-desc-props :type))
               (default-exists? (cli-request-arg-desc-has-default? arg-desc))
               (required? (getf arg-desc-props :required))
               (default-value (and default-exists? (getf arg-desc-props :default))))
          (cond ((or (eq arg-type :switch)
                     (eq arg-type :keyword))
                 ;; It either has a default, or it was marked as :REQUIRED
                 (cond (default-exists?
                           (return-from cli-request-match-arg-to-description
                             (values (list arg-name default-value) ;the binding
                                     nil ;no excess descriptions
                                     nil ;no excess args
                                     args-remaining ;we haven't stripped off any args here
                                     (cdr descriptions-remaining) ; descriptions minus the one we just processed
                                     )))
                       (required?
                        ;; Arg doesn't match description, and description is positionally flexible
                        ;; but required.  That we didn't find it before indicates that one of the following
                        ;; is true:
                        ;; 1) the required but moveable arg was not supplied
                        ;; 2) the required but moveable arg exists further down in the arguments
                        ;;    and the current argument we examined doesn't match any known non-positional
                        ;;    descriptor.
                        ;; If there's a positional argument descriptor remaining, match that.
                        ;; Otherwise return the current argument as unmatched.
                        (let ((positional-desc (find-if (lambda (desc)
                                                            (eq (cli-request-arg-desc-type desc) :positional))
                                                        descriptions-remaining)))
                          (if positional-desc
                              ;; Have positional desc, return a binding for that
                              (return-from cli-request-match-arg-to-description
                                (values (list (car positional-desc) arg-value) ; positional match
                                        nil ;no excess description
                                        nil ;no excess args
                                        (cdr args-remaining)
                                        (remove positional-desc descriptions-remaining)
                                        ))
                            ;; Current argument is unmatched
                            (return-from cli-request-match-arg-to-description
                              (values nil ;no binding here
                                      nil ;excess descriptions
                                      (list arg-value) ; excess args
                                      (cdr args-remaining) ;remaining args
                                      descriptions-remaining ; descriptions remaining
                                      )))))
                       (t (error "FLOW CONTROL: position-independent argument is neither defaulted ~
                                 nor required."))))

                ((eq arg-type :positional)
                 (return-from cli-request-match-arg-to-description
                   (values (list arg-name arg-value) ;the binding
                           nil          ;no excess descriptions
                           nil          ;no excess args
                           (cdr args-remaining) ;args minus the one we just bound
                           (cdr descriptions-remaining) ; descriptions minus the one we just processed
                           )))
                ((eq arg-type :rest)
                 (when (> (length descriptions-remaining) 1)
                   (error "Have some argument descriptor following a :REST argument descriptor"))
                 (return-from cli-request-match-arg-to-description
                   (values (list arg-name args-remaining) ;all remaining args
                           nil          ;no excess descriptions
                           nil          ;no excess args
                           nil          ;all args consumed by this one
                           nil          ;shouldn't be more descriptors
                           )))))
      ;; We don't have any  more descriptions, we ate them up when we stripped the defaults,
      ;; return appropriate values and the caller will decide if they should call us again
      (return-from cli-request-match-arg-to-description
        (values nil                     ;no bindings this run
                nil                     ;no excess descriptions
                nil                     ;no excess args detected ... yet
                args-remaining          ;we didn't consume any args
                nil                     ;no more descriptions remain
                )))
    ;; If we reach this point, we've screwed up
    (error "FLOW CONTROL in cli-request-match-arg-to-description")))

(defun cli-request-argument-value (key argument-bindings &optional default)
  "Given a list of argument bindings returned by CLI-REQUEST-PARSE-HTTP-URI-ARGS
   or CLI-REQUEST-PARSE-ORDERED-ARGS, and a symbol naming one of the arguments in the list
   which was also specified in the parameter descriptions, return the value associated with the binding.
   Return DEFAULT if there was no match for KEY in the bindings."
  (let ((cons (assoc key argument-bindings)))
    (or (and cons (second cons))
        default)))
||#

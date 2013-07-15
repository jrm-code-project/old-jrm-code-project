;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright (c) 2000 Content Integrity, Inc.
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
;;;; File Name:     protect.lsp
;;;; Author:        Joe Marshall
;;;;
;;;; Module Description:  Protect
;;;;
;;;; Fixes race conditions in standard franz macros.
;;;; Handles control-c deferral in franz interrupt system.
;;;; Adds deadlock detection to MP:PROCESS-LOCK
;;;;
;;;;  THIS CODE IS VERY TRICKY, BE EXTREMELY CAREFUL IF YOU MODIFY IT
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CSF/UTILITY")

(eval-when (:load-toplevel :execute)
  (export '(
	    *defer-interrupts*

	    atomic-setq-if-null
	    atomic-test-and-setq
            atomic-test-and-setf
	    atomic-update
	    atomic-incf
	    atomic-decf
	    atomic-push
	    atomic-pop

	    simulate-user-interrupt
	    with-redirected-user-interrupts
	    call-with-redirected-user-interrupts

	    process-safe-event
	    process-safe-event-condition-handler

	    generate-initial-binding-list
	    call-with-child-process
	    call-with-deferred-interrupts
	    process-kill
	    process-suspend
	    signal-deferrable-event

	    unwind-protect-without-interrupts
	    make-semaphore
	    signal-semaphore
	    clear-semaphore
	    semaphore-case
	    wait-for

	    current-special-bindings
	    )))

(defmacro without-scheduling (&body body)
  #+:allegro `(MP:WITHOUT-SCHEDULING ,@body)
  #+:lispworks `(LW:WITHOUT-PREEMPTION ,@body)
  #-(or :allegro :lispworks)
  (error "No expansions for without-scheduling."))

(defmacro without-interrupts (&body body)
  #+:allegro `(EXCL:WITHOUT-INTERRUPTS ,@body)
  #+:lispworks `(LW:WITHOUT-INTERRUPTS ,@body)
  #-(or :allegro :lispworks)
  (error "No expansion for without-interrupts."))

(defmacro current-process ()
  #+:allegro 'SYS:*CURRENT-PROCESS*
  #+:lispworks 'MP:*CURRENT-PROCESS*)

(defmacro process-add-arrest-reason (process reason)
  #+allegro `(MP:PROCESS-ADD-ARREST-REASON ,process ,reason)
  ;; a guess
  #+lispworks `(WITHOUT-SCHEDULING
		(PUSHNEW ,reason (MP:PROCESS-ARREST-REASONS ,process))))

(defmacro lock-holder (lock)
  #+allegro   `(MP:PROCESS-LOCK-LOCKER ,lock)
  #+lispworks `(MP:LOCK-OWNER ,lock))

;;; Some utility functions that may be of interest.

(defmacro atomic-setq-if-null (symbol value)
  "If value of SYMBOL is null, SETQ SYMBOL to VALUE, and return VALUE.
   Otherwise return NIL.

   This expression is atomic and suitable for synchronizing
   multiple processes."
  (with-unique-names (val)
    `(LET ((,val ,value))
       (WITHOUT-SCHEDULING
	 (UNLESS ,symbol
	   (SETQ ,symbol ,val))))))

(defmacro atomic-test-and-setq (symbol old-value new-value)
  "If value of SYMBOL is EQ to OLD-VALUE, SETQ SYMBOL to NEW-VALUE, and return NEW-VALUE.
   Otherwise return NIL.

   This expression is atomic and suitable for synchronizing
   multiple processes."
  (with-unique-names (old-val new-val)
    `(LET ((,old-val ,old-value)
	   (,new-val ,new-value))
       (WITHOUT-SCHEDULING
	 (WHEN (EQ ,symbol ,old-val)
	   (SETQ ,symbol ,new-val))))))

(defmacro atomic-test-and-setf (&environment env place old-value new-value)
  "If value of PLACE is EQ to OLD-VALUE, SETF PLACE to NEW-VALUE, and return NEW-VALUE.
   Otherwise return NIL.

   This expression is atomic and suitable for synchronizing
   multiple processes."
  (with-unique-names (old-value-name new-value-name)
    (multiple-value-bind (temporaries vals new setter getter)
        (get-setf-expansion place env)
      (when (cdr new) (error "Cannot expand this form."))
      `(LET* (,@(mapcar #'list temporaries vals)
                (,old-value-name ,old-value)
                (,new-value-name ,new-value))
         (WITHOUT-SCHEDULING
          (LET (,(car new) ,getter)
            (WHEN (eq ,(car new) ,old-value-name)
              (SETQ ,(car new) ,new-value-name)
              ,setter)))))))

(defmacro atomic-update (&environment env place function)
  "Atomically SETF PLACE to the value of (function symbol).
   Function must take one argument and return one value.

   Function is *not* called atomically, but may be called multiple
   times.  The update *does* happen atomically.

   The new value is returned."
  (multiple-value-bind (temporaries vals new-val setter getter)
      (get-setf-expansion place env)
    `(LET* (,@(mapcar #'list temporaries vals))


       ,(flet ((general-expansion ()
                 (with-unique-names (old-val)
                   `(DO ((,old-val ,getter ,getter)
                         ,(car new-val))
                        ((PROGN
                           (SETQ ,(car new-val) (FUNCALL ,function ,old-val))
                           (WITHOUT-INTERRUPTS
                            (WHEN (EQL ,old-val ,getter)
                              ,setter)))
                         ,(car new-val))))))

          (destructure-function-lambda 
           1 function
           (lambda (bvl docstring decls body)
             (declare (ignore docstring))
             (if (= (length bvl) 1)
                 `(DO ((,(car bvl) ,getter ,getter)
                       ,(car new-val))
                      ((PROGN
                         (SETQ ,(car new-val) ,@body)
                         (WITHOUT-INTERRUPTS
                          (WHEN (EQL ,(car bvl) ,getter)
                            ,setter)))
                       ,(car new-val))
                    ,@decls)
                 (general-expansion)))
           #'general-expansion)))))

(defmacro atomic-incf (place)
  "Increment a variable atomically."
  `(ATOMIC-UPDATE ,place #'1+))

(defmacro atomic-decf (place)
  "Atomically decrement a variable."
  `(ATOMIC-UPDATE ,place #'1-))

(defmacro atomic-push (object symbol)
  "Atomically push an object onto a list.
   This may CONS multiple times if several processes are attempting
   to update the list simultaneously."
  (with-unique-names (val)
    `(ATOMIC-UPDATE ,symbol (LAMBDA (,val) (CONS ,object ,val)))))

(defmacro atomic-pop (symbol)
  "Atomically remove an object from a list.
   Returns NIL if list is NIL."
  `(WITHOUT-SCHEDULING
     (SETQ ,symbol (CDR ,symbol))))

(defun check-scheduling-allowed (reason &rest reason-args)
  (when #+allegro sys:*disallow-scheduling*
	#+lispworks mp:*inhibit-scheduling-flag*
    (error "Attempt to ~? with scheduling disabled." reason reason-args)))

(defun check-interrupts-allowed (reason &rest reason-args)
  (when #+allegro excl::*without-interrupts*
	#+lispworks system::*in-no-interrupts*
    (error "Attempt to ~? with interrupts disabled." reason reason-args)))

(defun simulate-user-interrupt (&optional process)
  (if (or (null process)
	  (eq process (current-process))
	  (not (mp::process-active-p process)))
      (progn
	#+allegro (mp::interrupt-break 2)
        #+lispworks (break)
	#-(or allegro lispworks) (error "Don't know how to simulate a user interrupt."))
    (mp:process-interrupt process #'simulate-user-interrupt)))

(defmacro with-redirected-user-interrupts ((target-process) &body body)
  `(CALL-WITH-REDIRECTED-USER-INTERRUPTS
    ,target-process
    (LAMBDA () ,@body)))

(defun call-with-redirected-user-interrupts (target-process body)
  #-(and allegro (not dont-fix-franz))
  (declare (ignore target-process))
  #-(and allegro (not dont-fix-franz))
  (funcall body)
  #+(and allegro (not dont-fix-franz))
  (handler-bind ((excl::interrupt-signal
		  (lambda (condition)
		    (when (mp::process-active-p target-process)
		      (debug-message 1 "Redirecting user interrupt to ~s" target-process))
		    (simulate-user-interrupt target-process)
		    (continue condition))))
    (funcall body)))

(defvar *defer-interrupts* nil
  "If not NIL, defer processing of interrupts, including user-interrupt (control-c),
   process-kill, and interrupts caused by process-interrupt.  Interrupts
   will not be delivered to the process immediately, but will be deferred until
   *defer-interrupts* is NIL.

   *defer-interrupts* will be bound to :KILL if the process is deferring
   interrupts because it is doing its final unwind.  Interrupts received
   when *defer-interrupts* is :KILL are discarded.  (This is probably not
   quite the right thing.  We should probably throw an error in the
   interrupting process.)

   This variable should be dynamically bound per process.")

(defparameter  *maximum-deferred-interrupts* 5
  "This parameter controls how many control-c's may be deferred before the process
   punts on deferring and immediately enters a break loop.

   If you set this to zero, control-c interrupts will not be deferred.

   Note:  Interrupts are deferred for a good reason!  If you force a control-c to
   be handled immediately, you will abort the topmost UNWIND-PROTECT cleanup.
   This will void your warranty in Massachusetts and California.")

#+(and :allegro-version>= (:version>= 6 0))
(declaim (:fbound process-safe-event-deferred-action))

(define-condition process-safe-event ()
  ;; This condition is signalled when an interrupt is delivered to a process.
  ;; There should be a CONTINUE restart available for this signal.  The process
  ;; may either decline to handle this, in which case it is safe to force the
  ;; action, or the process may invoke the CONTINUE restart, in which case the
  ;; process is taking responsibility for the action.
  ;; The deferred-action should be a PROMISE.
  ((deferred-action
       :accessor process-safe-event-deferred-action
     :initarg :deferred-action)))

(defun call-with-deferred-interrupts (reason thunk)
  "Invoke THUNK in a context where interrupts are deferred.  Interrupts include
   user interrupts (control-c), process-kill, and interrupts caused by
   process-interrupt.

   Thunk should be a procedure of one argument.  This argument can be used to
   undefer interrupts (restore to original state).  It is less complicated that
   it sounds.  Here is an example.

   (call-with-deferred-interrupts \"example reason\"
     (lambda (call-with-restored-interrupts)
       (foo) ;; interrupts are deferred here.
       (funcall call-with-restored-interrupts
         (lambda ()
            (bar)))  ;; interrupts are back on if they were before.
       (baz))) ;; interrupts are deferred again.

   If enough user interrupts are recieved during a deferral, they will be processed
   right away.  This is to allow you to break out of the code if you *really* want to
   by hitting control-c enough times.

   Note:  Interrupts are deferred for a good reason!  If you force a control-c to
   be handled immediately, you will probably abort the innermost UNWIND-PROTECT cleanup.
   This will void your warranty in Massachusetts and California."

;  #+lispworks
;  (if mp::*processing-interrupts*
;      (funcall thunk #'cl:funcall)
;      (multiple-value-prog1
;       (let ((mp::*processing-interrupts* t))
;         (funcall thunk (lambda (thunk)
;                          (let ((mp::*processing-interrupts* nil))
;                            (mp::process-outstanding-interrupts)
;                            (funcall thunk)))))
;       (mp::process-outstanding-interrupts)))

  #+lispworks
  (if mp::*processing-interrupts*
      (funcall thunk #'cl:funcall)
      (let ((mp::*processing-interrupts* t))
        (debug-message 5 "Deferring interrupts: ~a" reason)
        (multiple-value-prog1
             (funcall thunk #'mp::funcall-within-process-interrupt)
         ;; Now poll for interrupts on this thread.
         (mp::funcall-within-process-interrupt #'false)
         )))


;  (if SYSTEM::*IN-NO-INTERRUPTS*
;      (funcall thunk #'cl:funcall)
;      (progn
;        (debug-message 5 "Deferring interrupts: ~a" reason)
;        (system::fast-multiple-value-prog1
;         (let ((interrupt-enables SYSTEM::*IN-NO-INTERRUPTS*)
;               (SYSTEM::*IN-NO-INTERRUPTS* 1))
;           (cl:unwind-protect
;               (funcall thunk (lambda (thunk)
;                                (cl:unwind-protect
;                                     (let ((system::*in-no-interrupts* interrupt-enables))
;                                       (system::without-interrupt-check-for-interrupts)
;                                       (debug-message 5 "Temporarily re-enabling interrupts: ~a" reason)
;                                       (funcall thunk))
;                                  (debug-message 5 "Returning to deferred context: ~a" reason))))
;             (debug-message 5 "Exiting deferral context: ~a" reason)))
;         (system::without-interrupt-check-for-interrupts))))


  #+allegro
  (if *defer-interrupts*
  ;; If this is a recursive call, we *don't* want the inner handler to grab
  ;; the interrupts.  In this case we just funcall the thunk.
  ;; Also note that no action is needed in order to restore interrupts, so
  ;; CL:FUNCALL is the appropriate re-enable function.

      (funcall thunk #'cl:funcall)

    ;; We are interested in two sorts of events.
    ;; Control-c interrupts, and interrupts that must take place
    ;; outside a deferred context.
    (let ((pending-control-c-count 0)
	  (forced-interrupt        nil)
	  (pending-actions         nil))

      (tail-labels ((process-pending-events ()
		      ;; This is called each time we re-enter a non-deferred context.

		      ;; Cause a control-c break.
		      (unless (zerop pending-control-c-count)
			;; Make sure that accumulated control-c's are handled only once.
			(setq pending-control-c-count 0)
			(debug-message 0 "Now processing deferred control-c interrupt.  Reason was: ~a" reason)
			(simulate-user-interrupt))

		      ;; Force the promised interrupts UNLESS *defer-interrupts* is :KILL.
		      ;; We check for :KILL after each promise in case a promise earlier in
		      ;; the list does a kill.
		      (dolist (promise pending-actions)
			(cond ((eq *defer-interrupts* :kill))
			      ;; Sanity check.  Should never happen.
			      (*defer-interrupts*
			       (error "Attempting to process pending events when interrupts are ~s"
				      *defer-interrupts*))
			      (t (force promise)))))

		    (undefer (thunk)
		      ;; Temporarily re-enable interrupts.  (unless we are killing!)
		      (cond ((eq *defer-interrupts* :KILL) (funcall thunk))
			    (*defer-interrupts*
			     ;; This is correct.  If interrupts were OFF when we called
			     ;; CALL-WITH-DEFERRED-INTERRUPTS, then we would never get
			     ;; to this code at all.
			     (let ((*defer-interrupts* nil))
			       (debug-message 5 "Allowing interrupts within deferral.  Reason was:  ~a" reason)
			       (unless (and (zerop pending-control-c-count)
					    (null pending-actions))
				 (process-pending-events))
			       ;; Multiple-value-prog1 makes sure we pass back all values.
			       (multiple-value-prog1 (funcall thunk)
				 (debug-message 5 "Deferring interrupts again:  ~a" reason))))
			    ;; Sanity check
			    (t (error "Recursively invoking undefer??"))))

		    (safe-event-handler (event-condition)
		      ;; Handler for process-safe-event condition.
		      ;; If interrupts are deferred, we take the process out of the
		      ;; condition and stuff it on our pending-actions.
		      ;; If not, we simply decline it.
		      ;; Signaller MUST have a continue and abort restart!  Continue
		      ;; is invoked if we are deferring, ABORT is invoked if we
		      ;; are killing.
		      (cond ((eq *defer-interrupts* :kill)
			     ;; Tell caller we are unwinding.
			     (abort event-condition))
			    (*defer-interrupts*
			     (let ((action (process-safe-event-deferred-action event-condition)))
			       (push action pending-actions)
			       ;; Tell caller we'll handle it later.
			       (continue event-condition)))
			    ;; We decline it.
			    (t nil)))

		    (control-c-handler (condition)
		      (debug-message 5 "Control C while: ~a" reason)
		      ;; Handler for user interrupts.
		      ;; Test to see if we should defer.
		      ;; Do not defer if:
		      ;;  1.  the *defer-interrupts* flag is NIL.  This is when we undefer
		      ;;      within the deferred context.
		      ;;
		      ;;  2.  control-c has been seen *maximum-deferred-interrupts* times.
		      ;;      So if the process becomes wedged, we can still interrupt it.
		      (cond ((null *defer-interrupts*)
			     (setq pending-control-c-count 0)
			     ;; We return NIL to decline it.
			     nil)

			    ;; Well, we are supposedly deferring, but is the user impatient?
			    ((>= pending-control-c-count *maximum-deferred-interrupts*)
			     ;; Not deferring because we reached the maximum count.
			     (setq pending-control-c-count 0
				   forced-interrupt        t)
			     ;; Warn of dangers.
			     (when (eq *defer-interrupts* :kill)
			       (warn "Process is attempting to unwind for a kill."))
			     (warn "Forcing control-c interrupt during deferral.
                                    ~&Cleanup forms are not protected and may not run!")
			     (invoke-debugger "Forced control-c interrupt.")
			     (continue condition))

			    ;; We are deferring or dying.
			    (t
			     (incf pending-control-c-count)
			     (debug-message 0 "Deferring control-c (~d interrupt~:p seen):  ~a"
					    pending-control-c-count
					    reason)
			     ;; Warn that next control-c will *really* interrupt
			     ;; when we are at the maximum.
			     (when (>= pending-control-c-count *maximum-deferred-interrupts*)
			       (debug-message 0 "Next control-c enters debugger.")
			       (warn "Control-c processing has been deferred:  ~a ~
                                        ~&Next control-c interrupt will force an immediate interrupt.
                                        ~&This may result in a corrupted state." reason))
			     ;; Dismiss the interrupt.
			     (continue condition))))
		    )
	(declare (dynamic-extent #'control-c-handler)
		 (dynamic-extent #'safe-event-handler)
		 (dynamic-extent #'undefer)
		 (dynamic-extent #'process-pending-events))

	;; First, set up the condition handlers.
	;; These are essentially no-ops until we turn on the *defer-interrupts* flag.
	(handler-bind ((#+allegro excl:interrupt-signal
                        #+lispworks conditions:debug-break
                                   #'control-c-handler)
		       (process-safe-event    #'safe-event-handler))
	  (debug-message 5 "with-deferred-interrupts:  ~a" reason)
	  ;; Now bind the flag to T, causing the interrupts to be deferred.
	  (let ((*defer-interrupts* t))
	    ;; Regular unwind-protect works because we are deferring interrupts.
	    ;; Note that we can still switch processes.
	    (cl:unwind-protect
		;; Call the thunk.
		(funcall thunk #'undefer)

	      (debug-message 4 "with-deferred-interrupts finished:  ~a" reason)
	      ;; If a throw has occurred, and forced-interrupt is T,
	      ;; then the user must have been impatient, and not continued.
	      ;; Tell him that he may lose.
	      (when forced-interrupt
		(warn "Forced interrupt during deferral, cleanup may not have run."))

	      ;; This ensures that the deferred interrupts are processed
	      ;; even if the body throws for other reasons.
	      ;; Note that the deferred interrupts are promises, so they will
	      ;; be forced only once.
	      ;; Turn on interrupts.
	      (unless (eq *defer-interrupts* :kill)
		(setq *defer-interrupts* nil))
	      ;; Start forcing the promises.
	      (unless (and (zerop pending-control-c-count)
			   (null pending-actions))
		(process-pending-events)))))))))

#+(or lispworks
      (and allegro (not dont-fix-franz)))
(defun signal-deferrable-event (action on-error)
  "Arrange for action (a promise) to be forced.  If *defer-interrupts* is T,
   action is deferred until later.  If *defer-interrupts* is NIL, action may
   be forced immediately.

   If *DEFER-INTERRUPTS* is :KILL, ON-ERROR is called."
  (check-type action promise)
  (let ((condition (make-condition 'process-safe-event
		     :deferred-action action)))
    (restart-case (progn
		    (signal condition)
		    ;; Now if the condition was not handled, it is probably because
		    ;; there is no handler present (i.e., no call to
		    ;; call-with-deferred-interrupts dynamically available).
		    ;; We can handle it ourselves.
		    (force action))

      ;; If the ABORT handler is called, it is because the process
      ;; is no longer accepting events.
      (abort ()
	(debug-message 4 "Process cannot handle event.")
	(funcall on-error))

      ;; If the signal was processed or deferred, we forget it.
      (continue ()
	(debug-message 5 "Event processed or deferred.")
	nil))))

#+(and allegro (not dont-fix-franz))
(defun process-safe-event-condition-handler (condition)
  (signal condition)
  (force (process-safe-event-deferred-action condition))
  (continue condition))

#+(or (and allegro (not dont-fix-franz))
      lispworks)
(defun process-suicide ()
  "Returns a promise to kill the current process.  This is a something
   you might want to signal deferrably."
  (delay
   (if #+allegro (or excl::*without-interrupts*
                     sys::*disallow-scheduling*)
       #+lispworks (or system::*in-no-interrupts*
                       mp:*inhibit-scheduling-flag*)
       (mp:process-interrupt
	(current-process)
	#'mp:process-kill
	(current-process))
     (progn
       (check-interrupts-allowed "kill process ~s" (current-process))
       (check-scheduling-allowed "kill process ~s" (current-process))
       (debug-message 4 "Process ~s dying." (current-process))
       (mp:process-kill (current-process))))))

(defun process-kill (process &key (wait (not (eq process (current-process)))))
  "This is a better implementation of Franz's mp:process-kill.
   It ensures these important qualities:

   1.  If :WAIT is T, PROCESS-KILL will not return until the
       target process is dead.

   2.  If :WAIT is T, PROCESS-KILL must be called from a context
       where interrupts are permitted (otherwise we couldn't wait, right?)

   3.  Unwind-protects in the target process are correctly run
       because interrupt deferral is obeyed."
  #+(and allegro dont-fix-franz)
  (declare (ignore wait))
  #+(and allegro dont-fix-franz)
  (mp:process-kill process)

  #-(and allegro dont-fix-franz)
  (progn
    (debug-message 4 "Killing ~s" process)
    (when wait
      (check-interrupts-allowed "wait for ~s" 'process-kill)
      (check-scheduling-allowed "wait for ~s" 'process-kill)
      (when (eq process (current-process))
	(error "Cannot await own death.")))
    (if (eq process (current-process))
	(signal-deferrable-event
	 #'process-suicide
	 (lambda ()
	   (error "Signal was aborted.  This means the process was already dying.")))
      (mp:process-interrupt process
			    #'signal-deferrable-event
			    (process-suicide)
			    ;; This is called on abort.  We just punt because we
			    ;; are waiting for it to die anyway.
			    (constantly nil)))
    (when wait
      (mp:process-wait
       (format nil "Awaiting death of process ~s" process)
       (complement #+allegro #'mp::process-active-p #+lispworks #'mp::process-alive-p)
       process))))

#+(and allegro (not dont-fix-franz))
(defun make-process-self-suspension (reason)
  (delay
   (check-interrupts-allowed "suspend process ~s (~s)" (current-process) reason)
   (check-scheduling-allowed "suspend process ~s (~s)" (current-process) reason)
   (debug-message 4 "Process ~s suspending." (current-process))
   (process-add-arrest-reason (current-process) reason)))

(defun process-suspend (process arrest-reason &key (wait (not (eq process (current-process)))))
  "Suspend execution of process by adding an arrest reason.
   This is better than just adding the arrest reason because
   it obeys interrupt deferral."
  #-(and allegro (not dont-fix-franz))
  (declare (ignore wait))
  #-(and allegro (not dont-fix-franz))
  (process-add-arrest-reason process arrest-reason)

  #+(and allegro (not dont-fix-franz))
  (let ((suspend (make-process-self-suspension arrest-reason)))
    (debug-message 4 "Suspending ~s" process)
    (when wait
      (when (eq process (current-process))
	(error "Cannot await own suspension."))
      (check-scheduling-allowed "wait for process-suspend")
      (check-scheduling-allowed "wait for process-suspend"))
    (if (eq process (current-process))
	(signal-deferrable-event
	 suspend
	 (lambda ()
	   (error "Signal was aborted.  This means the process was dying during a suspend.")))
      (mp:process-interrupt process
			    #'signal-deferrable-event
			    suspend
			    ;; If the process is dying, suspending it
			    ;; is pointless anyway.
			    (constantly nil)))
    (when wait
      (mp:process-wait
       (format nil "Awaiting suspension of process ~s" process)
       (complement #'mp:process-runnable-p)
       process))))

(defmacro unwind-protect-without-interrupts (protected-form &body cleanup-forms)
  "Like UNWIND-PROTECT, but the cleanup forms are run without-interrupts.
   This is structured so that there is no race condition in the cleanup
   forms."
  #+lispworks
  (let ((interrupt-enables (gensym "INTERRUPT-ENABLES-")))
    `(SYSTEM::FAST-MULTIPLE-VALUE-PROG1
      (LET ((,interrupt-enables SYSTEM::*IN-NO-INTERRUPTS*)
	   (SYSTEM::*IN-NO-INTERRUPTS* 1))
       (CL:UNWIND-PROTECT
	   (LET ((SYSTEM::*IN-NO-INTERRUPTS* ,interrupt-enables))
	     (SYSTEM::WITHOUT-INTERRUPT-CHECK-FOR-INTERRUPTS)
	     ,protected-form)
	 ,@cleanup-forms))
      (SYSTEM::WITHOUT-INTERRUPT-CHECK-FOR-INTERRUPTS)))

  #+(and allegro (not dont-fix-franz))
  (let ((interrupt-enables (gensym (symbol-name :INTERRUPT-ENABLES-))))
    `(LET ((,interrupt-enables EXCL::*WITHOUT-INTERRUPTS*)
	   (EXCL::*WITHOUT-INTERRUPTS* t))
       (CL:UNWIND-PROTECT
	   (LET ((EXCL::*WITHOUT-INTERRUPTS* ,interrupt-enables))
	     ,protected-form)
	 ;; interrupts are off during cleanup
	 ,@cleanup-forms))))

;; Temporary debugging stub.
(defun semaphore-noise (message &rest stuff)
  (declare #+allegro (:fbound conman::conman-server-log-string)
	   (special conman::*conman-server-log*))
  (debug-message 4 "~a ~?." (current-process) message stuff)
#||
  (when conman::*conman-server-log*
    (conman::conman-server-log-string
     nil
     'server
     (format nil "~a ~?." (current-process) message stuff)))
  ||#)

;;;; Semaphores
(defstruct (semaphore
	    #+(and :allegro-version>= (:version>= 6 0))
	    (:constructor make-semaphore (name &aux (gate (mp:make-gate nil))))
	    #-(and :allegro-version>= (:version>= 6 0))
	    (:constructor make-semaphore (name)))
  (name "Unnamed" :read-only t)
  #+(and :allegro-version>= (:version>= 6 0))
  gate
  #-(and :allegro-version>= (:version>= 6 0))
  (signalled nil)
  )

#+(and :allegro-version>= (:version>= 6 0))
(defsubst semaphore-signalled (semaphore)
  (mp:gate-open-p (semaphore-gate semaphore)))

(defmethod print-object ((object semaphore) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s (~@[un~]signalled)"
	    (semaphore-name object)
	    (not (semaphore-signalled object)))))

(defun signal-semaphore (semaphore)
  (check-type semaphore semaphore)
  (debug-message 4 "Signalling ~s" semaphore)
  (semaphore-noise "signalling ~s" semaphore)
  #+(and :allegro-version>= (:version>= 6 0))
  (mp:open-gate (semaphore-gate semaphore))
  #-(and :allegro-version>= (:version>= 6 0))
  (setf (semaphore-signalled semaphore) t)
  )

(defun clear-semaphore (semaphore)
  (check-type semaphore semaphore)
  (debug-message 4 "Clearing ~s" semaphore)
  (semaphore-noise "clearing ~s" semaphore)
  #+(and :allegro-version>= (:version>= 6 0))
  (mp:close-gate (semaphore-gate semaphore))
  #-(and :allegro-version>= (:version>= 6 0))
  (setf (semaphore-signalled semaphore) nil)
  )

;;;; Wait-for
(defvar *waiting-processes* nil
  "An alist of process and what they are waiting for.
   This is for diagnosing hangs and deadlocks.")

(defgeneric wait-for (object)
  (:documentation
  "Suspend the current process until OBJECT is `signalled'.
   If object is a process, wait for it to be dead.")
  (:method :before (object)
    (check-interrupts-allowed "wait for ~s" object)
    (check-scheduling-allowed "wait for ~s" object)
    (semaphore-noise "waiting for ~s" object)
    (debug-message 5 "Process ~s waiting for ~s" (current-process) object))
  (:method :after (object)
    (semaphore-noise "proceeding (~s)" object)
    (debug-message 5 "Process ~s proceeding." (current-process) object))
  (:method :around (object)
    (without-scheduling
      (without-interrupts
	(cl:unwind-protect
	    (progn
	      (push (cons (current-process) object) *waiting-processes*)
	      (let (#+allegro (excl::*without-interrupts* nil)
		    #+lispworks (system::*in-no-interrupts* nil)
		    #+allegro (sys::*disallow-scheduling* nil)
		    #+lispworks (mp:*inhibit-scheduling-flag* nil))
		#+lispworks (SYSTEM::WITHOUT-INTERRUPT-CHECK-FOR-INTERRUPTS)
		#+lispworks (WHEN SYSTEM::*TIME-EXPIRED-HAPPENED* (MP::PROCESS-TIME-EXPIRED))
		(without-scheduling nil) ;; poll sequence breaks
		(call-next-method)))
	  (setq *waiting-processes*
		(delete (current-process) *waiting-processes*
			:key #'car)))))))

(defmethod wait-for ((object semaphore))
  #+(and :allegro-version>= (:version>= 6 0))
  (mp:process-wait
   (format nil "Awaiting ~s" object)
   #'mp:gate-open-p
   (semaphore-gate object))
  #-(and :allegro-version>= (:version>= 6 0))
  (mp:process-wait
   (format nil "Awaiting ~s" object)
   #'semaphore-signalled
   object))

(defmacro semaphore-case (&rest clauses)
  (let ((action (gensym (symbol-name :SEMAPHORE-CASE-ACTION-)))
	(semaphore-names (mapcar (lambda (clause)
				   (declare (ignore clause))
				   (gensym (symbol-name :SEMAPHORE-)))
				 clauses))
	(semaphore-values (mapcar #'car clauses))
	(semaphore-actions (mapcar #'cdr clauses)))

    `(LET ((,action NIL)
	   ,@(mapcar #'list semaphore-names semaphore-values))
       (SEMAPHORE-NOISE "semaphore case:  ~s" (LIST ,@semaphore-names))
       (DEBUG-MESSAGE 5 "Semaphore case:  ~s" (LIST ,@semaphore-names))
       (MP:PROCESS-WAIT
	(FORMAT NIL "Semaphore case ~s." (LIST ,@semaphore-names))
	(LAMBDA ()
	  (COND ,@(mapcar (lambda (name action-form)
			    `((SEMAPHORE-SIGNALLED ,name)
			      (SEMAPHORE-NOISE "semaphore-case noticed ~s" ,name)
			      (DEBUG-MESSAGE 5 "Semaphore case noticed ~s" ,name)
			      (SETQ ,action (LAMBDA ()
					      (SEMAPHORE-NOISE "semaphore-case proceeding")
					      ,@action-form))
			      T))
			  semaphore-names
			  semaphore-actions)
		(T NIL))))
       (FUNCALL ,action))))

(defmethod wait-for ((object mp:process))
  (mp:process-wait
   (format nil "Awaiting death of process ~s" object)
   (complement #+allegro #'mp::process-active-p #+lispworks #'mp::process-alive-p)
   object))

;;(defmethod wait-for ((object socket::socket))
;;  (socket:accept-connection object))

;;; Child processes

(defun generate-initial-binding-list ()
  "Return an appropriate value for :initial-bindings by finding the special variables
   that are currently bound in this stack.  The ones of interest are the ones
   we bind and the documented common-lisp ones."
  ;; We could use some mechanism for enumerating the packages here,
  ;; but I don't think it is necessary, and it will probably be more
  ;; error prone that a constant list.
  (let ((interesting-packages (mapcar #'find-package (list "CL" "CONMAN" "CORE" "SERVER" "RFM" "VM" "WEB" "UTILITY"))))
    ;; Symbols not interned in an `interesting' package will not be initialized.
    ;; This prevents cruft like TPL EXCL MP etc. bindings from showing
    ;; up.
    (mapcar (lambda (name)
	      (cons name (list 'quote (symbol-value name))))
	    (remove-if (complement
			(lambda (symbol)
			  (member (symbol-package symbol) interesting-packages)))
		       (current-special-bindings)))))

(defun call-with-child-process (child-process-name child-process-function receiver
				&key (which-return-values :parent))
  "Spawn a child process with name CHILD-PROCESS-NAME.

   CHILD-PROCESS-FUNCTION will be invoked in the spawned process.
   The spawned process will receive fresh copies of all currently special bound variables.

   CHILD-PROCESS-FUNCTION is a function of two arguments, the parent process, and a semaphore.

   RECEIVER is a procedure of three arguments, the spawned child process,
   and a semaphore that should be used to trigger the child.  The child
   process will wait for this semaphore.  The return value of RECEIVER is
   returned from this function.

   If RECEIVER returns, the child process will be killed, and the parent process
   will wait for the child to die.  If RECEIVER throws, the child process will be
   killed, but the parent process will *NOT* wait for the child process to die.

   The child process will have an interactive restart that allows the user to
   abort the child process, and signal a CONTROL-C interrupt in the parent process.

   WHICH-RETURN-VALUES specifes :PARENT, :CHILD, or :BOTH.
   If :PARENT, the return value of this function is the return-value of the receiver.
   If :CHILD, the return value of this function is the return-value of the child-process-function.
   If :BOTH, the return value of this function is a list of the return values of the parent and child
   in that order."

  (let ((parent-process (current-process))
	(child-process nil)
	(child-return-values nil)
	(parent-return-values nil)
	;; We have to run the child for a bit to install the cleanup forms.
	;; So we pause the parent until the child is ready.
	(child-ready-semaphore (make-semaphore "Child ready."))
	;; Then the child will await this semaphore.
	(child-start-semaphore (make-semaphore "Child start."))
	(child-exit-semaphore  (make-semaphore "Child exit."))
	(parent-exit-semaphore (make-semaphore "Parent exit.")))

    (tail-labels ((spawn-child-process ()
		    (setq child-process
			  (mp:process-run-function
			   #+allegro (list :name child-process-name
                                           :initial-bindings
                                           `(,@(generate-initial-binding-list)
                                             ,@excl:*cl-default-special-bindings*))
                           #+lispworks child-process-name
                           #+lispworks ()
			   (lambda ()
			     (let ((success    nil)
				   (resignal-p nil))
			       (unwind-protect
				   (progn
				     (with-simple-restart (abort-child "Abort child process and interrupt parent process.")
				       ;; Tell parent we are ready to go.
				       (signal-semaphore child-ready-semaphore)
				       (wait-for child-start-semaphore)
				       (debug-message 4 "Child process ~s starting." child-process-name)
				       ;; When parent says so, we start running.
				       (setq child-return-values
					     (multiple-value-list
					      (funcall child-process-function parent-process parent-exit-semaphore)))
				       ;; And when we are done, set the success flag.
				       (setq success t))
				     ;; We come here on success or user restart only.
				     ;; Aborts pass through.
				     (setq resignal-p (not success)))
				 "Exiting child process."
				 ;; If the user told us, interrupt the parent.
				 (when resignal-p
				   (simulate-user-interrupt parent-process))
				 (signal-semaphore child-exit-semaphore)
				 (debug-message 4 "Child process ~s ending ~@[un~]successfully"
						child-process-name
						(not success)))))))
		    ;; Wait here until the child notifies us.
		    (wait-for child-ready-semaphore)))

      (unwind-protect
	  (progn
	    ;; Start the child.  This returns when the child is ready.
	    (debug-message 4 "Spawning child process ~s." child-process-name)
	    (spawn-child-process)
	    (debug-message 4 "Child process ~s ready." child-process-name)
	    ;; Invoke the receiver on the child and the start semaphore.
	    (setq parent-return-values
		  (multiple-value-list
		   (funcall receiver child-process child-start-semaphore)))
	    ;; tell the child to finish up.
	    (signal-semaphore parent-exit-semaphore)
	    (wait-for child-exit-semaphore)
	    (setq child-process nil)
	    (ecase which-return-values
	      (:parent (apply #'values parent-return-values))
	      (:child  (apply #'values child-return-values))
	      (:both   (values parent-return-values child-return-values))))
	"Cleaning up call-with-child-process"
	;; If we abort, kill the child and don't wait.
	(when (and child-process
                   ;; stupid.  lispworks errors on killing a dead process
                   ;; but process may die between checking!
                   #+:lispworks(mp:process-alive-p child-process) 
                   )
	  (debug-message 4 "Killing child process ~s" child-process-name)
	   #+(and allegro dont-fix-franz)
	   (process-kill child-process)
	   #-(and allegro dont-fix-franz)
	   (process-kill child-process :wait nil))))))

;;; MP::PROCESS-LOCK is broken on Allegro.  Go figure.
;;; It is an error to attempt to sieze a lock with interrupts turned off or with
;;; scheduling disabled. (This would lead to deadlock).
;;;
;;; However, not only doesn't Franz detect this, it contributes to the problem
;;; by having MP:WITH-PROCESS-LOCK call EXCL:WITHOUT-INTERRUPTS before checking
;;; the lock!
;;;
;;; What we need to do is find out if interrupts *were* off before WITH-PROCESS-LOCK
;;; mucked with them.  But although WITH-PROCESS-LOCK saves the old value, *it doesn't
;;; bother to tell us*!  So what do we do?
;;;
;;; We peek in the special binding stack for the most recent binding of
;;; the interrupt enables.  We report the saved value.
;;;
;;; This is evil.
;;;
;;; We only use this code to trap deadlock errors in legacy Franz code.
;;; Our multitasking code uses the new macros.

#+allegro
(defun were-interrupts-off? ()
  (let ((index (excl::index-in-bindstack 'excl::*without-interrupts*)))
    (when index
      (excl::bindstack-value index))))

#+lispworks
(defun were-interrupts-off? () nil)

(defun check-deadlock (lock &optional (lock-value (current-process)) &rest whatever)
  (declare (ignore whatever))
  (when (and (or (were-interrupts-off?) 		 ;; can't wait for it
		 #+allegro sys:*disallow-scheduling*
		 #+lispworks mp:*inhibit-scheduling-flag*)
	     (not (eq (lock-holder lock) lock-value)))  ;; don't have it
    (error "DEADLOCK!  Multitasking is off while trying to sieze lock ~s." lock)))

#+(and allegro (not dont-fix-franz))
(eval-when (:load-toplevel :execute)
  ;; Make sure we don't try to run this interpreted!
  (when (member :compiler *features*)
    (let ((*compile-advice* t))
      (defadvice mp:process-lock :before
	(apply #'check-deadlock excl:arglist)))))

;;; We need to be able to get a list of variables that are specbound
;;; in the current stack, so we can create processes that share the same state.

(defvar *bindstack-bnp-probe-1* nil "A random special variable for probing the bindstack.")
(defvar *bindstack-bnp-probe-2* nil "A random special variable for probing the bindstack.")

#+allegro
(defun determine-bindstack-bnp-scale ()
  "Determine how far apart the bindings are in the bindstack
   by probing on the running system.  Gross, but it avoids having to
   deal with nasty franz compiler guts."
  (let ((*bindstack-bnp-probe-1* t)
	(*bindstack-bnp-probe-2* t))
    (- (excl::rindex-in-bindstack '*bindstack-bnp-probe-2*)
       (excl::rindex-in-bindstack '*bindstack-bnp-probe-1*))))

#+allegro
(defun current-special-bindings ()
  (let ((step (determine-bindstack-bnp-scale)))
    (do ((i 0 (+ i step))
	 (vars nil (adjoin (excl::bindstack-symbol i) vars)))
	((>= i (comp::ll :glob-c-value 'sys::c_bnp)) vars))))

#+lispworks
(defun current-special-bindings () nil)


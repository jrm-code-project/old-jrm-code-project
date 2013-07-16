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
;;;; File Name:     container.lsp
;;;; Author:        Dave Tenny
;;;;
;;;; Module Description:
;;;;
;;;; CONTAINER and CONTAINEE classes are behavioral mixins which allow
;;;; representation of hierarchies with cyclic dependencies.  Containers
;;;; may contain zero or more containees.  Containees may have zero or more
;;;; containers.  Since both objects provide for versioned content, note that
;;;; an object which is contained in a container with one CID-SET may not be
;;;; contained with another CID-SET.
;;;;
;;;; NOTE: within a single consistent CID-SET enabled VIEW, any containee
;;;; in a container should always exhibit complete inverse relationships.
;;;; I.e.: if you see a containee in a container, you MUST be able to see
;;;; the container in the in the containee's parent container list.  This
;;;; is true ONLY when the same CID-SET is used to view both relationships.
;;;;
;;;; These classes are mostly useful for determining who-uses information.
;;;; NOTE: In many cases the CONTAINER parents of CONTAINEEs *CANNOT* be
;;;; used to derive user-visible path information for a view of the object.
;;;; This is because a single view of an object may provide for multiple
;;;; paths to the object if it is shared in multiple containers.
;;;; Therefore, path information must often be derived top-down and fully
;;;; represented if used as a handle.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CSF/VERSION-MANAGEMENT")

(proclaim (standard-optimizations))

(eval-when (:load-toplevel :execute)
  (export '(containee
	    ;;containee-containers
	    ;;set-containee-containers
	    container
	    ;;container-containees
	    set-containee-%containers
	    add-containee
	    set-container-containees
	    containee-initialize
	    containee/container-list
            containee/add-container
            scan-container
	    container/containee-list
	    container-find-containee-if

	    ;; Ouch. Lurking bad practices here.  I had to export CONTAINEES so that
	    ;; rfm::x-vm.lsp->version-list-fse-changes would correctly resolve the slot-based
	    ;; symbol reference.  Chances are that whatever we're doing to reference the slot name
	    ;; is inappropriate, however rewriting use of CORE::VERSIONED-OBJECT-MAP-CHANGED-SLOT
	    ;; at this time is not an option.  We will have to consider it later however...
	    ;; Alternatively, the module affected is RFM::X-VM.LSP, which is extending the
	    ;; modules of the VM package, and which is peeking at class slots for VM classes,
	    ;; and which could reasonably be expected to qualify symbol names for VM-specific package
	    ;; references.
	    %containers
	    %containees
	    )))

;;; TBD: should container inherit containee?
(defclass containee ()
  ;; This slot has a funny name to prevent accidental setting.
  ((%containers :initform nil
                :initarg :containers
                :version-technique :composite-set
                :reader containee/container-list
                :writer containee/%set-containers))
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defclass container ()
  ;; This slot has a funny name to prevent accidental setting,
  ;; Use set-container-containees to safely set this slot.
  ((%containees :initform nil 
                :initarg :containees
                :version-technique :composite-set
                :reader container/containee-list
                :writer container/%set-containees))
  (:metaclass versioned-standard-class)
  (:schema-version 0))

(defun scan-container (container)
  (declare (optimizable-series-function))
  (versioned-object/scan-composite-versioned-slot container '%containees))

(defun (setf container/containee-list) (new-containee-list container)
  ;; Make sure there are no duplicates
  (maplist (lambda (tail)
             (assert (not (member (car tail) (cdr tail)))))
	   new-containee-list)
  (let* ((old-containee-list (container/containee-list container))
         (removed-containees (set-difference old-containee-list new-containee-list))
         (added-containees   (set-difference new-containee-list old-containee-list)))
    (if (and (null removed-containees)
	     (null added-containees));; someone just permuted the list, eh?
	(debug-message 4 "Container containee list (unchanged) is:  ~s" (container/containee-list container))
        (progn
          (dolist (removed removed-containees)
            (containee/%set-containers (remove container (containee/container-list removed)) removed))

          (dolist (added added-containees)
            (containee/%set-containers (cons container (containee/container-list added)) added))

          (container/%set-containees new-containee-list container)
          (debug-message 4 "Container containee list is:  ~s" (container/containee-list container))))))

(defmethod graph-node-children ((g container))
  (container/containee-list g))

#||
(defmethod containee-initialize ((containee containee))
  "All containee objects should initialize relevant CONTAINEE portions with this method."
  ;;  ;; Assumes parent container is the first.
  ;;  (set-containee-containers containee (list parent-container))
  )

(defsubst containee-container-list (containee)
  "A convenience method for manipulating the CONTAINERS slot of CONTAINEE objects."
  (vi-stream-as-list (containee-%containers containee)))

(defsubst container-containee-list (container)
  "A convenience method for manipulating the CONTAINEES slot of CONTAINER objects."
  (vi-stream-as-list (container-%containees container)))

(defsubst container-find-containee-if (predicate container)
  "A method for searching for a containee that satisfys predicate"
  (vi-stream-find-if predicate (container-%containees container)))

(defmethod graph-node-children ((g container))
  (container-containee-list g))

(defun set-container-containees (container new-containee-list)
  (maplist (lambda (tail)
	       (assert (not (member (car tail) (cdr tail)))))
	   new-containee-list)
  (let* ((old-containee-list (container-containee-list container))
	 (removed-containees (set-difference old-containee-list new-containee-list))
	 (added-containees   (set-difference new-containee-list old-containee-list)))

    (if (and (null removed-containees)
	     (null added-containees)) ;; someone just permuted the list, eh?
	(debug-message 4 "Container containee list (unchanged) is:~_~s" (container-containee-list container))
      (progn
	(dolist (removed removed-containees)
	  (set-containee-%containers removed (remove container (containee-container-list removed))))

	(dolist (added added-containees)
	  (set-containee-%containers added (cons container (containee-container-list added))))

	(set-container-%containees container new-containee-list)
	(debug-message 4 "Container containee list is:~_~s" (container-containee-list container))
	))))

(defun add-containee (container containee)
  (assert (not (member containee (container-containee-list container))))
  (set-container-containees container (cons containee (container-containee-list container))))
||#
;;; NOTE: we do not automatically assume that changes to containers/contianees slots
;;; are interesting as per OBJECT-CHANGE-INTERESTING-P.  Subtypes must decide this.

;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright (c) 1999 Content Integrity, Inc.
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
;;;; File Name:     cm-cli-command-syntax-constants.lsp
;;;; Author:        naha
;;;; Creation Date: 2000-01-27
;;;;
;;;; Module Description:
;;;;
;;;; This is the file where we define all of the strings that the user might
;;;; type when composing a CM command line.  These include the names of the
;;;; commands and their switch and keyword arguments.
;;;;
;;;; 2/24/2000 htc bring command names up to Rev 3.3 of Command Descriptions
;;;; 11/15/2000 RWM added additional command symbols
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CONMAN")

(proclaim (standard-optimizations))

;;
;; Parameterized command names, where general product commands are expected to differ from HP
;; commands.  I suppose we should do this for all commands, but it's not work which advances my next paycheck
;; right now.
;;
;; We don't use these yet as extensively in CM-CLI as we should.  In particular, the command
;; documentation provided in this module would have to be substituted with the affected references.
;; So parameterizing the documentation and define-cm-cli-command names remains to be done.
;; *FINISH*(after HP deliverables).
;;

;;; Ideally, we would have a function for each of the ChangeSafe commands
;;; and a separate mechanism for binding the user visible connamd
;;; names to the functions which implement them.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; change_* commands
  (defconstant +cm-cli-command/change+ "cset"
    "General substitution, if we're calling a 'change' a 'cset', or
     a 'change', it is reflected in this variable."))
(defconstant +cm-cli-command/class-create+        "class_create")
(defconstant +cm-cli-command/change-create+       "cset_create" "really change_create")
(defconstant +cm-cli-command/change-add+          "cset_add" "really change_add")
(defconstant +cm-cli-command/change-remove+       "cset_remove" "really change_remove")
(defconstant +cm-cli-command/change-uncreate+     "cset_uncreate" "really change_uncreate")
(defconstant +cm-cli-command/change-close+        "cset_close" "really change_close")
(defconstant +cm-cli-command/changes-from-file+   "csets_from_file")
(defconstant +cm-cli-command/check-out+           "co")
(defconstant +cm-cli-command/description-query+   "description_query")
(defconstant +cm-cli-command/description-replace+ "description_replace")
(defconstant +cm-cli-command/file-add+            "file_add")
(defconstant +cm-cli-command/file-remove+         "file_remove")
(defconstant +cm-cli-command/file-rename+         "file_rename")
(defconstant +cm-cli-command/master-change+       "master_change")
(defconstant +cm-cli-command/master-lock+         "master_lock")
(defconstant +cm-cli-command/master-unlock+       "master_unlock")
(defconstant +cm-cli-command/master-unlock-force+ "master_unlock_force")
(defconstant +cm-cli-command/product-create+      "product_create" "Command to create a product configuration")
(defconstant +cm-cli-command/product-deactivate+  "product_deactivate")
(defconstant +cm-cli-command/product-directory+   "product_directory" "was master_directory")
(defconstant +cm-cli-command/product-reactivate+  "product_reactivate")
(defconstant +cm-cli-command/product-rename+      "product_rename")
(defconstant +cm-cli-command/release-create+      "release_create")
(defconstant +cm-cli-command/release-freeze+      "release_freeze")
(defconstant +cm-cli-command/release-rename+      "release_rename")
(defconstant +cm-cli-command/subsys-create+       "subsys_create")
(defconstant +cm-cli-command/subsys-inherit-from+ "subsys_inherit_from")
(defconstant +cm-cli-command/subsys-rename+       "subsys_rename")
(defconstant +cm-cli-command/subsys-subscriber-list+ "subsys_subscriber_list" "was subsys_user_list")
(defconstant +cm-cli-command/uncheck-out+         "unco")
(defconstant +cm-cli-command/undo+                "undo")
(defconstant +cm-cli-command/undo-file-add+       "undo_file_add")
(defconstant +cm-cli-command/wall-hole+           "wall_hole")
(defconstant +cm-cli-command/wall-lower+          "wall_lower")
(defconstant +cm-cli-command/wall-raise+          "wall_raise")
(defconstant +cm-cli-command/workspace-create+    "ws_create")
(defconstant +cm-cli-command/workspace-delete+    "ws_delete")
(defconstant +cm-cli-command/workspace-move+      "ws_move")
(defconstant +cm-cli-command/workspace-query+     "ws_query")
(defconstant +cm-cli-command/workspace-regenerate+ "ws_regenerate"
  "Command to regenerate a workspace, so that the file on disk match their
   versioned contents in the repository.")
(defconstant +cm-cli-command/workspace-set+       "ws_set")
(defconstant +cm-cli-command/workspace-update+    "ws_update")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Parameter descriptions which are candidates for representation in
;;; a cm-session-context object, as well as other diddling (read
;;; save-and-reuse) by way of the user's environment, perhaps a
;;; .csf file.
;;;

;;; These constants are wrapped in an eval-when so they may be #.'d
;;; into define-cm-cli-command macro invocations.  We no longer need
;;; the EVAL-WHEN since these constants are now in a file which is
;;; loaded before the files in which they are referenced.  With the
;;; EVAL-WHEN gone, M-. should have an easier time finding them.

;;; Key/value pairs
(defconstant +cm-cli-keyword/logto+ "-logto" "file to log to")
(defconstant +cm-cli-keyword/master+ "-database" "Master repository name")
(defconstant +cm-cli-keyword/image-file+ "-image-file"
  "path to file to backup repository into")
(defconstant +cm-cli-keyword/description+ "-description"
  "Description for various described objects")
(defconstant +cm-cli-keyword/reference-dirspec+ "-product-directory"
  "Path string for a 'product' (used to be 'master') reference directory, i.e. a server-relative
   reference directory for a product configuration")
(defconstant +cm-cli-keyword/db-dirspec+ "-db-directory" "Path string for a master repository directory")
(defconstant +cm-cli-keyword/class-dirspec+ "-subdirectory"
  "Relative path string for identifying relative subdirectory occupied by a class/subsystem in a
   product configuration.")
(defconstant +cm-cli-keyword/subsys-dirspec+ "-subdirectory"
  "Relative path string for identifying relative subdirectory occupied by a subsystem in a
   product configuration.")
(defconstant +cm-cli-keyword/pc-name+ "-product" "Product configuration name")
(defconstant +cm-cli-keyword/base-pc-name+ "-base-product"
  "Base product-configuration name, used in addition to, or sometimes in place of,
   a product-configuration name")
(defconstant +cm-cli-keyword/subscriber-pc-name+ "-subscriber-product"
  "A subscriber product-configuration name used primarily for the subsys_create command
   to specify the product which is the initial subscriber to the created subsystem.")
(defconstant +cm-cli-keyword/base-subsystem-name+ "-base-subsystem"
  "The base subsystem from which a subsystem inherits")
(defconstant +cm-cli-keyword/subsystem-name+ "-subsystem" "Subsystem name")
(defconstant +cm-cli-keyword/class-name+ "-class" "Class name")
(defconstant +cm-cli-keyword/label+ "-label" "Label specification")
(defconstant +cm-cli-keyword/time+ "-date-time" "Time specification")
(defconstant +cm-cli-keyword/release-name+ "-release" "Release specification")
(defconstant +cm-cli-keyword/type+ "-type"
  "Type specification for an alias")
(defconstant +cm-cli-keyword/directory-name+ "-directory"
  "Directory specification for a release")
(defconstant +cm-cli-keyword/change-name+ "-name"
  ;; Yes, a formatted comment, with word-cap case conversion.
  #.(format nil "~:(~a~) Name" +cm-cli-command/change+))
(defconstant +cm-cli-keyword/abstract-name+ "-abstract" "Abstract file name")
(defconstant +cm-cli-keyword/ws-id+ "-ws"
  "Workspace identifier, either the unique identifier for a workspace, or a path specification
   which uniquely identifies the rood directory of a workspace.")
(defconstant +cm-cli-keyword/from-release+ "-from-release"
  "used by release_create to specify the release the new release is created from")
(defconstant +cm-cli-keyword/subsys-inherit-from-subsys+ "-from-subsystem"
  "the inherited from subsystem of subsys_inherit_from")
(defconstant +cm-cli-keyword/subsys-inherit-from-product+ "-from-product"
  "a product name which identifies the inherited from subsystem of subsys_inherit_from")
(defconstant +cm-cli-keyword/base-pc-copy-file+ "-copy-file"
  "When creating a product based on another product,
  use those subsystems which are named in the specified file.")

;; Switches
(defconstant +cm-cli-switch/stoplog+ "-stoplog" "Stop logging")
(defconstant +cm-cli-switch/showparam+ "-showparam"
  "Show the parameters of the admin_database_error_log command")
(defconstant +cm-cli-switch/force+ "-force" "Force an activity to occur")
(defconstant +cm-cli-switch/server-relative+ "-server-relative"
  "Use the ChangeSafe application server file system to carry out this operation.
   This option may be disabled by the ChangeSafe system administrator.")
(defconstant +cm-cli-switch/xeno+ "-xeno"
  "Allow workspace creation outside of the user's home directory.")
(defconstant +cm-cli-switch/no-update+ "-noupdate"
  "Don't update the workspace with ws_update.")
(defconstant +cm-cli-switch/update-report+ "-report"
  "Don't update the workspace with ws_update.")
(defconstant +cm-cli-switch/nocopy+ "-nocopy"
  "Don't make a backup copy of the on-disk file when overwriting it.")
(defconstant +cm-cli-switch/all-files+ "-all"
  "operate on all relevant files.")
(defconstant +cm-cli-switch/pc-product-directory+ "-product-directory"
  "flag the workspace as the product reference workspace")

(defconstant +cm-cli-switch/subsys-inherit-all-changes+ "-all"
    "the -all mode for subsys_inherit_from")
(defconstant +cm-cli-switch/subsys-inherit-select-changes+ "-select"
    "the -select mode for subsys_inherit_from")
(defconstant +cm-cli-switch/subsys-dont-inherit+ "-delete"
  "the switch to delete this inheritance relationshit for subsys_inherit_from")

(defconstant +cm-cli-switch/subsys-subscriber-write+ "-write"
  "the switch to subsys_user_list so that a product can modify a subsystem that it uses.")
(defconstant +cm-cli-switch/subsys-subscriber-readonly+ "-nowrite"
  "the switch to subsys_user_list so that a product can use a subsystem but not modify it.")
(defconstant +cm-cli-switch/subsys-subscriber-delete+ "-delete"
  "the switch to subsys_user_list so that a product will stop using a subsystem.")
(defconstant +cm-cli-switch/base-pc-copy-none+ "-copy-none"
  "When creating a product based on another product, don't use any of the source product's subsystems.")
(defconstant +cm-cli-switch/base-pc-copy-all+ "-copy-all"
  "When creating a product based on another product, use all of the source product's subsystems.")
(defconstant +cm-cli-switch/base-pc-copy-file+ "-copy-file"
  "When creating a product based on another product, use just the source product's subsystems listed in specified file.")

(defconstant +cm-cli-switch/v0+ "-v0" "requests verbosity level quiet")
(defconstant +cm-cli-switch/v1+ "-v1" "requests verbosity level brief")
(defconstant +cm-cli-switch/v2+ "-v2" "requests verbosity level verbose")

(defconstant +cm-cli-switch/show-work+ "-show-work" "-show-work argument to ws_query")
(defconstant +cm-cli-switch/list-user-workspaces+ "-list-user-ws" "-list-ws argument to ws_query")
(defconstant +cm-cli-switch/list-all-workspaces+ "-list-all-ws" "-all switch to ws_query")

(defconstant +cm-cli-switch/port-gui+ "-gui" "-gui switch for port command")
(defconstant +cm-cli-switch/port-act+ "-act" "-act swicth for port command")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Each constant below names a special key used in URI <query> arg processing,
;;; or as a key to weed out values in arg-bindings.  These values are stripped from the
;;; http connection args.

(defconstant +cm-cli-uri-key/allow-busy-redirect+ "ALLOW-REDIRECT"
  "Uri arg: whether server must process command even if busy.")
(defconstant +cm-cli-uri-key/user-name+ "USER-NAME"
  "Uri arg: user name as supplied by some context, typically OS.")
(defconstant +cm-cli-uri-key/client-platform+ "CLIENT-PLATFORM"
  "Uri arg: name of the operating system supplied by some context, typically OS.")
(defconstant +cm-cli-uri-key/current-directory+ "CURRENT-DIRECTORY"
  "Uri arg: current directory as supplied by some context, typically OS.")
(defconstant +cm-cli-uri-key/user-home-directory+ "USER-HOME-DIRECTORY"
  "Uri arg: user's home directory as supplied by some context, typically OS.")
(defconstant +cm-cli-uri-key/rcpath+ "RCPATH"
  "Client file path in which CONMANRC contents were found, present only if they were found.")
(defconstant +cm-cli-uri-key/timezone+ "TIMEZONE-OFFSET"
  "Client timezone offset from UTC in minutes (add to UTC for client local time).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Here we establish the registry of all keywords, and of all switches
;;;;
(defun make-parameter-hash-table (list)
  (let* ((n (length list))
         (table (make-hash-table :test #'equal :size (+ n (floor n 3)))))
    (dolist (i list)
      (setf (gethash (symbol-value i) table) i))
    table))

(defparameter *all-keyword-names* (make-parameter-hash-table
                                   (apropos-list "+CM-CLI-KEYWORD/" "CONMAN")))
(defparameter *all-switch-names* (make-parameter-hash-table
                                  (apropos-list "+CM-CLI-SWITCH/" "CONMAN")))

(defun switch-name? (name)
  "Return true if NAME, a string, is the name of a switch, such as -v0.
  "
  (gethash name *all-switch-names*))

(defun keyword-name? (name)
  "Return true if NAME, a string, is the name of a keyword, such as -product.
  "
  (gethash name *all-keyword-names*))

(defun parameter-name? (name)
  (or (keyword-name? name)
      (switch-name? name)))

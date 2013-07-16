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
;;;; File Name:     cm-returns.lsp
;;;; Author:        Dave Tenny
;;;; Creation Date: November 1999
;;;;
;;;; Module Description: Return code values for Conman CLI
;;;;
;;;; These codes are not fully supported yet, but the intent is that
;;;; there will be coherent ranges of success, warning, and error codes,
;;;; and that the codes are potentially shared across commands.
;;;; (Which is why we put them into a single module up front, to share them).
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "CONMAN")

(proclaim (standard-optimizations))

;;;
;;; NOTE: docstrings important, we may use them in diagnostic messages.
;;; Also note common symbolic name prefixes for warnings and errors
;;;

;;; SUCCESS codes.   Probably only the one...
(defconstant *cm-returns-success* 0 "Successful return code.")

;;; WARNING codes.
;;; 1-100 Are warnings.  1 is "unknown", or a default in absence of additional information.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *cm-returns-warning-min-code* 1
    "The minimum value for a warning code."))

(define-enumeration (:start *cm-returns-warning-min-code*
                            :set-symbol *cm-returns-warning-all-codes*
                            :last-value-parameter-name *cm-returns-warning-max-code*)
  (*cm-returns-warning-default*
   "A warning was indicated, but no specific reason was available.")
  (*cm-returns-warning-no-subsystems-to-lock*
   "A master_lock command was attempted, but there were no affected subsystems to lock.")
  (*cm-returns-warning-no-subsystems-locked*
   "A master_unlock command was attempted, but  there no locks held on subsystems in the workspace.")
  (*cm-returns-warning-file-already-deleted*
   "A file argument to file_remove has already been removed from the workspace.")
  (*cm-returns-warning-file-already-renamed*
   "The file may have already been renamed.")
  (*cm-returns-warning-old-file-does-not-exist*
   "The old file argument to file_rename is not in the workspace.")
  (*cm-returns-warning-new-file-already-exists*
   "The new file argument to file_rename is already in the workspace.")
  (*cm-returns-warning-bogus-arguments*
   "The combination of arguments supplied to the command don't make sense
   but enough information was supplied that the command could execute
   This is a warning that the extraneous arguments were ignored.")
  (*cm-returns-warning-file-not-present*
   "The specified file isn't present in the on-disk workspace.")
  (*cm-returns-warning-missing-arguments-no-op*
   "The command did nothing because no arguments were specified, e.g. file_add with no files.")
  (*cm-returns-warning-file-already-added*
   "The file has already be file_add'ed in the current change.")
  (*cm-returns-warning-file-already-checked-out*
   "The specified file has already been checked out in the current change.")
  (*cm-returns-warning-remove-new-file*
   "The file specified for removal was added in the current change.")
  (*cm-returns-warning-remove-newly-renamed-file*
   "The file specified for removal was renamed in the current change.")
  (*cm-returns-warning-renaming-newly-added-file*
   "Attempt to rename a file that was added by the current change.")
  (*cm-returns-warning-directory-filename-ignored*
   "The specified file name is a directory, which will be ignored because
   the operation is not appropriate for directories, e.f. file_add.")
  (*cm-returns-warning-product-already-inactive*
   "A product configuration is inactive, and product_deactivate was called.")
  (*cm-returns-warning-product-already-active*
   "A product configuration is active, and product_reactivate was called.")
  (*cm-returns-warning-product-is-inactive*
   "A general warning/note that a command is being executed in the context of an inactive product.")
  (*cm-returns-warning-unimplemented*
   "This command is unimplemented.")
  (*cm-returns-warning-unable-to-delete-directory*
   "Some directory could not be deleted.")
  (*cm-returns-warning-multiple-warnings-issued*
   "The operation resulted in several warnings of different types.")
  (*cm-returns-warning-no-changes-found*
  "The change transaction detected no real changes to a specific entity which was supposed to change,
   such as a checked-out file.  However if the warning form of this message is thrown, cset creation
   may (or may not) proceed.")
  (*cm-returns-warning-already-frozen*
   "The branch/release is already frozen.")
  (*cm-returns-warning-wall-status-unchanged*
   "The status of a wall with respect to a given operation remains unchanged because
    it already had the desired state.")
  (*cm-returns-warning-notification-failed*
   "Email notification failed, no further information available.")
  (*cm-returns-warning-no-smtp-server-configured*
   "I don't know what smtp server to use for sending mail.")
  (*cm-returns-warning-no-file-was-unco-ed*
   "No files for your class, subsystem, or cset were UNCO'd (because there were none that applied)")
  (*cm-returns-warning-workspace-not-up-to-date*
   "The workspace is not up to date.")
  (*cm-returns-warning-duplicate-change-name*
   "A cset was referenced by change name (after having the augmented system-generated name applied)
   but that name is already in use.")
  (*cm-returns-warning-no-state-change*
   "This is the most generic form of warning for when the user requests some state change,
    but no change is made.  Chance are that if you're deep in the bowels of semantically challenging code,
    you may want a more specific form of this code for support/debugging purposes.")
  (*cm-returns-warning-log-file-closed*
   "This message is signalled when an existing log file is closed in response to specification of a new
    log file for performance and error logging.")
  (*cm-returns-warning-error-closing-log-stream*
   "This message is a WARNING raised to note an ERROR which occurred while closing a log stream.
    This means we've suppressed (one way or the other) the error condition so that we may handle
    and present it in some tamer context.")
  (*cm-returns-warning-file-not-already-checked-out*
   "The specified file is not already been checked out in the current change and therefore, may not be unchecked out.")
  (*cm-returns-warning-file-way-too-big*
   "The specified file is too large to be represented internally and cannot be loaded.")
  (*cm-returns-warning-file-too-big*
   "The specified file is larger than *binary-file-size-limit*.")
  )


;;; ERROR codes.  10000 is "unknown", or a default in absence of additional information.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant *cm-returns-error-min-code* 10000
    "The minimum value for an error code."))

(define-enumeration (:start *cm-returns-error-min-code*
                            :set-symbol *cm-returns-error-all-codes*
                            :last-value-parameter-name *cm-returns-error-max-code*)
  (*cm-returns-error-default*
   "An error was indicated, but no specific reason was available.")
  (*cm-returns-error-unrecognized-command*
   "The command was not recognized.")
  (*cm-returns-error-too-few-arguments*
   "The command did not receive the minimum number of arguments.")
  (*cm-returns-error-too-many-arguments*
   "The command received more than the maximum number of arguments.")
  (*cm-returns-error-invalid-database-directory*
   "The directory specified is not valid or doesn't exist.")
  (*cm-returns-error-product-configuration-name-exists*
   "The product configuration name is already in use.")
  (*cm-returns-error-subsystem-already-exists*
   "The subsystem name is already in use by an existing subsystem.")
  (*cm-returns-error-invalid-base-product*
   "The base product configuration does not exist.")
  (*cm-returns-error-reference-directory-exists*
   "The reference directory already exists.")
  (*cm-returns-error-subsystem-lock-failed*
   "An attempt to lock a subsystem failed because the subsystem was already locked by another workspace.")
  (*cm-returns-error-missing-change-description*
  "An attempt was made to create or promote change in the master repository, and no change description
   is available.")
  (*cm-returns-error-non-existent-disk-file*
   "An attempt was made to read a disk file which was specified but did not exist.")
  (*cm-returns-error-invalid-disk-file-spec*
  "A disk (not repository) file specification was given which didn't identify a proper file
   (perhaps it was a directory).")
  (*cm-returns-error-empty-abstract-file*
   "An textual file was specified, but didn't have any content.")
  (*cm-returns-error-binary-abstract-file*
   "An textual file was specified, but it appears to have binary content.")
  (*cm-returns-error-change-already-present*
  "A cset was specified for addition to a workspace via cset_add or PORT commands,
  but the cset is already present.")
  (*cm-returns-error-change-already-absent*
  "A change-set was specified for removal from a workspace via cset_remove or PORT commands,
  but the change-set is already absent.")
  (*cm-returns-error-pc-subsystem-not-found*
  "An attempt was made to locate a subsystem which doesn't exist within a specific product configuration.
   This is distinct from another error below which is global in the search space, not local to a PC.")
  (*cm-returns-error-change-set-name-does-not-exist*
   "An attempt was made to locate a master repository change-set by name, and no such change-set exists.")
  (*cm-returns-error-missing-change-name*
  "An attempt was made to create or promote change in the master repository, and no change name
   is available.")
  (*cm-returns-error-ambiguous-change-name*
  "A cset was referenced by simple change name (not the augmented system-generated name).
   There were multiple csets which match the indicated name.")
  (*cm-returns-error-bogus-arguments*
   "The combination of arguments supplied to the command don't make sense and execution can't continue.")
  (*cm-returns-error-no-changes-found*
  "The change transaction detected no real changes to the repository, typically no file content changes.
   therefore no change-set was created.")
  (*cm-returns-error-file-not-readonly*
   "The permissions of the specified file are not read-only.")
  (*cm-returns-error-file-not-in-any-subsystem-directory*
   "A subsystem could not be determined for a specified pathname.")
  (*cm-returns-error-pc-branch-not-found*
   "A product configuration branch was specified but not found.")
  (*cm-returns-error-cant-undo*
   "The specified command cannot be undone.")
  (*cm-returns-error-undo-not-done*
   "The command cannot be undone because it was never issued in the first place.")
  (*cm-returns-error-change-not-permitted*
  "An active change context exists and a command was issued which does not permit
   an active change context if it is to proceed.  This error does NOT cover
   the validity of change-add/remove activities in the workspace as it applies to the command,
   just the presence of a change context.  An example is the presence of a change context in the ws_set
   command, a situation which is prohibited.")
  (*cm-returns-error-invalid-repository-file-spec*
  "A repository file specification was given, but it cannot be resolved to an actual entity in the
   repository, possibly because the repository version context is such that the file exists
   but is not actually reachable in the versioned context.")
  (*cm-returns-error-no-args-to-unco*
   "Neither files, a class, a subsystem or all was specified to unco.")
  (*cm-returns-error-too-many-args-to-unco*
   "More than one option of files, a class, a subsystem or all was specified to unco.")
  (*cm-returns-error-change-already-active*
   "A change is active in the workspace, so the command cannot be run.")
  (*cm-returns-error-workspace-invalid-for-master-change*
  "An attempt is being made to perform the master_change command on a workspace which doesn't
   correspond to a branch tip.  In this case, change_close is the only viable option.")
  (*cm-returns-error-workspace-not-under-homedir*
   "The user tried to create a workspace that wasn't under his home directory")
  (*cm-returns-error-open-change-required*
   "There is no active change context and the command requires that a change be in progress.")
  (*cm-returns-error-new-file-already-present*
   "There is already a file with the specified name in the user's workspace directory.")
  (*cm-returns-error-new-file-already-exists*
   "There is already a file with the specified name in the repository")
  (*cm-returns-error-file-cannot-be-checked-out*
   "The specified file can not be checked out.")
  (*cm-returns-error-file-not-present*
   "The specified file isn't present in the on-disk workspace.")
  (*cm-returns-error-file-is-checked-out*
   "The requested operation can not be performed on a file that's checked out.")
  (*cm-returns-error-file-renamed-old-name*
  "When referring to a file that was renamed by the current change, you
   can not refer to the file by its old name.")
  (*cm-returns-error-file-name-already-added*
   "A file with the same name has already been added by the current change.")
  (*cm-returns-error-file-was-removed*
   "Can't operate on a file that's been removed in the current change.")
  (*cm-returns-error-master-repository-already-exists*
   "There is already a master repository with the specified name.")
  (*cm-returns-error-create-existing-workspace*
  "An attempt was made to create a workspace associated with a directory
   that is already part of a workspace.")
  (*cm-returns-error-no-such-workspace*
   "There is no workspace corresponding to the specified directory.")
  (*cm-returns-error-unimplemented*
   "This command and/or option is not yet implemented.")
  (*cm-returns-error-product-not-found*
   "A specified product name was not found in attempting to resolve the name.")
  (*cm-returns-error-subsystem-not-found*
   "A specified subsystem name was not found in attempting to resolve the name (outside of a PC scope).")
  (*cm-returns-error-rename-is-nop*
   "The old and new names for an object are the same. This code is intended to be used by multiple commands.")
  (*cm-returns-error-product-rename-another-product-has-name*
   "Another product exists that has the new name for a product rename.")
  (*cm-returns-error-subsystem-rename-another-subsystem-has-name*
   "Another subsystem exists that has the new name for a subsystem rename.")
  (*cm-returns-error-workspace-not-up-to-date*
  "The operation can only be performed on an up-to-date workspace but
   the workspace was not up-to-date.")
  (*cm-returns-error-base-product-requires-subsystem-copy-specifier*
   "User specified -base to product_create without accompanying -COPY-* argument.")
  (*cm-returns-error-copy-parameter-requires-base-parameter-for-product-create*
   "User specified one of the COPY-* parameters without -BASE for product_create")
  (*cm-returns-error-mutually-exclusive-arguments*
   ;; *CM-RETURNS-ERROR-BOGUS-ARGUMENTS* is used in a number of places for this.
   "A generic error for any situation where mutually exclusive arguments are caught and signalled.")
  (*cm-returns-error-port-activity-in-progress*
   "The specified operation could not be performed because the workspace has a port
    operation in progress.")
  (*cm-returns-error-missing-change-abstract*
  "An attempt was made to close or promote a change in the master repository, and no change abstract
   is available.")
  (*cm-returns-error-invalid-change-name*
   "The cset name specified was invalid.")
  (*cm-returns-error-empty-product-copy-file*
   "An empty file was specified as the '-copy-file' argument to product_create.")
  (*cm-returns-error-binary-product-copy-file*
   "A binary file was specified as the '-copy-file' argument to product_create.")
  (*cm-returns-error-subsystem-classes-differ*
   "The specified subsystems are not of the same class")
  (*cm-returns-error-invalid-release-name*
   "The release name specified was invalid.")
  (*cm-returns-error-release-already-exists*
   "The release name specified already exists.")
  (*cm-returns-error-invalid-description-replace-item-type*
   "Description Replace item-type was invalid.")
  (*cm-returns-error-project-not-found*
   "The Class or Project was not found in the repository.")
  (*cm-returns-error-product-name-needed*
   "A product name must be given to fully specify a release name")
  (*cm-returns-error-duplicate-change-name*
   "A cset was referenced by change name (after having the augmented system-generated name applied)
   but that name is already in use.")
  (*cm-returns-error-release-rename-another-release-has-name*
   "Another product exists that has the new name for a product rename.")
  (*cm-returns-error-release-rename-is-frozen*
   "You may not rename a frozen release.")
  (*cm-returns-error-wall-hole-required*
   "Walls are up on subsystems.
    One or more holes are required in one more more walls in one or more subsystems.
    A cset_close may also be required with holes for the resulting cset.")
  (*cm-returns-error-time-spec-not-valid*
   "The specified time specification was not appropriate for the command.")
  (*cm-returns-error-subsystem-subdirectory-overlap*
   "The operation would cause a product to have subsystems with overlapping subdirectories.")
  (*cm-returns-error-release-is-frozen*
   "Operation not permitted because the release is frozen.")
  (*cm-returns-error-ws-move-current-dir-in-old-ws*
   "Your current directory may not be within the existing workspace directory.")
  (*cm-returns-error-cant-merge-binary-files*
   "No automatic merges on binary files during ws_update.  You must do an UNCO first.")
  (*cm-returns-error-reference-workspace-change-not-permitted*
   "No changes may be made to a product reference workspace.")
  (*cm-returns-error-no-such-alias*
   "No such alias is active.")
  (*cm-returns-error-no-write-permission*
   "No write permission to subsystem.")
  (*cm-returns-error-copy-all-required*
   "The -copy-all switch is required when a time is specified.")
  (*cm-returns-error-workspace-id-mismatch*
   "Workspace identifiers must match (database & disk).")
  (*cm-returns-error-workspace-directory-mismatch*
   "You cannot manipulate a workspace while within another workspace.")
  (*cm-returns-error-database-name-mismatch*
   "Database names must match (command argument & ws .csf file).")
  (*cm-returns-error-workspace-id-must-be-integer*
   "A workspace identifier must be an integer.")
  (*cm-returns-error-has-write-permission*
   "Another product has write permission to the subsystem.")
  (*cm-returns-error-subsystem-is-frozen*
   "That subsystem is frozen, so write permission cannot be granted.")
  (*cm-returns-error-subsystem-class-already-in-use*
   "The proposed subsystem class already has a subsystem.")
  (*cm-returns-error-merge-in-master-change-ws-update*
   "Master_change failed because a merge occurred during auto ws_update")
  (*cm-returns-error-invalid-name*
   "The name given is invalid.")
  (*cm-returns-error-file-already-checked-out*
   "The specified file has already been checked out in the current change.")
  (*cm-returns-error-file-is-readwrite-on-check-out*
   "You may not check out a file which is already writeable.")
  (*cm-returns-error-osbackup-nonzero-exitcode*
   "osbackup returned a nonzero exit code.")
  (*cm-returns-error-workspace-in-transition*
   "Your workspace is out of sync with the repository.  Please issue a ws_sync command.")
  (*cm-returns-error-workspace-not-in-transition*
   "Your workspace is already in sync with the repository.")
  (*cm-returns-error-host-not-specified*
   "You must specify a host.")
  (*cm-returns-error-host-not-available*
   "The specified host is not available.")
  (*cm-returns-error-image-file-missing*
   "The image-file was not specified.")
  (*cm-returns-error-improper-email-list-for-check-ins*
   "The email-list-for-check-ins is improperly structured, must be a list of lists of strings")
  )                                     ;define-enumeration of error codes


;;;
;;; Miscellaneous return code predicates
;;;

(defun cm-returns-warning-p (return-code)
  "Return true if the RETURN-CODE, an integer,
   is in a range which would indicate that it represents a warning, and NIL otherwise."
  (and (integerp return-code)
       (<= *cm-returns-warning-min-code* return-code
           *cm-returns-warning-max-code*)))

(defun cm-returns-error-p (return-code)
  "Return true if the RETURN-CODE, an integer, is in a range which would indicated that it represents
   failure, and NIL otherwise."
  (and (integerp return-code)
       (<= *cm-returns-error-min-code* return-code
           *cm-returns-error-max-code*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hacks for looking up status codes

(defun cm-returns-lookup-code (code-number &key just-the-name)
  "CODE-NUMBER is an integer ChangeSafe return code.  This function looks up
   the return code and prints out the corresponding symbol name and
   documentation.  Returns a string of the symbol name(s)"
  (let ((result nil))
    (flet ((try-it (code-variable)
             (when (= code-number (symbol-value code-variable))
               (let ((answer (if just-the-name
                                 (format nil "~a" code-variable)
                               (format nil "~3d ~a ~a"
                                       code-number code-variable
                                       (documentation code-variable 'variable)))))
                 (setq result (if result (concatenate 'string result " " answer)
                                answer)))
               )))
      (try-it '*cm-returns-success*)
      (mapc #'try-it *cm-returns-error-all-codes*)
      (mapc #'try-it *cm-returns-warning-all-codes*)
      result)))

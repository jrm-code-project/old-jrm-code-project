(in-package "CSF/CONFIG")

(eval-when (:load-toplevel :execute)
  (export '(*major-schema-version*   *minor-schema-version*
	    *major-software-version* *minor-software-version*
	    +company-name+
	    )))

(defconstant +company-name+ "ChangeSafe, LLC"
  "Company named printed in various products based on this code.")

(defconstant +product-name+ "ChangeSafe")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Variables which impact all modules, and which are updated for all software changes.
;;; These variables should actually be emitted by a product footprint mechanism at some point,
;;; rather than maintain them manually.
;;;

(defparameter *major-schema-version* 9
  "Major version of persistent data structures represented by code.  Major versions 
   require specific upgrades to the database.  Changes to this variable should be 
   documented in the VERSION.TXT file.")

(defparameter *minor-schema-version* 19
  "Minor version of persistent data structures represented by code.  Minor versions are
   typically backward compatible with previous minor versions in the same major version, 
   though once an updated is applied, the database minor version is advanced making it
   incompatible with earlier versions of the code schema.   Changes to this variable 
   should be documented in the VERSION.TXT file.")

(defparameter *major-software-version* 0
  "Major version of the software, typically embodying significant changes to overall software
   function, possibly requiring client software upgrades, server upgrades, etc.
   All changes to this variable should be documented in the VERSION.TXT file.")

(defparameter *minor-software-version* 558
  "Minor version of the software, typically embodying minor changes in software which do not
   have any user visible impact from the standpoint of installation or server-side running 
   of the software.
   Changes to user interface may be present in minor version upgrades.
   All changes to this variable should be documented in the VERSION.TXT file.
   This variable is more apropos of a build number.")

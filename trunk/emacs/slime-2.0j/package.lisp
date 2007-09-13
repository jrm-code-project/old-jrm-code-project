;;; This file contains the package definitions for SLIME/SWANK
;;; It is better to keep package definition files separate so you
;;; can configure the package topology without loading and executing code.

;;; Author: jrm

(in-package "CL-USER")

(defpackage "MONITOR" (:nicknames "MON") (:use "COMMON-LISP")
  (:export
   "*MONITORED-FUNCTIONS*"
   "DISPLAY-MONITORING-RESULTS"
   "MONITOR"
   "MONITOR-ALL"
   "MONITOR-FORM"
   "MONITORED"
   "MONITORING-ENCAPSULATE"
   "MONITORING-UNENCAPSULATE"
   "REPORT"
   "REPORT-MONITORING"
   "RESET-ALL-MONITORING"
   "RESET-MONITORING-INFO"
   "UNMONITOR"
   "WITH-MONITORING"
   ))

(defpackage "PXREF" (:use "COMMON-LISP"))

(defpackage "SLIME-NREGEX" (:use "COMMON-LISP"))

(defpackage "SWANK" (:use "COMMON-LISP"
                          "CLOS"
                          #+clisp "GRAY"
                          #+lispworks "STREAM"
                          )
  #+clisp (:import-from "CLOS"
                        "SLOT-DEFINITION-DOCUMENTATION"))

(defpackage "SWANK-IO-PACKAGE"
    (:use)
  (:import-from "COMMON-LISP"
                "NIL"
                "T"
                "QUOTE"))

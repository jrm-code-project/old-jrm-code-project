;;; -*- Mode:LISP; Package:USER; Base:8; Readtable:ZL -*-

(DEFSYSTEM FILE-SYSTEM-UTILITIES
  (:COMPONENT-SYSTEMS LOCAL-FILE FILE-SERVER))

(DEFSYSTEM LOCAL-FILE
  (:NAME "Local-File")
  (:SHORT-NAME "LFS")
  (:PATHNAME-DEFAULT "SYS: FILE;")
  (:PATCHABLE NIL "FS")
  (:warnings-pathname-default "SYS:FILE;LOCAL-FILE-CWARNS.LISP")
  (:NOT-IN-DISK-LABEL)
  (:PACKAGE FILE-SYSTEM)
  (:MODULE DEFS "FSDEFS")
  (:MODULE MAIN ("FSSTR" "FSGUTS" "FSACC" "FSACL"))
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD MAIN ((:FASLOAD DEFS))))


(DEFSYSTEM FILE-SERVER
  (:NAME "File-Server")
  (:NICKNAMES "Server")
  (:PATHNAME-DEFAULT "SYS: FILE;")
  (:PATCHABLE NIL "Server")
  (:warnings-pathname-default "SYS:FILE;FILE-SERVER-CWARNS.LISP")
  (:NOT-IN-DISK-LABEL)
  (:PACKAGE FILE-SYSTEM)
  (:COMPILE-LOAD ("SERVER")))

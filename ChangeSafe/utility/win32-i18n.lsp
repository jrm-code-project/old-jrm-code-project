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

;;; API to the Windows Locale database
(in-package "CSF/UTILITY")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '()))

(proclaim (standard-optimizations))

#-(or :microsoft-32 :win32)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This file may only be loaded or compiled under windows."))

;;; Use the foreign function interface to get at the
;;; Win32 natives that can read the locale information.

#+:lispworks
(fli:define-foreign-function (get-system-default-lang-id "GetSystemDefaultLangID")
  ()
  :calling-convention :stdcall
  :result-type :unsigned-short)

(defun primary-language-id (lang-id)
  (ldb (byte 10 0) lang-id))

(defun sublanguage-id (lang-id)
  (ldb (byte 6 10) lang-id))
                             
#+:lispworks
(fli:define-foreign-function (get-system-default-lcid "GetSystemDefaultLCID")
  ()
  :calling-convention :stdcall
  :result-type :unsigned-int)

(defun LCID/language-id (LCID)
  (ldb (byte 16 0) LCID))

(defun LCID/sort (LCID)
  (ldb (byte 4 16) LCID))

(defconstant LOCALE_SDAYNAME1              #x0000002A   "long name for Monday")
(defconstant LOCALE_SDAYNAME2              #x0000002B   "long name for Tuesday")
(defconstant LOCALE_SDAYNAME3              #x0000002C   "long name for Wednesday")
(defconstant LOCALE_SDAYNAME4              #x0000002D   "long name for Thursday")
(defconstant LOCALE_SDAYNAME5              #x0000002E   "long name for Friday")
(defconstant LOCALE_SDAYNAME6              #x0000002F   "long name for Saturday")
(defconstant LOCALE_SDAYNAME7              #x00000030   "long name for Sunday")
(defconstant LOCALE_SABBREVDAYNAME1        #x00000031   "abbreviated name for Monday")
(defconstant LOCALE_SABBREVDAYNAME2        #x00000032   "abbreviated name for Tuesday")
(defconstant LOCALE_SABBREVDAYNAME3        #x00000033   "abbreviated name for Wednesday")
(defconstant LOCALE_SABBREVDAYNAME4        #x00000034   "abbreviated name for Thursday")
(defconstant LOCALE_SABBREVDAYNAME5        #x00000035   "abbreviated name for Friday")
(defconstant LOCALE_SABBREVDAYNAME6        #x00000036   "abbreviated name for Saturday")
(defconstant LOCALE_SABBREVDAYNAME7        #x00000037   "abbreviated name for Sunday")
(defconstant LOCALE_SMONTHNAME1            #x00000038   "long name for January")
(defconstant LOCALE_SMONTHNAME2            #x00000039   "long name for February")
(defconstant LOCALE_SMONTHNAME3            #x0000003A   "long name for March")
(defconstant LOCALE_SMONTHNAME4            #x0000003B   "long name for April")
(defconstant LOCALE_SMONTHNAME5            #x0000003C   "long name for May")
(defconstant LOCALE_SMONTHNAME6            #x0000003D   "long name for June")
(defconstant LOCALE_SMONTHNAME7            #x0000003E   "long name for July")
(defconstant LOCALE_SMONTHNAME8            #x0000003F   "long name for August")
(defconstant LOCALE_SMONTHNAME9            #x00000040   "long name for September")
(defconstant LOCALE_SMONTHNAME10           #x00000041   "long name for October")
(defconstant LOCALE_SMONTHNAME11           #x00000042   "long name for November")
(defconstant LOCALE_SMONTHNAME12           #x00000043   "long name for December")
(defconstant LOCALE_SMONTHNAME13           #x0000100E   "long name for 13th month (if exists)")
(defconstant LOCALE_SABBREVMONTHNAME1      #x00000044   "abbreviated name for January")
(defconstant LOCALE_SABBREVMONTHNAME2      #x00000045   "abbreviated name for February")
(defconstant LOCALE_SABBREVMONTHNAME3      #x00000046   "abbreviated name for March")
(defconstant LOCALE_SABBREVMONTHNAME4      #x00000047   "abbreviated name for April")
(defconstant LOCALE_SABBREVMONTHNAME5      #x00000048   "abbreviated name for May")
(defconstant LOCALE_SABBREVMONTHNAME6      #x00000049   "abbreviated name for June")
(defconstant LOCALE_SABBREVMONTHNAME7      #x0000004A   "abbreviated name for July")
(defconstant LOCALE_SABBREVMONTHNAME8      #x0000004B   "abbreviated name for August")
(defconstant LOCALE_SABBREVMONTHNAME9      #x0000004C   "abbreviated name for September")
(defconstant LOCALE_SABBREVMONTHNAME10     #x0000004D   "abbreviated name for October")
(defconstant LOCALE_SABBREVMONTHNAME11     #x0000004E   "abbreviated name for November")
(defconstant LOCALE_SABBREVMONTHNAME12     #x0000004F   "abbreviated name for December")
(defconstant LOCALE_SABBREVMONTHNAME13     #x0000100F   "abbreviated name for 13th month (if exists)")

(defconstant LOCALE_SISO639LANGNAME        #x00000059   "ISO abbreviated language name")
(defconstant LOCALE_SISO3166CTRYNAME       #x0000005A   "ISO abbreviated country name")

#+:lispworks
(fli:define-foreign-function (%get-locale-info "GetLocaleInfo" :dbcs)
 ((LCID :unsigned-int)
  (LCTYPE win32:DWORD)
  (LPTSTR (:pointer :void))
  (bufsize :int))
 :calling-convention :stdcall
 :result-type :int)

(defun get-locale-info-needed-buffer (LCID LCTYPE)
  (%get-locale-info LCID LCTYPE NIL 0))

(defun get-locale-info (LCID LCTYPE)
  (let ((bufsiz (get-locale-info-needed-buffer LCID LCTYPE)))
    (fli:with-dynamic-foreign-objects ()
      (let ((fstring (fli:allocate-foreign-object :nelems bufsiz :type '(:pointer :char))))
        (%get-locale-info LCID LCTYPE fstring bufsiz)
        (fli:convert-from-foreign-string fstring :length bufsiz :external-format :unicode)))))

(defun win32-server-locale ()
  (let ((system-locale-id (get-system-default-lcid)))
    (concatenate 'string 
                 (get-locale-info system-locale-id LOCALE_SISO639LANGNAME)
                 "_"
                 (get-locale-info system-locale-id LOCALE_SISO3166CTRYNAME))))

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

(in-package "CSF/UTILITY")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Products

(make-instance 'java-product
  :product-name '|FSTestApplet|
  :default-directory #p"CSF:JAVA;FSA;"
  :jar-file "FSTestApplet.jar"
  :cab-file "FSTestApplet.cab"
  :ini-file "FSTestApplet.ini"
  :java-components '("FSAProgressIndicator"
                   "FileSystemAgent"
                   "FileSystemAgentException"
                   "FileSystemAgentProtocol"
                   "FSAAppletThread"
                   "FSAProgressIndicatorPanel"
                   "FSAProgressIndicatorApplet"
                   "FSTestApplet"
                   "HttpFileSystemAgent"
                   "HttpImpl"
                   "OSServices"
                   "OSServicesInterface"
                   "ProgressIndication"
                   "Queue"
                   "SunServices"
                   "crc")
  :classpath-additions (list (translate-logical-pathname #p"CSF:JAVA;"))
  :require-ms-extensions '("MSServices")
  :other-components nil)

;;; A pseudo product for server testing.
(make-instance 'java-product
  :product-name '|FSTest|
  :default-directory #p"CSF:JAVA;fsa;"
  :java-components '("FSAProgressIndicator"
                     "FileSystemAgent"
                     "FileSystemAgentException"
                     "FileSystemAgentProtocol"
                     ;; "FSAAppletThread"
                     "FSAProgressIndicatorPanel"
                     ;; "FSAProgressIndicatorApplet"
                     ;; "FSTestApplet"
                     "FSTest"
                     "HttpFileSystemAgent"
                     "HttpImpl"
                     "OSServices"
                     "OSServicesInterface"
                     "ProgressIndication"
                     "Queue"
                     "SunServices"
                     "crc")
  :classpath-additions (list (translate-logical-pathname #p"CSF:JAVA;"))
  :require-ms-extensions '("MSServices")
  :other-components nil)

(make-instance 'java-product
  :product-name 'dirchooser
  :default-directory #p"CSF:JAVA;dirchooser;"
  :jar-file "DirChooser.jar"
  :cab-file "DirChooser.cab"
  :ini-file "DirChooser.ini"
  :java-components '("ArraySorter"
                     "Border"
                     "Colors"
                     "Comparable"
                     "Comparator"
                     "DirChooserApplet"
                     "DirChooserThread"
                     "GrayFilter"
                     "ImageButton"
                     "ImageLabel"
                     "ImageLoader"
                     "LabelledItem"
                     "ListPane"
                     "MultiColumnList")
  :classpath-additions (list (translate-logical-pathname #p"CSF:JAVA;"))
  :require-ms-extensions nil
  :other-components '("folderUp.gif"
                      "folderOpen.gif"
                      "folderClosed.gif"
                      ))

(make-instance 'java-product
  :product-name '|ServerRequest|
  :default-directory #p"CSF:JAVA;cm;"
  :java-components '("ServerRequest")
  :classpath-additions (list (translate-logical-pathname #p"CSF:JAVA;fsa;"))
  )

(make-instance 'java-product
  :product-name '|CONMAN-command|
  :default-directory #p"CSF:JAVA;cm;"
  :java-components '("CM"
                     "RedirectBusy"
                     "RedirectContinue"
                     "RedirectNoService"
                     "RedirectReadOnly"
                     "Set"
                     "UserPrefs")
  :classpath-additions (list (translate-logical-pathname #p"CSF:JAVA;fsa;"))
  ;; We don't technically need these, but if you don't specify them,
  ;; the product doesn't get built (dependency issues).  Should fix in
  ;; makejava.lsp
  :jar-file "cm.jar"
  :cab-file "cm.cab"

;;; This works, but is not useful because we cannot currently pass in the
;;; CHANGESAFE_SERVER_URI  *sigh*
;  :ms-executable-file (cons "cm.exe"
;                           (mapcar (lambda (filename)
;                                       (merge-pathnames
;                                        (make-pathname :name filename :type "class")
;                                        (translate-logical-pathname #p"CSF:JAVA;fsa;")))
;                                   (java-product-ms-extension-java-files (find-java-product '|FSTest|))))
  :other-components (mapcar (lambda (filename)
                                (merge-pathnames
                                 (make-pathname :name filename :type "class")
                                 (translate-logical-pathname #p"CSF:JAVA;fsa;")))
                            (java-product-java-components (find-java-product '|FSTest|)))
  )

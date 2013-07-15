;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; coding: iso-8859-1 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2003,2004,2005 ChangeSafe, LLC
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


(in-package "CL-USER")

;;; Well, we wanted to use the lispworks defsystem, but it got confused.
;;; So we're going to try the MK defsystem and hope for better luck.

(mk:defsystem :csf-foundation
  :language :lisp
  :source-pathname #.(translate-logical-pathname "CSF:UTILITY;")
  :source-extension "lsp"
  :components
  (
   (:file "conditions"     :depends-on ("declarations"))
   (:file "declarations")
   (:file "fixnum"         :depends-on ("declarations" "types"))
   (:file "if-star")
   (:file "length"         :depends-on ("declarations" "types" "conditions"))
   (:file "mpcompat"       :depends-on ("declarations" "conditions" "protect-base"
                                        "utility-base" "utility-macros"))
   (:file "protect-base"   :depends-on ("declarations"))
   (:file "replacement-functions" :depends-on ("declarations"))
   (:file "series-extra"   :depends-on ("declarations" "utility-macros"))
   (:file "syscompat"      :depends-on ("declarations" "utility-macros"))
   (:file "types"          :depends-on ("declarations"))
   (:file "utility-base"   :depends-on ("declarations" "types" "conditions"))
   (:file "utility-macros" :depends-on ("declarations" "types" "conditions"))
   (:file "vector"         :depends-on ("declarations" "types" "conditions"))
   ))

(mk:defsystem :csf-utility
  :language :lisp
  :source-pathname #.(translate-logical-pathname "CSF:UTILITY;")
  :source-extension "lsp"
  :depends-on (:csf-foundation)
  :components
  (
   (:file "ascii")
   (:file "base64")
   (:file "buffer")
   (:file "cmdline"          :depends-on ("pathname-utilities" "platform" "utils"))
   (:file "crc")
   (:file "date-time")
   (:file "db-name"          :depends-on ("pathname-utilities" "platform" "utils"))
   (:file "fake-excl"        :depends-on ("pregexp" "ustring"))
   (:file "graph"            :depends-on ("oset" "pathname-utilities" "platform"))
   (:file "guid"             :depends-on ("hash" "timestamp" "utils"))
   (:file "hash")
   (:file "hatcheck"         :depends-on ("guid"))
   (:file "iso3166"          :depends-on ("utils"))
   (:file "iso639"           :depends-on ("utils"))
   (:file "media-type")
   (:file "ndiff")
   (:file "osapi"            :depends-on ("pathname-utilities" "platform" "utils"
                                          "win32-i18n"
                                          "win32-filesystem"))
   (:file "oset"             :depends-on ("rbtree"))
   (:file "pathname-utilities" :depends-on ("utils"))
   (:file "platform"         :depends-on ("pathname-utilities"))
   (:file "pregexp")
   (:file "promise")
   (:file "protect"          :depends-on ("promise"))
   (:file "rbtree")
   (:file "socketcompat"     :depends-on ("ascii"))
   (:file "strings"          :depends-on ("iso639" "iso3166" "osapi"))
   (:file "test-suites"      :depends-on ("date-time" "osapi"))
   (:file "tests"            :depends-on ("ascii" "buffer" "ndiff" "osapi" "oset"
                                          "pathname-utilities"
                                          "platform" "rbtree"
                                          "test-suites" "uri" "utils"))
   (:file "timestamp"        :depends-on ("date-time" "osapi" "win32-filesystem"))
   (:file "uri"              :depends-on ("ascii" "base64" "ustring" "utils"))
   (:file "uricompat"        :depends-on ("uri"))
   (:file "ustring"          :depends-on ("ascii"))
   (:file "utils")
   (:file "win32-filesystem" :depends-on ("pathname-utilities"))
   (:file "win32-i18n")
   ))

(mk:defsystem :allegroserve
  :depends-on (:csf-foundation :csf-utility)
  :language :lisp
  :source-pathname #.(translate-logical-pathname "CSF:;")
  :source-extension "lsp"
  :components
  ((:module :html-generator
            :source-pathname #+win32 "allegroserve\\htmlgen" #-win32 "allegroserve/htmlgen"
            :components ((:file "htmlgen")))

   (:module :meta-parser
            :source-pathname "utility"
            :components ((:file "meta")))

   (:module :allegroserve
            :depends-on (:html-generator :meta-parser)
            :source-pathname "allegroserve"
            :components
            ((:file "macs")
             ;; Bozos created a lot of forward references.
             (:file "authorize" :depends-on ("macs" "main" "publish"))
             (:file "client"  :depends-on ("macs" "main" "headers" "parse"))
             (:file "decode"  :depends-on ("macs"))
             (:file "headers" :depends-on ("macs" "main"))
             (:file "log"     :depends-on ("macs" "main"))
             (:file "main"    :depends-on ("macs" "decode"))
             (:file "parse"   :depends-on ("macs" "main" "headers"))
             (:file "proxy"   :depends-on ("client" "log" "macs" "main" "parse" "publish"))
             (:file "publish" :depends-on ("macs" "main" "parse" "headers"))
             ))))

(mk:defsystem :java-tools
  :depends-on (:csf-foundation :csf-utility)
  :language :lisp
  :source-pathname  #.(translate-logical-pathname "CSF:JAVA;")
  :source-extension "lsp"
  :components
  ((:subsystem :java-tools
               :source-pathname "auxiliary"
               :components
               (
                (:file "java-tools")
                (:file "makejava")
                ))))

(mk:defsystem :changesafe
  :depends-on (:allegroserve :java-tools :csf-foundation :csf-utility)
  :language :lisp
  :source-pathname #.(translate-logical-pathname "CSF:;")
  :source-extension "lsp"
  :components
  ((:subsystem :pstore
            :source-pathname "pstore"
            :components
            (
             (:file "did")
             (:file "pstore" :depends-on ("objmap" "serial" "symtab"))
             (:file "objmap" :depends-on ("pnode" "serial"))
             (:file "pclass" :depends-on ("objmap" "pnode" "ptxn"))
             (:file "phash"  :depends-on ("objmap" "pclass" "ptxn"))
             (:file "pnode")
             (:file "ptxn"   :depends-on ("objmap" "pstore" "serial"))
             (:file "serial" :depends-on ("symtab"))
             (:file "symtab" :depends-on ("did"))
             (:file "tests"  :depends-on ("pclass" "phash" "pstore" "ptxn" "symtab"))
             ))

   (:subsystem :java-components
               :source-pathname "build"
               :components
               (
                (:file "java-components")
                ))

   (:subsystem :server
               :depends-on (:java-components)
            :source-pathname "server"
            :components ((:file "file-system")
                         (:file "tests" :depends-on ("file-system"))))

#||
   (:subsystem :vfile
               :source-pathname "vfile"
               :components (
                            (:file "constants")
                            (:file "logging" :depends-on ("constants" "sourcesink" "types"))
                            (:file "pathnames"  :depends-on ("constants" "types" "txn"))
                            (:file "sourcesink" :depends-on ("constants" "types"))
                            (:file "txn"        :depends-on ("constants" "types"))
                            (:file "trie"       :depends-on ("constants"))
                            (:file "two-byter"  :depends-on ("constants"))
                            (:file "types"      :depends-on ("constants" "two-byter"))
                            (:file "vfile"   :depends-on ("constants" "logging" "pathnames" "sourcesink" "types"))
                            (:file "vfput"      :depends-on ("constants" "sourcesink" "types"))
                            ))
||#

   (:subsystem :core
            :depends-on (:pstore ;; :vfile
                         )
            :source-pathname "core"
            :components (
                         (:file "canonical"    :depends-on ("core-variables"))
                         (:file "cid-detail-table" :depends-on ("core-variables"))
                         (:file "cid-set"      :depends-on ("core-variables"))
                         (:file "cid-object"   :depends-on ("core-variables" "canonical"
                                                            "cid-set" "marshal" "master-table" "repository"))
                         (:file "core-variables")
                         (:file "core-user"    :depends-on ("core-variables" "distributed" "vclass"))
                         (:file "cvi"          :depends-on ("core-variables" "cid-set" "mapper" "repository" "txn-context" "vvalue"))
                         (:file "cvfile"       :depends-on ("core-variables" "cid-set" "mapper" "repository" "txn-context" "vvalue"))
                         (:file "distributed"  :depends-on ("core-variables" "mapper" "vclass"))
                         (:file "lvi"          :depends-on ("core-variables" "nvi" "vvalue"))
                         (:file "marshal"      :depends-on ("core-variables"))
                         (:file "master-table" :depends-on ("core-variables" "cid-set" "cid-detail-table"))
                         (:file "mapper"       :depends-on ("core-variables"))
                         (:file "nvi"          :depends-on ("core-variables" "cid-set" "txn-context" "vvalue"))
                         (:file "repository"   :depends-on ("core-variables"
                                                            "cid-set" "marshal"
                                                            "master-table" "mapper"
                                                            "txn-context" ))
                         (:file "svi"          :depends-on ("core-variables" "cid-set" "repository" "txn-context" "vvalue"))
                         (:file "txn-context"  :depends-on ("core-variables" "cid-set" "master-table"))
                         (:file "vclass"       :depends-on ("core-variables" "cid-object" "cvi" "cvfile" "lvi" "nvi" "svi" "txn-context"))
                         ;; (:file "vfile3"       :depends-on ("core-variables"))
                         (:file "vvalue"       :depends-on ("core-variables"))
                         (:file "tests"        :depends-on ("core-variables"  "repository" "vclass" ))))

   (:subsystem :vm
            :depends-on (:core)
            :source-pathname "vm"
            :components ((:file "container")
                         (:file "branch"      :depends-on ("described-object" "named-object" "version"))
                         (:file "change-set"  :depends-on ("described-object" "named-object"))
                         (:file "described-object")
                         (:file "named-object")
                         (:file "project"     :depends-on ("described-object" "named-object" "branch"))
                         (:file "project-reference" :depends-on ("described-object" "named-object"))
                         (:file "version"     :depends-on ("described-object" "named-object" "change-set"))
                         (:file "versionref"  :depends-on ())
                         (:file "vm-txn"      :depends-on ("change-set" "branch" "described-object" "named-object" "version"))))

   (:subsystem :rfm
            :depends-on (:vm :server)
            :source-pathname "rfm"
            :components ((:file "repository-file-system")
                         (:file "change-context" :depends-on ("rfm-project"))
                         (:file "publish" :depends-on ("repository-file-system"))
                         (:file "rfm-branch"  :depends-on ("repository-file-system" "rfm-project" "publish"))
                         (:file "rfm-project" :depends-on ("repository-file-system" "publish"))
                         (:file "rfm-user")
                         (:file "x-vm" :depends-on ("change-context" "repository-file-system" "rfm-project"))))

   (:subsystem :conman
            :depends-on (:rfm :server :vm)
            :source-pathname "conman"
            :components (
                         (:file "alias")
                         (:file "branch" :depends-on ("errors" "subsystem"))
                         (:file "class")
                         (:file "cli-request" :depends-on ("alias"))
                         (:file "cm-cli" :depends-on ("cm-cli-command-syntax-constants"
                                                      "cm-session-context"))
                         (:file "cm-cli-command-syntax-constants")
                         (:file "cm-returns")
                         (:file "cm-session-context")
                         (:file "cm-txn")
                         (:file "cmctl" :depends-on ("cm-cli-command-syntax-constants"
                                                     "class"
                                                     "cm-txn" "conman" "errors" "master-catalog"
                                                     "satellite-project" "satellite-subsystem"
                                                     "request" "workspace"))
                         (:file "conman")
                         (:file "conman-user")
                         (:file "errors")
                         (:file "master-catalog" :depends-on ("branch" "class" "cm-txn" "errors"
                                                              "product" "subsystem"))
                         (:file "new-tests"      :depends-on ("cli-request"
                                                              "cm-cli"
                                                              "cm-cli-command-syntax-constants"
                                                              "cm-returns"
                                                              "server-log"))
                         (:file "product"        :depends-on ("satellite-project"))
                         (:file "request")
                         (:file "subsystem"      :depends-on ("errors" "satellite-subsystem"))
                         (:file "satellite-project")
                         (:file "satellite-subsystem")
                         (:file "server-log")
                         (:file "subsystem-satellite")
                         (:file "workspace" :depends-on ("cm-txn"))
                         ))

   (:subsystem :main
            :depends-on (:conman)
            :source-pathname "conman"
            :components (
                         (:file "main")
                         ))))

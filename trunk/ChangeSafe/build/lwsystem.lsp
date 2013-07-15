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

(defsystem csf-foundation
  (:package "CSF/UTILITY"
   :default-pathname #.(translate-logical-pathname "CSF:UTILITY;"))
  :members ("declarations"
            "if-star"
             
            "conditions"
            "types"

            "fixnum"
            "length"
            "vector"
            "replacement-functions"

            "utility-base"
            "utility-macros"
            "protect-base"

            "series-extra"
            "syscompat"
            "mpcompat")
  :rules ((:in-order-to :compile ("conditions" "types")
                        (:caused-by (:compile "declarations"))
                        (:requires  (:load "declarations")))

          (:in-order-to :compile ("fixnum"
                                  "length"
                                  "replacement-functions"
                                  "utility-base"
                                  "utility-macros"
                                  "vector")
                        (:caused-by (:compile "declarations"
                                              "types"
                                              "conditions"))
                        (:requires (:load "declarations"
                                          "types"
                                          "conditions")))
          (:in-order-to :load ("fixnum"
                               "length"
                               "utility-base"
                               "utility-macros"                               
                               "vector")
                        (:requires (:load "declarations"
                                          "types"
                                          "conditions")))

          (:in-order-to :compile ("series-extra")
                        (:caused-by (:compile "declarations"
                                              "utility-macros"))
                        (:requires (:load "declarations"
                                          "utility-macros")))
          (:in-order-to :compile ("syscompat")
                        (:caused-by (:compile "declarations"
                                              "utility-macros"))
                        (:requires (:load "declarations"
                                          "utility-macros")))
          (:in-order-to :compile ("mpcompat")
                        (:caused-by (:compile "declarations"
                                              "conditions"
                                              "protect-base"
                                              "utility-macros"))
                        (:requires (:load "declarations"
                                          "conditions"
                                          "protect-base"
                                          "utility-macros")))))

(defsystem csf-utility
    (:package "CSF/UTILITY"
     :default-pathname #.(translate-logical-pathname "CSF:UTILITY;"))
  :members (("csf-foundation" :type :system)
            "ascii"
            "base64"
            "buffer"
            "cmdline"
            "crc"
            "date-time"
            "fake-excl"
            "graph"
            "hash"
            "iso639"
            "iso3166"
            "ndiff"
            "objmap"
            "oset"
            "pclass"
            "phash"
            "pnode"
            "pregexp"
            "pstore"
            "ptxn"
            "pathname-utilities"
            "platform"
            "osapi"
            "db-name"
            "rbtree"
            "serial"
            "time-stamp"
            "ustring"
            "utils"
            "strings"
            "guid"
            "uri"
            "uricompat"
            "socketcompat"
            "symtab"
            "win32-filesystem")
  :rules ((:in-order-to :compile ("ascii"
                                  "base64"
                                  "buffer"
                                  "cmdline"
                                  "crc"
                                  "date-time"
                                  "graph"
                                  "guid"
                                  "hash"
                                  "iso639"
                                  "iso3166"
                                  "ndiff"
                                  "osapi"
                                  "oset"
                                  "pathname-utilities"
                                  "platform"
                                  "pnode"
                                  "ptxn"
                                  "serial"
                                  "strings"
                                  "symtab"
                                  "time-stamp"
                                  "rbtree"
                                  "ustring"
                                  "utils"
                                  "win32-filesystem")
                        (:caused-by (:compile "csf-foundation"))
                        (:requires (:load "csf-foundation")))


          (:in-order-to :load ("ascii"
                               "base64"
                               "cmdline"
                               "crc"
                               "buffer"
                               "date-time"
                               "graph"
                               "hash"
                               "iso639"
                               "iso3166"
                               "ndiff"
                               "oset"
                               "pathname-utilities"
                               "platform"
                               "pnode"
                               "pregexp"
                               "rbtree"
                               "ustring"
                               "utils"
                               "symtab"
                               "time-stamp"
                               "win32-filesystem")
                        (:requires (:load "csf-foundation")))

          (:in-order-to :load ("strings")
                        (:requires (:load "csf-foundation" "utils")))

          (:in-order-to :compile ("db-name")
                        (:requires (:load "csf-foundation" "utils")))

          (:in-order-to :load ("db-name")
                        (:requires (:load "csf-foundation" "utils" "platform")))

          (:in-order-to :load ("osapi")
                        (:requires (:load "csf-foundation" "platform" "pathname-utilities")))

          (:in-order-to :load ("guid")
                        (:requires (:load "csf-foundation" "utils" "hash")))

          (:in-order-to :load ("uri")
                        (:requires (:load "ascii")))

          (:in-order-to :compile ("uri")
                        (:caused-by (:compile "csf-foundation" "ascii" "ustring"))
                        (:requires (:load "csf-foundation" "ascii" "ustring")))

          (:in-order-to :compile ("socketcompat")
                        (:caused-by (:compile "csf-foundation" "ascii" "ustring"))
                        (:requires (:load "csf-foundation" "ascii" "ustring")))

         (:in-order-to :compile ("fake-excl")
                       (:caused-by (:compile "csf-foundation" "ustring"))
                       (:requires (:load "csf-foundation" "ustring" "pregexp")))

          (:in-order-to :compile ("serial")
                        (:requires (:load "csf-foundation" "guid")))
          (:in-order-to :load ("serial")
                        (:requires (:load "csf-foundation" "guid")))
          (:in-order-to :compile ("objmap")
                        (:requires (:load "csf-foundation" "pnode" "serial")))
          (:in-order-to :compile ("pstore")
                        (:requires (:load "csf-foundation" "pnode" "serial" "objmap")))
          (:in-order-to :load ("pstore")
                        (:requires (:load "csf-foundation" "pnode" "serial" "objmap")))
          (:in-order-to :compile ("ptxn")
                        (:requires (:load "csf-foundation" "pnode" "serial" "objmap" "pstore")))
          (:in-order-to :load ("ptxn")
                        (:requires (:load "csf-foundation" "pnode" "serial" "objmap" "pstore")))
          (:in-order-to :compile ("pclass")
                        (:requires (:load "csf-foundation" "pnode" "serial" "objmap" "pstore" "ptxn")))
          (:in-order-to :compile ("phash")
                        (:requires (:load "csf-foundation" "pnode" "serial" "objmap" "pstore" "ptxn" "pclass")))
          ))

(defsystem csf-test-suites
  (:package "CSF/UTILITY"
   :default-pathname #.(translate-logical-pathname "CSF:UTILITY;"))
  :members (("csf-foundation" :type :system)
            ("csf-utility"    :type :system)
            "test-suites")
  :rules ((:in-order-to :compile ("test-suites")
                        (:requires (:load "csf-foundation" "csf-utility"))
                        (:caused-by (:compile "csf-foundation" "csf-utility")))
          (:in-order-to :load ("test-suites")
                        (:requires (:load "csf-foundation")))))

(defsystem csf-java-tools
  (:package "CSF/JAVA-TOOLS"
   :default-pathname #.(translate-logical-pathname "CSF:JAVA;AUXILIARY;"))
   :members (("csf-foundation" :type :system)
             ("csf-utility" :type :system)
             "java-tools"
             "makejava")
   :rules ((:in-order-to :compile ("java-tools")
                         (:requires (:load "csf-foundation"
                                           "csf-utility")))

           (:in-order-to :load ("java-tools")
                         (:requires (:load "csf-foundation"
                                           "csf-utility")))

           (:in-order-to :compile ("makejava")
                         (:requires (:load "csf-foundation"
                                           "csf-utility"
                                           "java-tools")))))

(defsystem csf-java-components
  (:package "CHANGESAFE"
   :default-pathname #.(translate-logical-pathname "CSF:BUILD;"))
  :members (("csf-java-tools" :type :system)
            "java-components"))

(defsystem csf-core
  (:package "CSF/CORE"
   :default-pathname #.(translate-logical-pathname "CSF:CORE;"))
  :members (("csf-utility"    :type :system)
            "core-variables"
            "distributed"
            "canonical"
            "cid-set"
            "master-table"
            "vclass"
            ;; "vvalue"
            "vfile3"
            "mapper"
            "cvi"
            "svi"
            "txn-context"
            "core-user"
            "repository")
  :rules ((:in-order-to :compile ("core-user" "distributed")
                        (:requires (:load "csf-utility" "core-variables" "vclass")))
          (:in-order-to :load ("core-user" "distributed")
                        (:requires (:load "csf-utility" "core-variables" "vclass")))
          (:in-order-to :compile ("cvi")
                        (:requires (:load "csf-utility" "core-variables" "vclass"))))
  )

(defsystem csf-server
  (:package "CSF/SERVER"
   :default-pathname #.(translate-logical-pathname "CSF:SERVER;"))
  :members (("csf-utility" :type :system)
            ("csf-test-suites" :type :system)
            "file-system"
            )
  :rules ((:in-order-to :compile ("file-system")
                        (:requires (:load "csf-test-suites"))))
  )

(defsystem csf-vm
  (:package "CSF/VERSION-MANAGEMENT"
   :default-pathname #.(translate-logical-pathname "CSF:VM;"))
  :members (("csf-utility" :type :system)
            ("csf-server"  :type :system)
            ("csf-core"    :type :system)
            "described-object"
            "named-object"
            "container"
            "version"
            "branch"
            "project"
            "vm-txn")
  :rules ((:in-order-to :compile ("described-object" "named-object" "container" "vm-txn" "project" "branch" "version")
                        (:caused-by (:compile "csf-utility" "csf-core"))
                        (:requires (:load "csf-utility" "csf-server" "csf-core")))
          (:in-order-to :compile ("project" "branch" "version")
                        (:requires (:load "described-object" "named-object")))))

(defsystem csf-rfm
  (:package "CSF/REPOSITORY-FILE-MANAGEMENT"
   :default-pathname #.(translate-logical-pathname "CSF:RFM;"))
  :members (("csf-utility" :type :system)
            ("csf-server"  :type :system)
            ("csf-core"    :type :system)
            ("csf-vm"      :type :system)
            "rfm-project"
            "repository-file-system"
            "rfm-server")
  :rules ((:in-order-to :compile ("repository-file-system" "rfm-server" "rfm-project")
                        (:caused-by (:compile "csf-utility" "csf-core" "csf-vm"))
                        (:requires (:load "csf-utility" "csf-server" "csf-core" "csf-vm")))))

(defsystem csf-core-tests
  (:package "CSF/CORE"
   :default-pathname #.(translate-logical-pathname "CSF:CORE;"))
  :members (("csf-test-suites" :type :system)
            ("csf-core"        :type :system)
            "tests")
  :rules ((:in-order-to :compile ("tests")
                        (:requires (:load "csf-test-suites" "csf-core")))
          (:in-order-to :load ("tests")
                        (:requires (:load "csf-test-suites" "csf-core")))))

(defsystem csf-utility-tests
  (:package "CSF/UTILITY"
   :default-pathname #.(translate-logical-pathname "CSF:UTILITY;"))
  :members (("csf-foundation" :type :system)
            ("csf-utility"     :type :system)
            ("csf-test-suites" :type :system)
            "tests")
  :rules ((:in-order-to :compile ("tests")
                        (:requires (:load "csf-foundation" "csf-utility" "csf-test-suites"))
                        (:caused-by (:compile "csf-foundation" "csf-utility" "csf-test-suites")))
          (:in-order-to :load ("tests")
                        (:requires (:load "csf-foundation" "csf-utility" "csf-test-suites")))))

(defsystem csf-vm-tests
  (:package "CSF/VERSION-MANAGEMENT"
   :default-pathname #.(translate-logical-pathname "CSF:VM;"))
  :members (("csf-test-suites" :type :system)
            ("csf-vm"          :type :system)
            "tests")
  :rules ((:in-order-to :compile ("tests")
                        (:requires (:load "csf-test-suites")))
          (:in-order-to :load ("tests")
                        (:requires (:load "csf-test-suites")))))

(defsystem csf-rfm-tests
  (:package "CSF/REPOSITORY-FILE-MANAGEMENT"
   :default-pathname #.(translate-logical-pathname "CSF:RFM;"))
  :members (("csf-foundation"  :type :system)
            ("csf-utility"     :type :system)
            ("csf-java-tools"  :type :system)
            ("csf-test-suites" :type :system)
            ("csf-server"      :type :system)
            ("csf-core"        :type :system)
            ("csf-vm"          :type :system)
            ("csf-rfm"         :type :system)
            "tests")
  :rules ((:in-order-to :compile ("tests")
                        (:requires (:load "csf-foundation"
                                          "csf-utility"
                                          "csf-test-suites"
                                          "csf-server"
                                          "csf-core"
                                          "csf-vm"
                                          "csf-rfm"))
                        (:caused-by (:compile "csf-foundation"
                                              "csf-utility"
                                              "csf-test-suites"
                                              "csf-server"
                                              "csf-core"
                                              "csf-vm"
                                              "csf-rfm")))
          (:in-order-to :load ("tests")
                        (:requires (:load "csf-foundation"
                                          "csf-test-suites")))))

(defsystem csf-server-tests
  (:package "CSF/SERVER"
   :default-pathname #.(translate-logical-pathname "CSF:SERVER;"))
   :members (("csf-java-tools"  :type :system)
             ("csf-test-suites" :type :system)
             ("csf-server"      :type :system)
             "tests")
  :rules ((:in-order-to :compile ("tests")
                        (:requires (:load "csf-test-suites" "csf-server")))
          (:in-order-to :load ("tests")
                        (:requires (:load "csf-java-tools" "csf-server" "csf-test-suites")))))

(defsystem csf-tests
  (:package "CHANGESAFE"
   :default-pathname #.(translate-logical-pathname "CSF:CORE;"))
  :members (("csf-utility-tests" :type :system)
            ("csf-server-tests"  :type :system)
            ("csf-core-tests"    :type :system)
            ("csf-vm-tests"      :type :system)
            ("csf-rfm-tests"     :type :system)))

(defsystem meta-parser
  (:package "META"
   :default-pathname #.(translate-logical-pathname "CSF:UTILITY;"))
  :members (("csf-foundation" :type :system)
            "meta")
  :rules ((:in-order-to :compile ("meta")
                        (:caused-by (:compile "csf-foundation"))
                        (:requires (:load "csf-foundation")))))

(defsystem html-generator
  (:package "NET.HTML.GENERATOR"
   :default-pathname #.(translate-logical-pathname "CSF:HTMLGEN;"))
   :members (("csf-foundation" :type :system)
             "htmlgen")
   :rules ((:in-order-to :compile ("htmlgen")
                         (:caused-by (:compile "csf-foundation"))
                         (:requires (:load "csf-foundation")))))

(defsystem allegroserve
  (:package "NET.ASERVE"
   :default-pathname #.(translate-logical-pathname "CSF:ALLEGROSERVE;"))
  :members (("csf-foundation" :type :system)
            ("meta-parser"    :type :system)
            ("csf-utility"    :type :system)
            ("html-generator" :type :system)
            "macs"
            "main"
            "headers"
            "parse"
            "decode"
            "publish"
            "authorize"
            "log"
            "client"
            "proxy")
  :rules (;; need utility for URI
          (:in-order-to :load ("macs")
                        (:requires (:load "csf-utility")))

          (:in-order-to :compile ("macs")
                        (:caused-by (:compile "csf-utility"))
                        (:requires (:load "csf-utility")))

          (:in-order-to :compile ("main"
                                  "decode")
                        (:caused-by (:compile "csf-foundation"
                                              "csf-utility"
                                              "html-generator"
                                              "meta-parser"
                                              "macs"))
                        (:requires (:load "csf-foundation"
                                          "csf-utility"
                                          "html-generator"
                                          "meta-parser"
                                          "macs")))
          (:in-order-to :load ("main")
                        (:requires (:load "meta-parser")))

          (:in-order-to :compile ("authorize"
                                  "client"
                                  "headers"
                                  "log"
                                  "parse"
                                  "publish")
                        (:caused-by (:compile "csf-foundation"
                                              "macs"
                                              "main"))
                        (:requires (:load  "csf-foundation"
                                              "macs"
                                              "main")))
          (:in-order-to :compile ("proxy")
                        (:caused-by (:compile "csf-foundation"
                                              "macs"
                                              "main"
                                              "headers"
                                              "client"))
                        (:requires (:load "csf-foundation"
                                          "macs"
                                          "main"
                                          "headers"
                                          "client")))

          ))

;(defsystem property-list
;  (:package "PLIST"
;   :default-pathname #.(translate-logical-pathname "CSF:PLIST;"))
;  :members (("csf-foundation" :type :system)
;            "package"
;            "plist")
;  :rules ((:in-order-to :compile ("package")
;                        (:requires (:load "csf-foundation")))
                                  
;          (:in-order-to :compile ("plist")
;                        (:requires (:load "csf-foundation" "package")))))

;(defsystem tokenizer
;  (:package "TOKENIZER"
;   :default-pathname #.(translate-logical-pathname "CSF:TOKENIZER;"))
;  :members (("csf-foundation"  :type :system)
;            "packages"
;            "tokenizer")
;  :rules ((:in-order-to :compile ("packages")
;                        (:requires (:load "csf-foundation")))
                                  
;          (:in-order-to :compile ("tokenizer")
;                        (:requires (:load "csf-foundation" "packages")))))

;(defsystem html-parser
;  (:package "HTML-PARSER"
;   :default-pathname #.(translate-logical-pathname "CSF:HTML-PARSER;"))
;  :members (("csf-foundation"  :type :system)
;            ("property-list" :type :system)
;            ("tokenizer" :type :system)
;            "packages"
;            "defs"
;            "patmatch"
;            "rewrite-engine"
;            "rewrite-rules"
;            "html-tags"
;            "html-reader"
;            "html-parser"
;            "html-utilities"
;            )
;  :rules ((:in-order-to :compile ("packages")
;                        (:requires (:load "csf-foundation" "property-list" "tokenizer")))
;          (:in-order-to :load ("packages")
;                        (:requires (:load "csf-foundation" "property-list" "tokenizer")))
;          (:in-order-to :compile ("defs" "patmatch" "rewrite-engine" "rewrite-rules" "html-tags"
;                                  "html-reader" "html-parser" "html-utilities")
;                        (:requires (:load :previous)))))

;(defsystem lsp-base
;  (:package "LSP"
;   :default-pathname #.(translate-logical-pathname "CSF:LSP;"))
;  :members (("csf-foundation" :type :system)
;            ("csf-utility" :type :system)
;            ("allegroserve" :type :system)
;            ("tokenizer" :type :system)
;            ("property-list" :type :system)
;            ("html-parser" :type :system)
;            "packages"
;            "define-macros")
;  :rules ((:in-order-to :compile ("packages")
;                       (:requires (:load "csf-foundation" 
;                                         "csf-utility"
;                                         "allegroserve"
;                                         "tokenizer"
;                                         "property-list"
;                                         "html-parser")))
;          (:in-order-to :load ("packages")
;                        (:requires (:load "csf-foundation"
;                                          "csf-utility"
;                                          "allegroserve"
;                                          "tokenizer"
;                                          "property-list"
;                                          "html-parser")))
;          (:in-order-to :compile ("define-macros")
;                        (:requires (:load "csf-foundation" 
;                                    "csf-utility"
;                                    "allegroserve"
;                                    "tokenizer"
;                                    "property-list"
;                                    "html-parser"
;                                    "packages")))
;          (:in-order-to :load ("define-macros")
;                        (:requires (:load "csf-foundation" 
;                                    "csf-utility"
;                                    "allegroserve"
;                                    "tokenizer"
;                                    "property-list"
;                                    "html-parser"
;                                    "packages")))))

;(defsystem lsp-servers
;  (:package "LSP"
;            :default-pathname #.(translate-logical-pathname "CSF:LSP;servers;"))
;  :members (("csf-foundation" :type :system)
;            ("csf-utility" :type :system)
;            ("allegroserve" :type :system)
;            ("tokenizer" :type :system)
;            ("property-list" :type :system)
;            ("html-parser" :type :system)
;            ("lsp-base" :type :system)
;            "aserve-engine"
;            "template-engine")
;  :rules ((:in-order-to :compile ("aserve-engine")
;                        (:requires (:load "csf-foundation" 
;                                          "csf-utility"
;                                          "allegroserve"
;                                          "tokenizer"
;                                          "property-list"
;                                          "html-parser"
;                                          "lsp-base")))
;          (:in-order-to :load ("aserve-engine")
;                        (:requires (:load "csf-foundation" 
;                                          "csf-utility"
;                                          "allegroserve"
;                                          "tokenizer"
;                                          "property-list"
;                                          "html-parser"
;                                          "lsp-base")))
;          (:in-order-to :compile ("template-engine")
;                        (:requires (:load "csf-foundation" 
;                                          "csf-utility"
;                                          "allegroserve"
;                                          "tokenizer"
;                                          "property-list"
;                                          "html-parser"
;                                          "lsp-base"
;                                          "aserve-engine")))
;          (:in-order-to :load ("template-engine")
;                        (:requires (:load "csf-foundation" 
;                                          "csf-utility"
;                                          "allegroserve"
;                                          "tokenizer"
;                                          "property-list"
;                                          "html-parser"
;                                          "lsp-base"
;                                          "aserve-engine")))))
            
;(defsystem lisp-server-pages
;  (:package "LSP"
;            :default-pathname #.(translate-logical-pathname "CSF:LSP;"))
;  :members (("csf-foundation" :type :system)
;            ("csf-utility" :type :system)
;            ("allegroserve" :type :system)
;            ("tokenizer" :type :system)
;            ("property-list" :type :system)
;            ("html-parser" :type :system)
;            ("lsp-base" :type :system)
;            ("lsp-servers" :type :system)
;            "parser"
;            "cookie"
;            "publish"
;            "scribe"
;            "lsp-user")
;  :rules (

;          (:in-order-to :compile ("parser" "cookie" "publish" "scribe" "lsp-user")
;                        (:requires (:load 
;                                    "csf-foundation" 
;                                    "csf-utility"
;                                    "allegroserve"
;                                    "tokenizer"
;                                    "property-list"
;                                    "html-parser"
;                                    "lsp-base"
;                                    "lsp-servers")))
;          (:in-order-to :load ("parser" "cookie" "publish" "scribe" "lsp-user")
;                        (:requires (:load 
;                                    "csf-foundation" 
;                                    "csf-utility"
;                                    "allegroserve"
;                                    "tokenizer"
;                                    "property-list"
;                                    "html-parser"
;                                    "lsp-base"
;                                    "lsp-servers")))))
;(defsystem publisher
;  (:package "PUBLISHER"
;   :default-pathname #.(translate-logical-pathname "CSF:PUBLISHER;"))
;  :members (("csf-foundation" :type :system)
;            ("lisp-server-pages" :type :system)
;            "packages"
;            "publish-site")
;  :rules ((:in-order-to :compile ("packages")
;                        (:requires (:load "csf-foundation" "lisp-server-pages")))
                                 
;          (:in-order-to :compile ("publish-site")
;                        (:requires (:load "csf-foundation" "lisp-server-pages" "packages")))))

(defsystem changesafe
  (:package "CHANGESAFE"
   :default-pathname #.(translate-logical-pathname "CSF:CONMAN;"))
  :members (("csf-foundation"  :type :system)
            ("csf-utility"     :type :system)
            ("allegroserve"    :type :system)
            ;("lisp-server-pages" :type :system)
            ;("publisher"       :type :system)
            "main")
  :rules ((:in-order-to :compile ("main")
                        (:requires (:load "csf-foundation" "csf-utility" "allegroserve" 
                                          ;; "lisp-server-pages"
                                          ;; "publisher"
                                          )))
          (:in-order-to :load ("main")
                        (:requires (:load "csf-foundation" "csf-utility" "allegroserve"
                                          ;;"lisp-server-pages"
                                          ;;"publisher"
                                          )))))

#||
;;; The foundation system is stuff fixes up lisp.
(defsystem csf-foundation
  (:package "CSF/UTILITY"
   :default-pathname #.(translate-logical-pathname "CSF:UTILITY;"))
  :members ("declarations"
            "if-star"

            "conditions"
            "types"

            "fixnum"
            "index"
            "length"
            "vector"

            "protect-base"
            "utility-base"
            "utility-macros"

            "series-extra"
            "syscompat"
            "mpcompat"
            )

  :rules ((:in-order-to :compile ("types"
                                  "conditions")
                        (:requires (:load "declarations"))
                        (:caused-by (:compile "declarations")))

          (:in-order-to :load ("conditions" "types")
                        (:requires (:load "declarations")))

          (:in-order-to :compile ("index"
                                  "fixnum")
                        (:caused-by (:compile "declarations" "types"))
                        (:requires (:load "declarations" "types")))

          (:in-order-to :load ("index"
                               "fixnum")
                        (:requires (:load "declarations" "types")))

          (:in-order-to :compile ("length"
                                  "vector")
                        (:caused-by (:compile "declarations"
                                              "types"
                                              "conditions"))
                        (:requires (:load "declarations"
                                          "types"
                                          "conditions")))

          (:in-order-to :load ("length"
                               "vector")
                        (:requires (:load "declarations" "types")))
                               

          (:in-order-to :compile ("utility-base")
                        (:caused-by (:compile "declarations"
                                              "types"))
                        (:requires (:load "declarations"
                                          "types")))

          (:in-order-to :compile ("utility-macros")
                        (:caused-by (:compile "declarations"))
                        (:requires (:load "declarations")))

          (:in-order-to :compile ("series-extra")
                        (:caused-by (:compile "declarations"
                                              "utility-macros"))
                        (:requires (:load "declarations"
                                          "utility-macros")))

          (:in-order-to :compile ("syscompat")
                        (:caused-by (:compile "declarations"
                                              "utility-macros"))
                        (:requires (:load "declarations"
                                          "utility-macros")))

          (:in-order-to :compile ("mpcompat")
                        (:caused-by (:compile "declarations"
                                              "conditions"
                                              "protect-base"
                                              "utility-macros"))
                        (:requires (:load "declarations"
                                          "conditions"
                                          "protect-base"
                                          "utility-macros")))))

(defsystem meta-parser
  (:package "META"
   :default-pathname #.(translate-logical-pathname "CSF:UTILITY;"))
  :members (("csf-foundation" :type :system)
            "meta")
  :rules ((:in-order-to :compile ("meta")
                        (:caused-by (:compile "csf-foundation"))
                        (:requires (:load "csf-foundation")))))

(defsystem html-generator
  (:package "NET.HTML.GENERATOR"
   :default-pathname #.(translate-logical-pathname "CSF:HTMLGEN;"))
   :members (("csf-foundation" :type :system)
             "htmlgen")
   :rules ((:in-order-to :compile ("htmlgen")
                         (:caused-by (:compile "csf-foundation"))
                         (:requires (:load "csf-foundation")))))

(defsystem csf-utility
    (:package "CSF/UTILITY"
     :default-pathname #.(translate-logical-pathname "CSF:UTILITY;"))
  :members (("csf-foundation" :type :system)

            "ascii"
            "date-time"
            "hash"
            "platform"
            "pregexp"

            "buffer"
            "guid"
            "ndiff"
            "osapi"
            "pathname-utilities"
            "promise"
            "test-suites"
            "ustring"

            "uri"

            "uricompat"
            "socketcompat"
            "fake-excl"
            "utils")

  :rules ((:in-order-to :load ("ascii")
                        (:requires (:load "csf-foundation")))

          (:in-order-to :compile ("ascii"
                                  "date-time"
                                  "hash"
                                  "platform"
                                  "pregexp"
                                  "utils"
                                  )
                        (:caused-by (:compile "csf-foundation"))
                        (:requires (:load "csf-foundation")))
          (:in-order-to :compile ("buffer"
                                  "guid"
                                  "ndiff"
                                  "pathname-utilities"
                                  "promise"
                                  "test-suites"
                                  "ustring")
                        (:caused-by (:compile "csf-foundation"
                                              "ascii"
                                              "hash"
                                              "platform"
                                              ))
                        (:requires (:load "csf-foundation"
                                          "ascii"
                                          "hash"
                                          "platform"
                                          )))
          (:in-order-to :load ("osapi")
                        (:requires (:load "pathname-utilities")))

          (:in-order-to :load ("uri")
                        (:requires (:load "ascii")))

          (:in-order-to :compile ("uri")
                        (:caused-by (:compile "csf-foundation"
                                              "ascii"
                                              "ustring"
                                              ))
                        (:requires (:load "csf-foundation"
                                          "ascii"
                                          "ustring"
                                          )))

          (:in-order-to :compile ("uricompat")
                        (:caused-by (:compile "csf-foundation" "uri"))
                        (:requires (:load "uri")))


         (:in-order-to :compile ("socketcompat")
                       (:caused-by (:compile "csf-foundation"
                                             "ustring"
                                             ))
                       (:requires (:load "csf-foundation"
                                         "ustring"
                                         )))
         (:in-order-to :compile ("fake-excl")
                       (:caused-by (:compile "csf-foundation"
                                             "ustring"
                                             ))
                       (:requires (:load "csf-foundation"
                                         "ustring"
                                         "pregexp"
                                         )))))

(defsystem allegroserve
  (:package "NET.ASERVE"
   :default-pathname #.(translate-logical-pathname "CSF:ALLEGROSERVE;"))
  :members (("csf-foundation" :type :system)
            ("meta-parser"    :type :system)
            ("csf-utility"    :type :system)
            ("html-generator" :type :system)
            "macs"
            "main"
            "headers"
            "parse"
            "decode"
            "publish"
            "authorize"
            "log"
            "client"
            "proxy")
  :rules ((:in-order-to :compile ("macs")
                        (:caused-by (:compile "csf-utility"))
                        (:requires (:load "csf-utility")))

          (:in-order-to :compile ("main"
                                  "decode")
                        (:caused-by (:compile "csf-foundation"
                                              "csf-utility"
                                              "html-generator"
                                              "meta-parser"
                                              "macs"))
                        (:requires (:load "csf-foundation"
                                          "csf-utility"
                                          "html-generator"
                                          "meta-parser"
                                          "macs")))
          (:in-order-to :compile ("authorize"
                                  "client"
                                  "headers"
                                  "log"
                                  "parse"
                                  "publish")
                        (:caused-by (:compile "csf-foundation"
                                              "macs"
                                              "main"))
                        (:requires (:load  "csf-foundation"
                                              "macs"
                                              "main")))
          (:in-order-to :compile ("proxy")
                        (:caused-by (:compile "csf-foundation"
                                              "macs"
                                              "main"
                                              "headers"
                                              "client"))
                        (:requires (:load "csf-foundation"
                                          "macs"
                                          "main"
                                          "headers"
                                          "client")))

          ))

(defsystem csf-java-tools
  (:package "CSF/JAVA-TOOLS"
   :default-pathname #.(translate-logical-pathname "CSF:JAVA;AUXILIARY;"))
   :members (("csf-foundation" :type :system)
             ("csf-utility" :type :system)
             "java-tools"
             "makejava")
   :rules ((:in-order-to :load ("java-tools")
                         (:requires (:load "csf-foundation"
                                           "csf-utility")))

           (:in-order-to :compile ("makejava")
                         (:requires (:load "csf-foundation"
                                           "csf-utility"
                                           "java-tools")))))

(defsystem csf-java-components
  (:package "CHANGESAFE"
   :default-pathname #.(translate-logical-pathname "CSF:BUILD;"))
  :members (("csf-java-tools" :type :system)
            "java-components"))

(defsystem csf-core
  (:package "CSF/CORE"
   :default-pathname #.(translate-logical-pathname "CSF:CORE;"))
  :members ("vstream"
            "tests")
  :rules ((:in-order-to :load ("tests")
                       (:requires (:load "vstream")))))

(defsystem conman
  (:package "CHANGESAFE"
   :default-pathname #.(translate-logical-pathname "CSF:CONMAN;"))
  :members (("allegroserve" :type :system)
             "main")
  :rules ((:in-order-to :compile ("main")
                        (:requires (:load "allegroserve")))))

(defsystem changesafe
  (:package "CHANGESAFE"
   :default-pathname #.(translate-logical-pathname "CSF:;"))
  :members (("csf-foundation" :type :system)
            ("html-generator" :type :system)
            ("csf-utility" :type :system)
            #|| ("allegroserve" :type :system) ||#
            ("csf-core" :type :system))

  :rules ((:in-order-to :compile ("html-generator")
                        (:requires (:load "csf-foundation"))
                        (:caused-by (:compile "csf-foundation")))

          (:in-order-to :compile ("csf-utility")
                        (:requires (:load "csf-foundation"))
                        (:caused-by (:compile "csf-foundation")))

||#

 #||(:in-order-to :compile ("allegroserve")
                        (:requires (:load "csf-utility"))
                        (:caused-by (:compile "csf-utility"))) ||#
#||
          (:in-order-to :compile ("csf-core")
                (:requires (:load "csf-foundation" "csf-utility"))
                (:caused-by (:compile "csf-foundation" "csf-utility")))))
||#

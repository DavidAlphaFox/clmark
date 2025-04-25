
(asdf:defsystem #:clmark
  :depends-on (#:clmark/base #:clmark/commonmark #:clmark/custom))

(asdf:defsystem #:clmark/base
  :author "Lillia Friedel"
  :license "LGPLv3"
  :depends-on (#:cl-ppcre)
  :serial t
  :components ((:module :src
                :components ((:file "package")
                             (:file "utilities")
                             (:file "node")
                             (:file "parse")
                             (:file "render")))))

(asdf:defsystem #:clmark/commonmark
  :license "BSD3"
  :depends-on (#:clmark/base)
  :components ((:module "src/nodes/commonmark-nodes/"
                :components ((:module :block-nodes
                              :components ((:file "atx-heading")
                                           (:file "block-quote")
                                           (:file "bullet-list")
                                           (:file "fenced-code-block")
                                           (:file "indented-code-block")
                                           ;; (:file "list-container")
                                           (:file "paragraphs")
                                           (:file "thematic-break")))
                             (:module :inline-nodes
                              :components ((:file "emphasis")
                                           (:file "images")
                                           (:file "links")))))))

(asdf:defsystem #:clmark/custom
  :license "BSD3"
  :depends-on (#:clmark/base)
  :components ((:module "src/nodes/custom-nodes/"
                :components ((:module :block-nodes
                              :components ((:file "spoiler")))))))



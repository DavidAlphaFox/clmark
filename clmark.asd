
(asdf:defsystem #:clmark
  :depends-on (#:cl-ppcre)
  :serial t
  :components ((:module :src
                :components ((:file "package")
                             (:file "utilities")
                             ;; (:file "generics")
                             
                             (:file "node")
                             (:module :nodes
                              :components ((:file "atx-heading")
                                           (:file "block-quote")
                                           (:file "bullet-list")
                                           (:file "fenced-code-block")
                                           (:file "indented-code-block")
                                           (:file "list-container")
                                           (:file "paragraphs")
                                           (:file "spoiler")
                                           (:file "thematic-break")))
                             (:file "parse")
                             (:module :render
                              :components
                              ((:file "render")
                               (:file "html"))
                              ;; ((:module :html
                              ;;   :components ((:file "atx-heading"))))
                              )))))

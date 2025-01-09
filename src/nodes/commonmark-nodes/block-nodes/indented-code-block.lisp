
(in-package #:clmark)

(defblock (indented-code-block
           "^\\s{4}"
           ;; "^(\\s{4}\\s*\\S*)\\s*$"
           "^(\\s{4}\\s*\\S*)\\s*$"
           "^(\\s{0,3}\\S*|$)")
          (leaf-block-node child-node)
          ())

;; (defmethod test-print-block ((block indented-code-block)))

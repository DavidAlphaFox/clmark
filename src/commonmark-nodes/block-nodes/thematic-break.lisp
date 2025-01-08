
(in-package :clmark)

(defblock (thematic-break
           "^\\s{0,3}(-{3,}|_{3,}|\\*{3,})\\s*$"
           +null-regex+
           +null-regex+)
          (leaf-block-node child-node)
          ())

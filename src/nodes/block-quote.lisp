
(in-package #:clmark)

(defblock (block-quote "^\\s{0,3}>\\s?" "^\\s{0,3}>\\s?" "^\\s$" :dc t)
          (block-node parent-node child-node)
          ())

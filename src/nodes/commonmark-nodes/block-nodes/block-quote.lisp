
(in-package #:clmark)

(defblock (block-quote "^\\s{0,3}>\\s?" "^\\s{0,3}>\\s?" "^\\s$" :dc t)
          (block-node parent-node child-node)
          ())

(defmethod render ((node block-quote) (as (eql :html)) stream)
  (with-tags (stream ("<blockquote>") ("</blockquote>"))
    (render-text node :stream stream :style as)
    (render-children node :stream stream :style as)))

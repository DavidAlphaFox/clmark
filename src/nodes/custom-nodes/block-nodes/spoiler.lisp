
(in-package :clmark)


(defblock (spoiler "^::: spoiler\\s+" "^.*$" "^:::\\s*$")
          (block-node parent-node child-node)
          (spoiler-header))

(defmethod check-line-satisfies-block-and-advance ((block spoiler) line)
  (with-line (line)
    (multiple-value-bind (s e)
        (line-closes-block block line)
      (if s
          (progn
            (advance-line e)
            nil)
          t))))

(defmethod check-line-opens-block-and-advance ((block spoiler) line)
  (with-line (line)
    (multiple-value-bind (s e)
        (line-opens-block block line)
      (when s
        (setf (spoiler-header block) (subseq line e))
        (finish-line line)
        t))))

(defmethod render ((node spoiler) (as (eql :html)) stream)
  (with-tags (stream ("<details>") ("</details>"))
    (with-tags (stream ("<summary>") ("</summary>"))
      (cond ((spoiler-header node)
             (write-string (spoiler-header node) stream))
            (t 
             (render-text node :stream stream :style as))))
    (render-children node :stream stream :style as)))

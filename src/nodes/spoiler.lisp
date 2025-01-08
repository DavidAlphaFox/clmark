
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
            (incf *line-position* e)
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

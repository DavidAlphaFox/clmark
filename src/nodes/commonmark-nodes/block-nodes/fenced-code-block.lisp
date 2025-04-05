
(in-package :clmark)

(defblock (fenced-code-block
           "^\\s{0,3}(~~~|```)\\s*"
           +no-advance-regex+
           "^\\s{0,3}(~~~|```)\\s*$")
          (leaf-block-node child-node)
          (info-string style))

(defmethod check-line-opens-block-and-advance ((block fenced-code-block) line)
  (with-line (line)
    (multiple-value-bind (s e)
        (line-opens-block block line)
      (when s
        (setf (info-string block) (subseq line e)
              (style block) (cond ((char= (char line (1- e)) #\~) :tilde)
                                  ((char= (char line (1- e)) #\`) :backtick)
                                  (t (error "Unknown char ~C"
                                            (char line (1- e))))))
        (incf *line-position* (length line))
        t))))

(defmethod check-line-satisfies-block-and-advance ((block fenced-code-block) line)
  (with-line (line)
    (multiple-value-bind (s e)
        (line-closes-block block line)
      (if s
          (progn
            (ecase (style block)
              ((:tilde)
               (when (char= (char line (1- e)) #\`)
                 (return-from check-line-satisfies-block-and-advance t)))
              ((:backtick)
               (when (char= (char line (1- e)) #\~)
                 (return-from check-line-satisfies-block-and-advance t))))
            (incf *line-position* e)
            nil)
          t))))

(defmethod render ((node fenced-code-block) (as (eql :html)) stream)
  (with-tags (stream
              ("<pre><code~@[ class=\"language-~A\"~]>"
               (info-string fenced-code-block))
              ("</code></pre>"))
    (format stream "~{~A~^~%~}" (node-text node))))


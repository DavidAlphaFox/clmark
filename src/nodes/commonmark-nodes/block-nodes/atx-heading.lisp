
(in-package :clmark)

(defblock (atx-heading
           "\\s{0,3}#{1,6}\\s?"
           +null-regex+
           +null-regex+)
          (leaf-block-node child-node)
          (atx-header-level))

(defmethod check-line-opens-block-and-advance ((block atx-heading) line)
  (with-line (line)
    (multiple-value-bind (s e)
        (line-opens-block block line)
      (when s
        (setf (atx-header-level block) (min 6 (count #\# (subseq line s e)))
              (node-text block) (cons (subseq line e)
                                      (node-text block)))
        (finish-line line)
        t))))

(defmethod check-line-satisfies-block-and-advance ((block atx-heading) line)
  ;; Return nil because an atx heading can only last for one line
  nil)

(defmethod render ((node atx-heading) (as (eql :html)) stream)
  (with-tags (stream
              ("<h~D>" (atx-header-level node))
              ("</h~D>" (atx-header-level node)))
    (loop for text in (node-text node)
          do (render text as stream))))

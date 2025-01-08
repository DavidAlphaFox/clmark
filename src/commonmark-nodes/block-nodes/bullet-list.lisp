
(in-package :clmark)

;; TODO [11:15 07.01.2025]: When rendering, e.g. to html, bullet list blocks are
;; equivalent to list items. Therefore we should rearrange them into a single
;; container in our postprocessing.

(defblock (bullet-list-block
           "^\\s*(-|\\+|\\*)\\s{1,4}"
           "^\\s*"
           "^\\S")
          (block-node parent-node child-node)
          (indent-level
           (empty-lines-count :initform 0 :accessor empty-lines-count)))

(defmethod test-print-block ((block bullet-list-block) stream)
  (format stream "~&BULLET-LIST: (indent: ~D)~&~{~2T~A~^~%~}~&"
          (indent-level block)
          (reverse (node-text block))))

(defmethod check-line-opens-block-and-advance ((block bullet-list-block) line)
  (declare (optimize (debug 3) (speed 0) (safety 3)))
  (with-line (line)
    (multiple-value-bind (s e)
        (line-opens-block block line)
      (when s
        (assert (= s 0))
        (setf (indent-level block) (- e s))
        (advance-line (- e s))
        t))))

(defmethod check-line-satisfies-block-and-advance ((block bullet-list-block) line)
  (with-line (line)
    (multiple-value-bind (s e)
        (line-satisfies-block block line)
      (when s
        (assert (= s 0))
        (cond ((>= (empty-lines-count block) 3)
               nil)
              ((> e (indent-level block))
               (advance-line e)
               t)
              ((and (< e (indent-level block))
                    (every #'sb-unicode:whitespace-p line))
               (incf (empty-lines-count block))
               t)
              ((and (< e (indent-level block))
                    (some (complement #'sb-unicode:whitespace-p) line))
               nil)
              (t
               (advance-line e)
               t))))))

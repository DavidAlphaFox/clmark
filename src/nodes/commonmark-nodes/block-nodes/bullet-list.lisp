
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

(defblock (list-container-block +null-regex+ +null-regex+ +null-regex+)
          (block-node child-node parent-node)
          ())

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

(defmethod check-line-satisfies-block-and-advance ((block list-container-block)
                                                   line)
  t)

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

(defmethod add-node-as-child :around ((parent list-container-block) child)
  (if (typep child 'bullet-list-block)
      (call-next-method)
      (progn
        (%close-block nil parent)
        (add-node-as-child (parent parent) child))))

(defmethod add-node-as-child :around (parent (child bullet-list-block))
  (if (typep parent 'list-container-block)
      (call-next-method)
      (let ((container (make-instance 'list-container-block
                                      :root (root parent)
                                      :node-text nil
                                      :open? t
                                      :parent parent
                                      :children nil)))
        (add-node-as-child parent container)
        (add-node-as-child container child))))

(defmethod render ((node bullet-list-block) (as (eql :html)) stream)
  (with-tags (stream ("<li>") ("</li>"))
    (render-text node :stream stream :style as)
    (render-children node :stream stream :style as)))

(defmethod render ((node list-container-block) (as (eql :html)) stream)
  (with-tags (stream ("<ul>") ("</ul>"))
    (render-children node)))

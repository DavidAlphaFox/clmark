
(in-package :clmark)

(defblock (list-container-block +null-regex+ +null-regex+ +null-regex+)
          (block-node child-node parent-node)
          ())

(defmethod check-line-satisfies-block-and-advance ((block list-container-block)
                                                   line)
  t)

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

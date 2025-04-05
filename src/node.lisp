
(in-package :clmark)

 ; general class shape&structure
(dc parser-node () (rendered-text))

(dc parent-node (parser-node)
    (children)
  (:default-initargs :children nil))

(dc child-node (parser-node)
    (parent)
  (:default-initargs :parent nil))

(dc inline-node (child-node parser-node)
    (node-text root
               self-contained-p
               (regexp :initarg :regexp :accessor regexp :initform nil)
               
               open-delimiter close-delimiter
               open-delimiter-start open-delimiter-end
               close-delimiter-start close-delimiter-end
               text-start text-end
               
               close-immediately ;; process-closing-delimiter
               active?
               enabled-subparsers)
  (:default-initargs :self-contained-p nil))

(dc %block-node (parser-node)
    (open? node-text #| start-index end-index |# root))

(dc block-node (%block-node))

(dc leaf-block-node (%block-node))

 ; Begin block nodes
(dc document-node (block-node parent-node) (document-block-parsers))

(defmethod check-line-satisfies-block-and-advance ((block document-node) line)
  t)

(defmethod render ((node document-node) as stream)
  (render-children node :stream stream :style as))

(defmacro defblock ((blockname open-regex remain-open-regex close-regex
                     &key (dc t))
                    supers slots &body options)
  `(progn
     (,(if dc 'dc 'defclass) ,blockname ,supers ,slots ,@options)
     (defmethod line-closes-block ((block ,blockname) line)
       (cl-ppcre:scan ,close-regex line))
     (defmethod line-opens-block ((block ,blockname) line)
       (cl-ppcre:scan ,open-regex line))
     (defmethod line-satisfies-block ((block ,blockname) line)
       (cl-ppcre:scan ,remain-open-regex line))

     (defmethod line-closes-block ((block (eql ',blockname)) line)
       (cl-ppcre:scan ,close-regex line))
     (defmethod line-opens-block ((block (eql ',blockname)) line)
       (cl-ppcre:scan ,open-regex line))
     (defmethod line-satisfies-block ((block (eql ',blockname)) line)
       (cl-ppcre:scan ,remain-open-regex line))))

(defmacro definline ((name open-delim close-delim &key (dc t))
                     supers slots &body options)
  (let ((defins (assoc :default-initargs options)))
    (if defins
        (setf defins `(,(car defins)
                       :open-delimiter ,open-delim
                       :close-delimiter ,close-delim
                       ,@(cdr defins)))
        (setf defins `(:default-initargs :open-delimiter ,open-delim
                                         :close-delimiter ,close-delim
                                         )))
    (let ((options (remove :default-initargs options :key #'car)))
      `(,(if dc 'dc 'defclass) ,name ,supers ,slots ,@options ,defins))))

(defmacro define-simple-open-delimiter-processor
    (node-type (&optional stack-var node-var string-var index-var)
     &key try-close-open-nodes)
  (let ((stack-var (or stack-var (gensym "STACK-VAR")))
        (node-var (or node-var (gensym "NODE-VAR")))
        (string-var (or string-var (gensym "STRING-VAR")))
        (index-var (or index-var (gensym "INDEX-VAR"))))
    `(defmethod process-open-delimiter (,stack-var (,node-var ,node-type)
                                        ,string-var ,index-var)
       (flet ((do1 ()
                (let ((id1 (+ ,index-var (length (open-delimiter ,node-var)))))
                  (values (cons (make-instance ',node-type
                                               :open-delimiter-start ,index-var
                                               :open-delimiter-end id1
                                               :text-start id1
                                               :active? t)
                                ,stack-var)
                          id1)))
              ,@(when try-close-open-nodes
                  `((do2 (close-nodes)
                         (let ((to-close (car (sort close-nodes #'>
                                                    :key #'text-start))))
                           (setf (close-delimiter-start to-close) ,index-var
                                 (close-delimiter-end to-close) (+ ,index-var
                                                                   (length
                                                                    (close-delimiter
                                                                     to-close)))
                                 (text-end to-close) ,index-var
                                 (active? to-close) nil)
                           (values ,stack-var
                                   (+ ,index-var (length (close-delimiter
                                                          to-close)))))))))
         ,(cond ((eq try-close-open-nodes t)
                 `(let* ((active (remove-if-not #'active? ,stack-var))
                         (close-nodes (remove-if-not (typedp ',node-type) active)))
                    (if close-nodes
                        (do2 close-nodes)
                        (do1))))
                ((eq try-close-open-nodes nil)
                 `(do1))
                (t
                 `(if ,try-close-open-nodes
                      (let* ((active (remove-if-not #'active?
                                                    ,stack-var))
                             (close-nodes (remove-if-not (typedp ',node-type)
                                                         active)))
                        (if close-nodes
                            (do2 close-nodes)
                            (do1)))
                      (do1))))))))

(defmacro define-simple-close-delimiter-processor
    (node-type (&optional stack-var node-var string-var index-var))
  (let ((stack-var (or stack-var (gensym "STACK-VAR")))
        (node-var (or node-var (gensym "NODE-VAR")))
        (string-var (or string-var (gensym "STRING-VAR")))
        (index-var (or index-var (gensym "INDEX-VAR"))))
    `(defmethod process-close-delimiter (,stack-var (,node-var ,node-type)
                                         ,string-var ,index-var)
       (let* ((active (remove-if-not #'active? ,stack-var))
              (current (car (sort (remove-if-not (typedp ',node-type) active) #'>
                                  :key #'text-start))))
         (if current
             (progn
               (setf (close-delimiter-start current) ,index-var
                     (close-delimiter-end current) (+ ,index-var
                                                      (length (close-delimiter
                                                               current)))
                     (text-end current) ,index-var
                     (active? current) nil)
               (values ,stack-var
                       (+ ,index-var (length (close-delimiter
                                              current)))
                       t))
             (values ,stack-var
                     ,index-var
                     nil))))))

 ; helper functions

(defgeneric check-line-opens-block-and-advance (block line)
  (:method ((block %block-node) line)
    (with-line (line)
      (multiple-value-bind (s e)
          (line-opens-block block line)
        (when s
          (advance-line e)
          t)))))

(defgeneric check-line-satisfies-block-and-advance (block line)
  (:method ((block %block-node) line)
    (with-line (line)
      (multiple-value-bind (s e)
          (line-satisfies-block block line)
        (when s
          (incf *line-position* e)
          t)))))

 ; iterate over open blocks
(defun %iterate-open-blocks (node function)
  (funcall function node)
  (when (and node (ignore-errors (children node)))
    (multiple-value-bind (exactly objects)
        (exactly-n 1 (children node) #'open? :return-matches t :error nil)
      (declare (ignore exactly))
      (setf objects (remove-if #'null objects))
      (if (null objects)
          nil
          (let ((open-child (car objects)))
            (%iterate-open-blocks open-child function))))))

(defun iterate-open-blocks (root function &key (invoke-with-root t))
  (when invoke-with-root
    (funcall function root))
  (let ((toplevel-open
          (loop for el in (children root) when (open? el) collect el)))
    (assert (null (cdr toplevel-open)))
    (when toplevel-open
      (%iterate-open-blocks (car toplevel-open) function))))


(defun add-block-to-tree (root block)
  (flet ((cont (node)
           (when (and (open? node)
                      ;; When node is open and none of its children are open
                      (not (some #'open? (children node))))
             (add-node-as-child node block)
             (return-from add-block-to-tree (values root block)))))
    (%iterate-open-blocks root #'cont)))

(defgeneric add-node-as-child (parent child)
  (:method ((p parent-node) (c child-node))
    ;; (break "P: ~A C: ~A" p c)
    (setf (children p) (append (children p) (list c))
          (parent c) p)))

(defun add-remaining-text-to-innermost-child (root line)
  (with-line (line)
    (let ((child (car (last (open-blocks root)))))
      (push line (node-text child)))))

(defun open-blocks (root-node)
  "Return a list of open blocks"
  (let ((ac nil))
    (iterate-open-blocks root-node
                         (lambda (node) (push node ac))
                         :invoke-with-root t)
    (nreverse ac)))

(defun close-blocks (root blocks-to-close)
  "CLose each block explicitly"
  (loop for block in blocks-to-close
        do (%close-block root block)))

(defun %close-block (root block)
  "explicitly close block"
  (declare (ignore root))
  (when (open? block)
    (setf (open? block) nil
          (node-text block) (nreverse (node-text block)))))

(defun close-block (root block)
  "Close block and all of its children"
  (%close-block root block)
  (map nil
       #'(lambda (b) (close-block root b))
       (ignore-errors (children block))))

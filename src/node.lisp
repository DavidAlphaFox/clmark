
(in-package :clmark)

 ; general class shape&structure
(dc parser-node () (rendered-text))
;;父亲节点
(dc parent-node (parser-node)
    (children)
  (:default-initargs :children nil))
;;儿子节点
(dc child-node (parser-node)
    (parent)
  (:default-initargs :parent nil))
;;行内节点
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
;; 块节点
(dc %block-node (parser-node)
    (open? node-text #| start-index end-index |# root))
;; 普通块节点
(dc block-node (%block-node))
;; 叶子块节点
(dc leaf-block-node (%block-node))

 ; Begin block nodes
(dc document-node (block-node parent-node) (document-block-parsers))

(defmethod check-line-satisfies-block-and-advance ((block document-node) line)
  t)

(defmethod render ((node document-node) as stream)
  (render-children node :stream stream :style as))
;;定义块状类的函数
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

 ; iterate all blocks
;;遍历所有的块，使用的是先序遍历
(defun iterate-blocks (node function)
  (funcall function node)
  (loop for c in (ignore-errors (children node)) ;;遍历自己的子节点
        do (iterate-blocks c function)))

 ; iterate over open blocks 遍历开放的块，也是使用的先序遍历
(defun %iterate-open-blocks (node function)
  (funcall function node)
  (when (and node (ignore-errors (children node))) ;;得到自己的子节点
    (multiple-value-bind (exactly objects)
        (exactly-n 1 (children node) #'open? :return-matches t :error nil)
      (declare (ignore exactly))
      (setf objects (remove-if #'null objects)) ;;删除objects中所有为null的
      (if (null objects)
        nil ;;如果是空的objects，直接返回nil
          (let ((open-child (car objects))) ;;处理打开的子节点
            (%iterate-open-blocks open-child function))))))

(defun iterate-open-blocks (root function &key (invoke-with-root t))
  (when invoke-with-root
    (funcall function root)) ;;当需要调用父节点的时候，就执行此处
  (let ((toplevel-open
          (loop for el in (children root) when (open? el) collect el)))
    (assert (null (cdr toplevel-open)))
    (when toplevel-open
      (%iterate-open-blocks (car toplevel-open) function))))


;;将节点加入到树中
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

(defgeneric postprocess-block-structure (root)
  (:method (root)
    t))

 ; inline nodes

;; Ok so to process inlines we walk through and whenever an inline should OPEN
;; we will push a new node with a start index onto our node stack. We also 

(defgeneric process-text-for-inline-node (parent inline-node text enabled-inlines)
  )

(defgeneric delim-match (node text &key start delimiter)
  (:method ((node inline-node) text &key (start 0) (delimiter :open))
    (string= (ecase delimiter
               (:open (open-delimiter node))
               (:close (close-delimiter node)))
             (subseq text start))))

;; (defun delim-match (delimiter text &key (start 0))
;;   (string= (subseq text start (length delimiter)) delimiter))

(defun delim-match-any (string enabled-inlines open-inlines)
  
  (let ((to-open (loop for inline in enabled-inlines
                       when (delim-match inline string
                                         :delimiter :open)
                         collect inline))
        (to-close (loop for inline in open-inlines
                        when (delim-match inline string
                                          :delimiter :close)
                          collect inline)))
    (if (and (null to-open) (null to-close))
        (values nil nil nil)
        (values t to-open to-close))))


;; When handling multiple delimiters at the same index, we take the one with the
;; longest delimiter match. I.e. if given the string "***" it will open a
;; potential emphasis node and strong node, all at the same point. The strong
;; node has a delimiter of **, so it gets taken first. Then we continue
;; processing without the emphasis node. And lo and behold the first thing we
;; get is a delimiter opening an emphasis node.
(defgeneric process-text-for-inline-nodes (parent-block-node enabled-inlines)
  ;; The idea behind this is to walk through the inline text (with newlines,
  ;; because inline can span a \\n) and register where we encounter
  ;; delimiters. Then when we encounter a specific closing delimiter, we walk
  ;; *back* through the text to find its opener. We can do that by going back
  ;; through the open nodes (in reverse) until we find a matching opening
  ;; delimiter. Then we take the text for that delimiter and if applicable
  ;; re-parse it (because any potential openers are now invalid). When we hit
  ;; the end of the text, we walk back and resolve any 
  ;; (:method (node enabled)
  ;;   (let ((text (format nil "~{~A~^~%~}" (node-text node)))
  ;;         (stack nil)
  ;;         escaped)
  ;;     (loop for i from 0
  ;;           for subtext = (subseq text i)
  ;;           do (cond (escaped
  ;;                     (setf escaped nil))
  ;;                    ((char= #\\ (char subtext 0))
  ;;                     (setf escaped t))
  ;;                    (t
  ;;                     (let ((open (find-opening-delims subtext enabled)))
  ;;                       (cond ((null (cdr open))
  ;;                              (push (make-instance (class-of open) :text-start i)
  ;;                                    stack))
  ;;                             (open
  ;;                              (setf stack
  ;;                                    (append (mapcar (lambda (el)
  ;;                                                      (make-instance
  ;;                                                       (class-of el)
  ;;                                                       :text-start i))
  ;;                                                    open)
  ;;                                            stack))))))))
  ;;     (postprocess-inline-structure
  ;;      ;; POSTPROCESS-INLINE-STRUCTURE should take a list of nodes that are
  ;;      ;; known to be valid (e.g. emphasis and strong nodes directly following
  ;;      ;; each other are properly handled and not just an open-close-open of
  ;;      ;; emphasis nodes) and determine the end index of every node.
  ;;      text
  ;;      (let ((min 0))
  ;;        (do ((nodes (nreverse stack) (cdr nodes))
  ;;             (ac nil))
  ;;            ((null nodes) (nreverse ac))
  ;;          (if (> (text-start node) min)
  ;;              (multiple-value-bind (node next-nodes)
  ;;                  (determine-following-nodes (car nodes) (cdr nodes))
  ;;                ;; DETERMINE-FOLLOWING-NODES should remove any illegal nodes
  ;;                ;; and return the new node list as NEXT-NODES, and return a
  ;;                ;; created node
  ;;                (incf min (length (open-delimiter node)))
  ;;                (push node ac)
  ;;                (setf nodes next-nodes))))))))
  )

;; (defgeneric determine-following-nodes (node following-nodes)
;;   (:method (node rest)
;;     ()))

(defun find-opening-delims (string enabled)
  (loop for node in enabled
        if (delim-match node string :delimiter :open)
          collect node))

(defun find-closing-delims (string stack)
  (loop for node in stack
        if (delim-match node string :delimiter :close)
          collect node))

;; (defun postprocess-inline-structure (nodes text)
;;   (loop for node in nodes
;;         do (setf (text node) (subseq text (text-start node) (text-end node)))))

;; (defgeneric postprocess-inline-structure (node))

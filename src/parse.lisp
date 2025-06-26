
(in-package #:clmark)

 ; begin block helpers
(defvar *line-position* 0
  "Track where in a line we are while parsing")
;;向前吃进amnt个字符
(defun advance-line (amnt)
  (incf *line-position* amnt))
;;直接向前吃进整个行的字符
(defun finish-line (line)
  (incf *line-position* (length line)))

(defun preprocess-string (string)
  (subst #\REPLACEMENT_CHARACTER #\Nul string))
;; 将数据拆解成多行
(defun split-into-lines (string &optional double-escaped)
  ;; (break)
  (cl-ppcre:split (if double-escaped
                      "(\\\\n|\\\\r|\\\\r\\\\n)"
                      "(\\n|\\r|\\r\\n)")
                  string))

 ; begin inline helpers
(defgeneric process-open-delimiter (node-stack node string index)
  (:documentation
   "called with a prototype node and the location where the node should potentially
be opened. This method must create and return a new node to be pushed onto the
node stack."))

(defgeneric process-close-delimiter (stack node string index))

(defgeneric looking-at (node string looking-at &key regex)
  (:method (node string looking-at &key regex)
    (if regex ;;如果有正则表达式
        (cl-ppcre:scan looking-at string)
        (let ((l (min (length looking-at) (length string))))
          (string= (subseq string 0 l) (subseq looking-at 0 l)))))
  (:method (node string (looking-at function) &key regex)
    (declare (ignore regex)) ;;如果looking-at是函数的情况下，直接忽略正则表达式
    (funcall looking-at node string))) ;;直接调用looking-at函数进行相关操作

;; (defun looking-at (string looking-at &key regex)
;;   (if regex
;;       (cl-ppcre:scan looking-at string)
;;       (let ((l (min (length looking-at) (length string))))
;;         (string= (subseq string 0 l) (subseq looking-at 0 l)))))
;; 行内节点，打开分隔符是否出现
(defmethod open-delimiter-is-appropriate-p
    ((node inline-node) string (index integer))
  (let ((s (or (ignore-errors (subseq string index)) ;;在index处切开字串
               (return-from open-delimiter-is-appropriate-p nil))))
    (looking-at node s (open-delimiter node) :regex (regexp node))))
;;行内节点，结束分隔符是否出现
(defmethod close-delimiter-is-appropriate-p
    ((node inline-node) string (index integer))
  (and (active? node)
       (let ((s (subseq string index)))
         (looking-at node s (close-delimiter node) :regex (regexp node)))))

(defun find-current-opening-delimiters (enabled string index)
  (loop for %node in enabled
        for node = (if (symbolp %node)
                       (make-instance %node)
                       %node)
        if (open-delimiter-is-appropriate-p node string index)
          collect node))

(defun find-current-closing-delimiters (enabled string index)
  "This function is used to find the most important closing delimiter when there
are multiple that can be closed from the same text."
  (loop for node in enabled
        if (close-delimiter-is-appropriate-p node string index)
          collect node))

;;处理块状态结构
(defun parse-block-structure (string enabled-blocks &key double-escaped)
  (let ((root (make-instance 'document-node
                             :children nil
                             :open? t
                             ;; :start-index 0
                             ;; :end-index nil
                             :node-text nil
                             :document-block-parsers enabled-blocks))
        (*line-position* 0))
    (setf (root root) root)
    (do ((*line-position* 0 0)
         (lines (split-into-lines string double-escaped) (cdr lines)))
        ((null lines) (close-block root root) root)
      (let ((line (preprocess-string (car lines)))
            (all-current-open (open-blocks root)))
        (multiple-value-bind (remain-open to-close)
                                        ; remain-open is matched
                                        ; blocks, to-close is
                                        ; unmatched
            (loop for block in all-current-open
                  if (check-line-satisfies-block-and-advance block line)
                    collect block into remain-open
                  else
                    collect block into to-close
                  finally (return (values remain-open to-close)))
          ;; (declare (ignore remain-open))
          (let ((innermost (car (last all-current-open))))
            (if (and (typep innermost 'leaf-block-node)
                     (member innermost remain-open))
                (add-remaining-text-to-innermost-child root line)
                (let ((to-open
                        (loop for block in (document-block-parsers root)
                              for inst = (make-instance block :open? t
                                                              :node-text nil
                                                              :parent innermost
                                                              :root root)
                              ;; TODO [12:47 06.01.2025]: this is wrong, we dont
                              ;; want to check all remain-open blocks, only the
                              ;; innermost one
                              when (and (not (typep ;; (car (last remain-open))
                                              (or (car (last reter))
                                                  (car (last remain-open)))
                                              block))
                                        (check-line-opens-block-and-advance inst
                                                                            line))
                                collect inst into reter
                              finally (return reter))))
                  (when to-open
                    (close-blocks root to-close)
                    (loop for block in to-open do (add-block-to-tree root block)))
                  (add-remaining-text-to-innermost-child root line)
                  (unless to-open
                    (close-blocks root to-close))))))))))

(defun process-inline-text (string enabled)
  "Process the inline text (latest)"
  (let ((stack nil))
    (do ((strl (length string))
         (i 0 (1+ i)))
        ((= i strl) (reverse stack))
      (let ((closers (find-current-closing-delimiters stack string i)))
        (when closers
          (do ((closers (sort closers #'>
                              :key (lambda (node)
                                     (length (close-delimiter node))))
                        (cdr closers)))
              ((null closers))
            (let ((closer (car closers)))
              ;; TODO [11:08 16.01.2025]: This isnt doing what we want. It
              ;; doesnt process all closers propperly
              (multiple-value-bind (new-stack index just-closed?)
                  (process-close-delimiter stack closer string i)
                (if just-closed?
                    (progn
                      (setf i index
                            stack new-stack)
                      (setf closers
                            (cons nil
                                  (sort (find-current-closing-delimiters stack
                                                                         string
                                                                         i)
                                        #'>
                                        :key (lambda (node)
                                               (length (close-delimiter node)))))))
                    (setf closers
                          (sort (find-current-closing-delimiters stack
                                                                 string
                                                                 i)
                                #'>
                                :key (lambda (node)
                                       (length (close-delimiter node)))))))))))
      (let ((openers (find-current-opening-delimiters enabled string i)))
        (when openers
          (do ((openers (sort openers #'>
                              :key (lambda (node)
                                     (length (open-delimiter node))))
                        (cdr openers)))
              ((null openers))
            (let ((opener (car openers)))
              (multiple-value-bind (new-stack index)
                  (process-open-delimiter stack opener string i)
                (setf stack new-stack
                      i index)
                (setf openers
                      (cons opener
                            (sort (find-current-opening-delimiters enabled
                                                                   string
                                                                   i)
                                  #'>
                                  :key (lambda (node)
                                         (length (open-delimiter node))))))))))))))

(defgeneric parse-block-inlines (block enabled-inlines)
  (:documentation "Call PROCESS-INLINE-TEXT on all text for BLOCK.")
  (:method ((block %block-node) inlines)
    (process-inline-text (setf (rendered-text block)
                               (format nil "~{~A~%~}" (node-text block)))
                         inlines)))

(defun parse-string (string blocks inlines &key double-escaped (render-as :html))
  (let ((tree (parse-block-structure string
                                     blocks
                                     :double-escaped double-escaped)))
    (flet ((inlines (block)
             ;; TODO [10:13 05.04.2025]: function should be called
             ;; parse-block-inlines...
             (render-block-inlines block
                                   (parse-block-inlines block inlines)
                                   render-as)))
      (iterate-blocks tree #'inlines)
      tree)))

(defun parse-and-render-markdown
    (string render-as &optional
                        (enabled-blocks
                         ;; (gethash to *enabled-blocks-by-render-type*)
                         )
                        (enabled-inlines
                         ;; (gethash to *enabled-inlines-by-render-type*)
                         ))
  (let ((tree (parse-block-structure string enabled-blocks :double-escaped t)))
    (flet ((inlines (block)
             (render-block-inlines block
                                   (parse-block-inlines block enabled-inlines)
                                   render-as)))
      (iterate-blocks tree #'inlines)
      tree)))

;; TODO [09:56 12.01.2025]: For inline nodes, when we are closing them, we will
;; potentially have multiple matches. So we want to choose the one with the
;; shortest length.

;; TODO [09:56 12.01.2025]: Regarding the above, Why?

;;  ; begin regex inlines
;; (definline (image-node "\\[.*\\](\\(\\S*\\s*\\)|\\(\\S*\\s*\".*\"\\))" t)
;;            (inline-node)
;;            ()
;;   (:default-initargs :close-immediately t))

;; (defmethod process-open-delimiter ((node image-node) string index)
;;   ())

;; 

;; (definline (inline-link-node "[" "]")
;;            (inline-node)
;;            (link-text link-uri link-title)
;;   (:default-initargs :close-immediately t))

;; ;; the inline link node regex
;; ;; "(\\[(.*)\\]\\((\\S*)\\s*\\)|\\[(.*)\\]\\((\\S*)\\s*\"(.*)\"\\))"
;; (defmethod process-open-delimiter ((node inline-link-node) string index)
;;   ;; This method looks ahead to see if we are looking at an inline link. 
;;   (multiple-value-bind (full-match matches)
;;       (cl-ppcre:scan-to-strings
;;        ;; this regex is two in one, matchine inline links with or without a title. 
;;        "((\\[.*\\])\\((\\S*)\\s*\\)|(\\[.*\\])\\((\\S*)\\s*\"(.*)\"\\))"
;;        string :start index)
;;     (when full-match
;;       (with-array-elements ((total stext slink ltext llink ltitle) matches)
;;         (declare (ignore total))
;;         (let ((text (or stext ltext))
;;               (link (or slink llink))
;;               (title (or ltitle "")))
;;           (values (make-instance 'link-node :link-title title
;;                                             :link-text text
;;                                             :link-uri link
;;                                             :active? nil)
;;                   (length full-match)))))))

;;  ; begin inline nodes
;; (definline (image-node "[" "]") (inline-node) ()
;;   ;; Approach: open the node on a [ but close on a ) and then verify the node by
;;   ;; parsing the text in the middle?
;;   (:default-initargs :close-immediately t))

;; ;; INFO [10:05 12.01.2025]: Because of how we parse inlines using
;; ;; FOR-ESCAPED-CHARS, we need to handle image nodes or nodes with multiple
;; ;; delimiters a little differently. Image nodes are an example of this type of
;; ;; node, because they have a shape of [text](link "title"). One idea is to
;; ;; collect individual delimited elements, and join them after the fact. E.g. we
;; ;; parse through and get POTENTIAL-IMAGE-TEXT-NODE (whose start and end are the
;; ;; open/close brackets), and a POTENTIAL-IMAGE-LINKE-AND-TITLE-NODE (whose start
;; ;; and end are the open/close parens) in succession. Then once weve parsed out
;; ;; the string into all of our potential nodes, we go back through and call one
;; ;; of three generic functions: MERGE-3-NODES and MERGE-2-NODES.

;; (defmethod process-closing-delimiter (node-stack (node image-node) string)
;;   ;; node-stack is in-order, meaning the first element is the earliest occuring
;;   ;; node. We want to look from the end of the stack and parse backwards. This
;;   ;; method should return the modified node-stack and the new index to parse
;;   ;; from.
;;   (let ((found (find-if (lambda (node)
;;                           (and (typep node 'image-node)
;;                                (or (char= #\[ (char string (text-start node)))
;;                                    (char= #\! (char string (text-start node))))))
;;                         node-stack
;;                         :from-end t)))
;;     (if found
;;         (if (active? node)
;;             (let ((lookahead (find))))
;;             (remove node node-stack))
;;         node-stack)))

;; (definline (emphasis-node "*" "*")
;;            (inline-node)
;;            ()
;;   (:default-initargs :close-immediately nil))

;; ;; (defmethod process-closing-delimiter (node-stack (node emphasis-node)))

;; (definline (strong-node "**" "**") (inline-node) ()
;;   (:default-initargs :close-immediately nil))

;; ;; (defmethod process-closing-delimiter (node-stack (node strong-node)))


;; 
;; ;; (defun process-emphasis (stack string)
;; ;;   (let ((current-position stack))
;; ;;     ))

;; ;; (flet ((processor (stack string index)
;; ;;          ())
;; ;;        )
;; ;;   ;; (defmethod process-closing-delimiter ((node emphasis-node))
;; ;;   ;;   #'processor)
;; ;;   )

;; ;; (defun process-block-node-content (node enabled-inlines)
;; ;;   (let ((stack nil))
;; ;;     ()))

;; ;; (defun process-node-text (node enabled-inlines)
;; ;;   (let ((stack nil))
;; ;;     (labels ((generate-inlines (opens i)
;; ;;                (setf stack (append (nreverse
;; ;;                                     (mapcar (lambda (el)
;; ;;                                               (make-instance (class-of el)
;; ;;                                                              :text-start i))
;; ;;                                             opens))
;; ;;                                    stack)))
;; ;;              (on-escape (char string index)
;; ;;                (declare (ignore char))
;; ;;                (let* ((openers ())))
;; ;;                (let* ((opening-delims (find-opening-delims (subseq string index)
;; ;;                                                            enabled-inlines))
;; ;;                       (closing-delims (find-closing-delims (subseq string index)
;; ;;                                                            stack))
;; ;;                       (closing-delims (remove-if-not #'close-immediately
;; ;;                                                      closing-delims)))
;; ;;                  ;; Now we have opening delimiters, and a set of closing
;; ;;                  ;; delimiters that should be closed immediately. Now we call
;; ;;                  ;; every closing delimiters PROCESS-CLOSING-DELIMITER function
;; ;;                  ;; to process the stack and string@index.
;; ;;                  (loop for delim in closing-delims
;; ;;                        for new-stack = (funcall (process-closing-delimiter delim)
;; ;;                                                 stack string index)
;; ;;                        do (setf stack new-stack))
;; ;;                  (when opening-delims
;; ;;                    (generate-inlines opening-delims index)))))
;; ;;       (for-escaped-chars (format nil "~{~A~^~%~}" (node-text node))
;; ;;                          #'on-escape))))

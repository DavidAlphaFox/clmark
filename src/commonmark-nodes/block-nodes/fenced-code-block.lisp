
(in-package :clmark)

(defblock (fenced-code-block
           "^\\s{0,3}(~~~|```)\\s*"
           +no-advance-regex+
           "^\\s{0,3}(~~~|```)\\s*$")
          (leaf-block-node child-node)
          (info-string style))

(defmethod test-print-block ((node fenced-code-block) stream)
  (format stream
          "~&FENCED-CODE-BLOCK (info: ~S) - Content:~%~{~A~^~%~}~&END_FENCED_CODE_BLOCK"
          (info-string node)
          (remove-if (lambda (el)
                       (or (null el)
                           (string= el "")))
                     (reverse (node-text node)))))

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


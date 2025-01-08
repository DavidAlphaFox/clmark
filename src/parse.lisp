
(in-package #:clmark)

(defvar *line-position* 0
  "Track where in a line we are while parsing")

(defun advance-line (amnt)
  (incf *line-position* amnt))

(defun finish-line (line)
  (incf *line-position* (length line)))

(defun preprocess-string (string)
  (subst #\REPLACEMENT_CHARACTER #\Nul string))

(defun split-into-lines (string &optional double-escaped)
  ;; (break)
  (cl-ppcre:split (if double-escaped
                      "(\\\\n|\\\\r|\\\\r\\\\n)"
                      "(\\n|\\r|\\r\\n)")
                  string))

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

(defun parse-string (string enabled-blocks &key double-escaped)
  (parse-block-structure string enabled-blocks :double-escaped double-escaped))

(defun parse-inline-string (string enabled-inline)
  ())

(defun parse-inline-structure (root)
  )

 ;;; Begin node specific parsers

 ;; leaf block

 ; atx heading

 ; fenced code blocks



 ; Paragraphs
 ; block quote

 ; spoiler




(in-package #:clmark)

(defun link-open-p (node string)
  (when (char= #\[ (char string 0))
    (let ((text nil)
          (link nil)
          (end nil))
      (flet ((getc (i) (char string i)))
        (loop with opencounter = 1
              for i from 1 to (1- (length string))
              for c = (getc i)
              if (char= #\\ c)
                do (incf i)
              else if (char= #\[ c)
                     do (incf opencounter)
              else if (char= #\] c)
                     do (decf opencounter) 
                        (when (zerop opencounter)         ;this duplicates in
                          (setf text (subseq string 1 i)  ;images code this 1 is
                                end i)                    ;the only difference
                          (loop-finish)))
        (when (and end (char= #\( (char string (1+ end))))
          (let* ((s (subseq string (+ 2 end)))
                 (ps (position #\space s))
                 (p (position #\) s)))
            (setf link (subseq s 0 (if (and ps p)
                                       (min ps p)
                                       (progn (assert p)
                                              p))))
            (setf (link-text node) text
                  (link-link node) link
                  (close-delimiter node) ")"
                  (close-delimiter-start node) (+ end p)
                  (close-delimiter-end node) (+ 1 end p)
                  ;; set the open delimiter to the full link, to ensure it gets
                  ;; eaten during rendering (weve saved all relevant
                  ;; information)
                  (open-delimiter node) (make-string (+ 1 ; [
                                                        p
                                                        end)))
            ))))))

;; TODO [14:25 05.04.2025]: we need to fix this so that the link text gets
;; parsed for inline markup as well... we need to use a subset of what was used
;; to call the code, i.e. if a user invokes the parser toplevel with the enabled
;; inlines being N, then the link-text of the link object needs to be parsed and
;; rendered with (REMOVE LINK-NODE N).

(defclass link-mixin ()
  ((text :accessor link-text)
   (link :accessor link-link)))

(defun read-open-link-delim (node string)
  "return true and set the delimiter of node."
  ())

(defun read-link-delim-close (node string))

(definline (link-node #'link-open-p "")
           (link-mixin inline-node)
           ())

(defmethod process-open-delimiter (stack (node link-node) string index)
  (setf (active? node) nil
        (open-delimiter-start node) index
        (open-delimiter-end node) (1- (+ index (length (open-delimiter node))))
        ;; These are duplicated for close delimiter because we need a close
        ;; delimiter for math but images/links are kinda atomic.
        (close-delimiter-start node) (+ index (length (open-delimiter node)))
        (close-delimiter-end node) (+ index 1 (length (open-delimiter node))))
  (values (cons node stack)
          (+ index (length (open-delimiter node)))))


(defmethod render-node-open-delimiter ((node link-node) (as (eql :html)) stream)
  (format stream "<a href=~S>~S</a>" (link-link node) (link-text node)))

(defmethod render-node-close-delimiter ((node link-node) (as (eql :html)) stream)
  nil)

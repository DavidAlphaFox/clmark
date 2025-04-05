
(in-package :clmark)

(defun image-open-p (node string)
  (when (and (char= #\! (char string 0))
             (char= #\[ (char string 1)))
    (let ((text nil)
          (link nil)
          (end nil))
      (flet ((getc (i) (char string i)))
        (loop with opencounter = 1
              for i from 2 to (1- (length string))
              for c = (getc i)
              if (char= #\\ c)
                do (incf i)
              else if (char= #\[ c)
                     do (incf opencounter)
              else if (char= #\] c)
                     do (decf opencounter)
                        (when (zerop opencounter)
                          (setf text (subseq string 2 i)
                                end i)
                          (loop-finish)))
        (when (and end
                   (char= #\( (char string (1+ end))))
          (let* ((s (subseq string (1+ (1+ end))))
                 (ps (position #\space s))
                 (p (position #\) s)))
            (setf link (subseq s 0 (if (and ps p)
                                       (min ps p)
                                       (progn (assert p)
                                              p))))
            (setf (image-text node) text
                  (image-link node) link
                  (close-delimiter node) ")"
                  (close-delimiter-start node) (+ end p)
                  (close-delimiter-end node) (+ 1 end p)
                  (open-delimiter node) (make-string (+ 2 ; ![
                                                        p
                                                        end))
                  )
            t))))))

(definline (image-node #'image-open-p "")
           (inline-node)
           (image-text image-link)
  (:default-initargs :close-immediately t))

(defmethod process-open-delimiter (stack (node image-node) string index)
  (setf (active? node) nil
        (open-delimiter-start node) index
        (open-delimiter-end node) (1- (+ index (length (open-delimiter node))))
        ;; These are duplicated for close delimiter because we need a close
        ;; delimiter for math but images are kinda atomic.
        (close-delimiter-start node) (+ index (length (open-delimiter node)))
        (close-delimiter-end node) (+ index (length (open-delimiter node))))
  (values (cons node stack)
          (+ index (length (open-delimiter node)))))

(defmethod process-close-delimiter (stack (node image-node) string index)
  (setf (active? node) nil))

(defmethod render-node-open-delimiter ((node image-node) (as (eql :html)) stream)
  (format stream "<img src=~S alt=~S>" (image-link node) (image-text node)))

(defmethod render-node-close-delimiter ((node image-node) (as (eql :html)) stream)
  nil)

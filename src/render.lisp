
(in-package #:clmark)

(defvar *current-rendering-style* nil)
(defvar *current-rendering-stream* nil)

(defgeneric render (thing style stream)
  (:method :around (o a s)
    (let ((*current-rendering-style* a)
          (*current-rendering-stream* s))
      (call-next-method)))
  (:method ((text string) as stream)
    (write-string text stream)))

(defun render-children (object &key (stream *current-rendering-stream*)
                                 (style *current-rendering-style*))
  (loop for child in (children object)
        do (render child style stream)))

(defun render-text (object &key (stream *current-rendering-stream*)
                             (style *current-rendering-style*))
  (if (rendered-text object)
      (write-string (rendered-text object) stream)
      (loop for text in (node-text object)
            do (render text style stream))))

(defmacro defrender (for-class (style stream-argument) &body body)
  (let ((as (gensym)))
    `(defmethod render ((,for-class ,for-class) (,as (eql ,style)) ,stream-argument)
       (declare (ignore ,as))
       ,@body)))

(defmacro with-tags ((stream (open &rest open-args) (close &rest close-args))
                     &body body)
  (let ((s (gensym)))
    `(let ((,s ,stream))
       ,(if open-args
            `(apply #'format ,s ,open ,@open-args)
            `(write-string ,open ,s))
       (unwind-protect (progn ,@body)
         ,(if close-args
              `(apply #'format ,s ,close ,@close-args)
              `(write-string ,close ,s))))))

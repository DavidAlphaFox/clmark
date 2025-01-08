
(in-package #:clmark)

(defvar *current-rendering-style* nil)
(defvar *current-rendering-stream* nil)

(defgeneric render (thing style stream)
  (:method :around (o a s)
    (let ((*current-rendering-style* a)
          (*current-rendering-stream* s))
      (call-next-method))))

(defun render-children (object &key (stream *current-rendering-stream*)
                                 (style *current-rendering-style*))
  (loop for child in (children object)
        do (render child style stream)))

(defun render-text (object &key (stream *current-rendering-stream*))
  (format stream "~{~A~^~%~}" (node-text object)))

(defmacro defrender (for-class (style stream-argument) &body body)
  (let ((as (gensym)))
    `(defmethod render ((,for-class ,for-class) (,as (eql ,style)) ,stream-argument)
       (declare (ignore ,as))
       ,@body)))

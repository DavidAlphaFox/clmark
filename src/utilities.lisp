
(in-package #:clmark)

(defmacro breaklet ((var &optional string args) &body forms)
  `(let ((,var (progn ,@forms)))
     (break ,(or string "BREAKLET GOT: ~S") ,@(or args (list var)))
     ,var))

(defvar +null-regex+ "$^"
  "The null regex will never match a string, because it is looking for the end of a
line/string followed by the beginning of a line/string.")

(defvar +no-advance-regex+ "^"
  "This just checks for beginning of string. We always work with substrings so this
will always return a start and end of 0 and 0.

This is intended to be used to prevent advancing *line-position* while checking
for an open/satisfies/close condition for a line.")
;;一行内部分数据的操作宏
(defmacro with-line ((line &optional (offset '*line-position*)) &body body)
  (let ((l (gensym)))
    `(let* ((,l ,line)
            (,line (subseq ,l ,offset))) ;;使用局部变量遮蔽
       ,@body)))
;;构建class的宏，自动为每个slot添加initarg和accessor
(defmacro dc (name &optional supers slots &body options)
  (let ((slots (mapcar (lambda (slot)
                         (if (atom slot) ;;只有是原子的时候才会对slot进行操作
                             `(,slot :initarg ,(intern (string slot)
                                                       (find-package :keyword))
                                     :accessor ,slot)
                             slot))
                       slots)))
    `(defclass ,name ,supers ,slots
       ,@options)))

(defun exactly-n (count sequence predicate
                  &key return-matches (key #'identity) error)
  (declare (type (integer 0) count)
           (type (or (function (integer) (values t))
                     symbol)
                 predicate))
  (let ((i 0)
        (reter nil))
    (do ((els sequence (cdr els)))
        ((or (null els)
             (> i count))
         (when (and error
                    (not (= i count)))
           (cerror "Ignore" "Expected ~D matches, got ~D" count i))
         (values (= i count)
                 (nreverse reter)))
      (when (funcall predicate (funcall key (car els)))
        (when return-matches (push (car els) reter))
        (incf i)))))

(defmacro with-array-elements ((varlist array &key bind-nil-on-error) &body body)
  (let ((a (gensym)))
    `(let* ((,a ,array)
            ,@(loop for i from 0
                    for var in varlist
                    if bind-nil-on-error
                      collect `(,var (ignore-errors (aref ,a ,i)))
                    else
                      collect `(,var (aref ,a ,i))))
       ,@body)))

(defun typedp (thing)
  (lambda (el)
    (typep el thing)))

(defmacro with-mvb-split ((var1 var2) arg &body body)
  (let ((a (gensym)))
    `(let ((,a ,arg))
       (multiple-value-bind (,var1 ,var2)
           (if (atom ,a) (list ,a) (values (car ,a) (cdr ,a)))
         ,@body))))

(defmacro with-tags ((stream open-and-args close-and-args) &body body)
  (with-mvb-split (open oargs) open-and-args
    (with-mvb-split (close cargs) close-and-args
      (let ((s (gensym)))
        `(let ((,s ,stream))
           (apply #'format ,s ,open (list ,@oargs))
           (unwind-protect (progn ,@body)
             (apply #'format ,s ,close (list ,@cargs))))))))

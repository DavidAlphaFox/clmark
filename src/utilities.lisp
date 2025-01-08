
(in-package #:clmark)

(defvar +null-regex+ "$^"
  "The null regex will never match a string, because it is looking for the end of a
line/string followed by the beginning of a line/string.")

(defvar +no-advance-regex+ "^"
  "This just checks for beginning of string. We always work with substrings so this
will always return a start and end of 0 and 0.

This is intended to be used to prevent advancing *line-position* while checking
for an open/satisfies/close condition for a line.")

(defmacro with-line ((line &optional (offset '*line-position*)) &body body)
  (let ((l (gensym)))
    `(let* ((,l ,line)
            (,line (subseq ,l ,offset)))
       ,@body)))

(defmacro dc (name &optional supers slots &body options)
  (let ((slots (mapcar (lambda (slot)
                         (if (atom slot)
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

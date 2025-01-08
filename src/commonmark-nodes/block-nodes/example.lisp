
(in-package :clmark)

(defblock (myblock "^aaa" "^a{1,3}" "^$")
          (leaf-block-node child-node)
          (someslot))

(defmethod render-as ((node myblock) (as (eql :html)) stream)
  ...)

(defmethod check-line-opens-block-and-advance ((block myblock) line)
  (with-line (line)
    (multiple-value-bind (start end)
        (line-opens-block block line)
      (when start
        (setf (someslot block) (subseq line e))
        (finish-line line)
        t))))

(defmethod check-line-satisfies-block-and-advance ((block myblock) line)
  #|
Do the same thing as in CHECK-LINE-OPENS-BLOCK-AND-ADVANCE, but return T if the
  line satisfies the conditions of MYBLOCK to remain open. If MYBLOCK should be
  closed, return nil
  |#
  ...)



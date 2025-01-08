# CLMARK

An extensible markdown parser implemented in Common Lisp

## Overview

CLMARK implements a line-by-line markdown parser with extensible node types. The
general algorithm used is a variant of the one presented in the commonmark
specification (as of version 31.2).

## Extending the Block Structure

A simple block is somewhat easy to implement. As an example lets look at the
spoiler block implementation: 

```lisp
(defblock (spoiler "^::: spoiler\\s+" "^.*$" "^:::\\s*$")
          (block-node parent-node child-node)
          (spoiler-header))

(defmethod check-line-satisfies-block-and-advance ((block spoiler) line)
  (with-line (line)
    (multiple-value-bind (s e)
        (line-closes-block block line)
      (if s
          (progn
            (advance-line e)
            nil)
          t))))

(defmethod check-line-opens-block-and-advance ((block spoiler) line)
  (with-line (line)
    (multiple-value-bind (s e)
        (line-opens-block block line)
      (when s
        (setf (spoiler-header block) (subseq line e))
        (finish-line line)
        t))))
```

The `DEFBLOCK` form takes the name of the class to define (`SPOILER`), as well
as regexes to open the block, satisfy the block such that it remains open, and
close the block. It also takes superclasses and slots. The spoiler block is a
block node (but not a leaf block node) and may be a parent and child. It also
carries a spoiler header with it. 

We have two methods we can implement that determine whether to open a spoiler
block and whether to keep the spoiler block open. 

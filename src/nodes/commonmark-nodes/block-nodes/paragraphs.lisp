
(in-package :clmark)

;; TODO [12:04 07.01.2025]: We need to fix either paragraphs or blockquotes or fenced code blocks. Because the text:
#|
> blockquote
> ```lisp
> (do thing)
> ```
> more blockquote text
>
> more text
|#
;; ends up dropping the text "more blockquote text"

(defblock (paragraph "^\\s*\\S+.*" "^\\s*\\S+.*" "^\\s*$")
          (block-node parent-node child-node)
          ())

(defmethod check-line-satisfies-block-and-advance ((block paragraph) line)
  (with-line (line)
    (if (every #'sb-unicode:whitespace-p line)
        nil
        t)))

(defmethod check-line-opens-block-and-advance ((block paragraph) line)
  (with-line (line)
    (if (every #'sb-unicode:whitespace-p line)
        nil
        t)))

(defmethod render ((node paragraph) (as (eql :html)) stream)
  (with-tags (stream ("<p>") ("</p>"))
    (render-text node :stream stream :style as)
    (render-children node :stream stream :style as)))


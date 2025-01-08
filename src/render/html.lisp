(in-package #:clmark)

(defmacro defhtml ((block stream) &body body)
  `(defrender ,block (:html ,stream) ,@body))

(defun subrender (thing)
  (render-text thing)
  (render-children thing))

(defhtml (document-node stream)
  (render-children document-node))

(defhtml (atx-heading stream)
  (format stream "<h~D>~A</h~D>"
          (atx-header-level atx-heading)
          (apply #'concatenate 'string (node-text atx-heading))
          (atx-header-level atx-heading)))

(defhtml (block-quote stream)
  (format stream "<blockquote>")
  (unwind-protect (subrender block-quote)
    (format stream "</blockquote>")))

(defhtml (list-container-block stream)
  (format stream "<ul>")
  (unwind-protect (render-children list-container-block)
    (format stream "</ul>")))

(defhtml (bullet-list-block stream)
  (format stream "<li>")
  (unwind-protect (render-children bullet-list-block)
    (format stream "</li>")))

(defhtml (fenced-code-block stream)
  (format stream "<pre><code~@[ class=\"language-~A\"~]>"
          (info-string fenced-code-block))
  (format stream "~{~A~^~%~}" (node-text fenced-code-block))
  (format stream "</code></pre>"))

;; TODO [08:30 08.01.2025]: INDENTED-CODE-BLOCK

(defhtml (paragraph stream)
  (format stream "<p>")
  (unwind-protect (progn (render-text paragraph)
                         (render-children paragraph))
    (format stream "</p>")))

(defhtml (spoiler stream)
  (format stream "<details><summary>~A</summary>"
          (spoiler-header spoiler))
  (unwind-protect (progn (render-text spoiler)
                         (render-children spoiler))
    (format stream "</details>")))

(defhtml (thematic-break stream)
  (format stream "<hr/>"))

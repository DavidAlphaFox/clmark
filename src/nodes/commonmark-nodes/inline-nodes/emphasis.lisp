
(in-package #:clmark)

(defclass emphasis-and-strong-mixin () ())

(definline (strong "**" "**")
           (emphasis-and-strong-mixin inline-node)
           ())

(definline (emphasis "*" "*")
           (emphasis-and-strong-mixin inline-node)
           ()
  (:default-initargs :close-immediately nil))

(define-simple-close-delimiter-processor strong ())
(define-simple-open-delimiter-processor strong () :try-close-open-nodes t)

(defmethod render-node-open-delimiter ((node strong) (as (eql :html)) stream)
  (format stream "<strong>"))

(defmethod render-node-close-delimiter ((node strong) (as (eql :html)) stream)
  (format stream "</strong>"))

(define-simple-close-delimiter-processor emphasis ())
(define-simple-open-delimiter-processor emphasis () :try-close-open-nodes t)

(defmethod render-node-open-delimiter ((node emphasis) (as (eql :html)) stream)
  (format stream "<emph>"))

(defmethod render-node-close-delimiter ((node emphasis) (as (eql :html)) stream)
  (format stream "</emph>"))

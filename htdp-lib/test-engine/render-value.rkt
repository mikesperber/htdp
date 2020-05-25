#lang racket/base

(provide render-value-parameter
         render-value)

(define render-value-parameter (make-parameter
                                (lambda (v)
                                  (format "~V" v))))
(define (render-value v)
  ((render-value-parameter) v))

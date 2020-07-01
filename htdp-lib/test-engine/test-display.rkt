#lang racket/base
(provide display-test-results-parameter
         display-test-results!)

(require test-engine/markup
         test-engine/test-markup)
(define display-test-results-parameter
  (make-parameter
   (lambda (test-object)
     (display-markup (test-object->markup test-object)))))

(define (display-test-results! test-object)
  ((display-test-results-parameter) test-object))

#lang racket/base

;; Manage the recording of tests and their failures

(provide error-handler test-execute test-silence
	 (struct-out test-object)
	 test-object-tests-count test-object-checks-count test-object-failed-checks-count
	 initialize-test-object!
	 
	 add-test! add-check! add-failed-check! add-called-wish! add-signature-violation!
	 run-tests!
	 test-display-results!)
(require racket/class
         test-engine/render-value
         "test-info.rkt")

;; Terminology:
;; - a test is a piece of code run for testing
;; - a check is a single assertion within that code

(struct check
	(test
	 expect
	 range
	 src
	 kind)
	#:transparent)

(struct test-object
	(tests ; reverse list of thunks
	 checks ; reverse list of check structs
	 failed-checks ; reverse list of failed-check structs
	 wishes ; reverse list of FIXME
	 called-wishes
	 signature-violations ; reverse list of FIXME structs
	 )
	#:mutable #:transparent)

(define (empty-test-object)
  (test-object '() '() '() '() '() '()))

(define (test-object-tests-count test-object)
  (length (test-object-tests test-object)))

(define (test-object-checks-count test-object)
  (length (test-object-checks test-object)))

(define (test-object-failed-checks-count test-object)
  (length (test-object-failed-checks test-object)))

(define (test-object-wishes-count test-object)
  (length (test-object-wishes test-object)))

(define (initialize-test-object!)
  (display "initialize-test-object!" (current-error-port))
  (newline (current-error-port))
  (namespace-set-variable-value! 'test~object (empty-test-object)))

(define (current-test-object)
  (namespace-variable-value 'test~object))

(define (add-check! test expect range src kind)
  (write (list 'add-check! test expect range src kind) (current-error-port))
  (newline (current-error-port))
  (let ((test-object (current-test-object)))
    (set-test-object-checks! test-object (cons (check test expect range src kind)
					       (test-object-checks test-object)))))
					 

(define (add-wish! wish)
  (write (list 'add-wish! wish) (current-error-port))
  (newline (current-error-port))
  (let ((test-object (current-test-object)))
    (set-test-object-wishes! test-object
			     (cons wish
				   (test-object-wishes test-object)))))

(define (add-test! thunk)
  (write (list 'add-test! thunk) (current-error-port))
  (newline (current-error-port))
  (let ((test-object (current-test-object)))
    (set-test-object-tests! test-object
			    (cons thunk (test-object-tests test-object)))))

(define (run-tests!)
  (write (list 'run-tests! (test-object-tests (current-test-object))) (current-error-port))
  (newline (current-error-port))
  (for-each (lambda (thunk)
	      (thunk))
	    (test-object-tests (current-test-object))))

(define (add-failed-check! failed-check)
  (write (list 'add-failed-check! failed-check) (current-error-port))
  (newline (current-error-port))
  (let ((test-object (current-test-object)))
    (set-test-object-failed-checks! test-object
				    (cons failed-check (test-object-failed-checks test-object)))))

(define (add-called-wish! wish)
  (let ((test-object (current-test-object)))
    (set-test-object-called-wishes! test-object
				    (cons wish (test-object-called-wishes test-object)))))

(define (add-signature-violation! signature-violation)
  (write (list 'add-signature-violation!! signature-violation) (current-error-port))
  (newline (current-error-port))
  (let ((test-object (current-test-object)))
    (set-test-object-signature-violations! test-object
					   (cons signature-violation
						 (test-object-signature-violations test-object)))))

(define test-execute (make-parameter #t))
(define error-handler (make-parameter (lambda (e) (e))))
(define test-silence (make-parameter #f))

;; FIXME: everything below needs to go somewhere else

;; careful here: When moving this, must not introduce a dependency from here to mred

(define (test-display-results! display-rep display-event-space test-display%)
  ;; FIXME: This needs to default test-display-textual%
  (let ((test-display (make-object test-display% (current-test-object)))
        ;; need to transport this binding to the UI thread
        (render-value-proc (render-value-parameter)))
    (cond
     [(and display-rep display-event-space)
      (parameterize ([(dynamic-require 'mred/mred 'current-eventspace) display-event-space])
	((dynamic-require 'mred/mred 'queue-callback)
	 (lambda ()
           (parameterize ([render-value-parameter render-value-proc])
             (send display-rep display-test-results test-display)))))]
     [display-event-space 
      (parameterize ([(dynamic-require 'mred/mred 'current-eventspace) display-event-space])
	((dynamic-require 'mred/mred 'queue-callback)
         (lambda ()
           (parameterize ([render-value-parameter render-value-proc])
             (send test-display display-results)))))]
     [else (send test-display display-results)])))




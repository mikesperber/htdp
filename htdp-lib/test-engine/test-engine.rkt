#lang racket/base

;; Manage the recording of tests and their failures

(provide test-display-textual% error-handler test-execute test-silence
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

(define test-display-textual%
  (class* object% ()

    (init-field (current-rep #f))

    (define test-info #f)
    (define/pubment (install-info t)
      (set! test-info t)
      (inner (void) install-info t))

    (define/public (display-results)
      (insert-test-results test-info))

    (define/pubment (insert-test-results test-info)
      (let* ([style (send test-info test-style)]
             [total-tests (send test-info tests-run)]
             [failed-tests (send test-info tests-failed)]
             [total-checks (send test-info checks-run)]
             [failed-checks (send test-info checks-failed)]
             [test-outcomes
              (lambda (zero-message)
                (printf "~a"
                        (cond [(zero? total-tests) zero-message]
                              [(= 1 total-tests) "Ran 1 test.\n"]
                              [else (format "Ran ~a tests.\n" total-tests)]))
                (when (> total-tests 0)
                  (printf "~a"
                          (cond
                            [(and (zero? failed-tests) (= 1 total-tests))
                             "Test passed!\n\n"]
                            [(zero? failed-tests) "All tests passed!\n\n"]
                            [(= failed-tests total-tests) "0 tests passed.\n"]
                            [else "~a of the ~a tests failed.\n\n"]))))]
             [check-outcomes
              (lambda (zero-message)
                (printf "~a"
                        (cond
                          [(zero? total-checks) zero-message]
                          [(= 1 total-checks) "Ran 1 check.\n"]
                          [else (format "Ran ~a checks.\n" total-checks)]))
                (when (> total-checks 0)
                  (printf "~a"
                          (cond
                            [(and (zero? failed-checks) (= 1 total-checks))
                             "Check passed!\n\n"]
                            [(zero? failed-checks) "All checks passed!\n\n"]
                            [(= failed-checks total-checks) "0 checks passed.\n"]
                            [else (format "~a of the ~a checks failed.\n\n"
                                          failed-checks total-checks)]))))])
        (case style
          [(test-require)
           (test-outcomes "This program must be tested!\n")
           (check-outcomes "This program is unchecked!\n")]
          [(check-require)
           (check-outcomes "This program is unchecked!\n")]
          [(test-basic)
           (test-outcomes "")
           (check-outcomes "")]
          [else (check-outcomes "")])

        (unless (and (zero? total-checks) (zero? total-tests))
          (inner (display-check-failures (send test-info failed-checks)
                                         test-info)
                 insert-test-results test-info))))

    (define/public (display-check-failures checks test-info)
      (for ([failed-check (reverse checks)])
        (printf "~a" "\t")
        (make-link (failed-check-reason failed-check)
                   (check-fail-src (failed-check-reason failed-check)))
        (printf "~a" "\n")))

    (define/public (next-line) (printf "~a" "\n\t"))

    ;; make-link: (listof (U check-fail (U string snip%))) src -> void
    (define (make-link reason dest)
      (print-reason reason)
      (printf "~a" (format-src dest)))

    (define (format-src src)
      (let ([src-file car]
            [src-line cadr]
            [src-col caddr])
        (string-append
         (cond [(symbol? (src-file src)) " At "]
               [(path? (src-file src))
                (string-append " In " (path->string (src-file src)) " at ")]
               [else " At "])
         "line " (cond [(src-line src) => number->string]
                       [else "(unknown)"])
         " column " (cond [(src-col src) => number->string]
                          [else "(unknown)"]))))

      (super-instantiate ())))


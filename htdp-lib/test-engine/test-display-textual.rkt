#lang racket/base
(provide test-display-textual%)

(require racket/class
         (only-in racket/port make-pipe-with-specials)
         racket/pretty
         (only-in racket/list make-list)
         (only-in deinprogramm/quickcheck/quickcheck result-arguments-list)
         test-engine/test-info
         test-engine/render-value
         (only-in deinprogramm/signature/signature signature-name))

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

; This is like the printf procedures---it uses `print-string' to print
; the string portions, and print-formatted to print the values
; referenced via ~F. ~<w> is not supported.

(define (print-with-values fstring print-string print-formatted
                           . vals)
  (let ((size (string-length fstring)))
    (let loop ((start 0)
               (i 0)
               (vals vals)
               (seen-vals '())) ; reversed
      (cond
        ((>= i size)
         (print-string (apply format (substring fstring start i) (reverse seen-vals))))
        ((char=? (string-ref fstring i) #\~)
         (case (string-ref fstring (+ 1 i))
           ((#\n #\~) (loop start (+ 1 i) vals seen-vals))
           ((#\F #\f)
            (print-string (apply format (substring fstring start i) (reverse seen-vals)))
            (print-formatted (car vals))
            (loop (+ 2 i) (+ 2 i) (cdr vals) '()))
           (else
            (loop start (+ 2 i) (cdr vals) (cons (car vals) seen-vals)))))
        (else
         (loop start (+ 1 i) vals seen-vals))))))

; helper for printing error messages
(define (print-reason fail)
  (define (do-printing fstring . vals)
    (apply print-with-values fstring display render-value vals))
  (cond
    [(unsatisfied-error? fail)
     (do-printing 
      "check-satisfied encountered an error instead of the expected kind of value, ~F. \n   :: ~a"
      (unsatisfied-error-expected fail)
      (unsatisfied-error-message fail))]
    [(unexpected-error? fail)
     (do-printing
      "check-expect encountered the following error instead of the expected value, ~F. \n   :: ~a"
      (unexpected-error-expected fail)
      (unexpected-error-message fail))]
    [(violated-signature? fail)
     (do-printing
      "signature violation :: ~a violated ~a"
      (violated-signature-obj fail)
      (signature-name (violated-signature-signature fail))
      ; don't know what to do with this here:
      #;(violated-signature-blame fail))]
    [(satisfied-failed? fail)
     (do-printing "Actual value ~F does not satisfy ~F.\n"
                  (satisfied-failed-actual fail)
                  (satisfied-failed-name fail))]
    [(unequal? fail)
     (do-printing "Actual value differs from the expected value.\n")

     ;; any -> (listof (listof (or/c string? <special>)))
     (define (to-loloss v)
       (define-values (in out) (make-pipe-with-specials))
       (thread
        (λ ()
          (parameterize ([pretty-print-columns 40])
            (print v out))
          (close-output-port out)))
       (define (fetch-line)
         (let loop ([sofar '()])
           (define c (read-char-or-special in))
           (cond
             [(eof-object? c)
              (if (null? sofar)
                  #f
                  (reverse sofar))]
             [(equal? #\newline c)
              (reverse sofar)]
             [else (loop (cons c sofar))])))
       (define lines
         (let loop ([lines 0])
           (cond
             [(= lines 40) (list (string->list "..."))]
             [else
              (define l (fetch-line))
              (cond
                [l (cons l (loop (+ lines 1)))]
                [else '()])])))
       (close-input-port in)
       lines)
     
     (define (send-loss loss)
       (for ([ele (in-list loss)])
         (cond
           [(char? ele) (display ele)]
           [(port-writes-special? (current-output-port))
            (write-special ele)]
           [else (display ele)])))
     
     (define prefix "  ")
     (define between "  ")
     (define plain-expected-los (to-loloss (unequal-actual fail)))
     (define plain-actual-los (to-loloss (unequal-test fail)))
     (define actual-value   "Actual value:")
     (define expected-value "Expected value:")
     (cond
       [(and (= 1 (length plain-expected-los))
             (= 1 (length plain-actual-los)))
        (define δ (- (string-length expected-value) (string-length actual-value)))
        (display prefix)
        (display (make-string (max 0 δ) #\space))
        (display actual-value)
        (display " ")
        (send-loss (car plain-actual-los))
        (newline)
        (display prefix)
        (display (make-string (max 0 (- δ)) #\space))
        (display expected-value)
        (display " ")
        (send-loss (car plain-expected-los))
        (newline)]
       [else
        (define expected-los (cons (string->list actual-value) plain-actual-los))
        (define actual-los (cons (string->list expected-value) plain-expected-los))
        (define max-expected (apply max 0 (map length expected-los)))
        (define padded-expected-los
          (for/list ([l (in-list expected-los)])
            (append l
                    (make-list (- max-expected (length l))
                               #\space))))
        (let loop ([los-left padded-expected-los]
                   [los-right actual-los])
          (cond
            [(and (null? los-left) (null? los-right)) (void)]
            [(null? los-left)
             (display prefix)
             (display (make-string max-expected #\space))
             (display between)
             (send-loss (car los-right))
             (newline)
             (loop '() (cdr los-right))]
            [(null? los-right)
             (display prefix)
             (send-loss (car los-left))
             (newline)
             (loop (cdr los-left) '())]
            [else
             (display prefix)
             (send-loss (car los-left))
             (display between)
             (send-loss (car los-right))
             (newline)
             (loop (cdr los-left) (cdr los-right))]))])]
    [(outofrange? fail)
     (do-printing "Actual value ~F is not within ~a of expected value ~F."
                  (outofrange-test fail)
                  (outofrange-range fail)
                  (outofrange-actual fail))]
    [(incorrect-error? fail)
     (do-printing "check-error encountered the following error instead of the expected ~a\n   :: ~a"
                  (incorrect-error-expected fail)
                  (incorrect-error-message fail))]
    [(expected-error? fail)
     (do-printing "check-error expected the following error, but instead received the value ~F.\n ~a"
                  (expected-error-value fail)
                  (expected-error-message fail))]
    [(message-error? fail)
     (for-each display (message-error-strings fail))]
    [(not-mem? fail)
     (define items (not-mem-set fail))
     (cond
       [(null? (cdr items))
        (do-printing "Actual value ~F differs from ~F."
                     (not-mem-test fail)
                     (car items))]
       [else
        (do-printing "Actual value ~F differs from all given members in: ~F"
                     (not-mem-test fail)
                     (car items))
        (for ([a (in-list (cdr items))])
          (do-printing ", ~F" a))
        (do-printing ".")])]
    [(not-range? fail)
     (do-printing "Actual value ~F is not between ~F and ~F, inclusive."
                  (not-range-test fail)
                  (not-range-min fail)
                  (not-range-max fail))]
    [(unimplemented-wish? fail)
     (do-printing (string-append "Test relies on a call to wished-for function ~F "
                                 " that has not been implemented, with arguments ~F.")
                  (unimplemented-wish-name fail)
                  (unimplemented-wish-args fail))]
    [(property-fail? fail)
     (display "Property falsifiable with")
     (for ([arguments (in-list (result-arguments-list (property-fail-result fail)))])
       (for ([p (in-list arguments)])
         (if (car p)
             (do-printing " ~a = ~F" (car p) (cdr p))
             (do-printing "~F" (cdr p)))))]
    [(property-error? fail)
     (do-printing "check-property encountered the following error\n:: ~a"
                  (property-error-message fail))])
  (display "\n"))


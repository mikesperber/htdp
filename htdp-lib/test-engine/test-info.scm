#lang racket/base

(require racket/class
         racket/pretty
         racket/port
         racket/list
         lang/private/continuation-mark-key
         deinprogramm/quickcheck/quickcheck
	 (except-in deinprogramm/signature/signature signature-violation)
         "print.ss"
	 "srcloc.rkt")

(provide (all-defined-out))

;; (make-failed-check check-fail (U #f exn) (U #f srcloc?)
(define-struct failed-check (reason exn? srcloc?))

(define-struct check-fail (src format))

;; (make-unexpected-error src format string exn)
(define-struct (unexpected-error check-fail) (expected message exn))
(define-struct (unsatisfied-error check-fail) (expected message exn))
;; (make-unequal src format scheme-val scheme-val)
(define-struct (unequal check-fail) (test actual))
;; (make-outofrange src format scheme-val scheme-val inexact)
(define-struct (outofrange check-fail) (test actual range))
;; (make-incorrect-error src format string exn)
(define-struct (incorrect-error check-fail) (expected message exn))
;; (make-expected-error src format string scheme-val)
(define-struct (expected-error check-fail) (message value))
;; (make-expected-an-error src format scheme-val)
(define-struct (expected-an-error check-fail) (value))
;; (make-not-mem src format scheme-val scheme-val)
(define-struct (not-mem check-fail) (test set))
;; (make-not-range src format scheme-val scheme-val scheme-val)
(define-struct (not-range check-fail) (test min max))
;; (make-satisfied-failed src format scheme-val symbol)
(define-struct (satisfied-failed check-fail) (actual name))

;;Wishes
(define-struct (unimplemented-wish check-fail) (name args))

(define-struct (violated-signature check-fail) (obj signature blame))

(define-struct signature-got (value format))

(define-struct signature-violation (obj signature message srcloc blame))

(define-struct (property-fail check-fail) (result))
(define-struct (property-error check-fail) (message exn))

;; (make-message-error src format (listof string))
(define-struct (message-error check-fail) (strings))

(define test-format (make-parameter (λ (v p) (print v p))))

(define test-info%
  (class* object% ()
    (super-instantiate ())
    
    (init-field (style 'check-base))
    (field [analyses null])

    ;; Tests
    (define total-tsts 0)
    (define failed-tsts 0)
    (define total-cks 0)
    (define failed-cks 0)
    (define total-called-wishes 0)
    
    (define failures null)
    (define wishes null)
    
    (define unreported-failures #f)
 
    (define/public (clear-unreported-failures)
      (set! unreported-failures #f))
    
    (define/public (report-failure)
      (set! unreported-failures #t))
    
    (define/public (has-unreported-failures)
      unreported-failures)
    
    (define/public (test-style) style)
    (define/public (tests-run) total-tsts)
    (define/public (tests-failed) failed-tsts)
    (define/public (checks-run) total-cks)
    (define/public (checks-failed) failed-cks)
    (define/public (summarize-results)
      (cond [(and (zero? total-tsts) (zero? total-cks)) 'no-tests]
            [(and (zero? failed-cks) (zero? failed-tsts)) 'all-passed]
            [else 'mixed-results]))
    (define/public (called-wishes) total-called-wishes)
    
    (define/public (failed-checks) failures)
    (define/public (unimplemented-wishes) wishes)
    
    (define/pubment (add-wish-call name)
      (set! total-called-wishes (add1 total-called-wishes))
      (unless (memq name wishes) (set! wishes (cons name wishes)))
      (inner (void) add-wish-call name))
    
    (define/pubment (add-check)
      (set! total-cks (add1 total-cks))
      (inner (void) add-check))
    
    (define/pubment (add-test)
      (set! total-tsts (add1 total-tsts))
      (inner (void) add-test))
    
    (define/pubment (add-check-failure fail exn? srcloc?)
      (set! failed-cks (add1 failed-cks))
      (set! failures (cons (make-failed-check fail exn? srcloc?) failures))
      (inner (void) add-check-failure fail exn? srcloc?))
    
    (define/pubment (add-wish name)
      (unless (memq name wishes)
        (set! wishes (cons name wishes)))
      (inner (void) add-wish name))
    
    ;; check-failed: (U check-fail (list (U string snip%))) src (U exn false) -> void
    (define/pubment (check-failed msg src exn? srcloc?)
      (let ((fail
             ;; We'd like every caller to make a check-fail object,
             ;; but some (such as ProfessorJ's run time) cannot because
             ;; of phase problems.  Therefore, do the coercion here.
             (if (check-fail? msg)
                 msg
                 (make-message-error src #f msg))))
        (add-check-failure fail exn? srcloc?)
        (report-failure)
        (inner (void) check-failed fail src exn? srcloc?)))
    
    (define/pubment (test-failed failed-info)
      (set! failed-tsts (add1 failed-tsts))
      (report-failure)
      (inner (void) test-failed failed-info))

    ;; Signatures
    (define signature-violations '())

    (define/pubment (signature-failed obj signature message blame)
      
      (let* ([srcloc (continuation-marks-srcloc (current-continuation-marks))]
             [message
              (or message
                  (make-signature-got obj (test-format)))])
        
        (set! signature-violations
              (cons (make-signature-violation obj signature message srcloc blame)
                    signature-violations)))
      (report-failure)
      (inner (void) signature-failed obj signature message))
    
    (define/public (failed-signatures) (reverse signature-violations))
    
    (define/pubment (property-failed result src-info)
      (report-failure)
      (add-check-failure (make-property-fail src-info (test-format) result) #f #f))
    
    (define/pubment (property-error exn src-info)
      (report-failure)
      (add-check-failure (make-property-error src-info (test-format) (exn-message exn) exn) exn (exn-srcloc exn)))
    
    (define/public (add-analysis a) (set! analyses (cons a analyses)))
    
    (define/public (analyze-position src . vals)
      (for ([a analyses]) (send a analyze src vals)))
    (define/public (extract-info pred?)
      (filter pred? (map (lambda (a) (send a provide-info)) analyses)))))

; helper for printing error messages
(define (print-reason fail)
  (define (print-formatted val)
    (define cff (check-fail-format fail))
    (cond
      [(procedure-arity-includes? cff 2)
       (cff val (current-output-port))]
      [else
       (display (cff val))]))
  (define (do-printing fstring . vals)
    (apply print-with-values fstring display print-formatted vals))
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

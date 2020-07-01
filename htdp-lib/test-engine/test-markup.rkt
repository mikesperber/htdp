#lang racket/base

(provide render-value-parameter
         test-object->markup)

(require (only-in racket/string string-split)
         string-constants
         "test-info.rkt"
         "test-engine.rkt"
         test-engine/render-value
         test-engine/markup
         (except-in deinprogramm/signature/signature signature-violation) ; clashes with test-engine
         deinprogramm/quickcheck/quickcheck)

(define (test-object->markup test-object)
  (let* ([total-checks (test-object-checks-count test-object)]
         [failed-checks (test-object-failed-checks-count test-object)]
         [violated-signatures (test-object-signature-violations test-object)] ; FIXME name
         [wishes (test-object-wishes test-object)]
         [total-wishes (length wishes)]
         [total-wish-calls (test-object-called-wishes test-object)])
         
    (vertical
     (cond
       [(zero? total-checks)
        (vertical (string-constant test-engine-must-be-tested)
                  empty-markup)]
       [(= 1 total-checks) 
        (vertical (string-constant test-engine-ran-1-test)
                  empty-markup)]
       [else
        (vertical
         (format (string-constant test-engine-ran-n-tests) total-checks)
         empty-markup)])

     empty-line

     (cond 
       [(null? wishes) empty-markup]
       [(= 1 total-wishes)
        (format "Wished for function ~a has not been implemented." (car wishes))]
       [(= 2 total-wishes)
        (format "Wished for functions ~a and ~a have not been implemented."
                (car wishes) (cadr wishes))]
       [else (format "Wished for functions ~a have not been implemented."
                     (format-list wishes))])

     (if (> total-checks 0)
         (vertical
          (cond
            [(and (zero? failed-checks) (= 1 total-checks))
             (string-constant test-engine-1-check-passed)]
            [(zero? failed-checks) 
             (string-constant test-engine-all-tests-passed)]
            [(= failed-checks total-checks)
             (string-constant test-engine-0-tests-passed)]
            [else (format (string-constant test-engine-m-of-n-tests-failed)
                          failed-checks total-checks)])
          empty-line)
         empty-markup)

     (cond
       ((null? violated-signatures)
        (string-constant test-engine-no-signature-violations))
       ((null? (cdr violated-signatures))
        (string-constant test-engine-1-signature-violation))
       (else
        (format (string-constant test-engine-n-signature-violations)
                (length violated-signatures))))

     empty-line

     (check-failures->markup (test-object-failed-checks test-object))
     (signature-violations->markup violated-signatures))))

(define (format-list l)
  (cond
   [(null? (cdr l)) (format "and ~a" (car l))]
   [else (format "~a, ~a" (car l) (format-list (cdr l)))]))

(define (check-failures->markup checks)
  (if (pair? checks)
      (vertical (string-constant test-engine-check-failures)
                (apply vertical
                       (map  failed-check->markup
                             (reverse checks))))
      empty-markup))

(define (signature-violations->markup violations)
  (if (pair? violations)
      (vertical (string-constant test-engine-signature-violations)
                (vertical-markup
                 (map (lambda (violation)
                        (horizontal "        "
                                    (signature-violation->markup violation)))
                      violations)))
      empty-markup))

(define (failed-check->markup failed-check)
  (horizontal
   "        "
   (if (failed-check-exn? failed-check)
       (error-link->markup (failed-check-reason failed-check)
                           (failed-check-exn? failed-check)
                           (failed-check-srcloc? failed-check)
                           (check-fail-src (failed-check-reason failed-check)))
       (link->markup (failed-check-reason failed-check)
                     (check-fail-src (failed-check-reason failed-check))))))

(define (link->markup reason dest)
  (horizontal
   (reason->markup reason)
   ;; FIXME: display-link - specifically format-src used there - does something fancier
   (list->srcloc dest)))

; keep this for reference:
#|
(define (display-link text dest src-editor)
  (let ((start (send text get-end-position)))
    (send text insert (format-src dest))
    (when src-editor
      (send text set-clickback
            start (send text get-end-position)
            (lambda (t s e) (highlight-check-error dest  src-editor))
            #f #f)
      (set-clickback-style text start "royalblue"))))

;;format-src: src -> string
(define (format-src src)
  (format-position (car src) (cadr src) (caddr src)))

(define (format-position file line column)
  (let ([line (cond [line => number->string]
                    [else 
                     (string-constant test-engine-unknown)])]
        [col
         (cond [column => number->string]
               [else (string-constant test-engine-unknown)])])  
          
    (if (path? file)
        (let-values (((base name must-be-dir?)
                      (split-path file)))
          (if (path? name)
              (format (string-constant test-engine-in-at-line-column)
                      (path->string name) line col)
              (format (string-constant test-engine-at-line-column)
                      line col)))
        (format (string-constant test-engine-at-line-column)
                line col))))
|#

; the check-fail src field has a list, not a srcloc
(define (list->srcloc list)
  (apply srcloc list))

(define (format->markup format-string . vals)
  (let loop ((chars (string->list format-string))
             (vals vals)
             (rev-markups '())
             (rev-lines '()))
    (cond
      ((null? chars)
       (apply vertical
              (reverse (cons (apply horizontal (reverse rev-markups))
                             (reverse rev-lines))))) ; this will normalize
      ((char=? (car chars) #\~)
       (case (cadr chars)
         ((#\n #\~) (loop (cddr chars) vals
                          '()
                          (cons (apply horizontal (reverse rev-markups))
                                rev-lines)))
         ((#\F #\f)
          (loop (cddr chars)
                (cdr vals)
                (cons (framed (value->markup (car vals))) rev-markups)
                rev-lines))
         (else
          (loop (cddr chars)
                (cdr vals)
                (cons (format (string #\~ (cadr chars)) (car vals)) rev-markups)
                rev-lines))))
      (else
       (let inner-loop ((chars chars)
                        (rev-seen '()))
         (if (or (null? chars)
                 (char=? (car chars) #\~))
             (loop chars vals (cons (list->string (reverse rev-seen)) rev-markups) rev-lines)
             (inner-loop (cdr chars) (cons (car chars) rev-seen))))))))

(define (value->markup value)
  (let* ((text (render-value value))
         (lines (string-split text "\n")))
    (vertical-markup lines)))

(define (reason->markup fail)
  (cond
    [(unexpected-error? fail)
     (format->markup (string-constant test-engine-check-encountered-error)
                     (unexpected-error-expected fail)
                     (unexpected-error-message fail))]
    [(unsatisfied-error? fail)
     (format->markup
      "check-satisfied encountered an error instead of the expected kind of value, ~F. \n  :: ~a"
      (unsatisfied-error-expected fail)
      (unsatisfied-error-message fail))]
    [(unequal? fail)
     (format->markup (string-constant test-engine-actual-value-differs-error)
                     (unequal-test fail)
                     (unequal-actual fail))]
    [(satisfied-failed? fail)
     (format->markup "Actual value ~F does not satisfy ~a."
                     (satisfied-failed-actual fail)
                     (satisfied-failed-name fail))]
    [(outofrange? fail)
     (if (string-constant-in-current-language? test-engine-actual-value-not-within-error)
         (format->markup (string-constant test-engine-actual-value-not-within-error)
                         (outofrange-test fail)
                         (outofrange-range fail)
                         (outofrange-actual fail))
         (format->markup (string-constant test-engine-actual-value-not-within-error/alt-order)
                         (outofrange-test fail)
                         (outofrange-actual fail)
                         (outofrange-range fail)))]
    [(incorrect-error? fail)
     (format->markup (string-constant test-engine-encountered-error-error)
                     (incorrect-error-expected fail)
                     (incorrect-error-message fail))]
    [(expected-error? fail)
     (format->markup (string-constant test-engine-expected-error-error)
                     (expected-error-value fail)
                     (expected-error-message fail))]
    [(expected-an-error? fail)
     (format->markup (string-constant test-engine-expected-an-error-error)
                     (expected-an-error-value fail))]
    [(message-error? fail)
     (apply horizontal (message-error-strings fail))]
    [(not-mem? fail)
     (horizontal
      (format->markup (string-constant test-engine-not-mem-error)
                      (not-mem-test fail))
      (apply horizontal
             (map (lambda (a)
                    (format->markup " ~F" a))
                  (not-mem-set fail)))
      ".")]
    [(not-range? fail)
     (format->markup (string-constant test-engine-not-range-error)
                     (not-range-test fail)
                     (not-range-min fail)
                     (not-range-max fail))]
    [(unimplemented-wish? fail)
     (format->markup "Test relies on a call to wished for function ~F that has not been implemented, with arguments ~F."
                     (symbol->string (unimplemented-wish-name fail))
                     (unimplemented-wish-args fail))]
    [(property-fail? fail)
     (horizontal 
      (string-constant test-engine-property-fail-error)
      (apply horizontal
             (map (lambda (arguments)
                    (map (lambda (p)
                           (if (car p)
                               (format->markup " ~a = ~F" (car p) (cdr p))
                               (format->markup "~F" (cdr p))))
                         arguments))
                  (result-arguments-list (property-fail-result fail)))))]
    [(property-error? fail)
     (format->markup (string-constant test-engine-property-error-error)
                     (property-error-message fail))]))

(define (error-link->markup reason exn srcloc dest)
  (horizontal (link->markup reason dest)
              (if (and exn srcloc) ; FIXME: does not even use exn
                  (horizontal
                   (string-constant test-engine-check-error-cause) " "
                   srcloc)
                  empty-markup)))

(define (signature-violation->markup violation)
  (let* ((signature (signature-violation-signature violation))
         (stx (signature-syntax signature))
         (srcloc (signature-violation-srcloc violation))
         (message (signature-violation-message violation)))
    (horizontal
     (cond
       ((string? message)
        message)
       ((signature-got? message)
        
        (horizontal (string-constant test-engine-got)
                    " "
                    (render-value (signature-got-value message))))
       (else empty-markup))
    
     (if srcloc
         (horizontal " " srcloc)
         empty-markup)
     ", "
     (string-constant test-engine-signature)
     " "
     (syntax-srcloc stx)
     (cond
       ((signature-violation-blame violation)
        => (lambda (blame)
             (horizontal
              "        "
              (string-constant test-engine-to-blame)
              " "
              (syntax-srcloc blame))))
       (else empty-markup)))))

(define (syntax-srcloc stx)
  (srcloc (syntax-source stx)
          (syntax-line stx) (syntax-column stx)
          (syntax-position stx) (syntax-span stx)))

(module+ test
  (require rackunit)

    (parameterize
      ((render-value-parameter
        (lambda (v)
          (format "<~V>" v))))
      (check-equal? (format->markup "abc")
                    "abc")
      (check-equal? (format->markup "foo ~F bar ~v~~bar" 5 #f)
                    (vertical
                     (horizontal "foo "
                                 (framed "<5>")
                                 " bar "
                                 "#f")
                     "bar")))
  
  (parameterize
      ((render-value-parameter
        (lambda (v)
          (format "<~V>" v))))
    (call-with-current-language
     'english
     (lambda ()
       ;; FIXME: more of these
       (check-equal? (reason->markup
                      (make-unexpected-error #f 'expected "not expected" #f))
                     (vertical
                      (horizontal
                       "check-expect encountered the following error instead of the expected value, "
                       (framed "<'expected>")
                       ". ")
                      "   :: not expected"))))))



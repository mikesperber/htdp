#lang racket/base
(provide horizontal-markup horizontal
         horizontal-markup? horizontal-markups
         vertical-markup vertical
         vertical-markup? vertical-markups
         empty-markup empty-markup?
         empty-line
         framed
         framed-markup? framed-markup
         markup?
         flatten-markup
         display-markup)
(require racket/list)

; TODO: styled

(define (markup? x)
  (or (string? x)
      (empty? x)
      (horizontal-markup? x)
      (vertical-markup? x)
      (srcloc? x)
      (framed? x)))

(struct empty
  ()
  #:transparent)

(define empty-markup (empty))
(define (empty-markup? thing)
  (empty? thing))

(struct horizontal-markup
  (markups)
  #:transparent)

(define horizontal-markups horizontal-markup-markups)

(define empty-line (horizontal-markup '()))

; flatten out nested markup elements, merge adjacent strings
(define (normalize-horizontal markups)
  (let ((flattened
         (append-map (lambda (markup)
                       (cond
                         ((empty? markup) '())
                         ((horizontal-markup? markup)
                          (horizontal-markup-markups markup))
                         (else
                          (list markup))))
                     markups)))
    (merge-adjacent-strings flattened)))

(define (merge-adjacent-strings markups)
  (call-with-values
   (lambda () (splitf-at markups string?))
   (lambda (strings after)
     (let ((after-merged
            (if (null? after)
                '()
                (cons (car after)
                      (merge-adjacent-strings (cdr after))))))
       (if (null? strings)
           after-merged
           (cons (apply string-append strings)
                 after-merged))))))

(define (horizontal . markups)
  (let ((markups (normalize-horizontal markups)))
    (cond
      ((null? markups) empty-markup)
      ((null? (cdr markups))
       (car markups))
      (else (horizontal-markup markups)))))

(struct vertical-markup
  (markups)
  #:transparent)

(define vertical-markups vertical-markup-markups)

(define (flatten-vertical markups)
  (append-map (lambda (markup)
                (cond
                  ((empty? markup) '())
                  ((vertical-markup? markup)
                   (vertical-markup-markups markup))
                  (else (list markup))))
              markups))

(define (vertical . markups)
  (let ((markups (flatten-vertical markups)))
    (cond
      ((null? markups)
       empty-markup)
      ((null? (cdr markups))
       (car markups))
      (else
       (vertical-markup markups)))))

(struct framed
  (markup)
  #:transparent)

(define framed-markup? framed?)

(define (flatten-markup markup)
  (cond
    ((string? markup) markup)
    ((horizontal-markup? markup)
     (apply string-append
            (map flatten-markup (horizontal-markup-markups markup))))
    ((srcloc? markup)
     (srcloc->string markup))
    ((framed? markup)
     (flatten-markup (framed-markup markup)))))
    

(define (listify thing)
  (if (or (null? thing)
          (pair? thing))
      thing
      (list thing)))

(define (block-width block)
  (if (null? block)
      0
      (string-length (car block))))

(define (block-adjust block height)
  (let ((block-height (length block))
        (width (block-width block)))
    (if (< block-height height)
        (let ((height-diff (abs (- height block-height))))
          (append (make-list (quotient height-diff 2) (make-string width #\space))
                  block
                  (make-list (quotient (+ height-diff 1) 2) (make-string width #\space))))
        block)))
      
(define (append-blocks-2 block1 block2)
  (let ((height1 (length block1))
        (height2 (length block2)))
    (let ((height (max height1 height2)))
      (map string-append
           (block-adjust block1 height)
           (block-adjust block2 height)))))

(define (append-blocks . blocks)
  (foldr append-blocks-2 '() blocks))

(define (block-box block)
  (let ((width (block-width block)))
    (append (list (string-append "┌─" (make-string width #\─) "─┐"))
            (map (lambda (line)
                   (string-append "│ " line " │"))
                 block)
            (list (string-append "└─" (make-string width #\─) "─┘")))))

(define (block-display port block)
  (for-each (lambda (line)
              (display line port)
              (newline port))
            block))

(define (pad-to line width)
  (let ((diff (max 0 (- width (string-length line)))))
    (if (zero? diff)
        line
        (string-append line (make-string diff #\space)))))

(define (normalize-lines lines)
  (let ((width (apply max (map string-length lines))))
    (map (lambda (line)
           (pad-to line width))
         lines)))

(define (markup->block markup)
  (cond
    ((string? markup) (list markup))
    ((horizontal-markup? markup)
     (apply append-blocks
            (map markup->block (horizontal-markup-markups markup))))
    ((vertical-markup? markup)
     (normalize-lines
      (append-map markup->block (vertical-markup-markups markup))))
    ((srcloc? markup)
     (list (srcloc->string markup)))
    ((framed? markup)
     (block-box (markup->block (framed-markup markup))))))

(define display-markup
  (case-lambda
    ((markup) (display-markup markup (current-output-port)))
    ((markup port)
     (block-display port (markup->block markup)))))

(module+ test
  (require rackunit)

  (check-equal? (append-blocks (list "aaa" "bbb" "ccc") (list "xxxx" "yyyy" "zzzz"))
                '("aaaxxxx" "bbbyyyy" "ccczzzz"))
  (check-equal? (append-blocks (list "aaa" "bbb" "ccc") (list "xxxx" "zzzz"))
                '("aaaxxxx" "bbbzzzz" "ccc    "))
  (check-equal? (append-blocks (list "aaa" "bbb" "ccc") (list "xxxx"))
                '("aaa    " "bbbxxxx" "ccc    "))
  (check-equal? (append-blocks (list "aaa" "ccc") (list "xxxx" "yyyy" "zzzz"))
                '("aaaxxxx" "cccyyyy" "   zzzz"))
  (check-equal? (append-blocks (list "ccc") (list "xxxx" "yyyy" "zzzz"))
                '("   xxxx" "cccyyyy" "   zzzz"))

  (check-equal? (block-box (list "xxx" "yyy" "zzz"))
                '("┌─────┐" "│ xxx │" "│ yyy │" "│ zzz │" "└─────┘"))

  (check-equal? (markup->block (horizontal "foo" "bar"
                                             (framed "baz")
                                             "bam" "wup"))
                '("      ┌─────┐      " "foobar│ baz │bamwup" "      └─────┘      "))
  
  (check-equal? (markup->block (horizontal "foo" "bar"
                                             (framed "baz")
                                             (vertical "bam" (framed "wup"))))
                '("      ┌─────┐bam    "
                  "foobar│ baz │┌─────┐"
                  "      └─────┘│ wup │"
                  "             └─────┘"))
  
  (check-equal? (horizontal "foo" "bar" "baz")
                "foobarbaz")
  (check-equal? (horizontal "foo" "bar"
                           (horizontal "baz" "bla")
                           "bam" "wup")
                "foobarbazblabamwup")

  (check-equal? (horizontal "foo" "bar"
                           (framed "baz")
                           "bam" "wup")
                (horizontal "foobar" (framed "baz") "bamwup"))
  (check-equal? (vertical)
                empty-markup)
  (check-equal? (vertical "foo")
                "foo")
  (check-equal? (vertical "foo")
                "foo")
  (check-equal? (vertical "foo" empty-markup)
                "foo")
  (check-equal? (vertical "foo" empty-markup "bar")
                (vertical "foo" "bar"))
  (check-equal? (vertical "foo" empty-line "bar")
                (vertical "foo" empty-line "bar"))
  (check-equal? (vertical "foo" "bar")
                (vertical-markup '("foo" "bar")))
  (check-equal? (vertical "foo" (vertical "bla" "baz") "bar")
                (vertical "foo" "bla" "baz" "bar"))
                          
  (check-equal? (flatten-markup "foo") "foo"
                "String")
  (check-equal? (flatten-markup (horizontal "foo" "bar" "baz"))
                "foobarbaz"
                "List of strings"))
  
  

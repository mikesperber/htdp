#lang racket/base
(provide horizontal? horizontal-fragments ; don't want to export markup constructor
         horizontal
         empty-markup empty-markup?
         (struct-out framed)
         fragment?
         flatten-fragment
         vertical? vertical-fragments
         vertical)
(require racket/list)

; TODO: styled

(define (fragment? x)
  (or (string? x)
      (horizontal-markup? x)
      (vertical-markup? x)
      (srcloc? x)
      (framed? x)))

(struct horizontal-markup
  (fragments)
  #:transparent)

(define horizontal? horizontal-markup?)
(define horizontal-fragments horizontal-markup-fragments)

(define empty-markup (horizontal-markup '()))
(define (empty-markup? markup)
  (and (horizontal-markup? markup)
       (empty? (horizontal-markup-fragments markup))))

; flatten out nested markup elements, merge adjacent strings
(define (normalize-horizontal fragments)
  (let ((flattened
         (apply append
                (map (lambda (fragment)
                       (if (horizontal-markup? fragment)
                           (horizontal-markup-fragments fragment)
                           (list fragment)))
                     fragments))))
    (merge-adjacent-strings flattened)))

(define (merge-adjacent-strings fragments)
  (call-with-values
   (lambda () (splitf-at fragments string?))
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

(define (horizontal . fragments)
  (let ((fragments (normalize-horizontal fragments)))
    (cond
      ((null? fragments) empty-markup)
      ((null? (cdr fragments))
       (car fragments))
      (else (horizontal-markup fragments)))))

(struct vertical-markup
  (fragments)
  #:transparent)

(define vertical? vertical-markup?)
(define vertical-fragments vertical-markup-fragments)

(define (flatten-vertical fragments)
  (apply append
         (map (lambda (fragment)
                (cond
                  ((empty-markup? fragment) '())
                  ((vertical-markup? fragment)
                   (vertical-markup-fragments fragment))
                  (else (list fragment))))
              fragments)))

(define (vertical . fragments)
  (let ((fragments (flatten-vertical fragments)))
    (cond
      ((null? fragments)
       empty-markup)
      ((null? (cdr fragments))
       (car fragments))
      (else
       (vertical-markup fragments)))))

(struct framed
  (fragment)
  #:transparent)

(define (flatten-fragment fragment)
  (cond
    ((string? fragment) fragment)
    ((horizontal-markup? fragment)
     (apply string-append
            (map flatten-fragment (horizontal-markup-fragments fragment))))
    ((srcloc? fragment)
     (srcloc->string fragment))
    ((framed? fragment)
     (flatten-fragment (framed-fragment fragment)))))
    

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

(define (fragment->block fragment)
  (cond
    ((string? fragment) (list fragment))
    ((horizontal-markup? fragment)
     (apply append-blocks
            (map fragment->block (horizontal-markup-fragments fragment))))
    ((vertical-markup? fragment)
     (normalize-lines
      (apply append
             (map fragment->block (vertical-markup-fragments fragment)))))
    ((srcloc? fragment)
     (list (srcloc->string fragment)))
    ((framed? fragment)
     (block-box (fragment->block (framed-fragment fragment))))))

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

  (check-equal? (fragment->block (horizontal "foo" "bar"
                                             (framed "baz")
                                             "bam" "wup"))
                '("      ┌─────┐      " "foobar│ baz │bamwup" "      └─────┘      "))
  
  (check-equal? (fragment->block (horizontal "foo" "bar"
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
  (check-equal? (vertical "foo" "bar")
                (vertical-markup '("foo" "bar")))
  (check-equal? (vertical "foo" (vertical "bla" "baz") "bar")
                (vertical "foo" "bla" "baz" "bar"))
                          
  (check-equal? (flatten-fragment "foo") "foo"
                "String")
  (check-equal? (flatten-fragment (horizontal "foo" "bar" "baz"))
                "foobarbaz"
                "List of strings"))
  
  

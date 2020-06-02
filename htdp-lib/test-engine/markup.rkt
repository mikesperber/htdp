#lang racket/base
(provide horizontal? horizontal-fragments ; don't want to export markup constructor
         horizontal
         empty-markup
         (struct-out framed)
         fragment?
         flatten-fragment
         vertical? vertical-fragments
         vertical
         insert-fragment)
(require racket/gui/base
         racket/list
         (only-in racket/class send make-object is-a? new) ;; FIXME: zap
         racket/snip
         framework)

; TODO: box
; TODO: test
; TODO: change code to use it
; TODO: styled
; TODO: separate rendering from representation

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
  (horizontal-markup (normalize-horizontal fragments)))

(struct vertical-markup
  (fragments)
  #:transparent)

(define vertical? vertical-markup?)
(define vertical-fragments vertical-markup-fragments)

(define (flatten-vertical fragments)
  (apply append
         (map (lambda (fragment)
                (if (vertical-markup? fragment)
                    (vertical-markup-fragments fragment)
                    (list fragment)))
              fragments)))

(define (vertical . fragments)
  (vertical-markup (flatten-vertical fragments)))

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
    
; NEED fancier version that actually does Unicode frame

; src-editor can be #f
(define (insert-srcloc srcloc text src-editor)
  (let ((start (send text get-end-position)))
    (send text insert (srcloc->string srcloc))
    (when src-editor
      (send text set-clickback
            start (send text get-end-position)
            (lambda (t s e) (highlight-srcloc srcloc src-editor))
            #f #f)
      (let ([end (send text get-end-position)])
        (send text insert " ")
        (send text change-style
              (make-object style-delta% 'change-underline #t)
              start end #f)
        (when (color-prefs:known-color-scheme-name? 'drracket:read-eval-print-loop:value-color)
          (send text change-style
                (color-prefs:lookup-in-color-scheme 'drracket:read-eval-print-loop:value-color)
                start end #f))))))

(define (definitions-tab definitions-text)
  (and definitions-text (send definitions-text get-tab)))

(define (definitions-rep definitions-text)
  (cond
   ((definitions-tab definitions-text) =>
    (lambda (tab)
      (send tab get-ints)))
   (else #f)))

(define (highlight-srcloc srcloc src-editor)
  (let ((current-rep (definitions-rep src-editor)))
    (when (and current-rep src-editor
               (is-a? src-editor text:basic<%>))
      (let ((error-src (if (send src-editor port-name-matches? (srcloc-source srcloc)) ; definitions or REPL?
                           src-editor
                           current-rep)))
        (send current-rep highlight-errors
              (list srcloc) #f)
        (let* ([current-tab (definitions-tab src-editor)]
               [frame (send current-tab get-frame)])
          (unless (send current-tab is-current-tab?)
            (let loop ([tabs (send frame get-tabs)] [i 0])
              (unless (null? tabs)
                (if (eq? (car tabs) current-tab)
                    (send frame change-to-nth-tab i)
                    (loop (cdr tabs) (add1 i))))))
          (send frame show #t))))))

(define (insert-fragment fragment text src-editor)
  (cond
    ((string? fragment)
     (send text insert fragment))
    ((horizontal-markup? fragment)
     (for-each (lambda (fragment)
                 (insert-fragment fragment text src-editor))
               (horizontal-markup-fragments fragment)))
    ((vertical-markup? fragment)
     (for-each (lambda (fragment)
                 (insert-fragment fragment text src-editor)
                 (send text insert #\newline))
               (vertical-markup-fragments fragment)))
    ((srcloc? fragment)
     (insert-srcloc fragment text src-editor))
    ((framed? fragment)
     (insert-framed (lambda (text)
                      (insert-fragment (framed-fragment fragment) text src-editor))
                    text src-editor))))

(define (insert-framed insert-text text src-editor)
  (let* ([framed-text (new (text:wide-snip-mixin ; FIXME: make a constant class here
                            (text:basic-mixin
                             (editor:standard-style-list-mixin
                              (editor:basic-mixin
                               text%)))))]
         [snip (new editor-snip% [editor framed-text])])
    (send snip use-style-background #t)
    (insert-text framed-text)
    (send framed-text lock #t)
    (send text insert snip)))

;; for development
(define (display-fragment fragment)
  (let* ((frame (new frame%
                     [label "Fragment"]
                     [width 600] [height 400]))
         (text (new text%))
         (canvas (new editor-canvas% [parent frame])))
    (send canvas set-editor text)
    (insert-fragment fragment text text)
    (send text lock #t)
    (send frame show #t)))

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
  
  (define (render-fragment-via-text fragment)
    (let ((text (new text%)))
      (insert-fragment fragment text #f)
      (send text get-text 0 'eof #t)))

  (check-equal? (horizontal "foo" "bar" "baz")
                (horizontal "foobarbaz"))
  (check-equal? (horizontal "foo" "bar"
                           (horizontal "baz" "bla")
                           "bam" "wup")
                (horizontal "foobarbazblabamwup"))

  (check-equal? (horizontal "foo" "bar"
                           (framed "baz")
                           "bam" "wup")
                (horizontal "foobar" (framed "baz") "bamwup"))

  (check-equal? (flatten-fragment "foo") "foo"
                "String")
  (check-equal? (flatten-fragment (horizontal "foo" "bar" "baz"))
                "foobarbaz"
                "List of strings")
  (check-equal? (render-fragment-via-text "foo")
                "foo"
                "String via text")
  (check-equal? (render-fragment-via-text (horizontal "foo" (framed "bar") "baz"))
                "foobarbaz"
                "Framed via text"))
  
  

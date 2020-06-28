#lang racket/base
(provide insert-fragment)
(require test-engine/markup
         racket/gui/base
         (only-in racket/class send make-object is-a? new) ;; FIXME: zap
         racket/snip
         framework)

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
    ((horizontal? fragment)
     (for-each (lambda (fragment)
                 (insert-fragment fragment text src-editor))
               (horizontal-fragments fragment)))
    ((vertical? fragment)
     (for-each (lambda (fragment)
                 (insert-fragment fragment text src-editor)
                 (send text insert #\newline))
               (vertical-fragments fragment)))
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


(module+ test
  (require rackunit)

  (define (render-fragment-via-text fragment)
    (let ((text (new text%)))
      (insert-fragment fragment text #f)
      (send text get-text 0 'eof #t)))

  (check-equal? (render-fragment-via-text "foo")
                "foo"
                "String via text")
  (check-equal? (render-fragment-via-text (horizontal "foo" (framed "bar") "baz"))
                "foobarbaz"
                "Framed via text"))

(module racket-gui scheme/base

  (provide format-value make-formatter (all-from-out "racket-tests.rkt"))
    

  (require mred framework scheme/class 
           mzlib/pconvert mzlib/pretty
	   (for-syntax scheme/base))
  
  (require (except-in "racket-tests.rkt" test)
	   "test-display-gui.rkt")
  
  (define (make-formatter printer)
    (lambda (value)
      (let* ([text* (new (text:ports-mixin
                          (text:wide-snip-mixin
                           (text:basic-mixin
                            (editor:standard-style-list-mixin
                             (editor:basic-mixin
                              text%))))))]
             [text-snip (new editor-snip% [editor text*])])
        (send text-snip use-style-background #t)
        (printer value (send text* get-value-port))
        (flush-output (send text* get-value-port))
        (send text* delete/io (- (send text* last-position) 1) (send text* last-position))
        (send text* lock #t) 
        text-snip)))
  
  (define (format-value value)
    (parameterize ([constructor-style-printing #t]
                   [pretty-print-columns 40])
      (make-formatter (lambda (v o) (pretty-print (print-convert v) o)))))
  
  #;(define (format-value value)
    (cond
      [(is-a? value snip%) value]
      [(or (pair? value) (struct? value))
       (parameterize ([constructor-style-printing #t]
                      [pretty-print-columns 40])
         (let* ([text* (new (editor:standard-style-list-mixin text%))]
                [text-snip (new editor-snip% [editor text*])])
           (pretty-print (print-convert value) (open-output-text-editor text*))
           (send text* lock #t)
           text-snip))]
      [else (format "~v" value)]))
  
  
  )

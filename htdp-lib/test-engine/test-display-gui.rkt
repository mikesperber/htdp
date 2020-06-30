#lang racket/base
(provide test-panel% test-window% test-display%)

(require racket/class
         racket/gui/base
         framework
         test-engine/markup
         test-engine/markup-gui
         test-engine/test-markup
         test-engine/test-engine
         string-constants)

(define test-display%
  (class* object% ()

    (init-field (test-object #f))
    
    (define/public (display-results src-editor)
      (let* ([current-tab (definitions-tab src-editor)]
	     [drscheme-frame (definitions-frame src-editor)]
	     [curr-win (and current-tab (send current-tab get-test-window))]
             [window (or curr-win (make-object test-window%))]
             [content (make-object (editor:standard-style-list-mixin text%))])
        
        (insert-test-results content test-object src-editor)
        (send content lock #t)
        (send window update-editor content)
        (when current-tab
          (send current-tab current-test-editor content)
          (unless curr-win
            (send current-tab current-test-window window)
            (send drscheme-frame register-test-window window)
            (send window update-switch
                  (lambda () (send drscheme-frame dock-tests)))
            (send window update-disable
                  (lambda () (send current-tab update-test-preference #f)))
            (send window update-closer
                  (lambda()
                    (send drscheme-frame deregister-test-window window)
                    (send current-tab current-test-window #f)
                    (send current-tab current-test-editor #f)))))
        (if (and drscheme-frame
		 (preferences:get 'test-engine:test-window:docked?))
            (send drscheme-frame display-test-panel content)
            (send window show #t))))

    (super-instantiate ())))

(define (definitions-tab definitions-text)
  (and definitions-text (send definitions-text get-tab)))

(define (definitions-rep definitions-text)
  (cond
   ((definitions-tab definitions-text) =>
    (lambda (tab)
      (send tab get-ints)))
   (else #f)))

(define (definitions-frame definitions-text)
  (cond
   ((definitions-tab definitions-text) =>
    (lambda (tab)
      (send tab get-frame)))
   (else #f)))

(define (insert-test-results editor test-object src-editor)
  (insert-fragment (test-object->markup test-object) editor src-editor)

  (send editor change-style
        (send (editor:get-standard-style-list) find-named-style
              (editor:get-default-color-style-name))
        0
        (send editor last-position)))

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

     newline

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
          newline)
         empty-markup)

     (cond
       ((null? violated-signatures)
        (string-constant test-engine-no-signature-violations))
       ((null? (cdr violated-signatures))
        (string-constant test-engine-1-signature-violation))
       (else
        (format (string-constant test-engine-n-signature-violations)
                (length violated-signatures))))

     newline

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
                (apply vertical
                       (map (lambda (violation)
                              (horizontal "\t"
                                          (signature-violation->markup violation)))
                            violations)))
      empty-markup))

(frame:setup-size-pref 'htdp:test-engine-window-size 400 350
                       #:position-preferences 'htdp:test-engine-window-position)

(define test-window%
  (class* (frame:size-pref-mixin frame:standard-menus%) ()

    (super-new
     [label (string-constant test-engine-window-title)]
     [size-preferences-key 'htdp:test-engine-window-size]
     [position-preferences-key 'htdp:test-engine-window-position])

    (define switch-func void)
    (define disable-func void)
    (define close-cleanup void)

    (inherit get-area-container)
    
    (define content
      (make-object canvas:color% (get-area-container) #f '(auto-vscroll)))

    (define button-panel
      (make-object horizontal-panel% (get-area-container)
                   '() #t 0 0 0 0 '(right bottom) 0 0 #t #f))

    (define buttons
      (list (make-object button%
                         (string-constant close)
                         button-panel
                         (lambda (b c)
                           (when (eq? 'button (send c get-event-type))
                             (close-cleanup)
                             (send this show #f))))
            (make-object button%
                         (string-constant dock)
                         button-panel
                         (lambda (b c)
                           (when (eq? 'button (send c get-event-type))
                             (send this show #f)
                             (preferences:set 'test-engine:test-window:docked? #t)
                             (switch-func))))
            (make-object grow-box-spacer-pane% button-panel)))

    (define/override (edit-menu:between-select-all-and-find menu) (void))
    
    (define/public (update-editor e)
      (send content set-editor e))

    (define/public (update-switch thunk)
      (set! switch-func thunk))
    (define/public (update-closer thunk)
      (set! close-cleanup thunk))
    (define/public (update-disable thunk)
      (set! disable-func thunk))))

(define test-panel%
  (class* vertical-panel% ()

    (inherit get-parent)

    (super-instantiate ())

    (define content (make-object canvas:color% this #f '()))
    (define button-panel (make-object horizontal-panel% this
                                      '() #t 0 0 0 0 '(right bottom) 0 0 #t #f))
    (define (hide)
      (let ([current-tab (send frame get-current-tab)])
        (send frame deregister-test-window 
              (send current-tab get-test-window))
        (send current-tab current-test-window #f)
        (send current-tab current-test-editor #f))
      (remove))

    (make-object button%
                 (string-constant hide)
                 button-panel
                 (lambda (b c)
                   (when (eq? 'button (send c get-event-type))
                     (hide))))
    #;(make-object button%
                 (string-constant profj-test-results-hide-and-disable)
                 button-panel
                 (lambda (b c)
                   (when (eq? 'button (send c get-event-type))
                     (hide)
                     (send (send frame get-current-tab)
                           update-test-preference #f))))
    (make-object button%
                 (string-constant undock)
                 button-panel
                 (lambda (b c)
                   (when (eq? 'button (send c get-event-type))
                     (preferences:set 'test-engine:test-window:docked? #f)
                     (send frame undock-tests))))

    (define/public (update-editor e)
      (send content set-editor e))

    (define frame #f)
    (define/public (update-frame f)
      (set! frame f))

    (define/public (remove)
      (let ([parent (get-parent)])
        (preferences:set 'test-engine:test-dock-size (send parent get-percentages))
        (send parent delete-child this)))))

#lang racket/base

;; Representation of test and signature failures

(provide (all-defined-out))

;; (make-failed-check check-fail (U #f exn) (U #f srcloc?)
(define-struct failed-check (reason exn? srcloc?))

; the src is a list (source line column position span), see check-expect-maker
(define-struct check-fail (src))

;; (make-unexpected-error src format string exn)
(define-struct (unexpected-error check-fail) (expected message exn))
(define-struct (unsatisfied-error check-fail) (expected message exn))
;; (make-unequal src scheme-val scheme-val)
(define-struct (unequal check-fail) (test actual))
;; (make-outofrange src scheme-val scheme-val inexact)
(define-struct (outofrange check-fail) (test actual range))
;; (make-incorrect-error src string exn)
(define-struct (incorrect-error check-fail) (expected message exn))
;; (make-expected-error src string scheme-val)
(define-struct (expected-error check-fail) (message value))
;; (make-expected-an-error src scheme-val)
(define-struct (expected-an-error check-fail) (value))
;; (make-not-mem src scheme-val scheme-val)
(define-struct (not-mem check-fail) (test set))
;; (make-not-range src scheme-val scheme-val scheme-val)
(define-struct (not-range check-fail) (test min max))
;; (make-satisfied-failed src scheme-val symbol)
(define-struct (satisfied-failed check-fail) (actual name))

;;Wishes
(define-struct (unimplemented-wish check-fail) (name args))

(define-struct (violated-signature check-fail) (obj signature blame))

(define-struct signature-got (value format))

(define-struct signature-violation (obj signature message srcloc blame))

(define-struct (property-fail check-fail) (result))
(define-struct (property-error check-fail) (message exn))

;; FIXME: printing stuff below potentially needs to go somewhere else

;; (make-message-error src format (listof string))
(define-struct (message-error check-fail) (strings))







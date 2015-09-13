#lang racket

(provide (all-defined-out))
(require (only-in "j-bob-lang.rkt" if/nil))

(define-syntax if
  (syntax-rules ()
				((_ Q A E)
				 (if/nil Q (lambda () A) (lambda () E)))))


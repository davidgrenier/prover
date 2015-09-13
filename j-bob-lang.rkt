#lang racket

(provide (all-defined-out))

(define (num x)
  (if (number? x) x 0))

(define (if/nil Q A E)
  (if (equal? Q 'nil) (E) (A)))

(define (atom x) (if (pair? x) 'nil 't))

(define (s.car x) (if (pair? x) (car x) '()))
(define (s.cdr x) (if (pair? x) (cdr x) '()))
(define (s.+ x y) (+ (num x) (num y)))
(define (s.< x y) (if (< (num x) (num y)) 't 'nil))

(define (equal x y) (if (equal? x y) 't 'nil))
(define (natp x)
  (if (integer? x) (if (< x 0) 'nil 't) 'nil))

(define (size x)
  (if (pair? x)
	(+ '1 (size (s.car x)) (size (s.cdr x)))
    '0))

(define-syntax defun
  (syntax-rules ()
				((_ name (arg ...) body)
				 (define (name arg ...) body))))

(define-syntax dethm
  (syntax-rules ()
				((_ name (arg ...) body)
				 (define (name arg ...) body))))

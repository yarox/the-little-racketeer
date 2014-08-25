#lang racket


(define (atom? x) 
  (not (or (pair? x) (null? x))))

(define (add1 n)
  (+ n 1))

(define (sub1 n)
  (- n 1))


(provide (all-defined-out))

#lang racket


(define (atom? x) 
  (not (or (pair? x) (null? x))))

(define (add1 n)
  (+ n 1))

(define (sub1 n)
  (- n 1))

(define (o> n m)
  (cond
    ((zero? n) #f)
    ((zero? m) #t)
    (else (o> (sub1 n) (sub1 m)))))

(define (o< n m)
  (cond
    ((zero? m) #f)
    ((zero? n) #t)
    (else (o< (sub1 n) (sub1 m)))))

(define (o= n m)
  (cond 
    ((o< n m) #f)
    ((o> n m) #f)
    (else #t)))

(define (eqan? a1 a2)
  (cond 
    ((and (number? a1) (number? a2)) (o= a1 a2))
    ((or (number? a1) (number? a2)) #f)
    (else (eq? a1 a2))))


(provide (all-defined-out))

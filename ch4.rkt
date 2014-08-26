#lang racket
(require "common.rkt")


;; Chapter 4: Numbers Games
(define (o+ n m)
  (cond 
    ((zero? m) n)
    (else (add1 (o+ n (sub1 m))))))

(define (o- n m)
  (cond 
    ((zero? m) n)
    (else (sub1 (o- n (sub1 m))))))

(define (addtup tup)
  (cond
    ((null? tup) 0)
    (else (o+ (car tup) (addtup (cdr tup))))))

(define (x n m)
  (cond
    ((zero? m) 0)
    (else (o+ n (x n (sub1 m))))))

(define (tup+ tup1 tup2)
  (cond
    ((null? tup1) tup2)
    ((null? tup2) tup1)
    (else (cons (o+ (car tup1) (car tup2)) 
                (tup+ (cdr tup1) (cdr tup2))))))

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

(define (oexpt n m)
  (cond
    ((zero? m) 1)
    (else (x n (oexpt n (sub1 m))))))

(define (oquotient n m)
  (cond 
    ((o< n m) 0)
    (else (add1 (oquotient (o- n m) m)))))

(define (length lat)
  (cond
    ((null? lat) 0)
    (else (add1 (length (cdr lat))))))

(define (pick n lat)
  (cond
    ((null? lat) '())
    ((zero? (sub1 n)) (car lat))
    (else (pick (sub1 n) (cdr lat)))))

(define (rempick n lat)
  (cond
    ((null? lat) '())
    ((zero? (sub1 n)) (cdr lat))
    (else (cons (car lat) (rempick (sub1 n) (cdr lat))))))

(define (no-nums lat)
  (cond
    ((null? lat) '())
    ((number? (car lat)) (no-nums (cdr lat)))
    (else (cons (car lat) (no-nums (cdr lat))))))

(define (all-nums lat)
  (cond
    ((null? lat) '())
    ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
    (else (all-nums (cdr lat)))))

(define (eqan? a1 a2)
  (cond 
    ((and (number? a1) (number? a2)) (o= a1 a2))
    ((or (number? a1) (number? a2)) #f)
    (else (eq? a1 a2))))

(define (occur a lat)
  (cond
    ((null? lat) 0)
    ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
    (else (occur a (cdr lat)))))

(define (one? n)
  (o= n 1))

(define (rempick-v2 n lat)
  (cond
    ((null? lat) '())
    ((one? n) (cdr lat))
    (else (cons (car lat) (rempick-v2 (sub1 n) (cdr lat))))))

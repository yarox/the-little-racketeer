#lang racket
(require "common.rkt")


;; Chapter 4: Numbers Games
(define (p+ n m)
  (cond 
    ((zero? m) n)
    (else (p+ (add1 n) (sub1 m)))))

(define (o+ n m)
  (cond 
    ((zero? m) n)
    (else (add1 (o+ n (sub1 m))))))

(define (o- n m)
  (cond 
    ((zero? m) n)
    (else (sub1 (o- n (sub1 m))))))
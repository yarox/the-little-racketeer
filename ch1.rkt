#lang racket


;; Chapter 1: Toys
(define (atom? x) 
  (not (or (pair? x) (null? x))))


(provide (all-defined-out))

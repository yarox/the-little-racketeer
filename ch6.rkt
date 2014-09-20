#lang racket
(require "common.rkt")


;; Chapter 6: Shadows
(define (numbered1? aexp)
  (cond
    ((atom? aexp) (number? aexp))
    ((eq? (car (cdr aexp) '+)) (and (numbered1? (car aexp)) 
                                    (numbered1? (car (cdr (cdr aexp))))))
    ((eq? (car (cdr aexp) '*)) (and (numbered1? (car aexp)) 
                                    (numbered1? (car (cdr (cdr aexp))))))
    ((eq? (car (cdr aexp) '^)) (and (numbered1? (car aexp)) 
                                    (numbered1? (car (cdr (cdr aexp))))))))

(define (numbered2? aexp)
  (cond
    ((atom? aexp) number? aexp)
    (else
     (and (numbered2? (car aexp))(numbered2? (car (cdr (cdr aexp))))))))

(define (value nexp)
  (cond 
    ((atom? nexp) nexp)
    ((eq? (car (cdr nexp)) '+) (+ (value (car nexp)) 
                                  (value (car (cdr (cdr nexp))))))
    ((eq? (car (cdr nexp)) '*) (* (value (car nexp)) 
                                  (value (car (cdr (cdr nexp))))))
    (else (expt (value (car nexp)) 
                (value (car (cdr (cdr nexp))))))))

(define (1st-sub-exp aexp)
  (car (cdr aexp)))

(define (2nd-sub-exp aexp)
  (car (cdr (cdr aexp))))

(define (operator aexp)
  (car aexp))

(define (value2 nexp)
  (cond
    ((atom? nexp) nexp)
    ((eq? (operator nexp) '+) (+ (value2 (1st-sub-exp nexp)) 
                                 (value2 (2nd-sub-exp nexp))))
    ((eq? (operator nexp) '*) (* (value2 (1st-sub-exp nexp)) 
                                 (value2 (2nd-sub-exp nexp))))
    (else (expt (value2 (1st-sub-exp nexp)) 
                (value2 (2nd-sub-exp nexp))))))

(define (sero? n)
  (null? n))

(define (edd1 n)
  (cons '() n))

(define (zub1 n)
  (cdr n))

(define (o+ n m)
  (cond
    ((sero? m) n)
    (else (edd1 (o+ n (zub1 m))))))

(define (hatom? x)
  (cond
    ((null? x) #t)
    ((pair? x) (and (hatom? (car x)) (hatom? (cdr x))))
    (else #f)))

(define (lat? l)
  (cond
    ((null? l) #t)
    ((hatom? (car l)) (lat? (cdr l)))
    (else #f)))

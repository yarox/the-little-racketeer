#lang racket
(require "common.rkt")


;; Chapter 5: Oh My Gawd*: It's Full of Stars
(define (rember* a l)
  (cond 
    ((null? l) '())
    ((atom? (car l)) 
     (cond
       ((eq? (car l) a) (rember* a (cdr l)))
       (else (cons (car l) (rember* a (cdr l))))))
    (else (cons (rember* a (car l)) (rember* a (cdr l))))))

(define (insertR* new old l)
  (cond 
    ((null? l) '())
    ((atom? (car l)) 
     (cond
       ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
       (else (cons (car l) (insertR* new old (cdr l))))))
    (else (cons (insertR* new old (car l)) (insertR* new old (cdr l))))))
  
(define (occur* a l)
  (cond
    ((null? l) 0)
    ((atom? (car l))
     (cond
       ((eq? (car l) a) (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
    (else (+ (occur* a (car l)) (occur* a (cdr l))))))
  
(define (subst* new old l)
  (cond 
    ((null? l) '())
    ((atom? (car l)) 
     (cond
       ((eq? (car l) old) (cons new (subst* new old (cdr l))))
       (else (cons (car l) (subst* new old (cdr l))))))
    (else (cons (subst* new old (car l)) (subst* new old (cdr l))))))   
  
(define (insertL* new old l)
  (cond 
    ((null? l) '())
    ((atom? (car l)) 
     (cond
       ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
       (else (cons (car l) (insertL* new old (cdr l))))))
    (else (cons (insertL* new old (car l)) (insertL* new old (cdr l))))))
  
(define (member* a l)
  (cond
    ((null? l) #f)
    ((atom? (car l))
     (cond
       ((eq? (car l) a) #t)
       (else (member* a (cdr l)))))
    (else (or (member* a (car l)) (member* a (cdr l))))))

(define (member2* a l)
  (cond
    ((null? l) #f)
    ((atom? (car l))
     (or (eq? (car l) a) (member2* a (cdr l))))
    (else (or (member2* a (car l)) (member2* a (cdr l))))))

(define (leftmost l)
  (cond
    ((atom? (car l)) (car l))
    (else (leftmost (car l)))))

(define (eqlist0? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((and (null? l1) (atom? (car l2))) #f)
    ((null? l1) #f)
    ((and (atom? (car l1)) (null? l2)) #f)
    ((and (atom? (car l1)) (atom? (car l2))) 
     (and (eqan? (car l1) (car l2)) (eqlist0? (cdr l1) (cdr l2))))
    ((atom? (car l1)) #f)
    ((null? l2) #f)
    ((atom? (car l2)) #f)
    (else (and (eqlist0? (car l1) (car l2)) (eqlist0? (cdr l1) (cdr l2))))))

(define (eqlist1? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    ((and (atom? (car l1)) (atom? (car l2))) 
     (and (eqan? (car l1) (car l2)) (eqlist1? (cdr l1) (cdr l2))))
    ((atom? (car l1)) #f)
    ((null? l2) #f)
    ((atom? (car l2)) #f)
    (else (and (eqlist1? (car l1) (car l2)) (eqlist1? (cdr l1) (cdr l2))))))

(define (equal0? s1 s2)
  (cond
    ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
    ((atom? s1) #f)
    ((atom? s2) #f)
    (else (eqlist1? s1 s2))))

(define (equal1? s1 s2)
  (cond
    ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
    ((or (atom? s1) (atom? s2)) #f)
    (else (eqlist1? s1 s2))))

(define (equal2? s1 s2)
  (cond
    ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
    ((or (atom? s1) (atom? s2)) #f)
    (else (eqlist2? s1 s2))))

(define (eqlist2? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    (else
     (and (equal2? (car l1) (car l2)) (eqlist2? (cdr l1) (cdr l2))))))

(define (rember s l)
  (cond
    ((null? l) '())
    ((equal? (car l) s) (cdr l))
    (else (cons (car l) (rember s (cdr l))))))

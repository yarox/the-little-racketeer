#lang racket
(require "common.rkt")
(require "ch3.rkt")


;; Chapter 7: Friends and Relations
(define (set? lat)
  (cond
    ((null? lat) #t)
    ((member? (car lat) (cdr lat)) #f)
    (else (set? (cdr lat)))))

(define (makeset lat)
  (cond
    ((null? lat) '())
    ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
    (else (cons (car lat) (makeset (cdr lat))))))

(define (makeset2 lat)
  (cond
    ((null? lat) '())
    (else (cons (car lat) (makeset2 (multirember (car lat) (cdr lat)))))))

(define (subset? set1 set2)
  (cond
    ((null? set1) #t)
    (else (and (member (car set1) set2) (subset? (cdr set1) set2)))))

(define (eqset? set1 set2)
  (and (subset? set1 set2) (subset? set2 set1)))

(define (intersect? set1 set2)
  (cond
    ((null? set1) #f)
    ((member (car set1) set2) #t)
    (else (intersect? (cdr set1) set2))))

(define (intersect2? set1 set2)
  (cond
    ((null? set1) #f)
    (else (or (member? (car set1) set2) (intersect2? (cdr set1) set2)))))

(define (intersect set1 set2)
  (cond
    ((null? set1) '())
    ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
    (else (intersect (cdr set1) set2))))

(define (union set1 set2)
  (cond
    ((null? set1) set2)
    ((member? (car set1) set2) (union (cdr set1) set2))
    (else (cons (car set1) (union (cdr set1) set2)))))

(define (intersectall l-set)
  (cond
    ((null? (cdr l-set)) (car l-set))
    (else (intersect (car l-set) (intersectall (cdr l-set))))))

(define (a-pair? x)
  (cond
    ((atom? x) #f)
    ((null? x) #f)
    ((null? (cdr x)) #f)
    ((null? (cdr (cdr x)) #t))
    (else #f)))

(define (first p)
  (car p))

(define (second p)
  (car (cdr p)))

(define (build s1 s2)
  (cons s1 (cons s2 '())))

(define (third l)
  (car (cdr (cdr l))))

(define (fun? rel)
  (set? (firsts rel)))

(define (revrel rel)
  (cond
    ((null? rel) '())
    (else (cons (build (second (car rel)) 
                       (first (car rel)))
                (revrel (cdr rel))))))

(define (revpair pair)
  (build (second pair) (first pair)))

(define (revrel2 rel)
  (cond
    ((null? rel) '())
    (else (cons (revpair (car rel)) (revrel (cdr rel))))))

;; (define (firsts l)
;;   (cond
;;     ((null? l) '())
;;    (else (cons (first (car l)) (firsts (cdr l))))))

(define (seconds l)
  (cond
    ((null? l) '())
    (else (cons (second (car l)) (seconds (cdr l))))))

(define (fullfun? fun)
  (and (set? (seconds fun))))

(define (one-to-one? fun)
  (fun? (revrel2 fun)))




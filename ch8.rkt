#lang racket
(require "common.rkt")


;; Chapter 7: Lambda the ultimate
;;(define (rember-f test? a l)
;;  (cond 
;;    ((null? l) '())
;;    ((test? (car l) a) (cdr l))
;;    (else (cons (car l)(rember-f test? a (cdr l))))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond 
        ((null? l) '())
        ((test? (car l) a) (cdr l))
        (else (cons (car l)((rember-f test?) a (cdr l))))))))

(define rember-eq (rember-f eq?))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old) (cons new (cons old (cdr l))))
        (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old) (cons old (cons new (cdr l))))
        (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))


(define insert-g
  (lambda (seq)
    (lambda (test?)
      (lambda (new old l)
        (cond
          ((null? l) '())
          ((eq? (car l) old) (seq new old (cdr l)))
          (else (cons (car l) ((insertR-f test?) new old (cdr l)))))))))

(define (seqL new old l)
  (cons new (cons old l)))

(define (seqR new old l)
  (cons old (cons new l)))

(define instertL (insert-g seqL))

(define instertR (insert-g seqR))

(define (seqS new old l) 
  (cons new l))

(define subst (insert-g seqS))

(define (atom-to-function x)
  (cond
    ((eq? x '+) +)
    ((eq? x '*) *)
    (else expt)))

(define (1st-sub-exp aexp)
  (car (cdr aexp)))

(define (2nd-sub-exp aexp)
  (car (cdr (cdr aexp))))

(define (value nexp)
  (cond
    ((atom? nexp) nexp)
    (else ((atom-to-function nexp) (value (1st-sub-exp nexp)) 
                                   (value (2nd-sub-exp nexp))))))

(define (multirember-f test?)
  (lambda (a l)
    (cond 
      ((null? l) '())
      ((test? (car l) a) ((multirember-f test?) a (cdr l)))
      (else (cons (car l)((multirember-f test?) a (cdr l)))))))

(define multirember-eq (multirember-f eq?))

(define (multiremberT test? l)
  (cond 
    ((null? l) '())
    ((test? (car l)) (multiremberT test? (cdr l)))
    (else (cons (car l)(multiremberT test? (cdr l))))))


;; (define (even? n)
;;   (= (remainder n 2) 0))

(define (even? n)
  (= (* (quotient n 2) 2) n))

(define (evens-only* l)
  (cond 
    ((null? l) '())
    ((atom? (car l)) 
     (cond
       ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
       (else (evens-only* (cdr l)))))
    (else (cons (evens-only* (car l)) (evens-only* (cdr l))))))

;; Good explanation of what's happening here:
;; http://stackoverflow.com/questions/10692449/the-little-schemer-evens-onlyco
(define evens-only*&co
 (lambda (l col)
   (cond
    ((null? l) (col '() 1 0))
    ((atom? (car l))
     (cond
      ((even? (car l))
       (evens-only*&co (cdr l)
                    (lambda (newl product sum)
                      (col (cons (car l) newl)
                           (* (car l) product)
                           sum))))
      (else
       (evens-only*&co (cdr l)
                    (lambda (newl product sum)
                      (col newl product (+ (car l) sum)))))))
    (else
     (evens-only*&co (car l)
                  (lambda (newl product sum)
                    (evens-only*&co (cdr l)
                                    (lambda (dnewl dproduct dsum)
                                      (col (cons newl dnewl)
                                           (* product dproduct)
                                           (+ sum dsum))))))))))











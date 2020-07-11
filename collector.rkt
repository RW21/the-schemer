#lang racket

; continuation passing style

(define identity
  (lambda (l col)
    (cond
      ((null? l) (col '()))
      (else (identity
             (cdr l)
             (lambda (newl)
               (col (cons (car l) newl))))))))

(define (self x) x)

(define (reverse l col)
  (cond
    [(null? l) (col '())]
    [else (reverse (cdr l)
                   (lambda (new1)
                     (col
                      (append new1
                              (list (car l))))))]))

(define (fact n)
  (if (zero? n)
      0
      (+ n (fact (sub1 n)))))

(define (fact2 n r)
  (if (zero? n)
      r
      (fact2 (sub1 n) (+ r n))))

(define (to-cps-fun op)
  (lambda (x y k) (k (op x y))))

(define (+c x y k)
  (k (+ x y)))

(define (*c x y k)
  (k (* x y)))

; we want to do 2 * 3 + 1

(define (k x) (+ x 1))

; k is the continuation

(define a (*c 2 3 k))

; 1*2 + 3*4
; (+ (* 1 2) (* 3 4))
(define a_
  (*c 1 2 (lambda (x)
            (+ x (* 3 4)))))

(define (s n)
  (/ (* n (+ n 1)) 2))

(define (/c x y k)
  (k (/ x y)))

(define (s-c n)
  (+c n 1 (lambda (x)
            (*c x n (lambda (x1)
                      (/c x1 2 self))))))

(define =c (to-cps-fun =))

(define -c (to-cps-fun -))

(define (recsum n)
  (if (zero? n)
      0
      (+ n (recsum (sub1 n)))))

(define (recsum-c n k)
  (=c n 0 (lambda (x)
            (if x
                (k 0)
                (-c n 1 (lambda (x2)
                                 (recsum-c x2 (lambda (x3)
                                                (+c n x3 k)))))))))

(define (length-c l k)
  (if (null) 

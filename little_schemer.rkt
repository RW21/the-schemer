#lang racket

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (lat? lst)
  (cond
    [(null? lst) #t]
    [(atom? (car lst)) (lat? (cdr lst))]
    [else #f]))

(define (member? a lat)
  (cond
    [(null? lat) #f]
    [else (or (eq? (car lat) a) (member? a (cdr lat)))]))

(define (member_? a lat)
  (cond
    [(null? lat) #f]
    [(eq? a (car lat)) #t]
    [else (member_? a (cdr lat))]))

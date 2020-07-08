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

(define (rember a lat)
  (cond
    [(null? lat) '()]
    [(eq? a (car lat)) (cdr lat)]
    [else (cons (car lat) (rember a (cdr lat)))]))

(define (firsts lst)
  (if
   (null? lst)
   ('())
    ((cons (car (car lst)) (firsts (cdr lst))))))

(define (firsts_ lst)
  (map (lambda (a) (car a)) lst))

(define (insertR new old lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) old) (cons old
                              (cons new (cdr lat)))]
    [else (cons (car lat) (insertR new old (cdr lat)))]))

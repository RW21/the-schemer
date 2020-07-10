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
    [(equal? a (car lat)) (cdr lat)]
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


(define (insertL new old lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) old) (cons new lat)]
    [else (cons (car lat) (insertR new old (cdr lat)))]))


(define (subst new old lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) old) (cons new (cdr lat))]
    [else (cons (car lat) (subst new old (cdr lat)))]))

(define (subst2 new o1 o2 lat)
  (cond
    [(null? lat) '()]
    [(or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat))]
    [else (cons (car lat) (subst2 new o1 o2 (cdr lat)))]))

(define (multirember a lat)
  (cond
    [(null? lat) '()]
    [(eq? a (car lat)) (multirember a (cdr lat))]
    [else (cons (car lat) (multirember a (cdr lat)))]))

(define (multiinsertR new old lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) old) (cons old
                               (cons new
                                     (multiinsertR new old (cdr lat))))]
    [else (cons (car lat)
                (multiinsertR new old (cdr lat)))])) 

(define (multiinsertL new old lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) old) (cons new
                               (cons old
                                     (multiinsertL new old (cdr lat))))]
    [else (cons (car lat) (multiinsertL new old (cdr lat)))]))

(define (multisubst new old lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) old) (cons new (multisubst new old (cdr lat)))]
    [else (cons (car lat) (multisubst new old (cdr lat)))]))

(define (+ n m)
  (if (zero? m)
      n
      (add1 (+ n (sub1 m)))))

; only consider natural numbers
(define (- n m)
  (if (zero? m)
      n
      (sub1 (- n (sub1 m)))))

(define (addtup tup)
  (cond
    [(null? tup) 0]
    [else (+ (car tup) (addtup (cdr tup)))]))

(define (* n m)
  (cond
    [(zero? m) 0]
    [else (+ n (* n (sub1 m)))]))

(define (tup+ tup1 tup2)
  (cond
    [(null? tup1) tup2]
    [(null? tup2) tup1]
    [else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))]))

(define (tup+2 tup1 tup2)
  (map + tup1 tup2))

(define (> n m)
  (cond
    [(zero? n) #f]
    [(zero? m) #t]
    [else (> (sub1 n) (sub1 m))]))

(define (< n m)
  (cond
    [(zero? m) #f]
    [(zero? n) #t]
    [else (< (sub1 n) (sub1 m))]))

(define (= n m)
  (cond
    [(> n m) #f]
    [(< n m) #f]
    [else #t]))

(define (** n m)
  (cond
    [(zero? m) 1]
    [else (* n (** n (sub1 m)))]))

(define (/ n m)
  (cond
    [(< n m) 0]
    [else (add1 (/ (- n m) m))]))

(define (length lat)
  (cond
    [(null? lat) 0]
    [else (add1 (length (cdr lat)))]))

(define (pick n lat)
  (cond
    [(zero? (sub1 n)) (car lat)]
    [else (pick (sub1 n) (cdr lat))]))

(define (rempick n lat)
  (cond
    [(one? n) (cdr lat)]
    [else (cons (car lat)
                (rempick (sub1 n) (cdr lat)))]))

(define (no-nums lat)
  (cond
    [(null? lat) '()]
    [(number? (car lat)) (no-nums (cdr lat))]
    [else (cons (car lat) (no-nums (cdr lat)))]))

(define (no-nums2 lat)
  (filter
   (lambda (a) (not (number? a)))
   lat))
    
(define (all-nums lat)
  (cond
    [(null? lat) '()]
    [(number? (car lat)) (cons (car lat)
                               (all-nums (cdr lat)))]
    [else (all-nums (cdr lat))]))

(define (all-nums2 lat)
  (filter number? lat))

(define (eqan? a1 a2)
  (cond
    [(and (number? a1) (number? a2)) (= a1 a2)]
    [(or (number? a1) (number? a2)) #f]
    [else (eq? a1 a2)]))

(define (occur a lat)
  (cond
    [(null? lat) 0]
    [(eqan? a (car lat)) (add1 (occur a (cdr lat)))]
    [else (occur a (cdr lat))]))

(define (one? n)
  (= n 1))

(define (rember* a l)
  (cond
    [(null? l) '()]
    [(atom? (car l)) (if (eqan? (car l) a)
                         (rember* a (cdr l))
                         (cons (car l) (rember* a (cdr l))))]
    [else (cons (rember* a (car l)) (rember* a (cdr l)))]))

(define (insertR* new old l)
  (cond
    [(null? l) '()]
    [(atom? (car l)) (if (eqan? (car l) old)
                   (cons old
                         (cons new (insertR* new old (cdr l))))
                   (cons (car l) (insertR* new old (cdr l))))]
    [else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))]))
              
(define (occur* a l)
  (cond
    [(null? l) 0]
    [(atom? (car l)) (if (eqan? (car l) a)
                         (add1 (occur* a (cdr l)))
                         (occur* a (cdr l)))]
    [else (+ (occur* a (car l)) (occur* a (cdr l)))]))

(define (subst* new old l)
  (cond
    [(null? l) '()]
    [(atom? (car l)) (if (eqan? (car l) old)
                         (cons new (subst* new old (cdr l)))
                         (cons (car l) (subst* new old (cdr l))))]
    [else (cons (subst* new old (car l)) (subst* new old (cdr l)))]))

(define (insertL* new old l)
  (cond
    [(null? l) '()]
    [(atom? (car l)) (if (eqan? old (car l))
                         (cons new
                               (cons (car l) (insertL* new old (cdr l))))
                         (cons (car l) (insertL* new old (cdr l))))]
    [else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))]))

(define (member* a l)
  (cond
    [(null? l) #f]
    [(atom? (car l)) (if (eqan? a (car l))
                         #t
                         (member* a (cdr l)))]
    [else (or (member* a (car l)) (member* a (cdr l)))]))

(define (leftmost l)
  (cond
    [(atom? (car l)) (car l)]
    [else (leftmost (car l))])) 

(define (eqlist? l1 l2)
  (cond
    [(and (null? l1) (null? l2)) #t]
    [(or (null? l1) (null? l2)) #f]
    [else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))]))
                                                 
(define (equal? s1 s2)
  (cond
    [(and (atom? s1) (atom? s2)) (eqan? s1 s2)]
    [(or (atom? s1) (atom? s2)) #f]
    [else (eqlist? s1 s2)]))

(define (numbered? aexp)
  (cond
    [(atom? aexp) (number? aexp)]
    [(eq? (car (cdr aexp)) (quote +)) (and
                                       (number? (car aexp))
                                       (number? (car (cdr (cdr aexp)))))]
    [(eq? (car (cdr aexp)) (quote *)) (and
                                       (number? (car aexp))
                                       (number? (car (cdr (cdr aexp)))))]
    [(eq? (car (cdr aexp)) (quote **)) (and
                                       (number? (car aexp))
                                       (number? (car (cdr (cdr aexp)))))]))

(define (value nexp)
  (cond
    [(atom? nexp) nexp]
    [(eq? (operator nexp) (quote +)) (+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))]
    [(eq? (operator nexp) (quote *)) (* (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))]
    [(eq? (operator nexp) (quote **)) (** (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))]))

(define (1st-sub-exp aexp)
  (car (cdr aexp)))

(define (2nd-sub-exp aexp)
  (car (cdr (cdr aexp))))

(define (operator aexp)
  (car aexp))

; ()
(define (sero? n)
  (null? n))

(define (edd1 n)
  (cons '() n))

(define (zub1 n)
  (cdr n))

(define (+_ n m)
  (cond
    [(sero? n) m]
    [else (edd1 (+_ (zub1 n) m))]))

(define (set? lat)
  (cond
    [(null? lat) #t]
    [(member? (car lat) (cdr lat)) #f]
    [else (set? (cdr lat))]))

(define (makeset lat)
  (cond
    [(null? lat) '()]
    [(member? (car lat) (cdr lat)) (makeset (cdr lat))]
    [else (cons (car lat) (makeset (cdr lat)))]))

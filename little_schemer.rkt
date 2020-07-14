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


(define (subst_ new old lat)
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

(define (atom-to-function x)
  (cond
    [(eq? x (quote +)) +]
    [(eq? x (quote *)) *]
    [else **]))

(define (value nexp)
  (cond
    [(atom? nexp) nexp]
    [else ((atom-to-function (operator nexp))
           (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))]))

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

(define (makeset_ lat)
  (cond
    [(null? lat) '()]
    [else (cons (car lat) (makeset_ (multirember (car lat) (cdr lat))))]))

(define (subset? set1 set2)
  (cond
    [(null? set1) #t]
    [else (and (member? (car set1) set2) (subset? (cdr set1) set2))]))

(define (eqset? set1 set2)
  (and (subset? set1 set2) (subset? set2 set1)))

(define (intersect? set1 set2)
  (cond
    [(null? set1) #f]
    [else (or (member? (car set1) set2) (intersect? (cdr set1) set2))]))

(define (intersect set1 set2)
  (cond
    [(null? set1) '()]
    [(member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2))]
    [else (intersect (cdr set1) set2)]))

(define (union set1 set2)
  (cond
    [(null? set1) set2] 
    [(member? (car set1) set2) (union (cdr set1) set2)]
    [else (cons (car set1) (union (cdr set1) set2))]))

(define (difference set1 set2)
  (cond
    [(null? set1) '()]
    [(member? (car set1) set2) (difference (cdr set1) set2)]
    [else (cons (car set1) (difference (cdr set1) set2))]))

(define (intersectall l)
  (cond
    [(null? (cdr l)) (car l)]
    [else (intersect (car l) (intersectall (cdr l)))]))

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
    [(null? rel) '()]
    [else (cons (revpair (car rel))
                (revrel (cdr rel)))]))

(define (revpair pair)
  (build (second pair) (first pair)))

(define (fullfun? fun)
  (set? (seconds fun)))

(define (seconds l)
  (map second l))

(define (one-to-one? fun)
  (fun? (revrel fun)))

(define (rember-f test?)
  (lambda (a l)
    (cond
      [(null? l) '()]
      [(test? (car l) a) (cdr l)]
      [else (cons (car l)
                  ((rember-f test?) a (cdr l)))])))

(define (eq?-c a)
  (lambda (x) (eq? x a)))

(define eq?-salad
  (eq?-c "salad"))

(define rember=? (rember-f =))

(define (insertL-f test?)
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(test? old (car l)) (cons new (cons old (cdr l)))]
      [else (cons (car l) ((insertL-f test?) new old (cdr l)))])))

(define insertL= (insertL-f =))

(define (insertR-f test?)
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(test? old (car l)) (cons old (cons new (cdr l)))]
      [else (cons (car l) ((insertR-f test?) new old (cdr l)))])))

(define (seqL new old l)
  (cons new (cons old l)))

(define (seqR new old l)
  (cons old (cons new l)))

(define (insert-g seq)
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(eq? old (car l)) (seq new old (cdr l))]
      [else (cons (car l) ((insert-g seq) new old (cdr l)))])))

(define subst
  (insert-g (lambda (new old l)
              (cons new l))))

(define (seqrem new old l) l)

(define (rember_ a l) ((insert-g seqrem) #f a l))

(define (multirember-f test?)
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [(test? a (car lat)) ((multirember-f test?) a (cdr lat))]
      [else (cons (car lat) ((multirember-f test?) a (cdr lat)))])))

(define (multiremberT test? lat)
  (cond
    [(null? lat) '()]
    [(test? (car lat)) (multiremberT test? (cdr lat))]
    [else (cons (car lat) (multiremberT test? (cdr lat)))]))

(define (a-friend x y)
  (null? y))

(define (identity x) x)

(define (multirember&co a lat k)
  (cond
    [(null? lat) (k '() '())]
    [(eq? (car lat) a) (multirember&co a (cdr lat)
                                       (lambda (newlat seen)
                                         (k newlat
                                              (cons (car lat) seen))))]
    [else (multirember&co a (cdr lat)
                          (lambda (newlat seen)
                            (k (cons (car lat) newlat) seen)))]))

(define (new-friend newlat seen)
  (a-friend newlat (cons (quote tuna) seen)))

(define (last-friend x y) (length x))

(define (multiinsertLR new oldL oldR lat)
  (cond
    [(null? lat) '()]
    [(equal? (car lat) oldL)
     (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat))))]
    [(equal? (car lat) oldR)
     (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat))))]
    [else
     (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))]))
    
(define (multiinsertLR&co new oldL oldR lat col)
  (cond
    [(null? lat)
     (col '() 0 0)]
    [(equal? (car lat) oldL)
     (multiinsertLR&co new oldL oldR (cdr lat)
                       (lambda (newlat L R)
                         (col (cons new (cons oldL newlat)) (add1 L) R)))]
    [(equal? (car lat) oldR)
     (multiinsertLR&co new oldL oldR (cdr lat)
                       (lambda (newlat L R)
                         (col (cons oldR (cons new newlat)) L (add1 R))))]
    [else (multiinsertLR&co new oldL oldR (cdr lat)
                            (lambda (newlat L R)
                              (col (cons (car lat) newlat) L R)))]))

(define (even? n) (= (* (/ n 2) 2) n))

(define (evens-only* l)
  (cond
    [(null? l) '()]
    [(atom? (car l)) (cond
                       [(even? (car l))
                        (cons (car l) (evens-only* (cdr l)))]
                       [else
                        (evens-only* (cdr l))])]
    [else (cons (evens-only* (car l)) (evens-only* (cdr l)))]))

(define (map* f l)
  (cond
    [(null? l) '()]
    [(atom? (car l)) (if (f (car l))
                         (cons (car l) (map* f (cdr l)))
                         (map* f (cdr l)))]
    [else (cons (map* f (car l)) (map* f (cdr l)))]))

(define (evens-only*_ l)
  (map* even? l))
                     
(define (evens-only*&co l col)
  (cond
    [(null? l) (col '() 1 0)]
    [(atom? (car l)) (cond
                       [(even? (car l)) (evens-only*&co (cdr l)
                                                        (lambda (newl p s)
                                                          (col (cons (car l) newl)
                                                               (* (car l) p)
                                                               s)))]
                       [else (evens-only*&co (cdr l)
                                             (lambda (newl p s)
                                               (col (cons (car l) newl)
                                                    p
                                                    (+ (car l) s))))])]
    [else (evens-only*&co (car l)
                          (lambda (al ap as)
                            (evens-only*&co (cdr l)
                                            (lambda (dl dp ds)
                                              (col (cons al dl)
                                                   (* ap dp)
                                                   (+ as ds))))))]))

(define (the-last-friend newl product sum)
  (cons sum (cons product newl)))

(define (looking a lat)
  (keep-looking a (pick 1 lat) lat))

(define (keep-looking a sorn lat)
  (cond
    [(number? sorn) (keep-looking  a (pick sorn lat) lat)]
    [else (eq? sorn a)]))


(define (shift pair)
  (build (first (first pair))
         (build (second (first pair)) (second pair))))

#lang racket

(define zun "ズン")
(define doko "ドコ")
(define kiyoshi "キ・ヨ・シ！")

(define (zundokonator)
  (if (zero? (random 2))
      zun
      doko))

; zun zun zun zun doko
(define (zundoko [a (zundokonator)]
                 [progress 4])
  (cond 
    [(and (zero? progress) (equal? a doko)) (begin (display a)
                                                   (display kiyoshi))]
    [(and (equal? a zun) (> progress 0)) (begin (display a)
                           (zundoko (zundokonator) (sub1 progress)))]
    [else (begin (display a) (zundoko))]))
    
  

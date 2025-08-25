#lang sicp

(define (foldr f v l)
  (if (null? l)
      v
      (f (car l) (foldr f v (cdr l)))))

(define (sum l)
  (foldr + 0 l))

(define (prod l)
  (foldr * 1 l))

(define (length l)
  (foldr (lambda (x y) (+ 1 y)) 0 l))

(define l (list 1 2 3 4 5))

#lang sicp

(define-syntax delay
  (syntax-rules ()
    ((delay expr) (lambda () expr))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b) (cons a (delay b)))))

(define (stream-car s)
  (car s))

(define (force promise) (promise))

(define (stream-cdr s) (force (cdr s)))

(define the-empty-stream '())

(define (stream-null? s) (eq? s the-empty-stream))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                         (stream-map proc (stream-cdr s)))))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))

(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

;(stream-ref integers 0)
;(stream-ref integers 100)

(define (divisible? x y) (= (remainder x y) 0))
(define nd7 (stream-filter (lambda (x) (not (divisible? x 7)))
                             integers))
;(stream-ref nd7 1000)

(define (fibgen a b)
    (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

;(stream-ref fibs 3)
;(stream-ref fibs 10)
;(stream-ref fibs 100)

;;; Question 3
(define (take s n)
  (if (= n 0) the-empty-stream
      (cons (stream-car s) (take (stream-cdr s) (- n 1)))))
(define infinite-squares (stream-map (lambda (x) (* x x)) integers))
(define (take-n-squares n)
  (take infinite-squares n))

;;; Question 4
(define (merge-streams str1 str2)
  (cond ((= (stream-car str1) (stream-car str2)) (cons-stream (stream-car str1) (merge-streams (stream-cdr str1) (stream-cdr str2))))
        ((> (stream-car str1) (stream-car str2)) (cons-stream (stream-car str2) (merge-streams str1 (stream-cdr str2))))
        (else (cons-stream (stream-car str1) (merge-streams (stream-cdr str1) str2)))))
(define multiple-of3 (stream-map (lambda (x) (* 3 x)) integers))
(define multiple-of5 (stream-map (lambda (x) (* 5 x)) integers))
(define merged-stream (merge-streams multiple-of3 multiple-of5))


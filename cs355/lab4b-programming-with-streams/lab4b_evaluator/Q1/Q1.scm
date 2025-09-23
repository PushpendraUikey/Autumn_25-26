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
(define integers (integers-starting-from 2))

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

;;; Question 1
(define (square x) (* x x ))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (first-n-prime n)
  (define (take k)
    (if (> k n) '()
        (if (prime? k) (cons k (take (+ k 1)))
            (take (+ k 1)))))
  (take 2))
(define (find-nth-prime-inRange rng n)
  (define (find n lst)
    (cond ((null? lst) '())
          ((= n 0) (car lst))
          (else (find (- n 1) (cdr lst)))))
  (find n (first-n-prime rng)))

(define (findnthprimeRange rng n)
  (if (> n rng) '()
      (stream-ref (stream-filter prime? integers) n)))



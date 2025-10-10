#lang sicp

;; Formulating Abstractions with higher-order Procedures
(define (cube x) (* x x x))

;; General Sum Abstraction
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (inc x) (+ x 1))
(define (sum-cubes a b)
  (sum cube a inc b))
(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))
(* 8 (pi-sum 1 100000))

;; Definite integral of a function f between the limits a and b can be approximated
;; numerically as below
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))
(integral cube 0 1 0.0001)


;; Exercise 1.29: Simpson rule indeed produces more accurate integral
(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (add-kh x) (+ x h))
  (define (simpson-sum term a next b i)
    (if (> a b)
        0
        (if (= (remainder i 2) 0)
            (+ (* 2.0 (term a)) (simpson-sum term (next a) next b (+ i 1)))
            (+ (* 4.0 (term a)) (simpson-sum term (next a) next b (+ i 1))))))
  (* (- (simpson-sum f a add-kh b 0) (+ (f a) (f b))) (/ h 3)))
(simpson-integral cube 0 1 10000)

;; Product abstraciton
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (* result (term a)))))
  (iter a 1))
(define (factorial n)
  (product identity 1 inc n))
(define (pi-approx n)
  (define (inc x) (+ x 2))
  (define (square x) (* x x))
  (if (= (remainder n 2) 0)
      (* (/ (product square 2 inc n) (* (product square 3 inc n) n 2)) 4.0)
      (* (/ (* (product square 2 inc n) n) (* (product square 3 inc n) 2)) 4.0)))
  
(product identity 1 inc 6)
(product-iter identity 1 inc 6)
(factorial 6)
(pi-approx 21)

;; Exercise 1.32: More General abstraction
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (filtered-accumulate combiner null-value pred term a next b)
  (if (> a b) null-value
      (if (pred a)
          (combiner (term a) (filtered-accumulate combiner null-value pred term (next a) next b))
          (filtered-accumulate combiner null-value pred term (next a) next b))))

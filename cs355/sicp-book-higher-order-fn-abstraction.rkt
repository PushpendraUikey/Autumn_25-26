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
;;(* 8 (pi-sum 1 100000))

;; Definite integral of a function f between the limits a and b can be approximated
;; numerically as below
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))
;;(integral cube 0 1 0.0001)


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
;;(simpson-integral cube 0 1 10000)

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
  
;; (product identity 1 inc 6)
;; (product-iter identity 1 inc 6)
;; (factorial 6)
;; (pi-approx 21)

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


;; Using `lambda` to create procedures and local variables
 (define (square x) (* x x))
;; (define (f x y)
;;  ((lambda (a b)
;;     (+ (* x (square a))
;;        (* y b)
;;        (* a b)))
;;   (+ 1 (* x y))
;;  (- 1 y)))
; The above can be alternatively written as
;; (define (f x y)
;;   (let ((a (+ 1 (* x y)))
;;         (b (- 1 y)))
;;     (+ (* x (square a))
;;        (* y b)
;;        (* a b))))

;; let allows to bind the variables as locally as possible
;; (define x 5)
;; (+ (let ((x 3))
;;  (+ x (* x 10)))
;;  x)


;; The variable's values are computed outside the let.
;; (define x 2)
;; (let ((x 3)
;;       (y (+ x 2)))
;;   (* x y))

;; These similar effects can be observed by having the internal definition
;; of these expression and then using them. However we prefer having let.
;(define (f g) (g 2))


;; Procedures as General Methods
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value) (search f neg-point midpoint))
                ((negative? test-value) (search f midpoint pos-point))
                (else midpoint))))))
(define (close-enough? x y) (< (abs (- x y)) 0.0001))
; to maintain the correctness of program additional indirection
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (positive? a-value) (negative? b-value))
           (search f b a))
          ((and (positive? b-value) (negative? a-value))
           (search f a b))
          (else (error "Values are not of opposite sign: " a b)))))
(define (average a b) (/ (+ a b) 2.0))


;; Finding Fixed Points of functions
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((fx (f guess)))
      (display "New guess: ")
      (display fx)
      (newline)
      (if (close-enough? fx guess) fx
          (try fx))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;; Average Damp
(define (average-damp g)
  (lambda (y) (average y (g y))))

;; Average Dumping makes the convergance faster.
(define golden-ratio
  (fixed-point (lambda (x) (+ 1.0 (/ 1.0 x))) 1.0))
(define golden-ration-avgDump
  (fixed-point (lambda (x) (average x (+ 1.0 (/ 1.0 x)))) 1.0))

;; k-term finite continued fraction
(define (cont-frac n d k)
   (define (cont-frac-iter ctr)
     (if (>= ctr k) (* 1.0 (/ (n ctr) (d ctr)))
         (/ (n ctr) (+ (d ctr) (cont-frac-iter (+ ctr 1))))))
   (cont-frac-iter 1))
   
(define (approximate-e iter)
  (define (ni i) 1)
  (define (di i)
    (if (= (remainder i 3) 2) (* 2 (/ (+ i 1) 3))
        1))
  (+ (cont-frac ni di iter) 2))


;; Exercise 1.39
(define (tan-cf x k)
  (define (di i) (- (* 2 i) 1))
  (define (ni i) (if (= i 1) x (expt x 2)))
  (define (cont-frac-iter ctr)
     (if (>= ctr k) (* 1.0 (/ (ni ctr) (di ctr)))
         (/ (ni ctr) (- (di ctr) (cont-frac-iter (+ ctr 1))))))
  (cont-frac-iter 1))


;;;---------- Functions as Return Values -------------
(define (deriv g)
  (define dx 0.0000001)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

;; Newton Transform equation f(x) = x - g(x)/Dg(x)
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (newton-squareroot x)
  (newtons-method (lambda (y) (- (* y y) x)) 1.0))


;;; More general abstraction of above procedures
;; Abstraction and First class procedures


(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
(define (sqrt-1 x)
  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))
(define (sqrt-2 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x)) newton-transform 1.0))


;; exercise 1.40
(define (cubic a b c) (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

;; exercise 1.41
(define (double f) (lambda (x) (f (f x))))

;; exercise 1.42
(define (compose f g) (lambda (x) (f (g x))))

;; exercise 1.43
(define (repeated f n)  
  (define (fn ctr)
    (if (>= ctr n) f (compose f (fn (+ ctr 1)))))
  (fn 1))

;; exercise 1.44
(define (smooth f)
  (define dx 0.0000001)
  (lambda (x) (/ (+ (f (+ x dx)) (f (- x dx)) (f x)) 3.0)))

(define (n-fold-smoothed f n)
  (repeated (smooth f) n))

;; exercise 1.45


;; exercise 1.46
(define (iterative-improve good-enough? improve)
  (lambda (x) (define (iter guess)
                (let ((new (improve guess)))
                  (if (good-enough? guess new) new
                      (iter new))))
    (iter x)))

(define (close-enough-iter? v1 v2)
    (< (abs (- v1 v2)) tolerance))

(define (fixed-point-iter f first-guess)
  ((iterative-improve close-enough-iter? f) first-guess)
  )

(define (sqrt-iter x)
  ((iterative-improve close-enough-iter? (lambda (y) (average y (/ x y)))) 1.0))       
(define (cube-root-iter x)
  ((iterative-improve close-enough-iter? (lambda (y) (average y (/ x (* y y))))) 1.0))


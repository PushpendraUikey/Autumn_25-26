#lang sicp

;; OverView of Object Oriented Programming.
(define (make-rat x y)
  (lambda (which) (if (= which 0) x y)))

(define n1 (make-rat 2 3))
(define n2 (make-rat 3 4))

(define (numer n) (n 0))
(define (denom n) (n 1))

(define (mult-rat x y)
  (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))


;;; Let's continue the discussion of Complex Number representation

;; Complex number using rectangular representation
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))

(define (real-part z) (car z))
(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))
(define (square x) (* x x))

;;; Polar Complex Number Representation
(define (make-from-mag-ang-pol r a) (cons r a))
(define (make-from-real-imag-pol x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
(define (magnitude-pol z) (car z))
(define (angle-pol z) (cdr z))
(define (real-part-pol z) (* (magnitude-pol z) (cos (angle-pol z))))
(define (imag-part-pol z) (* (magnitude-pol z) (sin (angle-pol z))))


;; Operations are independent of the representation
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))


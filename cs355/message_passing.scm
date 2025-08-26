#lang sicp

;;; Message Passing
;;; Instead of making operations intelligent we're making the
;;; the data objects more intelligent

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond  ((eq? op 'real-part) x)
           ((eq? op 'imag-part) y)
           ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
           ((eq? op 'angle) (atan y x))
           (else (error "Unkonwn: op: MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          ((eq? op 'real-part) (* x (cos y)))
          ((eq? op 'imag-part) (* x (sin y)))
          (else (error "Unknown: op: MAKE-FROM-REAL-IMAG" op))))
  dispatch)


(define (square x) (* x x))


(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (apply-generic op arg) (arg op))


;;; Rational class after achieving Packaging and Dispatch
(define (Rational x y)
  (lambda (msg)
    (cond ((eq? 'numer msg) x)
          ((eq? 'denom msg) y)
          ((eq? 'mult-rat msg)
           (lambda (other)
             (Rational (* x (other 'numer))
                       (* y (other 'denom))))))))

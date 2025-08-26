#lang sicp

;; Tagging

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum) (car datum))
(define (contents datum) (cdr datum))

(define (rectangular? z) (eq? (type-tag z) 'rectangular))
(define (polar? z) (eq? (type-tag z) 'polar))

(define (square x) (* x x))

;;; Attach Tags in Constructor
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'polar (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

;;; Similarly Revised Polar Representation
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x)
                             (square y)))
                    (atan y x))))
(define (make-from-mag-ang r a)
  (attach-tag 'polar (cons r a)))


;;; Now we're gonna use this tagging as the type checker
(define (real-part z)
  (cond ((rectangular? z) (real-part-rectangular z))
        ((polar? z) (real-part-polar z))
        (else (error "Unknown type: REAL-PART" z))))

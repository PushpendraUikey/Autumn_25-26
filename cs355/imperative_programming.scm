#lang sicp

;;; Imperatinve Programming

#| (define (withdraw balance amount)
     (if (>= balance amount)
         (- balance amount)
      "Insufficient balance!")) |#


;;; Attempt 2
#| (define (account-ops init-bal amounts)
     (if (null? amounts)
         init-bal
         (let ((bal (withdraw init-bal (car amounts))))
           (if (eq? bal "Insufficient balance!")
               (account-ops init-bal (cdr amounts))
            (account-ops bal (cdr amounts)))))) |#

; An assignment updates the state.
; A sequence of assignments leads to a sequence of states
; assignments are statements (Against expression)
; Each assignment is like a command to change the state.

#| (define balance 100)
   (define (withdraw amount)
     (if (>= balance amount)
         (begin (set! balance (- balance amount))
                balance)
      "Insufficient balance!")) |#

;; Withdrawal processors
#| (define (make-withdraw balance)
     (lambda (amount)
       (if (<= amount balance)
           (begin (set! balance (- balance amount))
                  balance)
           "Insufficient balance!"
           )))
 |#

;; Making more general account
(define (make-account balance)
  (define (withdraw amount)
    (if (<= amount balance)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient balance!"
        ))
  (define (deposit amount)
    (set! balance (+ balance amount))
     balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)

;; Imperative factorial program
(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* product counter))
                 (set! counter (+ counter 1))
                 (iter)))
      )
    (iter)))

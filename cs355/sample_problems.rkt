#lang sicp

;; last box of a list
(define (my-last lst)
  (if (null? lst)
      nil
      (if (null? (cdr lst))
          (car lst)
          (my-last (cdr lst)))))

;; last and second last box of a list
(define (my-but-last lst)
  (if (null? lst)
      nil
      (if (null? (cdr lst))
          nil
          (if (null? (cdr (cdr lst)))
              lst
              (my-but-last (cdr lst))))))

;; Kth element of a list
(define (element-at lst k)
  (if (<= k 0)
      nil
      (if (null? lst)
          nil
          (if (= k 1)
              (car lst)
              (element-at (cdr lst) (- k 1))))))

;; Number of elements of a list
(define (num-elements lst)
  (if (null? lst)
      0
      (+ 1 (num-elements (cdr lst)))))

;; Reversing the list
(define (reverse lst)
  (reverse-aux lst '()))

(define (reverse-aux lst tmp)
  (if (null? lst)
      tmp
      (reverse-aux (cdr lst) (cons (car lst) tmp))))

;; Checking if lst a palindrome
(define (palindrome lst)
  (define tmp (reverse lst))
  (check-equal-lst lst tmp))
(define (check-equal-lst l1 l2)
  (cond ((null? l1) (null? l2))
        ((null? l2) (null? l1))
        ((= (car l1) (car l2)) (check-equal-lst (cdr l1) (cdr l2)))
        (else #f)))

;; Flatten a nested list structure
(define (flatten lst)
  (if (null? lst)
      nil
      (let ((curr (car lst)) (rest (cdr lst))) ;Simultaneous binding
         (if (list? curr)
             (append (flatten curr) (flatten rest))
             (cons curr (flatten rest))))))



;; Eliminate Consecutive Duplicates of list elements
(define (compress lst)
  (if (null? lst)
      nil
      (compress-aux (cdr lst) (list (car lst)) (car lst))))
(define (compress-aux lst ans ele)
  (if (null? lst)
      ans
      (if (= ele (car lst))
          (compress-aux (cdr lst) ans ele)
          (compress-aux (cdr lst) (append ans (list (car lst))) (car lst)))))



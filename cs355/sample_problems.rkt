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


;; Pack Consecutive duplicates of a list elements into sublists
(define (pack lst)
  (if (null? lst)
      nil
      (cons (take-run lst) (pack (remove-run lst)))))

(define (take-run lst)
  (cond ((null? lst) '()) ;; list is empty
        ((null? (cdr lst)) lst) ;; when we reached the end of list
        ((equal? (car lst) (cadr lst)) ;; if the first ele of list is same as second
         (cons (car lst) (take-run (cdr lst))))
        (else (list (car lst))))) ;; Returning it in the form of list!

(define (remove-run lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) '())
        ((equal? (car lst) (cadr lst)) ;; if first and second ele same then check 
         (remove-run (cdr lst))) ;; recursively for next consecuents
        (else (cdr lst))))


;;;;;;;;; Problem 10: Run length encoding
(define (encode lst)
  (if (null? lst)
      nil
      (cons (list (take-run-cnt lst 0) (car lst)) (encode (remove-run lst)))))
(define (take-run-cnt lst cnt)
  (cond ((null? lst) cnt)
        ((null? (cdr lst)) (+ cnt 1))
        ((equal? (car lst) (cadr lst))
         (take-run-cnt (cdr lst) (+ cnt 1)))
        (else (+ cnt 1))))

;;;; Problem 11: Run length encoding with keeping unrepeated element as it is
(define (encode-modified lst)
  (if (null? lst)
      nil
      (let ((cnt (take-run-cnt lst 0)))
        (if (= cnt 1)
            (cons (car lst) (encode-modified (remove-run lst)))
            (cons (list cnt (car lst)) (encode-modified (remove-run lst)))))))

;;; Problem 12: Decode a run length encoded list
(define (decode lst)
  (cond ((null? lst) nil)
        ((list? (car lst))
         (let ((num (car (car lst))) ; first element (the count)
               (data (cadr (car lst)))) ; second element (the actual symbol) if cdr is used instead it gives char as '(char)'
           (append (generate-lst data num)
                 (decode (cdr lst)))))
        (else (cons (car lst) (decode (cdr lst))))))
(define (generate-lst data num)
  (if (= num 0)
      nil
      (cons data (generate-lst data (- num 1)))))


;;; Problem 13: Run length encoding of a list (direct solution)
(define (encode-direct lst)
  (define (encode lst cnt)
    (cond
      ((null? lst) '())
      ((null? (cdr lst))
       (if (= 1 (+ cnt 1))
           (list (car lst))
           (list (list (+ cnt 1) (car lst)))))
      ((equal? (car lst) (cadr lst))
       (encode (cdr lst) (+ cnt 1)))
      (else
       (if (= 1 (+ cnt 1))
           (cons (car lst) (encode (cdr lst) 0))
           (cons (list (+ cnt 1) (car lst)) (encode (cdr lst) 0))))))
  (encode lst 0))


;; Problem 14: Duplicate the list
(define (dupli lst)
  (if (null? lst)
      nil
      (append (list (car lst) (car lst)) (dupli (cdr lst)))))


;;; Problem 15: Replicate the list
(define (repli lst num)
  (if (null? lst)
      nil
      (append (generate-lst (car lst) num) (repli (cdr lst) num))))

;;; Problem 16: Drop every nth character from the list
(define (drop lstd n)
  (define (drop-aux lst cnt)
    (cond ((null? lst) nil)
          ((= cnt n) (drop-aux (cdr lst) 1))
          (else (cons (car lst) (drop (cdr lst) (+ cnt 1))))))
  (drop-aux lstd 1))


;;; Problem 17: Split a list into two parts
(define (split lst num)
  (define (aux-split new ori num)
    (if (or (null? lst) (= num 0))
        (cons new (list ori))
        (aux-split (append new (list (car ori))) (cdr ori) (- num 1))))
  (aux-split nil lst num))

;;; Problem 18: Extract Slice from a list.
(define (slice lst b e)
  (define (slice-aux lst new b e ctr)
    (cond ((null? lst) new)
          ((> ctr e) new)
          ((and (>= ctr b) (<= ctr e)) (slice-aux (cdr lst) (append new (list (car lst))) b e (+ ctr 1)))
          (else (slice-aux (cdr lst) new b e (+ ctr 1)))))
  (slice-aux lst '() b e 1))

;;; Problem 19: Rotate a list N places to the left.
(define (rotate lst num)
  (let ((len (length lst)))
    (let ((newlst (split lst (remainder (+ len num) len))))
      (append (cadr newlst) (car newlst)))))


;;; Problem 20: Remove the K'th element from the list
(define (remove-at lst num)
  (if (null? lst)
      nil
      (if (= num 1)
          (cdr lst)
          (cons (car lst) (remove-at (cdr lst) (- num 1))))))

;;; Problem 21: Insert an element at a given position in a list

(define (insert-at expr lst num)
  (if (null? lst)
      '()
      (if (= num 1)
          (cons expr lst)
          (cons (car lst) (insert-at expr (cdr lst) (- num 1))))))

;;; Problem 22: Create a list containing all integers withing a given integers
(define (range first last)
  (define (range-aux-incr first last)
    (if (> first last)
        '()
        (cons first (range-aux-incr (+ first 1) last))))
  
  (define (range-aux-decr first last)
    (if (< first last)
        '()
        (cons first (range-aux-decr (- first 1) last))))

  (if (> first last)
      (range-aux-decr first last)
      (range-aux-incr first last))
  )

;;; Problem 23: Extract a given number of randomly selected elements from the list
(define (pick-at-locn lst num)
  (if (= num 1)
      (car lst)
      (pick-at-locn (cdr lst) (- num 1))))

(define (rnd-select lst num)
  (if (or (null? lst) (= 0 num))
      '()
      (cons (pick-at-locn lst (+ 1 (random (length lst)))) (rnd-select lst (- num 1)))))

;;; Problem 24: Lotto Draw N different random numbers from the set 1...M
(define (lotto-select n m)
  (if (= n 0)
      '()
      (cons (+ 1 (random m)) (lotto-select (- n 1) m))))

;;; Problem 25: Generate a random permutation of a list

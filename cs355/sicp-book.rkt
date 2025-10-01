#lang sicp


(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
(define (expt-iter b n)
  (define (tmp b counter pdt)
    (if (= counter 0)
        pdt
        (tmp b (- counter 1) (* b pdt))))
  (tmp b n 1))

(define (even? n)
  (= (remainder n 2) 0))
(define (square x) (* x x))
(define (fast-expt b n)
  (if (= n 0) 1
      (if (even? n)
          (square (fast-expt b (/ n 2.0)))
          (* b (fast-expt b (- n 1))))))

;;; Exercise 1.16
;; As long as the function call (recursive or not) is the final action, the procedure is in tail position.
(define (fast-expt-iter b n)
  (define (compute a b n)
    (if (= n 0) a
        (if (even? n)
            (compute a (square b) (/ n 2.0))  ;;; This similar state invariability is seen in iterative fast exponentiation
            (compute (* a b) b (- n 1)))))
  (compute 1 b n))

;;; Exercise 1.17
#| (define (fast-expt-temp b n)
     (define (* a b)
       (if (= b 0) 0
           (+ a (* a (- b 1))))) ;; multiplication as repeated addition
     (define (double a) (* a 2))
     (define (halve a) (/ a 2.0))
  (define (fast-expt b n) |#

;;; Exercise 1.19
(define (fib n)
 (fib-iter 1 0 0 1 n))
 (define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q)); compute p′
                   (- (square (+ p q)) (square p)); compute q′
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


;;; Primality test for any number n
(define (smallest-divisor n) (find-divisor n 2))
(define (next test-divisor)
  (if (= test-divisor 2) 3 (+ test-divisor 2)))
(define (find-divisor n test-divisor)
  (if (> (square test-divisor) n) n
      (if (divides? test-divisor n) test-divisor
          (find-divisor n (next test-divisor)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (= (smallest-divisor n) n))

;;; modular exponentiation
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2.0) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
; times time check for fermat test and if every time true then return true else false
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; Exercise 1.21
;; (smallest-divisor 199)
;; (smallest-divisor 1999)
;; (smallest-divisor 19999)


(define (timed-prime-test n)
 (newline)
 (display n)
 (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
 (if (prime? n)
 (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
 (display " *** ")
 (display elapsed-time))


; Exercise 1.22
(define (test-primes-inRange a b)
  (if (> a b) '()
      (begin (timed-prime-test a) (test-primes-inRange (+ a 1) b))))
;; (test-primes-inRange 1000 1020)
;; (test-primes-inRange 10000 10020)
;; (test-primes-inRange 100000 100040)
;; (test-primes-inRange 1000000 1000050)

; Exercise 1.23 : for this exercise I've created the (next divisor)
;; (test-primes-inRange 1000 1020)
;; (test-primes-inRange 10000 10020)
;; (test-primes-inRange 100000 100040)
;; (test-primes-inRange 1000000 1000050)


;; Exercise 1.24 : primality test using fermat fast prime check
(define (timed-prime-test-fast n)
 (newline)
 (display n)
 (start-prime-test-fast n (runtime)))
(define (start-prime-test-fast n start-time)
 (if (fast-prime? n 19)
 (report-prime (- (runtime) start-time))))

(define (test-primes-inRange-fast a b)
  (if (> a b) '()
      (begin (timed-prime-test-fast a) (test-primes-inRange-fast (+ a 1) b))))
;; (test-primes-inRange-fast 1000 1020)
;; (test-primes-inRange-fast 10000 10020)
;; (test-primes-inRange-fast 100000 100040)
;; (test-primes-inRange-fast 1000000 1000050)

;; (test-primes-inRange 10000000 10000070)
;; (test-primes-inRange-fast 10000000 10000070)


;; Exercise 1.25 : Miller-Rabin Test : Somehow works a bit not fully correct
(define (expmod-miller base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (if (and (not (= base 1)) (not (= base (- m 1))) (= (remainder (square base) m) 1)) 0
             (remainder (square (expmod base (/ exp 2) m)) m)))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))
(define (Millar-Rabin-test n)
  (define (try-it a)
    (let ((val (expmod-miller a (- n 1) n)))
      (not (= val 0))))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime-millar? n times)
  (cond ((= times 0) true)
        ((Millar-Rabin-test n) (fast-prime-millar? n (- times 1)))
        (else false)))

#| ; ChatGPT
;; modular exponentiation (fast exponentiation)
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let ((half (expmod base (/ exp 2) m)))
           (remainder (* half half) m)))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

;; Miller–Rabin single test with base a
(define (miller-rabin-test n a)
  ;; factor n-1 = 2^s * d
  (define (factor-out-2 k s d)
    (if (even? d)
        (factor-out-2 (+ s 1) (/ d 2) (/ d 2))
        (list s d)))
  
  (let* ((sd (factor-out-2 0 0 (- n 1)))
         (s (car sd))
         (d (cadr sd))
         (x (expmod a d n)))
    (cond ((or (= x 1) (= x (- n 1))) #t)  ; passes
          (else
           (let loop ((r 1) (x x))
             (cond ((= r s) #f)              ; failed
                   (else
                    (let ((x2 (remainder (* x x) n)))
                      (if (= x2 (- n 1))
                          #t                 ; passes
                          (if (= x2 1)
                              #f              ; composite
                              (loop (+ r 1) x2)))))))))))

;; probabilistic primality test
(define (fast-prime-miller? n times)
  (cond ((= n 2) #t)
        ((even? n) #f)
        ((= times 0) #t)
        (else
         (let ((a (+ 2 (random (- n 3)))))  ; 2 <= a <= n-2
           (if (miller-rabin-test n a)
               (fast-prime-miller? n (- times 1))
               #f)))))
|#
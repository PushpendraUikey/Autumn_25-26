(define multiples-of-3
  (stream-map (lambda (n) (* 3 n))
              (stream-enumerate-interval 1 1000)))

(define multiples-of-5
  (stream-map (lambda (n) (* 5 n))
              (stream-enumerate-interval 1 1000)))

(define merged (merge-streams multiples-of-3 multiples-of-5))

(stream-ref merged 20)
(define multiples-of-13
  (stream-map (lambda (n) (* 13 n))
              (stream-enumerate-interval 1 1000)))

(define multiples-of-15
  (stream-map (lambda (n) (* 15 n))
              (stream-enumerate-interval 1 1000)))

(define merged (merge-streams multiples-of-13 multiples-of-15))

(stream-ref merged 10)
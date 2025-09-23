(define multiples-of-7
  (stream-map (lambda (n) (* 7 n))
              (stream-enumerate-interval 1 1000)))

(define multiples-of-9
  (stream-map (lambda (n) (* 9 n))
              (stream-enumerate-interval 1 1000)))

(define merged (merge-streams multiples-of-7 multiples-of-9))

(stream-ref merged 20)
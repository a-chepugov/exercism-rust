#lang racket

(provide encode decode)

(define alphabet "abcdefghijklmnopqrstuvwxyz")
(define digits-list (string->list "0123456789"))
(define chars-list (string->list alphabet))
(define rev-chars-list (reverse chars-list))
(define encode-table (make-hash (map (λ (c1 c2) (cons c1 c2)) chars-list rev-chars-list)))
(define decode-table (make-hash (map (λ (c1 c2) (cons c2 c1)) chars-list rev-chars-list)))

(define (encode m)
  ((compose
    (λ (chars) (string-join chars ""))
    (λ (chars)
      (map
       (λ (c i)
         (cond
           [(and (= (remainder i 5) 0) (> i 0)) (string-append " " c)]
           [else c]
           ))
       chars (range (length chars))))
    (λ (chars) (filter (λ (c) (> (string-length c) 0))chars))
    (λ (chars)
      (map
       (λ (char)
         (cond
           [(member char chars-list) (string (hash-ref encode-table char))]
           [(member char digits-list) (string char)]
           [else ""]
           ))
       chars)
      )
    string->list
    string-downcase
   ) m))

(define (decode m)
  ((compose
    (λ (s) (string-join s ""))
    (λ (chars)
      (map
       (λ (char)
         (cond
           [(member char chars-list) (string (hash-ref decode-table char))]
           [(member char digits-list) (string char)]
           [else ""]
           ))
       chars))
    string->list
    )
   m))
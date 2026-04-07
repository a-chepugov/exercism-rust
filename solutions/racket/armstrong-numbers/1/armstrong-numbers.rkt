#lang racket

(provide armstrong-number?)

(define (armstrong-number? n)
  (let* ([str (number->string n)]
         [digits (map (λ (c) (string->number (string c))) (string->list str))]
         [len (length digits)]
         [total (apply + (map (λ (digit) (expt digit len)) digits))])
    (= total n)))
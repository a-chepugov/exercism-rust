#lang racket

(provide acronym)

(define (acronym string)
  (if (string? string)
      (list->string
       (map (λ (s) (char-upcase (string-ref s 0)))
            (string-split
             (regexp-replace*
              #px"[^A-Za-z ]" 
              (string-replace string "-" " ")
              ""))))
      (error "Please implement 'acronym'")))
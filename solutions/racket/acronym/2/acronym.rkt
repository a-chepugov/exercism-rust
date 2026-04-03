#lang racket

(provide acronym)

(define (acronym string)
  (unless (string? string)
    (error "Please implement 'acronym'"))
  ((compose1
    (λ (chars) (list->string chars))
    (λ (lst) (map (λ (s) (char-upcase (string-ref s 0))) lst))
    string-split
    (λ (s) (regexp-replace* #px"[^A-Za-z ]" s ""))
    (λ (s) (string-replace s "-" " "))
  ) string)
)
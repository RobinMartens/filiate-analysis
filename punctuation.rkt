#lang racket

;; make a function to remove punctuation
(define punctuation-string "。,;、")
(define punctuation (foldr (λ(elm s) (set-add s elm)) (set) (string->list punctuation-string)))

(define (rm-punc chars punct)
  (cond
    [(empty? chars) empty]
    [else (if (set-member? punct (first chars))
              (rm-punc (rest chars) punct)
              (cons (first chars) (rm-punc (rest chars) punct)))]))

(define (remove-punctuation s)
  (list->string (rm-punc (string->list s) punctuation)))

(provide remove-punctuation)
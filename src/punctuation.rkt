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

;; make a function to replace the small square by a larger character
(define bad #\□)
(define good #\囗)

(define (substitute chars good bad)
  (cond
    [(empty? chars) empty]
    [else (if (equal? bad (first chars)) 
              (cons good (substitute (rest chars) good bad))
              (cons (first chars) (substitute (rest chars) good bad)))]))

(define (sub-square s)
  (list->string (substitute (string->list s) good bad)))

(provide remove-punctuation)
(provide sub-square)
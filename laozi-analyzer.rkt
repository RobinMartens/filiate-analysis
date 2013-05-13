#lang racket



;; some imports
(require test-engine/racket-tests)
(require racket/set)

;; import custom code
(require "levenshtein.rkt")

;; some general tests on the equality of Chinese characters
(check-expect (equal? "道可道非常道" "道可道非常道") #t)
(check-expect (equal? "道" "道") #t)
(check-expect (equal? "" "") #t)

(check-expect (equal? "道可道非常道" "道可道也非常道也") #f)
;(check-expect (equal? "" "") #f) 
;(check-expect (equal? "" "") #f)
 
 
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



;; figure out how to split a string into individual characters
(check-expect (string? "道可道非常道") #t)
(check-expect (char? #\道) #t)
(check-expect (equal? (substring "道可道非常道" 0 3) "道可道") #t)
(check-expect (equal? (string-append "道可道" "非常道") "道可道非常道") #t)

(define zhang1 "道可道,非常道。名可名,非常名。無名天地之始;有名萬物之母。故常無欲,以觀其妙;常有欲,以觀其徼。此兩者,同出而異名,同謂之玄。玄之又玄,衆妙之門。")










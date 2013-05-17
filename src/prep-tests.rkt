#lang racket


;; some imports
(require test-engine/racket-tests)
(require racket/set)

;; import custom code
(require "levenshtein.rkt")
(require "punctuation.rkt")
(require "source.rkt")

;; some general tests on the equality of Chinese characters
(check-expect (equal? "道可道非常道" "道可道非常道") #t)
(check-expect (equal? "道" "道") #t)
(check-expect (equal? "" "") #t)

(check-expect (equal? "道可道非常道" "道可道也非常道也") #f)
;(check-expect (equal? "" "") #f) 
;(check-expect (equal? "" "") #f)
 
 




;; figure out how to split a string into individual characters
(check-expect (string? "道可道非常道") #t)
(check-expect (char? #\道) #t)
(check-expect (equal? (substring "道可道非常道" 0 3) "道可道") #t)
(check-expect (equal? (string-append "道可道" "非常道") "道可道非常道") #t)

(define zhang1 "道可道,非常道。名可名,非常名。無名天地之始;有名萬物之母。故常無欲,以觀其妙;常有欲,以觀其徼。此兩者,同出而異名,同謂之玄。玄之又玄,衆妙之門。")


;; compare two variants
(define received1 "道可道,非常道。名可名,非常名。無名天地之始;有名萬物之母。故常無欲,以觀其妙;常有欲,以觀其徼。此兩者,同出而異名,同謂之玄。玄之又玄,衆妙之門")
(define hsg1 "道可道,非常道。名可名,非常名。無名,天地之始。有名,萬物之母。故常無欲,以觀其妙;常有欲,以觀其徼。此兩者,同出而異名,同謂之玄,玄之又玄,眾妙之門。")
(define mwd-jia-1 "道可道也,非恆道也。名可名也,非恆名也。無名萬物之始也;有名萬物之母也。□恆無欲也,以觀其眇;恆有欲也,以觀其所噭。兩者同出,異名同胃,玄之有玄,眾眇之□。")
(define mwd-yi-1 "《馬王堆·老子乙道經》:	道可道也,□□□□□□□□恆名也。無名萬物之始也;有名萬物之母也。故恆無欲也,□□□□;恆又欲也,以觀其所噭。兩者同出,異名同胃,玄之又玄,眾眇之門。")


;; account for similar characters


;; write a procedure to display similarities and differences

;(diff-trivial (remove-punctuation received1) 
 ;     (remove-punctuation hsg1))

;; trivially compare two texts
(define (compare chapter text1 text2)
  (let ([source1 (source text1 chapter)]
        [source2 (source text2 chapter)])
    (list source1
          (diff-trivial source1 source2)
          source2)))




;; make a list of all parallel passages

#|
(define the-chapters (list 1 5 11 16 20 25 38 42 48 56))

(define (compare-all chapters text1 text2)
  (foldr (λ(n base) (cons (compare n text1 text2) base)) empty chapters))

(define stuff (map (λ(n) (lev-compare (source "received" n)
                        (source "mawangdui-yi" n))) the-chapters))
|#


;; introduce the char-diff type
(define-struct char-diff 
  (template other operation status))

(lev-compare (source "received" 56) (source "xihan" 56))

;(lev-compare (source "mawangdui-yi" 5) (source "xihan" 5))

;;   "我 我"
;;  "亦%而"

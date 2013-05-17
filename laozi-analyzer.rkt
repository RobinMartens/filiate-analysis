#lang racket

;; some imports
(require test-engine/racket-tests)
(require racket/set)

;; import custom code
(require "src/levenshtein.rkt")
(require "src/punctuation.rkt")
(require "src/source.rkt")

;; trivially compare two texts
(define (compare chapter text1 text2)
  (let ([source1 (source text1 chapter)]
        [source2 (source text2 chapter)])
    (list source1
          (diff-trivial source1 source2)
          source2)))


;; make a list of all parallel passages


(define the-chapters (list 1 5 11 16 20 #|25|# 38 42 48 56))

(define (compare-all chapters text1 text2)
  (foldr (λ(n base) (cons (compare n text1 text2) base)) empty chapters))


;; make critical versions of all texts; lev compare everything to everything
(define (scribe-lev-comp text1 text2)
  (foldr (λ(cur all) (string-append cur "\n" all)) "" (lev-compare text1 text2)))

(define (save-lev-comp source1 source2 chapters)
  (map (λ(cur-chapter) (display-to-file (scribe-lev-comp (source source1 cur-chapter) (source source2 cur-chapter))
                                      (string-append "work/comp/" source1 "_" source2 "_" (number->string cur-chapter) ".txt")))
       chapters))
                

;; now compare everything
(define save-critical
  (λ() (begin
    (save-lev-comp "received" "mawangdui-yi" the-chapters)
    (save-lev-comp "received" "xihan" the-chapters)
    (save-lev-comp "mawangdui-yi" "xihan" the-chapters))))


;(define stuff (map (λ(n) (lev-compare (source "received" n)
 ;                       (source "mawangdui-yi" n))) the-chapters))



;; introduce the char-diff type
#|
(define-struct char-diff 
  (template other operation status))

(display (lev-compare (source "received" 56) (source "xihan" 56)))
|#
;(lev-compare (source "mawangdui-yi" 5) (source "xihan" 5))

;;   "我 我"
;;  "亦%而"


;; compute the Levenshtein distances between different versions
(define sources (list "received" "xihan" "mawangdui-yi"))


(define (cross-compare-helper outer inner)
  (cond
    [(empty? outer) empty]
    [else (cons (map (λ(t1) (lev t1 (first outer))) inner)
                (cross-compare-helper (rest outer) inner))]))

(define (cross-compare texts)
  (cross-compare-helper texts texts))

;(define test-texts (list "foo" "bar" "baz"))
;(cross-compare test-texts)

;(cross-compare (map (λ(title) (source title 1)) sources))


;; compare all three sources to one another
;; Haskell-style
(define all-lev (λ() (map (λ(chapter) (cross-compare (map 
                                                 (λ(title) (source title chapter)) 
                                                 sources))) 
                       the-chapters)))

(define (cc n) (cross-compare (map (λ(t) (source t n)) sources)))

;; find all critical editions
(define (matrix->csv ll)
  (foldr (λ(line done) 
           (string-append 
            (foldr (λ(num str) (string-append (number->string num) "," str)) "" line)
            "\n"
            done))
           ""
           ll))
 
(define cc1 (λ() (matrix->csv (cc 1))))

;(display (cc1))

;; write everything to file
(define save-all-cc (λ() (map (λ(chapter) (display-to-file (matrix->csv (cc chapter))
                                                         (string-append "work/lev" "cc_" (number->string chapter) ".csv")))
                              the-chapters)))


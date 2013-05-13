#lang racket



;; some imports
(require test-engine/racket-tests)

;; some general tests on the equality of Chinese characters
(check-expect (equal? "道可道非常道" "道可道非常道") #t)
(check-expect (equal? "道" "道") #t)
(check-expect (equal? "" "") #t)

(check-expect (equal? "道可道非常道" "道可道也非常道也") #f)
;(check-expect (equal? "" "") #f) 
;(check-expect (equal? "" "") #f)
 
 
;; make a function to remove punctuation
(define punctuation (list ))


;; figure out how to split a string into individual characters
(check-expect (string? "道可道非常道") #t)
(check-expect (char? #\道) #t)
(check-expect (equal? (substring "道可道非常道" 0 3) "道可道") #t)
(check-expect (equal? (string-append "道可道" "非常道") "道可道非常道") #t)

(test)


;; convert String to list of integers
(define (string->integer-list s)
  (map char->integer (string->list s)))

;; some utility functions
(define (++ x) (+ x 1))
(define (-- x) (- x 1))

;; now compute the LED
;; (list char?) (list char?) -> integer
(define (led a b len-a len-b ht)
  (let ([key (append a (cons #\0 b))]) 
    (cond
      [(zero? (min len-a len-b)) 0] 
      [(hash-has-key? ht key) (hash-ref ht key)]
      [else (let ([cur-led (min (++ (led (rest a) b (-- len-a) len-b ht))
                                (++ (led a (rest b) len-a (-- len-b) ht))
                                (+ (led (rest a) (rest b) (-- len-a) (-- len-b) ht)
                                   (if (equal? (first a) (first b)) 0 1)))])
            (begin (hash-set! ht key cur-led)
                   cur-led))])))
      
;; string? string? -> integer
(define (lev text-a text-b)
  (led (string->list text-a) 
       (string->list text-b) 
       (string-length text-a) 
       (string-length text-b)
       (make-hash)))

;; test the levenshtein edit distance computer



;; optimize for efficiency


;; work with hash tables
(define ht (make-hash))
(hash-set! ht "apple" '(red round))
(hash-set! ht "banana" '(yellow long))
#lang racket

;; some imports
(require test-engine/racket-tests)

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
      [(zero? (min len-a len-b)) (max len-a len-b)] 
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
;; identity
(check-expect (zero? (lev "hello" "hello")) #t)

;; empty string
(check-expect (equal? (lev "hello" "") 5) #t)
(check-expect (equal? (lev "" "world") 5) #t)

;; append 1
(check-expect (equal? (lev "hell" "hello") 1) #t)

;; delete 1
(check-expect (equal? (lev "hello" "hell") 1) #t)

;; substitute 1
(check-expect (equal? (lev "jello" "hello") 1) #t)

;; run tests
(test)

;; export lev
(provide lev)
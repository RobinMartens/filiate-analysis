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
  (begin 
   ; (display a)
    ;(display b)
    ;(display len-a)
    ;(display len-b)
  (let ([key (append a (cons #\0 b))]) 
    (cond
      [(zero? (min len-a len-b)) (max len-a len-b)] 
      [(hash-has-key? ht key) (hash-ref ht key)]
      [else (let ([cur-led (min (++ (led (rest a) b (-- len-a) len-b ht))
                                (++ (led a (rest b) len-a (-- len-b) ht))
                                (+ (led (rest a) (rest b) (-- len-a) (-- len-b) ht)
                                   (if (equal? (first a) (first b)) 0 1)))])
            (begin (hash-set! ht key cur-led)
                   cur-led))]))))
    

;; define a few constants that indicate what is happening to the text

(define ag #\*)
(define trunc #\-)
(define ins #\+)
(define sub #\%)

(define testing #\$)

(define neutral #\space)


#|
(define ag #\同)
(define trunc #\減)
(define ins #\加)
(define sub #\變)

(define testing #\$)

(define neutral #\空)
|#

;; collect all delimiters into a set
(define delimiter-list (list ag trunc ins sub testing))
(define delimiters (foldr (λ(elm s) (set-add s elm)) (set) delimiter-list))

;; shall put the character provided in between any two characters from the list
(define (mix-in char l)
  (cond
    [(empty? l) empty]
    [else (cons char (cons (first l)(mix-in char (rest l))))]))

;; corresponds to removing the first character
(define (truncate raw template len-raw len-template ht)
  (++ (led (rest raw) template (-- len-raw) len-template ht)))

;; corresponds to adding a chracters
(define (insert raw template len-raw len-template ht)
  (++ (led raw (rest template) len-raw (-- len-template) ht)))

;; corresponds to substituting a character
(define (substitute raw template len-raw len-template ht)
  (++ (led (rest raw) (rest template) (-- len-raw) (-- len-template) ht)))

;; conditionally space out stuff
(define (cond-delim chars prev)
  (cond
    [(empty? chars) empty]
    [else (if (or (set-member? delimiters (first chars)) (set-member? delimiters prev))
              (cons (first chars) (cond-delim (rest chars) (first chars)))
              (cons neutral (cons (first chars) (cond-delim (rest chars) (first chars)))))]))

;; this function shall display the minimum difference between two texts
;; (list char?) (list char?) -> (list char?)
(define (led-diff raw template len-raw len-template ht)
  (let ([min-led (led raw template len-raw len-template ht)])
    (cond
      [(empty? raw) (mix-in ins template)]
      [(empty? template) (mix-in trunc raw)]
      [(equal? (first raw) (first template)) (cons (first raw) (led-diff (rest raw) (rest template) 
                                                                                  (-- len-raw) (-- len-template) ht))]
      [(equal? (truncate raw template len-raw len-template ht) min-led) (cons trunc (led-diff (rest raw) template
                                                                                              (-- len-raw) len-template ht))]
      [(equal? (insert raw template len-raw len-template ht) min-led) (cons ins (cons (first template) (led-diff raw (rest template)
                                                                                                            len-raw (-- len-template) ht)))]
      [(equal? (substitute raw template len-raw len-template ht) min-led) (cons sub (cons (first raw) (led-diff (rest raw) (rest template)
                                                                                                                (-- len-raw) (-- len-template) ht)))]                                                                
      [else (cons testing (cons (first raw) (led-diff (rest raw) (rest template)
                                                      (-- len-raw) (-- len-template) ht)))])))

;; directly construct comparision list
(define (led-comp raw template len-raw len-template ht)
  (let ([min-led (led raw template len-raw len-template ht)])
    (cond
      [(empty? raw) (mix-in ins template)]
      [(empty? template) (mix-in trunc raw)]
      [(equal? (first raw) (first template)) (cons (list (first raw) neutral (first template)) (led-comp (rest raw) (rest template) 
                                                                                  (-- len-raw) (-- len-template) ht))]
      [(equal? (truncate raw template len-raw len-template ht) min-led) (cons (list neutral trunc (first template)) (led-comp (rest raw) template
                                                                                              (-- len-raw) len-template ht))]
      [(equal? (insert raw template len-raw len-template ht) min-led) (cons (list neutral ins (first template)) (led-comp raw (rest template)
                                                                                                            len-raw (-- len-template) ht))]
      [(equal? (substitute raw template len-raw len-template ht) min-led) (cons (list (first raw) sub (first template)) (led-comp (rest raw) (rest template)
                                                                                                                (-- len-raw) (-- len-template) ht))]                                                                
      [else (cons testing (cons (first raw) (led-comp (rest raw) (rest template)
                                                      (-- len-raw) (-- len-template) ht)))])))

;; wrapper for strings
(define (lev-comp raw template)
  (led-comp (string->list raw) (string->list template)
            (string-length raw) (string-length template)
            (make-hash)))


(define (lev-diff raw template)
  (list->string (cond-delim (led-diff (string->list raw) (string->list template)
                          (string-length raw) (string-length template)
                          (make-hash)) neutral)))

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



;; make a trivial implementation of diff first
(define (adapt-trivial a b)
  (cond
    [(empty? a) b]
    [(empty? b) a]
    [else (if (equal? (first a) (first b))
              (cons (first a) (adapt-trivial (rest a) (rest b)))
              (cons #\X (adapt-trivial (rest a) (rest b))))]))

(define (diff-trivial a b)
  (list->string (adapt-trivial (string->list a) (string->list b))))


;; run tests
(test)

;; export lev
(provide lev)

;; export diff
(provide diff-trivial)
(provide lev-diff)


(require "source.rkt")

(define rec (source "received" 1))
(define mwd (source "mawangdui-yi" 1))

(define (find-diff template raw)
  (list (list->string (mix-in neutral (string->list raw))) 
        (lev-diff raw template) 
        (list->string (mix-in neutral (string->list template)))))

;; run some tests
(define my-data (lev-comp rec mwd))

(define (pack-lev-comp data)
  (map (λ(l) (list->string l)) data))

(provide find-diff)
(provide lev-comp)

(pack-lev-comp my-data)

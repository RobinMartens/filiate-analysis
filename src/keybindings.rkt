#lang s-exp framework/keybinding-lang
 
(define modifiers
  (apply string-append
         (map (λ (p)
                (case p
                  [(ctl) "c:"] [(cmd) "d:"] [(alt) "m:"]
                  [(shift) "s:"] [(option) "a:"]))
              (get-default-shortcut-prefix))))
#lang racket
(provide (all-defined-out))

(define alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
(define numbers '(one two three four five))

;string-position : number number -> symbol
(define (string-position n len)
  (cond
    [(= n 0) 'leftmost]
    [(= n (- len 1)) 'rightmost]
    [(= n (/ ( - len 1) 2)) 'middle]
    [else #f]))
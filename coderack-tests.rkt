#lang racket
(require test-engine/racket-tests)
(require "coderack.rkt")

(define example-codelet1 (make-codelet  (lambda(x) (make-codelet add1 (list x) 0)) (list 1) 0))
(define example-codelet2 (make-codelet * (list 1 2 3) 0))
(define example-codelet3 (make-codelet + (list 5 8) 0))
(define example-codelet4 (make-codelet * (list 4 4) 0))
(define example-coderack (make-coderack empty empty (list example-codelet1 example-codelet2)))

;codelet-run
(check-expect (codelet-run (codelet-run example-codelet1)) 2)
(check-expect (codelet-run example-codelet2) 6)
(check-expect (codelet-run example-codelet3) 13)
(check-expect (codelet-run example-codelet4) 16)

;coderack-copy
(check-expect (coderack-copy example-coderack) (make-coderack empty empty (list example-codelet1 example-codelet2)))

;coderack-codelet-to-index
(check-expect (coderack-codelet-to-index example-coderack example-codelet1) 0)
(check-expect (coderack-codelet-to-index example-coderack example-codelet2) 1)
(check-error (coderack-codelet-to-index example-coderack example-codelet3))

;coderack-index-to-codelet
(check-expect (coderack-index-to-codelet example-coderack 0) example-codelet1)
(check-expect (coderack-index-to-codelet example-coderack 1) example-codelet2)
(check-error (coderack-index-to-codelet example-coderack 2))

;coderack-set
(check-expect (coderack-codelets-set example-coderack (list example-codelet1 example-codelet2 example-codelet3)) (make-coderack empty empty (list example-codelet1 example-codelet2 example-codelet3)))
(check-expect (coderack-codelets-set example-coderack (list example-codelet1)) (make-coderack empty empty (list example-codelet1)))

;coderack-codelet-add
(check-expect (coderack-codelet-add example-coderack example-codelet3) (make-coderack empty empty (list example-codelet1 example-codelet2 example-codelet3)))
(check-expect (coderack-codelet-add example-coderack example-codelet4) (make-coderack empty empty (list example-codelet1 example-codelet2 example-codelet4)))

;coderack-codelet-remove
(check-expect (coderack-codelet-remove example-coderack example-codelet1) (make-coderack empty empty (list example-codelet2)))
(check-expect (coderack-codelet-remove example-coderack example-codelet2) (make-coderack empty empty (list example-codelet1)))

;coderack-codelet-remove-index
(check-expect (coderack-codelet-remove-index example-coderack 0) (make-coderack empty empty (list example-codelet2)))
(check-expect (coderack-codelet-remove-index example-coderack 1) (make-coderack empty empty (list example-codelet1)))

;coderack-codelet-run
;(check-expect (coderack-codelet-run example-coderack example-codelet1) (make-coderack empty empty (list example-codelet2 (codelet-run example-codelet1))))
(check-expect (coderack-codelet-run example-coderack example-codelet2) (make-coderack empty empty (list example-codelet1)))

;coderack-codelet-run-index
;(check-expect (coderack-codelet-run-index example-coderack 0) (make-coderack empty empty (list example-codelet2 (codelet-run example-codelet1))))
(check-expect (coderack-codelet-run-index example-coderack 1) (make-coderack empty empty (list example-codelet1)))

(test)
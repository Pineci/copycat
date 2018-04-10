#lang racket
(require "utilities.rkt")
(require "slipnet.rkt")
(require "workspace.rkt")
(provide (all-defined-out))

(define-struct coderack (slipnet workspace codelets) #:transparent #:mutable)
(define-struct codelet (lambda args urgency) #:transparent #:mutable)

;codelet-run                       : codelet -> any/c
;coderack-copy                     : coderack -> coderack
;coderack-codelets-set             : coderack list -> coderack
;coderack-index-to-codelet         : coderack number -> codelet
;coderack-codelet-to-index         : coderack codelet -> number
;coderack-codelet-add              : coderack codelet -> coderack
;coderack-codelet-remove           : coderack codelet -> coderack
;coderack-codelet-remove-index     : coderack number -> coderack
;coderack-codelet-run              : coderack codelet -> coderack
;coderack-codelet-run-index        : coderack number -> coderack
;coderack-slipnet-workspace-update : coderack slipnet workspace -> coderack

;codelet-run : codelet -> any/c
(define (codelet-run cdlt)
  (apply (codelet-lambda cdlt) (codelet-args cdlt)))

;coderack-copy : coderack -> coderack
(define (coderack-copy rack)
  (make-coderack (coderack-slipnet rack) (coderack-workspace rack) (coderack-codelets rack)))

;coderack-codelet-to-index : coderack codelet -> number
(define (coderack-codelet-to-index rack cdlt)
  (local ((define idx (index-of (coderack-codelets rack) cdlt)))
    (cond
      [(boolean? idx) (error "Could not find the given codelet in the given coderack!")]
      [else idx])))

;coderack-index-to-codelet : coderack number -> codelet
(define (coderack-index-to-codelet rack idx)
  (list-ref (coderack-codelets rack) idx))

;coderack-codelets-set : coderack list -> coderack
(define (coderack-codelets-set rack l)
  (local ((define new-rack (coderack-copy rack)))
    (begin
      (set-coderack-codelets! new-rack l)
      new-rack)))

;coderack-codelet-add : coderack codelet -> coderack
(define (coderack-codelet-add rack cdlt)
  (coderack-codelets-set rack (append (coderack-codelets rack) (list cdlt))))

;coderack-codelet-remove-index : coderack number -> coderack
(define (coderack-codelet-remove-index rack idx)
  (local ((define-values (before after) (split-at (coderack-codelets rack) idx)))
    (coderack-codelets-set rack (append before (drop after 1)))))

;coderack-codelet-remove : coderack codelet -> coderack
(define (coderack-codelet-remove rack cdlt)
  (coderack-codelet-remove-index rack (coderack-codelet-to-index rack cdlt)))

;coderack-codelet-run : coderack codelet -> coderack
(define (coderack-codelet-run rack cdlt)
  (local ((define results (codelet-run cdlt)))
    (cond
      [(codelet? results) (coderack-codelet-add (coderack-codelet-remove rack cdlt) results)]
      [else (coderack-codelet-remove rack cdlt)])))

;coderack-codelet-run-index : coderack number -> coderack
(define (coderack-codelet-run-index rack idx)
  (coderack-codelet-run rack (coderack-index-to-codelet rack idx)))

;coderack-slipnet-workspace-update : coderack slipnet workspace -> coderack
(define (coderack-slipnet-workspace-update rack net space)
  (local ((define new-rack (coderack-copy rack)))
    (begin
      (set-coderack-slipnet! new-rack rack)
      (set-coderack-workspace! new-rack space)
      new-rack)))

;random-urgency : -> number
(define (random-urgency)
  (random 7))

;description-scout-bottom-up : coderack slipnet workspace 
(define (description-scout-bottom-up rack net space)
  (local ((define object (workspace-object-choose-random space))
          (define description (workspace-object-description-choose-random space object))
          (define property-links (links-filter-type (slipnet-links-from net (description-type description)) 'property)))
    (cond
      [(empty? property-links) (void)]
      [else (make-codelet description-strength-tester
                          (list (make-description (link-to (choose-random property-links)) (workspace-structure-to-pointer space object)))
                          (random-urgency))])))

;description-strength-tester : 
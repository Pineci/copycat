#lang racket
(provide (all-defined-out))
(require "utilities.rkt")
(require "workspace.rkt")

(define this-workspace (workspace-empty "abc" "abd" "ijk"))

(define this-workspace-letters (apply append (map (lambda(f) (workspace-letters-initialize this-workspace f)) (list workspace-initial workspace-modified workspace-target))))

(set! this-workspace (foldl (lambda(x y) (workspace-structure-add y x)) this-workspace this-workspace-letters))

(define this-workspace-letter-descriptions (apply append (map (lambda(ltr) (workspace-letter-make-descriptions this-workspace ltr)) this-workspace-letters)))

(set! this-workspace (foldl (lambda(x y) (workspace-structure-add y x)) this-workspace this-workspace-letter-descriptions))
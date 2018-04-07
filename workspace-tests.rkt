#lang racket
(require test-engine/racket-tests)
(require "workspace.rkt")

(define example-letter-initial-a (make-letter "a" (make-pointer workspace-initial 0)))
(define example-letter-initial-b (make-letter "b" (make-pointer workspace-initial 1)))
(define example-letter-initial-c (make-letter "c" (make-pointer workspace-initial 2)))
(define example-letter-modified-a (make-letter "a" (make-pointer workspace-modified 0)))
(define example-letter-modified-b (make-letter "b" (make-pointer workspace-modified 1)))
(define example-letter-modified-d (make-letter "d" (make-pointer workspace-modified 2)))
(define example-letter-target-i (make-letter "i" (make-pointer workspace-target 0)))
(define example-letter-target-j1 (make-letter "j" (make-pointer workspace-target 1)))
(define example-letter-target-j2 (make-letter "j" (make-pointer workspace-target 2)))
(define example-letter-target-k1 (make-letter "k" (make-pointer workspace-target 3)))
(define example-letter-target-k2 (make-letter "k" (make-pointer workspace-target 4)))
(define example-letter-target-k3 (make-letter "k" (make-pointer workspace-target 5)))
(define example-group-initial-abc (make-group (list (make-pointer workspace-initial 0) (make-pointer workspace-initial 1) (make-pointer workspace-initial 2))))
(define example-group-target-j (make-group (list (make-pointer workspace-target 1) (make-pointer workspace-target 2))))
(define example-description-initial-a (make-description 'letter (make-pointer workspace-initial 0)))
(define example-description-initial-b (make-description 'letter (make-pointer workspace-initial 1)))
(define example-bond-target-j (make-bond 'sameness (make-pointer workspace-target 1) (make-pointer workspace-target 2)))
(define example-correspondence-cd (make-correspondence 'successor (make-pointer workspace-initial 2) (make-pointer workspace-modified 2)))


(define example-workspace (make-workspace "abc" "abd" "ijjkkk" ""
                                          (vector example-letter-initial-a example-letter-initial-b #f example-letter-initial-c)
                                          (vector example-group-initial-abc #f)
                                          (vector example-description-initial-a #f #f)
                                          (vector example-bond-target-j #f #f)
                                          (vector example-correspondence-cd #f)
                                          (vector empty)))




;vector-index
(check-expect (vector-index (vector 1 2 3) 2) 1)
(check-expect (vector-index (vector 1 2 #f #f 3 #f) #f) 2)
(check-error (vector-index (vector 1 2 3) #f))

;vector-add
(check-expect (vector-add (vector 1 2 #f) 3) (vector 1 2 3))
(check-expect (vector-add (vector 1 2 #f #f 4 #f) 3) (vector 1 2 3 #f 4 #f))
(check-error (vector-add (vector 1 2 3) 4))
(check-error (vector-add (vector 1 2 3 #f) 1))

;vector-remove
(check-expect (vector-remove (vector 1 2 3) 2) (vector 1 #f 3))
(check-expect (vector-remove (vector 1 2 3 #f) 2) (vector 1 #f 3 #f))
(check-error (vector-remove (vector 1 2 3) 4))

;vector-remove-index
(check-expect (vector-remove-index (vector 1 2 3) 0) (vector #f 2 3))

;vector-clean
(check-expect (vector-clean (vector 1 2 3)) (vector 1 2 3))
(check-expect (vector-clean (vector #f 1 2 #f #f 3)) (vector 1 2 3))
(check-expect (vector-clean (vector #f #f #f #f)) (vector))

;structure-type
(check-expect (structure-type example-letter-initial-a) 'letter)
(check-expect (structure-type example-group-initial-abc) 'group)
(check-expect (structure-type example-description-initial-a) 'description)
(check-expect (structure-type example-bond-target-j) 'bond)
(check-expect (structure-type example-correspondence-cd) 'correspondence)
(check-error (structure-type (make-pointer workspace-initial 0)))

;structure-pointer
(check-expect (structure-pointer example-letter-initial-a) (list (letter-pointer example-letter-initial-a)))
(check-expect (structure-pointer example-group-initial-abc) (group-members example-group-initial-abc))
(check-expect (structure-pointer example-description-initial-a) (list (description-pointer example-description-initial-a)))
(check-expect (structure-pointer example-bond-target-j) (list (bond-from example-bond-target-j) (bond-to example-bond-target-j)))
(check-expect (structure-pointer example-correspondence-cd) (list (correspondence-from example-correspondence-cd) (correspondence-to example-correspondence-cd)))
(check-error (structure-pointer (make-pointer workspace-initial 0)))

;structure-vector-function
(check-expect ((structure-vector-function 'letter) example-workspace) (workspace-letters example-workspace))
(check-expect ((structure-vector-function 'group) example-workspace) (workspace-groups example-workspace))
(check-expect ((structure-vector-function 'description) example-workspace) (workspace-descriptions example-workspace))
(check-expect ((structure-vector-function 'bond) example-workspace) (workspace-bonds example-workspace))
(check-expect ((structure-vector-function 'correspondence) example-workspace) (workspace-correspondences example-workspace))
(check-error (structure-vector-function 'pointer))

;structure-set-function
(define example-workspace-duplicate1 (struct-copy workspace example-workspace))
(define example-workspace-duplicate2 (struct-copy workspace example-workspace))
(check-expect (begin ((structure-set-function 'letter) example-workspace-duplicate1 empty) example-workspace-duplicate1) (begin (set-workspace-letters! example-workspace-duplicate2 empty) example-workspace-duplicate2))
(check-expect (begin ((structure-set-function 'group) example-workspace-duplicate1 empty) example-workspace-duplicate1) (begin (set-workspace-groups! example-workspace-duplicate2 empty) example-workspace-duplicate2))
(check-expect (begin ((structure-set-function 'description) example-workspace-duplicate1 empty) example-workspace-duplicate1) (begin (set-workspace-descriptions! example-workspace-duplicate2 empty) example-workspace-duplicate2))
(check-expect (begin ((structure-set-function 'bond) example-workspace-duplicate1 empty) example-workspace-duplicate1) (begin (set-workspace-bonds! example-workspace-duplicate2 empty) example-workspace-duplicate2))
(check-expect (begin ((structure-set-function 'correspondence) example-workspace-duplicate1 empty) example-workspace-duplicate1) (begin (set-workspace-correspondences! example-workspace-duplicate2 empty) example-workspace-duplicate2))
(check-error (structure-set-function 'pointer))

;workspace-structures
(check-expect (workspace-structures example-workspace) (list example-letter-initial-a example-letter-initial-b example-letter-initial-c
                                                             example-group-initial-abc example-description-initial-a example-bond-target-j example-correspondence-cd))

;workspace-structures-type
(check-expect (workspace-structures-type example-workspace 'letter) (workspace-letters example-workspace))
(check-expect (workspace-structures-type example-workspace 'group) (workspace-groups example-workspace))
(check-expect (workspace-structures-type example-workspace 'description) (workspace-descriptions example-workspace))
(check-expect (workspace-structures-type example-workspace 'bond) (workspace-bonds example-workspace))
(check-expect (workspace-structures-type example-workspace 'correspondence) (workspace-correspondences example-workspace))
(check-error (workspace-structures-type example-workspace 'pointer))

;workspace-structures-set
(set! example-workspace-duplicate1 (struct-copy workspace example-workspace))
(set! example-workspace-duplicate2 (struct-copy workspace example-workspace))
(check-expect (begin (workspace-structures-set example-workspace-duplicate1 'letter (vector 1 2 3)) example-workspace-duplicate1) (begin (set-workspace-letters! example-workspace-duplicate2 (vector 1 2 3)) example-workspace-duplicate2))
(check-expect (begin (workspace-structures-set example-workspace-duplicate1 'group (vector 1 2 3)) example-workspace-duplicate1) (begin (set-workspace-groups! example-workspace-duplicate2 (vector 1 2 3)) example-workspace-duplicate2))
(check-expect (begin (workspace-structures-set example-workspace-duplicate1 'description (vector 1 2 3)) example-workspace-duplicate1) (begin (set-workspace-descriptions! example-workspace-duplicate2 (vector 1 2 3)) example-workspace-duplicate2))
(check-expect (begin (workspace-structures-set example-workspace-duplicate1 'bond (vector 1 2 3)) example-workspace-duplicate1) (begin (set-workspace-bonds! example-workspace-duplicate2 (vector 1 2 3)) example-workspace-duplicate2))
(check-expect (begin (workspace-structures-set example-workspace-duplicate1 'correspondence (vector 1 2 3)) example-workspace-duplicate1) (begin (set-workspace-correspondences! example-workspace-duplicate2 (vector 1 2 3)) example-workspace-duplicate2))
(check-error (workspace-structures-set example-workspace 'pointer (vector 1 2 3)))

;workspace-structure-index
(check-expect (workspace-structure-index example-workspace example-letter-initial-c) 3)
(check-expect (workspace-structure-index example-workspace example-group-initial-abc) 0)
(check-expect (workspace-structure-index example-workspace example-description-initial-a) 0)
(check-expect (workspace-structure-index example-workspace example-bond-target-j) 0)
(check-expect (workspace-structure-index example-workspace example-correspondence-cd) 0)
(check-error (workspace-structure-index example-workspace example-letter-target-k1))
(check-error (workspace-structure-index example-workspace example-group-target-j))

;workspace-structure-add
(check-expect (workspace-letters (workspace-structure-add example-workspace example-letter-target-i)) (vector example-letter-initial-a example-letter-initial-b example-letter-target-i example-letter-initial-c))
(check-expect (workspace-groups (workspace-structure-add example-workspace example-group-target-j)) (vector example-group-initial-abc example-group-target-j))
(check-expect (workspace-descriptions (workspace-structure-add example-workspace example-description-initial-b)) (vector example-description-initial-a example-description-initial-b #f))

;workspace-structure-remove
(check-expect (workspace-letters (workspace-structure-remove example-workspace example-letter-initial-a)) (vector #f example-letter-initial-b #f example-letter-initial-c))
(check-expect (workspace-groups (workspace-structure-remove example-workspace example-group-initial-abc)) (vector #f #f))
(check-expect (workspace-descriptions (workspace-structure-remove example-workspace example-description-initial-a)) (vector #f #f #f))
(check-expect (workspace-bonds (workspace-structure-remove example-workspace example-bond-target-j)) (vector #f #f #f))
(check-expect (workspace-correspondences (workspace-structure-remove example-workspace example-correspondence-cd)) (vector #f #f))

;workspace-pointer-to-structure
(check-expect (workspace-pointer-to-structure example-workspace (make-pointer workspace-letters 3)) example-letter-initial-c)
(check-expect (workspace-pointer-to-structure example-workspace (make-pointer workspace-groups 0)) example-group-initial-abc)
(check-expect (workspace-pointer-to-structure example-workspace (make-pointer workspace-descriptions 0)) example-description-initial-a)
(check-expect (workspace-pointer-to-structure example-workspace (make-pointer workspace-bonds 0)) example-bond-target-j)
(check-expect (workspace-pointer-to-structure example-workspace (make-pointer workspace-correspondences 1)) #f)

;workspace-pointer-to-string
(check-expect (workspace-pointer-to-string example-workspace (make-pointer workspace-initial 0)) "a")
(check-expect (workspace-pointer-to-string example-workspace (make-pointer workspace-modified 1)) "b")
(check-expect (workspace-pointer-to-string example-workspace (make-pointer workspace-modified 2)) "d")
(check-expect (workspace-pointer-to-string example-workspace (make-pointer workspace-target 3)) "k")
(check-expect (workspace-pointer-to-string example-workspace (make-pointer workspace-target 5)) "k")

;workspace-structure-to-pointer
(check-expect (workspace-pointer-to-structure example-workspace (workspace-structure-to-pointer example-workspace example-letter-initial-c)) example-letter-initial-c)
(check-expect (workspace-pointer-to-structure example-workspace (workspace-structure-to-pointer example-workspace example-group-initial-abc)) example-group-initial-abc)
(check-expect (workspace-pointer-to-structure example-workspace (workspace-structure-to-pointer example-workspace example-description-initial-a)) example-description-initial-a)
(check-expect (workspace-pointer-to-structure example-workspace (workspace-structure-to-pointer example-workspace example-bond-target-j)) example-bond-target-j)
(check-expect (workspace-pointer-to-structure example-workspace (workspace-structure-to-pointer example-workspace example-correspondence-cd)) example-correspondence-cd)

;workspace-pointer-search
(check-expect (workspace-pointer-search example-workspace (make-pointer workspace-initial 0)) (list example-letter-initial-a example-group-initial-abc example-description-initial-a))
(check-expect (workspace-pointer-search example-workspace (make-pointer workspace-initial 1)) (list example-letter-initial-b example-group-initial-abc))
(check-expect (workspace-pointer-search example-workspace (make-pointer workspace-modified 2)) (list example-correspondence-cd))
(check-expect (workspace-pointer-search example-workspace (make-pointer workspace-target 1)) (list example-bond-target-j))

;workspace-letters-initialize
;(workspace-letters-initialize example-workspace workspace-initial) (list example-letter-initial-a example-letter-initial-b example-letter-initial-c)
;(workspace-letters-initialize example-workspace workspace-modified) (list example-letter-modified-a example-letter-modified-b example-letter-modified-d)

;workspace-letter-make-descriptions
;(workspace-letter-make-descriptions example-workspace example-letter-initial-a) (list (make-description 'a (make-pointer workspace-letters 0))
;                                                                                      (make-description 'letter-category (make-pointer workspace-letters 0))
;                                                                                      (make-description 'leftmost (make-pointer workspace-letters 0)))
;(workspace-letter-make-descriptions example-workspace example-letter-initial-b) (list (make-description 'b (make-pointer workspace-letters 1))
;                                                                                      (make-description 'letter-category (make-pointer workspace-letters 1))
;                                                                                      (make-description 'middle (make-pointer workspace-letters 1)))


(test)
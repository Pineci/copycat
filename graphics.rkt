#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "workspace.rkt")
(require "workspace-initialization.rkt")

(define small-font 12)
(define big-font 40)
(define basic-width 60)
(define basic-height 40)
(define big-height 100)
(define small-space (rectangle basic-width basic-height 'solid 'white))
(define big-space (rectangle basic-width big-height 'solid 'white))
(define arrow (overlay (text "->" big-font 'black) big-space))

;descriptions-to-image : string alodesc -> image
(define (descriptions-to-image ltr alodesc)
  (overlay
   (foldr (lambda (d r)
            (above (text (symbol->string (description-type d)) small-font 'black) r))
          (text ltr big-font 'black) alodesc)
   big-space))

;workspace-string-pointer-to-image : workspace pointer -> scene
(define (workspace-string-pointer-to-image space ptr)
  (local ((define this-letter (car (filter letter? (workspace-pointer-search space ptr)))))
    (descriptions-to-image (letter-string this-letter)
                           (filter description? (workspace-pointer-search space (workspace-structure-to-pointer space this-letter))))))

;workspace-graphics-x-pos : workspace pointer number -> number
(define (workspace-graphics-x-pos space ptr offset)
  (local ((define traced-pointer (workspace-pointer-trace space ptr))
          (define x-pos (* (+ 0.5 (pointer-index traced-pointer)) basic-width)))
    (cond
      [(equal? (pointer-target traced-pointer) workspace-modified) (+ x-pos offset)]
      [else x-pos])))

;workspace-graphics-y-pos : workspace pointer -> number
(define (workspace-graphics-y-pos space ptr)
  (local ((define type (pointer-target (workspace-pointer-trace space ptr))))
         (cond
          [(or (equal? type workspace-initial) (equal? type workspace-modified)) basic-height]
          [(equal? type workspace-target) (+ basic-height (* 2 big-height))])))

;workspace-graphics-posn : workspace pointer number-> number number
(define (workspace-graphics-posn space p x-offset)
  (values (workspace-graphics-x-pos space p x-offset)
          (workspace-graphics-y-pos space p)))


;workspace-overlay-group : workspace scene group -> scene
(define (workspace-overlay-group space scene grp)
  (local ((define type (pointer-target (first (group-members grp))))
          (define x0 (workspace-graphics-x-pos space (first (group-members grp)) 0))
          (define x1 (workspace-graphics-x-pos space (last (group-members grp)) 0)))
    (place-image/align (rectangle (+ (- x1 x0 1) basic-width) (- big-height 1) "outline" "black")
                       (- x0 (* .5 basic-width)) (/ big-height 2)
                       "left" "center"
                       scene)))

;workspace-overlay-groups : workspace scene alogroup -> scene
(define (overlay-groups space scene groups)
  (foldr (lambda (group scene-l) (workspace-overlay-group space scene-l group))
         scene groups))

;workspace-pointer-target-to-groups : workspace function -> alogroup
(define (workspace-pointer-target-to-groups space func)
  (filter (lambda (grp)
             (equal? (pointer-target (first (group-members grp))) func))
           (vector->list (vector-clean (workspace-groups space)))))

;workspace-string-function-to-scene : workspace function -> scene
(define (workspace-string-function-to-scene space func)
  (overlay-groups space
   (apply beside (map (lambda (ptr) (workspace-string-pointer-to-image space ptr))
                      (build-list (string-length (func space)) (lambda (x) (make-pointer func x)))))
   (workspace-pointer-target-to-groups space func)))

;extract-to-from : bond/corr -> pointer pointer
(define (extract-to-from obj)
  (cond
   [(correspondence? obj) (values (correspondence-from obj) (correspondence-to obj))]
   [(bond? obj) (values (bond-from obj) (bond-to obj))]
   [else (println obj) (error "Not given a Correspondence or Bond!")]))

;workspace-draw-connection : workspace scene number bond/correspondence/desc -> scene
(define (workspace-draw-connection space scene offset connection)
  (local ((define-values (from to) (extract-to-from connection))
          (define-values (x1 y1) (workspace-graphics-posn space from offset))
          (define-values (x2 y2) (workspace-graphics-posn space to offset))
          )
         (add-curve scene
                    x1 y1
                    25 1/3
                    x2 y2
                    -25 1/3 "black")))

;workspace-draw-connections : workspace scene number -> scene
(define (workspace-draw-connections space scene offset)
  (foldr (lambda (connection img)
                 (workspace-draw-connection space img offset connection))
               scene (vector->list (vector-clean (vector-append (workspace-bonds space) (workspace-correspondences space))))))

;create-scene : workspace -> scene
(define (create-scene space)
  (local ((define initial-scene (workspace-string-function-to-scene space workspace-initial))
          (define modified-scene (workspace-string-function-to-scene space workspace-modified))
          (define target-scene (workspace-string-function-to-scene space workspace-target))
          (define initial-width (image-width initial-scene))
          (define basic-scene (above/align "left" small-space
                                           (beside initial-scene arrow modified-scene)
                                           big-space
                                           (beside target-scene arrow))))
    (workspace-draw-connections space basic-scene initial-width)))

;(create-scene this-workspace)
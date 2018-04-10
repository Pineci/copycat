#lang racket
(require "utilities.rkt")
(provide (all-defined-out))

(define-struct pointer (target index) #:transparent #:mutable)

(define-struct letter (string pointer) #:transparent #:mutable)
(define-struct group (members) #:transparent #:mutable)
(define-struct description (type pointer) #:transparent #:mutable)
(define-struct bond (type from to) #:transparent #:mutable)
(define-struct correspondence (type from to) #:transparent #:mutable)



(define-struct workspace (initial modified target answer letters groups descriptions bonds correspondences rules) #:transparent #:mutable)

;vector-index                            : vector any/c -> number
;vector-add                              : vector any/c -> vector
;vector-remove                           : vector any/c -> vector
;vector-remove-index                     : vector number -> vector
;vector-clean                            : vector -> vector

;structure-type                          : (or letter? group? description? bond? correspondence?) -> symbol
;structure-pointer                       : (or letter? group? description? bond? correspondence?) -> alopointer
;structure-vector-function               : symbol -> function
;structure-set-function                  : symbol -> function
;workspace-empty                         : string string string -> workspace
;workspace-copy                          : workspace -> workspace
;workspace-structures                    : workspace -> list
;workspace-structures-type               : workspace symbol -> vector
;workspace-structures-set                : workspace symbol vector -> void
;workspace-structure-index               : workspace (or letter? group? description? bond? correspondence?) -> number
;workspace-structure-add                 : workspace (or letter? group? description? bond? correspondence?) -> workspace
;workspace-structure-remove              : workspace (or letter? group? description? bond? correspondence?) -> workspace
;workspace-structure-to-pointer          : workspace (or letter? group? description? bond? correspondence?) -> pointer
;workspace-pointer-to-structure          : workspace pointer -> (or letter? group? description? bond? correspondence?)
;workspace-pointer-to-string             : workspace pointer -> string
;workspace-pointer-search                : workspace pointer -> (list letter group description bond correspondence)
;workspace-letters-initialize            : workspace function -> aloletters
;workspace-letter-make-descriptions      : workspace letter -> description
;workspace-object-choose-random          : workspace -> (or letter? group?)
;workspace-object-descriptions           : workspace (or letter? group?) -> alodescription

;NOT ADDED:
;workspace-objects
;workspace-structures : functionality changed from original
;vector->list-sanitized : became vector-clean
;workspace-s-objects
;workspace-s-letters
;workspace-s-groups
;workspace-s-correspondences
;workspace-s-descriptions
;workspace-s-structures
;object-pointer : no current equivalent
;object-descriptors : use workspace-structure-search instead
;workspace-search : became workspace-structure-search
;workspace-get-pointers : became structure-pointer
;letters-init : became workspace-letters-initialize
;desc-pointer-equal?
;assoc-pointer
;bond-pointer-member?
;corr-pointer-member?
;group-pointer-member?
;contains-pointer?
;vector-insert-top-index
;vector-insert-top
;vector-insert-top-list
;vector-remove
;workspace-to : became structure-vector-function
;workspace-to-set! : became workspace-structure-set
;workspace-search : became workspace-structure-to-pointer
;new-string-ref
;workspace-get-from-pointer : became workspace-pointer-to-structure
;workspace-mutate!
;workspace-add!
;workspace-remove!

;NEED TO BE ADDED:
;break
;position
;filter-by-pointer



;vector-index : vector any/c -> number
(define (vector-index v obj)
  (local ((define index (vector-member obj v))
          )
    (cond
      [(boolean? index) (error "Could not locate the given object in the given vector!")]
      [else index])))

;vector-add : vector any/c -> vector
(define (vector-add v obj)
  (local ((define index (vector-member #f v))
          )
    (cond
      [(boolean? index) (error "Could not find an empty space in the given vector to insert the given object!")]
      [(not (boolean? (vector-member obj v))) (error "The given object already exists in the given vector!")]
      [else (begin (vector-set! v index obj) v)])))

;vector-remove : vector any/c -> vector
(define (vector-remove v obj)
  (begin (vector-set! v (vector-index v obj) #f) v))

;vector-remove-index : vector number -> vector
(define (vector-remove-index v idx)
  (begin (vector-set! v idx #f) v))

;vector-clean : vector -> vector
(define (vector-clean v)
  (vector-filter (lambda(x) (not (boolean? x))) v))

;structure-type : (or letter? group? description? bond? correspondence?) -> symbol
(define (structure-type obj)
  (cond
    [(letter? obj) 'letter]
    [(group? obj) 'group]
    [(description? obj) 'description]
    [(bond? obj) 'bond]
    [(correspondence? obj) 'correspondence]
    [else (error "Given object was not an expected type!")]))

;structure-pointer : (or letter? group? description? bond? correspondence?) -> alopointer
(define (structure-pointer obj)
  (cond
    [(letter? obj) (list (letter-pointer obj))]
    [(group? obj) (group-members obj)]
    [(description? obj) (list (description-pointer obj))]
    [(bond? obj) (list (bond-from obj) (bond-to obj))]
    [(correspondence? obj) (list (correspondence-from obj) (correspondence-to obj))]
    [else (error "Given object was not an expected type!")]))

;structure-vector-function : symbol -> function
(define (structure-vector-function type)
  (cond
    [(symbol=? type 'letter) workspace-letters]
    [(symbol=? type 'group) workspace-groups]
    [(symbol=? type 'description) workspace-descriptions]
    [(symbol=? type 'bond) workspace-bonds]
    [(symbol=? type 'correspondence) workspace-correspondences]
    [else (error "Given object was not an expected type!")]))

;structure-set-function : symbol -> function
(define (structure-set-function type)
  (cond
    [(symbol=? type 'letter) set-workspace-letters!]
    [(symbol=? type 'group) set-workspace-groups!]
    [(symbol=? type 'description) set-workspace-descriptions!]
    [(symbol=? type 'bond) set-workspace-bonds!]
    [(symbol=? type 'correspondence) set-workspace-correspondences!]
    [else (error "Given object was not an expected type!")]))

;workspace-empty : string string string -> workspace
(define (workspace-empty initial modified target)
  (make-workspace initial modified target ""
                  (make-vector 100 #f)
                  (make-vector 20 #f)
                  (make-vector 100 #f)
                  (make-vector 100 #f)
                  (make-vector 100 #f)
                  #f))

;workspace-copy : workspace -> workspace
(define (workspace-copy space)
  (make-workspace (workspace-initial space)
                  (workspace-modified space)
                  (workspace-target space)
                  (workspace-answer space)
                  (vector-copy (workspace-letters space))
                  (vector-copy (workspace-groups space))
                  (vector-copy (workspace-descriptions space))
                  (vector-copy (workspace-bonds space))
                  (vector-copy (workspace-correspondences space))
                  (workspace-rules space)))

;workspace-structures : workspace -> list
(define (workspace-structures space)
  (vector->list (vector-clean (vector-append (workspace-letters space)
                                             (workspace-groups space)
                                             (workspace-descriptions space)
                                             (workspace-bonds space)
                                             (workspace-correspondences space)))))

;workspace-structures-type : workspace symbol -> vector
(define (workspace-structures-type space type)
  ((structure-vector-function type) space))

;workspace-structures-set : workspace symbol vector -> void
(define (workspace-structures-set space type v)
  ((structure-set-function type) space v))

;workspace-structure-index : workspace (or letter? group? description? bond? correspondence?) -> number
(define (workspace-structure-index space obj)
  (vector-index (workspace-structures-type space (structure-type obj)) obj))

;workspace-structure-add : workspace (or letter? group? description? bond? correspondence?) -> workspace
(define (workspace-structure-add space obj)
  (local ((define new-space (workspace-copy space))
          (define type (structure-type obj)))
    (begin
      (workspace-structures-set new-space type (vector-add (workspace-structures-type new-space type) obj))
      new-space)))

;workspace-structure-remove : workspace (or letter? group? description? bond? correspondence?) -> workspace
(define (workspace-structure-remove space obj)
  (local ((define new-space (workspace-copy space))
          (define type (structure-type obj)))
    (begin
      (workspace-structures-set new-space type (vector-remove (workspace-structures-type new-space type) obj))
      new-space)))

;workspace-pointer-to-structure : workspace pointer -> (or letter? group? description? bond? correspondence?)
(define (workspace-pointer-to-structure space ptr)
  (vector-ref ((pointer-target ptr) space) (pointer-index ptr)))

;workspace-pointer-to-string : workspace pointer -> string
(define (workspace-pointer-to-string space ptr)
  (local ((define idx (pointer-index ptr)))
    (substring ((pointer-target ptr) space) idx (add1 idx))))

;workspace-structure-to-pointer : workspace (or letter? group? description? bond? correspondence?) -> pointer
(define (workspace-structure-to-pointer space obj)
  (make-pointer (structure-vector-function (structure-type obj)) (workspace-structure-index space obj)))

;workspace-pointer-search : workspace pointer -> (list letter group description bond correspondence)
(define (workspace-pointer-search space ptr)
  (filter (lambda(obj) (not (boolean? (member ptr (structure-pointer obj))))) (workspace-structures space)))

;workspace-letters-initialize : workspace function -> aloletters
(define (workspace-letters-initialize space func)
  (map (lambda(n) (make-letter (substring (func space) n (add1 n)) (make-pointer func n))) (build-list (string-length (func space)) values)))

;workspace-letter-make-descriptions : workspace letter -> description
(define (workspace-letter-make-descriptions space ltr)
  (local ((define ltr-ptr (workspace-structure-to-pointer space ltr))
          (define pos (string-position (pointer-index (letter-pointer ltr)) (string-length ((pointer-target (letter-pointer ltr)) space))))
          (define desc-pos (if (boolean? pos) empty (list (make-description pos ltr-ptr)))))
    (append (list (make-description (string->symbol (letter-string ltr)) ltr-ptr)
                  (make-description 'letter-category ltr-ptr))
            desc-pos)))

;workspace-pointer-trace : workspace pointer -> pointer
(define (workspace-pointer-trace space ptr)
  (cond
    [(ormap (lambda(x) (equal? x (pointer-target ptr))) (list workspace-initial workspace-modified workspace-target)) ptr]
    [else (first (map (lambda(x) (workspace-pointer-trace space x)) (structure-pointer (workspace-pointer-to-structure space ptr))))]))

;workspace-object-choose-random : workspace -> (or letter? group?)
(define (workspace-object-choose-random space)
  (choose-random (vector->list (vector-clean (vector-append (workspace-letters space) (workspace-groups space))))))

;workspace-object-descriptions : workspace (or letter? group?) -> alodescription
(define (workspace-object-descriptions space obj)
  (workspace-pointer-search space (workspace-structure-to-pointer obj)))

;workspace-object-description-choose-random : workspace (or letter? group?) -> description
(define (workspace-object-description-choose-random space obj)
  (choose-random (workspace-object-descriptions space obj)))
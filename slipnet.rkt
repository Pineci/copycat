#lang racket
(provide (all-defined-out))

(define-struct slipnet (nodes links) #:transparent)
(define-struct link (from to type intrinsic length) #:transparent)
(define-struct node (name depth activation) #:transparent)

;node-name=?                   : node symbol -> boolean
;slipnet-node-exists?          : slipnet symbol -> boolean
;slipnet-node-add              : slipnet node -> slipnet
;slipnet-nodes-append          : slipnet alonode -> slipnet
;slipnet-node-get              : slipnet symbol -> node

;link-from=?                   : link symbol -> boolean
;link-to=?                     : link symbol -> boolean
;links-filter-type             : alolink symbol -> alolink
;slipnet-link-exists?          : slipnet symbol symbol -> boolean
;slipnet-link-add              : slipnet link -> slipnet
;slipnet-links-append          : slipnet alolink -> slipnet
;slipnet-link-get              : slipnet symbol symbol -> link
;slipnet-links-from            : slipnet symbol -> alolink
;slipnet-links-to              : slipnet symbol -> alolink
;slipnet-links-filter-type     : slipnet symbol -> alolink
;slipnet-link-calculate-length : slipnet symbol symbol number -> number
;make-links-within-list         : alosym type intrinsic length -> alolink
;make-links-double              : symbol symbol alosym alosym alon -> alolink
;make-links-reversed           : symbol symbol symbol symbol number -> alolink
;slipnet-make-links-double      : symbol symbol alosym alosym number -> alolink

;FUNCTIONS NOT ADDED:
;slipnode-init
;slipnode-init-map
;slipnode-init-pair
;slipnet-node-active?
;slipnet-node-activate
;slipnode-similar-has-property-links
;slipnet-link-degree-of-association
;slipnet-node-descriptors

;node-name=? : node symbol -> boolean
(define (node-name=? node name)
  (equal? (node-name node) name))

;link-from=? : link symbol -> boolean
(define (link-from=? link name)
  (equal? (link-from link) name))

;link-to=? : link symbol -> boolean
(define (link-to=? link name)
  (equal? (link-to link) name))

;slipnet-node-exists?: slipnet symbol -> boolean
(define (slipnet-node-exists? net node-name)
  (ormap (lambda (node) (node-name=? node node-name)) (slipnet-nodes net)))

;slipnet-link-exists?: slipnet symbol symbol -> boolean
(define (slipnet-link-exists? net from to)
  (ormap (lambda (link) (and (link-from=? link from)
                             (link-to=? link to))) (slipnet-links net)))

;slipnet-node-add: slipnet node -> slipnet
(define (slipnet-node-add net node)
  (cond
    [(slipnet-node-exists? net (node-name node)) (error "Node with given name already exits!")]
    [else (make-slipnet
           (append (slipnet-nodes net) (list node))
           (slipnet-links net))]))

;slipnet-link-add: slipnet link -> slipnet
(define (slipnet-link-add net link)
  (cond
    [(slipnet-link-exists? net (link-from link) (link-to link)) (error "Link with given from or to node already exists!")]
    [(not (and (slipnet-node-exists? net (link-from link)) (slipnet-node-exists? net (link-to link)))) (error "At least one node in the given link does not exist!")]
    [else (make-slipnet
           (slipnet-nodes net)
           (append (slipnet-links net) (list link)))]))

;slipnet-nodes-append : slipnet alonode -> slipnet
(define (slipnet-nodes-append net alonode)
  (foldl (lambda(x y) (slipnet-node-add y x)) net alonode))

;slipnet-links-append : slipnet alolink -> slipnet
(define (slipnet-links-append net alolink)
  (foldl (lambda(x y) (slipnet-link-add y x)) net alolink))

;slipnet-node-get : slipnet symbol -> node
(define (slipnet-node-get net name)
  (cond
    [(not (slipnet-node-exists? net name)) (error "Node with given name does not exist!")]
    [else (car (filter (lambda (node) (node-name=? node name)) (slipnet-nodes net)))]))

;slipnet-link-get : slipnet symbol symbol -> link
(define (slipnet-link-get net from to)
  (cond
    [(not (slipnet-link-exists? net from to)) (error "Link with given from or to node does not exist!")]
    [else (car (filter (lambda (link) (and (link-from=? link from)
                                           (link-to=? link to))) (slipnet-links net)))]))

;slipnet-links-from : slipnet symbol -> alolink
(define (slipnet-links-from net name)
  (cond
    [(not (slipnet-node-exists? net name)) (error "Node with given name does not exist!")]
    [else (filter (lambda (link) (link-from=? link name)) (slipnet-links net))]))

;slipnet-links-to : slipnet symbol -> alolink
(define (slipnet-links-to net name)
  (cond
    [(not (slipnet-node-exists? net name)) (error "Node with given name does not exist!")]
    [else (filter (lambda (link) (link-to=? link name)) (slipnet-links net))]))

;links-filter-type : alolink symbol -> alolink
(define (links-filter-type alolink type)
  (filter (lambda(link) (equal? (link-type link) type)) alolink))

;slipnet-links-filter-type : slipnet symbol -> alolink
(define (slipnet-links-filter-type net type)
  (links-filter-type (slipnet-links net) type))

;slipnet-link-calculate-length : slipnet symbol symbol number -> number
(define (slipnet-link-calculate-length net from to maxlength)
  (let ([length (- (node-depth (slipnet-node-get net to))
                   (node-depth (slipnet-node-get net from)))])
    (cond
      [(< length 0) maxlength]
      [else length])))

;make-link-within-list : alosym symbol symbol number -> alolink
(define (make-links-within-list alos type intrinsic len)
  (map (lambda(from to) (make-link from to type intrinsic len))
       (take alos (sub1 (length alos)))
       (drop alos 1)))

;make-link-double : symbol symbol alosym alosym alon -> alolink
(define (make-links-double node1 node2 types intrinsics lengths)
  (list
   (make-link node1 node2 (car types) (car intrinsics) (car lengths))
   (make-link node2 node1 (cadr types) (cadr intrinsics) (cadr lengths))))

;make-links-reversed : symbol symbol symbol symbol number -> alolink
(define (make-links-reversed from to type intrinsic length)
  (list (make-link from to type intrinsic length)
        (make-link to from type intrinsic length)))

;slipnet-make-link-double : slipnet symbol symbol alosym alosym number -> alolink
(define (slipnet-make-links-double net node1 node2 types intrinsics maxlength)
  (make-links-double node1 node2 types intrinsics (list (slipnet-link-calculate-length net node1 node2 maxlength)
                                                       (slipnet-link-calculate-length net node2 node1 maxlength))))

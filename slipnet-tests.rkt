#lang racket
(require "slipnet.rkt")
(require test-engine/racket-tests)

(define example-node-a (make-node 'a 1 0))
(define example-node-b (make-node 'b 2 0))
(define example-node-c (make-node 'c 3 0))
(define example-node-d (make-node 'd 1 0))
(define example-node-e (make-node 'e 5 0))
(define example-link-ab (make-link 'a 'b 'test1 empty 10))
(define example-link-ac (make-link 'a 'c 'test1 empty 20))
(define example-link-bc (make-link 'b 'c 'test2 empty 30))
(define example-link-cb (make-link 'c 'b 'test2 empty 30))
(define example-link-ad (make-link 'a 'd 'test2 empty 40))
(define example-slipnet (make-slipnet (list example-node-a example-node-b example-node-c) (list example-link-ab)))

;node-name=?
(check-expect (node-name=? example-node-a 'a) #t)
(check-expect (node-name=? example-node-a 'b) #f)

;link-from=?
(check-expect (link-from=? example-link-ab 'a) #t)
(check-expect (link-from=? example-link-ab 'b) #f)

;link-to=?
(check-expect (link-to=? example-link-ab 'a) #f)
(check-expect (link-to=? example-link-ab 'b) #t)

;slipnet-node-exists?
(check-expect (slipnet-node-exists? example-slipnet 'a) #t)
(check-expect (slipnet-node-exists? example-slipnet 'd) #f)

;slipnet-link-exists?
(check-expect (slipnet-link-exists? example-slipnet 'a 'b) #t)
(check-expect (slipnet-link-exists? example-slipnet 'a 'c) #f)

;slipnet-node-add
(check-expect (slipnet-node-add example-slipnet example-node-d) (make-slipnet (list example-node-a example-node-b example-node-c example-node-d) (list example-link-ab)))
(check-error (slipnet-node-add example-slipnet example-node-a))

;slipnet-link-add
(check-expect (slipnet-link-add example-slipnet example-link-ac) (make-slipnet (list example-node-a example-node-b example-node-c) (list example-link-ab example-link-ac)))
(check-error (slipnet-link-add example-slipnet example-link-ab))
(check-error (slipnet-link-add example-slipnet example-link-ad))

;slipnet-nodes-append
(check-expect (slipnet-nodes-append example-slipnet (list example-node-d)) (make-slipnet (list example-node-a example-node-b example-node-c example-node-d) (list example-link-ab)))
(check-expect (slipnet-nodes-append example-slipnet (list example-node-d example-node-e)) (make-slipnet (list example-node-a example-node-b example-node-c example-node-d example-node-e) (list example-link-ab)))
(check-error (slipnet-nodes-append example-slipnet (list example-node-d example-node-e example-node-a)))

;slipnet-links-append
(check-expect (slipnet-links-append example-slipnet (list example-link-ac example-link-bc)) (make-slipnet (list example-node-a example-node-b example-node-c) (list example-link-ab example-link-ac example-link-bc)))
(check-error (slipnet-links-append example-slipnet (list example-link-ab example-link-ac)))
(check-error (slipnet-links-append example-slipnet (list example-link-ac example-link-ad)))

;slipnet-node-get
(check-expect (slipnet-node-get example-slipnet 'a) example-node-a)
(check-expect (slipnet-node-get example-slipnet 'b) example-node-b)
(check-error (slipnet-node-get example-slipnet 'd))

;slipnet-link-get
(check-expect (slipnet-link-get example-slipnet 'a 'b) example-link-ab)
(check-error (slipnet-link-get example-slipnet 'a 'c))

;slipnet-links-from
(check-expect (slipnet-links-from example-slipnet 'a) (list example-link-ab))
(check-expect (slipnet-links-from example-slipnet 'b) empty)
(check-expect (slipnet-links-from example-slipnet 'c) empty)
(check-error (slipnet-links-from example-slipnet 'd))

;slipnet-links-to
(check-expect (slipnet-links-to example-slipnet 'a) empty)
(check-expect (slipnet-links-to example-slipnet 'b) (list example-link-ab))
(check-expect (slipnet-links-to example-slipnet 'c) empty)
(check-error (slipnet-links-to example-slipnet 'd))

;links-filter-type
;slipnet-links-filter-type (passthrough func to links-filter-type)
(check-expect (links-filter-type (list example-link-ab example-link-ac) 'test1) (list example-link-ab example-link-ac))
(check-expect (links-filter-type (list example-link-ab example-link-ad) 'test1) (list example-link-ab))
(check-expect (links-filter-type (list example-link-ab example-link-ac) 'test2) empty)
(check-expect (links-filter-type (list example-link-ab example-link-ad) 'test2) (list example-link-ad))

;slipnet-link-calculate-length
(check-expect (slipnet-link-calculate-length example-slipnet 'a 'b 10) 1)
(check-expect (slipnet-link-calculate-length example-slipnet 'a 'c 10) 2)
(check-expect (slipnet-link-calculate-length example-slipnet 'c 'b 10) 10)
(check-expect (slipnet-link-calculate-length (slipnet-node-add example-slipnet example-node-d) 'a 'd 10) 0)

;make-links-within-list
(check-expect (make-links-within-list '(a b) 'test1 empty 10) (list example-link-ab))
(check-expect (make-links-within-list '(a b c) 'test1 empty 10) (list example-link-ab (make-link 'b 'c 'test1 empty 10)))

;make-links-double
(check-expect (make-links-double 'a 'b '(test1 test2) '(intrinsic1 intrinsic2) '(10 20)) (list (make-link 'a 'b 'test1 'intrinsic1 10)
                                                                                              (make-link 'b 'a 'test2 'intrinsic2 20)))

;make-links-reversed
(check-expect (make-links-reversed 'a 'b 'test 'intrinsic 10) (list (make-link 'a 'b 'test 'intrinsic 10)
                                                                 (make-link 'b 'a 'test 'intrinsic 10)))


;slipnet-make-links-double
(check-expect (slipnet-make-links-double example-slipnet 'a 'b '(test1 test2) '(intrinsic1 intrinsic2) 10) (list (make-link 'a 'b 'test1 'intrinsic1 1)
                                                                                                                (make-link 'b 'a 'test2 'intrinsic2 10)))

(test)
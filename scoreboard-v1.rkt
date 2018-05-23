#lang racket
(require "stream.rkt")
(require "ordered-set.rkt")
(require "total-order.rkt")

(define (count s)
  (define count (lambda (x) (os-before (second x) (to-hide (stream-car (first x)) +inf.0))))
  (stream-generate  
   (list s empty)
   (lambda (x) (stream-empty? (first x)))
   (lambda (x) (list (stream-cdr (first x))
                     (if (= (stream-car (first x)) 
                             (left-rep (count x)))  
                         (os-union (os-singleton (to-hide (stream-car (first x)) (add1 (right-rep (count x)))))
                                (second x))
                         (os-union (os-singleton (to-hide (stream-car (first x)) 0)) (second x)))))
   (lambda (x)
     (if (= (stream-car (first x))
           (left-rep (count x)))                 
         (list (stream-car (first x)) (add1 (right-rep (count x))))
         (list (stream-car (first x)) 0)))
  )
) 


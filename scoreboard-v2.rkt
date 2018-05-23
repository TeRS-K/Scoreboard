#lang racket
(require "stream.rkt")
(require "total-order.rkt")
(require "ordered-set.rkt")

(define (scoreboard s)
  (define count
    (lambda (x) (os-before (second x) (to-hide (car (stream-car (first x))) +inf.0))))
  (stream-generate
   (list s empty)
   (lambda (x) (stream-empty? (first x)))
   (lambda (x) (list (stream-cdr (first x))
                     (if (equal? (car (stream-car (first x))) (name-rep (count x)))
                         (os-difference (os-union (os-singleton
                                   (to-hide (first (stream-car (first x)))
                                            (+ (fscore-rep (count x))
                                               (second (stream-car (first x))))))
                                    (second x)) (os-singleton (count x)))
                         (os-union (os-singleton
                                    (to-hide (first (stream-car (first x))) 
                                             (second (stream-car (first x)))))
                                   (second x)))))
   (lambda (x) (if (equal? (car (stream-car (first x))) (name-rep (count x)))
                   (list (first (stream-car (first x)))
                         (second (stream-car (first x)))
                         (+ (fscore-rep (count x))
                            (second (stream-car (first x))))
                         (name-rep (os-op
                                    (os-difference (os-union (os-singleton
                                              (to-hide (first (stream-car (first x)))
                                                       (+ (fscore-rep (count x))
                                                          (second (stream-car (first x))))))
                                    (second x)) (os-singleton (count x))))))
                   (list (first (stream-car (first x)))
                         (second (stream-car (first x)))
                         (second (stream-car (first x)))
                         (name-rep (os-op
                                    (os-union (os-singleton
                                    (to-hide (first (stream-car (first x))) 
                                             (second (stream-car (first x)))))
                                   (second x)))))))))


#lang scheme

;; Total Order ADT used by Ordered Set ADT

;; Must provide:
;;   a membership predicate
;;   a total order
;;   min and max functions with identities
;;   user-defined associative operator and identity
;;
;;   any other operations required by the user
;;       e.g. to-hide and to-unhide

(provide to? to< to<= to> to> to>= to= to!= to-min to-min-ident 
         to-max to-max-ident to-op to-op-ident to-hide to-unhide name-rep fscore-rep)

;; For this example, we use a number hidden in a struct

   (define-struct to-rep (name fscore)#:transparent)
   (define (to-unhide x) (list (to-rep-name x) (to-rep-fscore)))
   (define (to-hide a b) (make-to-rep a b))
   (define (name-rep x) (to-rep-name x))
   (define (fscore-rep x) (to-rep-fscore x))

;; membership predicate

   (define (to? x) (and (to-rep? x) (symbol? (to-rep-name x))
                                    (number? (to-rep-fscore x))))

;; defining relation must be total, reflexive, transitive


   (define (to<= a b)
     (or (string<? (symbol->string (name-rep a))
                   (symbol->string (name-rep b)))
         (and (equal? (name-rep a) (name-rep b))
              (<= (fscore-rep a) (fscore-rep b)))))

;; derived relations
   (define (to> a b) (not (to<= a b)))
   (define (to= a b) (and (to<= a b) (to<= b a)))
   (define (to>= a b) (to<= b a))
   (define (to< a b) (to> b a))
   (define (to!= a b) (not (to= a b)))

;; min/max functions and identities
;; +inf.0 stands for positive infinity
;; -inf.0 stands for negative infinity
   (define (to-min a b) (if (to< a b) a b))
   (define to-min-ident (make-to-rep 'zzzz +inf.0))

   (define (to-max a b) (if (to< a b) b a))
   (define to-max-ident (make-to-rep 'AAAA -inf.0))

;; user-defined associative operator and identity


;; for this example we just add the secret numbers

   (define (to-op a b)
     (cond
       [(< (fscore-rep a) (fscore-rep b)) b]
       [(< (fscore-rep b) (fscore-rep a)) a]
       [(= (fscore-rep b) (fscore-rep a))
        (if (string<? (symbol->string (name-rep b))
                      (symbol->string (name-rep a))) b a)]))
                          
   (define to-op-ident (to-hide 'AAAA -inf.0))

#lang racket
(provide model% view-controller%)

(define model%
  (class object%
    (super-new)
    
    (define views '())
    (define (add-view v)
      (set! views (cons v views)))
    (define (update)
      (for ([v views])
        (send v update)))
    ))

(define view-controller%
  (class object%
    (define views 
      (make-parameter '()))
    
    (define the-model 
      (make-parameter false))
    
    (define/public (update)
      (error 'view-controller% "Override base update method."))
    
    (define/public (add-view v)
      (views (cons v (views))))
    
    (define/public (set-model! m)
      (the-model m))
    
    ))
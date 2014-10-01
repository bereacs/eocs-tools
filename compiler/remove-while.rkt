#lang racket

(require "base.rkt")

(provide remove-while)

(define (remove-while statement)
  (cond
    [(while0? statement)
     (let ([top-label (gensym 'TOPWHILE)]
           [end-label (gensym 'ENDWHILE)]
           [FLAG (gensym 'FLAG)]
           )
       (seq
        (list
         (label top-label)
         (set FLAG (remove-while (while0-test statement)))
         (if0 (variable FLAG)
              (seq
               (list
                (remove-while (while0-body statement))
                (goto top-label)))
              (goto end-label))
         (label end-label)
         ))
       )]
    [(binop? statement)
     (binop (binop-op statement)
            (remove-while (binop-lhs statement))
            (remove-while (binop-rhs statement)))]
    
    
    [(if0? statement) (if0 (remove-while (if0-test statement))
                           (remove-while (if0-truecase statement))
                           (remove-while (if0-falsecase statement)))]
    
    [(seq? statement) (seq (map remove-while (seq-expressions statement)))]
    [(set? statement) (set (set-ident statement) (remove-while (set-e statement)))]
    [(num? statement) statement]
    [(variable? statement) statement]
    ))
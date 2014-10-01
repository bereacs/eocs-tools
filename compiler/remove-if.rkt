#lang racket

(require "base.rkt")

(provide remove-if)

(define (remove-if statement)
  (cond
    [(binop? statement)
     (binop (binop-op statement)
            (remove-if (binop-lhs statement))
            (remove-if (binop-rhs statement)))]
    
    
    [(if0? statement) 
     (let ([false-label (gensym 'FALSE-LABEL)]
           [endif-label (gensym 'ENDIF-LABEL)]
           [test (gensym 'TEST)])
       (seq
        (list
         (set test (remove-if (if0-test statement)))
         (jump 'JNE (variable test) (variable false-label))
         (remove-if (if0-truecase statement))
         (goto endif-label)
         (label false-label)
         (remove-if (if0-falsecase statement))
         (goto endif-label)
         (label endif-label)))
       )]
    
    [(seq? statement) (seq (map remove-if (seq-expressions statement)))]
    [(set? statement) (set (set-ident statement) (remove-if (set-e statement)))]
    [(num? statement) statement]
    [(variable? statement) statement]
    [(label? statement) statement]
    [(goto? statement) statement]
    [else (error (format "~a" statement))]
    ))
#lang racket

(require "base.rkt")
(provide parse)

(define (operator? sym)
  (member sym '(+ - *)))

(define (parse e)
  (match e
    [(? number? n) (num n)]
    [(? symbol? s) (variable s)]
    [`(,(? operator? op) ,lhs ,rhs)
     (binop op (parse lhs) (parse rhs))]
    [`(set ,id ,val)
     (set id (parse val))]
    [`(if0 ,test ,true ,false)
     (if0 (parse test) (parse true) (parse false))]
    [`(while0 ,test ,body)
     (while0 (parse test) (parse body))]
    [`(seq ,e ...)
     (seq (map parse e))]))
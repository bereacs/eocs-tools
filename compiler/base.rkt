#lang racket

(provide (all-defined-out))
(require racket/pretty)

;;make hash table
(define GENSYM-TABLE (make-hash))

;;starts symbols at zero
(define (sym id)
 (let ([next (hash-ref GENSYM-TABLE id
                       (lambda () 0))])
   (hash-set! GENSYM-TABLE id (add1 next))
   (string->symbol
    (format "~a~a" id next))))

;;set symbols back to zero
(define (reset-syms)
 (set! GENSYM-TABLE (make-hash)))


;; Structures

;; Number Structure
;; contains a
;; - value
(struct num (value)
  #:inspector (make-inspector))

;; Jump Structure
;; contains a
;; - symbol
;; - symbol
(struct jump (jumpsym test jumpdest)
  #:inspector (make-inspector))

;; Variable Structure
;; contains a
;; - symbol
(struct variable (value)
  #:inspector (make-inspector))

;; Binop Structure
;; contains a
;; - operator
;; - left hand operand
;; - right hand operand
(struct binop (op lhs rhs)
  #:inspector (make-inspector))

;; Identifier Structure
;; contains a
;; - symbol
;; - number or simple
(struct id (sym value)
  #:inspector (make-inspector))

;; Label Structure
;; contains a
;; - symbol
(struct label (sym)
  #:inspector (make-inspector))

;; Goto Stucture
;; contains a
;; - symbol
(struct goto (sym)
  #:inspector (make-inspector))

;; set struct
;; contains an id and expression
(struct set (ident e)
  #:inspector (make-inspector))

;; If0 Structure
;; contains a
;; - test
;; - true case
;; - false case
;; The test and cases must be expressions
(struct if0 (test truecase falsecase)
  #:inspector (make-inspector))

;; While0 Structure
;; contains a
;; - test
;; - body
;; The test and body must be expressions
(struct while0 (test body)
  #:inspector (make-inspector))

;; Seq Structure
;; contains a 
;; - a list of one or more expressions
(struct seq (expressions)
  #:inspector (make-inspector))

(define (show e)
  (pretty-print e)
  (newline)
  e)
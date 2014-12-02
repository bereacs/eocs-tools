;; Pass 2

#lang racket
(require "base.rkt")
(provide create-ids)


;; PURPOSE
;; Take in the structures from the previous pass
;; and produce a list of structures (broken down into the simple forms)
;; CONTRACT
;; createids : struct -> list of (lists of) ids

#|
(+ 3 5) -> (simple + (num 3) (num 5)) -> (prog
                                            (id a 3)
                                            (id b 5)
                                            (id c (+ a b)))

(+ 3 (+ 2 3)) -> (binop + 3 (simple + (num 2) (num 3))
                          |
                          -> (prog
                                (id a 2)        (*)
                                (id b 3)        (**)
                                (id c (+ a b))  (***)
                                (id d 3)
                                (id e (+ c d)))
|#


;; This function is used to extract syms from lists as well as id structs
(define (last ls)
  (first (reverse ls)))

(define (extract-sym i)
  (cond
    [(set? i) (set-ident i)]
    [(num? i) (num-value i)]
    [(variable? i) (variable-value i)]
    [else (id-sym i)]
    
    ))

;; This is the main parsing pass, and parses all received structs into id structs
;; returns a list of lists that must be flattened by create-ids
(define (create-ids->lol structure)
  (cond
    [(seq? structure) 
     (map create-ids->lol (seq-expressions structure))]
    
    
    [(binop? structure)
     (let ([lhs (create-ids->lol (binop-lhs structure))]
           [rhs (create-ids->lol (binop-rhs structure))])
       (append
        lhs
        rhs
        (list
         (id (sym 'bin) (binop (binop-op structure)
                               (extract-sym (last lhs))
                               (extract-sym (last rhs))
                               )))))]
    [(jump? structure) 
     (let ([t (create-ids->lol (jump-test structure))])
       (append
        t
        (list
         (jump (jump-jumpsym structure)
               (variable (extract-sym (last t)))
               (jump-jumpdest structure))
         )))]
    
    
    [(set? structure) 
     (let ([e (create-ids->lol (set-e structure))])
       (append
        e
        (list
         (set (set-ident structure) (extract-sym (last e))))))]
    [(goto? structure) (list structure)]
    [(label? structure) (list structure)]
    [(num? structure) (list structure)]
    [(variable? structure) (list structure)]
    ))

;; This function will return the final formatted output of the pass
(define (create-ids structure)
  (filter (lambda (o)
            (not (or (num? o)
                     (variable? o))))
          (flatten 
           (create-ids->lol structure))))

(define cid
  (list
   (set 'n 10) ;; =>
   ;; @10   -- Load the value
   ;; D=A   -- Store it in D
   ;; @16   -- Load the memory address of 'n'
   ;; M=D   -- Store the value of D into RAM
   (set 'sum 0)
   (set 'flag 0)
   (label 'TOPWHILE110383) ;; => We need to calculate these locations, but they fall out.
   (set 'FLAG110385 'flag)
   ;; @flag  -- Load the address of flag
   ;; D=M    -- Store the RAM into D
   ;; @FLAG110385 -- Load the address of @FLAG...
   ;; M=D    -- Store flag's value into @FLAG...
   (set 'TEST110388 'FLAG110385)
   (jump 'JNE (variable 'TEST110388) (variable 'FALSE-LABEL110386))
   ;; @TEST -- Load address of TEST into A
   ;; D=M -- Store the value at that location into D
   ;; @FALSE -- Load addr of label into A
   ;; D;JNE -- If D is JNE, jump to the location in A
   (set 'TEST110391 'n)
   (jump 'JNE (variable 'TEST110391) (variable 'FALSE-LABEL110389))
   (set 'flag -1)
   (goto 'ENDIF-LABEL110390)
   ;; @ENDIF -- Load label addr
   ;; A;JMP -- Jump to the location
   (label 'FALSE-LABEL110389)
   (id 'bin0 (binop '+ 'sum 'n))
   ;; @n -- Load n
   ;; D=A -- Move it to D
   ;; @sum -- Load sum
   ;; D=D+A
   ;; @bin -- Load bin0
   ;; M=D -- Store D into RAM at the location of bin0
   (set 'sum 'bin0)
   (id 'bin1 (binop '- 'n 1))
   (set 'n 'bin1)
   (goto 'ENDIF-LABEL110390)
   (label 'ENDIF-LABEL110390)
   (goto 'TOPWHILE110383)
   (goto 'ENDIF-LABEL110387)
   (label 'FALSE-LABEL110386)
   (goto 'ENDWHILE110384)
   (goto 'ENDIF-LABEL110387)
   (label 'ENDIF-LABEL110387)
   (label 'ENDWHILE110384)))

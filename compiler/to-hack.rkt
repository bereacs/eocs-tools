#lang racket
(require "base.rkt")

(provide to-hack)

(define LOOKUP (make-hash))
(define RAMLOC 15)
(define (map-loc sym)
  (define result (hash-ref LOOKUP sym #f))
  (cond
    [result result]
    [else
     (set! RAMLOC (add1 RAMLOC))
     (hash-set! LOOKUP sym RAMLOC)
     RAMLOC]))

(define LABELS (make-hash))
(define (insert-line sym num)
  (hash-set! LABELS sym num))
(define (lookup-line sym)
  (hash-ref LABELS sym #f))

(define (assign-locations input line)
  (cond
    [(number? (id-value (first input)))
     (id (map-loc (id-sym (first input))) 
          (id-value (first input)))]

    [(binop? (id-value (first input)))
      (id (map-loc (id-sym (first input)))
          (binop (assign-locations (binop-lhs (id-value (first input))))
                 (assign-locations (binop-rhs (id-value (first input))))))]
    
    [(label? (id-value (first input)))
     (insert-line (id-sym (first input)) line)
     (label line)]
    
    [(goto? (id-value (first input)))
     (goto (lookup-line (id-value (first input))))]
    
    [(jump? (id-value (first input)))
     (jump (id-value (first input)))]
    
    [(symbol? (id-value (first input)))
     (string-append (asm-symbol (first input)) (assign-locations (rest input)))
     ]
    
    [else (error (format "~a" (first input)))]
    ))

(define (to-hack input)  
  (show input)
  (cond
    [(empty? input) (prog-end)]
    
    [(number? (id-value (first input)))
     (string-append (asm-num (first input)) (to-hack (rest input)))
     ]
    
    [(binop? (id-value (first input)))
     (string-append (asm-binop (first input)) (to-hack (rest input)))
     ]
    
    [(label? (id-value (first input)))
     (string-append (asm-label (first input)) (to-hack (rest input)))
     ]
    
    [(goto? (id-value (first input)))
     (string-append (asm-goto (first input)) (to-hack (rest input)))
     ]
    
    [(jump? (id-value (first input)))
     (string-append (asm-jump (first input)) (to-hack (rest input)))
     ]
    
    [(symbol? (id-value (first input)))
     (string-append (asm-symbol (first input)) (to-hack (rest input)))
     ]
    
    [else (error (format "~a" (first input)))]
    ))

(define table (make-hash))
(define loc 16)
(define (map-location sym)
  (cond
    [(hash-ref table sym #f)
     (hash-ref table sym #f)]
    [else
     (hash-set! table sym loc)
     (set! loc (add1 loc))
     (hash-ref table sym)
     ]))

; CONTRACT
;; input Num ID -> String
(define (asm-num input)
  (string-append 
   (b16 (id-value input)) "\n"
   (bin #:a 0 #:comp 'A #:dest 'D) "\n"
   (b16 (map-location (id-sym input))) "\n"
   (bin #:a 1 #:comp 'D #:dest 'M) "\n"
   ))

; CONTRACT
;; input Sim ID -> String
(define (asm-binop input)
  (string-append 
   ;; "@" (stringify (binop-lhs (id-value input))) "\n"
   (b16 (to-hack (binop-lhs (id-value input)))) "\n"
   ;;"D=M\n"
   (bin #:a 1 #:comp 'M #:dest 'D) "\n"
   ;; "@" (stringify (binop-rhs (id-value input))) "\n"
   (b16 (to-hack (binop-rhs (id-value input)))) "\n"
   ;; "A=M\n"
   (bin #:a 1 #:comp 'M #:dest 'A) "\n"

   (cond 
     [(equal? '+ (binop-op (id-value input))) 
      ;;"D=D+A\n"
      (bin #:a 0 #:comp 'D+A #:dest 'D) "\n"
      ]
     [(equal? '- (binop-op (id-value input))) 
      ;; "D=D-A\n"
      (bin #:a 0 #:comp 'D-A #:dest 'D) "\n"
      ])
   
   ;;"@" (stringify (id-sym input)) 
   (b16 (id-sym input))
   ;;"\nM=D\n"
   (bin #:a 1 #:comp 'D #:dest 'M)
   ))

; CONTRACT
;; input Label -> String
(define (asm-label input)
  (string-append "(" (stringify (label-sym (id-value input))) ")\n")
  )

; CONTRACT
;; input GoTo -> String
(define (asm-goto input)
  (string-append "@" (stringify (goto-sym (id-value input))) "\n"
                 "0;JMP\n")
  )

; CONTRACT
;; input Jump -> String
(define (asm-jump input)
  (let ([input (id-value input)])
    (cond
      [(variable? (jump-test input))
       (string-append "@" (stringify (variable-value (jump-test input))) "\n"
                      "D=M\n"
                      "@" (stringify (variable-value (jump-jumpdest input))) "\n"   
                      "D;" (stringify (jump-jumpsym input)) "\n")]
      [(num? (jump-test input))
       (string-append "@" (stringify (num-value (jump-test input))) "\n"
                      "D=A\n"
                      "@" (stringify (variable-value (jump-jumpdest input))) "\n"   
                      "A;" (stringify (jump-jumpsym input)) "\n")]
      [else (error (format "~a" input))]
      )))

; CONTRACT
;; input Symbol -> String
(define (asm-symbol input)
  (string-append "@" (stringify (id-value input)) "\n"
                 "D=M\n"
                 "@" (stringify (id-sym input)) "\n"
                 "M=D\n")
  )

; CONTRACT
;; input NumberorSymbol -> String
(define (stringify input)
  (cond
    [(number? input)
     ;; Can't use number->string... messes up with negative numbers.
     (b16 input)]
    [else
     (symbol->string input)]))

(define (lookup-c c)
  (cond
    [(equal? c '-D)  	"001111"]
    [(equal? c '-A)  	"110011"]
    [(equal? c 'D-A)  	"010011"]
    [(equal? c '1)  	"111111"]
    [(equal? c '0)  	"101010"]
    [(equal? c 'D+A)  	"000010"]
    [(equal? c 'A-1)  	"110010"]
    [(equal? c 'A)  	"110000"]
    [(equal? c 'D)  	"001100"]
    [(equal? c 'D+1)  	"011111"]
    [(equal? c 'A+1)  	"110111"]
    [(equal? c 'A-D)  	"000111"]
    [(equal? c 'D-1)  	"001110"]
    [(equal? c 'D\|A)  	"010101"]
    [(equal? c '!A)  	"110001"]
    [(equal? c '!D)  	"001101"]
    [(equal? c '-1)  	"111010"]
    [(equal? c 'D&A)  	"000000"]
    
    [(equal? c '-M)  	"110011"]
    [(equal? c 'M+1)  	"110111"]
    [(equal? c 'M-1)  	"110010"]
    [(equal? c 'D-M)  	"010011"]
    [(equal? c 'D+M)  	"000010"]
    [(equal? c 'M)  	"110000"]
    [(equal? c 'M-D)  	"000111"]
    [(equal? c 'D\|M)  	"010101"]
    [(equal? c '!M)  	"110001"]
    [(equal? c 'D&M)  	"000000"]
    [else (error (format "Cannot find case for c = ~a" c))]
    ))

(define (lookup-d d)
  (cond
    [(equal? d 'NULL)      "000"]
    [(equal? d 'M)         "001"]
    [(equal? d 'D)         "010"]
    [(equal? d 'MD)        "011"]
    [(equal? d 'A)         "100"]
    [(equal? d 'AM)        "101"]
    [(equal? d 'AD)        "110"]
    [(equal? d 'AMD)       "111"]
    [else (error (format "Cannot find case for d = ~a" d))]
    ))

(define (lookup-j j)
  (cond
    [(equal? j 'NULL)      "000"]
    [(equal? j 'JGT)       "001"]
    [(equal? j 'JEQ)       "010"]
    [(equal? j 'JGE)       "011"]
    [(equal? j 'JLT)       "100"]
    [(equal? j 'JNE)       "101"]
    [(equal? j 'JLE)       "110"]
    [(equal? j 'JMP)       "111"]
    [else (error (format "Cannot find case for j = ~a" j))]
    ))

(define (bin #:a [a 0] #:comp [c 'A] #:dest [d 'NULL] #:jump [j 'NULL] )
  (string-append
   (format "111~a" a)
   (lookup-c c)
   (lookup-d d)
   (lookup-j j)))

(define (pad str padding tgt-len)
  (define extra (- tgt-len (string-length str)))
  (format "~a~a"
          (make-string extra padding)
          str))

(define (b16 n)
  (cond
    [(and (<= n (sub1 (expt 2 15)))
          (>= n 0))
     (pad (number->string n 2) #\0 16)]
    [(and (>= n (add1 (- (expt 2 15))))
          (< n 0))
     (define pos (number->string (* n -1) 2))
     (define invert (list->string
                     (map (lambda (c) (if (equal? c #\1) #\0 #\1))
                          (string->list pos))))
     (define inv (string->number invert 2))
     (pad (number->string (add1 inv) 2) #\0 16)
     ]
    [else
     (error (format "Number out of range: ~a" n))]))

; CONTRACT
;; input Nothing -> String
(define (prog-end)
  (string-append (b16 0)
                 "\n"
                 (bin 1 'D 'M 'NULL)))
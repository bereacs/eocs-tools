#lang racket

(require "base.rkt"
         "parse.rkt"
         "remove-mult.rkt"
         "remove-while.rkt"
         "remove-if.rkt"
         "create-ids.rkt"
         "to-asm.rkt"
         "to-hack.rkt"
         )

(provide driver)

(define (output-asm base asm)
  (let ([op-asm (open-output-file 
                 (string-append base ".asm")
                 #:exists 'replace
                 )])
        (fprintf op-asm asm)
        (newline op-asm)
        (close-output-port op-asm)
        ))

(define (output-hack base hack)
  'hack)

(define (driver file)
  (let ([sexp (read (open-input-file file))]
        [base (second (regexp-match "(.*)\\.420" file))]
        )
    (let ([interim 0]
          [asm ""]
          [hack ""])
      
      (printf "parse~n")
      (set! interim (parse sexp))
      (show interim)
      
      (printf "remove mult~n")
      (set! interim (remove-mult interim))
      (show interim)
      
      (printf "remove while~n")
      (set! interim (remove-while interim))
      (show interim)
      
      (printf "remove if~n")
      (set! interim (remove-if interim))
      (show interim)
      
      (printf "create-ids~n")
      (set! interim (create-ids interim))
      (show interim)
      
      (printf "to-asm~n")
      ;(set! asm (to-asm interim)) 
      ;(show interim)
      
      (printf "to-hack~n")
      ;(set! hack (to-hack interim))
      
      (output-asm base asm)
      (output-hack base hack)
      
      )))

    

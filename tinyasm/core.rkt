#lang racket

;; For the GUI
(provide convert-instruction
         verbose-mode
         comment-mode)

;; Parameters to control output.
(define verbose-mode (make-parameter false))
(define comment-mode (make-parameter false))

(define jumps
  (make-hash 
   '(("JGT" . "001")
     ("JEQ" . "010")
     ("JGE" . "011")
     ("JLT" . "100")
     ("JNE" . "101")
     ("JLE" . "110")
     ("JMP" . "111"))))

(define dests
  (make-hash
   '(("M"   . "001")
     ("D"   . "010")
     ("MD"  . "011")
     ("A"   . "100")
     ("AM"  . "101")
     ("AD"  . "110")
     ("AMD" . "111"))))

(define comps
  (make-hash
   '(("0" . "101010")
     ("1"   . "111111")
     ("-1"  . "111010")
     ("D"   . "001100")
     ("A"   . "110000")
     ("M"   . "110000")
     ("!D"  . "001101")
     ("!A"  . "110001")
     ("!M"  . "110001")
     ("-D"  . "001111")
     ("-A"  . "110011")
     ("-M"  . "110011")
     ("D+1" . "011111")
     ("A+1" . "110111")
     ("M+1" . "110111")
     ("D-1" . "001110")
     ("A-1" . "110010")
     ("M-1" . "110010")
     ("D+A" . "000010")
     ("D+M" . "000010")
     ("D-A" . "010011")
     ("D-M" . "010011")
     ("A-D" . "000111")
     ("M-D" . "000111")
     ("D&A" . "000000")
     ("D&M" . "000000")
     ("D|A" . "010101")
     ("D|M" . "010101")
     )))



(define (extract-jump instr)
  (cond
    [(regexp-match ";" instr)
     (define res (list-ref (regexp-match ".*;(.*?)$" instr) 1))
     (when (verbose-mode)
       (printf "\tjump: ~a => ~a~n" res (hash-ref jumps res)))
     (hash-ref jumps res)]
    [else 
     (when (verbose-mode)
       (printf "\tjump: None~n"))
     "000"]))
  
(define (get-comp-portion instr)
  (define just-c instr)
  (when (regexp-match ";" just-c)
    (set! just-c (list-ref (regexp-match "(.*?);.*?" just-c) 1)))
  (when (regexp-match "=" just-c)
    (set! just-c (list-ref (regexp-match ".*?=(.*)" just-c) 1)))
  just-c)

(define (extract-comp instr)
  (when (verbose-mode)
    (printf "\tcomp: ~a => ~a~n" (get-comp-portion instr) (hash-ref comps (get-comp-portion instr))))
  (hash-ref comps (get-comp-portion instr)))

(define (extract-dest instr)
  (cond
    [(regexp-match "=" instr)
     (define res (list-ref (regexp-match "^(.*?)=.*" instr) 1))
     (when (verbose-mode)
       (printf "\tdest: ~a => ~a~n" res (hash-ref dests res)))
     (hash-ref dests res)]
    [else
     (when (verbose-mode)
       (printf "\tdest: None~n"))
     "000"]))

(define (get-a-value instr)
  (define patterns '("M" "!M" "-M" "M+1" "M-1" "D+M" "D-M" "M-D" "D&M" "D|M"))
  (define just-c (get-comp-portion instr))
  (if (member just-c patterns)
      "1"
      "0"))

(define (convert-c-instr instr)
  (format "111~a~a~a~a"
          (get-a-value instr)
          (extract-comp instr)
          (extract-dest instr)
          (extract-jump instr)))

(define (bits n)
  (let loop ((n n) (acc '()))
    (if (= 0 n) 
        acc
        (loop (arithmetic-shift n -1) (cons (bitwise-and n 1) acc)))))

(define (pad-left ls val leng)
  (for ([cnt (- leng (length ls))])
    (set! ls (cons val ls)))
  ls)

(define (convert-a-instr instr)
  (define n (string->number (list-ref (regexp-match "@([0-9]+)" instr) 1)))
  (define b (bits n))
  (when (> 15 (length b))
    (set! b (pad-left b "0" 15)))
  (set! b (cons "0" b))
  (apply string-append (map (lambda (n) (format "~a" n)) b)))
         

(define (is-a-instr? instr)
  (regexp-match "^@" instr))

(define (is-c-instr? instr)
  (not (is-a-instr? instr)))

(define (convert-instruction instr)
  (cond
    [(is-a-instr? instr)
     (convert-a-instr instr)]
    [(is-c-instr? instr)
     (convert-c-instr instr)]
    [else
     (error 'convert-instruction "Error in convert instruction: ~a" instr)]
    ))
  

#lang racket

;; For the GUI
(provide convert-instruction
         calculate-label-locations
         verbose-mode
         comment-mode
         clear-label-locations
         line-number)

;; Parameters to control output.
(define verbose-mode (make-parameter false))
(define comment-mode (make-parameter false))
;; ROM IS ZERO INDEXED... NOT LINE NUMBERS...
(define ROMSTART 0)
(define line-number (make-parameter ROMSTART))

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

(define label-location-counter 16)

(define (dec->bin n)
  (define b (bits n))
     (when (> 15 (length b))
       (set! b (pad-left b "0" 15)))
     (set! b (cons "0" b))
     (apply string-append (map (lambda (n) (format "~a" n)) b)))

(define (convert-a-instr instr)
  (cond
    ;; Is a number
    [(regexp-match "^@[0-9]+" instr)
     ;;(displayln "y")
     (define n (string->number (list-ref (regexp-match "^@([0-9]+)" instr) 1)))
     (dec->bin n)
     ]
    ;; Is a label
    [(regexp-match "^@([a-zA-Z0-9_]+)" instr)
     (define lab (list-ref (regexp-match "^@([a-zA-Z0-9_]+)" instr) 1))
     (cond
       [(hash-ref label-locs lab false)
        (dec->bin (hash-ref label-locs lab))]
       [else
        ;;(displayln "x")
        (hash-set! label-locs lab label-location-counter)
        (set! label-location-counter (add1 label-location-counter))
        (dec->bin (hash-ref label-locs lab))])
     ]))
         

(define (is-a-instr? instr)
  (regexp-match "^@" instr))

(define (is-c-instr? instr)
  (not (is-a-instr? instr)))

(define (is-loop-label? instr)
  (regexp-match "\\(.*?\\)" instr))

(define label-locs (make-hash))
   
(define (clear-label-locations)
  (set! label-location-counter 16)
  (line-number ROMSTART)
  (set! label-locs
        (make-hash 
         (map (lambda (n)
                (cons (format "R~a" n) n))
              (range 0 15)))))

(define (strip s)
  (regexp-replaces s '([#px"^(\\s*)" ""]
                       [#px"(\\s*)$" ""])))

(define (convert-instruction instr)
  (set! instr (strip instr))
  (cond
    [(regexp-match "//" instr)
     (define m (regexp-replace "(.*?)(//.*)" instr "\\1"))
     (convert-instruction (strip m))]
    [(< (string-length instr) 1) ""]
    [(is-loop-label? instr)
     (hash-set! label-locs
                (list-ref (regexp-match "\\((.*?)\\)" instr) 1)
                (line-number))
     ""]
    [(is-a-instr? instr)
     (define result (convert-a-instr instr))
     (line-number (add1 (line-number)))
     result]
    [(is-c-instr? instr)
     (define result (convert-c-instr instr))
     (line-number (add1 (line-number)))
     result]
    [else
     (error 'convert-instruction "Error in convert instruction: ~a" instr)]
    ))
  

(define (calculate-label-locations instrs)
  (clear-label-locations)
  (for ([instr instrs])
    (cond
      [(< (string-length instr) 1) ""]
      ;; 
      [(is-loop-label? instr)
       (hash-set! label-locs
                  (list-ref (regexp-match "\\((.*?)\\)" instr) 1)
                  (line-number))
       ""]
      [(is-a-instr? instr)
       (line-number (add1 (line-number)))]
      [(is-c-instr? instr)
       (line-number (add1 (line-number)))]
      [else (error 'convert-instruction "Error in calc label locations: ~a" instr)]
      ))
  (line-number ROMSTART)
  )
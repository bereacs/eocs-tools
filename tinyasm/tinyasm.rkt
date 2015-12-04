#lang racket

(require "core.rkt")

(require racket/cmdline)
(require racket/string)

(define output-rom (make-parameter false))

(define (mangle-name infile)
  (regexp-replace ".asm" infile ".hack"))

(define (pad4 str)
  (define zeroes (- 4 (string-length str)))
  (string-append
   (make-string zeroes #\0) 
   str))

(define (make-spaces str tot)
  (define spaces (- tot (string-length str)))
  (when (< spaces 0)
    (set! spaces 0))
  (make-string spaces #\space))

(define (process infile outfile)
  (define outp (open-output-file outfile #:exists 'replace))
  ;; Initialize the label locations.
  (clear-label-locations)
  
  (define lines (file->list infile read-line))
  ;;(printf "Number of lines: ~a~n" (length lines))
  ;;(printf "LINES: ~a~n" lines)
  
  (define ROMSIZE 0)
  
  (when (output-rom)
    (fprintf outp "uint16_t ROM[] = {~n")
    )
  
  (define longest-line 6)
  (for ([line lines])
    (set! line (string-trim line " "))
    (set! line (convert-instruction line))
    (when (< longest-line (string-length line))
      (set! longest-line (string-length line))))
  
  (for ([line lines]
        ;; Racket quits iterating at the end of the shortest list...
        [lineno (range 1 (add1 (length lines)))])
    (set! line (string-trim line " "))
    
    (when (< 0 (string-length line))
      
      (when (verbose-mode)
        (printf "Line ~a: ~a~n" lineno line))
      (define converted (convert-instruction line))
     
      (cond
        [(comment-mode)
         (when (< 1 (string-length converted))
         (fprintf outp "~a\t// ~a~n" 
                  converted
                  line))]
        [(output-rom)
         (when (< 1 (string-length converted))
           (set! ROMSIZE (add1 ROMSIZE))
           (define hex (number->string (string->number converted 2) 16))
           ;;(printf "ROM HEX: ~a~n" (pad4 hex))
           (fprintf outp "\t0x~a, /* ~a ~a => ~a */~n" 
                    (pad4 hex)
                    line
                    (make-spaces line (+ longest-line 1))
                    converted
                    ))]
        
        [else
         (fprintf outp "~a~n" converted)])
                  
      
      (when (verbose-mode)
        (printf "~n"))
      ))
  
  (when (output-rom)
    ;; Add a 0x0000 to the end of the array.
    (fprintf outp "\t0x0000  /*              END OF LINE      */~n};~n")
    (fprintf outp "#define ROMSIZE ~a~n" (add1 ROMSIZE))
    )
  
  (close-output-port outp)
  )
    

(define tinyasm
  (command-line
   #:program "tinyasm"
   #:once-each
   [("-v" "--verbose") "Print lots of stuff."
                       (verbose-mode true)]
   [("-c" "--comments") "Print comments on every line."
                        (comment-mode true)]
   [("-r" "--rom") "Output a rom.h file."
                   (output-rom true)]
   #:args (infile)
   (begin
     (unless (regexp-match ".asm" infile)
       (printf "Input file must end with a .asm extension.~n")
       (exit))
     (define outfile (mangle-name infile))
     
     (cond
       [(output-rom)
        (process infile "rom.h")]
       [else
        (process infile outfile)])
     )))

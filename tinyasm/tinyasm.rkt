#lang racket

(require "core.rkt")

(require racket/cmdline)
(require racket/string)

(define (process infile outfile)
  (define outp (open-output-file outfile #:exists 'replace))
  
  (define lines (file->list infile read-line))
  
  (for ([line lines]
        [num (range 1 (length lines))])
    (set! line (string-trim line " "))
    (when (< 0 (string-length line))
      (when (verbose-mode)
        (printf "Line ~a: ~a~n" num line))
      
      (cond
        [(comment-mode)
         (fprintf outp "~a\t// ~a~n" 
                  (convert-instruction line)
                  line)]
        [else
         (fprintf outp "~a~n" (convert-instruction line))])
                  
      
      (when (verbose-mode)
        (printf "~n"))
      ))
  
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
   #:args (infile outfile)
   (process infile outfile)))

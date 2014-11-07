#lang racket

(provide (all-defined-out))
 
(define (strip s)
  (regexp-replaces s '([#px"^(\\s*)" ""]
                       [#px"(\\s*)$" ""])))

(define (bits n)
  (let loop ((n n) (acc '()))
    (if (= 0 n) 
        acc
        (loop (arithmetic-shift n -1) (cons (bitwise-and n 1) acc)))))

(define (pad-left ls val leng)
  (for ([cnt (- leng (length ls))])
    (set! ls (cons val ls)))
  ls)

(define (dec->bin n)
  (define b (bits n))
  (when (> 15 (length b))
    (set! b (pad-left b "0" 15)))
  (set! b (cons "0" b))
  (apply string-append (map (lambda (n) (format "~a" n)) b)))

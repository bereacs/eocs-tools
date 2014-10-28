#lang racket/gui

(require framework)
(require "core.rkt")

(define model%
  (class object%
    (define views '())
    (define (add-view v)
      (set! views (cons v views)))
    (define (update)
      (for ([v views])
        (send v update)))
    ))

(define view%
  (class object%
    (define/public (update)
      (error 'view% "Override base update method."))))

;; CALLBACKS
(define assemble-callback
  (make-parameter
   (lambda (b e)
     (printf "ASSEMBLE!\n"))))

(define show-comments?
  (make-parameter false))

(define (build-buttons f)
  (define button-pane (new horizontal-pane%
                           [parent f]
                           [alignment '(right center)]
                           [stretchable-height false]))
  (define cb
    (new check-box%
         [parent button-pane]
         [label "Show Comments"]
         [callback (lambda (b e)
                     (define v (send b get-value))
                     ;; (printf "Current v: ~a~n" v)
                     (show-comments? v) )]))
  
  (define assemble-button
    (new button%
         [label "Assemble"]
         [parent button-pane]
         [callback 
          (lambda (b e)
            ((assemble-callback) b e))]))
  assemble-button
  )

(define no-text%
  (class text%
    (super-new)
    (define insertable? false)
    (define (can-insert? s e)
      insertable?)
    (define/public (enable-insert)
      (set! insertable? true))
    (define/public (disable-insert)
      (set! insertable? false))
    (augment can-insert?)
    ))

(define (build-editors f)
  (define hpane (new horizontal-pane%
                     [parent f]
                     [stretchable-height true]))
  (define asm-editor
    (new text%))
  
  (define asm-canvas
    (new editor-canvas%
         [parent hpane]
         [editor asm-editor]
         [horiz-margin 10]
         [vert-margin 10]
         [stretchable-width true]
         [stretchable-height true]
         [style '(auto-vscroll)]))
  
  (define bin-editor
    (new no-text%))
  (define bin-canvas
    (new editor-canvas%
         [parent hpane]
         [editor bin-editor]
         [horiz-margin 10]
         [vert-margin 10]
         [stretchable-width true]
         [stretchable-height true]
         [style '(auto-vscroll)]))
  
  (values asm-editor bin-editor))
  
(define assembler
  (lambda (bin asm)
    (lambda (b e)
      
      (define asm-text
        (send asm get-text 0 'eof))
      (set! asm-text
            (format "~a~n" asm-text))
      
      (send bin select-all)
      (send bin clear)
      ;; Set Comment Mode
      (comment-mode (show-comments?))
      
      (send bin enable-insert)
      (define lines (regexp-split "\n" asm-text))
      (for ([line lines]
            [lineno (range 1 (length lines))])
        (with-handlers ([exn:fail?
                         (lambda (e)
                           ;; (printf "~a~n" e)
                           (send bin
                                 insert
                                 (format "Line ~a: Error in [ ~a ]~n"
                                         lineno
                                         line)
                                 (send bin get-end-position)))])
          (define converted
            (if (show-comments?)
                (format "~a // ~a~n" 
                        (convert-instruction line)
                        line)
                (format "~a~n" 
                        (convert-instruction line))))
          (send bin insert
                converted
                (send bin get-end-position))
          ))
      (send bin disable-insert))))

(define (main)
  (define f (new frame%
                 [label "TinyASM"]
                 [width 600]
                 [height 400]
                 ))
  
  (define-values (asm bin)
    (build-editors f))
  
  (build-buttons f)
  
  (assemble-callback
   (assembler bin asm))
  
  (send f show true)
  )

(main)
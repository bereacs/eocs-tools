#lang racket/gui

(require framework)
(require "core.rkt")

(define VERSION "20141029")

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
  (define asm-keymap (keymap:get-editor))
  (send asm-editor set-keymap asm-keymap)
  
  (define asmvpane (new vertical-panel% [parent hpane]))
  (new message% [parent asmvpane] [label "Assembly (.asm)"])
  (define asm-canvas
    (new editor-canvas%
         [parent asmvpane]
         [editor asm-editor]
         [horiz-margin 10]
         [vert-margin 10]
         [stretchable-width true]
         [stretchable-height true]
         [style '(auto-vscroll)]))
  
  (define bin-editor
    (new no-text%))
  (define bin-keymap (keymap:get-editor))
  (send bin-editor set-keymap bin-keymap)
  
  (define binvpane (new vertical-pane% [parent hpane]))
  (new message% [parent binvpane] [label "Binary (.hack)"])
  (define bin-canvas
    (new editor-canvas%
         [parent binvpane]
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
      
      (calculate-label-locations lines)
      
      (for ([line lines])
        (define current-line (line-number))
        (define converted-instruction
          (with-handlers ([exn:fail?
                           (lambda (e)
                             (printf "~a~n" e))])
            (convert-instruction line)))
          ;; (displayln converted-instruction)
          (define display-string
            (if (show-comments?)
                (format "~a // L~a: ~a~n" 
                        converted-instruction
                        current-line
                        line)
                (format "~a~n" 
                        converted-instruction)))
          
        (send bin insert
              display-string
              (send bin get-end-position))
        )
      (send bin disable-insert))))

(define (make-save-callback f suffix editor)
  (lambda (m e)
    (finder:default-extension suffix)
    (cond
      [(equal? suffix ".asm")
       (finder:default-filters '(("Assembly" "*.asm")))]
      [else 
       (finder:default-filters '(("Binary" "*.hack")))])
    (define d (seconds->date (current-seconds)))
    (define (pad n)
      (if (< n 10)
          (format "0~a" n) n))
    (define filepath
      (finder:common-put-file
       (format "~a~a~a-~a~a"
               (pad (date-year d))
               (pad (date-month d))
               (pad (date-day d))
               (cond 
                 [(equal? ".asm" suffix) "assembly"]
                 [else "binary"])
               suffix)
       false
       true
       (format "Save ~a file" suffix)
       (byte-regexp (string->bytes/utf-8 (format ".*\\~a" suffix)))
       (format "File must end with ~a" suffix)
       f
      ))
    
    (when filepath
      (define op (open-output-file filepath #:exists 'replace))
      (for ([line (regexp-split "\n" (send editor get-text))])
        (when (> (string-length line) 1)
          (fprintf op "~a~n" line)))
      (close-output-port op))
    ))
    

(define open-assembly-callback
  (make-parameter
   (lambda (m e)  'foo)))

(require racket/file)
(define (build-menu f bin asm)

  (open-assembly-callback 
   (lambda (m e)
     (finder:default-extension ".asm")
     (finder:default-filters '(("Assembly" "*.asm")))
     (define filepath (finder:common-get-file
                       false
                       "Select .asm to load"
                       (byte-regexp
                        (string->bytes/utf-8 ".*?\\.asm"))
                       "Load .asm files only."
                       false))
     (when filepath
       (send bin select-all)
       (send bin clear)
       (send asm select-all)
       (send asm clear)
       (send asm insert 
           (file->string filepath)
           0
           ))))
     
  (define mb (new menu-bar% 
                  [parent f]))
  (define file 
    (new menu%
         [label "File"]
         [parent mb]))
  
  (new menu-item%
       [label "&Save Assembly"]
       [parent file]
       [callback (make-save-callback f ".asm" asm)])
  (new menu-item%
       [label "&Open Assembly"]
       [parent file]
       [callback (lambda (m e)
                   ((open-assembly-callback) m e))])
  (new menu-item%
       [label "Save &Hack"]
       [parent file]
       [callback (make-save-callback f ".hack" bin)])
  
  (send mb enable true)
  mb)
                  

(define (main)
  (define f (new frame%
                 [label (format "TinyASM ~a" VERSION)]
                 [width 600]
                 [height 400]
                 ))
  
  (define-values (asm bin)
    (build-editors f))
  
  (build-buttons f)
  
  (assemble-callback
   (assembler bin asm))
  
  (build-menu f bin asm)
  
  (send f show true)
  )

(main)
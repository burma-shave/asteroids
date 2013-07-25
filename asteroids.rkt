#lang racket

(require racket/gui)
(require racket/draw)

(define SHIP-POINTS `(,(make-object point% 0 0)
        ,(make-object point% 60 10)
        ,(make-object point% 0 20)))

(define (basic-path) 
  (define path (new dc-path%))
  (send path move-to 0 0)
  (send path lines SHIP-POINTS)
  (send path close)
  (send path translate -20 -10)
  path)

(define ship%
  (class object%
    (define path (new dc-path%))
    (define position (pt 200 200))
    
    (send path move-to 0 0)
    (send path lines SHIP-POINTS)
    (send path close)
    (send path translate -20 -10)    
              
    (define/public (rotate-left)
      (send path rotate -0.1))
      
    (define/public (rotate-right)      
      (send path rotate 0.1))
    
    (define/public (up)
      (set! position (struct-copy pt position [y (- (pt-y position) 5)])))
    
    (define/public (down)
      (set! position (struct-copy pt position [y (+ (pt-y position) 5)])))
          
    (define/public (draw dc)
      (send path translate (pt-x position) (pt-y position))
      (send dc draw-path path)
      (send path translate (- 0 (pt-x position)) (- 0 (pt-y position))))
    
    (super-new)))

(struct pt (x y) #:transparent)

(define game-world%
  (class object%    
    (define player (new ship%))
    
    (define/public (rotate-left)
      (send player rotate-left))
    
    (define/public (rotate-right)
      (send player rotate-right))
    
    (define/public (up)
      (send player up))
    
    (define/public (down)
      (send player down))
    
    (define/public (draw canvas dc)      
      (send player draw dc))
          
    (super-new)))

(define frame (new frame%
                   [label "Example"]
                   [width 600]                   
                   [height 600]))


(define space-canvas%      
  (class canvas%    
    (define game-world (new game-world%))
    
    (define/override (on-event event)
      (send msg set-label "Canvas mouse"))
    
    (define/override (on-char event)      
      (define key-code (send event get-key-code))
      (cond [(equal? key-code 'left) (send game-world rotate-left)
                                     (send frame refresh)]
            [(equal? key-code 'right) (send game-world rotate-right)
                                     (send frame refresh)]
            [(equal? key-code 'up) (send game-world up)
                                     (send frame refresh)]
            [(equal? key-code 'down) (send game-world down)
                                     (send frame refresh)]
            [(key-code-symbol? key-code) 
             (send msg set-label 
                   (string-append "'" 
                                  (symbol->string (send event get-key-code))))]
            [else 
             (send msg set-label  (string (send event get-key-code)))]))
        
    (super-new [paint-callback
                (lambda (canvas dc)
                  (send game-world draw canvas dc))])))

(define msg (new message% [parent frame]
                          [label "No events so far..."]))

(new space-canvas% [parent frame])

(send frame show #t)
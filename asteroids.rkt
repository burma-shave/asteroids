#lang racket

; 1. body at rest
; 2. body in motion x
; 3. body in motion y
; 4. wrap screen
; 5. thrust
; 6. resistence

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
    (define x-velocity 0)    
    (define y-velocity 0)    
    (define mass 1)
    (define thrust-force  0.5)    
    (define burn-duration 1)
    (define angle pi)
    
    (send path move-to 0 0)
    (send path lines SHIP-POINTS)
    (send path close)
    (send path translate -20 -10)    
              
    (define/public (rotate-left)
      (set! angle (+ angle 0.1))
      (send path rotate 0.1))
      
    (define/public (rotate-right)      
      (set! angle (- angle 0.1))
      (send path rotate -0.1))
    
    (define/public (thrust)
      (set! x-velocity (final-velocity x-velocity (x-thrust-component)))
      (set! y-velocity (final-velocity y-velocity (y-thrust-component))))
    
    (define/public (update-position)      
      (define x-position (- (pt-x position) (* x-velocity burn-duration)))
      (define y-position (+ (pt-y position) (* y-velocity burn-duration)))
      (set! position (pt x-position y-position)))
    
    (define (final-velocity initial-velocity force)
      (+ initial-velocity (/ (* force burn-duration) mass)))
                                
    (define (x-thrust-component)
      (* (cos angle) thrust-force))
    
    (define (y-thrust-component)
      (* (sin angle) thrust-force))
    
    (define/public (up)
      (set! position (struct-copy pt position [y (- (pt-y position) 5)])))
    
    (define/public (down)
      (set! position (struct-copy pt position [y (+ (pt-y position) 5)])))
          
    (define/public (draw dc)
      (update-position)
      (send path translate (pt-x position) (pt-y position))
      (send dc draw-path path)
      (send path translate (- 0 (pt-x position)) (- 0 (pt-y position))))
    
    (super-new)))

(struct pt (x y) #:transparent)

(define player (new ship%))

(define game-world%
  (class object%    
    
    
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

(define game-world (new game-world%))

(define space-canvas%      
  (class canvas%    
    
    
    (define/override (on-event event)
      (send msg set-label "Canvas mouse"))
    
    (define/override (on-char event)      
      (define key-code (send event get-key-code))
      (cond [(equal? key-code 'left) (send game-world rotate-left)]
            [(equal? key-code 'right) (send game-world rotate-right)]
            [(equal? key-code 'up) (send player thrust)]
            [(equal? key-code 'down) (send game-world down)]           
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
  
(define timer (new timer% 
                   [notify-callback (lambda () (send frame refresh))]
                   [interval 30]))

(send frame show #t)
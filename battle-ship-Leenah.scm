; battle-ship.scm

(import image)
(import canvas)
(import reactive)
(import lab)
(problem "This file lacks documentation on some functions, and there's a lot of commneted code (under experimentation.)")



(define white-peg
  (overlay
    (circle 10 "outline" "black")
    (circle 10 "solid" "white")))

(define red-peg
  (overlay
    (circle 10 "outline" "black")
    (circle 10 "solid" "red")))


;from Maddy
;;; (square-piece size) -> drawing?
;;;    size : integer? size > 0
;;; Returns a solid, grey square of size 'size' with a black circle 
;;; of radius 'size/5' overlayed on top of it
(define square-piece
  (lambda (size)
    (overlay/align "middle" "center"
                    (circle (/ size 5) "solid" "black") 
                    (square size "outline" "black")
                    (square size "solid" "gray"))))



;;; (grid-vec vec i)-> void?
;;;    vec : vector?
;;;    i : integer? represents the index of the vector
;;; Recursively goes through the elements in 'vec'
;;; and sets each elements to be 'square-piece'
(define grid-vec
  (lambda (vec i)
    (if (>= i (vector-length vec))
      void
      (begin
        (vector-set! vec i (square-piece 50))
        (grid-vec vec (+ i 1))
        vec))))

;;; (grid) -> vector? of-image?
;;; Returns the drif of the game as a vector
(define grid
  (grid-vec (make-vector 100 0) 0))


;;; Try overlying the ships

(define make-ship
  (lambda (size)
    (let
      ([ship-base (solid-rectangle size (/ size 3) "green")]
       [ship-edge (solid-triangle (/ size 3) "green")])
        (beside (rotate -90 ship-edge) 
                ship-base 
                (rotate 90 ship-edge)))))


;;; use 'ship' for testing
(define ship
  (make-ship 150))
; (vector-set! grid 5 ship)

; Idk how to overlay a ship on top of the vector. 
; How can I make the ship perfectly fit horizontally and perfectly on the vector grid?
 ; ship



;;; incomplete
; (define vector->canvas
;   (lambda (vec i row)
;   (let
;     ([canv (make-canvas 100 100)]))))

; (define make-row
;   (lambda (vec i width height x y)
;     (let ([canv (make-canvas width height)])
;       (if (>= x width)
;           void
;           (begin
;            (canvas-drawing! canv x y (vector-ref vec i)) 
;            (make-row vec (+ i 1) width height (+ x 50) y)
;             canv)))))
; (make-row grid 0 500 500 0 0)



;The code below works
(part "A reactive grid of pegs")
;;; The code below is using the 'reactive library' for more information check the
;;; 'Interactivity, Events, and Reactivity' reading.



;initial model struct
(struct state (peg x y))

; view function
(define view
  (lambda (st canvass)
    (match st
      [(state peg x y)
        (begin
          (ignore
          ;;; This is purposefully low-level for testing
          (begin 
                 (canvas-drawing! canvass 0 0 (vector-ref grid 0))
                 (canvas-drawing! canvass 50 0 (vector-ref grid 0))
                 (canvas-drawing! canvass 50 50 (vector-ref grid 0))
                 (canvas-drawing! canvass 0 50 (vector-ref grid 0))))
          (cond 
            [(equal? (state-peg st) white-peg)
              (canvas-drawing! canvass x y red-peg)]
        
            [(equal? (state-peg st) red-peg)
              (canvas-drawing! canvass x y white-peg)]
            [else "what's going on?"]))])))
      
;the update function  
(define update
  (lambda (msg st)
    (match st
      [(state peg x y)
        (match msg
          [(event-mouse-click btn cx cy)
            (if (equal? (state-peg st) white-peg)
                (state red-peg (truncate(- cx 10)) (truncate(- cy 10)))
                (state white-peg (truncate(- cx 10)) (truncate(- cy 10))))])])))
                      
             

(display
  (reactive-canvas
    100 100
    ;initial model
      (state (circle 10 "solid" "black") 15 15)
    ;view function
      view
    ;update fucntion
      update
    ;subscriptions
    (on-mouse-click)))

(problem "Click anywhere on the canvas to place the peg. 
  \n Tip: You have to click exactly in the middle of the 
          black circle to place the red/white peg accurately.
  \n Tip: The fist click makes a red peg, click a second time to place a white peg on the grid")


; ships-w-aiden.scm

; battle-ship.scm

(import image)
(import canvas)
(import reactive)
(import lab)



; (define white-peg
;   (overlay
;     (circle 10 "outline" "black")
;     (circle 10 "solid" "white")))

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
  (rotate 90 (make-ship 100)))


(define peg-on-square
  (overlay red-peg (square-piece 50)))

(define ship-on-square
  (overlay ship (square-piece 50)))

(define alter-objects
  (lambda (vec i)
   (begin 
    (cond
      [(equal? (vector-ref vec i) peg-on-square)
        (vector-set! vec i ship-on-square)]
      [(equal? (vector-ref vec i) ship-on-square)
        (vector-set! vec i peg-on-square)]
      [else (vector-set! vec i (square-piece 50))]) 
    (vector-ref vec i))
    ))
(alter-objects (vector peg-on-square) 0)
(alter-objects (vector ship-on-square) 0)


;;; (grid-vec vec i)-> void?
;;;    vec : vector?
;;;    i : integer? represents the index of the vector
;;; Recursively goes through the elements in 'vec'
;;; and sets each elements to be 'square-piece'
(define grid-vec
  (lambda (vec i griddy)
    (if (>= i (vector-length vec))
      void
      (begin
        (if (equal? (vector-ref griddy i) 1)
            ; (alter-objects vec i)
            ; (alter-objects vec i))
             (vector-set! vec i (overlay red-peg (square-piece 50)))
             (vector-set! vec i (square-piece 50)))
        (grid-vec vec (+ i 1) griddy)
        vec))))

;;; (grid) -> vector? of-image?
;;; Returns the drif of the game as a vector

(define grid0 (make-vector 100 0))
(define grid1 (make-vector 100 0))

(define griddy (vector (square-piece 50)))
(define grid
  (grid-vec grid0 0 griddy))



;The code below works
(part "A reactive grid of pegs")
;;; The code below is using the 'reactive library' for more information check the
;;; 'Interactivity, Events, and Reactivity' reading.



;initial model struct
(struct state (peg x y griddy-p1 griddy-p2 turns))


(define grid-maker
  (lambda (st canvass i k j vec)
  (if (equal? k 500)
    null
    (begin (canvas-drawing! canvass i k (vector-ref vec j))
      (if (< i 450)
          (grid-maker st canvass (+ i 50) k (+ j 1) vec)
          (grid-maker st canvass 0 (+ k 50) (+ j 1) vec))))))

(define view
  (lambda (st canvass)
    (match st
      [(state peg x y griddy-p1 griddy-p2 turns)
        (begin
          (ignore
            (begin 
              (if (negative? turns)
                  (grid-maker st canvass 0 0 0 (grid-vec (make-vector 100 0) 0 griddy-p1)); (canvas-circle! canvass x y 20 "solid" "blue");
                  (grid-maker st canvass 0 0 0 (grid-vec (make-vector 100 0) 0 griddy-p2)); (canvas-circle! canvass x y 20 "solid" "red") ;
                ))))])))
      
;the update function  
(define update
  (lambda (msg st)
    (match st
      [(state peg x y griddy-p1 griddy-p2 turns)
        (match msg
          [(event-mouse-click btn cx cy)
            (if (negative? turns)
                (let* ([a (vector-set! griddy-p1 (truncate (+ (* (/ cy 50) 10) (/ cx 50))) 1)])
                        (state peg x y griddy-p1 griddy-p2 (* turns -1)))
                (let* ([a (vector-set! griddy-p2 (truncate (+ (* (/ cy 50) 10) (/ cx 50))) 1)])
                      (state peg x y griddy-p1 griddy-p2 (* turns -1)))
              )])])))

(display
  (reactive-canvas
    500 500
    ;initial model
      (state (circle 10 "solid" "black") 15 15 grid0 grid1 1)
    ;view function
      view
    ;update fucntion
      update ; eventualy this will need to update the grid function.
    ;subscriptions
    (on-mouse-click)))



;Checking if the ship is sunken:

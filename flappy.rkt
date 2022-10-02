;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname flappy) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)


;; funny flappy bird program
;; ehehehe

(@htdw GameState)

;; CONSTANTS ===================================================================

(define (get-sprite s)
  (bitmap/url (string-append "https://raw.githubusercontent.com/samuelcust/
                             flappy-bird-assets/master/sprites/" s ".png")))

(define FLAPPY-IMG (get-sprite "yellowbird-midflap"))
(define FLAPPY-LEN (image-height FLAPPY-IMG))
(define FLAPPY-X-POS 50)
(define MAX-Y-SPEED 10)
(define MTS (get-sprite "yellowbird-midflap"))
(define WIDTH (image-width MTS))
(define HEIGHT(image-height MTS))
(define X-SPEED 5)
(define PIPE-WIDTH (/ WIDTH 5))
(define PIPE-V-GAP (/ HEIGHT 10))
(define PIPE-H-GAP (* 1.2 PIPE-WIDTH))
(define ACCELERATION 2)
(define ANGLE-UP 45)
(define ANGLE-DOWN -45)


;; DATA DEFINITIONS ============================================================

(@htdd Flappy)
(define-struct flappy (y dy r))
;; Flappy is (make-flappy Number Number Number)
;; interp. y  - the vertical position of Flappy in px
;;         dy - the vertical velocity of Flappy in px/tick
;;         r  - the rotation of Flappy in degrees
;; CONSTRAINT: r in [-45, 45]

(@dd-template-rules compound) ;3 fields

(define (fn-for-flappy f)
  (... (flappy-y f)
       (flappy-dy f)
       (flappy-r f)))


(@htdd Pipe)
(define-struct pipe (x y))
;; Pipe is (make-pipe Number Number)
;; interp. x, y - the position of the top left corner of the pipes in px

(@dd-template-rules compound) ;2 fields

(define (fn-for-pipe p)
  (... (pipe-x p)
       (pipe-y p)))


(@htdd ListOfPipe)
;; ListOfPipe is one of:
;; - empty
;; - (cons Pipe ListOfPipe)
;; interp. a list of pipe obstacles

(@dd-template-rules one-of
                    atomic-distinct
                    compound
                    ref
                    self-ref)

(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-pipe (first lob))
              (fn-for-lob (rest lob)))]))


(@htdd GameState)
(define-struct gs (flappy lop points state))
;; GameState is (make-gs Flappy ListOfPipe Natural Boolean)
;; interp. flappy - the bird
;;         lop    - a list of pipe obstacles
;;         points - the number of pipes passed
;;         state  - true if game is in progress

(define START (make-gs (make-flappy 0 0 0) empty 0 true))

(@dd-template-rules compound
                    ref
                    ref)

(define (fn-for-gs gs)
  (... (fn-for-flappy (gs-flappy gs))
       (fn-for-lop (gs-lop gs))
       (gs-points gs)
       (gs-state gs)))


;; FUNCTIONS ===================================================================

(@htdf main)
(@signature GameState -> GameState)
;; start world program with (main START)

(@template-origin htdw-main)

(@template
 (define (main gs)
   (big-bang gs              ;GameState
     (on-tick tock)          ;GameState -> GameState
     (to-draw render)        ;GameState -> Image
     (on-mouse handle-mouse) ;GameState Integer Integer MouseEvent -> GameState 
     (on-key handle-key)     ;GameState KeyEvent -> GameState 
     (state false))))          

(define (main gs)
  (big-bang gs
    (to-draw render)
    (on-tick tock)
    (on-mouse handle-mouse)
    (on-key handle-key)))


(@htdf render)
(@signature GameState -> Image)
;; render GameState on MTS
;; !!!
(define (render gs) MTS)


(@htdf tock)
(@signature GameState -> GameState)
;; produce the next GameState

;; !!! check expects go here
;(define (tock gs) gs)

(@template-origin GameState)

(@template
 (define (tock gs)
   (... (fn-for-flappy (gs-flappy gs))
        (fn-for-lop (gs-lop gs))
        (gs-points gs)
        (gs-state gs))))

(define (tock gs)
  (cond [(false? (gs-state gs)) gs]
        [(touch-pipe? (gs-flappy gs) (gs-lop gs))
         (make-gs (tock-flappy (gs-flappy gs))
                  (gs-lop gs)
                  (gs-points gs)
                  false)]
        [else
         (make-gs (tock-flappy (gs-flappy gs))
                  (tock-pipes (gs-lop gs))
                  (gs-points gs)
                  true)]))


(@htdf tock-flappy)
(@signature Flappy -> Flappy)
;; produce the next Flappy
;; !!!
(define (tock-flappy f) f)

(@htdf tock-pipes)
(@signature ListOfPipe -> ListOfPipe)
;; produce the next ListOfPipes
;; !!!
(define (tock-pipes lop) lop)

(@htdf touch-pipe?)
(@signature Flappy ListOfPipe -> Boolean)
;; end the game if Flappy is overlapping a pipe
;; !!!
(define (touch-pipe? f lop) false)


(@htdf handle-mouse)
(@signature GameState Integer Integer MouseEvent -> GameState)
;; accelerate Flappy upwards on "button-down"

;; !!! make this cleaner
(define LOP10 (cons (make-pipe 200 300) (cons (make-pipe 300 400) empty)))
(define FP10 (make-flappy 300 -4 0))
(define FP11 (make-flappy 200 -10 ANGLE-DOWN))

(check-expect (handle-mouse (make-gs FP10 LOP10  500 true) 0 0 "button-down")
              (make-gs (make-flappy 300 MAX-Y-SPEED ANGLE-UP) LOP10 500 true))
(check-expect (handle-mouse (make-gs FP11 LOP10  500 true) WIDTH HEIGHT "drag")
              (make-gs FP11 LOP10 500 true))

;(define (handle-mouse gs x y me) gs)

(@template-origin MouseEvent)

(@template
 (define (handle-mouse gs x y me)
   (cond [(mouse=? "button-down" me)
          (... (fn-for-gs gs))]
         [else
          (... (fn-for-gs gs))])))

(define (handle-mouse gs x y me)
  (cond [(mouse=? "button-down" me)
         (start-flap gs)]     
        [else gs]))


(@htdf handle-key)
(@signature GameState KeyEvent -> GameState)
;; accelerate Flappy upwards on " " and "up"

;; !!! make this cleaner
(check-expect (handle-key (make-gs FP10 LOP10  500 true) "up")
              (make-gs (make-flappy 300 MAX-Y-SPEED ANGLE-UP) LOP10 500 true))
(check-expect (handle-key (make-gs FP11 LOP10  500 true) " ")
              (make-gs (make-flappy 200 MAX-Y-SPEED ANGLE-UP) LOP10 500 true))
(check-expect (handle-key (make-gs FP11 LOP10  500 true) "a")
              (make-gs FP11 LOP10 500 true))

;(define (handle-key gs ke) gs)

(@template-origin KeyEvent)

(@template
 (define (handle-key gs ke)
   (cond [(key=? " " ke)
          (... (fn-for-gs gs))]
         [else
          (... (fn-for-gs gs))])))

(define (handle-key gs ke)
  (cond [(key=? " " ke)
         (start-flap gs)]
        [(key=? "up" ke)
         (start-flap gs)]
        [else gs]))


(@htdf start-flap)
(@signature GameState -> GameState)
;; update GameState so that Flappy flaps

;; !!! check expects go here
;(define (start-flap gs) gs)

(@template-origin GameState)

(@template
 (define (start-flap gs)
   (... (fn-for-flappy (gs-flappy gs))
        (gs-lop gs)
        (gs-points gs)
        (gs-state gs))))

(define (start-flap gs)
  (make-gs (flap (gs-flappy gs)) (gs-lop gs) (gs-points gs) (gs-state gs)))


(@htdf flap)
(@signature Flappy -> Flappy)
;; accelerate Flappy upwards

;; !!! check expects go here
;(define (flap f) f)

(@template-origin Flappy)

(@template
 (define (flap f)
   (... (flappy-y f)
        (flappy-dy f)
        (flappy-r f))))

(define (flap f)
  (make-flappy (flappy-y f) MAX-Y-SPEED ANGLE-UP))


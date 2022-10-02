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
  (bitmap/url
   (string-append
    "https://raw.githubusercontent.com/Hanson-Sun/FLAPPY_BSL/main/sprites/"
    s
    ".png")))

(define FLAPPY-IMG (get-sprite "yellowbird-midflap"))
(define FLAPPY-LEN (image-height FLAPPY-IMG))
(define FLAPPY-X-POS 50)
(define MAX-Y-SPEED 10)
(define MTS (get-sprite "background-day"))
(define WIDTH (image-width MTS))
(define HEIGHT (image-height MTS))
(define X-SPEED 5)
(define PIPE-WIDTH (/ WIDTH 5.5))
(define PIPE-V-GAP (* FLAPPY-LEN 4.5))
(define PIPE-H-GAP (* 1.2 PIPE-WIDTH))
(define PIPE-COLOR "darkgreen") ;!!! change color lol?
(define GRAVITY 2)
(define MAX-ANGLE 30)
(define MIN-ANGLE -90)
(define POINTS-X (/ WIDTH 2)) ; x position of where points show up
(define POINTS-Y (/ HEIGHT 5)) ; y position of where points show up
(define TEXT-COLOR "white")
(define FONT-SIZE 20)


;; DATA DEFINITIONS ============================================================

(@htdd Flappy)
(define-struct flappy (y dy r))
;; Flappy is (make-flappy Number Number Number)
;; interp. y  - the vertical position of Flappy in px
;;         dy - the vertical velocity of Flappy in px/tick
;;         r  - the rotation of Flappy in degrees

(define F1 (make-flappy (/ HEIGHT 2) 5 45))
(define F2 (make-flappy (/ HEIGHT 2.5) -2 -45))

(@dd-template-rules compound) ;3 fields

(define (fn-for-flappy f)
  (... (flappy-y f)
       (flappy-dy f)
       (flappy-r f)))


(@htdd Pipe)
(define-struct pipe (x y))
;; Pipe is (make-pipe Number Number)
;; interp. x, y - the position of the top left corner of the bottom pipe in px

(define P1 (make-pipe 0 (/ HEIGHT 2)))
(define P2 (make-pipe (/ WIDTH 2) (/ HEIGHT 3)))

(@dd-template-rules compound) ;2 fields

(define (fn-for-pipe p)
  (... (pipe-x p)
       (pipe-y p)))

(@htdd ListOfPipe)
;; ListOfPipe is one of:
;; - empty
;; - (cons Pipe ListOfPipe)
;; interp. a list of pipe obstacles

(define LOP1 empty)
(define LOP2 (cons P1 (cons P2 empty)))

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

(define START (make-gs (make-flappy (/ HEIGHT 2) 0 0) empty 0 true))
(define GS1 (make-gs F2 LOP2 10 true))
(define GS-END (make-gs F2 LOP2 20 false))

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
;; render points on game render

(check-expect (render START)
              (place-image (text (number->string (gs-points START))
                                 FONT-SIZE TEXT-COLOR)
                           POINTS-X
                           POINTS-Y
                           (render-game START)))

(check-expect (render GS1)
              (place-image (text (number->string (gs-points GS1))
                                 FONT-SIZE TEXT-COLOR)
                           POINTS-X
                           POINTS-Y
                           (render-game GS1)))

(check-expect (render GS-END)
              (place-image
               (above (text (number->string (gs-points GS-END))
                            (* 2 FONT-SIZE) TEXT-COLOR)
                      (text "GAME OVER" (* 2 FONT-SIZE) TEXT-COLOR))
               POINTS-X
               POINTS-Y
               (render-game GS-END)))

;(define (render gs) MTS)

(@template-origin GameState)
(@template
 (define (render gs)
   (... (fn-for-flappy (gs-flappy gs))
        (fn-for-lop (gs-lop gs))
        (gs-points gs)
        (gs-state gs))))

(define (render gs)
  (if (gs-state gs)
      (place-image (text (number->string (gs-points gs)) FONT-SIZE TEXT-COLOR)
                   POINTS-X
                   POINTS-Y
                   (render-game gs))
      (place-image (above (text (number->string (gs-points gs))
                                (* 2 FONT-SIZE) TEXT-COLOR)
                          (text "GAME OVER" (* 2 FONT-SIZE) TEXT-COLOR))
                   POINTS-X
                   POINTS-Y
                   (render-game gs))))

(@htdf render-game)
(@signature GameState -> Image)
;; render game (bird and pipe) on MTS with pipes
(check-expect (render-game START)
              (place-image (rotate (flappy-r (gs-flappy START)) FLAPPY-IMG)
                           FLAPPY-X-POS
                           (flappy-y (gs-flappy START))
                           (render-pipes (gs-lop START))))

(check-expect (render-game GS1)
              (place-image (rotate (flappy-r (gs-flappy GS1)) FLAPPY-IMG)
                           FLAPPY-X-POS
                           (flappy-y (gs-flappy GS1))
                           (render-pipes (gs-lop GS1))))


;(define (render-game gs) MTS)

(@template-origin GameState)
(@template
 (define (render-game gs)
   (... (fn-for-flappy (gs-flappy gs))
        (fn-for-lop (gs-lop gs))
        (gs-points gs)
        (gs-state gs))))

(define (render-game gs)
  (place-image (rotate (flappy-r (gs-flappy gs)) FLAPPY-IMG)
               FLAPPY-X-POS
               (flappy-y (gs-flappy gs))
               (render-pipes (gs-lop gs))))


(@htdf render-pipes)
(@signature ListOfPipe -> Image)
;; render pipes on mts

(check-expect (render-pipes LOP1) MTS)

;(define (render-pipes lop) MTS)

(@template-origin ListOfPipe)
(@template
 (define (render-pipes lop)
   (cond [(empty? lop) (...)]
         [else
          (... (fn-for-pipe (first lop))
               (render-pipes (rest lop)))])))

(define (render-pipes lop)
  (cond [(empty? lop) MTS]
        [else
         (render-pipe (first lop)
                      (render-pipes (rest lop)))]))

(@htdf render-pipe)
(@signature Pipe Image -> Image)
;; render pipe on a given image

(check-expect (render-pipe P1 MTS)
              (place-image (generate-pipe-img P1)
                           (+ (pipe-x P1)(/ PIPE-WIDTH 2)) (/ HEIGHT 2) MTS))

(check-expect (render-pipe P2
                           (place-image (generate-pipe-img P1)
                                        (+ (pipe-x P1)(/ PIPE-WIDTH 2))
                                        (pipe-y P1) MTS))
              (place-image (generate-pipe-img P2)
                           (+ (pipe-x P2)(/ PIPE-WIDTH 2)) (/ HEIGHT 2)
                           (place-image (generate-pipe-img P1)
                                        (+ (pipe-x P1)(/ PIPE-WIDTH 2))
                                        (/ HEIGHT 2) MTS)))

;(define (render-pipe p img) MTS)

(@template-origin Pipe)
(@template
 (define (render-pipe p img)
   (... (pipe-x p)
        (pipe-y p)
        img)))

(define (render-pipe p img)
  (place-image (generate-pipe-img p)
               (+ (pipe-x p) (/ PIPE-WIDTH 2))
               (/ HEIGHT 2)
               img))

(@htdf generate-pipe-img)
(@signature Pipe -> Image)
;; generate image of pipe

(check-expect (generate-pipe-img P1)
              (above (rectangle PIPE-WIDTH (- HEIGHT PIPE-V-GAP (pipe-y P1))
                                "solid" PIPE-COLOR)
                     (rectangle PIPE-WIDTH PIPE-V-GAP
                                "outline" (make-color 0 0 0 0))
                     (rectangle PIPE-WIDTH (pipe-y P1)
                                "solid" PIPE-COLOR)))
(check-expect (generate-pipe-img P2)
              (above (rectangle PIPE-WIDTH (- HEIGHT PIPE-V-GAP (pipe-y P2))
                                "solid" PIPE-COLOR)
                     (rectangle PIPE-WIDTH PIPE-V-GAP
                                "outline" (make-color 0 0 0 0))
                     (rectangle PIPE-WIDTH (pipe-y P2)
                                "solid" PIPE-COLOR)))

;(define (generate-pipe-img p) empty-image)

(@template-origin Pipe)
(@template
 (define (generate-pipe-img p)
   (... (pipe-x p)
        (pipe-y p)
        img)))
;; the pipe-y coordinate gives the top left corner of the bottom pipe
(define (generate-pipe-img p)
  (above (rectangle PIPE-WIDTH (- HEIGHT PIPE-V-GAP (pipe-y p))
                    "solid" PIPE-COLOR)
         (rectangle PIPE-WIDTH PIPE-V-GAP "outline" (make-color 0 0 0 0))
         (rectangle PIPE-WIDTH (pipe-y p)
                    "solid" PIPE-COLOR)))


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

;; !!! check expects go here
;(define (tock-flappy f) f)

(@template-origin Flappy)

(@template
 (define (tock-flappy f)
   (... (flappy-y f)
        (flappy-dy f)
        (flappy-r f))))

(define (tock-flappy f)
  (make-flappy (+ (flappy-y f) (flappy-dy f))
               (+ (flappy-dy f) GRAVITY)
               (cond [(> (* (flappy-dy f) (/ MAX-ANGLE MAX-Y-SPEED)) MAX-ANGLE)
                      MAX-ANGLE]
                     [(< (* (flappy-dy f) (/ MAX-ANGLE MAX-Y-SPEED)) MIN-ANGLE)
                      MIN-ANGLE]
                     [else (* (flappy-dy f) (/ MAX-ANGLE MAX-Y-SPEED))])))


(@htdf tock-pipes)
(@signature ListOfPipe -> ListOfPipe)
;; produce the next ListOfPipes
;; !!!
(define (tock-pipes lop) lop)

(@template-origin ListOfPipe)
(@template
 (define (tock-pipes lop)
   (cond [(empty? lop) (...)]
         [else
          (... (fn-for-pipe (first lop))
               (tock-pipes (rest lop)))])))



(@htdf touch-pipe?)
(@signature Flappy ListOfPipe -> Boolean)
;; end the game if Flappy is overlapping a pipe
;; !!!
(define (touch-pipe? f lop) false)


(@htdf handle-mouse)
(@signature GameState Integer Integer MouseEvent -> GameState)
;; accelerate Flappy upwards on "button-down"

;; !!! check expects go here
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

;; !!! check expects go here
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
  (make-flappy (flappy-y f) (- MAX-Y-SPEED) (flappy-r f)))

;; RANDOM TESTING CORNER ===============================

;(render-pipe P1 MTS)
;(render-game GS1)
;(render GS-END)
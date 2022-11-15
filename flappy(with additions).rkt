;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname flappy) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
(define FLAPPY-LEN (image-height FLAPPY-IMG)) ; length of flappy image
(define FLAPPY-HFLEN (/ FLAPPY-LEN 2)) ; half the length of flappy image
(define FLAPPY-X-POS 50) ; x position of flappy
(define MAX-Y-SPEED 10)
(define MIN-Y-SPEED 10) ; constrain flappy falling speed
(define MTS (get-sprite "background-day"))
(define WIDTH (image-width MTS))
(define HEIGHT (image-height MTS))
(define PIPE-WIDTH (/ WIDTH 5.5))
(define PIPE-V-GAP (* FLAPPY-LEN 5.5)) ; vertical gap between pipes
(define PIPE-H-GAP (* 3.1 PIPE-WIDTH)) ; horizontal gap between pipes
(define PIPE-SPEED 3) ; speed of pipe
(define PIPE-COLOR "darkgreen") ;!!! change color lol?
(define GRAVITY 0.9) ; downward acceleration that flappy experiences
(define MAX-ANGLE -20) ; maximum upward rotation that flappy experiences
(define MIN-ANGLE 90) ; minimum angle when flappy is falling
(define POINTS-X (/ WIDTH 2)) ; x position of where points show up
(define POINTS-Y (/ HEIGHT 5)) ; y position of where points show up
(define TEXT-COLOR "white")
(define FONT-SIZE 20)
(define ROTATE-SPEED 5) ; speed of which flappy rotates when falling
(define GOLD-POINTS 30) ; points for getting a gold medal
(define SILVER-POINTS 20) ; points for getting a silver medal
(define BRONZE-POINTS 10) ; points for getting a bronze medal
(define flappy-font (list
                     (get-sprite "0")
                     (get-sprite "1")
                     (get-sprite "2")
                     (get-sprite "3")
                     (get-sprite "4")
                     (get-sprite "5")
                     (get-sprite "6")
                     (get-sprite "7")
                     (get-sprite "8")
                     (get-sprite "9")))



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
(define P3 (make-pipe WIDTH (/ HEIGHT 2)))

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
(define LOP2 (cons P2 (cons P3 empty)))

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

(define START (make-gs (make-flappy (/ HEIGHT 2) 0 0) (list P3) 0 true))
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
              (place-image
               (text-flappy (number->string (gs-points START)))
               POINTS-X
               POINTS-Y
               (render-game START)))

(check-expect (render GS1)
              (place-image
               (text-flappy (number->string (floor (/ (gs-points GS1) 25))))
               POINTS-X
               POINTS-Y
               (render-game GS1)))

(check-expect (render GS-END)
              (place-image
               (above
                (text-flappy (number->string (floor (/ (gs-points GS-END) 25))))
                (get-sprite "gameover"))
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
      (place-image (text-flappy (number->string (floor (/ (gs-points gs) 25))))
                   POINTS-X
                   POINTS-Y
                   (render-game gs))
      (cond [(>= (floor (/ (gs-points gs) 25)) GOLD-POINTS)
             (place-image
              (above (text-flappy (number->string
                                   (floor (/ (gs-points gs) 25))))
                     (get-sprite "gameover")
                     (text "GOLD" (* 2 FONT-SIZE) TEXT-COLOR))
              POINTS-X
              POINTS-Y
              (render-game gs))]
            [(>= (floor (/ (gs-points gs) 25)) SILVER-POINTS)
             (place-image
              (above (text-flappy (number->string
                                   (floor (/ (gs-points gs) 25))))
                     (get-sprite "gameover")
                     (text "SILVERL" (* 2 FONT-SIZE) TEXT-COLOR))
              POINTS-X
              POINTS-Y
              (render-game gs))]
            [(>= (floor (/ (gs-points gs) 25)) BRONZE-POINTS)
             (place-image
              (above (text-flappy (number->string
                                   (floor (/ (gs-points gs) 25))))
                     (get-sprite "gameover")
                     (text "BRONZE" (* 2 FONT-SIZE) TEXT-COLOR))
              POINTS-X
              POINTS-Y
              (render-game gs))]
            [else (place-image
                   (above (text-flappy
                           (number->string (floor (/ (gs-points gs) 25))))
                          (get-sprite "gameover"))
                   POINTS-X
                   POINTS-Y
                   (render-game gs))])))

(@htdf render-game)
(@signature GameState -> Image)
;; render game (bird and pipe) on MTS with pipes
(check-expect (render-game START)
              (place-image (rotate (- (flappy-r (gs-flappy START))) FLAPPY-IMG)
                           FLAPPY-X-POS
                           (flappy-y (gs-flappy START))
                           (render-pipes (gs-lop START))))

(check-expect (render-game GS1)
              (place-image (rotate (- (flappy-r (gs-flappy GS1))) FLAPPY-IMG)
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
  (place-image (rotate (- (flappy-r (gs-flappy gs))) FLAPPY-IMG)
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
              (above (rectangle PIPE-WIDTH (pipe-y P1)
                                "solid" PIPE-COLOR)
                     (rectangle PIPE-WIDTH PIPE-V-GAP
                                "outline" (make-color 0 0 0 0))
                     (rectangle PIPE-WIDTH (- HEIGHT PIPE-V-GAP (pipe-y P1))
                                "solid" PIPE-COLOR)))
(check-expect (generate-pipe-img P2)
              (above (rectangle PIPE-WIDTH (pipe-y P2)
                                "solid" PIPE-COLOR)
                     (rectangle PIPE-WIDTH PIPE-V-GAP
                                "outline" (make-color 0 0 0 0))
                     (rectangle PIPE-WIDTH (- HEIGHT PIPE-V-GAP (pipe-y P2))
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
  (above (rectangle PIPE-WIDTH (pipe-y p)
                    "solid" PIPE-COLOR)
         (rectangle PIPE-WIDTH PIPE-V-GAP "outline" (make-color 0 0 0 0))
         (rectangle PIPE-WIDTH (- HEIGHT PIPE-V-GAP (pipe-y p))
                    "solid" PIPE-COLOR)))


(@htdf tock)
(@signature GameState -> GameState)
;; produce the next GameState

(check-expect (tock START)
              (make-gs (tock-flappy (gs-flappy START))
                       (filter-pipe (create-new-pipe
                                     (tock-pipes (gs-lop START))))
                       (gs-points START)
                       true))

(check-expect (tock GS-END)
              (make-gs (make-flappy 204.8 -2 -45)
                       (cons (make-pipe 144 512/3)
                             (cons (make-pipe 288 256) empty))
                       20
                       false))

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

        [(touch-pipes? (gs-flappy gs) (gs-lop gs))
         (make-gs (tock-flappy (gs-flappy gs))
                  (gs-lop gs)
                  (gs-points gs)
                  false)]
        [(touch-ground? (gs-flappy gs))
         (make-gs (tock-flappy (gs-flappy gs))
                  (gs-lop gs)
                  (gs-points gs)
                  false)]
        [(past-pipes? (gs-flappy gs) (gs-lop gs))
         (make-gs (tock-flappy (gs-flappy gs))
                  (filter-pipe (create-new-pipe (tock-pipes (gs-lop gs))))
                  (add1 (gs-points gs))
                  true)]
        [else
         (make-gs (tock-flappy (gs-flappy gs))
                  (filter-pipe (create-new-pipe (tock-pipes (gs-lop gs))))
                  (gs-points gs)
                  true)]))


(@htdf tock-flappy)
(@signature Flappy -> Flappy)
;; produce the next Flappy

(check-expect (tock-flappy F1)
              (make-flappy 261 5.9 25))
(check-expect (tock-flappy F2)
              (make-flappy 202.8 -1.1 -10))

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
               (cond
                 [(> (* (flappy-dy f) (/ MIN-ANGLE MAX-Y-SPEED)) 180)
                  ;angle of 180 degrees is equivalent to rotate 90
                  MIN-ANGLE]
                 [(< (* (flappy-dy f) (/ (- MAX-ANGLE) MAX-Y-SPEED)) -10)
                  ;-10 is resistance to change, idk if this should be a constant
                  MAX-ANGLE]
                 [else (* (flappy-dy f) ROTATE-SPEED)])))
;when flappy falls downwards, his speed is positive, not negative.


(@htdf tock-pipes)
(@signature ListOfPipe -> ListOfPipe)
;; produce the next ListOfPipes

(check-expect (tock-pipes LOP1) LOP1)
(check-expect (tock-pipes LOP2)
              (list (next-pipe P2) (next-pipe P3)))

;(define (tock-pipes lop) lop)

(@template-origin ListOfPipe)
(@template
 (define (tock-pipes lop)
   (cond [(empty? lop) (...)]
         [else
          (... (next-pipe (first lop))
               (tock-pipes (rest lop)))])))

(define (tock-pipes lop)
  (cond [(empty? lop) empty]
        [else
         (cons (next-pipe (first lop))
               (tock-pipes (rest lop)))]))

(@htdf next-pipe)
(@signature Pipe -> Pipe)
;; produce the next pipe state (move pipe left by pipe-speed)

(check-expect (next-pipe (make-pipe 10 50)) (make-pipe (- 10 PIPE-SPEED) 50))
(check-expect (next-pipe (make-pipe 60 20)) (make-pipe (- 60 PIPE-SPEED) 20))

(define (next-pipe p)
  (make-pipe (- (pipe-x p) PIPE-SPEED) (pipe-y p)))

(@htdf create-new-pipe)
(@signature ListOfPipe -> ListOfPipe)
;; add new pipe if last pipe is far enough

(check-random (create-new-pipe empty)
              (list (make-pipe WIDTH
                               (abs (- (* 0.7 (random HEIGHT))
                                       (* 0.1 HEIGHT))))))

(check-random (create-new-pipe LOP2)
              LOP2)

(@template-origin ListOfPipe)

(define (create-new-pipe lop)
  (cond [(empty? lop)
         (list (make-pipe WIDTH (abs (- (* 0.7 (random HEIGHT))
                                        (* 0.1 HEIGHT)))))]
        [else
         (if (< (+ (pipe-x (first (reverse lop)))
                   PIPE-H-GAP) WIDTH)
             (append lop
                     (list (make-pipe WIDTH (abs (- (* 0.7 (random HEIGHT))
                                                    (* 0.1 HEIGHT))))))
             lop)
         ]))

(@htdf filter-pipe)
(@signature ListOfPipe -> ListOfPipe)
;; check if pipe is out of bound

(check-expect (filter-pipe empty) empty)
(check-expect (filter-pipe LOP2) LOP2)

(@template-origin Pipe)

(define (filter-pipe lop)
  (cond [(empty? lop) empty]
        [else
         (if (< (pipe-x (first lop)) (- PIPE-WIDTH))
             (filter-pipe (rest lop))
             (cons (first lop) (filter-pipe (rest lop))))]))


(@htdf touch-pipes?)
(@signature Flappy ListOfPipe -> Boolean)
;; end the game if Flappy is overlapping any pipe in lop

(check-expect (touch-pipes? F1 empty) false)
(check-expect (touch-pipes? F2 LOP2) false)

;(define (touch-pipes? f lop) false)

(@template-origin ListOfPipe
                  Flappy)
(@template
 (define (touch-pipes? f lop)
   (cond [(empty? lop) (...)]
         [else
          (... (fn-for-pipe (first lop))
               (touch-pipes? (rest lop))
               (fn-for-flappy f))])))

(define (touch-pipes? f lop)
  (cond [(empty? lop) false]
        [else
         (if (pipe-in-range? (first lop))
             (touch-pipe? f (first lop))
             (touch-pipes? f (rest lop)))]))

(@htdf pipe-in-range?)
(@signature Pipe -> Boolean)
;; check if pipe is in range of flappy (x-values)

(check-expect (pipe-in-range? (make-pipe (+ FLAPPY-X-POS FLAPPY-HFLEN)
                                         (/ HEIGHT 2)))
              true)
(check-expect (pipe-in-range? (make-pipe (+ FLAPPY-X-POS FLAPPY-HFLEN 1)
                                         (/ HEIGHT 2)))
              false)
(check-expect (pipe-in-range? (make-pipe (+ FLAPPY-X-POS FLAPPY-HFLEN -1)
                                         (/ HEIGHT 2)))
              true)

(check-expect (pipe-in-range?
               (make-pipe (- FLAPPY-X-POS PIPE-WIDTH (* 1 FLAPPY-HFLEN))
                          (/ HEIGHT 2)))
              true)
(check-expect (pipe-in-range?
               (make-pipe (- FLAPPY-X-POS PIPE-WIDTH (* 1 FLAPPY-HFLEN) 1)
                          (/ HEIGHT 2)))
              false)
(check-expect (pipe-in-range?
               (make-pipe (- FLAPPY-X-POS PIPE-WIDTH (* 1 FLAPPY-HFLEN) -1)
                          (/ HEIGHT 2)))
              true)
;(define (pipe-in-range? f p) true)

(@template-origin Pipe)
(@template
 (define (pipe-in-range? p)
   (... p)))

(define (pipe-in-range? p)
  (or (and (>= (+ FLAPPY-X-POS FLAPPY-HFLEN) (pipe-x p))
           (<= (+ FLAPPY-X-POS FLAPPY-HFLEN) (+ (pipe-x p) PIPE-WIDTH)))
      (and (>= (- FLAPPY-X-POS FLAPPY-HFLEN) (pipe-x p))
           (<= (- FLAPPY-X-POS FLAPPY-HFLEN) (+ (pipe-x p) PIPE-WIDTH)))))

(@htdf past-pipes?)
(@signature Flappy ListOfPipe -> Boolean)
;;produces true if flappy flies past a pipe

(check-expect (past-pipes? F1 LOP1) false)
(check-expect (past-pipes? F2 LOP2) false)

(@template-origin ListOfPipe
                  Flappy)
(@template
 (define (past-pipes? f lop)
   (cond [(empty? lop) (...)]
         [else
          (... (fn-for-pipe (first lop))
               (touch-pipes? (rest lop))
               (fn-for-flappy f))])))

(define (past-pipes? f lop)
  (cond [(empty? lop) false]
        [else
         (if (pipe-in-range? (first lop))
             (past-pipe? f (first lop))
             (past-pipes? f (rest lop)))]))

(@htdf past-pipe?)
(@signature Flappy Pipe -> Boolean)
;;produces true if flappy flies past a pipe in range

(@template-origin Flappy Pipe)
(@template
 (define (past-pipe? f p)
   (...(flappy-y f)
       (flappy-dy f)
       (flappy-r f))))


(define (past-pipe? f p)
  (or (and (>= (+ FLAPPY-X-POS FLAPPY-HFLEN) (pipe-x p))
           (<= (+ FLAPPY-X-POS FLAPPY-HFLEN) (+ (pipe-x p) PIPE-WIDTH)))
      (and (>= (- FLAPPY-X-POS FLAPPY-HFLEN) (pipe-x p))
           (<= (- FLAPPY-X-POS FLAPPY-HFLEN) (+ (pipe-x p) PIPE-WIDTH)))))


(@htdf touch-pipe?)
(@signature Flappy Pipe -> Boolean)
;; check if flappy is in contact with a single pipe (y-values)

(check-expect (touch-pipe? (make-flappy 60 0 0)
                           (make-pipe (+ FLAPPY-X-POS 1)
                                      (+ 60 FLAPPY-HFLEN)))
              true)
(check-expect (touch-pipe? (make-flappy 60 0 0)
                           (make-pipe (+ FLAPPY-X-POS 1)
                                      (+ 60 FLAPPY-HFLEN 1)))
              true)
(check-expect (touch-pipe? (make-flappy 60 0 0)
                           (make-pipe (+ FLAPPY-X-POS 1)
                                      (+ 60 FLAPPY-HFLEN -1)))
              true)

(check-expect (touch-pipe? (make-flappy (- 60 PIPE-V-GAP (- FLAPPY-HFLEN)) 0 0)
                           (make-pipe (+ FLAPPY-X-POS 1)
                                      60))
              true)

(check-expect (touch-pipe? (make-flappy (- 60 PIPE-V-GAP(- FLAPPY-HFLEN) -1)0 0)
                           (make-pipe (+ FLAPPY-X-POS 1)
                                      60))
              true)

(check-expect (touch-pipe? (make-flappy (- 60 PIPE-V-GAP (- FLAPPY-HFLEN) 1)0 0)
                           (make-pipe (+ FLAPPY-X-POS 1)
                                      60))
              true)
;(define (pipe-in-range? f p) true)

(@template-origin Flappy Pipe)
(@template
 (define (touch-pipe? f p)
   (... f p)))

(define (touch-pipe? f p)
  (or (<= (- (flappy-y f) FLAPPY-HFLEN) (pipe-y p))
      (>= (+ (flappy-y f) FLAPPY-HFLEN) (+ (pipe-y p) PIPE-V-GAP))))

(@htdf touch-ground?)
(@signature Flappy -> Flappy)
;make the gamestate false when flappy touches the ground
(check-expect (touch-ground? (make-flappy 204.8 -2 -45)) false)
(check-expect (touch-ground? (make-flappy HEIGHT 3 45)) true)
(check-expect (touch-ground? (make-flappy (+ HEIGHT 1) 2 34)) true)
(check-expect (touch-ground? (make-flappy (- HEIGHT 1) 2 34)) false)

(define (touch-ground? f)
  (>= (flappy-y f) HEIGHT))


(@htdf handle-mouse)
(@signature GameState Integer Integer MouseEvent -> GameState)
;; accelerate Flappy upwards on "button-down"

(check-expect (handle-mouse START 50 10 "button-up") START)
(check-expect (handle-mouse START 50 10 "button-down")
              (start-flap START))

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
;; accelerate Flappy upwards on " " and "up"; restart if false

(check-expect (handle-key START " ") (start-flap START))
(check-expect (handle-key START "up") (start-flap START))
(check-expect (handle-key START "b") START)
(check-expect (handle-key GS-END " ") START)

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
         (if (false? (gs-state gs))
             START
             (start-flap gs))]

        [(key=? "up" ke)
         (start-flap gs)]
        [else gs]))


(@htdf start-flap)
(@signature GameState -> GameState)
;; update GameState so that Flappy flaps

(check-expect (start-flap START)
              (make-gs (flap (gs-flappy START)) (gs-lop START)
                       (gs-points START) (gs-state START)))

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

(check-expect (flap F1)
              (make-flappy (flappy-y F1) (- MAX-Y-SPEED) (flappy-r F1)))
(check-expect (flap F2)
              (make-flappy (flappy-y F2) (- MAX-Y-SPEED) (flappy-r F2)))

;(define (flap f) f)

(@template-origin Flappy)

(@template
 (define (flap f)
   (... (flappy-y f)
        (flappy-dy f)
        (flappy-r f))))

(define (flap f)
  (make-flappy (flappy-y f) (- MAX-Y-SPEED) (flappy-r f)))

(@htdf text-flappy)
(@signature String -> Image)
; produces an image using the flappy-font definition
;; CONSTRAINT: string must only consist of numbers
(check-expect (text-flappy "0") (beside (list-ref flappy-font 0) empty-image))
(check-expect (text-flappy "1") (beside (list-ref flappy-font 1) empty-image))
(check-expect (text-flappy "236") (beside (list-ref flappy-font 2)
                                          (beside (list-ref flappy-font 3)
                                                  (list-ref flappy-font 6))))
;(define (text-flappy s) (array-ref flappy-font 0))


(@template-origin String)

(define (text-flappy s)
  (cond [(string=? s "") empty-image]
        [else
         (beside (list-ref flappy-font (- (char->integer (string-ref s 0)) 48))
                 (text-flappy (substring s 1)))]))

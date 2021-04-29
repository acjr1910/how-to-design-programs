;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

;; Game
(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom       (game-missiles s))
       (fn-for-tank      (game-tank     s))))


;; Tank
(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))


;; Invader
(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right
(define I4 (make-invader 150 100 -12))          ;not landed, moving left

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


;; Invaders
(define-struct invaders (x y dx))
;; Invaders is one of:
;;  - empty
;;  - (cons (make-invader Natural Natural Natural) Invaders)
;; interp. a list of invaders, where each 
;;           x   is the invader position x in pixels 
;;           y   is the invader position y in pixels
;;           dx  is the invader along x by dx pixels per clock tick

(define LOI1 empty)
(define LOI2 (list I1 empty))
(define LOI3 (list I1 I4 empty))
(define LOI4 (list I1 I2 I3 empty))

#;
(define (fn-for-invaders invaders)
  (cond [(empty? invaders) (...)]
        [else
         (... (invaders-x  (first invaders)) ;Natural
              (invaders-y  (first invaders)) ;Natural
              (invaders-dx (first invaders)) ;Natural
              (fn-for-invaders (rest invaders)))]))

;; Missile
(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; Missiles
(define-struct missiles (x y))
;; Missiles is one of:
;;  - empty
;;  - (cons (make-missile Natural Natural) Missiles)
;; interp. a list of missiles, where each 
;;           x is the missile position x in pixels 
;;           y is the missile position y in pixels

;; !!! add missiles definition examples

#;
(define (fn-for-missiles missiles)
  (cond [(empty? missiles) (...)]
        [else
         (... (missiles-x  (first missiles)) ;Natural
              (missiles-y  (first missiles)) ;Natural
              (fn-for-missiles (rest missiles)))]))

;; Functions:

;; Game -> Game
;; Called to start the space invaders game; start with (main (make-game LOI LOM 0))
;; no tests for main function
(define (main game)
  (big-bang game
    (on-tick advance-game) ; Game          -> Game
    (to-draw render-game)  ; Game          -> Image
    (on-key  handle-key))) ; Game KeyEvent -> Game

;; Game -> Game
;; !!!
;(define (advance-game s) 0) ; stub

(define (advance-game s)
  (if (game-over? (game-invaders s))
      s
      (make-game
       (advance-invaders (game-invaders s))
       (advance-missiles (game-missiles s))
       (advance-tank     (game-tank s)))))

;; Game -> Image
;; !!!
(define (render-game g) BACKGROUND) ; stub

;; Game Key -> Game
(define (handle-key g key) 0) ; stub

;; Invaders -> Boolean
;; !!! 
(define (game-over? loi) false) ; stub

;; Invaders -> Invaders
;; !!!
(define (advance-invaders loi) 0) ; stub

;; Missiles -> Missiles
;; !!!
(define (advance-missiles lom) 0) ; stub

;; Tank -> Tank
;; Produce a Tank with incremented or decremented position x based on dir Interval[-1,1]
;; Assume: Tank dir remains the same.
(check-expect (advance-tank (make-tank 0  0))  (make-tank 0  0))
(check-expect (advance-tank (make-tank 1  1))  (make-tank 2  1))
(check-expect (advance-tank (make-tank 2 -1))  (make-tank 1 -1))

; (define (advance-tank t) (make-tank 0 0)) ; stub

;; took template from Tank

(define (advance-tank t)
  (cond [(= (tank-dir t)  1) (make-tank (+ (tank-x t) 1) (tank-dir t))]
        [(= (tank-dir t) -1) (make-tank (- (tank-x t) 1) (tank-dir t))]
        [else (make-tank 0 0)]))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


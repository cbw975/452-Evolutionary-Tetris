(ns tetris-clojure-452.core
  (:import
    (java.awt.event ActionListener KeyListener KeyEvent)))

;;; Resources:
;;; - using swing with Clojure

;; For storing game state + string based display to repl
(def empty-block 0)
(def active-block 1) ; active cells recognized as 1s
(def placed-block 2) ; filled cells recognized as 2s
(def world-height 20) ;;world refers to the area tetris blocks can be in
(def world-width 10)

(def I [[0 1 0 0]
        [0 1 0 0]
        [0 1 0 0]
        [0 1 0 0]])
(def O [[1 1]
        [1 1]])
(def L [[0 1 0]
        [0 1 0]
        [0 1 1]])
(def R [[0 1 1]
        [0 1 0]
        [0 1 0]])
(def T [[0 0 0]
        [1 1 1]
        [0 1 0]])
(def Z [[0 0 0]
        [1 1 0]
        [0 1 1]])
(def S [[0 0 0]
        [0 1 1]
        [1 1 0]])

(def shapes [I O L R T Z S])

(defn start-state []
  {:score 0 ; score of game. starts at 0
   ;:speed 100 ; implement later
   :high-score 0
   :filled #{} ; set of coordinates of filled cells in world
   :active-pos [(rand-int world-width) 0] ; top-left coord of active tetris block. randomly somewhere along the top row of world
   :active-shape ((rand-nth (keys shapes)) shapes)}) ; shape is the current falling/active block

;;make a row (vector) of specified blocks
(defn make-row
  ([] (make-row world-width empty-block))
  ([num-rows block] (vec (repeat num-rows block))))

(defn empty-world []
  (make-row world-height (make-row))) ;empty rows up to world's height

(defn transpose [matrix]
  (into [] ;make transposed shape structure a vector of rows
        (for [row (apply map list matrix)] ;transposes matrix, but structure ends up as list of lists.
          (into [] row)))) ;make each row a vector

;; swap the items at i1 and i2 in a seq (i.e. two rows the world, where items are rows and seq is world of rows)
(defn swap [items i1 i2]
  (assoc items i1 (items i2) i2 (items i1))) ; (assoc seq i1 item1 i2 item2) replaces item1 with item2 at index i2 and vice versa

;; rotates a tetris block (clockwise)
(defn rotate [shape] ;; TODO: Rotate only if doesn't go out of bounds or collide with other blocks
  (swap (transpose shape) 0 (dec (count shape))))
;; Thought process Note: when a shape rotates, each cell "moves" around the center-point (area-length - 1) over. (for i-block, think of middle 1's as own mini-block)
;; => Basically if block matrix is a square, can take the transpose of matrix and swap first and last rows

(defn cell-in [shape x y]
  (get-in shape [x y]))

; deconstruct active shape and return coordinates of its 4 filled cells (1's)
(defn shape-cells [{:keys [active-pos active-shape]}]
  (let [d (count active-shape)]
    (for [x (range d) y (range d)
          :when (= 1 (cell-in active-shape x y))]
      (mapv + [i j] active-pos))))

; check if active-block intersects with filled/out-of-bounds blocks
(defn valid? [{:keys [filled] :as state}]
  ; for every coordinate in the current active shape (shape-cells), check if within world boundaries and not in the filled cells
  (every? (fn [[x y :as coord]]
               (and ((complement filled) coord)
                    (<= 0 x (- world-width 1)) (< y (- world-height 1))))
          (shape-cells state)))

(defn place-shape [state])

; TODO: keyboard events (arrow keys + shift to rotate)
; TODO: Check for game over
; TODO: Falling blocks generating, falling, etc.
; TODO: Placing blocks
; TODO: Tetris rules, etc.

(defn get-key-input [key world ])

(defn gameover? [world]
  )

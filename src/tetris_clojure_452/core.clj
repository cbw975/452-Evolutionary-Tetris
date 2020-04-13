(ns tetris-clojure-452.core
  (:import
    (java.awt.event ActionListener KeyListener KeyEvent)))

;;; Resources:
;;; - using swing with Clojure

;  For storing game state + string based display to repl
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
   :speed 50 ; lower speed => shapes fall faster. Here, 50 is no rush
   :frame 0
   :high-score 0
   :filled #{} ; set of coordinates of filled cells in world
   :active-pos [(rand-int (- world-width 3)) 0] ; top-left coord of active tetris block. randomly somewhere along the top row of world
   :active-shape ((rand-nth (keys shapes)) shapes)}) ; shape is the current falling/active block

; make a row (vector) of specified blocks
(defn make-row
  ([] (make-row world-width empty-block))
  ([num-rows block] (vec (repeat num-rows block))))

(defn empty-world []
  (make-row world-height (make-row))) ;empty rows up to world's height

(defn transpose [matrix]
  (into [] ;make transposed shape structure a vector of rows
        (for [row (apply map list matrix)] ;transposes matrix, but structure ends up as list of lists.
          (into [] row)))) ;make each row a vector

; swap the items at i1 and i2 in a seq (i.e. two rows the world, where items are rows and seq is world of rows)
(defn swap [items i1 i2]
  (assoc items i1 (items i2) i2 (items i1))) ; (assoc seq i1 item1 i2 item2) replaces item1 with item2 at index i2 and vice versa

; check if active-block intersects with filled/out-of-bounds blocks
(defn valid? [{:keys [filled] :as state}]
  ; for every coordinate in the current active shape (shape-cells), check if within world boundaries and not in the filled cells
  (every? (fn [[x y :as coord]]
            (and ((complement filled) coord)
                 (<= 0 x (- world-width 1)) (< y (- world-height 1))))
          (shape-coords state)))

; rotates a tetris block (clockwise)
(defn rotate [state shape]
  (let [rotated (update state :active-shape
                        (swap (transpose shape) 0 (dec (count shape))))] ;transpose and switch first and last rows
    (if (valid? rotated) rotated state)))
;; Thought process Note: when a shape rotates, each cell "moves" around the center-point (area-length - 1) over. (for i-block, think of middle 1's as own mini-block)
;; => Basically if block matrix is a square, can take the transpose of matrix and swap first and last rows


;;;;; for below: USED RESOURCE: http://fn-code.blogspot.com/2016/04/another-tetris-clone-in-clojure.html

; translates a tetris block (f: inc for right, dec for left)
(defn shift [state direction]
  (let [shifted (update-in state [:active-pos 0] direction)] ; next (or shifted) state has shifted active-pos
    (if (valid? shifted) shifted state)))

(defn cell-in [shape x y]
  (get-in shape [x y]))

; deconstruct active shape and return coordinates of its 4 filled cells (1's)
(defn shape-coords [{:keys [active-pos active-shape]}]
  (let [d (count active-shape)]                             ; the number of cells a shape has
    (for [x (range d) y (range d)                           ; for each coord in the shape
          :when (= 1 (cell-in active-shape x y))]           ; if it is an active non-empty block
      (mapv + [i j] active-pos))))

; when an entire row is filled, the row clears and add 10 to the score
(defn clear-row [{:keys [filled] :as state} row]
  (if (every? filled (for [i (range world-width)] [i row])) ; if every cell is filled in a row, clear the row
    (-> state
        (score 10)                                          ; 10 points for every cleared row
        (update :speed dec)                                ; speed of blocks
        (assoc :filled                                      ; the blocks in the row are no longer filled
               (set (for [[i j] filled :when (not= j row)]  ; transfer/keep all the filled cells that were not in the cleared-row
                      (if (< j row) [i (inc j)] [i j])))))  ; shift down all the rows above cleared-row
    state))                                                 ; return the updated state

; shift cell down
(defn fall [state]
  (let [shifted (update-in state [:active-pos 1] inc)]
    (if (valid? shifted)
      shifted
      (let [filled-coord (shape-coords state)]
        (-> state
            (update :filled into filled-coord)))            ; place the unshifted cells into the world
      (score 1)                                             ; add 1 to score for every placed block
      (#(reduce clear-row % (map second filled-coord)))     ; clear any rows that got filled by placing the shape
      (into { :active-shape ((rand-nth (keys shapes)) shapes) ; generate new random active shape
             :active-pos [(rand-int (- world-width 3)) 0]})))) ; have active shape at random spot at top of world

; place figure all the way down.
(defn drop [{:keys [filled] :as state}]
  (some #(when (not= filled (:filled %)) %)
        (iterate fall state)))

; Updates frame at each time step; called by user interface layer for each game time step.
(defn step-forward [{:keys [frame filled speed] :as state}]
  (cond-> (update state :frame inc)                         ; conditional threading at the new game step
          (zero? (mod frame (max speed 1))) fall            ; if the frame modded with the current speed is zero, the shape falls.
          (some zero? (map second filled))
          ; (into (dissoc (initial-state) :score)
          ))                                          ; if y-coord of any filled cells is at top (0), gameover

; TODO: keyboard events (arrow keys + shift to rotate)

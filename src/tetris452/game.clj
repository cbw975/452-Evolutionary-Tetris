(ns tetris452.game)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   TETRIS SHAPES + CELL/BLOCK REPRESENTATIONS   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; For storing game state + string based display to repl
(def empty-block 0)                                         ; Empty/Unoccupied cells = zeros
(def active-block 1)                                        ; Active/Moving cells = 1s
(def placed-block 2)                                        ; Filled/Occupied cells = 2s
(def world-height 20)                                       ; row 0 is at top
(def world-width 10)                                        ; world refers to the area tetris blocks can be in

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   REPL/CMD-LINE VISUALIZATION + REPORTING   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn str-board
  "Returns 'board' as readable formatted string"
  [board]
  (apply str (mapcat #(conj % \newline ) board)))

; TODO: Visualization - Quil visualization

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   BOARD-RELATED CONVERSIONS   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-cell
  "Accesses the block type/value within 2D vectors (board)"
  [x y matrix]
  (get-in matrix [y x]))

; deconstruct active shape and return coordinates of its 4 active cells (1's)
; TODO NOTE: this might not work if I keep the active-shape not imbedded in the board until it it placed... depends on visualization
(defn shape-coords
  "Returns the relative coordinates of active-blocks/cells (i.e. 1's) in 'matrix'"
  [matrix]
  (into [] (for [x (range world-width)
                 y (range world-height)                     ; for each coord on the board...
                 :when (= active-block (read-cell x y matrix))] ; if it is an active block...
             [x y])))

(defn abs-shape-coords
  "Returns the absolute coordinates (on 'board') of the active cells,
  given that the active-'shape' starts at '(ref-x,ref-y)' or :active-pos"
  [[ref-x ref-y] shape]
  (let [rows (count shape)
        cols (count (first shape))]
    (for [x (range cols) y (range rows)
          :when (= active-block (read-cell x y shape))]
      [(+ x ref-x) (+ ref-y y)]))
  )

#_(def m [[1 2 3] [4 5 1] [7 8 9] [10 1 12]])
#_(shape-coords m) ; => [[0 0] [1 3] [2 1]]

; ##Board handling:
; Example board:
; 1  2  3
; 4  5  6
; 7  8  9
; 10 11 12
#_(def m [[1 2 3] [4 5 6] [7 8 9] [10 11 12]])
; How board is stored:
; "x" is column #, increasing from left (0) to right
; "y" is row #, increasing from top (0) to bottom
; (0,0) (1,0) (2,0)
; (0,1) (1,1) (2,1)
; (0,2) (1,2) (2,2)
; (0,3) (1,3) (2,3)
#_(read-cell 1 2 m) ; => 8
#_(print (str-board m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   BOARD MAKING + MANIPULATION   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-row
  "Return 'num-rows' of empty rows of specified 'block's"
  ([] (make-row world-width empty-block))
  ([num-rows block] (vec (repeat num-rows block))))

; Board = 2D vector of cells/blocks (types denoted by numbers)
(defn make-board []
  (make-row world-height (make-row)))                       ; empty rows up to world's height

(defn start-state
  "Makes a new-game state"
  []
  {:board        (make-board)
   :score        0                                          ; score of game. starts at 0
   ;:speed       100                                       ; how quickly before block will fall down 1
   :cleared-rows 0                                          ; number of rows that have been cleared in game
   :active-pos   [(rand-int (- world-width 3)) 0]           ; top-left coord of active tetris shape. randomly somewhere along the top row of world
   :active-shape ((rand-nth (keys shapes)) shapes)})        ; shape is the current falling/active block
;TODO: Set the active shape to be:
;     a) 2 separate state-properties: fall-seq and active-ind, where active-ind is the index of the shape we are using from the seq
;     b) call to a .... however did in project

(defn cell-valid?
  "checks for collisions - Returns bool for if the cell at (r,c) is un-filled and in the 'board' bounds"
  [x y board]
  (and board (< -1 x world-width) (< -1 y world-height)     ; board exists (non-nil) and the cell is within the world-width and world-height
       (not (pos? (read-cell x y board)))))                 ; the cell is non-negative

(defn place-block
  "Returns updated 'board' with the 'block-type' placed at cell '(x,y)', or nil if invalid board"
  [board [x y] block-type]
  (when (cell-valid? x y board)
    (assoc-in board [y x] block-type)))

(defn place-blocks
  "Returns updated 'board' with blocks of type 'block-type' at cells in the 'coords' list, or 'nil' if invalid board"
  [board coords block-type]
  (if (count coords)                                        ; if there are (still) blocks to be placed, place them
    (let [curr-coord (first coords)
          rest-coords (rest coords)
          ;curr-coord (map + curr-coord [x y])
          updated-board (place-block board curr-coord block-type)] ; 'nil' if invalid board
      (recur updated-board rest-coords block-type))
    ; else:
    board                                                   ; return the updated board with all the blocks placed. If invalid, will be nil
    ))
; NOTE: When in game loop, place the active-block when applicable (touches bottom or a filled-block),
;       but not in the actual state :board, (so use let to keep it local)... b/c have active pos... so don't need it to be constantly in board

(defn place-shape
  "Returns updated 'board' with 'shape' placed at/relative to the 'active-pos' (coords: [x y]), or nil if invalid board"
  [shape active-pos block-type board]
  ;(let [coords (shape-coords shape)]
  (let [coords-to-place (abs-shape-coords active-pos shape)]
    (place-blocks board coords-to-place block-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   GAME RULE RELATED COMPUTATIONS/UPDATES   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn calc-level
  "Calculates level from total number of `lines` cleared. Max level of 10
  for 90+ lines."
  [lines]
  (cond
    (> lines 90) 10
    (pos? lines) (inc (quot (dec lines) 10))
    :default 0))

(defn score-lines
  "Returns score computed based on number of 'lines' cleared at once and
  current `level`. Better score when more lines cleared at once."
  [lines level]
  (let [base-score {1 30 2 100 3 300 4 1200}]
    (* (base-score lines) (inc level))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   BOARD TRANSFORMATIONS + MOVES, CHECKS   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rotate
  "Applies rotation (cw or ccw) to given 'matrix' (active-block) if valid (no collisions)
  when positioned relative to absolution position denoted by '(x,y)'"
  [matrix [x y] board]
  ; TODO
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   PREVIOUS CODE TO BE MODIFIED/REPLACED   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn transpose [matrix]
  (into []                                                  ;make transposed shape structure a vector of rows
        (for [row (apply map list matrix)]                  ;transposes matrix, but structure ends up as list of lists.
          (into [] row))))                                  ;make each row a vector

; swap the items at i1 and i2 in a seq (i.e. two rows the world, where items are rows and seq is world of rows)
(defn swap [items i1 i2]
  (assoc items i1 (items i2) i2 (items i1)))                ; (assoc seq i1 item1 i2 item2) replaces item1 with item2 at index i2 and vice versa

; rotates a tetris block (clockwise)
(defn rotate [state shape]
  (let [rotated (update state :active-shape
                        (swap (transpose shape) 0 (dec (count shape))))] ;transpose and switch first and last rows
    (if (valid? rotated) rotated state)))
;; Thought process Note: when a shape rotates, each cell "moves" around the center-point (area-length - 1) over. (for i-block, think of middle 1's as own mini-block)
;; => Basically if block matrix is a square, can take the transpose of matrix and swap first and last rows


; translates a tetris block (f: inc for right, dec for left)
(defn shift [state direction]
  (let [shifted (update-in state [:active-pos 0] direction)] ; next (or shifted) state has shifted active-pos
    (if (valid? shifted) shifted state)))

; shift cell down
(defn down [state]
  (let [shifted (update-in state [:active-pos 1] inc)]      ; y-coord of active block shifted down 1
    (if (valid? shifted)
      shifted
      (let [filled-coord (shape-coords state)]
        (-> state
            (update :filled into filled-coord)              ; place the fallen cells into the world
            (update :score + 1)                             ; add 1 to score for every placed block
            (#(reduce clear-row % (map second filled-coord))) ; clear any rows that got filled by placing the shape
            (into {:active-shape ((rand-nth (keys shapes)) shapes) ; generate new random active shape
                   :active-pos   [(rand-int (- world-width 3)) 0]}) ; have active shape at random spot at top of world
            )))))

; place figure all the way down.
(defn drop-shape [{:keys [filled] :as state}]
  (some #(when (not= filled (:filled %)) %)
        (iterate down state)))

(defn game-over [{:keys [score] :as state}]
  ; TODO: use this function to return whatever will get sent to evolution code ??
  score
  )


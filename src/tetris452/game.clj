(ns tetris452.game)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   TETRIS INITIALIZATION + CELL/BLOCK REPRESENTATIONS   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; For storing game state + string based display to repl
(def empty-block 0)                                         ; Empty/Unoccupied cells = zeros
(def active-block 1)                                        ; Active/Moving cells = 1s
(def placed-block 2)                                        ; Filled/Occupied cells = 2s
(def world-height 20)                                       ; row 0 is at top
(def world-width 10)                                        ; world refers to the area tetris blocks can be in
(def shape-spawn-pos [(- (/ world-width 2) 2) 0])

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

(defn make-row
  "Returns a vector with 'row-width' of specified 'block's"
  ([] (make-row world-width empty-block))
  ([row-width block] (vec (repeat row-width block))))

(defn next-shape
  "Returns the shape corresponding to the 'ind'-th shape-type (as an int, 0-6) from the 'seed'/falling shapes seq"
  [ind seed]
  (let [shape-int (nth seed ind)                            ; the int for the shape, from the seed at the ind-th index
        shape (nth shapes shape-int)]                       ; corresponding shape matrix
    shape))

(defn make-board
  "Returns a 2D vector 'board' of cells/blocks"
  ([] (make-row world-height (make-row)))                   ; empty rows up to world's height
  ([num-rows] (make-row num-rows (make-row))))

(def get-seed (repeatedly #(rand-int 7)))

(defn start-state
  "Makes a new-game state"
  []
  {:board        (make-board)
   :score        0                                          ; score of game. starts at 0
   ;:speed       100                                       ; how quickly before block will fall down 1
   :cleared-rows 0                                          ; number of rows that have been cleared in game
   :active-pos   [(rand-int (- world-width 3)) 0]           ; top-left coord of active tetris shape. randomly somewhere along the top row of world
   :active-shape ((rand-nth (keys shapes)) shapes)})        ; shape is the current falling/active block
;TODO: Set the active-shape to be:
    ; a) 2 separate state-properties: fall-seq and active-ind, where active-ind is the index of the shape we are using from the seq
    ; b) call to a .... however did in project

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   REPL/CMD-LINE VISUALIZATION + REPORTING   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn str-board
  "Returns 'board' as readable formatted string"
  [board]
  (apply str (mapcat #(conj % \newline) board)))

; TODO: Visualization - Quil visualization

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   BOARD-RELATED CONVERSIONS   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-cell
  "Accesses the block type/value within 2D vectors (board)"
  [x y matrix]
  (get-in matrix [y x]))

(defn abs-shape-coords
  "Returns the absolute coordinates (on 'board') of the active cells,
  given that the active-'shape' starts at '(ref-x,ref-y)' or :active-pos"
  [[ref-x ref-y] shape]
  (let [rows (count shape)
        cols (count (first shape))]
    (for [x (range cols) y (range rows)
          :when (= active-block (read-cell x y shape))]
      [(+ x ref-x) (+ ref-y y)])))

#_(def m [[1 2 3] [4 5 1] [7 8 9] [10 1 12]])
;#_(shape-coords m)                                          ; => [[0 0] [1 3] [2 1]]

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   GAME RULES CALCULATIONS AND CHECKS   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cell-valid?
  "checks for collisions - Returns bool for if the cell at (r,c) is un-filled and in the 'board' bounds"
  [x y board]
  (and board (< -1 x world-width) (< -1 y world-height)     ; board exists (non-nil) and the cell is within the world-width and world-height
       (= empty-block (read-cell x y board))))              ; the cell is empty

(defn state-valid?
  "Returns bool for if the 'state' with the active-block placed is valid.
  If not, return the original state"
  [{:keys [board active-shape active-pos] :as state}]
  (if (place-shape active-shape active-pos active-block board) true false))

(defn calc-level
  "Calculates level from total number of `lines` cleared. Max level of 10
  for 90+ lines."
  [lines]
  (cond
    (> lines 90) 10
    (pos? lines) (inc (quot (dec lines) 10))
    :else 0))

(defn score-lines
  "Returns score computed based on number of 'lines' cleared at once and
  current `level`. Better score when more lines cleared at once."
  [lines level]
  (let [base-score {0 0 1 40 2 100 3 300 4 1200}
        result (* (base-score lines) (inc level))]
    result))

(defn gameover?
  "Returns if the 'board' triggers gameover, meaning filled-blocks reach the top row"
  ([{:keys [board] :as state}] (gameover? board))
  ([board] 
   (not (empty? (for [block (first board) :when (= block filled-block)] block)))))

(defn imbed-shape?
  "Checks if the active-'shape' at the active-pos, '[ref-x ref-y]', in the 'board' is to be placed, meaning
   if there are any active-blocks that touch / are adjacent to a filled-block"
  ([active-shape [ref-x ref-y] board]
   (imbed-shape? (abs-shape-coords ref-x ref-y active-shape) board))
  ([active-cells board]
  (let [valid-coords? (every? true? (map #(cell-valid? (first %1) (second %1) board) active-cells)) ; if all the active coordinates are on valid (on the board and not filled)
        active-bottom-cells (into [] (for [[x y] active-cells :when (= y (dec world-height))] [x y])) ; active-cells/coords in bottom row
        active-above-filled-cells (into [] (for [[x y] active-cells :when (= filled-block (read-cell x (inc y) board))] [x y])) ; active-coords (cells) that have a filled-block in the cell immediately below
        ]
    (or (not (empty? active-bottom-cells)) (not (empty? active-above-filled-cells))))))

(defn shape-fits?
  "Checks if the active shape will collide with anything in the current board
  when the coords are translated, adding dx, dx to each of the shape's coords"
  [shape-coords dx dy board]
  (every? #(block-fits? % dx dy board) shape-coords))

(defn get-drop-pos
  "Get the future drop position of the given piece (at active-pos or [ref-x ref-y])"
  [shape [ref-x ref-y] x y board]
  (let [collide? (fn [cy] (not (shape-fits? (abs-shape-coords ref-x ref-y shape) x cy board)))
        cy (first (filter collide? (iterate inc y)))]
    (max y (dec cy))))

; TODO??? dropped-shape-state/board function... maybe for consideration of how good a state looks, for Push (Clojush) to decide what move to make
;         (so as not to only be able to consider the state fully when almost at bottom with almost no time before active-shape is placed) 
;   => would go in 'BOARD TRANSFORMATIONS' section

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   BOARD TRANSFORMATIONS   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn clear-rows
  "Returns the state with any filled rows cleared, number of cleared-rows,
  and updated score (if cleared rows)"
  [{:keys [board score level cleared-rows] :as state}]
  (let [; the board with the full rows removed
        remaining-rows (into [] (for [row board :when (some #(= empty-block %) row)] (into [] row)))
        num-rows-cleared (- (count board) (count remaining-rows))
        updated-cleared-rows (+ num-rows-cleared cleared-rows)
        updated-score (+ score (score-lines num-rows-cleared level))
        updated-board (reduce conj (make-board num-rows-cleared) remaining-rows)]
    (assoc state
      :cleared-rows updated-cleared-rows
      :board updated-board
      :score updated-score
      :level (calc-level updated-cleared-rows))))


(defn place-block
  "Returns updated 'board' with the 'block-type' placed at cell '(x,y)', or nil if invalid board"
  [board [x y] block-type]
  (when (cell-valid? x y board)
    (assoc-in board [y x] block-type)))

(defn place-blocks
  "Returns updated 'board' with blocks of type 'block-type' at cells in the 'coords' list, or 'nil' if invalid board"
  [board coords block-type]
  (if (> (count coords) 0)                                  ; if there are (still) blocks to be placed, place them
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
  "Returns state with updated 'board', with 'shape' placed at/relative to the 'active-pos' (coords: [x y]), or nil if invalid board"
  ([state] (place-shape state filled-block))
  ([{:keys [board score level cleared-rows active-pos active-ind active-shape] :as state} block-type]
   (let [coords-to-place (abs-shape-coords active-pos active-shape)
         placed-board (place-blocks board coords-to-place block-type)
         temp-part-state {:board placed-board :score score :level level :cleared-rows cleared-rows}
         updated-state (clear-rows temp-part-state)
         updated-score (inc (:score updated-state))
         updated-shape-ind (inc active-ind)]
     (when (:board updated-state)
       (assoc state
         :board (:board updated-state)
         :score updated-score
         :level (:level updated-state)
         :cleared-rows (:cleared-rows updated-state)
         :active-pos shape-spawn-pos
         :active-ind updated-shape-ind
         :active-shape nil)))))

(defn imbed-shape?
  "Checks if the active-'shape' at the active-pos, '[ref-x ref-y]', in the 'board' is to be placed, meaning
   if there are any active-blocks that touch / are adjacent to a filled-block"
  [{:keys [board active-pos active-shape]}]                 ; INPUT: state
  (let [[ref-x ref-y] active-pos
        active-cells (abs-shape-coords [ref-x ref-y] active-shape)
        ;valid-coords? (every? true? (map #(cell-valid? (first %1) (second %1) board) active-cells)) ; if all the active coordinates are on valid (on the board and not filled)
        active-bottom-cells (filter #(= (dec world-height) (second %)) active-cells) ; active-cells/coords in bottom row
        active-above-filled-cells (filter #(= filled-block (read-cell (first %1) (inc (second %1)) board)) active-cells) ; active-coords (cells) that have a filled-block in the cell immediately below
        ]
    (or (not (empty? active-bottom-cells)) (not (empty? active-above-filled-cells)))))

(defn gameover?
  "Returns if the 'board' triggers gameover, meaning filled-blocks reach the top row and/or
  the active-shape (a) intersects or (b) imbeds on filled-blocks when 'active-pos' is at the shape-spawn-pos / in the top row"
  [{:keys [board active-pos active-shape] :as state}]
  (or (and (= active-pos shape-spawn-pos) (imbed-shape? state)) ; active-shape immediately placed/intersects filled blocks
      (not (empty? (filter #(= filled-block %) (first board)))))) ; filled blocks in top row

(defn block-fits?
  "Checks if the given coordinates '(x,y)' fit after translation '(dx,dy)' on the 'board'"
  [[x y] dx dy board]
  (let [next-x (+ x dx)
        next-y (+ y dy)]
    (cell-valid? next-x next-y board)))

(defn shape-fits?
  "Checks if the active shape will collide with anything in the current board
  when the coords are translated, adding dx, dx to each of the shape's coords"
  [shape-coords dx dy board]
  (every? #(block-fits? % dx dy board) shape-coords))

(defn get-drop-pos
  "Get the future drop position of the given piece (at active-pos or [ref-x ref-y])"
  [shape [ref-x ref-y] x y board]
  (let [collide? (fn [cy] (not (shape-fits? (abs-shape-coords [ref-x ref-y] shape) x cy board)))
        cy (first (filter collide? (iterate inc y)))]
    (max y (dec cy))))

; TODO??? dropped-shape-state/board function... maybe for consideration of how good a state looks, for Push (Clojush) to decide what move to make
;         (so as not to only be able to consider the state fully when almost at bottom with almost no time before active-shape is placed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   MOVE TRANSFORMATIONS   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rotate-cw
  "rotates the inputted 'matrix' clockwise once"
  [matrix]
  (let [mtx (reverse matrix)]
    (apply mapv #(into [] %&) mtx)))

(defn rotate-ccw
  "rotates the inputted 'matrix' counter-clockwise once"
  [matrix]
  (into [] (reverse (apply mapv #(into [] %&) matrix))))

(defn block-fits?
  "Checks if the given coordinates '(x,y)' fit after translation '(dx,dy)' on the 'board'"
  [[x y] dx dy board]
  (let [next-x (+ x dx)
        next-y (+ y dy)]
    (cell-valid? next-x next-y board)))

(defn move-valid?
  "Checks if 'translation' of 'shape' positioned at 'pos' will be valid, so without colliding in the 'board'"
  [shape [ref-x ref-y] translation board]
  (case translation
    (or :rotate-left :rl -2) (every? #(block-fits? % 0 0 board) (abs-shape-coords [ref-x ref-y] (rotate-ccw shape)))
    (or :left -1) (every? #(block-fits? % -1 0 board) (abs-shape-coords [ref-x ref-y] shape))
    (or :fall :down 0) (every? #(block-fits? % 0 1 board) (abs-shape-coords [ref-x ref-y] shape))
    (or :right 1) (every? #(block-fits? % 1 0 board) (abs-shape-coords [ref-x ref-y] shape))
    (or :rotate-right :rr 2) (every? #(block-fits? % 0 0 board) (abs-shape-coords [ref-x ref-y] (rotate-ccw shape)))))

(defn move-shape
  "Returns the 'state' with the active-shape moved in the 'direction'. If invalid, returns the original state"
  [state direction]
  (if (and (not (gameover? state)) (move-valid? (:active-shape state) (:active-pos state) direction (:board state)))
    (case direction
      (or :left -1) (update-in state [:active-pos 0] dec)
      (or :down 0) (update-in state [:active-pos 1] inc)
      (or :right 1) (update-in state [:active-pos 0] inc)
      (or :rotate-left :rl -2) (assoc state :active-shape (rotate-ccw (:active-shape state)))
      (or :rotate-right :rr 2) (assoc state :active-shape (rotate-cw (:active-shape state))))
    state))

(defn move-state
  "Returns the transformed game state after the 'move'. If invalid, returns the original 'state'
   moves: {-1 = shift left, 0 = shift down/fall, 1 = shift right}"
  [state move seed]
  (let [move-state (move-shape state move)]
    (if (imbed-shape? move-state)
      (assoc (place-shape move-state) :active-shape (next-shape (:active-ind (place-shape move-state)) seed))
      move-state)))

(defn endgame
  "Returns the endgame state and reports info specified here"
  [end-state]
  (do (print end-state)
      end-state))

(defn display-board
  "Returns state with updated 'board', with 'shape' placed at/relative to the 'active-pos' (coords: [x y]), or nil if invalid board"
  [{:keys [board active-pos active-shape]}]
  (place-blocks board (abs-shape-coords active-pos active-shape) active-block))

(defn play-game
  "Game loop for Tetris, updating the state with each move"
  [seed]
  (loop [old-time (System/currentTimeMillis)
         initial-state (new-state seed)
         ;score 0
         ;shape-ind 0
         ;board (:board (new-state))
         ;shape (next-shape shape-ind seed)
         ]
    (Thread/sleep 2)

    ; NOTE: FOR TESTING + SHOWING GAME PROGRESS
    (println "******************\nGAME BOARD:\n" (str-board (display-board initial-state)))

    (let [curr-time (System/currentTimeMillis)
          new-time (long (if (> (- curr-time old-time) 5)   ; changes game tick
                           curr-time
                           old-time))
          fall? (> new-time old-time)
          state (if fall? (move-state initial-state :down seed) initial-state)

          ; state-features = CALL FUNCTION THAT RETURNS VECTOR OF THESE FEATURE VALUES (ints and bools)
          ;result             (peek-stack                                    ; what's on top of the .. stack? ... being the other arg to this function
          ;                    (interpret-program                            ; for the state of the game, decide move
          ;                     program
          ;                     (assoc empty-push-state :input               ; input will be the values from the state feature functions
          ;                            {:in1 (:in1 input)
          ;                             :in2 (:in2 input)
          ;                             :in3 (:in3 input)})
          ;                     (:step-limit argmap))
          ;                    :integer)
          ;move               (if (= result :no-stack-item)
          ;                     0
          ;                     result )                                     ; TODO: not quite right. If result is less than -2, just use -2. if larger than 2, just use 2

          ; NOTE: temporarily random move for now
          move (- (rand-int 5) 2)
          moved-state (move-state state move seed)
          ]
      (println "\tNEXT STATE:" (:board moved-state))
      ;(if (gameover? moved-state)
      (if (gameover? moved-state)
        ; return the endgame state
        (do (print "\n\nGAMEOVER\n\n")
            (endgame moved-state))

        (if (every? moved-state (keys (new-state)))
          (recur                                            ; recur to next game-round/iteration
            new-time
            moved-state)
          (print "FAILED. State value(s) ended up being nil"))))))

(play-game get-seed)

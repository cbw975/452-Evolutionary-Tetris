(ns tetris452.gameboard)

(def WIDTH (atom nil))
(def HEIGHT (atom nil))

(def COLS 10)
(def ROWS 20)

(def OFFSET (atom [0 0]))
(def ROTATION (atom nil))
(def COLORS ["red" "blue" "green" "yellow" "orange" "pink"])
(def SHAPES [[[0 1] [0 2] [0 3] [0 4]]
             [[0 0] [0 1] [1 1] [1 2]]
             [[1 2] [1 1] [0 1] [0 0]]
             [[0 1] [1 1] [1 0] [2 1]]
             [[0 0] [0 1] [1 0] [1 1]]
             [[0 0] [0 1] [0 2] [1 2]]
             [[1 0] [1 1] [1 2] [0 2]]])
;;edited so that it takes in a specific block from a seed, look at the github code for the original function
(defn get-block [next-seedNum]
  (let [shape (nth SHAPES next-seedNum)
        offset (inc 3)]
    {:color (rand-nth COLORS)
     :shape (map (fn [[x y]] [(+ x offset) y]) shape)}))

(defn get-board []
  (vec (take (* ROWS COLS) (repeat "black"))))

(defn pos-to-xy [pos]
  (let [x (mod pos COLS)
        y (int (/ (- pos x) COLS))]
    [x, y]))


(defn collides?
  ([board x y pos]
   (let [[posx posy] (pos-to-xy pos)]
     (and
       (> x (- 1))
       (< x COLS)
       (< y ROWS)
       (not (and
              (= posx x)
              (= posy y)
              (not= "black" (get board (+ pos COLS))))))))
  ([board shape pos]
   (every?
     #{true}
     (for [[x y] shape]
       (collides? board x y pos))))
  ([board shape]
   (not (reduce
          #(and %1 (collides? board shape %2))
          (range (count board))))))

(defn rotate [board shape]
  (if @ROTATION
    (let [[avg-x avg-y] (->> shape
                             (reduce
                               (fn [[tx ty] [x y]]
                                 [(+ tx x) (+ ty y)]))
                             (map #(int (/ % 4))))

          rotated (map (fn [[x y]]
                         [(int (+ avg-x (- y avg-y)))
                          (+(int (- avg-y (- x avg-x)))1)])
                       shape)]
      (if (collides? board rotated)
        shape rotated))
    shape))

(defn shift [board shape]
  (let [shifted (map
                  (fn [[x y]]
                    [(+ x (first @OFFSET)) y])
                  shape)]
    (if (collides? board shifted)
      shape shifted)))

(defn transform [board {:keys [color shape]} drop?]
  (let [rotated (->> shape (shift board) (rotate board))]
    {:color color
     :shape (if drop?
              (map (fn [[x y]] [x (inc y)]) rotated)
              rotated)}))

(defn clear-lines [board]
  (let [new-board (->> board
                       (partition COLS)
                       (filter #(some #{"black"} %))
                       (apply concat))
        num-removed (- (count board) (count new-board))]
    [num-removed
     (into (vec (take num-removed (repeat "black")))
           new-board)]))

(defn clear-lines [board]
  (let [new-board (->> board
                       (partition COLS)
                       (filter #(some #{"black"} %))
                       (apply concat))
        num-removed (- (count board) (count new-board))]
    [num-removed
     (into (vec (take num-removed (repeat "black")))
           new-board)]))

(defn update-board [board {:keys [color shape]}]
  (vec (map #(let [[x y] (pos-to-xy %)]
               (if (some (fn [[px py]] (and (= x px) (= y py)))
                         shape)
                 color (get board %)))
            (range (count board)))))

(defn game-over? [board]
  (not (reduce #(and %1 (= "black" %2))
               (butlast (rest (take COLS board))))))
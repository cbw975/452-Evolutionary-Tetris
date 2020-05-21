(ns tetris452.tempgame
  (:use tetris452.tempgameboard)
  ;(:use tetris452.core)
  (:import
    (javax.swing JFrame)
    (java.awt Canvas Font Graphics Color Toolkit))
  (:gen-class))

;Some highly performing individuals
;(def new-individual
;  {:genome [-160.3566 400.766 -0.31006 -1.0003]})
;(def new-individual
;  {:genome [-150.3566 400.766 -0.31006 -1.8443]})
;(def new-individual
;  {:genome [-150.3566 200.766 -0.31006 -0.6043]})
;(def new-individual
;  {:genome [-150.3566 200.766 -0.31006 -0.6043]})

(def get-seed (repeatedly #(rand-int 7)))

(defn board-see [board]
  (println board))

(defn abs [n] (max n (- n)))

(defn pos-check [pos]
  (if (and (< pos 200) (> pos -1))
    true
    false))

(defn aggregate-height [board]
  (loop [height-sum 0
         height     0
         pos        0
         counter    0]
    (if (>= counter 10)
      height-sum
      (cond
        (and (pos-check pos)(= (nth board pos) "black"))
        (recur
          height-sum
          (inc height)
          (+ pos 10)
          counter)

        :default
        (recur
          (+ height-sum (- 20 height))
          (* height 0)
          (+ (* pos 0) (inc counter))
          (inc counter))))))

(defn bumpiness [board]
  (loop [height-sum 0
         height     0
         prev-height 0
         pos        0
         counter    0]
    (if (>= counter 10)
      height-sum
      (cond
        (and (pos-check pos)(= (nth board pos) "black"))
        (recur
          height-sum
          (inc height)
          prev-height
          (+ pos 10)
          counter)

        (= counter 0)
        (recur
          height-sum
          (* height 0)
          (- 20 height)
          (+ (* pos 0) (inc counter))
          (inc counter))

        :default
        (recur
          (+ height-sum (abs (- (- 20 height) prev-height)))
          (* height 0)
          (- 20 height)
          (+ (* pos 0) (inc counter))
          (inc counter))))))

(defn cal-holes [board]
  (loop [counter 10
         holes 0]
    (if (>= counter (count board))                          ;; for any size board
      holes
      (recur
        (inc counter)
        (if (and (= (nth board counter) "black")
                 (not (= (nth board (- counter 10)) "black")))
          (inc holes)
          holes)))))

(defn complete-lines [board]
  (let [new-board (->> board
                       (partition 10)
                       (filter #(some #{"black"} %))
                       (apply concat))
        num-removed (- (count board) (count new-board))]
    (/ num-removed 10)))

(defn heuristic-sum [h1 h2 h3 h4 individual]
  (let [weights (:genome individual)]
    (+ (+
         (+ (* h1 (first weights)) (* h2 (nth weights 1)))
         (* h3 (nth weights 2)))
       (* h4 (nth weights 3)))))

(defn printoutstuff [results]
  (let [max (val (apply max-key val results))
        best (rand-nth (keys (into (hash-map) (filter #(>= (second %) max) results))))]
    (do
      best)))

(defn drop-board [board block drop?]
  (loop [current-board board
         current-block block
         condition drop?
         counter 0]
    (if (collides? current-board (:shape current-block))
      (do
        ;(println "Collision Detected")
        ;;Return the board here that goes in the heuristics
        ;; print out last board (board-see (update-board current-board current-block)))
        (update-board current-board current-block))

      (do
        (let [newer-board (update-board current-board (transform current-board current-block condition))]
          ;(board-see newer-board)
          ;(println counter)
          (recur
            current-board
            (transform current-board current-block condition)
            condition
            (inc counter)))))))

(defn calculate-move [board block drop? individual]
  (loop [results {:left 0 :right 0 :up 0 :down 0 :still 0}
         moves '(:left :right :up :down :still)
         counter 0]
    ;(println "This is the original board:")
    ;(println results)
    (reset! OFFSET [0 0])
    (reset! ROTATION nil)
    (if (>= counter 5)
      (do
        (reset! OFFSET [0 0])
        (reset! ROTATION nil)
        ;Defaulted to :down when weights are tied
        (printoutstuff results))

      (do
        (case (nth moves counter)
          :left (swap! OFFSET #(map + [-1 0] %))
          :right (swap! OFFSET #(map + [1 0] %))
          :up (reset! ROTATION :left)
          :down (reset! ROTATION :right)
          :still (swap! OFFSET #(map + [0 0] %))
          )
        ;(println "This is the current move" (nth moves counter) ". ")
        ;;What happens if the initial spawn collides? check transform
        (let [current-block (if (collides? board (:shape (transform board block false))) block (transform board block false))]
          (let [t-board (drop-board board current-block true)]
            (recur
              (assoc results (nth moves counter) (heuristic-sum (cal-holes t-board) (complete-lines t-board) (aggregate-height t-board) (bumpiness t-board) individual))
              moves
              (inc counter)
              )))))))

;;;;Controls;;;;

;(defn rand-move []
;(rand-nth '(:left :right :up :down)))
(defn rand-move []
  (rand-nth '(:left :left :left :left)))

(defn finish-game
  [score board] 
  (println {:score score 
            :board board}) 
  [score board]) 

(defn close-game [frame] 
  (doto frame 
    (.setVisible false) 
    (.dispose)))

(defn finish-game2 [ score board]
  (println {:score score
            :board board})
  [score board])

;;;;;;;UI;;;;;;;;;
(def colors {"black"  Color/black
             "blue"   Color/blue
             "green"  Color/green
             "yellow" Color/yellow
             "orange" Color/orange
             "pink"   Color/pink
             "red"    Color/red})

(defn draw [#^Canvas canvas draw-fn]
  (let [buffer (.getBufferStrategy canvas)
        g (.getDrawGraphics buffer)]
    (try
      (draw-fn g)

      (finally (.dispose g)))
    (if (not (.contentsLost buffer))
      (. buffer show))
    (.. Toolkit (getDefaultToolkit) (sync))))

(defn draw-square [x y color #^Graphics g]
  (let [width (/ @WIDTH COLS)
        height (/ @HEIGHT ROWS)
        xpos (* x width)
        ypos (* y width)]
    (doto g
      (.setColor (get colors color))
      (.fillRect xpos ypos width height)
      (.setColor Color/black)
      (.drawRect xpos ypos width height))))

(defn draw-text [#^Graphics g color text x y]
  (doto g
    (.setColor color)
    (.drawString text x y)))

(defn draw-game-over [score]
  (fn [#^Graphics g]
    (doto g
      (.setColor (new Color (float 0) (float 0) (float 0) (float 0.7)))
      (.fillRect 0 0 @WIDTH @HEIGHT))
    (draw-text g Color/red "GAME OVER" (- (/ @WIDTH 2) 50) (/ @HEIGHT 2))
    (draw-text g Color/red (str "Final Score: " score) (- (/ @WIDTH 2) 55) (+ 15 (/ @HEIGHT 2)))))

(defn draw-board [board block score]
  (fn [#^Graphics g]
    (doto g
      (.setColor Color/BLACK)
      (.fillRect 0 0 @WIDTH @HEIGHT))

    (doseq [square (range (count board))]
      (let [[x y] (pos-to-xy square)]
        (draw-square x y (get board square) g)))

    (doseq [[x y] (:shape block)]
      (draw-square x y (:color block) g))

    (draw-text g Color/green (str "score: " score) 20 25)))

;;Make this main method take in genome as a paramater, and then call a function that decides a move based on the genome and state of the board
(defn play-game [toDisplay individual]
  (reset! WIDTH 300)
  (reset! HEIGHT 600)
  (if toDisplay (let [frame (JFrame. "Tetris Genetic Programming")
                         canvas (Canvas.)]
                     (doto frame
                       (.setSize @WIDTH (+ (/ @HEIGHT ROWS) @HEIGHT))
                       (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
                       (.setResizable false)
                       (.add canvas)
                       (.setVisible true)
                       )

                     (doto canvas
                       (.createBufferStrategy 2)

                       (.setVisible true)
                       (.requestFocus))

                     ;;game loop
                     (loop [score 0
                            counter 0
                            ;seed get-seed
                            seed (:seed individual)
                            board (get-board)
                            block (get-block (nth seed counter))
                            old-time (System/currentTimeMillis)]
                       (reset! OFFSET [0 0])
                       (reset! ROTATION nil)
                       (Thread/sleep 5)


                       ;(calculate-move board block)
                       (draw canvas (draw-board board block score))

                       (let [cur-time (System/currentTimeMillis)
                             new-time (long (if (> (- cur-time old-time) 25) ;;changes game tick
                                              cur-time
                                              old-time))
                             drop? (> new-time old-time)
                             [num-removed new-board] (clear-lines board)]

                         (case (calculate-move board block drop? individual)
                           :left (swap! OFFSET #(map + [-1 0] %))
                           :right (swap! OFFSET #(map + [1 0] %))
                           :up (reset! ROTATION :left)
                           :down (reset! ROTATION :right)
                           :still (swap! OFFSET #(map + [0 0] %))
                           )

                         (cond
                           (game-over? board)

                             (do   (finish-game score board)   (close-game frame)) 

                           ;; (draw canvas (draw-game-over score))


                           (collides? board (:shape block))


                           ;;recursion once a block is placed
                           (do
                             ;(println "updated")
                             (recur
                               (inc score)
                               (inc counter)
                               seed
                               (update-board board block)
                               (get-block (nth seed (inc counter)))
                               new-time))
                           ;; this is the default recursion when the block is not colliding
                           :default
                           ;;must have the same number of variables to proceed
                           (do
                             ;(println "transformed")
                             (recur
                               (+ score (* num-removed num-removed))
                               counter
                               seed
                               new-board
                               (transform board block drop?)
                               new-time)
                             )
                           )))))
  (if (not toDisplay)
    (loop [score 0
           counter 0
           ;seed get-seed
           seed (:seed individual)
           board (get-board)
           block (get-block (nth seed counter))
           old-time (System/currentTimeMillis)]
      (reset! OFFSET [0 0])
      (reset! ROTATION nil)
      (Thread/sleep 1)

      (let [cur-time (System/currentTimeMillis)
            new-time (long (if (> (- cur-time old-time) 100) ;;changes game tick
                             cur-time
                             old-time))
            drop? (> new-time old-time)

            ;;Do a random move for this tick


            [num-removed new-board] (clear-lines board)]
        (case (calculate-move board block drop? individual)
          :left (swap! OFFSET #(map + [-1 0] %))
          :right (swap! OFFSET #(map + [1 0] %))
          :up (reset! ROTATION :left)
          :down (reset! ROTATION :right)
          :still (swap! OFFSET #(map + [0 0] %))
          )
        (cond
          (game-over? board)
          (finish-game2 score board)
          ;; (draw canvas (draw-game-over score))

          (collides? board (:shape block))
          ;;recursion once a block is placed
          (recur
            (inc score)
            (inc counter)
            seed
            (update-board board block)
            (get-block (nth seed (inc counter)))
            new-time)
          ;; this is the default recursion when the block is not colliding
          :default
          ;;must have the same number of variables to proceed
          (recur
            (+ score (* num-removed num-removed))
            counter
            seed
            new-board
            (transform board block drop?)
            new-time))))))
;(-main)


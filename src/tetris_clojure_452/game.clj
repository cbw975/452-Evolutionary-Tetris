;(ns tetrisclone.core
(ns tetris_clojure_452.game
  (:use tetris_clojure_452.gameboard)
  (:import
   (javax.swing JFrame)
   (java.awt Canvas Font Graphics Color Toolkit)
   (java.awt.event ActionListener KeyListener KeyEvent))
  (:gen-class))

(def get-seed (repeatedly #(rand-int 6)))


;;;;Controls;;;;

(defn rand-move []
  (rand-nth '(:left :right)))

(defn finish-game [score board]
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
(defn play-game [individual]                                ; TODO: USE GENOME AND SEED FROM INDIVIUDAL
  (reset! WIDTH 300)
  (reset! HEIGHT 600)
  ;;game loop
  (loop [score 0
         counter 0
         seed get-seed
         ;;seed (:seed individual)
         board (get-board)
         block (get-block (nth seed counter))
         old-time (System/currentTimeMillis)]
    (reset! OFFSET [0 0])
    (reset! ROTATION nil)
    (Thread/sleep 0)
    ;;random move here for now but use the calculate-move function here later; is this where the random move should be?
    (case (rand-move)
      :left (swap! OFFSET #(map + [-1 0] %))
      :right (swap! OFFSET #(map + [1 0] %))
      )

    (let [cur-time (System/currentTimeMillis)
          new-time (long (if (> (- cur-time old-time) 1) ;;changes game tick
                           cur-time
                           old-time))
          drop? (> new-time old-time)

          ;;Do a random move for this tick


          [num-removed new-board] (clear-lines board)]

      (cond
        (game-over? board)
        ;;updating the genome's score weight
        ;; Make the following line record the state of the board if tld to by us, return the score (for evolutoin fitness), whatever evolution needs, TBD
        ;; Finish-game closes the frame
        (finish-game score board)
        ;; (draw canvas (draw-game-over score))
        ;; return a vector of

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
          new-time)))))

;(-main)

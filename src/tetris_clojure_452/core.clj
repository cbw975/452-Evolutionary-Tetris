(ns tetris-clojure-452.core
  (:import
    (java.awt.event ActionListener KeyListener KeyEvent)))

;;; Resources:
;;; - using swing with Clojure

;; For storing game state + string based display to repl
(def empty-block 0)
(def active-block 1)
(def placed-blocks 2)
(def world-height 20) ;;world refers to the area tetris blocks can be in
(def world-width 10)

(def i-block [[0 1 0 0]
              [0 1 0 0]
              [0 1 0 0]
              [0 1 0 0]])
(def o-block [[1 1]
              [1 1]])
(def l-block [[0 1 0]
              [0 1 0]
              [0 1 1]])
(def r-block [[0 1 1]
              [0 1 0]
              [0 1 0]])
(def t-block [[0 0 0]
              [1 1 1]
              [0 1 0]])
(def z-block [[0 0 0]
              [1 1 0]
              [0 1 1]])
(def s-block [[0 0 0]
              [0 1 1]
              [1 1 0]])

(def blocks [i-block o-block l-block r-block t-block z-block s-block])

;;make a row (vector) of specified blocks
(defn make-row
  ([] (make-row world-width empty-block))
  ([num-rows block] (vec (repeat num-rows block))))

(defn empty-world []
  (make-row world-height (make-row))) ;empty rows up to world's height

(defn transpose [matrix]
  (into [] ;make transposed block structure a vector of rows
        (for [row (apply map list matrix)] ;transposes matrix, but structure ends up as list of lists.
          (into [] row)))) ;make each row a vector

;; swap the items at i1 and i2 in a seq (i.e. two rows the world, where items are rows and seq is world of rows)
(defn swap [items i1 i2]
  (assoc items i1 (items i2) i2 (items i1))) ; (assoc seq i1 item1 i2 item2) replaces item1 with item2 at index i2 and vice versa

;; rotates a tetris block (clockwise)
(defn rotate     ;; TODO: Rotate only if doesn't go out of bounds or collide with other blocks
  (swap (transpose block) 0 (dec (count block))))
;; Thought process Note: when a block rotates, each cell "moves" around the center-point (area-length - 1) over. (for i-block, think of middle 1's as own mini-block)
;; => Basically if block matrix is a square, can take the transpose of matrix and swap first and last rows

; TODO: keyboard events (arrow keys + shift to rotate)
; TODO: Check for game over
; TODO: Falling blocks generating, falling, etc.
; TODO: Placing blocks
; TODO: Tetris rules, etc.

(defn get-key-input [key world ])

(defn gameover? [world]
  )


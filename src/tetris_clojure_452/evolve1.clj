(ns tetris-clojure-452.evolve1
  (:require [tetris-clojure-452.game :as game]))

;; Clojure code for a Tetris playing evolutionary algorithm
;; SIMPLE WEIGHT EVOLUTION:
;; - "features" to consider for strategies
;; - relative "weight" (-10 to 10) for each feature determines in what way and how that feature influences move-choices when playing
;; - WEIGHT EVOLUTION - features are given, weights are evolved

(def moves '(:left :right :rotate :fall))                   ; possible moves that can be made

(def features [:numHoleTiles :bumpiness :height])

(defn dict-features-weights [features genome]
  "Returns a dictionary with ':feature weight' 'key value' pairs"
  (zipmap features genome))

(defn rand-weight []                                        ; used for setting random weights in gen 0 randomly generated individuals
  "Returns random float value b/w -10 and 10"
  (- (rand 22) 10))

(defn new-individual []
  "Returns a new, random individual."
  {:state  (game/start-state)
   :score  0
   :seed   0
   :genome [(rand-weight) (rand-weight) (rand-weight)]})    ; genome = weights (floats [-10,10) in order of defined features)

(defn make-population [population-size]
  (repeatedly population-size new-individual))

(defn best [individuals]
  "Returns the best-scoring of the given individuals."
  (reduce (fn [ind1 ind2]
            (if (> (:score ind1) (:score ind2))             ; If an individual is higher than the highest score so far,
              ind1
              ind2))                                        ; ... otherwise keep the individual[s] considered best so far
          individuals))                                     ; return the individuals that had the best values

(defn normalized-score [individual population]
  "Returns the normalized fitness aka score of the individual in a population"
  (/ (:score individual) (apply max (map :score population))))

(defn roulette-selection [population population-size]
  (loop [cnt 1 selected []]
    (if (> cnt population-size)
      selected
      (let [r (rand)
            pop (filter #(> (normalized-score % population) r) population)
            individual (rand-nth pop)]
        (recur (inc cnt) (conj selected individual))))))

(defn select [population population-size parent-selection group-size]
  "Returns an individual selected from population using the specified selection method
  Selection methods include: tournament (w/ replacement), roulette
  with group-size as the tournament or elitism size"
  (case parent-selection
    :tournament (best (repeatedly group-size #(rand-nth population)))
    :roulette (let [elites (take group-size (reverse (sort-by :score population)))
                    selection (roulette-selection population (- population-size group-size))]
                (into elites selection))))

(defn mutate [genome mutation-rate mutation-range]
  "Returns a possibly mutated copy of genome. Each gene (a weight for a feature) has a chance of being mutated (mutation-range)
  and if mutated, it is inc/dec by a small amount (mutation-range)"
  (for [gene genome]                                        ; For each gene,
    (if (> (rand) mutation-rate)                            ; 15% chance of mutating,
      gene                                                  ; 85 % chance not changed
      (if (> (rand) 0.5)                                    ; For mutations that do occur, 50%/50% chance will decrease/increase by a random float between [1,3)
        (max -10 (- gene (+ (rand mutation-range) (/ mutation-range 2))))
        (min 10 (+ gene (+ (rand mutation-range) (/ mutation-range 2))))))))

; TODO: Modify the inputs of this to be what game code needs --> individual's genome instead of individual?
;       --> TODO: choose-move function to decide a move during gameplay (called by game code) based on genome (unchanging while individual is playing a game, possibly mutated after a generation)
;TODO: falling-blocks generator function to generate a random sequence of blocks that will fall for the individuals of a given generation (so block seqs are different for each generation, but the same for individuals in a generation)         
(defn play-and-update [individual]
  "updates an indiviudal's state and score after it plays a game of tetris"
  (let [new-individual individual                           ; individual with updated/replaced state and score
        end-game (play-game individual (:seed individual))   ; output of gameplay
        new-score (first end-game)
        new-state (second end-game)]                        ; TODO: CHECK: reverse order of outputs if necessary
    (assoc new-individual :state new-state :score new-score))) ; return individual with newly associated values after gameplay

(defn make-child [population]
  "Returns a new, evaluated child, produced by mutating the result
  of a parent selected from the given population."
  (let [individual (select population (count population) :tournament 2)
        new-genome (mutate (:genome individual) 0.15 2)
        new-individual {:state (game/start-state)
                        :score 0
                        :seed 0
                        :genome new-genome}]
    (play-and-update new-individual)))

(defn score [population]
  (map #(play-and-update %) population))

; TODO: (high level functions) obtain the features info (i.e. how much hole tiles, actual height value, etc)

(defn rand-tetris-seq [seed]
  "generates a random sequence of falling tetris block for a generation of game(s)"
  ; TODO: called for the (random) sequence of falling blocks for a generation of individuals
  )

(defn reset-individuals [population]
  ; TODO: reset the state and score of each individual (that was mutaed, or just all for ease of coding)
  )

(defn report-individual [generation individual]
  "Prints a report on the status of an individual at the given generation."
  (println {:generation  generation
            :score  (:score individual)
            (dict-features-weights features (:genome individual))}))

(defn report-generation [generation population]
  "Prints a report on the status of the population at the given generation."
  (let [current-best (select population (count population) :tournament 2)]
    (println {:generation  generation
              :best-score  (:score current-best)
              :features    features})))

(defn evolve-tetris [population-size generations]
  "Runs an evolutionary algorithm to play tetris (strategies genome).
  Runs for a specified number of generations"
  (loop [population (make-population population-size)       ; at generation 0, create population with random weights (for given possible strategies)
         generation 0]                                      ; loop through each generation...
    (score population)       ; each individual play a tetris game (with their genome of strategy weights) and store the end-game state and score
    (report-generation generation population)                          ; Report on each generation
    (if (>= generation generations)
      (best population)                                     ; if last generation, return the best individual
      (recur
        (conj (repeatedly (dec population-size) #(make-child population)) ; make a child, have it play game, put it in the population
                   (best population))
        ;(reset-individuals population rand)                      ; TODO: reset individuals' states between generations
        (inc generation)))))

;Vector of cells (string  in 20 x 10

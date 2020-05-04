(ns tetris452.evolve
  (:use tetris452.gameboard)
  (:use tetris452.game)
  (:require [clojure.string :as string]))

;; Clojure code for a Tetris playing evolutionary algorithm
;; SIMPLE WEIGHT EVOLUTION:
;; - "features" to consider for strategies
;; - relative "weight" (-10 to 10) for each feature determines in what way and how that feature influences move-choices when playing
;; - WEIGHT EVOLUTION - features are given, weights are evolved

(def features [:numHoleTiles :bumpiness :height])

(defn dict-features-weights
  "Returns a dictionary with ':feature weight' 'key value' pairs"
  [features genome]
  (zipmap features genome))

(defn rand-weight                                           ; used for setting random weights in gen 0 randomly generated individuals
  "Returns random float value b/w -10 and 10"
  []
  (- (rand 22) 10))

(defn new-individual
  "Returns a new, random individual."
  []
  {:state  (get-board)                                      ; (mygame/start-state)
   :score  0
   :seed   0
   :genome [(rand-weight) (rand-weight) (rand-weight)]})    ; genome = weights (floats [-10,10) in order of defined features)

(defn make-population
  "Returns a population of population-size randomly weighted individuals"
  [population-size]
  (repeatedly population-size new-individual))

(defn best
  "Returns the best-scoring of the given individuals."
  [individuals]
  (reduce
    (fn [ind1 ind2]
      (if (> (:score ind1) (:score ind2))                   ; If an individual is higher than the highest score so far,
        ind1
        ind2))
    individuals))                                           ; return the individuals that had the best values

(defn normalized-score
  "Returns the normalized fitness aka score of the individual in a population"
  [individual population]
  (/ (:score individual) (apply max (map :score population))))

(defn roulette-selection
  "Roulette selection helper funtion - returns individual selected via roulette"
  [population population-size]
  (loop [cnt 1 selected []]
    (if (> cnt population-size)
      selected
      (let [r (rand)
            pop (filter #(> (normalized-score % population) r) population)
            individual (rand-nth pop)]
        (recur (inc cnt) (conj selected individual))))))

(defn select
  "Returns an individual selected from population using the specified selection method
  Selection methods include: tournament (w/ replacement), roulette
  with group-size as the tournament or elitism size"
  ([population] (select population -1 :tournament 2))
  ([population population-size parent-selection group-size]
   (case parent-selection
     :best (best population)
     :tournament (best (repeatedly group-size #(rand-nth population)))
     :roulette (let [elites (take group-size (reverse (sort-by :score population)))
                     selection (roulette-selection population (- population-size group-size))]
                 (into elites selection)))))

(defn mutate
  "Returns a possibly mutated copy of genome. Each gene (a weight for a feature) has a chance of being mutated (mutation-range)
  and if mutated, it is inc/dec by a small amount (mutation-range)"
  [genome mutation-rate mutation-range]
  (for [gene genome]                                        ; For each gene,
    (if (> (rand) mutation-rate)                            ; 15% chance of mutating,
      gene                                                  ; 85 % chance not changed
      (if (> (rand) 0.5)                                    ; For mutations that do occur, 50%/50% chance will decrease/increase by a random float between [1,3)
        (max -10 (- gene (+ (rand mutation-range) (/ mutation-range 2))))
        (min 10 (+ gene (+ (rand mutation-range) (/ mutation-range 2))))))))

;TODO: in game code: choose-move function to decide a move during gameplay (called by game code) based on genome (unchanging while individual is playing a game, possibly mutated after a generation)
(defn play-and-update
  "updates an indiviudal's state and score after it plays a game of tetris"
  [individual seed toDisplay]
  (let [new-seed seed
        new-individual (assoc individual :seed new-seed)    ; for the game code, only the seed and genome are used. The state and score will be updated at the end of the game
        game-output (play-game toDisplay new-individual)    ; output of gameplay
        game-score (first game-output)                      ; TODO: MAKE SURE PROPERLY GETS THE gameover-state and gameover-score
        game-state (second game-output)]
    (assoc new-individual :state game-state :score game-score))) ; return individual with newly associated values (state and score) after gameplay

(def child-count 0)

(defn make-child
  "Returns a new, evaluated child, produced by mutating the result
  of a parent selected from the given population."
  [population]
  (let [individual (select population)
        new-genome (mutate (:genome individual) 0.15 2)
        new-individual {:state  (get-board)                 ;empty board       :state (mygame/start-state)
                        :score  0
                        :seed   (:seed individual)
                        :genome new-genome}
        new-played-individual (play-and-update new-individual (:seed new-individual) false)
        ]
    ;(print "\n\tchildren:" (inc child-count) "\n")
    new-played-individual))

(defn score
  "Returns the population of individuals after playing their tetris games"
  [population falling-blocks toDisplay]
  (map #(play-and-update % falling-blocks toDisplay) population))

(defn individual-info
  "(map of) report on the status of an individual at the given generation"
  [generation individual]
  {:generation   generation
   :score        (:score individual)
   :part-of-seed (take 100 (:seed individual))
   :weights      (dict-features-weights features (:genome individual))})

(defn report-individual
  "Prints a report on the status of an individual at the given generation."
  [generation individual]
  (print "\t" (individual-info generation individual)) "\n")

(defn report-generation
  "Prints a report on the status of the population at the given generation."
  [generation population]
  (let [current-best (best population)]
    (println "\n************ gen report:" {:generation generation
                                           :best-score (:score current-best)
                                           :features   features})))

;;Things to make this better: make the name of the file display current generation; does the seed vector work?
(defn record-best
  "Creates a text file of the best individual in a given generation"
  [generation population]
  (let [fileName (string/join ["Gen_" generation ".txt"])]
    (spit fileName (with-out-str (println (individual-info generation (best population)))))))
;(spit fileName (individual-info generation (best population)))))

;(defn sus? [generation individual average]                  ; TODO: Make this instead take population, then "let" average calculation be done given population
;  (if (> (:weight individual) average + 50)
;    (record-best (concat "sus" generation) individual)))

(def get-seed1 (repeatedly #(rand-int 7)))

(defn in?
  "Returns true if coll contains item"
  [coll item]
  (some #(= item %) coll))

(defn evolve-tetris
  "Runs an evolutionary algorithm to play tetris (strategies genome).
  Runs for a specified number of generations.
  Optionally, can record certain generations, specified in a seq (i.e. vector) - Note: first generation is generation 1"
  [population-size generations displayGen recordGen]
  (loop [population (make-population population-size)       ; at generation 0, create population with random weights (for given possible strategies)
         generation 0]                                      ; loop through each generation...
    (report-generation generation population)               ; Report on each generation
    ;(print "\nIndividuals (and child until end):\n")        ; includes child until last generation
    ;(for [individual population] (print "\t" (report-individual generation individual)))
    (let [seed get-seed1
          population (score population seed false)
          ;(cond (contains? displayGen generation) (score population seed true)
          ;            :else (score population seed false))
          ]

      (if (.contains recordGen generation)                  ; If this generation is speicfied to be recorded...
        (record-best generation population)                 ; ... record it
        ; TODO: Insert option to video record the game being played --> call play-game with this "best" indivdual (weights and seed)
        )
      (if (> (:score (best population)) 400)
        (record-best generation population))
      (if (>= generation generations)
        ;(do (print "done!: ") (best population))                                  ; if last generation, return the best individual
        (print (individual-info generation (best population)))
        (recur (conj (repeatedly (dec population-size) #(make-child population)) ; make a child, have it play game, put it in the population
                     (best population))
               (inc generation))))))

; TODO: make toDisplay be able to specify individuals list instead of all???

(evolve-tetris 4 20 [] [1 5 10])
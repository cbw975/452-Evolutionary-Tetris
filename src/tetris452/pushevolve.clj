(ns tetris452.pushevolve
  (:gen-class)
  (:use tetris452.game))

;; Chloe Wohlgemuth (cbw975) 2020   -   https://github.com/cbw975/Evolutionary-Tetris
;;
;; This is a version of propel/core.clj from Lee Spector 
;; https://github.com/lspector/propel. This evolves 
;; tetris playing programs (individuals) and evolves
;; heuristics/behaviors (both the weights and arithmetic
;; combinations/manipulations of features) to decide moves 
;; given features/info about the current game-state / board.
;; Tetris Game code is game.clj

(def example-push-state
  {:exec    '()
   :integer '()
   :input   {:in1 4 :in2 2 :in3 1 :in4 0.5 :in5 0}})

; Instructions must all be either functions that take one Push state and return another
; or constant literals.
(def default-instructions
  (list
    'in1 'in2 'in3 'in4 'in5
    'integer_+ 'integer_- 'integer_* 'integer_% 'integer_=
    'exec_dup 'exec_if
    'boolean_and 'boolean_or 'boolean_not 'boolean_=
    'close
    true false
    -2                                                      ; rotate left
    -1                                                      ; left
    0                                                       ; fall/drop 1
    1                                                       ; right
    2                                                       ; rotate right
    ))

(def opens                                                  ; number of blocks opened by instructions (default = 0)
  {'exec_dup 1
   'exec_if  2})

;;;;;;;;;
;; Utilities

(def empty-push-state
  {:exec    '()
   :integer '()
   :string  '()
   :boolean '()
   :input   {}})

(defn not-lazy
  "Returns lst if it is not a list, or a non-lazy version of lst if it is."
  [lst]
  (if (seq? lst)
    (apply list lst)
    lst))

(defn push-to-stack
  "Pushes item onto stack in state"
  [state stack item]
  (update state stack conj item))

(defn pop-stack
  "Removes top item of stack."
  [state stack]
  (update state stack rest))

(defn peek-stack
  "Returns top item on a stack."
  [state stack]
  (if (empty? (get state stack))
    :no-stack-item
    (first (get state stack))))

(defn empty-stack?
  "Returns true if the stack is empty."
  [state stack]
  (empty? (get state stack)))

(defn get-args-from-stacks
  "Takes a state and a list of stacks to take args from. If there are enough args
  on each of the desired stacks, returns a map of the form {:state :args}, where
  :state is the new state and :args is a list of args from the stacks. If there
  aren't enough args on the stacks, returns :not-enough-args."
  [state stacks]
  (loop [state state
         stacks (reverse stacks)
         args '()]
    (if (empty? stacks)
      {:state state :args args}
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          :not-enough-args
          (recur (pop-stack state stack)
                 (rest stacks)
                 (conj args (peek-stack state stack))))))))

(defn make-push-instruction
  "A utility function for making Push instructions. Takes a state, the function
  to apply to the args, the stacks to take the args from, and the stack to return
  the result to. Applies the function to the args (taken from the stacks) and pushes
  the return value onto return-stack."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result (apply function (:args args-pop-result))
            new-state (:state args-pop-result)]
        (push-to-stack new-state return-stack result)))))

;;;;;;;;;
;; Instructions

(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:in1 (:input state))))

;; the input functions (in1, in2, etc) are all pushed on whichever stack based on their type/structure
(defn in2
  "Pushes the input labeled :in2 on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:in2 (:input state))))

(defn in3
  "Pushes the input labeled :in3 on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:in3 (:input state))))

(defn in4
  "Pushes the input labeled :in4 on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:in4 (:input state))))

(defn in5
  "Pushes the input labeled :in5 on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:in5 (:input state))))

(defn integer_+
  [state]
  (make-push-instruction state +' [:integer :integer] :integer))

(defn integer_-
  [state]
  (make-push-instruction state -' [:integer :integer] :integer))

(defn integer_*
  [state]
  (make-push-instruction state *' [:integer :integer] :integer))

(defn integer_%
  [state]
  (make-push-instruction state
                         (fn [int1 int2]
                           (if (zero? int2)
                             int1
                             (quot int1 int2)))
                         [:integer :integer]
                         :integer))

(defn integer_=
  [state]
  (make-push-instruction state = [:integer :integer] :boolean))

(defn exec_dup
  [state]
  (if (empty-stack? state :exec)
    state
    (push-to-stack state :exec (first (:exec state)))))

(defn exec_if
  [state]
  (make-push-instruction state
                         #(if %1 %3 %2)
                         [:boolean :exec :exec]
                         :exec))

(defn boolean_and
  [state]
  (make-push-instruction state #(and %1 %2) [:boolean :boolean] :boolean))

(defn boolean_or
  [state]
  (make-push-instruction state #(or %1 %2) [:boolean :boolean] :boolean))

(defn boolean_not
  [state]
  (make-push-instruction state not [:boolean] :boolean))

(defn boolean_=
  [state]
  (make-push-instruction state = [:boolean :boolean] :boolean))

;;;;;;;;;
;; Interpreter

(defn interpret-one-step
  "Takes a Push state and executes the next instruction on the exec stack."
  [state]
  (let [popped-state (pop-stack state :exec)
        first-raw (first (:exec state))
        first-instruction (if (symbol? first-raw)
                            (eval first-raw)
                            first-raw)]
    (cond
      (fn? first-instruction)
      (first-instruction popped-state)
      ;
      (integer? first-instruction)
      (push-to-stack popped-state :integer first-instruction)
      ;
      (string? first-instruction)
      (push-to-stack popped-state :string first-instruction)
      ;
      (seq? first-instruction)
      (update popped-state :exec #(concat %2 %1) first-instruction)
      ;
      (or (= first-instruction true) (= first-instruction false))
      (push-to-stack popped-state :boolean first-instruction)
      ;
      :else
      (throw (Exception. (str "Unrecognized Push instruction in program: "
                              first-instruction))))))

(defn interpret-program
  "Runs the given problem starting with the stacks in start-state."
  [program start-state step-limit]
  (loop [state (assoc start-state :exec program :step 1)]
    (if (or (empty? (:exec state))
            (> (:step state) step-limit))
      state
      (recur (update (interpret-one-step state) :step inc)))))

(defn push-from-plushy
  "Returns the Push program expressed by the given plushy representation."
  [plushy]
  (let [opener? #(and (vector? %) (= (first %) 'open))]     ;; [open <n>] marks opens
    (loop [push ()                                          ;; iteratively build the Push program from the plushy
           plushy (mapcat #(if-let [n (get opens %)] [% ['open n]] [%]) plushy)]
      (if (empty? plushy)                                   ;; maybe we're done?
        (if (some opener? push)                             ;; done with plushy, but unclosed open
          (recur push '(close))                             ;; recur with one more close
          push)                                             ;; otherwise, really done, return push
        (let [i (first plushy)]
          (if (= i 'close)
            (if (some opener? push)                         ;; process a close when there's an open
              (recur (let [post-open (reverse (take-while (comp not opener?)
                                                          (reverse push)))
                           open-index (- (count push) (count post-open) 1)
                           num-open (second (nth push open-index))
                           pre-open (take open-index push)]
                       (if (= 1 num-open)
                         (concat pre-open [post-open])
                         (concat pre-open [post-open ['open (dec num-open)]])))
                     (rest plushy))
              (recur push (rest plushy)))                   ;; unmatched close, ignore
            (recur (concat push [i]) (rest plushy))))))))   ;; anything else

;;;;;;;;;
;; GP

(defn make-random-plushy
  "Creates and returns a new plushy."
  [instructions max-initial-plushy-size]
  (repeatedly (rand-int max-initial-plushy-size)
              #(rand-nth instructions)))

(defn tournament-selection
  "Selects an individual from the population using a tournament."
  [pop argmap]
  (let [tournament-size (:tournament-size argmap)
        tournament-set (take tournament-size (shuffle pop))]
    (apply min-key :total-error tournament-set)))

(defn lexicase-selection
  "Selects an individual from the population using lexicase selection."
  [pop argmap]
  (loop [survivors pop
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (rand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

(defn select-parent
  "Selects a parent from the population using the specified method."
  [pop argmap]
  (case (:parent-selection argmap)
    :tournament (tournament-selection pop argmap)
    :lexicase (lexicase-selection pop argmap)))

(defn crossover
  "Crosses over two individuals using uniform crossover. Pads shorter one."
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (= shorter plushy-a)
                 plushy-b
                 plushy-a)
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))]
    (remove #(= % :crossover-padding)
            (map #(if (< (rand) 0.5) %1 %2)
                 shorter-padded
                 longer))))

(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the plushy) with some probability."
  [plushy instructions]
  (let [rand-code (repeatedly (inc (count plushy))
                              (fn []
                                (if (< (rand) 0.05)
                                  (rand-nth instructions)
                                  :mutation-padding)))]
    (remove #(= % :mutation-padding)
            (interleave (conj plushy :mutation-padding)
                        rand-code))))

(defn uniform-deletion
  "Randomly deletes instructions from plushy at some rate."
  [plushy]
  (remove (fn [x] (< (rand) 0.05))
          plushy))

(defn new-individual
  "Returns a new individual produced by selection and variation of
  individuals in the population."
  [pop argmap]
  {:plushy
   (let [prob (rand)]
     (cond
       (< prob 0.5) (crossover (:plushy (select-parent pop argmap))
                               (:plushy (select-parent pop argmap)))
       (< prob 0.75) (uniform-addition (:plushy (select-parent pop argmap))
                                       (:instructions argmap))
       :else (uniform-deletion (:plushy (select-parent pop argmap)))))})

(defn report
  "Reports information each generation."
  [pop generation]
  (let [best (first pop)]
    (println "-------------------------------------------------------")
    (println "               Report for Generation" generation)
    (println "-------------------------------------------------------")
    (print "Best plushy: ") (prn (:plushy best))
    (print "Best program: ") (prn (push-from-plushy (:plushy best)))
    (println "Best total error:" (:total-error best))
    (println "Best errors:" (:errors best))
    (println "Best behaviors:" (:behaviors best))
    (println)))

(defn propel-gp
  "Main GP loop."
  [{:keys [population-size max-generations error-function instructions
           max-initial-plushy-size num-games-per-individual]
    :as   argmap}]
  (println "Starting GP with args:" argmap)
  (loop [generation 0
         population (repeatedly
                      population-size
                      #(hash-map :plushy                    ; gives individual/hash-map a random plushy (genome) vector of instructions which will turn into a program
                                 (make-random-plushy instructions
                                                     max-initial-plushy-size)))] ; a map initially just containing the individuals, which are only plushies
    (let [evaluated-pop (sort-by :total-error
                                 (map (partial error-function argmap (generate-seeds num-games-per-individual)) ; adding to plushy-individuals => When evaluated, we get total error (vector of errors)
                                      population))]         ; full individual = plushy, total error, and errors vector
      (report evaluated-pop generation)
      (cond
        (zero? (:total-error (first evaluated-pop))) (println "SUCCESS")
        (>= generation max-generations) nil
        :else (recur (inc generation)
                     (repeatedly population-size
                                 #(new-individual evaluated-pop argmap)))))))

;;;;;;;;;
;; Tetris game

(defn play-game
  "Returns ending score of a tetris game played with 'seed' as the falling sequence.
  Updates the state with each move and fall from time-elapse."
  [program seed argmap]
  (loop [old-time (System/currentTimeMillis)
         initial-state (new-state seed)]
    (Thread/sleep 10)

    (let [curr-time (System/currentTimeMillis)
          new-time (long (if (> (- curr-time old-time) (:pace initial-state)) ; changes game tick
                           curr-time
                           old-time))
          fall? (> new-time old-time)
          state (if fall? (move-state initial-state :down seed) initial-state)
          fell-gameover? (gameover? state)

          result (peek-stack
                   (interpret-program
                     program
                     (assoc empty-push-state :input {:in1 (get-aggregate-height state)
                                                     :in2 (get-max-height state)
                                                     :in3 (get-bumpiness state)
                                                     :in4 (get-bumpiness state)
                                                     :in5 (get-num-holes state)})
                     (:step-limit argmap))
                   :integer)
          move (cond
                 (= result :no-stack-item) 0
                 (= result 0) 0
                 (<= result -2) -2
                 (= result -1) -1
                 (= result 1) 1
                 (>= result 2) 2)
          moved-state (move-state state move seed)]
      ;(println "fall?:" fall? "\tmove:" move "\tmoved-board: " (:board moved-state))
      (if fell-gameover?
        (endgame state)                                     ; return the endgame state, after fall
        (if (gameover? moved-state)
          (endgame moved-state)                             ; return the endgame state, after move
          (when (seq (:board moved-state))
            (recur new-time moved-state)))))))              ; recur to next game-round/iteration

(defn tetris-error-function
  "Finds the behaviors and errors of an individual: Error is the scaled and inverted scores of played Tetris game(s)
  with the program's selected behavior. The behavior is here defined as the final top item on the :integer stack."
  [argmap seeds individual]                                 ; error function gets argmap (for many systems, passed around) and all the system settings (pop size, etc) = way to avoid global variables
  (let [program (push-from-plushy (:plushy individual))     ; individual with just a plushy (aka genome)
        inputs seeds                                        ; Each call to error function will play (a) whole game(s) and produce a (vector of) score(s)
        outputs (map (fn [input] (play-game program input argmap)) inputs) ; outputs = scores of the games played
        errors (map (fn [score] (/ 1 (inc score))) outputs)]
    (assoc individual
      :behaviors outputs
      :errors errors
      :total-error (apply +' errors))))

(defn -main
  "Runs propel-gp, giving it a map of arguments."
  [& args]
  (binding [*ns* (the-ns 'tetris452.pushevolve)]
    (propel-gp (update-in (merge {:instructions             default-instructions
                                  :error-function           tetris-error-function
                                  :max-generations          100
                                  :population-size          50
                                  :max-initial-plushy-size  50
                                  :step-limit               50 ;100 ; NOTE: How big we allow our programs to be. If programs have loops, want this to be larger.
                                  :parent-selection         :tournament
                                  :tournament-size          5
                                  :num-games-per-individual 2}
                                 (apply hash-map
                                        (map read-string args))) ; the stuff around the map is to allow overrides of mapped values (so we can run with cmd-line args)
                          [:error-function]
                          #(if (fn? %) % (eval %))))))
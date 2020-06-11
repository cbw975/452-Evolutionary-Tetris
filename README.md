# tetris452
Chloe Wohlgemuth's Clojure library designed to evolve Tetris playing computer programs using (simple) PushGP.

## Description
Evolutionary Tetris is a simple implementation of PushGP to evolve Tetris programs (players) and their gameplay strategies. It evolves heuristics/behaviors (both the weights and arithmetic combinations/manipulations of features) to decide moves given features/info about the current game-state / board. 

The Tetris Game code is in game.clj. It follows simple rules of Tetris. 

## Usage
This is a version of propel/core.clj modified from Lee Spector.
To run PushGP on the Tetris genetic programming problem from a REPL, load tetris452.pushevolve into your REPL (i.e. lein repl), and run (-main). 

User-specifiable arguments include:
* max-generations
* population-size
* max-initial-plushy-size
* step-limit  -  How big we allow our programs to be. If programs have loops, want this to be larger.
* parent-selection (generally: tournament)
* tournament-size
* num-games-per-individual  -  How many sequences of falling blocks each individual plays in a generation. A given seed/seq is the same between individuals in a generation, but changes over generations.

To run PushGP on the default genetic programming problem from command line, execute lein run. Command-line arguments may be provided to override the defaults specified in -main, for example, lein run :population-size 100. You can use something like lein run | tee outfile to send output both to the terminal and to outfile.

# munchman-gloss

a clone of the arcade pac man written in haskell using gloss for graphics and game loop and alut for background music and sounds.

## In Work
+ Ghost Collision and reduce score, game over on negative score


## TODO
+ Refactor magic numbers is Tunnel passing in Game.hs
+ Refactor: Tidy up Ghost.hs, Board.hs, Lib.hs, Game.hs, maybe introduce global State 
+ Game Over
+ Level Up
+ search for `TODO` and `undefined` in source code
+ Blue Pill Chase Mode
+ kill Ghosts in Bluepill mode, aimate Ghosts dying
+ Cherries

## DONE
+ Score Count display
+ make Ghosts more intelligen, eg.
    + use shortes path algorithm
    + to introduce randomness, use a random subset of the gamefield for the shortest path
module Main (main) where

import Game qualified

main :: IO ()
main = Game.makeState 7 >>= Game.runGame

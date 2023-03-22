module Main (main) where

import Main.Utf8 (withUtf8)
import System.IO.CodePage (withCP65001)

import Game qualified

main :: IO ()
main = withUtf8 $ withCP65001 $ Game.makeState 7 >>= Game.runGame

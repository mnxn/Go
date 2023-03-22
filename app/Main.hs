module Main (main) where

import Main.Utf8 (withUtf8)
import System.Environment
import System.IO.CodePage (withCP65001)

import Game qualified

main :: IO ()
main = withUtf8 $ withCP65001 $ do
    args <- getArgs
    case args of
        [] -> Game.runGame True
        ["--ansi"] -> Game.runGame True
        ["--ascii"] -> Game.runGame False
        _ -> putStrLn helpMessage

helpMessage :: String
helpMessage =
    unlines
        [ "Usage:"
        , "  --ascii"
        , "        use ascii output"
        , "  --ansi"
        , "        use ansi output"
        , "  --help"
        , "        show help message"
        ]

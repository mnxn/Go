module Main (main) where

import Main.Utf8 (withUtf8)
import System.Environment
import System.IO.CodePage (withCP65001)

import Game qualified

main :: IO ()
main = withUtf8 $ withCP65001 $ do
    args <- getArgs
    case args of
        [] -> Game.run True
        ["--ansi"] -> Game.run True
        ["--ascii"] -> Game.run False
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

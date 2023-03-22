module Display (
    Params (..),
    ascii,
    ansi,
    newLine,
    char,
    string,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Colour.SRGB (sRGB24)
import Data.Word (Word8)
import System.Console.ANSI

data Params io = Params
    { intersection :: io ()
    , intersectionN :: io ()
    , intersectionE :: io ()
    , intersectionS :: io ()
    , intersectionW :: io ()
    , cornerNE :: io ()
    , cornerSE :: io ()
    , cornerSW :: io ()
    , cornerNW :: io ()
    , vertical :: io ()
    , horizontal :: io ()
    , black :: io ()
    , white :: io ()
    , begin :: io ()
    , end :: io ()
    , clear :: io ()
    }

newLine :: MonadIO io => io ()
newLine = liftIO $ putStrLn ""

char :: MonadIO io => Char -> io ()
char c = liftIO $ putChar c

string :: MonadIO io => String -> io ()
string s = liftIO $ putStr s

color :: MonadIO io => ConsoleLayer -> Word8 -> Word8 -> Word8 -> io ()
color l r g b = liftIO $ setSGR [SetRGBColor l $ sRGB24 r g b]

reset :: MonadIO io => io ()
reset = liftIO $ setSGR [Reset]

ascii :: MonadIO io => Params io
ascii =
    Params
        { intersection
        , intersectionN = intersection
        , intersectionE = intersection
        , intersectionS = intersection
        , intersectionW = intersection
        , cornerNE = intersection
        , cornerSE = intersection
        , cornerSW = intersection
        , cornerNW = intersection
        , vertical
        , horizontal
        , black
        , white
        , begin
        , end
        , clear
        }
  where
    intersection = char '+'
    vertical = char '|'
    horizontal = string "---"
    black = char 'B'
    white = char 'W'
    begin = return ()
    end = newLine
    clear = return ()

ansi :: MonadIO io => Params io
ansi =
    Params
        { intersection
        , intersectionN
        , intersectionE
        , intersectionS
        , intersectionW
        , cornerNE
        , cornerSE
        , cornerSW
        , cornerNW
        , vertical
        , horizontal
        , black
        , white
        , begin
        , end
        , clear
        }
  where
    intersection = string "┼"
    intersectionN = string "┬"
    intersectionE = string "┤"
    intersectionS = string "┴"
    intersectionW = string "├"

    cornerNE = string "┐"
    cornerSE = string "┘"
    cornerSW = string "└"
    cornerNW = string "┌"

    vertical = string "│"

    horizontal = string "───"

    black = string "■"
    white = string "○"

    begin = do
        color Background 210 180 140
        color Foreground 0 0 0
    end = reset >> newLine

    clear = liftIO clearScreen

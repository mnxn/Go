module Display (
    DisplayParams (..),
    ascii,
    ansi,
    Display (display),
    newLine,
    char,
    string,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Colour.SRGB (sRGB24)
import Data.Word (Word8)
import System.Console.ANSI

data DisplayParams io = DisplayParams
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
    , verticalE :: io ()
    , verticalW :: io ()
    , horizontal :: io ()
    , black :: io ()
    , white :: io ()
    }

class Display d where
    display :: MonadIO io => DisplayParams io -> d -> io ()

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

ascii :: MonadIO io => DisplayParams io
ascii =
    DisplayParams
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
        , verticalE = vertical
        , verticalW = vertical
        , horizontal
        , black
        , white
        }
  where
    intersection = char '+'
    vertical = char '|'
    horizontal = string "---"
    black = char 'B'
    white = char 'W'

ansi :: MonadIO io => DisplayParams io
ansi =
    DisplayParams
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
        , verticalE
        , verticalW
        , horizontal
        , black
        , white
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
    verticalE = string "│"
    verticalW = string "│"

    horizontal = string "───"

    black = do
        string "■"
        reset

    white = do
        string "■"
        reset

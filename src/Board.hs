module Board (
    Piece (Empty, Black, White),
    Board (Board),
    Position,
    make,
    position,
    get,
    set,
    remove,
) where

import Data.Vector.Mutable qualified as VM

data Piece = Empty | Black | White

data Board = Board
    { width :: Int
    , vector :: VM.IOVector Piece
    }

data Position = Position
    { row :: Int
    , column :: Int
    }

make :: Int -> IO Board
make width = Board width <$> VM.replicate (width * width) Empty

position :: Board -> (Int, Int) -> Maybe Position
position b (row, column)
    | inRange row && inRange column = Just $ Position{row, column}
    | otherwise = Nothing
  where
    inRange i = 0 <= i && i < width b

index :: Board -> Position -> Int
index Board{width} Position{row, column} = row * width + column

get :: Board -> Position -> IO Piece
get b pos = VM.read (vector b) (index b pos)

set :: Board -> Position -> Piece -> IO ()
set b pos = VM.write (vector b) (index b pos)

remove :: Board -> Position -> IO ()
remove b pos = set b pos Empty

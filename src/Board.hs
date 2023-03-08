module Board (
    Piece (Empty, Black, White),
    Board (Board, width),
    Position,
    make,
    position,
    positions,
    get,
    set,
    remove,
    equals,
    fromList,
) where

import Control.Monad (forM_, liftM2)
import Data.Vector.Mutable qualified as VM

data Piece = Empty | Black | White
    deriving (Eq, Show)

data Board = Board
    { width :: Int
    , vector :: VM.IOVector Piece
    }

data Position = Position
    { row :: Int
    , column :: Int
    }
    deriving (Eq, Show)

make :: Int -> IO Board
make width = Board width <$> VM.replicate (width * width) Empty

position :: Board -> (Int, Int) -> Maybe Position
position b (row, column)
    | inRange row && inRange column = Just $ Position{row, column}
    | otherwise = Nothing
  where
    inRange i = 0 <= i && i < width b

positions :: Board -> [Position]
positions b = Position <$> indices b <*> indices b

index :: Board -> Position -> Int
index Board{width} Position{row, column} = row * width + column

indices :: Board -> [Int]
indices Board{width} = [0 .. width - 1]

get :: Board -> Position -> IO Piece
get b pos = VM.read (vector b) (index b pos)

set :: Board -> Position -> Piece -> IO ()
set b pos = VM.write (vector b) (index b pos)

remove :: Board -> Position -> IO ()
remove b pos = set b pos Empty

equals :: Board -> Board -> IO Bool
equals l r
    | width l /= width r = return False
    | otherwise = equals' (positions l)
  where
    equals' :: [Position] -> IO Bool
    equals' [] = return True
    equals' (p : ps) = liftM2 (&&) ((==) <$> get l p <*> get r p) (equals' ps)

fromList :: Int -> [[Piece]] -> IO Board
fromList width xss = do
    b <- make width
    let is = indices b
    forM_ (zip is xss) $ \(row, xs) ->
        forM_ (zip is xs) $ \(column, x) ->
            set b Position{row, column} x
    return b

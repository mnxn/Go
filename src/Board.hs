module Board (
    Display (display),
    Player (Black, White),
    Piece (Empty, Piece),
    Board (Board, width),
    Position (row, column),
    make,
    position,
    positions,
    get,
    set,
    remove,
    equals,
    fromList,
) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (chr, ord)
import Data.Vector.Mutable qualified as VM
import Text.Printf (printf)

class Display d where
    display :: MonadIO io => d -> io ()

data Player = Black | White
    deriving (Eq, Show)

data Piece = Empty | Piece Player
    deriving (Eq, Show)

instance Display Piece where
    display :: MonadIO io => Piece -> io ()
    display Empty = liftIO $ putChar '+'
    display (Piece Black) = liftIO $ putChar 'B'
    display (Piece White) = liftIO $ putChar 'W'

data Position = Position
    { row :: Int
    , column :: Int
    }
    deriving (Eq, Ord, Show)

data Board = Board
    { width :: Int
    , vector :: VM.IOVector Piece
    }

instance Display Board where
    display :: MonadIO io => Board -> io ()
    display b = liftIO $ do
        forM_ (indices b) $ \col ->
            printf "   %c" (chr $ ord 'A' + col)
        putStrLn ""
        forM_ (reverse $ indices b) $ \row -> do
            printf "%-3d" (row + 1)
            forM_ (indices b) $ \column -> do
                piece <- get b Position{row, column}
                display piece
                when (column < width b - 1) $ putStr "---"
            putStrLn ""
            when (row > 0) $ do
                replicateM_ (width b) $ putStr "   |"
                putStrLn ""

make :: MonadIO io => Int -> io Board
make width = liftIO $ Board width <$> VM.replicate (width * width) Empty

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

get :: MonadIO io => Board -> Position -> io Piece
get b pos = liftIO $ VM.read (vector b) (index b pos)

set :: MonadIO io => Board -> Position -> Piece -> io ()
set b pos piece = liftIO $ VM.write (vector b) (index b pos) piece

remove :: MonadIO io => Board -> Position -> io ()
remove b pos = set b pos Empty

equals :: MonadIO io => Board -> Board -> io Bool
equals l r
    | width l /= width r = return False
    | otherwise = equals' (positions l)
  where
    equals' :: MonadIO io => [Position] -> io Bool
    equals' [] = return True
    equals' (p : ps) = liftM2 (&&) ((==) <$> get l p <*> get r p) (equals' ps)

fromList :: MonadIO io => Int -> [[Piece]] -> io Board
fromList width xss = do
    b <- make width
    let is = indices b
    forM_ (zip is xss) $ \(row, xs) ->
        forM_ (zip is xs) $ \(column, x) ->
            set b Position{row, column} x
    return b

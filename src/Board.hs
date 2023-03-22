module Board (
    Display (display),
    Player (Black, White),
    opposite,
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

import Display

data Player = Black | White
    deriving (Eq, Show)

instance Display Player where
    display :: DisplayParams io -> Player -> io ()
    display params Black = Display.black params
    display params White = Display.white params

opposite :: Player -> Player
opposite Black = White
opposite White = Black

data Piece = Empty | Piece Player
    deriving (Eq, Show)

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
    display :: MonadIO io => DisplayParams io -> Board -> io ()
    display params b = do
        forM_ (indices b) $ \col ->
            liftIO $ printf "   %c" (chr $ ord 'A' + col)
        Display.newLine
        forM_ (reverse $ indices b) $ \row -> do
            liftIO $ printf "%-3d" (row + 1)
            forM_ (indices b) $ \column -> do
                piece <- get b Position{row, column}
                case piece of
                    Empty | row == 0 && column == 0 -> Display.cornerSW params
                    Empty | row == 0 && column == width b - 1 -> Display.cornerSE params
                    Empty | row == 0 -> Display.intersectionS params
                    Empty | row == width b - 1 && column == 0 -> Display.cornerNW params
                    Empty | row == width b - 1 && column == width b - 1 -> Display.cornerNE params
                    Empty | row == width b - 1 -> Display.intersectionN params
                    Empty | column == 0 -> Display.intersectionW params
                    Empty | column == 0 -> Display.intersectionE params
                    Empty | column == width b - 1 -> Display.intersectionE params
                    Empty -> Display.intersection params
                    Piece p -> Display.display params p
                when (column < width b - 1) $ Display.horizontal params
            Display.newLine
            when (row > 0) $ do
                forM_ (indices b) $ \i -> do
                    Display.string "   "
                    if i == 0
                        then Display.verticalW params
                        else
                            if i == width b - 1
                                then Display.verticalE params
                                else Display.vertical params
                Display.newLine

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

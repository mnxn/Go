module Board (
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
    count,
    fromList,
    display,
) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (chr, ord)
import Data.Vector.Mutable qualified as VM
import Text.Printf (printf)

import Display qualified

data Player = Black | White
    deriving (Eq, Show)

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

opposite :: Player -> Player
opposite Black = White
opposite White = Black

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

count :: Board -> Player -> IO Int
count b player = VM.foldr counter 0 (vector b)
  where
    counter :: Piece -> Int -> Int
    counter piece acc
        | piece == Piece player = 1 + acc
        | otherwise = acc

fromList :: MonadIO io => Int -> [[Piece]] -> io Board
fromList width xss = do
    b <- make width
    let is = indices b
    forM_ (zip is xss) $ \(row, xs) ->
        forM_ (zip is xs) $ \(column, x) ->
            set b Position{row, column} x
    return b

display :: forall io. MonadIO io => Display.Params io -> Board -> io ()
display params b = do
    forM_ (indices b) $ \column ->
        liftIO $ printf "   %c" (chr $ ord 'A' + column)
    Display.newLine
    forM_ (reverse $ indices b) $ \row -> do
        liftIO $ printf "%-3d" (row + 1)
        Display.begin params
        forM_ (indices b) $ \column -> do
            piece <- get b Position{row, column}
            displayPiece row column piece
            when (column < width b - 1) $ Display.horizontal params
        Display.end params
        when (row > 0) $ do
            Display.string "   "
            Display.begin params
            forM_ (indices b) $ \column -> do
                Display.vertical params
                when (column < width b - 1) $ Display.string "   "
            Display.end params
  where
    displayPiece :: Int -> Int -> Piece -> io ()
    displayPiece _ _ (Piece Black) = Display.black params
    displayPiece _ _ (Piece White) = Display.white params
    displayPiece r c Empty
        | r == w && c == w = Display.cornerNE params
        | r == w && c == 0 = Display.cornerNW params
        | r == w = Display.intersectionN params
    displayPiece 0 c Empty
        | c == w = Display.cornerSE params
        | c == 0 = Display.cornerSW params
        | otherwise = Display.intersectionS params
    displayPiece _ c Empty
        | c == w = Display.intersectionE params
        | c == 0 = Display.intersectionW params
        | otherwise = Display.intersection params

    w = width b - 1

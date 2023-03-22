module Logic (
    LogicError (PositionTaken, SelfCapture),
    neighbors,
    Group (Group, getGroupSet),
    group,
    liberties,
    play,
) where

import Control.Monad (filterM)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (foldrM)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

import Board (Board, Position)
import Board qualified

data LogicError
    = PositionTaken
    | SelfCapture
    deriving (Show, Eq)

newtype Group = Group {getGroupSet :: Set Position} deriving (Show, Eq)

isPiece :: MonadIO io => Board -> Board.Piece -> Position -> io Bool
isPiece b piece = fmap (== piece) . Board.get b

neighbors :: Board -> Position -> [Position]
neighbors b pos =
    let (row, column) = (Board.row pos, Board.column pos)
     in mapMaybe
            (Board.position b)
            [ (row - 1, column)
            , (row, column + 1)
            , (row + 1, column)
            , (row, column - 1)
            ]

group :: MonadIO io => Board -> Position -> io Group
group b start = do
    target <- Board.get b start
    Group . Set.insert start <$> group' target Set.empty start
  where
    group' :: MonadIO io => Board.Piece -> Set Position -> Position -> io (Set Position)
    group' target set pos
        | Set.member pos set = return set
        | otherwise = do
            piece <- Board.get b pos
            if piece == target
                then mconcat <$> mapM (group' target (Set.insert pos set)) (neighbors b pos)
                else return Set.empty

liberties :: MonadIO io => Board -> Position -> io (Set Position)
liberties b start = do
    Group g <- group b start
    foldrM liberties' Set.empty g
  where
    liberties' :: MonadIO io => Position -> Set Position -> io (Set Position)
    liberties' pos acc = mappend acc . Set.fromList <$> filterM (isPiece b Board.Empty) (neighbors b pos)

play :: MonadIO io => Board -> Position -> Board.Player -> ExceptT LogicError io (Set Position)
play b pos player = do
    currentPiece <- Board.get b pos
    case currentPiece of
        Board.Piece _ -> throwError PositionTaken
        Board.Empty -> do
            Board.set b pos (Board.Piece player)
            ownLiberties <- liberties b pos
            oppositeNeighbors <- filterM isOppositePiece (neighbors b pos)
            oppositeCaptures <- captures oppositeNeighbors
            if Set.null ownLiberties && Set.null oppositeCaptures
                then do
                    Board.remove b pos
                    throwError SelfCapture
                else return oppositeCaptures
  where
    isOppositePiece :: MonadIO io => Position -> io Bool
    isOppositePiece = isPiece b (Board.Piece $ Board.opposite player)

    captures :: MonadIO io => [Position] -> io (Set Position)
    captures [] = return Set.empty
    captures (p : ps) = do
        ls <- liberties b p
        if Set.null ls
            then mappend . getGroupSet <$> group b p <*> captures ps
            else captures ps

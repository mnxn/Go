module Logic (
    LogicError (PositionTaken, SelfCapture),
    neighbors,
    group,
    liberties,
    play,
) where

import Control.Monad (filterM)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

import Board (Board, Position)
import Board qualified
import Control.Monad.Except (ExceptT, MonadError (throwError), MonadTrans (lift))
import Data.Foldable (foldrM)

data LogicError
    = PositionTaken
    | SelfCapture
    deriving (Show, Eq)

isPiece :: Board -> Board.Piece -> Position -> IO Bool
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

group :: Board -> Position -> IO (Set Position)
group b start = do
    target <- Board.get b start
    Set.insert start <$> group' target Set.empty start
  where
    group' :: Board.Piece -> Set Position -> Position -> IO (Set Position)
    group' target set pos
        | Set.member pos set = return set
        | otherwise = do
            piece <- Board.get b pos
            if piece == target
                then mconcat <$> mapM (group' target (Set.insert pos set)) (neighbors b pos)
                else return Set.empty

liberties :: Board -> Position -> IO (Set Position)
liberties b start = do
    g <- group b start
    foldrM liberties' Set.empty g
  where
    liberties' :: Position -> Set Position -> IO (Set Position)
    liberties' pos acc = mappend acc . Set.fromList <$> filterM (isPiece b Board.Empty) (neighbors b pos)

play :: Board -> Position -> Board.Player -> ExceptT LogicError IO (Set Position)
play b pos player = do
    currentPiece <- Board.get b pos
    case currentPiece of
        Board.Piece _ -> throwError PositionTaken
        Board.Empty -> do
            Board.set b pos (Board.Piece player)
            ownLiberties <- lift $ liberties b pos
            oppositeNeighbors <- lift $ filterM isOppositePiece (neighbors b pos)
            oppositeCaptures <- lift $ captures oppositeNeighbors
            if Set.null ownLiberties && Set.null oppositeCaptures
                then do
                    Board.remove b pos
                    throwError SelfCapture
                else return oppositeCaptures
  where
    isOppositePiece :: Position -> IO Bool
    isOppositePiece = isPiece b (Board.Piece $ Board.opposite player)

    captures :: [Position] -> IO (Set Position)
    captures [] = return Set.empty
    captures (p : ps) = do
        ls <- liberties b p
        if Set.null ls
            then mappend <$> group b p <*> captures ps
            else captures ps

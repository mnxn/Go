module Logic (
    neighbors,
    group,
    liberties,
) where

import Control.Monad (filterM)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

import Board (Board, Position)
import Board qualified
import Data.Foldable (foldrM)

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
    liberties' pos acc = mappend acc . Set.fromList <$> filterM isEmpty (neighbors b pos)

    isEmpty :: Position -> IO Bool
    isEmpty = fmap (== Board.Empty) . Board.get b

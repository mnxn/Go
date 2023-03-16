module Logic (
    neighbors,
    liberties,
    group,
) where

import Control.Monad (filterM)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

import Board (Board, Position)
import Board qualified

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

liberties :: Board -> Position -> IO [Position]
liberties b pos = filterM isEmpty (neighbors b pos)
  where
    isEmpty :: Position -> IO Bool
    isEmpty = fmap (== Board.Empty) . Board.get b

type Group = Set Board.Position

group :: Board -> Position -> IO Group
group b start = do
    target <- Board.get b start
    Set.insert start <$> group' target Set.empty start
  where
    group' :: Board.Piece -> Group -> Position -> IO Group
    group' target set pos
        | Set.member pos set = return set
        | otherwise = do
            piece <- Board.get b pos
            if piece == target
                then mconcat <$> mapM (group' target (Set.insert pos set)) (neighbors b pos)
                else return Set.empty
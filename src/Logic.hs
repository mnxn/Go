module Logic (
    neighbors,
    liberties,
) where

import Board (Board, Position)
import Board qualified

import Control.Monad (filterM)
import Data.Maybe (mapMaybe)

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

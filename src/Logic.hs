module Logic (
    neighbors,
) where

import Board (Board, Position)
import Board qualified

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

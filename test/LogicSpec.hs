module LogicSpec (spec) where

import Data.Maybe (fromJust)

import Test.Hspec

import Board (Piece (..), Player (..))
import Board qualified
import Logic qualified

black, white :: Piece
black = Piece Black
white = Piece White

spec :: Spec
spec = do
    describe "Logic.neighbors" $ do
        let boardIO =
                Board.fromList
                    3
                    [ [Empty, black, Empty]
                    , [black, black, Empty]
                    , [white, white, white]
                    ]

        it "returns 2 neighbors in the corner" $ do
            b <- boardIO
            let pos = fromJust $ Board.position b (0, 0)
                neighbors = Logic.neighbors b pos
            pieces <- mapM (Board.get b) neighbors
            pieces `shouldBe` [black, black]

        it "returns 3 neighbors on the side" $ do
            b <- boardIO
            let pos = fromJust $ Board.position b (2, 1)
                neighbors = Logic.neighbors b pos
            pieces <- mapM (Board.get b) neighbors
            pieces `shouldBe` [black, white, white]

        it "returns 4 neighbors in the center" $ do
            b <- boardIO
            let pos = fromJust $ Board.position b (1, 1)
                neighbors = Logic.neighbors b pos
            pieces <- mapM (Board.get b) neighbors
            pieces `shouldBe` [black, Empty, white, black]

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
    describe "neighbors" $ do
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

    describe "liberties" $ do
        let boardIO =
                Board.fromList
                    4
                    [ [black, black, black, Empty]
                    , [Empty, black, Empty, black]
                    , [white, Empty, white, Empty]
                    , [Empty, white, Empty, white]
                    ]

        it "returns 0 positions when surrounded" $ do
            b <- boardIO
            let pos = fromJust $ Board.position b (1, 0)
            Logic.liberties b pos >>= (`shouldBe` [])

        it "returns between 1 and 4 positions when not completely surrounded" $ do
            b <- boardIO
            let positionList = fmap (fromJust . Board.position b)
                p00 = fromJust $ Board.position b (0, 0)
                p02 = fromJust $ Board.position b (0, 2)
                p20 = fromJust $ Board.position b (2, 0)
                p22 = fromJust $ Board.position b (2, 2)
            Logic.liberties b p00 >>= (`shouldBe` positionList [(1, 0)])
            Logic.liberties b p02 >>= (`shouldBe` positionList [(0, 3), (1, 2)])
            Logic.liberties b p20 >>= (`shouldBe` positionList [(1, 0), (2, 1), (3, 0)])
            Logic.liberties b p22 >>= (`shouldBe` positionList [(1, 2), (2, 3), (3, 2), (2, 1)])

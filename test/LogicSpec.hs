module LogicSpec (spec) where

import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set

import Test.Hspec

import Board (Board, Piece (..), Player (..))
import Board qualified
import Logic qualified

black, white :: Piece
black = Piece Black
white = Piece White

positionSet :: Board -> [(Int, Int)] -> Set Board.Position
positionSet b = Set.fromList . fmap (fromJust . Board.position b)

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

    describe "group" $ do
        let boardIO =
                Board.fromList
                    4
                    [ [black, black, black, Empty]
                    , [Empty, black, Empty, black]
                    , [white, Empty, white, Empty]
                    , [Empty, white, Empty, white]
                    ]

        it "returns a group of a single position" $ do
            b <- boardIO
            let pos = fromJust $ Board.position b (2, 2)
            Logic.group b pos >>= (`shouldBe` positionSet b [(2, 2)])

        it "returns a group of multiple positions" $ do
            b <- boardIO
            let pos = fromJust $ Board.position b (0, 1)
            Logic.group b pos >>= (`shouldBe` positionSet b [(0, 0), (0, 1), (0, 2), (1, 1)])

    describe "liberties" $ do
        it "returns a set of no liberties when the board is full" $ do
            b <-
                Board.fromList
                    2
                    [ [black, black]
                    , [black, black]
                    ]
            let pos = fromJust $ Board.position b (0, 0)
            Logic.liberties b pos >>= (`shouldBe` Set.empty)

        it "returns a set of no liberties when surrounded" $ do
            b <-
                Board.fromList
                    3
                    [ [Empty, white, Empty]
                    , [white, black, white]
                    , [Empty, white, Empty]
                    ]
            let pos = fromJust $ Board.position b (1, 1)
            Logic.liberties b pos >>= (`shouldBe` Set.empty)

        it "returns a set of multiple liberties" $ do
            b <-
                Board.fromList
                    4
                    [ [black, black, black, Empty]
                    , [Empty, black, Empty, black]
                    , [white, Empty, white, Empty]
                    , [Empty, white, Empty, white]
                    ]
            let pos = fromJust $ Board.position b (0, 1)
            Logic.liberties b pos >>= (`shouldBe` positionSet b [(1, 0), (2, 1), (1, 2), (0, 3)])

module LogicSpec (spec) where

import Control.Monad.Except
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

groupSet :: Board -> [(Int, Int)] -> Logic.Group
groupSet b = Logic.Group . positionSet b

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
            Logic.group b pos >>= (`shouldBe` groupSet b [(2, 2)])

        it "returns a group of multiple positions" $ do
            b <- boardIO
            let pos = fromJust $ Board.position b (0, 1)
            Logic.group b pos >>= (`shouldBe` groupSet b [(0, 0), (0, 1), (0, 2), (1, 1)])

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

    describe "play" $ do
        it "returns an empty set if no groups would be captured" $ do
            b <-
                Board.fromList
                    2
                    [ [Empty, Empty]
                    , [Empty, Empty]
                    ]
            let pos = fromJust $ Board.position b (0, 0)
            runExceptT (Logic.play b pos Black) >>= (`shouldBe` Right Set.empty)

        it "returns a capture set if an opposite group is captured" $ do
            b <-
                Board.fromList
                    3
                    [ [Empty, Empty, Empty]
                    , [black, white, black]
                    , [black, black, black]
                    ]
            let pos = fromJust $ Board.position b (0, 1)
            runExceptT (Logic.play b pos Black) >>= (`shouldBe` Right (positionSet b [(1, 1)]))

        it "returns a capture set if an opposite group is captured to avoid a self-capture" $ do
            b <-
                Board.fromList
                    3
                    [ [black, Empty, black]
                    , [black, white, black]
                    , [black, black, black]
                    ]
            let pos = fromJust $ Board.position b (0, 1)
            runExceptT (Logic.play b pos Black) >>= (`shouldBe` Right (positionSet b [(1, 1)]))

        it "returns a capture set if multiple enemy groups are captured" $ do
            b <-
                Board.fromList
                    3
                    [ [black, white, black]
                    , [white, Empty, white]
                    , [black, white, black]
                    ]
            let pos = fromJust $ Board.position b (1, 1)
            runExceptT (Logic.play b pos Black) >>= (`shouldBe` Right (positionSet b [(0, 1), (1, 0), (2, 1), (1, 2)]))

        it "returns a capture set if a loop enemy group is captured" $ do
            b <-
                Board.fromList
                    3
                    [ [Empty, white, white]
                    , [white, black, white]
                    , [white, white, white]
                    ]
            let pos = fromJust $ Board.position b (0, 0)
            runExceptT (Logic.play b pos Black) >>= (`shouldBe` Right (positionSet b [(0, 1), (0, 2), (1, 2), (2, 2), (2, 1), (2, 0), (1, 0)]))

        it "returns a PositionTaken error if there is already a piece" $ do
            b <-
                Board.fromList
                    2
                    [ [black, black]
                    , [black, black]
                    ]
            let pos = fromJust $ Board.position b (0, 0)
            runExceptT (Logic.play b pos Black) >>= (`shouldBe` Left Logic.PositionTaken)

        it "returns a SelfCapture error if own group would be captured" $ do
            b <-
                Board.fromList
                    2
                    [ [Empty, black, white]
                    , [black, black, white]
                    , [white, white, white]
                    ]
            let pos = fromJust $ Board.position b (0, 0)
            runExceptT (Logic.play b pos Black) >>= (`shouldBe` Left Logic.SelfCapture)

        it "returns a SelfCapture error if opposite group would not be captured" $ do
            b <-
                Board.fromList
                    2
                    [ [Empty, white]
                    , [white, Empty]
                    ]
            let pos = fromJust $ Board.position b (0, 0)
            runExceptT (Logic.play b pos Black) >>= (`shouldBe` Left Logic.SelfCapture)

module BoardSpec (spec) where

import Control.Monad (forM_)
import Data.Maybe (fromJust)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Board (Piece (..), Player (..))
import Board qualified

black, white :: Piece
black = Piece Black
white = Piece White

spec :: Spec
spec = do
    describe "make w" $ do
        prop "returns a board of width w" $ \w -> do
            b <- Board.make w
            Board.width b `shouldBe` w

    describe "position" $ do
        prop "returns Nothing when a position is outside the board width" $ \w -> do
            b <- Board.make w
            Board.position b (w, w) `shouldBe` Nothing

        prop "returns Just when a position is within the width" $ \(Positive w) -> do
            b <- Board.make w
            Board.position b (w - 1, w - 1) `shouldNotBe` Nothing

    describe "positions" $ do
        it "returns all positions in the board" $ do
            b <- Board.make 2
            let ps = fmap (fromJust . Board.position b) [(0, 0), (0, 1), (1, 0), (1, 1)]
            Board.positions b `shouldBe` ps

        prop "returns width^2 positions" $ \(NonNegative w) -> do
            b <- Board.make w
            length (Board.positions b) `shouldBe` (w * w)

    describe "get" $ do
        it "returns the piece at the given position" $ do
            b <-
                Board.fromList
                    3
                    [ [Empty, Empty, Empty]
                    , [Empty, black, Empty]
                    , [Empty, Empty, Empty]
                    ]
            let pos = fromJust $ Board.position b (1, 1)
            Board.get b pos >>= (`shouldBe` black)

    describe "set" $ do
        it "set the piece at the given position" $ do
            b <- Board.make 3
            let pos = fromJust $ Board.position b (1, 1)
            Board.set b pos black
            Board.get b pos >>= (`shouldBe` black)

    describe "remove" $ do
        it "removes the piece at the given position" $ do
            b <-
                Board.fromList
                    3
                    [ [Empty, Empty, Empty]
                    , [Empty, black, Empty]
                    , [Empty, Empty, Empty]
                    ]
            let pos = fromJust $ Board.position b (1, 1)
            Board.remove b pos
            Board.get b pos >>= (`shouldBe` Empty)

    describe "equals" $ do
        it "returns True for two empty lists" $ do
            l <- Board.make 0
            r <- Board.make 0
            Board.equals l r >>= (`shouldBe` True)

        it "returns True for two lists with the same elements" $ do
            l <-
                Board.fromList
                    3
                    [ [Empty, black, white]
                    , [black, white, Empty]
                    , [white, Empty, black]
                    ]
            r <- Board.make 3
            forM_ (Board.positions l) $ \pos -> do
                lpiece <- Board.get l pos
                Board.set r pos lpiece
            Board.equals l r >>= (`shouldBe` True)

        prop "returns True for the same list" $ \(NonNegative w) -> do
            b <- Board.make w
            Board.equals b b >>= (`shouldBe` True)

        prop "returns False for boards of different lengths" $ \(NonNegative w) -> do
            l <- Board.make w
            r <- Board.make (w + 1)
            Board.equals l r >>= (`shouldBe` False)

        prop "returns False for boards with different elements" $ \(Positive w) -> do
            l <- Board.make w
            r <- Board.make w
            let pos = fromJust $ Board.position r (0, 0)
            Board.set r pos white
            Board.equals l r >>= (`shouldBe` False)

    describe "fromList" $ do
        it "returns an empty board given an empty list" $ do
            l <- Board.fromList 0 []
            r <- Board.make 0
            Board.equals l r >>= (`shouldBe` True)

        it "returns a board with no excess elements" $ do
            let width = 2
            l <-
                Board.fromList
                    width
                    [ [black, white, black]
                    , [white, Empty, white]
                    , [black, white, black]
                    ]
            r <-
                Board.fromList
                    width
                    [ [black, white]
                    , [white, Empty]
                    ]
            Board.equals l r >>= (`shouldBe` True)

        it "returns a board that replaces missing elements with Empty" $ do
            let width = 3
            l <-
                Board.fromList
                    width
                    [ [white, black]
                    , [black, white]
                    ]
            r <-
                Board.fromList
                    width
                    [ [white, black, Empty]
                    , [black, white, Empty]
                    , [Empty, Empty, Empty]
                    ]
            Board.equals l r >>= (`shouldBe` True)

        it "returns a board with the correct elements set" $ do
            b <-
                Board.fromList
                    2
                    [ [white, black]
                    , [black, white]
                    ]
            let coordinates = [(0, 0), (0, 1), (1, 0), (1, 1)]
            pieces <- mapM (Board.get b . fromJust . Board.position b) coordinates
            pieces `shouldBe` [white, black, black, white]

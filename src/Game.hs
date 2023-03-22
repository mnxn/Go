module Game (GameState, makeState, runGame) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Char (isAsciiUpper, ord)
import Data.Set qualified as Set
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Board (Board, Player (Black, White))
import Board qualified
import Display (Params)
import Display qualified
import Logic qualified

data GameState = GameState
    { board :: Board
    , current :: Player
    , passCount :: Int
    , displayParams :: Params GameM
    }

makeState :: Int -> IO GameState
makeState width = do
    board <- Board.make width
    return $
        GameState
            { board
            , current = Black
            , passCount = 0
            , displayParams = Display.ansi
            }

data GameError
    = InvalidPosition
    | PlayerPassed
    | LogicError Logic.LogicError

type GameM = ExceptT GameError (StateT GameState IO)

runGame :: GameState -> IO ()
runGame gs = do
    result <- runStateT (runExceptT loop) gs
    case result of
        (Left InvalidPosition, gs') -> do
            putStrLn "Error: invalid position"
            runGame gs'
        (Left (LogicError Logic.PositionTaken), gs') -> do
            putStrLn "Invalid move: position is already taken"
            runGame gs'
        (Left (LogicError Logic.SelfCapture), gs') -> do
            putStrLn "Invalid move: self capture"
            runGame gs'
        (Left PlayerPassed, GameState{board, passCount = 2}) -> do
            putStrLn "Game end: both players passed"
            blackScore <- Board.count board Black
            whiteScore <- Board.count board White
            printf "\nStone scoring:\n  Black: %d\n  White: %d\n" blackScore whiteScore
            putStrLn $ case compare blackScore whiteScore of
                GT -> "  Black wins!"
                LT -> "  White wins!"
                EQ -> "  Tie."
        (_, gs'@GameState{current}) ->
            runGame gs'{current = Board.opposite current}

loop :: GameM ()
loop = do
    gs@GameState{board, current, displayParams} <- get

    Display.newLine
    Board.display displayParams board
    Display.newLine

    pos <- askTurn
    put gs{passCount = 0}

    result <- runExceptT $ Logic.play board pos current
    case result of
        Left e -> throwError (LogicError e)
        Right captures -> Set.foldr (\p io -> io >> Board.remove board p) (return ()) captures

    Display.clear displayParams

askTurn :: GameM Board.Position
askTurn = do
    gs@GameState{board, current, passCount} <- get
    liftIO $ putStr (show current ++ "'s turn: ")
    liftIO $ hFlush stdout
    line <- liftIO getLine
    when (line == "pass") $ do
        put gs{passCount = passCount + 1}
        throwError PlayerPassed
    case readPosition board line of
        Just p -> return p
        Nothing -> throwError InvalidPosition

readPosition :: Board -> String -> Maybe Board.Position
readPosition _ [] = Nothing
readPosition b (rowChar : columnString) = do
    guard $ isAsciiUpper rowChar
    let row = ord rowChar - ord 'A'
    column :: Int <- readMaybe columnString
    guard $ column >= 1
    Board.position b (column - 1, row)

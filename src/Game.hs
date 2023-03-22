module Game (GameState, makeState, runGame) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Char (isAsciiUpper, ord)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import Board (Board, Player (Black))
import Board qualified

data GameState = GameState
    { board :: Board
    , current :: Player
    , passCount :: Int
    }

makeState :: Int -> IO GameState
makeState width = do
    board <- Board.make width
    return $ GameState{board, current = Black, passCount = 0}

data GameError = InvalidPosition | PositionTaken | PlayerPassed

type GameM = ExceptT GameError (StateT GameState IO)

runGame :: GameState -> IO ()
runGame gs = do
    result <- runStateT (runExceptT loop) gs
    case result of
        (Left InvalidPosition, gs') -> do
            putStrLn "Error: invalid position"
            runGame gs'
        (Left PositionTaken, gs') -> do
            putStrLn "Error: position is already taken"
            runGame gs'
        (Left PlayerPassed, GameState{passCount = 2}) ->
            putStrLn "Game end: both players passed"
        (_, gs'@GameState{current}) ->
            runGame gs'{current = Board.opposite current}

loop :: GameM ()
loop = do
    gs@GameState{board, current} <- get

    liftIO $ putStrLn ""
    Board.display board
    liftIO $ putStrLn ""

    pos <- askTurn
    put gs{passCount = 0}

    posPiece <- Board.get board pos
    case posPiece of
        Board.Empty -> Board.set board pos (Board.Piece current)
        Board.Piece _ -> throwError PositionTaken

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

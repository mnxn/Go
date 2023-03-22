# Go

Implementation of the Go board game in Haskell.

```
Usage:
  --ascii
        use ascii output
  --ansi
        use ansi output
  --help
        show help message
```

-   Start the program, optionally passing a display mode on the command line (`--ascii` or `--ansi`)
-   Enter the width of the board as an integer between 2 and 26
-   Each turn, a player will be asked to enter a position to place their piece
    -   A position is in the format `A1` where letters are the columns and numbers are the rows
    -   Players may also enter `pass` to skip their turn
-   After both players pass in a row, the game ends
-   Each player's score is calculated using the stone scoring method
-   The player with the highest score is the winner

# Implementation

## Display

```haskell
data Params io

newLine :: MonadIO io => io ()
ascii   :: MonadIO io => Params io
ansi    :: MonadIO io => Params io
```

## Board

```haskell
data Player   = Black | White
data Piece    = Empty | Piece Player
data Position = Position { row :: Int, column :: Int }
data Board

opposite  :: Player -> Player
make      :: MonadIO io => Int -> io Board
position  :: Board -> (Int, Int) -> Maybe Position
positions :: Board -> [Position]
get       :: MonadIO io => Board -> Position -> io Piece
set       :: MonadIO io => Board -> Position -> Piece -> io ()
remove    :: MonadIO io => Board -> Position -> io ()
equals    :: MonadIO io => Board -> Board -> io Bool
count     :: Board -> Player -> IO Int
fromList  :: MonadIO io => Int -> [[Piece]] -> io Board
display   :: MonadIO io => Display.Params io -> Board -> io ()
```

## Logic

```haskell
data LogicError = PositionTaken | SelfCapture
newtype Group   = Group {getGroupSet :: Set Position}

neighbors :: Board -> Position -> [Position]
group     :: MonadIO io => Board -> Position -> io Group
liberties :: MonadIO io => Board -> Position -> io (Set Position)
play      :: MonadIO io => Board -> Position -> Board.Player -> ExceptT LogicError io (Set Position)
```

## Game

```haskell
data GameState = GameState
    { board         :: Board
    , current       :: Player
    , passCount     :: Int
    , displayParams :: Params GameM
    }
data GameError = InvalidPosition | PlayerPassed | LogicError Logic.LogicError
type GameM     = ExceptT GameError (StateT GameState IO)

runGame  :: Bool -> IO ()
runGameM :: GameState -> IO ()
```

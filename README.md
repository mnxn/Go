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
ascii   :: MonadIO io => Params io
ansi    :: MonadIO io => Params io

newLine :: MonadIO io => io ()
char    :: MonadIO io => Char -> io ()
string  :: MonadIO io => String -> io ()
```

The Display module contains the Display.Params type, which holds the specific ways to display various parts of the game
board. Every function is parameterized over the `MonadIO` to make them usable in any Monad transformers that have IO.

-   The `ascii` value uses normal ASCII characters without any special terminal features.

-   The `ansi` value uses Unicode characters and the functions from the `ansi-terminal` library for terminal clearing
    and colors.

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

The core of the game is represented in the `Board` type. Under the hood, the implementation is a flat `IOVector` from
the `vector` library. Each element of the board is `Empty` or a `Piece` for one of two players (`Black` or `White`).

Board must have a width to be created through either `make` or `fromList`. The other functions use the board's width to
perform calculations.

-   The `equals` function takes two boards and only returns true if the width and all elements are equal.
-   The `count` function is implemented using the vector `foldr` higher-order function and returns the number of pieces
    of a giver player.
-   The `display` function iterates over the entire vector and uses the functions from the `Display` module to print a
    representation of the board to the terminal.

Positions are represented as a record with the row and column indices. When accessing or modifying the vector, the
2-dimensional position is converted into a 1-dimensional index into the vector.

-   The `position` smart constructor takes a board and 2-dimensional coordinate as parameters and only returns `Just` of
    a position if the position is valid for the board.
-   The `positions` function returns a list of all valid positions within the board's width.

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

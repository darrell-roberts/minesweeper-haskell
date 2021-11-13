{-# LANGUAGE TemplateHaskell #-}

module MineSweeperData (
  -- * Types
  Board,
  Cell (..),
  CellState (..),
  GameEnv (..),
  Pos,
  GameState (..),
  GameStatus (..),
  Difficulty (..),
  AdjacentCount,
  HasDigit (..),

  -- * Board lenses and prisms
  coveredLens,
  coveredFlaggedLens,
  coveredMinedLens,
  unCoveredLens,
  xCoordLens,
  yCoordLens,

  -- * Cell generated lens
  pos,
  state,
  cellId,
  adjacentMines,

  -- * CellState generated lens
  mined,
  flagged,
  _UnCovered,
  _Covered,
) where

import Control.Lens (
  Lens',
  Traversal',
  makeLenses,
  makePrisms,
  _1,
  _2,
  (^?),
 )

import Data.Word (Word8)
import qualified Data.Sequence as Seq
import Data.Bool (bool)

-- | Position of a cell in x y coordinates.
type Pos = (Int, Int)

-- | Board is a Sequence of cells.
type Board = Seq.Seq Cell

-- | State of a cell.
data CellState
  = -- | Covered cell. Either flagged or closed.
    Covered {_mined :: !Bool, _flagged :: !Bool}
  | -- | Uncovered cell.
    UnCovered {_mined :: !Bool}
  deriving (Show, Eq)

-- | A Cell in the Minesweeper board.
data Cell = Cell
  { -- | Cell coordinates.
    _pos :: !Pos
  , -- | Cell state.
    _state :: !CellState
  , -- | Cell unique identifier.
    _cellId :: !Int
  , -- | Number of adjacent mines.
    _adjacentMines :: !AdjacentCount
  }
  -- deriving (Show)

-- | A constrained unsigned integer from 0 to 8.
newtype AdjacentCount = AC Word8 deriving (Show, Eq, Ord)

-- | Smart constructor for AdjacentCount type to enforce bounds.
mkAdjacentCount :: Word8 -> AdjacentCount
mkAdjacentCount n | n >= 0 && n <= 8 = AC n
                  | otherwise = error $
                      show "adjacent count of " <> show n <> " is out of range."

instance Num AdjacentCount where
  AC a + AC b = mkAdjacentCount $ a + b
  AC a - AC b = mkAdjacentCount $ a - b
  AC a * AC b = mkAdjacentCount $ a * b
  fromInteger = mkAdjacentCount . fromIntegral
  abs (AC n) = mkAdjacentCount $ abs n
  signum (AC n) = mkAdjacentCount $ signum n

instance Semigroup AdjacentCount where
  c1 <> c2 = c1 + c2

instance Monoid AdjacentCount where
  mempty = AC 0

instance Bounded AdjacentCount where
  minBound = AC 0
  maxBound = AC 8

instance Enum AdjacentCount where
  succ (AC n) = mkAdjacentCount $ succ n
  pred (AC n) = mkAdjacentCount $ pred n
  enumFrom c = [c..maxBound]
  fromEnum (AC n) = fromEnum n
  toEnum n = mkAdjacentCount $ toEnum n

class HasDigit a where
  toChar :: a -> Char

instance HasDigit AdjacentCount where
  toChar (AC n) = head $ show n

instance Eq Cell where
  a == b = _cellId a == _cellId b

instance Ord Cell where
  compare a b = compare (_cellId a) (_cellId b)

newtype Difficulty = Difficulty (Float, Float)

-- | Game environment. Holds the board dimentions.
data GameEnv = GameEnv
  { -- | Total rows.
    rows :: Int
  , -- | Total columns.
    columns :: Int
  , -- | Difficulty settings.
    difficulty :: Difficulty
  }

-- | Game status.
data GameStatus
  = -- | Game Win.
    Win
  | -- | Game Lose.
    Loss
  | -- | Game Active.
    Active

-- | Game state.
data GameState = GameState
  { -- | Game board.
    board :: Board
  , -- | Game status.
    status :: GameStatus
  , -- | Total mines.
    totalMines :: Int
  , -- | Total covered cells.
    totalCovered :: Int
  , -- | Total flagged cells.
    totalFlagged :: Int
  }

makePrisms ''CellState
makeLenses ''CellState
makeLenses ''Cell

-- Re-useable lens.

-- | Focus on all covered cells.
coveredLens :: Traversal' Cell (Bool, Bool)
coveredLens = state . _Covered

coveredMinedLens, coveredFlaggedLens, unCoveredLens :: Traversal' Cell Bool

-- | Focus on covered and mined cells.
coveredMinedLens = coveredLens . _1

-- | Focus on covered and flagged cells.
coveredFlaggedLens = coveredLens . _2

-- | Focus on all opened cells.
unCoveredLens = state . _UnCovered

-- | Focus on position coordinates.
xCoordLens, yCoordLens :: Lens' Cell Int
xCoordLens = pos . _1
yCoordLens = pos . _2

instance Show Cell where
  show c = "id: " <> show (_cellId c) <> " Pos: " <> show (_pos c) <> bool "" "\x1f4a3" (c ^? coveredMinedLens == Just True)

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
 )

-- | Position of a cell in x y coordinates.
type Pos = (Int, Int)

-- | Board is a list of cells.
type Board = [Cell]

-- | State of a cell.
data CellState
  = -- | Covered cell. Either flagged or closed.
    Covered {_mined :: Bool, _flagged :: Bool}
  | -- | Uncovered cell.
    UnCovered {_mined :: Bool}
  deriving (Show, Eq)

-- | A Cell in the Minesweeper board.
data Cell = Cell
  { -- | Cell coordinates.
    _pos :: Pos
  , -- | Cell state.
    _state :: CellState
  , -- | Cell unique identifier.
    _cellId :: Int
  , -- | Number of adjacent mines.
    _adjacentMines :: Int
  }
  deriving (Show)

instance Eq Cell where
  a == b = _cellId a == _cellId b

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
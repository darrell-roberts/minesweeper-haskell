module Main where

import Control.Lens (view, (&), (.~), (^.))
import Data.Bool (bool)
import Data.Foldable
import qualified Data.IntSet as Set
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as CSet
import MineSweeper hiding (openCell)
import MineSweeperData hiding (board)
import MineSweeperDisplay (drawGameBoard)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

type CellSet = CSet.Set Cell
type VisitedKeys = Set.IntSet

testBoard :: Board
testBoard = Seq.fromList ps
  where
    ps =
        zipWith
            (\i p -> Cell p (Covered False False) i mempty)
            [0 ..]
            [(x, y) | x <- [1 .. 15], y <- [1 .. 15]]

minedPositions :: [Pos]
minedPositions =
    [ (2, 5)
    , (3, 2)
    , (3, 5)
    , (2, 1)
    , (5, 5)
    , (11, 7)
    , (12, 11)
    , (8, 6)
    , (13, 9)
    , (11, 14)
    , (3, 12)
    ]

minedBoard :: Board
minedBoard =
    updateMineCount $
        fmap
            ( \c@Cell{_pos = p} ->
                bool c (c & coveredMinedLens .~ True) (p `elem` minedPositions)
            )
            testBoard

openCell :: Board -> Cell -> Board
openCell board cell =
    let updatedCells = fst $ collect board (Set.singleton $ cell ^. cellId) (getAdjacentCells board cell)
        openedCells = fmap openUnMined (CSet.toList updatedCells)
     in foldr updateCell board openedCells

collect :: Board -> VisitedKeys -> CellSet -> (CellSet, VisitedKeys)
collect board v = foldr fcheck (mempty, v)
  where
    fcheck c@Cell{_cellId = key} (freeCells, visited)
        | key `Set.member` visited = (freeCells, visited)
        | c^.adjacentMines == mempty =
            let allAdjacent = getAdjacentCells board c
                (nestedFreeCells, nestedVisited) = collect board (Set.insert key visited) allAdjacent
             in (allAdjacent <> freeCells <> nestedFreeCells, visited <> nestedVisited)
        | otherwise = (CSet.insert c freeCells, Set.insert key visited)

getAdjacentCells :: Board -> Cell -> CellSet
getAdjacentCells board = CSet.fromList . okToOpen . (`adjacentCells` board)

parseArgs :: [String] -> Pos
parseArgs (x : y : _) = fromMaybe (7, 7) p
  where
    p =
        (,) <$> (readMaybe x :: Maybe Int)
            <*> (readMaybe y :: Maybe Int)
parseArgs _ = (7, 7)

main :: IO ()
main = do
    p <- parseArgs <$> getArgs
    let cell = find ((== p) . view pos) minedBoard
    case cell of
        Just c -> do
            putStrLn $ "Opened cell " <> show c
            drawGameBoard $ openCell minedBoard c
        Nothing -> pure ()

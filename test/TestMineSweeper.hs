{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Lens (view, (&), (.~), (^.))
import Data.Bool (bool)
import Data.Foldable
import qualified Data.IntSet as IntSet
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Debug.Trace
import MineSweeper hiding (openCell)
import MineSweeperData hiding (board)
import MineSweeperDisplay (drawGameBoard)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

type CellSet = Set.Set Cell
type VisitedKeys = IntSet.IntSet

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
    [ (1, 5)
    , (3, 2)
    , (3, 3)
    , (1, 6)
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
    let updatedCells = fst $ collect board (IntSet.singleton $ cell ^. cellId) (getAdjacentCells board cell)
        openedCells = fmap openUnMined (Set.toList updatedCells)
     in foldr updateCell board openedCells

collect :: Board -> VisitedKeys -> CellSet -> (CellSet, VisitedKeys)
collect board !v !cs = trace ("adjacent: " <> show cs <> " visited: " <> show v <> "\n") foldr fcheck (mempty, v) cs
  where
    fcheck c@Cell{_cellId = key} (!freeCells, !visited)
        | key `IntSet.member` visited = (freeCells, visited)
        | c ^. adjacentMines == mempty =
            let allAdjacent = getAdjacentCells board c
                (nestedFreeCells, nestedVisited) = collect board (IntSet.insert key visited) allAdjacent
             in (allAdjacent <> freeCells <> nestedFreeCells, visited <> nestedVisited)
        | otherwise = (Set.insert c freeCells, IntSet.insert key visited)

getAdjacentCells :: Board -> Cell -> CellSet
getAdjacentCells board = Set.fromList . okToOpen . (`adjacentCells` board)

parseArgs :: [String] -> Pos
parseArgs (x : y : _) = fromMaybe (7, 7) p
  where
    p =
        (,) <$> (readMaybe x :: Maybe Int)
            <*> (readMaybe y :: Maybe Int)
parseArgs _ = (7, 7)

posToCell :: Pos -> Maybe Cell
posToCell p = find ((== p) . view pos) minedBoard

main :: IO ()
main = do
    p <- parseArgs <$> getArgs
    let cell = posToCell p
    case cell of
        Just c -> do
            putStrLn $ "Opened cell " <> show c
            drawGameBoard $ openCell minedBoard c
        Nothing -> pure ()

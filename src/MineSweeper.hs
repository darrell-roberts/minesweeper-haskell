{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

{- |
    Game Types and functions for a MineSweeper Game.
-}
module MineSweeper (
    -- * Game actions
    newGameBoard,
    mineBoard,

    -- * Game state
    updateBoardState,

    -- * User actions
    openCell,
    flagCell,
) where

import Control.Lens (
    anyOf,
    ix,
    filtered,
    folded,
    lengthOf,
    preview,
    to,
    view,
    (%~),
    (&),
    (.~),
    (^.),
    (^?),
    _1,
 )
import Control.Monad.Reader (MonadIO, MonadReader, ask, reader)
import Control.Monad.State (MonadState, gets, put)
import Data.List (delete, elemIndex, find, nub)
import Data.Maybe (isJust)
import MineSweeperData (
    Board,
    Cell (..),
    CellState (..),
    Difficulty (..),
    GameEnv (..),
    GameState (..),
    GameStatus (..),
    Pos,
    adjacentMines,
    cellId,
    coveredFlaggedLens,
    coveredLens,
    coveredMinedLens,
    flagged,
    mined,
    pos,
    state,
    unCoveredLens,
    _Covered,
 )
import System.Random (getStdGen, getStdRandom, randomR, randomRs)

{- |
    Update the game board in state with evaluated
    game status and statistics.
-}
updateBoardState ::
    (MonadState GameState m, MonadReader GameEnv m) =>
    -- | Board to be udpated in `GameState`.
    Board ->
    m ()
updateBoardState b =
    put $
        GameState
            { board = getBoard gameStatus b
            , status = gameStatus
            , totalMines = getTotalMines b
            , totalCovered = getTotalCovered b
            , totalFlagged = getTotalFlagged b
            }
  where
    gameStatus
        | isWin b && not (isUnMinedBoard b) = Win
        | isLoss b && not (isUnMinedBoard b) = Loss
        | otherwise = Active

    getBoard Loss = exposeMines
    getBoard _ = id

    getTotalMines, getTotalFlagged, getTotalCovered :: Board -> Int
    getTotalMines = lengthOf (traverse . coveredMinedLens . filtered (== True))
    getTotalFlagged = lengthOf (traverse . coveredFlaggedLens . filtered (== True))
    getTotalCovered = lengthOf (traverse . coveredLens)

-- | Create a new unmined board using Players settings.
newGameBoard ::
    (MonadReader GameEnv m, MonadState GameState m) =>
    m ()
newGameBoard = do
    GameEnv{rows, columns} <- ask
    let positions = (,) <$> [1 .. columns] <*> [1 .. rows]
    updateBoardState $
        ( \(n, p) ->
            Cell
                { _pos = p
                , _state = Covered False False
                , _adjacentMines = 0
                , _cellId = n
                }
        )
            <$> zip [1 ..] positions

-- | Replace cell in the board.
updateCell :: Cell -> Board -> Board
updateCell cell board =
    case elemIndex cell board of
        Just i -> board & ix i .~ cell
        Nothing -> board

-- | Update board with modified cells.
updateBoard :: Board -> [Cell] -> Board
updateBoard = foldr updateCell

-- | Filter out mined and flagged cells.
okToOpen :: [Cell] -> [Cell]
okToOpen = filter (\c -> c ^? coveredLens == Just (False, False))

-- | Update `CellState` to `Uncovered` for `Cell`
openUnMined :: Cell -> Cell
openUnMined = state .~ UnCovered False

{-- |
    User action requesting to open a cell with provided @x, y@ coordinates.
    If the cell is not mined or flagged then the cell will be opened. An
    unmined cell will also open adjacent cells and a mined cell will end
    the game.
-}
openCell ::
    (MonadReader GameEnv m, MonadState GameState m) =>
    -- | Cell position requested to opened.
    Pos ->
    m ()
openCell p = do
    b <- gets board
    n <- reader totalCells
    updateBoardState $ open b n (findCell b)
  where
    findCell = find (\c -> c ^. pos == p)
    open b n (Just c)
        | c ^? coveredFlaggedLens == Just True = b
        | c ^? coveredMinedLens == Just True =
            updateCell
                (c & state .~ UnCovered True)
                b
        | isCovered c =
            isFirstMove b n & \firstMove ->
                if c ^. adjacentMines == 0 && not firstMove
                    then updateCell (openUnMined c) $ expandEmptyCells b c
                    else updateCell (openUnMined c) b
        | otherwise = b
    open b _ Nothing = b
    isCovered = isJust . preview coveredLens

{- |
    Opens all adjacent cells recursively starting from the
    postition cell provided. Any cells that are either already
    opened, mined or flagged will not be opened.
-}
expandEmptyCells ::
    -- | Board to expand cells.
    Board ->
    -- | Cell position to expand from.
    Cell ->
    -- | Expanded Board.
    Board
expandEmptyCells board cell
    | null openedCells = board
    | otherwise = foldr (flip expandEmptyCells) updatedBoard (zeroAdjacent openedCells)
  where
    findMore _ [] = []
    findMore exclude (c : xs)
        | c `elem` exclude = findMore exclude xs
        | c ^. adjacentMines == 0 =
            c :
            adjacent c
                <> findMore (c : exclude <> adjacent c) xs
        | otherwise = c : findMore (c : exclude) xs
    adjacent = okToOpen . flip adjacentCells board
    openedCells = openUnMined <$> nub (findMore [cell] (adjacent cell))
    zeroAdjacent = filter (view (adjacentMines . to (== 0)))
    updatedBoard = updateBoard board openedCells

-- | User action to flag a cell.
flagCell ::
    (MonadReader GameEnv m, MonadState GameState m) =>
    -- | Cell position requestd to be flagged.
    Pos ->
    m ()
flagCell p = do
    board <- gets board
    case find ((== p) . view pos) board of
        Just c -> updateBoardState $ updateCell (c & state . flagged %~ not) board
        Nothing -> pure ()

-- | Find all adjacent cells for a given cell in the game board.
adjacentCells ::
    -- | Cell to search for adjacent siblings.
    Cell ->
    -- | Game board.
    Board ->
    -- | Adjacent cells.
    [Cell]
adjacentCells Cell{_pos = c@(x1, y1)} = filter (\c' -> c' ^. pos `elem` positions)
  where
    f n = [pred n, n, succ n]
    positions = delete c $ [(x, y) | x <- f x1, x > 0, y <- f y1, y > 0]

-- | Evaluate various Game states.
isLoss
    , isWin
    , allUnMinedOpen
    , allMinesFlagged
    , isUnMinedBoard ::
        -- | Game Board to evaluate.
        Board ->
        -- | Game State evaluation result.
        Bool
isLoss = anyOf (traverse . unCoveredLens) (== True)
isWin b = allUnMinedOpen b || allMinesFlagged b
allUnMinedOpen = (== 0) . lengthOf (traverse . coveredMinedLens . filtered (== False))
allMinesFlagged b =
    let flaggedMineCount = lengthOf (traverse . coveredLens . filtered (== (True, True)))
     in minedCount b == flaggedMineCount b
isUnMinedBoard = (== 0) . minedCount

-- | Total number of mined cells.
minedCount :: Board -> Int
minedCount = lengthOf (traverse . coveredMinedLens . filtered (== True))

-- | Determine if this is the first move to be made on the board.
isFirstMove :: Board -> Int -> Bool
isFirstMove b n =
    (== n) $ lengthOf (folded . coveredFlaggedLens . filtered (== False)) b

-- | Expose all mined cells on the board.
exposeMines :: Board -> Board
exposeMines = fmap (\c -> c & state . filtered (\s -> s ^? _Covered . _1 == Just True) .~ UnCovered True)

updateMineCount :: Board -> Board
updateMineCount b = go b
  where
    go [] = []
    go (x : xs) = (x & adjacentMines .~ totalAdjacentMines b) : go xs
      where
        totalAdjacentMines =
            foldr (\c acc -> if c ^. state . mined then succ acc else acc) 0 . adjacentCells x

{- |
    Take a new board and randomly mine it based on
    the players settings for difficulty. The player
    would have opened one cell and we make sure we don't
    mine directly adjacent to that cell.
-}
mineBoard ::
    (MonadReader GameEnv m, MonadIO m, MonadState GameState m) =>
    -- | Position where user first opened a cell.
    Pos ->
    m ()
mineBoard p = do
    board <- gets board
    totalMines <- randomMinedCount
    b <- minedBoard totalMines board
    updateBoardState $ updateMineCount b
  where
    mines n = take n <$> randomCellIds
    minedBoard n board =
        ( \m ->
            fmap
                ( \c ->
                    if c ^. cellId `elem` m
                        then c & state . mined .~ True
                        else c
                )
                board
        )
            . filter (\c -> openedCell board ^. cellId /= c)
            <$> mines n
    openedCell board = head $ filter (\c -> c ^. pos == p) board

{- |
    Count the number of cells the Game Board
    will have based on the Players selection.
-}
totalCells :: GameEnv -> Int
totalCells = (*) <$> rows <*> columns

-- | Randomly select cell ids from 1 to total board size.
randomCellIds :: (MonadReader GameEnv m, MonadIO m) => m [Int]
randomCellIds = reader totalCells >>= \n -> randomRs (1, n) <$> getStdGen

-- | Randomly select total mined cells based on Player difficulty settings.
randomMinedCount ::
    (MonadReader GameEnv m, MonadIO m) =>
    -- | Total mined cells.
    m Int
randomMinedCount = do
    n <- reader totalCells
    Difficulty (maxMinedPercent, minMinedPercent) <- reader difficulty
    let maxMinedCells = floor $ realToFrac n * maxMinedPercent
        minMinedCells = floor $ realToFrac n * minMinedPercent
    getStdRandom $ randomR (minMinedCells, maxMinedCells)
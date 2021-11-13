{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

{- |
    Module for game display.
-}
module MineSweeperDisplay (
    displayCell,
    drawBoard,
    drawGameBoard
) where

import Control.Lens (preview, to, view, (^.), (^?))
import Control.Monad.Reader (MonadIO, liftIO)
import Control.Monad.State (MonadState, get)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (groupBy)
import qualified Data.Sequence as Seq
import MineSweeperData (
    Board,
    Cell,
    GameState (..),
    -- HasDigit(toChar),
    adjacentMines,
    coveredFlaggedLens,
    coveredMinedLens,
    pos,
    unCoveredLens,
    xCoordLens,
    yCoordLens,
 )
import Text.Printf (printf)

-- | Group board by rows
groupedByRows :: Board -> [[Cell]]
groupedByRows =
    let yAxis = view yCoordLens
     in groupBy ((==) `on` yAxis) . toList . Seq.sortBy (compare `on` yAxis)

-- | Cell character rendering.
displayCell :: Cell -> String
displayCell c
    | c ^? unCoveredLens == Just True = "\x1f4a3" -- 'X'
    | c ^? coveredFlaggedLens == Just True = "\x1f3f4" -- "?"
    | c ^? (unCoveredLens . to not) == Just True =
        if c ^. adjacentMines /= mempty
            then "   " <> show (c^.adjacentMines) -- toChar $ c ^. adjacentMines
            else "\x1f431" -- '▢'
    | otherwise = "\x26aa" -- '.'

-- | Display mined cell coordinates for cheating.
cheat :: Board -> String
cheat = show . fmap (view pos) . Seq.filter ((== Just True) . preview coveredMinedLens)

rows :: Board -> [[Cell]]
rows = groupedByRows

drawGameBoard :: Board -> IO ()
drawGameBoard board = do
    printf "%3s" ""
    mapM_ (printf " %3d") $ view xCoordLens <$> head (rows board)
    printf "\n\n"
    mapM_
        ( \row -> do
            printf "%3d" $ yCoord row
            mapM_ (printf "%3s" . displayCell) row
            printf "\n\n"
        )
        $ rows board
    where
        yCoord = view yCoordLens . head

-- | Draw the Game Board to the console.
drawBoard :: (MonadState GameState m, MonadIO m) => m ()
drawBoard =
    get >>= \GameState{totalMines, totalCovered, totalFlagged, board} ->
        liftIO $ do
            -- print board
            -- printf "Cheat: %s\n" $ cheat board
            printf $
                unlines
                    [ "  Actions:"
                    , "    Open Cell: o x y"
                    , "    Flag Cell: f x y"
                    , "\n"
                    ]

            printf
                "  Mines: %d Covered: %d Flagged: %d\n\n"
                totalMines
                totalCovered
                totalFlagged
            drawGameBoard board


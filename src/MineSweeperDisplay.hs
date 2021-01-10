{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

{- |
    Module for game display.
-}
module MineSweeperDisplay (
    displayCell,
    drawBoard,
) where

import Control.Lens (preview, to, view, (^.), (^?))
import Control.Monad.Reader (MonadIO, liftIO)
import Control.Monad.State (MonadState, get)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import MineSweeperData (
    Board,
    Cell,
    GameState (..),
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
groupedByRows :: Board -> [Board]
groupedByRows =
    let yAxis = view yCoordLens
     in groupBy ((==) `on` yAxis) . sortBy (compare `on` yAxis)

-- | Cell character rendering.
displayCell :: Cell -> String
displayCell c
    | c ^? unCoveredLens == Just True = "X"
    | c ^? coveredFlaggedLens == Just True = "?"
    | c ^? (unCoveredLens . to not) == Just True =
        if c ^. adjacentMines > 0 then show $ c ^. adjacentMines else "▢"
    | otherwise = "."

-- | Display mined cell coordinates for cheating.
cheat :: Board -> String
cheat = show . fmap (view pos) . filter ((== Just True) . preview coveredMinedLens)

-- | Draw the Game Board to the console.
drawBoard :: (MonadState GameState m, MonadIO m) => m ()
drawBoard =
    get >>= \GameState{totalMines, totalCovered, totalFlagged, board} ->
        liftIO $ do
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
            printf "%3s" ""
            mapM_ (printf "%3d") $ view xCoordLens <$> head (rows board)
            printf "\n"
            mapM_
                ( \row -> do
                    printf "%3d" $ yCoord row
                    mapM_ (printf "%3s" . displayCell) row
                    printf "\n"
                )
                $ rows board
  where
    rows b = groupedByRows b
    yCoord = view yCoordLens . head

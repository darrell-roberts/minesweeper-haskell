module Main where

import Control.Monad        (guard)
import Control.Monad.Reader (MonadIO, MonadReader, liftIO, reader, runReaderT)
import Control.Monad.State  (MonadState, get, runStateT)
import Data.Maybe           (fromMaybe)
import MineSweeper          (flagCell, mineBoard, newGameBoard, openCell)
import MineSweeperData      (Difficulty (..), GameEnv (..), GameState (..),
                             GameStatus (..), Pos)
import MineSweeperDisplay   (drawBoard)
import System.Environment   (getArgs)
import System.IO            (BufferMode (..), hSetBuffering, stdout)
import Text.Printf          (printf)
import Text.Read            (readMaybe)

-- * Types

-- | Type for Player Actions
data Command
  -- | Open command with position.
  = Open Pos
  -- | Flag command with position.
  | Flag Pos
  -- | Invalid command for unknown player input.
  | Invalid String

-- * Game functions

-- | Parse player input into a Player Action `Command`.
parseInput ∷
  MonadReader GameEnv m ⇒
  -- | Player input.
  String →
  -- | Game command.
  m Command
parseInput s
  | length input /= 3 = pure $ Invalid s
  | otherwise = reader (maybe (Invalid s) command . parsedPos)
  where
    input = words s
    parsedPos (GameEnv r c _) = do
      x <- readMaybe (input !! 1)
      y <- readMaybe (input !! 2)
      guard (x <= c && y <= r)
      pure (x, y)
    command p = case head input of
      "o" -> Open p
      "f" -> Flag p
      _   -> Invalid s

-- |
--    Game loop that contiually requets Player commands and
--    draws the updated game board after each valid command
--    until the game completes.
gameLoop ∷
  (MonadReader GameEnv m, MonadIO m, MonadState GameState m) ⇒
  m ()
gameLoop =
  get >>= \case
    GameState {status = Loss} -> do
      drawBoard
      liftIO $ putStrLn "\nYou Lose!"
    GameState {status = Win} -> do
      drawBoard
      liftIO $ putStrLn "\nYou Win!"
    GameState {status = Active} -> do
      promptUser >>= parseInput >>= \case
        Open p    -> openCell p
        Flag p    -> flagCell p
        Invalid s -> liftIO $ printf "\nInvalid command: \"%s\"\n" s
      gameLoop

-- | Create a `GameEnv` from opitonal arguments.
parseArgs ∷ [String] → Maybe GameEnv
parseArgs (x : y : _) =
  GameEnv <$> readMaybe x
    <*> readMaybe y
    <*> Just defaultDifficulty
parseArgs _ = Nothing

-- | Draw the current board state and prompt Player for command.
promptUser ∷
  (MonadState GameState m, MonadIO m) ⇒
  -- | Player input.
  m String
promptUser = do
  drawBoard
  liftIO $ putStr "\nPick a cell: " >> getLine

-- |
--    Draw intial board user Player settings and get
--    Player command. Start the game loop that repeats
--    prompting for Player commands and rendering game board.
startGame ∷
  (MonadReader GameEnv m, MonadIO m, MonadState GameState m) ⇒
  m ()
startGame =
  newGameBoard >> promptUser >>= parseInput >>= \case
    Open p -> do
      mineBoard p
      openCell p
      gameLoop
    _ -> startGame

-- |
--    Default difficulty settings. Mined cells between
--    20% and 10% of total cells.
defaultDifficulty ∷ Difficulty
defaultDifficulty = Difficulty (0.2, 0.1)

-- | Default game board 20x20 with default difficulty.
defaultEnvironment ∷ GameEnv
defaultEnvironment = GameEnv 20 20 defaultDifficulty

-- |
--    Create a `GameEnv` from optional arguments or
--    with defaults. Start the game loop and run until
--    game completes.
main ∷ IO ()
main = do
  hSetBuffering stdout NoBuffering
  env@GameEnv {rows, columns} <- fromMaybe defaultEnvironment . parseArgs <$> getArgs
  let state = GameState mempty Active 0 (rows * columns) 0
  _ <- runReaderT (runStateT startGame state) env
  putStrLn "Game Over."

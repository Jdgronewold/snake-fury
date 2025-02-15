{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent (
  forkIO,
  threadDelay,
 )
import EventQueue (
  EventQueue,
  readEvent,
  writeUserInput,
  setSpeed
 )
import GameState (GameState, move)
import Initialization (gameInitialization)
import RenderState (BoardInfo, RenderState (gameOver, score), render, ppScore)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBinaryMode, hSetBuffering, hSetEcho, stdin, stdout)
import Control.Monad (unless)
import Data.ByteString.Builder (toLazyByteString )
import qualified Data.ByteString.Char8 as B


-- The game loop is easy:
--   - wait some time
--   - read an Event from the queue
--   - Update the GameState
--   - Update the RenderState based on message delivered by GameState update
--   - Render into the console
gameloop :: BoardInfo -> GameState -> RenderState -> EventQueue -> IO ()
gameloop binf gstate rstate queue = do
  newSpeed <- setSpeed (score rstate) queue
  threadDelay newSpeed
  event <- readEvent queue
  (messages, gstate') <- move event binf gstate
  (builder, rstate') <- render messages binf rstate
  let isGameOver = gameOver rstate'
      newScore = score rstate'
      formattedScore = toLazyByteString  $ ppScore newScore
  putStr "\ESC[2J" --This cleans the console screen
  B.putStr $ B.toStrict formattedScore
  B.putStr $ (B.toStrict . toLazyByteString) builder
  unless isGameOver $ gameloop binf gstate' rstate' queue

-- | main.
main :: IO ()
main = do
  -- enable reading key strokes
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  hSetBuffering stdout NoBuffering
  hSetBinaryMode stdout True

  -- Game Initializacion
  [h, w, fps] <- fmap read <$> getArgs
  let timeSpeed = 1_000_000 `div` fps -- One second is 1_000_000 microseconds, which is the unit used by GHC internally.
  (binf, gameState, renderState, eventQueue) <- gameInitialization h w timeSpeed

  -- Game Loop. We run two different threads, one for the gameloop (main) and one for user inputs.
  _ <- forkIO $ writeUserInput eventQueue
  let initialState = gameState
  gameloop binf initialState renderState eventQueue

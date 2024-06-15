{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
This module defines the board. A board is an array of CellType elements indexed by a tuple of ints: the height and width.

for example, The following array represents a 3 by 4 board (left top corner is (1,1); right bottom corner is (3,4)) with a snake at 
(2, 2) and (2, 3) and an apple at (3,4)

< ((1,1) : Empty), ((1,2) : Empty), ((1,3) : Empty),     ((1,2) : Empty)
, ((2,1) : Empty), ((2,2) : Snake)  ((2,3) : SnakeHead)  ((2,4) : Empty)
, ((3,1) : Empty), ((3,2) : Empty), ((3,3) : Empty),     ((3,4) : Apple) >

Which would look like this:

- - - -
- 0 $ -
- - - X


-}
module RenderState where

-- This are all imports you need. Feel free to import more things.
import Data.Array ( (//), listArray, Array, (!))
import Data.List (intercalate)
import Data.ByteString.Builder (Builder, stringUtf8, intDec)
import Control.Monad.Reader
import Control.Monad.State (StateT (..), MonadState, put, get)

-- A point is just a tuple of integers.
type Point = (Int, Int)

-- | Cell types. We distinguish between Snake and SnakeHead
data CellType = Empty | Snake | SnakeHead | Apple deriving (Show, Eq)

-- | The board info is just a description of height and width.
data BoardInfo = BoardInfo {height :: Int, width :: Int} deriving (Show, Eq)
type Board = Array Point CellType     -- ^The board is an Array indexed by points with elements of type CellType

-- | A delta is a small change in the board at some points. For example [((2,2), SnakeHead), ((2,1), Empty)]
--   would represent the change "cell (2,2) should change to become the SnakeHead and cell (2,1) should change by an empty cell"
type DeltaBoard = [(Point, CellType)]

-- | The render message represent all message the GameState can send to the RenderState
--   Right now Possible messages are a RenderBoard with a payload indicating which cells change
--   or a GameOver message.
data RenderMessage
  =
    RenderBoard DeltaBoard
  |
    GameOver
  |
    UpdateScore
  deriving Show

-- | The RenderState contains the board and if the game is over or not.
data RenderState   = RenderState {board :: Board, gameOver :: Bool, score :: Int} deriving Show

-- newtype RenderStep m a = 
--   RenderStep {runRenderStep :: ReaderT BoardInfo (StateT RenderState m) a}
--   deriving (Functor, Applicative, Monad, MonadState RenderState, MonadReader BoardInfo) 

-- | Given The board info, this function should return a board with all Empty cells
emptyGrid :: BoardInfo -> Board
emptyGrid BoardInfo {..} =
    listArray ((1,1), (width, height)) $ replicate (width * height) Empty

{- 
This is a test for emptyGrid. It should return 
array ((1,1),(2,2)) [((1,1),Empty),((1,2),Empty),((2,1),Empty),((2,2),Empty)]
-}
-- >>> emptyGrid (BoardInfo 2 2)
-- array ((1,1),(2,2)) [((1,1),Empty),((1,2),Empty),((2,1),Empty),((2,2),Empty)]


-- | Given BoardInfo, initial point of snake and initial point of apple, builds a board
buildInitialBoard
  :: BoardInfo -- ^ Board size
  -> Point     -- ^ initial point of the snake
  -> Point     -- ^ initial Point of the apple
  -> RenderState
buildInitialBoard BoardInfo {..} snekPoint applePoint =
    RenderState
      {
        board = emptyGrid BoardInfo {..} // [(snekPoint, SnakeHead), (applePoint, Apple)]
      , gameOver = False
      , score = 0
      }

{- 
This is a test for buildInitialBoard. It should return 
RenderState {board = array ((1,1),(2,2)) [((1,1),SnakeHead),((1,2),Empty),((2,1),Empty),((2,2),Apple)], gameOver = False}
-}
-- >>> buildInitialBoard (BoardInfo 2 2) (1,1) (2,2)
-- RenderState {board = array ((1,1),(2,2)) [((1,1),SnakeHead),((1,2),Empty),((2,1),Empty),((2,2),Apple)], gameOver = False}


-- | Given tye current render state, and a message -> update the render state
updateRenderState :: (MonadState RenderState m, MonadReader BoardInfo m) => RenderMessage -> m ()
updateRenderState message = do 
  oldState <- get
  case message of
    RenderBoard delta -> 
      put $ oldState {board = board oldState // delta}
    GameOver -> 
      put $ oldState {gameOver = True}
    UpdateScore -> 
      put $ oldState { score = score oldState + 1 }

-- | takes the old state, applies a list of renderMessages to it, each time returning a new renderState
updateMessages ::  (MonadState RenderState m, MonadReader BoardInfo m) => [RenderMessage] -> m ()
updateMessages = mapM_ updateRenderState

{-
This is a test for updateRenderState

message1 should return:
RenderState {board = array ((1,1),(2,2)) [((1,1),Empty),((1,2),SnakeHead),((2,1),Apple),((2,2),Apple)], gameOver = False}

message2 should return:
RenderState {board = array ((1,1),(2,2)) [((1,1),SnakeHead),((1,2),Empty),((2,1),Empty),((2,2),Apple)], gameOver = True}
-}
-- >>> initial_board =  buildInitialBoard (BoardInfo 2 2) (1,1) (2,2)
-- >>> message1 = RenderBoard [((1,2), SnakeHead), ((2,1), Apple), ((1,1), Empty)]
-- >>> message2 = GameOver
-- >>> updateRenderState initial_board message1
-- >>> updateRenderState initial_board message2
-- RenderState {board = array ((1,1),(2,2)) [((1,1),Empty),((1,2),SnakeHead),((2,1),Apple),((2,2),Apple)], gameOver = False}
-- RenderState {board = array ((1,1),(2,2)) [((1,1),SnakeHead),((1,2),Empty),((2,1),Empty),((2,2),Apple)], gameOver = True}


-- | Provisional Pretty printer
--   For each cell type choose a string to representing. 
--   a good option is
--     Empty -> "- "
--     Snake -> "0 "
--     SnakeHead -> "$ "
--     Apple -> "X "
--   In other to avoid shrinking, I'd recommend to use some charachter followed by an space.
ppCell :: CellType -> String
ppCell = \case
  Empty -> "- "
  Snake -> "0 "
  SnakeHead -> "S "
  Apple -> "X "

ppScore :: Int -> Builder
ppScore s = 
  let 
  asteriskLine = stringUtf8 "**********\n"
  in mconcat [ asteriskLine
  , "Score: "
  , intDec s
  , stringUtf8 "\n"
  , asteriskLine
  ]
  
renderStep :: (MonadState RenderState m, MonadReader BoardInfo m) => [RenderMessage] -> m Builder
renderStep messages = do
  BoardInfo {..} <- ask
  oldState <- get
  updateMessages messages
  newState <- get
  let values = [renderCharacter (board oldState ! (x,y)) (y == width) | x <- [1..height], y <- [1..width] ]
      renderCharacter cellType addNewLine =
        if gameOver newState then "X "
        else ppCell cellType ++ (if addNewLine then "\n" else "")
  pure $ stringUtf8 $ intercalate "" values

-- | convert the RenderState in a String ready to be flushed into the console.
--   It should return the Board with a pretty look. If game over, return the empty board.
-- render :: [RenderMessage] -> BoardInfo -> RenderState ->  (Builder, RenderState)
render :: Monad m => [RenderMessage] -> BoardInfo -> RenderState -> m (Builder, RenderState)
render messages board state = 
  runReaderT (runStateT (renderStep messages) state) board  


{-
This is a test for render. It should return:
"- - - - \n- 0 $ - \n- - - X \n"

Notice, that this depends on what you've chosen for ppCell
-}
-- >>> board = listArray ((1,1), (3,4)) [Empty, Empty, Empty, Empty, Empty, Snake, SnakeHead, Empty, Empty, Empty, Empty, Apple]
-- >>> board_info = BoardInfo 3 4
-- >>> render_state = RenderState board  False
-- >>> render board_info render_state
-- "- - - - \n- 0 S - \n- - - X \n"

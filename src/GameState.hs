{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-|
This module defines the logic of the game and the communication with the `Board.RenderState`
-}
module GameState where

-- These are all the import. Feel free to use more if needed.
import RenderState (BoardInfo (..), Point)
import qualified RenderState as Board
import Data.Sequence ( Seq(..), ViewR(..))
import qualified Data.Sequence as S
import System.Random ( StdGen, Random (randomR))
import Data.Maybe (isJust)
import Control.Monad.Trans.State.Strict (State, get, put, modify, runState)

-- The movement is one of this.
data Movement = North | South | East | West deriving (Show, Eq)

-- | The snakeSeq is a non-empty sequence. It is important to use precise types in Haskell
--   In first sight we'd define the snake as a sequence, but If you think carefully, an empty 
--   sequence can't represent a valid Snake, therefore we must use a non empty one.
--   You should investigate about Seq type in haskell and we it is a good option for our porpouse.
data SnakeSeq = SnakeSeq {snakeHead :: Point, snakeBody :: Seq Point} deriving (Show, Eq)

-- | The GameState represents all important bits in the game. The Snake, The apple, the current direction of movement and 
--   a random seed to calculate the next random apple.
data GameState = GameState
  { snakeSeq :: SnakeSeq
  , applePosition :: Point
  , movement :: Movement
  , randomGen :: StdGen
  }
  deriving (Show, Eq)

type GameStep a = State GameState a

-- | This function should calculate the opposite movement.
opositeMovement :: Movement -> Movement
opositeMovement = \case
  North -> South
  South -> North
  East -> West
  West -> East

-- >>> opositeMovement North == South
-- >>> opositeMovement South == North
-- >>> opositeMovement East == West
-- >>> opositeMovement West == East


-- | Purely creates a random point within the board limits
--   You should take a look to System.Random documentation. 
--   Also, in the import list you have all relevant functions.
-- makeRandomPoint :: BoardInfo -> StdGen -> (Point, StdGen)
-- makeRandomPoint boardInfo gen =
--   let (x, gen') = randomR (1, width boardInfo) gen
--       (y, gen'') = randomR (1, height boardInfo) gen'
--    in ((x, y), gen'')

makeRandomPoint :: BoardInfo -> GameStep Point
makeRandomPoint BoardInfo {..} = do
  game@GameState {..} <- get 
  let (x, gen') = randomR (1, width) randomGen
      (y, gen'') = randomR (1, height) gen'
  put $ game { randomGen = gen''}
  pure (x, y)
{-
We can't test makeRandomPoint, because different implementation may lead to different valid result.
-}


-- | Check if a point is in the snake
inSnake :: Point -> SnakeSeq -> Bool
inSnake point snek =
  let snekBody = snakeBody snek
   in point == snakeHead snek || isJust (point `S.elemIndexL` snekBody)

{-
This is a test for inSnake. It should return 
True
True
False
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> inSnake (1,1) snake_seq
-- >>> inSnake (1,2) snake_seq
-- >>> inSnake (1,4) snake_seq
-- True
-- True
-- False

-- | Calculates de new head of the snake. Considering it is moving in the current direction
--   Take into acount the edges of the board
nextHead :: BoardInfo -> GameStep Point
nextHead BoardInfo {..}  = do
  GameState {..} <- get 
  let prevHead = snakeHead snakeSeq  
      (newHeight, newWidth) = calculateNextPoint prevHead  movement 
  pure (correctValue height newHeight, correctValue width newWidth)
  where 
    correctValue maxV v
      | v < 1 = maxV
      | v > maxV = 1
      | otherwise = v

    calculateNextPoint :: Point -> Movement -> (Int, Int)
    calculateNextPoint prevHead = \case
      North -> (fst prevHead -1, snd prevHead)
      South -> (fst prevHead + 1, snd prevHead)
      East -> (fst prevHead, snd prevHead + 1)
      West -> (fst prevHead, snd prevHead -1)
{-
This is a test for nextHead. It should return
True
True
True
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,2) 
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> nextHead board_info game_state1 == (1,4)
-- >>> nextHead board_info game_state2 == (2,1)
-- >>> nextHead board_info game_state3 == (4,1)
-- Couldn't match expected type: GameState
--                               -> (a0_a3s9U[tau:1], b0_a3s9V[tau:1])
--             with actual type: StateT GameState Identity Point
-- The function `nextHead' is applied to two value arguments,
--   but its type `BoardInfo -> StateT GameState Identity Point'
--   has only one
-- In the first argument of `(==)', namely
--   `nextHead board_info game_state1'
-- In the expression: nextHead board_info game_state1 == (1, 4)


-- | Calculates a new random apple, avoiding creating the apple in the same place, or in the snake body
newApple :: BoardInfo -> GameStep Point
newApple board = do
  GameState {..} <- get
  newPoint <- makeRandomPoint board
  let snekHead = snakeHead snakeSeq
      snekBody = snakeBody snakeSeq
      unallowedPositions = snekBody S.>< S.fromList [snekHead, applePosition]
  if newPoint `elem` unallowedPositions then do
    newApple board
  else do
    modify (\oldState -> oldState {applePosition = newPoint})
    pure newPoint

-- Add a point to the snake head
extendSnake :: Point -> BoardInfo -> GameStep Board.DeltaBoard
extendSnake point board = do
  GameState {..} <- get
  applePos' <- newApple board
  let snekHead = snakeHead snakeSeq
      snekBody = snakeBody snakeSeq
      snekBody' = snekHead S.<| snekBody
      delta' = [ (point, Board.SnakeHead)
                , (applePos', Board.Apple)
                , (snekHead, Board.Snake)
                ]
  modify (\oldState -> oldState {snakeSeq = SnakeSeq point snekBody'})              
  pure delta'
    

displaceSnake :: Point -> BoardInfo -> GameStep Board.DeltaBoard
displaceSnake point _  = do
  GameState {..} <- get
  let snekHead = snakeHead snakeSeq
      snekBody = snakeBody snakeSeq
      (snekTail, droppedPoint) = dropLast snekBody
      snekBody' = snekHead S.<| snekTail
      delta' = [ (point, Board.SnakeHead)
              , (snekHead, Board.Snake) 
              ] <> maybe [] (\p -> [(p, Board.Empty)]) droppedPoint
  modify (\oldState -> oldState {snakeSeq = SnakeSeq point snekBody'})
  pure delta'

{- We can't test this function because it depends on makeRandomPoint -}


-- | Moves the snake based on the current direction. It sends the adequate RenderMessage
-- Notice that a delta board must include all modified cells in the movement.
-- For example, if we move between this two steps
--        - - - -          - - - -
--        - 0 $ -    =>    - - 0 $
--        - - - -    =>    - - - -
--        - - - X          - - - X
-- We need to send the following delta: [((2,2), Empty), ((2,3), Snake), ((2,4), SnakeHead)]
--
-- Another example, if we move between this two steps
--        - - - -          - - - -
--        - - - -    =>    - X - -
--        - - - -    =>    - - - -
--        - 0 $ X          - 0 0 $
-- We need to send the following delta: [((2,2), Apple), ((4,3), Snake), ((4,4), SnakeHead)]
-- 

step :: BoardInfo -> GameStep [Board.RenderMessage]
step board = do
  GameState {..} <- get
  newHead <- nextHead board
  if newHead == applePosition then do
    delta' <- extendSnake newHead board
    pure [Board.RenderBoard delta', Board.UpdateScore]
  else do
    delta' <- displaceSnake newHead board
    pure [Board.RenderBoard delta']


move :: BoardInfo -> GameState -> ([Board.RenderMessage], GameState)
move board = runState (step board) 

      
dropLast :: Seq a -> (Seq a, Maybe a)
dropLast s = case S.viewr s of
    EmptyR   -> (S.empty, Nothing)  -- Handles the case where the sequence is already empty
    s' :> x -> (s', Just x)          -- s' is the sequence without the last element

{- This is a test for move. It should return

RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),Empty)]
RenderBoard [((2,1),SnakeHead),((1,1),Snake),((3,1),Apple)] ** your Apple might be different from mine
RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),Empty)]

-}

-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,1) 
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> fst $ move board_info game_state1
-- >>> fst $ move board_info game_state2
-- >>> fst $ move board_info game_state3
-- RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),Empty)]
-- RenderBoard [((2,1),SnakeHead),((2,4),Apple),((1,1),Snake)]
-- RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),Empty)]

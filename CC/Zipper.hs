{-# LANGUAGE TypeFamilies #-}

module CC.Zipper where

import Control.Monad.State

import CC.Syntax


---------------------
-- Generic Zippers --
---------------------

type Location a = (Context a, Current a)
type Move a     = Location a -> Maybe (Location a)

-- Zippable types
class Zip a where
  
  data Context a
  type Current a
  
  enter    :: a -> Location a
  exit     :: Location a -> a
  
  down     :: Move a
  up       :: Move a
  left     :: Move a
  right    :: Move a

{-
bottom   :: Location a -> Bool
top      :: Location a -> Bool
leftEnd  :: Location a -> Bool
rightEnd :: Location a -> Bool
  
-- sequence two moves
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

-- sequence many moves
foldMoves :: [Move a] -> Move a
foldMoves = foldr (.>) id

-- move to a particular subexpression
toSub :: Int -> 

-- change the current value
transform :: (Current a -> Current a) -> Move a
transform f (ctx, cur) = (ctx, f cur)

------------------
-- Zipper Monad --
------------------

type Zipper a = State (Location a)

runZipper :: Zip a => a -> Zipper a b -> a
runZipper a z = exit (execState z (enter a))

evalZipper :: Zip a => a -> Zipper a b -> b
evalZipper = flip evalState . enter

move :: Move a -> Zipper a ()
move m = get >>= put . m

moveToSub :: Int -> Zipper a ()
moveToSub = move down >> replicateM_ i (move right)

moveIf :: Zipper a Bool -> Move a -> Zipper a ()
moveIf t m = t >>= flip unless (fail "Illegal move.") >> move m

moveIfCurrent :: (Current a -> Bool) -> Move a -> Zipper a ()
moveIfCurrent t m = moveIf (fmap t current) m

location :: Zipper a (Location a)
location = get

context :: Zipper a (Context a)
context = fmap fst get

current :: Zipper a (Current a)
current = fmap snd get
-}

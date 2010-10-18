module CC.Zipper where

import Control.Monad
import Data.Maybe

import CC.Syntax
import CC.Pretty () -- for Show (CC a) instance


-------------
-- Focuses --
-------------

-- The immediate context in which a subexpression exists.
-- Conceptually, an expresion with a hole.
data Focus a =
        InB (CCB a) [CC a] [CC a] -- in branch expression
      | InL InL Var (CC a)        -- in let expression, False=bind, True=use
      | InD Dim [Tag]             -- in dimension declaration
  deriving (Eq,Show)

-- focus inside a let
data InL = B | U -- bind | use
  deriving (Eq,Show)

-- Apply a focus to an expression, i.e. fill the hole.
apply :: Focus a -> CC a -> CC a
apply (InB b l r) e = b :< foldl (flip (:)) (e:r) l
apply (InL B v u) b = Let v b u
apply (InL U v b) u = Let v b u
apply (InD d ts)  e = Dim d ts e


----------------------------
-- Contexts and Locations --
----------------------------

type Context  a = [Focus a]
type Location a = (Context a, CC a)

-- enter an expression (get a location for the top node)
enter :: CC a -> Location a
enter e = ([],e)

-- exit all the way out of an expression
exit :: Location a -> CC a
exit (c,e) = foldl (flip apply) e c


-----------
-- Moves --
-----------

type Move a = Location a -> Maybe (Location a)

-- execute a move from the top of an expression and return the final location
goto :: Move a -> CC a -> Maybe (Location a)
goto m c = m (enter c)

-- 
-- generic moves
--

up :: Move a
up ((f:c), e) = Just (c, apply f e)
up _ = Nothing

down :: Move a
down (c, b :< (e:es)) = Just (InB b [] es : c, e)
down (c, Dim d ts e)  = Just (InD d ts    : c, e)
down (c, Let v b u)   = Just (InL B v u   : c, b)
down _ = Nothing

left :: Move a
left (InB b (l:ls) rs : c, r) = Just (InB b ls (r:rs) : c, l)
left (InL U v b       : c, u) = Just (InL B v u       : c, b)
left _ = Nothing

right :: Move a
right (InB b ls (r:rs) : c, l) = Just (InB b (l:ls) rs : c, r)
right (InL B v u       : c, b) = Just (InL U v b       : c, u)
right _ = Nothing

--
-- specific moves
--

-- move if the current expression satisfies the provided predicate
moveIf :: (CC a -> Bool) -> Move a -> Move a
moveIf test move loc | test (snd loc) = move loc
                     | otherwise      = Nothing

-- in dimension declaration, let-binding, or let-use
inDim,inBind,inUse :: Move a
inDim  = moveIf isDim down
inBind = moveIf isLet down
inUse  = moveIf isLet (down >=> right)

-- move into a particular (indexed) subexpresion
inSub :: Int -> Move a
inSub i | i >= 0    = foldl (>=>) down (replicate i right)
        | otherwise = const Nothing

-- in structure subexpression or alternative
inStr,inAlt :: Int -> Move a
inStr = moveIf isStr . inSub
inAlt = moveIf isChc . inSub


--------------------------
-- Matching and Queries --
--------------------------

-- are we at the top/bottom/leftEnd/rightEnd of the expression?
top,bottom,leftEnd,rightEnd :: Location a -> Bool
top      = isNothing . up
bottom   = isNothing . down
leftEnd  = isNothing . left
rightEnd = isNothing . right

-- find a matching location using a preorder traversal
match :: (CC a -> Bool) -> Location a -> Maybe (Location a)
match f l | f (snd l) = Just l
          | otherwise = case down l >>= match f of
                          Nothing -> tryRight
                          success -> success
  where tryRight | rightEnd l = Nothing
                 | otherwise  = right l >>= match f


---------------------
-- Transformations --
---------------------

-- change the value at the current location
transform :: (CC a -> CC a) -> Move a
transform f (c,e) = Just (c,f e)

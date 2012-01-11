{-# LANGUAGE Rank2Types #-}

module CC.Zipper where

import Control.Monad
import Data.Maybe
import Data.Generics.Zipper -- requires, from Hackage: syz

import CC.Syntax


---------------
-- CC Zipper --
---------------

type CCZ e = Zipper (CC e)

-- Enter the zipper.
enter :: ExpT e => CC e -> CCZ e
enter = toZipper

-- Exit the zipper.
exit :: CCZ e -> CC e
exit = fromZipper


--------------------------------
-- Generic Hole Manipulations --
--------------------------------

-- Apply a query to the choice calculus expression at the current hole.
cczQ :: ExpT e => r -> CCQ r -> CCZ e -> r
cczQ d f z = query (ccQ (unXCC z) d f) z

-- Apply a transformation to the choice calculus expression at the current hole.
cczT :: ExpT e => CCT -> CCZ e -> CCZ e
cczT f z = trans (ccT (unXCC z) f) z

-- Apply a monadic transformation to the choice calculus expression at the current hole.
cczM :: (Monad m, ExpT e) => CCM m -> CCZ e -> m (CCZ e)
cczM f z = transM (ccM (unXCC z) f) z


-------------
-- Queries --
-------------

-- Boolean version of cczQ, asking am I at a location that satisfies the query?
atCC :: ExpT e => CCQ Bool -> CCZ e -> Bool
atCC = cczQ False

-- Is the current hole at a specific syntactic category?
atExp, atDim, atChc, atLet, atRef :: ExpT e => CCZ e -> Bool
atExp = atCC isExp
atDim = atCC isDim
atChc = atCC isChc
atLet = atCC isLet
atRef = atCC isRef

-- Are we at the top/bottom/leftEnd/rightEnd of the expression?
atTop, atBottom, atLeftEnd, atRightEnd :: ExpT e => CCZ e -> Bool
atTop      = isNothing . up
atBottom   = isNothing . down
atLeftEnd  = isNothing . left
atRightEnd = isNothing . right


-----------
-- Moves --
-----------

type Move   e = CCZ e -> Maybe (CCZ e)

-- Execute the given move if the given test passes.
moveIf :: ExpT e => (CCZ e -> Bool) -> Move e -> Move e
moveIf test move z | test z    = move z
                   | otherwise = Nothing

-- Move into a subexpression, dimension declaration, let-binding, or let-use.
inExp, inDim, inBind, inUse :: ExpT e => Move e
inExp  = moveIf atExp down
inDim  = moveIf atDim down
inBind = moveIf atLet (down >=> left)
inUse  = moveIf atLet down

-- Move into a particular (indexed) alternative.
inAlt :: ExpT e => Int -> Move e
inAlt i | i >= 0    = foldl (>=>) down' (replicate (i+1) right)
        | otherwise = const Nothing

-- Find the next matching location using a preorder traversal.
match :: ExpT e => CCQ Bool -> Move e
match f z | atCC f z  = Just z
          | otherwise = case down' z >>= match f of
                          Nothing -> tryRight
                          success -> success
  where tryRight | atRightEnd z = Nothing
                 | otherwise    = right z >>= match f


---------------------
-- Transformations --
---------------------

type Trans  e = CCZ e -> CCZ e
type TransM e = CCZ e -> Maybe (CCZ e)

-- Create a new dimension at the current location.
newDim :: ExpT e => Dim -> [Tag] -> Trans e
newDim d ts = cczT (Dim d ts)

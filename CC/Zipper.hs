{-# LANGUAGE Rank2Types #-}
-- requires, from Hackage: syz
module CC.Zipper where

import Control.Monad
import Data.Maybe
import Data.Generics.Zipper

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
ccQuery :: ExpT e => r -> (forall f. ExpT f => CC f -> r) -> CCZ e -> r
ccQuery d f z = query (ccQ (unXCC z) d f) z

-- Apply a transformation to the choice calculus expression at the current hole.
ccTrans :: ExpT e => (forall f. ExpT f => CC f -> CC f) -> CCZ e -> CCZ e
ccTrans f z = trans (ccT (unXCC z) f) z

-- Apply a monadic transformation to the choice calculus expression at the current hole.
ccTransM :: (Monad m, ExpT e) => (forall f. ExpT f => CC f -> m (CC f)) -> CCZ e -> m (CCZ e)
ccTransM f z = transM (ccM (unXCC z) f) z

-- Apply a query to the immediate subexpressions of the current expression.
ccQuerySubs :: ExpT e => r -> (forall f. ExpT f => CC f -> r) -> CCZ e -> [r]
ccQuerySubs d f z = zmapQ (ccQ (unXCC z) d f) z

-- Apply a transformation to the immediate subexpressions of the current expression.
ccTransSubs :: ExpT e => (forall f. ExpT f => CC f -> CC f) -> CCZ e -> CCZ e
ccTransSubs f z = zmapT (ccT (unXCC z) f) z

-- Apply a monadic transformation to the immediate subexpressions of the current expression.
ccTransSubsM :: (Monad m, ExpT e) => (forall f. ExpT f => CC f -> m (CC f)) -> CCZ e -> m (CCZ e)
ccTransSubsM f z = zmapM (ccM (unXCC z) f) z

-- Recursively apply a transformation to the immediate subexpressions of this expression.
transformSubs :: ExpT e => (forall f. ExpT f => CC f -> CC f) -> CC e -> CC e
transformSubs f e = exit (ccTransSubs f (enter e))

-- Recursively apply a monadic transformation to the immediate subexpressions of this expression.
transformSubsM :: (Monad m, ExpT e) => (forall f. ExpT f => CC f -> m (CC f)) -> CC e -> m (CC e)
transformSubsM f e = ccTransSubsM f (enter  e) >>= return . exit


-------------
-- Queries --
-------------

-- Boolean version of ccQuery, asking am I at a location that satisfies the query?
atCC :: ExpT e => (forall f. ExpT f => CC f -> Bool) -> CCZ e -> Bool
atCC = ccQuery False

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
match :: ExpT e => (forall f. ExpT f => CC f -> Bool) -> Move e
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
newDim d ts = ccTrans (Dim d ts)

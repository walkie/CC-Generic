{-# LANGUAGE Rank2Types #-}
-- requires, from Hackage: syz
module CC.Zipper where

import Control.Monad
import Data.Maybe
import qualified Data.Generics.Zipper as Z

import CC.Syntax


---------------
-- CC Zipper --
---------------

type Zipper e = Z.Zipper (CC e)

enter :: ExpT e => CC e -> Zipper e
enter = Z.toZipper

exit :: ExpT e => Zipper e -> CC e
exit = Z.fromZipper

getHole :: ExpT e => Zipper e -> Maybe (CC e)
getHole = Z.getHole


-------------
-- Queries --
-------------

-- apply a query to the choice calculus expression at the current hole
queryCC :: ExpT e => r -> (forall f. CC f -> r) -> Zipper e -> r
queryCC d f z = maybe d (ccQ (unXCC z) d f) (getHole z)

-- boolean version of queryCC, asking am I at a location that satisfies the query?
atCC :: ExpT e => (forall f. CC f -> Bool) -> Zipper e -> Bool
atCC = queryCC False

-- is the current hole at a specific syntactic category?
atExp, atDim, atChc, atLet, atRef :: ExpT e => Zipper e -> Bool
atExp = atCC isExp
atDim = atCC isDim
atChc = atCC isChc
atLet = atCC isLet
atRef = atCC isRef

-- are we at the top/bottom/leftEnd/rightEnd of the expression?
atTop, atBottom, atLeftEnd, atRightEnd :: ExpT e => Zipper e -> Bool
atTop      = isNothing . Z.up
atBottom   = isNothing . Z.down
atLeftEnd  = isNothing . Z.left
atRightEnd = isNothing . Z.right


-----------
-- Moves --
-----------

type Move e = Zipper e -> Maybe (Zipper e)

-- execute the given move if the given test passes
moveIf :: ExpT e => (Zipper e -> Bool) -> Move e -> Move e
moveIf test move z | test z    = move z
                   | otherwise = Nothing

-- move into a subexpression, dimension declaration, let-binding, or let-use
inExp, inDim, inBind, inUse :: ExpT e => Move e
inExp  = moveIf atExp Z.down
inDim  = moveIf atDim Z.down
inBind = moveIf atLet (Z.down >=> Z.left)
inUse  = moveIf atLet Z.down

-- move into a particular (indexed) alternative
inAlt :: ExpT e => Int -> Move e
inAlt i | i >= 0    = foldl (>=>) Z.down' (replicate (i+1) Z.right)
        | otherwise = const Nothing

-- find the next matching location using a preorder traversal
match :: ExpT e => (forall f. CC f -> Bool) -> Move e
match f z | atCC f z  = Just z
          | otherwise = case Z.down' z >>= match f of
                          Nothing -> tryRight
                          success -> success
  where tryRight | atRightEnd z = Nothing
                 | otherwise    = Z.right z >>= match f

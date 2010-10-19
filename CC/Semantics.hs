module CC.Semantics where

import Control.Monad (guard)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe,fromJust)

import CC.Syntax
import CC.Zipper

---------------
-- Selection --
---------------

type QTag     = (Dim,Tag)
type Selector = (Dim,Int)
type Decision = [QTag]

-- Tag selection. Given qualified tag d.t, select tag t in the first
-- dimension d encountered in an inorder traversal of the choice expression.
selectTag :: QTag -> CC a -> Maybe (CC a)
selectTag (d,t) e = do 
    (c, Dim _ ts e') <- goto (match dim) e
    i                <- elemIndex t ts
    e''              <- choiceElim (d,i) e'
    return (exit (c,e''))
  where dim (Dim d' _ _) = d == d'
        dim _            = False

-- Choice elimination Given selector d.i, replace all choices in dimension d
-- with their ith alternative. Used in tag selection and the choice semantics.
choiceElim :: Selector -> CC a -> Maybe (CC a)
choiceElim (d,_) e@(Dim d' _ _) | d == d' = return e
choiceElim (d,i) (Chc d' :< es) | d == d' = do guard (i < length es)
                                               choiceElim (d,i) (es !! i)
choiceElim s e = transformSubsM (choiceElim s) e

-- Make a decision by applying each selection in order.
decide :: Decision -> CC a -> Maybe (CC a)
decide d e = foldl (\m q -> m >>= selectTag q) (Just e) d


------------------------
-- Decision Semantics --
------------------------

type Map k v = [(k,v)]
type Semantics a = Map Decision (CC a)

-- Let expansion.  This transformation should only be applied to dimension-free
-- expressions, otherwise the semantics will be changed.
letExp :: Map Var (CC a) -> CC a -> CC a
letExp m (Let v b u) = letExp ((v, letExp m b):m) u
letExp m e@(Ref v)   = fromMaybe e (lookup v m)
letExp m e           = transformSubs (letExp m) e

-- The variants of a choice calculus expression.  Removes all dimensions.
-- Removes all choices if well-dimensioned.
variants :: CC a -> Semantics a
variants (Dim d ts e) = do
    (t,i)   <- zip ts [0..]
    (qs,e') <- variants (fromJust (choiceElim (d,i) e))
    return ((d,t):qs, e')
variants e | null (subs e) = [([],e)]
           | otherwise     = [(qs, replaceSubs e es) | (qs,es) <- product e]
  where product = cross . mapSubs variants
        cross []     = []
        cross [v]    = [(qs    , [e])  | (qs,e) <- v]
        cross (v:vs) = [(qs++rs, e:es) | (qs,e) <- v, (rs,es) <- cross vs]

-- If well-formed, provides a mapping from decisions to plain expressions.
semantics :: CC a -> Semantics a
semantics e = [(qs, letExp [] e') | (qs,e') <- variants e]

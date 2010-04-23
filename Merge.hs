{-# LANGUAGE PatternGuards #-}

module Merge where

import Data.List (nub,sort)

import Choice
import Patch

--
-- Merging patch expressions
--

merge :: Eq a => PExpr a -> PExpr a -> PExpr a
merge a b = foldr ($) (merge' a' b') (map bdim ds)
  where (ads,a') = stripDims a
        (bds,b') = stripDims b
        ds = (sort . nub) (ads ++ bds)

-- Pattern match failure indicates invalid expressions,
-- error "Conflict!" indicates an actual conflict.
merge' :: Eq a => PExpr a -> PExpr a -> PExpr a

merge' (a :< as) (b :< bs) 
    | a == b, length as == length bs = a :< zipWith merge' as bs
merge' (d :? ds) (e :? es) 
    | d == e, length ds == length es = d :? zipWith merge' ds es
    | d /= e                         = error "Conflict!"

merge' (Var v) (Var w) | v == w = (Var v)

merge' (Let (v:=a) (d :? ds)) (Let (w:=b) (e :? es))
    | v == w, d == e,
      length ds == length es = Let (v := merge' a b) $ d :? zipWith merge' ds es
    | d /= e                 = error "Conflict!"

merge' a (Let (v:=b) (d :? cs)) = Let (v := merge' a b) (d :? cs)
merge' (Let (v:=a) (d :? cs)) b = Let (v := merge' a b) (d :? cs)

stripDims :: PExpr a -> ([Name], PExpr a)
stripDims e = strip ([],e)
  where strip (ds,Dim (d:=_) e) = strip (d:ds,e)
        strip (ds,e           ) = (ds,e)


--
-- Testing
--

e23 = merge e2 e3
--e23 = merge (expand [] e2) (expand [] e3)

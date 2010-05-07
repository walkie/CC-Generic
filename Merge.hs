{-# LANGUAGE PatternGuards #-}

module Merge where

import Data.Function (on)
import Data.List     (elemIndex,nub,sort,sortBy)
import Data.Maybe    (fromJust,fromMaybe)

import Choice
import Patch

--
-- Merging patch expressions
--

{-
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
-}

data TagMerge t = TM Int [Int] [Int] deriving (Eq,Show)
type MergeMap t = Map (TagMerge t)
  
maybes :: (a -> b) -> (a -> a -> b) -> Maybe a -> Maybe a -> b
maybes _ g (Just a) (Just b) = g a b
maybes f _ (Just a) Nothing  = f a
maybes f _ Nothing  (Just b) = f b

lookups :: Eq k => (Maybe a -> Maybe a -> b) -> k -> [(k,a)] -> [(k,a)] -> b
lookups f k as bs = f (lookup k as) (lookup k bs)

buildList :: Int -> a -> [(Int,a)] -> [a]
buildList l a as = [fromMaybe a (lookup i as) | i <- [0..l-1]]

mergeLists :: Int -> (a -> a -> a) -> [(Int,a)] -> [(Int,a)] -> [a]
mergeLists l f as bs = [lookups (maybes id f) i as bs | i <- [0..l-1]]

mergeTags :: Ord t => [t] -> [t] -> ([t],TagMerge t)
mergeTags ts us = (vs, TM (length vs) (ix ts) (ix us))
  where vs = nub (ts ++ us)
        ix = map (fromJust . flip elemIndex vs)

mergeDims :: Ord t => Dims t -> Dims t -> (Dims t, MergeMap t)
mergeDims as bs = ([(n,ts) | (n,(ts,_)) <- m], [(n,tm) | (n,(_,tm)) <- m])
  where mds ns = [(n, lookups (maybes (dup mergeTags) mergeTags) n as bs) | n <- ns]
        m = (mds . nub) (dom as ++ dom bs)

declareDims :: Dims t -> Expr t a -> Expr t a
declareDims ds e = foldr Dim e (rezip (:=) ds)

merge :: (Ord t, Eq a) => Expr t a -> Expr t a -> Expr t a
merge e f | not (dimLinear e && dimLinear f) = error "Only dimension linear expressions can be merged!"
          | otherwise = declareDims ds (merge' m e f)
  where (ds,m) = mergeDims (dims e) (dims f)

--
-- the core of the merge algorithm
--
merge' :: (Ord t, Eq a) => MergeMap t -> Expr t a -> Expr t a -> Expr t a

-- remove dimension declarations
merge' m (Dim _ e) f         = merge' m e f
merge' m e         (Dim _ f) = merge' m e f

-- merge choices
merge' m (a :? es) (b :? fs) | a == b, Just (TM l is js) <- lookup a m
    = a :? mergeLists l (merge' m) (zip is es) (zip js fs)

-- default case, propogate merge to children
merge' m a b | a ~= b = czip (merge' m) a b

-- incorporate new choices
merge' m (Let (v:=a) (d :? cs)) b = Let (v := merge' m a b) $ d :?
    case lookup d m of
      Just (TM l is _) -> buildList l (Var v) (zip is cs)
      Nothing          -> cs
merge' m a (Let (v:=b) (d :? cs)) = Let (v := merge' m a b) $ d :?
    case lookup d m of
      Just (TM l _ js) -> buildList l (Var v) (zip js cs)
      Nothing          -> cs

-- everything else...
merge' _ _ _ = error "Kaboom!"


-- Shallow equality

infix 4 ~=
(~=) :: (Eq t, Eq a) => Expr t a -> Expr t a -> Bool
a :< as      ~= b :< bs      = a == b && length as == length bs
Let (v:=_) _ ~= Let (w:=_) _ = v == w
Var v        ~= Var w        = v == w
Dim (a:=t) _ ~= Dim (b:=u) _ = a == b && t == u
a :? as      ~= b :? bs      = a == b && length as == length bs
_            ~= _            = False

--
-- Helper functions
--

czip :: (Eq t, Eq a) => (Expr t a -> Expr t a -> Expr t a) -> Expr t a -> Expr t a -> Expr t a
czip f a b {- | a ~= b -} = cswap a (zipWith f (children a) (children b))

dup :: (a -> a -> b) -> a -> b
dup f a = f a a

rezip :: (a -> b -> c) -> [(a,b)] -> [c]
rezip f abs = [f a b | (a,b) <- abs]

--
-- Testing
--

axbc = merge axb abc

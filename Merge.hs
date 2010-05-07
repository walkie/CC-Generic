{-# LANGUAGE PatternGuards #-}

module Merge where

import Data.Function (on)
import Data.List     (elemIndex,find,nub,sort,sortBy)
import Data.Maybe    (fromJust,fromMaybe)

import Choice
import Patch

--
-- Merging patch expressions
--

data TagMerge = TM Int [Int] [Int] deriving (Eq,Show)
type MergeMap = Map TagMerge
  
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

mergeTags :: Ord t => [t] -> [t] -> ([t],TagMerge)
mergeTags ts us = (vs, TM (length vs) (ix ts) (ix us))
  where vs = nub (ts ++ us)
        ix = map (fromJust . flip elemIndex vs)

mergeDims :: Ord t => Dims t -> Dims t -> (Dims t, MergeMap)
mergeDims as bs = ([(n,ts) | (n,(ts,_)) <- m], [(n,tm) | (n,(_,tm)) <- m])
  where mds ns = [(n, lookups (maybes (dup mergeTags) mergeTags) n as bs) | n <- ns]
        m = (mds . nub) (dom as ++ dom bs)

newChoice :: (Eq t, Eq a) => Bool -> MergeMap -> Name -> Name -> Expr t a -> Expr t a -> Name -> [Expr t a] -> Expr t a
newChoice isL m c v l r d as = Let (v := merge' m c l r) $ d :?
    case lookup d m of
      Just (TM n is js) -> buildList n (Var v) (zip (if isL then is else js) as)
      Nothing           -> as

conflictDimName :: Dims t -> Name
conflictDimName ds = fromJust $ find (flip notElem (dom ds)) (map name [1..])
  where name n = "*Conflict" ++ id n ++ "*"
        id 1 = ""
        id n = show n

conflictDim :: Dims t -> t -> t -> Expr t a -> Expr t a
conflictDim ds l r = Dim (conflictDimName ds := [l,r])

declareDims :: Dims t -> Expr t a -> Expr t a
declareDims ds e = foldr Dim e (rezip (:=) ds)

merge :: (Ord t, Eq a) => t -> t -> Expr t a -> Expr t a -> Expr t a
merge cl cr l r | not (dimLinear l && dimLinear r) = error "Only dimension linear expressions can be merged!"
                | otherwise = wrap merged
  where (ds,m) = mergeDims (dims l) (dims r)
        cd     = conflictDimName ds
        merged = declareDims ds (merge' m cd l r)
        wrap | cd `elem` freeDims merged = Dim (cd := [cl,cr])
             | otherwise                 = id

--
-- the core of the merge algorithm
--

merge' :: (Eq t, Eq a) => MergeMap -> Name -> Expr t a -> Expr t a -> Expr t a

-- remove dimension declarations
merge' m c (Dim _ l) r         = merge' m c l r
merge' m c l         (Dim _ r) = merge' m c l r

-- merge choices
merge' m c (ld :? ls) (rd :? rs) | ld == rd, Just (TM len is js) <- lookup ld m
    = ld :? mergeLists len (merge' m c) (zip is ls) (zip js rs)

-- default case, propogate merge to children
merge' m c l r | l ~= r = czip (merge' m c) l r

-- incorporate new choices
merge' m c (Let (v:=l) (d :? as)) r = newChoice True  m c v l r d as
merge' m c l (Let (v:=r) (d :? as)) = newChoice False m c v l r d as

-- everything else is a conflict...
merge' _ c l r = c :? [l,r]


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

mergeLR :: Eq a => Expr String a -> Expr String a -> Expr String a
mergeLR = merge "Left" "Right"

mergeTF :: Eq a => Expr Bool a -> Expr Bool a -> Expr Bool a
mergeTF = merge True False

axbc = mergeTF axb abc

{-# LANGUAGE PatternGuards #-}

module Merge3 where

import Data.Function (on)
import Data.List     (elemIndex)

import Choice hiding (Context)

type Context t = Decision t
type TagOrder = [Maybe Int]

-- get the indexes at which each new tag appears in the old tags
-- i.e. given old = [a,b,c], new = [c,d,b]
--      tagOrder old new = [Just 2,Nothing,Just 1]
tagOrder :: Eq t => [t] -> [t] -> TagOrder
tagOrder = map . flip elemIndex

-- do these dimension have the same name?
sameName :: Dim t -> Dim t -> Bool
sameName = (==) `on` var

-- was this dimension selected in the given context?
wasHidden :: Dim t -> Context t -> Bool
wasHidden (d:=_) = elem d . dom

--
-- merge back state
--

-- contains information that is needed for the mergeBack algorithm
data MergeState = MS {
  mergedTags  :: Map TagOrder,
  hiddenIxs   :: Map Int,
  deletedDims :: [Name]
}

-- is this dimension in the merged dimension map?
isMerged :: Name -> MergeState -> Bool
isMerged d = elem d . dom . mergedTags

-- is this dimension in the hidden dimension map?
isHidden :: Name -> MergeState -> Bool
isHidden d = elem d . dom . hiddenIxs

-- was this dimension deleted?
isDeleted :: Name -> MergeState -> Bool
isDeleted d = elem d . deletedDims

-- add merged dimension information to the merge state
addMerged :: Eq t => Dim t -> Dim t -> MergeState -> MergeState
addMerged (a:=ts) (b:=us) (MS os is ds) {-| a == b-} = MS ((b,tagOrder ts us):os) is ds

-- add hidden dimension information to the merge state
addHidden :: Eq t => Dim t -> Context t -> MergeState -> MergeState
addHidden (d:=ts) ctx (MS os is ds) | Just t <- lookup d ctx
                                    , Just i <- t `elemIndex` ts
                                    = MS os ((d,i):is) ds

-- add deleted dimension to the merge state
addDeleted :: Dim t -> MergeState -> MergeState
addDeleted (d:=_) (MS os is ds) = MS os is (d:ds)

--
-- merge back
--

-- merge a zoomed and changed expression back into the original expression
mergeBack :: (Eq t, Eq a) => Dim t -> Context t -> Expr t a -> Expr t a -> Expr t a
mergeBack cd ctx orig new = merge (MS [] [] []) orig new
  where 
 -- merge :: (Eq t, Eq a) => Map TagOrder -> Map Int -> Expr t a -> Expr t a -> Expr t a

    -- ** choices **
    merge s (dl :? ls) (dr :? rs) = undefined

    -- ** default case **
    
    -- nodes match, merge corresponding children
    merge s o n | o ~= n = cswap o (zipWith (merge s) (children o) (children n))

    -- ** dimension declarations **

    -- merge dimensions with same name (but different tags)
    merge s (Dim dl l) (Dim dr r)
      | sameName dl dr = Dim dr $ merge (addMerged dl dr s) l r
    
    -- restore a hidden dimension, or note a deleted dimension
    merge s (Dim d l) r
      | wasHidden d ctx = Dim d $ merge (addHidden d ctx s) l r
      | otherwise       = Dim d $ merge (addDeleted d s)    l r

    -- add a newly introduced dimension
    merge s l (Dim d r) = Dim d $ merge s l r
    
    -- ** conflict **
    
    --merge s

merge3 :: (Eq t, Eq a) => [Dim t] -> Expr t a -> Expr t a -> Expr t a -> Expr t a
merge3 = undefined


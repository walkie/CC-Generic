{-# LANGUAGE PatternGuards #-}
module Choice.Static where

import Data.List     (concatMap,delete,nub)

import Choice.Syntax
import Choice.Util hiding (delete)

-----------
-- Types --
-----------

-- mapping from names to values
type Map a = [(Name,a)]

-- mapping from dimension names to lists of tags
type Dims t = Map [t]

-- qualified tag
type QTag t = (Name,t)

-- decision
type Decision t = [QTag t]


----------------------
-- Helper Functions --
----------------------

dom = map fst
rng = map snd


---------------------------
-- Basic Static Analyses --
---------------------------

-- set of free dimensions
freeDims :: Expr t a -> [Name]
freeDims (Dim (d := _) e) = delete d (freeDims e)
freeDims (d :? es)        = nub (d : concatMap freeDims es)
freeDims e                = union (cmap freeDims e)

-- set of free variables
freeVars :: Expr t a -> [Name]
freeVars (Let (v := _) e) = delete v (freeVars e)
freeVars (Var v)          = [v]
freeVars e                = union (cmap freeVars e)

-- all dimension declarations
dims :: Expr t a -> Dims t
dims (Dim (d := ts) e) = (d,ts) : dims e
dims e                 = concat (cmap dims e)

-- is the expression dimension linear?
dimLinear :: Expr t a -> Bool
dimLinear e = ds == nub ds
  where ds = map fst (dims e)

-- is the expression well dimensioned?
wellDim :: Expr t a -> Bool
wellDim = well []
  where well :: Map Int -> Expr t a -> Bool
        well m (Dim (d := ts) e) = well ((d,length ts):m) e
        well m (d :? es)
          | Just n <- lookup d m = length es == n && all (well m) es
          | otherwise            = False
        well m e                 = all (well m) (children e)

-- is the expression dimension free?
dimFree :: Expr t a -> Bool
dimFree (Dim _ _) = False
dimFree e         = all dimFree (children e)

-- is the expression choice free?
choiceFree :: Expr t a -> Bool
choiceFree (_ :? _) = False
choiceFree e        = all choiceFree (children e)

-- is the expression binding free?
bindFree :: Expr t a -> Bool
bindFree (Let _ _) = False
bindFree e         = all bindFree (children e)

-- is the expression reference free?
refFree :: Expr t a -> Bool
refFree (Var _) = False
refFree e       = all refFree (children e)

-- is the expression variation free?
varFree :: Expr t a -> Bool
varFree e = dimFree e && choiceFree e

-- is the expression sharing free?
shareFree :: Expr t a -> Bool
shareFree e = bindFree e && refFree e

-- is the expression plain?
plain :: Expr t a -> Bool
plain e = varFree e && shareFree e

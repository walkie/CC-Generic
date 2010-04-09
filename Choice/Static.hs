{-# LANGUAGE PatternGuards #-}
module Choice.Static where

import Data.List     (concatMap,delete,nub)

import Choice.Syntax

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

union = nub . concat


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

-- is the expression variation free?
varFree :: Expr t a -> Bool
varFree (Dim _ _) = False
varFree (_ :? _)  = False
varFree e         = all varFree (children e)

-- is the expression sharing free?
shareFree :: Expr t a -> Bool
shareFree (Let _ _) = False
shareFree (Var _)   = False
shareFree e         = all shareFree (children e)

-- is the expression plain?
plain :: Expr t a -> Bool
plain e = varFree e && shareFree e

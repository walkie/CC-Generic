{-# LANGUAGE ExistentialQuantification #-}
module CC.Static where

import Data.Generics

import Data.List  (find)
import Data.Maybe (fromJust)
import Data.Set   (Set)
import qualified Data.Set as S

import CC.Syntax

--------------------------
-- Free and Bound Names --
--------------------------

-- set of bound dimensions
boundDims :: ExpT e => CC e -> Set Dim
boundDims (Dim d _ e) = S.insert d (boundDims e)
boundDims e           = ccUnionsMap boundDims e

-- set of bound variables
boundVars :: ExpT e => CC e -> Set Var
boundVars (Let v b e) = S.insert v (boundVars b `S.union` boundVars e)
boundVars e           = ccUnionsMap boundVars e

-- set of free dimensions
freeDims :: ExpT e => CC e -> Set Dim
freeDims (Dim d _ e) = S.delete d (freeDims e)
freeDims (Chc d es)  = S.insert d (S.unions (map freeDims es))
freeDims e           = ccUnionsMap freeDims e

-- set of free variables
freeVars :: ExpT e => CC e -> Set Var
freeVars (Let v b e) = S.delete v (freeVars e) `S.union` freeVars b
freeVars (Ref v)     = S.singleton v
freeVars e           = ccUnionsMap freeVars e

-- generate an unused name given a seed name, used names are given in a set
safeName :: Name -> Set Name -> Name
safeName n s = fromJust $ find (flip S.notMember s) [n ++ show i | i <- [1..]]

-- generate a variable name from a seed that won't capture any free variables
-- in the given expression
safeVar :: ExpT e => Var -> CC e -> Var
safeVar v = safeName v . freeVars

-- generate a dimension name from a seed that won't capture any free dimensions
-- in the given expression
safeDim :: ExpT e => Dim -> CC e -> Dim
safeDim d = safeName d . freeDims


--------------------------
-- X-Free and Plainness --
--------------------------

-- is the expression binding free?
bindFree :: ExpT e => CC e -> Bool
bindFree (Let _ _ _) = False
bindFree e           = ccAll bindFree e

-- is the expression reference free?
refFree :: ExpT e => CC e -> Bool
refFree (Ref _) = False
refFree e       = ccAll refFree e

-- is the expression dimension free?
dimFree :: ExpT e => CC e -> Bool
dimFree (Dim _ _ _) = False
dimFree e           = ccAll dimFree e

-- is the expression choice free?
choiceFree :: ExpT e => CC e -> Bool
choiceFree (Chc _ _) = False
choiceFree e         = ccAll choiceFree e

-- is the expression sharing free?
shareFree :: ExpT e => CC e -> Bool
shareFree e = bindFree e && refFree e

-- is the expression variation free?
variationFree :: ExpT e => CC e -> Bool
variationFree e = dimFree e && choiceFree e

-- is the expression plain?
plain :: ExpT e => CC e -> Bool
plain e = variationFree e && shareFree e


---------------------
-- Well Formedness --
---------------------

-- is the expression well dimensioned?
wellDim :: ExpT e => CC e -> Bool
wellDim = well []
  where well :: ExpT e => [(Dim,Int)] -> CC e -> Bool
        well m (Dim d ts e) = well ((d,length ts):m) e
        well m (Chc d es)   = case lookup d m of
                                 Just n    -> length es == n && all (ccAll (well m)) es
                                 otherwise -> False
        well m e = ccAll (well m) e

-- is the expression well formed?
wellFormed :: ExpT e => CC e -> Bool
wellFormed e = S.null (freeVars e) && wellDim e

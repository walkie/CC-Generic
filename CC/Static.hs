module CC.Static where

import Data.List  (find)
import Data.Maybe (fromJust)
import Data.Set   (Set)
import qualified Data.Set as S

import CC.Syntax


--------------------------
-- Free and Bound Names --
--------------------------

-- set of bound dimensions
boundDims :: CC a -> Set Dim
boundDims (Dim d _ e) = S.insert d (boundDims e)
boundDims e           = S.unions (mapSubs boundDims e)

-- set of bound variables
boundVars :: CC a -> Set Var
boundVars (Let v b e) = S.insert v (boundVars b `S.union` boundVars e)
boundVars e           = S.unions (mapSubs boundVars e)

-- set of free dimensions
freeDims :: CC a -> Set Dim
freeDims (Dim d _ e)   = S.delete d (freeDims e)
freeDims (Chc d :< es) = S.insert d (S.unions (map freeDims es))
freeDims e             = S.unions (mapSubs freeDims e)

-- set of free variables
freeVars :: CC a -> Set Var
freeVars (Let v b e) = S.delete v (freeVars e) `S.union` freeVars b
freeVars (Ref v)     = S.singleton v
freeVars e           = S.unions (mapSubs freeVars e)

-- generate an unused name given a seed name, used names are given in a set
safeName :: Name -> Set Name -> Name
safeName n s = fromJust $ find (flip S.notMember s) [n ++ show i | i <- [1..]]

-- generate a variable name from a seed that won't capture any free variables
-- in the given expression
safeVar :: Var -> CC a -> Var
safeVar v = safeName v . freeVars

-- generate a dimension name from a seed that won't capture any free dimensions
-- in the given expression
safeDim :: Dim -> CC a -> Dim
safeDim d = safeName d . freeDims


--------------------------
-- X-Free and Plainness --
--------------------------

-- is the expression binding free?
bindFree :: CC a -> Bool
bindFree (Let _ _ _) = False
bindFree e           = all bindFree (subs e)

-- is the expression reference free?
refFree :: CC a -> Bool
refFree (Ref _) = False
refFree e       = all refFree (subs e)

-- is the expression dimension free?
dimFree :: CC a -> Bool
dimFree (Dim _ _ _) = False
dimFree e           = all dimFree (subs e)

-- is the expression choice free?
choiceFree :: CC a -> Bool
choiceFree (Chc _ :< _) = False
choiceFree e            = all choiceFree (subs e)

-- is the expression sharing free?
shareFree :: CC a -> Bool
shareFree e = bindFree e && refFree e

-- is the expression variation free?
variationFree :: CC a -> Bool
variationFree e = dimFree e && choiceFree e

-- is the expression plain?
plain :: CC a -> Bool
plain e = variationFree e && shareFree e


---------------------
-- Well Formedness --
---------------------

-- is the expression well dimensioned?
wellDim :: CC a -> Bool
wellDim = well []
  where well :: [(Dim,Int)] -> CC a -> Bool
        well m (Dim d ts e)  = well ((d,length ts):m) e
        well m (Chc d :< es) = case lookup d m of
                                 Just n    -> length es == n && all (well m) es
                                 otherwise -> False
        well m e = all (well m) (subs e)

-- is the expression well formed?
wellFormed :: CC a -> Bool
wellFormed e = S.null (freeVars e) && wellDim e

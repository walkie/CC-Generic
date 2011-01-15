-- see comment on variability in CC.hs
{-# OPTIONS_GHC -cpp #-} -- -DSHARING_SEPARABLE -DSHARING_EARLY #-}

module CC.Static where

import Data.Generics

import Data.List  (find)
import Data.Maybe
import Data.Set   (Set)
import qualified Data.Set as S

import CC.Syntax
import CC.Error


--------------------------
-- Free and Bound Names --
--------------------------

-- set of bound dimensions
boundDims :: ExpT e => CC e -> Set Dim
boundDims (Dim d _ e) = S.insert d (boundDims e)
boundDims e           = ccUnionsMap boundDims e

-- set of bound variables
boundVars :: ExpT e => CC e -> Set Var
#ifdef SHARING_SEPARABLE
boundVars (Abs v e)   = S.insert v (boundVars e)
#else
boundVars (Let v b e) = S.insert v (onBnd boundVars b `S.union` boundVars e)
#endif
boundVars e           = ccUnionsMap boundVars e

-- set of free dimensions
freeDims :: ExpT e => CC e -> Set Dim
freeDims (Dim d _ e) = S.delete d (freeDims e)
freeDims (Chc d es)  = S.insert d (S.unions (map freeDims es))
freeDims e           = ccUnionsMap freeDims e

-- set of free variables
freeVars :: ExpT e => CC e -> Set Var
#ifdef SHARING_SEPARABLE
freeVars (Abs v e)   = S.delete v (freeVars e)
#else
freeVars (Let v b e) = S.delete v (freeVars e) `S.union` onBnd freeVars b
#endif
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

#ifdef SHARING_SEPARABLE
-- is the expression abstraction free?
absFree :: ExpT e => CC e -> Bool
absFree (Abs _ _) = False
absFree e         = ccAll absFree e

-- is the expression application free?
appFree :: ExpT e => CC e -> Bool
appFree (Abs _ _) = False
appFree e         = ccAll appFree e
#else
-- is the expression binding free?
bindFree :: ExpT e => CC e -> Bool
bindFree (Let _ _ _) = False
bindFree e           = ccAll bindFree e
#endif

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
#ifdef SHARING_SEPARABLE
shareFree e = absFree e && appFree e && refFree e
#else
shareFree e = bindFree e && refFree e
#endif

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
wellDim :: ExpT e => CC e -> WellDim
wellDim = well []
  where well :: ExpT e => Map Dim Int -> CC e -> WellDim
        well m (Dim d ts e) = well ((d,length ts):m) e
        well m (Chc d es)   = case lookup d m of
          Nothing -> err (UndefinedDim d)
          Just n  -> let i = length es in
                     if i /= n
                       then err (ChcArityError d i)
                       else checkAll $ map (well m) es
        well m e = checkAll $ ccMap ok (well m) e

-- are all references defined and well-typed?
wellRef :: ExpT e => CC e -> WellRef
wellRef = well []
  where tryCast :: ExpT e => CC e -> Bound -> Maybe (CC e)
        tryCast _ (Bnd b) = cast b 
        well :: ExpT e => Map Var Bound -> CC e -> WellRef
#ifdef SHARING_SEPARABLE
        -- TODO generalize this!
        well m (App (Abs v u) b) = checkAll [onBnd (well m) b, well ((v,b):m) u]
#else
        well m (Let v b u) = checkAll [onBnd (well m) b, well ((v,b):m) u]
#endif
        well m e@(Ref v)   = case lookup v m of
          Nothing -> err (UndefinedVar v)
          Just b  -> maybe (err (RefTypeError v)) (const ok) (tryCast e b)
        well m e = checkAll $ ccMap ok (well m) e

-- is the expression well formed?
wellFormed :: ExpT e => CC e -> WellFormed
wellFormed e = checkAll [fmap NotWellRef (wellRef e), fmap NotWellDim (wellDim e)]

isWellDim :: ExpT e => CC e -> Bool
isWellDim = (ok ==) . wellDim

isWellRef :: ExpT e => CC e -> Bool
isWellRef = (ok ==) . wellRef

isWellFormed :: ExpT e => CC e -> Bool
isWellFormed = (ok ==) . wellFormed

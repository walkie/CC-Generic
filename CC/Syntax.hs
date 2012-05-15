{-# LANGUAGE
      DeriveDataTypeable,
      EmptyDataDecls,
      ExistentialQuantification,
      FlexibleContexts,
      Rank2Types,
      ScopedTypeVariables,
      TypeFamilies,
      TypeOperators #-}

module CC.Syntax where 

import Control.Monad (liftM,liftM2)
import Data.Generics hiding (empty)
import Data.Set (Set,empty,unions)


------------
-- Syntax --
------------

type Name = String
type Dim = Name
type Tag = Name
type Var = Name

-- choice calculus expressions
data CC e =
    Exp e                 -- subexpressions
  | Dim Dim [Tag] (CC e)  -- dimension declaration
  | Chc Dim [CC e]        -- choice branching
  | Let Var Bound (CC e)  -- variable binding
  | Ref Var               -- variable reference
  deriving (Eq,Data,Typeable)


-----------------------
-- Bound Expressions --
-----------------------

data Bound = forall e. ExpT e => Bnd (CC e)
  deriving Typeable

-- Execute a query on a bound expression.
onBnd :: CCQ r -> Bound -> r
onBnd f (Bnd b) = f b

-- Execute a transformation in a bound expression.
inBnd :: CCT -> Bound -> Bound
inBnd f (Bnd b) = Bnd (f b)

-- Execute a monadic transformation in a bound expression.
inBndM :: Monad m => CCM m -> Bound -> m Bound
inBndM f (Bnd b) = f b >>= return . Bnd

instance Eq Bound where
  (Bnd a) == (Bnd b) = maybe False (a==) (asTypeOf (cast b) (Just a))
instance Data Bound where
  gfoldl k z (Bnd e) = z Bnd `k` e
  gunfold = error "gunfold on Bound"
  --gunfold k z _ = k (z Bnd)
  toConstr b = mkConstr (dataTypeOf b) "Bnd" [] Prefix
  dataTypeOf b = mkDataType "CC.Syntax.Bound" [toConstr b]


-----------------------------
-- Handling Subexpressions --
-----------------------------

-- type-level singleton list
data List t deriving Typeable

-- type-level left-associative cons
data l :> t deriving Typeable

-- build queries and transformations based on type-level lists of subexpression types
class Typeable t => TypeList t where
  ccQ' :: t -> r -> CCQ r -> GenericQ r
  ccT' :: t -> CCT -> GenericT
  ccM' :: Monad m => t -> CCM m -> GenericM m

instance ExpT t => TypeList (List t) where
  ccQ' _ d f = mkQ d (asTypeOf f (undefined :: CC t -> r))
  ccT' _   f = mkT   (asTypeOf f (undefined :: CC t -> CC t))
  ccM' _   f = mkM   (asTypeOf f (undefined :: CC t -> m (CC t)))

instance (ExpT t, TypeList l) => TypeList (l :> t) where
  ccQ' _ d f = ccQ' (undefined :: l) d f `extQ` asTypeOf f (undefined :: CC t -> r)
  ccT' _   f = ccT' (undefined :: l)   f `extT` asTypeOf f (undefined :: CC t -> CC t)
  ccM' _   f = ccM' (undefined :: l)   f `extM` asTypeOf f (undefined :: CC t -> m (CC t))

-- Subexpression type class, should only need to instantiate the SubExps type.
class (Data e, TypeList (SubExps e), Eq e, Show e) => ExpT e where
  type SubExps e


-------------------
-- Generic Stuff --
-------------------

type CCQ r = forall e. ExpT e => CC e -> r        -- Generic CC queries
type CCT   = forall e. ExpT e => CC e -> CC e     -- Generic CC transformations
type CCM m = forall e. ExpT e => CC e -> m (CC e) -- Generic CC monadic transformations

-- Just used for its type.
getSubExps :: ExpT e => e -> SubExps e
getSubExps = undefined

-- Construct a generic query over choice calculus expressions
-- of all subexpression types.
ccQ :: ExpT e => e -> r -> CCQ r -> GenericQ r
ccQ e r f = ccQ' (getSubExps e) r f

-- Construct a generic transformation over choice calculus expressions
-- of all subexpression types.
ccT :: ExpT e => e -> CCT -> GenericT
ccT e f = ccT' (getSubExps e) f

-- Construct a generic monadic transformation over choice calculus expressions
-- of all subexpression types.
ccM :: (ExpT e, Monad m) => e -> CCM m -> GenericM m
ccM e f = ccM' (getSubExps e) f

-- Construct a generic query over choice calculus expressions
-- from a type-specific one.
mkCCQ :: (ExpT e, ExpT f) => r -> (CC e -> r) -> CC f -> r
mkCCQ = mkQ

-- Construct a generic transformation over choice calculus expressions
-- from a type-specific one.
mkCCT :: (ExpT e, ExpT f) => (CC e -> CC e) -> CC f -> CC f
mkCCT = mkT

-- Construct a generic monadic transformation over choice calculus expressions
-- from a type-specific one.
mkCCM :: (Monad m, ExpT e, ExpT f) => (CC e -> m (CC e)) -> CC f -> m (CC f)
mkCCM = mkM


---------------------
-- Generic Queries --
---------------------

-- Execute a query everywhere, stopping recursion when the provided predicate
-- is satisfied.
queryUntil :: GenericQ Bool -> GenericQ r -> GenericQ [r]
queryUntil q f x | q x       = [f x]
                 | otherwise = f x : concat (gmapQ (queryUntil q f) x)

-- Apply a function to every immediate choice calculus subexpression of an
-- expression and collect the results.
ccMap :: ExpT e => r -> CCQ r -> CC e -> [r]
ccMap d f (Exp e)     = queryUntil (ccQ e False isCC) (ccQ e d f) e
ccMap _ f (Dim _ _ e) = [f e]
ccMap _ f (Chc _ es)  = map f es
ccMap d f (Let _ b u) = onBnd f b : [f u]
ccMap d _ (Ref _)     = [d]

-- A list-specific version of ccMap.
ccConcatMap :: ExpT e => CCQ [r] -> CC e -> [r]
ccConcatMap f = concat . ccMap [] f

-- A set-specific version of ccMap.
ccUnionsMap :: (ExpT e, Ord r) => CCQ (Set r) -> CC e -> Set r
ccUnionsMap f = unions . ccMap empty f

-- A boolean-AND-specific version of ccMap.
ccAll :: ExpT e => CCQ Bool -> CC e -> Bool
ccAll f = and . ccMap True f

-- A boolean-OR-specific version of ccMap.
ccAny :: ExpT e => CCQ Bool -> CC e -> Bool
ccAny f = or . ccMap False f


-----------------------------
-- Generic Transformations --
-----------------------------

-- Execute a transformation everywhere, stopping recursion when the provided
-- predicate is satisfied.
transUntil :: GenericQ Bool -> GenericT -> GenericT
transUntil q f x | q x       = f x
                 | otherwise = gmapT (transUntil q f) x

-- Execute a monadic transformation everywhere, stopping recursion when the
-- provided predicate is satisfied.
transUntilM :: Monad m => GenericQ Bool -> GenericM m -> GenericM m
transUntilM q f x | q x       = f x
                  | otherwise = gmapM (transUntilM q f) x

-- Apply a transformation to every immediate choice calculus subexpression.
ccTransSubs :: ExpT e => CCT -> CC e -> CC e
ccTransSubs f (Exp e)      = Exp $ transUntil (ccQ e False isCC) (ccT e f) e
ccTransSubs f (Dim d ts e) = Dim d ts (f e)
ccTransSubs f (Chc d es)   = Chc d (map f es)
ccTransSubs f (Let v b u)  = Let v (inBnd f b) (f u)
ccTransSubs _ (Ref v)      = Ref v

-- Apply a monadic transformation to every immediate choice calculus subexpression.
ccTransSubsM :: (Monad m, ExpT e) => CCM m -> CC e -> m (CC e)
ccTransSubsM f (Exp e)      = liftM Exp $ transUntilM (ccQ e False isCC) (ccM e f) e
ccTransSubsM f (Dim d ts e) = liftM (Dim d ts) (f e)
ccTransSubsM f (Chc d es)   = liftM (Chc d) (mapM f es)
ccTransSubsM f (Let v b u)  = liftM2 (Let v) (inBndM f b) (f u)
ccTransSubsM _ (Ref v)      = return (Ref v)


------------------------
-- Other Useful Stuff --
------------------------

-- mappings from k to v
type Map k v = [(k,v)]

-- dimension declaration
type Decl = (Dim,[Tag])

-- for manipulating types only
unCC :: CC e -> e
unCC = error "unCC was evaluated..."

-- for manipulating types only
unXCC :: x (CC e) -> e
unXCC = error "unXCC was evaluated..."

-- true if this is a choice calculus expression (useful in generic functions)
isCC :: CC e -> Bool
isCC _ = True

-- true if the top node is of the corresponding syntactic category
isExp, isDim, isChc, isLet, isRef :: CC e -> Bool
isExp (Exp _)     = True
isExp _           = False
isDim (Dim _ _ _) = True
isDim _           = False
isChc (Chc _ _)   = True
isChc _           = False
isLet (Let _ _ _) = True
isLet _           = False
isRef (Ref _)     = True
isRef _           = False

-- get the dimension name at this node, if applicable
getDim :: CC e -> Maybe Dim
getDim (Dim d _ _) = Just d
getDim (Chc d _)   = Just d
getDim _           = Nothing

-- get the tag list at this node, if applicable
getTags :: CC e -> Maybe [Tag]
getTags (Dim _ ts _) = Just ts
getTags _            = Nothing

-- get the dimension declaration at this node, if applicable
getDecl :: CC e -> Maybe Decl
getDecl (Dim d ts _) = Just (d,ts)
getDecl _            = Nothing

-- get the number of alternatives at this node, if applicable
getAlts :: CC e -> Maybe Int
getAlts (Chc _ es) = Just (length es)
getAlts _          = Nothing

-- get the variable name at this node, if applicable
getVar :: CC e -> Maybe Var
getVar (Let v _ _) = Just v
getVar (Ref v)     = Just v
getVar _           = Nothing

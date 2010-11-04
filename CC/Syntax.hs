{-# LANGUAGE
      DeriveDataTypeable,
      EmptyDataDecls,
      FlexibleContexts,
      Rank2Types,
      ScopedTypeVariables,
      TypeFamilies,
      TypeOperators #-}
module CC.Syntax where 

import Data.Generics
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
  | Let Var (CC e) (CC e) -- variable binding
  | Ref Var               -- variable reference
  deriving (Eq,Show,Data,Typeable)


-----------------------------
-- Handling Subexpressions --
-----------------------------

-- type-level singleton list
data List t deriving Typeable

-- type-level left-associative cons
data l :> t deriving Typeable

-- build queries and transformations based on type-level lists of subexpression types
class Typeable t => TypeList t where
  query' :: t -> r -> (forall u. ExpT u => CC u -> r) -> (forall d. Data d => d -> r)

instance (ExpT t, Typeable t) => TypeList (List t) where
  query' _ d f = mkQ d (asTypeOf f x)
    where x = undefined :: CC t -> r

instance (ExpT t, TypeList l, Typeable t) => TypeList (l :> t) where
  query' _ d f = query' (undefined :: l) d f `extQ` asTypeOf f x
    where x = undefined :: CC t -> r

-- Subexpression type class, should only need to instantiate the SubExps type.
class (Data e, TypeList (SubExps e)) => ExpT e where
  type SubExps e
  query :: e -> r -> (forall f. ExpT f => CC f -> r) -> (forall d. Data d => d -> r)
  query _ r f = query' (undefined :: SubExps e) r f


-----------------------
-- Generic Functions --
-----------------------

-- Apply a function to every immediate choice calculus subexpression of an
-- expression and collect the results.
ccMap :: ExpT e => r -> (forall f. ExpT f => CC f -> r) -> CC e -> [r]
ccMap d f (Exp e)     = gmapQ (query e d f) e
ccMap _ f (Dim _ _ e) = [f e]
ccMap _ f (Chc _ es)  = map f es
ccMap _ f (Let _ b u) = [f b, f u]
ccMap d _ (Ref _)     = [d]

-- A list-specific version of ccMap.
ccConcatMap :: ExpT e => (forall f. ExpT f => CC f -> [r]) -> CC e -> [r]
ccConcatMap f = concat . ccMap [] f

-- A set-specific version of ccMap.
ccUnionsMap :: (ExpT e, Ord r) => (forall f. ExpT f => CC f -> Set r) -> CC e -> Set r
ccUnionsMap f = unions . ccMap empty f

-- A boolean-AND-specific version of ccMap.
ccAll :: ExpT e => (forall f. ExpT f => CC f -> Bool) -> CC e -> Bool
ccAll f = and . ccMap True f

-- A boolean-OR-specific version of ccMap.
ccAny :: ExpT e => (forall f. ExpT f => CC f -> Bool) -> CC e -> Bool
ccAny f = or . ccMap False f


----------------------------
-- Other Useful Functions --
----------------------------

-- for manipulating types only
unCC :: CC e -> e
unCC = error "unCC was evaluated..."

-- for manipulating types only
unXCC :: x (CC e) -> e
unXCC = error "unXCC was evaluated..."

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

-- get the variable name at this node, if applicable
getVar :: CC e -> Maybe Var
getVar (Let v _ _) = Just v
getVar (Ref v)     = Just v
getVar _           = Nothing

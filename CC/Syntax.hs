{-# LANGUAGE DeriveDataTypeable, Rank2Types #-}
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

class Data e => ExpT e where
  extQs :: e -> r -> (forall f. ExpT f => CC f -> r) -> (forall d. Data d => d -> r)


----------------------
-- Useful Functions --
----------------------

-- Apply a function to every immediate choice calculus subexpression of an
-- expression and collect the results.
ccMap :: ExpT e => r -> (forall f. ExpT f => CC f -> r) -> CC e -> [r]
ccMap d f (Exp e)     = gmapQ (extQs e d f) e
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

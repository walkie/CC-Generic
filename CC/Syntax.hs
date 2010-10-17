{-# LANGUAGE DeriveDataTypeable #-}
module CC.Syntax where 

import Data.Generics

------------
-- Syntax --
------------

type Name = String
type Dim = Name
type Tag = Name
type Var = Name

data CC a =
    Str a          [CC a] -- structure
  | Let Var (CC a) (CC a) -- binding
  | Ref Var               -- reference
  | Dim Dim [Tag]  (CC a) -- dimension
  | Chc Dim        [CC a] -- choice
  deriving (Eq,Data,Typeable)


----------------------
-- Useful Functions --
----------------------

-- true if the top node is of the corresponding syntactic type
isStr, isLet, isRef, isDim, isChc :: CC a -> Bool
isStr (Str _ _)   = True
isStr _           = False
isLet (Let _ _ _) = True
isLet _           = False
isRef (Ref _)     = True
isRef _           = False
isDim (Dim _ _ _) = True
isDim _           = False
isChc (Chc _ _)   = True
isChc _           = False

-- true if this is a leaf node
isLeaf :: CC a -> Bool
isLeaf (Ref _)    = True
isLeaf (Str _ []) = True
isLeaf (Chc _ []) = True
isLeaf _          = False

-- smart constructor for leaf nodes
leaf :: a -> CC a
leaf a = Str a []

-- immediate subexpressions of an expression
subs :: CC a -> [CC a]
subs (Str _ es)  = es
subs (Let _ b e) = [b,e]
subs (Ref _)     = []
subs (Dim _ _ e) = [e]
subs (Chc _ es)  = es

-- replace an expression's subexpressions
replaceSubs :: CC a -> [CC a] -> CC a
replaceSubs (Str a _)   es    = Str a es
replaceSubs (Let v _ _) [b,e] = Let v b e
replaceSubs r@(Ref _)   []    = r
replaceSubs (Dim d t _) [e]   = Dim d t e
replaceSubs (Chc d _)   es    = Chc d es

-- map a function across an expression's subexpressions
mapSubs :: (CC a -> b) -> CC a -> [b]
mapSubs f = map f . subs

-- transform an expression's subexpressions
transformSubs :: (CC a -> CC a) -> CC a -> CC a
transformSubs f e = replaceSubs e (mapSubs f e)

-- monadic version of transformSubs
transformSubsM :: Monad m => (CC a -> m (CC a)) -> CC a -> m (CC a)
transformSubsM f e = sequence (mapSubs f e) >>= return . replaceSubs e


---------------
-- Instances --
---------------

instance Functor CC where
  fmap f (Str a es)  = Str (f a) (map (fmap f) es)
  fmap f (Let v b e) = Let v (fmap f b) (fmap f e)
  fmap f (Ref v)     = Ref v
  fmap f (Dim d t e) = Dim d t (fmap f e)
  fmap f (Chc d es)  = Chc d (map (fmap f) es)

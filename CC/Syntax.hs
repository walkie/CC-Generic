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

-- choice calculus expressions
data CC a =
    CCB a :< [CC a]       -- branching
  | Dim Dim [Tag]  (CC a) -- dimension declaration
  | Let Var (CC a) (CC a) -- variable binding
  | Ref Var               -- variable reference
  deriving (Eq,Data,Typeable)

-- choice calculus branch type
data CCB a = Str a   -- structural branching
           | Chc Dim -- choice branching
  deriving (Eq,Show,Data,Typeable)

----------------------
-- Useful Functions --
----------------------

-- smart constructor for structure nodes
str :: a -> [CC a] -> CC a
str a es = Str a :< es

-- smart constructor for choice nodes
chc :: Dim -> [CC a] -> CC a
chc d es = Chc d :< es

-- smart constructor for leaf nodes
leaf :: a -> CC a
leaf a = str a []

-- true if the top node is of the corresponding syntactic type
isStr, isLet, isRef, isDim, isChc :: CC a -> Bool
isStr (Str _ :< _) = True
isStr _            = False
isChc (Chc _ :< _) = True
isChc _            = False
isDim (Dim _ _ _)  = True
isDim _            = False
isLet (Let _ _ _)  = True
isLet _            = False
isRef (Ref _)      = True
isRef _            = False

-- true if this is a leaf node
isLeaf :: CC a -> Bool
isLeaf (_ :< []) = True
isLeaf (Ref _)   = True
isLeaf _         = False

-- immediate subexpressions of an expression
subs :: CC a -> [CC a]
subs (_ :< es)   = es
subs (Dim _ _ e) = [e]
subs (Let _ b e) = [b,e]
subs (Ref _)     = []

-- replace an expression's subexpressions
replaceSubs :: CC a -> [CC a] -> CC a
replaceSubs (b :< _)    es    = b :< es
replaceSubs (Dim d t _) [e]   = Dim d t e
replaceSubs (Let v _ _) [b,e] = Let v b e
replaceSubs r@(Ref _)   []    = r

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
  fmap f (b :< es)   = fmap f b :< (map (fmap f) es)
  fmap f (Dim d t e) = Dim d t (fmap f e)
  fmap f (Let v b e) = Let v (fmap f b) (fmap f e)
  fmap _ (Ref v)     = Ref v

instance Functor CCB where
  fmap f (Str a) = Str (f a)
  fmap _ (Chc d) = Chc d

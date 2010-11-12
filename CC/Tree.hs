{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}
module CC.Tree where

import Data.Generics

import CC.Syntax

-------------------
-- Vanilla Trees --
-------------------

type TreeCC a = CC (Tree a)

-- Structural branching
data Tree a = Tree a [CC (Tree a)]
  deriving (Eq,Show,Data,Typeable)

instance (Eq a, Show a, Data a) => ExpT (Tree a) where
  type SubExps (Tree a) = List (Tree a)


----------------------
-- Useful Functions --
----------------------

-- smart constructor for structural nodes
node :: a -> [TreeCC a] -> TreeCC a
node a = Exp . Tree a

-- smart constructor for leaf nodes
leaf :: a -> TreeCC a
leaf a = node a []

-- true if this is a leaf node
isLeaf :: TreeCC a -> Bool
isLeaf (Exp (Tree _ [])) = True
isLeaf (Chc _ [])        = True
isLeaf (Ref _)           = True
isLeaf _                 = False

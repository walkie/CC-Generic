{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}
module CC.Tree where

import Control.Monad (guard)
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


----------------------------------
-- To/From Generic String Trees --
----------------------------------

-- Cram data type into a choice calculus expression.
toTreeCC :: Data a => a -> TreeCC String
toTreeCC = other `extQ` leaf
  where other a = node (showConstr (toConstr a)) (gmapQ toTreeCC a)

-- Pull data type out of a *plain* choice calculus expression.
fromTreeCC :: Data a => TreeCC String -> Maybe a
fromTreeCC = other `extR` string
  where string (Exp (Tree s [])) = Just s
        other  (Exp (Tree s es)) = result
          where unM = undefined :: m a -> a -- only used for type
                result = do c <- readConstr (dataTypeOf (unM result)) s
                            let a = fromConstr c
                            guard (glength a == length es)
                            snd (gmapAccumM (\(x:xs) _ -> (xs,fromTreeCC x)) es a)
        other _ = Nothing -- CC expression is not plain

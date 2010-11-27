{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, ScopedTypeVariables #-}
module CC.Test.Zipper where

import CC.Test

import CC.Arbitrary
import CC.Show
import CC.Syntax
import CC.Tree
import CC.Zipper

----------------
-- Invariants --
----------------

prop_enterExit e = e == (exit . enter) e
  where types = e :: TreeCC Int

-----------------------
-- "Automated" Tests --
-----------------------

tests = [$(testGroupGenerator)]
runTests = defaultMain tests

instance (Arbitrary a, TreeVal a) => Arbitrary (TreeCC a) where
  arbitrary = genCC (genState 20 3 arbitrary)

--
-- QuickCheck
--


{-
-- arbitrary navigables
enterExit l = l == (exit . enter) l
leftRight l = leftEnd  l || l == (right . left ) l
rightLeft l = rightEnd l || l == (left  . right) l
upDown l = top    l || l == (down . up  ) l
downUp l = bottom l || l == (up   . down) l

-- lists
focusExit i l = l == (exit . focus i) l
focusGetFocus i l = getFocus (focus i l) == boundedIx i l


prop_list_focusExit i l = focusExit i l
  where types = l :: [Int]

prop_list_focusExit_bounded i l = focusExit (mod i n) l
  where types = l :: [Int]
        n = if null l then 1 else length l 

prop_list_focusGetFocus i l = focusGetFocus i l
  where types = l :: [Int]

prop_list_focusGetFocus_bounded i l = focusGetFocus (mod i n) l
  where types = l :: [Int]
        n = if null l then 1 else length l 

prop_list_enterExit l = enterExit l
  where types = l :: [Int]

prop_list_leftRight l = leftRight l
  where types = l :: Location [Int]

prop_list_rightLeft l = rightLeft l
  where types = l :: Location [Int]

instance Arbitrary a => Arbitrary (Context [a]) where
  arbitrary = arbitrary >>= return . uncurry ListCtx

--
-- HUnit
--
  
-- lists
empty  = (ListCtx []      []     , Nothing)
single = (ListCtx []      []     , Just 0)
lempty = (ListCtx []      [1,2,3], Just 0)
rempty = (ListCtx [2,1,0] []     , Just 3)
middle = (ListCtx [2,1,0] [4,5,6], Just 3)

testListLocs :: (Eq a, Show a) => (Location [Int] -> a) -> [a] -> Assertion
testListLocs f = (@=? map f [empty,single,lempty,rempty,middle])

case_list_exit     = testListLocs exit     [[],[0],[0..3],[0..3],[0..6]]
case_list_leftEnd  = testListLocs leftEnd  [True,True,True,False,False]
case_list_rightEnd = testListLocs rightEnd [True,True,False,True,False]
case_list_left     = testListLocs left     [empty,single,lempty,(ListCtx [1,0] [3],Just 2),(ListCtx [1,0] [3,4,5,6],Just 2)]
case_list_right    = testListLocs right    [empty,single,(ListCtx [0] [2,3],Just 1),rempty,(ListCtx [3,2,1,0] [5,6],Just 4)]

inSub 0 ([], Str 0 :< [leaf i | i <- [1..5]]) :: Maybe (Location Int)
Just ([InB (Str 0) [] [2,3,4,5]],1)
*CC.Zipper> inSub 1 ([], Str 0 :< [leaf i | i <- [1..5]]) :: Maybe (Location Int)
Just ([InB (Str 0) [1] [3,4,5]],2)
*CC.Zipper> inSub 3 ([], Str 0 :< [leaf i | i <- [1..5]]) :: Maybe (Location Int)
Just ([InB (Str 0) [3,2,1] [5]],4)
*CC.Zipper> inSub 5 ([], Str 0 :< [leaf i | i <- [1..5]]) :: Maybe (Location Int)
Nothing
*CC.Zipper> inSub 4 ([], Str 0 :< [leaf i | i <- [1..5]]) :: Maybe (Location Int)
Just ([InB (Str 0) [4,3,2,1] []],5)
*CC.Zipper> inSub (-1) ([], Str 0 :< [leaf i | i <- [1..5]]) :: Maybe (Location Int)
Nothing
-}

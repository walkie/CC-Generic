module CC.Test.Semantics where

import CC
import CC.Test
import CC.Test.Expressions


-----------------------
-- "Automated" Tests --
-----------------------

tests = test_selectTag
     ++ test_letExp
     ++ test_semantics
     ++ []

runTests = defaultMain tests


----------------
-- Test Cases --
----------------

test_selectTag = []
test_letExp = testSame "letExp" (map (letExp []) ss) (map Right ss')
test_semantics = []

aSem = [([("A","a"),("B","c")],1),([("A","a"),("B","d")],2),([("A","b"),("B","c")],3),([("A","b"),("B","d")],3)]
bSem = [([("A","a"),("B","c")],1),([("A","a"),("B","d")],2),([("A","b")],3)]

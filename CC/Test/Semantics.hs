module CC.Test.Semantics where

import CC
import CC.Tree
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

test_selectTag = [] --testSame "selectTag" [selectTag e q | (e,q) <- 

test_letExp =
    testSames "letExp" (letExp []) ["none ","good","undefVar","typeErr"]
    [bs, ss, xsv2:uvs, rts]
    [map Right bs, map Right ss', repeat (Left (undefinedVar "v")), repeat (Left (refTypeError "v"))]

test_semantics =
    testSames "semantics" semantics ["basic","share","variation","undefVar","typeErr"]
    [bs, ss, vs, xsv2:uvs, rts]
    [[Right [([],b)] | b <- bs],
     [Right [([],s)] | s <- ss'],
     map Right [ [([a],b1),([b],b1)], [([a],leaf 1),([b],leaf 2)] ],
     repeat (Left (undefinedVar "v")),
     repeat (Left (refTypeError "v"))]
  where { a = ("A","a"); b = ("A","b"); c = ("B","c"); d = ("B","d"); e = ("B","e") }

aSem = [([("A","a"),("B","c")],1),([("A","a"),("B","d")],2),([("A","b"),("B","c")],3),([("A","b"),("B","d")],3)]
bSem = [([("A","a"),("B","c")],1),([("A","a"),("B","d")],2),([("A","b")],3)]

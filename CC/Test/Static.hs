{-# LANGUAGE TemplateHaskell #-}
module CC.Test.Static where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 -- requires QuickCheck-2.1.1.1
import Test.HUnit
import Test.QuickCheck

import Data.Set (empty,fromList)

import CC
import CC.Test.Expressions


-----------------------
-- "Automated" Tests --
-----------------------

tests = $(testGroupGenerator)
      : test_boundDims
     ++ test_boundVars
     ++ test_freeDims
     ++ test_freeVars
     ++ test_bindFree
     ++ test_refFree
     ++ test_dimFree
     ++ test_choiceFree
     ++ test_shareFree
     ++ test_variationFree
     ++ test_plain
     ++ []

runTests = defaultMain tests

-- There are two implementations of this function.  The first condenses all
-- cases into a single test, which makes for nicer test output.  The second
-- runs each case separately, which is useful for debugging a failed test
testSame n as es = [testCase n $ as @?= take (length as) es]
{-
testSame n as es = zipWith testCase names (zipWith (@?=) as es)
  where names = [n ++ ' ' : show i | i <- [0..]]
-}

testNoneSome n f nones somes rs =
       testSame (n ++ " none") (map f nones) (repeat empty)
    ++ testSame (n ++ " some") (map f somes) (map fromList rs)

testAllNone n f alls nones =
       testSame (n ++ " yes") (map f alls)  (repeat True)
    ++ testSame (n ++ " no ") (map f nones) (repeat False)


test_boundDims = testNoneSome "boundDims" boundDims dfs ndfs
                 [a,ab,                -- ud2,ud3
                  a,a,a,ab,ab,ab,ab,a, -- vs
                  a,a,a,a,             -- svs ++ xsvs
                  a,b,a,a,a]           -- ces
  where { a = ["A"]; ab = ["A","B"]; b = ["B"] }

test_boundVars = testNoneSome "boundVars" boundVars bfs nbfs
                 [v,v,       -- tail uvs
                  v,uv,uv,v, -- ss
                  v,v,v,v]   -- svs ++ xsvs
  where { v = ["v"]; uv = ["u","v"] }

test_freeDims = testNoneSome "freeDims" freeDims (xsv2 : wfs ++ nwrs) (xsv1 : uds)
                (replicate 4 ["A"])

test_freeVars = testNoneSome "freeVars" freeVars (wfs ++ nwds) uvs
                (replicate 4 ["v"])

test_bindFree      = testAllNone "bindFree"      bindFree      bfs nbfs
test_refFree       = testAllNone "refFree"       refFree       rfs nrfs
test_dimFree       = testAllNone "dimFree"       dimFree       dfs ndfs
test_choiceFree    = testAllNone "choiceFree"    choiceFree    cfs ncfs
test_shareFree     = testAllNone "shareFree"     shareFree     sfs nsfs
test_variationFree = testAllNone "variationFree" variationFree vfs nvfs
test_plain         = testAllNone "plain"         plain         ps  nps

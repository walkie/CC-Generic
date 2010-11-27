{-# LANGUAGE TemplateHaskell #-}
module CC.Test.Static where

import CC
import CC.Test
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

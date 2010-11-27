module CC.Test.Static where

import CC
import CC.Test
import CC.Test.Expressions


-----------------------
-- "Automated" Tests --
-----------------------

tests = test_boundDims
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
     ++ test_wellDim
     ++ test_wellRef
     ++ test_wellFormed
     ++ []

runTests = defaultMain tests


----------------
-- Test Cases --
----------------

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

test_freeDims = testNoneSome "freeDims" freeDims (wfs ++ nwrs) (xsv1 : uds)
                (repeat ["A"])

test_freeVars = testNoneSome "freeVars" freeVars (wfs ++ nwds) (xsv2 : uvs)
                (repeat ["v"])

test_bindFree      = testAllNone "bindFree"      bindFree      bfs nbfs
test_refFree       = testAllNone "refFree"       refFree       rfs nrfs
test_dimFree       = testAllNone "dimFree"       dimFree       dfs ndfs
test_choiceFree    = testAllNone "choiceFree"    choiceFree    cfs ncfs
test_shareFree     = testAllNone "shareFree"     shareFree     sfs nsfs
test_variationFree = testAllNone "variationFree" variationFree vfs nvfs
test_plain         = testAllNone "plain"         plain         ps  nps

test_wellDim    = testSames "wellDim" wellDim
                  ["ok", "UndefinedDim", "ChcArityError"]
                  [wds, xsv1:uds, ces]
                  [repeat ok, repeat (err (UndefinedDim "A")),
                    [err (ChcArityError d i) | (d,i) <- [("A",1),("B",2),("A",3),("A",1),("A",1)]]]
                 
test_wellRef    = testSames' "wellRef" wellRef
                  ["ok", "UndefinedRef", "RefTypeError"]
                  [wrs, xsv2:uvs, rts]
                  [ok, err (UndefinedVar "v"), err (RefTypeError "v")]

test_wellFormed = testAllNone "wellFormed" isWellFormed wfs nwfs

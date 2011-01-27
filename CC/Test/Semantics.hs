module CC.Test.Semantics where

import CC
import CC.Tree
import CC.Test
import CC.Test.Expressions


-----------------------
-- "Automated" Tests --
-----------------------

tests = test_selectTag
     ++ test_expand
     ++ test_semantics
     ++ []

runTests = defaultMain tests


----------------
-- Test Cases --
----------------

test_selectTag = [] --testSame "selectTag" [selectTag e q | (e,q) <- 

test_expand =
    testSames "expand" (expand []) ["none ","good","undefVar","typeErr"]
    [bs, ss, xsv2:uvs, rts]
    [map Right bs, map Right ss', repeat (Left (undefinedVar "v")), repeat (Left (refTypeError "v"))]

test_semantics =
    testSames "semantics" semantics ["basic","share","variation","both","undefVar","typeErr","undefDim","chcArity"]
    [bs, ss, vs, svs, xsv2:uvs, rts, uds, ces]
    [[Right [([],b)] | b <- bs],
     [Right [([],s)] | s <- ss'],
     map Right [ [([a],b1),([b],b1)], [([a],leaf 1),([b],leaf 2)], -- v0, v1
                 [([a], node 0 [leaf 1, leaf 3]), ([b], node 0 [leaf 2, leaf 4])], -- v2
                 x, x, -- v3, v4
                 [([a,c],leaf 3),([a,d],leaf 4),([a,e],leaf 5),
                  ([b,c],leaf 6),([b,d],leaf 6),([b,e],leaf 6)], -- v5
                 [([a,c],leaf 3),([a,d],leaf 4),([a,e],leaf 5),([b],leaf 6)], -- v6
                 [([a,a], node 0 [leaf 1, leaf 3]),
                  ([a,b], node 0 [leaf 1, leaf 4]),
                  ([b,a], node 0 [leaf 2, leaf 3]),
                  ([b,b], node 0 [leaf 2, leaf 4])] -- v7
               ],
     map Right [ [([a], node 0 [leaf 1, leaf 1]), ([b], node 0 [leaf 2, leaf 2])], -- sv1
                 [([a,a],leaf 3),([a,b],leaf 1),([b,a],leaf 3),([b,b],leaf 2)] -- sv2
               ],
     repeat (Left (undefinedVar "v")),
     repeat (Left (refTypeError "v")),
     map (Left . NotWellFormed . NotWellDim) udsEs,
     map (Left . NotWellFormed . NotWellDim) cesEs]
  where { a = ("A","a"); b = ("A","b"); c = ("B","c"); d = ("B","d"); e = ("B","e");
          x = [([a,c], node 0 [leaf 1, leaf 3]),
               ([a,d], node 0 [leaf 1, leaf 4]),
               ([a,e], node 0 [leaf 1, leaf 5]),
               ([b,c], node 0 [leaf 2, leaf 3]),
               ([b,d], node 0 [leaf 2, leaf 4]),
               ([b,e], node 0 [leaf 2, leaf 5])]
        }

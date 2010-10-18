module CC.Test.Expressions where

import CC.Syntax
import CC.Pretty

exprs :: [CC Int]
exprs = [b1,b2,b3,
         s1,s2,s3,
         v1,v2,v3,v4,v5]

-- basic expressions
b1 = leaf 1
b2 = str 1 [leaf 2, leaf 3]
b3 = str 4 [b2, leaf 5]

-- expressions with sharing
s1 = Let "v" (leaf 0) (Ref "v")
s1' = leaf 0

s2 = Let "u" s1 $ str 2 [b2, Ref "u"]
s2' = str 2 [b2, s1']

s3 = Let "v" s2 $ str 3 [b3, Ref "v"]
s3' = str 3 [b3, s2']

-- expressions with variation
dimA = Dim "A" ["a","b"]
dimB = Dim "B" ["c","d"]
dimC = Dim "C" ["e","f","g"]
v1 = dimA $ chc "A" [leaf 1, leaf 2]
v2 = dimB $ chc "B" [v1, v1]
v3 = dimA $ chc "A" [v1, leaf 3]
v4 = dimA $ str 0 [chc "A" [leaf 1, leaf 2], chc "A" [leaf 3, leaf 4]]
v5 = dimA $ str 0 [chc "A" [v2, leaf 3], str 4 [chc "A" [leaf 5, leaf 6]]]

-- expressions with both sharing and variation


module CC.Test.Expressions where

import CC.Syntax
import CC.Pretty
import CC.Semantics

exprs :: [CC Int]
exprs = [b1,b2,b3,
         s1,s2,s3,
         s1',s2',s3',
         v1,v2,v3,v4,v5,v6,v7]

-- 
-- basic expressions
--

b1 = leaf 1
b2 = str 1 [leaf 2, leaf 3]
b3 = str 4 [b2, leaf 5]

-- 
-- expressions with sharing
--

s1 = Let "v" (leaf 0) (Ref "v")
s1' = leaf 0

s2 = Let "u" s1 $ str 2 [b2, Ref "u"]
s2' = str 2 [b2, s1']

s3 = Let "v" s2 $ str 3 [b3, Ref "v"]
s3' = str 3 [b3, s2']

--
-- expressions with variation
--

dimA = Dim "A" ["a","b"]
dimB = Dim "B" ["c","d","e"]
ca1 = chc "A" [leaf 1, leaf 2]
ca2 = chc "A" [leaf 3, leaf 4]
cb  = chc "B" [leaf 3, leaf 4, leaf 5]

-- well-dimensioned expressions
v1 = dimA ca1
v2 = dimA $ str 0 [ca1, ca2]

v3 = dimA $ dimB $ str 0 [ca1,cb]
v4 = str 0 [dimA ca1, dimB cb]

v5 = dimA $ dimB $ chc "A" [cb, leaf 5]
v6 = dimA $ chc "A" [dimB cb, leaf 5]

v7 = dimA $ str 0 [ca1, dimA ca2]

-- not well-dimensioned expressions

-- expressions with both sharing and variation


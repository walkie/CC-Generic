module CC.Test.Expressions where

import CC
import CC.Tree

exprs :: [TreeCC Int]
exprs = bs ++ ss ++ vs

-- 
-- basic expressions
--

bs :: [TreeCC Int]
bs = [b1,b2,b3,s1',s2',s3']

b1 = leaf 1
b2 = node 1 [leaf 2, leaf 3]
b3 = node 4 [b2, leaf 5]

-- 
-- expressions with sharing
--

ss :: [TreeCC Int]
ss = [s1,s2,s3]

s1 :: TreeCC Int
s1 = Let "v" (Bnd b1) (Ref "v")
s1' = b1

s2 = Let "u" (Bnd s1) $ node 2 [b2, Ref "u"]
s2' = node 2 [b2, s1']

s3 = Let "v" (Bnd s2) $ node 3 [b3, Ref "v"]
s3' = node 3 [b3, s2']

--
-- expressions with variation
--

vs :: [TreeCC Int]
vs = [v1,v2,v3,v4,v5,v6,v7]

dimA = Dim "A" ["a","b"]
dimB = Dim "B" ["c","d","e"]
ca1 = Chc "A" [leaf 1, leaf 2]
ca2 = Chc "A" [leaf 3, leaf 4]
cb  = Chc "B" [leaf 3, leaf 4, leaf 5]

-- well-dimensioned expressions
v1 = dimA ca1
v2 = dimA $ node 0 [ca1, ca2]

v3 = dimA $ dimB $ node 0 [ca1,cb]
v4 = node 0 [dimA ca1, dimB cb]

v5 = dimA $ dimB $ Chc "A" [cb, leaf 5]
v6 = dimA $ Chc "A" [dimB cb, leaf 5]

v7 = dimA $ node 0 [ca1, dimA ca2]

-- not well-dimensioned expressions

-- expressions with both sharing and variation


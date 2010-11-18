module CC.Test.Expressions where

import CC
import CC.Tree


------------------------
-- Smart Constructors --
------------------------

dimA = Dim "A" ["a","b"]               :: TreeCC a -> TreeCC a
dimB = Dim "B" ["c","d","e"]           :: TreeCC a -> TreeCC a
ca1 = Chc "A" [leaf 1, leaf 2]         :: TreeCC Int
ca2 = Chc "A" [leaf 3, leaf 4]         :: TreeCC Int
cb  = Chc "B" [leaf 3, leaf 4, leaf 5] :: TreeCC Int


------------------
-- The Big Sets --
------------------

exprs,wfs,nwfs,nwds,nwrs :: [TreeCC Int]

-- everything
exprs = wfs ++ nwfs

-- all well formed expressions
wfs = bs ++ ss ++ vs

-- all not well formed expressions
nwfs = nwds ++ nwrs

-- all not well-referenced expressions
nwrs = xss

-- all not well-dimensioned expressions
nwds = xvs


-----------------------
-- Basic Expressions --
-----------------------

bs = [b1,b2,b3,s2',s3']

b1 = leaf 1                  :: TreeCC Int
b2 = node 1 [leaf 2, leaf 3] :: TreeCC Int
b3 = node 4 [b2, leaf 5]     :: TreeCC Int


-------------
-- Sharing --
-------------

-- well-ref'ed
ss = [s1,s2,s3]

s1 = Let "v" (Bnd b1) (Ref "v") :: TreeCC Int
s1' = b1

s2 = Let "u" (Bnd s1) $ node 2 [b2, Ref "u"]
s2' = node 2 [b2, s1']

s3 = Let "v" (Bnd s2) $ node 3 [s1, Ref "v"]
s3' = node 3 [s1', s2']

-- not well-ref'ed
xss = uvs ++ rts

-- undefined variables
uvs = [uv1,uv2,uv3,uv4]
uv1 = Ref "v" :: TreeCC Int
uv2 = Let "v" (Bnd uv1) b1
uv3 = node 0 [Let "v" (Bnd b1) (leaf 2), uv1]
uv4 = dimA $ Chc "A" [Let "v" (Bnd b1) (leaf 2), uv1]

-- ref type errors
rts = [rt1,rt2]
rt1 = Let "v" (Bnd (leaf True)) uv1
rt2 = Let "v" (Bnd b1) rt1


---------------
-- Variation --
---------------

vs = [v1,v2,v3,v4,v5,v6,v7]

v1 = dimA ca1
v2 = dimA $ node 0 [ca1, ca2]

v3 = dimA $ dimB $ node 0 [ca1,cb]
v4 = node 0 [dimA ca1, dimB cb]

v5 = dimA $ dimB $ Chc "A" [cb, leaf 5]
v6 = dimA $ Chc "A" [dimB cb, leaf 5]

v7 = dimA $ node 0 [ca1, dimA ca2]

-- not well-dim'ed
xvs = uds ++ ces

-- undefined dims
uds = [ud1,ud2,ud3]
ud1 = ca1
ud2 = node 0 [dimA b1, ca1]
ud3 = dimB $ Chc "B" [dimA b1, ca1, leaf 3]

-- choice arity error
ces = [ce1,ce2,ce3,ce4,ce5]
ce1 = dimA $ Chc "A" [b1]
ce2 = dimB $ Chc "B" [b1, leaf 2]
ce3 = dimA $ Chc "A" [b1, leaf 2, leaf 3]
ce4 = dimA $ Chc "A" [ce1, leaf 2]
ce5 = dimA $ Chc "A" [Chc "A" [b1], leaf 2]

-------------------------
-- Sharing + Variation --
-------------------------


module CC.Test.Expressions where

import CC
import CC.Tree

import Data.List ((\\),intersect,union)

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

-- well formed expressions
wfs = bs ++ ss ++ vs ++ svs
nwfs = nwds ++ nwrs

-- well referenced expressions
wrs = exprs \\ nwrs
nwrs = xsv2 : xss

-- well dimensioned expressions
wds = exprs \\ nwds
nwds = xsv1 : xvs

-- binding free
bfs = uv1 : bs ++ vs ++ xvs
nbfs = tail uvs ++ ss ++ svs ++ xsvs

-- reference free
rfs = s4 : bs ++ vs ++ xvs
nrfs = s1 : s2 : s3 : xss ++ svs ++ xsvs

-- dimension free
dfs = ud1 : bs ++ ss ++ xss
ndfs = ud2 : ud3 : vs ++ svs ++ xsvs ++ ces

-- choice free
cfs = v0 : bs ++ ss ++ xss
ncfs = tail vs ++ svs ++ xvs ++ svs ++ xsvs

-- sharing free
sfs = bfs `intersect` rfs
nsfs = nbfs `union` nrfs

-- variation free
vfs = dfs `intersect` cfs
nvfs = ndfs `intersect` ndfs

-- plain
ps = sfs `intersect` vfs
nps = nsfs `intersect` nvfs


-----------------------
-- Basic Expressions --
-----------------------

bs = [b1,b2,b3,s2',s3',s4']

b1 = leaf 1                  :: TreeCC Int
b2 = node 1 [leaf 2, leaf 3] :: TreeCC Int
b3 = node 4 [b2, leaf 5]     :: TreeCC Int


-------------
-- Sharing --
-------------

-- well-ref'ed
ss  = [s1 ,s2 ,s3 ,s4 ]
ss' = [s1',s2',s3',s4']

s1 = Let "v" (Bnd b1) (Ref "v") :: TreeCC Int
s1' = b1

s2 = Let "u" (Bnd s1) $ node 2 [b2, Ref "u"]
s2' = node 2 [b2, s1']

s3 = Let "v" (Bnd s2) $ node 3 [s1, Ref "v"]
s3' = node 3 [s1', s2']

s4 = Let "v" (Bnd b1) b2
s4' = b2

-- not well-ref'ed
xss = uvs ++ rts

-- undefined variables
uvs = [uv1,uv2,uv3]
uv1 = Ref "v" :: TreeCC Int
uv2 = Let "v" (Bnd uv1) b1
uv3 = node 0 [Let "v" (Bnd b1) (leaf 2), uv1]

-- ref type errors
rts = [rt1,rt2]
rt1 = Let "v" (Bnd (leaf True)) uv1
rt2 = Let "v" (Bnd b1) rt1


---------------
-- Variation --
---------------

vs = [v0,v1,v2,v3,v4,v5,v6,v7]

v0 = dimA b1
v1 = dimA ca1
v2 = dimA $ node 0 [ca1, ca2]

v3 = dimA $ dimB $ node 0 [ca1,cb]
v4 = node 0 [dimA ca1, dimB cb]

v5 = dimA $ dimB $ Chc "A" [cb, leaf 6]
v6 = dimA $ Chc "A" [dimB cb, leaf 6]

v7 = dimA $ node 0 [ca1, dimA ca2]

-- not well-dim'ed
xvs = [ud1,ud2,ud3] ++ ces

-- undefined dims
uds = [ud1,ud2,ud3,xsv1]
udsEs = replicate 4 (UndefinedDim "A")

ud1 = ca1
ud2 = node 0 [dimA b1, ca1]
ud3 = dimB $ Chc "B" [dimA b1, ca1, leaf 3]

-- choice arity error
ces = [ce1,ce2,ce3,ce4,ce5]
cesEs = [ChcArityError d i | (d,i) <- [("A",1),("B",2),("A",3),("A",1),("A",1)]]

ce1 = dimA $ Chc "A" [b1]
ce2 = dimB $ Chc "B" [b1, leaf 2]
ce3 = dimA $ Chc "A" [b1, leaf 2, leaf 3]
ce4 = dimA $ Chc "A" [ce1, leaf 2]
ce5 = dimA $ Chc "A" [Chc "A" [b1], leaf 2]

-------------------------
-- Sharing + Variation --
-------------------------

svs = [sv1,sv2]
sv1 = Let "v" (Bnd v1) $ node 0 [Ref "v", Ref "v"] :: TreeCC Int
sv2 = dimA $ Let "v" (Bnd ca1) $ dimA $ Chc "A" [ca2, Ref "v"]

-- not well-formed
xsvs = [xsv1,xsv2]
xsv1 = Let "v" (Bnd ca1) $ dimA (Ref "v") :: TreeCC Int
xsv2 = dimA $ Chc "A" [Let "v" (Bnd b1) (leaf 2), uv1]


module RCS where

import Choice

type Id   = Int


flatten :: Expr t a -> Expr t a
flatten (a :< es) = a :< concatMap flat es
  where flat (a :< es) = leaf a : concatMap flat es

--
-- Patch dimension and tag names
--

pn :: Id -> Name
pn i = "P" ++ show i

pt :: Id -> QTag Bool
pt i = (pn i, True)

pf :: Id -> QTag Bool
pf i = (pn i, False)

pdim :: Id -> Expr Bool a -> Expr Bool a
pdim i = Dim (pn i := [True,False])


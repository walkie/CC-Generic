
module Patch where

import Data.Maybe (fromJust)

import Choice
import Edit

--
-- Types
--

type Id    = Int
type PExpr = Expr Bool -- patch expressions

data Change a = Change Path (PExpr a -> PExpr a)

data Patch a = Patch Id [Change a]

--
-- Patch dimension and tag names
--

-- patch names
pn :: Id -> Name
pn i = "P" ++ show i

-- qualified patch tags
pt :: Id -> QTag Bool
pt i = (pn i, True)

pf :: Id -> QTag Bool
pf i = (pn i, False)

-- boolean dimension declaration
bdim :: Name -> Expr Bool a -> Expr Bool a
bdim n = Dim (n := [True,False])

-- patch dimension declarations
pdim :: Id -> PExpr a -> PExpr a
pdim = bdim . pn


--
-- Applying patches
-- 

change :: Id -> Change a -> PExpr a -> Maybe (PExpr a)
change i (Change p f) e = fmap chg (follow p e)
  where chg (c,e') = c $ Let (v:=e') $ pn i :? [f (Var v), Var v]
        v = "$" ++ show i

apply :: Patch a -> PExpr a -> Maybe (PExpr a)
apply (Patch i cs) e = fmap (pdim i) $ foldMaybe (change i) (Just e) cs

applyAll :: [Patch a] -> PExpr a -> Maybe (PExpr a)
applyAll ps e = foldMaybe apply (Just e) ps

--
-- Example
--

ins :: String -> Expr t String -> Expr t String
ins []     e = e
ins (c:cs) e = ":" :< [leaf [c], ins cs e]

str :: String -> Expr t String
str = flip ins (leaf "")

-- patch structure:
--   e
--   `-p1
--      |-p2
--      |  `-p3'
--      `-p3
--      `-p4 -- conflicts with p2

e :: PExpr String
e = str ""

p1 = Patch 1 [Change [] (ins "ab")]

p2 = Patch 2 [Change path (ins "x")]
  where path = [D, B True, C 0, S 1]

p3 = Patch 3 [Change path (ins "c")]
  where path = [D, B True, C 0, S 1, S 1]

p3' = Patch 3 [Change path (ins "c")]
  where path = [D, D, B True, C 0, S 1, B False, S 1]

p4 = Patch 4 [Change path (ins "y")]
  where path = [D, B True, C 0, S 1]

p5 = Patch 5 [Change path (ins "d")]
  where path = [D, B True, C 0, S 1, S 1]

ab   = fromJust $ apply p1 e
axb  = fromJust $ applyAll [p1,p2] e
ayb  = fromJust $ applyAll [p1,p4] e
abc  = fromJust $ applyAll [p1,p3] e
abd  = fromJust $ applyAll [p1,p5] e
--axbc = fromJust $ applyAll [p1,p2,p3'] e

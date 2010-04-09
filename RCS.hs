{-# LANGUAGE PatternGuards #-}

module RCS where

import Data.List  (elemIndex)
import Data.Maybe (fromJust,isJust)

import Choice hiding (Map)

---------------
-- Tag Trees --
---------------

type Map k v   = [(k,v)]
type TagTree t = Map (QTag t) (QTag t)

selectTT :: Eq t => TagTree t -> QTag t -> Expr t a -> Maybe (Expr t a)
selectTT tt q e = foldMaybe select (Just e) (path tt q)

path :: Eq t => TagTree t -> QTag t -> [QTag t]
path tt = reverse . initJusts . iterMaybe (flip lookup tt)

-----------------------
-- Utility functions --
-----------------------

initJusts :: [Maybe a] -> [a]
initJusts = map fromJust . takeWhile isJust

iterMaybe :: (a -> Maybe a) -> a -> [Maybe a]
iterMaybe f = iterate (maybe Nothing f) . Just

--------------
-- Examples --
--------------

p :: Int -> Name
p n = "P" ++ show n

patch :: Int -> QTag Bool
patch n = (p n, True)

patchDim :: Int -> Expr Bool a -> Expr Bool a
patchDim n = Dim ((p n) := [True,False])

-- 1
-- `-> 2
--     `-> 3
--     `-> 4
patchTree :: TagTree Bool
patchTree = [(patch 3, patch 2), (patch 2, patch 1), (patch 4, patch 2)]

ex0 :: Expr Bool String
ex0 = "" :< [leaf "aa",
             leaf "bb",
             leaf "cc"]

ex1 = patchDim 1
    $ "" :< [leaf "aa",
             p 1 :? [leaf "BB", leaf "bb"],
             leaf "cc"]

-- Would be easier to just put new dimension decl at the top, but my
-- implementation doesn't yet handle contexts correctly, so decls must
-- be added in a particular order for now...
ex2 = patchDim 1
    $ patchDim 2
    $ "" :< [leaf "aa",
             p 1 :? [leaf "BB", leaf "bb"],
             p 2 :? [leaf "CC", leaf "cc"]]

ex3 = patchDim 1
    $ patchDim 2
    $ patchDim 3
    $ "" :< [leaf "aa",
             p 1 :? [p 3 :? [leaf "$$", leaf "BB"], leaf "bb"],
             p 2 :? [leaf "CC", leaf "cc"]]

ex4 = patchDim 1
    $ patchDim 2
    $ patchDim 3
    $ patchDim 4
    $ "" :< [leaf "aa",
             p 1 :? [p 3 :? [leaf "$$", 
                     p 4 :? [leaf "££", leaf "BB"]], leaf "bb"],
             p 2 :? [leaf "CC", leaf "cc"]]

{-# LANGUAGE PatternGuards #-}

module Choice.Semantics where

import Data.List  (elemIndex,nub)
import Data.Maybe (fromMaybe)

import Choice.Syntax
import Choice.Static
import Choice.Util


-----------
-- Types --
-----------

type Selector      = QTag Int
type Semantics t a = [(Decision t, Expr t a)]
type Context t a   = Expr t a -> Expr t a
type Match t a     = Maybe (Context t a, Expr t a)


---------------
-- Selection --
---------------

decide :: Eq t => Decision t -> Expr t a -> Maybe (Expr t a)
decide d e = foldMaybe select (Just e) d

select :: Eq t => QTag t -> Expr t a -> Maybe (Expr t a)
select (d,t) e | Just (c, (Dim (_ := ts) e')) <- match decl e
               , Just i                       <- elemIndex t ts
               = (Just . c . selectIx (d,i)) e'
               | otherwise = Nothing
  where decl (Dim (d' := _) _) = d == d'
        decl _                 = False

selectIx :: Selector -> Expr t a -> Expr t a
selectIx (d,i) (d' :? es)          | d == d' = selectIx (d,i) (es !! i)
selectIx (d,i) e@(Dim (d' := _) _) | d == d' = e
selectIx s e = tcmap (selectIx s) e

-- match a context
match :: (Expr t a -> Bool) -> Expr t a -> Match t a
match f = m id
  where 
    m c e | f e           = Just (c,e)
    m c (Var v)           = Nothing
    m c (a :< es)         = leftMost f (c . (a:<)) es
    m c (Let (v := a) b)  = firstJust [m (\e -> c (Let (v := e) b)) a,
                                       m (c . Let (v := a)) b]
    m c (Dim (d := ts) a) = m (\e -> c (Dim (d := ts) e)) a
    m c (d :? es)         = leftMost f (c . (d:?)) es
        
    leftMost f c' es = firstJust [m (\e -> c' (replace i es e)) (es !! i)
                                 | i <- [0..length es-1]]


----------------------
-- Choice Semantics --
----------------------

-- all decisions excluding non-significant reorderings (needs to be tested)
decs :: Eq t => Dims t -> Expr t a -> [Decision t]
decs m (Dim (d := ts) e) = concatMap fix xs
  where xs = decs ((d,ts):m) e
        -- pick arbitrary tags if we haven't eliminated this dimension
        fix x = if elem d (map fst x) then [x] else [(d,t):x | t <- ts]
decs m (d :? es) | Just ts <- lookup d m = concat $ zipWith3 branch ts es [0..]
        -- select t in alternative to prevent duplicate entries in the decision
  where branch t e i = [(d,t):x | x <- decs m (selectIx (d,i) e)]
decs m e = if null xs then [[]] else xs
  where xs = (nub . concat . map (decs m) . children) e

-- eliminate let expressions
expand :: Map (Expr t a) -> Expr t a -> Expr t a
expand m e@(Var v)         = fromMaybe e (lookup v m)
expand m (Let (v := e) e') = expand ((v,expand m e):m) e'
expand m e                 = tcmap (expand m) e

-- eliminate all let expressions and variables
makeShareFree :: Expr t a -> Maybe (Expr t a)
makeShareFree e | shareFree e' = Just e'
                | otherwise    = Nothing
  where e' = expand [] e

-- decisions to variation free expressions
variants :: Eq t => Expr t a -> Semantics t a
variants e = map entry (decs [] e)
  where entry d | Just e' <- decide d e = (d,e')

-- decisions to plain expressions
semantics :: Eq t => Expr t a -> Semantics t a
semantics e = map entry (variants e)
  where entry (d,e) | Just e' <- makeShareFree e = (d,e')


bug  = dim "A" [True,False]
     $ dim "B" [True,False]
     $ "A" :? ["B" :? [leaf 1,leaf 2], leaf 3]

bug' = dim "A" [True,False]
     $ "A" :? [dim "B" [True,False] ("B" :? [leaf 1,leaf 2]), leaf 3]

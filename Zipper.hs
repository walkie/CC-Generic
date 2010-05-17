{-# LANGUAGE MultiParamTypeClasses, PatternGuards #-}

module Zipper where

import Maybe (fromMaybe)

import Choice hiding (Context)

--
-- Generic zipper stuff.
--

data Zipper f a = Z (Context f) a deriving (Eq,Show)

data Context f = Context f :> f
               | Top
               deriving (Eq,Show)

type Step f a = Zipper f a -> Zipper f a

class Focus f a where
  apply :: f -> a -> a

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

enter :: a -> Zipper f a
enter = Z Top

exit :: Focus f a => Zipper f a -> a
exit (Z Top a) = a
exit z         = exit (up z)

exec :: Focus f a => Step f a -> a -> a
exec s = enter .> s .> exit

up :: Focus f a => Step f a
up (Z Top      a) = Z Top a
up (Z (c :> f) a) = Z c (apply f a)


--
-- Choice calculus specific stuff.
--

data CCFocus t a = CCF (Map Int) (ExprFocus t a)

data ExprFocus t a =
    D (Bind [t])
  | C Name [Expr t a] [Expr t a]
  | S a    [Expr t a] [Expr t a]
  | B Name (Expr t a) 
  | U (Bind (Expr t a))
  deriving (Eq,Show)

type CCContext t a = Context (CCFocus t a)
type CCStep    t a = Step    (CCFocus t a) (Expr t a)

instance Focus (CCFocus t a) (Expr t a) where
  apply (CCF _ f) e = apply' f e 

apply' :: ExprFocus t a -> Expr t a -> Expr t a
apply' (D b)       e = Dim b e
apply' (C d as bs) e = d :? (as ++ e:bs)
apply' (S a as bs) e = a :< (as ++ e:bs)
apply' (B v e')    e = Let (v:=e) e'
apply' (U b)       e = Let b e

-- Navigating CC expressions.

dimNs :: CCContext t a -> Map Int
dimNs Top            = []
dimNs (_ :> CCF m _) = m

zoom :: CCContext t a -> ExprFocus t a -> CCContext t a
zoom c f = c :> CCF (dimNs c) f

inDim :: CCStep t a
inDim (Z c (Dim b e)) = Z c' e
  where c' = c :> CCF ((var b, length (val b)) : dimNs c) (D b)

inAlt :: Int -> CCStep t a
inAlt i (Z c (d :? es)) = Z (c `zoom` C d as bs) b
  where (as,b:bs) = splitAt i es

inSub :: Int -> CCStep t a
inSub i (Z c (a :< es)) = Z (c `zoom` S a as bs) b
  where (as,b:bs) = splitAt i es

inBind :: CCStep t a
inBind (Z c (Let (v:=b) u)) = Z (c `zoom` B v u) b

inUse :: CCStep t a
inUse (Z c (Let b u)) = Z (c `zoom` U b) u

-- Editing CC expressions.

addDim :: Bind [t] -> CCStep t a
addDim b (Z c e) = Z c (Dim b e)

addChoice :: Name -> [Maybe (Expr t a)] -> CCStep t a
addChoice d as (Z c e)
    | Just n <- lookup d (dimNs c)
    , n == length as
    = Z c $ Let (v := e) (d :? map (fromMaybe (Var v)) as)
  where v = safeVar "$" e

addTag :: t -> CCStep t a
addTag = undefined

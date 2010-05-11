{-# LANGUAGE MultiParamTypeClasses #-}

module Zipper where

import Choice hiding (Context)

--
-- Generic zipper stuff.
--

data Zipper f a = Zipper (Context f) a deriving (Eq,Show)

data Context f = Context f :> f
               | Top
               deriving (Eq,Show)

type Step f a = Zipper f a -> Zipper f a

class Focus f a where
  apply :: f -> a -> a

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

enter :: a -> Zipper f a
enter = Zipper Top

exit :: Focus f a => Zipper f a -> a
exit (Zipper Top a) = a
exit z              = exit (up z)

exec :: Focus f a => Step f a -> a -> a
exec s = enter .> s .> exit

up :: Focus f a => Step f a
up (Zipper Top      a) = Zipper Top a
up (Zipper (c :> f) a) = Zipper c (apply f a)


--
-- Choice calculus specific stuff
--

data CCFocus t a =
    D {- DimUse  -} (Bind [t])
  | C {- Alt     -} Name [Expr t a] [Expr t a]
  | S {- SubObj  -} a    [Expr t a] [Expr t a]
  | B {- LetBind -} Name (Expr t a) 
  | U {- LetUse  -} (Bind (Expr t a))
  deriving (Eq,Show)

instance Focus (CCFocus t a) (Expr t a) where
  apply (D b)       e = Dim b e
  apply (C d as bs) e = d :? (as ++ e:bs)
  apply (S a as bs) e = a :< (as ++ e:bs)
  apply (B v e')    e = Let (v:=e) e'
  apply (U b)       e = Let b e

type CCStep t a = Step (CCFocus t a) (Expr t a)

inDim :: CCStep t a
inDim (Zipper c (Dim b e)) = Zipper (c :> D b) e

inAlt :: Int -> CCStep t a
inAlt i (Zipper c (d :? es)) = Zipper (c :> C d as bs) b
  where (as,b:bs) = splitAt i es

inSub :: Int -> CCStep t a
inSub i (Zipper c (a :< es)) = Zipper (c :> S a as bs) b
  where (as,b:bs) = splitAt i es

inBind :: CCStep t a
inBind (Zipper c (Let (v:=b) u)) = Zipper (c :> B v u) b

inUse :: CCStep t a
inUse (Zipper c (Let b u)) = Zipper (c :> U b) u

{-
data Zipper t a = Z (Context t a) (Expr t a) deriving Eq

data Context t a = Context t a :> Focus t a
                 | Top
                 deriving (Eq,Show)

enter :: Expr t a -> Zipper t a
enter = Z Top

exit :: Zipper t a -> Expr t a
exit (Z Top e) = e
exit z         = exit (up z)

up :: Step t a
up (Z Top e)      = Z Top e
up (Z (c :> f) e) = Z c (apply f e)
-}


module Choice.Syntax where 

import Data.List  (intersperse)
import Data.Maybe (catMaybes)

-----------
-- Types --
-----------

type Name = String

data Expr t a = 
    a :< [Expr t a]                  -- object branching
  | Let (Bind (Expr t a)) (Expr t a) -- let expression
  | Var Name                         -- variable reference
  | Dim (Bind [t]) (Expr t a)        -- dimension declaration
  | Name :? [Expr t a]               -- choice
  deriving Eq

data Bind a = Name := a deriving Eq


------------------------
-- Smart Constructors --
------------------------

dim :: Name -> [t] -> Expr t a -> Expr t a
dim n ts = Dim (n := ts)

leaf :: a -> Expr t a
leaf = (:<[])

text :: [Expr t String] -> Expr t String
text = ("" :<)

-- Turn textual code into Exprs by extracting choice variables.
parse :: String -> Expr t String
parse = text . map decode . intersperse " " . words
        where decode ('@':s) = Var s
              decode s       = leaf s


----------------------
-- Helper Functions --
----------------------

var :: Bind a -> Name
var (n := _) = n

val :: Bind a -> a
val (_ := a) = a

children :: Expr t a -> [Expr t a]
children (_ :< es) = es
children (Let b e) = [val b,e]
children (Var _)   = []
children (Dim _ e) = [e]
children (_ :? es) = es

-- map a function across all children
cmap :: (Expr t a -> b) -> Expr t a -> [b]
cmap f = map f . children

-- swap the children of an expression for new children
cswap :: Expr t a -> [Expr t a] -> Expr t a
cswap (a :<  _) es    = a :< es
cswap (Let b _) [e,f] = Let (var b := e) f
cswap v@(Var _) _     = v
cswap (Dim b _) [e]   = Dim b e
cswap (d :?  _) es    = d :? es

-- apply a transformation function to all children
tcmap :: (Expr t a -> Expr t a) -> Expr t a -> Expr t a
tcmap f e = cswap e (cmap f e)


---------------
-- Instances --
---------------

instance Functor (Expr t) where
  fmap f (a :< es) = f a :< map (fmap f) es
  fmap f (Let b e) = Let (fmap (fmap f) b) (fmap f e)
  fmap _ (Var n)   = Var n
  fmap f (Dim b e) = Dim b (fmap f e)
  fmap f (n :? es) = n :? map (fmap f) es

instance Functor Bind where
  fmap f (n := a) = n := f a

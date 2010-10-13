{-# LANGUAGE TypeSynonymInstances #-}

module CC.Pretty (pretty,bw,color,ShowCC(..)) where

import Control.Monad.State
import Data.List (intersperse)

import CC.Syntax


----------------------
-- Public Interface --
----------------------

pretty :: ShowCC a => Colors -> CC a -> String
pretty c e = evalState (cc e) c

bw,color :: Colors
bw    = Colors id id id id id
color = Colors { _op  = style blue,
                 _key = style (blue ++ bold),
                 _var = style red,
                 _dim = style green,
                 _tag = style green }

class ShowCC a where
  showCC :: a -> String
instance ShowCC Int where
  showCC = show
instance ShowCC String where
  showCC = id

instance ShowCC a => Show (CC a) where
  show = pretty color -- Windows users: change to "pretty bw"


-------------
-- Innards --
-------------

data Colors = Colors {
  _op  :: String -> String,
  _key :: String -> String,
  _var :: String -> String,
  _dim :: String -> String,
  _tag :: String -> String
}

type Pretty = State Colors

cat :: [Pretty String] -> Pretty String
cat = liftM concat . sequence

adorn :: (Colors -> String -> String) -> String -> Pretty String
adorn f s = get >>= return . flip f s

val :: ShowCC a => a -> Pretty String
val = return . showCC

op,key,var,dim,tag :: String -> Pretty String
op  = adorn _op
key = adorn _key
var = adorn _var . ('$':)
dim = adorn _dim
tag = adorn _tag

commas :: (String -> Pretty String) -> [Pretty String] -> Pretty String
commas f = cat . intersperse (f ",")

surround :: (String -> Pretty String) -> String -> String -> Pretty String -> Pretty String
surround f l r s = cat [f l, s, f r]

braces,bracks,parens :: Pretty String -> Pretty String
braces = surround return "{" "}"
bracks = surround op     "<" ">"
parens = surround op     "(" ")"

cc :: ShowCC a => CC a -> Pretty String
cc (Str a [])  = val a
cc (Str a es)  = cat [val a, (braces . commas return) (map cc es)]
cc (Let v b u) = cat [key "let ", var v, op " = ", parens (cc b), key " in ", parens (cc u)]
cc (Ref v)     = var v
cc (Dim d t e) = cat [key "dim ", dim d, (bracks . commas op) (map tag t), key " in ", parens (cc e)]
cc (Chc d es)  = cat [dim d, (bracks . commas op) (map cc es)]


--------------------------------------
-- Martin's Color Module (modified) --
--------------------------------------

reset = "\27[0m"
bold  = "\27[1m"

attrFG c = "\27[3" ++ show c ++ "m"

black  = attrFG 0
red    = attrFG 1
green  = attrFG 2
yellow = attrFG 3
blue   = attrFG 4
purple = attrFG 5
cyan   = attrFG 6
white  = attrFG 7

defaultColor = black ++ reset

style c s = c ++ s ++ defaultColor

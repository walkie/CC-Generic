{-# LANGUAGE TypeSynonymInstances #-}

-- see comment on variability in CC.hs
{-# OPTIONS_GHC -cpp #-} -- -DSHARING_SEPARABLE -DSHARING_EARLY #-}

module CC.Pretty where

import Control.Monad.State
import Data.List (intersperse)

import CC.Syntax

----------------------
-- Public Interface --
----------------------

pretty :: ExpT e => Colors -> CC e -> String
pretty c e = evalState (cc e) c

bw,color :: Colors
bw    = Colors id id id id id
color = Colors { _op  = style blue,
                 _key = style (blue ++ bold),
                 _var = style red,
                 _dim = style green,
                 _tag = style green }


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

cc :: ExpT e => CC e -> Pretty String
cc (Exp e)     = return (show e)
cc (Chc d es)  = cat [dim d, (bracks . commas op) (map cc es)]
cc (Dim d t e) = cat [key "dim ", dim d, (bracks . commas op) (map tag t), key " in ", parens (cc e)]
#ifdef SHARING_SEPARABLE
cc (Abs v u)   = cat [op "\\", var v, op ".", parens (cc u)]
cc (App l b)   = cat [parens (cc l), op " ", parens (onBnd cc b)]
#else
cc (Let v b u) = cat [key "let ", var v, op " = ", parens (onBnd cc b), key " in ", parens (cc u)]
#endif
cc (Ref v)     = var v


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

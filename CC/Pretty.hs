{-# LANGUAGE TypeSynonymInstances #-}

-- uncomment one of the following lines to choose colored or uncolored output
{-# OPTIONS_GHC -cpp -DCOLOR #-} -- color
--{-# OPTIONS_GHC -cpp #-}       -- no color

module CC.Pretty where

import Data.List (intersperse)

import CC.Syntax


---------------------
-- Pretty Printing --
---------------------

showOp :: String -> String
showOp = color blue 

showKey :: String -> String
showKey = color (blue ++ boldOn)

showVar :: Var -> String
showVar = color red 

showDim :: Dim -> String
showDim = color green

showTag :: Tag -> String
showTag = color green

showInParens :: String -> String
showInParens s = showOp "(" ++ s ++ showOp ")"

showInBracks :: String -> String
showInBracks s = showOp "<" ++ s ++ showOp ">"

showSeq :: String -> [String] -> String
showSeq sep = concat . intersperse sep

showBrackSeq :: (a -> String) -> [a] -> String
showBrackSeq f = showInBracks . showSeq "," . map f

showTags :: [Tag] -> String
showTags = showBrackSeq showTag

showAlts :: ShowNest a => [CC a] -> String
showAlts = showBrackSeq showCC

showCC :: ShowNest a => CC a -> String
showCC (Str a [])  = showValue a
showCC (Str a es)  = concat [showValue a, showOpen a, showSeq (showSep a) (map showCC es), showClose a]
showCC (Let v b e) = concat [showKey "let ", showVar v, showOp " = ", showInParens (showCC b), showKey " in ", showInParens (showCC e)]
showCC (Ref v)     = showVar v
showCC (Dim d t e) = concat [showKey "dim ", showDim d, showTags t, showKey " in ", showInParens (showCC e)]
showCC (Chc d es)  = concat [showDim d, showAlts es]

instance ShowNest a => Show (CC a) where
  show = showCC


------------
-- Values --
------------

class Show a => ShowNest a where
  showValue :: a -> String
  showOpen  :: a -> String
  showSep   :: a -> String
  showClose :: a -> String
  showValue   = show
  showOpen  _ = "{"
  showSep   _ = ","
  showClose _ = "}"

instance ShowNest Bool
instance ShowNest Char    where showValue = (:[])
instance ShowNest Integer
instance ShowNest String
  --showS = id
  --opAng _ = ""
  --comma _ = ""
  --clAng _ = ""


--------------------------------------
-- Martin's Color Module (modified) --
--------------------------------------

type Color = String

#ifdef COLOR
reset  = "\27[0m"
boldOn = "\27[1m"

attrFG :: Int -> String
attrFG c = "\27[3" ++ show c ++ "m"
#else -- NO COLOR
reset    = ""
boldOn   = ""
attrFG _ = ""
#endif

black  = attrFG 0
red    = attrFG 1
green  = attrFG 2
yellow = attrFG 3
blue   = attrFG 4
purple = attrFG 5
cyan   = attrFG 6
white  = attrFG 7

defaultColor = black ++ reset

color :: Color -> String -> String
color c s = c ++ s ++ defaultColor

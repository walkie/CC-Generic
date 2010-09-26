{-# LANGUAGE TypeSynonymInstances #-}
module Choice.Pretty where

import Data.List (intersperse)

import Color
import PrintList

import Choice.Syntax
import Choice.Semantics

showPlain :: Show a => a -> String
showPlain = filter (/='"') . show

pv :: (ShowNesting a,ShowNesting t, Eq t) => Expr t a -> IO ()
pv = putStr . printList ["\n","\n\n","\n\n"] showPair . variants
     where showPair (qs,e) = asList (\(d,t)->d++"."++showPlain t) qs
                             ++":\n"++show e

psem :: (ShowNesting a,ShowNesting t, Eq t) => Expr t a -> IO ()
psem = pv . expand []

mpsem :: (ShowNesting a, ShowNesting t, Eq t) => Maybe (Expr t a) -> IO ()
mpsem (Just e) = psem e
mpsem Nothing  = print "Nothing"

instance (ShowNesting t, ShowNesting a) => Show (Expr t a) where
  show (Var v)   = colVar v
  show (a :< []) = showS a
  show (a :< es) = showS a ++ opAng a ++ showas es ++ clAng a
  show (Let b e) = colKey "let " ++ show b ++ colKey " in\n" ++ show e
  show (d :? es) = showdim d (map show es)
  show (Dim (d := ts) e) = 
    colKey "dim " ++ showdim d (map (colTag . showS) ts) ++
    colKey " in\n" ++ show e

instance Show a => Show (Bind a) where
  show (v := a) = colVar v ++ colOp " = " ++ show a

showdim :: Name -> [String] -> String
showdim d ss = colDim d ++ colOp "<" ++ showss ss ++ colOp ">"

showas :: (ShowNesting t, ShowNesting a) => [Expr t a] -> String
showas xs = printList ["", comma (getA (head xs)), ""] show xs
  where getA :: Expr t a -> a
        getA = undefined

showss :: [String] -> String
showss = concat . intersperse ","

colOp  = colorString blue 
colKey = colorString (blue ++ boldOn)
colVar = colorString red 
colDim = colorString green
colTag = colorString green

class Show a => ShowNesting a where
  showS :: a -> String
  opAng :: a -> String
  clAng :: a -> String
  comma :: a -> String
  showS = show
  opAng _ = "{"
  comma _ = ", "
  clAng _ = "}"

instance ShowNesting Bool where
  showS = show
  opAng _ = "{"
  comma _ = ","
  clAng _ = "}"

instance ShowNesting Char where
  showS = (:[])
  opAng _ = "{"
  comma _ = ","
  clAng _ = "}"

instance ShowNesting Int where
  showS = show
  opAng _ = "{"
  comma _ = ","
  clAng _ = "}"

instance ShowNesting Integer where
  showS = show
  opAng _ = "{"
  comma _ = ","
  clAng _ = "}"

instance ShowNesting String where
  showS = show
  opAng _ = "{"
  comma _ = ","
  clAng _ = "}"
  --showS = id
  --opAng _ = ""
  --comma _ = ""
  --clAng _ = ""

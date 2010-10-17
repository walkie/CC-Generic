{-# LANGUAGE TypeFamilies #-}

module CC.Zipper.CC where

import Control.Monad.State

import CC.Syntax
import CC.Pretty () -- for Show (CC a) instance
import CC.Zipper
import CC.Zipper.List

-----------
-- Types --
-----------

type ZipperCC a = Zipper (CC a)


-----------
-- Moves --
-----------

inDim, inBind, inUse :: ZipperCC a ()
inDim  = moveIfCurrent isDim down
inBind = moveIfCurrent isLet down
inUse  = moveIfCurrent isLet (down .> right)

inSub :: (CC a -> Bool) -> Int -> ZipperCC a ()
inSub t i = moveIfCurrent (\e -> t e && length (subs e) > i) (moveToSub i)

inStr, inChc :: Int -> ZipperCC a ()
inStr i = moveIfCurrent isStr down >> replicateM_ i (moveIf (fmap rightEnd get) right)
inChc i = undefined --inSub isChc


{-
moveIf :: Query a Bool -> Movement

moveIf :: Query a Bool -> Movement (CC a) -> Movement (CC a)
moveIf test move loc | test loc  = move loc
                     | otherwise = error "Illegal movement."

inStr i = moveIf isStr (until (\
inDim   = moveIf isDim down
inBind  = moveIf isLet down
inUse   = moveIf isLet (down .> right)
-}


{-
inDim :: Zipper (CC a)
inDim = get >>= 
-- Navigating CC expressions.

dimNs :: Ctx t a -> Map Int
dimNs Top            = []
dimNs (_ :> CCF m _) = m

zoom :: Ctx t a -> ExprFocus t a -> Ctx t a
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
-}

----------------------------------
-- Choice Calculus Zip Instance --
----------------------------------

-- The immediate context in which a subexpression exists.
-- Conceptually, an expresion with a hole.
data Focus a = S a   (Context [CC a]) -- in substructure
             | B Var (CC a)           -- in let binding
             | U Var (CC a)           -- in let use
             | D Dim [Tag]            -- in dim decl
             | C Dim (Context [CC a]) -- in alternative
             deriving (Eq,Show)

-- Apply a focus to an expression, i.e. fill the hole.
apply :: Focus a -> CC a -> CC a
apply (S a lc) e = Str a (exit (lc,Just e))
apply (B v u)  b = Let v b u
apply (U v b)  u = Let v b u
apply (D d ts) e = Dim d ts e
apply (C d lc) e = Chc d (exit (lc,Just e))

-- Navigable instance
instance Zip (CC a) where
  
  newtype Context (CC a) = Ctx [Focus a] deriving (Eq,Show)
  type    Current (CC a) = CC a

  enter e         = (Ctx [],e)
  exit  (Ctx c,e) = foldl (flip apply) e c

  bottom (_    ,e) = isLeaf e
  top    (Ctx c,_) = null c

  leftEnd (Ctx []   ,_) = True
  leftEnd (Ctx (f:_),e) = case f of
    (S _ lc) -> leftEnd (lc,Just e)
    (C _ lc) -> leftEnd (lc,Just e)
    (U _ _)  -> False
    _        -> True
  
  rightEnd (Ctx []   ,_) = True
  rightEnd (Ctx (f:_),e) = case f of
    (S _ lc) -> rightEnd (lc,Just e)
    (C _ lc) -> rightEnd (lc,Just e)
    (B _ _)  -> False
    _        -> True
    
  down (Ctx c, Str a es)   = (Ctx (S a lc : c), e) where (lc,Just e) = enter es
  down (Ctx c, Chc d es)   = (Ctx (C d lc : c), e) where (lc,Just e) = enter es
  down (Ctx c, Let v b u)  = (Ctx (B v u  : c), b) 
  down (Ctx c, Dim d ts e) = (Ctx (D d ts : c), e)
  down loc = loc
  
  up (Ctx (f:c),e) = (Ctx c, apply f e)
  up loc = loc

  left (Ctx (S a lc : c), e) = (Ctx (S a lc' : c), e') where (lc',Just e') = left (lc,Just e)
  left (Ctx (C d lc : c), e) = (Ctx (C d lc' : c), e') where (lc',Just e') = left (lc,Just e)
  left (Ctx (U v b  : c), u) = (Ctx (B v u   : c), b )
  left loc = loc

  right (Ctx (S a lc : c), e) = (Ctx (S a lc' : c), e') where (lc',Just e') = right (lc,Just e)
  right (Ctx (C d lc : c), e) = (Ctx (C d lc' : c), e') where (lc',Just e') = right (lc,Just e)
  right (Ctx (B v u  : c), b) = (Ctx (U v b   : c), u )
  right loc = loc

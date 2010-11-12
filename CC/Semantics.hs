{-# LANGUAGE TupleSections #-}

module CC.Semantics where

import Control.Monad
import Control.Monad.Error
import Data.List (elemIndex)
import Data.Typeable (cast)

import CC.Error
import CC.Static
import CC.Syntax
import CC.Zipper

---------------
-- Selection --
---------------

type QTag     = (Dim,Tag)
type Selector = (Dim,Int)
type Decision = [QTag]

-- Tag selection. Given qualified tag d.t, select tag t in the first
-- dimension d encountered in an inorder traversal of the choice expression.
selectTag :: ExpT e => CC e -> QTag -> SemanticsM (CC e)
selectTag e (d,t) = do
    z <- maybeErr (noMatchingDim d) $ match dimMatch (enter e)
    i <- maybeErr (noMatchingTag t) $ ccQuery Nothing getTags z >>= elemIndex t
    liftM exit $ ccTransM (choiceElim (d,i) . dimElim) z
  where dimElim  (Dim _ _ e) = e
        dimMatch (Dim x _ _) = d == x
        dimMatch _           = False

-- Choice elimination Given selector d.i, replace all choices in dimension d
-- with their ith alternative. Used in tag selection and the choice semantics.
choiceElim :: ExpT e => Selector -> CC e -> SemanticsM (CC e)
choiceElim (d,_) e@(Dim d' _ _) | d == d' = return e
choiceElim (d,i)   (Chc d' es)  | d == d' = if i < length es
                                            then throwError (noAlternative i)
                                            else choiceElim (d,i) (es !! i)
choiceElim s e = transformSubsM (choiceElim s) e

-- Make a decision by applying each selection in order.
decide :: ExpT e => CC e -> Decision -> SemanticsM (CC e)
decide = foldM selectTag


------------------------
-- Decision Semantics --
------------------------

-- A wildly inefficient implementation of the decision semantics.  A more
-- straightforward (and efficient) implementation is extremely difficult...
-- However, some gains could be easily made by simply threading the zipper
-- through each function, rather than starting back at the top each time.

type Semantics e = Map Decision (CC e)

-- Get the next dimension declaration, in a pre-order traversal.
nextDecl :: ExpT e => CC e -> Maybe Decl
nextDecl e = match isDim (enter e) >>= ccQuery Nothing getDecl

-- Select each tag in a dimension declaration.
selectAll :: ExpT e => CC e -> Decl -> SemanticsM (Map QTag (CC e))
selectAll e (d,ts) = do
    let qs = map (d,) ts
    es <- mapM (selectTag e) qs
    return (zip qs es)

-- The variants of a choice calculus expression.  Removes all dimensions.
-- Removes all choices if well-dimensioned.
variants :: ExpT e => CC e -> SemanticsM (Semantics e)
variants e = do
    qv <- maybe (return []) (selectAll e) (nextDecl e)
    vs <- mapM (variants . snd) qv
    return [(q:qs,e') | (q,v) <- zip (map fst qv) vs, (qs,e') <- v]

-- Let expansion.  This transformation should only be applied to dimension-free
-- expressions, otherwise the semantics will be changed.
letExp :: ExpT e => Map Var Bound -> CC e -> SemanticsM (CC e)
letExp m (Let v b u) = do b' <- inBndM (letExp m) b
                          letExp ((v,b'):m) u
letExp m (Ref v) = do Bnd b <- maybeErr (undefinedVar v) (lookup v m)
                      maybeErr (refTypeError v) (cast b)
letExp m e = transformSubsM (letExp m) e

-- If well-formed, provides a mapping from decisions to plain expressions.
semantics :: ExpT e => CC e -> SemanticsM (Semantics e)
semantics e = do
    -- perform well-formedness checking first
    maybe (return ()) (throwError . NotWellFormed) (wellFormed e)
    vs <- variants e
    es <- mapM (letExp []) (map snd vs)
    return $ zip (map fst vs) es

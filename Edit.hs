{-# LANGUAGE PatternGuards #-}

module Edit where

import Choice

type Path t a = [Step t a] 

data Step t a = S Int   -- structure subIx
              | B Bool  -- binding   inBody?
              | D       -- dimDecl
              | C Int   -- choice    subIx
              deriving (Eq,Show)

data Command t a = Command (Path t a) (Edit t a)

-- TODO this could be refactored...
data Edit t a = SData      a
              | SInsert    Int (Expr t a)
              | SDelete    Int
              | BRename    Name
              | BBound     (Expr t a)
              | BBody      (Expr t a)
              | VRename    Name
              | DRename    Name
              | DTagInsert Int t
              | DTagDelete Int
              | DBody      (Expr t a)
              | CRename    Name
              | CInsert    Int (Expr t a)
              | CDelete    Int

follow :: Path t a -> Expr t a -> Match t a
follow = f id
  where
    f c [] e = Just (c,e)
    f c (D   : p) (Dim d e)                       = f (c . Dim d               ) p e
    f c (S i : p) (a :< es)       | i < length es = f (c . (a:<) . replace i es) p (es !! i)
    f c (C i : p) (d :? es)       | i < length es = f (c . (d:?) . replace i es) p (es !! i)
    f c (B b : p) (Let (v:=e) e') | b             = f (c . Let (v:=e)          ) p e'
                                  | otherwise     = f (c . \x -> Let (v:=x) e' ) p e
    f _ _ _ = Nothing

edit :: Edit t a -> Expr t a -> Maybe (Expr t a)
edit (SData      a  ) (_ :< es)       = Just $ a :< es
edit (SInsert    i e) (a :< es)       = Just $ a :< insert i es e
edit (SDelete    i  ) (a :< es)       = Just $ a :< delete i es
edit (BRename    v  ) (Let (_:=e) e') = Just $ Let (v:=e) e'
edit (BBound     e  ) (Let (v:=_) e') = Just $ Let (v:=e) e'
edit (BBody      e' ) (Let (v:=e) _ ) = Just $ Let (v:=e) e'
edit (VRename    v  ) (Var _)         = Just $ Var v
edit (DRename    d  ) (Dim (_:=ts) e) = Just $ Dim (d:=ts) e
edit (DTagInsert i t) (Dim (d:=ts) e) = Just $ Dim (d:=insert i ts t) e
edit (DTagDelete i  ) (Dim (d:=ts) e) = Just $ Dim (d:=delete i ts  ) e
edit (DBody      e  ) (Dim (d:=ts) _) = Just $ Dim (d:=ts) e
edit (CRename    d  ) (_ :? es)       = Just $ d :? es
edit (CInsert    i e) (d :? es)       = Just $ d :? insert i es e
edit (CDelete    i  ) (d :? es)       = Just $ d :? delete i es
edit _ _ = Nothing

exec' :: Command t a -> Expr t a -> Maybe (Expr t a)
exec' (Command p ed) ex | Just (c,ex') <- follow p ex = fmap c (edit ed ex')
                        | otherwise                   = Nothing

exec :: [Command t a] -> Expr t a -> Maybe (Expr t a)
exec cs e = foldMaybe exec' (Just e) cs


------------------------
-- Smart Constructors --
------------------------

cmds :: Path t a -> [Edit t a] -> [Command t a]
cmds = map . Command

sReplace :: Path t a -> Int -> Expr t a -> [Command t a]
sReplace p i e = cmds p [SDelete i, SInsert i e]

dTagReplace :: Path t a -> Int -> t -> [Command t a]
dTagReplace p i t = cmds p [DTagDelete i, DTagInsert i t]

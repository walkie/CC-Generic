{-# LANGUAGE TypeFamilies #-}

module CC.Zipper.List where

import CC.Zipper


-----------------------
-- List Zip Instance --
-----------------------

boundedIx :: Int -> [a] -> Int
boundedIx i l = max 0 (min (length l - 1) i)

focus :: Int -> [a] -> Location [a]
focus _ [] = (ListCtx [] [], Nothing)
focus i as = (ListCtx ls rs, Just l)
  where (bs,rs) = splitAt (boundedIx i as + 1) as
        (l:ls)  = reverse bs

getFocus :: Location [a] -> Int
getFocus (ListCtx ls _,_) = length ls

instance Zip [a] where
  
  data Context [a] = ListCtx [a] [a] deriving (Eq,Show)
  type Current [a] = Maybe a

  enter = focus 0
  
  exit (ListCtx [] [], Nothing) = []
  exit (ListCtx ls rs, Just a)  = foldl (flip (:)) rs (a:ls)

  bottom _ = True
  top    _ = True
  
  leftEnd  (ListCtx l _, _) = null l
  rightEnd (ListCtx _ r, _) = null r

  down l = l
  up   l = l
  
  left (ListCtx (l:ls) rs, Just r) = (ListCtx ls (r:rs), Just l)
  left loc = loc
  
  right (ListCtx ls (r:rs), Just l) = (ListCtx (l:ls) rs, Just r)
  right loc = loc

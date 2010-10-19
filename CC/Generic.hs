module CC.Generic (toCC,fromCC) where

import Control.Monad (guard)
import Data.Generics

import CC.Syntax

-- a helper function just used for its type...
unM :: m a -> a
unM = undefined

-- Cram data type into a choice calculus expression.
toCC :: Data a => a -> CC String
toCC = other `extQ` string
  where string s = Str s :< []
        other  a = Str (showConstr (toConstr a)) :< gmapQ toCC a

-- Pull data type out of a *plain* choice calculus expression.
fromCC :: Data a => CC String -> Maybe a
fromCC = other `extR` string
  where string (Str s :< []) = Just s
        other  (Str s :< es) = result
          where result = do c <- readConstr (dataTypeOf (unM result)) s
                            let a = fromConstr c
                            guard (glength a == length es)
                            snd (gmapAccumM (\(x:xs) _ -> (xs,fromCC x)) es a)
        other _ = Nothing -- CC expression is not plain

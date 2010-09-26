
module Choice.Util where

import Data.Maybe (catMaybes,listToMaybe)
import Data.List  (nub)

----------------------
-- Helper Functions --
----------------------

union :: Eq a => [[a]] -> [a]
union = nub . concat

delete i es = take i es ++ drop (i+1) es

insert :: Int -> [a] -> a -> [a]
insert i es e = take i es ++ e : drop i es

replace :: Int -> [a] -> a -> [a]
replace i es e = take i es ++ e : drop (i+1) es

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes

foldMaybe :: (a -> b -> Maybe b) -> Maybe b -> [a] -> Maybe b
foldMaybe _ Nothing  _      = Nothing
foldMaybe _ mb       []     = mb
foldMaybe f (Just b) (a:as) = foldMaybe f (f a b) as

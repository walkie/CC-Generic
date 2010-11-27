
module CC.Test (
    module Test.Framework,
    module Test.Framework.TH,
    module Test.Framework.Providers.HUnit,
    module Test.Framework.Providers.QuickCheck2,
    module Test.HUnit,
    module Test.QuickCheck,
    testSame,testAB,testNoneSome,testAllNone
  ) where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 -- requires QuickCheck-2.1.1.1
import Test.HUnit hiding (Test,Testable)
import Test.QuickCheck

import Data.Set (Set,empty,fromList)

-- There are two implementations of this function.  The first condenses all
-- cases into a single test, which makes for nicer test output.  The second
-- runs each case separately, which is useful for debugging a failed test
testSame :: (Eq a, Show a) => String -> [a] -> [a] -> [Test]
testSame n as es = [testCase n $ as @?= take (length as) es]
{-
testSame n as es = zipWith testCase names (zipWith (@?=) as es)
  where names = [n ++ ' ' : show i | i <- [0..]]
-}

testAB :: (Eq b, Show b) => String -> String -> String -> (a -> b) ->
                            [a] -> [a] -> [b] -> [b] -> [Test]
testAB n na nb f as bs as' bs' =
       testSame (n ++ " " ++ na) (map f as) as'
    ++ testSame (n ++ " " ++ nb) (map f bs) bs'

testNoneSome :: (Ord b, Show b) => String -> (a -> Set b) -> [a] -> [a] -> [[b]] -> [Test]
testNoneSome n f nones somes rs = testAB n "none" "some" f nones somes
                                  (repeat empty) (map fromList rs)

testAllNone :: String -> (a -> Bool) -> [a] -> [a] -> [Test]
testAllNone n f alls nones = testAB n "yes" "no " f alls nones (repeat True) (repeat False)

{-# OPTIONS_GHC -cpp #-} -- -DTERSE #-}

module CC.Test (
    module Test.Framework,
    module Test.Framework.TH,
    module Test.Framework.Providers.HUnit,
    module Test.Framework.Providers.QuickCheck2,
    module Test.HUnit,
    module Test.QuickCheck,
    testSame,testAB,testAB',testABC,testABC',testNoneSome,testAllNone
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
#ifdef TERSE
testSame n as es = [testCase n $ as @?= take (length as) es]
#else
testSame n as es = zipWith testCase names (zipWith (@?=) as es)
  where names = [n ++ ' ' : show i | i <- [0..]]
#endif

testAB :: (Eq b, Show b) => String -> (a -> b)
                         -> String -> String
                         -> [a]    -> [a]
                         -> [b]    -> [b]
                         -> [Test]
testAB n f na nb as bs as' bs' =
       testSame (n ++ " " ++ na) (map f as) as'
    ++ testSame (n ++ " " ++ nb) (map f bs) bs'

testAB' :: (Eq b, Show b) => String -> (a -> b)
                          -> String -> String
                          -> [a]    -> [a]
                          -> b      -> b
                          -> [Test]
testAB' n f na nb as bs a b =
    testAB n f na nb as bs (repeat a) (repeat b)

testABC :: (Eq b, Show b) => String -> (a -> b)
                          -> String -> String -> String
                          -> [a]    -> [a]    -> [a]
                          -> [b]    -> [b]    -> [b]
                          -> [Test]
testABC n f na nb nc as bs cs as' bs' cs' =
       testAB n f na nb as bs as' bs'
    ++ testSame (n ++ " " ++ nc) (map f cs) cs'

testABC' :: (Eq b, Show b) => String -> (a -> b)
                           -> String -> String -> String
                           -> [a]    -> [a]    -> [a]
                           -> b      -> b      -> b  
                           -> [Test]
testABC' n f na nb nc as bs cs a b c =
    testABC n f na nb nc as bs cs (repeat a) (repeat b) (repeat c)

testNoneSome :: (Ord b, Show b) => String -> (a -> Set b) -> [a] -> [a] -> [[b]] -> [Test]
testNoneSome n f nones somes rs = testAB n f "none" "some" nones somes
                                  (repeat empty) (map fromList rs)

testAllNone :: String -> (a -> Bool) -> [a] -> [a] -> [Test]
testAllNone n f alls nones = testAB' n f "yes" "no " alls nones True False

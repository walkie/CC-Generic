{-# LANGUAGE TemplateHaskell #-}
module CC.Test.Static where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 -- requires QuickCheck-2.1.1.1
import Test.HUnit
import Test.QuickCheck

import Data.Set (empty,fromList)

import CC
import CC.Test.Expressions


-----------------------
-- "Automated" Tests --
-----------------------

tests = $(testGroupGenerator)
      : test_boundDims
     ++ test_boundVars
     ++ []

runTests = defaultMain tests

testSame n as es = zipWith testCase names (zipWith (@?=) as es)
  where names = [n ++ ' ' : show i | i <- [0..]]

test_boundDims = testSame "boundDims none" (map boundDims (bs ++ ss)) (repeat empty)
              ++ testSame "boundDims some" (map boundDims vs) 
                   (map fromList [["A"],["A"],["A","B"],["A","B"],["A","B"],["A","B"],["A"]])

test_boundVars = testSame "boundVars none" (map boundVars (bs ++ vs)) (repeat empty)
              ++ testSame "boundVars some" (map boundVars ss)
                   (map fromList [["v"],["u","v"],["u","v"]])
{-
case_freeDims  = undefined
case_freeVars  = undefined

case_bindFree      = undefined
case_refFree       = undefined
case_dimFree       = undefined
case_choiceFree    = undefined
case_shareFree     = undefined
case_variationFree = undefined
case_plain         = undefined

case_wellDim    = undefined
case_wellRef    = undefined
case_wellFormed = undefined
-}

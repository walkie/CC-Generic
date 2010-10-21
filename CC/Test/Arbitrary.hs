{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module CC.Test.Arbitrary where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 -- requires QuickCheck-2.1.1.1
import Test.HUnit
import Test.QuickCheck

import CC.Arbitrary
import CC.Syntax
import CC.Static
import CC.Pretty


----------------
-- Invariants --
----------------

prop_wellFormed e = wellFormed e
  where types = e :: CC Int

-----------------------
-- "Automated" Tests --
-----------------------

tests = $(testGroupGenerator)

runTests = defaultMain [tests]

instance Arbitrary a => Arbitrary (CC a) where
  arbitrary = genCC (genState 20 3 arbitrary)

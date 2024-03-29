{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, ScopedTypeVariables #-}
module CC.Test.Arbitrary where

import CC.Test

import CC.Arbitrary
import CC.Error
import CC.Syntax
import CC.Static
import CC.Tree
import CC.Show


----------------
-- Invariants --
----------------

prop_wellFormed e = wellFormed e == ok
  where types = e :: TreeCC Int

-----------------------
-- "Automated" Tests --
-----------------------

tests = $(testGroupGenerator) : []

runTests = defaultMain tests

instance (Arbitrary a, TreeVal a) => Arbitrary (TreeCC a) where
  arbitrary = genCC (genState 20 3 arbitrary)

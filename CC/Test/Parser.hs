{-# LANGUAGE TemplateHaskell #-}
module CC.Test.Parser where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 -- requires QuickCheck-2.1.1.1
import Test.HUnit
import Test.QuickCheck

import CC
import CC.Parser
import CC.Pretty
import CC.Test.Expressions


----------------
-- Invariants --
----------------

prettyParse c e = e == parse (pretty c e)

-----------------------
-- "Automated" Tests --
-----------------------

tests = $(testGroupGenerator)
      : prettyParseTest "BW"    bw
     ++ prettyParseTest "Color" color

runTests = defaultMain tests

prettyParseTest n c = zipWith testCase names (map (assert . prettyParse c) exprs)
  where names = ["prettyParse " ++ n ++ ' ' : show i | i <- [0..]]

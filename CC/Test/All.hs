
module CC.Test.All where

import Test.Framework

import qualified CC.Test.Arbitrary as Arb
import qualified CC.Test.Parser    as Par
--import qualified CC.Test.Semantics as Sem
import qualified CC.Test.Static    as Sta
import qualified CC.Test.Zipper    as Zip

tests = Arb.tests
     ++ Par.tests
--   ++ Sem.tests
     ++ Sta.tests
     ++ Zip.tests

runTests = defaultMain tests

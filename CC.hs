-- Dimensions of variation:
--   SHARING_SEPARABLE<undefined,defined>
--     undefined = let-style sharing
--     defined   = lambda-style sharing
--   SHARING_EARLY<undefined,defined>
--     undefined = sharing resolved after decisions
--     defined   = sharing resolved before decisions
--
-- Undefined options are selected by default.  To choose defined options
-- load in GHCi as follows.
-- > ghci -cpp -DSHARING_SEPARABLE -DSHARING_EARLY CC

-- Modules that are not exported here, but can be imported as needed:
--   * CC.Arbitrary
--   * CC.Parser
--   * CC.Pretty
--   * CC.Tree
--   * CC.Zipper

-- A convenience module that re-exports the core CC modules.
module CC
  ( module CC.Syntax,
    module CC.Show,
    module CC.Error,
    module CC.Static,
    module CC.Semantics
  ) where
  
import CC.Syntax
import CC.Show
import CC.Error
import CC.Static
import CC.Semantics

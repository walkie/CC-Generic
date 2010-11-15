
-- This module defines a Show instance for CC expressions.  Most clients should
-- import this rather than CC.Pretty.
module CC.Show where

import CC.Syntax
import CC.Pretty

instance ExpT e => Show (CC e) where
  show = pretty color -- Windows users: change to "pretty bw"

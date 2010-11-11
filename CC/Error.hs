
module CC.Error where

import Control.Monad.Error

import CC.Syntax

-- choice calculus error monad
type CCM = Either CCError

data CCError =
  -- tag selection
    NoMatchingDim Dim
  | NoMatchingTag Tag
  -- choice elimination
  | NoAlternative Int
  -- let expansion
  | RefTypeError  Var
  | RefUndefined  Var
  -- malformed choices
  | ChcUndefined  Dim
  | ChcWrongArity Dim Int
  -- other errors
  | InternalError
  | OtherError    String
  deriving (Eq,Show)

instance Error CCError where
  strMsg = OtherError

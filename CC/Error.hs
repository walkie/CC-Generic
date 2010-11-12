
module CC.Error where

import Control.Monad.Error

import CC.Syntax

-- choice calculus error monad
type ErrM = Either Err

data Err =
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

instance Error Err where
  strMsg = OtherError

maybeErr :: Err -> Maybe a -> ErrM a
maybeErr e = maybe (throwError e) return

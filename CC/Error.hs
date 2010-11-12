
module CC.Error where

import Control.Monad.Error
import Data.Maybe (catMaybes, listToMaybe)

import CC.Syntax


-----------------------------
-- Static Checking Results --
-----------------------------

type Check = Maybe

-- the static check succeeds
ok  = Nothing

-- the static check fails
err = Just

-- returns the first failed check, ok otherwise
checkAll :: [Check a] -> Check a
checkAll = listToMaybe . catMaybes

-- 
-- Dimension checking
--

type WellDim = Check NotWellDim

data NotWellDim =
    UndefinedDim  Dim
  | ChcArityError Dim Int
  deriving (Eq,Show)

--
-- Reference checking
--

type WellRef = Check NotWellRef

data NotWellRef =
    UndefinedVar Var
  | RefTypeError Var
  deriving (Eq,Show)

--
-- Well-formedness checking
--

type WellFormed = Check NotWellFormed

data NotWellFormed =
    NotWellDim NotWellDim
  | NotWellRef NotWellRef
  deriving (Eq,Show)


----------------------
-- Semantics Errors --
----------------------

-- error monads
type SemanticsM = Either SemanticsError

data SelectionError =
    NoMatchingDim Dim
  | NoMatchingTag Tag
  | NoAlternative Int
  deriving (Eq,Show)

data SemanticsError =
    SelectionError SelectionError
  | NotWellFormed  NotWellFormed
  | OtherError String
  deriving (Eq,Show)

instance Error SemanticsError where
  strMsg = OtherError

undefinedDim    = NotWellFormed . NotWellDim . UndefinedDim
chcArityError d = NotWellFormed . NotWellDim . ChcArityError d
undefinedVar    = NotWellFormed . NotWellRef . UndefinedVar
refTypeError    = NotWellFormed . NotWellRef . RefTypeError
noMatchingDim = SelectionError . NoMatchingDim
noMatchingTag = SelectionError . NoMatchingTag
noAlternative = SelectionError . NoAlternative

maybeErr :: MonadError e m => e -> Maybe a -> m a
maybeErr e = maybe (throwError e) return

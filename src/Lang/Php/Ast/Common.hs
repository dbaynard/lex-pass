{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Lang.Php.Ast.Common
    (
      module X
    ) where

import Data.Binary as X
import Data.Binary.Generic
import Data.Char as X
import Data.Data as X hiding (Infix, Prefix)
import Data.List as X hiding (uncons)
import Data.Maybe as X

import Common as X
import Parse as X
import Unparse as X
import Lang.Php.Ast.WS as X

instance (Data a) => Binary a where
  get = getGeneric
  put = putGeneric

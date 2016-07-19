{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Lang.Php.Ast.Common
    (
      module Data.Binary
    , module Data.Char
    , module Data.Data
    , module Data.List
    , module Data.Maybe

    , module Common
    , module Parse
    , module Unparse
    , module Lang.Php.Ast.WS
    ) where

import Data.Binary
import Data.Binary.Generic
import Data.Char
import Data.Data hiding (Infix, Prefix)
import Data.List hiding (uncons)
import Data.Maybe

import Common
import Parse
import Unparse
import Lang.Php.Ast.WS

instance (Data a) => Binary a where
  get = getGeneric
  put = putGeneric

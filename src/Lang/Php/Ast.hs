{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Lang.Php.Ast (
  module X,
  Ast
  ) where

import Control.Applicative hiding ((<|>), many)
import Control.Arrow
import Control.Monad
import Data.Binary.Generic ()
import Data.Char
import GHC.Generics
import Data.Aeson

import Common
--import qualified Data.ByteString as BS
--import qualified Data.Intercal as IC
import Lang.Php.Ast.Common as X
import Lang.Php.Ast.Lex as X
import Lang.Php.Ast.Stmt as X

data Ast = Ast TopLevel StmtList
  deriving (Eq, Show, Typeable, Data, Generic, FromJSON, ToJSON)

instance Unparse Ast where
  unparse (Ast t s) = unparse t ++ unparse s

instance Parse Ast where
  parse = liftM2 Ast parse stmtListP

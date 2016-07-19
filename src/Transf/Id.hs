module Transf.Id where

import Lang.Php hiding (encode)
import TransfUtil
import qualified Data.Intercal as IC
import Data.Aeson
import Data.String.Conv

transfs :: [Transf]
transfs = [
  "id" -:- ftype -?-
  "For testing lex-pass.  Rewrite all files as is."
  -=- argless (lexPass $ changeNothing Nothing True),
  "no-op" -:- ftype -?-
  "For testing lex-pass.  Scan all files but do nothing."
  -=- argless (lexPass $ changeNothing Nothing False),
  "dump-ast" -:- ftype -?-
  "For testing lex-pass.  Dump the AST."
  -=- argless (lexPass $ changeNothing (Just DumpAST) False),
  "dump-json" -:- ftype -?-
  "For testing lex-pass.  Dump the AST as JSON."
  -=- argless (lexPass $ changeNothing (Just DumpJSON) False),
  "dump-ast-id" -:- ftype -?-
  "For testing lex-pass.  Dump the AST and rewrite as is."
  -=- argless (lexPass $ changeNothing (Just DumpAST) True)]

data Dump = DumpAST
          | DumpJSON
          deriving (Eq, Ord, Enum, Bounded, Read, Show)

-- optionally pretend we changed something to make this file count and force
-- rewrite.
changeNothing :: Maybe Dump -> Bool -> Ast -> Transformed Ast
changeNothing dumpAst pretendMod ast =
  Transformed (dumpFn dumpAst) $
      if pretendMod
        then Just ast
        else Nothing
    where
        dumpFn (Just DumpJSON) = [toS . encode $ ast]
        dumpFn (Just DumpAST) = [show ast]
        dumpFn Nothing = []

module Lang.Php.AstSpec (main, spec) where

import Test.Hspec
import System.Directory
import System.FilePath

import Lang.Php.Ast

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Lang.Php.Ast" $ do
        undefined


assertUnchanged :: String -> IO ()
assertUnchanged s = do
  let
    astMb :: Either ParseError Ast
    astMb = runParser parse () "." s
  case astMb of
    Left e -> error $ "assertUnchanged:\n" ++ show s ++ "\n ->\n" ++ show e
    Right ast -> do
      let s' = unparse ast
      when (s /= s') . error $
        "assertUnchanged:\n" ++ show s ++ "\n ->\n" ++
        show ast ++ "\n ->\n" ++ show s'

testDir :: FilePath
testDir = "test"

main :: IO ()
main = do
  assertUnchanged "<?php ''.'';"
  doesDirectoryExist testDir >>= \ e -> when e $
    mapM_ ((assertUnchanged =<<) . readFile . (testDir </>)) =<<
    filter ((/= '.') . head) <$> getDirectoryContents testDir
  putStrLn "all tests passed"

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Transf.FuncManip where

import Lang.Php
import TransfUtil
import qualified Data.Intercal as IC

transfs :: [Transf]
transfs = [
  "rename-func <old-func-name> <new-func-name>" -:- ftype -?-
  "Rename a function in all callsites. OO old-func-name not yet supported."
  -=- (\ [oldF, newF] -> lexPass $ renameFunc oldF newF),
  "get-all-defd-funcs" -:- ftype -?-
  "Get a list of all defined functions."
  -=- (\ [] -> lexPass getAllDefdFuncs),
  "kill-func-arg <func-name> <arg-n-starting-at-1>" -:- ftype -?-
  "Kill the nth arg of all callsites. OO func-name not yet supported."
  -=- (\ [f, n] -> lexPass . killFuncArg f $ read n),
  "abstract-mysql" -:- ftype -?-
  "Replace all calls to mysql_query with calls to doSQL. Print list of calls"
  -=- argless (lexPass abstractMysql),
  "replace-appl-w-assign <old-func-name> <new-func-name>" -:- ftype -?-
  "Rename a function and replace with assignment using ->."
  -=- (\ [oldF, newF] -> lexPass $ replaceApplWAssign oldF newF),
  "replace-appl-w-pdo-assign <old-func-name> <new-func-name> <parameter>" -:- ftype -?-
  "Rename a function and replace with assignment using ->. Add PDO-prefixed parameter."
  -=- (\ [oldF, newF, par] -> lexPass $ replaceApplWPDOAssign oldF newF par),
  "make-public-explicit" -:- ftype -?-
  "Add \"public\" to class functions without an explicit access keyword."
  -=- (\ [] -> lexPass makePublicExplicit)]

renameFunc :: String -> String -> Ast -> Transformed Ast
renameFunc oldF newF = modAll $ \ a -> case a of
  ROnlyValFunc (Right (Const [] f)) w args ->
    if f == oldF
      then pure $ ROnlyValFunc (Right $ Const [] newF) w args
      else transfNothing
  _ -> transfNothing

getAllDefdFuncs :: Ast -> Transformed Ast
getAllDefdFuncs = modAll $ \ stmt -> case stmt of
  StmtClass (Class cPre cName@(WSCap _ n _) cExt cImpl cBlock) ->
    (StmtClass . Class cPre cName cExt cImpl <$>) . flip modAll cBlock $
      \ cStmt -> case cStmt of
        CStmtFuncDef _ (Func _ _ f _ _) -> Transformed [n ++ "::" ++ f] Nothing
        _ -> transfNothing
  StmtFuncDef (Func _ _ f _ _) -> Transformed [f] Nothing
  _ -> transfNothing

killFuncArg :: String -> Int -> Ast -> Transformed Ast
killFuncArg f n = modAll $ \ a -> case a of
  ROnlyValFunc c@(Right (Const [] f')) w (Right args) ->
    if f' == f
      then pure $ ROnlyValFunc c w (Right $ take (n - 1) args ++ drop n args)
      else transfNothing
  _ -> transfNothing

isAccessKeyword :: String -> Bool
isAccessKeyword x = x `elem` ["public", "private", "protected"]

makePublicExplicit :: Ast -> Transformed Ast
makePublicExplicit = modAll $ \ cStmt -> case cStmt of
  CStmtFuncDef pre f -> if any (isAccessKeyword . map toLower . fst) pre
    then transfNothing
    else pure $ CStmtFuncDef (("public", [WS " "]):pre) f
  _ -> transfNothing

abstractMysql :: Ast -> Transformed Ast
abstractMysql = modAll $ \case
  MysqlQuery query -> Transformed{..}
    where
      infoLines = ["Abstract: " ++ show query]
      transfResult = pure $ DoSQLQuery query
  _                -> transfNothing

replaceApplWAssign :: String -> String -> Ast -> Transformed Ast
replaceApplWAssign oldF newF = modAll $ \case
  RFuncL ((== oldF) -> True) input -> Transformed{..}
    where
        infoLines = [show oldF ++ show newF]
        transfResult = pure $ LToRAssign input newF
  _                                -> transfNothing

replaceApplWPDOAssign :: String -> String -> String -> Ast -> Transformed Ast
replaceApplWPDOAssign oldF newF par = modAll $ \case
  RFuncL ((== oldF) -> True) input -> Transformed{..}
    where
        infoLines = [oldF ++ newF ++ "(PDO::" ++ par ++ ")"]
        transfResult = pure $ LToRPDOAssign input newF par
  _                                -> transfNothing

pattern MysqlQuery query <- ROnlyValFunc (Right (Const _ "mysql_query")) _
    (Right
      [ WSCap
        { wsCapPre = _
        , wsCapMain = query
        , wsCapPost = _
        }
        ])
    where
        MysqlQuery query = ROnlyValFunc (Right (Const [] "mysql_query")) []
            (Right
              [ WSCap
                { wsCapPre = []
                , wsCapMain = query
                , wsCapPost = []
                }
                ])

pattern DoSQLQuery query <- ROnlyValFunc (Right (Const _ "doSQL")) _
    (Right
      [ WSCap
        { wsCapPre = _
        , wsCapMain = query
        , wsCapPost = _
        }
        , DatabaseName "db"
        , NoDisplaySQL
        ])
    where
        DoSQLQuery query = ROnlyValFunc (Right (Const [] "doSQL")) []
            (Right
              [ WSCap
                { wsCapPre = []
                , wsCapMain = query
                , wsCapPost = []
                }
                , DatabaseName "db"
                , NoDisplaySQL
                ])

pattern DatabaseName name <- WSCap
        { wsCapPre = _
        , wsCapMain = Left (ExprRVal (RValLRVal (LRValVar (DynConst _ (Var name _)))))
        , wsCapPost = _
        }
    where
        DatabaseName name = WSCap
            { wsCapPre = [WS " "]
            , wsCapMain = Left (ExprRVal (RValLRVal (LRValVar (DynConst [] (Var name [])))))
            , wsCapPost = []
            }

pattern NoDisplaySQL <- WSCap
        { wsCapPre = _
        , wsCapMain = Left (ExprStrLit (StrLit "\"no\""))
        , wsCapPost = _
        }
    where
        NoDisplaySQL = WSCap
            { wsCapPre = [WS " "]
            , wsCapMain = Left (ExprStrLit (StrLit "\"no\""))
            , wsCapPost = [WS " "]
            }

pattern LToRAssign funL funR <- ROnlyValFunc
          (Left
             (LRValMemb (RValLRVal (LRValVar (DynConst _ (Var funL _))))
                _ (MembStr funR)))
          _ _
    where
        LToRAssign funL funR = ROnlyValFunc
          (Left
             (LRValMemb (RValLRVal (LRValVar (DynConst [] (Var funL []))))
                ([], []) (MembStr funR)))
          [] (Left [])

pattern LToRPDOAssign funL funR attr <- ROnlyValFunc
          (Left
             (LRValMemb (RValLRVal (LRValVar (DynConst _ (Var funL _))))
                _ (MembStr funR)))
          _ (Right [AssignAttr "PDO" attr])
    where
        LToRPDOAssign funL funR attr = ROnlyValFunc
          (Left
             (LRValMemb (RValLRVal (LRValVar (DynConst [] (Var funL []))))
                ([], []) (MembStr funR)))
          [] (Right [AssignAttr "PDO" attr])

pattern RFuncL funR input <- ROnlyValFunc (Right (Const [] funR)) _
          (Right
             [ WSCap
                { wsCapPre = _
                , wsCapMain =
                      Left
                        (ExprRVal (RValLRVal (LRValVar (DynConst [] (Var input [])))))
                , wsCapPost = _
                }])
    where
        RFuncL funR input = ROnlyValFunc (Right (Const [] funR)) []
          (Right
             [ WSCap
                { wsCapPre = []
                , wsCapMain =
                      Left
                        (ExprRVal (RValLRVal (LRValVar (DynConst [] (Var input [])))))
                , wsCapPost = []
                }])

pattern AssignAttr pre attr <- WSCap
              { wsCapPre = _
              , wsCapMain = Left (ExprRVal (RValROnlyVal (ROnlyValConst (Const [(pre,_)] attr))))
              , wsCapPost = _
              }
    where
        AssignAttr pre attr =
            WSCap { wsCapPre = []
                  , wsCapMain = Left (ExprRVal (RValROnlyVal (ROnlyValConst (Const [(pre,([],[]))] attr))))
                  , wsCapPost = []
                  }

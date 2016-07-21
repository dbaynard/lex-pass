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
  "dont-die" -:- ftype -?-
  "Strip or die from doSQL queries."
  -=- argless (lexPass noDie),
  "replace-appl-w-assign <old-func-name> <new-func-name>" -:- ftype -?-
  "Rename a function and replace with assignment using ->."
  -=- (\ [oldF, newF] -> lexPass $ replaceApplWAssign oldF newF),
  "extend-1-par-2 <func-name> <new-par>" -:- ftype -?-
  "Add a 2nd parameter to single parameter function."
  -=- (\ [fun, par] -> lexPass $ extend1to2 fun par),
  "replace-func-w-method <old-func-name> <new-func-name> <object>" -:- ftype -?-
  "Rename a function and replace with assignment using ->, from the parameter."
  -=- (\ [oldF, newF, obj] -> lexPass $ replaceFunctWMethod oldF newF obj),
  "replace-appl-w-pdo-assign <old-func-name> <new-func-name> <parameter>" -:- ftype -?-
  "Rename a function and replace with assignment using ->. Add PDO-prefixed parameter."
  -=- (\ [oldF, newF, par] -> lexPass $ replaceApplWPDOAssign oldF newF par),
  "make-public-explicit" -:- ftype -?-
  "Add \"public\" to class functions without an explicit access keyword."
  -=- (\ [] -> lexPass makePublicExplicit)]

renameFunc :: String -> String -> Ast -> Transformed Ast
renameFunc oldF newF = modAll $ \ a -> case a of
  ROnlyVal ((== oldF) -> True) args ->
      pure $ ROnlyVal newF args
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
  ROnlyVal ((== f) -> True) (Right args) ->
      pure $ ROnlyVal f (Right $ take (n - 1) args ++ drop n args)
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
        transfResult = pure $ LToRAssignSimple input newF
  _                                -> transfNothing

replaceApplWPDOAssign :: String -> String -> String -> Ast -> Transformed Ast
replaceApplWPDOAssign oldF newF par = modAll $ \case
  RFuncL ((== oldF) -> True) input -> Transformed{..}
    where
        infoLines = [oldF ++ newF ++ "(PDO::" ++ par ++ ")"]
        transfResult = pure $ LToRPDOAssign input newF par
  _                                -> transfNothing

replaceFunctWMethod :: String -> String -> String -> Ast -> Transformed Ast
replaceFunctWMethod oldF newF obj = modAll $ \case
  RFuncLGen ((== oldF) -> True) input -> Transformed{..}
    where
        infoLines = [oldF ++ " => " ++ obj ++ "->"++ newF]
        transfResult = pure $ LToRAssign obj newF (Right [SinglePar input])
  _                                -> transfNothing

noDie :: Ast -> Transformed Ast
noDie = modAll $ \case
  DoOrDie res query -> Transformed{..}
    where
        infoLines = [res ++ "->" ++ show query]
        transfResult = pure $ DoDontDie res query
  DoOrDieNoAsn query -> Transformed{..}
    where
        infoLines = [show query]
        transfResult = pure $ WrappedSQLQuery query
  _                                -> transfNothing

extend1to2 :: String -> String -> Ast -> Transformed Ast
extend1to2 match par2 = modAll $ \case
  RFuncL ((== match) -> True) par1 -> Transformed{..}
    where
        infoLines = []
        transfResult = pure $ ROnlyVal match
                (Right [ SimplePar par1
                       , SimplePar par2
                       ])
  _                                -> transfNothing
  
pattern SimplePar par = SinglePar (SimpleName par)

pattern SinglePar query <- WSCap
                    { wsCapPre = _
                    , wsCapMain = query
                    , wsCapPost = _
                    }
    where
        SinglePar query = WSCap
            { wsCapPre = []
            , wsCapMain = query
            , wsCapPost = []
            }

pattern MysqlQuery query = ROnlyVal "mysql_query" (Right [ SinglePar query ])

pattern DoSQLQuery query = ROnlyVal "doSQL"
    (Right
        [ SinglePar query
        , SimplePar "db"
        , NoDisplaySQL
        ])

pattern SimpleName name <- Left (ExprRVal (RValLRVal (LRValVar (DynConst _ (Var name _)))))
    where
        SimpleName name = Left (ExprRVal (RValLRVal (LRValVar (DynConst [] (Var name [])))))

pattern NoDisplaySQL = SinglePar (Left (ExprStrLit (StrLit "\"no\"")))

pattern LToRAssignSimple funL funR = LToRAssign funL funR (Left [])

pattern LToRPDOAssign funL funR attr = LToRAssign funL funR (Right [AssignAttr "PDO" attr])

pattern LToRAssign funL funR vars <- ROnlyValFunc
          (Left
             (LRValMemb (RValLRVal (LRValVar (DynConst _ (Var funL _))))
                _ (MembStr funR)))
          _ vars
    where
        LToRAssign funL funR vars = ROnlyValFunc
          (Left
             (LRValMemb (RValLRVal (LRValVar (DynConst [] (Var funL []))))
                ([], []) (MembStr funR)))
          [] vars

pattern ROnlyVal funR par <- ROnlyValFunc (Right (Const _ funR)) _ par
    where ROnlyVal funR par = ROnlyValFunc (Right (Const [] funR)) [] par

pattern RFuncLGen funR cont = ROnlyVal funR
          (Right [ SinglePar cont ] )

pattern RFuncL funR input = RFuncLGen funR (SimpleName input)

pattern AssignAttr pre attr <- SinglePar (Left (ExprRVal (RValROnlyVal (ROnlyValConst (Const [(pre,_)] attr)))))
    where
        AssignAttr pre attr = SinglePar (Left (ExprRVal (RValROnlyVal (ROnlyValConst (Const [(pre,([],[]))] attr)))))

pattern DoOrDie res query <- StmtExpr (ExprAssign
           _
           (LValLRVal (LRValVar (DynConst [] (Var res []))))
           _
           (OrDie query)
           ) _ StmtEndSemi
    where
        DoOrDie res query = StmtExpr (ExprAssign
           Nothing
           (LValLRVal (LRValVar (DynConst [] (Var res []))))
           ([WS " "],[WS " "])
           (OrDie query)
           ) [] StmtEndSemi

pattern DoDontDie res query <- StmtExpr (ExprAssign
           _
           (LValLRVal (LRValVar (DynConst _ (Var res _))))
           _
           (ExprRVal (RValROnlyVal (DoSQLQuery query)))
           ) [] StmtEndSemi
    where
        DoDontDie res query = StmtExpr (ExprAssign
           Nothing
           (LValLRVal (LRValVar (DynConst [] (Var res []))))
           ([WS " "],[WS " "])
           (ExprRVal (RValROnlyVal (DoSQLQuery query)))
           ) [] StmtEndSemi

pattern DoOrDieNoAsn query = StmtExpr (OrDie query) [] StmtEndSemi

pattern WrappedSQLQuery query = StmtExpr (ExprRVal (RValROnlyVal (DoSQLQuery query))) [] StmtEndSemi

pattern OrDie query <-
        ExprBinOp BOrWd (ExprRVal (RValROnlyVal (DoSQLQuery query))) _
          (ExprExit _
             (Just
                (_,
                 Right (SinglePar
                           (ExprBinOp _ _ _
                             (ExprRVal
                                (RValROnlyVal
                                   (ROnlyVal "mysql_error" _)))
                       )))))
    where
        OrDie query = ExprBinOp
                BOrWd
                (ExprRVal (RValROnlyVal (DoSQLQuery query)))
                ([WS " \n     "], [WS " "])
                (ExprExit False
                   (Just
                      ([WS " "], Right (SinglePar
                                     (ExprBinOp
                                         (BByable BConcat)
                                         (ExprStrLit
                                            (StrLit "\"Unexpected MySQL Error: \""))
                                         ([WS " "], [WS " "])
                                         (ExprRVal
                                            (RValROnlyVal
                                               (ROnlyVal "mysql_error" (Left []))))
                                     )))))

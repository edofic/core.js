module Immediate.GenerateJs where

import Immediate.Syntax
import qualified Language.ECMAScript3.Syntax as JS
import Data.List (intercalate)

runtime = var "runtime"
runtimeMkThunk = JS.DotRef() runtime $ JS.Id() "mkthunk"
varDecl name expr = JS.VarDeclStmt() $ [JS.VarDecl() (JS.Id() name) $ Just $ expr]
var = JS.VarRef() . JS.Id()

wrapScope :: [JS.Statement ()] -> JS.Expression ()
wrapScope stmts = JS.CallExpr() (JS.FuncExpr() Nothing [] stmts) []

genLiteral :: Literal -> JS.Expression()
genLiteral (LiteralInteger n) = JS.IntLit() $ fromInteger n
genLiteral (LiteralRational r) = JS.NumLit() $ fromRational r
genLiteral (LiteralChar c) = JS.StringLit() [c]
genLiteral (LiteralString s) = JS.StringLit() s

genVdef :: Vdef -> JS.Statement ()
genVdef (Vdef name expr) = 
  if elem '.' name 
  then JS.ExprStmt() $ JS.AssignExpr() JS.OpAssign (JS.LVar() name) $ genExpr expr
  else varDecl name $ genExpr expr

genExpr :: Expression a -> JS.Expression ()
genExpr (Lit lit) = genLiteral lit
genExpr (Var name) = var name
genExpr (Lam name body) = JS.FuncExpr() Nothing [JS.Id() name] 
  [JS.ReturnStmt() $ Just $ genExpr body]
genExpr (App  f x) = JS.CallExpr() (genExpr f) [genExpr x]
genExpr (Let definitons expr) = 
  wrapScope $ fmap genVdef definitons ++ [JS.ReturnStmt() $ Just $ genExpr expr]
genExpr (Case expr name alts) = error "Not implemented: js case"
genExpr (Force expr) = JS.CallExpr() (genExpr expr) []
genExpr (Defer expr) = JS.CallExpr() runtimeMkThunk [JS.FuncExpr() Nothing [] [body]] where
  body = JS.ReturnStmt() $ Just $ genExpr expr


{-
data Alt = AltDefault (Expression Deferred)
         | AltLit Literal (Expression Deferred)
         | AltCon Name [Name] (Expression Deferred)
         deriving (Show, Eq)
-}

genTdef :: Tdef -> [JS.Statement ()]
genTdef = undefined

genImport :: Dependency -> JS.Statement()
genImport (Dependency varName (exe, parts, name)) = 
  varDecl varName $ JS.CallExpr() (var "require") [JS.StringLit() path] where
    path = exe ++ "/" ++ intercalate "_" (parts ++ [name])

genModule :: Module -> JS.JavaScript ()
genModule (Module name dependencies tdefs vdefs) = JS.Script() $ prolog ++ imprts ++ types ++ values where
  prolog = [varDecl name $ var "exports"]
  imprts = fmap genImport dependencies
  types = tdefs >>= genTdef
  values = fmap genVdef vdefs

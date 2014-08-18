module Immediate.GenerateJs where

import Immediate.Syntax
import qualified Language.ECMAScript3.Syntax as JS

runtime = JS.VarRef() $ JS.Id() "runtime"
runtimeMkThunk = JS.DotRef() runtime $ JS.Id() "mkthunk"

wrapScope :: [JS.Statement ()] -> JS.Expression ()
wrapScope stmts = JS.CallExpr() (JS.FuncExpr() Nothing [] stmts) []

genName :: Name -> JS.Expression()
genName = JS.VarRef() . JS.Id() 

genLiteral :: Literal -> JS.Expression()
genLiteral (LiteralInteger n) = JS.IntLit() $ fromInteger n
genLiteral (LiteralRational r) = JS.NumLit() $ fromRational r
genLiteral (LiteralChar c) = JS.StringLit() [c]
genLiteral (LiteralString s) = JS.StringLit() s

genDef :: Vdef -> JS.Statement ()
genDef (Vdef name expr) = 
  JS.VarDeclStmt() $ [JS.VarDecl() (JS.Id() name) $ Just $ genExpr expr]

genExpr :: Expression a -> JS.Expression ()
genExpr (Lit lit) = genLiteral lit
genExpr (Var name) = genName name
genExpr (Lam name body) = JS.FuncExpr() Nothing [JS.Id() name] 
  [JS.ReturnStmt() $ Just $ genExpr body]
genExpr (App  f x) = JS.CallExpr() (genExpr f) [genExpr x]
genExpr (Let definitons expr) = 
  wrapScope $ fmap genDef definitons ++ [JS.ReturnStmt() $ Just $ genExpr expr]
genExpr (Case expr name alts) = error "Not implemented: js case"
genExpr (Force expr) = JS.CallExpr() (genExpr expr) []
genExpr (Defer expr) = JS.CallExpr() runtime [JS.FuncExpr() Nothing [] [body]] where
  body = JS.ReturnStmt() $ Just $ genExpr expr


{-
data Alt = AltDefault (Expression Deferred)
         | AltLit Literal (Expression Deferred)
         | AltCon Name [Name] (Expression Deferred)
         deriving (Show, Eq)
-}

genModule :: Module -> JS.JavaScript()
genModule (Module dependencies name tdefs vdefs) = error "Not implemented: js module"

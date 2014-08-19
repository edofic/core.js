module Immediate.GenerateJs where

import Immediate.Syntax
import qualified Language.ECMAScript3.Syntax as JS
import qualified Language.ECMAScript3.Syntax.QuasiQuote as JsQ

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


genVdefJs :: Name -> JS.Expression() -> JS.Statement()
genVdefJs name expr =
  if elem '.' name 
  then JS.ExprStmt() $ JS.AssignExpr() JS.OpAssign (JS.LVar() name) expr
  else varDecl name expr

genVdef :: Vdef -> JS.Statement ()
genVdef (Vdef name expr) = genVdefJs name $ genExpr expr

defer :: JS.Expression() -> JS.Expression()
defer expr = JS.CallExpr() runtimeMkThunk [JS.FuncExpr() Nothing [] [body]] where
  body = JS.ReturnStmt() $ Just expr

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
genExpr (Defer expr) = defer $ genExpr expr


{-
data Alt = AltDefault (Expression Deferred)
         | AltLit Literal (Expression Deferred)
         | AltCon Name [Name] (Expression Deferred)
         deriving (Show, Eq)
-}

genTdef :: Tdef -> [JS.Statement ()]
genTdef (Data constructors) = concat $ zipWith mkData [0..] constructors where
  mkData i (Constructor name arity) = [varDecl name $ apply arity, varDecl name $ unapply] where
    apply 0 = defer $ JS.ArrayLit() $ 
      (JS.IntLit() i) : fmap (\p -> var $ "arg" ++ show p) [arity,arity-1..1]
    apply p = defer $ JS.FuncExpr() Nothing [JS.Id() $ "arg" ++ show p]
                      [JS.ReturnStmt() $ Just $ apply (p-1)]
    
    unapply = defer $ JS.FuncExpr() Nothing [JS.Id() "data", JS.Id() "f"] [force, iff] where
        force = JS.ExprStmt() $ JS.AssignExpr() JS.OpAssign (JS.LVar() "data") $
                  JS.CallExpr() (var "data") []
        iff = JS.IfSingleStmt() predicate $ JS.ExprStmt() callback 
        predicate = JS.InfixExpr() JS.OpEq consId $ JS.IntLit() i
        consId = JS.BracketRef() (var "data") $ JS.IntLit() 0
        callback = JS.CallExpr() (JS.DotRef() (var "f") (JS.Id() "apply")) [var "undefined", args]
        args = JS.CallExpr() (JS.DotRef() (var "data") (JS.Id() "slice")) [JS.IntLit() 1]

genTdef (Newtype name) = [apply, unapply] where
  apply = genVdefJs name $ defer $ JS.FuncExpr() Nothing [JS.Id() "x"] 
                                  [JS.ReturnStmt() $ Just $ var "x"]
  unapply = genVdefJs (name ++ ".unapply") $ defer $
    JS.FuncExpr() Nothing [JS.Id() "x", JS.Id() "f"] 
      [JS.ReturnStmt() $ Just $ JS.CallExpr() (var "f") [var "x"]]
 

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

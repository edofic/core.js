module Immediate.GenerateJs where

import Immediate.Syntax
import qualified Language.ECMAScript3.Syntax as JS
import qualified Language.ECMAScript3.Syntax.QuasiQuote as JsQ
import GHC.Real

import Data.List (intercalate)

runtime = var "___runtime"
runtimeMkThunk = JS.DotRef() runtime $ JS.Id() "mkthunk"
var = JS.VarRef() . JS.Id()
jsInt n = JS.CallExpr() (JS.DotRef() runtime $ JS.Id() "intlit") [JS.StringLit() $ show n]

wrapScope :: [JS.Statement ()] -> JS.Expression ()
wrapScope stmts = JS.CallExpr() (JS.FuncExpr() Nothing [] stmts) []

genLiteral :: Literal -> JS.Expression()
genLiteral (LiteralInteger n) = jsInt n
genLiteral (LiteralRational (e :% d)) = JS.CallExpr() (JS.DotRef() (jsInt e) $ JS.Id() "div") [jsInt d]
genLiteral (LiteralChar c) = JS.StringLit() [c]
genLiteral (LiteralString s) = JS.StringLit() s


varDecl :: Name -> JS.Expression() -> JS.Statement()
varDecl name expr =
  if elem '.' name 
  then JS.ExprStmt() $ JS.AssignExpr() JS.OpAssign (JS.LVar() name) expr
  else JS.VarDeclStmt() $ [JS.VarDecl() (JS.Id() name) $ Just $ expr]

genVdef :: Vdef -> JS.Statement ()
genVdef (Vdef name expr) = varDecl name $ genExpr expr

defer :: JS.Expression() -> JS.Expression()
defer expr = JS.CallExpr() runtimeMkThunk [JS.FuncExpr() Nothing [] [body]] where
  body = JS.ReturnStmt() $ Just expr

forceExp :: JS.Expression() -> JS.Expression()
forceExp expr = JS.CallExpr() expr []

genAlt :: Name -> Alt -> JS.Expression()
genAlt _ (AltDefault rhs) = genExpr rhs
genAlt name (AltLit lit rhs) = JS.CondExpr() cond (genExpr rhs) $ var "undefined" where
  cond = JS.InfixExpr() JS.OpStrictEq (var name) (genLiteral lit)
genAlt name (AltCon conName names rhs) = 
  JS.CallExpr() (JS.DotRef() (var conName) (JS.Id() "unapply")) 
    [ var name
    , JS.FuncExpr() Nothing (JS.Id() `fmap` names) 
        [ JS.ReturnStmt() $ Just $ genExpr rhs
        ]
    ]

genExpr :: Expression a -> JS.Expression ()
genExpr (Lit lit) = genLiteral lit
genExpr (Var name) = var name
genExpr (Lam name body) = JS.FuncExpr() Nothing [JS.Id() name] 
  [JS.ReturnStmt() $ Just $ genExpr body]
genExpr (App  f x) = JS.CallExpr() (genExpr f) [genExpr x]
genExpr (Let definitons expr) = 
  wrapScope $ fmap genVdef definitons ++ [JS.ReturnStmt() $ Just $ genExpr expr]
genExpr (Case expr name alts) = wrapScope 
  [ varDecl name $ genExpr expr
  , JS.ReturnStmt() $ Just $ foldr joinAlt (var "undefined") alts
  ] where
    joinAlt alt els = JS.InfixExpr() JS.OpLOr (genAlt name alt) els 
genExpr (Force expr) = forceExp $ genExpr expr
genExpr (Defer expr) = defer $ genExpr expr


{-
data Alt = AltDefault (Expression Deferred)
         | AltLit Literal (Expression Deferred)
         | AltCon Name [Name] (Expression Deferred)
         deriving (Show, Eq)
-}

genTdef :: Tdef -> [JS.Statement ()]
genTdef (Data constructors) = concat $ zipWith mkData [0..] constructors where
  mkData i (Constructor name arity) = 
    [ varDecl name $ defer $ apply arity
    , varDecl (name ++ ".unapply") $ unapply
    ] where
      apply 0 = JS.ArrayLit() $ 
        (JS.IntLit() i) : fmap (\p -> var $ "arg" ++ show p) [arity,arity-1..1]
      apply p = JS.FuncExpr() Nothing [JS.Id() $ "arg" ++ show p]
                [JS.ReturnStmt() $ Just $ apply (p-1)]
      
      unapply = JS.FuncExpr() Nothing [JS.Id() "data", JS.Id() "f"] [force, iff] where
          force = JS.ExprStmt() $ JS.AssignExpr() JS.OpAssign (JS.LVar() "data") $ forceExp $ var "data"
          iff = JS.IfSingleStmt() predicate $ JS.ReturnStmt() $ Just $ callback 
          predicate = JS.InfixExpr() JS.OpEq consId $ JS.IntLit() i
          consId = JS.BracketRef() (var "data") $ JS.IntLit() 0
          callback = JS.CallExpr() (JS.DotRef() (var "f") (JS.Id() "apply")) [var "undefined", args]
          args = JS.CallExpr() (JS.DotRef() (var "data") (JS.Id() "slice")) [JS.IntLit() 1]

genTdef (Newtype name) = [apply, unapply] where
  apply = varDecl name $ defer $ JS.FuncExpr() Nothing [JS.Id() "x"] 
                                  [JS.ReturnStmt() $ Just $ var "x"]
  unapply = varDecl (name ++ ".unapply") $ defer $
    JS.FuncExpr() Nothing [JS.Id() "x", JS.Id() "f"] 
      [JS.ReturnStmt() $ Just $ JS.CallExpr() (var "f") [var "x"]]
 

genImport :: Dependency -> JS.Statement()
genImport (Dependency varName (exe, parts, name)) = 
  varDecl varName $ JS.CallExpr() (var "require") [JS.StringLit() path] where
    path = "../" ++ exe ++ "/" ++ intercalate "_" (parts ++ [name]) ++ ".js"

genModule :: Module -> JS.JavaScript ()
genModule (Module name dependencies tdefs vdefs) = JS.Script() $ prolog ++ imprts ++ types ++ values where
  prolog = [varDecl name $ var "exports"]
  imprts = fmap genImport (runtimeDep : dependencies)
  types = tdefs >>= genTdef
  values = fmap genVdef vdefs
  runtimeDep = Dependency "___runtime" ("runtime", [], "runtime")


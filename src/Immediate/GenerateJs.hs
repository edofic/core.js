module Immediate.GenerateJs where

import Immediate.Syntax
import qualified Language.JavaScript.Parser.AST as JS

genName :: Name -> JS.JSNode
genName = JS.NN . JS.JSIdentifier 

genLiteral :: Literal -> JS.JSNode
genLiteral (LiteralInteger n) = JS.NN $ JS.JSDecimal $ show n
genLiteral (LiteralRational r) = JS.NN $ JS.JSDecimal $ show r
genLiteral (LiteralChar c) = JS.NN $ JS.JSStringLiteral '\'' [c]
genLiteral (LiteralString s) = JS.NN $ JS.JSStringLiteral '\'' s

genExpr :: Expression a -> JS.JSNode
genExpr (Lit lit) = genLiteral lit
genExpr (Var name) = genName name
genExpr (Lam name body) = error "Not implemented: lambda js"
genExpr (App  f x) = error "Not implemented: js application"
genExpr (Let definitons expr) = error "Not implemented: js let"
genExpr (Case expr name alts) = error "Not implemented: js case"
genExpr (Force expr) = error "Not implemented: js force"
genExpr (Defer expr) = error "Not implemented: js defer"


{-
data Alt = AltDefault (Expression Deferred)
         | AltLit Literal (Expression Deferred)
         | AltCon Name [Name] (Expression Deferred)
         deriving (Show, Eq)
-}

genModule :: Module -> JS.JSNode
genModule (Module name definitons) = error "Not implemented: js module"

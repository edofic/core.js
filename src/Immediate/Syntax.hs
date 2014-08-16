module Immediate.Syntax where

type Name = String

data Literal = LiteralInteger Integer 
             | LiteralRational Rational
             | LiteralChar Char
             | LiteralString String
             deriving (Show, Eq)

data Alt = AltDefault (Expression Deferred)
         | AltLit Literal (Expression Deferred)
         | AltCon Name [Name] (Expression Deferred)
         deriving (Show, Eq)


data Evaluation = Deferred | Forced deriving (Eq, Show)

data Expression :: Evaluation -> * where
  Lit :: Literal -> Expression Forced
  Var :: Name -> Expression Deferred
  Lam :: Name -> (Expression Forced) -> Expression Deferred
  App :: Expression Forced -> Expression Deferred -> Expression Forced
  Let :: [Definition] -> Expression Deferred -> Expression Deferred
  Case :: (Expression Deferred) -> Name -> [Alt] -> Expression Deferred -- TODO
  Force :: Expression Deferred -> Expression Forced
  Defer :: Expression Forced -> Expression Deferred



deriving instance Eq (Expression a)
deriving instance Show (Expression a)

data Definition = Definition Name (Expression Deferred) deriving (Eq, Show)

data Module = Module Name [Definition] deriving (Eq, Show)


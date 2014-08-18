module Immediate.Syntax where

type Name = String
type Arity = Int

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
  Lam :: Name -> (Expression Forced) -> Expression Forced
  App :: Expression Forced -> Expression Deferred -> Expression Forced
  Let :: [Vdef] -> Expression Deferred -> Expression Deferred
  Case :: (Expression Deferred) -> Name -> [Alt] -> Expression Deferred 
  Force :: Expression Deferred -> Expression Forced
  Defer :: Expression Forced -> Expression Deferred

deriving instance Eq (Expression a)
deriving instance Show (Expression a)

data Vdef = Vdef Name (Expression Deferred) deriving (Eq, Show)

data Constructor = Constructor Name Arity deriving (Eq, Show)

data Tdef = Data [Constructor]
          | Newtype Name 
          deriving (Eq, Show)

data Dependency = Dependency Name [Name] Name deriving (Eq, Show)

data Module = Module Name [Dependency] [Tdef] [Vdef] deriving (Eq, Show)


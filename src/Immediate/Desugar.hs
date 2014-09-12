module Immediate.Desugar where

import Immediate.Syntax 
import Immediate.Dependencies (usedModules)
import qualified Language.Core.Core as C
import Control.Applicative
import qualified Data.Set as Set
import Data.List (intercalate)

class RenderName a where
  renderName :: a -> Name

instance RenderName C.AnMname where
  renderName (C.M (C.P pkg, parts, name))= 
    "__" ++ pkg ++ "__" ++ intercalate "_" (parts ++ [name])

instance RenderName (C.Qual Name) where
  renderName (mname, name) = 
    (maybe "" (\mn -> renderName mn ++ ".") mname) ++ name

desugarModule :: C.Module -> Module
desugarModule cmod@(C.Module mName tdefs vdefgs) = 
  Module newName depdendencies typeDefs valueDefs
  where
  newName = renderName mName
  depdendencies = fmap unAnMname $ Set.toList $ usedModules cmod
  typeDefs = desugarTypeDefinition `fmap` tdefs
  valueDefs = desugarDefinition <$> (vdefgs >>= unVdefg)

unAnMname :: C.AnMname -> Dependency
unAnMname whole@(C.M (C.P e, parts, name)) = Dependency (renderName whole) (e, parts, name)

unVdefg :: C.Vdefg -> [C.Vdef]
unVdefg (C.Rec vdefs) = vdefs
unVdefg (C.Nonrec def) = [def]

desugarTypeDefinition :: C.Tdef -> Tdef
desugarTypeDefinition (C.Newtype name _ _ _) = Newtype (renderName name)
desugarTypeDefinition (C.Data _ _ cdefs) = Data $ fmap mkCons cdefs where
  mkCons (C.Constr name  _ args) = Constructor (renderName name) (length args)

desugarDefinition :: C.Vdef -> Vdef 
desugarDefinition (C.Vdef (name, _, expr)) = 
  Vdef (renderName name) (desugarExpr expr)

desugarExpr :: C.Exp -> Expression Deferred
desugarExpr (C.Var name) = Var $ renderName name
desugarExpr (C.Dcon name) = Var $ renderName name
desugarExpr (C.Lit literal) = Defer $ Lit $ desugarLiteral literal
desugarExpr (C.App f x) = Defer $ App (Force $  desugarExpr f) (desugarExpr x)
desugarExpr (C.Lam (C.Tb _) expr) = desugarExpr expr
desugarExpr (C.Lam (C.Vb (name, _)) expr) = Defer $ Lam name $ Force $ desugarExpr expr
desugarExpr (C.Let vdefg expr) = Let (fmap bind $ unVdefg vdefg) (desugarExpr expr) where
  bind (C.Vdef (name, _, expr)) = Vdef (renderName name) (desugarExpr expr)
desugarExpr (C.Case expr (name, _) _ alts) = 
  Case (desugarExpr expr) name (fmap desugarAlt alts)
desugarExpr (C.Note _ expr) = desugarExpr expr
desugarExpr (C.Appt expr _) = desugarExpr expr
desugarExpr (C.Cast expr _) = desugarExpr expr
desugarExpr (C.External _ _) = error "Not implemented: expression External"

desugarLiteral :: C.Lit -> Literal
desugarLiteral (C.Literal (C.Lint n)      _) = LiteralInteger  n
desugarLiteral (C.Literal (C.Lrational r) _) = LiteralRational r
desugarLiteral (C.Literal (C.Lchar c)     _) = LiteralChar     c
desugarLiteral (C.Literal (C.Lstring s)   _) = LiteralString   s

desugarAlt :: C.Alt -> Alt 
desugarAlt (C.Adefault expr) = AltDefault $ desugarExpr expr
desugarAlt (C.Alit lit expr) = AltLit (desugarLiteral lit) (desugarExpr expr)
desugarAlt (C.Acon dcon _ names expr) = 
  AltCon (renderName dcon) (fmap fst names) (desugarExpr expr)

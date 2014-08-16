module Immediate.Desugar where

import Immediate.Syntax 
import qualified Language.Core.Core as C
import Control.Applicative

class RenderName a where
  renderName :: a -> Name

instance RenderName C.AnMname where
  renderName = error "Not implemented: RenderName AnMname"

instance RenderName (C.Qual C.Var) where
  renderName = error "Not implemented: RenderName (Qual Var)"

instance RenderName C.Bind where
  renderName (C.Vb (name, _)) = name
  renderName (C.Tb _) = error "Not implemented: type binds"

desugarModule :: C.Module -> Module
desugarModule (C.Module mName tdefs vdefgs) = Module newName definitions where
  newName = renderName mName
  definitions = (tdefs >>= desugarTypeDefinition) ++ (desugarDefinition <$> (vdefgs >>= unVdefg))

unVdefg :: C.Vdefg -> [C.Vdef]
unVdefg (C.Rec vdefs) = vdefs
unVdefg (C.Nonrec def) = [def]


desugarTypeDefinition :: C.Tdef -> [Definition]
desugarTypeDefinition = error "Not implemented: desugarTypeDefinition"

desugarDefinition :: C.Vdef -> Definition 
desugarDefinition (C.Vdef (name, _, expr)) = 
  Definition (renderName name) (desugarExpr expr)

desugarExpr :: C.Exp -> Expression Deferred
desugarExpr (C.Var name) = Var $ renderName name
desugarExpr (C.Dcon name) = Var $ renderName name
desugarExpr (C.Lit literal) = Defer $ Lit $ desugarLiteral literal
desugarExpr (C.App f x) = Defer $ App (Force $  desugarExpr f) (desugarExpr x)
desugarExpr (C.Lam bind expr) = Lam (renderName bind) (Force $ desugarExpr expr)
desugarExpr (C.Let vdefg expr) = Let (fmap bind $ unVdefg vdefg) (desugarExpr expr) where
  bind (C.Vdef (name, _, expr)) = Definition (renderName name) (desugarExpr expr)
desugarExpr (C.Case expr (name, _) _ alts) = 
  Case (desugarExpr expr) name (fmap desugarAlt alts)
desugarExpr (C.Note _ expr) = desugarExpr expr
desugarExpr (C.Appt _ _) = error "Not implemented: expression Appt"
desugarExpr (C.Cast _ _) = error "Not implemented: expression Cast"
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

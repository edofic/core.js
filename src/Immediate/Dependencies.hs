-- extracted from extcore internal implementation 
-- and slimmed down since we don't need transitive dependencies
module Immediate.Dependencies where

import Language.Core.Core
import qualified Data.Set as S
import Data.Maybe
import Data.List

usedModules :: Module -> S.Set AnMname
usedModules (Module _ tds vds) = usedModuleVds vds

usedModuleVds :: [Vdefg] -> S.Set AnMname
usedModuleVds = some usedModuleVdefg

usedModuleVdefg :: Vdefg -> S.Set AnMname
usedModuleVdefg (Rec vds) = some usedModuleVdef vds
usedModuleVdefg (Nonrec vdef) = usedModuleVdef vdef

usedModuleVdef :: Vdef -> S.Set AnMname
usedModuleVdef (Vdef (_,t,e)) = usedModuleExp e

usedModuleExp :: Exp -> S.Set AnMname
usedModuleExp (Var v) | Just m' <- getModule v = S.singleton m'
usedModuleExp (Dcon d) | Just m' <- getModule d = S.singleton m'
usedModuleExp (Var _) = S.empty
usedModuleExp (Dcon _) = S.empty
usedModuleExp (Lit _) = S.empty
usedModuleExp (App a b) = someExps [a,b]
usedModuleExp (Appt e _) = usedModuleExp e 
usedModuleExp (Lam _ e) = usedModuleExp e
usedModuleExp (Let vd e) = usedModuleVdefg vd `S.union` usedModuleExp e
usedModuleExp (Case e _ _ alts) = usedModuleExp e `S.union` usedModuleAlts alts
usedModuleExp (Cast e _) = usedModuleExp e
usedModuleExp (Note _ e) = usedModuleExp e
usedModuleExp (External _ _) = S.empty

usedModuleAlts :: [Alt] -> S.Set AnMname
usedModuleAlts = some go'
  where go' (Acon dc _ _ e) = case getModule dc of
           Just m' -> S.insert m' (usedModuleExp e)
           _       -> usedModuleExp e
        go' (Alit _ e) = usedModuleExp e
        go' (Adefault e) = usedModuleExp e

some :: (a -> S.Set AnMname) -> [a] -> S.Set AnMname 
some f = S.unions . map f

someExps :: [Exp] -> S.Set AnMname
someExps = some usedModuleExp
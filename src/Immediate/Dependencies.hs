-- extracted from extcore internal implementation 
-- and slimmed down since we don't need transitive dependencies
module Immediate.Dependencies where

import Language.Core.Core
import qualified Data.Set as S
import Data.Maybe
import Data.List

usedModules :: Module -> S.Set AnMname
usedModules (Module _ tds vds) = usedModuleTds tds `S.union` usedModuleVds vds

usedModuleTds :: [Tdef] -> S.Set AnMname
usedModuleTds = some usedModuleTd

usedModuleTd :: Tdef -> S.Set AnMname
usedModuleTd (Data _ _ cds) = S.unions
  (map (\ (Constr _ _ ts) -> some usedModuleTy ts) cds)
usedModuleTd (Newtype _ _ _ t) = usedModuleTy t

usedModuleVds :: [Vdefg] -> S.Set AnMname
usedModuleVds = some usedModuleVdefg

usedModuleVdefg :: Vdefg -> S.Set AnMname
usedModuleVdefg (Rec vds) = some usedModuleVdef vds
usedModuleVdefg (Nonrec vdef) = usedModuleVdef vdef

usedModuleVdef :: Vdef -> S.Set AnMname
usedModuleVdef (Vdef (_,t,e)) = usedModuleTy t `S.union` usedModuleExp e

usedModuleExp :: Exp -> S.Set AnMname
usedModuleExp (Var v) | Just m' <- getModule v = S.singleton m'
usedModuleExp (Dcon d) | Just m' <- getModule d = S.singleton m'
usedModuleExp (Var _) = S.empty
usedModuleExp (Dcon _) = S.empty
usedModuleExp (Lit _) = S.empty
usedModuleExp (App a b) = someExps [a,b]
usedModuleExp (Appt e t) = usedModuleExp e `S.union` usedModuleTy t
usedModuleExp (Lam _ e) = usedModuleExp e
usedModuleExp (Let vd e) = usedModuleVdefg vd `S.union` usedModuleExp e
usedModuleExp (Case e _ t alts) = usedModuleExp e `S.union`
  usedModuleTy t `S.union` usedModuleAlts alts
usedModuleExp (Cast e t) = usedModuleExp e `S.union` usedModuleTy t
usedModuleExp (Note _ e) = usedModuleExp e
usedModuleExp (External _ t) = usedModuleTy t

usedModuleTy :: Ty -> S.Set AnMname
usedModuleTy (Tvar _) = S.empty
usedModuleTy (Tcon t) | Just m' <- getModule t = S.singleton m'
usedModuleTy (Tcon _) = S.empty
usedModuleTy (Tapp t u) = usedModuleTy t `S.union` usedModuleTy u
usedModuleTy (Tforall _ t) = usedModuleTy t
usedModuleTy (TransCoercion t u) = usedModuleTy t `S.union` usedModuleTy u
usedModuleTy (SymCoercion t) = usedModuleTy t
usedModuleTy (UnsafeCoercion t u) = usedModuleTy t `S.union` usedModuleTy u
usedModuleTy (InstCoercion t u) = usedModuleTy t `S.union` usedModuleTy u
usedModuleTy (LeftCoercion t) = usedModuleTy t
usedModuleTy (RightCoercion t) = usedModuleTy t

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
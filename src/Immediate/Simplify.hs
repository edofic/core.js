module Immediate.Simplify where

import Immediate.Syntax

class Simplify a where
  simplify :: a -> a

instance Simplify (Expression a) where
  simplify = simplifyForceDefer

instance Simplify Module where
  simplify (Module name deps tdefs vdefs) = Module name deps tdefs $ simplify `fmap` vdefs

instance Simplify Vdef where
  simplify (Vdef name expr) = Vdef name $ simplify expr

simplifyForceDefer :: Expression a -> Expression a
simplifyForceDefer (Lam name expr) = Lam name $ simplifyForceDefer expr
simplifyForceDefer (App f x) = App (simplifyForceDefer f) (simplifyForceDefer x)
simplifyForceDefer (Let vdefs expr) = Let (simplifyVdef `fmap` vdefs) (simplifyForceDefer expr) where
    simplifyVdef (Vdef name expr) = Vdef name $ simplifyForceDefer expr
simplifyForceDefer (Case expr name alts) =
  Case (simplifyForceDefer expr) name $ simplifyAlt `fmap` alts where
    simplifyAlt (AltDefault expr) = AltDefault $ simplifyForceDefer expr
    simplifyAlt (AltLit lit expr) = AltLit lit $ simplifyForceDefer expr
    simplifyAlt (AltCon name names expr) = AltCon name names $ simplifyForceDefer expr
simplifyForceDefer (Force (Defer expr)) = simplifyForceDefer expr
simplifyForceDefer (Force expr) = Force $ simplifyForceDefer expr
simplifyForceDefer (Defer (Force expr)) = simplifyForceDefer expr
simplifyForceDefer (Defer expr) = Defer $ simplifyForceDefer expr
simplifyForceDefer l@(Lit _) = l
simplifyForceDefer v@(Var _) = v
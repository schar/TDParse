import TDParse.Data.Algebra

mutual
inductive FX where
  | query (env : Ty)
  | spawn
  | store (out : Ty)
  | scope (ret : Ty)
deriving BEq, DecidableEq

inductive Ty where
  | nat
  | bool
  | fn (a r : Ty)
  | comp (e : FX) (a : Ty)
deriving BEq, DecidableEq
end

@[match_pattern] abbrev E := Ty.nat
@[match_pattern] abbrev T := Ty.bool
@[match_pattern] abbrev S a := Ty.comp FX.spawn a
syntax "R^" term:max term:max : term
syntax "W^" term:max term:max : term
syntax "C^" term:max term:max : term
macro_rules
  | `(R^$e $a) => `(Ty.comp (FX.query $e) $a)
  | `(W^$o $a) => `(Ty.comp (FX.store $o) $a)
  | `(C^$r $a) => `(Ty.comp (FX.scope $r) $a)
infixr:50 " ~> " => Ty.fn

mutual
@[reducible]
def FX.dom : FX -> Type -> Type
  | .query e => Reader e.dom
  | .spawn   => List
  | .store o => Prod o.dom
  | .scope r => Cont r.dom

@[reducible]
def Ty.dom : Ty -> Type
  | .nat      => Nat
  | .bool     => Bool
  | .fn a r   => a.dom -> r.dom
  | .comp e a => e.dom a.dom
end

def functor : (f : FX) -> Option (Functor f.dom)
  | .spawn | .query _ | .store _ | .scope _ => some (inferInstanceAs _)

def applicative : (f : FX) -> Option (Applicative f.dom)
  | .spawn | .query _ | .scope _ => some (inferInstanceAs _)
  | _ => none

def monad : (f : FX) -> Option (Monad f.dom)
  | .spawn | .query _ | .scope _ => some (inferInstanceAs _)
  | _ => none

def adjoint : (f g : FX) -> Option (Adjoint f.dom g.dom)
  | .store o, .query e =>
      if h : o = e then by subst h; exact some (inferInstanceAs _)
      else none
  | _, _ => none

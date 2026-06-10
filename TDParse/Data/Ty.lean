import TDParse.Data.Algebra

inductive Entity where
  | num : Nat -> Entity
  | name : String -> Entity
  | rel : String -> Entity -> Entity
deriving BEq, DecidableEq, Hashable

namespace Entity

def default : Entity := .num 0

def asNat? : Entity -> Option Nat
  | .num n => some n
  | _ => none

def mapNat (f : Nat -> Nat) (x : Entity) : Entity :=
  match x.asNat? with
  | some n => .num (f n)
  | none => x

partial def toString : Entity -> String
  | .num n => s!"{n}"
  | .name s => s
  | .rel r x => r ++ "(" ++ x.toString ++ ")"

instance : ToString Entity where
  toString := Entity.toString

instance : Repr Entity where
  reprPrec x _ := Std.Format.text x.toString

end Entity

mutual
inductive FX where
  | query (env : Ty)
  | spawn
  | store (out : Ty)
  | scope (ret : Ty) (ans : Ty)   -- generalized: ret may differ from ans
deriving BEq, DecidableEq, Hashable

inductive Ty where
  | nat
  | bool
  | fn (a r : Ty)
  | comp (f : FX) (a : Ty)
deriving BEq, DecidableEq, Hashable
end

@[match_pattern] abbrev E := Ty.nat
@[match_pattern] abbrev T := Ty.bool
@[match_pattern] abbrev S a := Ty.comp FX.spawn a
syntax "R^" term:max term:max : term
syntax "W^" term:max term:max : term
syntax "C^" term:max term:max term:max : term   -- C^ ret ans payload
macro_rules
  | `(R^$e $a)     => `(Ty.comp (FX.query $e) $a)
  | `(W^$o $a)     => `(Ty.comp (FX.store $o) $a)
  | `(C^$r $s $a)  => `(Ty.comp (FX.scope $r $s) $a)
infixr:50 " ~> " => Ty.fn

mutual
@[reducible]
def FX.dom : FX -> Type -> Type
  | .query e   => Reader e.dom
  | .spawn     => List
  | .store o   => Prod o.dom
  | .scope r a => Cont2 r.dom a.dom  -- (payload → ans) → ret

@[reducible]
def Ty.dom : Ty -> Type
  | .nat      => Entity
  | .bool     => Bool
  | .fn a r   => a.dom -> r.dom
  | .comp f a => f.dom a.dom
end

def functor : (f : FX) -> Option (Functor f.dom)
  | .spawn | .query _ | .store _ | .scope _ _ => some (inferInstanceAs _)

def FX.functorOk (f : FX) : (functor f).isSome = true := by
  cases f <;> simp [functor]

def applicative : (f : FX) -> Option (Applicative f.dom)
  | .spawn | .query _ => some (inferInstanceAs _)
  | .store o => if h : o = T then by subst h; exact some (inferInstanceAs _)
                 else none
  | .scope r a => if h : r = a then by subst h; exact some (inferInstanceAs _)
                 else none

def monad : (f : FX) -> Option (Monad f.dom)
  | .spawn | .query _ => some (inferInstanceAs _)
  | .store o => if h : o = T then by subst h; exact some (inferInstanceAs _)
                 else none
  | .scope r a => if h : r = a then by subst h; exact some (inferInstanceAs _)
                 else none

def adjoint : (f g : FX) -> Option ((_ : Functor f.dom) × (_ : Functor g.dom) × Adjoint f.dom g.dom)
  | .store o, .query e =>
      if h : o = e then by subst h; exact some ⟨_, _, inferInstanceAs _⟩
      else none
  | _, _ => none

def Ty.commutative (t : Ty) : Bool :=
  t == T

def FX.commutative : (f : FX) -> Bool
  | .spawn | .query _ => true
  | .store o => Ty.commutative o
  | _ => false

def FX.invertible : (f : FX) -> Bool
  | .store _ => false
  | _ => true

-- A type is "evaluated" if it contains no unresolved scope effects.
-- Used to filter scope islands.
def Ty.evaluated : Ty -> Bool
  | .nat | .bool         => true
  | .fn _ r              => r.evaluated
  | .comp (.scope _ _) _ => false
  | .comp _ a            => a.evaluated

def comonad : (f : FX) -> Option (Comonad f.dom)
  | .store _ => some (inferInstanceAs _)
  | _        => none

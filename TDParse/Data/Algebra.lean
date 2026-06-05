-- just providing some definitions that appear to be missing from
-- whatever lean brings into scope by default

abbrev Reader := ReaderM

instance : Applicative List where
  pure x := List.singleton x
  seq mf mx := mf.flatMap λf => mx () |>.flatMap λx => [f x]

instance : Monad List where
  pure x := List.singleton x
  bind m f := List.flatMap f m

instance : Alternative List where
  failure := []
  orElse xs ys := xs ++ ys ()

instance : Functor (Prod o) where
  map := fun f (o, a) => (o, f a)

def Cont (r : Type) (a : Type) := (a -> r) -> r

instance : Functor (Cont r) where
  map f m := fun k => m (fun a => k (f a))

instance : Applicative (Cont r) where
  pure x := fun k => k x
  seq mf mx := fun k => mf (fun f => mx () (fun x => k (f x)))

instance : Monad (Cont r) where
  bind x f g := x fun i => f i g

-- Two-parameter continuation: return type `ret`, answer type `ans`.
-- When ret = ans this reduces to the ordinary Cont monad.
def Cont2 (ret ans : Type) (a : Type) := (a -> ans) -> ret

instance : Functor (Cont2 ret ans) where
  map f m := fun k => m (k ∘ f)

-- Applicative and Monad require ret = ans
instance : Applicative (Cont2 r r) where
  pure x := fun k => k x
  seq mf mx := fun k => mf (fun f => mx () (fun x => k (f x)))

instance : Monad (Cont2 r r) where
  bind x f g := x fun i => f i g

class Adjoint (f g : Type → Type) [Functor f] [Functor g] where
  unit   : a -> g (f a)
  counit : f (g a) -> a
  phi    : (f a -> b) -> a -> g b := fun c => Functor.map c ∘ unit
  psi    : (a -> g b) -> f a -> b := fun k => counit ∘ Functor.map k

instance : Adjoint (Prod e) (Reader e) where
  unit x := λ io => (io, x)
  counit p := p.2 p.1

-- Comonad: dual of Monad; Prod e is the canonical example.
class Comonad (f : Type → Type) extends Functor f where
  extract : f a → a
  extend  : (f a → b) → f a → f b

instance (e : Type) : Comonad (Prod e) where
  extract  := Prod.snd
  extend f p := (p.1, f p)

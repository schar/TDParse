/-
adapted from:
https://github.com/leanprover-community/mathlib4/blob/master/Mathlib/Util/MemoFix.lean
by Gabriel Ebner, Edward Ayers
-/

import Std.Data.HashMap.Basic

open ShareCommon

private unsafe abbrev ObjectMap := @Std.HashMap Object Object ⟨Object.ptrEq⟩ ⟨Object.hash⟩

private unsafe def memoFixImplObj (f : (Object → Object) → (Object → Object)) (a : Object) : Object :=
  unsafeBaseIO do
    let cache : IO.Ref ObjectMap ← ST.mkRef ∅
    let rec fix a := unsafeBaseIO do
      if let some b := (← cache.get)[a]? then return b
      let b := f fix a
      cache.modify (·.insert a b)
      pure b
    pure (fix a)


-- memoize a unary dependently-typed function
private unsafe def memoFixImpl {α : Type u} {γ : α → Type v} [forall a, Nonempty (γ a)] :
    (f : ((a : α) → γ a) → ((a : α) → γ a)) → ((a : α) → γ a) :=
  unsafeCast memoFixImplObj

@[implemented_by memoFixImpl]
opaque memoFix {α : Type u} {γ : α → Type v} [forall a, Nonempty (γ a)] :
    (f : ((a : α) → γ a) → ((a : α) → γ a)) → (a : α) → γ a


-- memoize a binary dependently-typed function
abbrev MDep (α : Type u) (β : Type v) (γ : α → β → Type w) := ((a : α) → (b : β) → γ a b)
abbrev PDep (α : Type u) (β : Type v) (γ : α → β → Type w) := ((p : α × β) → γ p.1 p.2)

def memoFix2 {α : Type u} {β : Type v} {γ : α → β → Type w} [∀ a b, Nonempty (γ a b)]
    (f : MDep α β γ -> MDep α β γ) : MDep α β γ :=

  let curry (h : PDep α β γ) := fun a b => h (a,b) -- have to define these because
  let uncurry (h : MDep α β γ) := fun (a,b) => h a b -- built-in ones have independent types

  curry $ memoFix (uncurry ∘ f ∘ curry)

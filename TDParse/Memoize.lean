/-
adapted from:
https://github.com/leanprover-community/mathlib4/blob/master/Mathlib/Util/MemoFix.lean
by Gabriel Ebner, Edward Ayers
-/

import Std.Data.HashMap.Basic

-- memoize a unary dependently-typed function by structural keys
private unsafe def memoFixImpl {α : Type} {γ : α → Type}
    [BEq α] [Hashable α] [forall a, Nonempty (γ a)] :
    (f : ((a : α) → γ a) → ((a : α) → γ a)) → ((a : α) → γ a) :=
  fun f => unsafeBaseIO do
    let cache : IO.Ref (Std.HashMap α ((a : α) × γ a)) ← ST.mkRef ∅
    let rec fix (a : α) : γ a := unsafeBaseIO do
      if let some b := (← cache.get)[a]? then
        return unsafeCast b.2
      let b := f fix a
      cache.modify (·.insert a ⟨a, b⟩)
      pure b
    pure fix

@[implemented_by memoFixImpl]
opaque memoFix {α : Type} {γ : α → Type}
    [BEq α] [Hashable α] [forall a, Nonempty (γ a)] :
    (f : ((a : α) → γ a) → ((a : α) → γ a)) → (a : α) → γ a


-- memoize a binary dependently-typed function by structural keys
abbrev MDep (α : Type) (β : Type) (γ : α → β → Type) := ((a : α) → (b : β) → γ a b)
abbrev PDep (α : Type) (β : Type) (γ : α → β → Type) := ((p : α × β) → γ p.1 p.2)

def memoFix2 {α : Type} {β : Type} {γ : α → β → Type}
    [BEq α] [Hashable α] [BEq β] [Hashable β] [∀ a b, Nonempty (γ a b)]
    (f : MDep α β γ -> MDep α β γ) : MDep α β γ :=

  let curry (h : PDep α β γ) := fun a b => h (a,b)
  let uncurry (h : MDep α β γ) := fun (a,b) => h a b

  curry $ memoFix (uncurry ∘ f ∘ curry)

-- Binary memoized fixed point with an explicit state cache.
-- Use this when keys are only meaningful inside one caller-provided scope.
abbrev MemoCache (α : Type) (β : Type) (γ : Type)
    [BEq (α × β)] [Hashable (α × β)] :=
  Std.HashMap (α × β) γ

abbrev MemoM (α : Type) (β : Type) (γ : Type)
    [BEq (α × β)] [Hashable (α × β)] :=
  StateM (MemoCache α β γ)

partial def memoFix2State {α : Type} {β : Type} {γ : Type}
    [BEq (α × β)] [Hashable (α × β)] [Nonempty γ]
    (f : (α -> β -> MemoM α β γ γ) -> α -> β -> MemoM α β γ γ) :
    α -> β -> MemoM α β γ γ :=
  fun a b => do
    let key := (a,b)
    if let some result := (← get)[key]? then
      pure result
    else
      let result ← f (memoFix2State f) a b
      modify (·.insert key result)
      pure result

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

-- Array-backed binary memoized fixed point for local Nat-indexed charts.
-- Parser spans are dense `(lo, hi)` keys, so an array avoids hash overhead and
-- still supports higher-universe values such as tagless-final parser results.
abbrev MemoCache (γ : Type u) :=
  Array (Option γ)

abbrev MemoM (γ : Type u) (δ : Type v) :=
  MemoCache γ -> δ × MemoCache γ

instance : Monad (MemoM γ) where
  pure x := fun cache => (x, cache)
  bind x f := fun cache =>
    let (result, cache) := x cache
    f result cache

partial def memoFix2State {γ : Type u} [Nonempty γ]
    (width : Nat)
    (f : (Nat -> Nat -> MemoM γ γ) -> Nat -> Nat -> MemoM γ γ) :
    Nat -> Nat -> MemoM γ γ :=
  fun a b cache =>
    let key := a * width + b
    match cache[key]? with
    | some (some result) => (result, cache)
    | _ =>
      let (result, cache) := f (memoFix2State width f) a b cache
      (result, cache.set! key (some result))

def MemoCache.empty (width : Nat) (height : Nat) : MemoCache γ :=
  Array.replicate (width * height) none

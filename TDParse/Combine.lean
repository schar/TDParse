import TDParse.Data.Ty
import TDParse.Data.Derivation
import TDParse.Display

-- Modes of combination
-- ------------------------------------------------------------------------

section
open ModeLabel
variable {α α' β β' γ : Type}

-- Basic modes
def Mode.fa : Mode (α → β) α β := ⟨FA, (·<|·)⟩
def Mode.ba : Mode α (α → β) β := ⟨BA, (·|>·)⟩
def Mode.pm : Mode (α → Bool) (α → Bool) (α → Bool) := ⟨PM, fun p q x => p x && q x⟩

-- Meta-modes
variable {f g : Type -> Type} (m : ModeLabel) -- all of these take a ModeLabel as 1st arg

def Mode.ml (op : α → β → γ) : [Functor f] -> Mode (f α) β (f γ) :=
  ⟨ML m, fun xs y  => xs <&> (fun a => op a y)⟩

def Mode.mr (op : α → β → γ) : [Functor f] -> Mode α (f β) (f γ) :=
  ⟨MR m, fun x ys  => (fun b => op x b) <$> ys⟩

def Mode.ap (op : α → β → γ) : [Applicative f] -> Mode (f α) (f β) (f γ) :=
  ⟨AP m, fun xs ys => op <$> xs <*> ys⟩

def Mode.ul (op : α → (β → β') → γ) :[Applicative f] -> Mode α (f β → β') γ :=
  ⟨UL m, fun x y => op x (fun b => y (pure b))⟩

def Mode.ur (op : (α → α') → β → γ) : [Applicative f] -> Mode (f α → α') β γ :=
  ⟨UL m, fun x y => op (fun a => x (pure a)) y⟩

def Mode.cu (op : α → β → γ) : [Functor f] -> [Functor g] -> [Adjoint f g] -> Mode (f α) (g β) γ :=
  ⟨CU m, fun xs ys => Adjoint.counit ((fun a => op a <$> ys) <$> xs)⟩

def Mode.jn (op : α → β → f (f γ)) : [Monad f] -> Mode α β (f γ) :=
  ⟨JN m, fun x y => op x y >>= id⟩

def Mode.dn (op : α → β → (Cont γ γ)) : Mode α β γ :=
  ⟨DN m, fun x y => op x y id⟩

end


-- Primitive, deterministic combination
-- ------------------------------------------------------------------------

def prims : (u : Ty) -> (v : Ty) -> List ((w : Ty) × Mode u.dom v.dom w.dom)
  | a ~> T, b ~> T => if h : a = b then by subst h; exact [⟨a ~> T, .pm⟩] else []
  | a ~> b, c      => if h : a = c then by subst h; exact [⟨b     , .fa⟩] else []
  | a     , b ~> c => if h : a = b then by subst h; exact [⟨c     , .ba⟩] else []
  | _     , _      => []

#eval List.map (fun ⟨w, m, _⟩ => (w, m)) (prims (E ~> T) E)


-- Recursive, nondeterministic combination
-- ------------------------------------------------------------------------

mutual

def combine u v := bins >>= uns where
  bins  := prims u v ++ addML u v ++ addMR u v ++ addAP u v ++ addCU u v
  uns e := pure e ++ addJN e ++ addDN e

def addML (u v : Ty) : List ((w : Ty) × Mode u.dom v.dom w.dom) := do
  let .comp f a := u | []
  let some _ := functor f | []
  combine a v <&> λ⟨w,m,op⟩ => ⟨.comp f w, .ml m op⟩

def addMR (u v : Ty) : List ((w : Ty) × Mode u.dom v.dom w.dom) := do
  let .comp f b := v | []
  let some _ := functor f | []
  combine u b <&> λ⟨w,m,op⟩ => ⟨.comp f w, .mr m op⟩

def addAP (u v : Ty) : List ((w : Ty) × Mode u.dom v.dom w.dom) := do
  let .comp f a := u | []
  let .comp g b := v | []
  let some _ := applicative f | []
  if h : f = g then
    by subst h; exact
    combine a b <&> λ⟨w,m,op⟩ => ⟨.comp f w, .ap m op⟩
  else []

def addUL (u v : Ty) : List ((w : Ty) × Mode u.dom v.dom w.dom) := do
  let .comp f b ~> b' := v | []
  let some _ := applicative f | []
  combine u (b ~> b') <&> λ⟨w,m,op⟩ => ⟨w, .ul m op⟩

def addUR (u v : Ty) : List ((w : Ty) × Mode u.dom v.dom w.dom) := do
  let .comp f a ~> a' := u | []
  let some _ := applicative f | []
  combine (a ~> a') v <&> λ⟨w,m,op⟩ => ⟨w, .ur m op⟩

def addCU (u v : Ty) : List ((w : Ty) × Mode u.dom v.dom w.dom) := do
  let .comp f a := u | []
  let .comp g b := v | []
  let some ⟨_,_,_⟩ := adjoint f g | []
  combine a b <&> λ⟨w,m,op⟩ => ⟨w, .cu m op⟩

def addJN {a b : Ty} (e : (c : Ty) × Mode a.dom b.dom c.dom) : List ((w : Ty) × Mode a.dom b.dom w.dom) := do
  let ⟨.comp f (.comp g c), m, op⟩ := e | []
  let some _ := monad f | []
  if h : f = g then by subst h; exact [⟨.comp f c, .jn m op⟩] else []

def addDN {a b : Ty} (e : (c : Ty) × Mode a.dom b.dom c.dom) : List ((w : Ty) × Mode a.dom b.dom w.dom) := do
  let ⟨C^r a, m, op⟩ := e | []
  if h : r = a then by subst h; exact [⟨r, .dn m op⟩] else []

end

#eval combine (S (E ~> T)) (S E) <&> fun ⟨w, m, _⟩ => (w, m)
#eval combine (E ~> S T) (S E) <&> fun ⟨w, m, _⟩ => (w, m)


-- Normalization
-- ------------------------------------------------------------------------

open ModeLabel
def norm {u v w : Ty} (md : Mode u.dom v.dom w.dom) : Bool :=
  match md.mode with
  -- unit equivalences
  | m:UR MR __ | m:UR DN MR __
  | m:UL ML __ | m:UL DN ML __

  -- lowering equivalences
  | m:DN MR DN MR __ | m:DN ML DN ML __ | m:DN ML DN MR __
  | m:DN AP DN MR __ | m:DN ML DN AP __
  | m:DN CU __

  -- monad equivalences
  | m:JN MR MR __ | m:JN MR JN MR __
  | m:JN ML ML __ | m:JN ML JN ML __
  | m:JN ML MR __ | m:JN ML JN MR __
  | m:JN AP MR __ | m:JN AP JN MR __
  | m:JN ML AP __ | m:JN ML JN AP __
  => false

  -- commutative monad equivalences
  | mlab => Id.run do
    let .comp f _ := w | true
    if not (FX.commutative f) then true else
    match mlab with
    | m:JN MR AP __ | m:JN AP ML __
    | m:JN AP AP __ | m:JN AP JN AP __
    | m:JN MR ML __ | m:JN MR JN ML __
    => false

    | _ => true

#eval combine (S (E ~> T)) (S E) >>= fun ⟨w, m, o⟩ => guard (norm ⟨m,o⟩) *> pure (w, m)
#eval combine (C^T (E ~> T)) (C^T E) >>= fun ⟨w, m, o⟩ => guard (norm ⟨m,o⟩) *> pure (w, m)

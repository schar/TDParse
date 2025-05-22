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
variable {f g : Type -> Type}

def Mode.ml [Functor f] (m : Mode α β γ) : Mode (f α) β (f γ) :=
  ⟨ML m.mode, fun xs y  => xs <&> (fun a => m.op a y)⟩

def Mode.mr [Functor f] (m : Mode α β γ) : Mode α (f β) (f γ) :=
  ⟨MR m.mode, fun x ys  => (fun b => m.op x b) <$> ys⟩

def Mode.ap [Applicative f] (m : Mode α β γ) : Mode (f α) (f β) (f γ) :=
  ⟨AP m.mode, fun xs ys => m.op <$> xs <*> ys⟩

def Mode.ul [Applicative f] (m : Mode α (β → β') γ) : Mode α (f β → β') γ :=
  ⟨UL m.mode, fun x y => m.op x (fun b => y (pure b))⟩

def Mode.ur [Applicative f] (m : Mode (α → α') β γ) : Mode (f α → α') β γ :=
  ⟨UL m.mode, fun x y => m.op (fun a => x (pure a)) y⟩

def Mode.cu [Functor f] [Functor g] [Adjoint f g] (m : Mode α β γ) : Mode (f α) (g β) γ :=
  ⟨CU m.mode, fun xs ys => Adjoint.counit ((fun a => m.op a <$> ys) <$> xs)⟩

def Mode.jn [Monad f] (m : Mode α β (f (f γ))) : Mode α β (f γ) :=
  ⟨JN m.mode, fun x y => m.op x y >>= id⟩

def Mode.dn (m : Mode α β (Cont γ γ)) : Mode α β γ :=
  ⟨DN m.mode, fun x y => m.op x y id⟩

end


-- Primitive, deterministic combination
-- ------------------------------------------------------------------------

def prims : (u : Ty) -> (v : Ty) -> List ((w : Ty) × Mode u.dom v.dom w.dom)
  | a ~> T, b ~> T => if h : a = b then by subst h; exact [⟨a ~> T, .pm⟩] else []
  | a ~> b, c      => if h : a = c then by subst h; exact [⟨b     , .fa⟩] else []
  | a     , b ~> c => if h : a = b then by subst h; exact [⟨c     , .ba⟩] else []
  | _     , _      => []

#eval List.map (fun ⟨w, m, _⟩ => (w, m)) (prims (E ~> T) E)

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


-- Recursive, nondeterministic combination
-- ------------------------------------------------------------------------

mutual

def combine u v := bins >>= uns where
  bins  := prims u v ++ addML u v ++ addMR u v ++ addAP u v ++ addCU u v
  uns e := pure e ++ addJN e ++ addDN e

def addML (u v : Ty) : List ((w : Ty) × Mode u.dom v.dom w.dom) := do
  let .comp f a := u | []
  let some _ := functor f | []
  combine a v <&> λ⟨w,m⟩ => ⟨.comp f w, .ml m⟩

def addMR (u v : Ty) : List ((w : Ty) × Mode u.dom v.dom w.dom) := do
  let .comp f b := v | []
  let some _ := functor f | []
  combine u b <&> λ⟨w,m⟩ => ⟨.comp f w, .mr m⟩

def addAP (u v : Ty) : List ((w : Ty) × Mode u.dom v.dom w.dom) := do
  let .comp f a := u | []
  let .comp g b := v | []
  let some _ := applicative f | []
  if h : f = g then
    by subst h; exact
    combine a b <&> λ⟨w,m⟩ => ⟨.comp f w, .ap m⟩
  else []

def addUL (u v : Ty) : List ((w : Ty) × Mode u.dom v.dom w.dom) := do
  let .comp f b ~> b' := v | []
  let some _ := applicative f | []
  let ⟨w,m⟩ <- combine u (b ~> b')
  let m' := .ul m; guard (norm m') *> pure ⟨w, m'⟩

def addUR (u v : Ty) : List ((w : Ty) × Mode u.dom v.dom w.dom) := do
  let .comp f a ~> a' := u | []
  let some _ := applicative f | []
  let ⟨w,m⟩ <- combine (a ~> a') v
  let m' := .ur m; guard (norm m') *> pure ⟨w, m'⟩

def addCU (u v : Ty) : List ((w : Ty) × Mode u.dom v.dom w.dom) := do
  let .comp f a := u | []
  let .comp g b := v | []
  let some ⟨_,_,_⟩ := adjoint f g | []
  combine a b <&> λ⟨w,m⟩ => ⟨w, .cu m⟩

def addJN {a b : Ty} (e : (c : Ty) × Mode a.dom b.dom c.dom) : List ((w : Ty) × Mode a.dom b.dom w.dom) := do
  let ⟨.comp f (.comp g c), m⟩ := e | []
  let some _ := monad f | []
  if h : f = g then
    by subst h; exact
    let m' := .jn m; guard (norm m') *> pure ⟨.comp f c, m'⟩
  else []

def addDN {a b : Ty} (e : (c : Ty) × Mode a.dom b.dom c.dom) : List ((w : Ty) × Mode a.dom b.dom w.dom) := do
  let ⟨C^r a, m⟩ := e | []
  if h : r = a then
    by subst h; exact
    let m' := .dn m; guard (norm m')  *> pure ⟨r, .dn m⟩
  else []

end

#eval combine (S (E ~> T)) (S E) <&> fun ⟨w, m, _⟩ => (w, m)
#eval combine (E ~> S T) (S E) <&> fun ⟨w, m, _⟩ => (w, m)

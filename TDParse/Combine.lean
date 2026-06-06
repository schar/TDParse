import TDParse.Data.Ty
import TDParse.Data.Derivation
import TDParse.Display -- just used for #eval tests
import TDParse.Memoize

-- Modes of combination
-- ------------------------------------------------------------------------

section
open ModeLabel
variable {α α' β β' γ : Type}

-- Basic modes
def Mode.fa : Mode (α → β) α β := ⟨FA, (·<|·)⟩
def Mode.ba : Mode α (α → β) β := ⟨BA, (·|>·)⟩
def Mode.pm : Mode (α → Bool) (α → Bool) (α → Bool) := ⟨PM, fun p q x => p x && q x⟩
def Mode.fc : Mode (β → γ) (α → β) (α → γ) := ⟨FC, (· ∘ ·)⟩

-- Meta-modes
variable {f g : Type -> Type}

def Mode.ml (fx : FX) [Functor f] (m : Mode α β γ) : Mode (f α) β (f γ) :=
  ⟨ML fx m.mode, fun xs y  => xs <&> (fun a => m.op a y)⟩

def Mode.mr (fx : FX) [Functor f] (m : Mode α β γ) : Mode α (f β) (f γ) :=
  ⟨MR fx m.mode, fun x ys  => (fun b => m.op x b) <$> ys⟩

def Mode.ap (fx : FX) [Applicative f] (m : Mode α β γ) : Mode (f α) (f β) (f γ) :=
  ⟨AP fx m.mode, fun xs ys => m.op <$> xs <*> ys⟩

def Mode.ul (fx : FX) [Applicative f] (m : Mode α (β → β') γ) : Mode α (f β → β') γ :=
  ⟨UL fx m.mode, fun x y => m.op x (fun b => y (pure b))⟩

def Mode.ur (fx : FX) [Applicative f] (m : Mode (α → α') β γ) : Mode (f α → α') β γ :=
  ⟨UR fx m.mode, fun x y => m.op (fun a => x (pure a)) y⟩

def Mode.cu (lf rf : FX) [Functor f] [Functor g] [Adjoint f g] (m : Mode α β γ) : Mode (f α) (g β) γ :=
  ⟨CU lf rf m.mode, fun xs ys => Adjoint.counit ((fun a => m.op a <$> ys) <$> xs)⟩

def Mode.jn (fx : FX) [Monad f] (m : Mode α β (f (f γ))) : Mode α β (f γ) :=
  ⟨JN fx m.mode, fun x y => m.op x y >>= id⟩

def Mode.dn (m : Mode α β (Cont2 γ δ δ)) : Mode α β γ :=
  ⟨DN m.mode, fun x y => m.op x y id⟩

end


-- Compatible effect sequencing
-- ------------------------------------------------------------------------

structure AppCompat (f g : FX) where
  out : FX
  lift : (a b c : Ty) ->
    Mode a.dom b.dom c.dom ->
    Mode (f.dom a.dom) (g.dom b.dom) (out.dom c.dom)

structure JoinCompat (f g : FX) where
  out : FX
  lift : (a b c : Ty) ->
    Mode a.dom b.dom (f.dom (g.dom c.dom)) ->
    Mode a.dom b.dom (out.dom c.dom)

private def appCompatSame (f g : FX) : List (AppCompat f g) :=
  if h : f = g then
    by
      subst h
      match applicative f with
      | some _ => exact [{ out := f, lift := fun _ _ _ m => .ap f m }]
      | none   => exact []
  else []

def appCompat : (f g : FX) -> List (AppCompat f g)
  | .scope ret midL, .scope midR ans =>
      if h : midL = midR then
        by
          subst h
          exact
            [{ out := .scope ret ans
             , lift := fun _ _ _ m =>
                 ⟨.AP (.scope ret ans) m.mode,
                   fun xs ys k => xs fun a => ys fun b => k (m.op a b)⟩ }]
      else []
  | f, g => appCompatSame f g

private def joinCompatSame (f g : FX) : List (JoinCompat f g) :=
  if h : f = g then
    by
      subst h
      match monad f with
      | some _ => exact [{ out := f, lift := fun _ _ _ m => .jn f m }]
      | none   => exact []
  else []

def joinCompat : (f g : FX) -> List (JoinCompat f g)
  | .scope ret midL, .scope midR ans =>
      if h : midL = midR then
        by
          subst h
          exact
            [{ out := .scope ret ans
             , lift := fun _ _ _ m =>
                 ⟨.JN (.scope ret ans) m.mode,
                   fun x y k => m.op x y fun z => z k⟩ }]
      else []
  | f, g => joinCompatSame f g


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

private def invertOk : ModeLabel -> FX -> Bool
  | .ML g _, f => g != f || FX.invertible f
  | _      , _ => true

private def normLab : ModeLabel -> Bool
  | .UR f (.MR g _) => f != g
  | .UR f (.DN (.MR g _)) => f != g
  | .UL f (.ML g _) => f != g
  | .UL f (.DN (.ML g _)) => f != g

  | .DN (.MR _ (.DN (.MR _ _))) => false
  | .DN (.ML _ (.DN (.ML _ _))) => false
  | .DN (.ML _ (.DN (.MR _ _))) => false
  | .DN (.AP _ (.DN (.MR _ _))) => false
  | .DN (.ML _ (.DN (.AP _ _))) => false
  | .DN (.CU _ _ _) => false

  | .EL f (.ML g .FA) => f != g
  | .EL _ (.MR _ _) => false
  | .EL f (.AP g _) => f != g
  | .ER f (.MR g .BA) => f != g
  | .ER _ (.ML _ _) => false
  | .ER f (.AP g _) => f != g

  | .JN _ (.CU _ _ _) => false
  | .JN f (.AP g (.CU _ _ _)) => f != g
  | .JN f (.EL g (.ML h (.MR i _))) => not (f == g && g == h && h == i)

  | .JN f (.MR g (.MR h _)) => not (f == g && g == h)
  | .JN f (.MR g (.JN h (.MR i _))) => not (f == g && g == h && h == i)
  | .JN f (.ML g (.ML h _)) => not (f == g && g == h)
  | .JN f (.ML g (.JN h (.ML i _))) => not (f == g && g == h && h == i)
  | .JN f (.ML g (.MR h _)) => not (f == g && g == h)
  | .JN f (.ML g (.JN h (.MR i _))) => not (f == g && g == h && h == i)
  | .JN f (.AP g (.MR h _)) => not (f == g && g == h)
  | .JN f (.AP g (.JN h (.MR i _))) => not (f == g && g == h && h == i)
  | .JN f (.ML g (.AP h _)) => not (f == g && g == h)
  | .JN f (.ML g (.JN h (.AP i _))) => not (f == g && g == h && h == i)

  | .JN f lab =>
      if not (FX.commutative f) then true else
      match lab with
      | .MR g (.AP h _) => not (f == g && g == h)
      | .AP g (.ML h _) => not (f == g && g == h)
      | .AP g (.AP h _) => not (f == g && g == h)
      | .AP g (.JN h (.AP i _)) => not (f == g && g == h && h == i)
      | .MR g (.ML h _) => not (f == g && g == h)
      | .MR g (.JN h (.ML i _)) => not (f == g && g == h && h == i)
      | .ER g (.MR h (.ML i _)) => not (f == g && g == h && h == i)
      | .ER g (.MR h (.JN i (.ML j _))) => not (f == g && g == h && h == i && i == j)
      | _ => true

  | _ => true

def norm {u v w : Ty} (md : Mode u.dom v.dom w.dom) : Bool :=
  normLab md.mode



-- Recursive, nondeterministic combination
-- ------------------------------------------------------------------------

abbrev Combo (u : Ty) (v : Ty) := ((w : Ty) × Mode u.dom v.dom w.dom)

def combine : (u v : Ty) -> List (Combo u v) := memoFix2 go

  where go combine u v :=

    let addML : List (Combo u v) := do
          let .comp f a := u | []
          let some _ := functor f | []
          combine a v <&> λ⟨w,m⟩ => ⟨.comp f w, .ml f m⟩

    let addMR : List (Combo u v) := do
          let .comp f b := v | []
          let some _ := functor f | []
          let ⟨w, m⟩ <- combine u b
          let m' := .mr f m
          guard (invertOk m.mode f)
          pure ⟨.comp f w, m'⟩

    let addAP : List (Combo u v) := do
          let .comp f a := u | []
          let .comp g b := v | []
          let seq <- appCompat f g
          combine a b <&> λ⟨w,m⟩ => ⟨.comp seq.out w, seq.lift a b w m⟩

    let addUL : List (Combo u v) := do
          let .comp f b ~> b' := v | []
          let some _ := applicative f | []
          let ⟨w,m⟩ <- combine u (b ~> b')
          let m' := .ul f m; guard (norm m') *> pure ⟨w, m'⟩

    let addUR : List (Combo u v) := do
          let .comp f a ~> a' := u | []
          let some _ := applicative f | []
          let ⟨w,m⟩ <- combine (a ~> a') v
          let m' := .ur f m; guard (norm m') *> pure ⟨w, m'⟩

    let addCU : List (Combo u v) :=
        match u, v with
        | .comp f a, .comp g b =>
          match adjoint f g, comonad f with
          | some ⟨_,_,_⟩, some inst => do
              let ⟨w, m⟩ <- combine a b
              let mcu : Mode (Ty.dom (.comp f a)) (Ty.dom (.comp g b)) w.dom := .cu f g m
              let base : Combo (.comp f a) (.comp g b) := ⟨w, mcu⟩
              let mxl : Mode (Ty.dom (.comp f a)) (Ty.dom (.comp g b)) (Ty.dom (.comp f w)) :=
                ⟨XL f mcu.mode,
                  fun (xs : Ty.dom (.comp f a)) (ys : Ty.dom (.comp g b)) =>
                    inst.extend (fun xs' => mcu.op xs' ys) xs⟩
              let ext : Combo (.comp f a) (.comp g b) := ⟨.comp f w, mxl⟩
              [base, ext]
          | _, _ => []
        | _, _ => []

    let addEL : List (Combo u v) :=
        match u with
        | .fn a (.comp (.query i) b) => do
            let ⟨w, m⟩ <- combine (.comp (.query i) (.fn a b)) v
            let m' : Mode (Ty.dom (.fn a (.comp (.query i) b))) v.dom w.dom :=
              ⟨EL (.query i) m.mode, fun x y => m.op (fun env a' => x a' env) y⟩
            guard (norm m') *> pure ⟨w, m'⟩
        | _ => []

    let addER : List (Combo u v) :=
        match v with
        | .fn a (.comp (.query i) b) => do
            let ⟨w, m⟩ <- combine u (.comp (.query i) (.fn a b))
            let m' : Mode u.dom (Ty.dom (.fn a (.comp (.query i) b))) w.dom :=
              ⟨ER (.query i) m.mode, fun x y => m.op x (fun env a' => y a' env)⟩
            guard (norm m') *> pure ⟨w, m'⟩
        | _ => []

    let addJN (e : Combo u v) : List (Combo u v) := do
          let ⟨.comp f (.comp g c), m⟩ := e | []
          let seq <- joinCompat f g
          let m' : Mode u.dom v.dom (Ty.dom (.comp seq.out c)) := seq.lift u v c m
          guard (norm m')
          pure ⟨.comp seq.out c, m'⟩

    let addDN (e : Combo u v) : List (Combo u v) := do
          let ⟨.comp (.scope ret ans) payload, m⟩ := e | []
          if h : payload = ans then
            by subst h; exact
            let m' := .dn m; guard (norm m') *> pure ⟨ret, m'⟩
          else []

    let bins := prims u v ++ addML ++ addMR ++ addUL ++ addUR ++ addAP ++ addCU ++ addEL ++ addER
    let uns e := addDN e ++ addJN e ++ pure e

    bins >>= uns

#eval combine (S (E ~> T)) (S E) <&> fun ⟨w, m, _⟩ => (w, m)
#eval combine (E ~> S T) (S E) <&> fun ⟨w, m, _⟩ => (w, m)

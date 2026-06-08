import TDParse.Data.Ty
import TDParse.Data.Derivation
import TDParse.Display -- just used for #eval tests
import TDParse.Memoize

-- Modes of combination
-- ------------------------------------------------------------------------

section
open ModeLabel
variable {a a' b b' c : Ty}

-- Basic modes
def Mode.fa : Mode (a ~> b) a b :=
  ⟨FA, (·<|·), .fa⟩

def Mode.ba : Mode a (a ~> b) b :=
  ⟨BA, (·|>·), .ba⟩

def Mode.pm : Mode (a ~> T) (a ~> T) (a ~> T) :=
  ⟨PM,
    fun p q x => p x && q x,
    .pm⟩

def Mode.fc : Mode (b ~> c) (a ~> b) (a ~> c) :=
  ⟨FC, (· ∘ ·), .fc⟩

-- Meta-modes
def Mode.ml (f : FX) (inst : Functor f.dom) (ok : (functor f).isSome = true)
    (m : Mode a b c) : Mode (.comp f a) b (.comp f c) :=
  ⟨ML f m.mode,
    (letI : Functor f.dom := inst
     fun xs y => xs <&> (fun x => m.op x y)),
    .ml f ok m.recipe⟩

def Mode.mr (f : FX) (inst : Functor f.dom) (ok : (functor f).isSome = true)
    (m : Mode a b c) : Mode a (.comp f b) (.comp f c) :=
  ⟨MR f m.mode,
    (letI : Functor f.dom := inst
     fun x ys => (fun y => m.op x y) <$> ys),
    .mr f ok m.recipe⟩

def Mode.ap (f : FX) (inst : Applicative f.dom) (ok : (applicative f).isSome = true)
    (m : Mode a b c) : Mode (.comp f a) (.comp f b) (.comp f c) :=
  ⟨AP f m.mode,
    (letI : Applicative f.dom := inst
     fun xs ys => m.op <$> xs <*> ys),
    .ap f ok m.recipe⟩

def Mode.ul (f : FX) (inst : Applicative f.dom) (ok : (applicative f).isSome = true)
    (m : Mode a (b ~> b') c) :
    Mode a (.comp f b ~> b') c :=
  ⟨UL f m.mode,
    (letI : Applicative f.dom := inst
     fun x y => m.op x (fun b => y (pure b))),
    .ul f ok m.recipe⟩

def Mode.ur (f : FX) (inst : Applicative f.dom) (ok : (applicative f).isSome = true)
    (m : Mode (a ~> a') b c) :
    Mode (.comp f a ~> a') b c :=
  ⟨UR f m.mode,
    (letI : Applicative f.dom := inst
     fun x y => m.op (fun a => x (pure a)) y),
    .ur f ok m.recipe⟩

def Mode.cu (lf rf : FX) (instF : Functor lf.dom) (instG : Functor rf.dom)
    (adj : @Adjoint lf.dom rf.dom instF instG) (ok : (adjoint lf rf).isSome = true)
    (m : Mode a b c) :
    Mode (.comp lf a) (.comp rf b) c :=
  ⟨CU lf rf m.mode,
    (letI : Functor lf.dom := instF
     letI : Functor rf.dom := instG
     letI : Adjoint lf.dom rf.dom := adj
     fun xs ys => Adjoint.counit ((fun a => m.op a <$> ys) <$> xs)),
    .cu lf rf ok m.recipe⟩

def Mode.jn (f : FX) (inst : Monad f.dom) (ok : (monad f).isSome = true)
    (m : Mode a b (.comp f (.comp f c))) : Mode a b (.comp f c) :=
  ⟨JN f m.mode,
    (letI : Monad f.dom := inst
     fun x y => m.op x y >>= id),
    .jn f ok m.recipe⟩

def Mode.dn (m : Mode a b (.comp (.scope c b') b')) : Mode a b c :=
  ⟨DN m.mode,
    fun x y => m.op x y id,
    .dn m.recipe⟩

end


-- Compatible effect sequencing
-- ------------------------------------------------------------------------

structure AppCompat (f g : FX) where
  out : FX
  lift : (a b c : Ty) ->
    Mode a b c ->
    Mode (.comp f a) (.comp g b) (.comp out c)

structure JoinCompat (f g : FX) where
  out : FX
  lift : (a b c : Ty) ->
    Mode a b (.comp f (.comp g c)) ->
    Mode a b (.comp out c)

private def appCompatSame (f g : FX) : List (AppCompat f g) :=
  if h : f = g then
    by
      subst h
      match hApp : applicative f with
      | some inst => exact [{ out := f, lift := fun _ _ _ m => .ap f inst (by simp [hApp]) m }]
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
                   fun xs ys k => xs fun a => ys fun b => k (m.op a b),
                   .scopeAp m.recipe⟩ }]
      else []
  | f, g => appCompatSame f g

private def joinCompatSame (f g : FX) : List (JoinCompat f g) :=
  if h : f = g then
    by
      subst h
      match hMon : monad f with
      | some inst => exact [{ out := f, lift := fun _ _ _ m => .jn f inst (by simp [hMon]) m }]
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
                   fun x y k => m.op x y fun z => z k,
                   .scopeJn m.recipe⟩ }]
      else []
  | f, g => joinCompatSame f g


-- Primitive, deterministic combination
-- ------------------------------------------------------------------------

def prims : (u : Ty) -> (v : Ty) -> List ((w : Ty) × Mode u v w)
  | a ~> T, b ~> T => if h : a = b then by subst h; exact [⟨a ~> T, .pm⟩] else []
  | a ~> b, c      => if h : a = c then by subst h; exact [⟨b     , .fa⟩] else []
  | a     , b ~> c => if h : a = b then by subst h; exact [⟨c     , .ba⟩] else []
  | _     , _      => []

#eval List.map (fun ⟨w, m⟩ => (w, m.mode)) (prims (E ~> T) E)


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

def norm {u v w : Ty} (md : Mode u v w) : Bool :=
  normLab md.mode



-- Recursive, nondeterministic combination
-- ------------------------------------------------------------------------

abbrev Combo (u : Ty) (v : Ty) := ((w : Ty) × Mode u v w)

def combine : (u v : Ty) -> List (Combo u v) := memoFix2 go

  where go combine u v :=

    let addML : List (Combo u v) := do
          let .comp f a := u | []
          let some inst := functor f | []
          have ok : (functor f).isSome = true := by cases f <;> simp [functor]
          combine a v <&> λ⟨w,m⟩ => ⟨.comp f w, .ml f inst ok m⟩

    let addMR : List (Combo u v) := do
          let .comp f b := v | []
          let some inst := functor f | []
          have ok : (functor f).isSome = true := by cases f <;> simp [functor]
          let ⟨w, m⟩ <- combine u b
          let m' := .mr f inst ok m
          guard (invertOk m.mode f)
          pure ⟨.comp f w, m'⟩

    let addAP : List (Combo u v) := do
          let .comp f a := u | []
          let .comp g b := v | []
          let seq <- appCompat f g
          combine a b <&> λ⟨w,m⟩ => ⟨.comp seq.out w, seq.lift a b w m⟩

    let addUL : List (Combo u v) :=
        match v with
        | .fn (.comp f b) b' =>
          match hApp : applicative f with
          | some inst => do
              let ⟨w,m⟩ <- combine u (b ~> b')
              let m' := .ul f inst (by simp [hApp]) m
              guard (norm m') *> pure ⟨w, m'⟩
          | none => []
        | _ => []

    let addUR : List (Combo u v) :=
        match u with
        | .fn (.comp f a) a' =>
          match hApp : applicative f with
          | some inst => do
              let ⟨w,m⟩ <- combine (a ~> a') v
              let m' := .ur f inst (by simp [hApp]) m
              guard (norm m') *> pure ⟨w, m'⟩
          | none => []
        | _ => []

    let addCU : List (Combo u v) :=
        match u, v with
        | .comp f a, .comp g b =>
          match hAdj : adjoint f g, hCom : comonad f with
          | some ⟨instF, instG, adj⟩, some inst => do
              let ⟨w, m⟩ <- combine a b
              let adjOk : (adjoint f g).isSome = true := by simp [hAdj]
              let comOk : (comonad f).isSome = true := by simp [hCom]
              let mcu : Mode (.comp f a) (.comp g b) w := .cu f g instF instG adj adjOk m
              let base : Combo (.comp f a) (.comp g b) := ⟨w, mcu⟩
              let mxl : Mode (.comp f a) (.comp g b) (.comp f w) :=
                ⟨XL f mcu.mode,
                  fun (xs : Ty.dom (.comp f a)) (ys : Ty.dom (.comp g b)) =>
                    inst.extend (fun xs' => mcu.op xs' ys) xs,
                  .xl f comOk mcu.recipe⟩
              let ext : Combo (.comp f a) (.comp g b) := ⟨.comp f w, mxl⟩
              [base, ext]
          | _, _ => []
        | _, _ => []

    let addEL : List (Combo u v) :=
        match u with
        | .fn a (.comp (.query i) b) => do
            let ⟨w, m⟩ <- combine (.comp (.query i) (.fn a b)) v
            let m' : Mode (.fn a (.comp (.query i) b)) v w :=
              ⟨EL (.query i) m.mode,
                fun x y => m.op (fun env a' => x a' env) y,
                .el i m.recipe⟩
            guard (norm m') *> pure ⟨w, m'⟩
        | _ => []

    let addER : List (Combo u v) :=
        match v with
        | .fn a (.comp (.query i) b) => do
            let ⟨w, m⟩ <- combine u (.comp (.query i) (.fn a b))
            let m' : Mode u (.fn a (.comp (.query i) b)) w :=
              ⟨ER (.query i) m.mode,
                fun x y => m.op x (fun env a' => y a' env),
                .er i m.recipe⟩
            guard (norm m') *> pure ⟨w, m'⟩
        | _ => []

    let addJN (e : Combo u v) : List (Combo u v) := do
          let ⟨.comp f (.comp g c), m⟩ := e | []
          let seq <- joinCompat f g
          let m' : Mode u v (.comp seq.out c) := seq.lift u v c m
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

#eval combine (S (E ~> T)) (S E) <&> fun ⟨w, m⟩ => (w, m.mode)
#eval combine (E ~> S T) (S E) <&> fun ⟨w, m⟩ => (w, m.mode)

import TDParse.NBE

/-!
Tagless-final semantic interpretations.

Lexical entries and mode operations are interpreted through the `Semantics`
class.  The same object term can therefore be evaluated in a concrete model or
rendered as a normalized, printable higher-order expression.
-/

namespace TDParse

open FX Ty

class Semantics (repr : Ty -> Type) where
  prim : {t : Ty} -> String -> repr t

  app : {a b : Ty} -> repr (a ~> b) -> repr a -> repr b
  lam : {a b : Ty} -> (repr a -> repr b) -> repr (a ~> b)

  bool  : Bool -> repr T
  nat   : Nat -> repr E
  conj  : repr T -> repr T -> repr T
  imp   : repr T -> repr T -> repr T
  neg   : repr T -> repr T
  eqNat : repr E -> repr E -> repr T
  forallNat : List Nat -> (repr E -> repr T) -> repr T
  existsNat : List Nat -> (repr E -> repr T) -> repr T
  selectNat : List Nat -> (repr E -> repr T) -> repr E
  forallIn : repr (E ~> T) -> (repr E -> repr T) -> repr T
  existsIn : repr (E ~> T) -> (repr E -> repr T) -> repr T
  selectWhere : (repr E -> repr T) -> repr E
  chooseIn : repr (E ~> T) -> repr (S E)
  chooseStoreIn : repr (E ~> T) -> repr (S (W^E E))
  forallStoreIn : repr (E ~> T) -> (repr (W^E E) -> repr T) -> repr T
  listNat : List Nat -> repr (S E)
  listStoreNat : List Nat -> repr (S (W^E E))
  forallStoreNat : List Nat -> (repr (W^E E) -> repr T) -> repr T
  filterNat : List Nat -> (repr E -> repr T) -> repr (S E)

  ask : {a : Ty} -> repr (.comp (.query a) a)
  storePair : {out a : Ty} -> repr out -> repr a -> repr (.comp (.store out) a)
  cont : {r a : Ty} -> ((repr a -> repr r) -> repr r) -> repr (.comp (.scope r r) a)
  cont2 : {r s a : Ty} -> ((repr a -> repr s) -> repr r) -> repr (.comp (.scope r s) a)

  mapEff : {a b : Ty} -> (f : FX) -> (inst : Functor f.dom) ->
    repr (a ~> b) -> repr (.comp f a) -> repr (.comp f b)
  pureEff : {a : Ty} -> (f : FX) -> (inst : Applicative f.dom) ->
    repr a -> repr (.comp f a)
  apEff : {a b : Ty} -> (f : FX) -> (inst : Applicative f.dom) ->
    repr (.comp f (a ~> b)) -> repr (.comp f a) -> repr (.comp f b)
  joinEff : {a : Ty} -> (f : FX) -> (inst : Monad f.dom) ->
    repr (.comp f (.comp f a)) -> repr (.comp f a)
  lower : {r a : Ty} ->
    repr (.comp (.scope r a) a) -> repr r
  scopeMap2 : {r s q a b c : Ty} ->
    repr (a ~> b ~> c) ->
    repr (.comp (.scope r s) a) ->
    repr (.comp (.scope s q) b) ->
    repr (.comp (.scope r q) c)
  joinScope : {r s q a : Ty} ->
    repr (.comp (.scope r s) (.comp (.scope s q) a)) ->
    repr (.comp (.scope r q) a)
  counit : {a : Ty} -> (f g : FX) -> (instF : Functor f.dom) -> (instG : Functor g.dom) ->
    @Adjoint f.dom g.dom instF instG ->
    repr (.comp f (.comp g a)) -> repr a
  extend : {a b : Ty} -> (f : FX) -> (inst : Comonad f.dom) ->
    repr (.comp f a) -> (repr (.comp f a) -> repr b) -> repr (.comp f b)
  eject : {a b i : Ty} ->
    repr (a ~> .comp (.query i) b) -> repr (.comp (.query i) (a ~> b))

abbrev SemTerm (t : Ty) := {repr : Ty -> Type} -> [Semantics repr] -> repr t

def SemTerm.app {a b : Ty} (f : SemTerm (a ~> b)) (x : SemTerm a) : SemTerm b :=
  fun {repr} [Semantics repr] => Semantics.app (f (repr := repr)) (x (repr := repr))

def SemTerm.lam {a b : Ty}
    (body : {repr : Ty -> Type} -> [Semantics repr] -> repr a -> repr b) :
    SemTerm (a ~> b) :=
  fun {repr} [Semantics repr] =>
    Semantics.lam (body (repr := repr))

def Ty.default : (t : Ty) -> t.dom
  | .nat => Entity.default
  | .bool => false
  | .fn _ b => fun _ => Ty.default b
  | .comp .spawn _ => []
  | .comp (.query _) a => fun _ => Ty.default a
  | .comp (.store o) a => (Ty.default o, Ty.default a)
  | .comp (.scope r _) _ => fun _ => Ty.default r

def selectFirstNat : List Nat -> (Nat -> Bool) -> Nat
  | [], _ => 0
  | x :: xs, p => if p x then x else selectFirstNat xs p

def selectFirstEntity : List Entity -> (Entity -> Bool) -> Entity
  | [], _ => Entity.default
  | x :: xs, p => if p x then x else selectFirstEntity xs p

structure Model where
  domain : List Entity
  entity : String -> Entity
  truth : String -> Bool
  fun1 : String -> Entity -> Entity
  pred1 : String -> Entity -> Bool
  rel2 : String -> Entity -> Entity -> Bool
  relTE : String -> Bool -> Entity -> Bool
  rel3 : String -> Entity -> Entity -> Entity -> Bool
  det : String -> (Entity -> Bool) -> Entity
  unknown : (t : Ty) -> String -> t.dom

def defaultEntity : String -> Entity
  | "ann" => .name "ann"
  | "mary" => .name "mary"
  | "marianne" => .name "marianne"
  | "maryaling" => .name "maryaling"
  | "sassyacat" => .name "sassyacat"
  | name => .name name

def defaultFun1 : String -> Entity -> Entity
  | "successor", x => x.mapNat (· + 1)
  | "predecessor", x => x.mapNat (· - 1)
  | "mom", x => .rel "mom" x
  | "paycheck", x => .rel "paycheck" x
  | _, x => x

def numberPred1 : String -> Entity -> Bool
  | "number", .num _ => true
  | "number", _ => false
  | "prime", .num x => [2,3,5].contains x
  | "prime", _ => false
  | _, _ => false

def englishPred1 : String -> Entity -> Bool
  | "person", .name s => ["ann", "mary", "marianne", "maryaling", "sassyacat", "alex"].contains s
  | "person", _ => false
  | "number", .num _ => true
  | "number", _ => false
  | "prime", .num x => [2,3,5].contains x
  | "prime", _ => false
  | "left", .name s => ["cat1", "alex", "marianne"].contains s
  | "left", _ => false
  | "whistled", .name s => ["alex", "mary"].contains s
  | "whistled", _ => false
  | "dog", .name s => s == "dog1"
  | "dog", _ => false
  | "cat", .name s => s == "cat1"
  | "cat", _ => false
  | "big", .name s => s == "cat1"
  | "big", _ => false
  | "happy", .name s => s == "alex"
  | "happy", _ => false
  | _, _ => false

def numberRel2 : String -> Entity -> Entity -> Bool
  | "exceeds", .num x, .num y => x < y
  | "exceeds", _, _ => false
  | "succeeds", .num x, .num y => y = x + 1
  | "succeeds", _, _ => false
  | _, _, _ => false

def englishRel2 : String -> Entity -> Entity -> Bool
  | "saw", x, y => x != y
  | "saved", _, _ => true
  | "spent", _, _ => true
  | "near", .name "alex", .name "cat1" => true
  | "near", .name "cat1", .name "alex" => true
  | "near", x, y => x == y
  | _, _, _ => false

def defaultRelTE : String -> Bool -> Entity -> Bool
  | "said", p, _ => p
  | _, p, _ => p

def numberRel3 : String -> Entity -> Entity -> Entity -> Bool
  | "gave", .num x, .num y, .num z => x + y == z
  | _, _, _, _ => false

def englishRel3 : String -> Entity -> Entity -> Entity -> Bool
  | "gave", _, _, _ => true
  | _, _, _, _ => false

def numberDomain : List Entity :=
  (List.range 25).map Entity.num

def englishDomain : List Entity :=
  [.name "ann", .name "mary", .name "marianne", .name "maryaling",
   .name "sassyacat", .name "alex", .name "dog1", .name "cat1"]

def defaultDet (m : Model) : String -> (Entity -> Bool) -> Entity
  | _, p => selectFirstEntity m.domain p

def numberModelCore : Model where
  domain := numberDomain
  entity := defaultEntity
  truth := fun _ => false
  fun1 := defaultFun1
  pred1 := numberPred1
  rel2 := numberRel2
  relTE := defaultRelTE
  rel3 := numberRel3
  det := fun _ _ => Entity.default
  unknown := fun t _ => Ty.default t

def englishModelCore : Model where
  domain := englishDomain
  entity := defaultEntity
  truth := fun _ => false
  fun1 := defaultFun1
  pred1 := englishPred1
  rel2 := englishRel2
  relTE := defaultRelTE
  rel3 := englishRel3
  det := fun _ _ => Entity.default
  unknown := fun t _ => Ty.default t

def numberModel : Model :=
  { numberModelCore with det := defaultDet numberModelCore }

def englishModel : Model :=
  { englishModelCore with det := defaultDet englishModelCore }

def defaultModel : Model :=
  numberModel

def Model.prim (m : Model) : (t : Ty) -> String -> t.dom
  | .nat, name => m.entity name
  | .bool, name => m.truth name
  | .fn .nat .nat, name => m.fun1 name
  | .fn .nat .bool, name => m.pred1 name
  | .fn .nat (.fn .nat .bool), name => m.rel2 name
  | .fn .bool (.fn .nat .bool), name => m.relTE name
  | .fn .nat (.fn .nat (.fn .nat .bool)), name => m.rel3 name
  | .fn (.fn .nat .bool) .nat, name => m.det name
  | .fn (.fn .nat .bool) (.fn .nat .bool), "very" => fun p x => p x
  | .fn .nat (.fn (.fn .nat .bool) (.fn .nat .bool)), "with" =>
      fun y p x => p x && m.rel2 "near" x y
  | .fn (.comp .spawn .bool) .bool, "eclo" => fun xs => xs.any id
  | t, name => m.unknown t name

/- Denotational interpretation in a chosen model. -/

structure Eval (m : Model) (t : Ty) where
  val : t.dom

instance (m : Model) : Semantics (Eval m) where
  prim {t} name := ⟨m.prim t name⟩

  app f x := ⟨f.val x.val⟩
  lam f := ⟨fun x => (f ⟨x⟩).val⟩

  bool b := ⟨b⟩
  nat n := ⟨.num n⟩
  conj p q := ⟨p.val && q.val⟩
  imp p q := ⟨!p.val || q.val⟩
  neg p := ⟨!p.val⟩
  eqNat x y := ⟨x.val == y.val⟩
  forallNat xs p := ⟨xs.all fun x => (p ⟨.num x⟩).val⟩
  existsNat xs p := ⟨xs.any fun x => (p ⟨.num x⟩).val⟩
  selectNat xs p := ⟨.num (selectFirstNat xs fun x => (p ⟨.num x⟩).val)⟩
  forallIn restrict scope :=
    ⟨m.domain.all fun x => !restrict.val x || (scope ⟨x⟩).val⟩
  existsIn restrict scope :=
    ⟨m.domain.any fun x => restrict.val x && (scope ⟨x⟩).val⟩
  selectWhere p :=
    ⟨selectFirstEntity m.domain fun x => (p ⟨x⟩).val⟩
  chooseIn restrict :=
    ⟨m.domain.filter restrict.val⟩
  chooseStoreIn restrict :=
    ⟨m.domain.filter restrict.val |>.map fun x => (x, x)⟩
  forallStoreIn restrict scope :=
    ⟨m.domain.all fun x => !restrict.val x || (scope ⟨(x, x)⟩).val⟩
  listNat xs := ⟨xs.map Entity.num⟩
  listStoreNat xs := ⟨xs.map fun x => (.num x, .num x)⟩
  forallStoreNat xs p := ⟨xs.all fun x => (p ⟨(.num x, .num x)⟩).val⟩
  filterNat xs p := ⟨xs.filter (fun x => (p ⟨.num x⟩).val) |>.map Entity.num⟩

  ask := ⟨id⟩
  storePair o x := ⟨(o.val, x.val)⟩
  cont body := ⟨fun k => (body fun x => ⟨k x.val⟩).val⟩
  cont2 body := ⟨fun k => (body fun x => ⟨k x.val⟩).val⟩

  mapEff f inst g x :=
    letI : Functor f.dom := inst
    ⟨g.val <$> x.val⟩
  pureEff f inst x :=
    letI : Applicative f.dom := inst
    ⟨pure x.val⟩
  apEff f inst g x :=
    letI : Applicative f.dom := inst
    ⟨g.val <*> x.val⟩
  joinEff f inst x :=
    letI : Monad f.dom := inst
    ⟨x.val >>= id⟩
  lower x := ⟨x.val id⟩
  scopeMap2 h xs ys := ⟨fun k => xs.val fun a => ys.val fun b => k (h.val a b)⟩
  joinScope x := ⟨fun k => x.val fun y => y k⟩
  counit f g instF instG adj x :=
    letI : Functor f.dom := instF
    letI : Functor g.dom := instG
    letI : Adjoint f.dom g.dom := adj
    ⟨Adjoint.counit x.val⟩
  extend f inst x k :=
    letI : Comonad f.dom := inst
    ⟨Comonad.extend (fun v => (k ⟨v⟩).val) x.val⟩
  eject x := ⟨fun env a => x.val a env⟩

def SemTerm.eval (m : Model) {t : Ty} (e : SemTerm t) : t.dom :=
  (e (repr := Eval m)).val

def SemTerm.den {t : Ty} (e : SemTerm t) : t.dom :=
  e.eval defaultModel

structure Pretty (t : Ty) where
  val : NVal t

namespace Pretty

def build (p : Pretty t) : PBuild :=
  reify t p.val

def expr (p : Pretty t) : PExpr :=
  (p.build 0).1

def renderTop (p : Pretty t) : String :=
  PExpr.render p.expr 0

def ofBuild {t : Ty} (build : PBuild) : Pretty t :=
  ⟨reflect t build⟩

def atom (s : String) : Pretty t :=
  ofBuild (PBuild.atom s)

def var (s : String) : Pretty t :=
  ofBuild (PBuild.var s)

def op1 (name : String) (x : Pretty a) : Pretty b :=
  ofBuild (PBuild.op name [x.build])

def op2 (name : String) (x : Pretty a) (y : Pretty b) : Pretty c :=
  ofBuild (PBuild.op name [x.build, y.build])

def app (f : Pretty (a ~> b)) (x : Pretty a) : Pretty b :=
  ⟨NVal.app f.val x.val⟩

def lam {a b : Ty} (body : Pretty a -> Pretty b) : Pretty (a ~> b) :=
  ⟨.inr fun x => (body ⟨x⟩).val⟩

def bool (b : Bool) : Pretty T :=
  ⟨⟨PBuild.bool b, some b⟩⟩

def conj (p q : Pretty T) : Pretty T :=
  match p.val.known?, q.val.known? with
  | some true, _ => q
  | _, some true => p
  | some false, _ => bool false
  | _, some false => bool false
  | _, _ => ⟨⟨PBuild.conj p.val.build q.val.build, none⟩⟩

def imp (p q : Pretty T) : Pretty T :=
  match p.val.known?, q.val.known? with
  | some false, _ => bool true
  | some true, _ => q
  | _, some true => bool true
  | _, _ => ⟨⟨PBuild.imp p.val.build q.val.build, none⟩⟩

def neg (p : Pretty T) : Pretty T :=
  match p.val.known? with
  | some b => bool (!b)
  | none => ⟨⟨PBuild.neg p.val.build, none⟩⟩

def eqNatPretty (p q : Pretty E) : Pretty T :=
  ⟨⟨PBuild.eqNat p.val q.val, none⟩⟩

def quantNat (q : String) (xs : List Nat) (body : Pretty E -> Pretty T) : Pretty T :=
  ⟨⟨(fun n =>
    let v := "x" ++ toString n
    let (be, n) := (body (var v)).build (n + 1)
    (PExpr.quant q xs v be, n)), none⟩⟩

def quantIn (q : String) (restrict : Pretty (E ~> T)) (body : Pretty E -> Pretty T) : Pretty T :=
  ⟨⟨(fun n =>
    let v := "x" ++ toString n
    let xv : Pretty E := var v
    let (re, n) := (app restrict xv).build (n + 1)
    let (be, n) := (body xv).build n
    (PExpr.quantIn q v re be, n)), none⟩⟩

def selectWhere (body : Pretty E -> Pretty T) : Pretty E :=
  ⟨fun n =>
    let v := "x" ++ toString n
    let (be, n) := (body (var v)).build (n + 1)
    (PExpr.selectWhere v be, n)⟩

def chooseIn (restrict : Pretty (E ~> T)) : Pretty (S E) :=
  ⟨NSpawn.comp (fun x => NVal.app restrict.val x) id⟩

def chooseStoreIn (restrict : Pretty (E ~> T)) : Pretty (S (W^E E)) :=
  ⟨NSpawn.comp (fun x => NVal.app restrict.val x) (fun x => .inr (x, x))⟩

def listStoreNat (xs : List Nat) : Pretty (S (W^E E)) :=
  ⟨NSpawn.known (xs.map fun x => .inr (PBuild.nat x, PBuild.nat x))⟩

def spawnMap {a b : Ty} (g : Pretty (a ~> b)) (x : Pretty (S a)) : Pretty (S b) :=
  ⟨match x.val with
    | .known xs =>
        NSpawn.known (xs.map (NVal.app g.val))
    | .comp c =>
        NSpawn.compVal (NSpawn.mapComp g.val c)
    | .neutral xb =>
        NSpawn.neutral (PBuild.op "map" [g.build, xb])⟩

def spawnPure {a : Ty} (x : Pretty a) : Pretty (S a) :=
  ⟨NSpawn.known [x.val]⟩

def spawnAp {a b : Ty} (f : Pretty (S (a ~> b))) (x : Pretty (S a)) : Pretty (S b) :=
  ⟨match f.val, x.val with
    | .known fs, .known xs =>
        NSpawn.known (fs.flatMap fun f => xs.map (NVal.app f))
    | .known [g], .comp x =>
        NSpawn.compVal (NSpawn.mapComp g x)
    | .comp f, .known [y] =>
        NSpawn.compVal (NSpawn.mapComp (.inr fun g => NVal.app g y) f)
    | .comp f, .comp x =>
        NSpawn.compVal (NSpawn.apComp f x)
    | _, _ =>
        NSpawn.neutral (PBuild.op "ap[S]" [f.build, x.build])⟩

def spawnJoin {a : Ty} (x : Pretty (S (S a))) : Pretty (S a) :=
  ⟨match x.val with
    | .known xs =>
        let ys := xs.filterMap fun
          | .known ys => some ys
          | .neutral _ => none
          | .comp _ => none
        if ys.length == xs.length then NSpawn.known ys.flatten
        else NSpawn.neutral (PBuild.op "concat" [x.build])
    | _ => NSpawn.neutral (PBuild.op "concat" [x.build])⟩

def readerMap {env a b : Ty}
    (g : Pretty (a ~> b)) (x : Pretty (.comp (.query env) a)) :
    Pretty (.comp (.query env) b) :=
  ⟨.inr fun e => NVal.app g.val (NVal.queryApp x.val e)⟩

def readerPure {env a : Ty} (x : Pretty a) : Pretty (.comp (.query env) a) :=
  ⟨.inr fun _ => x.val⟩

def readerAp {env a b : Ty}
    (f : Pretty (.comp (.query env) (a ~> b))) (x : Pretty (.comp (.query env) a)) :
    Pretty (.comp (.query env) b) :=
  ⟨.inr fun e => NVal.app (NVal.queryApp f.val e) (NVal.queryApp x.val e)⟩

def readerJoin {env a : Ty}
    (x : Pretty (.comp (.query env) (.comp (.query env) a))) :
    Pretty (.comp (.query env) a) :=
  ⟨.inr fun e => NVal.queryApp (NVal.queryApp x.val e) e⟩

def readerEject {env a b : Ty}
    (x : Pretty (a ~> .comp (.query env) b)) :
    Pretty (.comp (.query env) (a ~> b)) :=
  ⟨.inr fun e => .inr fun y => NVal.queryApp (NVal.app x.val y) e⟩

def storeMap {out a b : Ty}
    (g : Pretty (a ~> b)) (x : Pretty (.comp (.store out) a)) :
    Pretty (.comp (.store out) b) :=
  let xv := NVal.storeView x.val
  ⟨.inr (xv.1, NVal.app g.val xv.2)⟩

def storeUnit : (out : Ty) -> NVal out
  | .bool => ⟨PBuild.bool true, some true⟩
  | out => reflect out (PBuild.op "unit[W]" [])

def storeSeq : (out : Ty) -> String -> NVal out -> NVal out -> NVal out
  | .bool, _, x, y => (conj ⟨x⟩ ⟨y⟩).val
  | out, name, x, y => reflect out (PBuild.op name [reify out x, reify out y])

def storePure {out a : Ty} (x : Pretty a) : Pretty (.comp (.store out) a) :=
  ⟨.inr (storeUnit out, x.val)⟩

def storeAp {out a b : Ty}
    (f : Pretty (.comp (.store out) (a ~> b))) (x : Pretty (.comp (.store out) a)) :
    Pretty (.comp (.store out) b) :=
  let fv := NVal.storeView f.val
  let xv := NVal.storeView x.val
  ⟨.inr (storeSeq out "ap[W]" fv.1 xv.1, NVal.app fv.2 xv.2)⟩

def storeJoin {out a : Ty}
    (x : Pretty (.comp (.store out) (.comp (.store out) a))) :
    Pretty (.comp (.store out) a) :=
  let xv := NVal.storeView x.val
  let inner := NVal.storeView xv.2
  ⟨.inr (storeSeq out "join[W]" xv.1 inner.1, inner.2)⟩

def storeCounit {out env a : Ty}
    (x : Pretty (.comp (.store out) (.comp (.query env) a))) : Pretty a :=
  if h : out = env then
    by
      subst h
      let xv := NVal.storeView x.val
      exact ⟨NVal.queryApp xv.2 xv.1⟩
  else
    ofBuild (PBuild.op "counit[W,R]" [x.build])

def storeExtend {out a b : Ty}
    (x : Pretty (.comp (.store out) a))
    (k : Pretty (.comp (.store out) a) -> Pretty b) :
    Pretty (.comp (.store out) b) :=
  let xv := NVal.storeView x.val
  ⟨.inr (xv.1, (k x).val)⟩

def scopeMap {ret ans a b : Ty}
    (g : Pretty (a ~> b)) (x : Pretty (.comp (.scope ret ans) a)) :
    Pretty (.comp (.scope ret ans) b) :=
  ⟨.inr fun k => NVal.scopeRun x.val (fun a => k (NVal.app g.val a))⟩

def scopePure {ret ans a : Ty} (x : Pretty a) : Pretty (.comp (.scope ret ans) a) :=
  if h : ret = ans then
    by subst h; exact ⟨.inr fun k => k x.val⟩
  else
    ⟨.inr fun _ => reflect ret (PBuild.op "pure[C]" [x.build])⟩

def scopeApEff {ret ans a b : Ty}
    (f : Pretty (.comp (.scope ret ans) (a ~> b)))
    (x : Pretty (.comp (.scope ret ans) a)) :
    Pretty (.comp (.scope ret ans) b) :=
  if h : ret = ans then
    by
      subst h
      exact ⟨.inr fun k =>
        NVal.scopeRun f.val fun g =>
          NVal.scopeRun x.val fun a =>
            k (NVal.app g a)⟩
  else
    ⟨.inr fun _ => reflect ret (PBuild.op "ap[C]" [f.build, x.build])⟩

def scopeJoinEff {ret ans a : Ty}
    (x : Pretty (.comp (.scope ret ans) (.comp (.scope ret ans) a))) :
    Pretty (.comp (.scope ret ans) a) :=
  if h : ret = ans then
    by subst h; exact ⟨.inr fun k => NVal.scopeRun x.val (fun y => NVal.scopeRun y k)⟩
  else
    ⟨.inr fun _ => reflect ret (PBuild.op "join[C]" [x.build])⟩

def scopeLower {r a : Ty} (x : Pretty (.comp (.scope r a) a)) : Pretty r :=
  ⟨NVal.scopeRun x.val id⟩

def scopeMap2Pretty {r s q a b c : Ty}
    (h : Pretty (a ~> b ~> c))
    (x : Pretty (.comp (.scope r s) a))
    (y : Pretty (.comp (.scope s q) b)) :
    Pretty (.comp (.scope r q) c) :=
  ⟨.inr fun k =>
    NVal.scopeRun x.val fun av =>
      NVal.scopeRun y.val fun bv =>
        k (NVal.app (NVal.app h.val av) bv)⟩

def scopeJoinPretty {r s q a : Ty}
    (x : Pretty (.comp (.scope r s) (.comp (.scope s q) a))) :
    Pretty (.comp (.scope r q) a) :=
  ⟨.inr fun k => NVal.scopeRun x.val (fun z => NVal.scopeRun z k)⟩

end Pretty

def fxName : FX -> String
  | .query _ => "R"
  | .spawn => "S"
  | .store _ => "W"
  | .scope _ _ => "C"

instance : Semantics Pretty where
  prim name := Pretty.atom name

  app f x := Pretty.app f x
  lam body := Pretty.lam body

  bool b := Pretty.bool b
  nat n := ⟨PBuild.nat n⟩
  conj p q := Pretty.conj p q
  imp p q := Pretty.imp p q
  neg p := Pretty.neg p
  eqNat x y := Pretty.eqNatPretty x y
  forallNat xs p := Pretty.quantNat "∀" xs p
  existsNat xs p := Pretty.quantNat "∃" xs p
  selectNat xs p := Pretty.op1 ("ι{" ++ PExpr.renderNatList xs ++ "}") (Pretty.lam p)
  forallIn restrict p := Pretty.quantIn "∀" restrict p
  existsIn restrict p := Pretty.quantIn "∃" restrict p
  selectWhere p := Pretty.selectWhere p
  chooseIn restrict := Pretty.chooseIn restrict
  chooseStoreIn restrict := Pretty.chooseStoreIn restrict
  forallStoreIn restrict p := Pretty.quantIn "∀" restrict fun x =>
    p ⟨.inr (x.val, x.val)⟩
  listNat xs := ⟨NSpawn.known (xs.map PBuild.nat)⟩
  listStoreNat xs := Pretty.listStoreNat xs
  forallStoreNat xs p := Pretty.quantNat "∀" xs fun x =>
    p ⟨.inr (x.val, x.val)⟩
  filterNat xs p := Pretty.op1 ("filter[" ++ PExpr.render (PExpr.listNat xs) 0 ++ "]") (Pretty.lam p)

  ask := ⟨.inr fun x => x⟩
  storePair o x := ⟨.inr (o.val, x.val)⟩
  cont body :=
    ⟨.inr fun k => (body fun x => ⟨NVal.app (.inr k) x.val⟩).val⟩
  cont2 body :=
    ⟨.inr fun k => (body fun x => ⟨NVal.app (.inr k) x.val⟩).val⟩

  mapEff
    | .query _, _, g, x =>
        Pretty.readerMap g x
    | .spawn, _, g, x =>
        Pretty.spawnMap g x
    | .store _, _, g, x =>
        Pretty.storeMap g x
    | .scope _ _, _, g, x =>
        Pretty.scopeMap g x
  pureEff
    | .query _, _, x =>
        Pretty.readerPure x
    | .spawn, _, x =>
        Pretty.spawnPure x
    | .store _, _, x =>
        Pretty.storePure x
    | .scope _ _, _, x =>
        Pretty.scopePure x
  apEff
    | .query _, _, f, x =>
        Pretty.readerAp f x
    | .spawn, _, f, x =>
        Pretty.spawnAp f x
    | .store _, _, f, x =>
        Pretty.storeAp f x
    | .scope _ _, _, f, x =>
        Pretty.scopeApEff f x
  joinEff
    | .query _, _, x =>
        Pretty.readerJoin x
    | .spawn, _, x =>
        Pretty.spawnJoin x
    | .store _, _, x =>
        Pretty.storeJoin x
    | .scope _ _, _, x =>
        Pretty.scopeJoinEff x
  lower x := Pretty.scopeLower x
  scopeMap2 h x y :=
    Pretty.scopeMap2Pretty h x y
  joinScope x :=
    Pretty.scopeJoinPretty x
  counit
    | .store _, .query _, _, _, _, x =>
        Pretty.storeCounit x
    | f, g, _, _, _, x => Pretty.op1 ("counit[" ++ fxName f ++ "," ++ fxName g ++ "]") x
  extend
    | .store _, _, x, k =>
        Pretty.storeExtend x k
    | f, _, x, k =>
        Pretty.op2 ("extend[" ++ fxName f ++ "]") x (Pretty.lam k)
  eject x :=
    Pretty.readerEject x

def SemTerm.pretty {t : Ty} (e : SemTerm t) : String :=
  (e (repr := Pretty)).renderTop

end TDParse

import TDParse.Data.Ty

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

inductive PExpr where
  | atom : String -> PExpr
  | var : String -> PExpr
  | app : PExpr -> PExpr -> PExpr
  | lam : String -> PExpr -> PExpr
  | bool : Bool -> PExpr
  | nat : Nat -> PExpr
  | listNat : List Nat -> PExpr
  | list : List PExpr -> PExpr
  | pair : PExpr -> PExpr -> PExpr
  | fst : PExpr -> PExpr
  | snd : PExpr -> PExpr
  | conj : PExpr -> PExpr -> PExpr
  | imp : PExpr -> PExpr -> PExpr
  | neg : PExpr -> PExpr
  | eqNat : PExpr -> PExpr -> PExpr
  | quant : String -> List Nat -> String -> PExpr -> PExpr
  | quantIn : String -> String -> PExpr -> PExpr -> PExpr
  | selectWhere : String -> PExpr -> PExpr
  | comp : String -> PExpr -> PExpr -> PExpr
  | comp2 : String -> PExpr -> String -> PExpr -> PExpr -> PExpr
  | op : String -> List PExpr -> PExpr

namespace PExpr

def subst (x : String) (v : PExpr) : PExpr -> PExpr
  | .atom s => .atom s
  | .var y => if y == x then v else .var y
  | .app f a => .app (subst x v f) (subst x v a)
  | .lam y body => if y == x then .lam y body else .lam y (subst x v body)
  | .bool b => .bool b
  | .nat n => .nat n
  | .listNat xs => .listNat xs
  | .list xs => .list (xs.map (subst x v))
  | .pair a b => .pair (subst x v a) (subst x v b)
  | .fst p => .fst (subst x v p)
  | .snd p => .snd (subst x v p)
  | .conj p q => .conj (subst x v p) (subst x v q)
  | .imp p q => .imp (subst x v p) (subst x v q)
  | .neg p => .neg (subst x v p)
  | .eqNat p q => .eqNat (subst x v p) (subst x v q)
  | .quant q xs y body => if y == x then .quant q xs y body else .quant q xs y (subst x v body)
  | .quantIn q y restrict body =>
      if y == x then .quantIn q y restrict body
      else .quantIn q y (subst x v restrict) (subst x v body)
  | .selectWhere y body => if y == x then .selectWhere y body else .selectWhere y (subst x v body)
  | .comp y restrict body =>
      if y == x then .comp y restrict body
      else .comp y (subst x v restrict) (subst x v body)
  | .comp2 y restrict z restrict' body =>
      let restrict := if y == x then restrict else subst x v restrict
      let restrict' := if z == x then restrict' else subst x v restrict'
      let body := if y == x || z == x then body else subst x v body
      .comp2 y restrict z restrict' body
  | .op name args => .op name (args.map (subst x v))

partial def normalize : PExpr -> PExpr
  | .app f a =>
      let nf := normalize f
      let na := normalize a
      match nf with
      | .lam x body => normalize (subst x na body)
      | _ => .app nf na
  | .lam x body => .lam x (normalize body)
  | .pair a b => .pair (normalize a) (normalize b)
  | .list xs => .list (xs.map normalize)
  | .fst p =>
      match normalize p with
      | .pair a _ => normalize a
      | p => .fst p
  | .snd p =>
      match normalize p with
      | .pair _ b => normalize b
      | p => .snd p
  | .conj p q => .conj (normalize p) (normalize q)
  | .imp p q => .imp (normalize p) (normalize q)
  | .neg p => .neg (normalize p)
  | .eqNat p q => .eqNat (normalize p) (normalize q)
  | .quant q xs x body => .quant q xs x (normalize body)
  | .quantIn q x restrict body => .quantIn q x (normalize restrict) (normalize body)
  | .selectWhere x body => .selectWhere x (normalize body)
  | .comp x restrict body => .comp x (normalize restrict) (normalize body)
  | .comp2 x restrict y restrict' body =>
      .comp2 x (normalize restrict) y (normalize restrict') (normalize body)
  | .op name args => .op name (args.map normalize)
  | e => e

def paren (b : Bool) (s : String) : String :=
  if b then "(" ++ s ++ ")" else s

mutual
partial def renderNatList : List Nat -> String
  | [] => ""
  | [x] => toString x
  | x :: xs => toString x ++ "," ++ renderNatList xs

partial def renderList : List PExpr -> String
  | [] => ""
  | [x] => x.render 0
  | x :: xs => x.render 0 ++ ", " ++ renderList xs

partial def renderArgs : List PExpr -> String
  | [] => ""
  | e :: es => " " ++ e.render 11 ++ renderArgs es

partial def render : PExpr -> Nat -> String
  | .atom s, _ => s
  | .var s, _ => s
  | .bool b, _ => if b then "true" else "false"
  | .nat n, _ => toString n
  | .listNat xs, _ => "[" ++ renderNatList xs ++ "]"
  | .list xs, _ => "[" ++ renderList xs ++ "]"
  | .pair a b, _ => "⟨" ++ a.render 0 ++ ", " ++ b.render 0 ++ "⟩"
  | .fst p, prec => paren (prec > 10) ("fst " ++ p.render 11)
  | .snd p, prec => paren (prec > 10) ("snd " ++ p.render 11)
  | .app f x, p => paren (p > 10) (f.render 10 ++ " " ++ x.render 11)
  | .lam x body, p => paren (p > 0) ("λ" ++ x ++ ". " ++ body.render 0)
  | .conj p q, prec => paren (prec > 3) (p.render 4 ++ " ∧ " ++ q.render 3)
  | .imp p q, prec => paren (prec > 2) (p.render 3 ++ " → " ++ q.render 2)
  | .neg p, prec => paren (prec > 10) ("¬" ++ p.render 11)
  | .eqNat x y, prec => paren (prec > 4) (x.render 5 ++ " = " ++ y.render 5)
  | .quant q xs x body, prec =>
      paren (prec > 0) (q ++ x ++ "∈{" ++ renderNatList xs ++ "}. " ++ body.render 0)
  | .quantIn q x restrict body, prec =>
      paren (prec > 0) (q ++ x ++ "[" ++ restrict.render 0 ++ "]. " ++ body.render 0)
  | .selectWhere x body, prec =>
      paren (prec > 0) ("ι" ++ x ++ "[" ++ body.render 0 ++ "]")
  | .comp _ restrict body, _ =>
      "[" ++ body.render 0 ++ " | " ++ restrict.render 0 ++ "]"
  | .comp2 _ restrict _ restrict' body, _ =>
      "[" ++ body.render 0 ++ " | " ++ restrict.render 0 ++ ", " ++ restrict'.render 0 ++ "]"
  | .op name args, p => paren (p > 10) (name ++ renderArgs args)
end

def asList? : PExpr -> Option (List PExpr)
  | .list xs => some xs
  | .listNat xs => some (xs.map PExpr.nat)
  | _ => none

end PExpr

/- Printable interpretation -/

structure Pretty (t : Ty) where
  build : Nat -> PExpr × Nat

namespace Pretty

def expr (p : Pretty t) : PExpr :=
  (p.build 0).1

def renderTop (p : Pretty t) : String :=
  PExpr.render (PExpr.normalize p.expr) 0

def atom (s : String) : Pretty t :=
  ⟨fun n => (PExpr.atom s, n)⟩

def var (s : String) : Pretty t :=
  ⟨fun n => (PExpr.var s, n)⟩

def ofExpr (e : PExpr) : Pretty t :=
  ⟨fun n => (e, n)⟩

def unary (name : String) (x : Pretty a) : Pretty b :=
  ⟨fun n =>
    let (xe, n) := x.build n
    (PExpr.op name [xe], n)⟩

def binary (name : String) (x : Pretty a) (y : Pretty b) : Pretty c :=
  ⟨fun n =>
    let (xe, n) := x.build n
    let (ye, n) := y.build n
    (PExpr.op name [xe, ye], n)⟩

def ternary (name : String) (x : Pretty a) (y : Pretty b) (z : Pretty c) : Pretty d :=
  ⟨fun n =>
    let (xe, n) := x.build n
    let (ye, n) := y.build n
    let (ze, n) := z.build n
    (PExpr.op name [xe, ye, ze], n)⟩

def app (f : Pretty (a ~> b)) (x : Pretty a) : Pretty b :=
  ⟨fun n =>
    let (fe, n) := f.build n
    let (xe, n) := x.build n
    (PExpr.app fe xe, n)⟩

def appRaw (f : Pretty a) (x : Pretty b) : Pretty c :=
  ⟨fun n =>
    let (fe, n) := f.build n
    let (xe, n) := x.build n
    (PExpr.app fe xe, n)⟩

def pairRaw (x : Pretty a) (y : Pretty b) : Pretty c :=
  ⟨fun n =>
    let (xe, n) := x.build n
    let (ye, n) := y.build n
    (PExpr.pair xe ye, n)⟩

def fstRaw (x : Pretty a) : Pretty b :=
  ⟨fun n =>
    let (xe, n) := x.build n
    (PExpr.fst xe, n)⟩

def sndRaw (x : Pretty a) : Pretty b :=
  ⟨fun n =>
    let (xe, n) := x.build n
    (PExpr.snd xe, n)⟩

def lam {a b : Ty} (body : Pretty a -> Pretty b) : Pretty (a ~> b) :=
  ⟨fun n =>
    let v := "x" ++ toString n
    let (be, n) := (body (var v)).build (n + 1)
    (PExpr.lam v be, n)⟩

def lamRaw (body : Pretty a -> Pretty b) : Pretty c :=
  ⟨fun n =>
    let v := "x" ++ toString n
    let (be, n) := (body (var v)).build (n + 1)
    (PExpr.lam v be, n)⟩

def idLam : Pretty (a ~> a) :=
  lam fun x => x

def op1 := @unary
def op2 := @binary
def op3 := @ternary

def conjRaw (p q : Pretty a) : Pretty b :=
  ⟨fun n =>
    let (pe, n) := p.build n
    let (qe, n) := q.build n
    (PExpr.conj pe qe, n)⟩

def impRaw (p q : Pretty a) : Pretty b :=
  ⟨fun n =>
    let (pe, n) := p.build n
    let (qe, n) := q.build n
    (PExpr.imp pe qe, n)⟩

def quantNat (q : String) (xs : List Nat) (body : Pretty E -> Pretty T) : Pretty T :=
  ⟨fun n =>
    let v := "x" ++ toString n
    let (be, n) := (body (var v)).build (n + 1)
    (PExpr.quant q xs v be, n)⟩

def quantIn (q : String) (restrict : Pretty (E ~> T)) (body : Pretty E -> Pretty T) : Pretty T :=
  ⟨fun n =>
    let v := "x" ++ toString n
    let xv : Pretty E := var v
    let (re, n) := (app restrict xv).build (n + 1)
    let (be, n) := (body xv).build n
    (PExpr.quantIn q v re be, n)⟩

def selectWhere (body : Pretty E -> Pretty T) : Pretty E :=
  ⟨fun n =>
    let v := "x" ++ toString n
    let (be, n) := (body (var v)).build (n + 1)
    (PExpr.selectWhere v be, n)⟩

def chooseIn (restrict : Pretty (E ~> T)) : Pretty (S E) :=
  ⟨fun n =>
    let v := "x" ++ toString n
    let xv : Pretty E := var v
    let (re, n) := (app restrict xv).build (n + 1)
    (PExpr.comp v re (PExpr.var v), n)⟩

def chooseStoreIn (restrict : Pretty (E ~> T)) : Pretty (S (W^E E)) :=
  ⟨fun n =>
    let v := "x" ++ toString n
    let xv : Pretty E := var v
    let (re, n) := (app restrict xv).build (n + 1)
    (PExpr.comp v re (PExpr.pair (PExpr.var v) (PExpr.var v)), n)⟩

def listStoreNat (xs : List Nat) : Pretty (S (W^E E)) :=
  let entries := xs.map fun x => PExpr.pair (PExpr.nat x) (PExpr.nat x)
  ofExpr (PExpr.list entries)

def spawnMap {a b : Ty} (g : Pretty (a ~> b)) (x : Pretty (S a)) : Pretty (S b) :=
  ⟨fun n =>
    let (ge, n) := g.build n
    let (xe, n) := x.build n
    match PExpr.asList? (PExpr.normalize xe) with
    | some xs => (PExpr.list (xs.map fun x => PExpr.app ge x), n)
    | none =>
        match PExpr.normalize xe with
        | .comp v restrict body =>
            (PExpr.comp v restrict (PExpr.app ge body), n)
        | xe => (PExpr.op "map" [ge, xe], n)⟩

def spawnPure {a : Ty} (x : Pretty a) : Pretty (S a) :=
  ⟨fun n =>
    let (xe, n) := x.build n
    (PExpr.list [xe], n)⟩

def spawnAp {a b : Ty} (f : Pretty (S (a ~> b))) (x : Pretty (S a)) : Pretty (S b) :=
  ⟨fun n =>
    let (fe, n) := f.build n
    let (xe, n) := x.build n
    match PExpr.asList? (PExpr.normalize fe), PExpr.asList? (PExpr.normalize xe) with
    | some fs, some xs => (PExpr.list (fs.flatMap fun f => xs.map fun x => PExpr.app f x), n)
    | _, _ =>
        match PExpr.normalize fe, PExpr.normalize xe with
        | .comp fv frestrict fbody, .comp xv xrestrict xbody =>
            (PExpr.comp2 fv frestrict xv xrestrict (PExpr.app fbody xbody), n)
        | fe, xe => (PExpr.op "ap[S]" [fe, xe], n)⟩

def spawnJoin {a : Ty} (x : Pretty (S (S a))) : Pretty (S a) :=
  ⟨fun n =>
    let (xe, n) := x.build n
    match PExpr.asList? (PExpr.normalize xe) with
    | some xs =>
        let ys := xs.filterMap fun x => PExpr.asList? (PExpr.normalize x)
        if ys.length == xs.length then (PExpr.list ys.flatten, n)
        else (PExpr.op "concat" [xe], n)
    | none => (PExpr.op "concat" [xe], n)⟩

def readerMap {env a b : Ty}
    (g : Pretty (a ~> b)) (x : Pretty (.comp (.query env) a)) :
    Pretty (.comp (.query env) b) :=
  lamRaw (a := env) (b := b) fun e =>
    app g (appRaw (a := .comp (.query env) a) (b := env) (c := a) x e)

def readerPure {env a : Ty} (x : Pretty a) : Pretty (.comp (.query env) a) :=
  lamRaw (a := env) (b := a) fun _ => x

def readerAp {env a b : Ty}
    (f : Pretty (.comp (.query env) (a ~> b))) (x : Pretty (.comp (.query env) a)) :
    Pretty (.comp (.query env) b) :=
  lamRaw (a := env) (b := b) fun e =>
    appRaw
      (a := a ~> b) (b := a) (c := b)
      (appRaw (a := .comp (.query env) (a ~> b)) (b := env) (c := a ~> b) f e)
      (appRaw (a := .comp (.query env) a) (b := env) (c := a) x e)

def readerJoin {env a : Ty}
    (x : Pretty (.comp (.query env) (.comp (.query env) a))) :
    Pretty (.comp (.query env) a) :=
  lamRaw (a := env) (b := a) fun e =>
    appRaw
      (a := .comp (.query env) a) (b := env) (c := a)
      (appRaw (a := .comp (.query env) (.comp (.query env) a)) (b := env)
        (c := .comp (.query env) a) x e)
      e

def readerEject {env a b : Ty}
    (x : Pretty (a ~> .comp (.query env) b)) :
    Pretty (.comp (.query env) (a ~> b)) :=
  lamRaw (a := env) (b := a ~> b) fun e =>
    lam fun y =>
      appRaw (a := .comp (.query env) b) (b := env) (c := b) (app x y) e

def storeMap {out a b : Ty}
    (g : Pretty (a ~> b)) (x : Pretty (.comp (.store out) a)) :
    Pretty (.comp (.store out) b) :=
  pairRaw (c := .comp (.store out) b)
    (fstRaw (b := out) x)
    (app g (sndRaw (b := a) x))

def storePure {out a : Ty} (x : Pretty a) : Pretty (.comp (.store out) a) :=
  pairRaw (c := .comp (.store out) a)
    (ofExpr (PExpr.bool true) : Pretty T)
    x

def storeAp {out a b : Ty}
    (f : Pretty (.comp (.store out) (a ~> b))) (x : Pretty (.comp (.store out) a)) :
    Pretty (.comp (.store out) b) :=
  pairRaw (c := .comp (.store out) b)
    (conjRaw (fstRaw (b := T) f) (fstRaw (b := T) x) : Pretty T)
    (appRaw (a := a ~> b) (b := a) (c := b) (sndRaw (b := a ~> b) f) (sndRaw (b := a) x))

def storeJoin {out a : Ty}
    (x : Pretty (.comp (.store out) (.comp (.store out) a))) :
    Pretty (.comp (.store out) a) :=
  let inner : Pretty (.comp (.store out) a) := sndRaw (b := .comp (.store out) a) x
  pairRaw (c := .comp (.store out) a)
    (conjRaw (fstRaw (b := T) x) (fstRaw (b := T) inner) : Pretty T)
    (sndRaw (b := a) inner)

def storeCounit {out env a : Ty}
    (x : Pretty (.comp (.store out) (.comp (.query env) a))) : Pretty a :=
  appRaw (a := .comp (.query env) a) (b := out) (c := a)
    (sndRaw (b := .comp (.query env) a) x)
    (fstRaw (b := out) x)

def storeExtend {out a b : Ty}
    (x : Pretty (.comp (.store out) a))
    (k : Pretty (.comp (.store out) a) -> Pretty b) :
    Pretty (.comp (.store out) b) :=
  pairRaw (c := .comp (.store out) b)
    (fstRaw (b := out) x)
    (k x)

def scopeMap {ret ans a b : Ty}
    (g : Pretty (a ~> b)) (x : Pretty (.comp (.scope ret ans) a)) :
    Pretty (.comp (.scope ret ans) b) :=
  lamRaw (a := b ~> ans) (b := ret) fun k =>
    appRaw (a := .comp (.scope ret ans) a) (b := a ~> ans) (c := ret) x
      (lam fun y => appRaw (a := b ~> ans) (b := b) (c := ans) k (app g y))

def scopePure {ret ans a : Ty} (x : Pretty a) : Pretty (.comp (.scope ret ans) a) :=
  lamRaw (a := a ~> ans) (b := ans) fun k =>
    appRaw (a := a ~> ans) (b := a) (c := ans) k x

def scopeApEff {ret ans a b : Ty}
    (f : Pretty (.comp (.scope ret ans) (a ~> b)))
    (x : Pretty (.comp (.scope ret ans) a)) :
    Pretty (.comp (.scope ret ans) b) :=
  lamRaw (a := b ~> ans) (b := ans) fun k =>
    appRaw (a := .comp (.scope ret ans) (a ~> b)) (b := (a ~> b) ~> ans) (c := ans) f
      (lam fun g =>
        appRaw (a := .comp (.scope ret ans) a) (b := a ~> ans) (c := ans) x
          (lam fun y => appRaw (a := b ~> ans) (b := b) (c := ans) k (app g y)))

def scopeJoinEff {ret ans a : Ty}
    (x : Pretty (.comp (.scope ret ans) (.comp (.scope ret ans) a))) :
    Pretty (.comp (.scope ret ans) a) :=
  lamRaw (a := a ~> ans) (b := ans) fun k =>
    appRaw (a := .comp (.scope ret ans) (.comp (.scope ret ans) a))
      (b := .comp (.scope ret ans) a ~> ans) (c := ans) x
      (lam fun y =>
        appRaw (a := .comp (.scope ret ans) a) (b := a ~> ans) (c := ans) y k)

def scopeLower {r a : Ty} (x : Pretty (.comp (.scope r a) a)) : Pretty r :=
  appRaw (a := .comp (.scope r a) a) (b := a ~> a) (c := r) x idLam

def scopeMap2Pretty {r s q a b c : Ty}
    (h : Pretty (a ~> b ~> c))
    (x : Pretty (.comp (.scope r s) a))
    (y : Pretty (.comp (.scope s q) b)) :
    Pretty (.comp (.scope r q) c) :=
  lamRaw (a := c ~> q) (b := r) fun k =>
    appRaw (a := .comp (.scope r s) a) (b := a ~> s) (c := r) x
      (lam fun av =>
        appRaw (a := .comp (.scope s q) b) (b := b ~> q) (c := s) y
          (lam fun bv =>
            appRaw (a := c ~> q) (b := c) (c := q) k (app (app h av) bv)))

def scopeJoinPretty {r s q a : Ty}
    (x : Pretty (.comp (.scope r s) (.comp (.scope s q) a))) :
    Pretty (.comp (.scope r q) a) :=
  lamRaw (a := a ~> q) (b := r) fun k =>
    appRaw (a := .comp (.scope r s) (.comp (.scope s q) a))
      (b := .comp (.scope s q) a ~> s) (c := r) x
      (lam fun z =>
        appRaw (a := .comp (.scope s q) a) (b := a ~> q) (c := s) z k)

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

  bool b := Pretty.ofExpr (PExpr.bool b)
  nat n := Pretty.ofExpr (PExpr.nat n)
  conj p q := ⟨fun n =>
    let (pe, n) := p.build n
    let (qe, n) := q.build n
    (PExpr.conj pe qe, n)⟩
  imp p q := ⟨fun n =>
    let (pe, n) := p.build n
    let (qe, n) := q.build n
    (PExpr.imp pe qe, n)⟩
  neg p := ⟨fun n =>
    let (pe, n) := p.build n
    (PExpr.neg pe, n)⟩
  eqNat x y := ⟨fun n =>
    let (xe, n) := x.build n
    let (ye, n) := y.build n
    (PExpr.eqNat xe ye, n)⟩
  forallNat xs p := Pretty.quantNat "∀" xs p
  existsNat xs p := Pretty.quantNat "∃" xs p
  selectNat xs p := Pretty.op1 ("ι{" ++ PExpr.renderNatList xs ++ "}") (Pretty.lam p)
  forallIn restrict p := Pretty.quantIn "∀" restrict p
  existsIn restrict p := Pretty.quantIn "∃" restrict p
  selectWhere p := Pretty.selectWhere p
  chooseIn restrict := Pretty.chooseIn restrict
  chooseStoreIn restrict := Pretty.chooseStoreIn restrict
  forallStoreIn restrict p := Pretty.quantIn "∀" restrict fun x =>
    p (Pretty.pairRaw x x)
  listNat xs := Pretty.ofExpr (PExpr.listNat xs)
  listStoreNat xs := Pretty.listStoreNat xs
  forallStoreNat xs p := Pretty.quantNat "∀" xs fun x =>
    p (Pretty.pairRaw x x)
  filterNat xs p := Pretty.op1 ("filter[" ++ PExpr.render (PExpr.listNat xs) 0 ++ "]") (Pretty.lam p)

  ask {a} := Pretty.lamRaw (a := a) (b := a) fun x => x
  storePair o x := Pretty.pairRaw o x
  cont {r} {a} body :=
    Pretty.lamRaw (a := a ~> r) (b := r) fun k =>
      body fun x => Pretty.appRaw (a := a ~> r) (b := a) (c := r) k x
  cont2 {r} {s} {a} body :=
    Pretty.lamRaw (a := a ~> s) (b := r) fun k =>
      body fun x => Pretty.appRaw (a := a ~> s) (b := a) (c := s) k x

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

import TDParse.Data.PExpr

/-!
Normalization-by-evaluation for inspectable semantic values.

Known Lean functions, booleans, pairs, lists, and continuations are kept as
host-language values.  Neutral values keep their printable syntax.  Reification
turns either form back into `PExpr`, performing beta-like reductions by applying
host closures instead of substituting over syntax.
-/

namespace TDParse

structure NBool where
  build : PBuild
  known? : Option Bool := none

structure SpawnComp (A : Type) where
  restricts : List (PBuild -> NBool)
  body : List PBuild -> A

inductive SpawnVal (A : Type) where
  | neutral : PBuild -> SpawnVal A
  | known : List A -> SpawnVal A
  | comp : SpawnComp A -> SpawnVal A

def NVal : Ty -> Type
  | .nat => PBuild
  | .bool => NBool
  | .fn a b => Sum PBuild (NVal a -> NVal b)
  | .comp .spawn a => SpawnVal (NVal a)
  | .comp (.query e) a => Sum PBuild (NVal e -> NVal a)
  | .comp (.store o) a => Sum PBuild (NVal o × NVal a)
  | .comp (.scope r s) a => Sum PBuild ((NVal a -> NVal s) -> NVal r)

mutual
def reify : (t : Ty) -> NVal t -> PBuild
  | .nat, x => x
  | .bool, x => x.build
  | .fn _ _, .inl x => x
  | .fn a b, .inr f =>
      PBuild.lam fun v =>
        let x := reflect a (PBuild.var v)
        reify b (f x)
  | .comp .spawn _, .neutral x => x
  | .comp .spawn a, .known xs => PBuild.list (xs.map (reify a))
  | .comp .spawn a, .comp c =>
      fun n =>
        let (binds, vars, n) := c.restricts.foldl
          (fun (binds, vars, n) restrict =>
            let v := "x" ++ toString n
            let x := PBuild.var v
            let (re, n) := reify T (restrict x) (n + 1)
            ((v, re) :: binds, x :: vars, n))
          ([], [], n)
        let (be, n) := reify a (c.body vars.reverse) n
        (PExpr.comp binds.reverse be, n)
  | .comp (.query _) _, .inl x => x
  | .comp (.query e) a, .inr f =>
      PBuild.lam fun v =>
        let env := reflect e (PBuild.var v)
        reify a (f env)
  | .comp (.store _) _, .inl x => x
  | .comp (.store o) a, .inr x =>
      PBuild.pair (reify o x.1) (reify a x.2)
  | .comp (.scope _ _) _, .inl x => x
  | .comp (.scope r s) a, .inr x =>
      PBuild.lam fun v =>
        let k := reflect (a ~> s) (PBuild.var v)
        reify r (x (apply k))

def reflect : (t : Ty) -> PBuild -> NVal t
  | .nat, x => x
  | .bool, x => ⟨x, none⟩
  | .fn _ _, x => .inl x
  | .comp .spawn _, x => .neutral x
  | .comp (.query _) _, x => .inl x
  | .comp (.store _) _, x => .inl x
  | .comp (.scope _ _) _, x => .inl x

def apply {a b : Ty} (f : NVal (a ~> b)) (x : NVal a) : NVal b :=
  match f with
  | .inr f => f x
  | .inl fb => reflect b (PBuild.app fb (reify a x))
end

namespace NSpawn

def neutral {a : Ty} (build : PBuild) : NVal (S a) :=
  .neutral build

def known {a : Ty} (xs : List (NVal a)) : NVal (S a) :=
  .known xs

def compVal {a : Ty} (c : SpawnComp (NVal a)) : NVal (S a) :=
  .comp c

def comp {a : Ty} (restrict : PBuild -> NBool) (body : PBuild -> NVal a) : NVal (S a) :=
  .comp {
    restricts := [restrict]
    -- Reification supplies one fresh variable per restriction.  The fallback
    -- is therefore unreachable unless a `SpawnComp` is built inconsistently.
    body := fun
      | [x] => body x
      | _ => reflect a (PBuild.op "arity[S]" [])
  }

def mapComp {a b : Ty} (g : NVal (a ~> b)) (c : SpawnComp (NVal a)) : SpawnComp (NVal b) :=
  { restricts := c.restricts
    body := fun xs => apply g (c.body xs) }

def apComp {a b : Ty}
    (f : SpawnComp (NVal (a ~> b))) (x : SpawnComp (NVal a)) :
    SpawnComp (NVal b) :=
  { restricts := f.restricts ++ x.restricts
    body := fun xs =>
      let fs := xs.take f.restricts.length
      let ys := xs.drop f.restricts.length
      apply (f.body fs) (x.body ys) }

end NSpawn

namespace NVal

def app {a b : Ty} (f : NVal (a ~> b)) (x : NVal a) : NVal b :=
  TDParse.apply f x

def queryApp {env a : Ty} (x : NVal (R^env a)) (e : NVal env) : NVal a :=
  match x with
  | .inr f => f e
  | .inl xb => reflect a (PBuild.app xb (reify env e))

def storeView {out a : Ty} (x : NVal (W^out a)) : NVal out × NVal a :=
  match x with
  | .inr p => p
  | .inl xb => (reflect out (PBuild.fst xb), reflect a (PBuild.snd xb))

def scopeRun {r s a : Ty} (x : NVal (C^r s a)) (k : NVal a -> NVal s) : NVal r :=
  match x with
  | .inr f => f k
  | .inl xb => reflect r (PBuild.app xb (reify (a ~> s) (.inr k)))

end NVal

end TDParse

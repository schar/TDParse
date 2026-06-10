import TDParse.Semantics

-- Syntactic derivations
-- ------------------------------------------------------------------------

inductive Cat : Type where
  | CP | Cmp -- Clauses and Complementizers
  | CBar | DBar | Cor -- Coordinators and Coordination Phrases
  | DP | Det | Gen | GenD | Dmp -- (Genitive) Determiners and full Determiner Phrases
  | NP | TN -- Transitive (relational) Nouns and full Noun Phrases
  | VP | TV | DV | AV -- Transitive, Ditransitive, and Attitude Verbs and Verb Phrases
  | AdjP | TAdj | Deg | AdvP | TAdv -- Modifiers
deriving Repr, BEq, DecidableEq

abbrev CFG := Cat -> Cat -> List Cat

inductive Tree (c : Type) (a : Type u): Type u where
  | leaf   : c -> a -> Tree c a
  | node   : c -> Tree c a → Tree c a → Tree c a
  | island : c -> Tree c a → Tree c a → Tree c a  -- scope-island: filters unresolved C
deriving Repr

def Tree.root : (t : Tree c a) -> c
  | .leaf   c _   => c
  | .node   c _ _ => c
  | .island c _ _ => c


-- Semantic derivations
-- ------------------------------------------------------------------------

inductive ModeLabel : Type where
  | FA | BA | PM | FC
  | MR (f : FX) (m : ModeLabel) | ML (f : FX) (m : ModeLabel)
  | AP (f : FX) (m : ModeLabel)
  | UR (f : FX) (m : ModeLabel) | UL (f : FX) (m : ModeLabel)
  | CU (f g : FX) (m : ModeLabel)
  | JN (f : FX) (m : ModeLabel)
  | DN (m : ModeLabel)
  | EL (f : FX) (m : ModeLabel) | ER (f : FX) (m : ModeLabel)
  | XL (f : FX) (m : ModeLabel)
deriving BEq, DecidableEq

-- A first-order recipe for the semantic content of a mode.
-- The parser stores this small syntax in chart entries; `ModeOp.sem` interprets
-- it tagless-finally when we want denotations or printable higher-order terms.
inductive ModeOp : Ty -> Ty -> Ty -> Type where
  | fa : ModeOp (a ~> b) a b
  | ba : ModeOp a (a ~> b) b
  | pm : ModeOp (a ~> T) (a ~> T) (a ~> T)
  | fc : ModeOp (b ~> c) (a ~> b) (a ~> c)
  | ml (f : FX) (ok : (functor f).isSome = true) :
      ModeOp a b c -> ModeOp (.comp f a) b (.comp f c)
  | mr (f : FX) (ok : (functor f).isSome = true) :
      ModeOp a b c -> ModeOp a (.comp f b) (.comp f c)
  | ap (f : FX) (ok : (applicative f).isSome = true) :
      ModeOp a b c -> ModeOp (.comp f a) (.comp f b) (.comp f c)
  | ul (f : FX) (ok : (applicative f).isSome = true) :
      ModeOp a (b ~> b') c -> ModeOp a (.comp f b ~> b') c
  | ur (f : FX) (ok : (applicative f).isSome = true) :
      ModeOp (a ~> a') b c -> ModeOp (.comp f a ~> a') b c
  | cu (f g : FX) (ok : (adjoint f g).isSome = true) :
      ModeOp a b c -> ModeOp (.comp f a) (.comp g b) c
  | jn (f : FX) (ok : (monad f).isSome = true) :
      ModeOp a b (.comp f (.comp f c)) -> ModeOp a b (.comp f c)
  | dn : ModeOp a b (.comp (.scope c ans) ans) -> ModeOp a b c
  | scopeAp : ModeOp a b c ->
      ModeOp (.comp (.scope r s) a) (.comp (.scope s q) b) (.comp (.scope r q) c)
  | scopeJn :
      ModeOp a b (.comp (.scope r s) (.comp (.scope s q) c)) ->
      ModeOp a b (.comp (.scope r q) c)
  | xl (f : FX) (ok : (comonad f).isSome = true) :
      ModeOp (.comp f a) b c -> ModeOp (.comp f a) b (.comp f c)
  | el (i : Ty) : ModeOp (.comp (.query i) (a ~> b)) c d ->
      ModeOp (a ~> .comp (.query i) b) c d
  | er (i : Ty) : ModeOp a (.comp (.query i) (b ~> c)) d ->
      ModeOp a (b ~> .comp (.query i) c) d

def ModeOp.label {a b c : Ty} : ModeOp a b c -> ModeLabel
  | .fa => .FA
  | .ba => .BA
  | .pm => .PM
  | .fc => .FC
  | .ml f _ m => .ML f m.label
  | .mr f _ m => .MR f m.label
  | .ap f _ m => .AP f m.label
  | .ul f _ m => .UL f m.label
  | .ur f _ m => .UR f m.label
  | .cu f g _ m => .CU f g m.label
  | .jn f _ m => .JN f m.label
  | .dn m => .DN m.label
  | .scopeAp (r := r) (q := q) m => .AP (.scope r q) m.label
  | .scopeJn (r := r) (q := q) m => .JN (.scope r q) m.label
  | .xl f _ m => .XL f m.label
  | .el i m => .EL (.query i) m.label
  | .er i m => .ER (.query i) m.label

def ModeOp.sem {a b c : Ty} (op : ModeOp a b c)
    {repr : Ty -> Type} [TDParse.Semantics repr] :
    repr a -> repr b -> repr c :=
  match op with
  | .fa => fun x y => TDParse.Semantics.app x y
  | .ba => fun x y => TDParse.Semantics.app y x
  | .pm => fun p q => TDParse.Semantics.lam fun x =>
      TDParse.Semantics.conj (TDParse.Semantics.app p x) (TDParse.Semantics.app q x)
  | .fc => fun f g => TDParse.Semantics.lam fun x =>
      TDParse.Semantics.app f (TDParse.Semantics.app g x)
  | .ml f ok m => fun xs y =>
      TDParse.Semantics.mapEff f ok
        (TDParse.Semantics.lam fun x => m.sem x y)
        xs
  | .mr f ok m => fun x ys =>
      TDParse.Semantics.mapEff f ok
        (TDParse.Semantics.lam fun y => m.sem x y)
        ys
  | .ap f ok m => fun xs ys =>
      TDParse.Semantics.apEff f ok
        (TDParse.Semantics.mapEff f f.functorOk
          (TDParse.Semantics.lam fun x => TDParse.Semantics.lam fun y => m.sem x y)
          xs)
        ys
  | .ul f ok m => fun x y =>
      m.sem x (TDParse.Semantics.lam fun b =>
        TDParse.Semantics.app y (TDParse.Semantics.pureEff f ok b))
  | .ur f ok m => fun x y =>
      m.sem (TDParse.Semantics.lam fun a =>
        TDParse.Semantics.app x (TDParse.Semantics.pureEff f ok a)) y
  | .cu f g ok m => fun xs ys =>
      TDParse.Semantics.counit f g ok
        (TDParse.Semantics.mapEff f f.functorOk
          (TDParse.Semantics.lam fun x =>
            TDParse.Semantics.mapEff g g.functorOk
              (TDParse.Semantics.lam fun y => m.sem x y)
              ys)
          xs)
  | .jn f ok m => fun x y =>
      TDParse.Semantics.joinEff f ok (m.sem x y)
  | .dn m => fun x y => TDParse.Semantics.lower (m.sem x y)
  | .scopeAp m => fun xs ys =>
      TDParse.Semantics.scopeMap2
        (TDParse.Semantics.lam fun x => TDParse.Semantics.lam fun y => m.sem x y)
        xs
        ys
  | .scopeJn m => fun x y => TDParse.Semantics.joinScope (m.sem x y)
  | .xl f ok m => fun xs y =>
      TDParse.Semantics.extend f ok xs (fun xs' => m.sem xs' y)
  | .el _ m => fun x y => m.sem (TDParse.Semantics.eject x) y
  | .er _ m => fun x y => m.sem x (TDParse.Semantics.eject y)

structure Mode (a : Ty) (b : Ty) (c : Ty) where
  recipe : ModeOp a b c

def Mode.mode (m : Mode a b c) : ModeLabel :=
  m.recipe.label

structure Lexeme (t : Ty) where
  name : String
  sem : TDParse.SemTerm t

inductive Expr : Ty → Type 1 where
  | lexeme : Lexeme a → Expr a
  | moc : Mode a b c → Expr a → Expr b → Expr c

namespace Expr

-- Surface constructors for lexical entries.  These are meant to be used like
-- tagless-final lexical definitions: the lexicon names abstract meanings, and
-- concrete model values are supplied only by an interpreter.
def lexWith {a : Ty} (name : String) (sem : TDParse.SemTerm a) : Expr a :=
  .lexeme ⟨name, sem⟩

def lex {a : Ty} (name : String) : Expr a :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.prim name)

def litNat (name : String) (n : Nat) : Expr E :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.nat n)

def entity (name : String) : Expr E :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.prim name)

def fun1 {a b : Ty} (name : String) : Expr (a ~> b) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    let f := TDParse.Semantics.prim (t := a ~> b) name
    TDParse.Semantics.lam fun x => TDParse.Semantics.app f x)

def fun2 {a b c : Ty} (name : String) : Expr (a ~> b ~> c) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    let f := TDParse.Semantics.prim (t := a ~> b ~> c) name
    TDParse.Semantics.lam fun x =>
      TDParse.Semantics.lam fun y =>
        TDParse.Semantics.app (TDParse.Semantics.app f x) y)

def fun3 {a b c d : Ty} (name : String) : Expr (a ~> b ~> c ~> d) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    let f := TDParse.Semantics.prim (t := a ~> b ~> c ~> d) name
    TDParse.Semantics.lam fun x =>
      TDParse.Semantics.lam fun y =>
        TDParse.Semantics.lam fun z =>
          TDParse.Semantics.app (TDParse.Semantics.app (TDParse.Semantics.app f x) y) z)

def ask {a : Ty} (name : String) : Expr (R^a a) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.ask (a := a))

def askStore {a : Ty} (name : String) : Expr (R^a (W^a a)) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.mapEff (FX.query a) (FX.query a).functorOk
      (TDParse.Semantics.lam fun x => TDParse.Semantics.storePair x x)
      (TDParse.Semantics.ask (a := a)))

def askStoreRel (name : String) : Expr ((E ~> E) ~> R^E (W^E E)) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.lam fun rel =>
      TDParse.Semantics.mapEff (FX.query E) (FX.query E).functorOk
        (TDParse.Semantics.lam fun x =>
          TDParse.Semantics.storePair x (TDParse.Semantics.app rel x))
        TDParse.Semantics.ask)

def askStoreRelFn (name : String) : Expr ((E ~> E) ~> R^E (W^(R^E E) E)) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.lam fun rel =>
      let relReader :=
        TDParse.Semantics.mapEff (FX.query E) (FX.query E).functorOk rel TDParse.Semantics.ask
      TDParse.Semantics.mapEff (FX.query E) (FX.query E).functorOk
        (TDParse.Semantics.lam fun x =>
          TDParse.Semantics.storePair relReader (TDParse.Semantics.app rel x))
        TDParse.Semantics.ask)

def storeEntity (name : String) : Expr (W^E E) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.storePair
      (TDParse.Semantics.prim (t := E) name)
      (TDParse.Semantics.prim (t := E) name))

def storeBoolEntity (name predName entityName : String) : Expr (W^T E) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.storePair
      (TDParse.Semantics.app
        (TDParse.Semantics.prim (t := E ~> T) predName)
        (TDParse.Semantics.prim (t := E) entityName))
      (TDParse.Semantics.prim (t := E) entityName))

def possessive : Expr (E ~> (E ~> E) ~> E) :=
  lexWith "'s" (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.lam fun x =>
      TDParse.Semantics.lam fun rel =>
        TDParse.Semantics.app rel x)

def possessiveStore : Expr (E ~> (E ~> E) ~> W^E E) :=
  lexWith "'s" (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.lam fun x =>
      TDParse.Semantics.lam fun rel =>
        TDParse.Semantics.storePair x (TDParse.Semantics.app rel x))

def push : Expr (E ~> W^E E) :=
  lexWith "push" (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.lam fun x => TDParse.Semantics.storePair x x)

def choose (name restrict : String) : Expr (S E) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.chooseIn (TDParse.Semantics.prim restrict))

def chooseStore (name restrict : String) : Expr (S (W^E E)) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.mapEff .spawn FX.spawn.functorOk
      (TDParse.Semantics.lam fun x => TDParse.Semantics.storePair x x)
      (TDParse.Semantics.chooseIn (TDParse.Semantics.prim restrict)))

def existsE (name restrict : String) : Expr (C^T T E) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.cont fun k =>
      TDParse.Semantics.existsIn (TDParse.Semantics.prim restrict) k)

def forallE (name restrict : String) : Expr (C^T T E) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.cont fun k =>
      TDParse.Semantics.forallIn (TDParse.Semantics.prim restrict) k)

def forallStore (name restrict : String) : Expr (C^T T (W^E E)) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.cont fun k =>
      TDParse.Semantics.forallIn (TDParse.Semantics.prim restrict) fun x =>
        k (TDParse.Semantics.storePair x x))

def someDet (name : String) : Expr ((E ~> T) ~> S E) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.lam fun restrict =>
      TDParse.Semantics.chooseIn restrict)

def everyDet (name : String) : Expr ((E ~> T) ~> C^T T E) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.lam fun restrict =>
      TDParse.Semantics.cont fun k => TDParse.Semantics.forallIn restrict k)

def everyPred (name : String) : Expr ((E ~> T) ~> (E ~> T) ~> T) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.lam fun restrict =>
      TDParse.Semantics.lam fun scope =>
        TDParse.Semantics.forallIn restrict fun x =>
          TDParse.Semantics.app scope x)

def andBool (name : String) : Expr (T ~> T ~> T) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.lam fun right =>
      TDParse.Semantics.lam fun left =>
        TDParse.Semantics.conj left right)

def firstEntity (name : String) : Expr (E ~> E ~> E) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.lam fun _right =>
      TDParse.Semantics.lam fun left => left)

def definite (name : String) : Expr (C^E T E) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.cont2 fun k => TDParse.Semantics.selectWhere k)

def everyCont (name : String) : Expr (C^(C^T T E) T E) :=
  lexWith name (fun {repr} [TDParse.Semantics repr] =>
    TDParse.Semantics.cont2 fun k =>
      TDParse.Semantics.cont fun restrict =>
        TDParse.Semantics.forallIn (TDParse.Semantics.lam restrict) k)

end Expr


def Expr.sem {ty : Ty} (e : Expr ty) : TDParse.SemTerm ty :=
  fun {repr} [TDParse.Semantics repr] =>
    match e with
    | lexeme l  => l.sem (repr := repr)
    | moc m x y =>
        m.recipe.sem (repr := repr) (x.sem (repr := repr)) (y.sem (repr := repr))

def Expr.den (e : Expr ty) : ty.dom :=
  TDParse.SemTerm.den (Expr.sem e)

def Expr.eval (m : TDParse.Model) (e : Expr ty) : ty.dom :=
  TDParse.SemTerm.eval m (Expr.sem e)

def Expr.pretty {ty : Ty} (e : Expr ty) : String :=
  TDParse.SemTerm.pretty (Expr.sem e)

-- convenience type synonyms (used for custom displays)
def TypedExpr := (t : Ty) × Expr t
def Exprs := List ((t : Ty) × Expr t)
def Interps (t : Ty) := List (Expr t × t.dom)


-- Lexicon
-- ------------------------------------------------------------------------

inductive HDict : List Ty -> Type 1
  | nil : HDict []
  | cons : (Cat × Expr t) → HDict ts → HDict (t::ts)

infixr:67 " :: " => HDict.cons

syntax (name := hdict) "{[" term,* "]}"  : term
macro_rules (kind := hdict)
  | `({[ ]})           => `(HDict.nil)
  | `({[ $a ]})        => `(HDict.cons $a HDict.nil)
  | `({[ $a, $as,* ]}) => `(HDict.cons $a {[$as,*]})

example: {[x,y,z]} = HDict.cons x (HDict.cons y (HDict.cons z HDict.nil)) := rfl

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

inductive Tree (c : Type) (a : Type): Type where
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
  | .ml f _ m => fun xs y =>
      match h : functor f with
      | some inst =>
          TDParse.Semantics.mapEff f inst
            (TDParse.Semantics.lam fun x => m.sem x y)
            xs
      | none => by cases f <;> simp [functor] at h
  | .mr f _ m => fun x ys =>
      match h : functor f with
      | some inst =>
          TDParse.Semantics.mapEff f inst
            (TDParse.Semantics.lam fun y => m.sem x y)
            ys
      | none => by cases f <;> simp [functor] at h
  | .ap f ok m => fun xs ys =>
      match h : applicative f with
      | some inst =>
          letI : Applicative f.dom := inst
          let instF := inferInstanceAs (Functor f.dom)
          TDParse.Semantics.apEff f inst
            (TDParse.Semantics.mapEff f instF
              (TDParse.Semantics.lam fun x => TDParse.Semantics.lam fun y => m.sem x y)
              xs)
            ys
      | none => by simp [h] at ok
  | .ul f ok m => fun x y =>
      match h : applicative f with
      | some inst =>
          m.sem x (TDParse.Semantics.lam fun b =>
            TDParse.Semantics.app y (TDParse.Semantics.pureEff f inst b))
      | none => by simp [h] at ok
  | .ur f ok m => fun x y =>
      match h : applicative f with
      | some inst =>
          m.sem (TDParse.Semantics.lam fun a =>
            TDParse.Semantics.app x (TDParse.Semantics.pureEff f inst a)) y
      | none => by simp [h] at ok
  | .cu f g ok m => fun xs ys =>
      match h : adjoint f g with
      | some ⟨instF, instG, adj⟩ =>
          TDParse.Semantics.counit f g instF instG adj
            (TDParse.Semantics.mapEff f instF
              (TDParse.Semantics.lam fun x =>
                TDParse.Semantics.mapEff g instG
                  (TDParse.Semantics.lam fun y => m.sem x y)
                  ys)
              xs)
      | none => by simp [h] at ok
  | .jn f ok m => fun x y =>
      match h : monad f with
      | some inst => TDParse.Semantics.joinEff f inst (m.sem x y)
      | none => by simp [h] at ok
  | .dn m => fun x y => TDParse.Semantics.lower (m.sem x y)
  | .scopeAp m => fun xs ys =>
      TDParse.Semantics.scopeMap2
        (TDParse.Semantics.lam fun x => TDParse.Semantics.lam fun y => m.sem x y)
        xs
        ys
  | .scopeJn m => fun x y => TDParse.Semantics.joinScope (m.sem x y)
  | .xl f ok m => fun xs y =>
      match h : comonad f with
      | some inst => TDParse.Semantics.extend f inst xs (fun xs' => m.sem xs' y)
      | none => by simp [h] at ok
  | .el _ m => fun x y => m.sem (TDParse.Semantics.eject x) y
  | .er _ m => fun x y => m.sem x (TDParse.Semantics.eject y)

structure Mode (a : Ty) (b : Ty) (c : Ty) where
  mode : ModeLabel
  op : a.dom → b.dom → c.dom
  recipe : ModeOp a b c

-- Parser-facing lexical storage.  This stays first-order so chart entries
-- remain small; the public constructors in `namespace Expr` below are the
-- tagless-final surface used by lexicons.
inductive LexOp : Ty -> Type where
  -- General lexical symbols
  | opaque (name : String) : LexOp t
  | bool (b : Bool) : LexOp T
  | nat (n : Nat) : LexOp E
  | entity (name : String) : LexOp E
  | prim1 (name : String) : LexOp (a ~> b)
  | prim2 (name : String) : LexOp (a ~> b ~> c)
  | prim3 (name : String) : LexOp (a ~> b ~> c ~> d)
  -- Effectful lexical recipes
  | storeEntity (name : String) : LexOp (W^E E)
  | storeBoolEntity (predName entityName : String) : LexOp (W^T E)
  | ask (name : String) : LexOp (.comp (.query a) a)
  | askStore (name : String) : LexOp (.comp (.query a) (.comp (.store a) a))
  | askStoreRel (name : String) : LexOp ((E ~> E) ~> R^E (W^E E))
  | askStoreRelFn (name : String) : LexOp ((E ~> E) ~> R^E (W^(R^E E) E))
  | poss (name : String) : LexOp (E ~> (E ~> E) ~> E)
  | possStore (name : String) : LexOp (E ~> (E ~> E) ~> W^E E)
  | andBool (name : String) : LexOp (T ~> T ~> T)
  | firstEntity (name : String) : LexOp (E ~> E ~> E)
  | push (name : String) : LexOp (E ~> W^E E)
  -- Finite-domain recipes used by toy numeric examples
  | listNat (name : String) (xs : List Nat) : LexOp (S E)
  | listStoreNat (name : String) (xs : List Nat) : LexOp (S (W^E E))
  | filterNat (name : String) (xs : List Nat) : LexOp ((E ~> T) ~> S E)
  | allNat (name : String) (xs : List Nat) : LexOp (C^T T E)
  | anyNat (name : String) (xs : List Nat) : LexOp (C^T T E)
  | allStoreNat (name : String) (xs : List Nat) : LexOp (C^T T (W^E E))
  | definiteNat (name : String) (xs : List Nat) : LexOp (C^E T E)
  | everyCont (name : String) (xs : List Nat) : LexOp (C^(C^T T E) T E)
  -- Abstract restricted quantification and choice
  | choosePred (name restrict : String) : LexOp (S E)
  | chooseStorePred (name restrict : String) : LexOp (S (W^E E))
  | allPred (name restrict : String) : LexOp (C^T T E)
  | anyPred (name restrict : String) : LexOp (C^T T E)
  | allStorePred (name restrict : String) : LexOp (C^T T (W^E E))
  | definite (name : String) : LexOp (C^E T E)
  | everyContPred (name : String) : LexOp (C^(C^T T E) T E)
  | everyDet (name : String) : LexOp ((E ~> T) ~> C^T T E)
  | everyPred (name : String) : LexOp ((E ~> T) ~> (E ~> T) ~> T)
  | someDet (name : String) : LexOp ((E ~> T) ~> S E)

def LexOp.sem {t : Ty} (op : LexOp t) : TDParse.SemTerm t :=
  fun {repr} [TDParse.Semantics repr] =>
    match op with
    | .opaque name => TDParse.Semantics.prim name
    | .bool b => TDParse.Semantics.bool b
    | .nat n => TDParse.Semantics.nat n
    | .entity name => TDParse.Semantics.prim name
    | .prim1 name =>
        let f := TDParse.Semantics.prim name
        TDParse.Semantics.lam fun x => TDParse.Semantics.app f x
    | .prim2 name =>
        let f := TDParse.Semantics.prim name
        TDParse.Semantics.lam fun x =>
          TDParse.Semantics.lam fun y =>
            TDParse.Semantics.app (TDParse.Semantics.app f x) y
    | .prim3 name =>
        let f := TDParse.Semantics.prim name
        TDParse.Semantics.lam fun x =>
          TDParse.Semantics.lam fun y =>
            TDParse.Semantics.lam fun z =>
              TDParse.Semantics.app (TDParse.Semantics.app (TDParse.Semantics.app f x) y) z
    | .storeEntity name =>
        TDParse.Semantics.storePair
          (TDParse.Semantics.prim name)
          (TDParse.Semantics.prim name)
    | .storeBoolEntity predName entityName =>
        TDParse.Semantics.storePair
          (TDParse.Semantics.app
            (TDParse.Semantics.prim (t := E ~> T) predName)
            (TDParse.Semantics.prim (t := E) entityName))
          (TDParse.Semantics.prim (t := E) entityName)
    | .ask _ => TDParse.Semantics.ask
    | .askStore _ =>
        let inst := inferInstanceAs (Functor (FX.query _).dom)
        TDParse.Semantics.mapEff (FX.query _) inst
          (TDParse.Semantics.lam fun x => TDParse.Semantics.storePair x x)
          TDParse.Semantics.ask
    | .askStoreRel _ =>
        TDParse.Semantics.lam fun rel =>
          let inst := inferInstanceAs (Functor (FX.query E).dom)
          TDParse.Semantics.mapEff (FX.query E) inst
            (TDParse.Semantics.lam fun x =>
              TDParse.Semantics.storePair x (TDParse.Semantics.app rel x))
            TDParse.Semantics.ask
    | .askStoreRelFn _ =>
        TDParse.Semantics.lam fun rel =>
          let inst := inferInstanceAs (Functor (FX.query E).dom)
          let relReader :=
            TDParse.Semantics.mapEff (FX.query E) inst rel TDParse.Semantics.ask
          TDParse.Semantics.mapEff (FX.query E) inst
            (TDParse.Semantics.lam fun x =>
              TDParse.Semantics.storePair relReader (TDParse.Semantics.app rel x))
            TDParse.Semantics.ask
    | .poss _ =>
        TDParse.Semantics.lam fun x =>
          TDParse.Semantics.lam fun rel =>
            TDParse.Semantics.app rel x
    | .possStore _ =>
        TDParse.Semantics.lam fun x =>
          TDParse.Semantics.lam fun rel =>
            TDParse.Semantics.storePair x (TDParse.Semantics.app rel x)
    | .andBool _ =>
        TDParse.Semantics.lam fun right =>
          TDParse.Semantics.lam fun left =>
            TDParse.Semantics.conj left right
    | .firstEntity _ =>
        TDParse.Semantics.lam fun _right =>
          TDParse.Semantics.lam fun left => left
    | .push _ =>
        TDParse.Semantics.lam fun x => TDParse.Semantics.storePair x x
    | .listNat _ xs =>
        TDParse.Semantics.listNat xs
    | .listStoreNat _ xs =>
        TDParse.Semantics.listStoreNat xs
    | .filterNat _ xs =>
        TDParse.Semantics.lam fun p => TDParse.Semantics.filterNat xs fun x =>
          TDParse.Semantics.app p x
    | .allNat _ xs =>
        TDParse.Semantics.cont fun k => TDParse.Semantics.forallNat xs k
    | .anyNat _ xs =>
        TDParse.Semantics.cont fun k => TDParse.Semantics.existsNat xs k
    | .allStoreNat _ xs =>
        TDParse.Semantics.cont fun k => TDParse.Semantics.forallStoreNat xs k
    | .definiteNat _ xs =>
        TDParse.Semantics.cont2 fun k => TDParse.Semantics.selectNat xs k
    | .everyCont _ xs =>
        TDParse.Semantics.cont2 fun k =>
          TDParse.Semantics.cont fun restrict =>
            TDParse.Semantics.forallNat xs fun x =>
              TDParse.Semantics.imp
                (restrict x)
                (k x)
    | .choosePred _ restrict =>
        TDParse.Semantics.chooseIn (TDParse.Semantics.prim restrict)
    | .chooseStorePred _ restrict =>
        TDParse.Semantics.chooseStoreIn (TDParse.Semantics.prim restrict)
    | .allPred _ restrict =>
        TDParse.Semantics.cont fun k =>
          TDParse.Semantics.forallIn (TDParse.Semantics.prim restrict) k
    | .anyPred _ restrict =>
        TDParse.Semantics.cont fun k =>
          TDParse.Semantics.existsIn (TDParse.Semantics.prim restrict) k
    | .allStorePred _ restrict =>
        TDParse.Semantics.cont fun k =>
          TDParse.Semantics.forallStoreIn (TDParse.Semantics.prim restrict) k
    | .definite _ =>
        TDParse.Semantics.cont2 fun k => TDParse.Semantics.selectWhere k
    | .everyContPred _ =>
        TDParse.Semantics.cont2 fun k =>
          TDParse.Semantics.cont fun restrict =>
            TDParse.Semantics.forallIn (TDParse.Semantics.lam restrict) k
    | .everyDet _ =>
        TDParse.Semantics.lam fun restrict =>
          TDParse.Semantics.cont fun k => TDParse.Semantics.forallIn restrict k
    | .everyPred _ =>
        TDParse.Semantics.lam fun restrict =>
          TDParse.Semantics.lam fun scope =>
            TDParse.Semantics.forallIn restrict fun x =>
              TDParse.Semantics.app scope x
    | .someDet _ =>
        TDParse.Semantics.lam fun restrict =>
          TDParse.Semantics.chooseIn restrict

structure Lexeme (t : Ty) where
  name : String
  recipe : LexOp t

def Lexeme.sem (l : Lexeme t) : TDParse.SemTerm t :=
  l.recipe.sem

inductive Expr : Ty → Type where
  | lexeme : Lexeme a → Expr a
  | moc : Mode a b c → Expr a → Expr b → Expr c

namespace Expr

-- Surface constructors for lexical entries.  These are meant to be used like
-- tagless-final lexical definitions: the lexicon names abstract meanings, and
-- concrete model values are supplied only by an interpreter.
def lex (name : String) : Expr a :=
  .lexeme ⟨name, .opaque name⟩

def lexWith (name : String) (recipe : LexOp a) : Expr a :=
  .lexeme ⟨name, recipe⟩

def litNat (name : String) (n : Nat) : Expr E :=
  lexWith name (.nat n)

def entity (name : String) : Expr E :=
  lexWith name (.entity name)

def fun1 (name : String) : Expr (a ~> b) :=
  lexWith name (.prim1 name)

def fun2 (name : String) : Expr (a ~> b ~> c) :=
  lexWith name (.prim2 name)

def fun3 (name : String) : Expr (a ~> b ~> c ~> d) :=
  lexWith name (.prim3 name)

def ask (name : String) : Expr (R^a a) :=
  lexWith name (.ask name)

def askStore (name : String) : Expr (R^a (W^a a)) :=
  lexWith name (.askStore name)

def askStoreRel (name : String) : Expr ((E ~> E) ~> R^E (W^E E)) :=
  lexWith name (.askStoreRel name)

def askStoreRelFn (name : String) : Expr ((E ~> E) ~> R^E (W^(R^E E) E)) :=
  lexWith name (.askStoreRelFn name)

def storeEntity (name : String) : Expr (W^E E) :=
  lexWith name (.storeEntity name)

def storeBoolEntity (name predName entityName : String) : Expr (W^T E) :=
  lexWith name (.storeBoolEntity predName entityName)

def possessive : Expr (E ~> (E ~> E) ~> E) :=
  lexWith "'s" (.poss "'s")

def possessiveStore : Expr (E ~> (E ~> E) ~> W^E E) :=
  lexWith "'s" (.possStore "'s")

def push : Expr (E ~> W^E E) :=
  lexWith "push" (.push "push")

def choose (name restrict : String) : Expr (S E) :=
  lexWith name (.choosePred name restrict)

def chooseStore (name restrict : String) : Expr (S (W^E E)) :=
  lexWith name (.chooseStorePred name restrict)

def existsE (name restrict : String) : Expr (C^T T E) :=
  lexWith name (.anyPred name restrict)

def forallE (name restrict : String) : Expr (C^T T E) :=
  lexWith name (.allPred name restrict)

def forallStore (name restrict : String) : Expr (C^T T (W^E E)) :=
  lexWith name (.allStorePred name restrict)

def someDet (name : String) : Expr ((E ~> T) ~> S E) :=
  lexWith name (.someDet name)

def everyDet (name : String) : Expr ((E ~> T) ~> C^T T E) :=
  lexWith name (.everyDet name)

def everyPred (name : String) : Expr ((E ~> T) ~> (E ~> T) ~> T) :=
  lexWith name (.everyPred name)

def andBool (name : String) : Expr (T ~> T ~> T) :=
  lexWith name (.andBool name)

def firstEntity (name : String) : Expr (E ~> E ~> E) :=
  lexWith name (.firstEntity name)

def definite (name : String) : Expr (C^E T E) :=
  lexWith name (.definite name)

def everyCont (name : String) : Expr (C^(C^T T E) T E) :=
  lexWith name (.everyContPred name)

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

-- x exceeds y
def exprTest (x y : Nat) : Expr T :=
  .moc ba sub (.moc fa exc obj)
  where sub : Expr E := .litNat "sub" x
        obj : Expr E := .litNat "obj" y
        exc : Expr (E~>E~>T) := .fun2 "exceeds"
        fa : Mode (E~>E~>T) E (E~>T) :=
          ⟨.FA, (·<|·), .fa⟩
        ba : Mode E (E~>T) T :=
          ⟨.BA, (·|>·), .ba⟩

#eval exprTest 5 2 |>.den
#eval exprTest 2 5 |>.den

-- convenience type synonyms (used for custom displays)
def TypedExpr := (t : Ty) × Expr t
def Exprs := List ((t : Ty) × Expr t)
def Interps (t : Ty) := List (Expr t × t.dom)


-- Lexicon
-- ------------------------------------------------------------------------

inductive HDict : List Ty -> Type
  | nil : HDict []
  | cons : (Cat × Expr t) → HDict ts → HDict (t::ts)

infixr:67 " :: " => HDict.cons

syntax (name := hdict) "{[" term,* "]}"  : term
macro_rules (kind := hdict)
  | `({[ ]})           => `(HDict.nil)
  | `({[ $a ]})        => `(HDict.cons $a HDict.nil)
  | `({[ $a, $as,* ]}) => `(HDict.cons $a {[$as,*]})

example: {[x,y,z]} = HDict.cons x (HDict.cons y (HDict.cons z HDict.nil)) := rfl

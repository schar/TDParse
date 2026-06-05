import TDParse.Data.Ty

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

structure Mode (α : Type) (β : Type) (γ : Type) where
  mode : ModeLabel
  op : α → β → γ

inductive Expr : Ty → Type where
  | lex : String → a.dom → Expr a
  | moc : Mode a.dom b.dom c.dom → Expr a → Expr b → Expr c

def Expr.den : Expr ty → ty.dom
  | lex _ x     => x
  | moc ⟨_,o⟩ x y => o (x.den) (y.den)

-- x exceeds y
def exprTest (x y : Nat) : Expr T :=
  .moc ⟨.BA, flip id⟩ sub (.moc ⟨.FA, id⟩ exc obj)
  where sub : Expr E := .lex "sub" x
        obj : Expr E := .lex "obj" y
        exc : Expr (E~>E~>T) := .lex "exceeds" (·<·)

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

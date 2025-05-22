import TDParse.Data.Ty
open Function -- provides 'curry'/'uncurry'


-- Syntactic derivations
-- ------------------------------------------------------------------------

inductive Cat : Type where
  | CP | Cmp -- Clauses and Complementizers
  | CBar | DBar | Cor -- Coordinators and Coordination Phrases
  | DP | Det | Gen | GenD | Dmp -- (Genitive) Determiners and full Determiner Phrases
  | NP | TN -- Transitive (relational) Nouns and full Noun Phrases
  | VP | TV | DV | AV -- Transitive, Ditransitive, and Attitude Verbs and Verb Phrases
  | AdjP | TAdj | Deg | AdvP | TAdv -- Modifiers
deriving Repr

abbrev CFG := Cat -> Cat -> List Cat

inductive Tree (c : Type) (a : Type): Type where
  | leaf : c -> a -> Tree c a
  | node : c -> Tree c a → Tree c a → Tree c a
deriving Repr

def Tree.root : (t : Tree c a) -> c
  | .leaf c _   => c
  | .node c _ _ => c


-- Semantic derivations
-- ------------------------------------------------------------------------

inductive ModeLabel : Type where
  | FA | BA | PM
  | MR (m : ModeLabel) | ML (m : ModeLabel)
  | AP (m : ModeLabel)
  | UR (m : ModeLabel) | UL (m : ModeLabel)
  | CU (m : ModeLabel)
  | JN (m : ModeLabel)
  | DN (m : ModeLabel)
deriving BEq, DecidableEq

-- for convenience, parse lists of mode labels as right-nested
section
open ModeLabel Lean
def ModeLabel.build : List Syntax → MacroM (TSyntax `term)
  | [] => Macro.throwError "Empty mode label sequence"
  | [m] => `($(mkIdent m.getId))
  | m :: ms => build ms >>= fun inner => `($(Lean.mkIdent m.getId) $inner)

syntax "m:" ident+ : term
macro_rules
  | `(m: $ids*) => build ids.toList

#eval match MR (AP FA) with
  | m:MR ML __ => true
  | m:MR MR __ => false
  | _ => true
end

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

def HDict.lookup (k : String) : HDict ts → Option (Cat × (t : Ty) × Expr t)
  | .nil                        => none
  | .cons (c, e@(.lex k' _)) xs => if k' = k then some ⟨c, _, e⟩ else xs.lookup k
  | .cons _ xs                  => xs.lookup k

-- not actually partial, but because it's not structurally inductive,
-- lean can't prove it terminates
partial def parse (cfg : CFG) (lex : HDict ts) : List String -> List (Tree Cat ((t : Ty) × Expr t))
  | [ ] => []
  | [w] => lex.lookup w <&> uncurry .leaf |>.toList
  | wds => do
      let (ls,rs) <- List.range' 1 (wds.length - 1) <&> wds.splitAt
      let lt <- parse cfg lex ls
      let rt <- parse cfg lex rs
      let nt <- cfg lt.root rt.root
      pure (.node nt lt rt)

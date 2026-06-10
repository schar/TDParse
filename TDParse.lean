import TDParse.Combine
import TDParse.Data.Derivation
import TDParse.Display
import TDParse.Memoize
import TDParse.Semantics

-- Dictionary lookup
-- ------------------------------------------------------------------------

def HDict.lookupAll (k : String) : HDict ts → List (Cat × (t : Ty) × Expr t)
  | .nil                        => []
  | .cons (c, e@(.lexeme l)) xs =>
      let rest := xs.lookupAll k
      if l.name = k then ⟨c, _, e⟩ :: rest else rest
  | .cons _ xs                  => xs.lookupAll k

def HDict.lookup (k : String) (d : HDict ts) := d.lookupAll k |>.head?


-- Syntactic parser from strings to uninterpreted trees
-- ------------------------------------------------------------------------

abbrev Parser := List String -> List (Tree Cat TypedExpr)
abbrev ParseResult := List (Tree Cat TypedExpr)
abbrev ParseChart := MemoM ParseResult

-- Split a token on possessive clitics: "john's" -> ["john", "'s"]
def stripClitics (w : String) : List String :=
  let clitics := ["'s"]
  match clitics.find? (fun c => w.endsWith c) with
  | some c => [w.dropRight c.length, c]
  | none   => [w]

def parse (cfg : CFG) (lex : HDict ts) : Parser :=
  fun wds =>
    let toks := (wds.flatMap stripClitics |>.filter (fun w => w != "")).toArray
    let width := toks.size + 1
    let parseSpan : Nat -> Nat -> ParseChart ParseResult := memoFix2State width (go toks)
    (parseSpan 0 toks.size (MemoCache.empty width width)).1
  where
    go (toks : Array String) (parse : Nat -> Nat -> ParseChart ParseResult)
        (lo hi : Nat) : ParseChart ParseResult :=
    if hi <= lo then pure []
    else if hi = lo + 1 then
      match toks[lo]? with
      | some w => pure (lex.lookupAll w <&> fun ⟨c, te⟩ => Tree.leaf c te)
      | none   => pure []
    else
      let rec collect : List Nat -> ParseChart ParseResult
        | [] => pure []
        | mid :: mids => do
            let lts ← parse lo mid
            let rts ← parse mid hi
            let here := do
              let lt ← lts
              let rt ← rts
              let nt ← (cfg lt.root rt.root).map ULift.up
              let nt := nt.down
              -- CP nodes are scope islands: quantifiers cannot scope out of them
              let mk := if nt == Cat.CP then Tree.island else Tree.node
              pure (mk nt lt rt)
            let rest ← collect mids
            pure (here ++ rest)
      collect (List.range' (lo + 1) (hi - lo - 1))

-- Semantic parser from trees to combinatoric expressions
-- ------------------------------------------------------------------------

def synsem : Tree c TypedExpr -> List TypedExpr
  | .leaf _ ⟨u,e⟩ => [⟨u, e⟩]
  | .node _ l r => do
      let ⟨lt,le⟩ ← synsem l
      let ⟨rt,re⟩ ← synsem r
      let ⟨wt,md⟩ ← (combine lt rt).map ULift.up
      pure ⟨wt, .moc md le re⟩
  | .island _ l r => do
      let ⟨lt,le⟩ ← synsem l
      let ⟨rt,re⟩ ← synsem r
      let ⟨wt,md⟩ ← (combine lt rt).map ULift.up
      -- Only survive if no unresolved scope effects.
      if wt.evaluated then pure ⟨wt, .moc md le re⟩ else []


-- Interpret a combinatoric expression
-- ------------------------------------------------------------------------

def run : TypedExpr -> ((t : Ty) × Expr t × t.dom)
  | ⟨t,e⟩ => ⟨t, e, e.den⟩

def runIn (m : TDParse.Model) : TypedExpr -> ((t : Ty) × Expr t × t.dom)
  | ⟨t,e⟩ => ⟨t, e, e.eval m⟩

def runPretty : TypedExpr -> ((t : Ty) × Expr t × String)
  | ⟨t,e⟩ => ⟨t, e, e.pretty⟩

def runAs : (s : Ty) -> TypedExpr -> Option (Expr s × s.dom)
  | s, ⟨t,e⟩ => if h : t = s then by subst h; exact some (e, e.den) else none

def runAsIn (m : TDParse.Model) : (s : Ty) -> TypedExpr -> Option (Expr s × s.dom)
  | s, ⟨t,e⟩ => if h : t = s then by subst h; exact some (e, e.eval m) else none

def runPrettyAs : (s : Ty) -> TypedExpr -> Option (Expr s × String)
  | s, ⟨t,e⟩ => if h : t = s then by subst h; exact some (e, e.pretty) else none

import TDParse.Combine
import TDParse.Data.Derivation
import TDParse.Display
import TDParse.Memoize

-- Dictionary lookup
-- ------------------------------------------------------------------------

def HDict.lookupAll (k : String) : HDict ts → List (Cat × (t : Ty) × Expr t)
  | .nil                        => []
  | .cons (c, e@(.lex k' _)) xs =>
      let rest := xs.lookupAll k
      if k' = k then ⟨c, _, e⟩ :: rest else rest
  | .cons _ xs                  => xs.lookupAll k

def HDict.lookup (k : String) (d : HDict ts) := d.lookupAll k |>.head?


-- Syntactic parser from strings to uninterpreted trees
-- ------------------------------------------------------------------------

abbrev Parser := List String -> List (Tree Cat TypedExpr)

-- Split a token on possessive clitics: "john's" -> ["john", "'s"]
def stripClitics (w : String) : List String :=
  let clitics := ["'s"]
  match clitics.find? (fun c => w.endsWith c) with
  | some c => [w.dropRight c.length, c]
  | none   => [w]

def parse (cfg : CFG) (lex : HDict ts) : Parser :=
  let parseRaw : Parser := memoFix go
  fun wds => parseRaw (wds.flatMap stripClitics)
  where go parse
  | [ ] => []
  | [w] => lex.lookupAll w <&> Function.uncurry .leaf
  | wds => do
      let (ls,rs) <- List.range' 1 (wds.length - 1) <&> wds.splitAt
      let lt <- parse ls
      let rt <- parse rs
      let nt <- cfg lt.root rt.root
      -- CP nodes are scope islands: quantifiers cannot scope out of them
      let mk := if nt == Cat.CP then Tree.island else Tree.node
      pure (mk nt lt rt)

open Cat Expr
#eval
  let cfg | DP, VP => [CP]
          | TV, DP => [VP]
          | Det,NP => [DP]
          | _  ,_  => [  ]
  let lex :=
    {[ (Det, @lex T "this" true),
       (NP , @lex T "string" true),
       (TV , @lex T "has" true),
       (Det, @lex T "five" true),
       (NP , @lex T "letters" true) ]}
  parse cfg lex "this string has five letters".splitOn


-- Semantic parser from trees to combinatoric expressions
-- ------------------------------------------------------------------------

def synsem : Tree c TypedExpr -> List TypedExpr
  | .leaf _ ⟨u,e⟩ => [⟨u, e⟩]
  | .node _ l r => do
      let ⟨lt,le⟩ <- synsem l
      let ⟨rt,re⟩ <- synsem r
      let ⟨wt,md⟩ <- combine lt rt
      pure ⟨wt, .moc md le re⟩
  | .island _ l r => do
      let ⟨lt,le⟩ <- synsem l
      let ⟨rt,re⟩ <- synsem r
      let ⟨wt,md⟩ <- combine lt rt
      guard wt.evaluated   -- only survive if no unresolved scope effects
      pure ⟨wt, .moc md le re⟩


-- Interpret a combinatoric expression
-- ------------------------------------------------------------------------

def run : TypedExpr -> ((t : Ty) × Expr t × t.dom)
  | ⟨t,e⟩ => ⟨t, e, e.den⟩

def runAs : (s : Ty) -> TypedExpr -> Option (Expr s × s.dom)
  | s, ⟨t,e⟩ => if h : t = s then by subst h; exact some (e, e.den) else none

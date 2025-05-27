import TDParse.Combine
import TDParse.Data.Derivation
import TDParse.Display
import TDParse.Memoize

-- Dictionary lookup
-- ------------------------------------------------------------------------

def HDict.lookup (k : String) : HDict ts → Option (Cat × (t : Ty) × Expr t)
  | .nil                        => none
  | .cons (c, e@(.lex k' _)) xs => if k' = k then some ⟨c, _, e⟩ else xs.lookup k
  | .cons _ xs                  => xs.lookup k


-- Syntactic parser from strings to uninterpreted trees
-- ------------------------------------------------------------------------

abbrev Parser := List String -> List (Tree Cat TypedExpr)

def parse (cfg : CFG) (lex : HDict ts) : Parser := memoFix go
  where go parse
  | [ ] => []
  | [w] => lex.lookup w <&> Function.uncurry .leaf |>.toList
  | wds => do
      let (ls,rs) <- List.range' 1 (wds.length - 1) <&> wds.splitAt
      let lt <- parse ls
      let rt <- parse rs
      let nt <- cfg lt.root rt.root
      pure (.node nt lt rt)

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


-- Interpret a combinatoric expression
-- ------------------------------------------------------------------------

def run : TypedExpr -> ((t : Ty) × Expr t × t.dom)
  | ⟨t,e⟩ => ⟨t, e, e.den⟩

def runAs : (s : Ty) -> TypedExpr -> Option (Expr s × s.dom)
  | s, ⟨t,e⟩ => if h : t = s then by subst h; exact some (e, e.den) else none

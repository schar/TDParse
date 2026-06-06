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

partial def parse (cfg : CFG) (lex : HDict ts) : Parser :=
  fun wds =>
    let toks := (wds.flatMap stripClitics).toArray
    let cache : Std.HashMap (Nat × Nat) (List (Tree Cat TypedExpr)) := ∅
    (go toks 0 toks.size cache).1
  where
    go (toks : Array String) (lo hi : Nat)
        (cache : Std.HashMap (Nat × Nat) (List (Tree Cat TypedExpr))) :
        List (Tree Cat TypedExpr) × Std.HashMap (Nat × Nat) (List (Tree Cat TypedExpr)) :=
      match cache[(lo, hi)]? with
      | some result => (result, cache)
      | none =>
          let (result, cache) :=
            if hi <= lo then ([], cache)
            else if hi = lo + 1 then
              let result :=
                match toks[lo]? with
                | some w => lex.lookupAll w <&> Function.uncurry .leaf
                | none   => []
              (result, cache)
            else
              Id.run do
                let mut result := []
                let mut cache := cache
                for mid in List.range' (lo + 1) (hi - lo - 1) do
                  let (lts, cache') := go toks lo mid cache
                  cache := cache'
                  let (rts, cache') := go toks mid hi cache
                  cache := cache'
                  for lt in lts do
                    for rt in rts do
                      for nt in cfg lt.root rt.root do
                        -- CP nodes are scope islands: quantifiers cannot scope out of them
                        let mk := if nt == Cat.CP then Tree.island else Tree.node
                        result := mk nt lt rt :: result
                (result.reverse, cache)
          (result, cache.insert (lo, hi) result)

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

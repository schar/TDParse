import TDParse
import TDParse.Display

open Expr
open Cat

-- Lexicon
-- ------------------------------------------------------------------------

def one : Expr E := lex "one" 1
def two : Expr E := lex "two" 2
def pro : Expr (R^E E) := lex "pro" id
def its : Expr (R^E E) := lex "its" id
def who : Expr (S E) := lex "whichnum" (.range' 1 5)
def nxt : Expr (E ~> E) := lex "successor" (·+1)
def prv : Expr (E ~> E) := lex "predecessor" (·-1)
def exc : Expr (E ~> E ~> T) := lex "exceeds" (·<·)
def scc : Expr (E ~> E ~> T) := lex "succeeds" (fun x y => y = x + 1)
def evn : Expr (C^T E) := lex "everyprime" ([2,3,5].all ·)
def smn : Expr (C^T E) := lex "somenum" (who.den.any ·)
def psh : Expr (E ~> W^E E) := lex "push" (fun x => (x,x))

def myLex :=
{[
  (DP  , one),
  (DP  , two),
  (DP  , pro),
  (DP  , who),
  (DP  , evn),
  (DP  , smn),
  (TN  , nxt),
  (TN  , prv),
  (Gen , its),
  (TV  , scc),
  (TV  , exc),
  (Dmp , psh)
]}

#eval myLex.lookup "one"
#eval myLex.lookup "exceeds"
#eval myLex.lookup "successor"


-- Grammar
-- ------------------------------------------------------------------------

def myCFG : CFG
  | DP   , VP    => [CP]
  | Cmp  , CP    => [CP]
  | Cor  , CP    => [CBar]
  | Cor  , DP    => [DBar]
  | DP   , DBar  => [DP]
  | Dmp  , DP    => [DP]
  | CP   , CBar  => [CP]
  | Det  , NP    => [DP]
  | Gen  , TN    => [DP]
  | DP   , GenD  => [Gen]
  | AdjP , NP    => [NP]
  | NP   , AdjP  => [NP]
  | TAdj , DP    => [AdjP]
  | Deg  , AdjP  => [AdjP]
  | TV   , DP    => [VP]
  | AV   , CP    => [VP]
  | DV   , DP    => [TV]
  | VP   , AdvP  => [VP]
  | TAdv , DP    => [AdvP]
  | _    , _     => []

def tree0 :=
  synsem (.node CP (.leaf DP ⟨_,two⟩) (.node VP (.leaf TV ⟨_,exc⟩) (.leaf DP ⟨_,one⟩)))

#eval tree0


-- Example derivations
-- ------------------------------------------------------------------------

def derive (u : String) : Exprs := parse myCFG myLex u.splitOn >>= synsem
def interpret (u : String) := derive u <&> run -- can't display this hlist (yet)
def interpretAs (t : Ty) (u : String) : Interps t := derive u |>.filterMap (runAs t)

#eval derive "two exceeds one"
#eval interpretAs T "two exceeds one"

#eval derive "whichnum exceeds two"
#eval interpretAs (S T) "whichnum exceeds two"
#eval derive "whichnum exceeds whichnum"
#eval interpretAs (S T) "whichnum exceeds whichnum"

#eval derive "everyprime exceeds pro"
#eval interpretAs (R^E T) "everyprime exceeds pro" >>= λ(e,v) => [(e, v 1), (e, v 3)]

#eval derive "push two exceeds one"
#eval interpretAs (W^E T) "push two exceeds one"

#eval derive "push two exceeds its predecessor"
#eval interpretAs T "push two exceeds its predecessor"
#eval derive "push two exceeds its successor"
#eval interpretAs T "push two exceeds its successor"

#eval derive "everyprime succeeds somenum"
#eval interpretAs T "everyprime succeeds somenum"

#eval derive "push everyprime succeeds its predecessor"
#eval interpretAs T "push everyprime succeeds its predecessor"


def main : IO Unit :=
  return ()

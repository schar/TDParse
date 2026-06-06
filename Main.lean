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
def evn : Expr (C^T T E) := lex "everyprime" ([2,3,5].all ·)
def smn : Expr (C^T T E) := lex "somenum" (who.den.any ·)
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

def ann : Expr E := lex "ann" 11
def maryW : Expr (W^E E) := lex "mary" (12, 12)
def marianne : Expr E := lex "marianne" 13
def marianneW : Expr (W^E E) := lex "marianne" (13, 13)
def poss : Expr (E ~> (E ~> E) ~> E) := lex "'s" (fun x rel => rel x)
def possW : Expr (E ~> (E ~> E) ~> W^E E) := lex "'s" (fun x rel => (x, rel x))
def leftV : Expr (E ~> T) := lex "left" (fun x => x % 2 == 1)
def whistled : Expr (E ~> T) := lex "whistled" (fun x => x % 3 == 1)
def sawV : Expr (E ~> E ~> T) := lex "saw" (fun x y => x != y)
def savedV : Expr (E ~> E ~> T) := lex "saved" (fun x y => x <= y)
def spentV : Expr (E ~> E ~> T) := lex "spent" (fun x y => x >= y)
def chasedV : Expr (E ~> E ~> T) := lex "chased" (fun x y => x + 1 == y)
def saidV : Expr (T ~> E ~> T) := lex "said" (fun p _ => p)
def gaveV : Expr (E ~> E ~> E ~> T) := lex "gave" (fun x y z => x + y == z)
def she : Expr (R^E E) := lex "she" id
def it1 : Expr (R^E E) := lex "it" id
def it2 : Expr (R^(R^E E) (R^E E)) := lex "it" id
def herGen : Expr (R^E E) := lex "her" id
def herDP : Expr (R^E E) := lex "her" id
def she2 : Expr (R^E (W^E E)) := lex "she2" (fun x => (x, x))
def her2Gen : Expr (R^E (W^E E)) := lex "her2" (fun x => (x, x))
def her2DP : Expr (R^E (W^E E)) := lex "her2" (fun x => (x, x))
def her2GenRelW : Expr ((E ~> E) ~> R^E (W^E E)) := lex "her2" (fun rel x => (x, rel x))
def her2GenRelRW : Expr ((E ~> E) ~> R^E (W^(R^E E) E)) := lex "her2" (fun rel x => (rel, rel x))
def mom : Expr (E ~> E) := lex "mom" (fun x => x + 100)
def paycheck : Expr (E ~> E) := lex "paycheck" (fun x => x + 1000)
def theDet : Expr ((E ~> T) ~> E) := lex "the" (fun p => if p 0 then 0 else 1)
def theC : Expr (C^E T E) := lex "theC" (fun k => if k 0 then 0 else 1)
def very : Expr ((E ~> T) ~> E ~> T) := lex "very" (fun adj x => adj x)
def everyDet : Expr ((E ~> T) ~> C^T T E) := lex "every" (fun restrict k => [0,1,2,3].filter restrict |>.all k)
def everyP : Expr ((E ~> T) ~> (E ~> T) ~> T) := lex "everyP" (fun restrict scope => [0,1,2,3].filter restrict |>.all scope)
def everyC : Expr (C^(C^T T E) T E) := lex "everyC" (fun k restrict => [0,1,2,3].filter restrict |>.all k)
def dog : Expr (E ~> T) := lex "dog" (fun x => x % 2 == 0)
def catN : Expr (E ~> T) := lex "cat" (fun x => x % 2 == 1)
def big : Expr (E ~> T) := lex "big" (fun x => x > 10)
def happy : Expr (E ~> T) := lex "happy" (fun x => x < 20)
def near : Expr (E ~> E ~> T) := lex "near" (fun x y => x == y || x + 1 == y)
def someDet : Expr ((E ~> T) ~> S E) := lex "some" (fun p => [0,1,2,3].filter p)
def someoneC : Expr (C^T T E) := lex "someone" ([0,1,2,3].any ·)
def someone2 : Expr (S (W^E E)) := lex "someone2" [(20,20),(21,21)]
def someone3 : Expr (S E) := lex "someone3" [0,1,2,3]
def everyone : Expr (C^T T E) := lex "everyone" ([0,1,2,3].all ·)
def everyone2 : Expr (C^T T (W^E E)) := lex "everyone2" (fun k => [(0,0),(1,1),(2,2),(3,3)].all k)
def tr : Expr (R^E E) := lex "tr" id
def andC : Expr (T ~> T ~> T) := lex "and" (fun p q => p && q)
def butC : Expr (T ~> T ~> T) := lex "but" (fun p q => p && q)
def andE : Expr (E ~> E ~> E) := lex "andE" (fun x _ => x)
def withAdv : Expr (E ~> (E ~> T) ~> E ~> T) := lex "with" (fun y p x => p x && (x == y || x + 1 == y))
def eclo : Expr (S T ~> T) := lex "eclo" (fun xs => xs.any id)
def maryaling : Expr (W^T E) := lex "maryaling" (true, 12)
def sassyacat : Expr (W^T E) := lex "sassyacat" (true, 14)

def demoLex :=
{[
  (DP  , ann),
  (DP  , maryW),
  (DP  , marianne),
  (DP  , marianneW),
  (GenD, poss),
  (GenD, possW),
  (VP  , leftV),
  (VP  , whistled),
  (TV  , sawV),
  (TV  , savedV),
  (TV  , spentV),
  (TV  , chasedV),
  (AV  , saidV),
  (DV  , gaveV),
  (DP  , she),
  (DP  , it1),
  (DP  , it2),
  (Gen , herGen),
  (DP  , herDP),
  (DP  , she2),
  (Gen , her2Gen),
  (DP  , her2DP),
  (Gen , her2GenRelW),
  (Gen , her2GenRelRW),
  (TN  , mom),
  (TN  , paycheck),
  (Det , theDet),
  (Det , theC),
  (Deg , very),
  (Det , everyDet),
  (Det , everyP),
  (Det , everyC),
  (NP  , dog),
  (NP  , catN),
  (AdjP, big),
  (AdjP, happy),
  (TAdj, near),
  (Det , someDet),
  (DP  , someoneC),
  (DP  , someone2),
  (DP  , someone3),
  (DP  , everyone),
  (DP  , everyone2),
  (DP  , tr),
  (Cor , andC),
  (Cor , butC),
  (Cor , andE),
  (TAdv, withAdv),
  (Cmp , eclo),
  (Dmp , eclo),
  (DP  , maryaling),
  (DP  , sassyacat)
]}

def d1 := "the very big cat left"
def d2 := "she saw her mom"
def d3 := "ann's mom saw her"
def d4 := "someone left and she2 whistled"
def d5 := "the cat near someone2 saw her"
def d6 := "marianne saved her2 paycheck but marianne's mom spent it"

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

def demoCFG := myCFG

def tree0 :=
  synsem (.node CP (.leaf DP ⟨_,two⟩) (.node VP (.leaf TV ⟨_,exc⟩) (.leaf DP ⟨_,one⟩)))

#eval tree0


-- Example derivations
-- ------------------------------------------------------------------------

def derive (u : String) : Exprs := parse myCFG myLex u.splitOn >>= synsem
def interpret (u : String) := derive u <&> run -- can't display this hlist (yet)
def interpretAs (t : Ty) (u : String) : Interps t := derive u |>.filterMap (runAs t)

def demoDerive (u : String) : Exprs := parse demoCFG demoLex u.splitOn >>= synsem
def demoInterpretAs (t : Ty) (u : String) : Interps t := demoDerive u |>.filterMap (runAs t)

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

#eval parse demoCFG demoLex d1.splitOn
#eval demoDerive d1
#eval demoDerive d2
#eval demoDerive d3
#eval demoDerive d4
#eval demoDerive d5
#eval (demoDerive d6).length


def main : IO Unit :=
  return ()

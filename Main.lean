import TDParse
import TDParse.Display

open Expr
open Cat

-- Lexicon
-- ------------------------------------------------------------------------

def one : Expr E := litNat "one" 1
def two : Expr E := litNat "two" 2
def pro : Expr (R^E E) := Expr.ask "pro"
def its : Expr (R^E E) := Expr.ask "its"
def who : Expr (S E) := choose "whichnum" "number"
def nxt : Expr (E ~> E) := fun1 "successor"
def prv : Expr (E ~> E) := fun1 "predecessor"
def exc : Expr (E ~> E ~> T) := fun2 "exceeds"
def scc : Expr (E ~> E ~> T) := fun2 "succeeds"
def evn : Expr (C^T T E) := Expr.forallE "everyprime" "prime"
def smn : Expr (C^T T E) := Expr.existsE "somenum" "number"
def psh : Expr (E ~> W^E E) := push

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

def ann : Expr E := entity "ann"
def maryW : Expr (W^E E) := storeEntity "mary"
def marianne : Expr E := entity "marianne"
def marianneW : Expr (W^E E) := storeEntity "marianne"
def poss : Expr (E ~> (E ~> E) ~> E) := possessive
def possW : Expr (E ~> (E ~> E) ~> W^E E) := possessiveStore
def leftV : Expr (E ~> T) := fun1 "left"
def whistled : Expr (E ~> T) := fun1 "whistled"
def sawV : Expr (E ~> E ~> T) := fun2 "saw"
def savedV : Expr (E ~> E ~> T) := fun2 "saved"
def spentV : Expr (E ~> E ~> T) := fun2 "spent"
def chasedV : Expr (E ~> E ~> T) := fun2 "chased"
def saidV : Expr (T ~> E ~> T) := fun2 "said"
def gaveV : Expr (E ~> E ~> E ~> T) := fun3 "gave"
def she : Expr (R^E E) := Expr.ask "she"
def it1 : Expr (R^E E) := Expr.ask "it"
def it2 : Expr (R^(R^E E) (R^E E)) := Expr.ask "it"
def herGen : Expr (R^E E) := Expr.ask "her"
def herDP : Expr (R^E E) := Expr.ask "her"
def she2 : Expr (R^E (W^E E)) := Expr.askStore "she2"
def her2Gen : Expr (R^E (W^E E)) := Expr.askStore "her2"
def her2DP : Expr (R^E (W^E E)) := Expr.askStore "her2"
def her2GenRelW : Expr ((E ~> E) ~> R^E (W^E E)) := Expr.askStoreRel "her2"
def her2GenRelRW : Expr ((E ~> E) ~> R^E (W^(R^E E) E)) := Expr.askStoreRelFn "her2"
def mom : Expr (E ~> E) := fun1 "mom"
def paycheck : Expr (E ~> E) := fun1 "paycheck"
def theDet : Expr ((E ~> T) ~> E) := fun1 "the"
def theC : Expr (C^E T E) := definite "theC"
def very : Expr ((E ~> T) ~> E ~> T) := fun2 "very"
def everyDet : Expr ((E ~> T) ~> C^T T E) := Expr.everyDet "every"
def everyP : Expr ((E ~> T) ~> (E ~> T) ~> T) := Expr.everyPred "everyP"
def everyC : Expr (C^(C^T T E) T E) := Expr.everyCont "everyC"
def dog : Expr (E ~> T) := fun1 "dog"
def catN : Expr (E ~> T) := fun1 "cat"
def big : Expr (E ~> T) := fun1 "big"
def happy : Expr (E ~> T) := fun1 "happy"
def near : Expr (E ~> E ~> T) := fun2 "near"
def someDet : Expr ((E ~> T) ~> S E) := Expr.someDet "some"
def someoneC : Expr (C^T T E) := Expr.existsE "someone" "person"
def someone2 : Expr (S (W^E E)) := chooseStore "someone2" "person"
def someone3 : Expr (S E) := choose "someone3" "person"
def everyone : Expr (C^T T E) := Expr.forallE "everyone" "person"
def everyone2 : Expr (C^T T (W^E E)) := forallStore "everyone2" "person"
def tr : Expr (R^E E) := Expr.ask "tr"
def andC : Expr (T ~> T ~> T) := andBool "and"
def butC : Expr (T ~> T ~> T) := andBool "but"
def andE : Expr (E ~> E ~> E) := firstEntity "andE"
def withAdv : Expr (E ~> (E ~> T) ~> E ~> T) := fun3 "with"
def eclo : Expr (S T ~> T) := fun1 "eclo"
def maryaling : Expr (W^T E) := storeBoolEntity "maryaling"
def sassyacat : Expr (W^T E) := storeBoolEntity "sassyacat"

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

def numberDerive (u : String) : Exprs := parse myCFG myLex u.splitOn >>= synsem
def numberInterpret (u : String) := numberDerive u <&> runIn TDParse.numberModel -- can't display this hlist (yet)
def numberInterpretAs (t : Ty) (u : String) : Interps t :=
  numberDerive u |>.filterMap (runAsIn TDParse.numberModel t)

def englishDerive (u : String) : Exprs := parse demoCFG demoLex u.splitOn >>= synsem
def englishInterpretAs (t : Ty) (u : String) : Interps t :=
  englishDerive u |>.filterMap (runAsIn TDParse.englishModel t)

def numberPrettyDerive (u : String) : List String :=
  numberDerive u |>.map (fun ⟨_, e⟩ => e.pretty)

def numberPrettyDeriveAs (t : Ty) (u : String) : List String :=
  numberDerive u |>.filterMap (runPrettyAs t) |>.map (fun (_, s) => s)

def englishPrettyDerive (u : String) : List String :=
  englishDerive u |>.map (fun ⟨_, e⟩ => e.pretty)

def englishPrettyDeriveAs (t : Ty) (u : String) : List String :=
  englishDerive u |>.filterMap (runPrettyAs t) |>.map (fun (_, s) => s)

#eval numberPrettyDeriveAs T "two exceeds one"
#eval numberPrettyDeriveAs (S T) "whichnum exceeds two"
#eval numberPrettyDeriveAs (S T) "whichnum exceeds whichnum"
#eval numberPrettyDeriveAs (R^E T) "everyprime exceeds pro"
#eval numberPrettyDeriveAs (W^E T) "push two exceeds one"
#eval numberPrettyDeriveAs T "push two exceeds its predecessor"
#eval numberPrettyDeriveAs T "push two exceeds its successor"
#eval numberPrettyDeriveAs T "everyprime succeeds somenum"
#eval numberPrettyDeriveAs T "push everyprime succeeds its predecessor"

#eval englishPrettyDeriveAs T "the very big cat left"
#eval englishPrettyDeriveAs (R^E T) "she saw her mom"
#eval englishPrettyDeriveAs T "marianne's mom saw her"
#eval englishPrettyDeriveAs (S T) "someone2 left and she whistled"
#eval englishPrettyDeriveAs (S T) "the cat near someone2 saw her"
#eval (englishPrettyDeriveAs T "marianne saved her2 paycheck but marianne's mom spent it").take 10
#eval (englishDerive "marianne saved her2 paycheck but marianne's mom spent it").length


def main : IO Unit :=
  return ()

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
def someoneS : Expr (S E) := choose "someoneS" "person"
def everyone : Expr (C^T T E) := Expr.forallE "everyone" "person"
def tr : Expr (R^E E) := Expr.ask "tr"
def andC : Expr (T ~> T ~> T) := andBool "and"
def butC : Expr (T ~> T ~> T) := andBool "but"
def andE : Expr (E ~> E ~> E) := firstEntity "andE"
def withAdv : Expr (E ~> (E ~> T) ~> E ~> T) := fun3 "with"
def eclo : Expr (S T ~> T) := fun1 "eclo"
def maryaling : Expr (W^T E) := storeBoolEntity "maryaling" "ling" "mary"
def sassyacat : Expr (W^T E) := storeBoolEntity "sassyacat" "cat" "sassy"

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
  (DP  , someoneS),
  (DP  , everyone),
  (DP  , tr),
  (Cor , andC),
  (Cor , butC),
  (Cor , andE),
  (TAdv, withAdv),
  (Cmp , eclo),
  (Dmp , eclo),
  (Dmp , psh),
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

-- Library smoke checks, kept here so library builds stay quiet.
#eval (S ((S (E ~> T)) ~> (S E)) ~> T)

#eval List.map (fun ⟨w, m⟩ => (w, m.mode)) (prims (E ~> T) E)
#eval combine (S (E ~> T)) (S E) <&> fun ⟨w, m⟩ => (w, m.mode)
#eval combine (E ~> S T) (S E) <&> fun ⟨w, m⟩ => (w, m.mode)

#eval
  let cfg | DP, VP => [CP]
          | TV, DP => [VP]
          | Det,NP => [DP]
          | _  ,_  => []
  let lex :=
    {[ (Det, @Expr.lex T "this"),
       (NP , @Expr.lex T "string"),
       (TV , @Expr.lex T "has"),
       (Det, @Expr.lex T "five"),
       (NP , @Expr.lex T "letters") ]}
  parse cfg lex "this string has five letters".splitOn

#eval
  let exprTest (x y : Nat) : Expr T :=
    .moc Mode.ba (.litNat "sub" x)
      (.moc Mode.fa (.fun2 "exceeds") (.litNat "obj" y))
  [exprTest 5 2 |>.den, exprTest 2 5 |>.den]


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

-- Interactive examples not otherwise covered by regression guards.
#eval numberPrettyDeriveAs (S T) "whichnum exceeds two"
#eval numberPrettyDeriveAs (R^E T) "everyprime exceeds pro"
#eval numberPrettyDeriveAs (W^E T) "push two exceeds one"
#eval numberPrettyDeriveAs T "push two exceeds its predecessor"
#eval numberPrettyDeriveAs T "push two exceeds its successor"
#eval numberPrettyDeriveAs T "push everyprime succeeds its predecessor"

#eval englishPrettyDeriveAs T "the very big cat left"
#eval englishPrettyDeriveAs (R^E T) "she saw her mom"
#eval englishPrettyDeriveAs T "marianne's mom saw her"
#eval englishPrettyDeriveAs (W^T T) "maryaling saw sassyacat"

-- Pretty-output regression checks for the typed NBE renderer.
#guard numberPrettyDeriveAs T "two exceeds one" ==
  ["exceeds 1 2"]

#guard numberPrettyDeriveAs (S T) "whichnum exceeds whichnum" ==
  ["[exceeds x1 x0 | number x0, number x1]"]

#guard numberPrettyDeriveAs T "everyprime succeeds somenum" ==
  [ "∃x0[number x0]. ∀x1[prime x1]. succeeds x0 x1"
  , "∀x0[prime x0]. ∃x1[number x1]. succeeds x1 x0"
  ]

#guard englishPrettyDeriveAs (S T) "push someoneS left and she whistled" ==
  ["[left x0 ∧ whistled x0 | person x0]"]

#guard englishPrettyDeriveAs T "eclo push someoneS left and she whistled" ==
  ["∃x0[person x0]. left x0 ∧ whistled x0"]

#guard englishPrettyDeriveAs T "eclo someoneS left and someoneS left and someoneS whistled" ==
  [ "∃x0[person x0]. ∃x1[person x1]. ∃x2[person x2]. left x0 ∧ left x1 ∧ whistled x2"
  , "∃x0[person x0]. ∃x1[person x1]. ∃x2[person x2]. (left x0 ∧ left x1) ∧ whistled x2"
  ]

#guard englishPrettyDeriveAs (R^E (W^E T)) "someone left and she2 whistled" ==
  ["λx0. ⟨x0, (∃x1[person x1]. left x1) ∧ whistled x0⟩"]

#guard englishPrettyDeriveAs (S T) "someoneS left and someoneS left and someoneS whistled" ==
  [ "[left x0 ∧ left x1 ∧ whistled x2 | person x0, person x1, person x2]"
  , "[(left x0 ∧ left x1) ∧ whistled x2 | person x0, person x1, person x2]"
  ]

#guard englishPrettyDeriveAs (S T) "the cat near push someoneS saw her" ==
  ["[saw x0 (the (λx1. cat x1 ∧ near x0 x1)) | person x0]"]

#guard englishPrettyDeriveAs T "marianne saved her2 paycheck but push everyone spent it" ==
  ["saved (paycheck marianne) marianne ∧ (∀x0[person x0]. spent (paycheck x0) x0)"]


def main : IO Unit :=
  return ()

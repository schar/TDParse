
module Lexicon.Demo where

import Prelude hiding ((*))

import TDParseCFG
import LambdaCalc
import Utils ( (^), type (^) )
import Data.List
import Data.Maybe
import Data.Foldable ( traverse_ )
import Effect ( Effect )
import Effect.Console ( log, logShow )
import Data.Either


demoLex :: Lexicon
demoLex = map mkLex $
    ("ann"       ^ pure ( Just ann  ^ DP   ^ E                                ))
  : ("mary"      ^ pure ( Just mref ^ DP   ^ (effW E E)                       ))
  : ("marianne"  ^ pure ( Just ma   ^ DP   ^ E                                )
                <> pure ( Just maref^ DP   ^ (effW E E)                       ))
  : ("'s"        ^ pure ( Just poss ^ GenD ^ E :-> (E :-> E) :-> E            )
                <> pure ( Just poss'^ GenD ^ E :-> (E :-> E) :-> effW E E     ))
  : ("left"      ^ pure ( Nothing   ^ VP   ^ (E :-> T)                        ))
  : ("whistled"  ^ pure ( Nothing   ^ VP   ^ (E :-> T)                        ))
  : ("saw"       ^ pure ( Nothing   ^ TV   ^ (E :-> E :-> T)                  ))
  : ("saved"     ^ pure ( Nothing   ^ TV   ^ (E :-> E :-> T)                  ))
  : ("spent"     ^ pure ( Nothing   ^ TV   ^ (E :-> E :-> T)                  ))
  : ("chased"    ^ pure ( Nothing   ^ TV   ^ (E :-> E :-> T)                  ))
  : ("said"      ^ pure ( Nothing   ^ AV   ^ (T :-> E :-> T)                  ))
  : ("gave"      ^ pure ( Nothing   ^ DV   ^ (E :-> E :-> E :-> T)            ))
  : ("she"       ^ pure ( Just pro  ^ DP   ^ (effR E E)                       ))
  : ("it"        ^ pure ( Just pro  ^ DP   ^ effR E E                         )
                <> pure ( Just pro  ^ DP   ^ effR (effR E E) (effR E E)       ))
  : ("her"       ^ pure ( Just pro  ^ DP   ^ (effR E E)                       )
                <> pure ( Just pro  ^ Gen  ^ (effR E E)                       ))
  : ("she2"      ^ pure ( Just pro2 ^ DP   ^ (effR E (effW E E))              ))
  : ("her2"      ^ pure ( Just pro2 ^ DP   ^ (effR E (effW E E))              )
                <> pure ( Just pro2 ^ Gen  ^ (effR E (effW E E))              )
                <> pure ( Just pc'  ^ Gen  ^ (E :-> E) :->
                                             effR E (effW E E)                )
                <> pure ( Just pc   ^ Gen  ^ (E :-> E) :->
                                             effR E (effW (effR E E) E)       ))
  : ("mom"       ^ pure ( Nothing   ^ FN   ^ (E :-> E)                        ))
  : ("paycheck"  ^ pure ( Nothing   ^ FN   ^ (E :-> E)                        ))
  : ("pictureof" ^ pure ( Nothing   ^ RN   ^ (E :-> E :-> E)                  ))
  : ("the"       ^ pure ( Nothing   ^ Det  ^ ((E :-> T) :-> E)                ))
  : ("theC"      ^ pure ( Nothing   ^ Det  ^ effC E T E                       ))
  : ("very"      ^ pure ( Nothing   ^ Deg  ^ ((E :-> T) :-> E :-> T)          ))
  : ("every"     ^ pure ( Nothing   ^ Det  ^ ((E :-> T) :-> effC T T E)       ))
  : ("everyP"    ^ pure ( Nothing   ^ Det  ^ ((E :-> T) :-> (E :-> T) :-> T)  ))
  : ("everyC"    ^ pure ( Nothing   ^ Det  ^ (effC (effC T T E) T E)          ))
  : ("big"       ^ pure ( Nothing   ^ AdjP ^ (E :-> T)                        ))
  : ("happy"     ^ pure ( Nothing   ^ AdjP ^ (E :-> T)                        ))
  : ("dog"       ^ pure ( Nothing   ^ NP   ^ (E :-> T)                        ))
  : ("cat"       ^ pure ( Nothing   ^ NP   ^ (E :-> T)                        ))
  : ("near"      ^ pure ( Nothing   ^ TAdj ^ (E :-> E :-> T)                  ))
  : ("some"      ^ pure ( Nothing   ^ Det  ^ ((E :-> T) :-> effS E)           ))
  : ("some2"     ^ pure ( Nothing   ^ Det  ^ ((E :-> T) :-> effS (effW E E))  ))
  : ("someone"   ^ pure ( Nothing   ^ DP   ^ (effC T T E)                     ))
  : ("someone2"  ^ pure ( Just so2  ^ DP   ^ (effS (effW E E))                ))
  : ("someone3"  ^ pure ( Just so3  ^ DP   ^ (effS E)                         ))
  : ("everyone"  ^ pure ( Just eo   ^ DP   ^ (effC T T E)                     ))
  : ("everyone2" ^ pure ( Just eo2  ^ DP   ^ (effC T T (effW E E))            ))
  : ("tr"        ^ pure ( Just pro  ^ DP   ^ (effR E E)                       ))
  : ("and"       ^ pure ( Nothing   ^ Cor  ^ (T :-> T :-> T)                  ))
  : ("but"       ^ pure ( Nothing   ^ Cor  ^ (T :-> T :-> T)                  ))
  : ("andE"      ^ pure ( Nothing   ^ Cor  ^ (E :-> E :-> E)                  ))
  : ("with"      {-^ pure ( Nothing   ^ TAdj ^ (E :-> E :-> T)                  )-}
                 ^ pure ( Nothing   ^ TAdv ^ (E :-> (E :-> T) :-> E :-> T)    ))
  : ("eclo"      ^ pure ( Just eclo ^ Cmp  ^ (effS T :-> T)                   )
                <> pure ( Just eclo ^ Dmp  ^ (effS T :-> T)                   ))
  : ("maryaling" ^ pure ( Just ml   ^ DP   ^ (effW T E)                       ))
  : ("sassyacat" ^ pure ( Just sc   ^ DP   ^ (effW T E)                       ))
  : ("another"   ^ pure ( Nothing   ^ Det  ^ ((E :-> T) :-> effR E (effS E))  ))
  : Nil
  where
    first  (a ^ s) f = f a ^ s
    second (s ^ a) f = s ^ f s a
    mkLex w = second w $ \s -> map (_ `first` (fromMaybe (make_con s)))
    idTerm = let a = make_var "a" in a ! a
    pushTerm = let x = make_var "x" in x ! (x * x)

    ann = make_con "a"
    mary = make_con "m"
    ma = make_con "ma"
    maref = pushTerm % ma
    mref = pushTerm % mary
    poss = let (p^x) = (make_var "p" ^ make_var "x") in x ! p ! p % x
    poss' = let (p^x) = (make_var "p" ^ make_var "x") in x ! p ! pushTerm % (p % x)
    ml = (make_con "ling" % mary) * mary
    sc = (make_con "cat" % make_con "s") * make_con "s"
    pro = idTerm
    pro2 = pushTerm
    pc = let (p^g) = (make_var "p" ^ make_var "g") in p ! g ! (p * p % g)
    pc' = let (p^g) = (make_var "p" ^ make_var "g") in p ! g ! pushTerm % (p % g)
    so3 = make_set (make_con "person")
    so2 = fmapTerm S % pushTerm % so3
    eo = make_con "everyone"
    eo2 = fmapTerm (C T T) % pushTerm % eo
    eclo = make_con "∃"

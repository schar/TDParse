
module Lexicon.Pure where

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


{- A toy lexicon -}

pureLex :: Lexicon
pureLex = map mkLex $
    ("ann"       ^ pure ( Nothing   ^ DP   ^ E                                ))
  : ("mary"      ^ pure ( Nothing   ^ DP   ^ E                                ))
  : ("'s"        ^ pure ( Just poss ^ GenD ^ E :-> (E :-> E) :-> E            ))
  : ("left"      ^ pure ( Nothing   ^ VP   ^ (E :-> T)                        ))
  : ("whistled"  ^ pure ( Nothing   ^ VP   ^ (E :-> T)                        ))
  : ("saw"       ^ pure ( Nothing   ^ TV   ^ (E :-> E :-> T)                  ))
  : ("saved"     ^ pure ( Nothing   ^ TV   ^ (E :-> E :-> T)                  ))
  : ("spent"     ^ pure ( Nothing   ^ TV   ^ (E :-> E :-> T)                  ))
  : ("chased"    ^ pure ( Nothing   ^ TV   ^ (E :-> E :-> T)                  ))
  : ("said"      ^ pure ( Nothing   ^ AV   ^ (T :-> E :-> T)                  ))
  : ("gave"      ^ pure ( Nothing   ^ DV   ^ (E :-> E :-> E :-> T)            ))
  : ("mom"       ^ pure ( Nothing   ^ FN   ^ (E :-> E)                        ))
  : ("paycheck"  ^ pure ( Nothing   ^ FN   ^ (E :-> E)                        ))
  : ("pictureof" ^ pure ( Nothing   ^ RN   ^ (E :-> E :-> E)                  ))
  : ("the"       ^ pure ( Nothing   ^ Det  ^ ((E :-> T) :-> E)                ))
  : ("very"      ^ pure ( Nothing   ^ Deg  ^ ((E :-> T) :-> E :-> T)          ))
  : ("everyP"    ^ pure ( Nothing   ^ Det  ^ ((E :-> T) :-> (E :-> T) :-> T)  ))
  : ("big"       ^ pure ( Nothing   ^ AdjP ^ (E :-> T)                        ))
  : ("happy"     ^ pure ( Nothing   ^ AdjP ^ (E :-> T)                        ))
  : ("dog"       ^ pure ( Nothing   ^ NP   ^ (E :-> T)                        ))
  : ("cat"       ^ pure ( Nothing   ^ NP   ^ (E :-> T)                        ))
  : ("near"      ^ pure ( Nothing   ^ TAdj ^ (E :-> E :-> T)                  ))
  : ("and"       ^ pure ( Nothing   ^ Cor  ^ (T :-> T :-> T)                  ))
  : ("but"       ^ pure ( Nothing   ^ Cor  ^ (T :-> T :-> T)                  ))
  : ("if"        ^ pure ( Nothing   ^ Adc  ^ (T :-> T :-> T)                  ))
  : ("andE"      ^ pure ( Nothing   ^ Cor  ^ (E :-> E :-> E)                  ))
  : ("with"      {-^ pure ( Nothing   ^ TAdj ^ (E :-> E :-> T)                  )-}
                 ^ pure ( Nothing   ^ TAdv ^ (E :-> (E :-> T) :-> E :-> T)    ))
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
    ml = mary * (make_var "ling" % mary)
    sc = make_var "s" * (make_var "cat" % make_var "s")
    pro = idTerm
    pro2 = pushTerm
    pc = let (p^g) = (make_var "p" ^ make_var "g") in p ! g ! (p * p % g)
    pc' = let (p^g) = (make_var "p" ^ make_var "g") in p ! g ! pushTerm % (p % g)
    so3 = make_set (make_con "person")
    so2 = fmapTerm S % pushTerm % so3
    eo = make_con "everyone"
    eo2 = fmapTerm (C T T) % pushTerm % eo
    eclo = make_con "∃"

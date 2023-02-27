
module Lexicon.Pro where

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


proLex :: Lexicon
proLex = map mkLex $
    ("she"       ^ pure ( Just pro  ^ DP   ^ effR E E                             ))
  : ("it"        ^ pure ( Just pro  ^ DP   ^ effR E E                             )
                <> pure ( Just pro  ^ DP   ^ effR (effR E E) (effR E E)           ))
  : ("her"       ^ pure ( Just pro  ^ DP   ^ effR E E                             )
                <> pure ( Just pro  ^ Gen  ^ effR E E                             ))
  : ("who"       ^ pure ( Just pro  ^ DP   ^ effR E E                             ))
  : ("__"        ^ pure ( Just pro  ^ DP   ^ effR E E                             )
                <> pure ( Just pro  ^ DP   ^ effR (effR E E) (effR E E)           ))
  : (","         ^ pure ( Just top  ^ AV   ^ effR E T :-> E :-> T                 )
                <> pure ( Just top  ^ AV   ^ effR (effR E E) T :-> effR E E :-> T )
                <> pure ( Just top  ^ AV   ^ effR (effR E E) (effR E T) :-> effR E E :-> effR E T ))
  : (":"         ^ pure ( Just top  ^ AV   ^ effR E T :-> E :-> T                 )
                <> pure ( Just top  ^ AV   ^ effR (effR E E) T :-> effR E E :-> T ))
  : Nil
  where
    first  (a ^ s) f = f a ^ s
    second (s ^ a) f = s ^ f s a
    mkLex w = second w $ \s -> map (_ `first` (fromMaybe (make_con s)))
    idTerm = let a = make_var "a" in a ! a
    pushTerm = let x = make_var "x" in x ! (x * x)

    top = let f = make_var "f" in f ! f
    ann = make_con "a"
    john = make_con "j"
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

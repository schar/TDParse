
module Lexicon.Push where

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


pushLex :: Lexicon
pushLex = map mkLex $
    ("mary"      ^ pure ( Just mref ^ DP   ^ (effW E E)                       ))
  : ("marianne"  ^ pure ( Just ma   ^ DP   ^ E                                )
                <> pure ( Just maref^ DP   ^ (effW E E)                       ))
  : ("'s"        ^ pure ( Just poss'^ GenD ^ E :-> (E :-> E) :-> effW E E     ))
  : ("she2"      ^ pure ( Just pro2 ^ DP   ^ (effR E (effW E E))              ))
  : ("her2"      ^ pure ( Just pro2 ^ DP   ^ (effR E (effW E E))              )
                <> pure ( Just pro2 ^ Gen  ^ (effR E (effW E E))              )
                <> pure ( Just pc'  ^ Gen  ^ (E :-> E) :->
                                             effR E (effW E E)                )
                <> pure ( Just pc   ^ Gen  ^ (E :-> E) :->
                                             effR E (effW (effR E E) E)       ))
  : ("some2"     ^ pure ( Nothing   ^ Det  ^ ((E :-> T) :-> effS (effW E E))  ))
  : ("someone2"  ^ pure ( Just so2  ^ DP   ^ (effS (effW E E))                ))
  : ("everyone2" ^ pure ( Just eo2  ^ DP   ^ (effC T T (effW E E))            ))
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

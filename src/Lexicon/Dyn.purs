
module Lexicon.Dyn where

import Prelude hiding ((*))

import TDParseCFG
import LambdaCalc
import Utils ( (^), type (^) )
import Data.List hiding ((!!))
import Data.Maybe
import Data.Foldable ( traverse_ )
import Effect ( Effect )
import Effect.Console ( log, logShow )
import Data.Either


dynLex :: Lexicon
dynLex = map mkLex $
    ("push"      ^ pure ( Just pD   ^ Dmp  ^ (E :-> effD G G E)               ))
  : ("pro0"      ^ pure ( Just p0   ^ DP   ^ effD G G E                       ))
  : ("pro1"      ^ pure ( Just p1   ^ DP   ^ effD G G E                       ))
  : ("pro2"      ^ pure ( Just p2   ^ DP   ^ effD G G E                       ))
  : ("someD"     ^ pure ( Just sD   ^ Det  ^ (E :-> T) :-> effD G G E         ))
  : ("aD"        ^ pure ( Just sD   ^ Det  ^ (E :-> T) :-> effD G G E         ))
  : ("someoneD"  ^ pure ( Just soD  ^ DP   ^ effD G G E                       ))
  : Nil
  where
    first  (a ^ s) f = f a ^ s
    second (s ^ a) f = s ^ f s a
    mkLex w = second w $ \s -> map (_ `first` (fromMaybe (make_con s)))
    idTerm = let a = make_var "a" in a ! a
    pushTerm = let x = make_var "x" in x ! (x * x)
    pD = let (g^x^p) = (make_var "g" ^ make_var "x" ^ make_var "p") in x ! g ! set ( (p ! (x * (x ~ g))) |? make_con "_" )
    p0 = let (g^p) = (make_var "g" ^ make_var "p") in g ! set ( (p ! ((g !! 0) * g)) |? make_con "_" )
    p1 = let (g^p) = (make_var "g" ^ make_var "p") in g ! set ( (p ! ((g !! 1) * g)) |? make_con "_" )
    p2 = let (g^p) = (make_var "g" ^ make_var "p") in g ! set ( (p ! ((g !! 2) * g)) |? make_con "_" )
    soD = let (g^x) = (make_var "g" ^ make_var "x") in g ! set ( (x ! (x * g)) |? make_con "someone" )
    sD = let (g^x^p) = (make_var "g" ^ make_var "x" ^ make_var "p") in p ! g ! set ( (x ! (x * g)) |? make_con "some" % p )

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
    no = let (p^x^k) = (make_var "p" ^ make_var "x" ^ make_var "k") in p ! make_con "nobody" % (x ! eclo % (fmapTerm S % (k ! k % x) % p))
    not = let (p^x) = (make_var "p" ^ make_var "x") in p ! x ! make_con "¬ ∃" % (p % x)

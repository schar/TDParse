-- |

module WFreeLexicon where

import Prelude hiding ((*), (!))
import LambdaCalc
import TDPretty ( showParse, showParse', prettyProof )
import TDParseCFG
import Data.Maybe ( fromMaybe )

wFreeLex :: Lexicon
wFreeLex = map mkLex
  [ ("ann"       , [( Just ann  , DP   , E                              )])
  , ("mary"      , [( Just mary , DP   , E                              )])
  , ("marianne"  , [( Just ma   , DP   , E                              )])
  , ("'s"        , [( Just poss , GenD , E :-> (E :-> E) :-> E          )])
  , ("left"      , [( Nothing   , VP   , E :-> T                        )])
  , ("whistled"  , [( Nothing   , VP   , E :-> T                        )])
  , ("saw"       , [( Nothing   , TV   , E :-> E :-> T                  )])
  , ("saved"     , [( Nothing   , TV   , E :-> E :-> T                  )])
  , ("spent"     , [( Nothing   , TV   , E :-> E :-> T                  )])
  , ("chased"    , [( Nothing   , TV   , E :-> E :-> T                  )])
  , ("said"      , [( Nothing   , AV   , T :-> E :-> T                  )])
  , ("gave"      , [( Nothing   , DV   , E :-> E :-> E :-> T            )])
  , ("she"       , [( Just pro  , DP   , effR E E                       )])
  , ("it"        , [( Just pro  , DP   , effR E E                       )]
                ++ [( Just pro  , DP   , effR (effR E E) (effR E E)     )])
  , ("her"       , [( Just pro  , DP   , effR E E                       )]
                ++ [( Just pro  , Gen  , effR (effI E) E                )]
                ++ [( Just pro  , Gen  , effR E E                       )])
    -- anaphors demand fresh (not yet combined) refs
    --                                         ^^^^^
  , ("herself"   , [( Just pro  , DP   , effR (effI E) E                )])
  , ("mom"       , [( Nothing   , TN   , E :-> E                        )])
  , ("paycheck"  , [( Nothing   , TN   , E :-> E                        )])
  , ("the"       , [( Nothing   , Det  , (E :-> T) :-> E                )])
  , ("theC"      , [( Nothing   , Det  , effC E T E                     )])
  , ("very"      , [( Nothing   , Deg  , (E :-> T) :-> E :-> T          )])
  , ("every"     , [( Nothing   , Det  , (E :-> T) :-> effC T T E       )])
  , ("everyP"    , [( Nothing   , Det  , (E :-> T) :-> (E :-> T) :-> T  )])
  , ("everyC"    , [( Nothing   , Det  , effC (effC T T E) T E          )])
  , ("big"       , [( Nothing   , AdjP , E :-> T                        )])
  , ("happy"     , [( Nothing   , AdjP , E :-> T                        )])
  , ("dog"       , [( Nothing   , NP   , E :-> T                        )])
  , ("cat"       , [( Nothing   , NP   , E :-> T                        )])
  , ("near"      , [( Nothing   , TAdj , E :-> E :-> T                  )])
  , ("some"      , [( Nothing   , Det  , (E :-> T) :-> effS E           )])
  , ("someone"   , [( Nothing   , DP   , effC T T E                     )])
  , ("someone3"  , [( Just so3  , DP   , effS E                         )])
  , ("everyone"  , [( Just eo   , DP   , effC T T E                     )])
  , ("tr"        , [( Just pro  , DP   , effR E E                       )])
  , ("and"       , [( Nothing   , Cor  , T :-> T :-> T                  )])
  , ("but"       , [( Nothing   , Cor  , T :-> T :-> T                  )])
  , ("andE"      , [( Nothing   , Cor  , E :-> E :-> E                  )])
  , ("with"      , [( Nothing   , TAdv , E :-> (E :-> T) :-> E :-> T    )]
                {-++ [( Nothing   , TAdj , (E :-> E :-> T)                  )]-})
  , ("eclo"      , [( Nothing   , Cmp  , effS T :-> T                   )]
                ++ [( Nothing   , Dmp  , effS T :-> T                   )])
  , ("maryaling" , [( Just ml   , DP   , effW T E                       )])
  , ("sassyacat" , [( Just sc   , DP   , effW T E                       )])
  ]
  where
    first  (a,s,t) f = (f a, s, t)
    second (s , a) f = (s , f s a)
    mkLex w = second w $ \s -> map (`first` fromMaybe (make_con s))

    ann = make_con "a"
    mary = make_con "m"
    ma = make_con "ma"
    maref = pushTerm % ma
    mref = pushTerm % mary
    mref' = let p = make_var "p" in p ! pushTerm % (p % mary)
    poss = let [p,x] = map make_var ["p","x"] in x ! p ! p % x
    poss' = let [p,x] = map make_var ["p","x"] in x ! p ! pushTerm % (p % x)
    ml = (make_con "ling" % mary) * mary
    sc = (make_con "cat" % make_con "s") * make_con "s"
    pro = idTerm
    pro2 = pushTerm
    pc = let [p,g] = map make_var ["p","g"] in p ! g ! (p * p % g)
    pc' = let [p,g] = map make_var ["p","g"] in p ! g ! pushTerm % (p % g)
    so3 = make_set (make_con "person")
    so2 = fmapTerm S % pushTerm % so3
    eo = make_con "everyone"
    eo2 = fmapTerm (C T T) % pushTerm % eo

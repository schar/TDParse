{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module TDDemo where

import Prelude hiding ((*), (!))
import LambdaCalc
import TDPretty
import TDParseCFG
import Data.Maybe


{- A toy lexicon -}

demoLex :: Lexicon
demoLex = map mkLex $
    ("ann"       , [( Just ann  , DP   , (E)                              )])
  : ("ann's"     , [( Just ann  , Gen  , (E)                              )])
  : ("mary"      , [( Just mref , DP   , (effW E E)                       )])
  : ("mary's"    , [( Just mref , Gen  , (effW E E)                       )])
  : ("maryaling" , [( Just ml   , DP   , (effW T E)                       )])
  : ("sassyacat" , [( Just sc   , DP   , (effW T E)                       )])
  : ("left"      , [( Nothing   , VP   , (E :-> T)                        )])
  : ("whistled"  , [( Nothing   , VP   , (E :-> T)                        )])
  : ("saw"       , [( Nothing   , TV   , (E :-> E :-> T)                  )])
  : ("chased"    , [( Nothing   , TV   , (E :-> E :-> T)                  )])
  : ("said"      , [( Nothing   , AV   , (T :-> E :-> T)                  )])
  : ("gave"      , [( Nothing   , DV   , (E :-> E :-> E :-> T)            )])
  : ("she"       , [( Just she  , DP   , (effR E E)                       )])
  : ("her"       , [( Just she  , DP   , (effR E E)                       )]
                ++ [( Just she  , Gen  , (effR E E)                       )])
  : ("she2"      , [( Just she2 , DP   , (effR E (effW E E))              )])
  : ("her2"      , [( Just she2 , DP   , (effR E (effW E E))              )]
                ++ [( Just she2 , Gen  , (effR E (effW E E))              )])
  : ("mom"       , [( Nothing   , TN   , (E :-> E)                        )])
  : ("the"       , [( Nothing   , Det  , ((E :-> T) :-> E)                )])
  : ("theC"      , [( Nothing   , Det  , (effC E T E)                     )])
  : ("very"      , [( Nothing   , Deg  , ((E :-> T) :-> E :-> T)          )])
  : ("every"     , [( Nothing   , Det  , ((E :-> T) :-> effC T T E)       )])
  : ("everyP"    , [( Nothing   , Det  , ((E :-> T) :-> (E :-> T) :-> T)  )])
  : ("everyC"    , [( Nothing   , Det  , (effC (effC T T E) T E)          )])
  : ("big"       , [( Nothing   , AdjP , (E :-> T)                        )])
  : ("happy"     , [( Nothing   , AdjP , (E :-> T)                        )])
  : ("dog"       , [( Nothing   , NP   , (E :-> T)                        )])
  : ("cat"       , [( Nothing   , NP   , (E :-> T)                        )])
  : ("near"      , [( Nothing   , TAdj , (E :-> E :-> T)                  )])
  : ("some"      , [( Nothing   , Det  , ((E :-> T) :-> effS E)           )])
  : ("someone"   , [( Nothing   , DP   , (effC T T E)                     )])
  : ("someone2"  , [( Just so2  , DP   , (effS (effW E E))                )])
  : ("someone3"  , [( Just so3  , DP   , (effS E)                         )])
  : ("everyone"  , [( Just eo   , DP   , (effC T T E)                     )])
  : ("everyone2" , [( Just eo2  , DP   , (effC T T (effW E E))            )])
  : ("tr"        , [( Nothing   , DP   , (effR E E)                       )])
  : ("and"       , [( Nothing   , Cor  , (T :-> T :-> T)                  )])
  : ("andE"      , [( Nothing   , Cor  , (E :-> E :-> E)                  )])
  : ("with"      , [( Nothing   , TAdv , (E :-> (E :-> T) :-> E :-> T)    )]
                {-++ [( Nothing   , TAdj , (E :-> E :-> T)                  )]-})
  : ("eclo"      , [( Nothing   , Cmp  , (effS T :-> T)                   )]
                ++ [( Nothing   , Dmp  , (effS T :-> T)                   )])
  : []
  where
    first  (a,s,t) f = (f a, s, t)
    second (s , a) f = (s , f s a)
    mkLex w = second w $ \s -> map (`first` (fromMaybe (make_con s)))
    a = make_var "a"
    ann = make_con "a"
    mary = make_con "m"
    mref = mary * mary
    ml = (make_con "ling" % mary) * mary
    sc = (make_con "cat" % make_con "s") * make_con "s"
    she = a ! a
    she2 = a ! (a * a)
    so3 = make_set (make_con "person")
    so2 = fmapTerm S % she2 % so3
    eo = make_con "everyone"
    eo2 = fmapTerm (C T T) % she2 % eo


-- a toy Context-Free Grammar
demoCFG :: CFG
demoCFG = curry \case
  (DP   , VP   ) -> [CP]
  (Cmp  , CP   ) -> [CP]
  (Cor  , CP   ) -> [CBar]
  (Cor  , DP   ) -> [DBar]
  (DP   , DBar ) -> [DP]
  (Dmp  , DP   ) -> [DP]
  (CP   , CBar ) -> [CP]
  (Det  , NP   ) -> [DP]
  (Gen  , TN   ) -> [DP]
  (AdjP , NP   ) -> [NP]
  (NP   , AdjP ) -> [NP]
  (TAdj , DP   ) -> [AdjP]
  (Deg  , AdjP ) -> [AdjP]
  (TV   , DP   ) -> [VP]
  (AV   , CP   ) -> [VP]
  (DV   , DP   ) -> [TV]
  (VP   , AdvP ) -> [VP]
  (TAdv , DP   ) -> [AdvP]
  (_    , _    ) -> []



{- Test cases -}

s1 = "the very big cat left"

s2 = "she saw her mom"

s3 = "ann's mom saw her"

s4 = "someone left and she2 whistled"

s5 = "the cat near someone2 saw her"

{- Testing helper function -}

main :: IO ()
main = mapM_ (print . fromMaybe ["No parse"] . showParse demoCFG demoLex) [s1, s2, s3, s4, s5]

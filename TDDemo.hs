{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}

module TDDemo where

import Prelude hiding ((*), (!))
import LambdaCalc
import TDPretty ( showParse, showParse', prettyProof )
import TDParseCFG
import Data.Maybe ( fromMaybe )


{- A toy lexicon -}

demoLex :: Lexicon
demoLex = map mkLex
  [ ("ann"       , [( Just ann  , DP   , E                              )])
  , ("mary"      , [( Just mref , DP   , effW E E                       )])
  , ("marianne"  , [( Just ma   , DP   , E                              )]
                ++ [( Just maref, DP   , effW E E                       )])
  , ("'s"        , [( Just poss , GenD , E :-> (E :-> E) :-> E          )]
                ++ [( Just poss', GenD , E :-> (E :-> E) :-> effW E E   )])
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
                ++ [( Just pro  , Gen  , effR E E                       )])
  , ("she2"      , [( Just pro2 , DP   , effR E (effW E E)              )])
  , ("her2"      , [( Just pro2 , DP   , effR E (effW E E)              )]
                ++ [( Just pro2 , Gen  , effR E (effW E E)              )]
                ++ [( Just pc'  , Gen  , (E :-> E) :->
                                         effR E (effW E E)              )]
                ++ [( Just pc   , Gen  , (E :-> E) :->
                                         effR E (effW (effR E E) E)     )])
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
  , ("someone2"  , [( Just so2  , DP   , effS (effW E E)                )])
  , ("someone3"  , [( Just so3  , DP   , effS E                         )])
  , ("everyone"  , [( Just eo   , DP   , effC T T E                     )])
  , ("everyone2" , [( Just eo2  , DP   , effC T T (effW E E)            )])
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
  (DP   , GenD ) -> [Gen]
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

s6 = "marianne saved her2 paycheck but marianne's mom spent it"

{- Testing helper function -}

main :: IO ()
main = mapM_ putStrLn $ ps ++ qs
  where
    ps = [s1,s2,s3,s4,s5] >>= fromMaybe ["No parse"] . showParse demoCFG demoLex
    qs = fromMaybe ["No parse"] $ showParse' demoCFG demoLex (hasType T) prettyProof s6

testLex = ("'s"        , [(poss , GenD , E :-> (E :-> E) :-> E          )]) : demoLex
  where
    poss = let [p,x] = map make_var ["p","x"] in x ! p ! p % x


noPushLex :: Lexicon
noPushLex = map mkLex
  [ ("ann"       , [( Just ann  , DP   , E                              )])
  -- , ("mary"      , [( Just mary , DP   , effW E E                       )])
  -- , ("marianne"  , [( Just ma   , DP   , E                              )]
  --               ++ [( Just maref, DP   , effW E E                       )])
  , ("'s"        , [( Just poss , GenD , E :-> (E :-> E) :-> E          )]
                -- ++ [( Just poss', GenD , E :-> (E :-> E) :-> effW E E   )]
    )
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
                ++ [( Just pro  , Gen  , effR E E                       )])
  -- , ("she2"      , [( Just pro2 , DP   , effR E (effW E E)              )])
  -- , ("her2"      , [( Just pro2 , DP   , effR E (effW E E)              )]
  --               ++ [( Just pro2 , Gen  , effR E (effW E E)              )]
  --               ++ [( Just pc'  , Gen  , (E :-> E) :->
  --                                        effR E (effW E E)              )]
  --               ++ [( Just pc   , Gen  , (E :-> E) :->
  --                                        effR E (effW (effR E E) E)     )])
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
  -- , ("someone2"  , [( Just so2  , DP   , effS (effW E E)                )])
  , ("someone3"  , [( Just so3  , DP   , effS E                         )])
  , ("everyone"  , [( Just eo   , DP   , effC T T E                     )])
  -- , ("everyone2" , [( Just eo2  , DP   , effC T T (effW E E)            )])
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TDDemo where

import Prelude hiding (and)

import TDPretty ( prettyParse )
import TDParseCFG


{- A toy lexicon -}

ann       = [("ann"      , DP     , E)                             ]
mary      = [("mary"     , DP     , effW E E)                      ]
sassyacat = [("Sassy     , a cat" , DP             , effW T E)     ]
left      = [("left"     , VP     , E :-> T)                       ]
saw       = [("saw"      , TV     , E :-> E :-> T)                 ]
chased    = [("chased"   , TV     , E :-> E :-> T)                 ]
said      = [("said"     , AV     , T :-> E :-> T)                 ]
gave      = [("gave"     , DV     , E :-> E :-> E :-> T)           ]
she       = [("she"      , DP     , effR E E)                      ]
her       = [("her"      , DP     , effR E E)                      ]
she'      = [("she'"     , DP     , effR E (effW E E))             ]
mom       = [("mom"      , TN     , E :-> E)                       ]
the       = [("the"      , Det    , (E :-> T) :-> E)               ]
very      = [("very"     , Deg    , (E :-> T) :-> E :-> T)         ]
every     = [("every"    , Det    , (E :-> T) :-> effC T T E)      ]
every'    = [("every'"   , Det    , (E :-> T) :-> (E :-> T) :-> T) ]
big       = [("big"      , AdjP   , E :-> T)                       ]
happy     = [("happy"    , AdjP   , E :-> T)                       ]
dog       = [("dog"      , NP     , E :-> T)                       ]
cat       = [("cat"      , NP     , E :-> T)                       ]
near      = [("near"     , TAdj   , E :-> E :-> T)                 ]
someone   = [("someone"  , DP     , effS (effW E E))               ]
someone2  = [("someone"  , DP     , effS E)                        ]
everyone  = [("everyone" , DP     , effC T T (effW E E))           ]
tr        = [("tr"       , DP     , effR E E)                      ]
and       = [("and"      , Cor    , T :-> T :-> T)                 ]
with      = [("with"     , TAdv   , E :-> E :-> T) ,
             ("with"     , TAdj   , E :-> (E :-> T) :-> E :-> T)   ]


-- a toy Context-Free Grammar
productions :: CFG
productions = curry $ \case
  (DP   , VP   ) -> [CP]
  (Det  , NP   ) -> [DP]
  (DP   , TN   ) -> [DP]
  (AdjP , NP   ) -> [NP]
  (NP   , AdjP ) -> [NP]
  (TAdj , DP   ) -> [AdjP]
  (Deg  , AdjP ) -> [AdjP]
  (TV   , DP   ) -> [VP]
  (AV   , CP   ) -> [VP]
  (DV   , DP   ) -> [TV]
  (VP   , AdvP ) -> [VP]
  (TAdv , DP   ) -> [AdvP]
  (Cor  , CP   ) -> [CorP]
  (CP   , CorP ) -> [CorP]
  (_    , _    ) -> []



{- Test cases -}

s1 = [the, very, big, cat, left]

s2 = [she, saw, her, mom]

s3 = [ann, mom, saw, her]

s4 = [someone, left, and, she', left]

s5 = [every, dog, saw, every, cat]

s6 = [every', happy, dog, chased, every, cat]


main :: IO ()
main = mapM_ (print . prettyParse productions) [s1, s2, s3, s4, s5, s6]

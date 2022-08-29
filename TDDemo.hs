{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TDDemo where

import Prelude hiding (and)

import TDPretty
import TDParseCFG


{- A toy lexicon -}

ann       = [("ann'"       , DP     , E)                             ]
mary      = [("mary'"      , DP     , effW E E)                      ]
maryaling = [("(m--ling)'" , DP     , effW T E)                      ]
sassyacat = [("(s--cat)'"  , DP     , effW T E)                      ]
left      = [("left'"      , VP     , E :-> T)                       ]
whistled  = [("whistled'"  , VP     , E :-> T)                       ]
saw       = [("saw'"       , TV     , E :-> E :-> T)                 ]
chased    = [("chased'"    , TV     , E :-> E :-> T)                 ]
said      = [("said'"      , AV     , T :-> E :-> T)                 ]
gave      = [("gave'"      , DV     , E :-> E :-> E :-> T)           ]
she       = [("she'"       , DP     , effR E E)                      ]
her       = [("her'"       , DP     , effR E E)                      ]
she2      = [("she2'"      , DP     , effR E (effW E E))             ]
her2      = [("her2'"      , DP     , effR E (effW E E))             ]
mom       = [("mom'"       , TN     , E :-> E)                       ]
the       = [("the'"       , Det    , (E :-> T) :-> E)               ]
very      = [("very'"      , Deg    , (E :-> T) :-> E :-> T)         ]
every     = [("every'"     , Det    , (E :-> T) :-> effC T T E)      ]
everyP    = [("everyP'"    , Det    , (E :-> T) :-> (E :-> T) :-> T) ]
big       = [("big'"       , AdjP   , E :-> T)                       ]
happy     = [("happy'"     , AdjP   , E :-> T)                       ]
dog       = [("dog'"       , NP     , E :-> T)                       ]
cat       = [("cat'"       , NP     , E :-> T)                       ]
near      = [("near'"      , TAdj   , E :-> E :-> T)                 ]
some      = [("some'"      , Det    ,(E :-> T) :-> effS E)           ]
someone   = [("someone'"   , DP     , effC T T E)                    ]
someone2  = [("someone2'"  , DP     , effS (effW E E))               ]
someone3  = [("someone3'"  , DP     , effS E)                        ]
everyone  = [("everyone'"  , DP     , effC T T E)                    ]
everyone2 = [("everyone2'" , DP     , effC T T (effW E E))           ]
tr        = [("tr'"        , DP     , effR E E)                      ]
and       = [("and'"       , Cor    , T :-> T :-> T)                 ]
with      = [{-("with'"      , TAdj   , E :-> E :-> T)-}
             ("with'"      , TAdv   , E :-> (E :-> T) :-> E :-> T)   ]
eclo      = [("eclo'"      , Cmp    , effS T :-> T)]


-- a toy Context-Free Grammar
productions :: CFG
productions = curry $ \case
  (DP   , VP   ) -> [CP]
  (Cmp  , CP   ) -> [CP]
  (Cor  , CP   ) -> [CorP]
  (CP   , CorP ) -> [CP]
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
  (_    , _    ) -> []



{- Test cases -}

s1 = [the, very, big, cat, left]

s2 = [she, saw, her, mom]

s3 = [ann, mom, saw, her]

s4 = [someone, left, and, she2, whistled]

s5 = [every, dog, saw, every, cat]


main :: IO ()
main = mapM_ (print . prettyParse productions) [s1, s2, s3, s4, s5]

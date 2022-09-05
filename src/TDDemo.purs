{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TDDemo where

import Prelude

import TDPretty ( showParse )
import Text.Pretty ( render )
import TDParseCFG
import Utils ( (^), type (^) )
import Data.List
import Data.Maybe
import Data.Foldable ( traverse_ )
import Effect ( Effect )
import Effect.Console ( log, logShow )


{- A toy lexicon -}

lexicon :: Lexicon
lexicon =
    ("ann"       ^ singleton ( "ann"       ^ DP    ^ (E)                              ))
  : ("ann's"     ^ singleton ( "ann"       ^ Gen   ^ (E)                              ))
  : ("mary"      ^ singleton ( "mary"      ^ DP    ^ (effW E E)                       ))
  : ("mary's"    ^ singleton ( "mary"      ^ Gen   ^ (effW E E)                       ))
  : ("maryaling" ^ singleton ( "(m--ling)" ^ DP    ^ (effW T E)                       ))
  : ("sassyacat" ^ singleton ( "(s--cat)"  ^ DP    ^ (effW T E)                       ))
  : ("left"      ^ singleton ( "left"      ^ VP    ^ (E :-> T)                        ))
  : ("whistled"  ^ singleton ( "whistled"  ^ VP    ^ (E :-> T)                        ))
  : ("saw"       ^ singleton ( "saw"       ^ TV    ^ (E :-> E :-> T)                  ))
  : ("chased"    ^ singleton ( "chased"    ^ TV    ^ (E :-> E :-> T)                  ))
  : ("said"      ^ singleton ( "said"      ^ AV    ^ (T :-> E :-> T)                  ))
  : ("gave"      ^ singleton ( "gave"      ^ DV    ^ (E :-> E :-> E :-> T)            ))
  : ("she"       ^ singleton ( "she"       ^ DP    ^ (effR E E)                       ))
  : ("her"       ^ singleton ( "her"       ^ DP    ^ (effR E E)                       )
                <> singleton ( "her"       ^ Gen   ^ (effR E E)                       ))
  : ("she2"      ^ singleton ( "she2"      ^ DP    ^ (effR E (effW E E))              ))
  : ("her2"      ^ singleton ( "her2"      ^ DP    ^ (effR E (effW E E))              )
                <> singleton ( "her2"      ^ Gen   ^ (effR E (effW E E))              ))
  : ("mom"       ^ singleton ( "mom"       ^ TN    ^ (E :-> E)                        ))
  : ("the"       ^ singleton ( "the"       ^ Det   ^ ((E :-> T) :-> E)                ))
  : ("very"      ^ singleton ( "very"      ^ Deg   ^ ((E :-> T) :-> E :-> T)          ))
  : ("every"     ^ singleton ( "every"     ^ Det   ^ ((E :-> T) :-> effC T T E)       ))
  : ("everyP"    ^ singleton ( "everyP"    ^ Det   ^ ((E :-> T) :-> (E :-> T) :-> T)  ))
  : ("everyC"    ^ singleton ( "everyC"    ^ Det   ^ (effC (effC T T E) T E)          ))
  : ("big"       ^ singleton ( "big"       ^ AdjP  ^ (E :-> T)                        ))
  : ("happy"     ^ singleton ( "happy"     ^ AdjP  ^ (E :-> T)                        ))
  : ("dog"       ^ singleton ( "dog"       ^ NP    ^ (E :-> T)                        ))
  : ("cat"       ^ singleton ( "cat"       ^ NP    ^ (E :-> T)                        ))
  : ("near"      ^ singleton ( "near"      ^ TAdj  ^ (E :-> E :-> T)                  ))
  : ("some"      ^ singleton ( "some"      ^ Det   ^ ((E :-> T) :-> effS E)           ))
  : ("someone"   ^ singleton ( "someone"   ^ DP    ^ (effC T T E)                     ))
  : ("someone2"  ^ singleton ( "someone2"  ^ DP    ^ (effS (effW E E))                ))
  : ("someone3"  ^ singleton ( "someone3"  ^ DP    ^ (effS E)                         ))
  : ("everyone"  ^ singleton ( "everyone"  ^ DP    ^ (effC T T E)                     ))
  : ("everyone2" ^ singleton ( "everyone2" ^ DP    ^ (effC T T (effW E E))            ))
  : ("tr"        ^ singleton ( "tr"        ^ DP    ^ (effR E E)                       ))
  : ("and"       ^ singleton ( "and"       ^ Cor   ^ (T :-> T :-> T)                  ))
  : ("andE"      ^ singleton ( "and"       ^ Cor   ^ (E :-> E :-> E)                  ))
  : ("with"      {-^ singleton ( "with"      ^ TAdj  ^ (E :-> E :-> T)                  )-}
                 ^ singleton ( "with"      ^ TAdv  ^ (E :-> (E :-> T) :-> E :-> T)    ))
  : ("eclo"      ^ singleton ( "eclo"      ^ Cmp   ^ (effS T :-> T)                   )
                <> singleton ( "eclo"      ^ Dmp   ^ (effS T :-> T)                   ))
  : Nil


-- a toy Context-Free Grammar
productions :: CFG
productions = case _,_ of
  DP   , VP    -> pure CP
  Cmp  , CP    -> pure CP
  CP   , CBar  -> pure CP
  Cor  , CP    -> pure CBar
  Det  , NP    -> pure DP
  Gen  , TN    -> pure DP
  Dmp  , DP    -> pure DP
  DP   , DBar  -> pure DP
  Cor  , DP    -> pure DBar
  AdjP , NP    -> pure NP
  NP   , AdjP  -> pure NP
  TAdj , DP    -> pure AdjP
  Deg  , AdjP  -> pure AdjP
  TV   , DP    -> pure VP
  AV   , CP    -> pure VP
  VP   , AdvP  -> pure VP
  DV   , DP    -> pure TV
  TAdv , DP    -> pure AdvP
  _    , _     -> Nil



{- Test cases -}

s1 = "the very big cat left"

s2 = "she saw her mom"

s3 = "ann's mom saw her"

s4 = "someone2 left and she2 whistled"

s5 = "every dog saw every cat"


main :: Effect Unit
main = traverse_ (maybe (logShow "unk") (traverse_ log) <<< showParse productions lexicon) $ s1: s2: s3: s4: s5: Nil

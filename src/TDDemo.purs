{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TDDemo where

import Prelude hiding ((*))

import TDPretty ( showParse )
import Text.Pretty ( render )
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

lexicon :: Lexicon
lexicon = map mkLex $
    ("ann"       ^ pure ( Just ann  ^ DP   ^ (E)                              ))
  : ("ann's"     ^ pure ( Just ann  ^ Gen  ^ (E)                              ))
  : ("mary"      ^ pure ( Just mref ^ DP   ^ (effW E E)                       ))
  : ("mary's"    ^ pure ( Just mref ^ Gen  ^ (effW E E)                       ))
  : ("maryaling" ^ pure ( Just ml   ^ DP   ^ (effW T E)                       ))
  : ("sassyacat" ^ pure ( Just sc   ^ DP   ^ (effW T E)                       ))
  : ("left"      ^ pure ( Nothing   ^ VP   ^ (E :-> T)                        ))
  : ("whistled"  ^ pure ( Nothing   ^ VP   ^ (E :-> T)                        ))
  : ("saw"       ^ pure ( Nothing   ^ TV   ^ (E :-> E :-> T)                  ))
  : ("chased"    ^ pure ( Nothing   ^ TV   ^ (E :-> E :-> T)                  ))
  : ("said"      ^ pure ( Nothing   ^ AV   ^ (T :-> E :-> T)                  ))
  : ("gave"      ^ pure ( Nothing   ^ DV   ^ (E :-> E :-> E :-> T)            ))
  : ("she"       ^ pure ( Just she  ^ DP   ^ (effR E E)                       ))
  : ("her"       ^ pure ( Just she  ^ DP   ^ (effR E E)                       )
                <> pure ( Just she  ^ Gen  ^ (effR E E)                       ))
  : ("she2"      ^ pure ( Just she2 ^ DP   ^ (effR E (effW E E))              ))
  : ("her2"      ^ pure ( Just she2 ^ DP   ^ (effR E (effW E E))              )
                <> pure ( Just she2 ^ Gen  ^ (effR E (effW E E))              ))
  : ("mom"       ^ pure ( Nothing   ^ TN   ^ (E :-> E)                        ))
  : ("the"       ^ pure ( Nothing   ^ Det  ^ ((E :-> T) :-> E)                ))
  : ("very"      ^ pure ( Nothing   ^ Deg  ^ ((E :-> T) :-> E :-> T)          ))
  : ("every"     ^ pure ( Nothing   ^ Det  ^ ((E :-> T) :-> effC T T E)       ))
  : ("everyP"    ^ pure ( Nothing   ^ Det  ^ ((E :-> T) :-> (E :-> T) :-> T)  ))
  : ("everyC"    ^ pure ( Nothing   ^ Det  ^ (effC (effC T T E) T E)          ))
  : ("big"       ^ pure ( Nothing   ^ AdjP ^ (E :-> T)                        ))
  : ("happy"     ^ pure ( Nothing   ^ AdjP ^ (E :-> T)                        ))
  : ("dog"       ^ pure ( Nothing   ^ NP   ^ (E :-> T)                        ))
  : ("cat"       ^ pure ( Nothing   ^ NP   ^ (E :-> T)                        ))
  : ("near"      ^ pure ( Nothing   ^ TAdj ^ (E :-> E :-> T)                  ))
  : ("some"      ^ pure ( Nothing   ^ Det  ^ ((E :-> T) :-> effS E)           ))
  : ("someone"   ^ pure ( Nothing   ^ DP   ^ (effC T T E)                     ))
  : ("someone2"  ^ pure ( Just so2  ^ DP   ^ (effS (effW E E))                ))
  : ("someone3"  ^ pure ( Just so3  ^ DP   ^ (effS E)                         ))
  : ("everyone"  ^ pure ( Just eo   ^ DP   ^ (effC T T E)                     ))
  : ("everyone2" ^ pure ( Just eo2  ^ DP   ^ (effC T T (effW E E))            ))
  : ("tr"        ^ pure ( Nothing   ^ DP   ^ (effR E E)                       ))
  : ("and"       ^ pure ( Nothing   ^ Cor  ^ (T :-> T :-> T)                  ))
  : ("andE"      ^ pure ( Nothing   ^ Cor  ^ (E :-> E :-> E)                  ))
  : ("with"      {-^ pure ( Nothing   ^ TAdj ^ (E :-> E :-> T)                  )-}
                 ^ pure ( Nothing   ^ TAdv ^ (E :-> (E :-> T) :-> E :-> T)    ))
  : ("eclo"      ^ pure ( Nothing   ^ Cmp  ^ (effS T :-> T)                   )
                <> pure ( Nothing   ^ Dmp  ^ (effS T :-> T)                   ))
  : Nil
  where
    first  (a ^ s) f = f a ^ s
    second (s ^ a) f = s ^ f s a
    mkLex w = second w $ \s -> map (_ `first` (fromMaybe (make_var s)))
    a = make_var "a"
    ann = make_var "a"
    mary = make_var "m"
    mref = mary * mary
    ml = mary * make_var "ling" % mary
    sc = make_var "s" * make_var "cat" % make_var "s"
    she = a ! a
    she2 = a ! a * a
    so3 = make_set "person"
    so2 = fmap S % she2 % so3
    eo = make_var "everyone"
    eo2 = fmap (C T T) % she2 % eo

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


-- main :: Effect Unit
-- main = traverse_ (maybe (logShow "unk") (traverse_ log) <<< showParse productions lexicon) $ s1: s2: s3: s4: s5: Nil

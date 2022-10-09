{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TDDemo where

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


-- a toy Context-Free Grammar
demoCFG :: CFG
demoCFG = case _,_ of
  DP   , VP    -> pure CP
  Cmp  , CP    -> pure CP
  CP   , CBar  -> pure CP
  Cor  , CP    -> pure CBar
  Det  , NP    -> pure DP
  Gen  , TN    -> pure DP
  Dmp  , DP    -> pure DP
  DP   , DBar  -> pure DP
  DP   , GenD  -> pure Gen
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

s5 = "the cat near someone2 saw her"

s6 = "marianne saved her2 paycheck but marianne's mom spent it"


-- main :: Effect Unit
-- main = traverse_ (maybe (logShow "unk") (traverse_ log) <<< showParse productions lexicon) $ s1: s2: s3: s4: s5: Nil


-- test = maybe (log "") (traverse_ log) $ showParse' demoCFG demoLex ((_ == effS T) <<< getProofType) prettyProof "someone3 gave some cat some dog"

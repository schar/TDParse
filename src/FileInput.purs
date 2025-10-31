module FileInput where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn3, runEffectFn1, runEffectFn3)
import Web.Event.Event (Event)
import Web.File.File (File)

foreign import getFileFromEvent_ :: EffectFn1 Event File

getFileFromEvent :: Event -> Effect File
getFileFromEvent = runEffectFn1 getFileFromEvent_

foreign import readFileAsText_ :: EffectFn3 (Error -> Effect Unit) (String -> Effect Unit) File Unit

readFileAsText :: File -> Aff String
readFileAsText file = makeAff \cb -> do
  runEffectFn3 readFileAsText_ (cb <<< Left) (cb <<< Right) file
  pure nonCanceler

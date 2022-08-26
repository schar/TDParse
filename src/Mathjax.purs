module Mathjax where

import Prelude

import Effect (Effect)

foreign import typeset :: Effect Unit
foreign import clearPhrase :: Effect Unit
foreign import lexFeedback :: String -> Effect Unit

module Utils where

import Data.List
import Control.Apply
import Data.Function (flip)

flippedApply :: forall f a b. Apply f => f a -> f (a -> b) -> f b
flippedApply = flip apply

infixl 4 flippedApply as <**>

one = flip any

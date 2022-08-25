{-# LANGUAGE FlexibleContexts #-}

module Memo where

import Control.Monad.State
import Data.Map
import Data.Maybe
import Prelude
import Data.Tuple.Nested
import Control.Lazy

-- memoization
memoize f p@(lo /\ hi /\ _) = do
  v <- gets $ lookup (lo /\ hi)
  case v of
    Just y -> pure y
    _ -> do
      y <- f p
      modify_ $ insert (lo /\ hi) y
      pure y

memo f x = evalState (fix (memoize <<< f) x) empty

view f x = runState (fix (memoize <<< f) x) empty

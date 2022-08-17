{-# LANGUAGE FlexibleContexts #-}

module Memo where

import Control.Monad.State.Lazy
import Data.Map
import Prelude hiding (lookup)

-- memoization
memoize f p@(lo, hi, _) = do
  v <- gets $ lookup (lo, hi)
  case v of
    Just y -> return y
    _ -> do
      y <- f p
      modify $ insert (lo, hi) y
      return y

memo f x = evalState (fix (memoize . f) x) empty

view f x = runState (fix (memoize . f) x) empty

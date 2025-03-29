{-# LANGUAGE FlexibleContexts #-}

module Memo where

import Control.Monad.State.Lazy
import Data.Map
import Data.Function (fix)
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

execute m = evalState m empty

memo f x = execute (fix (memoize . f) x)

view f x = runState (fix (memoize . f) x) empty

-- same as above, but simply uses p as key
memoize' f p = gets (lookup p) >>= maybe addkey return
  where
    addkey = mapState (extend $ uncurry (insert p)) $ f p
    extend g w = (fst w, g w) -- sadly comonad not in base

memoizeTag i f p = gets (lookup (i, p)) >>= maybe addkey return
  where
    addkey = mapState (extend $ uncurry (insert (i, p))) $ f p
    extend g w = (fst w, g w) -- sadly comonad not in base

memo' f x = execute (fix (memoize' . f) x)

executeT m = evalState (evalStateT m empty) empty
memoT f x = executeT (fix (memoize . f) x)

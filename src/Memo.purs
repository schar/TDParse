{-# LANGUAGE FlexibleContexts #-}

module Memo where

import Control.Extend
import Control.Lazy
import Control.Monad.State
import Data.Map
import Data.Maybe
import Data.Tuple
import Data.Tuple.Nested
import Prelude

-- memoization
memoize f p@(lo /\ hi /\ _) = do
  v <- gets $ lookup (lo /\ hi)
  case v of
    Just y -> pure y
    _ -> do
      y <- f p
      modify_ $ insert (lo /\ hi) y
      pure y

execute m = evalState m empty

memo f x = execute (fix (memoize <<< f) x)

view f x = runState (fix (memoize <<< f) x) empty

-- same as above, but simply uses p as key
memoize' :: forall v a . Ord a => (a -> State (Map a v) v) -> a -> State (Map a v) v
memoize' f p = gets (lookup p) >>= maybe addkey pure
  where
    addkey = mapState (extend $ uncurry (insert p)) $ f p
    extend g w = (fst w /\ g w) -- sadly comonad not in base

memoizeTag i f p = gets (lookup (i /\ p)) >>= maybe addkey pure
  where
    addkey = mapState (extend $ uncurry (insert (i /\ p))) $ f p
    extend g w = (fst w /\ g w) -- sadly comonad not in base

memo' f x = execute (fix (memoize' <<< f) x)

executeT m = evalState (evalStateT m empty) empty
memoT f x = executeT (fix (memoize <<< f) x)

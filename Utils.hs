{-# LANGUAGE ViewPatterns #-}

module Utils where

-- import TDParseCFG
-- import Data.List
-- import Data.Tree
-- import Data.Maybe
-- import TDParseCFG
-- import Data.List
-- import Data.Tree
-- import Data.Maybe
import Control.Monad (liftM2, MonadPlus (mplus))
import Control.Monad.State

-- modeTree :: Proof -> Tree (Maybe Mode)
-- modeTree (Proof _ sem _ []) = Node Nothing []
-- modeTree (Proof _ (Comb op _) _ [l,r]) = Node (Just op) [modeTree l, modeTree r]

-- pruneTree :: Tree (Maybe a) -> Tree a
-- pruneTree = fromJust . go
--   where
--     go :: Tree (Maybe a) -> Maybe (Tree a)
--     go (Node Nothing subtree) =
--       case mapMaybe go subtree of
--         [] -> Nothing
--         (Node x ts) : ts' -> Just $ Node x (ts ++ ts')
--     go (Node (Just a) subtree) = Just $ Node a (mapMaybe go subtree)

infixr 6 <+>
(<+>) :: Monad m => m [a] -> m [a] -> m [a]
(<+>) = liftM2 (++)

optionally v x = do
  n <- lift get
  if n > 0
    then lift $ (put (n-1) >> return v) `mplus` return x
    else return x

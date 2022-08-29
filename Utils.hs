{-# LANGUAGE ViewPatterns #-}

module Utils where

import TDParseCFG
import Data.List
import Data.Tree
import Data.Maybe
import GHC.Exts (currentCallStack)

modeTree :: Proof -> Tree (Maybe Mode)
modeTree (Proof _ sem _ []) = Node Nothing []
modeTree (Proof _ (Comb op _ _) _ [l,r]) = Node (Just op) [modeTree l, modeTree r]

pruneTree :: Tree (Maybe a) -> Tree a
pruneTree = fromJust . go
  where
    go :: Tree (Maybe a) -> Maybe (Tree a)
    go (Node Nothing subtree) =
      case mapMaybe go subtree of
        [] -> Nothing
        (Node x ts) : ts' -> Just $ Node x (ts ++ ts')
    go (Node (Just a) subtree) = Just $ Node a (mapMaybe go subtree)


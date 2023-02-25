-- |

module Test where

import Control.Monad.State.Lazy

data Tree = Leaf Int Char | Branch Int Tree Tree
  deriving (Show, Eq)

showTree d t@(Leaf n c) = putStrLn $ concat (replicate d " ") ++ show t
showTree d (Branch n l r) = do
  putStrLn $ concat (replicate d " ") ++ "Branch " ++ show n
  showTree (d+2) r
  showTree (d+2) l

label :: Tree -> State Int Tree
label tree = case tree of
  Leaf _ 'a' -> get >>= \n -> put (n+1) >> return (Leaf n 'a')
  Leaf _ c   -> get >>= \n -> return (Leaf n c)
  Branch _ left right -> do
    n <- get
    r <- label right
    n' <- get
    l <- label left
    return $ Branch n (assign n' l) r
  where
    assign n (Leaf _ c) = Leaf n c
    assign n (Branch _ l r) = Branch n l r

t =
  Branch 0
    (Branch 0
       (Leaf 0 'b')
       (Branch 0
          (Leaf 0 'a')
          (Leaf 0 'b')
       )
    )
    (Branch 0
       (Branch 0
         (Leaf 0 'b')
         (Leaf 0 'a')
       )
       (Branch 0
          (Branch 0
             (Leaf 0 'c')
             (Leaf 0 'a')
          )
          (Leaf 0 'a')
       )
    )

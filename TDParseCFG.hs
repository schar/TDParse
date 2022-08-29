{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TDParseCFG where

import Prelude hiding ((<>), (^), and)
import Control.Monad (join)
import Lambda_calc ( Term, make_var, (#), (^) )
import Memo ( memo )
import Data.Function ( (&) )
import Control.Applicative ( (<**>) )
import Data.List


{- Datatypes for syntactic and semantic composition-}

-- some syntactic categories
data Cat
  = CP | Cmp -- Clauses and Complementizers
  | CorP | Cor -- Coordinators and Coordination Phrases
  | DP | Det | Gen -- (Genitive) Determiners and full Determiner Phrases
  | NP | TN -- Transitive (relational) Nouns and full Noun Phrases
  | VP | TV | DV | AV -- Transitive, Ditransitive, and Attitude Verbs and Verb Phrases
  | AdjP | TAdj | Deg | AdvP | TAdv -- Modifiers
  deriving (Eq, Show-- , Read
           )

-- semantic types
infixr :->
data Type
  = E | T             -- Base types
  | Type :-> Type     -- Functions
  | Eff F Type        -- F-ectful
  deriving (Eq, Show-- , Read
           )


{- Syntactic parsing -}

-- A (binary-branching) grammar is a list of production rules, telling
-- you what new categories you can build out of ones you are handed
type CFG = Cat -> Cat -> [Cat]

-- Our syntactic objects are (binary-branching) constituency trees with
-- typed leaves
data Syn
  = Leaf String Type
  | Branch Syn Syn
  deriving (Show)

-- Phrases to be parsed are lists of "signs" whose various morphological
-- spellouts, syntactic categories, and types are known
type Sense = (String, Cat, Type) -- a single sense of a single word
type Sign = [Sense]              -- a word may have several senses
type Phrase = [Sign]

-- a simple memoized chart parser, parameterized to a particular grammar
protoParse ::
  Monad m
  => CFG
  -> ((Int, Int, Phrase) -> m [(Cat, Syn)])
  ->  (Int, Int, Phrase) -> m [(Cat, Syn)]
protoParse _   _ (_, _, [sign]) = return [(c, Leaf s t) | (s, c, t) <- sign]
protoParse cfg f phrase         = concat <$> mapM help (bisect phrase)
  where
    bisect (lo, hi, u) = do
      i <- [1 .. length u - 1]
      let (ls, rs) = splitAt i u
      return ((lo, lo + i - 1, ls), (lo + i, hi, rs))

    help (ls, rs) = do
      parsesL <- f ls
      parsesR <- f rs
      return
        [ (cat, Branch lsyn rsyn)
        | (lcat, lsyn) <- parsesL
        , (rcat, rsyn) <- parsesR
        , cat <- cfg lcat rcat
        ]

-- Return all the grammatical constituency structures of a phrase by parsing it
-- and throwing away the category information
parse :: CFG -> Phrase -> [Syn]
parse cfg ws = snd <$> memo (protoParse cfg) (0, length ws - 1, ws)

-- A semantic object is either a lexical entry or a mode of combination applied to
-- two other semantic objects
data Sem
  = Lex String
  | Comb Mode Sem Sem
  deriving (Show)

-- Modes of combination
data Mode
  = FA | BA | PM | FC -- Base        > < ^
  | MR Mode | ML Mode -- Functor     map
  | UR Mode | UL Mode -- Applicative pure
  -- | Z Mode            -- VFS         Z
  | A Mode            -- Applicative <*>
  | J Mode            -- Monad       join
  | Eps Mode          -- Adjoint     counit
  | D Mode            -- Cont        lower

instance Show Mode where
  show = \case
    FA      -> ">"
    BA      -> "<"
    PM      -> "&"
    FC      -> "."
    MR op   -> "R," ++ show op
    ML op   -> "L," ++ show op
    UL op   -> "UL," ++ show op
    UR op   -> "UR," ++ show op
    A op    -> "A," ++ show op
    J op    -> "J," ++ show op
    Eps op  -> "Eps," ++ show op
    D op    -> "D," ++ show op


-- Effects
data F = S | R Type | W Type | C Type Type
  deriving (Eq, Show-- , Read
           )

-- convenience constructors
effS     = Eff S
effR r   = Eff (R r)
effW w   = Eff (W w)
effC r o = Eff (C r o)


{- Type classes -}

-- You could implement some real logic here if you wanted,
-- but all our Effects are indeed Functors, and all but (W E) are
-- indeed Applicative and Monadic
-- The only adjunction we demonstrate is that between W and R
functor, applicative, monad :: F -> Bool
functor     _       = True
applicative f@(W w) = functor f && monoid w
applicative f       = functor f && True
monad       f       = applicative f && True

monoid :: Type -> Bool
monoid T = True
monoid _ = False

adjoint :: F -> F -> Bool
adjoint (W i) (R j) = i == j
adjoint _ _         = False


{- Type-driven combination -}

-- A semantic derivation is a proof that a particular string has a
-- particular meaning at a particular type
-- For displaying derivations, we also maintain the subproofs used at
-- each proof step
data Proof = Proof String Sem Type [Proof]
  deriving (Show)

-- Evaluate a constituency tree by finding all the derivations of its
-- daughers and then all the ways of combining those derivations in accordance
-- with their types and the available modes of combination
synsem :: Syn -> [Proof]
synsem (Leaf s t)   = [Proof s (Lex s) t []]
synsem (Branch l r) =
  [ Proof (lstr ++ " " ++ rstr) (Comb op lval rval) ty [lp, rp]
  | lp@(Proof lstr lval lty _) <- synsem l
  , rp@(Proof rstr rval rty _) <- synsem r
  , (op, ty) <- combine lty rty
  ]

-- The basic unEffectful modes of combination (add to these as you like)
modes :: Type -> Type -> [(Mode, Type)]
modes = curry $ \case
  (a :-> b , r      ) | a == r -> [(FA, b)]
  (l       , a :-> b) | l == a -> [(BA, b)]
  (a :-> T , b :-> T) | a == b -> [(PM, a :-> T)]
  (_       , _      )          -> []

-- Here is the essential type-driven combination logic; given two types,
-- what are all the ways that they may be combined
combine :: Type -> Type -> [(Mode, Type)]
combine l r = sweepSpurious . join $

  -- for starters, try the basic modes of combination
  modes l r

  -- then if the left daughter is Functorial, try to find a mode
  -- `op` that would combine its underlying type with the right daughter
  ++ [ (ML op, Eff f c)
     | Eff f a <- [l]
     , functor f
     , (op, c) <- combine a r
     ]

  -- vice versa if the right daughter is Functorial
  ++ [ (MR op, Eff f c)
     | Eff f b <- [r]
     , functor f
     , (op, c) <- combine l b
     ]

  -- if the left daughter requests something Functorial, try to find an
  -- `op` that would combine it with a `pure`ified right daughter
  ++ [ (UR op, c)
     | Eff f a :-> b <- [l]
     , applicative f
     , (op, c) <- combine (a :-> b) r
     ]

  -- vice versa if the right daughter requests something Functorial
  ++ [ (UL op, c)
     | Eff f a :-> b <- [r]
     , applicative f
     , (op, c) <- combine l (a :-> b)
     ]

  -- additionally, if both daughters are Applicative, then see if there's
  -- some mode `op` that would combine their underlying types
  -- ++ [ (A op, Eff h c)
  --    | Eff f a <- [l]
  --    , applicative f
  --    , Eff g b <- [r]
  --    , h <- combineFs f g
  --    , (op, c) <- combine a b
  --    ]

  -- this is only if you want to see some derivations in the Variable-Free style
  -- ++ [ (Z op, i :-> d)
  --    | a :-> i :-> b <- [l]
  --    , Eff (R i) c <- [r]
  --    , (op, d) <- combine (a :-> b) c
  --    ]

  -- finally see if the resulting types can additionally be lowered (D),
  -- joined (J), or canceled out (Eps)
  <**> [addD, addEps, addJ, return]

-- closeUnder :: [a -> [a]] -> [a] -> [a]
-- closeUnder fs = concat . takeWhile (not . null) . iterate (\xs -> join $ fs <*> xs)
-- close f xs = concat $ unfoldr (\xs -> if null xs then Nothing else Just (xs, f xs)) xs

-- Make sure that two Effects can compatibly be sequenced
-- (only relevant to A and J modes)
combineFs :: F -> F -> [F]
combineFs = curry $ \case
  (S    , S     )           -> [S]
  (R i  , R j   ) | i == j  -> [R i]
  (W i  , W j   ) | i == j  -> [W i]
  (C i j, C j' k) | j == j' -> [C i k]
  _                         -> []

addJ :: (Mode, Type) -> [(Mode, Type)]
addJ = \case
  (op, Eff f (Eff g a)) | monad f -> [(J op, Eff h a) | h <- combineFs f g]
  _                               -> []

addEps :: (Mode, Type) -> [(Mode, Type)]
addEps = \case
  (op, Eff f (Eff g a)) | adjoint f g -> [(Eps op, a)]
  _                                   -> []

addD :: (Mode, Type) -> [(Mode, Type)]
addD = \case
  (op, Eff (C i a) a') | a == a' -> [(D op, i)]
  _                              -> []

sweepSpurious :: [(Mode, Type)] -> [(Mode, Type)]
sweepSpurious ops = foldr filter ops [urll, murr, mrmr, mull, mlml]
  where urll (m,t) = not $ isInfixOf (init $ show (UR (MR FA))) (show m)
        murr (m,t) = not $ isInfixOf (init $ show (J (MR (MR FA)))) (show m)
        mrmr (m,t) = not $ isInfixOf (init $ show (J (MR (J (MR FA))))) (show m)
        mull (m,t) = not $ isInfixOf (init $ show (J (ML (ML FA)))) (show m)
        mlml (m,t) = not $ isInfixOf (init $ show (J (ML (J (ML FA))))) (show m)


{- Mapping semantic values to (un-normalized) Lambda_calc terms -}

semTerm :: Sem -> Term
semTerm (Lex w)       = make_var w
semTerm (Comb op l r) = modeTerm op # semTerm l # semTerm r

-- The definitions of the combinators that build our modes of combination
-- Here we are using the Lambda_calc library to write (untyped) lambda expressions
-- that we can display in various forms
modeTerm :: Mode -> Term
modeTerm = \case
         -- \l r -> l r
  FA     -> l ^ r ^ l # r

         -- \l r -> r l
  BA     -> l ^ r ^ r # l

         -- \l r a -> l a `and` r a
  PM     -> l ^ r ^ a ^ make_var "and'" # (l # a) # (r # a)

         -- \l r a -> l (r a)
  FC     -> l ^ r ^ a ^ l # (r # a)

         -- \l R -> (\a -> op l a) <$> r
  MR op  -> l ^ r ^ make_var "fmap" # (a ^ (modeTerm op # l # a)) # r

         -- \L r -> (\a -> op a r) <$> L
  ML op  -> l ^ r ^ make_var "fmap" # (a ^ (modeTerm op # a # r)) # l

         -- \l R -> op (\a -> r (pure a)) l
  UL op  -> l ^ r ^ modeTerm op # (a ^ r # (make_var "pure" # a)) # l

         -- \L r -> op (\a -> l (pure a)) r
  UR op  -> l ^ r ^ modeTerm op # (a ^ l # (make_var "pure" # a)) # r

         -- \L R -> op <$> L <*> R
  A op   -> l ^ r ^ make_var "(<*>)" # (make_var "fmap" # modeTerm op # l) # r

         -- \l r a -> op l (r a) a
  -- Z op   -> l ^ r ^ a ^ modeTerm op # l # (r # a) # a

         -- \l r -> join (op l r)
  J op   -> l ^ r ^ make_var "join" # (modeTerm op # l # r)

         -- \l r -> counit (op l r)
  Eps op -> l ^ r ^ make_var "counit" # (modeTerm op # l # r)

         -- \l r -> op l r id
  D op   -> l ^ r ^ modeTerm op # l # r # (a ^ a)

  where
    l = make_var "l"
    r = make_var "r"
    a = make_var "a"
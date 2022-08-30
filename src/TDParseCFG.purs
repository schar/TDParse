{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TDParseCFG where

import Data.Bounded
import Data.Enum
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Tuple.Nested
import Prelude hiding ((#))

import Control.Alternative (guard)
import Control.Monad (join)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Foldable (lookup)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromMaybe)
import Data.Show.Generic (genericShow)
import Data.String as DS
import Data.String.Utils (words)
import Data.Traversable (sequence)
import Data.Traversable (traverse)
import Effect.Exception.Unsafe (unsafeThrow)
import LambdaCalc (Term, make_var, (#), (^))
import Memo (memo)
import Unsafe.Coerce (unsafeCoerce)
import Utils ((<**>), one)

{- Datatypes for syntactic and semantic composition-}

-- some syntactic categories
data Cat
  = CP | Cmp -- Clauses and Complementizers
  | CorP | Cor -- Coordinators and Coordination Phrases
  | DP | Det | Gen -- (Genitive) Determiners and full Determiner Phrases
  | NP | TN -- Transitive (relational) Nouns and full Noun Phrases
  | VP | TV | DV | AV -- Transitive, Ditransitive, and Attitude Verbs and Verb Phrases
  | AdjP | TAdj | Deg | AdvP | TAdv -- Modifiers

derive instance Eq Cat
derive instance Ord Cat
derive instance Generic Cat _
instance Show Cat where
  show = genericShow
instance Enum Cat where
  succ = genericSucc
  pred = genericPred
instance Bounded Cat where
  top = genericTop
  bottom = genericBottom

-- semantic types
data Ty
  = E | T         -- Base types
  | Arr Ty Ty     -- Functions
  | Eff F Ty      -- F-ectful
derive instance Eq Ty
derive instance Generic Ty _
instance Show Ty where
  show t = genericShow t

infixr 1 Arr as :->

-- Effects
data F = S | R Ty | W Ty | C Ty Ty
derive instance Eq F
derive instance Generic F _
instance Show F where
  show = genericShow
showNoIndices :: F -> String
showNoIndices = case _ of
  S     -> "S"
  R _   -> "R"
  W _   -> "W"
  C _ _ -> "C"

atomicTypes = E : T : Nil
atomicEffects = atomicTypes >>= \i -> atomicTypes >>= \j -> S : R i : W i : C i j : Nil

-- convenience constructors
effS     = Eff S
effR r   = Eff (R r)
effW w   = Eff (W w)
effC r o = Eff (C r o)

{- Syntactic parsing -}

-- A (binary-branching) grammar is a list of production rules, telling
-- you what new categories you can build out of ones you are handed
type CFG = Cat -> Cat -> List Cat

-- Our syntactic objects are (binary-branching) constituency trees with
-- typed leaves
data Syn
  = Leaf String Ty
  | Branch Syn Syn

-- Phrases to be parsed are lists of "signs" whose various morphological
-- spellouts, syntactic categories, and types are known
type Sense = (String /\ Cat /\ Ty) -- a single sense of a single word
type Word = List Sense              -- a word may have several senses
type Phrase = List Word

type Lexicon = List (String /\ Word)

-- a simple memoized chart parser, parameterized to a particular grammar
protoParse ::
  forall m. Monad m
  => CFG
  -> (Int /\ Int /\ Phrase -> m (List (Cat /\ Syn)))
  ->  Int /\ Int /\ Phrase -> m (List (Cat /\ Syn))
protoParse _   _ (_ /\ _ /\ (sign:Nil)) = pure $ map (\(s /\ c /\ t) -> c /\ Leaf s t) sign
protoParse cfg f phrase         = concat <$> traverse help (bisect phrase)
  where
    bisect (lo /\ hi /\ u) = do
      i <- 1 .. (length u - 1)
      let (ls /\ rs) = take i u /\ drop i u
      pure $ (lo /\ (lo + i - 1) /\ ls) /\ ((lo + i) /\ hi /\ rs)

    help (ls /\ rs) = do
      parsesL <- f ls
      parsesR <- f rs
      pure $ do
        lcat /\ lsyn <- parsesL
        rcat /\ rsyn <- parsesR
        cat <- cfg lcat rcat
        pure $ cat /\ Branch lsyn rsyn

-- Return all the grammatical constituency structures of a phrase by parsing it
-- and throwing away the category information
parse :: CFG -> Lexicon -> String -> Maybe (List Syn)
parse cfg lex input = do
  ws <- sequence $ map (\s -> lookup s lex) <<< fromFoldable $ words input
  pure $ snd <$> memo (protoParse cfg) (0 /\ (length ws - 1) /\ ws)

-- A semantic object is either a lexical entry or a mode of combination applied to
-- two other semantic objects
data Sem
  = Lex String
  | Comb Mode Sem Sem
derive instance Eq Sem
derive instance Generic Sem _
instance Show Sem where
  show t = genericShow t

-- Modes of combination
data Mode
  = FA | BA | PM | FC     -- Base        > < ^
  | MR F Mode | ML F Mode -- Functor     map
  | UR F Mode | UL F Mode -- Applicative pure
  | A Mode                -- Applicative <*>
  | J Mode                -- Monad       join
  | Eps Mode              -- Adjoint     counit
  | D Mode                -- Cont        lower
derive instance Eq Mode
derive instance Generic Mode _
instance Show Mode where
  show = case _ of
    FA      -> ">"
    BA      -> "<"
    PM      -> "&"
    FC      -> "."
    MR _ op -> "R," <> show op
    ML _ op -> "L," <> show op
    UL _ op -> "UL," <> show op
    UR _ op -> "UR," <> show op
    A op    -> "A," <> show op
    J op    -> "J," <> show op
    Eps op  -> "Eps," <> show op
    D op    -> "D," <> show op

modeAsList :: Int -> Mode -> String
modeAsList v = case _ of
  MR f op -> "R"    <> showF f <> "," <> modeAsList v op
  ML f op -> "L"    <> showF f <> "," <> modeAsList v op
  UL f op -> "UL"   <> showF f <> "," <> modeAsList v op
  UR f op -> "UR"   <> showF f <> "," <> modeAsList v op
  A op    -> "A,"   <> modeAsList v op
  J op    -> "J,"   <> modeAsList v op
  Eps op  -> "Eps," <> modeAsList v op
  D op    -> "D,"   <> modeAsList v op
  _       -> ""
  where
    showF = case v of
      0 -> const ""        -- just the unparameterized combinators
      1 -> showNoIndices   -- the combinators parameterized by Effect constructor
      _ -> show            -- the combinators parameterized by full indexed Effect type


{- Type classes -}

-- You could implement some real logic here if you wanted,
-- but all our Effects are indeed Functors, and all but (W E) are
-- indeed Applicative and Monadic
-- The only adjunction we demonstrate is that between W and R
functor     _       = true
applicative f@(W w) = functor f && monoid w
applicative f       = functor f && true
monad       f       = applicative f && true

monoid T = true
monoid _ = false

adjoint (W i) (R j) = i == j
adjoint _ _         = false

class Commute f where
  commutative :: f -> Boolean
instance Commute Ty where
  commutative ty = ty == T
instance Commute F where
  commutative = case _ of
    S     -> true
    R _   -> true
    W w   -> commutative w
    C _ _ -> false

{- Type-driven combination -}

-- A semantic derivation is a proof that a particular string has a
-- particular meaning at a particular type
-- For displaying derivations, we also maintain the subproofs used at
-- each proof step
data Proof = Proof String Sem Ty (List Proof)
derive instance Eq Proof
derive instance Generic Proof _
instance Show Proof where
  show t = genericShow t

getProofType ∷ Proof → Ty
getProofType (Proof _ _ ty _) = ty

-- Evaluate a constituency tree by finding all the derivations of its
-- daughers and then all the ways of combining those derivations in accordance
-- with their types and the available modes of combination
synsem :: Syn -> List Proof
synsem (Leaf s t)   = pure $ Proof s (Lex s) t Nil
synsem (Branch l r) = do
  lp@(Proof lstr lval lty _) <- synsem l
  rp@(Proof rstr rval rty _) <- synsem r
  op /\ ty <- combine lty rty
  pure $ Proof (lstr <> " " <> rstr) (Comb op lval rval) ty (lp:rp:Nil)

prove ∷ CFG -> Lexicon -> String -> Maybe (List Proof)
prove cfg lex input = concatMap synsem <$> parse cfg lex input

-- The basic unEffectful modes of combination (add to these as you like)
modes :: Ty -> Ty -> List (Mode /\ Ty)
modes = case _,_ of
  a :-> b , r       | a == r -> pure (FA /\ b)
  l       , a :-> b | l == a -> pure (BA /\ b)
  a :-> T , b :-> T | a == b -> pure (PM /\ (a :-> T))
  _       , _                -> Nil

-- Make sure that two Effects can compatibly be sequenced
-- (only relevant to A and J modes)
combineFs :: F -> F -> List F
combineFs = case _,_ of
  S    , S                -> pure $ S
  R i  , R j    | i == j  -> pure $ R i
  W i  , W j    | i == j  -> pure $ W i
  C i j, C j' k | j == j' -> pure $ C i k
  _    , _                -> Nil

-- Here is the essential type-driven combination logic; given two types,
-- what are all the ways that they may be combined
combine :: Ty -> Ty -> List (Mode /\ Ty)
combine l r = sweepSpurious <<< join $

  -- for starters, try the basic modes of combination
  modes l r

  -- then if the left daughter is Functorial, try to find a mode
  -- `op` that would combine its underlying type with the right daughter
  <> case l of
       Eff f a | functor f -> combine a r <#> \(op /\ c) -> (ML f op /\ Eff f c)
       _                   -> Nil

  -- vice versa if the right daughter is Functorial
  <> case r of
      Eff f b | functor f -> combine l b <#> \(op /\ c) -> (MR f op /\ Eff f c)
      _                   -> Nil

  -- if the left daughter requests something Functorial, try to find an
  -- `op` that would combine it with a `pure`ified right daughter
  <> case l,r of
       Eff f a :-> b, _ -> combine (a :-> b) r <#> \(op /\ c) -> (UR f op /\ c)
       _            , _ -> Nil

  -- vice versa if the right daughter requests something Functorial
  <> case l,r of
       _, Eff f a :-> b -> combine l (a :-> b) <#> \(op /\ c) -> (UL f op /\ c)
       _,             _ -> Nil

  -- additionally, if both daughters are Applicative, then see if there's
  -- some mode `op` that would combine their underlying types
  <> case l,r of
       Eff f a, Eff g b | applicative f -> do (op /\ c) <- combine a b
                                              h <- combineFs f g
                                              pure (A op /\ Eff h c)
       _      , _                       -> Nil

  -- finally see if the resulting types can additionally be lowered (D),
  -- joined (J), or canceled out (Eps)
  <**> (addD : addEps : addJ : pure : Nil)

addJ :: (Mode /\ Ty) -> List (Mode /\ Ty)
addJ = case _ of
  op /\ Eff f (Eff g a) | monad f -> combineFs f g <#> \h -> (J op /\ Eff h a)
  _                               -> Nil

addEps :: (Mode /\ Ty) -> List (Mode /\ Ty)
addEps = case _ of
  op /\ Eff f (Eff g a) | adjoint f g -> pure (Eps op /\ a)
  _                                   -> Nil

addD :: (Mode /\ Ty) -> List (Mode /\ Ty)
addD = case _ of
  op /\ Eff (C i a) a' | a == a' -> pure (D op /\ i)
  _                              -> Nil

sweepSpurious :: List (Mode /\ Ty) -> List (Mode /\ Ty)
sweepSpurious ops = foldr filter ops
  [
  -- elim unit rule ambiguity (UR,R == R,UR)
    \(m /\ _) -> not $ m `contains 0` UR S (MR S FA)
                                    --   ^     ^  the Effects on these modes are
                                    --            ignored by `contains 0`

  -- could J earlier
  , \(m /\ _) -> not $ m `contains 0` J (MR S (MR S FA))
  , \(m /\ _) -> not $ m `contains 0` J (ML S (J (ML S FA)))

  , \(m /\ _) -> not $ m `contains 0` J (MR S (J (MR S FA)))
  , \(m /\ _) -> not $ m `contains 0` J (ML S (ML S FA))

  , \(m /\ _) -> not $ m `contains 0` J (A    (MR S FA))
  , \(m /\ _) -> not $ m `contains 0` J (ML S (A    FA))

    -- could A instead
  , \(m /\ _) -> not $ m `contains 0` J (ML S (MR S FA))
  , \(m /\ _) -> not $ m `contains 0` J (ML S (J (MR S FA)))

  -- for commutative effects, could J earlier
  , \(m /\ _) -> not $ m `contains 0` J (A    (ML S FA))
  , \(m /\ _) -> not $ m `contains 0` J (MR S (A    FA))

  -- for commutative effects, could A instead
  , \(m /\ _) -> not $ one commuter $ \f -> m `contains 2` J (MR f (ML f FA))
  , \(m /\ _) -> not $ one commuter $ \f -> m `contains 2` J (MR f (J (ML f FA)))

  -- could D . A instead
  , \(m /\ _) -> not $ m `contains 0` D (ML S (D (MR S FA)))

  -- could J earlier (maybe not desirable if J restricted w/Cont)
  , \(m /\ _) -> not $ m `contains 0` D (A  (D (MR S FA)))
  , \(m /\ _) -> not $ m `contains 0` D (ML S (D (A  FA)))
  , \(m /\ _) -> not $ m `contains 0` D (MR S (D (MR S FA)))
  , \(m /\ _) -> not $ m `contains 0` D (ML S (D (ML S FA)))
  ]
  where
    contains n haystack needle = DS.contains (DS.Pattern $ modeAsList n needle) $ modeAsList n haystack
    commuter = filter commutative atomicEffects


{- Mapping semantic values to (un-normalized) Lambda_calc terms -}

semTerm :: Sem -> Term
semTerm (Lex w)       = make_var (w <> "'")
semTerm (Comb op l r) = modeTerm op # semTerm l # semTerm r

-- The definitions of the combinators that build our modes of combination
-- Here we are using the Lambda_calc library to write (untyped) lambda expressions
-- that we can display in various forms
modeTerm :: Mode -> Term
modeTerm = case _ of
          -- \l r -> l r
  FA      -> l ^ r ^ l # r

          -- \l r -> r l
  BA      -> l ^ r ^ r # l

          -- \l r a -> l a `and` r a
  PM      -> l ^ r ^ a ^ make_var "and'" # (l # a) # (r # a)

          -- \l r a -> l (r a)
  FC      -> l ^ r ^ a ^ l # (r # a)

          -- \l R -> (\a -> op l a) <$> r
  MR _ op -> l ^ r ^ make_var "fmap" # (a ^ (modeTerm op # l # a)) # r

          -- \L r -> (\a -> op a r) <$> L
  ML _ op -> l ^ r ^ make_var "fmap" # (a ^ (modeTerm op # a # r)) # l

          -- \l R -> op (\a -> r (pure a)) l
  UL _ op -> l ^ r ^ modeTerm op # (a ^ r # (make_var "pure" # a)) # l

          -- \L r -> op (\a -> l (pure a)) r
  UR _ op -> l ^ r ^ modeTerm op # (a ^ l # (make_var "pure" # a)) # r

          -- \L R -> op <$> L <*> R
  A op    -> l ^ r ^ make_var "(<*>)" # (make_var "fmap" # modeTerm op # l) # r

          -- \l r a -> op l (r a) a
  -- Z op    -> l ^ r ^ a ^ modeTerm op # l # (r # a) # a

          -- \l r -> join (op l r)
  J op    -> l ^ r ^ make_var "join" # (modeTerm op # l # r)

          -- \l r -> counit (op l r)
  Eps op  -> l ^ r ^ make_var "counit" # (modeTerm op # l # r)

          -- \l r -> op l r id
  D op    -> l ^ r ^ modeTerm op # l # r # (a ^ a)

  where
    l = make_var "l"
    r = make_var "r"
    a = make_var "a"

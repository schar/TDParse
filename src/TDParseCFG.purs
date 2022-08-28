{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TDParseCFG where

import Data.List
import Data.Maybe
import Data.Tuple
import Data.Tuple.Nested
import Prelude hiding ((#))

import Control.Alternative (guard)
import Control.Monad (join)
import Data.Enum
import Data.Bounded
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Foldable (lookup)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromMaybe)
import Data.Show.Generic (genericShow)
import Data.String.Utils (words)
import Data.Traversable (sequence)
import Data.Traversable (traverse)
import Effect.Exception.Unsafe (unsafeThrow)
import LambdaCalc (Term, make_var, (#), (^))
import Memo (memo)
import Unsafe.Coerce (unsafeCoerce)
import Utils ((<**>))

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
-- instance BoundedEnum Cat where
--   cardinality = genericCardinality
--   toEnum = genericToEnum
--   fromEnum = genericFromEnum

-- semantic types
data Ty
  = E | T             -- Base types
  | Arr Ty Ty     -- Functions
  | Eff F Ty        -- F-ectful
derive instance Eq Ty
derive instance Generic Ty _
instance Show Ty where
  show t = genericShow t

-- Effects
data F = S | R Ty | W Ty | C Ty Ty
derive instance Eq   F
derive instance Generic F _
instance Show F where
  show f = genericShow f

-- convenience constructors
effS     = Eff S
effR r   = Eff (R r)
effW w   = Eff (W w)
effC r o = Eff (C r o)

infixr 1 Arr as :->

{- Syntactic parsing -}

-- A (binary-branching) grammar is a list of production rules, telling
-- you what new categories you can build out of ones you are handed
type CFG = Cat -> Cat -> List Cat

-- Our syntactic objects are (binary-branching) constituency trees with
-- typed leaves
data Syn
  = Leaf String Ty
  | Branch Syn Syn
-- derive instance Show Syn

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
  = FA | BA | PM | FC -- Base        > < ^
  | LR Mode | LL Mode -- Functor     <$>
  | UR Mode | UL Mode -- Applicative pure
  | A Mode            -- Applicative <*>
  | J Mode            -- Monad       join
  | Eps Mode          -- Adjoint     counit
  | D Mode            -- Cont        lower
derive instance Eq Mode
derive instance Generic Mode _
instance Show Mode where
  show t = genericShow t


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

-- -- Make sure that two Effects can compatibly be sequenced
-- -- (only relevant to A and J modes)
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
combine l r = join $

  -- for starters, try the basic modes of combination
  modes l r

  -- then if the left daughter is Functorial, try to find a mode
  -- `op` that would combine its underlying type with the right daughter
  <> case l of
       Eff f a | functor f -> combine a r <#> \(op /\ c) -> (LR op /\ Eff f c)
       _                   -> Nil

--   -- vice versa if the right daughter is Functorial
  <> case r of
      Eff f b | functor f -> combine l b <#> \(op /\ c) -> (LL op /\ Eff f c)
      _                   -> Nil

--   -- additionally, if both daughters are Applicative, then see if there's
--   -- some mode `op` that would combine their underlying types
  -- <> case l,r of
  --      Eff f a, Eff g b | applicative f -> do (op /\ c) <- combine a b
  --                                             h <- combineFs f g
  --                                             pure (A op /\ Eff h c)
  --      _      , _                       -> Nil

  <> case l,r of
       Eff f a :-> b, _ -> combine (a :-> b) r <#> \(op /\ c) -> (UR op /\ c)
       _            , _ -> Nil

  <> case l,r of
       _, Eff f a :-> b -> combine l (a :-> b) <#> \(op /\ c) -> (UL op /\ c)
       _,             _ -> Nil

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


-- {- Mapping semantic values to (un-normalized) Lambda_calc terms -}

semTerm :: Sem -> Term
semTerm (Lex w)       = make_var (w <> "'")
semTerm (Comb op l r) = modeTerm op # semTerm l # semTerm r

-- The definitions of the combinators that build our modes of combination
-- Here we are using the Lambda_calc library to write (untyped) lambda expressions
-- that we can display in various forms
modeTerm :: Mode -> Term
modeTerm = case _ of
         -- \l r -> l r
  FA     -> l ^ r ^ l # r

         -- \l r -> r l
  BA     -> l ^ r ^ r # l

         -- \l r a -> l a `and` r a
  PM     -> l ^ r ^ a ^ make_var "and'" # (l # a) # (r # a)

         -- \l r a -> l (r a)
  FC     -> l ^ r ^ a ^ l # (r # a)

         -- \l R -> (\a -> op l a) <$> r
  LL op  -> l ^ r ^ make_var "fmap" # (a ^ (modeTerm op # l # a)) # r

         -- \L r -> (\a -> op a r) <$> L
  LR op  -> l ^ r ^ make_var "fmap" # (a ^ (modeTerm op # a # r)) # l

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

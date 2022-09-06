{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module TDParseCFG where

import Data.Bounded
import Data.Enum
import Data.List
import Data.Maybe
import Data.Either
import Data.Tuple
import Data.Tuple.Nested
import Memo
import Prelude hiding ((#))

import Control.Alternative (guard)
import Control.Apply (lift2)
import Control.Lazy (fix)
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
import LambdaCalc (Term(Set,Dom,Map), make_var, (!), (%), _1, _2, (*))
import Unsafe.Coerce (unsafeCoerce)
import Utils ((<**>), one, (<+>), (^), type (^))


{- Datatypes for syntactic and semantic composition-}

-- some syntactic categories
data Cat
  = CP | Cmp -- Clauses and Complementizers
  | CBar | DBar | Cor -- Coordinators and Coordination Phrases
  | DP | Det | Gen | Dmp -- (Genitive) Determiners and Determiner Phrases
  | NP | TN -- Transitive (relational) Nouns and Noun Phrases
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
derive instance Ord Ty
derive instance Generic Ty _
instance Show Ty where
  show t = genericShow t

infixr 1 Arr as :->

-- Effects
data F = S | R Ty | W Ty | C Ty Ty
derive instance Eq F
derive instance Ord F
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
atomicEffects =
  pure S <> (R <$> atomicTypes) <> (W <$> atomicTypes) <> (lift2 C atomicTypes atomicTypes)

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
  = Leaf String Term Ty
  | Branch Syn Syn

-- Phrases to be parsed are lists of "signs" whose various morphological
-- spellouts, syntactic categories, and types are known
type Sense = (Term ^ Cat ^ Ty) -- a single sense of a single word
type Word = String ^ List Sense              -- a word may have several senses
type Phrase = List Word
type Lexicon = List Word

-- a simple memoized chart parser, parameterized to a particular grammar
protoParse ::
  forall m. Monad m
  => CFG
  -> (Int ^ Int ^ Phrase -> m (List (Cat ^ Syn)))
  ->  Int ^ Int ^ Phrase -> m (List (Cat ^ Syn))
protoParse _   _ (_^_^(s^sign):Nil) =
  pure $ map (\(d^c^t) -> c ^ Leaf s d t) sign
protoParse cfg f phrase =
  concat <$> traverse help (bisect phrase)
  where
    bisect (lo ^ hi ^ u) = do
      i <- 1 .. (length u - 1)
      let (ls ^ rs) = take i u ^ drop i u
      pure $ (lo ^ (lo + i - 1) ^ ls) ^ ((lo + i) ^ hi ^ rs)

    help (ls ^ rs) = do
      parsesL <- f ls
      parsesR <- f rs
      pure $ do
        lcat ^ lsyn <- parsesL
        rcat ^ rsyn <- parsesR
        cat <- cfg lcat rcat
        pure $ cat ^ Branch lsyn rsyn

-- Return all the grammatical constituency structures of a phrase by parsing it
-- and throwing away the category information
parse :: CFG -> Lexicon -> String -> Maybe (List Syn)
parse cfg lex input = do
  ws <- sequence $ map (\s -> map (s ^ _) $ lookup s lex) <<< fromFoldable $ words input
  pure $ snd <$> memo (protoParse cfg) (0 ^ (length ws - 1) ^ ws)

-- A semantic object is either a lexical entry or a mode of combination applied to
-- two other semantic objects
data Sem
  = Lex Term
  | Comb Mode Sem Sem
derive instance Eq Sem
derive instance Generic Sem _
instance Show Sem where
  show t = genericShow t

-- Modes of combination
data Mode
  = FA | BA | PM | FC     -- Base        > < & .
  | MR F Mode | ML F Mode -- Functor     map
  | UR F Mode | UL F Mode -- Applicative pure
  | A F Mode              -- Applicative <*>
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
    MR _ op -> "R, "   <> show op
    ML _ op -> "L, "   <> show op
    UL _ op -> "UL, "  <> show op
    UR _ op -> "UR, "  <> show op
    A  _ op -> "A, "   <> show op
    J op    -> "J, "   <> show op
    Eps op  -> "Eps, " <> show op
    D op    -> "D, "   <> show op

modeAsList :: Int -> Mode -> String
modeAsList v = case _ of
  MR f op -> "R"    <> showF f <> "," <> modeAsList v op
  ML f op -> "L"    <> showF f <> "," <> modeAsList v op
  UL f op -> "UL"   <> showF f <> "," <> modeAsList v op
  UR f op -> "UR"   <> showF f <> "," <> modeAsList v op
  A  f op -> "A,"   <> showF f <> "," <> modeAsList v op
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
functor _       = true
appl    f@(W w) = functor f && monoid w
appl    f       = functor f && true
monad   f       = appl f && true

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
    R _   -> false -- just to have a simple test case
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

getProofType :: Proof -> Ty
getProofType (Proof _ _ ty _) = ty

-- Evaluate a constituency tree by finding all the derivations of its
-- daughters and then all the ways of combining those derivations in accordance
-- with their types and the available modes of combination
synsem :: Syn -> List Proof
synsem = execute <<< go
  where
    go (Leaf s d t)   = pure $ singleton $ Proof s (Lex d) t Nil
    go (Branch l r) = do -- memo block
      lefts  <- go l
      rights <- go r
      map concat $ sequence do -- list block
        lp@(Proof lstr lval lty _) <- lefts
        rp@(Proof rstr rval rty _) <- rights
        pure do -- memo block
          combos <- combine lty rty
          pure do -- list block
            (op ^ ty) <- combos
            singleton $ Proof (lstr <> " " <> rstr) (Comb op lval rval) ty (lp:rp:Nil)

prove ∷ CFG -> Lexicon -> String -> Maybe (List Proof)
prove cfg lex input = concatMap synsem <$> parse cfg lex input

-- The basic unEffectful modes of combination (add to these as you like)
modes :: Ty -> Ty -> List (Mode ^ Ty)
modes = case _,_ of
  a :-> b , r       | a == r -> pure (FA ^ b)
  l       , a :-> b | l == a -> pure (BA ^ b)
  a :-> T , b :-> T | a == b -> pure (PM ^ (a :-> T))
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

combine = curry $ fix (memoize' <<< openCombine)

-- Here is the essential type-driven combination logic; given two types,
-- what are all the ways that they may be combined
-- openCombine :: Ty -> Ty -> List (Mode ^ Ty)
openCombine combine (l ^ r) = sweepSpurious <<< join <$>

  -- for starters, try the basic modes of combination
  pure (modes l r)

  -- then if the left daughter is Functorial, try to find a mode
  -- `op` that would combine its underlying type with the right daughter
  <+> case l of
        Eff f a        | functor f -> combine (a^r) <#> map \(op ^ c) -> (ML f op ^ Eff f c)
        _                          -> pure Nil

  -- vice versa if the right daughter is Functorial
  <+> case r of
        Eff f b        | functor f -> combine (l^b) <#> map \(op ^ c) -> (MR f op ^ Eff f c)
        _                          -> pure Nil

  -- if the left daughter requests something Functorial, try to find an
  -- `op` that would combine it with a `pure`ified right daughter
  <+> case l of
        Eff f a :-> b  | appl f    -> combine ((a :-> b)^r) <#> map \(op ^ c) -> (UR f op ^ c)
        _                          -> pure Nil

  -- vice versa if the right daughter requests something Functorial
  <+> case r of
        Eff f a :-> b  | appl f    -> combine (l^(a :-> b)) <#> map \(op ^ c) -> (UL f op ^ c)
        _                          -> pure Nil

  -- additionally, if both daughters are Applicative, then see if there's
  -- some mode `op` that would combine their underlying types
  <+> case l,r of
        Eff f a, Eff g b | appl f  -> combine (a^b) <#> lift2 (\h (op ^ c) -> (A h op ^ Eff h c)) (combineFs f g)
        _      , _                 -> pure Nil

  -- if the left daughter is left adjoint to the right, tthen cancel out the
  -- adjoint effects and try to combine the underlying types
  <+> case l,r of
        Eff f a, Eff g b | adjoint f g -> combine (a^b) <#> map \(op ^ c) -> (Eps op ^ c)
        _      , _                     -> pure Nil

  -- finally see if the resulting types can additionally be lowered (D),
  -- joined (J), or canceled out (Eps)
  <**> pure (addD : {-addEps :-} addJ : pure : Nil)

addJ :: (Mode ^ Ty) -> List (Mode ^ Ty)
addJ   (op ^ t) | Eff f (Eff g a) <- t
                , monad f
                  = combineFs f g <#> \h -> (J op ^ Eff h a)
                | otherwise = Nil

-- addEps :: (Mode ^ Ty) -> List (Mode ^ Ty)
-- addEps (op ^ t) | Eff f (Eff g a) <- t
--                 , adjoint f g
--                 , ML _ (MR _ _) <- op
--                   = pure (Eps op ^ a)
--                 | otherwise = Nil

addD :: (Mode ^ Ty) -> List (Mode ^ Ty)
addD   (op ^ t) | Eff (C i a) a' <- t
                , a == a'
                  = pure (D op ^ i)
                | otherwise = Nil

sweepSpurious :: List (Mode ^ Ty) -> List (Mode ^ Ty)
sweepSpurious ops = foldr filter ops
  [
  -- eliminate unit/map duplication (UR,MR == MR,UR)
    \(m ^ _) -> not $ m `contains 0` UR S (MR S FA)
                                   --   ^     ^  the Effects on these modes are
                                   --            ignored by `contains 0`

  -- avoid higher-order detours
  , \(m ^ _) -> not $ any (m `contains 0` _) $

         ( (J:identity:Nil) >>= \k -> (MR:ML:Nil) >>= \m -> pure $ J (m S (k (m S FA))) )
      <> ( (J:identity:Nil) <#> \k -> J (ML S (k (MR S FA))) )
      <> ( (J:identity:Nil) <#> \k -> J (A  S (k (MR S FA))) )
      <> ( (J:identity:Nil) <#> \k -> J (ML S (k (A  S FA))) )

  -- for commutative effects, all Js over ops of the same effect are detours
  , \(m ^ _) -> not $ any (m `contains 2` _) $

         ( commuter <#> \f -> J (MR f    (A  f FA) ) )
      <> ( commuter <#> \f -> J (A  f    (ML f FA) ) )
      <> ( commuter >>= \f -> (J:identity:Nil) >>= \k -> pure $ J (MR f (k (ML f FA))) )
      <> ( commuter >>= \f -> (J:identity:Nil) >>= \k -> pure $ J (A  f (k (A  f FA))) )

  -- avoid higher-order detours given D
  , \(m ^ _) -> not $ any (m `contains 0` _) $

         ( (MR:ML:Nil) <#> \m -> D (m  S (D (m  S FA))) )
      <> ( pure $ D (ML S (D (MR S FA))) )
      <> ( pure $ D (A  S (D (MR S FA))) )
      <> ( pure $ D (ML S (D (A  S FA))) )

  -- eliminate eps/J(D) duplication (Eps,J(D) == J(D),Eps)
  , \(m ^ _) -> not $ any (m `contains 0` _) $

         (        J (Eps FA) )
       : ( pure $ D (Eps FA) )

  -- EXPERIMENTAL: W's only take surface scope over W's
  , \(m ^ _) -> not $ any (m `contains 1` _) $

         ( (ML (W E): A (W E): Eps: Nil) <#> \m -> MR (W E) (m FA) )

  -- EXPERIMENTAL: drefs float up
  , \(m ^ _) -> not $ any (m `contains 1` _) $

         ( scopetakers >>= \f -> (ML f (MR (W E) FA) : MR f (ML (W E) FA) : Nil) )
  ]
  where
    contains n haystack needle = DS.contains (DS.Pattern $ modeAsList n needle) $ modeAsList n haystack
    commuter = filter commutative atomicEffects
    scopetakers = atomicEffects >>= case _ of
      W _ -> Nil
      R _ -> Nil
      x   -> pure x


{- Mapping semantic values to (un-normalized) Lambda_calc terms -}

semTerm :: Sem -> Term
semTerm (Lex w)       = w
semTerm (Comb op l r) = modeTerm op % semTerm l % semTerm r

-- The definitions of the combinators that build our modes of combination
-- Here we are using the Lambda_calc library to write (untyped) lambda expressions
-- that we can display in various forms
modeTerm :: Mode -> Term
modeTerm = case _ of
          -- \l r -> l r
  FA      -> l ! r ! l % r

          -- \l r -> r l
  BA      -> l ! r ! r % l

          -- \l r a -> l a `and` r a
  PM      -> l ! r ! a ! make_var "and'" % (l % a) % (r % a)

          -- \l r a -> l (r a)
  FC      -> l ! r ! a ! l % (r % a)

          -- \l R -> (\a -> op l a) <$> R
  MR f op -> l ! r ! fmap f % (a ! (modeTerm op % l % a)) % r

          -- \L r -> (\a -> op a r) <$> L
  ML f op -> l ! r ! fmap f % (a ! (modeTerm op % a % r)) % l

          -- \l R -> op (\a -> R (pure a)) l
  UL f op -> l ! r ! modeTerm op % (a ! r % (pr f % a)) % l

          -- \L r -> op (\a -> L (pure a)) r
  UR f op -> l ! r ! modeTerm op % (a ! l % (pr f % a)) % r

          -- \L R -> op <$> L <*> R
  A  _ op -> l ! r ! make_var "(<*>)" % (make_var "fmap" % modeTerm op % l) % r

          -- \l r a -> op l (r a) a
  -- Z op    -> l ! r ! a ! modeTerm op % l % (r % a) % a

          -- \l r -> join (op l r)
  J op    -> l ! r ! make_var "join" % (modeTerm op % l % r)

          -- \l r -> counit $ (\a -> op a <$> r) <$> l
  Eps op  -> l ! r ! counit % (fmap (W E) % (a ! fmap (R E) % (modeTerm op % a) % r) % l)

          -- \l r -> op l r id
  D op    -> l ! r ! modeTerm op % l % r % (a ! a)

l = make_var "l"
r = make_var "r"
a = make_var "a"
g = make_var "g"
k = make_var "k"
m = make_var "m"
c = make_var "c"
o = make_var "o"

fmap = case _ of
  S     -> k ! m ! Set (Dom m) (a ! k % (Map m % a))
  R _   -> k ! m ! g ! k % (m % g)
  W _   -> k ! m ! _1 m * (k % (_2 m))
  C _ _ -> k ! m ! c ! m % (a ! c % (k % a))
pr = case _ of
  S     -> a ! Set a (a ! a)
  R _   -> a ! g ! a
  W t   -> a ! (mzero t * a)
  C _ _ -> a ! k ! k % a
mzero = case _ of
  T     -> make_var "true"
  _     -> make_var "this really shouldn't happen"
counit = m ! _2 m % (_1 m)

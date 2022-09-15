{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module TDParseCFG where

import Prelude hiding ( (<>), (^) )
import Control.Monad ( liftM2 )
import Lambda_calc ( Term, make_var, (#), (^) )
import Memo
import Data.Function ( fix )
import Data.Functor ( (<&>) )
import Data.List ( isPrefixOf )


{- Datatypes for syntactic and semantic composition-}

-- some syntactic categories
data Cat
  = CP | Cmp -- Clauses and Complementizers
  | CBar | DBar | Cor -- Coordinators and Coordination Phrases
  | DP | Det | Gen | Dmp -- (Genitive) Determiners and full Determiner Phrases
  | NP | TN -- Transitive (relational) Nouns and full Noun Phrases
  | VP | TV | DV | AV -- Transitive, Ditransitive, and Attitude Verbs and Verb Phrases
  | AdjP | TAdj | Deg | AdvP | TAdv -- Modifiers
  deriving (Eq, Show, Ord {-, Read-})

-- semantic types
infixr :->
data Type
  = E | T           -- Base types
  | Type :-> Type   -- Functions
  | Eff F Type      -- F-ectful
  deriving (Eq, Show, Ord {-, Read-})

-- Effects
data F = S | R Type | W Type | C Type Type
  deriving (Eq, Show, Ord {-, Read-})

showNoIndices :: F -> String
showNoIndices = \case
  S     -> "S"
  R _   -> "R"
  W _   -> "W"
  C _ _ -> "C"

-- convenience constructors
effS     = Eff S
effR r   = Eff (R r)
effW w   = Eff (W w)
effC r o = Eff (C r o)

-- evaluated types (for scope islands)
-- we care about positive positions only
evaluated :: Type -> Bool
evaluated = \case
  E             -> True
  T             -> True
  _ :-> a       -> evaluated a
  Eff S a       -> evaluated a
  Eff (R _) a   -> evaluated a
  Eff (W _) a   -> evaluated a
  Eff (C _ _) _ -> False


{- Syntactic parsing -}

-- A (binary-branching) grammar is a list of production rules, telling
-- you what new categories you can build out of ones you are handed
type CFG = Cat -> Cat -> [Cat]

-- Our syntactic objects are (binary-branching) constituency trees with
-- typed leaves
data Syn
  = Leaf String Type
  | Branch Syn Syn
  | Island Syn Syn -- Scope islands
  deriving (Eq, Show, Ord)

-- Phrases to be parsed are lists of "signs" whose various morphological
-- spellouts, syntactic categories, and types are known
type Sense = (String, Cat, Type) -- a single sense of a single word
type Sign = [Sense]              -- a word may have several senses
type Phrase = [Sign]
type Lexicon = [(String, Word)]

-- a simple memoized chart parser, parameterized by a CFG
protoParse ::
  Monad m
  => CFG
  -> ((Int, Int, Phrase) -> m [(Cat, Syn)])
  ->  (Int, Int, Phrase) -> m [(Cat, Syn)]
protoParse _   _ (_, _, [sign]) = return [(c, Leaf s t) | (s, c, t) <- sign]
protoParse cfg parser phrase = concat <$> mapM help (bisect phrase)
  where
    bisect (lo, hi, u) = do
      i <- [1 .. length u - 1]
      let (ls, rs) = splitAt i u
      let break = lo + i
      return ((lo, break - 1, ls), (break, hi, rs))

    help (ls, rs) = do
      parsesL <- parser ls
      parsesR <- parser rs
      return $
        [ (cat, mkIsland cat lsyn rsyn)
        | (lcat, lsyn) <- parsesL
        , (rcat, rsyn) <- parsesR
        , cat <- cfg lcat rcat
        ]

    mkIsland CP = Island
    mkIsland _  = Branch

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

type Mode = [Op]
data Op
  = FA | BA | PM | FC -- Base        > < & .
  | MR | ML           -- Functor     fmap
  | UR | UL           -- Applicative pure
  | A                 -- Applicative <*>
  | J                 -- Monad       join
  | Eps               -- Adjoint     counit
  | D                 -- Cont        lower
  deriving (Eq)

instance Show Op where
  show = \case
    FA  -> ">"
    BA  -> "<"
    PM  -> "&"
    FC  -> "."
    MR  -> "R"
    ML  -> "L"
    UL  -> "UL"
    UR  -> "UR"
    A   -> "A"
    J   -> "J"
    Eps -> "Eps"
    D   -> "D"


{- Type classes -}

-- All our Effects are Functors, and all but (W E) are
-- indeed Applicative and Monadic
-- The only adjunction we demonstrate is that between W and R
functor, appl, monad :: F -> Bool
functor _    = True
appl f@(W w) = functor f && monoid w
appl f       = functor f && True
monad f      = appl f && True

monoid :: Type -> Bool
monoid T = True
monoid _ = False

adjoint :: F -> F -> Bool
adjoint (W i) (R j) = i == j
adjoint _ _         = False

class Commute f where
  commutative :: f -> Bool
instance Commute Type where
  -- commutative as a monoid
  commutative ty = ty == T
instance Commute F where
  -- commutative as a monad
  commutative = \case
    S     -> True
    R _   -> False
    W w   -> commutative w
    C _ _ -> False


{- Type-driven combination -}

-- A semantic derivation is a proof that a particular string has a
-- particular meaning at a particular type
-- For displaying derivations, we also maintain the subproofs used at
-- each proof step
data Proof = Proof String Sem Type [Proof]
  deriving (Show)

getProofType :: Proof -> Type
getProofType (Proof _ _ ty _) = ty

-- Evaluate a constituency tree by finding all the derivations of its
-- daughters and then all the ways of combining those derivations in accordance
-- with their types and the available modes of combination
synsem :: Syn -> [Proof]
synsem = execute . go
  where
    go = \case
      Leaf   s   ty -> return [Proof s (Lex s) ty []]
      Branch l r    -> goWith id l r
      Island l r    -> goWith (filter $ evaluated . snd) l r

    goWith f l r = do -- memo block
      lefts  <- go l
      rights <- go r
      fmap concat $ sequence do -- list block
        lp@(Proof lstr lval lty _) <- lefts
        rp@(Proof rstr rval rty _) <- rights
        return do -- memo block
          combos <- combineWith id lty rty
          return do -- list block
            (op, ty) <- combos
            return $ Proof (lstr ++ " " ++ rstr) (Comb op lval rval) ty [lp, rp]


-- The basic unEffectful modes of combination (add to these as you like)
modes :: Type -> Type -> [(Mode, Type)]
modes = curry \case
  (a :-> b , r      ) | a == r -> [([FA], b)]
  (l       , a :-> b) | l == a -> [([BA], b)]
  (a :-> T , b :-> T) | a == b -> [([PM], a :-> T)]
  (_       , _      )          -> []

-- Make sure that two Effects can compatibly be sequenced
-- (only relevant to A and J modes)
combineFs :: F -> F -> [F]
combineFs = curry \case
  (S    , S     )           -> [S]
  (R i  , R j   ) | i == j  -> [R i]
  (W i  , W j   ) | i == j  -> [W i]
  (C i j, C j' k) | j == j' -> [C i k]
  _                         -> []

-- combine = combineWith id
combineWith f = curry $ fix $ memoize' . ((f <$>) .) . openCombine

-- Here is the essential type-driven combination logic; given two types,
-- what are all the ways that they may be combined
openCombine ::
  Monad m
  => ((Type, Type) -> m [(Mode, Type)])
  ->  (Type, Type) -> m [(Mode, Type)]
openCombine combine (l, r) = concat <$>

  -- for starters, try the basic modes of combination
  return (modes l r)

  -- then if the left daughter is Functorial, try to find a mode
  -- `op` that would combine its underlying type with the right daughter
  <+> case l of
    Eff f a
      | functor f -> combine (a,r) <&> map \(op,c) -> (ML:op, Eff f c)
    _ -> return []

  -- vice versa if the right daughter is Functorial
  <+> case r of
    Eff f a
      | functor f -> combine (l,a) <&> map \(op,c) -> (MR:op, Eff f c)
    _ -> return []

  -- if the left daughter requests something Functorial, try to find an
  -- `op` that would combine it with a `pure`ified right daughter
  <+> case l of
    Eff f a :-> b
      | appl f ->
        combine ((a :-> b),r) <&>
        concatMap \(op,c) -> [(UR:op, c) | normU UR op]
    _ -> return []

  -- vice versa if the right daughter requests something Functorial
  <+> case r of
    Eff f a :-> b
      | appl f ->
        combine (l,(a :-> b)) <&>
        concatMap \(op,c) -> [(UL:op, c) | normU UL op]
    _ -> return []

  -- additionally, if both daughters are Applicative, then see if there's
  -- some mode `op` that would combine their underlying types
  <+> case (l,r) of
    (Eff f a, Eff g b)
      | appl f ->
        combine (a, b) <&>
        liftM2 (\h (op,c) -> (A:op, Eff h c)) (combineFs f g)
    _ -> return []

  -- canonical Eps configuration is Eps, ML, MR
  -- rules out xover, forces Eps to apply low
  -- there remains some derivational ambiguity:
  -- W,W,R,R has 3 all-cancelling derivations not 2 due to local WR/RW ambig
  <+> case (l,r) of
    (Eff f a, Eff g b)
      | adjoint f g ->
        combine (a, b) <&> map \(op,c) -> (Eps:op, c)
    _ -> return []

  -- finally see if the resulting types can additionally be lowered (D),
  -- joined (J), or canceled out (Eps)
  <**> return [addD, addJ, return]

  where
    infixr 6 <+>
    (<+>) = liftM2 (++)
    infixl 5 <**>
    (<**>) = liftM2 (flip (<*>))

    -- prefer M_,(D,)U_ over the equivalent U_,(D,)M_
    normU = \case
      UR -> \op -> not $ any (`isPrefixOf` op) [[MR], [D,MR]]
      UL -> \op -> not $ any (`isPrefixOf` op) [[ML], [D,ML]]
      _  -> const True

addJ :: (Mode, Type) -> [(Mode, Type)]
addJ =
  \case
    (op, Eff f (Eff g a))
      | monad f
      , normJ f op -> [(J:op, Eff h a) | h <- combineFs f g]
    _ -> []

  where
    normJ f op = not $ any (`isPrefixOf` op) $

      -- avoid higher-order detours for all J-able effects
         [ [m]  ++ k ++ [m]  | k <- [[J], []], m <- [MR, ML] ]
      ++ [ [ML] ++ k ++ [MR] | k <- [[J], []] ]
      ++ [ [A]  ++ k ++ [MR] | k <- [[J], []] ]
      ++ [ [ML] ++ k ++ [A]  | k <- [[J], []] ]
      ++ [ [Eps] ]

      -- for commutative effects
      ++ [ [MR     ,     A ] | commutative f ]
      ++ [ [A      ,     ML] | commutative f ]
      ++ [ [MR] ++ k ++ [ML] | commutative f, k <- [[J], []] ]
      ++ [ [A]  ++ k ++ [A]  | commutative f, k <- [[J], []] ]

addD :: (Mode, Type) -> [(Mode, Type)]
addD =
  \case
    (op, Eff (C i a) a')
      | a == a'
      , normD op -> [(D:op, i)]
    _ -> []

  where
    normD op = not $ any (`isPrefixOf` op) $
         [ [m , D, m ] | m <- [MR, ML] ]
      ++ [ [ML, D, MR]
         , [A , D, MR]
         , [ML, D, A ]
         , [Eps]
         ]


{- Mapping semantic values to (un-normalized) Lambda_calc terms -}

semTerm :: Sem -> Term
semTerm (Lex w)      = make_var (w ++ "'")
semTerm (Comb m l r) = modeTerm m # semTerm l # semTerm r

modeTerm :: Mode -> Term
modeTerm [op] = opTerm op
modeTerm (x:xs) = opTerm x # modeTerm xs
modeTerm _ = error "impossible"

-- The definitions of the combinators that build our modes of combination
-- Here we are using the Lambda_calc library to write (untyped) lambda expressions
-- that we can display in various forms
opTerm :: Op -> Term
opTerm = \case
      -- \l r -> l r
  FA  -> l ^ r ^ l # r

      -- \l r -> r l
  BA  -> l ^ r ^ r # l

      -- \l r a -> l a && r a
  PM  -> l ^ r ^ a ^ make_var "and'" # (l # a) # (r # a)

      -- \l r a -> l (r a)
  FC  -> l ^ r ^ a ^ l # (r # a)

      -- \op l r -> (\a -> op l a) <$> r
  MR  -> op ^ l ^ r ^ make_var "fmap" # (a ^ (op # l # a)) # r

      -- \op l r -> (\a -> op a r) <$> l
  ML  -> op ^ l ^ r ^ make_var "fmap" # (a ^ (op # a # r)) # l

      -- \op l r -> op (\a -> r (pure a)) l
  UL  -> op ^ l ^ r ^ op # (a ^ r # (make_var "pure" # a)) # l

      -- \op l r -> op (\a -> l (pure a)) r
  UR  -> op ^ l ^ r ^ op # (a ^ l # (make_var "pure" # a)) # r

      -- \op l l -> op <$> l <*> r
  A   -> op ^ l ^ r ^ make_var "(<*>)" # (make_var "fmap" # op # l) # r

      -- \l r -> join (op l r)
  J   -> op ^ l ^ r ^ make_var "join" # (op # l # r)

      -- \l r -> counit (op l r)
  Eps -> op ^ l ^ r ^ make_var "counit" # (op # l # r)

      -- \l r -> op l r id
  D   -> op ^ l ^ r ^ op # l # r # (a ^ a)

  where
    l  = make_var "l"
    r  = make_var "r"
    a  = make_var "a"
    op = make_var "op"

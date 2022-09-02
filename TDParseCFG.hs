{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module TDParseCFG where

import Prelude hiding ((<>), (^), and)
import Control.Monad (join, liftM2)
import Lambda_calc ( Term, make_var, (#), (^) )
import Memo
import Data.Function ( (&), fix )
import Data.Functor ( (<&>) )
import Data.List ( isInfixOf )


{- Datatypes for syntactic and semantic composition-}

-- some syntactic categories
data Cat
  = CP | Cmp -- Clauses and Complementizers
  | CBar | DBar | Cor -- Coordinators and Coordination Phrases
  | DP | Det | Gen -- (Genitive) Determiners and full Determiner Phrases
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
data F = S | R Type | W Type | C Type Type | U
  deriving (Show, Ord {-, Read-})

instance Eq F where
  U == _ = True
  _ == U = True
  S == S = True
  R t == R u = t == u
  W t == W u = t == u
  C t u == C v w = t == v && u == w
  _ == _ = False

showNoIndices :: F -> String
showNoIndices = \case
  S     -> "S"
  R _   -> "R"
  W _   -> "W"
  C _ _ -> "C"

atomicTypes = [E,T]
atomicEffects =
  [S] ++ (R <$> atomicTypes) ++ (W <$> atomicTypes) ++ (liftM2 C atomicTypes atomicTypes)

-- convenience constructors
effS     = Eff S
effR r   = Eff (R r)
effW w   = Eff (W w)
effC r o = Eff (C r o)


{- Syntactic parsing -}

-- A (binary-branching) grammar is a list of production rules, telling
-- you what new categories you can build out of ones you are handed
type CFG = Cat -> Cat -> [Cat]

-- Our syntactic objects are (binary-branching) constituency trees with
-- typed leaves
data Syn
  = Leaf String Type
  | Branch Syn Syn
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

type Mode = [Op]
data Op
  = FA | BA | PM | FC -- Base        > < & .
  | MR F | ML F       -- Functor     fmap
  | UR F | UL F       -- Applicative pure
  | A F               -- Applicative <*>
  | J                 -- Monad       join
  | Eps               -- Adjoint     counit
  | D                 -- Cont        lower
  deriving (Eq)

instance Show Op where
  show = showOp 0

showOp :: Int -> Op -> String
showOp v = \case
  FA   -> ">"
  BA   -> "<"
  PM   -> "&"
  FC   -> "."
  MR f -> "R"  ++ showF f
  ML f -> "L"  ++ showF f
  UL f -> "UL" ++ showF f
  UR f -> "UR" ++ showF f
  A  f -> "A"  ++ showF f
  J    -> "J"
  Eps  -> "Eps"
  D    -> "D"
  where
    showF =
      case v of
        0 -> const ""      -- just the unparameterized combinators
        1 -> showNoIndices -- the combinators parameterized by Effect constructor
        _ -> show          -- the combinators parameterized by full indexed Effect type


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
    go (Leaf s t)   = return [Proof s (Lex s) t []]
    go (Branch l r) = do -- memo block
      lefts  <- go l
      rights <- go r
      fmap concat $ sequence do -- list block
        lp@(Proof lstr lval lty _) <- lefts
        rp@(Proof rstr rval rty _) <- rights
        return do -- memo block
          combos <- combine lty rty
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

combine = curry $ fix (memoize' . openCombine)

-- Here is the essential type-driven combination logic; given two types,
-- what are all the ways that they may be combined
openCombine ::
  Monad m
  => ((Type, Type) -> m [(Mode, Type)])
  ->  (Type, Type) -> m [(Mode, Type)]
openCombine combine (l, r) = sweepSpurious . concat <$>

  -- for starters, try the basic modes of combination
  return (modes l r)


  -- then if the left daughter is Functorial, try to find a mode
  -- `op` that would combine its underlying type with the right daughter
  <+> case l of
        Eff f a         | functor f -> combine (a,r) <&> map \(op,c) -> (ML f : op, Eff f c)
        _                           -> return []

  -- vice versa if the right daughter is Functorial
  <+> case r of
        Eff f a         | functor f -> combine (l,a) <&> map \(op,c) -> (MR f : op, Eff f c)
        _                           -> return []

  -- if the left daughter requests something Functorial, try to find an
  -- `op` that would combine it with a `pure`ified right daughter
  <+> case l of
        Eff f a :-> b   | appl f    -> combine ((a :-> b),r) <&> map \(op,c) -> (UR f : op, c)
        _                           -> return []

  -- vice versa if the right daughter requests something Functorial
  <+> case r of
        Eff f a :-> b   | appl f    -> combine (l,(a :-> b)) <&> map \(op,c) -> (UL f : op, c)
        _                           -> return []

  -- additionally, if both daughters are Applicative, then see if there's
  -- some mode `op` that would combine their underlying types
  <+> case (l, r) of
        (Eff f a, Eff g b) | appl f -> combine (a,b) <&> liftM2 (\h (op,c) -> (A h : op, Eff h c)) (combineFs f g)
        _                           -> return []

  -- this is only if you want to see some derivations in the Variable-Free style
  -- <+> case (l, r) of
  --       (a :-> i :-> b, Eff (R r) c) | i == r -> combine ((a :-> b),c) <&> map \(op,d) -> (Z op, i :-> d)
  --       _                                     -> return []

  -- finally see if the resulting types can additionally be lowered (D),
  -- joined (J), or canceled out (Eps)
  <**> return [addD, addEps, addJ, return]

  where infixr 6 <+>
        (<+>) = liftM2 (++)
        infixl 5 <**>
        (<**>) = liftM2 (flip (<*>))

addJ :: (Mode, Type) -> [(Mode, Type)]
addJ = \case
  (op, Eff f (Eff g a)) | monad f -> [(J : op, Eff h a) | h <- combineFs f g]
  _                               -> []

addEps :: (Mode, Type) -> [(Mode, Type)]
addEps = \case
  (op, Eff f (Eff g a)) | adjoint f g -> [(Eps : op, a)]
  _                                   -> []

addD :: (Mode, Type) -> [(Mode, Type)]
addD = \case
  (op, Eff (C i a) a') | a == a' -> [(D : op, i)]
  _                              -> []

sweepSpurious :: [(Mode, Type)] -> [(Mode, Type)]
sweepSpurious ops = foldr filter ops
  [
  -- eliminate unit/map duplication (UR,(D,)MR == MR,(D,)UR)
    \(m,_) -> not $ any (m `contains`) $

         [ [UR U] ++ k ++ [ MR U] | k <- [[D], []] ]

  -- avoid higher-order detours
  , \(m,_) -> not $ any (m `contains`) $

         [ [J, m  U] ++ k ++ [m  U] | k <- [[J], []], m <- [MR, ML] ]
      ++ [ [J, ML U] ++ k ++ [MR U] | k <- [[J], []] ]
      ++ [ [J, A  U] ++ k ++ [MR U] | k <- [[J], []] ]
      ++ [ [J, ML U] ++ k ++ [A  U] | k <- [[J], []] ]

  -- for commutative effects, all Js over ops of the same effect are detours
  , \(m,_) -> not $ any (m `contains`) $

         [ [J, MR f,          A  f] | f <- commuter ]
      ++ [ [J, A  f,          ML f] | f <- commuter ]
      ++ [ [J, MR f] ++ k ++ [ML f] | f <- commuter, k <- [[J], []] ]
      ++ [ [J, A  f] ++ k ++ [A  f] | f <- commuter, k <- [[J], []] ]

  -- avoid higher-order detours given D
  , \(m,_) -> not $ any (m `contains`) $

         [ [D, m  U, D, m  U] | m <- [MR, ML] ]
      ++ [ [D, ML U, D, MR U]
         , [D, A  U, D, MR U]
         , [D, ML U, D, A  U] ]

  -- canonical Eps configuration is Eps (ML _ (MR _ ...))
  -- disallowing Eps (MR u ...) forces Eps to apply as low as possible
  -- (R cannot have a postponed W effect), also rules out xover (forcing W to
  -- be drawn from L)
  , \(m,_) -> not $ m `contains` [Eps, MR U]
  , \(m,_) -> not $ m `contains` [Eps, ML U, ML U]
  -- there remains some derivational ambiguity for some readings:
  -- WR a + R b ~ RW a + R b
  ]
  where
    contains haystack needle = needle `isInfixOf` haystack
    commuter = filter commutative atomicEffects


{- Mapping semantic values to (un-normalized) Lambda_calc terms -}

semTerm :: Sem -> Term
semTerm (Lex w)      = make_var (w ++ "'")
semTerm (Comb m l r) = modeTerm m # semTerm l # semTerm r

-- The definitions of the combinators that build our modes of combination
-- Here we are using the Lambda_calc library to write (untyped) lambda expressions
-- that we can display in various forms
opTerm :: Op -> Term
opTerm = \case
       -- \l r -> l r
  FA   -> l ^ r ^ l # r

       -- \l r -> r l
  BA   -> l ^ r ^ r # l

       -- \l r a -> l a && r a
  PM   -> l ^ r ^ a ^ make_var "and'" # (l # a) # (r # a)

       -- \l r a -> l (r a)
  FC   -> l ^ r ^ a ^ l # (r # a)

       -- \op l r -> (\a -> op l a) <$> r
  MR _ -> op ^ l ^ r ^ make_var "fmap" # (a ^ (op # l # a)) # r

       -- \op l r -> (\a -> op a r) <$> l
  ML _ -> op ^ l ^ r ^ make_var "fmap" # (a ^ (op # a # r)) # l

       -- \op l r -> op (\a -> r (pure a)) l
  UL _ -> op ^ l ^ r ^ op # (a ^ r # (make_var "pure" # a)) # l

       -- \op l r -> op (\a -> l (pure a)) r
  UR _ -> op ^ l ^ r ^ op # (a ^ l # (make_var "pure" # a)) # r

       -- \op l l -> op <$> l <*> r
  A  _ -> op ^ l ^ r ^ make_var "(<*>)" # (make_var "fmap" # op # l) # r

       -- \l r -> join (op l r)
  J    -> op ^ l ^ r ^ make_var "join" # (op # l # r)

       -- \l r -> counit (op l r)
  Eps  -> op ^ l ^ r ^ make_var "counit" # (op # l # r)

       -- \l r -> op l r id
  D    -> op ^ l ^ r ^ op # l # r # (a ^ a)

  where
    l  = make_var "l"
    r  = make_var "r"
    a  = make_var "a"
    op = make_var "op"

modeTerm :: Mode -> Term
modeTerm [op] = opTerm op
modeTerm (x:xs) = opTerm x # modeTerm xs

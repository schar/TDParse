{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module TDParseCFG where

import Prelude hiding ( (<>), (^), Word, (*) )
import Control.Monad ( join, liftM2 )
import LambdaCalc (Term, eval, make_var, make_con, (!), (%), _1, _2, (*), (|?), set, get_dom, get_rng, make_set, conc)
import Memo
import Data.Function ( fix )
import Data.Functor ( (<&>) )
import Data.List ( isPrefixOf, stripPrefix )


{- Datatypes for syntactic and semantic composition-}

-- some syntactic categories
data Cat
  = CP | Cmp -- Clauses and Complementizers
  | CBar | DBar | Cor -- Coordinators and Coordination Phrases
  | DP | Det | Gen | GenD | Dmp -- (Genitive) Determiners and full Determiner Phrases
  | NP | TN -- Transitive (relational) Nouns and full Noun Phrases
  | VP | TV | DV | AV -- Transitive, Ditransitive, and Attitude Verbs and Verb Phrases
  | AdjP | TAdj | Deg | AdvP | TAdv -- Modifiers
  deriving (Eq, Show, Ord {-, Read-})

-- semantic types
infixr :->
data Ty
  = E | T       -- Base types
  | Ty :-> Ty   -- Functions
  | Eff F Ty    -- F-ectful
  deriving (Eq, Show, Ord {-, Read-})

-- Effects
data F = S | R Ty | W Ty | C Ty Ty | U
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
  U     -> "_"

-- convenience constructors
effS     = Eff S
effR r   = Eff (R r)
effW w   = Eff (W w)
effC r o = Eff (C r o)

-- evaluated types (for scope islands)
-- we care about positive positions only
evaluated :: Ty -> Bool
evaluated = \case
  E             -> True
  T             -> True
  _ :-> a       -> evaluated a
  Eff (C _ _) _ -> False
  Eff _ a       -> evaluated a


{- Syntactic parsing -}

-- A (binary-branching) grammar is a list of production rules, telling
-- you what new categories you can build out of ones you are handed
type CFG = Cat -> Cat -> [Cat]

-- Our syntactic objects are (binary-branching) constituency trees with
-- typed leaves
data Syn
  = Leaf String Term Ty
  | Branch Syn Syn
  | Island Syn Syn -- Scope islands
  deriving (Eq, Show, Ord)

-- Phrases to be parsed are lists of "signs" whose various morphological
-- spellouts, syntactic categories, and types are known
type Sense = (Term, Cat, Ty) -- a single sense of a single word
type Word = (String, [Sense])              -- a word may have several senses
type Phrase = [Word]
type Lexicon = [Word]

-- a simple memoized chart parser, parameterized by a CFG
protoParse ::
  Monad m
  => CFG
  -> ((Int, Int, Phrase) -> m [(Cat, Syn)])
  ->  (Int, Int, Phrase) -> m [(Cat, Syn)]
protoParse _   _ (_, _, [(s,sign)]) = return [(c, Leaf s d t) | (d, c, t) <- sign]
protoParse cfg parse phrase             = concat <$> mapM help (bisect phrase)
  where
    bisect (lo, hi, u) = do
      i <- [1 .. length u - 1]
      let (ls, rs) = splitAt i u
      let break = lo + i
      return ((lo, break - 1, ls), (break, hi, rs))

    help (ls, rs) = do
      parsesL <- parse ls
      parsesR <- parse rs
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
parse :: CFG -> Lexicon -> String -> Maybe [Syn]
parse cfg dict input = do
  let lexes = filter (/= "") $ words input >>= stripClitics
  ws <- mapM (\s -> (s,) <$> lookup s dict) lexes
  return $ snd <$> memo (protoParse cfg) (0, length ws - 1, ws)
  where
    stripClitics s = clitics >>= \c -> maybe [s] ((:[c]) . reverse) $ stripPrefix (reverse c) (reverse s)
    clitics = ["'s"]

-- A semantic object is either a lexical entry or a mode of combination applied to
-- two other semantic objects
data Sem
  = Lex Term
  | Comb Mode Term
  deriving (Show)

-- Modes of combination

type Mode = [Op]
data Op
  = FA | BA | PM | FC -- Base        > < & .
  | MR F | ML F       -- Functor     fmap
  | UR F | UL F       -- Applicative pure
  | A F               -- Applicative <*>
  | J F               -- Monad       join
  | Eps               -- Adjoint     counit
  | D                 -- Cont        lower
  | XL F Op           -- Comonad     extend
  deriving (Eq)

instance Show Op where
  show = \case
    FA     -> ">"
    BA     -> "<"
    PM     -> "&"
    FC     -> "."
    MR _   -> "R"
    ML _   -> "L"
    UL _   -> "UL"
    UR _   -> "UR"
    A  _   -> "A"
    J  _   -> "J"
    Eps    -> "Eps"
    D      -> "D"
    XL f o -> "XL " ++ show o


{- Type classes -}

-- All our Effects are Functors, and all but (W E) are
-- indeed Applicative and Monadic
-- The only adjunction we demonstrate is that between W and R
functor, appl, monad :: F -> Bool
functor _    = True
appl f@(W w) = functor f && monoid w
-- appl (R E)   = False
appl f       = functor f && True
monad f      = appl f && True

monoid :: Ty -> Bool
monoid T = True
monoid _ = False

adjoint :: F -> F -> Bool
adjoint (W i) (R j) = i == j
adjoint _ _         = False

class Commute f where
  commutative :: f -> Bool
instance Commute Ty where
  -- commutative as a monoid
  commutative ty = ty == T
instance Commute F where
  -- commutative as a monad
  commutative = \case
    S     -> True
    R _   -> True
    W w   -> commutative w
    C _ _ -> False
    U     -> False

invertible :: F -> Bool
invertible (W _) = False
invertible _     = True

{- Type-driven combination -}

-- A semantic derivation is a proof that a particular string has a
-- particular meaning at a particular type
-- For displaying derivations, we also maintain the subproofs used at
-- each proof step
data Proof = Proof String Sem Ty [Proof]
  deriving (Show)

hasType :: Ty -> Proof -> Bool
hasType t (Proof _ _ t' _) = t == t'

-- Evaluate a constituency tree by finding all the derivations of its
-- daughters and then all the ways of combining those derivations in accordance
-- with their types and the available modes of combination
synsem :: Syn -> [Proof]
synsem = execute . go
  where
    go = \case
      Leaf   s d t -> return [Proof s (Lex d) t []]
      Branch l r   -> goWith False id l r
      Island l r   -> goWith True (filter $ \(_,_,t) -> evaluated t) l r
      -- boolean tags for islandhood, to avoid over-memoizing

    goWith tag handler l r = do -- memo block
      lefts  <- go l
      rights <- go r
      concat <$> sequence do -- list block
        lp@(Proof lstr lval lty _) <- lefts
        rp@(Proof rstr rval rty _) <- rights
        return do -- memo block
          combos <- combineWith tag handler lty rty
          return do -- list block
            (op, d, ty) <- combos
            let cval = Comb op (eval $ d % semTerm lval % semTerm rval)
            return $ Proof (lstr ++ " " ++ rstr) cval ty [lp, rp]

prove :: CFG -> Lexicon -> String -> Maybe [Proof]
prove cfg lex input = concatMap synsem <$> parse cfg lex input


-- The basic unEffectful modes of combination (add to these as you like)
modes :: Ty -> Ty -> [(Mode, Term, Ty)]
modes = curry \case
  (,) (a :-> b) r         | a == r -> [([FA], opTerm FA, b)]
  (,) l         (a :-> b) | l == a -> [([BA], opTerm BA, b)]
  (,) (a :-> T) (b :-> T) | a == b -> [([PM], opTerm PM, a :-> T)]
  (,) _         _                  -> []

-- Make sure that two Effects can compatibly be sequenced
-- (only relevant to A and J modes)
combineFs :: F -> F -> [F]
combineFs = curry \case
  (,) S       S                  -> [S]
  (,) (R i)   (R j)    | i == j  -> [R i]
  (,) (W i)   (W j)    | i == j  -> [W i]
  (,) (C i j) (C j' k) | j == j' -> [C i k]
  (,) _        _                 -> []

-- combine = combineWith ((), id)
combineWith tag handler =
  curry $ fix $ memoizeTag tag . ((handler <$>) .) . openCombine

-- Here is the essential type-driven combination logic; given two types,
-- what are all the ways that they may be combined
openCombine ::
  Monad m
  => ((Ty, Ty) -> m [(Mode, Term, Ty)])
  ->  (Ty, Ty) -> m [(Mode, Term, Ty)]
openCombine combine (l, r) = map (\(m,d,t) -> (m, eval d, t)) . concat <$>

  -- for starters, try the basic modes of combination
  return (modes l r)

  -- then if the left daughter is Functorial, try to find a mode
  -- `op` that would combine its underlying type with the right daughter
  <+> case l of
    Eff f a | functor f ->
      combine (a,r) <&>
      map \(op,d,c) -> (ML f:op, opTerm (ML f) % d, Eff f c)
    _ -> return []

  -- vice versa if the right daughter is Functorial
  <+> case r of
    Eff f a | functor f ->
      combine (l,a) <&>
      concatMap \(op,d,c) -> let m = MR f
                              in [(m:op, opTerm m % d, Eff f c) | norm op m]
    _ -> return []

  -- if the left daughter requests something Functorial, try to find an
  -- `op` that would combine it with a `pure`ified right daughter
  <+> case l of
    Eff f a :-> b | appl f ->
      combine (a :-> b,r) <&>
      concatMap \(op,d,c) -> let m = UR f
                              in [(m:op, opTerm m % d, c) | norm op m]
    _ -> return []

  -- vice versa if the right daughter requests something Functorial
  <+> case r of
    Eff f a :-> b | appl f ->
      combine (l,a :-> b) <&>
      concatMap \(op,d,c) -> let m = UL f
                              in [(m:op, opTerm m % d, c) | norm op m]
    _ -> return []

  -- additionally, if both daughters are Applicative, then see if there's
  -- some mode `op` that would combine their underlying types
  <+> case (l,r) of
    (Eff f a, Eff g b) | appl f ->
      combine (a, b) <&>
      liftM2 (\h (op,d,c) -> let m = A h
                              in (m:op, opTerm m % d, Eff h c)) (combineFs f g)
    _ -> return []

  -- if the left daughter is left adjoint to the right daughter, cancel them out
  -- and fina a mode `op` that will combine their underlying types
  -- note that the asymmetry of adjunction rules out xover
  -- there remains some derivational ambiguity:
  -- W,W,R,R has 3 all-cancelling derivations not 2 due to local WR/RW ambig
  -- also the left arg of Eps is guaranteed to be comonadic, so extend it
  <+> case (l,r) of
    (Eff f a, Eff g b) | adjoint f g ->
      combine (a, b) <&>
      concatMap \(op,d,c) -> do (m,eff) <- [(Eps, id), (XL f Eps, Eff f)]
                                return (m:op, opTerm m % d, eff c)
    _ -> return []

  -- finally see if the resulting types can additionally be lowered/joined
  <**> return [addD, addJ, return]

  where
    infixr 6 <+>
    (<+>) = liftM2 (++)
    infixl 5 <**>
    (<**>) = liftM2 (flip (<*>))

addJ :: (Mode, Term, Ty) -> [(Mode, Term, Ty)]
addJ = \case
  (op, d, Eff f (Eff g a)) | monad f, norm op (J f) ->
    [(J h:op, opTerm (J h) % d, Eff h a) | h <- combineFs f g]
  _ -> []

addD :: (Mode, Term, Ty) -> [(Mode, Term, Ty)]
addD = \case
  (op, d, Eff (C i a) a') | a == a', norm op D ->
    [(D:op, opTerm D % d, i)]
  _ -> []

-- these filters prevent generating "spurious" derivations, which are
-- guaranteed to be equivalent to other derivations we're already generating
norm op = \case
  -- prefer M_,(D,)U_ over the equivalent U_,(D,)M_
  UR f -> not $ (op `startsWith`) `anyOf` [[MR f], [D, MR f]]
  UL f -> not $ (op `startsWith`) `anyOf` [[ML f], [D, ML f]]
  D    -> not $ (op `startsWith`) `anyOf`
       [ [m U, D, m U] | m <- [MR, ML] ]
    ++ [ [ML U, D, MR U]
       , [A U, D, MR U]
       , [ML U, D, A U]
       , [Eps]
       ]
  J f -> not $ (op `startsWith`) `anyOf`
    -- avoid higher-order detours for all J-able effects
       [ [m f]  ++ k ++ [m f]  | k <- [[J f], []], m <- [MR, ML] ]
    ++ [ [ML f] ++ k ++ [MR f] | k <- [[J f], []] ]
    ++ [ [A f]  ++ k ++ [MR f] | k <- [[J f], []] ]
    ++ [ [ML f] ++ k ++ [A f]  | k <- [[J f], []] ]
    ++ [           k ++ [Eps]  | k <- [[A f], []] ] -- safe if no lexical FRFs
    -- and all (non-split) inverse scope for commutative effects
    ++ [ [MR f   ,     A  f]   | commutative f ]
    ++ [ [A f    ,     ML f]   | commutative f ]
    ++ [ [MR f] ++ k ++ [ML f] | commutative f, k <- [[J f], []] ]
    ++ [ [A f]  ++ k ++ [A f]  | commutative f, k <- [[J f], []] ]

  _ -> True

  where
    infixl 4 `anyOf`
    anyOf = any
    startsWith = flip isPrefixOf

-- control inversions
invertOk op =
  \case
    MR f -> not ([ML f] `isPrefixOf` op) || invertible f
    _ -> True

{- Mapping semantic values to (un-normalized) lambda terms -}

semTerm :: Sem -> Term
semTerm (Lex w)    = w
semTerm (Comb m d) = d

modeTerm :: Mode -> Term
modeTerm [op] = opTerm op
modeTerm (x:xs) = opTerm x % modeTerm xs
modeTerm _ = error "impossible"

-- The definitions of the combinators that build our modes of combination
-- Here we are using the LambdaCalc library to write (untyped) lambda expressions
-- that we can display in various forms
opTerm :: Op -> Term
opTerm = \case
          -- \l r -> l r
  FA      -> l ! r ! l % r

          -- \l r -> r l
  BA      -> l ! r ! r % l

          -- \l r a -> l a `and` r a
  PM      -> l ! r ! a ! make_con "and" % (l % a) % (r % a)

          -- \l r a -> l (r a)
  FC      -> l ! r ! a ! l % (r % a)

          -- \l R -> (\a -> op l a) <$> R
  MR f -> op ! l ! r ! fmapTerm f % (a ! (op % l % a)) % r

       --    \L r -> (\a -> op a r) <$> L
  ML f -> op ! l ! r ! fmapTerm f % (a ! (op % a % r)) % l

       --    \l R -> op (\a -> R (pure a)) l
  UL f -> op ! l ! r ! op % (a ! r % (pureTerm f % a)) % l

       --    \L r -> op (\a -> L (pure a)) r
  UR f -> op ! l ! r ! op % (a ! l % (pureTerm f % a)) % r

       --    \L R -> op <$> L <*> R
  A  f -> op ! l ! r ! joinTerm f % (fmapTerm f % (a ! fmapTerm f % (op % a) % r) % l)

       --    \l r a -> op l (r a) a
  -- Z    op ! -> l ! r ! a ! modeTerm op % l % (r % a) % a

       --    \l r -> join (op l r)
  J  f -> op ! l ! r ! joinTerm f % (op % l % r)

          -- \l r -> counit $ (\a -> op a <$> r) <$> l
  Eps  -> op ! l ! r ! counitTerm % (fmapTerm (W E) % (a ! fmapTerm (R E) % (op % a) % r) % l)

          -- \l r -> op l r id
  D    -> op ! l ! r ! op % l % r % (a ! a)

          -- \l r -> l =>> \l' -> o op l' r
  XL f o -> op ! l ! r ! extendTerm f % (l' ! opTerm o % op % l' % r) % l


l = make_var "l"
l' = make_var "l'"
r = make_var "r"
a = make_var "a"
b = make_var "b"
g = make_var "g"
k = make_var "k"
m = make_var "m"
mm = make_var "mm"
c = make_var "c"
op = make_var "op"

fmapTerm = \case
  S     -> k ! m ! set ( (a ! k % (get_rng m % a)) |? get_dom m )
  R _   -> k ! m ! g ! k % (m % g)
  W _   -> k ! m ! _1 m * k % _2 m
  C _ _ -> k ! m ! c ! m % (a ! c % (k % a))
  _     -> k ! m ! make_con "fmap" % k % m
pureTerm = \case
  S     -> a ! make_set a
  R _   -> a ! g ! a
  W t   -> a ! (mzeroTerm t * a)
  C _ _ -> a ! k ! k % a
  _     -> a ! make_con "pure" % a
counitTerm = m ! _2 m % _1 m
joinTerm = \case
  S     -> mm ! conc mm
  R _   -> mm ! g ! mm % g % g
  W t   -> mm !  mplusTerm t (_1 mm) (_1 (_1 mm)) * _2 (_2 mm)
  C _ _ -> mm ! c ! mm % (m ! m % c)
  _     -> mm ! make_con "join" % mm
extendTerm = \case
  W t   -> k ! m ! _1 m * k % m
  _     -> make_con "co-tastrophe"
mzeroTerm = \case
  T     -> make_con "true"
  _     -> make_con "this really shouldn't happen"
mplusTerm = \case
  T     -> \p q -> make_con "and" % p % q
  _     -> \_ _ -> make_con "this really shouldn't happen"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module TDParseCCG where

import Prelude hiding ((<>), (^), and)
import Control.Monad (join, liftM2)
import Control.Monad.State.Lazy (lift)
import Lambda_calc ( Term, make_var, (#), (^) )
import Memo
import Data.Function ( (&), fix )
import Data.Functor ( (<&>) )
import Data.List ( isInfixOf, isPrefixOf )
import Text.PrettyPrint hiding (Mode, cat)


{- Datatypes for syntactic and semantic composition-}

-- categories/types
data Type
  = E | T | N | Adj     -- Base types
  | Type :-> Type       -- Functions
  | Type :<- Type       -- Functions
  | Eff F Type          -- F-ectful
  deriving (Eq, Show, Ord)
infixr :->
infixl :<-

cat2type :: Type -> Type
cat2type N         = E :-> T
cat2type Adj       = E :-> T
cat2type (Eff f t) = Eff f (cat2type t)
cat2type (b :<- a) = cat2type (a :-> b)
cat2type (a :-> b) = cat2type a :-> cat2type b
cat2type ty        = ty

-- Effects
data F = S | R Type | W Type | C Type Type
  deriving (Eq, Show, Ord)

showNoIndices :: F -> String
showNoIndices = \case
  S     -> "S"
  R _   -> "R"
  W _   -> "W"
  C _ _ -> "C"

atomicTypes = [E,T]
atomicEffects =
  [S] ++ (R <$> atomicTypes) ++ (W <$> atomicTypes) ++ (liftM2 C atomicTypes atomicTypes)

commuter = filter commutative atomicEffects

-- convenience constructors
effS     = Eff S
effR r   = Eff (R r)
effW w   = Eff (W w)
effC r o = Eff (C r o)

type Sense = (String, Type)
type Sign = [Sense]
type Phrase = [Sign]
type Lexicon = [(String, Word)]

data Sem
  = Lex String
  | Comb Mode Sem Sem
  deriving (Show)

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


{- Type "classes" -}

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

protoParse parse (lo, hi, [sign]) = return [Proof s (Lex s) t [] | (s, t) <- sign]
protoParse parse phrase = concat <$> mapM go (bisect phrase)
  where
    bisect (lo, hi, u) =
      [ ((lo, lo + i - 1, ls), (lo + i, hi, rs))
      | i <- [1 .. length u - 1]
      , let (ls, rs) = splitAt i u
      ]

    go (ls, rs) = do
      parsesL <- parse ls
      parsesR <- parse rs
      fmap concat $ sequence do
        lp@(Proof lstr lval lty _) <- parsesL
        rp@(Proof rstr rval rty _) <- parsesR
        return do
          combos <- lift $ combine lty rty
          return do
            (op, ty) <- combos
            return $ Proof (lstr ++ " " ++ rstr) (Comb op lval rval) ty [lp, rp]


parse :: Phrase -> [Proof]
parse ws = memoT protoParse (0, length ws - 1, ws)

modes :: Type -> Type -> [(Mode, Type)]
modes = curry \case
  (b :<- a, r) | a == r
                 -> [([FA], b)]
  (l, a :-> b) | l == a
                 -> [([BA], b)]
  (l, r)       | l == r
               , E :-> T <- cat2type l
               , E :-> T <- cat2type r
                 -> [([PM], l)]
  (_, _)         -> []

combineFs :: F -> F -> [F]
combineFs = curry \case
  (S    , S     )          -> [S]
  (R i  , R j   ) | i == j -> [R i]
  (W i  , W j   ) | i == j -> [W i]
  (C i j, C j' k) | i == j -> [C i k]
  _                        -> []

combine = curry $ fix (memoize' . openCombine)

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

  -- finally see if the resulting types can additionally be lowered (D),
  -- joined (J), or canceled out (Eps)
  <**> return [addD, addEps, addJ, return]

  where
    infixr 6 <+>
    (<+>) = liftM2 (++)
    infixl 5 <**>
    (<**>) = liftM2 (flip (<*>))

    -- prefer M_,(D,)U_ over the equivalent U_,(D,)M_
    normU = \case
      UR -> \op -> not $ any (`isPrefixOf` op) [[MR], [D,MR]]
      UL -> \op -> not $ any (`isPrefixOf` op) [[ML], [D,ML]]

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

      -- for commutative effects
      ++ [ [MR     ,     A ] | f `elem` commuter]
      ++ [ [A      ,     ML] | f `elem` commuter]
      ++ [ [MR] ++ k ++ [ML] | f `elem` commuter, k <- [[J], []] ]
      ++ [ [A]  ++ k ++ [A]  | f `elem` commuter, k <- [[J], []] ]

addEps :: (Mode, Type) -> [(Mode, Type)]
addEps =
  \case
    (op, Eff f (Eff g a))
      | adjoint f g
      , normEps op -> [(Eps:op, a)]
    _ -> []

  where
    normEps (ML:MR:ops) = True
    normEps _ = False
    -- canonical Eps configuration is Eps, ML, MR
    -- rules out xover, forces Eps to apply low
    -- there remains some derivational ambiguity:
    -- W,W,R,R has 3 all-cancelling derivations not 2 due to local WR/RW ambig


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
         , [ML, D, A ] ]


{- Mapping semantic values to (un-normalized) Lambda_calc Term's -}

semTerm :: Sem -> Term
semTerm (Lex w)      = make_var (w ++ "'")
semTerm (Comb m l r) = modeTerm m # semTerm l # semTerm r

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

modeTerm :: Mode -> Term
modeTerm [op] = opTerm op
modeTerm (x:xs) = opTerm x # modeTerm xs


{- A toy lexicon -}

vp = E :-> T
rn = E :-> E

ann       = [("ann"       , E)                        ]
ann's     = [("ann's"     , E)                        ]
mary      = [("mary"      , effW E E)                 ]
mary's    = [("mary's"    , effW E E)                 ]
maryaling = [("(m--ling)" , effW T E)                 ]
sassyacat = [("(s--cat)"  , effW T E)                 ]
left      = [("left"      , vp)                       ]
whistled  = [("whistled"  , vp)                       ]
saw       = [("saw"       , vp :<- E)                 ]
chased    = [("chased"    , vp :<- E)                 ]
said      = [("said"      , vp :<- T)                 ]
gave      = [("gave"      , vp :<- E :<- E)           ]
she       = [("she"       , effR E E)                 ]
her       = [("her"       , effR E E)                 ]
she2      = [("she2"      , effR E (effW E E))        ]
her2      = [("her2"      , effR E (effW E E))        ]
mom       = [("mom"       , rn)                       ]
the       = [("the"       , E :<- N)                  ]
very      = [("very"      , Adj :<- Adj)              ]
every     = [("every"     , effC T T E :<- N)         ]
everyP    = [("everyP"    , (T :<- vp) :<- N)         ]
everyC    = [("everyC"    , effC (effC T T E) T E)    ]
big       = [("big"       , Adj)                      ]
happy     = [("happy"     , Adj)                      ]
dog       = [("dog"       , N)                        ]
cat       = [("cat"       , N)                        ]
near      = [("near"      , Adj :<- E)                ]
some      = [("some"      , effS E :<- N)             ]
someone   = [("someone"   , effC T T E)               ]
someone2  = [("someone2"  , effS (effW E E))          ]
someone3  = [("someone3"  , effS E)                   ]
everyone  = [("everyone"  , effC T T E)               ]
everyone2 = [("everyone2" , effC T T (effW E E))      ]
tr        = [("tr"        , effR E E)                 ]
conj      = [("and"       , (T :-> T) :<- T)          ]
conjE     = [("and"       , (E :-> E) :<- E)          ]
with      = [("with"      , (vp :-> vp) :<- E)        ]
         -- ++ [("with"   , (N :-> N) :<- E)          ]
eclo      = [("eclo"      , T :<- effS T)             ]

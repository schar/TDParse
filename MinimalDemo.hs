-- Minimal effect-driven interpretation.
--
-- This is meant to be read as a small implementation of the theory, not as an
-- efficient parser.  There is no memoization, no normalization of duplicate
-- derivations, and no lambda-calculus evaluator.  Semantic terms just record
-- lexical names, types, and the modes of composition used to build them.

module MinimalDemo where


-- Semantic types
-- ----------------------------------------------------------------------

-- Function types associate to the right:
--
--   TyE :-> TyE :-> TyT
--
-- is parsed as:
--
--   TyE :-> (TyE :-> TyT)
--
-- So a transitive verb has the expected curried type E -> E -> T.
infixr 5 :->

data Ty
  = TyE                 -- entity type
  | TyT                 -- truth-value type
  | Ty :-> Ty           -- function type
  | Comp EffX Ty        -- effectful/computation type
  deriving (Eq, Show)


-- Effect constructors
-- ----------------------------------------------------------------------

--   SX a        alternatives / indeterminate values
--   GX i a      values depending on an environment of type i
--   WX o a      values paired with stored output of type o
--   CX r a b    continuation/scope values, (b -> a) -> r
--
-- Continuations carry two answer-type indices.  The extra index is useful
-- because continuations compose by matching the middle answer type:
-- C i j followed by C j k gives C i k.
data EffX
  = SX
  | GX Ty
  | WX Ty
  | CX Ty Ty
  deriving (Eq, Show)

effS :: Ty -> Ty
effS = Comp SX

effG :: Ty -> Ty -> Ty
effG i = Comp (GX i)

effW :: Ty -> Ty -> Ty
effW o = Comp (WX o)

effC :: Ty -> Ty -> Ty -> Ty
effC r a = Comp (CX r a)


-- Algebraic facts about effects
-- ----------------------------------------------------------------------

-- These booleans stand in for typeclass instances.  In a real Haskell
-- implementation, "Functor", "Applicative", and "Monad" would come with
-- operations.  Here we only need to know whether the corresponding rule is
-- available for a given effect.
--
--   functor:      S, G, W, C
--   applicative:  S, G, W o if o is monoidal, C r a if r = a
--   monad:        same cases as applicative
--
-- The continuation case is deliberately restricted.  A continuation C r a is
-- an ordinary applicative/monad only when r = a.  Indexed continuations with
-- different endpoints still compose, but that happens in combineEffs below.
functor, applicative, monad :: EffX -> Bool
functor _               = True
applicative SX          = True
applicative (GX _)      = True
applicative (WX o)      = monoid o
applicative (CX r ans)  = r == ans
monad                   = applicative

-- Truth values form the only monoid used in this file.  That is enough to
-- show the ordinary applicative/monadic writer effect W T.
monoid :: Ty -> Bool
monoid TyT = True
monoid _   = False

-- Compatible effect pairs can sequence.  For S, G, and monoidal W, this is
-- ordinary applicative/monadic sequencing.  For continuations it is indexed:
--
--   CX i j  followed by  CX j k  gives  CX i k
--
combineEffs :: EffX -> EffX -> [EffX]
combineEffs SX SX = [SX]
combineEffs (GX i) (GX j)
  | i == j = [GX i]
combineEffs (WX i) (WX j)
  | i == j && monoid i = [WX i]
combineEffs (CX i j) (CX j' k)
  | j == j' = [CX i k]
combineEffs _ _ = []

-- Writer/reader adjunction:
--
--   WX i  left adjoint to  GX i
adjoint :: EffX -> EffX -> Bool
adjoint (WX o) (GX i) = i == o
adjoint _ _           = False


-- Modes of semantic composition
-- ----------------------------------------------------------------------

-- A mode is a label for a type-driven rule.  The code does not interpret
-- these labels into actual semantic functions; it uses them to record the
-- derivational shape.
data Mode
  = FA | BA | PM       -- application, backwards application, predicate modification
  | MR Mode | ML Mode  -- map a composition through a right/left effect
  | AP Mode            -- applicative sequencing
  | UR Mode | UL Mode  -- insert pure on the right/left
  | JN Mode            -- monadic join
  | EP Mode            -- counit for an adjunction
  | XL Mode            -- comonadic extension accompanying the adjunction
  | EL Mode | ER Mode  -- move a reader/query out of a function result
  | DN Mode            -- discharge a continuation
  deriving (Show)


-- Syntactic categories and trees
-- ----------------------------------------------------------------------

-- The categories are just the ones needed for the examples.  "Gen" is the
-- category of "her" in "her predecessor"; "Dmp" is the category of "push".
data Cat
  = CP | DP | VP | TV | Gen | TN | Dmp
  deriving (Eq, Show)

data Syn
  = Leaf Cat Ty String
  | Branch Cat Syn Syn
  | Island Cat Syn Syn
  deriving (Show)

root :: Syn -> Cat
root (Leaf c _ _)     = c
root (Branch c _ _)   = c
root (Island c _ _)   = c


-- Semantic derivations
-- ----------------------------------------------------------------------

-- A semantic derivation is either a lexical item at a type, or a composition
-- of two smaller derivations by a licensed mode.
data Sem
  = Lex Ty String
  | Comb Ty Mode Sem Sem
  deriving (Show)

getType :: Sem -> Ty
getType (Lex ty _)       = ty
getType (Comb ty _ _ _)  = ty

-- Island boundary: no unresolved continuation effect may remain.  This is a
-- type-level approximation of "cannot scope out of this constituent".
evaluated :: Ty -> Bool
evaluated ty = case ty of
  Comp (CX _ _) _ -> False
  Comp _ a        -> evaluated a
  _ :-> a         -> evaluated a
  _               -> True

-- Interpret a syntactic tree by interpreting its daughters and keeping every
-- composition licensed by their types.  The list comprehension is ordinary
-- nondeterministic search: choose a left derivation, choose a right derivation,
-- choose a compatible mode, and return the result.
synsem :: Syn -> [Sem]
synsem syn = case syn of
  Leaf _ t w ->
    [Lex t w]

  Branch _ lsyn rsyn ->
    [ Comb ty op lsem rsem
      | lsem     <- synsem lsyn
      , rsem     <- synsem rsyn
      , (op, ty) <- combine (getType lsem) (getType rsem) ]

  Island _ lsyn rsyn ->
    [ sem
      | sem <- synsem (Branch CP lsyn rsyn)
      , evaluated (getType sem) ]


-- Type-driven composition
-- ----------------------------------------------------------------------

modes :: Ty -> Ty -> [(Mode, Ty)]
modes l r = case (l, r) of
  (a :-> b, _) | r == a ->
    [(FA, b)]

  (_, a :-> b) | l == a ->
    [(BA, b)]

  (a :-> TyT, b :-> TyT) | a == b ->
    [(PM, a :-> TyT)]

  _ ->
    []

-- Close the basic modes under the effect operations.
--
-- First try all binary composition rules.  Then optionally post-process the
-- result with continuation discharge or join.  Since there is no normalization
-- pass, equivalent derivations may occur more than once.
combine :: Ty -> Ty -> [(Mode, Ty)]
combine l r = binaryCombs >>= unaryCombs
  where
    binaryCombs =
      modes l r
      ++ addML l r
      ++ addMR l r
      ++ addUR l r
      ++ addUL l r
      ++ addAP l r
      ++ addEP l r
      ++ addEL l r
      ++ addER l r

    unaryCombs e =
      addDN e
      ++ addJN e
      ++ return e

addML, addMR, addUR, addUL :: Ty -> Ty -> [(Mode, Ty)]
addAP, addEP, addEL, addER :: Ty -> Ty -> [(Mode, Ty)]

-- Lift a composition through an effect on the left.
addML l r = case l of
  Comp f s | functor f ->
    [ (ML op, Comp f u) | (op, u) <- combine s r ]
  _ ->
    []

-- Lift a composition through an effect on the right.
addMR l r = case r of
  Comp f t | functor f ->
    [ (MR op, Comp f u) | (op, u) <- combine l t ]
  _ ->
    []

-- Insert "pure" on the right: if the left side wants an effectful argument,
-- treat the right side as a pure value of that effect.
addUR l r = case l of
  Comp f s :-> s' | applicative f ->
    [ (UR op, u) | (op, u) <- combine (s :-> s') r ]
  _ ->
    []

-- Symmetric pure insertion.
addUL l r = case r of
  Comp f t :-> t' | applicative f ->
    [ (UL op, u) | (op, u) <- combine l (t :-> t') ]
  _ ->
    []

-- Applicative sequencing, generalized to indexed continuations by combineEffs.
addAP l r = case (l, r) of
  (Comp f s, Comp g t) ->
    [ (AP op, Comp h u)
      | h       <- combineEffs f g
      , (op, u) <- combine s t ]
  _ ->
    []

-- Writer/reader counit, plus the corresponding extension option.  EP cancels
-- W/G; XL keeps the writer effect around.
addEP l r = case (l, r) of
  (Comp f s, Comp g t) | adjoint f g ->
    [ (EP op, u) | (op, u) <- combine s t ]
    ++ [ (XL (EP op), Comp f u) | (op, u) <- combine s t ]
  _ ->
    []

-- Reader ejection: a function returning a reader can be viewed as a reader
-- returning a function.
addEL l r = case l of
  a :-> Comp (GX i) b ->
    [ (EL op, u) | (op, u) <- combine (Comp (GX i) (a :-> b)) r ]
  _ ->
    []

addER l r = case r of
  a :-> Comp (GX i) b ->
    [ (ER op, u) | (op, u) <- combine l (Comp (GX i) (a :-> b)) ]
  _ ->
    []

addJN, addDN :: (Mode, Ty) -> [(Mode, Ty)]

-- Join compatible nested effects.
addJN e = case e of
  (op, Comp f (Comp g a)) ->
    [ (JN op, Comp h a) | h <- combineEffs f g ]
  _ ->
    []

-- Continuation discharge: if the payload type is the answer type, run the
-- continuation at identity.
addDN e = case e of
  (op, Comp (CX r a) a') | a == a' ->
    [(DN op, r)]
  _ ->
    []


-- Grammar and lexicon
-- ----------------------------------------------------------------------

type Grammar = Cat -> Cat -> [Cat]
type Lexicon = [(String, Cat, Ty)]

-- The grammar is intentionally tiny.  It builds just enough binary trees to
-- put the semantic machinery on display:
--
--   DP VP    -> CP
--   TV DP    -> VP
--   Gen TN   -> DP
--   Dmp DP   -> DP
--
-- Syntax and semantic effects are kept separate.  The grammar decides which
-- trees exist; synsem decides which type-driven interpretations those trees
-- support.
grammar :: Grammar
grammar DP VP    = [CP]
grammar TV DP    = [VP]
grammar Gen TN   = [DP]
grammar Dmp DP   = [DP]
grammar _ _      = []

-- The lexicon is small, but it includes one natural example of each effect.
--
--   ann, mary             DP    E
--     plain individual-denoting DPs
--
--   someone               DP    S E
--     an alternative/indefinite DP
--
--   everyone              DP    C T T E
--     a scope-taking DP
--
--   maryaling             DP    W T E
--   sassyacat             DP    W T E
--     writer DPs with truth-valued output; since T is monoidal, these show
--     the ordinary applicative/monadic writer behavior
--
--   push                  Dmp   E -> W E E
--     stores an entity for the writer/reader interaction
--
--   her                   Gen   G E E
--     an anaphoric reader
--
--   predecessor           TN    E -> E
--   left                  VP    E -> T
--   saw                   TV    E -> E -> T
lexicon :: Lexicon
lexicon =
  [ ("ann"        , DP   , TyE)
  , ("mary"       , DP   , TyE)
  , ("someone"    , DP   , effS TyE)
  , ("everyone"   , DP   , effC TyT TyT TyE)
  , ("push"       , Dmp  , TyE :-> effW TyE TyE)
  , ("maryaling"  , DP   , effW TyT TyE)
  , ("sassyacat"  , DP   , effW TyT TyE)
  , ("her"        , Gen  , effG TyE TyE)
  , ("predecessor", TN   , TyE :-> TyE)
  , ("left"       , VP   , TyE :-> TyT)
  , ("saw"        , TV   , TyE :-> TyE :-> TyT)
  ]


-- Parser
-- ----------------------------------------------------------------------

-- This parser tries every binary split of the input.  It is exponential and
-- proud of it: the goal is to keep parsing out of the way, not to optimize it.
parse :: Grammar -> Lexicon -> [String] -> [Syn]
parse _ _ [] =
  []
parse _ lex [w] =
  [ Leaf cat ty w | (w', cat, ty) <- lex, w == w' ]
parse cfg lex ws =
  [ makeNode cat left right
    | (ls, rs) <- splits ws
    , left     <- parse cfg lex ls
    , right    <- parse cfg lex rs
    , cat      <- cfg (root left) (root right) ]

makeNode :: Cat -> Syn -> Syn -> Syn
makeNode CP = Island CP
makeNode c  = Branch c

splits :: [a] -> [([a], [a])]
splits xs =
  [ splitAt n xs | n <- [1 .. length xs - 1] ]

tokenize :: String -> [String]
tokenize = words

interpret :: String -> [Sem]
interpret =
  concatMap synsem . parse grammar lexicon . tokenize


-- Worked examples
-- ----------------------------------------------------------------------

-- Evaluate these names in GHCi to inspect the derivations.
--
--   ann left
--     plain backwards application
--
--   ann saw mary
--     "saw mary" forms a VP, then "ann" saturates it
--
--   someone left
--     the predicate is lifted through S
--
--   everyone left
--     the predicate is lifted through C, then DN discharges the continuation
--
--   push ann saw her predecessor
--     writer/reader interaction: "push ann" stores an entity, and "her" reads
--     it back inside "her predecessor"
--
--   maryaling saw sassyacat
--     applicative/monadic writer with output type T
examplePlain, exampleObject, exampleIndefinite :: [Sem]
exampleQuantifier, exampleAnaphora, exampleWriter :: [Sem]
examplePlain       = interpret "ann left"
exampleObject      = interpret "ann saw mary"
exampleIndefinite  = interpret "someone left"
exampleQuantifier  = interpret "everyone left"
exampleAnaphora    = interpret "push ann saw her predecessor"
exampleWriter      = interpret "maryaling saw sassyacat"

workedExamples :: [(String, [Sem])]
workedExamples =
  [ ("ann left"                    , examplePlain)
  , ("ann saw mary"                , exampleObject)
  , ("someone left"                , exampleIndefinite)
  , ("everyone left"               , exampleQuantifier)
  , ("push ann saw her predecessor", exampleAnaphora)
  , ("maryaling saw sassyacat"     , exampleWriter)
  ]

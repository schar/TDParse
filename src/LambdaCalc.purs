{-# LANGUAGE LambdaCase #-}

-- | The following is adapted from Oleg Kiselyov's normal order
-- lambda calculator:
-- https://okmij.org/ftp/Computation/lambda-calc.html#lambda-calculator-haskell
-- Here we add constructs and reductions for pairs and "sets"
-- Sets are encoded as abstractions with explicit domains:
-- {f x | x <- dom} ~~> Set dom (\x -> f x)

module LambdaCalc
  ( eval, evalFinal
  , lam, (!), (%)
  , make_var
  , (|?), set, make_set, get_dom, get_rng, set', conc
  , (*), _1, _2
  -- , a,b,c,x,y,z,f,g,h,p,q
  , show_term
  , show_tex
  , show_hs
  , enough_vars, unwind, occurs, bump_color
  , showVar, unrollDom, rerollDom, var_stock, tuple
  , Term(..), VarName(..), VColor
  ) where

import Control.Monad.Writer
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Tuple.Nested
import Prelude
import Control.Lazy


import Data.Array (cons)
import Data.Foldable (lookup, and)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String (replaceAll, Pattern(..), Replacement(..))
import Effect.Exception.Unsafe (unsafeThrow)

type VColor = Int
data VarName = VC VColor String
derive instance Eq VarName
derive instance Generic VarName _
instance Show VarName where
  show = genericShow
showVar (VC color name) | color == 0 = name
                        | otherwise  = name <> show color

data Term
  = Var VarName | App Term Term | Lam VarName Term
  | Pair Term Term | Fst Term | Snd Term
  | Set Term Term | Dom Term | Rng Term | Cct Term | Spl Int Term
derive instance Eq Term
derive instance Generic Term _


eval term = fix openEval Nil term
openEval eval' s e = case e,s of
  (Var v)      , Nil    -> e
  (Var v)      , _      -> unwind e s
  (Lam v body) , Nil    -> check_eta $ Lam v (ev body)
  (Lam v body) , (t:ts) -> eval' ts (subst body v t)
  (App t1 t2)  , _      -> eval' (t2:s) t1
  (Pair t1 t2) , Nil    -> Pair (ev t1) (ev t2)
  (Pair _ _)   , (a:_)  -> unsafeThrow ("trying to apply a pair: "
                                        <> show e <> " to " <> show a)
  (Fst t0)     , _      -> case ev t0 of
    (Pair t1 _)         -> eval' s t1 -- might not need eval' here
    t                   -> unwind (Fst t) s
  (Snd t0)     , _      -> case ev t0 of
    (Pair _ t2)         -> eval' s t2 -- or here
    t                   -> unwind (Snd t) s
  (Set t1 t2)  , Nil    -> Set (ev t1) (ev t2)
  (Set _ _)    , (a:_)  -> unsafeThrow ("trying to apply a set: "
                                        <> show e <> " to " <> show a)
  (Dom t0)  , Nil       -> case ev t0 of
    (Set t1 _)             -> ev t1 -- or here
    t                      -> Dom t
  (Dom _)   , (a:_)     -> unsafeThrow ("trying to apply a dom: "
                                        <> show e <> " to " <> show a)
  (Rng t0)   , _      -> case ev t0 of
    (Set _ t2)          -> eval' s t2
    t                   -> unwind (Rng t) s
  (Cct _)      , (a:_)  -> unsafeThrow ("trying to apply a set: "
                                        <> show e <> " to " <> show a)
  (Cct t0)     , Nil    -> case ev t0 of
    f@(Set t1 g@(Lam v (Set t2 t3))) ->
                           let (Tuple t vars) = unrollDom f var_stock
                               (Tuple t' vars') = unrollDom (ev $ g % tuple (map Var vars)) (drop (length vars) var_stock)
                               dom = rerollDom t t' (vars <> vars')
                            in ev $ Set dom (p ! get_rng (g % (_1 (Spl (length vars) p))) % (_2 (Spl (length vars) p)))
    t                   -> Cct t
  (Spl _ _)    , (a:_)  -> unsafeThrow ("trying to apply a split: "
                                        <> show e <> " to " <> show a)
  (Spl n t0)   , Nil    -> case ev t0 of
    f@(Pair t1 t2)      -> Pair (leftSplit n f) (rightSplit n f)
    t                   -> Spl n t
  (Spl _ _)    , _      -> unsafeThrow ("trying to split something weird: "
                                        <> show e <> " to " <> show a)
  where
    unwind t Nil = t
    unwind t (t1:rest) = unwind (App t $ ev t1) rest
    ev = eval' Nil
    leftSplit 1 (Pair x _) = x
    leftSplit n (Pair x p) = Pair x (leftSplit (n-1) p)
    leftSplit n p = unsafeThrow ("bad leftSplit " <> show n <> ": " <> show_term p <> " within " <> show_term e)
    rightSplit 1 (Pair _ y) = y
    rightSplit n (Pair _ p) = (rightSplit (n-1) p)
    rightSplit n p = unsafeThrow ("bad rightSplit " <> show n <> ": " <> show_term p <> " within " <> show_term e)


evalFinal term = fix (openFinal <<< openEval) Nil term
openFinal eval' s e = case e,s of
  (Dom t0) , Nil     -> case ev t0 of
    (Set t1 _)          -> ev t1
    t                   -> t
  (Rng t0)  , _       -> case ev t0 of
    (Set _ t2)          -> eval' s t2
    _                   -> eval' s (a ! a)
  _           , _       -> eval' s e
  where
    ev = eval' Nil

unwind t Nil = t
unwind t (t1:rest) = unwind (App t $ eval t1) rest

rewind t Nil = t
rewind t (v1:rest) = (Lam v1 (rewind t rest))

unrollDom :: Term -> List VarName -> Tuple Term (List VarName)
unrollDom e@(Set dom rng) vs =
  let vars' = enough_vars dom vs
      occs = map (\s -> Tuple (occurs e s) s) vars'
      vars = map (\(Tuple (Tuple f v') s) -> if f then bump_color v' s else s) occs
      getvar vs = Tuple (maybe (make_var "s") Var (head vs)) (fromMaybe Nil (tail vs))
      go t vs apps = let (Tuple v rest) = getvar vs in
        case t of
          Pair t1 t2 -> Pair (eval $ unwind t1 apps) (go t2 rest (v:apps))
          _          -> (eval $ unwind t apps)
   in Tuple (go dom vars Nil) vars
unrollDom e _ = unsafeThrow ("trying to unroll dom: " <> show e)

rerollDom d d' vs = makeItRain (linearize d d') Nil vs
  where
    linearize (Pair d0 d1) d = Pair d0 (linearize d1 d)
    linearize t d = Pair t d

makeItRain (Pair d0 d1) use (s:stock) = Pair (rewind d0 use) (makeItRain d1 (use <> pure s) stock)
makeItRain e@(Pair d0 d1) _ Nil = unsafeThrow ("not enough vars for: " <> show_term e)
makeItRain t use _ = rewind t use

tuple Nil = unsafeThrow "not enough vars"
tuple (v:Nil) = v
tuple (v:vs)  = Pair v (tuple vs)

var_stock = map (VC 0) $ "s":"t":"u":"v":"w":"a":"b":"c":Nil

enough_vars :: Term -> List VarName -> List VarName
enough_vars t vs = go t $ vs
  where
    go t Nil = Nil
    go (Pair t1 t2) (v:vars) = v : go t2 vars
    go _ (v:vars) = v:Nil


subst term v (Var v') | v == v' = term
subst t@(Var x) v st | x == v    = st
                     | otherwise = t
subst (Pair t1 t2) v st = Pair (subst t1 v st) (subst t2 v st)
subst (Fst p) v st = Fst (subst p v st)
subst (Snd p) v st = Snd (subst p v st)
subst (Set t1 t2) v st = Set (subst t1 v st) (subst t2 v st)
subst (Dom s) v st = Dom (subst s v st)
subst (Rng s) v st = Rng (subst s v st)
subst (App t1 t2) v st = App (subst t1 v st) (subst t2 v st)
subst (Cct s) v st = Cct (subst s v st)
subst (Spl n s) v st = Spl n (subst s v st)
subst t@(Lam x _) v _ | v == x  = t
subst (Lam x body) v st = (Lam x' (subst body' v st))
  where
    (Tuple f x_occur_st) = occurs st x
    (Tuple x' body') =
      if f
        then let x_uniq_st_v             = bump_color' (bump_color x x_occur_st) v
                 (Tuple bf x_occur_body) = occurs body x_uniq_st_v
                 x_unique =
                   if bf then bump_color x_uniq_st_v x_occur_body else x_uniq_st_v
              -- x_unique used to be x'; seems the same?
              in (Tuple x_unique (subst body x (Var x_unique)))
        else (Tuple x body)

bump_color (VC color name) (VC color' _) =
  (VC ((max color color')+1) name)
bump_color' v1@(VC _ name) v2@(VC _ name') =
  if name==name' then bump_color v1 v2 else v1

occurs (Var v'@(VC c' name')) v@(VC c name)
  | not (name == name')  = (Tuple false v)
  | c == c'              = (Tuple true  v)
  | otherwise            = (Tuple false v')
occurs (App t1 t2) v
  = let (Tuple f1 v1@(VC c1 _)) = occurs t1 v
        (Tuple f2 v2@(VC c2 _)) = occurs t2 v
     in (Tuple (f1 || f2)  (if c1 > c2 then v1 else v2))
occurs (Pair t1 t2) v
  = let (Tuple f1 v1@(VC c1 _)) = occurs t1 v
        (Tuple f2 v2@(VC c2 _)) = occurs t2 v
     in (Tuple (f1 || f2)  (if c1 > c2 then v1 else v2))
occurs (Fst p) v = occurs p v
occurs (Snd p) v = occurs p v
occurs (Set t1 t2) v
  = let (Tuple f1 v1@(VC c1 _)) = occurs t1 v
        (Tuple f2 v2@(VC c2 _)) = occurs t2 v
     in (Tuple (f1 || f2)  (if c1 > c2 then v1 else v2))
occurs (Dom s) v = occurs s v
occurs (Rng s) v = occurs s v
occurs (Cct s) v = occurs s v
occurs (Spl _ s) v = occurs s v
occurs (Lam x body) v
  | x == v    = (Tuple false v)
  | otherwise = occurs body v

check_eta (Lam v (App t (Var v')))
  | v == v' && (let (Tuple flag _) = occurs t v in not flag) = t
check_eta term = term

note_reduction label redex = tell $ singleton (Tuple label redex)

mweval term = runWriter (meval' term Nil)

meval' t@(Var v) Nil = pure t
meval' (Lam v body) Nil = do
  body' <- meval' body Nil
  mcheck_eta $ Lam v body'
meval' a@(Lam v body) (t: rest) = do
  note_reduction "beta" (App a t)
  meval' (subst body v t) rest
meval' (App t1 t2) stack = meval' t1 (t2:stack)
meval' t@(Var v) stack = munwind t stack
meval' e@(Pair t1 t2) stack = case stack of
  Nil -> do
    t1' <- meval' t1 Nil
    t2' <- meval' t2 Nil
    pure $ Pair t1' t2'
  (s:_) -> unsafeThrow ("trying to apply a pair: " <> show_term e <> " to " <> show_term s)
meval' (Fst p) stack = do
  p' <- meval' p Nil
  case p' of
    (Pair t1 t2)  -> note_reduction "fst" (Fst p') *> meval' t1 stack
    t             -> munwind (Fst t) stack
meval' (Snd p) stack = do
  p' <- meval' p Nil
  case p' of
    (Pair t1 t2)  -> note_reduction "snd" (Snd p') *> meval' t2 stack
    t             -> munwind (Snd t) stack
meval' e@(Set t1 t2) stack = case stack of
  Nil -> do
    t1' <- meval' t1 Nil
    t2' <- meval' t2 Nil
    pure $ Set t1' t2'
  (s:_) -> unsafeThrow ("trying to apply a set: " <> show_term e <> " to " <> show_term s)
meval' e@(Dom s) stack = case stack of
  Nil -> do
    s' <- meval' s Nil
    case s' of
      (Set t1 t2)  -> note_reduction "domain" (Dom s') *> pure t1
      t            -> pure t
  (s:_) -> unsafeThrow ("trying to apply the domain of a set: " <> show_term e <> " to " <> show_term s)
meval' (Rng s) stack = do
  s' <- meval' s Nil
  case s' of
    (Set t1 t2)  -> note_reduction "map" (Rng s') *> meval' t2 stack
    t            -> meval' (a ! a) stack
meval' (Cct s) stack = unsafeThrow ("need to implement meval' CCt")
meval' (Spl n s) stack = unsafeThrow ("need to implement meval' CCt")

munwind ::  Term -> List Term -> Writer (List (Tuple String Term)) Term
munwind t Nil = pure t
munwind t (t1:rest) =
  do t1' <- meval' t1 Nil
     munwind (App t t1') rest

mcheck_eta red@(Lam v (App t (Var v')))
  | v == v' && (let (Tuple flag _) = occurs t v in not flag)
    = do note_reduction "eta" red
         pure t
mcheck_eta term = pure term

make_var = Var <<< VC 0
x = make_var "x"
y = make_var "y"
z = make_var "z"
f = make_var "f"
g = make_var "g"
a = make_var "a"
b = make_var "b"
c = make_var "c"
p = make_var "p"
q = make_var "q"

-- a little DSL for building lambda terms

infixl 8 App as %
lam (Var v) body = Lam v body
lam _ _ = unsafeThrow "ill-formed abstraction"
infixr 6 lam as !
_1 = Fst
_2 = Snd
infixr 7 Pair as *
make_set p = Set (make_var "some" % p) (x ! x)
get_dom = Dom
get_rng = Rng
set' = flip Set
infix 5 set' as |?
set = identity
conc = Cct

showRight = case _ of
  (Set _ _)  -> identity
  (Pair _ _) -> identity
  (Var _)    -> identity
  _          -> parens
showLeft = case _ of
  (Lam _ _) -> parens
  _         -> identity
parens s = "(" <> s <> ")"

type Formatter =
  { lam' :: String, arr' :: String
  , lb' :: String, rb' :: String, mid' :: String, la' :: String, ra' :: String
  , fst' :: String, snd' :: String, elem' :: String, dom' :: String, rng' :: String
  }

show_formatted_term form term depth
  | depth <= 0 = "..."
  | otherwise  = showt term
  where
    showt = case _ of
      Var v -> showVar v
      Lam v body -> form.lam' <> (showVar v) <> form.arr' <> (showt' body)
      App t1 t2 -> showLeft t1 (showt' t1) <> " " <> showRight t2 (showt' t2)
      Pair t1 t2 -> form.la' <> showt' t1 <> ", " <> showt' t2 <> form.ra'
      Fst p -> form.fst' <> showRight p (showt p)
      Snd p -> form.snd' <> showRight p (showt p)
      e@(Set _ cond) ->
        let Tuple t vars = unrollDom e var_stock
            getvar vs = Tuple (maybe (make_var "s") Var (head vs)) (fromMaybe Nil (tail vs))
            showDom t vs = let (Tuple v rest) = getvar vs in
              case t of
                Pair a b ->
                  showt v <> form.elem' <> showt a <> ", " <> showDom b rest
                t' -> showt v <> form.elem' <> showt t'
         in form.lb' <> showt' (eval $ cond % (tuple $ map Var vars)) <>
            form.mid' <> showDom t vars <> form.rb'
      Dom p -> form.dom' <> showRight p (showt p)
      Rng p -> form.rng' <> showRight p (showt p)
      Cct t -> "concat " <> showRight t (showt t)
      Spl n t -> "splitAt " <> show n <> showRight t (showt t)
    showt' term = show_formatted_term form term (depth - 1)

default_term_form =
  { lam': "\\", arr': ". "
  ,lb': "[", rb': "]", mid': " | ", la': "<", ra': ">"
  , fst': "fst ", snd': "snd ", elem': " <- " , dom': "sdom ", rng': "srng "
  }

show_term term = show_formatted_term default_term_form term 100
show_hs term = show_formatted_term hs_form term 100
  where
    hs_form = default_term_form { lam' = "\\textbackslash ", arr' = " -> ", la' = "(", ra' = ")" }
show_tex term = show_formatted_term tex_form term 100
  where
    tex_form =
      { lam': "\\lambda ", arr': ". "
      , lb': "\\{ ", rb': "\\} ", mid': " \\mid ", la': "\\langle ", ra': "\\rangle "
      , fst': "\\textsf{fst} ", snd': "\\textsf{snd} ", elem': " \\in " , dom': "\\textsf{dom} ", rng': "\\textsf{rng} "
      }

instance Show Term where
   show term = genericShow term

free_vars term = free_vars' term [] []
  where
    free_vars' (Var v) bound free = if v `elem` bound then free else v `cons` free
    free_vars' (App t1 t2) bound free =
      free_vars' t1 bound $ free_vars' t2 bound free
    free_vars' (Lam v body) bound free = free_vars' body (v `cons` bound) free
    free_vars' (Pair t1 t2) bound free =
      free_vars' t1 bound $ free_vars' t2 bound free
    free_vars' (Fst p) bound free = free_vars' p bound free
    free_vars' (Snd p) bound free = free_vars' p bound free
    free_vars' (Set t1 t2) bound free =
      free_vars' t1 bound $ free_vars' t2 bound free
    free_vars' (Dom p) bound free = free_vars' p bound free
    free_vars' (Rng p) bound free = free_vars' p bound free

term_equal_p term1 term2 = term_equal_p' term1 term2 (Nil /\ Nil /\ 0)
  where
  term_equal_p' (Var v1) (Var v2) (bdic1 /\ bdic2 /\ _) =
    case (Tuple (lookup v1 bdic1) (lookup v2 bdic2)) of
    (Tuple (Just bv1) (Just bv2)) -> bv1 == bv2 -- both v1 v2 are bound to the same val
    (Tuple Nothing Nothing)       -> v1 == v2   -- both v1 and v2 are free
    _                             -> false

  term_equal_p' (Lam v1 b1) (Lam v2 b2) (bdic1 /\ bdic2 /\ counter) =
    term_equal_p' b1 b2
      (Tuple ((Tuple v1 counter):bdic1) (Tuple ((Tuple v2 counter):bdic2) (counter+1)))

  term_equal_p' (App t1 t1') (App t2 t2') env =
    term_equal_p' t1  t2  env &&
    term_equal_p' t1' t2' env

  term_equal_p' (Pair t1 t1') (Pair t2 t2') env =
    term_equal_p' t1  t2  env &&
    term_equal_p' t1' t2' env

  term_equal_p' (Fst p1) (Fst p2) env = term_equal_p' p1 p2 env
  term_equal_p' (Snd p1) (Snd p2) env = term_equal_p' p1 p2 env

  term_equal_p' (Set t1 t1') (Set t2 t2') env =
    term_equal_p' t1  t2  env &&
    term_equal_p' t1' t2' env

  term_equal_p' (Dom p1) (Dom p2) env = term_equal_p' p1 p2 env
  term_equal_p' (Rng p1) (Rng p2) env = term_equal_p' p1 p2 env

  term_equal_p' _ _ _ = false

expectg f exp expected_result = case f exp expected_result of
  true -> true
  false -> unsafeThrow ("Test case failure: Expected " <> (show expected_result)
                <> ", received: " <> (show exp))
expect :: forall a. Eq a => Show a => a -> a -> Boolean
expect = expectg (==)
expectd = expectg term_equal_p -- test using comparison modulo alpha-renaming
notexpectd = expectg (\x y -> not $ term_equal_p x y)
free_var_tests = and [
   expect (map Var (free_vars $ x))  [x],
   expect (map Var (free_vars $ x!x)) [],
   expect (map Var (free_vars $ x%y%z)) [x,y,z],
   expect (map Var (free_vars $ x!x%y)) [y],
   expect (map Var (free_vars $ (x!x%y)%(x%y%z))) [y,x,y,z],
   expect (map Var (free_vars $ (x!x!x%y)%(x!y!x%y))) [y]
   ]
alpha_comparison_tests = and [
   expectd    x x,
   notexpectd x y,
   expectd    (x) x,
   expectd    x  ((x)),
   expectd    (x) ((x)),
   expectd    (x%y%(z)) ((x%y)%z),
   expectd    (((a%(b%c))%(q))%(p%f)) (a%(b%c)%q%(p%f)),
   notexpectd (a%(b%c)%q%(p%f)) (a%b%c%q%(p%f)),
   notexpectd (x!x) (x!y),
   expectd    (x!x) (y!y),
   expectd    (x!x!x) (y!y!y),
   notexpectd (x!(x%x)) $ y!(y%x),
   notexpectd (y!(y%x)) $ x!(x%x),
   expectd    (y!(y%x)) $ z!(z%x),
   notexpectd (x!y!(x%y)) $ f!f!(f%f),
   expectd    (x!x!(x%x)) $ f!f!(f%f),
   expectd    (x!y!(y%y)) $ f!f!(f%f),
   expectd    (f!x!f%x) $ f!x!f%x,
   notexpectd (f!x!f%x) $ f!x!x,
   expectd    (f!x!f%x) $ g!x!(g%x),
   expectd    (f!x!f%x) $ g!y!g%y,
   expectd    (g!y!g%y) $ f!x!f%x,
   notexpectd (g!y!g%x) $ f!x!f%x,
   notexpectd (f!x!f%x) (g!y!g%x)
   ]

subst_tests = and [
  expectd (subst (c!c)  (VC 1 "c") c) (z!z),
  expectd (subst (Lam (VC 1 "c") (App (Var (VC 0 "c")) (App (Var (VC 1 "c"))
                 (App (Var (VC 2 "c")) (Var (VC 3 "c") )))))
                 (VC 0 "c") (App (Var (VC 1 "c")) (Var (VC 2 "c"))))
        (a!(Var $ VC 1 "c")%(Var $ VC 2 "c")%
           (a%((Var $ VC 2 "c")%(Var $ VC 3 "c"))))
  ]

eval_tests = and [
   expectd (eval $ ((x!(a%b%x))%(a!a%b))) $
         (a%b%(p!p%b)),
   expectd (eval $ (((f!x!(f%x))%g)%z))
         (g%z),
   expectd (eval $ ((c!f!x!f%(c%f%x))%(f!x!x)))
         (f!f),
   expectd (((x!x%x)%(x!x%x)))
         ((p!p%p)%(q!q%q)),
   expectd (eval $ ((x!y)%((x!x%x)%(x!x%x))))
         y,
   expectd (eval $ ((x!y!(f%x%y%y))%(g%y)))
         (z!(f%(g%y)%z%z)),
   expectd (eval $ ((c!f!x!f%(c%f%x))%(f!x!(f%x))))
         (g!x!(g%(g%x))),
   expectd (eval $ a ! (x ! a ! a % x) % (a % x))
         (a!b!(b%(a%x))),
   expectd (eval $ a ! (x ! a ! x % a) % a)
         (z!z),
   expectd (eval $ a ! (x ! b ! x % a) % a)
         (a!b!a%a)
   ]
mweval_tests = and [
   expectd (fst $ mweval $ ((x!(a%b%x))%(a!a%b))) $
         (a%b%(p!p%b)),
   expectd (fst $ mweval $ (((f!x!(f%x))%g)%z))
         (g%z),
   expectd (fst $ mweval $ ((c!f!x!f%(c%f%x))%(f!x!x)))
         (f!f),
   expectd (fst $ mweval $ ((x!y)%((x!x%x)%(x!x%x))))
         y,
   expectd (fst $ mweval $ ((x!y!(f%x%y%y))%(g%y)))
         (z!(f%(g%y)%z%z)),
   expectd (fst $ mweval $ ((c!f!x!f%(c%f%x))%(f!x!(f%x))))
         (g!x!(g%(g%x))),
   expectd (fst $ mweval $ a ! (x ! a ! a % x) % (a % x))
         (a!b!(b%(a%x))),
   expectd (fst $ mweval $ a ! (x ! a ! x % a) % a)
         (z!z),
   expectd (fst $ mweval $ a ! (x ! b ! x % a) % a)
         (a!b!a%a)
   -- , expect (show $ mweval $ a ! (x ! a ! x % a) % a)
   --        "((\\a. a),[(\"beta\",(\\x. (\\a. x a)) a),(\"eta\",(\\a~1. a a~1))])"
   ]

all_tests = and [ {-free_var_tests, -}alpha_comparison_tests,
                  subst_tests, eval_tests, mweval_tests ]

-- (Pair (Fst (Lam b (Pair (Var s) (App (App (Var saw) (Var b)) (Var s))))) (App (Var eclo) (Set (Snd (Lam b (Pair (Var s) (App (App (Var saw) (Var b)) (Var s))))) (Lam a1 (Var a1))))) to (Var t)


-- | The following is adapted from Oleg Kiselyov's normal order
-- lambda calculator:
-- https://okmij.org/ftp/Computation/lambda-calc.html#lambda-calculator-haskell
-- Here we add constructs and reductions for pairs and "sets"
-- Sets are encoded as abstractions with explicit domains:
-- {f x | x <- dom} ~~> Set dom (\x -> f x)

module LambdaCalc
  ( eval, evalFinal
  , lam, (!), (%), make_var, make_con
  , (|?), set, make_set, get_dom, get_rng, set', conc
  , (*), _1, _2
  , (!!), (~), ix
  -- , a,b,c,x,y,z,f,g,h,p,q
  , show_term, show_tex, show_hs
  , enough_vars, unwind, occurs, bump_color
  , showVar, unrollDom, rerollDom, var_stock, tuple
  , Term(..), VarName(..), VColor
  -- , all_tests
  ) where

import Control.Monad.Writer
import Data.List hiding (null)
import Data.Maybe
import Data.Tuple
import Data.Tuple.Nested
import Prelude
import Control.Lazy


import Data.Array (cons)
import Data.Foldable (lookup, and, null)
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
  = Con String | Var VarName | App Term Term | Lam VarName Term
  | Pair Term Term | Fst Term | Snd Term
  | Set Term Term | Dom Term | Rng Term | Cct Term | Spl Int Term
  | Push Term Term | Proj Int Term
derive instance Eq Term
derive instance Generic Term _

eval term = fix openEval term Nil
openEval eval' e s = case e,s of
  (Con c)      , Nil    -> e
  (Con c)      , _      -> unwind ev e s
  (Var v)      , Nil    -> e
  (Var v)      , _      -> unwind ev e s
  (Lam v body) , Nil    -> check_eta $ Lam v (ev body)
  (Lam v body) , (t:ts) -> eval' (subst body v t) ts
  (App t1 t2)  , _      -> eval' t1 (t2:s)
  (Pair t1 t2) , Nil    -> Pair (ev t1) (ev t2)
  (Pair _ _)   , (a:_)  -> unsafeThrow ("trying to apply a pair: "
                                        <> show_term e <> " to " <> show_term a)
  (Fst t0)     , _      -> case ev t0 of
    (Pair t1 _)         -> eval' t1 s -- might not need eval' here
    t                   -> unwind ev (Fst t) s
  (Snd t0)     , _      -> case ev t0 of
    (Pair _ t2)         -> eval' t2 s -- or here
    t                   -> unwind ev (Snd t) s
  (Set t1 t2)  , Nil    -> Set (ev t1) (ev t2)
  (Set _ _)    , (a:_)  -> unsafeThrow ("trying to apply a set: "
                                        <> show_term e <> " to " <> show_term a)
  (Dom t0)  , Nil       -> case ev t0 of
    (Set t1 _)             -> ev t1 -- or here
    t                      -> Dom t
  (Dom _)   , (a:_)     -> unsafeThrow ("trying to apply a dom: "
                                        <> show_term e <> " to " <> show_term a)
  (Rng t0)   , _      -> case ev t0 of
    (Set _ t2)          -> eval' t2 s
    t                   -> unwind ev (Rng t) s
  (Cct t0)     , Nil    -> case ev t0 of
    o@(Set t1 i@(Lam v (Set t2 t3)))
      | null $ [t1,t2] >>= free_vars ->
      let (Tuple t vars) = unrollDom ev o var_stock
          (Tuple t' vars') = unrollDom ev (ev $ i % tuple (map Var vars)) (drop (length vars) var_stock)
          dom = rerollDom t t' (vars <> vars')
       in check_sigma $ ev $ Set dom (p ! get_rng (i % (_1 (Spl (length vars) p))) % (_2 (Spl (length vars) p)))
    t                   -> Cct t
  (Cct _)      , (a:_)  -> unsafeThrow ("trying to apply a set: "
                                        <> show_term e <> " to " <> show_term a)
  (Spl n t0)   , Nil    -> case ev t0 of
    f@(Pair t1 t2)      -> Pair (leftSplit n f) (rightSplit n f)
    t                   -> Spl n t
  (Spl _ _)    , _      -> unsafeThrow ("trying to split something weird: "
                                        <> show_term e <> " to " <> show_term a)
  (Push t1 t2) , Nil    -> Push (ev t1) (ev t2)
  (Push _ _)   , (a:_)  -> unsafeThrow ("trying to apply a push: "
                                        <> show_term e <> " to " <> show_term a)
  (Proj n t0)  , Nil    -> case ev t0 of
    Push x g            -> if n == 0 then eval' x s else eval' (Proj (n-1) g) s
    t                   -> (Proj n t)
  (Proj _ _)   , (a:_)  -> unsafeThrow ("trying to apply a proj: "
                                        <> show_term e <> " to " <> show_term a)
  where
    ev t = eval' t Nil
    leftSplit n = case _ of
      Pair x p -> if n == 1 then x else Pair x (leftSplit (n-1) p)
      _        -> unsafeThrow ("bad leftSplit " <> show n <> ": " <> show_term p <> " within " <> show_term e)
    rightSplit n = case _ of
      Pair x p -> if n == 1 then p else rightSplit (n-1) p
      _        -> unsafeThrow ("bad rightSplit " <> show n <> ": " <> show_term p <> " within " <> show_term e)

evalFinal term = fix (openFinal <<< openEval) term Nil
openFinal eval' e s = case e,s of
  (Dom t0) , Nil     -> case ev t0 of
    (Set t1 _)       -> ev t1
    t                -> t
  (Rng t0)  , _      -> case ev t0 of
    (Set _ t2)       -> eval' t2 s
    _                -> eval' (a ! a) s
  (Cct t0)  , Nil    -> case ev t0 of
    o@(Set t1 i@(Lam v (Set t2 t3))) ->
      let (Tuple t vars  ) = unrollDom ev o var_stock
          (Tuple t' vars') = unrollDom ev (ev $ i % tuple (map Var vars)) (drop (length vars) var_stock)
          dom = rerollDom t t' (vars <> vars')
       in check_sigma $ ev $ Set dom (p ! get_rng (i % (_1 (Spl (length vars) p))) % (_2 (Spl (length vars) p)))
    (Set t1 (Lam v t2)) -> check_sigma $ openFinal eval' (Cct (Set t1 (Lam v (Set t2 (a ! a))))) Nil
    t                -> Cct t
  _           , _    -> eval' e s
  where
    ev t = openFinal eval' t Nil

unwind _ t Nil = t
unwind f t (t1:rest) = unwind f (t `App` f t1) rest

rewind t Nil = t
rewind t (v1:rest) = (Lam v1 (rewind t rest))

-- unrollDom :: Term -> List VarName -> Tuple Term (List VarName)
unrollDom eval e@(Set dom rng) vs =
  let vars' = enough_vars dom vs
      occs = map (\s -> Tuple (occurs e s) s) vars'
      vars = map (\(Tuple (Tuple f v') s) -> if f then bump_color v' s else s) occs
      getvar (v:vs) = Tuple (Var v) vs
      getvar Nil = unsafeThrow "could not getvar while unrolling dom"
      go t vs apps = let (Tuple v rest) = getvar vs in
        case t of
          Pair t1 t2 -> Pair (eval $ unwind eval t1 apps) (go t2 rest (v:apps))
          _          -> (eval $ unwind eval t apps)
   in Tuple (go dom vars Nil) vars
unrollDom _ e _ = unsafeThrow ("trying to unroll dom: " <> show e)

rerollDom d d' vs = go (linearize d d') Nil vs
  where
    linearize (Pair d0 d1) d = Pair d0 (linearize d1 d)
    linearize t d = Pair t d
    go (Pair d0 d1) use (s:stock) = Pair (rewind d0 use) (go d1 (use <> pure s) stock)
    go e@(Pair d0 d1) _ Nil        = unsafeThrow ("not enough vars for: " <> show_term e)
    go t use _                     = rewind t use

tuple Nil = unsafeThrow "not enough vars"
tuple (v:Nil) = v
tuple (v:vs)  = Pair v (tuple vs)

var_stock = map (VC 0) $ "s":"t":"u":"v":"w":"a":"b":"c":Nil

enough_vars :: Term -> List VarName -> List VarName
enough_vars t vs = go t $ vs
  where
    go t Nil = unsafeThrow "exceeded variable stock"
    go (Pair t1 t2) (v:vars) = v : go t2 vars
    go _ (v:vars) = v:Nil


subst t@(Con s) _ _ = t
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
subst (Push x g) v st = Push (subst x v st) (subst g v st)
subst (Proj n g) v st = Proj n (subst g v st)
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

occurs (Con s) v = Tuple false v
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
occurs (Push t1 t2) v
  = let (Tuple f1 v1@(VC c1 _)) = occurs t1 v
        (Tuple f2 v2@(VC c2 _)) = occurs t2 v
     in (Tuple (f1 || f2)  (if c1 > c2 then v1 else v2))
occurs (Proj _ s) v = occurs s v
occurs (Lam x body) v
  | x == v    = (Tuple false v)
  | otherwise = occurs body v

check_eta (Lam v (App t (Var v')))
  | v == v' && (let (Tuple flag _) = occurs t v in not flag) = t
check_eta term = term

check_sigma (Set (Pair d1 (Lam v (Set d2 r2))) r0) =
  let dom = Pair d1 $ abs v d2
      rng = p ! r0 % (_1 p * ((Lam v r2) % _1 p % _2 (Spl 1 p)))
   in Set dom rng
  where
    abs v (Pair x y) = Pair (Lam v x) (abs v y)
    abs v t          = Lam v t
check_sigma term = term


{- for tracing evaluations and debugging... -}

note_reduction label redex = tell $ singleton (Tuple label redex)

mweval term = runWriter (meval' term Nil)

meval' t@(Con s) _ = pure t
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
    (Set t1 t2)  -> note_reduction "range" (Rng s') *> meval' t2 stack
    t            -> munwind (Rng t) stack
meval' e@(Cct t0) stack = case stack of
  Nil -> do
    f <- meval' t0 Nil
    case f of
      Set t1 g@(Lam v (Set t2 t3)) -> do
        note_reduction "concat" f
        let (Tuple t vars ) = unrollDom eval f var_stock
        note_reduction "unrolled outer dom" (Pair t (tuple $ map Var vars))
        g' <-  meval' (g % tuple (map Var vars)) Nil
        let (Tuple t' vars') = unrollDom eval g' (drop (length vars) var_stock)
            dom = rerollDom t t' (vars <> vars')
        note_reduction "unrolled inner dom" (Pair t' (tuple $ map Var vars))
        note_reduction "rerolled dom" dom
        meval' (Set dom (p ! get_rng (g % (_1 (Spl (length vars) p))) % (_2 (Spl (length vars) p)))) Nil
      t -> pure (Cct t)
  (a:_) -> unsafeThrow ("trying to apply a set: "
                  <> show_term e <> " to " <> show_term a)
meval' e@(Spl n t0) stack = case stack of
  Nil -> do
    f <- meval' t0 Nil
    case f of
      Pair t1 t2 -> pure $ Pair (leftSplit n f) (rightSplit n f)
      t          -> pure (Spl n t)
  (a:_) -> unsafeThrow ("trying to apply a split: "
                  <> show_term e <> " to " <> show_term a)
  where
    leftSplit n = case _ of
      Pair x p -> if n == 1 then x else Pair x (leftSplit (n-1) p)
      _        -> unsafeThrow ("bad leftSplit " <> show n <> ": " <> show_term p <> " within " <> show_term e)
    rightSplit n = case _ of
      Pair x p -> if n == 1 then p else rightSplit (n-1) p
      _        -> unsafeThrow ("bad rightSplit " <> show n <> ": " <> show_term p <> " within " <> show_term e)
meval' e@(Push t1 t2) stack = case stack of
  Nil -> do
    t1' <- meval' t1 Nil
    t2' <- meval' t2 Nil
    pure $ Push t1' t2'
  (s:_) -> unsafeThrow ("trying to apply a push: " <> show_term e <> " to " <> show_term s)
meval' e@(Proj n t0) stack = do
  f <- meval' t0 Nil
  case f of
    Push x g -> if n == 0 then note_reduction "proj" (Push x g) *> meval' x stack else meval' (Proj (n-1) g) stack
    Var v    -> pure $ Proj n (Var v)
    _        -> unsafeThrow ("bad proj " <> show n <> ": " <> show_term g <> " within " <> show_term e)

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


-- a little DSL for building lambda terms

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
infixl 8 App as %
lam (Var v) body = Lam v body
lam _ _ = unsafeThrow "ill-formed abstraction"
infixr 6 lam as !
_1 = Fst
_2 = Snd
infixr 7 Pair as *
make_set p = Set (make_con "some" % p) (x ! x)
get_dom = Dom
get_rng = Rng
set' = flip Set
infix 5 set' as |?
set = identity
conc = Cct
make_con = Con
ix = flip Proj
infixl 9 ix as !!
infixl 9 Push as ~

{- term pretty printing -}

showRight disp = case _ of
  t@(Set _ _)  -> disp t
  t@(Pair _ _) -> disp t
  t@(Var _)    -> disp t
  t@(Con _)    -> disp t
  t            -> parens (disp t)
showLeft disp = case _ of
  t@(Lam _ _) -> parens (disp t)
  t           -> disp t
parens s = "(" <> s <> ")"

type Formatter =
  { lam' :: String, arr' :: String, app' :: String
  , con' :: String -> String, var' :: VarName -> String
  , lb' :: String, rb' :: String, mid' :: String, la' :: String, ra' :: String
  , fst' :: String, snd' :: String, dom' :: String, rng' :: String, elem' :: String
  , prep' :: String
  }

show_formatted_term form term depth
  | depth <= 0 = "..."
  | otherwise  = showt term
  where
    showt = case _ of
      Con s -> form.con' s
      Var v -> form.var' v
      Lam v body -> form.lam' <> form.var' v <> form.arr' <> showt' body
      App t1 t2 -> showLeft showt' t1 <> form.app' <> showRight showt' t2
      Pair t1 t2 -> form.la' <> showt' t1 <> ", " <> showt' t2 <> form.ra'
      Fst p -> form.fst' <> showRight showt p
      Snd p -> form.snd' <> showRight showt p
      e@(Set _ cond) ->
        let Tuple t vars = unrollDom eval e var_stock
            getvar (v:vs) = Tuple (Var v) vs
            getvar Nil = unsafeThrow "getvar error in show_term"
            showDom t vs = let (Tuple v rest) = getvar vs in
              case t of
                Pair a b ->
                  showt v <> form.elem' <> showt a <> ", " <> showDom b rest
                t' -> showt v <> form.elem' <> showt t'
         in form.lb' <> showt' (eval $ cond % (tuple $ map Var vars)) <>
            form.mid' <> showDom t vars <> form.rb'
      Dom p -> form.dom' <> showRight showt p
      Rng p -> form.rng' <> showRight showt p
      Cct t -> "concat " <> showRight showt t
      Spl n t -> "splitAt " <> show n <> " " <> showRight showt t
      Push x g -> showt x <> form.prep' <> showt g
      Proj n g -> showt g <> "~{" <> show n <> "}"
    showt' term = show_formatted_term form term (depth - 1)

default_term_form =
  { lam' : "\\", arr' : ". ", app' : " ", con' : identity, var' : showVar
  , lb' : "[", rb' : "]", mid' : " | ", la' : "<", ra' : ">"
  , fst' : "fst ", snd' : "snd ", elem' : " <- ", dom' : "sdom ", rng' : "srng "
  , prep' : ":"
  }

show_term term = show_formatted_term default_term_form term 100
show_hs term = show_formatted_term hs_form term 100
  where
    hs_form = default_term_form { lam' = "\\textbackslash ", arr' = " -> ", la' = "(", ra' = ")" }
show_tex term = show_formatted_term tex_form term 100
  where
    tex_form =
      { lam' : "\\ensuremath{\\lambda}", arr' : ".\\;", app' : "\\,"
      , con' : \s -> "\\textsf{" <> s <> "}", var' : showv
      , lb' : "\\ensuremath{\\{}", rb' : "\\ensuremath{\\}}"
      , mid' : "\\ensuremath{\\mid}", la' : "\\ensuremath{\\langle}", ra' : "\\ensuremath{\\rangle}"
      , fst' : "\\textsf{fst} ", snd' : "\\textsf{snd} "
      , elem' : "\\ensuremath{\\in}" , dom' : "\\textsf{dom} ", rng' : "\\textsf{rng} "
      , prep' : "{~}"
      }
    showv (VC n s) = "\\ensuremath{" <> s <> (if n == 0 then "" else "_{" <> show n <> "}") <> "}"

instance Show Term where
   show term = genericShow term


{- tests for evaluation -}

free_vars term = go term [] []
  where
    go (Con s) bound free = free
    go (Var v) bound free = if v `elem` bound then free else v `cons` free
    go (App t1 t2) bound free =
      go t1 bound $ go t2 bound free
    go (Lam v body) bound free = go body (v `cons` bound) free
    go (Pair t1 t2) bound free =
      go t1 bound $ go t2 bound free
    go (Fst p) bound free = go p bound free
    go (Snd p) bound free = go p bound free
    go (Set t1 t2) bound free =
      go t1 bound $ go t2 bound free
    go (Dom p) bound free = go p bound free
    go (Rng p) bound free = go p bound free
    go (Cct p) bound free = go p bound free
    go (Spl _ p) bound free = go p bound free
    go (Push x g) bound free =
      go x bound $ go g bound free
    go (Proj _ g) bound free = go g bound free

term_equal_p term1 term2 = go term1 term2 (Nil /\ Nil /\ 0)
  where
  go (Con s1) (Con s2) _ = s1 == s2
  go (Var v1) (Var v2) (bdic1 /\ bdic2 /\ _) =
    case (Tuple (lookup v1 bdic1) (lookup v2 bdic2)) of
    (Tuple (Just bv1) (Just bv2)) -> bv1 == bv2 -- both v1 v2 are bound to the same val
    (Tuple Nothing Nothing)       -> v1 == v2   -- both v1 and v2 are free
    _                             -> false
  go (Lam v1 b1) (Lam v2 b2) (bdic1 /\ bdic2 /\ counter) =
    go b1 b2
      (Tuple ((Tuple v1 counter):bdic1) (Tuple ((Tuple v2 counter):bdic2) (counter+1)))
  go (App t1 t1') (App t2 t2') env =
    go t1  t2  env &&
    go t1' t2' env
  go (Pair t1 t1') (Pair t2 t2') env =
    go t1  t2  env &&
    go t1' t2' env
  go (Fst p1) (Fst p2) env = go p1 p2 env
  go (Snd p1) (Snd p2) env = go p1 p2 env
  go (Set t1 t1') (Set t2 t2') env =
    go t1  t2  env &&
    go t1' t2' env
  go (Dom p1) (Dom p2) env = go p1 p2 env
  go (Rng p1) (Rng p2) env = go p1 p2 env
  go (Cct p1) (Cct p2) env = go p1 p2 env
  go (Spl n p1) (Spl m p2) env = n == m && go p1 p2 env
  go (Push x g) (Push x' g') env = x == x' && go g g' env
  go _ _ _ = false

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
         (a!b!a%a),
  expect (show_term $ evalFinal ea)
       "[saw t s | s <- person, t <- some (\\a1. and (cat a1) (near s a1))]"
  ]
  where
    ea =
      let person = make_con "person"
          some = make_con "some"
          a1 = make_var "a1"
          and' = make_con "and"
          cat = make_con "cat"
          near = make_con "near"
          saw = make_con "saw"
       in set (
            (a ! b ! (saw % ((get_rng (some % (a1 ! (and' % (cat % a1) % (near % a % a1))))) % b) % a))
          |? person * (a ! (get_dom (some % (a1 ! and' % (cat % a1) % (near % a % (a1))))))
          )

all_tests = and [ free_var_tests, alpha_comparison_tests,
                  subst_tests, eval_tests, mweval_tests ]


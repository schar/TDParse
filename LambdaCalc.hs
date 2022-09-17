{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

-- | The following is adapted from Oleg Kiselyov's normal order
-- lambda calculator:
-- https://okmij.org/ftp/Computation/lambda-calc.html#lambda-calculator-haskell
-- Here we add constructs and reductions for pairs and "sets"
-- Sets are encoded as abstractions with explicit domains:
-- {f x | x <- dom} ~~> Set dom (\x -> f x)

module LambdaCalc
    ( eval, evalFinal
    , (!), (%), make_var, make_con
    , (|?), set, make_set, get_dom, get_rng, conc
    , (*), _1, _2
    -- , a,b,c,x,y,z,f,g,h,p,q
    , show_term, show_tex, show_hs
    , Term
    )
  where

import Prelude hiding ((!), (*))
import Control.Monad.Writer
import Data.Maybe
import Data.Monoid

type VColor = Int
data VarName = VC VColor String deriving (Eq,Show)
showVar (VC color name) | color == 0 = name
                        | otherwise  = name <> show color
data Term
  = Con String | Var VarName | App Term Term | Lam VarName Term
  | Pair Term Term | Fst Term | Snd Term
  | Set Term Term | Dom Term | Rng Term | Cct Term | Spl Int Term
  deriving Eq -- (,Show)

eval term = fix openEval [] term
openEval eval' s e = case (e, s) of
  (,) (Con c)                   []     -> e
  (,) (Con c)                   _      -> unwind e s
  (,) (Var v)                   []     -> e
  (,) (Var v)                   _      -> unwind e s
  (,) (Lam v body)              []     -> check_eta $ Lam v (ev body)
  (,) (Lam v body)              (t:ts) -> eval' ts (subst body v t)
  (,) (App t1 t2)               _      -> eval' (t2:s) t1
  (,) (Pair t1 t2)              []     -> Pair (ev t1) (ev t2)
  (,) (Pair _  _ )              (a:_)  -> error ("trying to apply a pair: "
                                                 ++ show_term e ++ " to " ++ show_term a)
  (,) (Fst (ev -> Pair t1 _))   _      -> eval' s t1 -- might not need eval' here
  (,) (Fst (ev -> t))           _      -> unwind (Fst t) s
  (,) (Snd (ev -> Pair _ t2))   _      -> eval' s t2 -- or here
  (,) (Snd (ev -> t))           _      -> unwind (Snd t) s
  (,) (Set t1 t2)               []     -> Set (ev t1) (ev t2) 
  (,) (Set _  _ )               (a:_)  -> error ("trying to apply a set: "
                                                 ++ show_term e ++ " to " ++ show_term a)
  (,) (Dom (ev -> Set t1 _))    []     -> ev t1 -- or here
  (,) (Dom (ev -> t))           []     -> Dom t
  (,) (Dom _)                   (a:_)  -> error ("trying to apply a dom: "
                                                 ++ show_term e ++ " to " ++ show_term a)
  (,) (Rng (ev -> Set _ t2))    _      -> eval' s t2
  (,) (Rng (ev -> t))           _      -> unwind (Rng t) s
  (,) (Cct t0)                  []     -> case ev t0 of
    f@(Set t1 g@(Lam v (Set t2 t3)))
      | null $ [t1,t2] >>= free_vars   ->
      let (t, vars ) = unrollDom f var_stock
          (t',vars') = unrollDom (ev $ g % tuple (map Var vars)) (drop (length vars) var_stock)
          dom = rerollDom t t' (vars ++ vars')
       in ev $ Set dom (p ! get_rng (g % (_1 (Spl (length vars) p))) % (_2 (Spl (length vars) p)))
    t                                  -> Cct t
  (,) (Cct _)                   (a:_)  -> error ("trying to apply a set: "
                                                 ++ show_term e ++ " to " ++ show_term a)
  (,) (Spl n t0)                []     -> case ev t0 of
    f@(Pair t1 t2)                     -> Pair (leftSplit n f) (rightSplit n f)
    t                                  -> Spl n t
  (,) (Spl _ _)                 (a:_)  -> error ("trying to apply a split: "
                                                 ++ show_term e ++ " to " ++ show_term a)
  where
    unwind t [] = t
    unwind t (t1:rest) = unwind (App t $ ev t1) rest
    ev = eval' []
    leftSplit 1 (Pair x _) = x
    leftSplit n (Pair x p) = Pair x (leftSplit (n-1) p)
    leftSplit n p = error ("bad leftSplit " <> show n <> ": " <> show_term p <> " within " <> show_term e)
    rightSplit 1 (Pair _ y) = y
    rightSplit n (Pair _ p) = (rightSplit (n-1) p)
    rightSplit n p = error ("bad rightSplit " <> show n <> ": " <> show_term p <> " within " <> show_term e)

evalFinal term = fix (openFinal . openEval) [] term
openFinal eval' s e = case (e, s) of
  (,) (Dom (ev -> Set t1 t2)) [] -> ev t1
  (,) (Dom (ev -> t))         [] -> t
  (,) (Rng (ev -> Set _ t2))  _  -> eval' s t2
  (,) (Rng _)                 _  -> eval' s (a ! a)
  (,) (Cct t0)                [] -> case ev t0 of
    f@(Set t1 g@(Lam v (Set t2 t3))) ->
      let (t, vars ) = unrollDom f var_stock
          (t',vars') = unrollDom (ev $ g % tuple (map Var vars)) (drop (length vars) var_stock)
          dom = rerollDom t t' (vars ++ vars')
       in ev $ Set dom (p ! get_rng (g % (_1 (Spl (length vars) p))) % (_2 (Spl (length vars) p)))
    t                                  -> Cct t
  (,) _                       _  -> eval' s e
  where
    ev = eval' []

unwind t [] = t
unwind t (t1:rest) = unwind (App t $ eval t1) rest

rewind t [] = t
rewind t (v1:rest) = (Lam v1 (rewind t rest))

unrollDom :: Term -> [VarName] -> (Term, [VarName])
unrollDom e@(Set dom rng) vs =
  let vars' = enough_vars dom vs
      occs = map (\s -> (occurs e s, s)) vars'
      vars = map (\((f,v'),s) -> if f then bump_color v' s else s) occs
      getvar (v:vs) = (Var v, vs)
      getvar [] = {- (make_var "s", []) -} error "getvar error in show_term"
      go t vs apps = let (v, rest) = getvar vs in
        case t of
          Pair t1 t2 -> Pair (eval $ unwind t1 apps) (go t2 rest (v:apps))
          _          -> (eval $ unwind t apps)
   in (go dom vars [], vars)
unrollDom e _ = error ("trying to unroll dom: " ++ show e)

rerollDom d d' vs = makeItRain (linearize d d') [] vs
  where
    linearize (Pair d0 d1) d = Pair d0 (linearize d1 d)
    linearize t d = Pair t d

makeItRain (Pair d0 d1) use (s:stock) = Pair (rewind d0 use) (makeItRain d1 (use ++ [s]) stock)
makeItRain e@(Pair d0 d1) _ [] = error ("not enough vars for: " ++ show_term e)
makeItRain t use _ = rewind t use

tuple [] = error "not enough vars"
tuple (v:[]) = v
tuple (v:vs)  = Pair v (tuple vs)

var_stock = map (VC 0) $ "s":"t":"u":"v":"w":"a":"b":"c":[]

enough_vars :: Term -> [VarName] -> [VarName]
enough_vars t vs = go t $ vs
  where
    go t [] = error "exceeded variable stock"
    go (Pair t1 t2) (v:vars) = v : go t2 vars
    go _ (v:vars) = v:[]


subst term v (Var v') | v == v' = term -- identity substitution
subst t@(Con s) _ _ = t
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
    (f,x_occur_st) = occurs st x
    (x',body') =
      if f
        then let x_uniq_st_v       = bump_color' (bump_color x x_occur_st) v
                 (bf,x_occur_body) = occurs body x_uniq_st_v
                 x_unique =
                   if bf then bump_color x_uniq_st_v x_occur_body else x_uniq_st_v
             -- in (x_unique,subst body x (Var x'))
              in (x_unique,subst body x (Var x_unique))
     else (x,body)

bump_color (VC color name) (VC color' _) =
  (VC ((max color color')+1) name)
bump_color' v1@(VC _ name) v2@(VC _ name') =
  if name==name' then bump_color v1 v2 else v1

occurs (Con s) v = (False, v)
occurs (Var v'@(VC c' name')) v@(VC c name)
    | not (name == name')  = (False, v)
    | c == c'              = (True, v)
    | otherwise            = (False,v')
occurs (App t1 t2) v =
  let (f1,v1@(VC c1 _)) = occurs t1 v
      (f2,v2@(VC c2 _)) = occurs t2 v
   in (f1 || f2, if c1 > c2 then v1 else v2)
occurs (Pair t1 t2) v
  = let (f1, v1@(VC c1 _)) = occurs t1 v
        (f2, v2@(VC c2 _)) = occurs t2 v
     in ((f1 || f2),  (if c1 > c2 then v1 else v2))
occurs (Fst p) v = occurs p v
occurs (Snd p) v = occurs p v
occurs (Set t1 t2) v
  = let (f1, v1@(VC c1 _)) = occurs t1 v
        (f2, v2@(VC c2 _)) = occurs t2 v
     in ((f1 || f2),  (if c1 > c2 then v1 else v2))
occurs (Dom s) v = occurs s v
occurs (Rng s) v = occurs s v
occurs (Cct s) v = occurs s v
occurs (Spl _ s) v = occurs s v
occurs (Lam x body) v
  | x == v    = (False,v)
  | otherwise = occurs body v

check_eta (Lam v (App t (Var v')))
  | v == v' && (let (flag,_) = occurs t v in not flag) = t
check_eta term = term


note_reduction :: a -> b -> Writer [(a,b)] ()
note_reduction label redex = tell [(label,redex)]

mweval term = runWriter (meval' term [])

meval' :: Term -> [Term] -> Writer [(String,Term)] Term
meval' t@(Con s) _ = return t
meval' t@(Var v) [] = return t
meval' (Lam v body) [] = do { body' <- meval' body []; mcheck_eta $ Lam v body' }
meval' a@(Lam v body) (t: rest) = do
  note_reduction "beta" (App a t)
  meval' (subst body v t) rest
meval' (App t1 t2) stack = meval' t1 (t2:stack)
meval' t@(Var v) stack = munwind t stack
meval' e@(Pair t1 t2) stack = case stack of
  [] -> do
    t1' <- meval' t1 []
    t2' <- meval' t2 []
    return $ Pair t1' t2'
  (s:_) -> error ("trying to apply a pair: " ++ show_term e ++ " to " ++ show_term s)
meval' (Fst p) stack = do
  p' <- meval' p []
  case p' of
    (Pair t1 t2)  -> note_reduction "fst" (Fst p') *> meval' t1 stack
    t             -> munwind (Fst t) stack
meval' (Snd p) stack = do
  p' <- meval' p []
  case p' of
    (Pair t1 t2)  -> note_reduction "snd" (Snd p') *> meval' t2 stack
    t             -> munwind (Snd t) stack
meval' e@(Set t1 t2) stack = case stack of
  [] -> do
    t1' <- meval' t1 []
    t2' <- meval' t2 []
    return $ Set t1' t2'
  (s:_) -> error ("trying to apply a set: " ++ show_term e ++ " to " ++ show_term s)
meval' e@(Dom s) stack = case stack of
  [] -> do
    s' <- meval' s []
    case s' of
      (Set t1 t2)  -> note_reduction "domain" (Dom s') *> return t1
      t            -> return (Dom t)
  (s:_) -> error ("trying to apply the domain of a set: " ++ show_term e ++ " to " ++ show_term s)
meval' (Rng s) stack = do
  s' <- meval' s []
  case s' of
    (Set t1 t2)  -> note_reduction "range" (Rng s') *> meval' t2 stack
    t            -> munwind (Rng t) stack
meval' e@(Cct t0) stack = case stack of
  [] -> do
    f <- meval' t0 []
    case f of
      Set t1 g@(Lam v (Set t2 t3)) -> do
        note_reduction "concat" f
        let (t, vars ) = unrollDom f var_stock
        note_reduction "unrolled outer dom" (Pair t (tuple $ map Var vars))
        g' <-  meval' (g % tuple (map Var vars)) []
        let (t',vars') = unrollDom g' (drop (length vars) var_stock)
            dom = rerollDom t t' (vars ++ vars')
        note_reduction "unrolled inner dom" (Pair t' (tuple $ map Var vars))
        note_reduction "rerolled dom" dom
        meval' (Set dom (p ! get_rng (g % (_1 (Spl (length vars) p))) % (_2 (Spl (length vars) p)))) []
      t -> return (Cct t)
  (a:_) -> error ("trying to apply a set: "
                  ++ show_term e ++ " to " ++ show_term a)
meval' e@(Spl n t0) stack = case stack of
  [] -> do
    f <- meval' t0 []
    case f of
      Pair t1 t2 -> return $ Pair (leftSplit n f) (rightSplit n f)
      t          -> return (Spl n t)
  (a:_) -> error ("trying to apply a split: "
                  ++ show_term e ++ " to " ++ show_term a)
  where
    leftSplit 1 (Pair x _) = x
    leftSplit n (Pair x p) = Pair x (leftSplit (n-1) p)
    leftSplit n p = error ("bad leftSplit " <> show n <> ": " <> show_term p <> " within " <> show_term e)
    rightSplit 1 (Pair _ y) = y
    rightSplit n (Pair _ p) = (rightSplit (n-1) p)
    rightSplit n p = error ("bad rightSplit " <> show n <> ": " <> show_term p <> " within " <> show_term e)

munwind t [] = return t
munwind t (t1:rest) = do { t1' <- meval' t1 []; munwind (App t t1') rest }

mcheck_eta red@(Lam v (App t (Var v')))
      | v == v' && (let (flag,_) = occurs t v in not flag)
        = do { note_reduction "eta" red; return t }
mcheck_eta term = return term

[a,b,c,x,y,z,f,g,h,p,q] =
   map make_var ["a","b","c","x","y","z","f","g","h","p","q"]

-- a little DSL for building lambda terms

make_var = Var . VC 0
infixl 8 %
(%) = App
infixr 6 !
(Var v) ! body = Lam v body
_1 = Fst
_2 = Snd
infixr 7 *
(*) = Pair
make_set p = Set (make_var "some" % p) (x ! x)
get_dom = Dom
get_rng = Rng
infix 5 |?
(|?) = flip Set
set = id
conc = Cct
make_con = Con

showRight disp = \case
  t@(Set _ _)  -> disp t
  t@(Pair _ _) -> disp t
  t@(Var _)    -> disp t
  t@(Con _)    -> disp t
  t            -> parens (disp t)
showLeft disp = \case
  t@(Lam _ _) -> parens (disp t)
  t           -> disp t
parens s = "(" ++ s ++ ")"

data Formatter = Form
  { lam' :: String, arr' :: String
  , lb' :: String, rb' :: String, mid' :: String, la' :: String, ra' :: String
  , fst' :: String, snd' :: String, dom' :: String, rng' :: String, elem' :: String
  }

show_formatted_term form term depth
  | depth <= 0 = "..."
  | otherwise = showt term
  where
    showt = \case
      Con s -> s
      Var v -> showVar v
      Lam v body -> lam' form ++ (showVar v) ++ arr' form ++ (showt' body)
      App t1 t2 -> showLeft showt' t1 ++ " " ++ showRight showt' t2
      Pair t1 t2 -> la' form ++ showt' t1 ++ ", " ++ showt' t2 ++ ra' form
      Fst p -> fst' form ++ showRight showt p
      Snd p -> snd' form ++ showRight showt p
      e@(Set _ cond) ->
        let (t,vars) = unrollDom e var_stock
            getvar (v:vs) = (Var v,vs)
            getvar [] = {- (make_var "s", []) -} error "getvar error in show_term"
            showDom t vs = let (v,rest) = getvar vs in
              case t of
                Pair a b ->
                  showt v ++ elem' form ++ showt a ++ ", " ++ showDom b rest
                t' -> showt v ++ elem' form ++ showt t'
         in lb' form ++ showt' (eval $ cond % (tuple $ map Var vars)) ++
            mid' form ++ showDom t vars ++ rb' form
      Dom p -> dom' form ++ showRight showt p
      Rng p -> rng' form ++ showRight showt p
      Cct t -> "concat " ++ showRight showt t
      Spl n t -> "splitAt " ++ show n ++ showRight showt t
    showt' term = show_formatted_term form term (depth - 1)

default_term_form = Form
  { lam' = "\\", arr' = ". "
  , lb' = "[", rb' = "]", mid' = " | ", la' = "<", ra' = ">"
  , fst' = "fst ", snd' = "snd ", elem' = " <- ", dom' = "sdom ", rng' = "srng "
  }

show_term term = show_formatted_term default_term_form term 100
show_hs term = show_formatted_term hs_form term 100
  where
    hs_form = default_term_form { lam' = "\\textbackslash ", arr' = " -> ", la' = "(", ra' = ")" }
show_tex term = show_formatted_term tex_form term 100
  where
    tex_form = Form
      { lam' = "\\ensuremath{\\lambda}", arr' = ".\\;"
      , lb' = "\\ensuremath{\\{}", rb' = "\\ensuremath{\\}}"
      , mid' = "\\ensuremath{\\mid}", la' = "\\ensuremath{\\langle}", ra' = "\\ensuremath{\\rangle}"
      , fst' = "\\textsf{fst} ", snd' = "\\textsf{snd} "
      , elem' = "\\ensuremath{\\in}" , dom' = "\\textsf{dom} ", rng' = "\\textsf{rng} "
      }

deriving instance Show Term

free_vars term = free_vars' term [] []
   where
     free_vars' (Con s) bound free = free
     free_vars' (Var v) bound free = if v `elem` bound then free else v:free
     free_vars' (App t1 t2) bound free =
       free_vars' t1 bound $ free_vars' t2 bound free
     free_vars' (Lam v body) bound free = free_vars' body (v:bound) free
     free_vars' (Pair t1 t2) bound free =
       free_vars' t1 bound $ free_vars' t2 bound free
     free_vars' (Fst p) bound free = free_vars' p bound free
     free_vars' (Snd p) bound free = free_vars' p bound free
     free_vars' (Set t1 t2) bound free =
       free_vars' t1 bound $ free_vars' t2 bound free
     free_vars' (Dom p) bound free = free_vars' p bound free
     free_vars' (Rng p) bound free = free_vars' p bound free
     free_vars' (Cct p) bound free = free_vars' p bound free
     free_vars' (Spl _ p) bound free = free_vars' p bound free

term_equal_p term1 term2 = term_equal_p' term1 term2 ([],[],0)
  where
  term_equal_p' (Con s1) (Con s2) _ = s1 == s2
  term_equal_p' (Var v1) (Var v2) (bdic1,bdic2,_) =
    case (lookup v1 bdic1,lookup v2 bdic2) of
    (Just bv1,Just bv2) -> bv1 == bv2
    (Nothing,Nothing)   -> v1 == v2
    _                   -> False

  term_equal_p' (Lam v1 b1) (Lam v2 b2) (bdic1,bdic2,counter) =
    term_equal_p' b1 b2
     ((v1,counter):bdic1,(v2,counter):bdic2,counter+1)

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

  term_equal_p' (Cct p1) (Cct p2) env = term_equal_p' p1 p2 env
  term_equal_p' (Spl n p1) (Spl m p2) env = n == m && term_equal_p' p1 p2 env

  term_equal_p' _ _ _ = False

expectg (==) exp expected_result =
       exp == expected_result ||
       error ("Test case failure: Expected " ++ (show expected_result)
                ++ ", received: " ++ (show exp))
expect:: (Eq a,Show a) => a -> a -> Bool
expect = expectg (==)
expectd = expectg term_equal_p -- test using comparison modulo alpha-renaming
notexpectd = expectg (\x y -> not $ term_equal_p x y)
free_var_tests = and [
   expect (map Var (free_vars $ x))  [x],
   expect (map Var (free_vars $ x!x)) [],
   expect (map Var (free_vars $ p%y%z)) [p,y,z],
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
   expectd    (a%b%(c)) ((a%b)%c),
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
   expect (fmap (fmap show_term) $ snd $ mweval $ a ! (x ! a ! x % a) % a)
         [("beta","(\\x. \\a. x a) a"),("eta","\\a1. a a1")],
   expect (show_term $ evalFinal ea)
        "[saw t s | s <- person, t <- some (\\a1. and (cat a1) (near s a1))]"
   ]

ea = let person = make_con "person"
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


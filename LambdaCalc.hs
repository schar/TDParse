{-# LANGUAGE LambdaCase #-}

-- | The following is adapted from Oleg Kiselyov's normal order
-- lambda calculator:
-- https://okmij.org/ftp/Computation/lambda-calc.html#lambda-calculator-haskell
-- Here we add constructs and reductions for pairs and "sets"
-- Sets are encoded as abstractions with explicit domains:
-- {f x | x <- dom} ~~> Set dom (\x -> f x)

module LambdaCalc
    ( eval, evalFinal
    , (!), (%)
    , make_var
    , (|?), set, make_set, get_dom, get_rng
    , (*), _1, _2
    -- , a,b,c,x,y,z,f,g,h,p,q
    , show_term
    , show_tex
    , show_hs
    , Term
    ) where

import Prelude hiding ((!), (*))
import Control.Monad.Writer

type VColor = Int
data VarName = VC VColor String deriving (Eq)
data Term
  = Var VarName | App Term Term | Lam VarName Term
  | Pair Term Term | Fst Term | Snd Term
  | Set Term Term | Domain Term | Range Term
            deriving Eq -- (,Show)

evalFinal term = evalFinal' term []
evalFinal' (Domain (Set t1 t2)) [] = t1
evalFinal' (Domain t) [] = t
evalFinal' (Range (Set t1 t2)) stack = evalFinal' t2 stack
evalFinal' (Range t) stack = evalFinal' (a ! a) stack
evalFinal' t stack = eval' t stack

eval term = eval' term []
eval' t@(Var v) [] = t
eval' (Lam v body) [] = check_eta $ Lam v (eval body)
eval' (Lam v body) (t: rest) = eval' (subst body v t) rest
eval' (App t1 t2) stack = eval' t1 (t2:stack)
eval' t@(Var v) stack = unwind t stack
eval' e@(Pair t1 t2) stack = case stack of
  []   -> Pair (eval t1) (eval t2)
  (s:_) -> error ("trying to apply a pair: " ++ show e ++ " to " ++ show s)
eval' (Fst p) stack =
  case eval p of
    (Pair t1 t2)  -> eval' t1 stack
    t             -> unwind (Fst t) stack
eval' (Snd p) stack =
  case eval p of
    (Pair t1 t2)  -> eval' t2 stack
    t             -> unwind (Snd t) stack
eval' e@(Set t1 t2) stack = case stack of
  [] -> Set (eval t1) (eval t2)
  (s:_) -> error ("trying to apply a set: " ++ show e ++ " to " ++ show s)
eval' e@(Domain s) stack = case stack of
  []   ->
    case eval s of
      (Set t1 t2)  -> t1
      t            -> Domain t
  (s:_) -> error ("trying to apply the domain of a set: " ++ show e ++ " to " ++ show s)
eval' (Range s) stack =
  case eval s of
    (Set t1 t2)  -> eval' t2 stack
    t            -> unwind (Range t) stack

unwind t [] = t
unwind t (t1:rest) = unwind (App t $ eval t1) rest

subst term v (Var v') | v == v' = term -- identity substitution
subst t@(Var x) v st | x == v    = st
                     | otherwise = t
subst (Pair t1 t2) v st = Pair (subst t1 v st) (subst t2 v st)
subst (Fst p) v st = Fst (subst p v st)
subst (Snd p) v st = Snd (subst p v st)
subst (Set t1 t2) v st = Set (subst t1 v st) (subst t2 v st)
subst (Domain s) v st = Domain (subst s v st)
subst (Range s) v st = Range (subst s v st)
subst (App t1 t2) v st = App (subst t1 v st) $ (subst t2 v st)
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
occurs (Domain s) v = occurs s v
occurs (Range s) v = occurs s v
occurs (Lam x body) v
  | x == v    = (False,v)
  | otherwise = occurs body v

check_eta (Lam v (App t (Var v')))
  | v == v' && (let (flag,_) = occurs t v in not flag) = t
check_eta term = term

note_reduction label redex = tell [(label,redex)]

mweval term = runWriter (meval' term [])

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
meval' e@(Domain s) stack = case stack of
  [] -> do
    s' <- meval' s []
    case s' of
      (Set t1 t2)  -> note_reduction "domain" (Domain s') *> return t1
      t            -> return t
  (s:_) -> error ("trying to apply the domain of a set: " ++ show_term e ++ " to " ++ show_term s)
meval' (Range s) stack = do
  s' <- meval' s []
  case s' of
    (Set t1 t2)  -> note_reduction "map" (Range s') *> meval' t2 stack
    t            -> meval' (a ! a) stack

munwind t [] = return t
munwind t (t1:rest) = do { t1' <- meval' t1 []; munwind (App t t1') rest }

mcheck_eta red@(Lam v (App t (Var v')))
      | v == v' && (let (flag,_) = occurs t v in not flag)
        = do { note_reduction "eta" red; return t }
mcheck_eta term = return term

make_var = Var . VC 0
[a,b,c,x,y,z,f,g,h,p,q] =
   map make_var ["a","b","c","x","y","z","f","g","h","p","q"]

-- a little DSL for building lambda terms

infixl 8 %
(%) = App
infixr 6 !
(Var v) ! body = Lam v body
_1 = Fst
_2 = Snd
infixr 7 *
(*) = Pair
make_set s = Set (make_var s) (x ! x)
get_dom = Domain
get_rng = Range
infix 5 |?
(|?) = flip Set
set = id

instance Show VarName where
   show (VC color name) = if color == 0 then name
                                         else name ++ "" ++ (show color)

enough_vars :: Term -> [VarName]
enough_vars t = go t $ map (VC 0) ["s","t","u","v","w","a","b","c"]
  where
    go t [] = []
    go (Pair t1 t2) (v:vars) = v : go t2 vars
    go _ (v:vars) = [v]

applyAll t = \case
  [] -> t
  v:vs -> applyAll (t % v) vs

showRight = \case
  (App _ _) -> parens
  (Fst _)   -> parens
  (Snd _)   -> parens
  (Lam _ _) -> parens
  _         -> id
showLeft = \case
  (Lam _ _) -> parens
  _         -> id
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
      Var v -> show v
      Lam v body -> lam' form ++ (show v) ++ arr' form ++ (showt' body)
      App t1 t2 -> showLeft t1 (showt' t1) ++ " " ++ showRight t2 (showt' t2)
      Pair t1 t2 -> la' form ++ showt' t1 ++ ", " ++ showt' t2 ++ ra' form
      Fst p -> fst' form ++ showRight p (showt p)
      Snd p -> snd' form ++ showRight p (showt p)
      e@(Set dom cond) ->
        let vars' = enough_vars dom
            occs = map (\s -> (occurs e s, s)) vars'
            vars = map (\((f, v'), s) -> Var $ if f then bump_color v' s else s) occs
            getvar (v:vs) = (v, vs)
            unrollDom t vs apps = let (v, rest) = getvar vs in
              case t of
                Pair t1 t2 ->
                  showt v ++ elem' form ++ showt (eval $ applyAll t1 apps) ++ ", " ++ unrollDom t2 rest (v:apps)
                _ ->
                  showt v ++ elem' form ++ showt (eval $ applyAll t apps)
         in lb' form ++ showt' (eval $ applyAll cond vars) ++ mid' form ++ unrollDom dom vars [] ++ rb' form
      Domain p -> dom' form ++ showRight p (showt p)
      Range p -> rng' form ++ showRight p (showt p)
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
      { lam' = "\\lambda ", arr' = ". "
      , lb' = "\\{ ", rb' = "\\} ", mid' = " \\mid ", la' = "\\langle ", ra' = "\\rangle "
      , fst' = "\\textsf{fst} ", snd' = "\\textsf{snd} ", elem' = " \\in " , dom' = "\\textsf{dom} ", rng' = "\\textsf{rng} "
      }

deriving instance Show Term

free_vars term = free_vars' term [] []
   where
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
     free_vars' (Domain p) bound free = free_vars' p bound free
     free_vars' (Range p) bound free = free_vars' p bound free

term_equal_p term1 term2 = term_equal_p' term1 term2 ([],[],0)
  where
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

  term_equal_p' (Domain p1) (Domain p2) env = term_equal_p' p1 p2 env
  term_equal_p' (Range p1) (Range p2) env = term_equal_p' p1 p2 env

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
         (a!b!a%a)
   -- , expect (show $ mweval $ a ! (x ! a ! x % a) % a)
   --        "((\\a. a),[(\"beta\",(\\x. (\\a. x a)) a),(\"eta\",(\\a~1. a a~1))])"
   ]

all_tests = and [ free_var_tests, alpha_comparison_tests,
                  subst_tests, eval_tests, mweval_tests ]

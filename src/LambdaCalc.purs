{-# LANGUAGE FlexibleContexts #-}

module LambdaCalc where

import Prelude

-- the following imports are required for a tracing eval
import Control.Monad.Writer
import Effect.Exception.Unsafe ( unsafeThrow )
import Data.List
import Data.String ( replaceAll, Pattern(..), Replacement(..) )
import Data.Array ( cons )
import Data.Foldable ( lookup, and )
import Data.Maybe
import Data.Tuple
import Data.Tuple.Nested

type VColor = Int  -- the "color" of a variable. 0 is the "transparent color"
data VarName = VC VColor String
derive instance Eq VarName
data Term = Var VarName | Ap Term Term | Lam VarName Term
derive instance Eq Term
eval term = eval' term Nil
eval' t@(Var v) Nil = t -- just a variable with nothing to apply it to
eval' (Lam v body) Nil = check_eta $ Lam v (eval body)
eval' (Lam v body) (t: rest) = eval' (subst body v t) rest
eval' (Ap t1 t2) stack = eval' t1 (t2:stack)
eval' t@(Var v) stack = unwind t stack
unwind t Nil = t
unwind t (t1:rest) = unwind (Ap t $ eval t1) rest
subst term v (Var v') | v == v' = term -- identity substitution
subst t@(Var x) v st | x == v    = st
                     | otherwise = t
subst (Ap t1 t2) v st = Ap (subst t1 v st) $ (subst t2 v st)
subst t@(Lam x _) v _ | v == x  = t  -- v is shadowed in lambda form
subst (Lam x body) v st = (Lam x' (subst body' v st))
    where
       bump_color (VC color name) (VC color' _) =
                                     (VC ((max color color')+1) name)
       bump_color' v1@(VC _ name) v2@(VC _ name') =
                   if name==name' then bump_color v1 v2 else v1
       (Tuple f x_occur_st) = occurs st x
       (Tuple x' body') =
            if f
               then let x_uniq_st_v = bump_color' (bump_color x x_occur_st) v
                        (Tuple bf x_occur_body) = occurs body x_uniq_st_v
                        x_unique = if bf
                                      then bump_color x_uniq_st_v x_occur_body
                                      else x_uniq_st_v
                    in (Tuple x_unique (subst body x (Var x_unique))) -- x_unique used to x'; not sure if this is safe
               else (Tuple x body)
occurs (Var v'@(VC c' name')) v@(VC c name)
    | not (name == name')  = (Tuple false v)
    | c == c'              = (Tuple true  v)
    | otherwise            = (Tuple false v')
occurs (Ap t1 t2) v = let (Tuple f1 v1@(VC c1 _)) = occurs t1 v
                          (Tuple f2 v2@(VC c2 _)) = occurs t2 v
                      in (Tuple (f1 || f2)  (if c1 > c2 then v1 else v2))
occurs (Lam x body) v | x == v    = (Tuple false v)
                    | otherwise = occurs body v
check_eta (Lam v (Ap t (Var v')))
      | v == v' && (let (Tuple flag _) = occurs t v in not flag) = t
check_eta term = term
note_reduction label redex = tell $ singleton (Tuple label redex)

meval' :: Term -> List Term -> Writer (List (Tuple String Term)) Term
meval' t@(Var v) Nil = pure t -- just a variable with nothing to apply it to
meval' (Lam v body) Nil =
  do body' <- meval' body Nil
     mcheck_eta $ Lam v body'
meval' a@(Lam v body) (t: rest) = do
                                note_reduction "beta" (Ap a t)
                                meval' (subst body v t) rest
meval' (Ap t1 t2) stack = meval' t1 (t2:stack)
meval' t@(Var v) stack = munwind t stack
munwind :: Term -> List Term -> Writer (List (Tuple String Term)) Term
munwind t Nil = pure t
munwind t (t1:rest) =
  do t1' <- meval' t1 Nil
     munwind (Ap t t1') rest
mcheck_eta red@(Lam v (Ap t (Var v')))
      | v == v' && (let (Tuple flag _) = occurs t v in not flag)
                                       = do note_reduction "eta" red
                                            pure t
mcheck_eta term = pure term
mweval term = runWriter (meval' term Nil)
make_var = Var <<< VC 0  -- a convenience function
-- [a,b,c,x,y,z,f,g,h,p,q] =
--    map make_var ["a","b","c","x","y","z","f","g","h","p","q"]
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

app = Ap
infixl 8 app as #
lam (Var v) body = Lam v body
lam _ _ = unsafeThrow "ill-formed abstraction"
infixr 6 lam as ^            -- a better notation for a lambda-abstraction
instance Show VarName where
   show (VC color name) = if color == 0 then name
                                         else name <> "" <> (show color)
-- The pretty-printer for terms
--    show_term term max_depth
-- prints terms up to the specific depth. Beyond that, we print ...

show_term _ depth | depth <= 0 = "..."
show_term term depth = showt term
 where
   showt (Var v) = show v       -- show the variable regardless of depth
   showt (Lam v body) = "(\\" <> (show v) <> " -> " <> (showt' body) <> ")"
   showt (Ap t1 t2@(Ap _ _)) = (showt' t1) <> " " <> "(" <> (showt' t2) <> ")"
   showt (Ap t1 t2) = (showt' t1) <> " " <> (showt' t2)
   showt' term = show_term term (depth - 1)

show_tex _ depth | depth <= 0 = "..."
show_tex term depth = showt term
 where
   showt (Var v) = replaceAll (Pattern "'") (Replacement "$$'$$") $ show v       -- show the variable regardless of depth
   showt (Lam v body) = "(\\lambda " <> (show v) <> ". " <> (showt' body) <> ")"
   showt (Ap t1 t2@(Ap _ _)) = (showt' t1) <> " " <> "(" <> (showt' t2) <> ")"
   showt (Ap t1 t2) = (showt' t1) <> " " <> (showt' t2)
   showt' term = show_tex term (depth - 1)

show_hs _ depth | depth <= 0 = "..."
show_hs term depth = showt term
 where
   showt (Var v) = replaceAll (Pattern "\'") (Replacement "$$'$$") $ show v       -- show the variable regardless of depth
   showt (Lam v body) = "(\\textbackslash " <> (show v) <> " -> " <> (showt' body) <> ")"
   showt (Ap t1 t2@(Ap _ _)) = (showt' t1) <> " " <> "(" <> (showt' t2) <> ")"
   showt (Ap t1 t2) = (showt' t1) <> " " <> (showt' t2)
   showt' term = show_hs term (depth - 1)

instance Show Term where
   show term = show_term term 100
free_vars:: Term -> Array VarName
free_vars term = free_vars' term [] []
   where
     -- free_vars' term list-of-bound-vars list-of-free-vars-so-far
     free_vars' (Var v) bound free = if v `elem` bound then free else v `cons` free
     free_vars' (Ap t1 t2) bound free =
              free_vars' t1 bound $ free_vars' t2 bound free
     free_vars' (Lam v body) bound free = free_vars' body (v `cons` bound) free
term_equal_p term1 term2 = term_equal_p' term1 term2 (Nil /\ Nil /\ 0)
  where
  -- both terms are variables
  term_equal_p' (Var v1) (Var v2) (bdic1 /\ bdic2 /\ _) =
    case (Tuple (lookup v1 bdic1) (lookup v2 bdic2)) of
    (Tuple (Just bv1) (Just bv2)) -> bv1 == bv2 -- both v1 v2 are bound to the same val
    (Tuple Nothing Nothing)       -> v1 == v2   -- both v1 and v2 are free
    _                             -> false

  -- both terms are abstractions
  term_equal_p' (Lam v1 b1) (Lam v2 b2) (bdic1 /\ bdic2 /\ counter) =
              -- we bind both v1 and v2 to the common value,
              -- and compare the bodies of the abstractions in the
              -- amended environment
     term_equal_p' b1 b2
                (Tuple ((Tuple v1 counter):bdic1) (Tuple ((Tuple v2 counter):bdic2) (counter+1)))

  -- both terms are applications
  term_equal_p' (Ap t1 t1') (Ap t2 t2') env =
     term_equal_p' t1  t2  env &&
     term_equal_p' t1' t2' env

  -- otherwise, the terms do not compare
  term_equal_p' _ _ _ = false
-- Generic tester
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
   expect (map Var (free_vars $ x^x)) [],
   expect (map Var (free_vars $ x#y#z)) [x,y,z],
   expect (map Var (free_vars $ x^x#y)) [y],
   expect (map Var (free_vars $ (x^x#y)#(x#y#z))) [y,x,y,z],
   expect (map Var (free_vars $ (x^x^x#y)#(x^y^x#y))) [y]
   ]
alpha_comparison_tests = and [
   expectd    x x,
   notexpectd x y,
   expectd    (x) x,
   expectd    x  ((x)),
   expectd    (x) ((x)),
   expectd    (x#y#(z)) ((x#y)#z),
   expectd    (((a#(b#c))#(q))#(p#f)) (a#(b#c)#q#(p#f)),
   notexpectd (a#(b#c)#q#(p#f)) (a#b#c#q#(p#f)),
   notexpectd (x^x) (x^y),
   expectd    (x^x) (y^y),
   expectd    (x^x^x) (y^y^y),
   notexpectd (x^(x#x)) $ y^(y#x),
   notexpectd (y^(y#x)) $ x^(x#x),
   expectd    (y^(y#x)) $ z^(z#x),
   notexpectd (x^y^(x#y)) $ f^f^(f#f),
   expectd    (x^x^(x#x)) $ f^f^(f#f),
   expectd    (x^y^(y#y)) $ f^f^(f#f),
   expectd    (f^x^f#x) $ f^x^f#x,
   notexpectd (f^x^f#x) $ f^x^x,
   expectd    (f^x^f#x) $ g^x^(g#x),
   expectd    (f^x^f#x) $ g^y^g#y,
   expectd    (g^y^g#y) $ f^x^f#x,
   notexpectd (g^y^g#x) $ f^x^f#x,
   notexpectd (f^x^f#x) (g^y^g#x)
   ]

subst_tests = and [
  expectd (subst (c^c)  (VC 1 "c") c) (z^z),
  expectd (subst (Lam (VC 1 "c") (Ap (Var (VC 0 "c")) (Ap (Var (VC 1 "c"))
                 (Ap (Var (VC 2 "c")) (Var (VC 3 "c") )))))
                 (VC 0 "c") (Ap (Var (VC 1 "c")) (Var (VC 2 "c"))))
        (a^(Var $ VC 1 "c")#(Var $ VC 2 "c")#
           (a#((Var $ VC 2 "c")#(Var $ VC 3 "c"))))
  ]

eval_tests = and [
   expectd (eval $ ((x^(a#b#x))#(a^a#b))) $
         (a#b#(p^p#b)),
   expectd (eval $ (((f^x^(f#x))#g)#z))
         (g#z),
   expectd (eval $ ((c^f^x^f#(c#f#x))#(f^x^x)))
         (f^f),
   expectd (((x^x#x)#(x^x#x)))
         ((p^p#p)#(q^q#q)),
   expectd (eval $ ((x^y)#((x^x#x)#(x^x#x))))
         y,
   expectd (eval $ ((x^y^(f#x#y#y))#(g#y)))
         (z^(f#(g#y)#z#z)),
   expectd (eval $ ((c^f^x^f#(c#f#x))#(f^x^(f#x))))
         (g^x^(g#(g#x))),
   expectd (eval $ a ^ (x ^ a ^ a # x) # (a # x))
         (a^b^(b#(a#x))),
   expectd (eval $ a ^ (x ^ a ^ x # a) # a)
         (z^z),
   expectd (eval $ a ^ (x ^ b ^ x # a) # a)
         (a^b^a#a)
   ]
mweval_tests = and [
   expectd (fst $ mweval $ ((x^(a#b#x))#(a^a#b))) $
         (a#b#(p^p#b)),
   expectd (fst $ mweval $ (((f^x^(f#x))#g)#z))
         (g#z),
   expectd (fst $ mweval $ ((c^f^x^f#(c#f#x))#(f^x^x)))
         (f^f),
   expectd (fst $ mweval $ ((x^y)#((x^x#x)#(x^x#x))))
         y,
   expectd (fst $ mweval $ ((x^y^(f#x#y#y))#(g#y)))
         (z^(f#(g#y)#z#z)),
   expectd (fst $ mweval $ ((c^f^x^f#(c#f#x))#(f^x^(f#x))))
         (g^x^(g#(g#x))),
   expectd (fst $ mweval $ a ^ (x ^ a ^ a # x) # (a # x))
         (a^b^(b#(a#x))),
   expectd (fst $ mweval $ a ^ (x ^ a ^ x # a) # a)
         (z^z),
   expectd (fst $ mweval $ a ^ (x ^ b ^ x # a) # a)
         (a^b^a#a)
   -- , expect (show $ mweval $ a ^ (x ^ a ^ x # a) # a)
   --        "((\\a. a),[(\"beta\",(\\x. (\\a. x a)) a),(\"eta\",(\\a~1. a a~1))])"
   ]

all_tests = and [ free_var_tests, alpha_comparison_tests,
                  subst_tests, eval_tests, mweval_tests ]

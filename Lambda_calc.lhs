;; -*-mode: Outline;  -*-

	A normal order lambda calculator

The present calculator implements what seems to be an efficient and
elegant algorithm of normal order reductions. The algorithm is "more
functional" than the traditionally used approach. The algorithm seems
identical to that employed by yacc sans one critical difference.  The
calculator also takes a more "functional" approach to the hygiene of
beta-substitutions, which is achieved by coloring of identifiers where
absolutely necessary. This approach is "more functional" because it
avoids a "global" counter or the threading of the paint bucket through
the whole the process. The integration of the calculator with Haskell
lets us store terms in variables and easily and intuitively combine
them.
A monadic version of the calculator gives us the trace of the reduction
steps.

$Id: Lambda_calc.lhs,v 2.3 2005/05/05 20:52:25 oleg Exp oleg $

* Preliminaries

> {-# LANGUAGE FlexibleContexts #-}
>
> module Lambda_calc
>     ( eval
>     , (^)                   -- abstraction: x ^ body
>     , (#)                   -- application: a # b
>     , a,b,c,x,y,z,f,g,h,p,q -- variables
>     , make_var              -- to make more variables
>     , show_term	            -- to print out the term to the specific depth
>     , show_tex
>     , show_hs
>     , term_equal_p	        -- compare two terms modulo alpha-renaming
>     , expectd		            -- a regression testing predicate
>     , Term                  -- SC: don't export Term constructors
>     ) where
>
> import Prelude hiding ((^))
>
> -- the following imports are required for a tracing eval
> import Control.Monad.Writer

* The syntax of terms: the language to parse

The primitive construct of the language is a variable
(a.k.a. identifier).  In our language, identifiers are colored for the
sake of the hygiene of beta-substitutions (see below for more
details).

> type VColor = Int  -- the "color" of a variable. 0 is the "transparent color"
> data VarName = VC VColor String deriving (Eq)

Identifiers along with composite terms (abstractions and applications)
make up our language:

> data Term = Var VarName | A Term Term | L VarName Term
>             deriving Eq -- (,Show)

* Normal-order reduction as parsing

The traditional recipe for normal-order reductions includes an
unpleasant phrase "cook until done". The phrase makes it necessary to
keep track of reduction attempts, and implies an ugly iterative
algorithm. We're proposing what seems to be efficient and elegant
technique that can be implemented through intuitive re-writing rules.
Our calculator, like yacc, possesses a stack and works by doing a
sequence of 'shift' and 'reduce' steps. We consider an application (a
b) as a _delayed_ normalization. We delay dealing with 'b' and with
the application until we figure out what to do with the term 'a'. A
sequence of applications (A (A (A t1 t2) t3) t4) is then a to-do list:
terms t2, t3, and t4 are to be applied, in that order, to term t1.

The calculator's stack contains all the terms to be applied to the
current one. The evaluator does a 'reduce' step when it is sure it has
the redex or it is sure it does not (e.g., (x (\y. y)) where 'x' is
free). When the evaluator is not sure about the current term, it
'shifts'. The only difference from yacc is that the lambda-calculator
"reparses" the result after the successful reduce step. The source and
the target languages of our "parser" (lambda-calculator) are the same;
therefore, the parser can indeed apply itself.


** The main evaluator: evaluate a term as a top-level term

> eval term = eval' term []

** eval', the "parser"

The function eval' does all the work. The function is the parser of
our term language. Given a term 'term' and the stack [t1 t2 ...], the
invocation "eval' term [t1 t2 ...]" tries to reduce the the
application (A (A (A term t1) t2) t3 ...)

*** parsing variables and abstractions on the empty stack

> eval' t@(Var v) [] = t -- just a variable with nothing to apply it to

If we see a naked lambda at the top-level, it will remain the lambda
form but with the reduced body. However, eta-reductions can remove
abstractions. Therefore, after we reduced the body, we have to check
if an eta-reduction applies. Hmm, I wonder what happened to gamma...

> eval' (L v body) [] = check_eta $ L v (eval body)

*** we found the redex

If the current term is a lambda form and the stack is non-empty, we
have a (beta-) redex! We pop the stack, reduce (A (L v body) t), and
re-evaluate the result as the new current term.

This re-evaluation is the trait that distinguishes eval from
yacc. Yacc does not re-parse a reduced term.

> eval' (L v body) (t: rest) = eval' (subst body v t) rest

*** doing shifts
If the current term is an application, we _shift_.

> eval' (A t1 t2) stack = eval' t1 (t2:stack)

*** and unwinds
The current term is a variable and the stack (t1:t2...) is non-empty. That is,
the current context is (A (A (A x t1) t2) ...). Neither of these applications
can be reduced. Therefore, we just reduce t1, t2 ... separately

> eval' t@(Var v) stack = unwind t stack

** Unwind the stack

Given a term t and the stack [t1 t2 ...], unwind the stack and make a
term (A (A (A t t1') t2') ...) where t1' t2' ... are reduced t1 t2
... -- as top-level terms.

> unwind t [] = t
> unwind t (t1:rest) = unwind (A t $ eval t1) rest

** Comparisons with the lambda-calculator as a scheme macro

A (meta-) lambda-calculator implemented as a Scheme macro
   http://pobox.com/~oleg/ftp/Computation/rewriting-rule-lambda.txt

was the first implementation of the parsing normalization
algorithm. The rewriting-lambda implementation had a few differences,
and some elegance. The major difference is that all composite terms
were represented as S-expressions. An application of a term 'b' to
term 'a' was an S-expression (a b). An abstraction was also an
S-expression, with a distinguishes symbol 'lambda' at the head. This
uniform realization of terms as S-expressions is very close to the
original lambda-calculus. In Church's formulation, the language was
realized by a string of characters and delimiters on the
paper. 'lambda' was just a typographic convention.

A series of applications '(A (A (A t t1) t2) ...)' in the
rewriting-lambda implementation were represented by a list (t t1 t2
...). The stack is "rolled-in" in a term itself.  Therefore, the
rewriting-lambda implementation did not need a separate stack.

** Lambda-reductions as parsing without an explicit heterogeneous stack

Ken Shan wrote about his experience of using the above algorithm to
implement a lambda-calculator in Twelf for *typed* lambda-terms.  In
that case, the explicit stack of terms becomes a nuisance. Indeed, the
stack has to be a polymorphic, heterogeneous list, of a type (forall
t. [Term t]).

Ken Shan re-wrote the algorithm to elide the explicit stack. In
particular, his algorithm weaved the 'unwind' process into the eval
itself. His algorithm can be simplified to the following evaluation
rule:

-> eval t@(Var v) = t
-> eval (L v body) = check_eta $ L v (eval body)
-> eval (A (L v body) t2) = eval $ subst body v t2
-> eval (A t1@(A _ _) t2) = case eval t1 of
->   r@(L _ _) -> eval (A r t2)
->   r         -> A r $ eval t2 -- the head is normalized, but no redex
-> eval (A t1 t2) = A t1 $ eval t2

We can check whether unwinding is in progress simply by seeing whether
"r" is (not) an abstraction.  The stack is still there, but it is
implicit in the activation frames of eval. In contrast, eval' is "more
tail-recursive" (at least in the main branch), with the explicit
stack. Fortunately, Twelf is capable of a polymorphic recursion, which
is needed to effectively implement a polymorphic list as a sequence of
activation frames of a polymorphic function. Yet another example of
the practical need for polymorphic recursion!


* beta-substitutions

To complete the calculator, we need a beta substitutor:
"subst term v st" replaces free occurrences of identifier v in
'term' with st.

When performing a substitution in a lambda form:
     (lambda x body)[y<-term]
we generally need to repaint the bound variable 'x' to avoid an
accidental capture of free variables in 'term'. Rather than replacing
the bound variable with a generated identifier, we repaint it. This
way, we keep track of the original name of the identifier. This
approach makes the result more pleasant to look at.

** trivial cases

> subst term v (Var v') | v == v' = term -- identity substitution
> subst t@(Var x) v st | x == v    = st
>		       | otherwise = t
> subst (A t1 t2) v st = A (subst t1 v st) $ (subst t2 v st)
> subst t@(L x _) v _ | v == x  = t  -- v is shadowed in lambda form

** substitution in the lambda term

Now, we're about to substitute in the body of a (L x body) form.  If x
is free in the inserted term 'st', a capture is possible. Therefore, we
first need to check if x is indeed free in st. If so, we have to
repaint x within the body. We use an auxiliary function "occurs term
v" which returns (Bool,VarName). The result is (False,_) if v does not
appear free in term. Otherwise, the result is "(True, v')" where v' has
the same name as v but perhaps a different color. The color of v' is the
largest possible color of all variables with the name of "v" that
occur free in 'term'. To make the repainted x truly unique, we need to
paint it with the color that occurs neither in the inserted term,
nor in the body. The repainted x should also be different from v, the
variable being substituted.

The repainting of a variable (alpha-conversions) is itself a
substitution. Therefore, we can use the same function 'subst'.

> subst (L x body) v st = (L x' (subst body' v st))
>     where
>        (f,x_occur_st) = occurs st x
>        (x',body') =
>             if f then
>		     let x_uniq_st_v = bump_color' (bump_color x x_occur_st) v
>                        (bf,x_occur_body) = occurs body x_uniq_st_v
>		         x_unique = if bf
>                                     then bump_color x_uniq_st_v x_occur_body
>                                     else x_uniq_st_v
>                    in (x_unique,subst body x (Var x'))
>            else (x,body)
>        bump_color (VC color name) (VC color' _) =
>                                      (VC ((max color color')+1) name)
>        bump_color' v1@(VC _ name) v2@(VC _ name') =
>                    if name==name' then bump_color v1 v2 else v1


Note how the body of the if expression refers to its own result:
x'. A lazy language has its elegance.

> occurs (Var v'@(VC c' name')) v@(VC c name)
>     | not (name == name')  = (False, v)
>     | c == c'              = (True, v)
>     | otherwise            = (False,v')
> occurs (A t1 t2) v = let (f1,v1@(VC c1 _)) = occurs t1 v
>                          (f2,v2@(VC c2 _)) = occurs t2 v
>                      in (f1 || f2, if c1 > c2 then v1 else v2)
> occurs (L x body) v | x == v    = (False,v)
>                     | otherwise = occurs body v

* eta-reductions

Check to see if an eta-reduction applies, that is, that we're to
reduce (L v (A t v)) where v is not free in t.

> check_eta (L v (A t (Var v')))
>       | v == v' && (let (flag,_) = occurs t v in not flag) = t
> check_eta term = term


* Tracing (monadic) eval

The following modification to the basic `eval' makes it possible to
see the list of all reductions. First, we re-write the evaluator
in a monadic notation.

> note_reduction label redex = tell [(label,redex)]
>
> meval' t@(Var v) [] = return t -- just a variable with nothing to apply it to
> meval' (L v body) [] = do { body' <- meval' body []; mcheck_eta $ L v body' }
> meval' a@(L v body) (t: rest) = do
>                                 note_reduction "beta" (A a t)
>				  meval' (subst body v t) rest
> meval' (A t1 t2) stack = meval' t1 (t2:stack)
> meval' t@(Var v) stack = munwind t stack
> munwind t [] = return t
> munwind t (t1:rest) = do { t1' <- meval' t1 []; munwind (A t t1') rest }

> mcheck_eta red@(L v (A t (Var v')))
>       | v == v' && (let (flag,_) = occurs t v in not flag)
>         = do { note_reduction "eta" red; return t }
> mcheck_eta term = return term

There are two implementations. One uses the pure, Writer monad

> mweval term = runWriter (meval' term [])

We can now use mweval instead of eval to see both the result and the
trace.


The other uses the IO monad and `print' to print the reductions.
Alas, for that monad we need to make IO an instance of MonadWriter (and so
to make `tell' print out the result). Alas, MonadWriter is a two-parameter
class and so isn't Haskell98. We'd like to keep this code Haskell98.



* A few conveniences

** Pre-defined identifiers

Now, let's define a few identifiers.

> make_var = Var . VC 0  -- a convenience function

> [a,b,c,x,y,z,f,g,h,p,q] =
>    map make_var ["a","b","c","x","y","z","f","g","h","p","q"]

** Operators to build applications and abstractions

> infixl 8 #
> (#) = A
> infixr 6 ^		-- a better notation for a lambda-abstraction
> (Var v) ^ body = L v body

Note that
LC> x # y # z
means "(x y) z" and
LC> x ^ y ^ x # y # z
means (\x. (\y. x y z))

The first example:

LC> eval $ (x ^ x) # y

** A custom printer for the terms

To make the output prettier, we add a custom printer.

When we pretty-print a variable, we keep in mind that 0 is the
"transparent color".

> instance Show VarName where
>    show (VC color name) = if color == 0 then name
>                                          else name ++ "" ++ (show color)

Jón Fairbairn suggested limiting the print depth and showing '...'
for deeply nested expressions: "I picked up the 'print ... for deeply
nested expressions' habit from the SKIM (S K I Machine). It worked
very well, I think." This printing convention is a good idea indeed,
especially in the context of a non-strict language like
Haskell. Suppose 't' is a tail-divergent term, that is, the term whose
depth increases with reductions and whose normal redexes move away
from the root. For example, the Y-combinator and its application to a
variable are tail-divergent terms (in contrast, an application of the
Y-combinator to itself is not a tail-divergent term). When we print
'eval t', Haskell evaluates the term as it is being
printed. Therefore, branches that will not be shown because of the
depth limitation won't be evaluated. Hence we can print divergent
terms!

> -- The pretty-printer for terms
> --    show_term term max_depth
> -- prints terms up to the specific depth. Beyond that, we print ...
>
> show_term (Var v) _ = show v       -- show the variable regardless of depth
> show_term _ depth | depth <= 0 = "..."
> show_term term depth = showt term
>  where
>    showt (L v body) = "(\\" ++ (show v) ++ " -> " ++ (showt' body) ++ ")"
>    showt (A t1 t2@(A _ _)) = (showt' t1) ++ " " ++ "(" ++ (showt' t2) ++ ")"
>    showt (A t1 t2) = (showt' t1) ++ " " ++ (showt' t2)
>    showt' term = show_term term (depth - 1)
>
> show_tex (Var v) _ = show v       -- show the variable regardless of depth
> show_tex _ depth | depth <= 0 = "..."
> show_tex term depth = showt term
>  where
>    showt (L v body) = "(\\lambda " ++ (show v) ++ ". " ++ (showt' body) ++ ")"
>    showt (A t1 t2@(A _ _)) = (showt' t1) ++ " " ++ "(" ++ (showt' t2) ++ ")"
>    showt (A t1 t2) = (showt' t1) ++ " " ++ (showt' t2)
>    showt' term = show_tex term (depth - 1)
>
> show_hs (Var v) _ = show v       -- show the variable regardless of depth
> show_hs _ depth | depth <= 0 = "..."
> show_hs term depth = showt term
>  where
>    showt (L v body) = "(\\textbackslash " ++ (show v) ++ " -> " ++ (showt' body) ++ ")"
>    showt (A t1 t2@(A _ _)) = (showt' t1) ++ " " ++ "(" ++ (showt' t2) ++ ")"
>    showt (A t1 t2) = (showt' t1) ++ " " ++ (showt' t2)
>    showt' term = show_hs term (depth - 1)
>
> instance Show Term where
>    show term = show_term term 100




** Determining the list of free variables of a term

The returned list may contain duplicates (which do not change the
semantics of the list). The real work is done by a function
free_vars', which maintains the lists of bound variables and of free
variables seen so far. The former is the black list for free
variables.

> free_vars:: Term -> [VarName]
> free_vars term = free_vars' term [] []
>    where
>      -- free_vars' term list-of-bound-vars list-of-free-vars-so-far
>      free_vars' (Var v) bound free = if v `elem` bound then free else v:free
>      free_vars' (A t1 t2) bound free =
> 		free_vars' t1 bound $ free_vars' t2 bound free
>      free_vars' (L v body) bound free = free_vars' body (v:bound) free


** Comparing terms modulo renaming of bound variables

Comparison of terms modulo renaming of bound variables is akin to
unification of the corresponding trees, with bound identifiers playing
the role of unification variables within their branches.  Whenever we
come across two abstractions (L v body) and (L v' body') we "bind"
both variables v and v' to the same unique value. We then proceed as
in the conventional tree matching algorithm. The real work of matching
terms is done by a predicate term_equal_p', which takes the two terms
in question and the environment. The latter is a triple: a binding
dictionary for the first term, ditto for the second term, and the
counter to maintain the unique binding values.

> term_equal_p term1 term2 = term_equal_p' term1 term2 ([],[],0)
>   where
>   -- both terms are variables
>   term_equal_p' (Var v1) (Var v2) (bdic1,bdic2,_) =
>     case (lookup v1 bdic1,lookup v2 bdic2) of
>     (Just bv1,Just bv2) -> bv1 == bv2 -- both v1 v2 are bound to the same val
>     (Nothing,Nothing)   -> v1 == v2   -- both v1 and v2 are free
>     _                   -> False
>
>   -- both terms are abstractions
>   term_equal_p' (L v1 b1) (L v2 b2) (bdic1,bdic2,counter) =
> 		-- we bind both v1 and v2 to the common value,
> 		-- and compare the bodies of the abstractions in the
> 		-- amended environment
>      term_equal_p' b1 b2
> 		  ((v1,counter):bdic1,(v2,counter):bdic2,counter+1)
>
>   -- both terms are applications
>   term_equal_p' (A t1 t1') (A t2 t2') env =
>      term_equal_p' t1  t2  env &&
>      term_equal_p' t1' t2' env
>
>   -- otherwise, the terms do not compare
>   term_equal_p' _ _ _ = False


* Examples

Long notation:
LC>-- eval (A (L x (A (Var x) (Var x))) (L y (A (Var y) (Var z))))
LC>-- -- A (Var (VC 0 "z")) (Var (VC 0 "z"))

Short notation:
LC> eval $ (x ^ x # x) # (y ^ y # z)
LC> -- z z

The following example checks the hygiene of substitutions:

LC> -- eval (L a (A (L x (L a (A (Var x) (Var a)))) (Var a)))
LC> --- - L (VC 0 "a") (L (VC 1 "a") (A (Var (VC 0 "a")) (Var (VC 1 "a"))))

LC> eval $ a ^ (x ^ a ^ a # x) # (a # x)
LC> --- (\a. (\a~1. a~1 (a x)))

LC> subst (c^c)  (VC 1 "c") c
LC> -- (\c~2. c~2)

LC> -- Substituting (c~1 c~2) for c in (\c~1. c (c~1 (c~2 c~3)))
LC> subst (L (VC 1 "c") (A (Var (VC 0 "c")) (A (Var (VC 1 "c")) (A (Var (VC 2 "c")) (Var (VC 3 "c") ))))) (VC 0 "c") (A (Var (VC 1 "c")) (Var (VC 2 "c")))
LC> -- (\c~4. c~1 c~2 (c~4 (c~2 c~3)))

Now check eta-reductions:
LC> eval $ a ^ (x ^ a ^ x # a) # a
LC> --- (\a. a)

LC> eval $ a ^ (x ^ b ^ x # a) # a
LC> --- (\a. (\b. a a))

LC>-- compute a*(a+b)
LC> eval $ (a ^ b ^ f ^ a # (x ^ ((b # f) # (a # f # x))))
LC>  # (f ^ x ^ f # (f # x))	-- Numeral 2
LC>  # (f ^ x ^ f # x)		-- Numeral 1

LC> --- (\f. (\x. f (f (f (f (f (f x)))))))

Check determining the list of free variables in a term
LC> free_vars $ (x^x^y)#(x#y#z)         -- [y,x,y,z]
LC> free_vars $ eval $ (x^x^y)#(x#y#z)  -- [y]

We can compare two terms modulo alpha-renaming of bound identifiers
LC> term_equal_p (x^x^x#y) $ x^z^z#y  -- True
LC> term_equal_p (x^x^x#y) $ z^x^z#y  -- False

Evaluate and print an "infinite" term
LC> let y_comb = f^((p^p#p) # (c ^ f#(c#c))) in eval $ y_comb#c
LC> -- c (c (c (c (c (c (c (c (c (c (...))))))))))

* More extensive test cases

> -- Generic tester
> expectg (==) exp expected_result =
>        exp == expected_result ||
>        error ("Test case failure: Expected " ++ (show expected_result)
> 		  ++ ", received: " ++ (show exp))
> expect:: (Eq a,Show a) => a -> a -> Bool
> expect = expectg (==)
> expectd = expectg term_equal_p -- test using comparison modulo alpha-renaming
> notexpectd = expectg (\x y -> not $ term_equal_p x y)

> free_var_tests = and [
>    expect (map Var (free_vars $ x))  [x],
>    expect (map Var (free_vars $ x^x)) [],
>    expect (map Var (free_vars $ p#y#z)) [p,y,z],
>    expect (map Var (free_vars $ x^x#y)) [y],
>    expect (map Var (free_vars $ (x^x#y)#(x#y#z))) [y,x,y,z],
>    expect (map Var (free_vars $ (x^x^x#y)#(x^y^x#y))) [y]
>    ]

> alpha_comparison_tests = and [
>    expectd    x x,
>    notexpectd x y,
>    expectd    (x) x,
>    expectd    x  ((x)),
>    expectd    (x) ((x)),
>    expectd    (a#b#(c)) ((a#b)#c),
>    expectd    (((a#(b#c))#(q))#(p#f)) (a#(b#c)#q#(p#f)),
>    notexpectd (a#(b#c)#q#(p#f)) (a#b#c#q#(p#f)),
>    notexpectd (x^x) (x^y),
>    expectd    (x^x) (y^y),
>    expectd    (x^x^x) (y^y^y),
>    notexpectd (x^(x#x)) $ y^(y#x),
>    notexpectd (y^(y#x)) $ x^(x#x),
>    expectd    (y^(y#x)) $ z^(z#x),
>    notexpectd (x^y^(x#y)) $ f^f^(f#f),
>    expectd    (x^x^(x#x)) $ f^f^(f#f),
>    expectd    (x^y^(y#y)) $ f^f^(f#f),
>    expectd    (f^x^f#x) $ f^x^f#x,
>    notexpectd (f^x^f#x) $ f^x^x,
>    expectd    (f^x^f#x) $ g^x^(g#x),
>    expectd    (f^x^f#x) $ g^y^g#y,
>    expectd    (g^y^g#y) $ f^x^f#x,
>    notexpectd (g^y^g#x) $ f^x^f#x,
>    notexpectd (f^x^f#x) (g^y^g#x)
>    ]
>
> subst_tests = and [
>   expectd (subst (c^c)  (VC 1 "c") c) (z^z),
>   expectd (subst (L (VC 1 "c") (A (Var (VC 0 "c")) (A (Var (VC 1 "c"))
>                  (A (Var (VC 2 "c")) (Var (VC 3 "c") )))))
>                  (VC 0 "c") (A (Var (VC 1 "c")) (Var (VC 2 "c"))))
>         (a^(Var $ VC 1 "c")#(Var $ VC 2 "c")#
>            (a#((Var $ VC 2 "c")#(Var $ VC 3 "c"))))
>   ]
>
> eval_tests = and [
>    expectd (eval $ ((x^(a#b#x))#(a^a#b))) $
>    	   (a#b#(p^p#b)),
>    expectd (eval $ (((f^x^(f#x))#g)#z))
> 	   (g#z),
>    expectd (eval $ ((c^f^x^f#(c#f#x))#(f^x^x)))
> 	   (f^f),
>    expectd (((x^x#x)#(x^x#x)))
> 	   ((p^p#p)#(q^q#q)),
>    expectd (eval $ ((x^y)#((x^x#x)#(x^x#x))))
> 	   y,
>    expectd (eval $ ((x^y^(f#x#y#y))#(g#y)))
> 	   (z^(f#(g#y)#z#z)),
>    expectd (eval $ ((c^f^x^f#(c#f#x))#(f^x^(f#x))))
> 	   (g^x^(g#(g#x))),
>    expectd (eval $ a ^ (x ^ a ^ a # x) # (a # x))
>	   (a^b^(b#(a#x))),
>    expectd (eval $ a ^ (x ^ a ^ x # a) # a)
>	   (z^z),
>    expectd (eval $ a ^ (x ^ b ^ x # a) # a)
>          (a^b^a#a)
>    ]
> mweval_tests = and [
>    expectd (fst $ mweval $ ((x^(a#b#x))#(a^a#b))) $
>    	   (a#b#(p^p#b)),
>    expectd (fst $ mweval $ (((f^x^(f#x))#g)#z))
> 	   (g#z),
>    expectd (fst $ mweval $ ((c^f^x^f#(c#f#x))#(f^x^x)))
> 	   (f^f),
>    expectd (fst $ mweval $ ((x^y)#((x^x#x)#(x^x#x))))
> 	   y,
>    expectd (fst $ mweval $ ((x^y^(f#x#y#y))#(g#y)))
> 	   (z^(f#(g#y)#z#z)),
>    expectd (fst $ mweval $ ((c^f^x^f#(c#f#x))#(f^x^(f#x))))
> 	   (g^x^(g#(g#x))),
>    expectd (fst $ mweval $ a ^ (x ^ a ^ a # x) # (a # x))
>	   (a^b^(b#(a#x))),
>    expectd (fst $ mweval $ a ^ (x ^ a ^ x # a) # a)
>	   (z^z),
>    expectd (fst $ mweval $ a ^ (x ^ b ^ x # a) # a)
>          (a^b^a#a),
>    expect (show $ mweval $ a ^ (x ^ a ^ x # a) # a)
>           "((\\a. a),[(\"beta\",(\\x. (\\a. x a)) a),(\"eta\",(\\a~1. a a~1))])"
>    ]
>
> all_tests = and [ free_var_tests, alpha_comparison_tests,
>                   subst_tests, eval_tests, mweval_tests ]


* See also

The following two files show less and less trivial applications of the
present lambda-calculator:

LC_basic.lhs : basic lambda terms: booleans, conditionals, pairs,
Church numerals and their addition, multiplication and
exponentiation. The file shows many more examples of "algebraic
deductions" by the lambda-calculator.

LC_neg.lhs : several derivations of the predecessor functions;
comparison of Church numeral in magnitude. Negative numerals and their
addition, subtraction, multiplication and division are to come soon.

See also:
    http://pobox.com/~oleg/ftp/Computation/lambda-calc.html


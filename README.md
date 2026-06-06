# TDParse

TDParse is a small Lean 4 implementation of effect-driven interpretation, in
the sense of Bumford and Charlow's
[Effect-driven interpretation: Functors for natural language composition](https://arxiv.org/abs/2504.00316).

The central idea is that natural-language meanings can be organized around a
division between pure values and effectful computations: alternatives,
dependencies on context, stored discourse values, scope-taking, and similar
phenomena. TDParse represents those effects as semantic type constructors and
derives composition rules from their algebraic properties.

Given a binary CFG and a typed lexicon, the demo pipeline:

1. builds all CFG-licensed binary parse trees,
2. computes all effect-compatible semantic derivations for each tree,
3. evaluates those derivations as ordinary Lean values.

The intended reader is a formal semanticist who is comfortable with typed
functional programming, applicatives/monads, and algebraic data types, but may
not know Lean.

## Quick Start

Build the project:

```sh
lake build
```

Run the examples in [Main.lean](Main.lean):

```sh
lake env lean Main.lean
```

The library entry point is [TDParse.lean](TDParse.lean). The executable target
is declared in [lakefile.toml](lakefile.toml).

## File Map

- [TDParse/Data/Algebra.lean](TDParse/Data/Algebra.lean): small algebraic
  definitions missing from the Lean prelude for this project: `Reader`,
  `Cont2`, `Adjoint`, `Comonad`, and instances for `List`, `Prod`, and
  continuations.
- [TDParse/Data/Ty.lean](TDParse/Data/Ty.lean): object-language semantic types,
  effect constructors, their Lean denotations, and predicates such as
  `functor`, `applicative`, `monad`, and `adjoint`.
- [TDParse/Data/Derivation.lean](TDParse/Data/Derivation.lean): syntactic
  categories, parse trees, mode labels, typed semantic expressions, and
  heterogeneous lexicons.
- [TDParse/Combine.lean](TDParse/Combine.lean): the effect-driven composition
  search.
- [TDParse/Memoize.lean](TDParse/Memoize.lean): structurally keyed memoized
  fixed points.
- [TDParse/Display.lean](TDParse/Display.lean): pretty-printers for types,
  modes, expressions, and interpretations.
- [TDParse.lean](TDParse.lean): lexicon lookup, parsing, semantic derivation,
  and evaluation.
- [Main.lean](Main.lean): example grammars, lexicons, and `#eval` tests.

## The Pipeline

At the top level, the project implements this pipeline:

```text
List String
  -> List (Tree Cat TypedExpr)
  -> List TypedExpr
  -> List ((t : Ty) × Expr t × t.dom)
```

The three main functions are:

- `parse`: token strings to syntactic trees with typed lexical entries at the
  leaves.
- `synsem`: trees to typed semantic derivations.
- `run` / `runAs`: semantic derivations to Lean denotations.

The important design choice is that effectful interpretation is part of the
semantic type. A DP might denote a plain entity, a set of alternatives, a
context-dependent value, a stored value, or a continuation-like scope-taker.
Composition is then driven by the effects present in the daughter meanings.

Lean checks the ordinary type discipline while the code searches over the
available effect-lifting operations. There is no later dynamic check that an
expression's denotation has the right type; the expression could not have been
constructed otherwise.

## Effect-Driven Interpretation

The implementation is organized around a small inventory of semantic effects:

- `S`: indeterminacy or alternatives,
- `R`: dependence on an environment,
- `W`: stored output, used in the examples for discourse values,
- `C`: continuation-like scope-taking.

Each effect is a type constructor. If `a` is a semantic type, then `S a`,
`R^E a`, `W^E a`, and `C^r s a` are also semantic types. The composition
algorithm does not need separate lexical rules for every possible interaction
between these effects. Instead, it asks which algebraic operations the effects
support: functorial mapping, applicative sequencing, monadic join, comonadic
extension, or an adjunction.

This is the role of `combine`. It takes the semantic types of two daughters and
searches for all ways to compose them by applying the available effect
operations. The CFG decides which syntactic trees exist; effect-driven
interpretation decides which semantic derivations those trees support.

## Semantic Types

The object-language type system is defined by `Ty`:

```lean
inductive Ty where
  | nat
  | bool
  | fn (a r : Ty)
  | comp (f : FX) (a : Ty)
```

The usual notations are:

- `E`: entities/numbers, interpreted as `Nat`,
- `T`: truth values, interpreted as `Bool`,
- `a ~> b`: functions,
- `S a`: nondeterministic values, interpreted as `List a.dom`,
- `R^e a`: reader/query values, interpreted as `Reader e.dom a.dom`,
- `W^o a`: stored-output values, interpreted as `o.dom × a.dom`,
- `C^r s a`: continuation/scope values, interpreted as `(a.dom -> s.dom) -> r.dom`.

The interpretation function is:

```lean
def Ty.dom : Ty -> Type
```

For example:

```lean
E ~> T      -- Nat -> Bool
S E         -- List Nat
R^E T       -- Nat -> Bool
W^E T       -- Nat × Bool
C^T T E     -- (Nat -> Bool) -> Bool
```

The effect constructors live in `FX`: `spawn`, `query`, `store`, and `scope`.
`TDParse/Data/Ty.lean` also decides which effects support functorial mapping,
applicative sequencing, monadic join, comonadic extension, and the store/query
adjunction. Those decisions determine which composition rules are available.

## Syntax and Lexical Entries

The syntactic category inventory is the inductive type `Cat`; the grammar type
is deliberately minimal:

```lean
abbrev CFG := Cat -> Cat -> List Cat
```

A grammar is just a binary rule table: given a left category and a right
category, return all possible mother categories.

Lexical entries are stored in a heterogeneous dictionary:

```lean
inductive HDict : List Ty -> Type
  | nil : HDict []
  | cons : (Cat × Expr t) -> HDict ts -> HDict (t :: ts)
```

The point of the dependent index is that a single lexicon can store meanings of
different semantic types without erasing their types. Lookup returns a packed
semantic type together with an expression of exactly that type.

Semantic expressions are also indexed by type:

```lean
inductive Expr : Ty -> Type where
  | lex : String -> a.dom -> Expr a
  | moc : Mode a.dom b.dom c.dom -> Expr a -> Expr b -> Expr c
```

`Expr.den` evaluates an expression to its Lean denotation.

## Parsing

`parse` is an exhaustive binary chart parser. The public type is:

```lean
abbrev Parser := List String -> List (Tree Cat TypedExpr)
```

Internally, `parse` first splits possessive clitics such as `john's` into
`john` and `'s`, stores the resulting tokens in an array, and memoizes
recursive parsing by span `(lo, hi)`.

Operationally:

```text
parse(lo, hi)
  if the span is empty:
    return []
  if the span contains one token:
    return all lexical entries for that token
  otherwise:
    for each split point mid:
      for each left parse of (lo, mid):
      for each right parse of (mid, hi):
      for each category licensed by cfg left.root right.root:
        emit the corresponding tree
```

When the mother category is `CP`, the parser builds a `Tree.island` node rather
than an ordinary `Tree.node`. In `synsem`, islands filter out derivations whose
result type still contains an unresolved continuation/scope effect. This is how
the examples enforce scope islands.

The parser does not predict categories top-down. It enumerates all binary
bracketings and lets the CFG filter them.

## Effect-Driven Composition

The core algorithm is:

```lean
def combine : (u v : Ty) -> List (Combo u v)
```

Given a left type `u` and right type `v`, `combine` returns every composition
mode licensed by the relevant pure and effectful structure, packaged with the
resulting type. A `Mode α β γ` contains both a printable label and the actual
semantic operation:

```lean
structure Mode (α β γ : Type) where
  mode : ModeLabel
  op : α -> β -> γ
```

The primitive modes are:

- `FA`: forward application,
- `BA`: backward application,
- `PM`: predicate modification.

`Mode.fc` is also defined as function composition, but the current primitive
search does not generate it.

The effect-driven part is the closure of these primitive modes under operations
provided by the effects:

- `ML` / `MR`: map composition through a functor on the left/right.
- `AP`: combine two values under a compatible applicative effect.
- `UL` / `UR`: allow an applicative argument to be supplied where a pure
  argument is expected.
- `CU`: eliminate an adjoint store/query pair by counit.
- `XL`: keep the stored left context by comonadic extension.
- `EL` / `ER`: eject reader structure out of function results.
- `JN`: join nested monadic effects.
- `DN`: discharge a continuation/scope effect when its payload type matches its
  answer type.

The search has two layers:

1. Generate binary combinations: primitive modes plus one structural lift.
2. Apply unary post-processing: join and continuation discharge.

The algorithm is recursive because each lifted rule asks how to interpret the
payload types. For example, to combine `S (E ~> T)` with `S E`, `combine` first
asks how to combine `E ~> T` with `E`, then lifts the answer back through `S`.

## Normalization

Effect lifting generates many derivations that differ only in bookkeeping.
`norm` rejects a hand-written set of equivalent or uninformative mode histories.

For example, some derivations differ only in whether mapping happens before or
after a unit-like applicative step, or in the order of equivalent joins for
commutative effects. The current normalization is pragmatic: it is not a proof
of canonical form, but it keeps the output useful and the search space smaller.

The mode labels carry effect indices, so the normalization rules can distinguish
patterns such as:

```lean
| .UR f (.MR g _) => f != g
| .JN f (.MR g (.MR h _)) => not (f == g && g == h)
```

That is why the labels record not just `MR`, `ML`, `JN`, etc., but also the
effect involved.

## Memoization

Both main searches have overlapping subproblems:

- parsing repeatedly asks for the same token spans,
- composition repeatedly asks for the same type pairs.

[TDParse/Memoize.lean](TDParse/Memoize.lean) provides:

```lean
memoFix  : structurally keyed unary memoized fixed point
memoFix2 : structurally keyed binary memoized fixed point
```

The cache is a `Std.HashMap`. The parser is memoized by `(Nat, Nat)` spans, and
`combine` is memoized by structural `Ty` pairs. The implementation uses a small
internal `unsafeCast` because the result type of a memoized dependent function
depends on the key. The public API remains typed: callers still receive a result
at exactly the requested type.

## Example

In [Main.lean](Main.lean), the sentence:

```text
two exceeds one
```

has this basic derivation:

1. `two` is a `DP` of type `E`.
2. `exceeds` is a `TV` of type `E ~> E ~> T`.
3. `one` is a `DP` of type `E`.
4. The CFG combines `TV` and `DP` into `VP`.
5. Semantically, `exceeds` combines with `one` by `FA`, yielding `E ~> T`.
6. The CFG combines `DP` and `VP` into `CP`.
7. Semantically, `two` combines with the VP by `BA`, yielding `T`.
8. `runAs T` evaluates the result to `true`.

More interesting examples involve scope, anaphora-like reader dependencies,
stored discourse values, and nondeterministic individuals. In those cases, the
same syntactic tree may support several semantic derivations because composition
can be lifted through the effects in different ways.

## Lean Conventions Used Here

The code uses a small amount of dependent typing. The most common pattern is a
dependent pair:

```lean
def TypedExpr := (t : Ty) × Expr t
```

This means "some semantic type `t`, together with an expression whose index is
exactly `t`." It is the Lean version of storing an existentially typed semantic
object without losing the relation between the tag and the payload.

Branches such as:

```lean
if h : a = b then by
  subst h
  ...
else
  ...
```

use an equality proof to rewrite the local typing context. This is how symbolic
type equality, such as `a = b`, becomes an actual type equality that Lean can
use to build a semantic operator.

Square-bracket arguments such as `[Functor f]` are typeclass arguments, much as
in Haskell. The functions in `TDParse/Combine.lean` use them to build lifted
composition modes only when the relevant algebraic structure is available.

## Extending the Project

To add lexical material, define new `Expr` values in [Main.lean](Main.lean) and
insert them into a lexicon built with `{[ ... ]}`.

To add syntax, add cases to the CFG function in [Main.lean](Main.lean), or define
a new `CFG`.

To add a new effect or composition principle:

1. Extend `FX`, `Ty.dom`, and the notation if needed in
   [TDParse/Data/Ty.lean](TDParse/Data/Ty.lean).
2. Add any required algebraic instances in
   [TDParse/Data/Algebra.lean](TDParse/Data/Algebra.lean).
3. Update the effect predicates in [TDParse/Data/Ty.lean](TDParse/Data/Ty.lean).
4. Add mode labels and search rules in [TDParse/Combine.lean](TDParse/Combine.lean).
5. Add display cases in [TDParse/Display.lean](TDParse/Display.lean).

Because expressions are indexed by semantic type, many mistakes in new
composition rules show up as Lean type errors rather than as bad parses at
runtime.

## Suggested Reading Order

1. [Main.lean](Main.lean): concrete examples.
2. [TDParse.lean](TDParse.lean): the parser/semantics/evaluation pipeline.
3. [TDParse/Data/Derivation.lean](TDParse/Data/Derivation.lean): trees,
   expressions, modes, and lexicons.
4. [TDParse/Data/Ty.lean](TDParse/Data/Ty.lean): semantic types and effects.
5. [TDParse/Combine.lean](TDParse/Combine.lean): effect-driven composition.
6. [TDParse/Memoize.lean](TDParse/Memoize.lean): memoized recursion.

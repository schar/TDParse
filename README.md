Type-driven syntactic and semantic effectful parsing for natural language.

```
$ ghci TDDemo.hs
```

Handles functors, applicatives, monads, adjoints, and commutative effects.
Currently supports Reader for anaphora and context-sensitivity, various
Writer's for not-at-issue content and dref introduction, List/Set for
nondeterminism and indefiniteness, and Continuations for scope and
quantification. Also:

- Finds all analyses consistent with input, lexical category/type assignments,
  and type-driven composition rules.

- No type-shifting! This is an extended type-driven interpreter, in the mold
  of Klein & Sag 1985 or Heim & Kratzer 1998. We also do not instantiate
  effectful combinators in the syntax. Even so, useful higher-order constructs
  like composite functors and composite applicatives (and even some composite
  monads) arise _compositionally_.

- A new treatment of dynamic binding, decomposing the nondeterministic state
  monad (`StateT []`) into adjoint Reading and Writing functors, together with
  nondeterminism. The approach has an intriguing resemblance to algebraic
  effects, and other operationalizations of free monads.

  On this perspective, the State monad transformer arises compositionally,
  and (dynamic) binding arises via the `counit` of the adjunction `W -| R`.

- Includes a notion of normal form derivation to limit spurious ambiguity, in
  most cases (those not having to do with pronoun binding!) exploring exactly
  one derivation per meaning. Helps greatly with usability, and efficiency.

- Includes a treatment of scope islands (essentially the one in [SC's PhD
  thesis](https://semanticsarchive.net/Archive/2JmMWRjY/)) and crossover (in
  terms of a simple restriction on the application of `W -| R`'s `counit`).
  Also aids usability and efficiency (as discussed
  [here](https://aclanthology.org/W17-6208/)).

- Appears pretty efficient, from our informal benchmarking. Seems to handle
  massive ambiguity with relative aplomb.

Output can be displayed as natural deduction proofs, or as trees annotated
with types, rules, and (optionally) semantic values. Here's an example of
latex tree output with types and rules:

```
*TDDemo> outTrees' productions (hasType (S T)) [the, cat, near, someone2, saw, her]
```

<p align="center">
<img width="566" alt="image" src="https://user-images.githubusercontent.com/1521291/188498076-b1dfe76a-be6a-4975-9cba-0b4d5539dd58.png">
</p>

There are a number of routines for viewing output defined in
`TDPretty.hs`.

There is an online version of the software
[here](https://schar.github.io/TDParse) (ported by DB to Purescript). You can
read some notes on the general approach
[here](https://simoncharlow.com/esslli/).

To come/needs done:

- [ ] Integration with a robust/industrial-strength CCG substrate (e.g.,
  OpenCCG)

- [ ] More rigorous benchmarking of soundness and efficiency

- [ ] More HOAS, letting the Haskell compiler do the heavy lifting?

- [ ] Rules for dref introduction, normal form binding derivations. (Sloppy)
  ellipsis and paycheck pronouns!

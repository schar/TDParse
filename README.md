Type-driven syntactic and semantic effectful parsing for natural language.

```
$ ghci TDDemo.hs
```

Handles functors, applicatives, monads, adjoints, and commutative effects.
Finds all analyses consistent with input, lexical category/type assignments,
and type-driven composition rules. Includes a notion of normal form derivation
to limit spurious ambiguity, in most cases (those not having to do with
pronoun binding!) exploring exactly one derivation per meaning.

Output can be displayed as natural deduction proofs, or trees annotated with
types, rules, and (optionally) semantic values. Here is an example of a latex
tree output with types and rules:

```
*TDDemo> outTrees' productions ((== effS T) . getProofType) [the, cat, near, someone2, saw, her]
```

<img width="566" alt="image" src="https://user-images.githubusercontent.com/1521291/188498076-b1dfe76a-be6a-4975-9cba-0b4d5539dd58.png">

Online version [here](https://schar.github.io/TDParse).

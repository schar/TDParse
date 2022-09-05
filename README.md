Type-driven syntactic and semantic effectful parsing for natural language.

```
$ ghci TDDemo.hs
```

Handles functors, applicatives, monads, adjoints, and commutative effects.
Finds all analyses consistent with input, lexical category/type assignments,
and type-driven composition rules. Includes a notion of normal form derivation
to limit spurious ambiguity, in most cases (those not having to do with
pronoun binding!) exploring exactly one derivation per meaning.

```
*TDDemo> outTrees' productions ((== effS T) . getProofType) [the, cat, near, someone2, saw, her]
```

Online version [here](https://schar.github.io/TDParse).

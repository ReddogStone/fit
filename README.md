# fit

A small term-rewriting core. A program is one tree of rules. Applying a chain of rules to
a value gives the set of everything that fits. Models are ordinary arguments, so the same
text can be run as values, read as types, or wired as a machine.

Exploratory. The language changes as the ideas settle.

## The tree

Inner nodes: application `(f x)`, rule `left :> right`, cell `[head | tail]`.
Leaves: symbol, hole, unknown, end `[]`.

## The text

```
name := value          a line: the rest of the scope sees the value through the name
[a b] := value         a line with a pattern on the left
left :> right          a rule; `right <: left` is the same rule written backwards
[rule rule | old]      a chain of rules; only chains can be applied
[a b c]  [h | t]       chains are also the lists
(f x)   x |> f         application; `x |> a :> b` applies the rule to x
^name                  on a left side: a reference to a name bound above
#name                  a fresh unknown, solved by fitting
$name                  a new symbol, created once
```

Names are case-insensitive. A file is one rule, `left :> right`. The right side is any
term; usually it is a scope, lines followed by a result.

## Running

```
cabal build
$(cabal list-bin core) examples/peano.core host.core prelude.core
$(cabal list-bin core) fmt examples/peano.core
cabal test
```

The runner applies the first file to one chain: the core leaves `tie`, `foreign`,
`answers`, then the other files, each as an unapplied rule. `host.core` turns `foreign`
into the host's names. `prelude.core` holds `same`, `one`, `agree` and the host's type
rules.

## Examples

`examples/` holds the working programs, `examples/errors/` the ones that fail on
purpose. Each `.core` has a `.out` with the expected output. `peano.core` is the
shortest statement of the idea: one algebra, two models, same text.

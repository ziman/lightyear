# Lightyear

Lightweight parser combinator library for Idris,
inspired by [Parsec](http://hackage.haskell.org/package/parsec).

Module overview:
* `Lightyear.Core`: central definitions + instances
* `Lightyear.Errmsg`: error message formatting, mainly internal library
* `Lightyear.Combinators`: generic combinators like `many` or `sepBy`
* `Lightyear.Char`: char-bound parsers like `char` or `space`
* `Lightyear.Strings`: string-bound parsers like `strings` or `lexeme`

## Synopsis

This package is used (almost) the same way as Parsec, except for one difference: backtracking.

### Commitment
* Parsec combinators
  won't backtrack if a branch of `<|>` has consumed any input, hence Parsec
  parsers require an explicit `try`.

* Lightyear parsers are backtrack-by-default and there is
  the `commitTo` combinator that makes the parser commit to that branch.

In other words, the following two pieces of code are equivalent (using illustrative combinator names):

Parsec:
```haskell
elem :: Parser Int
elem = (try (string "0x") >> hexNumber) <|> decNumber
```

Lightyear:
```haskell
elem : Parser Int
elem = (string "0x" $> commitTo hexNumber) <|> decNumber
-- which may be abbreviated as:
--   = (string "0x" >! hexNumber) <|> decNumber
```

After reading the prefix `0x`, both parsers commit to reading a hexadecimal number
or nothing at all — Parsec does this automatically, Lightyear uses the `commitTo` combinator
for this purpose.
On the other hand, Parsec requires the `string "0x"` to be wrapped in `try` because
if we are reading `0123`, we definitely don't want to commit to the left branch
upon seeing the leading `0`.

For convenience, `commitTo` is merged in monadic and applicative operators,
yielding the operators `>!=`, `>!`, `<$!>`, `<$!`, and `$!>`.
The `!` in the names is inspired by the notation used for cuts in Prolog.

A parser that uses commitment might look like this (notice the leading
`char '@'` that leads to commitment):
```haskell
entry : Parser Entry
entry = char '@' >! do
  typ <- pack <@> some (satisfy (/= '{'))
  token "{"
  ident <- pack <@> some (satisfy (/= ','))
  token ","
  items <- item `sepBy` comma
  token "}"
  return $ En typ ident items
```

### Lazy Branching with `<|>|`

It is worth noting that Idris itself is a _strict_ language, and thus the `<|>`
operator will evaluate both its arguments eagerly by default. In order to lazily
evaluate different parsing branches we are required to use a special operator:
`<|>|`. In general, all recursive calls of combinators have to occur in a lazy context.

In the wild, it might look like this:

```idris
partial parseExpr : Parser SExpr
parseExpr = parseName <|>| ( MkSExpr <$> parens (many parseExpr) )
```

In the above example, the whole RHS of `<|>|` is lazy, and so the recursive
occurrence of `parseExpr` in it will be evaluated only if the LHS of `<|>|` fails.
Using `<|>` would cause infinite recursion.

For convenience, a version of `<*>` that lazily evalutes its second argument is
included as `<*>|`. Conversely to `<|>|`, the RHS of `<*>|` will be evaluated
only if the LHS of `<*>|` _succeeds_.

### Example
Lightyear is used to parse BibTeX in <a href="https://github.com/ziman/bibdris/blob/master/Bibtex.idr">bibdris</a>.

## Build
```bash
$ make clean
$ make test
$ make install
```
